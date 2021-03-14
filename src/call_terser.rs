use crate::get_minified_chunks::{get_minified_chunks, wrap_chunk_to_minify};
use crate::to_segments::Segment;
use crate::Timer;
use std::collections::HashMap;
use std::convert::AsRef;
use std::io::{Read, Write};
use std::process::{ChildStdin, ChildStdout, Command, Stdio};
use std::sync::{Arc, Mutex};
use std::thread::spawn;

// A chunk header can only be placed at the toplevel because of the export.
// And it can't be found inside a comment or string, because it would need escaping

static PRELUDE_SIZE: usize = 32;

#[cfg(not(test))]
static WORKLOAD_MAX_LENGTH: usize = 300_000;
#[cfg(test)]
static WORKLOAD_MAX_LENGTH: usize = 10;

// Speak a simple protocol with terser_worker/worker.js
fn execute_terser_process(
    stdin: &mut ChildStdin,
    stdout: &mut ChildStdout,
    seg_src: String,
) -> String {
    let _timer = Timer::new("actually calling terser");

    let writes_to_byte: &[u8] = seg_src.as_ref();

    let prelude = format!("{}", writes_to_byte.len());
    let prelude = format!("{}{}", prelude, " ".repeat(PRELUDE_SIZE - prelude.len()));

    stdin
        .write(&prelude.as_ref())
        .expect("write prelude to worker");

    stdin
        .write(&writes_to_byte)
        .expect("write javascript to terser process");

    let mut prelude = [0u8; 32];
    stdout
        .read_exact(&mut prelude)
        .expect("read minified prelude");

    let result_len = String::from_utf8(prelude.to_vec())
        .unwrap()
        .chars()
        .fold(String::from(""), |accum, c| {
            if c >= '0' && c <= '9' {
                format!("{}{}", accum, c)
            } else {
                accum
            }
        })
        .parse::<usize>()
        .unwrap();

    let mut result = vec![0; result_len];

    stdout
        .read_exact(&mut result)
        .expect("read javascript from terser process");

    String::from_utf8(result.to_vec()).unwrap()
}

fn get_workloads(chunks: Vec<Vec<Segment<'_>>>) -> Vec<String> {
    let mut out = vec![];

    let mut buffer = String::from("");
    for (index, segment) in chunks.into_iter().enumerate() {
        let segment = wrap_chunk_to_minify(&segment, index);

        if segment.len() + buffer.len() > WORKLOAD_MAX_LENGTH {
            let program_string = format!("{}{}", buffer, segment);

            out.push(program_string);

            buffer = String::from("");
        } else {
            buffer = format!("{}{}", buffer, segment);
        }
    }

    if buffer.len() > 0 {
        out.push(buffer);
    }

    out
}

pub fn call_terser(segments: Vec<Vec<Segment<'_>>>) -> std::io::Result<String> {
    let indexed_workloads = get_workloads(segments).into_iter().enumerate().collect();

    //let pool = ThreadPool::new(4);
    let mut thread_joiners = vec![];

    let shared_indexed_workloads: Arc<Mutex<Vec<(usize, String)>>> =
        Arc::new(Mutex::new(indexed_workloads));
    let shared_results: Arc<Mutex<HashMap<usize, String>>> = Arc::new(Mutex::new(HashMap::new()));

    for i in 0..4 {
        let workloads = shared_indexed_workloads.clone();
        let local_results = shared_results.clone();

        thread_joiners.push(spawn(move || {
            let child_process = Command::new("terser_worker/worker.js")
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn()
                .expect("spawn the terser process");

            let mut stdin = child_process.stdin.expect("child stdin is guaranteed");
            let mut stdout = child_process.stdout.expect("child stdout is guaranteed");

            loop {
                let popped = { workloads.lock().unwrap().pop() };
                if let Some((index, workload)) = popped {
                    println!("thread {} got task {}", i, index);

                    let result = execute_terser_process(&mut stdin, &mut stdout, workload);
                    local_results.lock().unwrap().insert(index, result);
                } else {
                    break;
                }
            }
        }))
    }

    // pool.join();
    for j in thread_joiners {
        j.join().expect("terser worker thread should not panic");
    }

    let mut result_list = vec![];
    let results = shared_results.lock().unwrap();

    for index in 0..results.len() {
        result_list.push(results.get(&index));
    }

    println!("finished {:?} jobs", result_list.len());

    Ok(String::from(""))
}
