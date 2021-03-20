use crate::get_minified_chunks::{get_minified_chunks, get_workloads, resolve_chunk_ids};
use crate::to_segments::Segment;
use crate::utils::parse_number_at_start;
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

    let result_len = parse_number_at_start(&String::from_utf8(prelude.to_vec()).unwrap());

    let mut result = vec![0; result_len];
    stdout
        .read_exact(&mut result)
        .expect("read javascript from terser process");

    String::from_utf8(result.to_vec()).unwrap()
}

pub fn call_terser(segments: Vec<Vec<Segment<'_>>>) -> std::io::Result<String> {
    let indexed_workloads: Vec<(usize, String)> =
        get_workloads(segments).into_iter().enumerate().collect();
    let workload_count = indexed_workloads.len();

    let mut thread_joiners = vec![];

    let shared_indexed_workloads = Arc::new(Mutex::new(indexed_workloads));
    let shared_results = Arc::new(Mutex::new(HashMap::new()));

    for i in 0..std::cmp::min(4, workload_count) {
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
                let popped = workloads.lock().unwrap().pop();
                if let Some((index, workload)) = popped {
                    println!("thread {} got task {}", i, index);

                    let result = execute_terser_process(&mut stdin, &mut stdout, workload);

                    for (chunk_id, source) in get_minified_chunks(&result, index) {
                        local_results.lock().unwrap().insert(chunk_id, source);
                    }
                } else {
                    break;
                }
            }
        }))
    }

    for joiner in thread_joiners {
        joiner.join().expect("terser worker thread panicked");
    }

    let mut result_list = vec![];
    let mut results = shared_results.lock().unwrap();

    for index in 0..results.len() {
        result_list.push(results.remove(&index).unwrap());
    }

    println!("finished {:?} jobs", result_list.len());

    Ok(resolve_chunk_ids(&result_list))
}

#[cfg(feature = "test_all")]
#[test]
fn test_call_terser() {
    let segments = vec![
        vec![
            Segment::Str("export default () => {"),
            Segment::Ident(1),
            Segment::Str("}"),
        ],
        vec![Segment::Str("hello = "), Segment::Str("'wo' + 'rld'")],
    ];

    assert_eq!(
        call_terser(segments).unwrap(),
        String::from("export default()=>{hello=\"world\"};")
    );
}
