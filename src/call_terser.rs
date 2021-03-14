use crate::to_segments::Segment;
use crate::Timer;
use std::collections::HashMap;
use std::convert::AsRef;
use std::io::{Read, Write};
use std::process::{Command, Stdio};
use std::sync::{Arc, Mutex};
use threadpool::ThreadPool;

// A chunk header can only be placed at the toplevel because of the export.
// And it can't be found inside a comment or string, because it would need escaping

#[cfg(not(test))]
static CHUNK_HEADER: &str = "/*!// '\"`*/export async function*chunk";
#[cfg(test)]
static CHUNK_HEADER: &str = "function chunk";

static CHUNK_END: &str = "END()";

#[cfg(not(test))]
static WORKLOAD_MAX_LENGTH: usize = 420_000;
#[cfg(test)]
static WORKLOAD_MAX_LENGTH: usize = 10;

fn execute_terser_process(seg_src: String) -> String {
    let _timer = Timer::new("actually calling terser");

    let child_process = Command::new("terser")
        .args(vec!["-c", "--timings"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("spawn the terser process");

    child_process
        .stdin
        .expect("child stdin is guaranteed")
        .write(&seg_src.as_ref())
        .expect("write javascript to terser process");

    let mut result = String::new();
    child_process
        .stdout
        .expect("child stdout is guaranteed")
        .read_to_string(&mut result)
        .expect("read javascript from terser process");

    result
}

fn chunk_rope_to_string(rope: &Vec<Segment>, index: usize) -> String {
    let rope_strings: Vec<String> = rope
        .iter()
        .map(|segment| match segment {
            Segment::Str(s) => String::from(*s),
            Segment::Ident(id) => format!("chunk{}()", id),
        })
        .collect();

    format!(
        "{}{}(){}\n\n{}\n\n{}{};",
        CHUNK_HEADER,
        index,
        "{",
        rope_strings.join(""),
        "};",
        CHUNK_END,
    )
}

fn get_workloads(segments: Vec<Vec<Segment<'_>>>) -> Vec<String> {
    let mut out = vec![];
    let segments_iter = segments.into_iter();

    let mut buffer = String::from("");
    for (index, rope) in segments_iter.enumerate() {
        let segment = chunk_rope_to_string(&rope, index);

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
    let shared_results: Arc<Mutex<HashMap<usize, String>>> = Arc::new(Mutex::new(HashMap::new()));
    let pool = ThreadPool::new(4);

    for (index, workload) in get_workloads(segments).into_iter().enumerate() {
        let local_index = index;
        let local_results = shared_results.clone();

        pool.execute(move || {
            let result = execute_terser_process(workload);
            local_results.lock().unwrap().insert(local_index, result);
        });
    }

    pool.join();

    let mut result_list = vec![];
    let results = shared_results.lock().unwrap();

    for index in 0..results.len() {
        result_list.push(results.get(&index));
    }

    println!("finished {:?} jobs", result_list.len());

    Ok(String::from(""))
}

// In minified code, the chunk header (export function chunk{id}) is around our chunk
// and we need to remove it
fn strip_chunk_wrapper<'a>(chunk: &'a str) -> (usize, &'a str) {
    assert_eq!(chunk.find(CHUNK_HEADER), Some(0));

    // Trim header. After the header there's a number
    let chunk = &chunk[CHUNK_HEADER.len()..];

    let chunk_number = chunk
        .chars()
        .take_while(|&c| c >= '0' && c <= '9')
        .collect::<String>()
        .parse::<usize>()
        .expect("bad chunk number");

    // Trim
    let chunk = match (chunk.find("{"), chunk.find("}")) {
        (Some(start), Some(end)) if start < end => &chunk[start + 1..end],
        _ => {
            panic!("corrupted chunks")
        }
    };

    (chunk_number, chunk)
}

fn get_minified_chunks<'a>(result: &'a str) -> HashMap<usize, &'a str> {
    let mut results = HashMap::new();
    let mut cursor = &result[..];

    loop {
        match (cursor.rfind(CHUNK_HEADER), cursor.rfind(CHUNK_END)) {
            (Some(header_start_index), Some(chunk_end)) => {
                let (chunk_id, result) =
                    strip_chunk_wrapper(&cursor[header_start_index..chunk_end]);

                results.insert(chunk_id, result);

                cursor = &cursor[..header_start_index];
            }
            (None, None) => {
                break;
            }
            _ => {
                panic!("corrupted chunks");
            }
        };
    }

    results
}

#[test]
fn test_extract_result_from_bounded_chunk() {
    assert_eq!(
        strip_chunk_wrapper("function chunk123(){};END();"),
        (123, "")
    );

    assert_eq!(
        strip_chunk_wrapper("function chunk42(){hi};END();"),
        (42, "hi")
    );
}

fn assert_chunks_eq(source: &str, expected_pairs: Vec<(usize, &str)>) {
    let chunks = get_minified_chunks(source);
    let expected_map: HashMap<usize, &str> = expected_pairs.into_iter().collect();

    assert_eq!(chunks, expected_map);
}

#[test]
fn test_get_result_map() {
    assert_eq!(get_minified_chunks(""), HashMap::new());

    assert_eq!(
        get_minified_chunks("function chunk1(){hi}END()"),
        vec![(1, "hi")].into_iter().collect()
    );

    assert_chunks_eq(
        " trash function chunk1(){hi}; trashwithoutcurlybrace END(); trash ",
        vec![(1, "hi")],
    );

    assert_chunks_eq(
        " function chunk1(){hi}; END(); function chunk2() {world} END() ",
        vec![(1, "hi"), (2, "world")],
    );
}
