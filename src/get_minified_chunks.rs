// In minified code, the chunk header (export function chunk{id})
// is wrapped around the chunk

// This module has fns to wrap the chunk before minify, and unwrap after.

use crate::rope::Rope;
use crate::to_segments::{to_segments, Segment};
use crate::utils::parse_number_at_start;
use std::collections::HashMap;

#[cfg(not(test))]
static CHUNK_HEADER: &str = "/*!// '\"`*/export async function*chunk";
#[cfg(test)]
static CHUNK_HEADER: &str = "export function chunk";
static CHUNK_STANDIN: &str = "chunk";
static CHUNK_END: &str = "END()";

#[cfg(not(test))]
static WORKLOAD_MAX_LENGTH: usize = 300_000;
#[cfg(test)]
static WORKLOAD_MAX_LENGTH: usize = 10;

fn join_chunk(chunk: &Vec<Segment>) -> String {
    chunk
        .iter()
        .map(|segment| match segment {
            Segment::Str(s) => String::from(*s),
            Segment::Ident(id) => format!("chunk{}()", id),
        })
        .collect::<Vec<String>>()
        .join("")
}

fn wrap_chunk_to_minify(chunk: &Vec<Segment>, index: usize) -> String {
    if index == 0 {
        join_chunk(&chunk)
    } else {
        format!(
            "{}{}(){}\n\n{}\n\n{}{};",
            CHUNK_HEADER,
            index,
            "{",
            join_chunk(&chunk),
            "};",
            CHUNK_END,
        )
    }
}

pub fn get_workloads(chunks: Vec<Vec<Segment<'_>>>) -> Vec<String> {
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

fn strip_chunk_wrapper<'a>(chunk: &'a str) -> (usize, String) {
    assert_eq!(chunk.find(CHUNK_HEADER), Some(0));

    // Trim header
    let chunk = &chunk[CHUNK_HEADER.len()..];

    // After the header there's a chunk ID with the function name
    let chunk_number = parse_number_at_start(chunk);

    // Trim
    let chunk = match (chunk.find("{"), chunk.find("}")) {
        (Some(start), Some(end)) if start < end => &chunk[start + 1..end],
        _ => {
            panic!("corrupted chunks")
        }
    };

    (chunk_number, String::from(chunk))
}

pub fn get_minified_chunks(minified_source: &str, index: usize) -> HashMap<usize, String> {
    if index == 0 {
        // First chunk has no boundary
        return vec![(0, String::from(minified_source))]
            .into_iter()
            .collect();
    }

    let mut results = HashMap::new();
    let mut cursor = &minified_source[..];

    loop {
        match (cursor.rfind(CHUNK_HEADER), cursor.rfind(CHUNK_END)) {
            (Some(header_start_index), Some(chunk_end)) => {
                let (chunk_id, minified_source) =
                    strip_chunk_wrapper(&cursor[header_start_index..chunk_end]);

                results.insert(chunk_id, minified_source);

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

fn resolve_chunk_ids_inner<'a>(chunks: &'a Vec<String>, target: usize) -> Rope<'a> {
    let mut rope = Rope { strings: vec![] };
    // 0th chunk is always the parent
    let mut cursor: &str = &chunks[target].as_ref();

    loop {
        if let Some(index) = cursor.find(CHUNK_STANDIN) {
            rope.push_string(&cursor[..index]);

            cursor = &cursor[index + CHUNK_STANDIN.len()..];

            let chunk_id = parse_number_at_start(cursor);
            rope.push_rope(resolve_chunk_ids_inner(&chunks, chunk_id));

            cursor = &cursor[cursor.find(')').unwrap() + 1..];

            if cursor.chars().next() == Some(';') {
                cursor = &cursor[1..];
            }
        } else {
            break;
        }
    }

    rope.push_string(cursor);

    rope
}

pub fn resolve_chunk_ids(chunks: &Vec<String>) -> String {
    resolve_chunk_ids_inner(chunks, 0).strings.join("")
}

#[allow(dead_code)]
fn to_strings(slices: Vec<&str>) -> Vec<String> {
    slices.into_iter().map(|s| String::from(s)).collect()
}

#[allow(dead_code)]
fn to_map(pairs: Vec<(usize, &str)>) -> HashMap<usize, String> {
    pairs
        .into_iter()
        .map(|(id, s)| (id, String::from(s)))
        .collect()
}

#[test]
fn test_resolve_chunk_ids() {
    assert_eq!(
        resolve_chunk_ids(&to_strings(vec!["chunk1();chunk2()", "hi", "world"])),
        "hiworld"
    );

    assert_eq!(
        resolve_chunk_ids(&to_strings(vec!["chunk1();", "hello chunk2();", "world"])),
        "hello world"
    );
}

#[test]
fn test_extract_result_from_bounded_chunk() {
    assert_eq!(
        strip_chunk_wrapper("export function chunk123(){};END();"),
        (123, String::from(""))
    );

    assert_eq!(
        strip_chunk_wrapper("export function chunk42(){hi};END();"),
        (42, String::from("hi"))
    );
}

#[test]
fn test_first_chunks() {
    let chunks = get_minified_chunks("I am the first chunk", 0);
    assert_eq!(
        chunks,
        vec![(0, String::from("I am the first chunk"))]
            .into_iter()
            .collect()
    );
}

#[allow(dead_code)]
fn assert_chunks_eq(source: &str, expected_pairs: Vec<(usize, &str)>) {
    let chunks = get_minified_chunks(source, 1);
    let expected_map = to_map(expected_pairs);

    assert_eq!(chunks, expected_map);
}

#[test]
fn test_get_result_map() {
    assert_chunks_eq("", vec![]);

    assert_chunks_eq("export function chunk1(){hi}END()", vec![(1, "hi")]);

    assert_chunks_eq(
        " trash export function chunk1(){hi}; trashwithoutcurlybrace END(); trash ",
        vec![(1, "hi")],
    );

    assert_chunks_eq(
        " export function chunk1(){hi}; END(); export function chunk2() {world} END() ",
        vec![(1, "hi"), (2, "world")],
    );
}

#[test]
fn test_get_one_workload() {
    assert_eq!(get_workloads(to_segments("hi")), vec!["hi"])
}
