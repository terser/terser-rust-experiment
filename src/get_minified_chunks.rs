// In minified code, the chunk header (export function chunk{id})
// is wrapped around the chunk

// This module has fns to wrap the chunk before minify, and unwrap after.

use crate::to_segments::Segment;
use std::collections::HashMap;

#[cfg(not(test))]
static CHUNK_HEADER: &str = "/*!// '\"`*/export async function*chunk";
#[cfg(test)]
static CHUNK_HEADER: &str = "function chunk";

static CHUNK_END: &str = "END()";

pub fn wrap_chunk_to_minify(chunk: &Vec<Segment>, index: usize) -> String {
    let chunk_text: Vec<String> = chunk
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
        chunk_text.join(""),
        "};",
        CHUNK_END,
    )
}

pub fn strip_chunk_wrapper<'a>(chunk: &'a str) -> (usize, &'a str) {
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

pub fn get_minified_chunks<'a>(result: &'a str) -> HashMap<usize, &'a str> {
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

#[allow(dead_code)]
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
