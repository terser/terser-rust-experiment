use crate::chunk::{chunk_tokens, tt, ChunkTree};

#[derive(PartialEq, Debug)]
pub enum Segment<'a> {
    Str(&'a str),
    Ident(usize),
}

fn insert_child_chunk_identifiers<'a, 'b>(
    code_string: &'a str,
    chunk: &ChunkTree,
    chunk_index: &'b mut usize,
) -> Vec<Segment<'a>> {
    let ChunkTree {
        start,
        end,
        ref children,
    } = *chunk;

    if children.len() == 0 {
        return vec![Segment::Str(&code_string[start as usize..end as usize])];
    }

    let mut code_slices: Vec<Segment> = vec![];
    let mut previous_end = start as usize;

    for child in children {
        *chunk_index += 1;
        code_slices.push(Segment::Str(
            &code_string[previous_end..child.start as usize],
        ));
        code_slices.push(Segment::Ident(*chunk_index));
        previous_end = child.end as usize;
    }

    if previous_end < end as usize {
        code_slices.push(Segment::Str(&code_string[previous_end..end as usize]));
    }

    return code_slices;
}

fn insert_chunk_identifiers<'a, 'b>(
    source: &'a str,
    chunk_tree: &ChunkTree,
    chunk_index: &'b mut usize,
) -> Vec<Vec<Segment<'a>>> {
    let code_slices_here = insert_child_chunk_identifiers(source, chunk_tree, chunk_index);
    let mut code_slices: Vec<Vec<Segment>> = vec![code_slices_here];

    for child in &chunk_tree.children {
        let child_idents = insert_chunk_identifiers(source, child, chunk_index);
        code_slices.extend(child_idents);
    }

    code_slices
}

pub fn to_segments<'a>(source: &'a str) -> Vec<Vec<Segment<'a>>> {
    let chunked = chunk_tokens(tt(source).as_slice(), source.len() as u32);
    let mut chunk_index = 0;

    insert_chunk_identifiers(source, &chunked, &mut chunk_index)
}

fn test_join_segments(segments: &Vec<Segment>) -> String {
    let seg_strings: Vec<String> = segments
        .iter()
        .map(|segment| match segment {
            Segment::Str(s) => String::from(*s),
            Segment::Ident(id) => format!("chunk{}()", id),
        })
        .collect();

    seg_strings.join("")
}

#[allow(dead_code)]
fn assert_chunk_identifiers(code: &str, result: Vec<&str>) {
    let with_segments = to_segments(code);
    let joined: Vec<String> = with_segments
        .iter()
        .map(|segments| test_join_segments(&segments))
        .collect();

    assert_eq!(joined, result);
}

#[test]
fn test_insert_chunk_identifiers() {
    assert_chunk_identifiers("", vec![""]);

    assert_chunk_identifiers("yo()", vec!["yo()"]);

    assert_chunk_identifiers(
        "() => { here_is_a_big_chunk() }",
        vec!["() => { chunk1() }", "here_is_a_big_chunk()"],
    );

    assert_chunk_identifiers(
        "(a) => { big_code(ayy) }; \
            (b) => { big_code(bee) }; \
            (c) => { big_code(cee) }",
        vec![
            "(a) => { chunk1() }; \
                (b) => { chunk2() }; \
                (c) => { chunk3() }",
            "big_code(ayy)",
            "big_code(bee)",
            "big_code(cee)",
        ],
    );

    assert_chunk_identifiers(
        "() => { () => { here_is_a_big_chunk() } }",
        vec![
            "() => { chunk1() }",
            "() => { chunk2() }",
            "here_is_a_big_chunk()",
        ],
    );
}

#[test]
fn test_insert_chunk_identifiers_children() {
    assert_chunk_identifiers("a => { one_chunk }", vec!["a => { chunk1() }", "one_chunk"]);

    assert_chunk_identifiers(
        "a => { one_chunk }; b => { other_chunk }; c => { third_chunk }",
        vec![
            "a => { chunk1() }; b => { chunk2() }; c => { chunk3() }",
            "one_chunk",
            "other_chunk",
            "third_chunk",
        ],
    );

    assert_chunk_identifiers(
        "a => { b => { large_code } }",
        vec!["a => { chunk1() }", "b => { chunk2() }", "large_code"],
    );
}
