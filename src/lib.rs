#[macro_use]
mod macros;
mod chunk;
mod chunk_to_string;

use chunk::ChunkTree;

#[cfg(feature = "benchme")]
fn main() {
    let source = &std::fs::read_to_string(
        "/home/fabio/devel/terser/memorygate/largeapp/0.6b230bcfa1a68fe8c36d.js",
        // "polyfill-mini.js",
    )
    .unwrap();
    println!("Tokenising");
    let tokens = tt(source);
    println!("Chunking");

    let chunk_tree = chunk_tokens(tokens.as_slice(), source.chars().count() as u32);
    let chunk_tree = combine_chunks(&chunk_tree);
    debug_print_chunks(&chunk_tree, 0);

    for chunk in insert_chunk_identifiers(&source, &chunk_tree) {
        println!("✂️{}✂️", chunk.code_string);
        println!("\n\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@");
        println!("This was {} - {}", chunk.start, chunk.end);
        println!("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\n");
    }

    println!("chunk count: {}", get_chunk_count(&chunk_tree));
}

fn get_chunk_count(chunk: &ChunkTree) -> u64 {
    1 + chunk
        .children
        .iter()
        .fold(0, |accum, chunk| accum + get_chunk_count(chunk))
}

fn debug_print_chunks(chunk: &ChunkTree, level: usize) {
    let ChunkTree {
        start,
        end,
        children,
    } = chunk;

    let indent = String::from("  ").repeat(level);

    println!(
        "{}({:?} - {:?}) {}",
        indent,
        start,
        end,
        if children.len() > 0 { "[" } else { "" }
    );

    for child in children {
        debug_print_chunks(&child, level + 1);
    }

    if children.len() > 0 {
        println!("{}]", String::from("  ").repeat(level));
    }
}
