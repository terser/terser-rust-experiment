use swc_common::{self, sync::Lrc, FileName, SourceMap};
use swc_ecma_parser::{
    lexer::Lexer,
    token::{Keyword, Token, TokenAndSpan, Word},
    JscTarget, StringInput, Syntax,
};

#[macro_use]
mod macros;

#[derive(Default, Debug, Clone, PartialEq)]
pub struct ChunkTree {
    start: u32,
    end: u32,
    children: Vec<ChunkTree>,
}

pub enum OpenOrClose {
    // It's a function, method or whatever
    OpenChunk,
    // It's an object or control flow statement
    OpenBrace,
    // Close a brace (braces--)
    Close,
    Neither,
}

#[cfg(not(test))]
static MAX_SIZE: u32 = 5_000;
#[cfg(not(test))]
static MIN_SIZE: u32 = 2_000;

// Smaller chunk sizes for testing!
#[cfg(test)]
static MAX_SIZE: u32 = 12;
#[cfg(test)]
static MIN_SIZE: u32 = 4;

fn process_open_close(tokens: &[TokenAndSpan]) -> (usize, OpenOrClose) {
    match tokens {
        // Chunk openers are all kinds of functions.
        [t!(Token::Arrow), t!(Token::LBrace), ..] => (2, OpenOrClose::OpenChunk),
        [t!(Token::Word(Word::Keyword(Keyword::Function))), rest @ ..] => {
            let (mut skip, rest) = match rest {
                [t!(Token::Word(_)), t!(Token::LParen), rest @ ..] => (3, rest),
                [t!(Token::LParen), rest @ ..] => (2, rest),
                _ => {
                    return (1, OpenOrClose::Neither);
                }
            };

            let mut open_parens = 1;

            // Skip name, parens
            for token in rest {
                skip += 1;
                match token {
                    t!(Token::LParen) => open_parens += 1,
                    t!(Token::RParen) => {
                        open_parens -= 1;

                        if open_parens == 0 {
                            break;
                        }
                    }

                    _ => {}
                };
            }

            // Expect rbrace
            if !matches!(tokens.get(skip), Some(t!(Token::LBrace))) {
                panic!("Invalid syntax");
            }
            skip += 1;

            (skip, OpenOrClose::OpenChunk)
        }

        [t!(Token::LBrace), ..] => (1, OpenOrClose::OpenBrace),
        [t!(Token::RBrace), ..] => (1, OpenOrClose::Close),
        [t!(_), ..] => (1, OpenOrClose::Neither),

        [] => unreachable!(),
    }
}

fn consume<T>(tokens: &[T], skip_count: usize) -> (&T, &[T]) {
    (&tokens[skip_count - 1], &tokens[skip_count..])
}

pub fn chunk_tokens_inner(tokens: &[TokenAndSpan]) -> (ChunkTree, &[TokenAndSpan]) {
    let start_byte = tokens.first().unwrap().span.lo.0;
    let mut end_byte = start_byte;
    let mut children: Vec<ChunkTree> = vec![];

    let mut tokens = tokens;
    let mut braces: i32 = 0;

    while tokens.len() > 0 {
        let (skip, open_or_close) = process_open_close(tokens);
        let (TokenAndSpan { span, .. }, new_tokens) = consume(tokens, skip);

        tokens = new_tokens;
        end_byte = span.hi.0;

        match open_or_close {
            OpenOrClose::OpenChunk => {
                braces += 1;

                let (chunk, new_tokens) = chunk_tokens_inner(tokens);
                tokens = new_tokens;
                end_byte = chunk.end;

                if chunk.end - chunk.start > 0 {
                    children.push(chunk);
                }
            }
            OpenOrClose::OpenBrace => {
                braces += 1;
            }
            OpenOrClose::Close => {
                braces -= 1;

                if braces < 0 {
                    break;
                }
            }
            OpenOrClose::Neither => {}
        };
    }

    (
        ChunkTree {
            start: start_byte,
            end: end_byte,
            children,
        },
        tokens,
    )
}

pub fn chunk_tokens(tokens: &[TokenAndSpan]) -> ChunkTree {
    if tokens.len() == 0 {
        return Default::default();
    }

    let start = tokens[0].span.lo.0;
    let end = tokens[tokens.len() - 1].span.hi.0;
    let (ChunkTree { children, .. }, _) = chunk_tokens_inner(tokens);

    ChunkTree {
        children,
        start,
        end,
    }
}

// Test tokens
fn tt(source: &str) -> Vec<TokenAndSpan> {
    let cm: Lrc<SourceMap> = Default::default();
    let fm = cm.new_source_file(FileName::Custom("test.js".into()), source.into());

    let lexer = Lexer::new(
        Syntax::Es(Default::default()),
        JscTarget::Es2020,
        StringInput::from(&*fm),
        None,
    );

    lexer.collect()
}

/*
#[cfg(feature = "benchme")]
fn main() {
    let source = &std::fs::read_to_string(
        "/home/fabio/devel/terser/memorygate/largeapp/0.6b230bcfa1a68fe8c36d.js",
    )
    .unwrap();
    println!("Tokenising");
    let tokens = tt(source);
    println!("Chunkening");
    let chunk_tree = chunk_tokens(tokens.as_slice());
    println!("{:?}", chunk_tree);
    print_chunk_sizes(chunk_tree);
}
*/

#[cfg(feature = "benchme")]
fn main() {
    let source = &std::fs::read_to_string(
        "/home/fabio/devel/terser/memorygate/largeapp/0.6b230bcfa1a68fe8c36d.js",
        // "polyfill.js",
    )
    .unwrap();
    println!("Tokenising");
    let tokens = tt(source);
    println!("Chunkening");
    let chunk_tree = chunk_tokens(tokens.as_slice());
    println!("{:?}", chunk_tree);
    print_chunk_sizes(chunk_tree, 0);
}

fn print_chunk_sizes(chunk: ChunkTree, level: usize) {
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

    for child in children.clone() {
        print_chunk_sizes(child, level + 1);
    }

    if children.len() > 0 {
        println!("{}]", String::from("  ").repeat(level));
    }
}

fn assert_sane_nesting(chunk: ChunkTree) {
    println!("chunk sizes\n");

    print_chunk_sizes(chunk.clone(), 0);

    println!("--");
}

fn assert_tokens_eq(code: &str, expected: ChunkTree) {
    let chunked = chunk_tokens(tt(code).as_slice());

    assert_sane_nesting(chunked.clone());

    assert_eq!(chunked, expected);
}

#[test]
fn test_consume() {
    assert_eq!(
        consume(&[0, 1, 2, 3, 4], 1),
        (&0, vec![1, 2, 3, 4].as_slice())
    );

    assert_eq!(consume(&[0, 1, 2, 3, 4], 3), (&2, vec![3, 4].as_slice()));

    assert_eq!(consume(&[0, 1, 2], 3), (&2, vec![].as_slice()));
}

#[test]
fn test_no_tok() {
    assert_eq!(
        chunk_tokens(vec![].as_slice()),
        ChunkTree {
            children: vec![],
            start: 0,
            end: 0
        }
    );
}

#[test]
fn test_one_tok() {
    assert_tokens_eq(
        "onetoken",
        ChunkTree {
            children: vec![],
            start: 0,
            end: 8,
        },
    );
}

#[test]
fn test_arrow() {
    assert_tokens_eq(
        "()=>{()=>{hellooo}};",
        ChunkTree {
            start: 0,
            end: 20,
            children: vec![ChunkTree {
                start: 5,
                end: 19,
                children: vec![ChunkTree {
                    start: 10,
                    end: 18,
                    children: vec![],
                }],
            }],
        },
    );
}

#[test]
fn test_small_arrow() {
    assert_tokens_eq(
        "()=>{hi}",
        ChunkTree {
            start: 0,
            end: 8,
            children: vec![],
        },
    );
}

#[test]
fn test_two_arrows() {
    assert_tokens_eq(
        "()=>{12345;()=>{1234567};()=>{1234567}}",
        ChunkTree {
            start: 0,
            end: 39,
            children: vec![ChunkTree {
                start: 5,
                end: 39,
                children: vec![
                    ChunkTree {
                        start: 16,
                        end: 24,
                        children: vec![],
                    },
                    ChunkTree {
                        start: 30,
                        end: 38,
                        children: vec![],
                    },
                ],
            }],
        },
    );
}

#[test]
fn test_garbage_at_end() {
    assert_tokens_eq(
        "()=>{123456789012}; garbage",
        ChunkTree {
            start: 0,
            end: 27,
            children: vec![ChunkTree {
                start: 5,
                end: 18,
                children: vec![],
            }],
        },
    );
}

#[test]
fn test_classic_fn() {
    assert_tokens_eq(
        "(function(__a__, __b){hellooo})",
        ChunkTree {
            start: 0,
            end: 31,
            children: vec![ChunkTree {
                start: 22,
                end: 30,
                children: vec![],
            }],
        },
    );
}

#[test]
fn test_missing_block() {
    assert_tokens_eq(
        "x({ not_block: true })",
        ChunkTree {
            start: 0,
            end: 22,
            children: vec![],
        },
    );
}
