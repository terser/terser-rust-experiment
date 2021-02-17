use swc_common::{self, sync::Lrc, FileName, SourceMap};
use swc_ecma_parser::{
    lexer::Lexer,
    token::{Keyword, Token, TokenAndSpan, Word},
    JscTarget, StringInput, Syntax,
};

#[derive(Default, Clone, Debug, PartialEq)]
pub struct ChunkTree {
    pub start: u32,
    pub end: u32,
    pub children: Vec<ChunkTree>,
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
static MAX_COMBINED_SIZE: u32 = 50_000;
#[cfg(not(test))]
static MAX_SIZE: u32 = 5_000;
#[cfg(not(test))]
static MIN_SIZE: u32 = 200;

// Smaller chunk sizes for testing!
#[cfg(test)]
static MAX_COMBINED_SIZE: u32 = 20;
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

            // Expect lbrace
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

        braces += match open_or_close {
            OpenOrClose::OpenBrace | OpenOrClose::OpenChunk => 1,
            OpenOrClose::Close => -1,
            _ => 0,
        };

        if braces < 0 {
            break;
        }

        let (TokenAndSpan { span, .. }, new_tokens) = consume(tokens, skip);

        tokens = new_tokens;
        end_byte = span.hi.0;

        if let OpenOrClose::OpenChunk = open_or_close {
            let (chunk, new_tokens) = chunk_tokens_inner(tokens);
            tokens = new_tokens;
            end_byte = chunk.end;

            if chunk.end - chunk.start > MIN_SIZE {
                children.push(chunk);
            }
        }
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

pub fn chunk_tokens(tokens: &[TokenAndSpan], code_end: u32) -> ChunkTree {
    if tokens.len() == 0 {
        return Default::default();
    }

    let (chunk, _) = chunk_tokens_inner(tokens);

    let ChunkTree { start, end, .. } = chunk;

    if start != 0 || end != code_end {
        ChunkTree {
            start: 0,
            end: code_end,
            children: vec![chunk],
        }
    } else {
        chunk
    }
}

pub fn combine_chunks(chunk: &ChunkTree) -> ChunkTree {
    let len = |chunk: &ChunkTree| chunk.end - chunk.start;
    let combined_nonchild_len = |chunk: &ChunkTree| {
        len(&chunk)
            - chunk
                .children
                .iter()
                .fold(0, |accum, child| accum + len(&child))
    };

    let ChunkTree {
        end,
        start,
        ref children,
    } = *chunk;

    if len(&chunk) < MAX_COMBINED_SIZE {
        ChunkTree {
            end,
            start,
            children: vec![],
        }
    } else if combined_nonchild_len(&chunk) < MAX_COMBINED_SIZE {
        ChunkTree {
            end,
            start,
            children: children.iter().map(|child| combine_chunks(child)).collect(),
        }
    } else {
        chunk.clone()
    }
}

// Test tokens
pub fn tt(source: &str) -> Vec<TokenAndSpan> {
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

#[cfg(test)]
fn assert_tokens_eq(code: &str, expected: ChunkTree) {
    let chunked = chunk_tokens(tt(code).as_slice(), code.chars().count() as u32);

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
        chunk_tokens(vec![].as_slice(), 0),
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
                end: 18,
                children: vec![ChunkTree {
                    start: 10,
                    end: 17,
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
fn test_nest_arrows() {
    assert_tokens_eq(
        "()=>{12345;()=>{1234567};()=>{1234567}}",
        ChunkTree {
            start: 0,
            end: 39,
            children: vec![ChunkTree {
                start: 5,
                end: 38,
                children: vec![
                    ChunkTree {
                        start: 16,
                        end: 23,
                        children: vec![],
                    },
                    ChunkTree {
                        start: 30,
                        end: 37,
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
                end: 17,
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
                end: 29,
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

#[test]
fn test_end_comment() {
    let code = "() => { something_yas() }/**/";

    assert_tokens_eq(
        code,
        ChunkTree {
            start: 0,
            end: 29,
            children: vec![ChunkTree {
                start: 0,
                end: 25,
                children: vec![ChunkTree {
                    start: 8,
                    end: 23,
                    children: vec![],
                }],
            }],
        },
    );

    assert_eq!(code[0..29], *code);
    assert_eq!(code[0..25], *"() => { something_yas() }");
    assert_eq!(code[8..23], *"something_yas()");
}
