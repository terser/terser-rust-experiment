extern crate nom;

use itertools::concat;
use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_till, take_until, take_while},
    character::complete::{alpha0, alpha1, one_of},
    character::is_alphabetic,
    combinator::{map_res, opt, recognize},
    error as nom_error,
    error::ParseError,
    sequence::tuple,
    IResult,
};
use std::{error, fmt};

#[derive(Clone, Debug, PartialEq)]
pub enum TreeNode {
    Comment(String),
    Function(String, Vec<TreeNode>),
    Block(Vec<TreeNode>),
    Code(String),
    String(String),
    RegExp(String),
    Call(String, Vec<TreeNode>),
    FlowBlock(String, Vec<TreeNode>),
    Switch(String),
    TryCatch(String),
    Reference(Vec<TreeNode>),
}

fn not_eol_eof(c: char) -> bool {
    c != '\n'
}

fn not_star(c: char) -> bool {
    c != '*'
}

fn skip_whitespace(input: &str) -> IResult<&str, ()> {
    let (input, _) = take_while(|c| c == ' ')(input)?;

    Ok((input, ()))
}

fn parse_line_comment(input: &str) -> IResult<&str, TreeNode> {
    let (input, contents) = recognize(tuple((tag("//"), take_while(not_eol_eof))))(input)?;

    Ok((input, TreeNode::Comment(String::from(contents))))
}

fn parse_multiline_comment(input: &str) -> IResult<&str, TreeNode> {
    let (input, contents) = recognize(tuple((tag("/*"), take_until("*/"), tag("*/"))))(input)?;

    Ok((input, TreeNode::Comment(String::from(contents))))
}

fn parse_string(input: &str) -> IResult<&str, TreeNode> {
    let (input, quote) = alt((tag("'"), tag("\""), tag("`")))(input)?;
    let (input, contents) = take_until(quote)(input)?;
    let (input, _) = take(1usize)(input)?;

    Ok((
        input,
        TreeNode::String(format!("{}{}{}", quote, contents, quote)),
    ))
}

fn parse_regexp(input: &str) -> IResult<&str, TreeNode> {
    let (input, regex) = recognize(tuple((
        tag("/"),
        take_until("/"),
        tag("/"),
        take_while(|c| c >= 'a' && c <= 'z'),
    )))(input)?;

    Ok((input, TreeNode::RegExp(String::from(regex))))
}

fn parse_identifier(input: &str) -> IResult<&str, &str> {
    alpha1(input)
}

fn read_parameter_list(input: &str) -> IResult<&str, ()> {
    // TODO actually read parameter list, counting matching parens, skipping comments and strings
    // etc.
    let (input, _) = take_until(")")(input)?;

    Ok((input, ()))
}

fn parse_classic_function(input: &str) -> IResult<&str, TreeNode> {
    let (input, start_contents) = recognize(tuple((
        tag("function"),
        skip_whitespace,
        opt(parse_identifier),
        skip_whitespace,
        tag("("),
        read_parameter_list,
        tag(")"),
        skip_whitespace,
    )))(input)?;

    if let (input, TreeNode::Block(func_body)) = parse_block(input)? {
        Ok((
            input,
            TreeNode::Function(String::from(start_contents), func_body),
        ))
    } else {
        panic!();
    }
}

fn parse_chain(input: &str) -> IResult<&str, &str> {
    recognize(tuple((
        alt((
            recognize(tuple((tag("."), parse_expression))),
            recognize(tuple((tag("["), parse_expression, tag("]")))),
        )),
        recognize(opt(parse_chain)),
    )))(input)
}

fn parse_simple_callee(input: &str) -> IResult<&str, &str> {
    recognize(tuple((parse_identifier, opt(parse_chain))))(input)
}

fn parse_optional_comma(input: &str) -> IResult<&str, TreeNode> {
    let (input, contents) =
        recognize(tuple((skip_whitespace, opt(tag(",")), skip_whitespace)))(input)?;

    Ok((input, TreeNode::Code(String::from(contents))))
}

fn parse_call(input: &str) -> IResult<&str, TreeNode> {
    let (input, callee) = parse_simple_callee(input)?;
    let (mut input, start_part) = recognize(tuple((tag("("), parse_optional_comma)))(input)?;

    let mut arg_code: Vec<TreeNode> = vec![TreeNode::Code(String::from(start_part))];

    loop {
        if input == "" || input.chars().next() == Some(')') {
            break;
        }

        let (new_input, arg) = parse_expression(input)?;
        arg_code.push(arg);

        let (new_input, separation) = parse_optional_comma(new_input)?;
        arg_code.push(separation);

        input = new_input;
    }

    let (input, _) = tag(")")(input)?;

    arg_code.push(TreeNode::Code(String::from(")")));

    Ok((
        input,
        TreeNode::Call(String::from(callee), join_statement_list(arg_code)),
    ))
}

fn parse_expression(input: &str) -> IResult<&str, TreeNode> {
    if let Ok((input, ident)) = parse_identifier(input) {
        return Ok((input, TreeNode::Code(String::from(ident))));
    }

    panic!();
}

fn parse_random_code(input: &str) -> IResult<&str, TreeNode> {
    let mut braces = 0;

    for i in 0..input.len() {
        let sliced: &str = &input[i..];

        match input.chars().nth(i) {
            Some('{') => {
                braces += 1;
                continue;
            }
            Some('}') => {
                braces -= 1;

                if braces < 0 {
                    return Ok((sliced, TreeNode::Code(String::from(&input[..i]))));
                }

                continue;
            }
            _ => {}
        };

        let result: IResult<&str, &str> = alt((
            tag("//"),
            tag("/*"),
            tag("function"),
            tag("'"),
            tag("\""),
            tag("`"),
        ))(sliced);

        if result.is_ok() {
            return Ok((sliced, TreeNode::Code(String::from(&input[..i]))));
        }
    }

    Ok(("", TreeNode::Code(String::from(input))))
}

fn join_statement_list(input: Vec<TreeNode>) -> Vec<TreeNode> {
    let mut out = vec![];
    let mut last = None;

    for item in input.into_iter() {
        if let Some(TreeNode::Code(last_code)) = last {
            if let TreeNode::Code(this_code) = item {
                out.pop();

                let joined_code = TreeNode::Code(format!("{}{}", last_code, this_code));

                last = Some(joined_code.clone());

                out.push(joined_code);

                continue;
            }
        }

        last = Some(item.clone());

        out.push(item);
    }

    out
}

fn parse_flow_block(input: &str) -> IResult<&str, TreeNode> {
    let (input, header) = recognize(tuple((
        alt((tag("if"), tag("while"), tag("for"))),
        skip_whitespace,
        tag("("),
        read_parameter_list,
        tag(")"),
        skip_whitespace,
    )))(input)?;

    let (input, body) = parse_statement(input)?;

    Ok((input, TreeNode::FlowBlock(String::from(header), vec![body])))
}

fn parse_switch(input: &str) -> IResult<&str, TreeNode> {
    let (input, code) = recognize(tuple((
        tag("switch"),
        skip_whitespace,
        tag("("),
        read_parameter_list,
        tag(")"),
        skip_whitespace,
        parse_block,
    )))(input)?;

    Ok((input, TreeNode::Switch(String::from(code))))
}

fn parse_trycatch(input: &str) -> IResult<&str, TreeNode> {
    let (input, contents) = recognize(tuple((
        tag("try"),
        skip_whitespace,
        parse_block,
        skip_whitespace,
        tag("catch"),
        skip_whitespace,
        opt(tuple((tag("("), read_parameter_list, tag(")")))),
        skip_whitespace,
        parse_block,
    )))(input)?;

    Ok((input, TreeNode::TryCatch(String::from(contents))))
}

fn parse_statement(input: &str) -> IResult<&str, TreeNode> {
    if input.chars().nth(0) == Some('{') {
        parse_block(input)
    } else {
        let (input, node): (&str, TreeNode) = alt((
            parse_line_comment,
            parse_multiline_comment,
            parse_flow_block,
            parse_classic_function,
            parse_switch,
            parse_trycatch,
            parse_random_code,
        ))(input)?;

        Ok((input, node))
    }
}

fn parse_statement_list(input: &str) -> IResult<&str, Vec<TreeNode>> {
    let mut nodes = vec![];
    let mut input = input;

    loop {
        let (new_input, node) = parse_statement(input)?;

        input = new_input;
        nodes.push(node);

        if input == "" || input.chars().nth(0) == Some('}') {
            break;
        }
    }

    Ok((input, join_statement_list(nodes)))
}

fn parse_block(input: &str) -> IResult<&str, TreeNode> {
    let (input, (_, children, _)) = tuple((tag("{"), parse_statement_list, tag("}")))(input)?;

    let concat_children = join_statement_list(concat(vec![
        vec![TreeNode::Code(String::from("{"))],
        children,
        vec![TreeNode::Code(String::from("}"))],
    ]));

    Ok((input, TreeNode::Block(concat_children)))
}

fn stringify_statement_list(input: Vec<TreeNode>) -> String {
    input.into_iter().fold(String::from(""), {
        |accum, item| format!("{}{}", accum, stringify(item))
    })
}

fn stringify(input: TreeNode) -> String {
    match input {
        TreeNode::Code(code) => code,
        TreeNode::Comment(contents) => contents,
        TreeNode::Function(header, body) => format!("{}{}", header, stringify_statement_list(body)),
        TreeNode::Block(body) => stringify_statement_list(body),
        node => panic!("Unknown node {:?}", node),
    }
}

fn get_node_children<'a>(node: &'a TreeNode) -> Option<&'a Vec<TreeNode>> {
    match node {
        TreeNode::Function(_, body) => Some(&body),
        TreeNode::Block(body) => Some(&body),
        TreeNode::FlowBlock(_, body) => Some(&body),
        TreeNode::Call(_, body) => Some(&body),
        _ => None,
    }
}

fn get_local_length(node: &TreeNode) -> usize {
    match node {
        TreeNode::Code(code) => code.len(),
        TreeNode::Comment(contents) => contents.len(),
        TreeNode::Function(header, _) => header.len(),
        TreeNode::String(contents) => contents.len(),
        TreeNode::RegExp(contents) => contents.len(),
        TreeNode::Call(callee, _) => callee.len(),
        TreeNode::Block(contents) => get_tree_total_length(contents),
        TreeNode::FlowBlock(header, _) => header.len(),
        TreeNode::TryCatch(contents) => contents.len(),
        TreeNode::Switch(contents) => contents.len(),
        TreeNode::Reference(_) => 32,
    }
}

enum TraverseSignal {
    Replace(TreeNode),
    Continue,
    Skip,
}

fn traverse_node<F>(node: &TreeNode, func: &mut F)
where
    F: FnMut(&TreeNode) -> TraverseSignal,
{
    if let TraverseSignal::Skip = func(node) {
        return;
    }

    if let Some(children) = get_node_children(node) {
        traverse(children, func);
    }
}

fn traverse<F>(tree: &Vec<TreeNode>, func: &mut F)
where
    F: FnMut(&TreeNode) -> TraverseSignal,
{
    for node in tree {
        traverse_node(node, func);
    }
}

fn get_children_length(given_node: &TreeNode) -> usize {
    let mut len: usize = 0;

    traverse_node(given_node, &mut |node| {
        if node != given_node {
            len += get_local_length(node);
        }

        TraverseSignal::Continue
    });

    len
}

fn get_total_length(given_node: &TreeNode) -> usize {
    get_local_length(given_node) + get_children_length(given_node)
}

fn get_tree_total_length(given_tree: &Vec<TreeNode>) -> usize {
    given_tree
        .iter()
        .fold(0, |acc, node| acc + get_total_length(node))
}

static mut BUCKET_SIZE: (usize, usize) = (500, 1000);
fn get_bucket_size() -> (usize, usize) {
    unsafe { BUCKET_SIZE }
}
unsafe fn set_bucket_size(new_size: (usize, usize)) {
    unsafe { BUCKET_SIZE = new_size };
}

fn get_compression_buckets(input: &Vec<TreeNode>) -> Vec<Vec<TreeNode>> {
    let mut out = vec![];
    let mut bucket = vec![];

    let mut current_bucket_size: usize = 0;
    let (min_bucket_size, max_bucket_size) = get_bucket_size();

    let whole_code_length = input.into_iter().fold(0, |accum, node| {
        accum + get_local_length(node) + get_children_length(node)
    });

    println!(" -> starting, code length {:?}", whole_code_length);

    if whole_code_length <= max_bucket_size {
        return vec![input.clone()];
    }

    traverse(input, &mut |node| {
        println!(" -> node {:?}", node);

        let this_size = get_local_length(node);
        let children_size = get_children_length(node);

        let combined_size = this_size + children_size;

        if combined_size > min_bucket_size {
            if current_bucket_size > 0 {
                current_bucket_size = 0;
                out.push(bucket.clone());
                bucket.clear();
            }

            out.push(vec![node.clone()]);

            return TraverseSignal::Continue;
        } else if current_bucket_size + combined_size > min_bucket_size {
            bucket.push(node.clone());
            current_bucket_size = 0;
            out.push(bucket.clone());
            bucket.clear();

            return TraverseSignal::Continue;
        }

        bucket.push(node.clone());
        current_bucket_size += combined_size;

        TraverseSignal::Continue
    });

    if bucket.len() > 0 {
        out.push(bucket);
    }

    out
}

fn deconstruct_with_refs(tree: &Vec<TreeNode>, depth: u64) -> Vec<TreeNode> {
    let (min_size, max_size) = get_bucket_size();

    let max_iters = 1;
    let mut iters = 0;

    let mut tree = tree.clone();

    loop {
        if get_tree_total_length(&tree) < min_size {
            break;
        }

        tree = tree
            .iter()
            .map(|node| match node {
                TreeNode::Function(head, body) => {
                    if iters > 0 || get_children_length(&node) >= min_size {
                        let func = TreeNode::Function(
                            head.clone(),
                            deconstruct_with_refs(body, depth + 1),
                        );

                        if depth > 0 {
                            TreeNode::Reference(vec![func])
                        } else {
                            func
                        }
                    } else {
                        node.clone()
                    }
                }
                _ => node.clone(),
            })
            .collect();

        let has_any_block = tree.iter().any(|node| {
            if let TreeNode::Block(_) = node {
                true
            } else {
                false
            }
        });

        if has_any_block && get_tree_total_length(&tree) > min_size && tree.len() > 0 {
            let first_ref = TreeNode::Reference(vec![tree.first().unwrap().clone()]);

            tree = tree.iter().skip(1).fold(vec![first_ref], |acc, node| {
                let len = acc.len();
                let mut acc = acc.clone();

                if let TreeNode::Reference(children) = acc.last().unwrap() {
                    if let Some(TreeNode::Block(_)) = children.last() {
                        acc.push(TreeNode::Reference(vec![node.clone()]));
                        acc
                    } else {
                        let mut new_children = children.clone();
                        new_children.push(node.clone());

                        acc[len - 1] = TreeNode::Reference(new_children);
                        acc
                    }
                } else {
                    panic!(
                        "Expected TreeNode::Reference, found {:?}",
                        acc.last().unwrap()
                    );
                }
            });
        }

        iters += 1;
        if iters >= max_iters {
            break;
        }
    }

    return tree.to_vec();
}

fn test_parse<T>(parser: fn(&str) -> IResult<&str, T>, input: &str) -> T {
    match parser(input) {
        Ok((input, result)) => {
            if input != "" {
                panic!("Trash remains in input: {:?}", input)
            }

            result
        }

        Err(nom::Err::Error(error)) => panic!("Parser error: {:?}", error),

        Err(err) => panic!("Unknown kind of parser error: {:?}", err),
    }
}

fn test_compression_buckets(input: &str, expect: Vec<Vec<TreeNode>>) {
    let result = get_compression_buckets(&test_parse(parse_statement_list, input));

    assert_eq!(result, expect);
}

#[test]
fn test_parse_line_comment() {
    assert_eq!(
        test_parse(parse_line_comment, "// hi"),
        TreeNode::Comment(String::from("// hi")),
    );
    assert_eq!(
        parse_line_comment("// hi\nnotcomment"),
        Ok(("\nnotcomment", TreeNode::Comment(String::from("// hi"))))
    );
}

#[test]
fn test_parse_multiline_comment() {
    assert_eq!(
        test_parse(parse_multiline_comment, "/* multiline \n\n hi */"),
        TreeNode::Comment(String::from("/* multiline \n\n hi */"))
    );

    assert_eq!(
        test_parse(parse_multiline_comment, "/* * * */"),
        TreeNode::Comment(String::from("/* * * */"))
    );
}

#[test]
fn test_parse_classic_function() {
    assert_eq!(
        test_parse(parse_classic_function, "function () {}"),
        TreeNode::Function(
            String::from("function () "),
            vec![TreeNode::Code(String::from("{}"))]
        )
    );

    assert_eq!(
        test_parse(parse_classic_function, "function withName () {}"),
        TreeNode::Function(
            String::from("function withName () "),
            vec![TreeNode::Code(String::from("{}"))]
        )
    );

    assert_eq!(
        parse_classic_function("function withName").unwrap_err(),
        nom::Err::Error(nom_error::Error {
            code: nom::error::ErrorKind::Tag,
            input: ""
        })
    );

    assert_eq!(
        test_parse(
            parse_classic_function,
            "function (args, moreargs) { CONTENTS }"
        ),
        TreeNode::Function(
            String::from("function (args, moreargs) "),
            vec![TreeNode::Code(String::from("{ CONTENTS }"))]
        )
    );
}

#[test]
fn test_parse_random_code() {
    assert_eq!(
        test_parse(parse_random_code, "random"),
        TreeNode::Code(String::from("random"))
    );

    assert_eq!(
        parse_random_code("x // x").unwrap(),
        ("// x", TreeNode::Code(String::from("x ")))
    );
}

#[test]
fn test_parse_block() {
    assert_eq!(
        test_parse(parse_block, "{myBlock()}"),
        TreeNode::Block(vec![TreeNode::Code(String::from("{myBlock()}"))])
    );
}

#[test]
fn test_parse_random_code_braces() {
    assert_eq!(
        test_parse(parse_random_code, "{}x"),
        TreeNode::Code(String::from("{}x"))
    );

    assert_eq!(
        test_parse(parse_random_code, "{ { }x"),
        TreeNode::Code(String::from("{ { }x"))
    );

    assert_eq!(
        parse_random_code("{ { } } }x").unwrap(),
        ("}x", TreeNode::Code(String::from("{ { } } ")))
    );
}

#[test]
fn test_parse_body() {
    assert_eq!(
        test_parse(parse_statement_list, "/*hi*//*hi*/"),
        vec![
            TreeNode::Comment(String::from("/*hi*/")),
            TreeNode::Comment(String::from("/*hi*/"))
        ]
    );

    assert_eq!(
        test_parse(parse_statement_list, "random"),
        vec![TreeNode::Code(String::from("random"))]
    );

    assert_eq!(
        parse_statement_list("hello worldfunction(){}"),
        Ok((
            "",
            vec![
                TreeNode::Code(String::from("hello world")),
                test_parse(parse_classic_function, "function(){}")
            ]
        ))
    );

    assert_eq!(
        parse_statement_list("hello worldfunction(){}"),
        Ok((
            "",
            vec![
                TreeNode::Code(String::from("hello world")),
                test_parse(parse_classic_function, "function(){}")
            ]
        ))
    );
}

#[test]
fn test_parse_nested_fn() {
    assert_eq!(
        test_parse(
            parse_statement_list,
            "function x() { function y() {CONTENT} }"
        ),
        vec![TreeNode::Function(
            String::from("function x() "),
            vec![
                TreeNode::Code(String::from("{ ")),
                TreeNode::Function(
                    String::from("function y() "),
                    vec![TreeNode::Code(String::from("{CONTENT}"))]
                ),
                TreeNode::Code(String::from(" }")),
            ]
        )]
    )
}

#[test]
fn test_parse_statement() {
    assert_eq!(
        test_parse(parse_statement, "foo = {}"),
        TreeNode::Code(String::from("foo = {}"))
    );
}

#[test]
fn test_parse_if() {
    assert_eq!(
        test_parse(parse_statement, "if (x) {}"),
        TreeNode::FlowBlock(
            String::from("if (x) "),
            vec![TreeNode::Block(vec![TreeNode::Code(String::from("{}"))])]
        )
    );
}

#[test]
fn test_parse_switch() {
    assert_eq!(
        test_parse(parse_statement, "switch (x) {}"),
        TreeNode::Switch(String::from("switch (x) {}"),)
    );
}

#[test]
fn test_parse_trycatch() {
    assert_eq!(
        test_parse(parse_statement, "try {} catch (e) {}"),
        TreeNode::TryCatch(String::from("try {} catch (e) {}"))
    );
}

#[test]
fn test_parse_regex() {
    assert_eq!(
        test_parse(parse_regexp, "/x/anythingisaflag"),
        TreeNode::RegExp(String::from("/x/anythingisaflag"))
    );
}

#[test]
fn test_parse_call() {
    assert_eq!(
        test_parse(parse_call, "func()"),
        TreeNode::Call(
            String::from("func"),
            vec![TreeNode::Code(String::from("()"))]
        )
    );

    assert_eq!(
        test_parse(parse_call, "func(a, b, c)"),
        TreeNode::Call(
            String::from("func"),
            vec![TreeNode::Code(String::from("(a, b, c)"))]
        )
    );

    assert_eq!(
        test_parse(parse_call, "obj[foo].bar()"),
        TreeNode::Call(
            String::from("obj[foo].bar"),
            vec![TreeNode::Code(String::from("()"))]
        )
    );
}

#[test]
fn test_parse_string() {
    assert_eq!(
        test_parse(parse_string, "'hello'"),
        TreeNode::String(String::from("'hello'"))
    );
    assert_eq!(
        test_parse(parse_string, "\"hello\""),
        TreeNode::String(String::from("\"hello\""))
    );
    assert_eq!(
        test_parse(parse_string, "`thiskindofstring`"),
        TreeNode::String(String::from("`thiskindofstring`"))
    );
}

#[test]
fn test_parse_confusing() {
    assert_eq!(
        test_parse(parse_statement_list, "/* * * */"),
        vec![TreeNode::Comment(String::from("/* * * */"))]
    );
}

#[test]
fn test_open_close_braces() {
    // A closing brace ends the body
    assert_eq!(
        parse_statement_list("code}rest").unwrap(),
        ("}rest", vec![TreeNode::Code(String::from("code"))])
    );

    // Two opening braces is an error
    assert_eq!(parse_statement_list("{ { ").is_ok(), false);
}

#[test]
fn test_traverse() {
    let mut found = vec![];
    let code = "\
        one\
        //comment\n\
        function x(){}\
    ";

    traverse(&test_parse(parse_statement_list, code), &mut |node| {
        found.push(node.clone());

        TraverseSignal::Continue
    });

    let fn_code = TreeNode::Code(String::from("{}"));

    assert_eq!(
        found,
        vec![
            TreeNode::Code(String::from("one")),
            TreeNode::Comment(String::from("//comment")),
            TreeNode::Code(String::from("\n")),
            TreeNode::Function(String::from("function x()"), vec![fn_code.clone()]),
            fn_code
        ]
    );
}

#[test]
fn test_compress_buckets_divide() {
    unsafe {
        set_bucket_size((5, 10));
    };

    test_compression_buckets("hi", vec![vec![TreeNode::Code(String::from("hi"))]]);

    test_compression_buckets(
        "h();/**/h2()",
        vec![
            vec![
                TreeNode::Code(String::from("h();")),
                TreeNode::Comment(String::from("/**/")),
            ],
            vec![TreeNode::Code(String::from("h2()"))],
        ],
    );
}

#[test]
fn test_deconstruct_with_refs() {
    unsafe {
        set_bucket_size((5, 10));
    };

    let statements = vec![TreeNode::Function(
        String::from("func"),
        vec![TreeNode::Function(
            String::from("func"),
            vec![TreeNode::Function(String::from("bucketme"), vec![])],
        )],
    )];
    assert_eq!(
        deconstruct_with_refs(&statements, 0),
        vec![TreeNode::Function(
            String::from("func"),
            vec![TreeNode::Reference(vec![TreeNode::Function(
                String::from("func"),
                vec![TreeNode::Function(String::from("bucketme"), vec![])]
            )])]
        )]
    );

    let statements_wider = vec![
        TreeNode::Code(String::from("none")),
        TreeNode::Block(vec![TreeNode::Code(String::from("{randomcodebucketme}"))]),
        TreeNode::Code(String::from("bucketmetoo")),
    ];
    assert_eq!(
        deconstruct_with_refs(&statements_wider, 0),
        vec![
            TreeNode::Reference(vec![
                TreeNode::Code(String::from("none")),
                TreeNode::Block(vec![TreeNode::Code(String::from("{randomcodebucketme}"))]),
            ]),
            TreeNode::Reference(vec![TreeNode::Code(String::from("bucketmetoo"))])
        ]
    );
}

#[test]
fn test_nested_code() {
    assert_eq!(
        test_parse(parse_statement_list, "{/*com*/{/*com2*/}}"),
        vec![TreeNode::Block(vec![
            TreeNode::Code(String::from("{")),
            TreeNode::Comment(String::from("/*com*/")),
            TreeNode::Block(vec![
                TreeNode::Code(String::from("{")),
                TreeNode::Comment(String::from("/*com2*/")),
                TreeNode::Code(String::from("}")),
            ]),
            TreeNode::Code(String::from("}")),
        ])]
    );
}

#[test]
fn test_stringify_code() {
    assert_eq!(
        stringify(TreeNode::Code(String::from("hello world"))),
        String::from("hello world")
    );

    assert_eq!(
        stringify(TreeNode::Comment(String::from("/* hello world */"))),
        String::from("/* hello world */")
    );

    assert_eq!(
        stringify(TreeNode::Function(
            String::from("/* hello world */"),
            vec![
                TreeNode::Code(String::from("{hello if (1) ")),
                TreeNode::Block(vec![TreeNode::Code(String::from("{gotoFail()}"))]),
                TreeNode::Code(String::from("}"))
            ]
        )),
        String::from("/* hello world */{hello if (1) {gotoFail()}}")
    );
}
