pub fn parse_number_at_start(slice: &str) -> usize {
    slice
        .chars()
        .take_while(|&c| c >= '0' && c <= '9')
        .collect::<String>()
        .parse::<usize>()
        .expect("bad chunk number")
}

pub fn skip_open_braces(source: &str, brace_count: i32) -> usize {
    let mut brace_count = brace_count;

    for (index, c) in source.chars().enumerate() {
        brace_count += match c {
            '{' => 1,
            '}' => -1,
            _ => 0,
        };

        if brace_count == 0 {
            return index;
        }
    }

    panic!("could not find closing brace");
}
