pub fn parse_number_at_start(slice: &str) -> usize {
    slice
        .chars()
        .take_while(|&c| c >= '0' && c <= '9')
        .collect::<String>()
        .parse::<usize>()
        .expect("bad chunk number")
}

