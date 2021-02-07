use swc_ecma_parser::token::TokenAndSpan;

macro_rules! t {
    ($toktype:pat) => {
        TokenAndSpan { token: $toktype, .. }
    }
}
