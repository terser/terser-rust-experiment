#[allow(unused_imports)]
use swc_ecma_parser::token::TokenAndSpan;

macro_rules! t {
    ($toktype:pat) => {
        TokenAndSpan { token: $toktype, .. }
    }
}
