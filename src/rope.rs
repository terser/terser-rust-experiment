// A rope is a set of &str taken from different places and joined together.
// It helps slicing and dicing large &str without much overhead, hopefully.
pub struct Rope<'a> {
    pub strings: Vec<&'a str>,
}

impl<'a> Rope<'a> {
    pub fn push_rope(self: &mut Self, rope: Rope<'a>) {
        self.strings.extend(rope.strings);
    }

    pub fn push_string(self: &mut Self, s: &'a str) {
        self.strings.push(s);
    }
}
