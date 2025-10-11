use derive_more::{Debug, Display};

#[derive(Clone, Copy, Debug, Display)]
#[debug("Position(line={}, column={})", self.line(), self.column())]
#[display("[{}:{}]", self.line(), self.column())]
pub struct Position<'a> {
    pub text: &'a str,
    // TODO: Maybe use nessie_parse::Pos instead?
    pub offset: usize,
}

impl<'a> Position<'a> {
    pub const fn new(text: &'a str, offset: usize) -> Self {
        Self { text, offset }
    }

    pub fn line(&self) -> usize {
        self.text[..self.offset]
            .chars()
            .filter(|&c| c == '\n')
            .count()
            + 1
    }

    pub fn column(&self) -> usize {
        self.text[..self.offset]
            .chars()
            .rev()
            .take_while(|&c| c != '\n')
            .count()
            + 1
    }
}

impl<'a> PartialEq for Position<'a> {
    fn eq(&self, other: &Self) -> bool {
        assert!(
            std::ptr::eq(self, other),
            "Comparing positions from different text texts is probably a bug"
        );
        self.offset == other.offset
    }
}
