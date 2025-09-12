use derive_more::Display;

#[derive(Clone, Copy, Debug, Display)]
#[display("[{}:{}]", self.line(), self.column())]
pub struct Position<'a> {
    pub text: &'a str,
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
