use derive_more::{Debug, Display};

#[derive(Clone, Copy, Debug, Display)]
#[debug("Position(line={}, column={})", self.line, self.column)]
#[display("[{}:{}]", self.line, self.column)]
pub struct Position {
    // TODO: Maybe use nessie_parse::Pos instead?
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

fn get_line(text: &str, offset: usize) -> usize {
    text[..offset]
        .chars()
        .filter(|&c| c == '\n')
        .count()
        + 1
}

fn get_column(text: &str, offset: usize) -> usize {
    text[..offset]
        .chars()
        .rev()
        .take_while(|&c| c != '\n')
        .count()
        + 1
}

impl Position {
    pub fn new(text: &str, offset: usize) -> Self {
        Self { column: get_column(text, offset), line: get_line(text, offset), offset }
    }
}

impl PartialEq for Position {
    fn eq(&self, other: &Self) -> bool {
        assert!(
            std::ptr::eq(self, other),
            "Comparing positions from different text texts is probably a bug"
        );
        self.offset == other.offset
    }
}
