use derive_more::{Debug, Display};

#[derive(Clone, Copy, Debug, Display, Eq, PartialEq)]
#[debug("Position(line={line}, column={column}, offset={offset})")]
#[display("[{}:{}]", self.line, self.column)]
pub struct Position {
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

impl Default for Position {
    fn default() -> Self {
        Position { line: 1, column: 1, offset: 0 }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_position() {
        let text = "Hello\nWorld\nThis is a test.";
        let pos1 = Position::new(text, 0);
        assert_eq!(pos1.line, 1);
        assert_eq!(pos1.column, 1);

        let pos2 = Position::new(text, 6);
        assert_eq!(pos2.line, 2);
        assert_eq!(pos2.column, 1);

        let pos3 = Position::new(text, 11);
        assert_eq!(pos3.line, 2);
        assert_eq!(pos3.column, 6);

        let pos4 = Position::new(text, 22);
        assert_eq!(pos4.line, 3);
        assert_eq!(pos4.column, 11);
    }

    #[test]
    fn test_get_column() {
        let text = "Hello\nWorld\nThis is a test.";
        assert_eq!(get_column(text, 0), 1);
        assert_eq!(get_column(text, 6), 1);
        assert_eq!(get_column(text, 11), 6);
        assert_eq!(get_column(text, 22), 11);
    }

    #[test]
    fn test_get_line() {
        let text = "Hello\nWorld\nThis is a test.";
        assert_eq!(get_line(text, 0), 1);
        assert_eq!(get_line(text, 6), 2);
        assert_eq!(get_line(text, 11), 2);
        assert_eq!(get_line(text, 22), 3);
    }
}
