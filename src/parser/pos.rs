pub type Row = u16;
pub type Col = u16;

/// A position in some source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pos {
    /// Starts at 0.
    pub offset: usize,
    /// Starts at 1.
    pub row: Row,
    /// Starts at 1.
    pub col: Col,
}

impl Pos {
    pub const fn start() -> Self {
        Pos {
            offset: 0,
            row: 1,
            col: 1,
        }
    }
}

impl Default for Pos {
    fn default() -> Self {
        Pos::start()
    }
}

