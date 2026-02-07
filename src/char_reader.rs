// TODO: Add support for utf-8 chars? Currently we support only single byte chars.

use crate::position::Position as Pos;
use derive_more::Debug;
use std::io::{self, Read, Result};

const BUFFER_SIZE: usize = 256;

#[derive(Debug)]
#[debug("CharReader {{ pos: {pos:?}, ... }}")]
pub struct CharReader<'a> {
    read: Box<dyn Read + 'a>,
    /// The position of the next character to read.
    pub pos: Pos,
    /// The amount of valid bytes in the buffer.
    buf_size: u16,
    /// The index of the next byte to read in the buffer.
    buf_idx: u16,
    /// Whether we have reached the end of the input.
    done: bool,
    buf: [u8; BUFFER_SIZE],
}

impl<'a, R: Read + 'a> From<R> for CharReader<'a> {
    fn from(value: R) -> Self {
        CharReader::new(value)
    }
}

impl<'a> CharReader<'a> {
    pub fn new(read: impl Read + 'a) -> Self {
        CharReader {
            read: Box::new(read),
            pos: Pos {
                line: 1,
                column: 1,
                offset: 0,
            },
            buf_size: 0,
            buf_idx: 0,
            done: false,
            buf: [0; BUFFER_SIZE],
        }
    }

    /// Obviously, not actually the rest of the text...
    fn rest(&self) -> &[u8] {
        &self.buf[self.buf_idx as _..]
    }

    fn done(&self) -> bool {
        self.done
    }

    fn reread(&mut self) -> io::Result<()> {
        let buf_size = self.read.read(&mut self.buf)?;
        self.buf_size = buf_size as _;
        self.buf_idx = 0;
        if self.buf_size == 0 {
            self.done = true;
        }
        Ok(())
    }

    fn need_reread(&self) -> bool {
        self.buf_idx >= self.buf_size
    }

    fn reread_if_need(&mut self) -> io::Result<()> {
        if self.need_reread() {
            self.reread()?;
        }
        Ok(())
    }

    fn advance_right(&mut self) -> io::Result<()> {
        self.buf_idx += 1;
        self.pos.offset += 1;
        self.pos.column += 1;
        self.reread_if_need()?;
        Ok(())
    }

    fn advance_down(&mut self) -> io::Result<()> {
        self.buf_idx += 1;
        self.pos.offset += 1;
        self.pos.line += 1;
        self.pos.column = 1;
        self.reread_if_need()?;
        Ok(())
    }

    // --- Public ---

    pub fn pos(&self) -> Pos {
        self.pos
    }

    pub fn peek(&mut self) -> Result<Option<char>> {
        self.reread_if_need()?;
        if self.done() {
            return Ok(None);
        }
        assert!(!self.need_reread());
        Ok(Some(self.rest()[0].into()))
    }

    pub fn pop(&mut self) -> Result<Option<char>> {
        let c = self.peek()?;
        match c {
            Some('\n') => self.advance_down()?,
            Some(_) => self.advance_right()?,
            None => (),
        }
        Ok(c)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn empty() {
        let mut reader = CharReader::from("".as_bytes());
        assert_eq!(reader.pop().unwrap(), None);
    }

    #[test]
    fn read_single_char() {
        let mut reader = CharReader::from("hello world".as_bytes());
        assert_eq!(reader.pop().unwrap(), Some('h'));
    }

    #[test]
    fn read_multiple_chars() {
        let mut reader = CharReader::from("hello world".as_bytes());
        assert_eq!(reader.pop().unwrap(), Some('h'));
        assert_eq!(reader.pop().unwrap(), Some('e'));
        assert_eq!(reader.pop().unwrap(), Some('l'));
        assert_eq!(reader.pop().unwrap(), Some('l'));
        assert_eq!(reader.pop().unwrap(), Some('o'));
    }

    #[test]
    fn read_three_chunks() {
        let str = "a".repeat(BUFFER_SIZE * 2 + 10);
        let mut reader = CharReader::from(str.as_bytes());
        for i in 0..(BUFFER_SIZE * 2 + 10) {
            println!("Reading char {i}");
            assert_eq!(reader.pop().unwrap(), Some('a'));
        }
        println!("Reading EOF");
        assert_eq!(reader.pop().unwrap(), None);
    }

    #[test]
    fn newlines() {
        let source = indoc! {"
            line 1
            line 2
            line 3
        "};
        let mut to_read = source.chars().rev().collect::<String>();
        let mut reader = CharReader::from(source.as_bytes());
        for expected_pos in [
            Pos {
                line: 1,
                column: 1,
                offset: 0,
            },
            Pos {
                line: 1,
                column: 2,
                offset: 1,
            },
            Pos {
                line: 1,
                column: 3,
                offset: 2,
            },
            Pos {
                line: 1,
                column: 4,
                offset: 3,
            },
            Pos {
                line: 1,
                column: 5,
                offset: 4,
            },
            Pos {
                line: 1,
                column: 6,
                offset: 5,
            },
            Pos {
                line: 1,
                column: 7,
                offset: 6,
            },
            Pos {
                line: 2,
                column: 1,
                offset: 7,
            },
            Pos {
                line: 2,
                column: 2,
                offset: 8,
            },
            Pos {
                line: 2,
                column: 3,
                offset: 9,
            },
            Pos {
                line: 2,
                column: 4,
                offset: 10,
            },
            Pos {
                line: 2,
                column: 5,
                offset: 11,
            },
            Pos {
                line: 2,
                column: 6,
                offset: 12,
            },
            Pos {
                line: 2,
                column: 7,
                offset: 13,
            },
            Pos {
                line: 3,
                column: 1,
                offset: 14,
            },
            Pos {
                line: 3,
                column: 2,
                offset: 15,
            },
            Pos {
                line: 3,
                column: 3,
                offset: 16,
            },
            Pos {
                line: 3,
                column: 4,
                offset: 17,
            },
            Pos {
                line: 3,
                column: 5,
                offset: 18,
            },
            Pos {
                line: 3,
                column: 6,
                offset: 19,
            },
            Pos {
                line: 3,
                column: 7,
                offset: 20,
            },
        ] {
            let pos = reader.pos();
            let c = reader.pop().unwrap().unwrap();
            println!("Read char {c:?} at pos {pos:?}");
            assert_eq!(pos, expected_pos);
            assert_eq!(c, to_read.pop().unwrap());
        }
    }
}
