// TODO: Add support for utf-8 chars? Currently we support only single byte chars.

use crate::Pos;
use derive_more::Debug;
use std::{collections::VecDeque, io::{Read, Result}};

const BUFFER_SIZE: usize = 1024;

#[derive(Debug)]
#[debug("CharReader {{ pos: {pos:?}, ... }}")]
pub struct CharReader<R: Read> {
    read: R,
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

impl<'a> From<&'a str> for CharReader<&'a [u8]> {
    fn from(value: &'a str) -> Self {
        CharReader::from_read(value.as_bytes())
    }
}

impl From<String> for CharReader<VecDeque<u8>> {
    fn from(value: String) -> Self {
        CharReader::from_read(value.into_bytes().into())
    }
}

impl<R: Read> CharReader<R> {
    pub fn from_read(read: R) -> Self {
        CharReader {
            read,
            pos: Pos::default(),
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

    fn reread(&mut self) -> Result<()> {
        let buf_size = self.read.read(&mut self.buf)?;
        dbg!(buf_size);
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

    fn reread_if_need(&mut self) -> Result<()> {
        if self.need_reread() {
            self.reread()?;
        }
        Ok(())
    }

    pub fn peek(&mut self) -> Result<Option<char>> {
        self.reread_if_need()?;
        if self.done() {
            return Ok(None);
        }
        assert!(!self.need_reread());
        Ok(Some(self.rest()[0].into()))
    }

    fn advance_right(&mut self) -> Result<()> {
        self.buf_idx += 1;
        self.pos.offset += 1;
        self.pos.col += 1;
        self.reread_if_need()?;
        Ok(())
    }

    fn advance_down(&mut self) -> Result<()> {
        self.buf_idx += 1;
        self.pos.offset += 1;
        self.pos.line += 1;
        self.pos.col = 0;
        self.reread_if_need()?;
        Ok(())
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

    #[test]
    fn empty() {
        let mut reader = CharReader::from("");
        assert_eq!(reader.pop().unwrap(), None);
    }

    #[test]
    fn read_single_char() {
        let mut reader = CharReader::from("hello world");
        assert_eq!(reader.pop().unwrap(), Some('h'));
    }
 
    #[test]
    fn read_multiple_chars() {
        let mut reader = CharReader::from("hello world");
        assert_eq!(reader.pop().unwrap(), Some('h'));
        assert_eq!(reader.pop().unwrap(), Some('e'));
        assert_eq!(reader.pop().unwrap(), Some('l'));
        assert_eq!(reader.pop().unwrap(), Some('l'));
        assert_eq!(reader.pop().unwrap(), Some('o'));
    }

    #[test]
    fn read_three_chunks() {
        let str = "a".repeat(BUFFER_SIZE * 2 + 10);
        let mut reader = CharReader::from(str);
        for i in 0..(BUFFER_SIZE * 2 + 10) {
            println!("Reading char {}", i);
            assert_eq!(reader.pop().unwrap(), Some('a'));
        }
        println!("Reading EOF");
        assert_eq!(reader.pop().unwrap(), None);
    }
}
