use std::io::Read;

/// A thing that contains characters
pub trait IntoRead {
    type Read: Read;
    fn into_read(self) -> Self::Read;
}

impl IntoRead for String {
    type Read = std::io::Cursor<Vec<u8>>;
    fn into_read(self) -> Self::Read {
        std::io::Cursor::new(self.into_bytes())
    }
}

impl<R: Read> IntoRead for R {
    type Read = R;
    fn into_read(self) -> Self::Read {
        self
    }
}
