//! TODO: write a description. right now it just handles reading, splicing and CRLF

use std::ops::Range;

#[derive(Debug)]
pub struct Pos {
    pub line: usize,
    pub column: usize,
}
pub struct Reader<'a> {
    source: &'a [u8],
    // byte position in source
    bpos: usize,
    line: usize,
    column: usize,
    tab_stop: usize,
    size_: usize,
}

impl<'a> Reader<'a> {
    pub fn new(src: &'a str) -> Self {
        let bytes = src.as_bytes();
        // getting rid of starting UTF-8 BOM
        let bpos = if bytes.starts_with(&[0xEF, 0xBB, 0xBF]) {
            3
        } else {
            0
        };
        Self {
            source: bytes,
            bpos,
            line: 1,
            column: 1,
            tab_stop: 8,
            size_: bytes.len(),
        }
    }

    pub fn from_source(&self, range: &Range<usize>) -> &str {
        let out = str::from_utf8(&self.source[range.start..range.end]).unwrap();
        out
    }

    pub fn size(&self) -> usize {
        self.size_
    }

    pub fn is_eof(&self) -> bool {
        let mut p = self.bpos;
        while let Some(l) = Self::ending_len(self.source, p) {
            p += l;
        }
        p >= self.size_
    }

    pub fn pos(&self) -> Pos {
        Pos {
            line: self.line,
            column: self.column,
        }
    }

    pub fn offset(&self) -> usize {
        self.bpos
    }

    /// returns the current byte
    pub fn peek(&self) -> u8 {
        self.peek_at(0)
    }

    pub fn peek_at(&self, n: usize) -> u8 {
        let mut p = self.bpos;
        // A splice (`\` + newline) is invisible to the caller, so it can sit
        // before *any* of the n+1 characters we step over, including the last.
        // The loop below skips the splices in front of each of the first n
        // characters. the trailing loop skips the ones in front of the character
        // we actually return. Without the trailing loop, `"\<newline>a"` with
        // n = 0 would return `\` instead of `a`.
        for _ in 0..n {
            while let Some(l) = Self::ending_len(self.source, p) {
                p += l;
            }
            if p >= self.source.len() {
                return 0;
            }
            p += Self::char_width(self.source, p);
        }
        while let Some(l) = Self::ending_len(self.source, p) {
            p += l;
        }
        match self.source.get(p).copied().unwrap_or(0) {
            b'\r' => b'\n',
            c => c,
        }
    }

    fn char_width(src: &[u8], p: usize) -> usize {
        if src.get(p) == Some(&b'\r') && src.get(p + 1) == Some(&b'\n') {
            2
        } else {
            1
        }
    }

    /// advances one step forward. consumes endings, folds CRLF into \n and tracks position
    pub fn advance(&mut self) -> u8 {
        self.eat_endings();
        let Some(&c) = self.source.get(self.bpos) else {
            return 0;
        };
        self.bpos += 1;
        match c {
            b'\n' => {
                self.column = 1;
                self.line += 1;
            }
            b'\r' => {
                if self.source.get(self.bpos) == Some(&b'\n') {
                    self.bpos += 1;
                }
                self.line += 1;
                self.column = 1;
                return b'\n';
            }
            b'\t' => {
                self.column = ((self.column - 1) / self.tab_stop + 1) * self.tab_stop + 1;
            }
            _ => self.column += 1,
        }

        return c;
    }

    fn eat_endings(&mut self) {
        while let Some(c) = Self::ending_len(self.source, self.bpos) {
            self.bpos += c;
            self.column = 1;
            self.line += 1;
        }
    }

    fn ending_len(src: &'a [u8], p: usize) -> Option<usize> {
        if src.get(p) != Some(&b'\\') {
            return None;
        }
        let mut q = p + 1;
        while matches!(src.get(q), Some(b' ' | b'\t')) {
            q += 1;
        }
        match src.get(q) {
            Some(b'\n') => Some(q + 1 - p),
            Some(b'\r') => {
                let extra = if src.get(q + 1) == Some(&b'\n') { 2 } else { 1 };
                Some(q + extra - p)
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::reader::Reader;

    #[test]
    fn peek_at_counts_crlf_as_one() {
        let r = Reader::new("a\r\nb");
        assert_eq!(r.peek_at(0), b'a');
        assert_eq!(r.peek_at(1), b'\n');
        assert_eq!(r.peek_at(2), b'b');
        assert_eq!(r.peek_at(3), 0);
    }
}
