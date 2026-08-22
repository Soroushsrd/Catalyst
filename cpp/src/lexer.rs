use std::ops::Range;

use crate::{
    errors::{ErrorType, PreprocessorErr},
    reader::{Pos, Reader},
};

#[derive(Debug)]
pub struct PPToken {
    pub kind: PPTokenType,
    pub pos: Pos,
    pub range: Range<usize>,
    pub gap: Gap,
}

/// What separated this token from the previous one.
#[derive(Clone, Copy, Debug)]
pub struct Gap {
    /// whitespace or a comment preceded this token
    pub ws_before: bool,
    /// first token on a logical line. needed for `#` directive
    pub line_start: bool,
}

impl Gap {
    /// The gap in front of the very first token in a file: no whitespace,
    /// but it *is* at a line start.
    pub fn bof() -> Self {
        Self {
            ws_before: false,
            line_start: true,
        }
    }

    /// or-together two adjacent gaps. Neither flag is ever cleared by merging.
    fn merge(&mut self, other: Gap) {
        self.ws_before |= other.ws_before;
        self.line_start |= other.line_start;
    }
}

impl Default for Gap {
    fn default() -> Self {
        Self {
            ws_before: false,
            line_start: false,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PPTokenType {
    /// any sequence of letters, digits, or underscores
    /// which begin with _ or a letter
    Ident,
    /// All normal ints and float consts as well as
    /// numbers defined with 'e+', 'e-', 'E+', 'E-',
    /// 'p+', 'p-', 'P-', 'P+'
    /// example: 0xE+12
    /// if a '.' appears in the number, after it a decimal digit is required
    /// after which any number of digits, '_' or numbers can occur
    Number,
    /// String consts. anything as "..." counts as string const
    /// inside "...", a backslash is used to escape chars
    StringLiteral,
    /// Char consts. anything as '...' counts as char const
    /// inside '...', a backslash is used to escape chars
    /// prefix forms include 'L', 'u', 'U', 'u8', and R"..."
    CharLiteral,
    /// Header file names (#include). they are in "..." or <...> format
    /// which defines where the preprocessor should look at
    /// inside header names, backslash is an ordinary char
    HeaderFile,
    Punc(Punct),
    /// any other single char is considered 'other'. it passes through unmodified
    /// the compiler will reject it later on
    /// '@', ``, and '$' as well as control characters other than NUL.
    /// all characters with the high bit set are also 'other' (numeric range 0x7F-0xFF)
    /// Nul is usually ignored (^@ is ascii nul)
    Other,
    EOF,
}

/// All normal punctuators in ASCII except '@' and '$' and `` .
/// all two and three char operators are punctuators as well
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Punct {
    // brackets
    LBracket, // [    also <:
    RBracket, // ]    also :>
    LParen,   // (
    RParen,   // )
    LBrace,   // {    also <%
    RBrace,   // }    also %>

    // member access
    Dot,   // .
    Arrow, // ->

    // increment / decrement
    PlusPlus,   // ++
    MinusMinus, // --

    // unary / ambiguous
    Amp,   // &
    Star,  // *
    Plus,  // +
    Minus, // -
    Tilde, // ~
    Bang,  // !

    // arithmetic / bitwise / shift
    Slash,   // /
    Percent, // %
    Shl,     // <<
    Shr,     // >>
    Caret,   // ^
    Pipe,    // |

    // relational / equality
    Lt,   // <
    Gt,   // >
    Lte,  // <=
    Gte,  // >=
    EqEq, // ==
    Ne,   // !=

    // logical
    AmpAmp,   // &&
    PipePipe, // ||

    // conditional / statement
    Question, // ?
    Colon,    // :
    Semi,     // ;
    Ellipsis, // ...

    // assignment
    Eq,        // =
    StarEq,    // *=
    SlashEq,   // /=
    PercentEq, // %=
    PlusEq,    // +=
    MinusEq,   // -=
    ShlEq,     // <<=
    ShrEq,     // >>=
    AmpEq,     // &=
    CaretEq,   // ^=
    PipeEq,    // |=

    // separators and the preprocessor's own operators
    Comma,    // ,
    Hash,     // #    also %:
    HashHash, // ##   also %:%:
}

pub struct Lexer<'a> {
    reader: Reader<'a>,
    tokens: Vec<PPToken>,
    errors: Vec<PreprocessorErr>,
}

impl<'a> Lexer<'a> {
    pub fn new(reader: Reader<'a>) -> Self {
        Self {
            reader,
            tokens: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn errors(&self) -> &[PreprocessorErr] {
        &self.errors
    }

    /// a high level parser. should call underlying parsing mechanisms
    pub fn lex(&mut self) -> Vec<PPToken> {
        // start-of-file counts as a line start, so a `#` in column 1 of line 1
        // is a directive even with no preceding newline
        let mut gap = Gap::bof();
        loop {
            gap.merge(self.skip_ws());
            if self.reader.is_eof() {
                break;
            }
            self.scan_tokens(gap);
            gap = Gap::default();
        }
        let (start_offset, start_pos) = self.reader.start_pos();
        self.push_token(PPTokenType::EOF, start_offset, start_pos, gap);
        std::mem::take(&mut self.tokens)
    }

    /// Consumes whitespace and comments, reporting what it crossed.
    /// Comments are whitespace. they never become tokens.
    fn skip_ws(&mut self) -> Gap {
        let mut gap = Gap::default();
        loop {
            match self.peek() {
                // Reader folds \r and \r\n into \n, so there is no b'\r' arm
                b' ' | b'\t' | 0x0B | 0x0C => {
                    self.advance();
                    gap.ws_before = true;
                }
                b'\n' => {
                    self.advance();
                    gap.ws_before = true;
                    gap.line_start = true;
                }
                b'/' if self.peek_at(1) == b'*' => {
                    self.skip_block_comment();
                    gap.ws_before = true;
                }
                b'/' if self.peek_at(1) == b'/' => {
                    self.advance();
                    self.advance();
                    while !self.reader.is_eof() && self.peek() != b'\n' {
                        self.advance();
                    }
                    gap.ws_before = true;
                }
                _ => return gap,
            }
        }
    }

    /// Never sets line_start, even when it swallows newlines. a block comment
    /// spanning lines does not end a directive line.
    fn skip_block_comment(&mut self) {
        self.advance(); // '/'
        self.advance(); // '*'
        loop {
            if self.reader.is_eof() {
                self.errors.push(PreprocessorErr::new(
                    ErrorType::UnexpectedEOF,
                    "Unterminated block comment",
                ));
                return;
            }
            if self.peek() == b'*' && self.peek_at(1) == b'/' {
                self.advance();
                self.advance();
                return;
            }
            self.advance();
        }
    }

    fn scan_tokens(&mut self, gap: Gap) {
        let (start_offset, start_pos) = self.reader.start_pos();
        let c = self.advance();
        match c {
            b'[' => self.push_token(
                PPTokenType::Punc(Punct::LBracket),
                start_offset,
                start_pos,
                gap,
            ),
            b']' => self.push_token(
                PPTokenType::Punc(Punct::RBracket),
                start_offset,
                start_pos,
                gap,
            ),
            b'(' => self.push_token(
                PPTokenType::Punc(Punct::LParen),
                start_offset,
                start_pos,
                gap,
            ),
            b')' => self.push_token(
                PPTokenType::Punc(Punct::RParen),
                start_offset,
                start_pos,
                gap,
            ),
            b'{' => self.push_token(
                PPTokenType::Punc(Punct::LBrace),
                start_offset,
                start_pos,
                gap,
            ),
            b'}' => self.push_token(
                PPTokenType::Punc(Punct::RBrace),
                start_offset,
                start_pos,
                gap,
            ),
            b'.' => {
                if self.peek() == b'.' && self.peek_at(1) == b'.' {
                    self.advance();
                    self.advance();
                    self.push_token(
                        PPTokenType::Punc(Punct::Ellipsis),
                        start_offset,
                        start_pos,
                        gap,
                    );
                } else if self.peek().is_ascii_digit() {
                    self.lex_number(start_offset, start_pos, gap);
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Dot), start_offset, start_pos, gap);
                }
            }
            b'*' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.push_token(
                        PPTokenType::Punc(Punct::StarEq),
                        start_offset,
                        start_pos,
                        gap,
                    );
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Star), start_offset, start_pos, gap);
                }
            }
            b'+' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.push_token(
                        PPTokenType::Punc(Punct::PlusEq),
                        start_offset,
                        start_pos,
                        gap,
                    );
                } else if self.peek() == b'+' {
                    self.advance();
                    self.push_token(
                        PPTokenType::Punc(Punct::PlusPlus),
                        start_offset,
                        start_pos,
                        gap,
                    );
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Plus), start_offset, start_pos, gap);
                }
            }
            b'-' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.push_token(
                        PPTokenType::Punc(Punct::MinusEq),
                        start_offset,
                        start_pos,
                        gap,
                    );
                } else if self.peek() == b'-' {
                    self.advance();
                    self.push_token(
                        PPTokenType::Punc(Punct::MinusMinus),
                        start_offset,
                        start_pos,
                        gap,
                    );
                } else if self.peek() == b'>' {
                    self.advance();
                    self.push_token(
                        PPTokenType::Punc(Punct::Arrow),
                        start_offset,
                        start_pos,
                        gap,
                    );
                } else {
                    self.push_token(
                        PPTokenType::Punc(Punct::Minus),
                        start_offset,
                        start_pos,
                        gap,
                    );
                }
            }
            b'&' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.push_token(
                        PPTokenType::Punc(Punct::AmpEq),
                        start_offset,
                        start_pos,
                        gap,
                    );
                } else if self.peek() == b'&' {
                    self.advance();
                    self.push_token(
                        PPTokenType::Punc(Punct::AmpAmp),
                        start_offset,
                        start_pos,
                        gap,
                    );
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Amp), start_offset, start_pos, gap);
                }
            }
            b'~' => self.push_token(
                PPTokenType::Punc(Punct::Tilde),
                start_offset,
                start_pos,
                gap,
            ),
            b'!' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::Ne), start_offset, start_pos, gap);
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Bang), start_offset, start_pos, gap);
                }
            }
            b'/' => {
                // `//` and `/*` are already gone since skip_ws ran before this
                if self.peek() == b'=' {
                    self.advance();
                    self.push_token(
                        PPTokenType::Punc(Punct::SlashEq),
                        start_offset,
                        start_pos,
                        gap,
                    );
                } else {
                    self.push_token(
                        PPTokenType::Punc(Punct::Slash),
                        start_offset,
                        start_pos,
                        gap,
                    );
                }
            }
            b'%' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.push_token(
                        PPTokenType::Punc(Punct::PercentEq),
                        start_offset,
                        start_pos,
                        gap,
                    );
                } else if self.peek() == b':' {
                    self.advance();
                    if self.peek() == b'%' && self.peek_at(1) == b':' {
                        self.advance();
                        self.advance();
                        self.push_token(
                            PPTokenType::Punc(Punct::HashHash),
                            start_offset,
                            start_pos,
                            gap,
                        );
                    } else {
                        self.push_token(
                            PPTokenType::Punc(Punct::Hash),
                            start_offset,
                            start_pos,
                            gap,
                        );
                    }
                } else if self.peek() == b'>' {
                    self.advance();
                    self.push_token(
                        PPTokenType::Punc(Punct::RBrace),
                        start_offset,
                        start_pos,
                        gap,
                    );
                } else {
                    self.push_token(
                        PPTokenType::Punc(Punct::Percent),
                        start_offset,
                        start_pos,
                        gap,
                    );
                }
            }
            b'<' => {
                if self.peek() == b'<' {
                    self.advance();
                    if self.peek() == b'=' {
                        self.advance();
                        self.push_token(
                            PPTokenType::Punc(Punct::ShlEq),
                            start_offset,
                            start_pos,
                            gap,
                        );
                    } else {
                        self.push_token(
                            PPTokenType::Punc(Punct::Shl),
                            start_offset,
                            start_pos,
                            gap,
                        );
                    }
                } else if self.peek() == b':' {
                    self.advance();
                    self.push_token(
                        PPTokenType::Punc(Punct::LBracket),
                        start_offset,
                        start_pos,
                        gap,
                    );
                } else if self.peek() == b'%' {
                    self.advance();
                    self.push_token(
                        PPTokenType::Punc(Punct::LBrace),
                        start_offset,
                        start_pos,
                        gap,
                    );
                } else if self.peek() == b'=' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::Lte), start_offset, start_pos, gap);
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Lt), start_offset, start_pos, gap);
                }
            }
            b'>' => {
                if self.peek() == b'>' {
                    self.advance();
                    if self.peek() == b'=' {
                        self.advance();
                        self.push_token(
                            PPTokenType::Punc(Punct::ShrEq),
                            start_offset,
                            start_pos,
                            gap,
                        );
                    } else {
                        self.push_token(
                            PPTokenType::Punc(Punct::Shr),
                            start_offset,
                            start_pos,
                            gap,
                        );
                    }
                } else if self.peek() == b'=' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::Gte), start_offset, start_pos, gap);
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Gt), start_offset, start_pos, gap);
                }
            }
            b'^' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.push_token(
                        PPTokenType::Punc(Punct::CaretEq),
                        start_offset,
                        start_pos,
                        gap,
                    );
                } else {
                    self.push_token(
                        PPTokenType::Punc(Punct::Caret),
                        start_offset,
                        start_pos,
                        gap,
                    );
                }
            }
            b'|' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.push_token(
                        PPTokenType::Punc(Punct::PipeEq),
                        start_offset,
                        start_pos,
                        gap,
                    );
                } else if self.peek() == b'|' {
                    self.advance();
                    self.push_token(
                        PPTokenType::Punc(Punct::PipePipe),
                        start_offset,
                        start_pos,
                        gap,
                    );
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Pipe), start_offset, start_pos, gap);
                }
            }
            b'=' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::EqEq), start_offset, start_pos, gap);
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Eq), start_offset, start_pos, gap);
                }
            }
            b'?' => self.push_token(
                PPTokenType::Punc(Punct::Question),
                start_offset,
                start_pos,
                gap,
            ),
            b':' => {
                if self.peek() == b'>' {
                    self.advance();
                    self.push_token(
                        PPTokenType::Punc(Punct::RBracket),
                        start_offset,
                        start_pos,
                        gap,
                    );
                } else {
                    self.push_token(
                        PPTokenType::Punc(Punct::Colon),
                        start_offset,
                        start_pos,
                        gap,
                    );
                }
            }
            b';' => self.push_token(PPTokenType::Punc(Punct::Semi), start_offset, start_pos, gap),
            b',' => self.push_token(
                PPTokenType::Punc(Punct::Comma),
                start_offset,
                start_pos,
                gap,
            ),
            b'#' => {
                if self.peek() == b'#' {
                    self.advance();
                    self.push_token(
                        PPTokenType::Punc(Punct::HashHash),
                        start_offset,
                        start_pos,
                        gap,
                    );
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Hash), start_offset, start_pos, gap);
                }
            }
            b'\'' => self.lex_chr_lit(start_offset, start_pos, gap),
            b'\"' => self.lex_str_lit(start_offset, start_pos, gap),
            _ => {
                if c.is_ascii_digit() {
                    self.lex_number(start_offset, start_pos, gap);
                } else if c.is_ascii_alphabetic() || c == b'_' {
                    self.lex_ident(start_offset, start_pos, gap);
                } else {
                    self.push_token(PPTokenType::Other, start_offset, start_pos, gap);
                }
            }
        }
    }

    fn lex_ident(&mut self, start: usize, start_pos: Pos, gap: Gap) {
        // preprocessor accepts numbers in the middle of idents
        while self.peek().is_ascii_alphanumeric() || self.peek() == b'_' {
            self.advance();
        }
        self.push_token(PPTokenType::Ident, start, start_pos, gap);
    }

    fn lex_number(&mut self, start: usize, start_pos: Pos, gap: Gap) {
        loop {
            let c = self.peek();
            match c {
                b'e' | b'E' | b'p' | b'P' => {
                    self.advance();
                    if matches!(self.peek(), b'+' | b'-') {
                        self.advance();
                    }
                }
                b'.' | b'_' => {
                    self.advance();
                }
                _ if c.is_ascii_alphanumeric() => {
                    self.advance();
                }
                _ => break,
            }
        }
        self.push_token(PPTokenType::Number, start, start_pos, gap);
    }

    fn lex_str_lit(&mut self, start: usize, start_pos: Pos, gap: Gap) {
        while self.peek() != b'\"' && !self.reader.is_eof() {
            let c = self.peek();
            match c {
                b'\n' => {
                    self.errors.push(PreprocessorErr::new(
                        ErrorType::NotStringLiteral,
                        "Unterminated string literal",
                    ));
                    return;
                }
                b'\\' => {
                    self.advance();
                    if !self.reader.is_eof() {
                        self.advance();
                    }
                }
                _ => {
                    self.advance();
                }
            }
        }
        if self.reader.is_eof() {
            self.errors.push(PreprocessorErr::new(
                ErrorType::NotStringLiteral,
                "Unterminated string literal",
            ));
            return;
        }
        self.advance();
        self.push_token(PPTokenType::StringLiteral, start, start_pos, gap);
    }

    fn lex_chr_lit(&mut self, start: usize, start_pos: Pos, gap: Gap) {
        while self.peek() != b'\'' && !self.reader.is_eof() {
            let c = self.peek();
            match c {
                b'\n' => {
                    self.errors.push(PreprocessorErr::new(
                        ErrorType::NotCharLiteral,
                        "Unterminated char literal",
                    ));
                    self.push_token(PPTokenType::Other, start, start_pos, gap);
                    return;
                }
                b'\\' => {
                    self.advance();
                    if !self.reader.is_eof() {
                        self.advance();
                    }
                }
                _ => {
                    self.advance();
                }
            }
        }
        if self.reader.is_eof() {
            self.errors.push(PreprocessorErr::new(
                ErrorType::NotCharLiteral,
                "Unterminated char literal",
            ));
            self.push_token(PPTokenType::Other, start, start_pos, gap);
            return;
        }
        self.advance();
        self.push_token(PPTokenType::CharLiteral, start, start_pos, gap);
    }

    fn push_token(&mut self, kind: PPTokenType, start: usize, pos: Pos, gap: Gap) {
        let end = self.reader.offset();
        self.tokens.push(PPToken {
            kind,
            pos,
            range: start..end,
            gap,
        });
    }

    fn advance(&mut self) -> u8 {
        self.reader.advance()
    }
    fn peek(&self) -> u8 {
        self.reader.peek()
    }
    fn peek_at(&self, n: usize) -> u8 {
        self.reader.peek_at(n)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_lex_define() {
        let define = r"#define A /*\n*/ 1";
        let reader = Reader::new(define);
        let mut lexer = Lexer::new(reader);
        let tokens = lexer.lex();
        println!("tokens: {:#?}", tokens);
        assert_eq!(tokens.len(), 5);

        assert_eq!(tokens[0].kind, PPTokenType::Punc(Punct::Hash));
        assert_eq!(tokens[1].kind, PPTokenType::Ident);
        assert_eq!(tokens[2].kind, PPTokenType::Ident);
        assert_eq!(tokens[3].kind, PPTokenType::Number);
        assert_eq!(tokens[4].kind, PPTokenType::EOF);

        assert_eq!(tokens[1].range, 1..7);
        assert_eq!(tokens[3].range, 17..18);

        assert_eq!(lexer.reader.from_source(&tokens[3].range), "1");
    }

    #[test]
    fn splice_before_token_does_not_widen_range() {
        let r = Reader::new("a\\\n#b");
        let mut lx = Lexer::new(r);
        let toks = lx.lex();
        let hash = &toks[1];
        assert_eq!(hash.range, 3..4);
        assert_eq!(hash.pos.line, 2);
        assert_eq!(hash.pos.column, 1);
    }
}
