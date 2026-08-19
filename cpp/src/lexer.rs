use std::ops::Range;

use crate::{
    errors::{ErrorType, PreprocessorErr},
    reader::{Pos, Reader},
};

type PPResult<T> = Result<T, PreprocessorErr>;

#[derive(Clone)]
pub struct PPToken {
    pub kind: PPTokenType,
    pub pos: Pos,
    pub range: Range<usize>,
}

impl PPToken {
    pub fn new(kind: PPTokenType, pos: Pos, range: Range<usize>) -> Self {
        Self { kind, pos, range }
    }
}

#[derive(Clone)]
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

    /// a high level parser. should call underlying parsing mechanisms
    /// TODO: need one parser per PPTokenType
    pub fn lex(&mut self) -> Vec<PPToken> {
        while !self.reader.is_eof() {
            self.scan_tokens();
        }
        let start_pos = self.pos();
        let start_offset = self.reader.offset();
        self.push_token(PPTokenType::EOF, start_offset, start_pos);
        std::mem::take(&mut self.tokens)
    }

    fn scan_tokens(&mut self) {
        let start_pos = self.pos();
        let start_offset = self.reader.offset();
        let c = self.advance();
        match c {
            b'[' => self.push_token(PPTokenType::Punc(Punct::LBracket), start_offset, start_pos),
            b']' => self.push_token(PPTokenType::Punc(Punct::RBracket), start_offset, start_pos),
            b'(' => self.push_token(PPTokenType::Punc(Punct::LParen), start_offset, start_pos),
            b')' => self.push_token(PPTokenType::Punc(Punct::RParen), start_offset, start_pos),
            b'{' => self.push_token(PPTokenType::Punc(Punct::LBrace), start_offset, start_pos),
            b'}' => self.push_token(PPTokenType::Punc(Punct::RBrace), start_offset, start_pos),
            b'.' => {
                if self.peek() == b'.' && self.peek_at(1) == b'.' {
                    self.advance();
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::Ellipsis), start_offset, start_pos);
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Dot), start_offset, start_pos);
                }
            }
            b'*' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::StarEq), start_offset, start_pos);
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Star), start_offset, start_pos);
                }
            }
            b'+' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::PlusEq), start_offset, start_pos);
                } else if self.peek() == b'+' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::PlusPlus), start_offset, start_pos);
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Plus), start_offset, start_pos);
                }
            }
            b'-' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::MinusEq), start_offset, start_pos);
                } else if self.peek() == b'-' {
                    self.advance();
                    self.push_token(
                        PPTokenType::Punc(Punct::MinusMinus),
                        start_offset,
                        start_pos,
                    );
                } else if self.peek() == b'>' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::Arrow), start_offset, start_pos);
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Minus), start_offset, start_pos);
                }
            }
            b'&' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::AmpEq), start_offset, start_pos);
                } else if self.peek() == b'&' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::AmpAmp), start_offset, start_pos);
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Amp), start_offset, start_pos);
                }
            }
            b'~' => self.push_token(PPTokenType::Punc(Punct::Tilde), start_offset, start_pos),
            b'!' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::Ne), start_offset, start_pos);
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Bang), start_offset, start_pos);
                }
            }
            b'/' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::SlashEq), start_offset, start_pos);
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Slash), start_offset, start_pos);
                }
            }
            b'%' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::PercentEq), start_offset, start_pos);
                } else if self.peek() == b':' {
                    self.advance();
                    if self.peek() == b'%' && self.peek_at(1) == b':' {
                        self.advance();
                        self.advance();
                        self.push_token(
                            PPTokenType::Punc(Punct::HashHash),
                            start_offset,
                            start_pos,
                        );
                    } else {
                        self.push_token(PPTokenType::Punc(Punct::Hash), start_offset, start_pos);
                    }
                } else if self.peek() == b'>' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::RBrace), start_offset, start_pos);
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Percent), start_offset, start_pos);
                }
            }
            b'<' => {
                if self.peek() == b'<' {
                    self.advance();
                    if self.peek() == b'=' {
                        self.advance();
                        self.push_token(PPTokenType::Punc(Punct::ShlEq), start_offset, start_pos);
                    } else {
                        self.push_token(PPTokenType::Punc(Punct::Shl), start_offset, start_pos);
                    }
                } else if self.peek() == b':' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::LBracket), start_offset, start_pos);
                } else if self.peek() == b'%' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::LBrace), start_offset, start_pos);
                } else if self.peek() == b'=' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::Lte), start_offset, start_pos);
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Lt), start_offset, start_pos);
                }
            }
            b'>' => {
                if self.peek() == b'>' {
                    self.advance();
                    if self.peek() == b'=' {
                        self.advance();
                        self.push_token(PPTokenType::Punc(Punct::ShrEq), start_offset, start_pos);
                    } else {
                        self.push_token(PPTokenType::Punc(Punct::Shr), start_offset, start_pos);
                    }
                } else if self.peek() == b'=' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::Gte), start_offset, start_pos);
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Gt), start_offset, start_pos);
                }
            }
            b'^' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::CaretEq), start_offset, start_pos);
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Caret), start_offset, start_pos);
                }
            }
            b'|' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::PipeEq), start_offset, start_pos);
                } else if self.peek() == b'|' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::PipePipe), start_offset, start_pos);
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Pipe), start_offset, start_pos);
                }
            }
            b'=' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::EqEq), start_offset, start_pos);
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Eq), start_offset, start_pos);
                }
            }
            b'?' => self.push_token(PPTokenType::Punc(Punct::Question), start_offset, start_pos),
            b':' => {
                if self.peek() == b'>' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::RBracket), start_offset, start_pos);
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Colon), start_offset, start_pos);
                }
            }
            b';' => self.push_token(PPTokenType::Punc(Punct::Semi), start_offset, start_pos),
            b',' => self.push_token(PPTokenType::Punc(Punct::Comma), start_offset, start_pos),
            b'#' => {
                if self.peek() == b'#' {
                    self.advance();
                    self.push_token(PPTokenType::Punc(Punct::HashHash), start_offset, start_pos);
                } else {
                    self.push_token(PPTokenType::Punc(Punct::Hash), start_offset, start_pos);
                }
            }
            b'\'' => self.lex_chr_lit(start_offset, start_pos),
            b'\"' => self.lex_str_lit(start_offset, start_pos),
            _ => {
                // if it starts with a '_' or alphabetic token
                // use lex_ident()

                // if it starts with a number
                // use lex_number()

                // if it starts with include,
                // use lex_header()

                // otherwise its 'other'
            }
        }
    }

    fn lex_ident(&mut self) {}
    fn lex_number(&mut self) {}
    fn lex_str_lit(&mut self, start: usize, start_pos: Pos) {
        // "
        self.advance();
        while self.peek() != b'\"' && !self.reader.is_eof() {
            self.advance();
        }
        if self.reader.is_eof() {
            self.errors.push(PreprocessorErr::new(
                ErrorType::SyntaxError,
                "Unterminated string literal",
            ));
            return;
        }
        self.advance();
        self.push_token(PPTokenType::StringLiteral, start, start_pos);
    }
    fn lex_chr_lit(&mut self, start: usize, start_pos: Pos) {
        // '
        self.advance();
        while self.peek() != b'\'' && !self.reader.is_eof() {
            self.advance();
        }
        if self.reader.is_eof() {
            self.errors.push(PreprocessorErr::new(
                ErrorType::SyntaxError,
                "Unterminated char literal",
            ));
            return;
        }
        self.advance();
        self.push_token(PPTokenType::CharLiteral, start, start_pos);
    }
    fn lex_header(&mut self) {}
    fn lex_other(&mut self) {}

    fn pos(&self) -> Pos {
        self.reader.pos()
    }
    fn push_token(&mut self, kind: PPTokenType, start: usize, pos: Pos) {
        let end = self.reader.offset();
        self.tokens.push(PPToken {
            kind,
            pos,
            range: start..end,
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
