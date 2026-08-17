use std::ops::Range;

use crate::{
    errors::PreprocessorErr,
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
    Shl,     //
    Shr,     // >>
    Caret,   // ^
    Pipe,    // |

    // relational / equality
    Lt,   //
    Gt,   // >
    Le,   // <=
    Ge,   // >=
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
        let pos = self.reader.pos();
        let range = Range {
            start: self.reader.offset(),
            end: self.reader.size(),
        };
        self.push_token(PPToken {
            kind: PPTokenType::EOF,
            pos,
            range,
        });
        self.tokens.to_vec()
    }

    fn scan_tokens(&mut self) {
        let c = self.advance();
        match c {
            b'[' => self.push_token(PPTokenType::Punc(Punct::LBracket), 1),
            b']' => self.push_token(PPTokenType::Punc(Punct::RBracket), 1),
        }
    }
    fn lex_ident(&mut self) {}
    fn lex_number(&mut self) {}
    fn lex_str_lit(&mut self) {}
    fn lex_chr_lit(&mut self) {}
    fn lex_header(&mut self) {}
    fn lex_punc(&mut self) {}
    fn lex_other(&mut self) {}

    fn range(&self, token_length: usize) -> Range<usize> {
        let start = self.reader.offset();
        Range {
            start,
            end: start + token_length,
        }
    }
    fn pos(&self) -> Pos {
        self.reader.pos()
    }
    fn push_token(&mut self, kind: PPTokenType, token_length: usize) {
        let token = PPToken::new(kind, self.pos(), self.range(token_length));
        self.tokens.push(token);
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
