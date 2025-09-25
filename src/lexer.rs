use crate::{
    errors::{CompilerError, ErrorType},
    parser::Types,
};
use std::fmt::Display;

// maximal munch: When two
// lexical grammar rules can both match a chunk of code that the scanner is
// looking at, whichever one matches the most characters wins.
// That rule states that if we can match orchid as an identifier
// or as a keyword, then the former wins

#[derive(Debug, Clone)]
pub struct Scanner {
    /// contains the source code chars
    chars: Vec<char>,
    /// to keep track of error locations(lines)
    source: String,
    /// generated tokens
    tokens: Vec<Token>,
    // to keep track of position
    start: usize,
    current: usize,
    line: usize,
    column: usize,
    start_column: usize,
    /// keeps track of errors generated while scanning
    errors: Vec<CompilerError>,
}

impl Scanner {
    /// Creates a new scanner using the source code (str)
    pub fn new(source: &str) -> Self {
        Self {
            chars: source.chars().collect(),
            source: source.to_string(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            column: 1,
            start_column: 1,
            errors: Vec::with_capacity(10),
        }
    }

    /// advances through the source code and using the
    /// start and current state of the scanner, captures
    /// different tokens
    pub fn scan_tokens(&mut self) -> Vec<Token> {
        while self.current < self.source.len() {
            self.start = self.current;
            self.start_column = self.column;
            self.scan_token();
        }
        self.tokens.push(Token::new(
            TokenType::Eof,
            "".to_string(),
            "null".to_string(),
            self.line,
            self.column,
        ));
        self.tokens.to_vec()
    }

    /// advances through the source code and adds token
    /// char per char. Handles chars, arithmetics,
    /// identifiers and numbers
    pub fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            // Single-character tokens
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            ':' => self.add_token(TokenType::Colon),
            '?' => self.add_token(TokenType::QMark),
            //TODO: handle pointers using the variation below(star). this should be able to handle
            // both a multipication and a pointer
            '*' => self.add_token(TokenType::Star),
            '!' => {
                let token_type = if self.match_char('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                self.add_token(token_type);
            }
            // TODO: pointers as references should also be handled at this point
            // imagine something like a = &b;
            '&' => {
                let token_type = if self.match_char('&') {
                    TokenType::And
                } else {
                    self.add_error(
                        ErrorType::UnexpectedToken,
                        &format!("Unexpected character {c}"),
                    );
                    TokenType::Error(format!("Unexpected character: {c}"))
                };
                self.add_token(token_type);
            }
            '|' => {
                let token_type = if self.match_char('|') {
                    TokenType::Or
                } else {
                    self.add_error(
                        ErrorType::UnexpectedToken,
                        &format!("Unexpected character {c}"),
                    );

                    TokenType::Error(format!("Unexpected character: {c}"))
                };
                self.add_token(token_type);
            }

            '=' => {
                let token_type = if self.match_char('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                self.add_token(token_type);
            }
            '<' => {
                let token_type = if self.match_char('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.add_token(token_type);
            }
            '>' => {
                let token_type = if self.match_char('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.add_token(token_type);
            }
            '/' => {
                if self.match_char('/') {
                    // single line comment. it will go on until \n
                    while self.peek() != '\n' && !self.current >= self.chars.len() {
                        self.advance();
                    }
                } else if self.match_char('*') {
                    self.handle_block_comment();
                } else {
                    // this is a simple division /
                    self.add_token(TokenType::Slash)
                };
            }
            ' ' => {}
            '\t' => self.column += 3,
            '\r' => {}
            '\n' => {
                self.line += 1;
                self.column = 1;
            }
            '"' => self.handle_string(),
            '~' => self.add_token(TokenType::BitwiseNot),
            _ => {
                if c.is_ascii_digit() {
                    self.handle_number();
                } else if c.is_alphabetic() {
                    self.handle_identifier();
                } else {
                    self.add_error(
                        ErrorType::UnexpectedToken,
                        &format!("Unexpected character {c}"),
                    );
                    self.add_token(TokenType::Error(format!("Unexpected character: {c}")));
                }
            }
        }
    }

    /// used to parse out identifiers
    /// maximum munch is considered.
    /// will move forward untill there are
    /// no more alphanumerics or hits a "_"
    fn handle_identifier(&mut self) {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }
        let text = self.chars[self.start..self.current]
            .iter()
            .collect::<String>();
        self.add_token(self.get_keyword_or_identifier(&text));
    }

    /// based on the input str, returns a TokenType
    fn get_keyword_or_identifier(&self, text: &str) -> TokenType {
        match text {
            "void" => TokenType::Void,
            "int" => TokenType::Int,
            "char" => TokenType::Char,
            "long" => TokenType::Long,
            "double" => TokenType::Double,
            "float" => TokenType::Float,
            //TODO: Add pointer here
            // "&" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            // "|" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "while" => TokenType::While,
            _ => TokenType::Identifier(text.to_string()),
        }
    }

    /// starts when a digit has been captured
    /// moves forward until it reaches a dot
    /// then checks if there are more digits
    /// after the dot, otherwise breaks
    /// then parses the captured string to f32
    fn handle_number(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }
        let text: String = self.chars[self.start..self.current].iter().collect();
        self.add_token(TokenType::Number(text.parse::<f32>().unwrap_or(0.0)));
    }
    /// starts at " and advances until either reaches
    /// the end of buffer or the ending ". Captures everything
    /// in between and adds them as a token.
    fn handle_string(&mut self) {
        while self.peek() != '"' && !self.current >= self.chars.len() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }
        if self.current >= self.chars.len() {
            self.add_error(ErrorType::SyntaxError, "Unterminated string");
            return;
        }
        self.advance();
        let text = &self.chars[self.start + 1..self.current - 1]
            .iter()
            .collect::<String>();
        self.add_token(TokenType::String(text.to_string()));
    }

    /// starts at * after / of the comment block
    /// consumes tokens without saving them
    fn handle_block_comment(&mut self) {
        while !self.current >= self.chars.len() {
            if self.peek() == '*' && self.peek_next() == '/' {
                self.advance(); // for *
                self.advance(); // then for /
                return;
            }
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }
        self.add_error(ErrorType::SyntaxError, "Unterminated comment block");
    }
    /// matches the expected char agains the current char
    /// in the buffer and moves one step forward to account
    /// for the char that comes after the current one
    fn match_char(&mut self, expected: char) -> bool {
        if self.current >= self.chars.len() {
            return false;
        }
        if self.chars[self.current] != expected {
            return false;
        }
        self.current += 1;
        true
    }

    /// Checks if buffer is at its end
    /// otherwise returns the current char
    /// and moves the cursor one char forward
    fn advance(&mut self) -> char {
        if self.current >= self.chars.len() {
            return '\0';
        }
        let c = self.chars[self.current];
        self.current += 1;
        self.column += 1;
        c
    }

    /// peaks and gets the current char
    /// it doesnt modify the current loc
    fn peek(&self) -> char {
        if self.current >= self.chars.len() {
            return '\0';
        }
        self.chars[self.current]
    }

    /// peaks and gets the next char
    /// it doesnt modify the current loc
    fn peek_next(&self) -> char {
        if self.current + 1 >= self.chars.len() {
            return '\0';
        }
        self.chars[self.current + 1]
    }

    /// creates a token based on token type and adds it to
    /// token store
    fn add_token(&mut self, token_type: TokenType) {
        // find the text the token is located at
        let text: String = self.chars[self.start..self.current].iter().collect();
        // start col
        // creating a Token object and pushing it to our scanner token vec
        self.tokens.push(Token::new(
            token_type.clone(),
            text.to_string(),
            token_type.to_string(),
            self.line,
            self.start_column,
        ));
    }
    fn add_error(&mut self, error_type: ErrorType, message: &str) {
        let source_line = self.get_current_line();
        let error = CompilerError::new(error_type, self.line, self.column, message)
            .with_source_line(source_line);
        self.errors.push(error);
    }
    fn get_current_line(&self) -> &str {
        let lines: Vec<&str> = self.source.lines().collect();
        if self.line > 0 && self.line <= lines.len() {
            return lines[self.line - 1];
        }
        ""
    }
    pub fn get_errors(&self) -> &[CompilerError] {
        &self.errors
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub type_: TokenType,
    lexeme: String,
    literal: String,
    line: usize,
    column: usize,
}

impl Token {
    pub fn new(
        type_: TokenType,
        lexeme: String,
        literal: String,
        line: usize,
        column: usize,
    ) -> Self {
        Token {
            type_,
            lexeme,
            literal,
            line,
            column,
        }
    }
    pub fn token_type(&self) -> &TokenType {
        &self.type_
    }
    pub fn line(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // single character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    QMark,
    Colon,

    // one or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    BitwiseNot,

    // literals
    Identifier(String),
    String(String),
    Number(f32),
    Pointer(Box<Types>),
    //keywords
    Void,
    Int,
    Long,
    Double,
    Float,
    Char,
    And,
    Class,
    Else,
    False,
    If,
    For,
    Nil,
    Or,
    Print,
    Return,
    This,
    True,
    While,
    Do,
    Eof,

    // err
    Error(String),
}
impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LeftParen => write!(f, "("),
            Self::RightParen => write!(f, ")"),
            Self::LeftBrace => write!(f, "'{{'"),
            Self::RightBrace => write!(f, "}}"),
            Self::Comma => write!(f, ","),
            Self::Dot => write!(f, "."),
            Self::Minus => write!(f, "-"),
            Self::Plus => write!(f, "+"),
            Self::Semicolon => write!(f, ";"),
            Self::Slash => write!(f, "/"),
            Self::Star => write!(f, "*"),
            Self::Colon => write!(f, ":"),
            Self::QMark => write!(f, "?"),
            Self::Bang => write!(f, "!"),
            Self::BangEqual => write!(f, "!="),
            Self::Equal => write!(f, "="),
            Self::EqualEqual => write!(f, "=="),
            Self::Greater => write!(f, ">"),
            Self::GreaterEqual => write!(f, ">="),
            Self::Less => write!(f, "<"),
            Self::LessEqual => write!(f, "<="),
            Self::Identifier(text) => write!(f, "Identifier: {text}"),
            Self::String(text) => write!(f, "String: {text}"),
            Self::Number(text) => write!(f, "Number: {text}"),
            Self::Pointer(value) => write!(f, "Pointer: {value:?}"),
            Self::BitwiseNot => write!(f, "~"),
            Self::Void => write!(f, "void"),
            Self::Int => write!(f, "int"),
            Self::Char => write!(f, "char"),
            Self::Long => write!(f, "long"),
            Self::Double => write!(f, "double"),
            Self::Float => write!(f, "float"),
            Self::And => write!(f, "&"),
            Self::Class => write!(f, "class"),
            Self::Else => write!(f, "else"),
            Self::False => write!(f, "false"),
            Self::If => write!(f, "if"),
            Self::For => write!(f, "for"),
            Self::Nil => write!(f, "Nil"),
            Self::Or => write!(f, "|"),
            Self::Print => write!(f, "print"),
            Self::Return => write!(f, "return"),
            Self::This => write!(f, "this"),
            Self::True => write!(f, "true"),
            Self::While => write!(f, "while"),
            Self::Do => write!(f, "do"),
            Self::Eof => write!(f, "eof"),
            Self::Error(text) => write!(f, "Error: {text}"),
        }
    }
}
