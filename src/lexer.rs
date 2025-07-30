use std::fmt::Display;

use tracing::error;

// maximal munch: When two
// lexical grammar rules can both match a chunk of code that the scanner is
// looking at, whichever one matches the most characters wins.
// That rule states that if we can match orchid as an identifier
// or as a keyword, then the former wins

pub fn report(line_number: usize, message: &str) {
    error!("line: {} --> Err: {} ", line_number, message);
}

#[derive(Debug, Clone)]
pub struct Scanner {
    chars: Vec<char>,
    source: String,
    tokens: Vec<Token>,
    // to keep track of position
    start: usize,
    current: usize,
    line: usize,
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
        }
    }

    /// advances through the source code and using the
    /// start and current state of the scanner, captures
    /// different tokens
    pub fn scan_tokens(&mut self) -> Vec<Token> {
        while self.current < self.source.len() {
            self.start = self.current;
            self.scan_token();
        }
        self.tokens.push(Token::new(
            TokenType::Eof,
            "".to_string(),
            "null".to_string(),
            self.line,
        ));
        self.tokens.to_vec()
    }

    /// advances through the source code and adds token
    /// char per char. Handles chars, arithmetics,
    /// identifiers and numbers
    pub fn scan_token(&mut self) {
        // let c = self.take_current_move_forward();
        // let token_type = TokenType::from(c);
        // self.add_token(token_type);
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
            '*' => self.add_token(TokenType::Star),
            '!' => {
                let token_type = if self.match_char('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
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
            ' ' | '\r' | '\t' => {}
            '\n' => {
                self.line += 1;
            }
            '"' => self.handle_string(),
            _ => {
                if c.is_ascii_digit() {
                    self.handle_number();
                } else if c.is_alphabetic() {
                    self.handle_identifier();
                } else {
                    report(self.line, "Unexpected Character");
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
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fun" => TokenType::Fun,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "var" => TokenType::Var,
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
            report(self.line, "Undetermined String");
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
        report(self.line, "undetermined comment block");
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
        // creating a Token object and pushing it to our scanner token vec
        self.tokens.push(Token::new(
            token_type.clone(),
            text.to_string(),
            String::from(token_type),
            self.line,
        ));
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    type_: TokenType,
    lexeme: String,
    literal: String,
    line: usize,
}

impl Token {
    pub fn new(type_: TokenType, lexeme: String, literal: String, line: usize) -> Self {
        Token {
            type_,
            lexeme,
            literal,
            line,
        }
    }
    // pub fn to_string(&self) -> String {
    //     return format!("{} {} {}", self.type_, self.lexeme, self.literal);
    // }
}

#[derive(Debug, Clone)]
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

    // one or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // literals
    Identifier(String),
    String(String),
    Number(f32),

    //keywords
    Void,
    And,
    Class,
    Else,
    False,
    Fun,
    If,
    For,
    Nil,
    Or,
    Print,
    Return,
    This,
    Super,
    True,
    Var,
    While,
    Eof,

    // err
    Error(String),
}

impl From<char> for TokenType {
    fn from(value: char) -> Self {
        match value {
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            '{' => TokenType::LeftBrace,
            '}' => TokenType::RightBrace,
            ',' => TokenType::Comma,
            '.' => TokenType::Dot,
            '-' => TokenType::Minus,
            '+' => TokenType::Plus,
            ';' => TokenType::Semicolon,
            '/' => TokenType::Slash,
            '*' => TokenType::Star,
            '!' => TokenType::Bang,
            '=' => TokenType::Equal,
            '>' => TokenType::Greater,
            '<' => TokenType::Less,
            //TODO: for now!
            _ => {
                report(0, &format!("Unexpected char {value}"));
                TokenType::Error(format!("Error: Invalid char => {value}"))
            }
        }
    }
}
impl From<TokenType> for String {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::LeftParen => "(".to_string(),
            TokenType::RightParen => ")".to_string(),
            TokenType::LeftBrace => "{".to_string(),
            TokenType::RightBrace => "}".to_string(),
            TokenType::Comma => ",".to_string(),
            TokenType::Dot => ".".to_string(),
            TokenType::Minus => "-".to_string(),
            TokenType::Plus => "+".to_string(),
            TokenType::Semicolon => ";".to_string(), // Fix: should be Semicolon
            TokenType::Slash => "/".to_string(),
            TokenType::Star => "*".to_string(),
            TokenType::Bang => "!".to_string(),
            TokenType::BangEqual => "!=".to_string(),
            TokenType::Equal => "=".to_string(),
            TokenType::EqualEqual => "==".to_string(),
            TokenType::GreaterEqual => ">=".to_string(),
            TokenType::Greater => ">".to_string(),
            TokenType::LessEqual => "<=".to_string(),
            TokenType::Less => "<".to_string(),
            TokenType::Identifier(text) => format!("Identifier: {text}"),
            TokenType::String(text) => format!("String: {text}"),
            TokenType::Number(text) => format!("Number: {text}"),
            TokenType::Void => "void".to_string(),
            TokenType::And => "and".to_string(),
            TokenType::Class => "class".to_string(),
            TokenType::Else => "else".to_string(),
            TokenType::False => "false".to_string(),
            TokenType::Fun => "function".to_string(),
            TokenType::If => "if".to_string(),
            TokenType::For => "for".to_string(),
            TokenType::Nil => "nil".to_string(),
            TokenType::Or => "or".to_string(),
            TokenType::Print => "print".to_string(),
            TokenType::Return => "return".to_string(),
            TokenType::This => "this".to_string(),
            TokenType::Super => "super".to_string(),
            TokenType::True => "true".to_string(),
            TokenType::Var => "var".to_string(),
            TokenType::While => "while".to_string(),
            TokenType::Eof => "eof".to_string(),
            TokenType::Error(text) => format!("Error: {text}"),
        }
    }
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
            Self::Void => write!(f, "void"),
            Self::And => write!(f, "and"),
            Self::Class => write!(f, "class"),
            Self::Else => write!(f, "else"),
            Self::False => write!(f, "false"),
            Self::Fun => write!(f, "function"),
            Self::If => write!(f, "if"),
            Self::For => write!(f, "for"),
            Self::Nil => write!(f, "Nil"),
            Self::Or => write!(f, "or"),
            Self::Print => write!(f, "print"),
            Self::Return => write!(f, "return"),
            Self::This => write!(f, "this"),
            Self::Super => write!(f, "super"),
            Self::True => write!(f, "true"),
            Self::Var => write!(f, "var"),
            Self::While => write!(f, "while"),
            Self::Eof => write!(f, "eof"),
            Self::Error(text) => write!(f, "Error: {text}"),
        }
    }
}
