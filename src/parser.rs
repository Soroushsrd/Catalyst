use tracing::info;

use crate::lexer::{Token, TokenType};

/// a program is the first node in an AST
/// it contains a single child which is the function
#[derive(Debug, Clone)]
pub struct Program {
    function_def: Function,
}

/// functions consist of a name (identifier) and a
/// body. identifiers represent function and variables names
/// they're basically strings but different from "strings".
#[derive(Debug, Clone)]
pub struct Function {
    name: Identifier,
    body: Statement,
    parameters: Vec<Parameter>,
    return_type: ReturnType,
}

/// represents variable names, function names,etc
#[derive(Debug, Clone)]
pub struct Identifier {
    name: String,
}

#[derive(Debug, Clone)]
pub enum ReturnType {
    Void,
    Int,
    //TODO:
}

#[derive(Debug, Clone)]
pub struct Parameter {
    parameter_type: ParameterType,
    name: Option<Identifier>,
}

//WARNING: Could be merged with ReturnType enum
#[derive(Debug, Clone)]
pub enum ParameterType {
    Void,
    Int,
    //TODO:
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(Vec<Statement>),
    Return(Option<Expression>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(String),
    Number(f32),
    //TODO:
}

#[derive(Debug, Clone)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            current: 0,
        }
    }

    /// returns a ref to the current token
    fn peak(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    /// checks to see if token vector is at its end,
    /// otherwise moves the pointer one token forward
    /// and returns the previous token
    fn advance(&mut self) -> Option<&Token> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    /// returns None if token vector is empty,
    /// otherwise returns the token at current-1 position
    fn previous(&self) -> Option<&Token> {
        if self.current > 0 {
            self.tokens.get(self.current - 1)
        } else {
            None
        }
    }

    /// retunrs true if the current token is EOF or
    /// the token list is empty, otherwise returns false
    fn is_at_end(&self) -> bool {
        if let Some(token) = self.peak() {
            matches!(token.token_type(), &TokenType::Eof)
        } else {
            true
        }
    }
}
