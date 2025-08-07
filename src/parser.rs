use crate::lexer::{Token, TokenType};

/// a program is the first node in an AST
/// it contains a single child which is the function
#[derive(Debug, Clone)]
pub struct Program {
    pub function_def: Function,
}

/// functions consist of a name (identifier) and a
/// body. identifiers represent function and variables names
/// they're basically strings but different from "strings".
#[derive(Debug, Clone)]
pub struct Function {
    pub name: Identifier,
    pub body: Statement,
    pub parameters: Vec<Parameter>,
    #[allow(dead_code)]
    pub return_type: ReturnType,
}

/// represents variable names, function names,etc
#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, Clone)]
pub enum ReturnType {
    Void,
    Int,
    //TODO:
}

#[derive(Debug, Clone)]
pub struct Parameter {
    #[allow(dead_code)]
    parameter_type: ParameterType,
    pub name: Option<Identifier>,
}

//WARNING: Could be merged with ReturnType enum
#[derive(Debug, Clone)]
pub enum ParameterType {
    Void,
    #[allow(dead_code)]
    Int,
    //TODO:
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(Vec<Statement>),
    Return(Option<Expression>),
    //TODO:
}

#[derive(Debug, Clone, Default)]
pub enum Expression {
    #[default]
    Unknown,
    Identifier(String),
    Number(f32),
    BitwiseNot(Box<Expression>),
    UnaryMinus(Box<Expression>),
    LogicalNot(Box<Expression>),
    Binary {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    //TODO:
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equals,
    NotEquals,
    And,
    Or,
}
#[derive(Debug, Clone)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }
    pub fn parse(&mut self) -> Result<Program, String> {
        let function = self.parse_function()?;
        Ok(Program {
            function_def: function,
        })
    }
    /// considering <return type> <function name>(<params>) {<statements>}
    pub fn parse_function(&mut self) -> Result<Function, String> {
        let return_type = self.parse_type()?;

        let name = self.parse_identifiers()?;

        self.consume_type(&TokenType::LeftParen, "Expected '(' after function name")?;
        let parameters = self.parse_parameters()?;
        self.consume_type(&TokenType::RightParen, "Expected ')' after parameters")?;

        let body = self.parse_block_statement()?;

        Ok(Function {
            name,
            body,
            parameters,
            return_type,
        })
    }
    fn parse_type(&mut self) -> Result<ReturnType, String> {
        let token = self
            .peek()
            .ok_or("unexpected end of file. Expected identifier")?;
        let type_ = match token.token_type() {
            TokenType::Int => Ok(ReturnType::Int),
            TokenType::Void => Ok(ReturnType::Void),
            _ => Err("Expected type (int or void)".to_string()),
        };

        self.advance();
        type_
    }
    fn parse_identifiers(&mut self) -> Result<Identifier, String> {
        let ident = self
            .peek()
            .ok_or("unexpected end of file. Expected identifier")?;
        let expression = match ident.token_type() {
            TokenType::Identifier(name) => Ok(Identifier { name: name.clone() }),
            _ => Err("Expected identifier".to_string()),
        };
        self.advance();
        expression
    }
    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, String> {
        let mut parameters = Vec::with_capacity(5);

        if let Some(token) = self.peek() {
            match *token.token_type() {
                TokenType::RightParen => {
                    return Ok(parameters);
                }
                TokenType::Void => {
                    self.advance();
                    parameters.push(Parameter {
                        parameter_type: ParameterType::Void,
                        name: None,
                    });
                    return Ok(parameters);
                }
                _ => {
                    todo!()
                }
            }
        }
        Ok(parameters)
    }
    fn parse_block_statement(&mut self) -> Result<Statement, String> {
        self.consume_type(&TokenType::LeftBrace, "Expected '{'")?;
        let mut statements = Vec::with_capacity(5);
        while !self.check_token_type(&TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.parse_statement()?);
        }
        self.consume_type(&TokenType::RightBrace, "Expected '}'")?;
        Ok(Statement::Block(statements))
    }
    fn parse_statement(&mut self) -> Result<Statement, String> {
        if self.check_token_type(&TokenType::Return) {
            self.parse_return_statement()
        } else {
            Err("unknown statement".to_string())
        }
    }
    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        self.consume_type(&TokenType::Return, "Expected 'return'")?;

        let expr = if self.check_token_type(&TokenType::Semicolon) {
            None
        } else {
            Some(self.parse_expression()?)
        };

        self.consume_type(
            &TokenType::Semicolon,
            "Expected ';' after the return statement",
        )?;
        Ok(Statement::Return(expr))
    }

    fn parse_expression(&mut self) -> Result<Expression, String> {
        self.parse_binary_expression(0)
    }
    fn parse_binary_expression(&mut self, min_precedence: u8) -> Result<Expression, String> {
        let mut left = self.parse_unary_expression()?;

        while let Some(token) = self.peek() {
            let operator = match token.token_type() {
                TokenType::Plus => BinaryOperator::Add,
                TokenType::Minus => BinaryOperator::Subtract,
                TokenType::Star => BinaryOperator::Multiply,
                TokenType::Slash => BinaryOperator::Divide,
                TokenType::And => BinaryOperator::And,
                TokenType::Or => BinaryOperator::Or,
                TokenType::BangEqual => BinaryOperator::NotEquals,
                TokenType::EqualEqual => BinaryOperator::Equals,
                _ => break,
            };

            let precendence = self.get_precendece(&operator);
            if precendence < min_precedence {
                break;
            }
            self.advance();

            let right = self.parse_binary_expression(precendence + 1)?;
            left = Expression::Binary {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
        }
        Ok(left)
    }
    /// parses unary expressions like ~, !, -
    fn parse_unary_expression(&mut self) -> Result<Expression, String> {
        let token = self
            .peek()
            .ok_or("unexpected end of line, expected operation.")?
            .clone();

        match token.token_type() {
            TokenType::Minus => {
                self.advance();
                let expr = self.parse_unary_expression()?;
                Ok(Expression::UnaryMinus(Box::new(expr)))
            }
            TokenType::Bang => {
                self.advance();
                let expr = self.parse_unary_expression()?;
                Ok(Expression::LogicalNot(Box::new(expr)))
            }
            TokenType::BitwiseNot => {
                self.advance();
                let expr = self.parse_unary_expression()?;
                Ok(Expression::BitwiseNot(Box::new(expr)))
            }
            _ => self.parse_primary_expression(),
        }
    }
    /// based on token type defined in lexer module, parses the primary tokens
    /// then matches it agains expressions and advances one token forward
    fn parse_primary_expression(&mut self) -> Result<Expression, String> {
        let token = self
            .advance()
            .ok_or("unexpected end of line, expected primary operation.")?;

        let expression = match token.token_type() {
            TokenType::Number(value) => Expression::Number(*value),
            TokenType::Identifier(name) => Expression::Identifier(name.clone()),
            TokenType::LeftParen => {
                let expr = self.parse_expression()?;
                self.consume_type(&TokenType::RightParen, "Expected ')' after expression")?;
                expr
            }
            _ => return Err(format!("Expected expression:{token:?}")),
        };

        Ok(expression)
    }

    fn get_precendece(&self, operator: &BinaryOperator) -> u8 {
        match operator {
            BinaryOperator::Or => 1,
            BinaryOperator::And => 2,
            BinaryOperator::Equals => 3,
            BinaryOperator::NotEquals => 4,
            BinaryOperator::Add | BinaryOperator::Subtract => 5,
            BinaryOperator::Multiply | BinaryOperator::Divide => 6,
        }
    }
    /// returns a ref to the current token
    fn peek(&self) -> Option<&Token> {
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
        if let Some(token) = self.peek() {
            matches!(token.token_type(), &TokenType::Eof)
        } else {
            true
        }
    }

    fn check_token_type(&self, token_type: &TokenType) -> bool {
        if let Some(token) = self.peek() {
            token.token_type() == token_type
        } else {
            false
        }
    }
    fn consume_type(&mut self, token_type: &TokenType, error_msg: &str) -> Result<&Token, String> {
        if self.check_token_type(token_type) {
            Ok(self.advance().unwrap())
        } else {
            Err(error_msg.to_string())
        }
    }
}
