/**
* For future refs===> Why i used Refcell in parser implementations
*
* RefCell was used for interior mutability to solve the borrow checker issues
* that arise in recursive-descent parsers. Here's why:
*
* 1. Recursive Nature: A recursive-descent parser makes nested method calls that need to
*    mutate parser state (such as advancing position). this creates overlapping mutable borrows.
*
* 2. RefCell Solution: RefCell allows interior mutability by moving the borrow checking from
*    compile-time to runtime. This enables us to:
*    - Keep a clean recursive API with methods that don't need to take &mut self
*    - Safely modify internal state through borrow() and borrow_mut()
*    - Avoid restructuring the entire parser architecture
*
* 3. Trade-offs: We gain simpler code at the cost of potential runtime panics if we
*    violate borrowing rules. We mitigate this by carefully structuring our method calls
*    and using local variables to break up overlapping borrows.

* TODO: implement alternative methods for benchmarking purposes
* Alternative approaches would include separating mutable state into a separate structure
* or implementing a non-recursive algorithm for expression parsing.
*/
use crate::{
    errors::{CompilerError, ErrorType},
    expect_token,
    lexer::{Token, TokenType},
};
use std::cell::RefCell;

type ParseResult<T> = Result<T, CompilerError>;
/// a program is the first node in an AST
/// it contains a single child which is the function
#[derive(Debug, Clone)]
pub struct Program {
    pub function_def: Vec<Function>,
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

    //WARNING: not implemented yet
    Long,
    Char,
    Float,
    Double,
    Pointer(Box<ReturnType>),
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
    //WARNING: not implemented yet
    Long,
    Char,
    Float,
    Double,
    //TODO:
}

/// performs an action but doesnt return a value
#[derive(Debug, Clone)]
pub enum Statement {
    Block(Vec<Statement>),
    Return(Option<Expression>),
    VarDeclaration {
        var_type: ReturnType,
        name: Identifier,
        initializer: Option<Expression>,
    },
    Expression(Expression),
    If {
        condition: Box<Expression>,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    }, //TODO:
}

/// evaluates to a value and can be used as part of other expressions
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
    Assignment {
        target: String,
        value: Box<Expression>,
    },
    TernaryOP {
        condition: Box<Expression>,
        true_expr: Box<Expression>,
        false_expr: Box<Expression>,
    },
    FunctionCall {
        name: String,
        // using expression instead of parameter struct
        // because we are passing some sort of expression to
        // a function when we call it
        arguments: Vec<Expression>,
    }, //TODO:
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equals,
    NotEquals,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    And,
    Or,
}
#[derive(Debug, Clone)]
pub struct Parser {
    tokens: Vec<Token>,
    current: RefCell<usize>,
    errors: RefCell<Vec<CompilerError>>,
    source: String,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, source: &str) -> Self {
        Self {
            tokens,
            current: RefCell::new(0),
            errors: RefCell::new(Vec::with_capacity(10)),
            source: source.to_string(),
        }
    }
    pub fn parse(&mut self) -> Result<Program, Vec<CompilerError>> {
        let mut functions = Vec::with_capacity(5);

        while !self.is_at_end() {
            match self.parse_function() {
                Ok(function) => functions.push(function),
                Err(e) => self.errors.borrow_mut().push(e),
            }
        }

        if self.errors.borrow().is_empty() {
            if !functions.iter().any(|f| f.name.name == "main") {
                self.errors.borrow_mut().push(CompilerError::new(
                    ErrorType::SemanticError,
                    1,
                    1,
                    "No main function found",
                ));
                return Err(self.errors.borrow().clone());
            }
            Ok(Program {
                function_def: functions,
            })
        } else {
            Err(self.errors.borrow().clone())
        }
    }
    /// considering <return type> <function name>(<params>) {<statements>}
    pub fn parse_function(&mut self) -> ParseResult<Function> {
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
    /// used to parse function return type
    fn parse_type(&mut self) -> ParseResult<ReturnType> {
        let token = expect_token!(self, ErrorType::UnexpectedToken, "Unexpected end of file");
        let type_ = match token.token_type() {
            TokenType::Int => Ok(ReturnType::Int),
            TokenType::Void => Ok(ReturnType::Void),
            TokenType::Char => Ok(ReturnType::Char),
            TokenType::Long => Ok(ReturnType::Long),
            TokenType::Float => Ok(ReturnType::Float),
            TokenType::Double => Ok(ReturnType::Double),
            _ => Err(self.error(ErrorType::TypeError, "Expected type (int or void)")),
        };

        self.advance();
        type_
    }
    /// parses identifiers. These identifiers could be variable names,
    /// method names, and so on!
    fn parse_identifiers(&self) -> ParseResult<Identifier> {
        let ident = expect_token!(self, ErrorType::UnexpectedToken, "Unexpected end of file");
        let expression = match ident.token_type() {
            TokenType::Identifier(name) => Ok(Identifier { name: name.clone() }),
            _ => Err(self.error(ErrorType::SyntaxError, "Expected identifier")),
        };
        self.advance();
        expression
    }
    /// parses function parameters
    /// should be able to handle 6 parameters per method
    fn parse_parameters(&self) -> ParseResult<Vec<Parameter>> {
        let mut parameters = Vec::with_capacity(6);

        if let Some(token) = self.peek() {
            match *token.token_type() {
                TokenType::RightParen => {
                    return Ok(parameters);
                }
                TokenType::Void => {
                    self.advance();
                    if self.check_token_type(&TokenType::RightParen) {
                        parameters.push(Parameter {
                            parameter_type: ParameterType::Void,
                            name: None,
                        });
                        return Ok(parameters);
                    }
                    // in case void is used a type for some identifier
                    let name = if self.check_token_type(&TokenType::Identifier("".to_string())) {
                        Some(self.parse_identifiers()?)
                    } else {
                        None
                    };

                    parameters.push(Parameter {
                        parameter_type: ParameterType::Void,
                        name,
                    });
                }
                _ => {
                    let param_type = self.parse_parameter_type()?;
                    let name = if self.is_identifier() {
                        Some(self.parse_identifiers()?)
                    } else {
                        None
                    };

                    parameters.push(Parameter {
                        parameter_type: param_type,
                        name,
                    });

                    while self.check_token_type(&TokenType::Comma) {
                        self.advance();

                        if parameters.len() >= 6 {
                            return Err(self.error(ErrorType::SemanticError, "Too many parameters"));
                        }
                        let param_type = self.parse_parameter_type()?;
                        let name = if self.is_identifier() {
                            Some(self.parse_identifiers()?)
                        } else {
                            None
                        };

                        parameters.push(Parameter {
                            parameter_type: param_type,
                            name,
                        });
                    }
                    //TODO:
                    // loop {
                    //     let param_type = self.parse_parameter_type()?;
                    //     let name = if matches!(token.token_type(), TokenType::Identifier(_)) {
                    //         Some(self.parse_identifiers()?)
                    //     } else {
                    //         None
                    //     };
                    //     parameters.push(Parameter {
                    //         parameter_type: param_type,
                    //         name,
                    //     });
                    //     if !self.check_token_type(&TokenType::Comma) {
                    //         break;
                    //     }
                    //     self.advance();
                    //     if parameters.len() > 6 {
                    //         return Err(self.error(
                    //             ErrorType::SemanticError,
                    //             "Too many parameters (maximum 6 supported",
                    //         ));
                    //     }
                    // }
                }
            }
        }
        Ok(parameters)
    }

    fn parse_parameter_type(&self) -> ParseResult<ParameterType> {
        let token = expect_token!(self, ErrorType::UnexpectedToken, "Unexpected end of file");
        let param_type = match token.type_ {
            TokenType::Int => ParameterType::Int,
            TokenType::Void => ParameterType::Void,
            TokenType::Char => ParameterType::Char,
            TokenType::Long => ParameterType::Long,
            TokenType::Float => ParameterType::Float,
            TokenType::Double => ParameterType::Double,
            _ => return Err(self.error(ErrorType::TypeError, "Expected parameter type")),
        };
        self.advance();
        Ok(param_type)
    }

    /// block statements contain what ever comes after function declaration
    /// and is inside its braces
    /// TODO: Closures
    fn parse_block_statement(&mut self) -> ParseResult<Statement> {
        self.consume_type(&TokenType::LeftBrace, "Expected '{'")?;
        let mut statements = Vec::with_capacity(5);
        while !self.check_token_type(&TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.parse_statement()?);
        }
        self.consume_type(&TokenType::RightBrace, "Expected '}'")?;
        Ok(Statement::Block(statements))
    }

    /// parses different statements
    fn parse_statement(&mut self) -> ParseResult<Statement> {
        if self.check_token_type(&TokenType::Return) {
            self.parse_return_statement()
        } else if self
            .peek()
            .is_some_and(|t| self.is_parameter_type(t.token_type()))
        {
            self.parse_var_declaration()
        } else if self.check_token_type(&TokenType::If) {
            self.parse_if_flow()
        } else if self.check_token_type(&TokenType::LeftBrace) {
            self.parse_block_statement()
        } else {
            let expr = self.parse_expression()?;
            self.consume_type(&TokenType::Semicolon, "expected a semicolon")?;
            Ok(Statement::Expression(expr))
        }
    }

    fn parse_if_flow(&mut self) -> ParseResult<Statement> {
        self.consume_type(&TokenType::If, "Expected an if statement")?;
        if self.check_token_type(&TokenType::LeftParen) {
            self.consume_type(
                &TokenType::LeftParen,
                "Condition should start with a parenthesis",
            )?;

            let condition = self.parse_expression()?;

            self.consume_type(
                &TokenType::RightParen,
                "Condition should end with a parenthesis",
            )?;

            let then_branch = self.parse_statement()?;

            let else_branch = if self.check_token_type(&TokenType::Else) {
                //for else
                self.advance();
                Some(Box::new(self.parse_statement()?))
            } else {
                None
            };
            return Ok(Statement::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch,
            });
        }
        Err(self.error(
            ErrorType::SyntaxError,
            "Conditiosns should be inside parenthesis",
        ))
    }

    fn is_parameter_type(&self, token_t: &TokenType) -> bool {
        if matches!(
            token_t,
            TokenType::Int
                | TokenType::Void
                | TokenType::Float
                | TokenType::Double
                | TokenType::Char
        ) {
            return true;
        }
        false
    }

    /// parses statements such as int a; or int a = 2;
    fn parse_var_declaration(&mut self) -> ParseResult<Statement> {
        let var_type = self.parse_type()?;
        let name = self.parse_identifiers()?;

        let initializer = if self.check_token_type(&TokenType::Equal) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        let _ = self.consume_type(&TokenType::Semicolon, "expected a semicolon");
        Ok(Statement::VarDeclaration {
            var_type,
            name,
            initializer,
        })
    }

    /// parses the return statements such as
    /// return b;
    /// or
    /// return 1 && 0;
    fn parse_return_statement(&mut self) -> ParseResult<Statement> {
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

    /// for now just calls parse assignment
    /// the differentiation between parse_assignment and parse_expression
    /// is done to allow parse_assignment to be recursively called
    fn parse_expression(&self) -> ParseResult<Expression> {
        self.parse_assignment()
    }

    /// handles assignment expressions such as int a = 1;
    fn parse_assignment(&self) -> ParseResult<Expression> {
        let expr = self.parse_ternary_operation()?;

        if self.check_token_type(&TokenType::Equal) {
            self.advance();
            let value = self.parse_assignment()?;

            if let Expression::Identifier(name) = expr {
                Ok(Expression::Assignment {
                    target: name,
                    value: Box::new(value),
                })
            } else {
                Err(self.error(ErrorType::InvalidAssignment, "Invalid assignment target"))
            }
        } else {
            Ok(expr)
        }
    }

    /// firsts imposes the ~ or ! unary expressions
    /// then parses the binary operators
    /// and based on operator precedence,
    /// parses the complete binary expression repeatedly
    fn parse_binary_expression(&self, min_precedence: u8) -> ParseResult<Expression> {
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
                TokenType::Greater => BinaryOperator::Greater,
                TokenType::GreaterEqual => BinaryOperator::GreaterEqual,
                TokenType::Less => BinaryOperator::Less,
                TokenType::LessEqual => BinaryOperator::LessEqual,
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

    /// parses ternary conditional expressions: condition ? true_expr : false_expr
    /// obviously ternary operators are right associative so this parsing is done with that in mind
    fn parse_ternary_operation(&self) -> ParseResult<Expression> {
        let condition = self.parse_binary_expression(0)?;

        if self.check_token_type(&TokenType::QMark) {
            // we consume the question mark
            self.advance();

            let true_expr = self.parse_assignment()?;
            self.consume_type(&TokenType::Colon, "expected a colon (:)")?;
            let false_expr = self.parse_assignment()?;

            Ok(Expression::TernaryOP {
                condition: Box::new(condition),
                true_expr: Box::new(true_expr),
                false_expr: Box::new(false_expr),
            })
        } else {
            Ok(condition)
        }
    }
    /// parses unary expressions like ~, !, -
    fn parse_unary_expression(&self) -> ParseResult<Expression> {
        let token =
            expect_token!(self, ErrorType::UnexpectedToken, "Unexpected end of file").clone();

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
    fn parse_primary_expression(&self) -> ParseResult<Expression> {
        let token = match self.advance() {
            Some(token) => token,
            None => {
                return Err(self.error(
                    ErrorType::UnexpectedToken,
                    "Unexpected end of input, expected expression",
                ));
            }
        };
        let expression = match token.token_type() {
            TokenType::Number(value) => Expression::Number(*value),
            TokenType::Identifier(name) => {
                // in case we run into a function

                let is_function_call = self.check_token_type(&TokenType::LeftParen);

                if is_function_call {
                    self.advance();
                    let mut arguments = Vec::new();

                    // Parse arguments
                    let is_empty_args = self.check_token_type(&TokenType::RightParen);
                    if !is_empty_args {
                        loop {
                            let arg = self.parse_expression()?;
                            arguments.push(arg);

                            let has_comma = self.check_token_type(&TokenType::Comma);
                            if !has_comma {
                                break;
                            }
                            self.advance(); // consume ','
                        }
                    }

                    self.consume_type(&TokenType::RightParen, "Expected ')' after arguments")?;
                    Expression::FunctionCall {
                        name: name.clone(),
                        arguments,
                    }
                } else {
                    Expression::Identifier(name.clone())
                }
            }
            TokenType::LeftParen => {
                let expr = self.parse_expression()?;
                self.consume_type(&TokenType::RightParen, "Expected ')' after expression")?;
                expr
            }
            _ => {
                return Err(self.error(ErrorType::SyntaxError, "Unexpected token"));
            }
        };

        Ok(expression)
    }

    /// returns the precendence on which binary operators
    /// must be ordered/handled.
    /// "||" and "&&" binops have the least precedences
    /// "==", "!=", ">", ">=", "<" and "<=" all have the same precendence
    /// "+" and "-" have the same precendence
    /// "*" and "/" have the same and highest precendence
    fn get_precendece(&self, operator: &BinaryOperator) -> u8 {
        match operator {
            BinaryOperator::Or => 1,
            BinaryOperator::And => 2,
            BinaryOperator::Equals => 3,
            BinaryOperator::NotEquals => 3,
            BinaryOperator::Greater => 4,
            BinaryOperator::GreaterEqual => 4,
            BinaryOperator::Less => 4,
            BinaryOperator::LessEqual => 4,
            BinaryOperator::Add | BinaryOperator::Subtract => 5,
            BinaryOperator::Multiply | BinaryOperator::Divide => 6,
            //TODO: %
        }
    }
    /// returns a ref to the current token
    fn peek(&self) -> Option<&Token> {
        let current = *self.current.borrow();
        self.tokens.get(current)
    }

    /// checks to see if token vector is at its end,
    /// otherwise moves the pointer one token forward
    /// and returns the previous token
    fn advance(&self) -> Option<&Token> {
        if !self.is_at_end() {
            *self.current.borrow_mut() += 1;
        }
        self.previous()
    }

    /// returns None if token vector is empty,
    /// otherwise returns the token at current-1 position
    fn previous(&self) -> Option<&Token> {
        let current = *self.current.borrow();
        if current > 0 {
            self.tokens.get(current - 1)
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

    /// pretty obvious by its name, checks to see if the current
    /// token in the stream matches a certain (input) token
    fn check_token_type(&self, token_type: &TokenType) -> bool {
        if let Some(token) = self.peek() {
            token.token_type() == token_type
        } else {
            false
        }
    }

    /// takes in a token type, sees if the current token in the stream matches
    /// the tokentype in question. if yes, consumes the token and moves 1 position forward
    /// otherwise returns an error
    fn consume_type(&self, token_type: &TokenType, error_msg: &str) -> ParseResult<&Token> {
        if self.check_token_type(token_type) {
            Ok(self.advance().unwrap())
        } else {
            Err(self.error(ErrorType::MissingToken, error_msg))
        }
    }
    fn error(&self, error_type: ErrorType, message: &str) -> CompilerError {
        let (line, column) = if let Some(token) = self.peek() {
            (token.line(), token.column())
        } else if let Some(token) = self.previous() {
            (token.line(), token.column())
        } else {
            (1, 1)
        };
        let source_line = self.get_source_line(line);

        CompilerError::new(error_type, line, column, message).with_source_line(&source_line)
    }
    fn get_source_line(&self, line_number: usize) -> String {
        self.source
            .lines()
            .nth(line_number.saturating_sub(1))
            .unwrap_or("")
            .to_string()
    }
    pub fn get_errors(&self) -> Vec<CompilerError> {
        self.errors.borrow().clone()
    }

    fn is_identifier(&self) -> bool {
        if let Some(ident) = self.peek() {
            matches!(ident.token_type(), TokenType::Identifier(_))
        } else {
            false
        }
    }
}
