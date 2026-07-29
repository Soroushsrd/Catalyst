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
*********************************/
use crate::{
    errors::{CompilerError, ErrorType, Span},
    expect_token,
    frontend::lexer::{Num, Token, TokenType},
};
use std::cell::RefCell;

type ParseResult<T> = Result<T, CompilerError>;

/// a program is the first node in an AST
/// it contains a single child which is the function
#[derive(Debug, Clone)]
pub struct Program {
    pub function_def: Vec<Function>,
    pub global_vars: Vec<GlobalVariable>,
}

pub enum TopLvlDecs {
    Function(Function),
    GlobalVar(GlobalVariable),
}

#[derive(Debug, Clone)]
pub struct GlobalVariable {
    pub _type: Types,
    pub name: Identifier,
    pub initializer: Option<TypedExpr>,
}

/// functions consist of a name (identifier) and a
/// body. identifiers represent function and variables names
/// they're basically strings but different from "strings".
#[derive(Debug, Clone)]
pub struct Function {
    pub name: Identifier,
    pub body: Statement,
    pub parameters: Vec<Parameter>,
    pub return_type: Types,
    pub forward_dec: bool,
}

/// represents variable names, function names,etc
#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub parameter_type: Types,
    pub name: Option<Identifier>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Types {
    Void,
    /// short int. covers 'int', 'short int' and 'short' (signed)
    Int,
    /// unsigned int
    UInt,
    /// long int. covers both 'long' and 'long int' (signed)
    Long,
    // unsigned long
    ULong,
    // signed char. should be treated as u8
    Char,
    UChar,
    Float,
    Double,
    Pointer(Box<Types>), //TODO: Numeric types should be seperated
}

/// performs an action but doesnt return a value
#[derive(Debug, Clone)]
pub enum Statement {
    Empty,
    Block(Vec<Statement>),
    Return(Option<TypedExpr>),
    VarDeclaration {
        var_type: Types,
        name: Identifier,
        initializer: Option<TypedExpr>,
    },
    // used for expressions like:
    //      x = 5;
    //      foo();
    //      a+b;
    Expression(TypedExpr),
    If {
        condition: TypedExpr,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    While {
        condition: TypedExpr,
        then_branch: Box<Statement>,
    },
    DoWhile {
        body: Box<Statement>,
        condition: TypedExpr,
    },
    For {
        counter_declaration: Option<Box<Statement>>,
        incrementor: Option<TypedExpr>,
        condition: Option<TypedExpr>,
        body: Box<Statement>,
    },
    Break,
    Continue,
}
#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub kind: Expression,
    pub type_: Option<Types>,
    pub span: Span,
}

fn make_expr(kind: Expression, span: Span) -> TypedExpr {
    TypedExpr {
        kind,
        type_: None,
        span,
    }
}

fn compound_assign_op(token: &TokenType) -> Option<BinaryOperator> {
    match token {
        TokenType::PlusEqual => Some(BinaryOperator::Add),
        TokenType::MinusEqual => Some(BinaryOperator::Subtract),
        TokenType::StarEqual => Some(BinaryOperator::Multiply),
        TokenType::SlashEqual => Some(BinaryOperator::Divide),
        TokenType::ModEqual => Some(BinaryOperator::Mod),
        _ => None,
    }
}

/// evaluates to a value and can be used as part of other expressions
#[derive(Debug, Clone, Default)]
pub enum Expression {
    #[default]
    Unknown,
    Identifier(String),
    Number {
        value: Num,
    },
    CharLiteral(char),
    BitwiseNot(Box<TypedExpr>),
    UnaryMinus(Box<TypedExpr>),
    LogicalNot(Box<TypedExpr>),
    AddressOf(Box<TypedExpr>),
    Dereference(Box<TypedExpr>),
    Binary {
        left: Box<TypedExpr>,
        operator: BinaryOperator,
        right: Box<TypedExpr>,
    },
    Assignment {
        target: Box<TypedExpr>,
        value: Box<TypedExpr>,
    },
    CompoundAssignment {
        target: Box<TypedExpr>,
        binary_op: BinaryOperator,
        value: Box<TypedExpr>,
    },
    // as in a ? 1 : 2;
    TernaryOP {
        condition: Box<TypedExpr>,
        true_expr: Box<TypedExpr>,
        false_expr: Box<TypedExpr>,
    },
    PostDecrement(Box<TypedExpr>),
    PreDecrement(Box<TypedExpr>),
    PostIncrement(Box<TypedExpr>),
    PreIncrement(Box<TypedExpr>),
    FunctionCall {
        name: String,
        // using expression instead of parameter struct
        // because we are passing some sort of expression to
        // a function when we call it
        // foo()
        // or
        // foo(a+b)
        arguments: Vec<TypedExpr>,
    },
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Mod,
    Equals,
    NotEquals,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXOr,
    ShiftLeft,
    ShiftRight,
}

#[derive(Debug, Clone)]
/// Takes and store a stream of tokens generated by
/// Scanner and turns them into an AST
pub struct Parser {
    tokens: Vec<Token>,
    current: RefCell<usize>,
    loop_depth: RefCell<usize>,
    errors: RefCell<Vec<CompilerError>>,
    source: String,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, source: &str) -> Self {
        Self {
            tokens,
            current: RefCell::new(0),
            loop_depth: RefCell::new(0),
            errors: RefCell::new(Vec::with_capacity(10)),
            source: source.to_string(),
        }
    }
    pub fn parse(&mut self) -> Result<Program, Vec<CompilerError>> {
        let mut functions = Vec::with_capacity(5);
        let mut globals: Vec<GlobalVariable> = Vec::with_capacity(5);
        // TODO: includes could be handled before this operation

        while !self.is_at_end() {
            match self.parse_top_decs() {
                Ok(TopLvlDecs::Function(func)) => {
                    if globals.iter().any(|f| f.name.name == func.name.name) {
                        let error = self.error(
                            ErrorType::RedefinedVariable,
                            &format!("name {} has already been defined!", &func.name.name),
                            Some("choose a different name"),
                        );
                        self.errors.borrow_mut().push(error);
                    } else {
                        functions.push(func);
                    }
                }
                Ok(TopLvlDecs::GlobalVar(glob)) => {
                    if functions.iter().any(|f| f.name.name == glob.name.name) {
                        let error = self.error(
                            ErrorType::RedefinedVariable,
                            &format!("name {} has already been defined!", &glob.name.name),
                            Some("choose a different name"),
                        );
                        self.errors.borrow_mut().push(error);
                    } else if let Some(existing_idx) =
                        globals.iter().position(|g| g.name.name == glob.name.name)
                    {
                        let existing = &globals[existing_idx];
                        // allowing a tentative declaration (no initializer) to be completed
                        // by a later definition with an initializer, but not two full definitions
                        if existing.initializer.is_none() && glob.initializer.is_some() {
                            // replacing the tentative declaration with the full definition,
                            globals[existing_idx] = glob;
                        } else {
                            let error = self.error(
                                ErrorType::RedefinedVariable,
                                &format!("name {} has already been defined!", &glob.name.name),
                                Some("choose a different name"),
                            );
                            self.errors.borrow_mut().push(error);
                        }
                    } else {
                        globals.push(glob);
                    }
                }
                Err(e) => {
                    self.errors.borrow_mut().push(e);
                    self.synchronize();
                }
            }
        }

        self.validate_function_decs(&functions);

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
                global_vars: globals,
            })
        } else {
            Err(self.errors.borrow().clone())
        }
    }

    fn parse_top_decs(&mut self) -> ParseResult<TopLvlDecs> {
        // let current_pos = *self.current.borrow();
        let var_type = self.parse_type()?;
        let name = self.parse_identifiers()?;
        if self.check_token_type(&TokenType::LeftParen) {
            // then its a function dec and not a global variable
            self.consume_type(&TokenType::LeftParen, "Expected '(' after function name")?;
            let parameters = self.parse_parameters()?;
            self.consume_type(&TokenType::RightParen, "Expected ')' after parameters")?;

            let (body, forward_dec) = if self.check_token_type(&TokenType::Semicolon) {
                self.advance();
                (Statement::Block(Vec::new()), true)
            } else {
                let body = self.parse_block_statement()?;
                (body, false)
            };

            Ok(TopLvlDecs::Function(Function {
                name,
                body,
                parameters,
                return_type: var_type,
                forward_dec,
            }))
        } else {
            let initializer = if self.check_token_type(&TokenType::Equal) {
                self.advance();
                Some(self.parse_expression()?)
            } else {
                None
            };
            self.consume_type(
                &TokenType::Semicolon,
                "expected a semicolon at the end of the expression",
            )?;
            Ok(TopLvlDecs::GlobalVar(GlobalVariable {
                _type: var_type,
                name,
                initializer,
            }))
        }
    }

    /// used to parse function return type
    fn parse_type(&mut self) -> ParseResult<Types> {
        let token = expect_token!(
            self,
            ErrorType::UnexpectedToken,
            "Unexpected end of file",
            None
        );
        let mut type_ = match token.token_type() {
            TokenType::Int => Types::Int,
            TokenType::Void => Types::Void,
            TokenType::Char => Types::Char,
            TokenType::Long => Types::Long,
            TokenType::Float => Types::Float,
            TokenType::Double => Types::Double,
            TokenType::Pointer(ptr) => Types::Pointer(ptr.clone()),
            _ => {
                return Err(self.error(
                    ErrorType::TypeError,
                    "Expected type",
                    Some("if your function doesn't return anything, use 'void' as return type"),
                ));
            }
        };
        self.advance();

        while self.check_token_type(&TokenType::Star) {
            self.advance();
            type_ = Types::Pointer(Box::new(type_));
        }
        Ok(type_)
    }

    /// parses identifiers. These identifiers could be variable names,
    /// method names, and so on!
    fn parse_identifiers(&self) -> ParseResult<Identifier> {
        let ident = expect_token!(
            self,
            ErrorType::UnexpectedToken,
            "Unexpected end of file",
            None
        );
        let expression = match ident.token_type() {
            TokenType::Identifier(name) => Ok(Identifier {
                name: name.clone(),
                span: Span::new(ident.line, ident.column),
            }),
            _ => Err(self.error(ErrorType::MissingToken, "Expected identifier", None)),
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
                            parameter_type: Types::Void,
                            name: None,
                        });
                        return Ok(parameters);
                    }
                    // in case void is used a type for some identifier
                    // foo(void *a)
                    let name = if self.check_token_type(&TokenType::Identifier("".to_string())) {
                        Some(self.parse_identifiers()?)
                    } else {
                        None
                    };

                    parameters.push(Parameter {
                        parameter_type: Types::Void,
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
                            return Err(self.error(
                                ErrorType::SemanticError,
                                "Too many parameters",
                                Some("keep the function paramters less than or equal to 6"),
                            ));
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
                }
            }
        }
        Ok(parameters)
    }

    fn parse_parameter_type(&self) -> ParseResult<Types> {
        let token = expect_token!(
            self,
            ErrorType::UnexpectedToken,
            "Unexpected end of file",
            None
        );
        let param_type = match token.type_ {
            TokenType::Int => Types::Int,
            TokenType::Void => Types::Void,
            TokenType::Char => Types::Char,
            TokenType::Long => Types::Long,
            TokenType::Float => Types::Float,
            TokenType::Double => Types::Double,
            _ => return Err(self.error(ErrorType::TypeError, "Expected parameter type", None)),
        };
        self.advance();
        Ok(param_type)
    }

    /// block statements contain what ever comes after function declaration
    /// and is inside its braces
    fn parse_block_statement(&mut self) -> ParseResult<Statement> {
        self.consume_type(&TokenType::LeftBrace, "Expected '{'")?;
        let mut statements = Vec::with_capacity(5);
        while !self.check_token_type(&TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.parse_statement()?);
        }
        self.consume_type(&TokenType::RightBrace, "Expected '}'")?;
        Ok(Statement::Block(statements))
    }

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        let result = self.parse_statement_inner();
        if result.is_err() {
            self.synchronize();
        }
        result
    }

    /// parses different statements
    fn parse_statement_inner(&mut self) -> ParseResult<Statement> {
        if self.check_token_type(&TokenType::Return) {
            self.parse_return_statement()
        } else if self.check_token_type(&TokenType::Break) {
            self.parse_break_statement()
        } else if self.check_token_type(&TokenType::Continue) {
            self.parse_continue_statement()
        } else if self.check_token_type(&TokenType::Semicolon) {
            self.advance();
            Ok(Statement::Empty)
        } else if self
            .peek()
            .is_some_and(|t| self.is_parameter_type(t.token_type()))
        {
            self.parse_var_declaration()
        } else if self.check_token_type(&TokenType::If) {
            self.parse_if_flow()
        } else if self.check_token_type(&TokenType::LeftBrace) {
            self.parse_block_statement()
        } else if self.check_token_type(&TokenType::While) {
            self.parse_while_flow()
        } else if self.check_token_type(&TokenType::Do) {
            self.parse_do_while_flow()
        } else if self.check_token_type(&TokenType::For) {
            self.parse_for_statement()
        } else {
            let expr = self.parse_expression()?;
            self.consume_type(&TokenType::Semicolon, "expected a semicolon")?;
            Ok(Statement::Expression(expr))
        }
    }

    fn parse_for_statement(&mut self) -> ParseResult<Statement> {
        self.consume_type(&TokenType::For, "for loop starts with a for keyword")?;
        self.consume_type(&TokenType::LeftParen, "expected ( after for keyword")?;

        let init = if self.check_token_type(&TokenType::Semicolon) {
            self.consume_type(&TokenType::Semicolon, "expectd ;")?;
            None
        } else if self
            .peek()
            .is_some_and(|t| self.is_parameter_type(t.token_type()))
        {
            Some(Box::new(self.parse_var_declaration()?))
        } else {
            let expr = self.parse_expression()?;
            self.consume_type(&TokenType::Semicolon, "expected ; after initiation")?;
            Some(Box::new(Statement::Expression(expr)))
        };

        let condition = if self.check_token_type(&TokenType::Semicolon) {
            self.consume_type(&TokenType::Semicolon, "expected ;")?;
            None
        } else {
            let expr = self.parse_expression()?;
            self.consume_type(&TokenType::Semicolon, "expected a semicolon")?;
            Some(expr)
        };

        let increment = if self.check_token_type(&TokenType::RightParen) {
            None
        } else {
            Some(self.parse_expression()?)
        };

        self.consume_type(&TokenType::RightParen, "expected a )")?;
        *self.loop_depth.borrow_mut() += 1;
        let body = Box::new(self.parse_statement()?);
        *self.loop_depth.borrow_mut() -= 1;

        Ok(Statement::For {
            counter_declaration: init,
            incrementor: increment,
            condition,
            body,
        })
    }

    fn parse_do_while_flow(&mut self) -> ParseResult<Statement> {
        self.consume_type(&TokenType::Do, "Expected a do keyword")?;

        *self.loop_depth.borrow_mut() += 1;
        let body = self.parse_block_statement()?;
        *self.loop_depth.borrow_mut() -= 1;

        self.consume_type(&TokenType::While, "Expected a while keyword")?;

        self.consume_type(
            &TokenType::LeftParen,
            "Expected the while statement to start with a parenthesis",
        )?;
        let condition = self.parse_expression()?;
        self.consume_type(
            &TokenType::RightParen,
            "Expected the while statement to end with a parenthesis",
        )?;

        self.consume_type(&TokenType::Semicolon, "Expected ';' after do-while")?;

        Ok(Statement::DoWhile {
            body: Box::new(body),
            condition,
        })
    }

    fn parse_while_flow(&mut self) -> ParseResult<Statement> {
        self.consume_type(&TokenType::While, "Expected a while keyword")?;

        if self.check_token_type(&TokenType::LeftParen) {
            self.consume_type(
                &TokenType::LeftParen,
                "Expected the while statement to start with a parenthesis",
            )?;
            let condition = self.parse_expression()?;
            self.consume_type(
                &TokenType::RightParen,
                "Expected the while statement to end with a parenthesis",
            )?;

            *self.loop_depth.borrow_mut() += 1;
            let then_branch = self.parse_statement()?;
            *self.loop_depth.borrow_mut() -= 1;

            Ok(Statement::While {
                condition,
                then_branch: Box::new(then_branch),
            })
        } else {
            Err(self.error(
                ErrorType::SyntaxError,
                "while loop must have a condition",
                Some("specify some kind of condition for your loop"),
            ))
        }
    }
    fn parse_if_flow(&mut self) -> ParseResult<Statement> {
        self.consume_type(&TokenType::If, "Expected an if keyword")?;
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

            if matches!(then_branch, Statement::VarDeclaration { .. }) {
                return Err(self.error(
                    ErrorType::SyntaxError,
                    "Variable declaration not allowed here",
                    Some("remove this variable declaration"),
                ));
            }

            let else_branch = if self.check_token_type(&TokenType::Else) {
                //for else
                self.advance();
                let stmt = self.parse_statement()?;

                if matches!(then_branch, Statement::VarDeclaration { .. }) {
                    return Err(self.error(
                        ErrorType::SyntaxError,
                        "Variable declaration not allowed here",
                        Some("remove this variable declaration"),
                    ));
                }

                Some(Box::new(stmt))
            } else {
                None
            };
            return Ok(Statement::If {
                condition,
                then_branch: Box::new(then_branch),
                else_branch,
            });
        }
        Err(self.error(
            ErrorType::MissingToken,
            "Conditiosns should be inside parenthesis",
            Some("use parenthesis!"),
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
                | TokenType::Long
                | TokenType::Short
                | TokenType::Unsigned
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

        self.consume_type(&TokenType::Semicolon, "expected a semicolon")?;
        Ok(Statement::VarDeclaration {
            var_type,
            name,
            initializer,
        })
    }

    fn parse_continue_statement(&mut self) -> ParseResult<Statement> {
        if *self.loop_depth.borrow() == 0 {
            return Err(self.error(
                ErrorType::SemanticError,
                "continue statement not withing a loop",
                Some("remove this continue statement"),
            ));
        }

        self.consume_type(&TokenType::Continue, "expected a continue keyword")?;
        self.consume_type(&TokenType::Semicolon, "expected a semicolon")?;

        Ok(Statement::Continue)
    }

    fn parse_break_statement(&mut self) -> ParseResult<Statement> {
        if *self.loop_depth.borrow() == 0 {
            return Err(self.error(
                ErrorType::SemanticError,
                "break statement not within a loop",
                Some("remove this break statement"),
            ));
        }

        self.consume_type(&TokenType::Break, "Expected Break")?;
        self.consume_type(&TokenType::Semicolon, "Expected a semicolon")?;
        Ok(Statement::Break)
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
    ///bin/ parse_expression()
    /// └── parse_assignment()          (lowest precedence)
    ///     └── parse_ternary_operation()
    ///         └── parse_binary_expression(0)
    ///             └── parse_unary_expression()
    ///                 └── parse_primary_expression()  (highest precedence)
    fn parse_expression(&self) -> ParseResult<TypedExpr> {
        self.parse_assignment()
    }

    /// handles assignment expressions such as int a = 1 and a +=1 ;
    fn parse_assignment(&self) -> ParseResult<TypedExpr> {
        let expr = self.parse_ternary_operation()?;

        if self.check_token_type(&TokenType::Equal) {
            self.advance();
            let value = self.parse_assignment()?;
            return self.make_assignment(expr, None, value);
        }
        let compound = self.peek().and_then(|c| compound_assign_op(c.token_type()));
        if let Some(op) = compound {
            self.advance();
            let value = self.parse_assignment()?;
            return self.make_assignment(expr, Some(op), value);
        }
        Ok(expr)
    }

    fn make_assignment(
        &self,
        target: TypedExpr,
        operator: Option<BinaryOperator>,
        value: TypedExpr,
    ) -> ParseResult<TypedExpr> {
        if !matches!(
            target.kind,
            Expression::Identifier(_) | Expression::Dereference(_)
        ) {
            return Err(self.error(
                ErrorType::InvalidAssignment,
                "Invalid assignment target",
                Some("Can only assign to variables or dereferenced pointers"),
            ));
        }
        let span = target.span;
        let kind = match operator {
            Some(op) => Expression::CompoundAssignment {
                target: Box::new(target),
                binary_op: op,
                value: Box::new(value),
            },
            None => Expression::Assignment {
                target: Box::new(target),
                value: Box::new(value),
            },
        };
        Ok(make_expr(kind, span))
    }
    /// firsts imposes the ~ or ! unary expressions
    /// then parses the binary operators
    /// and based on operator precedence,
    /// parses the complete binary expression repeatedly
    fn parse_binary_expression(&self, min_precedence: u8) -> ParseResult<TypedExpr> {
        let mut left = self.parse_unary_expression()?;

        while let Some(token) = self.peek() {
            let operator = match token.token_type() {
                TokenType::Plus => BinaryOperator::Add,
                TokenType::Minus => BinaryOperator::Subtract,
                TokenType::Star => BinaryOperator::Multiply,
                TokenType::Slash => BinaryOperator::Divide,
                TokenType::Mod => BinaryOperator::Mod,
                TokenType::And => BinaryOperator::And,
                TokenType::Or => BinaryOperator::Or,
                TokenType::BangEqual => BinaryOperator::NotEquals,
                TokenType::EqualEqual => BinaryOperator::Equals,
                TokenType::Greater => BinaryOperator::Greater,
                TokenType::GreaterEqual => BinaryOperator::GreaterEqual,
                TokenType::Less => BinaryOperator::Less,
                TokenType::LessEqual => BinaryOperator::LessEqual,
                TokenType::BitwiseOr => BinaryOperator::BitOr,
                TokenType::BitwiseXor => BinaryOperator::BitXOr,
                TokenType::Ampersand => BinaryOperator::BitAnd,
                TokenType::ShiftLeft => BinaryOperator::ShiftLeft,
                TokenType::ShiftRight => BinaryOperator::ShiftRight,
                _ => break,
            };

            let precendence = self.get_precendece(&operator);
            if precendence < min_precedence {
                break;
            }
            self.advance();

            let right = self.parse_binary_expression(precendence + 1)?;
            let span = left.span.clone();
            left = make_expr(
                Expression::Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                },
                span,
            );
        }
        Ok(left)
    }

    fn parse_postfix_expression(&self) -> ParseResult<TypedExpr> {
        let mut expr = self.parse_primary_expression()?;
        loop {
            let span = expr.span;
            if self.check_token_type(&TokenType::PlusPlus) {
                self.advance();
                expr = make_expr(Expression::PostIncrement(Box::new(expr)), span);
            } else if self.check_token_type(&TokenType::MinusMinus) {
                self.advance();
                expr = make_expr(Expression::PostDecrement(Box::new(expr)), span);
            } else {
                break;
            }
        }
        Ok(expr)
    }

    /// parses ternary conditional expressions: condition ? true_expr : false_expr
    /// obviously ternary operators are right associative so this parsing is done with that in mind
    fn parse_ternary_operation(&self) -> ParseResult<TypedExpr> {
        let condition = self.parse_binary_expression(0)?;

        if self.check_token_type(&TokenType::QMark) {
            // we consume the question mark
            self.advance();

            let true_expr = self.parse_assignment()?;
            self.consume_type(&TokenType::Colon, "expected a colon (:)")?;
            let false_expr = self.parse_assignment()?;

            let span = condition.span.clone();
            Ok(make_expr(
                Expression::TernaryOP {
                    condition: Box::new(condition),
                    true_expr: Box::new(true_expr),
                    false_expr: Box::new(false_expr),
                },
                span,
            ))
        } else {
            Ok(condition)
        }
    }
    /// parses unary expressions like ~, !, -, and prefixes such as -- and ++
    fn parse_unary_expression(&self) -> ParseResult<TypedExpr> {
        let token = expect_token!(
            self,
            ErrorType::UnexpectedToken,
            "Unexpected end of file",
            None
        )
        .clone();

        let span = Span::new(token.line, token.column);
        match token.token_type() {
            TokenType::Minus => {
                self.advance();
                let expr = self.parse_unary_expression()?;
                Ok(make_expr(Expression::UnaryMinus(Box::new(expr)), span))
            }
            TokenType::Bang => {
                self.advance();
                let expr = self.parse_unary_expression()?;
                Ok(make_expr(Expression::LogicalNot(Box::new(expr)), span))
            }
            TokenType::BitwiseNot => {
                self.advance();
                let expr = self.parse_unary_expression()?;
                Ok(make_expr(Expression::BitwiseNot(Box::new(expr)), span))
            }
            TokenType::Ampersand => {
                self.advance();
                let expr = self.parse_unary_expression()?;
                Ok(make_expr(Expression::AddressOf(Box::new(expr)), span))
            }
            TokenType::Star => {
                self.advance();
                let expr = self.parse_unary_expression()?;
                Ok(make_expr(Expression::Dereference(Box::new(expr)), span))
            }
            TokenType::PlusPlus => {
                self.advance();
                let expr = self.parse_unary_expression()?;
                Ok(make_expr(Expression::PreIncrement(Box::new(expr)), span))
            }
            TokenType::MinusMinus => {
                self.advance();
                let expr = self.parse_unary_expression()?;
                Ok(make_expr(Expression::PreDecrement(Box::new(expr)), span))
            }
            _ => self.parse_postfix_expression(),
        }
    }

    /// based on token type defined in lexer module, parses the primary tokens
    /// then matches it agains expressions and advances one token forward
    fn parse_primary_expression(&self) -> ParseResult<TypedExpr> {
        let span = self.with_current_span();
        let token = match self.advance() {
            Some(token) => token,
            None => {
                return Err(self.error(
                    ErrorType::UnexpectedToken,
                    "Unexpected end of input, expected expression",
                    None,
                ));
            }
        };
        let expression = match token.token_type() {
            // TODO: think of replacing f32 for numbers. probably needs expansion of Number type
            TokenType::Number { value } => make_expr(
                Expression::Number {
                    value: value.to_owned(),
                },
                span,
            ),
            TokenType::CharLiteral(ch) => make_expr(Expression::CharLiteral(*ch), span),
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

                    make_expr(
                        Expression::FunctionCall {
                            name: name.clone(),
                            arguments,
                        },
                        span,
                    )
                } else {
                    make_expr(Expression::Identifier(name.clone()), span)
                }
            }
            TokenType::LeftParen => {
                let expr = self.parse_expression()?;
                self.consume_type(&TokenType::RightParen, "Expected ')' after expression")?;
                expr
            }
            _ => {
                return Err(self.error(ErrorType::UnexpectedToken, "Unexpected token", None));
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
            BinaryOperator::BitOr => 3,
            BinaryOperator::BitXOr => 4,
            BinaryOperator::BitAnd => 5,
            BinaryOperator::Equals | BinaryOperator::NotEquals => 6,
            BinaryOperator::Greater
            | BinaryOperator::GreaterEqual
            | BinaryOperator::Less
            | BinaryOperator::LessEqual => 7,
            BinaryOperator::ShiftLeft | BinaryOperator::ShiftRight => 8,
            BinaryOperator::Add | BinaryOperator::Subtract => 9,
            BinaryOperator::Multiply | BinaryOperator::Divide | BinaryOperator::Mod => 10,
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
            Err(self.error(ErrorType::MissingToken, error_msg, None))
        }
    }

    /// cursor based errors. other type of errors are returned as return values for each function that needs them
    fn error(
        &self,
        error_type: ErrorType,
        message: &str,
        suggestion: Option<&str>,
    ) -> CompilerError {
        let (line, column) = match error_type {
            ErrorType::MissingToken => {
                if let Some(token) = self.previous() {
                    (token.line(), token.column())
                } else {
                    (1, 1)
                }
            }
            _ => {
                if let Some(token) = self.peek() {
                    (token.line(), token.column())
                } else if let Some(token) = self.previous() {
                    (token.line(), token.column())
                } else {
                    (1, 1)
                }
            }
        };
        let source_line = self.get_source_line(line);

        if let Some(sug) = suggestion {
            CompilerError::new(error_type, line, column, message)
                .with_source_line(&source_line)
                .with_suggestion(sug)
        } else {
            CompilerError::new(error_type, line, column, message).with_source_line(&source_line)
        }
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

    fn synchronize(&self) {
        self.advance();
        while !self.is_at_end() {
            //either stop at semicolon or at keywords. thus skipping the problematic area that caused
            // the error to pop up
            if let Some(prev) = self.previous()
                && matches!(prev.token_type(), TokenType::Semicolon)
            {
                return;
            }
            if let Some(token) = self.peek() {
                match token.token_type() {
                    TokenType::If
                    | TokenType::While
                    | TokenType::Do
                    | TokenType::For
                    | TokenType::Return
                    | TokenType::Int
                    | TokenType::Void
                    | TokenType::Char
                    | TokenType::Long
                    | TokenType::Float
                    | TokenType::Double => return,
                    _ => {}
                }
            }
            self.advance();
        }
    }

    fn validate_function_decs(&self, functions: &[Function]) {
        let forward_decs = functions
            .iter()
            .filter(|f| f.forward_dec)
            .collect::<Vec<&Function>>();

        for forward_dec in forward_decs {
            let implementations = functions
                .iter()
                .filter(|f| !f.forward_dec && f.name.name == forward_dec.name.name)
                .collect::<Vec<&Function>>();

            if implementations.len() > 1 {
                let span = forward_dec.name.span;
                self.errors.borrow_mut().push(
                    CompilerError::new(
                        ErrorType::SemanticError,
                        span.line,
                        span.column,
                        &format!(
                            "forward declaration '{}' has multiple implementation",
                            forward_dec.name.name
                        ),
                    )
                    .with_source_line(&self.get_source_line(span.line))
                    .with_suggestion("remove one of them"),
                );
                continue;
            }

            if !implementations.is_empty() {
                let implementation = implementations[0];

                if forward_dec.return_type != implementation.return_type {
                    let span = forward_dec.name.span;
                    self.errors.borrow_mut().push(CompilerError::new(
                    ErrorType::TypeError,
                    span.line,
                    span.column,
                    &format!(
                        "forward declaration '{}' has different return type({:?}) than its implementation({:?})",
                        forward_dec.name.name,
                        forward_dec.return_type,
                        implementation.return_type
                    ),
                    ).with_source_line(&self.get_source_line(span.line))
                    .with_suggestion("decide on one type!"));
                    continue;
                }

                if forward_dec.parameters.len() != implementation.parameters.len() {
                    let span = forward_dec.name.span;
                    self.errors.borrow_mut().push(
                        CompilerError::new(
                            ErrorType::TypeError,
                            span.line,
                            span.column,
                            &format!(
                        "forward declaration '{}' has different parameters than its implementation",
                        forward_dec.name.name
                    ),
                        ).with_source_line(&self.get_source_line(span.line))
                        .with_suggestion("you might want to reconsider your implementation"),
                    );
                    continue;
                }

                for (i, (dec_param, imp_param)) in forward_dec
                    .parameters
                    .iter()
                    .zip(implementation.parameters.iter())
                    .enumerate()
                {
                    if dec_param.parameter_type != imp_param.parameter_type {
                        let span = if dec_param.name.is_some() {
                            dec_param.name.as_ref().unwrap().span
                        } else {
                            forward_dec.name.span
                        };
                        self.errors.borrow_mut().push(CompilerError::new(
                            ErrorType::TypeError,
                            span.line,
                            span.column,
                            &format!(
                            "Parameter '{}' of '{}' has a different type than its implementation",
                            i + 1,
                            forward_dec.name.name
                        ),
                        )
                            .with_source_line(&self.get_source_line(span.line)));

                        continue;
                    }
                }
            }
        }

        self.check_for_mult_imp(functions);
    }

    fn check_for_mult_imp(&self, functions: &[Function]) {
        let implementations = functions
            .iter()
            .filter(|f| !f.forward_dec)
            .collect::<Vec<_>>();
        for i in 0..implementations.len() {
            for j in (i + 1)..implementations.len() {
                let func = implementations[j];
                if implementations[i].name.name == func.name.name {
                    let span = func.name.span;
                    self.errors.borrow_mut().push(
                        CompilerError::new(
                            ErrorType::SemanticError,
                            span.line,
                            span.column,
                            &format!(
                                "There are multiple forward declarations for '{}'",
                                implementations[i].name.name
                            ),
                        )
                        .with_source_line(&self.get_source_line(span.line))
                        .with_suggestion("remove one declaration"),
                    );
                }
            }
        }
    }
    fn with_current_span(&self) -> Span {
        self.peek()
            .or_else(|| self.previous())
            .map(|l| Span::new(l.line, l.column))
            .unwrap_or(Span::new(1, 1))
    }
}
