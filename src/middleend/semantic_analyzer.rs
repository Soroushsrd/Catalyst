use crate::{
    errors::{CompilerError, ErrorType},
    frontend::parser::*,
};
use std::collections::HashMap;

/// first pass validation
/// - check variable/function declaration order
/// - validate scoping rules
/// - detect use-before-declaration errors
/// - validate function signatures match between declaration and definition
///
/// this struct does not check:
/// - type compatibility (handled by code generator)
/// - type conversions (handled by code generator)
pub struct SemanticAnalyzer {
    declared_globals: HashMap<String, GlobalVariable>,
    declared_functions: HashMap<String, Function>,
    forward_declarations: HashMap<String, Function>,
    errors: Vec<CompilerError>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            declared_globals: HashMap::new(),
            declared_functions: HashMap::new(),
            forward_declarations: HashMap::new(),
            errors: Vec::new(),
        }
    }

    pub fn analyze(&mut self, program: &Program) -> Result<(), Vec<CompilerError>> {
        for function in &program.function_def {
            if function.forward_dec {
                self.forward_declarations
                    .insert(function.name.name.clone(), function.clone());
            }
            self.declared_functions
                .insert(function.name.name.clone(), function.clone());
        }

        let mut seen_globals = HashMap::new();
        let declared_functions = self.declared_functions.clone();

        for global in &program.global_vars {
            if let Some(init) = &global.initializer {
                self.check_global_initializer(init, &seen_globals, &declared_functions);
            }
            seen_globals.insert(global.name.name.clone(), global.clone());
        }

        self.declared_globals = seen_globals;

        let declared_functions = self.declared_functions.clone();

        for function in &program.function_def {
            if !function.forward_dec {
                let mut visible_globals = HashMap::new();
                let _ = program
                    .global_vars
                    .iter()
                    .filter(|g| g.declaration_idx < function.declaration_idx)
                    .map(|g| visible_globals.insert(g.name.name.clone(), g._type.clone()));
                let mut local_scope = LocalScope::new(visible_globals);

                // Adding function parameters to local scope
                for param in &function.parameters {
                    if let Some(name) = &param.name {
                        local_scope.declare_local(name.name.clone(), param.parameter_type.clone());
                    }
                }

                self.check_statement(
                    &function.body,
                    &mut local_scope,
                    &declared_functions,
                    &function.return_type,
                );
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }
    fn type_of(&mut self, expr: &Expression, scope: &LocalScope) -> Result<Types, CompilerError> {
        match expr {
            Expression::Number(_) => Ok(Types::Int),
            Expression::CharLiteral(_) => Ok(Types::Char),
            Expression::Identifier(ident) => scope
                .lookup_type(ident)
                .ok_or(
                    CompilerError::new(
                        ErrorType::TypeError,
                        1,
                        1,
                        &format!("failed to find {ident} in local scope"),
                    )
                    .with_suggestion("Are you sure you defined the variable?"),
                )
                .cloned(),
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                let left_type = self.type_of(left, scope)?;
                let right_type = self.type_of(right, scope)?;
                match operator {
                    BinaryOperator::Equals
                    | BinaryOperator::NotEquals
                    | BinaryOperator::Greater
                    | BinaryOperator::GreaterEqual
                    | BinaryOperator::Less
                    | BinaryOperator::LessEqual => {
                        self.check_comparable(&left_type, &right_type)?;
                        Ok(Types::Int)
                    }
                    _ => self.unify_arithmetic(&left_type, &right_type),
                }
            }
            Expression::AddressOf(expr) => {
                let inner_type = self.type_of(expr, scope)?;
                Ok(Types::Pointer(Box::new(inner_type)))
            }
            Expression::Dereference(expr) => match self.type_of(expr, scope)? {
                Types::Pointer(inner) => Ok(*inner),
                _ => Err(CompilerError::new(
                    ErrorType::TypeError,
                    1,
                    1,
                    "Only pointers can be dereferenced",
                )),
            },
            Expression::Assignment { target, value } => {
                let t_type = self.type_of(target, scope)?;
                let v_type = self.type_of(value, scope)?;
                self.check_assignable(&t_type, &v_type)
            }
            Expression::FunctionCall { name, arguments } => {
                let (param_types, return_type) = match self.function_exists(name) {
                    Some(func) => (
                        func.parameters
                            .iter()
                            .map(|p| p.parameter_type.clone())
                            .collect::<Vec<_>>(),
                        func.return_type.clone(),
                    ),
                    None => {
                        return Err(CompilerError::new(
                            ErrorType::TypeError,
                            1,
                            1,
                            &format!("call to undeclared function '{name}'"),
                        ));
                    }
                };
                // arity check
                if arguments.len() != param_types.len() {
                    return Err(CompilerError::new(
                        ErrorType::TypeError,
                        1,
                        1,
                        &format!(
                            "function '{name}' expects {} argument(s), got {}",
                            param_types.len(),
                            arguments.len()
                        ),
                    )
                    .with_suggestion("check the number of arguments"));
                }
                for (i, (arg, expected)) in arguments.iter().zip(param_types.iter()).enumerate() {
                    let actual = self.type_of(arg, scope)?;
                    if let Err(e) = self.check_assignable(expected, &actual) {
                        return Err(CompilerError::new(
                            ErrorType::TypeError,
                            1,
                            1,
                            &format!(
                                "argument {} of '{name}': expected {expected:?}, got {actual:?}",
                                i + 1
                            ),
                        ));
                    }
                }
                Ok(return_type)
            }

            Expression::TernaryOP {
                condition,
                true_expr,
                false_expr,
            } => {
                todo!()
            }
            Expression::BitwiseNot(tt) => self.type_of(tt, scope),
            Expression::LogicalNot(tt) => self.type_of(tt, scope),
            Expression::UnaryMinus(tt) => self.type_of(tt, scope),
            Expression::Unknown => Err(CompilerError::new(
                ErrorType::TypeError,
                1,
                1,
                "Unknown type",
            )),
        }
    }

    /// Arithmetic binary ops (+ - * / %): result is the unified operand type.
    /// `a + b` where a:Int, b:Long  ->  Long
    fn unify_arithmetic(&self, left: &Types, right: &Types) -> Result<Types, CompilerError> {
        unify(left, right).ok_or_else(|| {
            CompilerError::new(
                ErrorType::TypeError,
                1,
                1,
                &format!("cannot apply arithmetic operator to {left:?} and {right:?}"),
            )
            .with_suggestion("operands of an arithmetic operator must be compatible numeric types")
        })
    }

    fn function_exists(&self, name: &str) -> Option<&Function> {
        if let Some(func) = self.declared_functions.get(name) {
            return Some(func);
        }
        if let Some(func) = self.forward_declarations.get(name) {
            return Some(func);
        }
        None
    }

    // TODO: simplify!
    fn check_assignable(
        &self,
        target_type: &Types,
        value_type: &Types,
    ) -> Result<Types, CompilerError> {
        unify(target_type, value_type).ok_or_else(|| {
            CompilerError::new(
                ErrorType::TypeError,
                1,
                1,
                &format!("cannot assign {target_type:?} to {value_type:?}"),
            )
            .with_suggestion("Assignment must be compatible types")
        })
    }

    /// Comparison ops (== != < <= > >=): operands must unify, but the
    /// RESULT is always Int, regardless of operand type.
    /// `a < b` where a:Float, b:Int  ->  Int  (not Float)
    fn check_comparable(&self, left: &Types, right: &Types) -> Result<Types, CompilerError> {
        unify(left, right).map(|_| Types::Int).ok_or_else(|| {
            CompilerError::new(
                ErrorType::TypeError,
                1,
                1,
                &format!("cannot compare {left:?} with {right:?}"),
            )
            .with_suggestion("comparison operands must be compatible types")
        })
    }

    fn check_global_initializer(
        &mut self,
        expr: &Expression,
        seen_globals: &HashMap<String, GlobalVariable>,
        available_functions: &HashMap<String, Function>,
    ) {
        match expr {
            Expression::Identifier(name) => {
                if seen_globals.get(name).is_none() {
                    self.errors.push(CompilerError::new(
                        ErrorType::UndefinedVariable,
                        1,
                        1,
                        &format!(
                            "Global variable '{}' used in initializer before its declaration",
                            name
                        ),
                    ).with_suggestion("Move the declaration of this variable before its first use in an initializer"));
                }
            }
            Expression::Binary { left, right, .. } => {
                self.check_global_initializer(left, seen_globals, available_functions);
                self.check_global_initializer(right, seen_globals, available_functions);
            }
            Expression::UnaryMinus(e) | Expression::LogicalNot(e) | Expression::BitwiseNot(e) => {
                self.check_global_initializer(e, seen_globals, available_functions);
            }
            Expression::TernaryOP {
                condition,
                true_expr,
                false_expr,
            } => {
                self.check_global_initializer(condition, seen_globals, available_functions);
                self.check_global_initializer(true_expr, seen_globals, available_functions);
                self.check_global_initializer(false_expr, seen_globals, available_functions);
            }
            Expression::FunctionCall { name, arguments } => {
                if available_functions.get(name).is_none() {
                    self.errors.push(CompilerError::new(
                        ErrorType::UndefinedVariable,
                        1,
                        1,
                        &format!("Function '{}' not declared", name),
                    ));
                }
                for arg in arguments {
                    self.check_global_initializer(arg, seen_globals, available_functions);
                }
            }
            _ => {}
        }
    }

    // TODO: check if we can remove check_expression_in_function method
    fn check_expr(&mut self, expr: &Expression, scope: &LocalScope) {
        if let Err(e) = self.type_of(expr, scope) {
            self.errors.push(e);
        }
    }

    fn check_statement(
        &mut self,
        stmt: &Statement,
        local_scope: &mut LocalScope,
        available_functions: &HashMap<String, Function>,
        return_type: &Types,
    ) {
        match stmt {
            Statement::Block(stmts) => {
                local_scope.push_scope();
                for s in stmts {
                    self.check_statement(s, local_scope, available_functions, return_type);
                }
                local_scope.pop_scope();
            }
            Statement::Return(expr) => match expr {
                Some(ex) => match self.type_of(ex, local_scope) {
                    Ok(actual) => {
                        if let Err(e) = self.check_assignable(return_type, &actual) {
                            self.errors.push(e);
                        }
                    }
                    Err(e) => self.errors.push(e),
                },
                None => {
                    if *return_type != Types::Void {
                        self.errors.push(CompilerError::new(
                            ErrorType::TypeError,
                            1,
                            1,
                            &format!("non-void function must return a {return_type:?}"),
                        ));
                    }
                }
            },
            Statement::Expression(expr) => {
                self.check_expr(expr, local_scope);
            }
            Statement::VarDeclaration {
                initializer,
                name,
                var_type,
            } => {
                if let Some(init) = initializer {
                    // self.check_expr(init, local_scope );
                    match self.type_of(init, local_scope) {
                        Ok(init_ty) => {
                            if let Err(e) = self.check_assignable(var_type, &init_ty) {
                                self.errors.push(e);
                            }
                        }
                        Err(e) => self.errors.push(e),
                    }
                }
                // Declare the variable AFTER checking its initializer
                local_scope.declare_local(name.name.clone(), var_type.clone());
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.check_expr(condition, local_scope);
                self.check_statement(then_branch, local_scope, available_functions, return_type);
                if let Some(else_stmt) = else_branch {
                    self.check_statement(else_stmt, local_scope, available_functions, return_type);
                }
            }
            Statement::While {
                condition,
                then_branch,
            } => {
                self.check_expr(condition, local_scope);
                self.check_statement(then_branch, local_scope, available_functions, return_type);
            }
            Statement::DoWhile { body, condition } => {
                self.check_statement(body, local_scope, available_functions, return_type);
                self.check_expr(condition, local_scope);
            }
            Statement::For {
                counter_declaration,
                incrementor,
                condition,
                body,
            } => {
                // for loops create their own scope if they have a declaration
                let has_declaration = counter_declaration
                    .as_ref()
                    .map(|s| matches!(**s, Statement::VarDeclaration { .. }))
                    .unwrap_or(false);

                if has_declaration {
                    local_scope.push_scope();
                }

                if let Some(init) = counter_declaration {
                    self.check_statement(init, local_scope, available_functions, return_type);
                }
                if let Some(cond) = condition {
                    self.check_expr(cond, local_scope);
                }
                if let Some(inc) = incrementor {
                    self.check_expr(inc, local_scope);
                }
                self.check_statement(body, local_scope, available_functions, return_type);

                if has_declaration {
                    local_scope.pop_scope();
                }
            }
            _ => {}
        }
    }
}

/// Numeric "rank" for promotion. Higher rank wins in a binary operation.
/// Encodes: Char < Int < Long  and  Float < Double, with the integer
/// family promoting into the float family when mixed.
/// Returns None for types that have no numeric rank (Void, Pointer).
fn numeric_rank(t: &Types) -> Option<u8> {
    match t {
        Types::Char => Some(0),
        Types::Int => Some(1),
        Types::Long => Some(2),
        Types::Float => Some(3),
        Types::Double => Some(4),
        Types::Void | Types::Pointer(_) => None,
    }
}
/// The core promotion rule. Given two operand types, returns the single
/// type both should be converted to, or None if they can't be unified.
fn unify(left: &Types, right: &Types) -> Option<Types> {
    if left == right {
        return Some(left.clone());
    }
    // Pointers only unify with an identical pointer type (handled by the
    // equality check above). Mixing a pointer with anything else is illegal.
    if matches!(left, Types::Pointer(_)) || matches!(right, Types::Pointer(_)) {
        return None;
    }
    let lr = numeric_rank(left)?;
    let rr = numeric_rank(right)?;
    Some(if lr >= rr {
        left.clone()
    } else {
        right.clone()
    })
}
// tracks local and global variable scopes
struct LocalScope {
    /// Stack of scopes, each one contains a (local variable name -> its type)
    scopes: Vec<HashMap<String, Types>>,
    /// global variables
    globals: HashMap<String, Types>,
}

impl LocalScope {
    fn new(globals: HashMap<String, Types>) -> Self {
        Self {
            scopes: vec![HashMap::new()],
            globals,
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    fn lookup_type(&self, name: &str) -> Option<&Types> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }
    fn lookup_type_in_globals(&self, name: &str) -> Option<&Types> {
        self.globals.get(name)
    }

    fn declare_local(&mut self, name: String, type_: Types) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, type_);
        }
    }

    fn is_declared(&self, name: &str) -> bool {
        if self.globals.get(name).is_some() {
            return true;
        }

        for scope in self.scopes.iter().rev() {
            if scope.get(name).is_some() {
                return true;
            }
        }

        false
    }
}
