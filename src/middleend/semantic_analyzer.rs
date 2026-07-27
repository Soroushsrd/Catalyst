use crate::{
    errors::{CompilerError, ErrorType},
    frontend::parser::*,
};
use std::collections::HashMap;

/// - check variable/function declaration order
/// - validate scoping rules
/// - detect use-before-declaration errors
/// - validate function signatures match between declaration and definition
/// - Stamps TypedExpr with their type
pub struct SemanticAnalyzer {
    declared_functions: HashMap<String, Function>,
    errors: Vec<CompilerError>,
}

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            declared_functions: HashMap::new(),
            errors: Vec::new(),
        }
    }

    /// Runs the full semantic pass over a parsed program.
    ///
    /// Three phases, in order, because each depends on the previous:
    ///
    /// 1. **Collect all function signatures** into `declared_functions` up front.
    ///    This is a deliberate hoist: every function becomes visible to every
    ///    other function regardless of source order, so `main` can call a helper
    ///    defined below it without a forward declaration. This is more permissive
    ///    than C99 (which errors on calls to not-yet-declared functions), but it
    ///    matches how most toy compilers behave and keeps test programs painless
    ///    to write. The forward-declaration or definition signature-matching that
    ///    C requires is handled earlier, in the parser's `validate_function_decs`.
    ///
    /// 2. **Check global initializers** against globals seen *so far*. Globals are
    ///    added to `seen_globals` incrementally, so an initializer referencing a
    ///    global declared later in the file fails. this is correct C, where a
    ///    global initializer can only see globals defined above it. this
    ///    ordering rule applies ONLY to global initializers. function bodies
    ///    see all globals at once.
    ///
    /// 3. **Check every function body** with a fresh scope seeded with all globals
    ///    plus that function's parameters. Forward declarations are skipped. they
    ///    have no body to check.
    ///
    /// Errors are accumulated rather than short-circuited, so a single run reports
    /// as many problems as it can find.
    ///
    /// ## TODO: Known gaps
    /// - Duplicate function names silently overwrite in `declared_functions`
    ///   (last one wins). Harmless for now only because the parser rejects
    ///   redefinitions before this runs. this code does not defend against it.
    /// - No check that a nonvoid function actually returns on every path. a
    ///   function can fall off the end and only `Return` statements are validated.
    /// - `main`'s signature is not validated here (the parser only checks it exists).
    pub fn analyze(&mut self, program: &mut Program) -> Result<(), Vec<CompilerError>> {
        for function in &program.function_def {
            self.declared_functions
                .insert(function.name.name.clone(), function.clone());
        }

        let mut seen_globals: HashMap<String, Types> = HashMap::new();
        for global in program.global_vars.as_mut_slice() {
            if let Some(init) = global.initializer.as_mut() {
                let local_scope = LocalScope::new(&seen_globals);
                if let Err(e) = self.type_of(init, &local_scope) {
                    self.errors.push(e);
                }
            }
            seen_globals.insert(global.name.name.clone(), global._type.clone());
        }

        let globals: HashMap<String, Types> = program
            .global_vars
            .iter()
            .map(|g| (g.name.name.clone(), g._type.clone()))
            .collect();
        for function in &mut program.function_def {
            if !function.forward_dec {
                let mut local_scope = LocalScope::new(&globals);

                // Adding function parameters to local scope
                for param in &function.parameters {
                    if let Some(name) = &param.name {
                        local_scope.declare_local(name.name.clone(), param.parameter_type.clone());
                    }
                }

                self.check_statement(&mut function.body, &mut local_scope, &function.return_type);
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    /// Computes the type of an expression, checking it for validity along the way.
    ///
    /// This is the core of the type checker. It's written as a single recursive
    /// walk that returns the expression's type on success, or the first error it
    /// hits on failure (expression checking short circuits. unlike statement
    /// checking, which accumulates). Every arm both *derives* a type and *enforces*
    /// the rules for that construct, so calling `type_of` on an expression is how
    /// i validate it.
    ///
    /// ## TODO: Known gaps
    /// - Every numeric literal is typed Int, even `'0'` style or float looking
    ///   values. the lexer already distinguishes these but that information is
    ///   dropped here. This makes for example `long x = <big literal>` underchecked.
    /// - Ternary demands exact type equality instead of unifying the branches, so
    ///   `cond ? int_val : char_val` is rejected even though C would promote it.
    /// - No line/column on any error. every `CompilerError` is hard coded to
    ///   (1, 1) because expressions don't carry span info through the AST yet.
    /// - Pointer arithmetic and array/index typing are unhandled (`unify` refuses
    ///   to mix pointers with anything), so `ptr + 1` won't type check.
    /// - `LogicalNot`/`BitwiseNot` pass the operand type through unchanged rather
    ///   than normalizing to Int, which is loose relative to C semantics.
    fn type_of(
        &mut self,
        expr: &mut TypedExpr,
        scope: &LocalScope,
    ) -> Result<Types, CompilerError> {
        let span = expr.span;
        let t = match &mut expr.kind {
            // TODO: a function to analyuze the type of the number here
            Expression::Number(_) => Types::Int,
            Expression::CharLiteral(_) => Types::Char,
            Expression::Identifier(ident) => scope
                .lookup_type(ident)
                .ok_or(
                    CompilerError::new(
                        ErrorType::TypeError,
                        span.line,
                        span.column,
                        &format!("failed to find {ident} in local scope"),
                    )
                    .with_suggestion("Are you sure you defined the variable?"),
                )?
                .clone(),
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
                        match self.check_comparable(&left_type, &right_type) {
                            Some(t) => t,
                            None => {
                                return Err(CompilerError::new(
                                    ErrorType::TypeError,
                                    span.line,
                                    span.column,
                                    &format!("cannot compare {left_type:?} with {right_type:?}"),
                                ));
                            }
                        }
                        // Types::Int
                    }
                    _ => match self.unify_arithmetic(&left_type, &right_type) {
                        Some(t) => t,
                        None => {
                            return Err(CompilerError::new(
                                ErrorType::TypeError,
                                span.line,
                                span.column,
                                "left and right of the arithmetic dont have the same type",
                            ));
                        }
                    },
                }
            }
            Expression::AddressOf(expr) => {
                let inner_type = self.type_of(expr, scope)?;
                Types::Pointer(Box::new(inner_type))
            }
            Expression::Dereference(expr) => match self.type_of(expr, scope)? {
                Types::Pointer(inner) => *inner,
                _ => {
                    return Err(CompilerError::new(
                        ErrorType::TypeError,
                        span.line,
                        span.column,
                        "Only pointers can be dereferenced",
                    ));
                }
            },
            Expression::Assignment { target, value } => {
                let t_type = self.type_of(target, scope)?;
                let v_type = self.type_of(value, scope)?;
                match self.check_assignable(&t_type, &v_type) {
                    Some(t) => t,
                    None => {
                        return Err(CompilerError::new(
                            ErrorType::TypeError,
                            span.line,
                            span.column,
                            &format!("cannot assign {v_type:?} to {t_type:?}"),
                        ));
                    }
                }
            }
            Expression::FunctionCall { name, arguments } => {
                let (param_types, return_type) = match self.declared_functions.get(name) {
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
                            span.line,
                            span.column,
                            &format!("call to undeclared function '{name}'"),
                        ));
                    }
                };
                // arity check
                if arguments.len() != param_types.len() {
                    return Err(CompilerError::new(
                        ErrorType::TypeError,
                        span.line,
                        span.column,
                        &format!(
                            "function '{name}' expects {} argument(s), got {}",
                            param_types.len(),
                            arguments.len()
                        ),
                    )
                    .with_suggestion("check the number of arguments"));
                }
                for (i, (arg, expected)) in arguments.iter_mut().zip(param_types.iter()).enumerate()
                {
                    let actual = self.type_of(arg, scope)?;
                    match self.check_assignable(expected, &actual) {
                        Some(_) => {}
                        None => {
                            return Err(CompilerError::new(
                                ErrorType::TypeError,
                                span.line,
                                span.column,
                                &format!(
                                    "argument {} of '{name}': expected {expected:?}, got {actual:?}",
                                    i + 1
                                ),
                            ));
                        }
                    }
                }
                return_type
            }

            Expression::TernaryOP {
                condition: _,
                true_expr,
                false_expr,
            } => {
                let true_t = self.type_of(true_expr, scope)?;
                let false_t = self.type_of(false_expr, scope)?;
                if true_t != false_t {
                    return Err(CompilerError::new(
                        ErrorType::TypeError,
                        span.line,
                        span.column,
                        "ternary operation represents different types as outcome",
                    ));
                }
                true_t
            }
            Expression::BitwiseNot(tt) => self.type_of(tt, scope)?,
            Expression::LogicalNot(tt) => self.type_of(tt, scope)?,
            Expression::UnaryMinus(tt) => self.type_of(tt, scope)?,
            Expression::Unknown => {
                return Err(CompilerError::new(
                    ErrorType::TypeError,
                    span.line,
                    span.column,
                    "Unknown type",
                ));
            }
        };
        expr.type_ = Some(t.clone());
        Ok(t)
    }

    /// Arithmetic binary ops (+ - * / %): result is the unified operand type.
    /// `a + b` where a:Int, b:Long  ->  Long
    fn unify_arithmetic(&self, left: &Types, right: &Types) -> Option<Types> {
        unify(left, right) // ok_or_else(|| {
    }

    fn check_assignable(&self, target_type: &Types, value_type: &Types) -> Option<Types> {
        unify(target_type, value_type)
    }

    // TODO: fix this. shouldnt return int for everything
    /// Comparison ops (== != < <= > >=): operands must unify, but the
    /// RESULT is always Int, regardless of operand type.
    /// `a < b` where a:Float, b:Int  ->  Int  (not Float)
    fn check_comparable(&self, left: &Types, right: &Types) -> Option<Types> {
        unify(left, right).map(|_| Types::Int)
    }

    fn check_expr(&mut self, expr: &mut TypedExpr, scope: &LocalScope) {
        if let Err(e) = self.type_of(expr, scope) {
            self.errors.push(e);
        }
    }

    fn check_statement(
        &mut self,
        stmt: &mut Statement,
        local_scope: &mut LocalScope,
        return_type: &Types,
    ) {
        match stmt {
            Statement::Block(stmts) => {
                local_scope.push_scope();
                for s in stmts {
                    self.check_statement(s, local_scope, return_type);
                }
                local_scope.pop_scope();
            }
            Statement::Return(expr) => match expr {
                Some(ex) => match self.type_of(ex, local_scope) {
                    Ok(actual) => match self.check_assignable(return_type, &actual) {
                        Some(_) => {}
                        None => {
                            self.errors.push(CompilerError::new(ErrorType::TypeError,ex.span.line,ex.span.column,&format!("return type {return_type:?} and expected type {actual:?} dont match")));
                        }
                    },
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
                        Ok(init_ty) => match self.check_assignable(var_type, &init_ty) {
                            Some(_) => {}
                            None => {
                                self.errors.push(CompilerError::new(
                                    ErrorType::TypeError,
                                    1,
                                    1,
                                    &format!("types of {var_type:?} and {init_ty:?} dont match"),
                                ));
                            }
                        },
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
                self.check_statement(then_branch, local_scope, return_type);
                if let Some(else_stmt) = else_branch {
                    self.check_statement(else_stmt, local_scope, return_type);
                }
            }
            Statement::While {
                condition,
                then_branch,
            } => {
                self.check_expr(condition, local_scope);
                self.check_statement(then_branch, local_scope, return_type);
            }
            Statement::DoWhile { body, condition } => {
                self.check_statement(body, local_scope, return_type);
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
                    self.check_statement(init, local_scope, return_type);
                }
                if let Some(cond) = condition {
                    self.check_expr(cond, local_scope);
                }
                if let Some(inc) = incrementor {
                    self.check_expr(inc, local_scope);
                }
                self.check_statement(body, local_scope, return_type);

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
struct LocalScope<'a> {
    /// Stack of scopes, each one contains a (local variable name -> its type)
    scopes: Vec<HashMap<String, Types>>,
    /// global variables
    globals: &'a HashMap<String, Types>,
}

impl<'a> LocalScope<'a> {
    fn new(globals: &'a HashMap<String, Types>) -> Self {
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
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name))
            .or_else(|| self.globals.get(name))
    }

    fn declare_local(&mut self, name: String, type_: Types) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, type_);
        }
    }
}
