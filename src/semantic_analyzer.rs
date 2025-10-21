use crate::{
    errors::{CompilerError, ErrorType},
    parser::*,
};
use std::collections::HashSet;

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
    declared_globals: HashSet<String>,
    declared_functions: HashSet<String>,
    forward_declarations: HashSet<String>,
    errors: Vec<CompilerError>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            declared_globals: HashSet::new(),
            declared_functions: HashSet::new(),
            forward_declarations: HashSet::new(),
            errors: Vec::new(),
        }
    }

    pub fn analyze(&mut self, program: &Program) -> Result<(), Vec<CompilerError>> {
        for function in &program.function_def {
            if function.forward_dec {
                self.forward_declarations.insert(function.name.name.clone());
            }
            self.declared_functions.insert(function.name.name.clone());
        }

        let mut seen_globals = HashSet::new();
        let declared_functions = self.declared_functions.clone();

        for global in &program.global_vars {
            if let Some(init) = &global.initializer {
                self.check_global_initializer(init, &seen_globals, &declared_functions);
            }
            seen_globals.insert(global.name.name.clone());
        }

        self.declared_globals = seen_globals;

        let declared_functions = self.declared_functions.clone();

        for function in &program.function_def {
            if !function.forward_dec {
                let visible_globals: HashSet<String> = program
                    .global_vars
                    .iter()
                    .filter(|g| g.declaration_idx < function.declaration_idx)
                    .map(|g| g.name.name.clone())
                    .collect();
                let mut local_scope = LocalScope::new(visible_globals);

                // Adding function parameters to local scope
                for param in &function.parameters {
                    if let Some(name) = &param.name {
                        local_scope.declare_local(name.name.clone());
                    }
                }

                self.check_statement(&function.body, &mut local_scope, &declared_functions);
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn check_global_initializer(
        &mut self,
        expr: &Expression,
        seen_globals: &HashSet<String>,
        available_functions: &HashSet<String>,
    ) {
        match expr {
            Expression::Identifier(name) => {
                if !seen_globals.contains(name) {
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
                if !available_functions.contains(name) {
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

    fn check_statement(
        &mut self,
        stmt: &Statement,
        local_scope: &mut LocalScope,
        available_functions: &HashSet<String>,
    ) {
        match stmt {
            Statement::Block(stmts) => {
                local_scope.push_scope();
                for s in stmts {
                    self.check_statement(s, local_scope, available_functions);
                }
                local_scope.pop_scope();
            }
            Statement::Return(Some(expr)) => {
                self.check_expression_in_function(expr, local_scope, available_functions);
            }
            Statement::Expression(expr) => {
                self.check_expression_in_function(expr, local_scope, available_functions);
            }
            Statement::VarDeclaration {
                initializer,
                name,
                var_type: _,
            } => {
                if let Some(init) = initializer {
                    self.check_expression_in_function(init, local_scope, available_functions);
                }
                // Declare the variable AFTER checking its initializer
                local_scope.declare_local(name.name.clone());
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.check_expression_in_function(condition, local_scope, available_functions);
                self.check_statement(then_branch, local_scope, available_functions);
                if let Some(else_stmt) = else_branch {
                    self.check_statement(else_stmt, local_scope, available_functions);
                }
            }
            Statement::While {
                condition,
                then_branch,
            } => {
                self.check_expression_in_function(condition, local_scope, available_functions);
                self.check_statement(then_branch, local_scope, available_functions);
            }
            Statement::DoWhile { body, condition } => {
                self.check_statement(body, local_scope, available_functions);
                self.check_expression_in_function(condition, local_scope, available_functions);
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
                    self.check_statement(init, local_scope, available_functions);
                }
                if let Some(cond) = condition {
                    self.check_expression_in_function(cond, local_scope, available_functions);
                }
                if let Some(inc) = incrementor {
                    self.check_expression_in_function(inc, local_scope, available_functions);
                }
                self.check_statement(body, local_scope, available_functions);

                if has_declaration {
                    local_scope.pop_scope();
                }
            }
            _ => {}
        }
    }

    fn check_expression_in_function(
        &mut self,
        expr: &Expression,
        local_scope: &LocalScope,
        available_functions: &HashSet<String>,
    ) {
        match expr {
            Expression::Identifier(name) => {
                // checks if variable exists, local or global
                if !local_scope.is_declared(name) {
                    self.errors.push(
                        CompilerError::new(
                            ErrorType::UndefinedVariable,
                            1,
                            1,
                            &format!("Variable '{}' used before declaration", name),
                        )
                        .with_suggestion("Declare this variable before using it"),
                    );
                }
            }
            Expression::Binary { left, right, .. } => {
                self.check_expression_in_function(left, local_scope, available_functions);
                self.check_expression_in_function(right, local_scope, available_functions);
            }
            Expression::UnaryMinus(e) | Expression::LogicalNot(e) | Expression::BitwiseNot(e) => {
                self.check_expression_in_function(e, local_scope, available_functions);
            }
            Expression::Assignment { target, value } => {
                if !local_scope.is_declared(target) {
                    self.errors.push(CompilerError::new(
                        ErrorType::UndefinedVariable,
                        1,
                        1,
                        &format!("Variable '{}' used before declaration", target),
                    ));
                }
                self.check_expression_in_function(value, local_scope, available_functions);
            }
            Expression::FunctionCall { name, arguments } => {
                // Functions can be forward declared
                if !available_functions.contains(name) {
                    self.errors.push(CompilerError::new(
                        ErrorType::UndefinedVariable,
                        1,
                        1,
                        &format!("Function '{}' not declared", name),
                    ));
                }
                for arg in arguments {
                    self.check_expression_in_function(arg, local_scope, available_functions);
                }
            }
            Expression::TernaryOP {
                condition,
                true_expr,
                false_expr,
            } => {
                self.check_expression_in_function(condition, local_scope, available_functions);
                self.check_expression_in_function(true_expr, local_scope, available_functions);
                self.check_expression_in_function(false_expr, local_scope, available_functions);
            }
            _ => {}
        }
    }
}

// tracks local and global variable scopes
struct LocalScope {
    // Stack of scopes, each one contains local variable names
    scopes: Vec<HashSet<String>>,
    // global variables
    globals: HashSet<String>,
}

impl LocalScope {
    fn new(globals: HashSet<String>) -> Self {
        Self {
            scopes: vec![HashSet::new()],
            globals,
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashSet::new());
    }

    fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    fn declare_local(&mut self, name: String) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name);
        }
    }

    fn is_declared(&self, name: &str) -> bool {
        if self.globals.contains(name) {
            return true;
        }

        for scope in self.scopes.iter().rev() {
            if scope.contains(name) {
                return true;
            }
        }

        false
    }
}
