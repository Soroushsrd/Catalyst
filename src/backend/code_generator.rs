use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{CodeModel, FileType, RelocMode, Target, TargetMachine};
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, FunctionValue, GlobalValue, IntValue};
use inkwell::{AddressSpace, IntPredicate, OptimizationLevel};

use crate::frontend::lexer::Num;
use crate::frontend::parser::*;
use std::collections::HashMap;
use std::path::Path;

#[derive(Clone)]
struct VariableInfo<'ctx> {
    pub value: BasicValueEnum<'ctx>,
    pub var_type: Types,
}

pub struct LLVMCodeGenerator<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    // a vec of symbol -> stack offset
    // used to keep track of symbol scopes
    scope_stack: Vec<HashMap<String, VariableInfo<'ctx>>>,
    // to be used when we want to break out of a loop
    loop_exit_stack: Vec<BasicBlock<'ctx>>,
    // to be used when a continue keyword is used!
    loop_cont_stack: Vec<BasicBlock<'ctx>>,
    // current function being compiled
    current_function: Option<FunctionValue<'ctx>>,
    function_table: HashMap<String, FunctionValue<'ctx>>,
    global_vars: HashMap<String, GlobalValue<'ctx>>,
}

impl<'ctx> LLVMCodeGenerator<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        Self {
            context,
            module,
            builder,
            scope_stack: vec![HashMap::new()],
            loop_exit_stack: vec![],
            loop_cont_stack: vec![],
            current_function: None,
            function_table: HashMap::new(),
            global_vars: HashMap::new(),
        }
    }

    /// prints the generated output into a file.
    /// generate LLVM IR for the entire program
    pub fn generate_program(&mut self, program: &Program) -> Result<(), String> {
        for global in &program.global_vars {
            self.declare_global_variable(global)?;
        }

        self.declare_functions(&program.function_def)?;
        for function in &program.function_def {
            self.generate_function(function)?;
        }

        if let Err(e) = self.module.verify() {
            return Err(format!("LLVM module verification failed: {e}"));
        }
        Ok(())
    }

    fn declare_global_variable(&mut self, global: &GlobalVariable) -> Result<(), String> {
        let llvm_type = self.llvm_type_from_ast(&global._type)?;

        let global_var = self.module.add_global(llvm_type, None, &global.name.name);

        let initializer: BasicValueEnum = if let Some(init_expr) = &global.initializer {
            let as_u64 = |num: &Num| -> u64 {
                match num {
                    Num::Int(v) => *v,
                    Num::Float(v) => *v as u64,
                }
            };

            let as_f64 = |num: &Num| -> f64 {
                match num {
                    Num::Int(v) => *v as f64,
                    Num::Float(v) => *v,
                }
            };
            // For global variables, initializers must be constants
            match &init_expr.kind {
                Expression::Number { value: val } => match &global._type {
                    Types::Int => self.context.i32_type().const_int(as_u64(val), false).into(),
                    Types::UInt => self.context.i32_type().const_int(as_u64(val), false).into(),
                    Types::Long => self.context.i64_type().const_int(as_u64(val), false).into(),
                    Types::ULong => self.context.i64_type().const_int(as_u64(val), false).into(),
                    Types::Char => self.context.i8_type().const_int(as_u64(val), false).into(),
                    Types::UChar => self.context.i8_type().const_int(as_u64(val), false).into(),
                    Types::Float => self.context.f32_type().const_float(as_f64(val)).into(),
                    Types::Double => self.context.f64_type().const_float(as_f64(val)).into(),
                    _ => return Err("Unsupported global variable type".to_string()),
                },
                Expression::CharLiteral(ch) => match &global._type {
                    Types::Char | Types::UChar => {
                        self.context.i8_type().const_int(*ch as u64, false).into()
                    }
                    _ => return Err("char literal can only initialize a char global".to_string()),
                },
                _ => {
                    return Err(
                        "Global variable initializers must be constant expressions".to_string()
                    );
                }
            }
        } else {
            // Default zero initialization
            self.get_zero_value(&global._type)?
        };

        global_var.set_initializer(&initializer);

        // Store in our global variables map for lookup
        self.global_vars
            .insert(global.name.name.clone(), global_var);

        Ok(())
    }

    fn declare_functions(&mut self, functions: &[Function]) -> Result<(), String> {
        for function in functions {
            let param_types: Result<Vec<BasicTypeEnum>, String> = function
                .parameters
                .iter()
                .filter_map(|p| p.name.as_ref().map(|_| &p.parameter_type))
                .map(|t| self.llvm_type_from_ast(t))
                .collect();
            let param_types = param_types?.iter().map(|b| (*b).into()).collect::<Vec<_>>();

            let fn_type = if matches!(function.return_type, Types::Void) {
                self.context.void_type().fn_type(&param_types, false)
            } else {
                let return_types = self.llvm_type_from_ast(&function.return_type)?;
                return_types.fn_type(&param_types, false)
            };

            let fn_value = self.module.add_function(&function.name.name, fn_type, None);

            for (i, param) in function.parameters.iter().enumerate() {
                if let Some(name) = &param.name {
                    fn_value
                        .get_nth_param(i as u32)
                        .unwrap()
                        .set_name(&name.name);
                }
            }

            self.function_table
                .insert(function.name.name.clone(), fn_value);
        }
        Ok(())
    }

    fn generate_function(&mut self, function: &Function) -> Result<(), String> {
        // all functions are delcared in declare_functions method. so i shouldnt generate anything
        // in here for forward declarations
        if function.forward_dec {
            return Ok(());
        }

        let fn_value = self.function_table[&function.name.name];
        self.current_function = Some(fn_value);

        let entry_block = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(entry_block);

        self.push_scope();

        for (i, param) in function.parameters.iter().enumerate() {
            if let Some(name) = &param.name {
                let param_value = fn_value.get_nth_param(i as u32).unwrap();

                let param_type = self.llvm_type_from_ast(&param.parameter_type)?;
                let alloca = self
                    .builder
                    .build_alloca(param_type, &name.name)
                    .map_err(|e| format!("failed to create alloca: {e}"))?;

                self.builder
                    .build_store(alloca, param_value)
                    .map_err(|e| format!("failed to store parameter: {e}"))?;

                self.declare_variable(
                    &name.name,
                    VariableInfo {
                        value: alloca.into(),
                        var_type: param.parameter_type.clone(),
                    },
                );
            }
        }

        if let Statement::Block(statements) = &function.body {
            for stmt in statements {
                self.generate_statement(stmt)?;
                if self.current_block_terminator() {
                    break;
                }
            }
        } else {
            return Err("function must have a body".to_string());
        }

        if !self.current_block_terminator() {
            if matches!(function.return_type, Types::Void) {
                self.builder
                    .build_return(None)
                    .map_err(|e| format!("failed to build return: {e}"))?;
            } else {
                let zero = self.get_zero_value(&function.return_type)?;
                self.builder
                    .build_return(Some(&zero))
                    .map_err(|e| format!("failed to build return: {e}"))?;
            }
        }

        self.pop_scope();
        self.current_function = None;

        Ok(())
    }

    fn generate_statement(&mut self, statement: &Statement) -> Result<(), String> {
        match statement {
            Statement::Empty => {
                println!("came accross an empty block");
            }
            Statement::Block(statements) => {
                self.push_scope();
                for stmt in statements {
                    self.generate_statement(stmt)?;
                    if self.current_block_terminator() {
                        break;
                    }
                }
                self.pop_scope();
            }
            Statement::Return(expr) => {
                if let Some(expression) = expr {
                    let value = self.generate_expressions(expression)?;
                    // coerce to the function's actual return type
                    let fn_return_type =
                        self.current_function.unwrap().get_type().get_return_type();

                    let coerced = if let (
                        BasicValueEnum::IntValue(iv),
                        Some(BasicTypeEnum::IntType(ret_type)),
                    ) = (value, fn_return_type)
                    {
                        let vw = iv.get_type().get_bit_width();
                        let rw = ret_type.get_bit_width();
                        if vw < rw {
                            self.builder
                                .build_int_z_extend(iv, ret_type, "ret_ext")
                                .map_err(|e| format!("failed to extend return value: {e}"))?
                                .into()
                        } else if vw > rw {
                            self.builder
                                .build_int_truncate(iv, ret_type, "ret_trunc")
                                .map_err(|e| format!("failed to truncate return value: {e}"))?
                                .into()
                        } else {
                            value
                        }
                    } else {
                        value
                    };

                    self.builder
                        .build_return(Some(&coerced))
                        .map_err(|e| format!("failed to build return: {e}"))?;
                } else {
                    self.builder
                        .build_return(None)
                        .map_err(|e| format!("failed to build return: {e}"))?;
                }
            }
            Statement::Break => {
                if let Some(exit_block) = self.loop_exit_stack.last() {
                    self.builder
                        .build_unconditional_branch(*exit_block)
                        .map_err(|e| format!("failed to build break branch: {e}"))?;
                } else {
                    return Err("break statement used outside a loop".to_string());
                }
            }
            Statement::Continue => {
                if let Some(cont_block) = self.loop_cont_stack.last() {
                    self.builder
                        .build_unconditional_branch(*cont_block)
                        .map_err(|e| format!("failed to build branch: {e}"))?;
                } else {
                    return Err("Continue statement used outside a loop".to_string());
                }
            }
            Statement::Expression(expr) => {
                self.generate_expressions(expr)?;
            }
            Statement::VarDeclaration {
                var_type,
                name,
                initializer,
            } => {
                self.generate_var_declaration(var_type, name, initializer)?;
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.generate_if_statement(condition, then_branch, else_branch)?;
            }

            Statement::While {
                condition,
                then_branch,
            } => {
                self.generate_while_statement(condition, then_branch)?;
            }

            Statement::DoWhile { body, condition } => {
                self.generate_do_while_statement(body, condition)?;
            }
            Statement::For {
                counter_declaration,
                incrementor,
                condition,
                body,
            } => {
                self.generate_for_statement(counter_declaration, incrementor, condition, body)?;
            }
        }
        Ok(())
    }

    fn generate_for_statement(
        &mut self,
        counter_declaration: &Option<Box<Statement>>,
        incrementor: &Option<TypedExpr>,
        condition: &Option<TypedExpr>,
        body: &Statement,
    ) -> Result<(), String> {
        let loop_init = self
            .context
            .append_basic_block(self.current_function.unwrap(), "for_init");
        let loop_cond = self
            .context
            .append_basic_block(self.current_function.unwrap(), "for_cond");
        let loop_body = self
            .context
            .append_basic_block(self.current_function.unwrap(), "for_body");
        let loop_incr = self
            .context
            .append_basic_block(self.current_function.unwrap(), "for_incr");
        let loop_end = self
            .context
            .append_basic_block(self.current_function.unwrap(), "for_end");

        self.push_loop_context(loop_end, loop_incr);

        self.builder
            .build_unconditional_branch(loop_init)
            .map_err(|e| format!("failed to build init branch: {e}"))?;

        self.builder.position_at_end(loop_init);

        let has_declaration = if let Some(init) = counter_declaration {
            matches!(**init, Statement::VarDeclaration { .. })
        } else {
            false
        };

        if has_declaration {
            self.push_scope();
        }

        if let Some(init_stmnt) = counter_declaration {
            self.generate_statement(init_stmnt)?;
        }

        self.builder
            .build_unconditional_branch(loop_cond)
            .map_err(|e| format!("failed to build branch: {e}"))?;
        self.builder.position_at_end(loop_cond);

        if let Some(cond_expr) = condition {
            let condition_value = self.generate_expressions(cond_expr)?;
            let condition_bool = self.build_not_zero(condition_value)?;
            self.builder
                .build_conditional_branch(condition_bool, loop_body, loop_end)
                .map_err(|e| format!("failed to build conditional branch: {e}"))?;
        } else {
            self.builder
                .build_unconditional_branch(loop_body)
                .map_err(|e| format!("failed to build branch: {e}"))?;
        }

        self.builder.position_at_end(loop_body);
        self.generate_statement(body)?;
        if !self.current_block_terminator() {
            self.builder
                .build_unconditional_branch(loop_incr)
                .map_err(|e| format!("failed to build branch: {e}"))?;
        }

        self.builder.position_at_end(loop_incr);
        if let Some(incr_expr) = incrementor {
            self.generate_expressions(incr_expr)?;
        }
        self.builder
            .build_unconditional_branch(loop_cond)
            .map_err(|e| format!("failed to build branch: {e}"))?;

        if has_declaration {
            self.pop_scope();
        }
        self.pop_loop_context();
        self.builder.position_at_end(loop_end);

        Ok(())
    }

    fn generate_var_declaration(
        &mut self,
        var_type: &Types,
        name: &Identifier,
        initializer: &Option<TypedExpr>,
    ) -> Result<(), String> {
        if let Some(current_scope) = self.scope_stack.last()
            && current_scope.contains_key(&name.name)
        {
            return Err(format!(
                "variable {} already declared in this scope",
                name.name
            ));
        }
        let llvm_type = self.llvm_type_from_ast(var_type)?;
        let alloca = if let Some(func) = self.current_function {
            let entry_block = func.get_first_basic_block().unwrap();
            let current_block = self.builder.get_insert_block().unwrap();

            // Temporarily position at the start of entry block
            if let Some(first_instr) = entry_block.get_first_instruction() {
                self.builder.position_before(&first_instr);
            } else {
                self.builder.position_at_end(entry_block);
            }

            let alloca = self
                .builder
                .build_alloca(llvm_type, &name.name)
                .map_err(|e| format!("failed to build alloca: {e}"))?;

            // Restore position
            self.builder.position_at_end(current_block);

            alloca
        } else {
            return Err("No current function".to_string());
        };
        if let Some(init_expr) = initializer {
            let init_value = self.generate_expressions(init_expr)?;
            let init_value = self.coerce_for_store(init_value, var_type)?;
            self.builder
                .build_store(alloca, init_value)
                .map_err(|e| format!("failed to store initial value: {e}"))?;
        } else {
            let zero = self.get_zero_value(var_type)?;
            self.builder
                .build_store(alloca, zero)
                .map_err(|e| format!("failed to store zero value: {e}"))?;
        }

        self.declare_variable(
            &name.name,
            VariableInfo {
                value: alloca.into(),
                var_type: var_type.clone(),
            },
        );
        Ok(())
    }

    fn generate_if_statement(
        &mut self,
        condition: &TypedExpr,
        then_branch: &Statement,
        else_branch: &Option<Box<Statement>>,
    ) -> Result<(), String> {
        let condition_value = self.generate_expressions(condition)?;
        let condition_bool = self.build_not_zero(condition_value)?;

        let then_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "then");
        let else_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "else");
        let merge_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "merge");

        self.builder
            .build_conditional_branch(condition_bool, then_block, else_block)
            .map_err(|e| format!("Failed to build conditional branch: {:?}", e))?;

        self.builder.position_at_end(then_block);
        self.generate_statement(then_branch)?;
        if !self.current_block_terminator() {
            self.builder
                .build_unconditional_branch(merge_block)
                .map_err(|e| format!("Failed to build branch: {:?}", e))?;
        }

        self.builder.position_at_end(else_block);

        if let Some(else_stmt) = else_branch {
            self.generate_statement(else_stmt)?;
        }
        if !self.current_block_terminator() {
            self.builder
                .build_unconditional_branch(merge_block)
                .map_err(|e| format!("Failed to build branch: {:?}", e))?;
        }

        self.builder.position_at_end(merge_block);
        Ok(())
    }

    fn generate_do_while_statement(
        &mut self,
        body: &Statement,
        condition: &TypedExpr,
    ) -> Result<(), String> {
        let loop_body = self
            .context
            .append_basic_block(self.current_function.unwrap(), "do_body");
        let loop_cond = self
            .context
            .append_basic_block(self.current_function.unwrap(), "do_cond");
        let loop_end = self
            .context
            .append_basic_block(self.current_function.unwrap(), "do_end");

        self.push_loop_context(loop_end, loop_body);

        self.builder
            .build_unconditional_branch(loop_body)
            .map_err(|e| format!("Failed to build branch: {:?}", e))?;

        self.builder.position_at_end(loop_body);
        self.generate_statement(body)?;
        if !self.current_block_terminator() {
            self.builder
                .build_unconditional_branch(loop_cond)
                .map_err(|e| format!("Failed to build branch: {:?}", e))?;
        }

        self.builder.position_at_end(loop_cond);
        let condition_value = self.generate_expressions(condition)?;
        let condition_bool = self.build_not_zero(condition_value)?;
        self.builder
            .build_conditional_branch(condition_bool, loop_body, loop_end)
            .map_err(|e| format!("Failed to build conditional branch: {:?}", e))?;

        self.pop_loop_context();
        self.builder.position_at_end(loop_end);
        Ok(())
    }

    fn generate_while_statement(
        &mut self,
        condition: &TypedExpr,
        then_branch: &Statement,
    ) -> Result<(), String> {
        let loop_cond = self
            .context
            .append_basic_block(self.current_function.unwrap(), "loop_cond");
        let loop_body = self
            .context
            .append_basic_block(self.current_function.unwrap(), "loop_body");
        let loop_end = self
            .context
            .append_basic_block(self.current_function.unwrap(), "loop_end");

        self.push_loop_context(loop_end, loop_body);

        self.builder
            .build_unconditional_branch(loop_cond)
            .map_err(|e| format!("Failed to build branch: {:?}", e))?;

        self.builder.position_at_end(loop_cond);
        let condition_value = self.generate_expressions(condition)?;
        let condition_bool = self.build_not_zero(condition_value)?;
        self.builder
            .build_conditional_branch(condition_bool, loop_body, loop_end)
            .map_err(|e| format!("Failed to build conditional branch: {:?}", e))?;

        self.builder.position_at_end(loop_body);
        self.generate_statement(then_branch)?;
        if !self.current_block_terminator() {
            self.builder
                .build_unconditional_branch(loop_cond)
                .map_err(|e| format!("Failed to build branch: {:?}", e))?;
        }

        self.pop_loop_context();
        self.builder.position_at_end(loop_end);
        Ok(())
    }

    /// generate llvm IR for expressions
    fn generate_expressions(
        &mut self,
        expression: &TypedExpr,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        match &expression.kind {
            Expression::Number { value } => match value {
                Num::Int(v) => Ok(self.context.i32_type().const_int(*v as u64, false).into()),
                Num::Float(v) => Ok(self.context.f64_type().const_float(*v).into()),
            },
            Expression::Identifier(ident) => self.generate_identifier(ident),
            Expression::CharLiteral(ch) => {
                Ok(self.context.i8_type().const_int(*ch as u64, false).into())
            }
            Expression::Binary {
                left,
                operator,
                right,
            } => self.generate_binary_op(left, operator, right),
            Expression::UnaryMinus(expr) => self.generate_unary_minus(expr),
            Expression::LogicalNot(expr) => self.generate_logical_not(expr),
            Expression::BitwiseNot(expr) => self.generate_bitwise_not(expr),
            Expression::Assignment { target, value } => self.generate_assignment(target, value),
            Expression::FunctionCall { name, arguments } => {
                self.generate_function_call(name, arguments)
            }
            Expression::TernaryOP {
                condition,
                true_expr,
                false_expr,
            } => self.generate_ternary_op(condition, true_expr, false_expr),
            Expression::Unknown => Ok(self.context.i32_type().const_int(0, false).into()),
            Expression::AddressOf(inner) => self.generate_address_of(inner),
            Expression::Dereference(inner) => self.generate_dereference(inner),
        }
    }

    fn generate_dereference(&mut self, expr: &TypedExpr) -> Result<BasicValueEnum<'ctx>, String> {
        let load_type = self.get_deref_load_type(expr)?;
        let ptr_value = self.generate_expressions(expr)?;
        let ptr = ptr_value.into_pointer_value();

        let loaded = self
            .builder
            .build_load(load_type, ptr, "deref")
            .map_err(|e| format!("failed to dereference the pointer: {e}"))?;

        Ok(loaded)
    }
    /// Determines what LLVM type to load when dereferencing an expression.
    fn get_deref_load_type(&self, expr: &TypedExpr) -> Result<BasicTypeEnum<'ctx>, String> {
        match &expr.type_ {
            Some(Types::Pointer(inner)) => self.llvm_type_from_ast(inner),
            Some(other) => Err(format!("cannot deref non pointer types: {other:?}")),
            None => Err("dereference target has no resolved type".into()),
        }
    }

    fn generate_address_of(&mut self, expr: &TypedExpr) -> Result<BasicValueEnum<'ctx>, String> {
        match expr.kind {
            Expression::Identifier(ref name) => {
                // pointer to the variable (alloca or global)
                if let Some(var_info) = self.lookup_variable(name) {
                    // already a pointer
                    return Ok(var_info.value);
                }
                if let Some(global_var) = self.global_vars.get(name) {
                    return Ok(global_var.as_pointer_value().into());
                }
                Err(format!("Undefined variable: {}", name))
            }
            Expression::Dereference(ref inner) => {
                // &(*ptr) = ptr, just evaluatin the pointer
                self.generate_expressions(inner)
            }
            _ => Err("Cannot take address of non-lvalue".to_string()),
        }
    }

    fn generate_binary_op(
        &mut self,
        left: &TypedExpr,
        operator: &BinaryOperator,
        right: &TypedExpr,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        match operator {
            BinaryOperator::And => {
                let left_val = self.generate_expressions(left)?;
                let left_bool = self.build_not_zero(left_val)?;
                let left_block = self.builder.get_insert_block().unwrap();

                let right_block = self
                    .context
                    .append_basic_block(self.current_function.unwrap(), "and_right");
                let merge_block = self
                    .context
                    .append_basic_block(self.current_function.unwrap(), "and_merge");
                self.builder
                    .build_conditional_branch(left_bool, right_block, merge_block)
                    .map_err(|e| format!("failed to build conditional branch: {e}"))?;

                self.builder.position_at_end(right_block);

                let right_val = self.generate_expressions(right)?;
                let right_bool = self.build_not_zero(right_val)?;
                self.builder
                    .build_unconditional_branch(merge_block)
                    .map_err(|e| format!("failed to build branch: {e}"))?;
                let right_end_block = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(merge_block);
                let phi = self
                    .builder
                    .build_phi(self.context.bool_type(), "and_result")
                    .map_err(|e| format!("failed to build phi: {e}"))?;

                let false_val = self.context.bool_type().const_int(0, false);

                phi.add_incoming(&[(&false_val, left_block)]);
                phi.add_incoming(&[(&right_bool, right_end_block)]);

                let result = self
                    .builder
                    .build_int_z_extend(
                        phi.as_basic_value().into_int_value(),
                        self.context.i32_type(),
                        "and_to_int",
                    )
                    .map_err(|e| format!("failed to extend boolean: {e}"))?;
                Ok(result.into())
                // Ok(phi.as_basic_value())
            }
            BinaryOperator::Or => {
                let left_val = self.generate_expressions(left)?;
                let left_bool = self.build_not_zero(left_val)?;
                let left_block = self.builder.get_insert_block().unwrap();

                let right_block = self
                    .context
                    .append_basic_block(self.current_function.unwrap(), "or_right");
                let merge_block = self
                    .context
                    .append_basic_block(self.current_function.unwrap(), "or_merge");

                self.builder
                    .build_conditional_branch(left_bool, merge_block, right_block)
                    .map_err(|e| format!("failed to create conditional branch: {e}"))?;

                self.builder.position_at_end(right_block);

                let right_val = self.generate_expressions(right)?;
                let right_bool = self.build_not_zero(right_val)?;

                self.builder
                    .build_unconditional_branch(merge_block)
                    .map_err(|e| format!("failed to create unconditional branch: {e}"))?;

                let right_end_block = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(merge_block);

                let phi = self
                    .builder
                    .build_phi(self.context.bool_type(), "or_result")
                    .map_err(|e| format!("failed to build phi : {e}"))?;

                let true_val = self.context.bool_type().const_int(1, false);
                phi.add_incoming(&[(&true_val, left_block)]);
                phi.add_incoming(&[(&right_bool, right_end_block)]);

                let result = self
                    .builder
                    .build_int_z_extend(
                        phi.as_basic_value().into_int_value(),
                        self.context.i32_type(),
                        "or_into_int",
                    )
                    .map_err(|e| format!("failed to extend boolean: {e}"))?;

                Ok(result.into())
            }
            _ => {
                // TODO: add ptr arithmetics in here!
                let left_val = self.generate_expressions(left)?;
                let right_val = self.generate_expressions(right)?;
                let (lv, rv) =
                    self.coerce_to_int(left_val.into_int_value(), right_val.into_int_value());

                match operator {
                    BinaryOperator::Add => Ok(self
                        .builder
                        .build_int_add(lv, rv, "add")
                        .map_err(|e| format!("failed to build add: {e}"))?
                        .into()),
                    BinaryOperator::Subtract => Ok(self
                        .builder
                        .build_int_sub(lv, rv, "subtract")
                        .map_err(|e| format!("failed to build subtract: {e}"))?
                        .into()),
                    BinaryOperator::Multiply => Ok(self
                        .builder
                        .build_int_mul(lv, rv, "multiply")
                        .map_err(|e| format!("failed to build multiplication: {e}"))?
                        .into()),
                    BinaryOperator::Divide => Ok(self
                        .builder
                        .build_int_signed_div(lv, rv, "divide")
                        .map_err(|e| format!("failed to build division: {e}"))?
                        .into()),
                    BinaryOperator::Mod => Ok(self
                        .builder
                        .build_int_signed_rem(lv, rv, "remainder")
                        .map_err(|e| format!("failed to build remainder: {e}"))?
                        .into()),
                    BinaryOperator::Equals => {
                        let ret = self
                            .builder
                            .build_int_compare(IntPredicate::EQ, lv, rv, "eq")
                            .map_err(|e| format!("Failed to build equals: {:?}", e))?;
                        let result = self
                            .builder
                            .build_int_z_extend(ret, self.context.i32_type(), "eq_into_int")
                            .map_err(|e| format!("failed to extend boolean: {e}"))?;
                        Ok(result.into())
                    }
                    BinaryOperator::NotEquals => {
                        let ret = self
                            .builder
                            .build_int_compare(IntPredicate::NE, lv, rv, "ne")
                            .map_err(|e| format!("Failed to build ne: {:?}", e))?;
                        let result = self
                            .builder
                            .build_int_z_extend(ret, self.context.i32_type(), "ne_into_int")
                            .map_err(|e| format!("failed to extend boolean: {e}"))?;
                        Ok(result.into())
                    }
                    BinaryOperator::Less => {
                        let ret = self
                            .builder
                            .build_int_compare(IntPredicate::SLT, lv, rv, "lt")
                            .map_err(|e| format!("Failed to build Lt: {:?}", e))?;
                        let result = self
                            .builder
                            .build_int_z_extend(ret, self.context.i32_type(), "lt_into_int")
                            .map_err(|e| format!("failed to extend boolean: {e}"))?;
                        Ok(result.into())
                    }
                    BinaryOperator::LessEqual => {
                        let ret = self
                            .builder
                            .build_int_compare(IntPredicate::SLE, lv, rv, "le")
                            .map_err(|e| format!("Failed to build LE: {:?}", e))?;
                        let result = self
                            .builder
                            .build_int_z_extend(ret, self.context.i32_type(), "le_into_int")
                            .map_err(|e| format!("failed to extend boolean: {e}"))?;
                        Ok(result.into())
                    }
                    BinaryOperator::Greater => {
                        let ret = self
                            .builder
                            .build_int_compare(IntPredicate::SGT, lv, rv, "gt")
                            .map_err(|e| format!("Failed to build gt: {:?}", e))?;
                        let result = self
                            .builder
                            .build_int_z_extend(ret, self.context.i32_type(), "gt_into_int")
                            .map_err(|e| format!("failed to extend boolean: {e}"))?;
                        Ok(result.into())
                    }
                    BinaryOperator::GreaterEqual => {
                        let ret = self
                            .builder
                            .build_int_compare(IntPredicate::SGE, lv, rv, "ge")
                            .map_err(|e| format!("Failed to build ge: {:?}", e))?;
                        let result = self
                            .builder
                            .build_int_z_extend(ret, self.context.i32_type(), "ge_into_int")
                            .map_err(|e| format!("failed to extend boolean: {e}"))?;
                        Ok(result.into())
                    }
                    _ => unreachable!(),
                }
            }
        }
    }
    /// semantic analysis ensures the identifier is declared
    /// in here we focus on loading the correct LLVM value
    fn generate_identifier(&mut self, ident: &str) -> Result<BasicValueEnum<'ctx>, String> {
        if let Some(var_info) = self.lookup_variable(ident) {
            let var_type = var_info.var_type.clone();
            let ptr_value = var_info.value.into_pointer_value();
            let llvm_type = self.llvm_type_from_ast(&var_type)?;

            let loaded = self
                .builder
                .build_load(llvm_type, ptr_value, ident)
                .map_err(|e| format!("failed to load variable: {e}"))?;
            return Ok(loaded);
        }
        if let Some(global_var) = self.global_vars.get(ident) {
            let global_ptr = global_var.as_pointer_value();
            let global_type = global_var
                .get_initializer()
                .ok_or("global variable has no inititalizer")?
                .get_type();

            let loaded = self
                .builder
                .build_load(global_type, global_ptr, ident)
                .map_err(|e| format!("failed to load global variable: {e}"))?;
            return Ok(loaded);
        }
        Err(format!("undefined variable: {ident}"))
    }

    fn generate_ternary_op(
        &mut self,
        condition: &TypedExpr,
        true_expr: &TypedExpr,
        false_expr: &TypedExpr,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let cond_val = self.generate_expressions(condition)?;
        let cond_bool = self.build_not_zero(cond_val)?;

        let then_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "ternary_then");
        let else_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "ternary_else");
        let merge_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "ternary_merge");

        self.builder
            .build_conditional_branch(cond_bool, then_block, else_block)
            .map_err(|e| format!("failed to build conditional branch: {e}"))?;

        self.builder.position_at_end(then_block);
        let then_val = self.generate_expressions(true_expr)?;
        self.builder
            .build_unconditional_branch(merge_block)
            .map_err(|e| format!("failed to build branch: {e}"))?;
        let then_end_blk = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(else_block);
        let else_val = self.generate_expressions(false_expr)?;
        self.builder
            .build_unconditional_branch(merge_block)
            .map_err(|e| format!("failed to build branch: {e}"))?;
        let else_end_blk = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(merge_block);
        let phi = self
            .builder
            .build_phi(then_val.get_type(), "ternary_result")
            .map_err(|e| format!("failed to build phi: {e}"))?;

        phi.add_incoming(&[(&then_val, then_end_blk), (&else_val, else_end_blk)]);

        Ok(phi.as_basic_value())
    }

    fn generate_function_call(
        &mut self,
        name: &str,
        arguments: &[TypedExpr],
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let function = if let Some(&func) = self.function_table.get(name) {
            func
        } else {
            // function is not declared. to fix the issue with functions
            // declared in libc and used in our codebases, we will declare them
            // in here and assume that it will return an i32 (C default)
            let param_type: Vec<_> = arguments
                .iter()
                .map(|_| self.context.i32_type().into())
                .collect();
            let fn_type = self.context.i32_type().fn_type(&param_type, false);
            let func = self.module.add_function(name, fn_type, None);
            self.function_table.insert(name.to_string(), func);
            func
        };

        let mut args: Vec<BasicValueEnum> = Vec::new();
        for arg in arguments {
            args.push(self.generate_expressions(arg)?);
        }

        let args_refs = args.iter().map(|v| (*v).into()).collect::<Vec<_>>();
        let call_site = self
            .builder
            .build_call(function, &args_refs, "call")
            .map_err(|e| format!("failed to build call: {e}"))?;

        if let Some(return_val) = call_site.try_as_basic_value().left() {
            Ok(return_val)
        } else {
            Ok(self.context.i32_type().const_int(0, false).into())
        }
    }

    // TODO: handle pointer assignment in here
    fn generate_assignment(
        &mut self,
        target: &TypedExpr,
        value: &TypedExpr,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let val = self.generate_expressions(value)?;

        match &target.kind {
            Expression::Identifier(name) => {
                if let Some(info) = self.lookup_variable(name) {
                    let ptr = info.value.into_pointer_value();
                    self.builder
                        .build_store(ptr, val)
                        .map_err(|e| format!("Failed to store: {e:?}"))?;
                    return Ok(val);
                }

                if let Some(global_var) = self.global_vars.get(name) {
                    let ptr = global_var.as_pointer_value();
                    self.builder
                        .build_store(ptr, val)
                        .map_err(|e| format!("failed to store: {e:?}"))?;
                    return Ok(val);
                }
                Err(format!("Undefined variable in assignment: {}", name))
            }
            Expression::Dereference(inner) => {
                let ptr_value = self.generate_expressions(inner)?;
                let ptr = ptr_value.into_pointer_value();
                self.builder
                    .build_store(ptr, val)
                    .map_err(|e| format!("failed to store dereferenced pointer: {e}"))?;
                Ok(val)
            }
            _ => Err("Invalid assignment target".to_string()),
        }
    }

    fn generate_bitwise_not(&mut self, expr: &TypedExpr) -> Result<BasicValueEnum<'ctx>, String> {
        let val = self.generate_expressions(expr)?;
        Ok(self
            .builder
            .build_not(val.into_int_value(), "not")
            .map_err(|e| format!("failed to build not: {e}"))?
            .into())
    }

    fn generate_logical_not(&mut self, expr: &TypedExpr) -> Result<BasicValueEnum<'ctx>, String> {
        let val = self.generate_expressions(expr)?;
        let is_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                val.into_int_value(),
                self.context.i32_type().const_int(0, false),
                "is_zero",
            )
            .map_err(|e| format!("failed to build compare: {e}"))?;

        let result = self
            .builder
            .build_int_z_extend(is_zero, self.context.i32_type(), "bool_to_int")
            .map_err(|e| format!("failed to extend boolean: {e}"))?;

        Ok(result.into())
    }

    fn generate_unary_minus(&mut self, expr: &TypedExpr) -> Result<BasicValueEnum<'ctx>, String> {
        let val = self.generate_expressions(expr)?;
        Ok(self
            .builder
            .build_int_neg(val.into_int_value(), "neg")
            .map_err(|e| format!("failed to build neg: {e}"))?
            .into())
    }

    fn llvm_type_from_ast(&self, ast_type: &Types) -> Result<BasicTypeEnum<'ctx>, String> {
        match ast_type {
            Types::Int | Types::UInt => Ok(self.context.i32_type().into()),
            Types::Char | Types::UChar => Ok(self.context.i8_type().into()),
            Types::Long | Types::ULong => Ok(self.context.i64_type().into()),
            Types::Float => Ok(self.context.f32_type().into()),
            Types::Double => Ok(self.context.f64_type().into()),
            Types::Pointer(_inner) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Types::Void => Err("cannot create basic type for void".to_string()),
        }
    }

    /// used to map Types to llvm types. it will create zero values for each one of my types
    fn get_zero_value(&self, ast_type: &Types) -> Result<BasicValueEnum<'ctx>, String> {
        match ast_type {
            Types::Int | Types::UInt => Ok(self.context.i32_type().const_int(0, false).into()),
            Types::Char | Types::UChar => Ok(self.context.i8_type().const_int(0, false).into()),
            Types::Long | Types::ULong => Ok(self.context.i64_type().const_int(0, false).into()),
            Types::Float => Ok(self.context.f32_type().const_float(0.0).into()),
            Types::Double => Ok(self.context.f64_type().const_float(0.0).into()),
            Types::Pointer(_) => {
                let ptr_t = self.context.ptr_type(AddressSpace::default());
                Ok(ptr_t.const_null().into())
            }
            Types::Void => Err("Cant create zero value for void".to_string()),
        }
    }

    /// used for boolean comparison. we need to map 1,0 to true and false for the llvm
    /// to understand the equation
    fn build_not_zero(&mut self, value: BasicValueEnum<'ctx>) -> Result<IntValue<'ctx>, String> {
        let zero = match value.get_type() {
            BasicTypeEnum::IntType(int_type) => int_type.const_zero(),
            BasicTypeEnum::FloatType(_float) => {
                return Err("float comparison not implemented".to_string());
            }
            _ => return Err("unsupported type for boolean conversion".to_string()),
        };
        self.builder
            .build_int_compare(
                IntPredicate::NE,
                value.into_int_value(),
                zero,
                "is_not_zero",
            )
            .map_err(|e| format!("failed to build compare: {e}"))
    }

    /// each block has to have a terminator otherwise we could add an instruction after
    /// the block has been terminated. this method is used as a check for that
    fn current_block_terminator(&self) -> bool {
        if let Some(block) = self.builder.get_insert_block() {
            block.get_terminator().is_some()
        } else {
            false
        }
    }

    fn push_loop_context(&mut self, exit_block: BasicBlock<'ctx>, cont_block: BasicBlock<'ctx>) {
        self.loop_cont_stack.push(cont_block);
        self.loop_exit_stack.push(exit_block);
    }

    fn pop_loop_context(&mut self) {
        self.loop_cont_stack.pop();
        self.loop_exit_stack.pop();
    }

    fn push_scope(&mut self) {
        self.scope_stack.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        if self.scope_stack.len() > 1 {
            self.scope_stack.pop();
        }
    }
    /// semantic analysis has already validated that this variable exists
    /// we are using this lookup to get the LLVM value and type information
    fn lookup_variable(&mut self, name: &str) -> Option<&VariableInfo<'ctx>> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(var) = scope.get(name) {
                return Some(var);
            }
        }
        None
    }

    fn declare_variable(&mut self, name: &str, var_info: VariableInfo<'ctx>) {
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.insert(name.to_string(), var_info);
        }
    }

    pub fn compile_to_obj(&self, output_path: &Path) -> Result<(), String> {
        Target::initialize_all(&inkwell::targets::InitializationConfig::default());

        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple)
            .map_err(|e| format!("Failed to create target: {e}"))?;

        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or("Failed to create the target machine")?;
        target_machine
            .write_to_file(&self.module, FileType::Object, output_path)
            .map_err(|e| format!("failed to create object file: {e}"))?;
        Ok(())
    }
    pub fn print_ir(&self) {
        self.module.print_to_stderr();
    }
    /// helper func to truncate before storing
    fn coerce_for_store(
        &self,
        value: BasicValueEnum<'ctx>,
        target_type: &Types,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let Ok(llvm_target) = self.llvm_type_from_ast(target_type) else {
            return Ok(value);
        };
        match (value, llvm_target) {
            (BasicValueEnum::IntValue(iv), BasicTypeEnum::IntType(it)) => {
                let vw = iv.get_type().get_bit_width();
                let tw = it.get_bit_width();
                if vw == tw {
                    return Ok(value);
                }
                if vw > tw {
                    return Ok(self
                        .builder
                        .build_int_truncate(iv, it, "truct")
                        .map_err(|e| e.to_string())?
                        .into());
                }
                Ok(self
                    .builder
                    .build_int_z_extend(iv, it, "zextend")
                    .map_err(|e| e.to_string())?
                    .into())
            }
            _ => Ok(value),
        }
    }
    /// promotes to i32 or what ever type lhs and rhs have
    fn coerce_to_int(
        &self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
    ) -> (IntValue<'ctx>, IntValue<'ctx>) {
        let lw = lhs.get_type().get_bit_width();
        let rw = rhs.get_type().get_bit_width();
        if lw == rw {
            return (lhs, rhs);
        }
        let target = self.context.i32_type();
        let l = if lw < 32 {
            self.builder
                .build_int_z_extend(lhs, target, "zext_l")
                .unwrap()
        } else {
            lhs
        };

        let r = if rw < 32 {
            self.builder
                .build_int_z_extend(rhs, target, "zext_l")
                .unwrap()
        } else {
            rhs
        };
        (l, r)
    }
}
