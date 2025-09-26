use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{CodeModel, FileType, RelocMode, Target, TargetMachine};
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue};
use inkwell::{AddressSpace, IntPredicate, OptimizationLevel};

use crate::parser::*;
use std::collections::HashMap;
use std::path::Path;

#[derive(Clone)]
struct VariableInfo<'ctx> {
    pub value: BasicValueEnum<'ctx>,
    pub var_type: Types,
}

#[allow(dead_code)]
struct FunctionInfo {
    name: String,
    return_type: Types,
    parameters: Vec<Parameter>,
}
pub struct LLVMCodeGenerator<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    ///a vec of symbol -> stack offset
    ///used to keep track of symbol scopes
    scope_stack: Vec<HashMap<String, VariableInfo<'ctx>>>,
    // current function being compiled
    current_function: Option<FunctionValue<'ctx>>,
    // TODO: forward declaration table
    function_table: HashMap<String, FunctionValue<'ctx>>,
    // label caounter for unique labels
    label_counter: u32,
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
            current_function: None,
            function_table: HashMap::new(),
            label_counter: 0u32,
        }
    }

    /// prints the generated output into a file.
    /// generate LLVM IR for the entire program
    pub fn generate_program(&mut self, program: &Program) -> Result<(), String> {
        self.declare_functions(&program.function_def)?;
        for function in &program.function_def {
            self.generate_function(function)?;
        }

        self.generate_start_function()?;
        if let Err(e) = self.module.verify() {
            return Err(format!("LLVM module verification failed: {e}"));
        }
        Ok(())
    }

    fn generate_start_function(&mut self) -> Result<(), String> {
        // void _start()
        let start_fn_type = self.context.void_type().fn_type(&[], false);
        let start_fn = self.module.add_function("_start", start_fn_type, None);

        let entry_block = self.context.append_basic_block(start_fn, "entry");
        self.builder.position_at_end(entry_block);

        // calling main function
        let main_fn = self
            .function_table
            .get("main")
            .ok_or("No main function found")?;

        let main_result = self
            .builder
            .build_call(*main_fn, &[], "main_result")
            .map_err(|e| format!("Failed to build call to main: {e}"))?;

        // the return value from main (should be i32)
        let exit_code = if let Some(return_val) = main_result.try_as_basic_value().left() {
            // covnerting to i64 for syscall
            self.builder
                .build_int_z_extend(
                    return_val.into_int_value(),
                    self.context.i64_type(),
                    "exit_code",
                )
                .map_err(|e| format!("Failed to extend exit code: {e}"))?
        } else {
            self.context.i64_type().const_int(0, false)
        };

        let asm_type = self.context.void_type().fn_type(
            &[
                self.context.i64_type().into(),
                self.context.i64_type().into(),
            ],
            false,
        );

        let inline_asm = self.context.create_inline_asm(
            asm_type,
            "mov $0, %rax\nmov $1, %rdi\nsyscall".to_string(),
            "r,r,~{rax},~{rdi}".to_string(),
            true,  // has side effects
            false, // not aligned stack
            None,  // no dialect
            false, // can_throw - 7th argument
        );

        let syscall_number = self.context.i64_type().const_int(60, false);
        let args = &[syscall_number.into(), exit_code.into()];

        self.builder
            .build_indirect_call(asm_type, inline_asm, args, "exit")
            .map_err(|e| format!("Failed to build syscall: {e}"))?;

        self.builder
            .build_unreachable()
            .map_err(|e| format!("Failed to build unreachable: {e}"))?;

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

        self.generate_statement(&function.body)?;

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
                    self.builder
                        .build_return(Some(&value))
                        .map_err(|e| format!("failed to build return: {e}"))?;
                } else {
                    self.builder
                        .build_return(None)
                        .map_err(|e| format!("failed to build return: {e}"))?;
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
                if let Some(current_scope) = self.scope_stack.last() {
                    if current_scope.contains_key(&name.name) {
                        return Err(format!(
                            "variable {} already declared in this scope",
                            name.name
                        ));
                    }
                }
                let llvm_type = self.llvm_type_from_ast(var_type)?;
                let alloca = self
                    .builder
                    .build_alloca(llvm_type, &name.name)
                    .map_err(|e| format!("failed to build alloca: {e}"))?;

                if let Some(init_expr) = initializer {
                    let init_value = self.generate_expressions(init_expr)?;
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
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
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
            }

            Statement::While {
                condition,
                then_branch,
            } => {
                let loop_cond = self
                    .context
                    .append_basic_block(self.current_function.unwrap(), "loop_cond");
                let loop_body = self
                    .context
                    .append_basic_block(self.current_function.unwrap(), "loop_body");
                let loop_end = self
                    .context
                    .append_basic_block(self.current_function.unwrap(), "loop_end");

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

                self.builder.position_at_end(loop_end);
            }

            Statement::DoWhile { body, condition } => {
                let loop_body = self
                    .context
                    .append_basic_block(self.current_function.unwrap(), "do_body");
                let loop_cond = self
                    .context
                    .append_basic_block(self.current_function.unwrap(), "do_cond");
                let loop_end = self
                    .context
                    .append_basic_block(self.current_function.unwrap(), "do_end");

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

                self.builder.position_at_end(loop_end);
            }
        }
        Ok(())
    }

    /// generate llvm IR for expressions
    fn generate_expressions(
        &mut self,
        expression: &Expression,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        match expression {
            Expression::Number(value) => Ok(self
                .context
                .i32_type()
                .const_int(*value as u64, false)
                .into()),
            Expression::Identifier(ident) => self.generate_identifier(ident),
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
        }
    }

    fn generate_binary_op(
        &mut self,
        left: &Box<Expression>,
        operator: &BinaryOperator,
        right: &Box<Expression>,
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

                // Ok(phi.as_basic_value())
            }
            _ => {
                let left_val = self.generate_expressions(left)?;
                let right_val = self.generate_expressions(right)?;

                match operator {
                    BinaryOperator::Add => Ok(self
                        .builder
                        .build_int_add(left_val.into_int_value(), right_val.into_int_value(), "add")
                        .map_err(|e| format!("failed to build add: {e}"))?
                        .into()),
                    BinaryOperator::Subtract => Ok(self
                        .builder
                        .build_int_sub(
                            left_val.into_int_value(),
                            right_val.into_int_value(),
                            "subtract",
                        )
                        .map_err(|e| format!("failed to build subtract: {e}"))?
                        .into()),
                    BinaryOperator::Multiply => Ok(self
                        .builder
                        .build_int_mul(
                            left_val.into_int_value(),
                            right_val.into_int_value(),
                            "multiply",
                        )
                        .map_err(|e| format!("failed to build multiplication: {e}"))?
                        .into()),
                    BinaryOperator::Divide => Ok(self
                        .builder
                        .build_int_signed_div(
                            left_val.into_int_value(),
                            right_val.into_int_value(),
                            "divide",
                        )
                        .map_err(|e| format!("failed to build division: {e}"))?
                        .into()),
                    BinaryOperator::Equals => {
                        let ret = self
                            .builder
                            .build_int_compare(
                                IntPredicate::EQ,
                                left_val.into_int_value(),
                                right_val.into_int_value(),
                                "eq",
                            )
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
                            .build_int_compare(
                                IntPredicate::NE,
                                left_val.into_int_value(),
                                right_val.into_int_value(),
                                "ne",
                            )
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
                            .build_int_compare(
                                IntPredicate::SLT,
                                left_val.into_int_value(),
                                right_val.into_int_value(),
                                "lt",
                            )
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
                            .build_int_compare(
                                IntPredicate::SLE,
                                left_val.into_int_value(),
                                right_val.into_int_value(),
                                "le",
                            )
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
                            .build_int_compare(
                                IntPredicate::SGT,
                                left_val.into_int_value(),
                                right_val.into_int_value(),
                                "gt",
                            )
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
                            .build_int_compare(
                                IntPredicate::SGE,
                                left_val.into_int_value(),
                                right_val.into_int_value(),
                                "ge",
                            )
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

    fn generate_identifier(&mut self, ident: &str) -> Result<BasicValueEnum<'ctx>, String> {
        if let Some(var_info) = self.lookup_variable(ident) {
            let var_type = var_info.var_type.clone();
            let ptr_value = var_info.value.into_pointer_value();
            let llvm_type = self.llvm_type_from_ast(&var_type)?;

            let loaded = self
                .builder
                .build_load(llvm_type, ptr_value, &ident)
                .map_err(|e| format!("failed to load variable: {e}"))?;
            Ok(loaded)
        } else {
            Err(format!(" undefined variable: {ident}"))
        }
    }

    fn generate_ternary_op(
        &mut self,
        condition: &Box<Expression>,
        true_expr: &Box<Expression>,
        false_expr: &Box<Expression>,
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
        arguments: &[Expression],
    ) -> Result<BasicValueEnum<'ctx>, String> {
        if let Some(&function) = self.function_table.get(name) {
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
        } else {
            Err(format!("unknown function: {name}"))
        }
    }

    fn generate_assignment(
        &mut self,
        target: &str,
        value: &Box<Expression>,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let val = self.generate_expressions(value)?;

        let var_info = if let Some(info) = self.lookup_variable(&target) {
            info.value.into_pointer_value()
        } else {
            return Err(format!("Undefined variable in assignment: {}", target));
        };

        self.builder
            .build_store(var_info, val)
            .map_err(|e| format!("Failed to store: {:?}", e))?;
        Ok(val)
    }

    fn generate_bitwise_not(
        &mut self,
        expr: &Box<Expression>,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let val = self.generate_expressions(expr)?;
        Ok(self
            .builder
            .build_not(val.into_int_value(), "not")
            .map_err(|e| format!("failed to build not: {e}"))?
            .into())
    }

    fn generate_logical_not(
        &mut self,
        expr: &Box<Expression>,
    ) -> Result<BasicValueEnum<'ctx>, String> {
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

    fn generate_unary_minus(
        &mut self,
        expr: &Box<Expression>,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let val = self.generate_expressions(expr)?;
        Ok(self
            .builder
            .build_int_neg(val.into_int_value(), "neg")
            .map_err(|e| format!("failed to build neg: {e}"))?
            .into())
    }

    fn llvm_type_from_ast(&self, ast_type: &Types) -> Result<BasicTypeEnum<'ctx>, String> {
        match ast_type {
            Types::Int => Ok(self.context.i32_type().into()),
            Types::Char => Ok(self.context.i8_type().into()),
            Types::Long => Ok(self.context.i64_type().into()),
            Types::Float => Ok(self.context.f32_type().into()),
            Types::Double => Ok(self.context.f64_type().into()),
            Types::Pointer(_inner) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Types::Void => Err("cannot create basic type for void".to_string()),
        }
    }

    /// used to map Types to llvm types. it will create zero values for each one of my types
    fn get_zero_value(&self, ast_type: &Types) -> Result<BasicValueEnum<'ctx>, String> {
        match ast_type {
            Types::Int => Ok(self.context.i32_type().const_int(0, false).into()),
            Types::Char => Ok(self.context.i8_type().const_int(0, false).into()),
            Types::Long => Ok(self.context.i64_type().const_int(0, false).into()),
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

    fn push_scope(&mut self) {
        self.scope_stack.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        if self.scope_stack.len() > 1 {
            self.scope_stack.pop();
        }
    }

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
}
