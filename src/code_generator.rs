use crate::parser::*;
use std::io::Write;
use std::{collections::HashMap, fs::File, io};

pub struct AssemblyGenerator {
    output: Vec<String>,
    stack_offset: i32,
    ///symbol -> stack offset
    symbols: HashMap<String, i32>,
}

impl AssemblyGenerator {
    pub fn new() -> Self {
        Self {
            output: Vec::new(),
            stack_offset: 0,
            symbols: HashMap::new(),
        }
    }

    /// prints the generated output into a file.
    pub fn compile_to_file(&self, filename: &str) -> io::Result<()> {
        let mut file = File::create(filename)?;
        for line in &self.output {
            writeln!(file, "{line}")?;
        }
        Ok(())
    }

    pub fn generate_program(&mut self, program: &Program) {
        // assembly prologue
        self.emit(".text");
        self.emit(".global _start");
        self.emit("_start:");

        // call main function and its return values
        self.emit("    call main");
        self.emit("    # Exit with main's return value");
        self.emit("    mov %rax, %rdi     # Move return value to exit status");
        self.emit("    mov $60, %rax      # sys_exit");
        self.emit("    syscall");
        self.emit("");

        self.generate_function(&program.function_def);
    }

    fn generate_function(&mut self, function: &Function) {
        // first the label:
        self.emit(&format!("{}:", function.name.name));

        // its prologues
        self.emit("    push %rbp");
        self.emit("    mov %rsp, %rbp");

        // reserving some stack space for
        // local variables
        self.emit("    sub $16, %rsp");

        self.generate_parameter(&function.parameters);
        self.generate_statement(&function.body);

        //TODO: control flow analysis
        if !last_statement_is_return(&function.body) {
            self.emit_function_epilogue();
        }
    }
    fn generate_parameter(&mut self, parameter: &[Parameter]) {
        // since we are using x86-64 conventions, we can assume that
        // first 6 ints are gonna be stored in these registers:
        let register = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];

        for (i, param) in parameter.iter().enumerate() {
            if let Some(name) = &param.name {
                if i < register.len() {
                    // mvoing from register to stack
                    // -8bytes for each 64bit value
                    self.stack_offset -= 8;
                    self.symbols.insert(name.name.clone(), self.stack_offset);
                    self.emit(&format!(
                        "   mov {}, {}(%rbp)",
                        register[i], self.stack_offset
                    ));
                }
                //TODO: handle the case that method has taken more than 6 inputs. some sort of
                //error maybe ?!
            }
        }
    }
    fn generate_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Block(statement) => {
                for stmt in statement {
                    self.generate_statement(stmt);
                }
            }
            Statement::Return(expr) => {
                if let Some(expression) = expr {
                    self.generate_expression(expression);
                } else {
                    // handling void in this case
                    self.emit("    mov $0, %rax");
                }

                self.emit_function_epilogue();
            }
        }
    }
    fn generate_expression(&mut self, expression: &Expression) {
        match expression {
            Expression::Number(value) => {
                // we can load the value into the return register
                self.emit(&format!("    mov ${}, %rax", *value as i32));
            }
            Expression::Identifier(name) => {
                // should first lookup the value in symbol table
                if let Some(offset) = self.symbols.get(name) {
                    // then load it into rax
                    self.emit(&format!("   mov {offset}(%rbp), %rax"));
                } else {
                    // undefied
                    self.emit(&format!("   # Error: undefined variable '{name}'"));
                    self.emit("   mov $0, %rax");
                }
            }
            Expression::BitwiseNot(value) => {
                self.generate_expression(value);
                self.emit("    not %rax        #Bitwise Not op");
            }
            Expression::UnaryMinus(expr) => {
                self.generate_expression(expr);
                //negate the results
                self.emit("    neg %rax           # Unary minus operation");
            }
            Expression::LogicalNot(expr) => {
                self.generate_expression(expr);
                self.emit("    test %rax, %rax    # Test if value is zero");
                self.emit("    setz %al           # Set AL to 1 if zero, 0 if non-zero");
                self.emit("    movzbl %al, %eax   # Zero-extend AL to EAX");
            }
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                self.generate_expression(left);
                self.emit("    push %rax       # Save left operatnd");
                self.generate_expression(right);
                self.emit("    pop %rcx        # Restore left operand to %rcx");

                self.handle_binops(right, left, operator);
            }
            Expression::Unknown => {
                self.emit("   # Unknown expression");
                self.emit("   mov $0, %rax");
            }
        }
    }

    fn handle_binops(&mut self, left: &Expression, right: &Expression, operator: &BinaryOperator) {
        match operator {
            BinaryOperator::Add => {
                self.emit("    add %rcx, %rax");
            }
            BinaryOperator::Subtract => {
                self.emit("    sub %rax, %rcx");
                self.emit("    mov %rcx, %rax");
            }
            BinaryOperator::Multiply => {
                self.emit("    imul %rcx, %rax    # Multiply: left * right");
            }
            BinaryOperator::Divide => {
                // for division: dividend in %rax, divisor in register
                //we need: %rax = %rcx / %rax  (left / right)
                self.emit("    mov %rax, %rbx     # Save right operand (divisor) in %rbx");
                self.emit("    mov %rcx, %rax     # Move left operand (dividend) to %rax");
                self.emit("    cqo                # Sign extend %rax to %rdx:%rax");
                self.emit("    idiv %rbx          # Divide %rdx:%rax by %rbx, result in %rax");
            }
            BinaryOperator::Or => {
                self.generate_expression(left);
                self.emit("    test %rax, %rax    # Test left operand");

                // Create unique labels for this OR operation
                let true_label = format!("or_true_{}", self.output.len());
                let end_label = format!("or_end_{}", self.output.len());
                // if left is true (non-zero), jump to true_label
                self.emit(&format!(
                    "    jnz {true_label}         # Jump if left is true"
                ));

                // left is false, evaluate right part
                self.generate_expression(right);
                self.emit("    test %rax, %rax    # Test right operand");
                self.emit(&format!(
                    "    jnz {true_label}         # Jump if right is true"
                ));

                // if both of them are false, result is 0
                self.emit("    mov $0, %rax       # Both operands false");
                self.emit(&format!("    jmp {end_label}         # Jump to end"));

                // true case-> result is 1
                self.emit(&format!("{true_label}:"));
                self.emit("    mov $1, %rax       # Result is true");

                self.emit(&format!("{end_label}:"));
            }
            BinaryOperator::And => {
                // Generate left operand
                self.generate_expression(left);
                self.emit("    test %rax, %rax    # Test left operand");

                // TODO:labesl should be unique, for now im using output length
                let false_label = format!("and_false_{}", self.output.len());
                let end_label = format!("and_end_{}", self.output.len());

                // left is false (0), jump to false_label
                self.emit(&format!(
                    "    jz {false_label}          # Jump if left is false"
                ));

                // left is true, evaluate right operand
                self.generate_expression(right);
                self.emit("    test %rax, %rax    # Test right operand");
                self.emit(&format!(
                    "    jz {false_label}          # Jump if right is false"
                ));

                // both are true, result is 1
                self.emit("    mov $1, %rax       # Both operands true");
                self.emit(&format!("    jmp {end_label}         # Jump to end"));

                // false case-> result is 0
                self.emit(&format!("{false_label}:"));
                self.emit("    mov $0, %rax       # Result is false");

                self.emit(&format!("{end_label}:"));
            }
            BinaryOperator::Equals => {
                self.emit("    cmp %rax, %rcx     # Compare left and right");

                let true_label = format!("eq_true_{}", self.output.len());
                let end_label = format!("eq_end_{}", self.output.len());

                // jump to true_label if the values are equal (zero flag set)
                self.emit(&format!("    je {true_label}        # Jump if equal"));

                // if values are not equal, result is 0
                self.emit("    mov $0, %rax       # Result is false (not equal)");
                self.emit(&format!("    jmp {end_label}        # Jump to end"));

                // eq case -> result is 1
                self.emit(&format!("{true_label}:"));
                self.emit("    mov $1, %rax       # Result is true (equal)");

                self.emit(&format!("{end_label}:"));
            }
            BinaryOperator::NotEquals => {
                self.emit("    cmp %rax, %rcx     # Compare left and right");

                let true_label = format!("neq_true_{}", self.output.len());
                let end_label = format!("neq_end_{}", self.output.len());

                self.emit(&format!("    jne {true_label}       # Jump if not equal"));

                // values are equal, result is 0
                self.emit("    mov $0, %rax       # Result is false (equal)");
                self.emit(&format!("    jmp {end_label}        # Jump to end"));

                // not equal case -> result is 1
                self.emit(&format!("{true_label}:"));
                self.emit("    mov $1, %rax       # Result is true (not equal)");

                self.emit(&format!("{end_label}:"));
            }
            BinaryOperator::Greater => {
                self.emit("    cmp %rax, %rcx     # Compare left and right");

                let true_label = format!("gt_true_{}", self.output.len());
                let end_label = format!("gt_end_{}", self.output.len());

                self.emit(&format!("    jg {true_label}        # Jump if greater"));

                // if left is not greater than right, result is 0
                self.emit("    mov $0, %rax       # Result is false (not greater)");
                self.emit(&format!("    jmp {end_label}        # Jump to end"));

                // greater case -> result is 1
                self.emit(&format!("{true_label}:"));
                self.emit("    mov $1, %rax       # Result is true (greater)");

                self.emit(&format!("{end_label}:"));
            }
            BinaryOperator::GreaterEqual => {
                self.emit("    cmp %rax, %rcx     # Compare left and right");

                let true_label = format!("ge_true_{}", self.output.len());
                let end_label = format!("ge_end_{}", self.output.len());

                self.emit(&format!(
                    "    jge {true_label}       # Jump if greater or equal"
                ));

                // if left is not greater or equal to right, result is 0
                self.emit("    mov $0, %rax       # Result is false (not greater/equal)");
                self.emit(&format!("    jmp {end_label}        # Jump to end"));

                // greater or equal case -> result is 1
                self.emit(&format!("{true_label}:"));
                self.emit("    mov $1, %rax       # Result is true (greater/equal)");

                self.emit(&format!("{end_label}:"));
            }
            BinaryOperator::Less => {
                self.emit("    cmp %rax, %rcx     # Compare left and right");

                let true_label = format!("lt_true_{}", self.output.len());
                let end_label = format!("lt_end_{}", self.output.len());

                self.emit(&format!("    jl {true_label}        # Jump if less"));

                // if left is not less than right, result is 0
                self.emit("    mov $0, %rax       # Result is false (not less)");
                self.emit(&format!("    jmp {end_label}        # Jump to end"));

                // less case -> result is 1
                self.emit(&format!("{true_label}:"));
                self.emit("    mov $1, %rax       # Result is true (less)");

                self.emit(&format!("{end_label}:"));
            }
            BinaryOperator::LessEqual => {
                self.emit("    cmp %rax, %rcx     # Compare left and right");

                let true_label = format!("le_true_{}", self.output.len());
                let end_label = format!("le_end_{}", self.output.len());

                self.emit(&format!("    jle {true_label}        # Jump if lessequal"));

                // if left is not less than right, result is 0
                self.emit("    mov $0, %rax       # Result is false (not lessequal)");
                self.emit(&format!("    jmp {end_label}        # Jump to end"));

                // less equal case -> result is 1
                self.emit(&format!("{true_label}:"));
                self.emit("    mov $1, %rax       # Result is true (lessequal)");

                self.emit(&format!("{end_label}:"));
            }
        }
    }
    /// saves the generated instruction into output vec
    fn emit(&mut self, instruction: &str) {
        self.output.push(instruction.to_string());
    }
    fn emit_function_epilogue(&mut self) {
        self.emit("    mov %rbp, %rsp");
        self.emit("    pop %rbp");
        self.emit("    ret");
    }
}

fn last_statement_is_return(statement: &Statement) -> bool {
    match statement {
        Statement::Block(statements) => statements
            .last()
            .map(last_statement_is_return)
            .unwrap_or(false),
        Statement::Return(_) => true,
    }
}
