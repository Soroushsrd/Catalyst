use crate::parser::*;
use std::io::Write;
use std::{collections::HashMap, fs::File, io};

#[derive(Clone)]
struct VariableInfo {
    pub offset: i32,
    #[allow(dead_code)]
    pub size: usize,
    pub var_type: Types,
}

#[allow(dead_code)]
struct FunctionInfo {
    name: String,
    return_type: Types,
    parameters: Vec<Parameter>,
}
pub struct AssemblyGenerator {
    output: Vec<String>,
    stack_offset: i32,
    ///a vec of symbol -> stack offset
    ///used to keep track of symbol scopes
    scope_stack: Vec<HashMap<String, VariableInfo>>,
    function_table: HashMap<String, FunctionInfo>,
    label_counter: u32,
}

impl AssemblyGenerator {
    pub fn new() -> Self {
        Self {
            output: Vec::new(),
            stack_offset: 0,
            scope_stack: vec![HashMap::new()],
            function_table: HashMap::new(),
            label_counter: 0u32,
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
        self.create_function_table(&program.function_def);
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

        for function in &program.function_def {
            self.generate_function(function);
            self.emit("");
        }
    }

    fn create_function_table(&mut self, functions: &[Function]) {
        for function in functions {
            //TODO: think of something to get rid of exessive cloning
            self.function_table.insert(
                function.name.name.clone(),
                FunctionInfo {
                    name: function.name.name.clone(),
                    return_type: function.return_type.clone(),
                    parameters: function.parameters.clone(),
                },
            );
        }
    }

    fn generate_function_call(&mut self, name: &str, arguments: &[Expression]) {
        if !self.function_table.contains_key(name) {
            //TODO: better error handling
            self.emit("    #Error: Unknown function {name}");
            self.emit("    mov $0, %rax");
            return;
        }

        // since we are using x86-64 conventions, we can assume that
        // first 6 ints are gonna be stored in these registers:
        let register = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];

        if arguments.len() > 6 {
            //TODO: better error handling
            self.emit(&format!(
                "   #Error: function {} has too many arguments",
                name
            ));
            return;
        }

        // saving caller saved registers
        for reg in &[
            "%rax", "%rcx", "%rdx", "%rsi", "%rdi", "%r8", "%r9", "%r10", "%r11",
        ] {
            self.emit(&format!("    push {}", reg));
        }

        let mut arg_t = Vec::with_capacity(6);

        // savign arguments on stack
        for (i, arg) in arguments.iter().enumerate() {
            self.generate_expression(arg);
            self.emit(&format!("   push %rax   #save arg number {i}"));
            arg_t.push(arg);
        }

        //popping argument sinto their registers
        for i in (0..arguments.len()).rev() {
            self.emit(&format!(
                "    pop {}  #Load argument {} into register",
                register[i], i,
            ));
        }
        self.emit(&format!("   call {name}"));

        // not including %rax because it contains the return code
        for reg in &["%r11", "%r10", "%r9", "%r8", "%rdi", "%rsi", "%rdx", "%rcx"] {
            self.emit(&format!("    pop {}", reg));
        }
        self.emit("    add $8, %rsp");
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
            if let Some(name) = &param.name
                && i < register.len()
            {
                //WARNING: for now im just handling ints as 64 bits

                // mvoing from register to stack
                // -8bytes for each 64bit value
                // aligning to 8byte boundary for stack efficiency
                self.stack_offset -= 8;
                let var = VariableInfo {
                    offset: self.stack_offset,
                    size: self.get_type_size(&Types::Int),
                    var_type: Types::Int,
                };

                self.declare_var(&name.name, &var);

                self.emit(&format!(
                    "   mov {}, {}(%rbp)",
                    register[i], self.stack_offset
                ));

                //TODO: handle the case that method has taken more than 6 inputs. some sort of
                //error maybe ?!
            }
        }
    }
    //TODO: break into smaller parts
    fn generate_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Block(statement) => {
                self.push_scope();

                for stmt in statement {
                    self.generate_statement(stmt);
                }

                self.pop_scop();
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
            Statement::Expression(expr) => self.generate_expression(expr),
            Statement::VarDeclaration {
                var_type,
                name,
                initializer,
            } => {
                if let Some(curr_scope) = self.scope_stack.last()
                    && curr_scope.contains_key(&name.name)
                {
                    self.emit(&format!(
                        "   #Error: variable {} already declared in this scope",
                        name.name
                    ));
                    eprintln!("Variable {} already declared", name.name);
                    return;
                }
                // allocating some stack space
                //WARNING: for now im handling everything as u64
                // so 8 bytes is being allocated here.
                let size = self.get_type_size(var_type);

                if size == 0 {
                    self.emit(&format!(
                        "    # Error: Cannot declare variable of void type: {}",
                        name.name
                    ));
                    return;
                }

                //  using 8 bytes per variables for consistency
                self.stack_offset -= 8;

                let var_info = VariableInfo {
                    offset: self.stack_offset,
                    size,
                    var_type: var_type.clone(),
                };
                self.declare_var(&name.name, &var_info);

                self.emit(&format!("   #variables declaration: {}", name.name));

                if let Some(init) = initializer {
                    self.generate_expression(init);
                    self.emit(&format!(
                        "   {} {}, {}(%rbp)  #initializes variables {}",
                        self.get_mov_instr(var_type),
                        self.get_register_name(var_type),
                        self.stack_offset,
                        name.name
                    ));
                } else {
                    self.emit(&format!(
                        "   {} $0, {}(%rbp)   #initializes variables {} to zero",
                        self.get_mov_instr(var_type),
                        self.stack_offset,
                        name.name
                    ));
                }
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let id = self.generate_label();
                let else_label = format!("if_else_{}", id);
                let end_label = format!("if_end_{}", id);

                self.generate_expression(condition);
                self.emit("    test %rax, %rax    # Test if condition");

                if else_branch.is_some() {
                    self.emit(&format!(
                        "    jz {}              # Jump to else branch if condition is false",
                        else_label
                    ));

                    self.generate_statement(then_branch);
                    self.emit(&format!(
                        "    jmp {}             # Jump to end, skip else branch",
                        end_label
                    ));

                    self.emit(&format!("{}:", else_label));
                    self.generate_statement(else_branch.as_ref().unwrap());

                    self.emit(&format!("{}:", end_label));
                } else {
                    self.emit(&format!(
                        "    jz {}              # Jump to end if condition is false",
                        end_label
                    ));

                    self.generate_statement(then_branch);

                    self.emit(&format!("{}:", end_label));
                }
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

                if let Some(info) = self.lookup_var(name) {
                    match info.var_type {
                        Types::Int => {
                            self.emit(&format!("   movl {}(%rbp), %eax", info.offset));
                            // sign extend 32bit to 64 bit for consistency
                            self.emit("   movslq %eax, %rax");
                        }
                        Types::Char => {
                            self.emit(&format!("   movb {}(%rbp), %al", info.offset));
                            // sign extending 8 bit to 64 bit
                            self.emit("   movsbq %al, %rax");
                        }
                        _ => {
                            self.emit(&format!("   mov {}(%rbp), %rax", info.offset));
                        }
                    }
                } else {
                    // undefied
                    self.emit(&format!("   # Error: undefined variable '{name}'"));
                    self.emit("   mov $0, %rax");
                }
            }
            Expression::FunctionCall { name, arguments } => {
                self.generate_function_call(name, arguments);
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
            } => match operator {
                BinaryOperator::And => {
                    self.emit_logical_and(left, right);
                }
                BinaryOperator::Or => {
                    self.emit_logical_or(left, right);
                }
                _ => {
                    self.generate_expression(left);
                    self.emit("    push %rax       # Save left operatnd");
                    self.generate_expression(right);
                    self.emit("    pop %rcx        # Restore left operand to %rcx");

                    self.handle_binops(left, right, operator);
                }
            },
            Expression::Assignment { target, value } => {
                // handle the value
                self.generate_expression(value);

                //store it in target variable
                if let Some(info) = self.lookup_var(target) {
                    self.emit(&format!(
                        "   {} {}, {}(%rbp)  #assigning to variable {}",
                        self.get_mov_instr(&info.var_type),
                        self.get_register_name(&info.var_type),
                        info.offset,
                        target
                    ));
                    //now assignment expr will retrn the assigned value
                    // rax already contains it
                } else {
                    self.emit(&format!(
                        "   # Error: undefined variable '{target}' in assignment"
                    ));
                }
            }
            Expression::TernaryOP {
                condition,
                true_expr,
                false_expr,
            } => {
                let id = self.generate_label();
                let false_label = format!("ternary_false_{id}");
                let end_label = format!("ternary_end_{id}");

                self.generate_expression(condition);
                self.emit("    test %rax, %rax  #test ternary op");
                self.emit(&format!(
                    "    jz {false_label}    #jump to false branch if condition returns zero",
                ));

                // if condition is true:
                self.generate_expression(true_expr);
                self.emit(&format!(
                    "    jmp {end_label}   #jump to end and skip false branch",
                ));

                // if its false:
                self.emit(&format!("{false_label}:"));
                self.generate_expression(false_expr);

                self.emit(&format!("{end_label}:"));
                self.emit("#end of ternary operation");
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
            BinaryOperator::Or => self.emit_logical_or(left, right),
            BinaryOperator::And => self.emit_logical_and(left, right),
            BinaryOperator::Equals => self.emit_comparison("je", "eq"),
            BinaryOperator::NotEquals => self.emit_comparison("jne", "neq"),
            BinaryOperator::Greater => self.emit_comparison("jg", "gt"),
            BinaryOperator::GreaterEqual => self.emit_comparison("jge", "ge"),
            BinaryOperator::Less => self.emit_comparison("jl", "lt"),
            BinaryOperator::LessEqual => self.emit_comparison("jle", "le"),
        }
    }

    fn emit_logical_or(&mut self, left: &Expression, right: &Expression) {
        self.generate_expression(left);
        self.emit("    test %rax, %rax    # Test left operand");

        // Create unique labels for this OR operation
        let true_label = format!("or_true_{}", self.generate_label());
        let end_label = format!("or_end_{}", self.generate_label());
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

    fn emit_logical_and(&mut self, left: &Expression, right: &Expression) {
        // Generate left operand
        self.generate_expression(left);
        self.emit("    test %rax, %rax    # Test left operand");

        let false_label = format!("and_false_{}", self.generate_label());
        let end_label = format!("and_end_{}", self.generate_label());

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

    fn emit_comparison(&mut self, jump_instr: &str, op: &str) {
        self.emit("    cmp %rax, %rcx     # Compare left and right");

        let true_label = format!("{}_true_{}", op, self.generate_label());
        let end_label = format!("{}_end_{}", op, self.generate_label());

        self.emit(&format!(
            "    {jump_instr} {true_label}        # Jump if true"
        ));

        // if left is not less than right, result is 0
        self.emit("    mov $0, %rax       # Result is false");
        self.emit(&format!("    jmp {end_label}        # Jump to end"));

        // less equal case -> result is 1
        self.emit(&format!("{true_label}:"));
        self.emit("    mov $1, %rax       # Result is true");

        self.emit(&format!("{end_label}:"));
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

    fn generate_label(&mut self) -> u32 {
        self.label_counter += 1;
        self.label_counter
    }

    fn get_mov_instr(&self, var_type: &Types) -> &'static str {
        match var_type {
            Types::Int => "movl",
            Types::Void => "movq",
            Types::Long => "movq",
            Types::Char => "movb",
            Types::Float => "movss",
            Types::Double => "movsd",
            Types::Pointer(_) => todo!("how do i handle pointers?"),
        }
    }
    fn get_type_size(&self, var_type: &Types) -> usize {
        match var_type {
            Types::Int => 4,
            Types::Void => 0,
            Types::Long => 8,
            Types::Char => 1,
            Types::Float => 4,
            Types::Double => 8,
            Types::Pointer(_) => 8, // u64 on x86-64
        }
    }
    fn get_register_name(&self, var_type: &Types) -> &'static str {
        match var_type {
            // a 32 bit register
            Types::Int => "%eax",
            // two below are 64 bits
            Types::Void => "%rax",
            Types::Long => "%rax",
            // 8 bit register
            Types::Char => "%al",
            // SSE reg for floats
            Types::Float => "%xmm0",
            // SSE reg for doubles
            Types::Double => "%xmm0",
            // a 64 bit register used for pointers
            Types::Pointer(_) => "%rax",
        }
    }
    fn push_scope(&mut self) {
        self.scope_stack.push(HashMap::new());
    }
    fn pop_scop(&mut self) {
        if self.scope_stack.len() > 1 {
            self.scope_stack.pop();
        }
    }
    fn lookup_var(&self, name: &str) -> Option<&VariableInfo> {
        // using .rev to look from inner most scope to outter most one
        for scope in self.scope_stack.iter().rev() {
            if let Some(var) = scope.get(name) {
                return Some(var);
            }
        }
        None
    }

    fn declare_var(&mut self, name: &str, var: &VariableInfo) {
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.insert(name.to_string(), var.clone());
        }
    }
}

fn last_statement_is_return(statement: &Statement) -> bool {
    match statement {
        Statement::Block(statements) => statements
            .last()
            .map(last_statement_is_return)
            .unwrap_or(false),
        Statement::Return(_) => true,
        _ => false,
    }
}
