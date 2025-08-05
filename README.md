# C Language Compiler

A simple compiler written in Rust that translates C language into x86-64 assembly code. This project demonstrates the fundamental phases of compilation: lexical analysis, parsing, and code generation.

## Features

### Currently Supported ✓

#### Language Constructs
- [x] **Function definitions** with return types (`int`, `void`)
- [x] **Return statements** with optional expressions
- [x] **Function parameters** (basic parameter parsing implemented)
- [x] **Block statements** with curly braces `{}`

#### Data Types
- [x] **Integers** (`int`)
- [x] **Void** (`void`)
- [x] **Floating-point numbers** (parsed but treated as integers in codegen)

#### Expressions
- [x] **Arithmetic operations**: `+`, `-`, `*`, `/`
- [x] **Unary operations**: 
  - [x] Unary minus (`-`)
  - [x] Logical NOT (`!`)
  - [x] Bitwise NOT (`~`)
- [x] **Binary expressions** with proper operator precedence
- [x] **Parenthesized expressions**
- [x] **Variable references** (identifier lookup)
- [x] **Numeric literals**

#### Comments
- [x] **Single-line comments** (`//`)
- [x] **Block comments** (`/* */`)

#### Core Language Features
- [ ] **Variable declarations and assignments**
- [ ] **Local variable scoping**
- [ ] **Function calls with argument passing**
- [ ] **Multiple parameter support** (currently limited to 6 due to calling convention)

#### Control Flow
- [ ] **Conditional statements** (`if`/`else`)
- [ ] **Loops** (`while`, `for`)
- [ ] **Comparison operators** (`==`, `!=`, `<`, `>`, `<=`, `>=`)
- [ ] **Logical operators** (`&&`, `||`)

### Advanced Features
- [ ] **Arrays and indexing**
- [ ] **Pointers and references**
- [ ] **Structures/records**
- [ ] **String handling**
- [ ] **Multiple source files**

### Optimization & Tooling
- [ ] **Basic optimizations** (constant folding, dead code elimination)
- [ ] **Better error messages** with line numbers and suggestions
- [ ] **Debugging information generation**
- [ ] **Standard library functions** (`printf`, etc.)

### Language Extensions
- [ ] **Type system improvements**
- [ ] **Generic/template support**
- [ ] **Module system**
- [ ] **Memory management features**

## Architecture

The compiler follows a traditional three-phase design:

### 1. Lexical Analysis (`lexer.rs`)
- **Scanner** tokenizes the source code
- Handles keywords, operators, identifiers, numbers, and strings
- Supports both single-line and block comments
- Implements "maximal munch" principle for token recognition

### 2. Parsing (`parser.rs`)
- **Recursive descent parser** builds an Abstract Syntax Tree (AST)
- Implements operator precedence for binary expressions
- Handles unary expressions and function definitions
- Error reporting for syntax errors

### 3. Code Generation (`code_generator.rs`)
- **AssemblyGenerator** translates AST to x86-64 assembly
- Follows System V ABI calling conventions
- Manages stack frame allocation and variable storage
- Generates complete executable assembly with proper prologue/epilogue

## Usage

### Prerequisites
- Rust compiler
- GNU Assembler (`as`)
- GNU Linker (`ld`)

### Building and Running

1. **Compile the compiler:**
```bash
cargo build --release
```

2. **Compile a source file:**
```bash
./target/release/your_compiler_name source_file.c
```

3. **Run the generated executable:**
```bash
./source_file
```

### Example Workflow

```bash
# Create a simple program
echo 'int main() { return 42; }' > test.c

# Compile it
cargo run test.c

# Run the generated executable
./test

# Check the exit code
echo $?  # Should output: 42
```


## Contributing

This is a learning project, but contributions are welcome! Areas that need attention:

1. **Parser improvements** - Better error recovery and reporting
2. **Code generation** - More expression types and optimizations
3. **Testing** - Comprehensive test suite for all components
4. **Documentation** - Code comments and usage examples


*This compiler is a work in progress and serves as an educational project for understanding compiler construction principles.*
