
# C Language Compiler

A simple compiler written in Rust that uses LLVM to compile C code.

## Features

### Currently Supported ✓

#### Language Constructs

- [x] **Function definitions** with return types
- [x] **Return statements** with optional expressions
- [x] **Function parameters** (basic parameter parsing implemented)
- [x] **Declarations and Assignments**
- [x] **Block statements** with curly braces `{}`

#### Data Types

- [x] **Integers** (`int`)
- [x] **Void** (`void`)
- [x] **Char** (`char`)
- [x] **Long** (`long`)
- [x] **Double** (`double`)
- [x] **Float** (`float`)

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
- [x] **Ternary expressions**

#### Comments

- [x] **Single-line comments** (`//`)
- [x] **Block comments** (`/* */`)

#### Core Language Features

- [x] **Variable declarations and assignments**
- [x] **Local variable scoping**
- [x] **Function calls with argument passing**
- [x] **Multiple parameter support** (currently limited to 6 due to calling convention)
- [x] **Global variable declaration**

#### Control Flow

- [x] **Conditional statements** (`if`/`else`)
- [x] **Loops** (`while`, `for`,`continue`,`break`)
- [x] **Comparison operators** (`==`, `!=`, `<`, `>`, `<=`, `>=`)
- [x] **Logical operators** (`&&`, `||`)

### Advanced Features

- [ ] **Arrays and indexing**
- [x] **Nested Blocks**
- [ ] **Pointers and references**
- [ ] **Structures/records**
- [ ] **String handling**
- [ ] **Multiple source files**

### Optimization & Tooling

- [ ] **Basic optimizations** (constant folding, dead code elimination)
- [x] **Better error messages** with line numbers and suggestions
- [x] **Debugging information generation**
- [x] **Standard library functions** (`printf`, etc.)

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

### 3. Code Generation (`semantic_analyzer.rs`)

- **SemanticAnalyzer** analyzes the parsed AST semanticly
- Checks for usage before declaration errors

### 4. Code Generation (`code_generator.rs`)

- **LLVMCodeGenerator** translates AST to llvm generated assembly
- Follows System V ABI calling conventions
- Manages stack frame allocation and variable storage
- Generates complete executable assembly with proper prologue/epilogue

## Usage

### Prerequisites

- Rust compiler
- Clang
- LLVM 18.1 (Just to compile Catalyst)
- Polly (libpolly-18-dev)

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
