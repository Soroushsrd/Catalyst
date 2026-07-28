# Catalyst

A C compiler written in Rust, targeting native binaries through LLVM.

Catalyst takes a single `.c` file, runs it through a hand written lexer, recursive descent parser, semantic analyzer, and LLVM IR code generator, then links the result with `clang` into an executable.

```bash
echo 'int main() { return 42; }' > test.c
cargo run -- test.c
./test; echo $?   # 42
```

## Building

Prerequisites:

- Rust (edition 2024)
- LLVM 18.1 development libraries
- `libpolly-18-dev`
- `clang` on `PATH` for linking

```bash
cargo build --release
./target/release/catalyst source.c            # writes ./source
./target/release/catalyst source.c -o myprog  # writes ./myprog
```

## Supported language

### Types

- `int`, `char`, `long`, `float`, `double`, `void`
- Pointers of arbitrary depth: `int *p`, `char **pp`
- Integer promotion by rank: `char < int < long`, integers promote into `float < double`

### Declarations

- Local variables, with and without initializers (uninitialized locals are zeroed)
- Global variables with constant initializers (integer or char literal), zeroed by default
- Tentative globals: `int foo;` followed by `int foo = 3;` completes the declaration, two full definitions are an error
- Functions with up to 6 parameters
- Forward declarations, checked against their definition for return type, parameter count, and parameter types
- Duplicate function definitions and name collisions between globals and functions are rejected

### Statements

- `{ }` blocks with nested scoping
- `if` / `else`
- `while`, `do ... while`, `for` (the `for` initializer gets its own scope)
- `break` and `continue`, rejected at parse time outside a loop
- `return` with or without a value

### Expressions

- Integer and floating literals, character literals (`'a'`, `'0'`)
- Identifiers, function calls
- Unary `-`, `!`, `~`, address-of `&`, dereference `*`
- Binary `+ - * / %`, comparisons `== != < <= > >=`, logical `&& ||`
- Ternary `cond ? a : b`
- Assignment to a variable or through a pointer: `x = 1`, `*p = 1`

### Code generation notes

- `alloca` instructions are hoisted into the function entry block
- `&&` and `||` short circuit with real branches and a `phi` merge
- Ternaries compile to branches plus `phi`
- Comparisons produce `i1`, zero extended to `i32`
- Integer widths are coerced at three boundaries: binary operands, stores, and returns
- Dereference picks its load type from the declared pointer type, so `**pp` works
- Calls to functions Catalyst has never seen (`printf`, etc.) are implicitly declared as `i32 (i32, ...)` so they link against libc

## Not supported yet

Ranked by how much they are needed:

1. **Arrays and indexing** — no `[]`, no aggregate types
2. **Pointer arithmetic** — `p + 1` is rejected by the type checker
3. **Float and double arithmetic** — the types parse, declare, and store, but binary operations assume integers
4. **Compound and increment operators** — no `+=`, `++`, `--`
5. **Bitwise binary operators** — `& | ^ << >>` are not parsed (only unary `~`)

Also missing: `struct`, `union`, `enum`, `typedef`, string literals (lexed, never consumed), casts, `sizeof`, `switch`, `goto`, the preprocessor and `#include`, multiple translation units, `unsigned` and `short` qualifiers, and debug info.

Known behavioral gaps:

- Functions are hoisted, so any function can call any other regardless of source order. This is more permissive than C99.
- Character literals accept only ASCII alphanumerics, so `'\n'` and `'+'` do not lex.
- No check that a non-void function returns on every path; a function that falls off the end gets an implicit zero return.
- `main`'s signature is not validated, only its existence.

## Contributing

Highest value areas right now: arrays, pointer arithmetic, a float code generation path, and an actual test harness.

*Educational project, in active development.*
