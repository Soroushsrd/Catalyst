# cpp — a C preprocessor as a standalone crate

Spec notes for what a C preprocessor is expected to do, and how GNU cpp (the reference
implementation everybody is compared against) actually does it. Everything here is
behaviour, not implementation.

Ordering of the whole pipeline (matches C's "phases of translation"):

```text
phase 1:   map input to the source character set
phase 2:   textual transformations (line splicing, comments)
phase 3:   tokenization into preprocessing tokens
phase 4:   execute directives, expand macros  <- "the preprocessor" to most people
phase 5/6: string/char consts converted to execution charset, adjacent strings
           concatenated — usually the compiler's job, not ours
```

Conceptually each phase runs over the whole file before the next begins. Real
implementations fuse them for performance; we may too, but observable behaviour must
match the strict ordering.

---

## 0. Character sets

There are (at least) four charsets in play:

- **input charset** — whatever the file is on disk. gcc: `-finput-charset=`.
- **source charset** — what the preprocessor works in internally. Must be isomorphic to
  Unicode; GNU cpp uses UTF-8. Conversion happens before we even look for line boundaries.
- **execution charset** — string/char constants are converted into this after
  preprocessing. Default UTF-8. (`-fexec-charset=`)
- **wide execution charset** — for `L"..."` / `L'...'`. UTF-16 or UTF-32 depending on the
  target's `wchar_t`. (`-fwide-exec-charset=`)

Octal and hex escapes are NOT converted: `'\x12'` is `0x12` in whatever charset. Every
other escape is resolved in the source charset first, then converted like an ordinary
character.

Identifiers: classic gcc only allows ASCII. Non-ASCII identifiers and `\u`/`\U` escapes in
identifiers are gated behind `-fextended-identifiers` (experimental in the era of this
manual, standard behaviour in modern C/C++).

---

## 1. Textual transformation

The entire file runs through each transformation before the next one begins (can happen
all at once).

**1. The input file is read into memory and broken into lines.**
For each file, its EOF is declared/found. Accepted EOL markers: LF (unix), CR LF (dos),
CR (classic mac). All three must work, and may even be mixed in one file (gcc may then
lose track of line numbers, which is its problem, not an error). If the last line has no
EOL marker, one is implicitly supplied — the C standard calls this UB; gcc warns and
carries on.

**2. If trigraphs are enabled, replace them with their single character.**
Off by default in gcc; enabled by `-trigraphs` or a strict `-std`. Nine sequences, all
beginning with `??`:

```text
Trigraph:    ??(  ??)  ??<  ??>  ??=  ??/  ??'  ??!  ??-
Replacement:  [    ]    {    }    #    \    ^    |    ~
```

Note `??/` is a backslash, so it can *create* a line continuation. That is the only case
where a trigraph inside a comment can change meaning, which is why `-Wtrigraphs` only
fires for that case inside comments. Trigraphs were removed in C++17 and are dead weight;
still worth a flag.

**3. Continued lines are merged into one long line.**
A continued line is a line which ends with a backslash `\`. The backslash is removed and
the following line is joined with the current one. Line splits may happen even in the
middle of a word! but usually happens at white space. If there is white space between a
backslash and the end of a line, that is still a continued line (compiler can actually
raise an error). There is no way to escape a trailing backslash so it is *not* a splice.

**4. All comments are replaced with single spaces** (blocks and single line comments).
Block comments `/* ... */` do not nest: the first `*/` ends it. Line comments `//` run to
EOL, and also do not nest. A line comment may be continued onto the next line with
backslash-newline. Comments are NOT recognised inside string literals: `"/* blah */"` is
a string, not an empty one. Because comment removal happens in this phase, a block
comment that extends past EOL also continues a directive line.

Consequence of doing these first: `/*`, `*/`, `//` and even `#define` itself can be split
across lines with backslash-newline and it still works. Cursed, but legal:

```text
/\
*
*/ # /*
*/ defi\
ne FO\
O 10\
20
```

is exactly `#define FOO 1020`. A test suite should contain this.

**Line-number bookkeeping.** After splicing, we still need the *original* line and column
of every token for diagnostics and for `__LINE__`. Do not throw away the mapping when
merging lines. Tab width for column reporting is configurable (`-ftabstop=`, default 8).

---

## 2. Tokenization

Preprocessing tokens fall into five classes: **identifiers, preprocessing numbers, string
literals, punctuators, other**.

White space separates tokens but is not itself a token. Tokens need not be separated by
whitespace.

Lexing is *greedy* / maximal munch: starting from the left, each token is made as large as
possible. `a+++++b` lexes as `a ++ ++ + b` and is therefore a syntax error later, even
though `a ++ + ++ b` would have been valid C. The preprocessor does not backtrack to help
the parser.

Once lexed, token boundaries never change again — the only exception is `##`, which pastes
two tokens into one. The compiler does not re-tokenize our output: each pp-token becomes
exactly one compiler token. This is why:

```text
#define foo() bar
foo()baz        ->  bar baz     (not barbaz)
```

and why textual output must insert a space wherever a paste would otherwise happen
accidentally.

### Identifiers

Any sequence of letters, digits or underscores which begins with an underscore or a
letter. The only keyword in regard to the preprocessor is the word `defined`.
C keywords are ordinary identifiers here; you may `#define` one. (C++ named operators are
the exception, see §6.7.) gcc treats `$` as a letter by default on most targets
(`-fdollars-in-identifiers` / `-fno-...`); not when preprocessing assembly.

### Preprocessing numbers

All normal ints and float consts, as well as numbers defined with `e+`, `e-`, `E+`, `E-`,
`p+`, `p-`, `P+`, `P-`. Example: `0xE+12` is a preprocessing number without any meaning
and is not the same as `0xE + 12`.

Formal shape: optional `.`, then a required decimal digit, then any run of letters,
digits, underscores, periods and the exponent pairs above.

The point of this loose definition is that the preprocessor never has to decide whether a
numeric constant is *valid* — that is the compiler's job — and that any identifier can be
split anywhere and pasted back with `##`.

### String literals

String consts, char consts, and header file names (`#include`). Anything as `'...'` or
`"..."` counts as char and string consts. No limit on their length, but the value of a
char const that contains more than one char is implementation defined. Header file names
are in `"..."` or `<...>` format; the preprocessor looks for headers in different places
based on which one you use. No string literal may extend past the EOL.

Inside `"..."` and `'...'`, embedded quotes escape with backslash. Inside header names,
backslash is an ordinary character and there is *no* escaping at all. Prefixed forms
(`L`, `u`, `U`, `u8`, and `R"..."` raw strings in C++) are later additions worth handling
if we want to be usable on modern code.

### Punctuators

All normal punctuation in ASCII except `@`, `$` and `` ` `` (backtick). All two- and
three-character operators are punctuators as well. There are six digraphs which are merely
alternative ways to spell other punctuators:

```text
Digraph:    <%  %>   <:  :>   %:  %:%:
Punctuator: {    }   [    ]    #   ##
```

Digraphs are real tokens, so `%:define` is a valid directive introducer. Unlike trigraphs
they are a lexing-level alias, not a text substitution, so `"<%"` inside a string stays
`<%`.

### Other

Any other single character is considered "other". It passes through unmodified and the C
compiler will later reject it. In ASCII these are `@`, `` ` `` and (usually) `$`, as well
as control characters other than NUL. All characters with the high bit set are also
"other" (numeric range 0x7F–0xFF).

In comments NUL is ignored, and elsewhere it is usually considered whitespace. These two
are the same (where `^@` is ASCII NUL):

```text
#define X^@1
#define X  1
```

Within string or character constants, NULs are preserved and the preprocessor emits a
warning.

---

## 3. The preprocessing language

The pp language consists of directives to be executed and macros to be expanded:

1. **Inclusion of header files** (substitution).
2. **Macro expansion** — macros produce fragments of C code. The pp will replace macros
   with their definition throughout the program.
3. **Conditional compilation** — you can include or exclude parts of the program according
   to various conditions.
4. **Line control** — we need to let the compiler know about the original line numbers
   when rearranging source files and substituting code.
5. **Diagnostics** — issuing errors or warnings.

All of the features above except macro expansion are *pp directives*. PP directives are
lines in the program that start with `#`. Whitespace is allowed before and after the `#`.
The `#` is followed by an identifier (the directive name), which specifies the operation to
perform. For example `#define` is the directive that defines macros.

The `#` which begins a directive cannot come from a macro expansion, and the directive
name is not macro expanded. Programs cannot define new pp directives; their names are
fixed.

Some directives require arguments which come after the directive (same line) and must be
separated from the directive name by whitespace:

```text
#define <macro name> <intended expansion>
```

A directive cannot cover more than one line. The line may however be continued with
backslash-newline or by a block comment which extends past the EOL — in that case the
continuations have already been merged with the first line to make one long line.

Full directive set we must recognise:

```text
#include  #include_next  #import
#define   #undef
#if  #ifdef  #ifndef  #elif  #else  #endif
#line
#error  #warning
#pragma                  (+ the _Pragma operator)
#ident  #sccs            (deprecated, System V leftovers)
#assert  #unassert       (obsolete gcc extension)
#                        (null directive — '#' alone on a line, produces nothing)
```

Anything else after `#` is an "invalid preprocessing directive" error — except inside a
skipped conditional block, where unknown directives are ignored.

---

## 4. Header files and `#include`

### 4.1 Syntax

- `#include <file>` — system form. Searched in the bracket chain.
- `#include "file"` — user form. Searched first in the directory of the file containing
  the directive, then the quote chain (`-iquote`), then the bracket chain.

The argument behaves like a string constant: comments are not recognised inside it and
macro names are not expanded. `#include <x/*y>` includes a file literally named `x/*y`.
Backslashes are ordinary characters, no escapes are processed: `#include "x\n\\y"` names a
file with three backslashes in it. Anything other than a comment after the filename is an
error.

### 4.2 Operation

Textual: scan the named file to completion, then resume. The output is everything so far,
then the included file's output, then the rest.

An included file must consist of *complete tokens*: an unterminated comment or string
literal at EOF is invalid (for error recovery, treat it as closed at EOF). The line after
the `#include` is always treated as a new line even if the included file lacked a final
newline.

### 4.3 Search path

Bracket chain on a normal unix box, in order:

```text
/usr/local/include
libdir/gcc/<target>/<version>/include
/usr/<target>/include
/usr/include
```

`-I dir` prepends to this (left to right). If dir is already a default system dir, the
`-I` is ignored so the system-header treatment is not defeated. Duplicates are removed
from the quote and bracket chains *before* the two are merged, so a dir can still appear
twice in the final chain if it was named in both. `-nostdinc` drops the defaults;
`-isystem` appends after all `-I` and marks the dir as a system dir; `-idirafter` appends
after everything. `#line` does not change what "the directory containing the current file"
means.

### 4.4 Include guards / once-only

The portable idiom:

```text
#ifndef FILE_FOO_SEEN
#define FILE_FOO_SEEN
  ...entire file...
#endif
```

The macro is the *controlling macro* / *guard macro*.

Optimization worth implementing: after scanning a file, detect that its entire contents
were wrapped in such a conditional and remember the guard name. On a later `#include` of
the same file, if the guard is still defined, skip opening the file at all. Comments
outside the wrapper must not defeat this.

Two non-portable alternatives, both of which we should support and neither of which we
should recommend:

- `#import` — includes at most once. Standard in Objective-C, deprecated elsewhere. In
  gcc, one `#import` prevents the file from ever being read again, by `#import` or
  `#include`.
- `#pragma once` — file is never read again, period. Not universally supported but in
  practice supported by every compiler anyone uses today. Identity of a file must be by
  inode/dev or canonical path, not by the spelling in the directive, or symlinks and `..`
  break it.

### 4.5 Computed includes

If the first non-whitespace character after `#include` is not `"` or `<`, the rest of the
line is macro-expanded as running text, and then:

- If it expands to a single string constant → its contents are the filename, searched by
  the quote rules. The string is NOT re-examined for embedded quotes and backslash escapes
  are NOT processed: `#define HEADER "a\"b"` + `#include HEADER` looks for the file `a\"b`.
- If it expands to a token stream starting with `<` and containing a `>`, the tokens
  between them are joined into the filename, searched by bracket rules. Whitespace between
  tokens collapses to one space; a space right after `<` is kept, a space before `>` is
  dropped.
- Anything else, or extra tokens after the filename → error.

This is implementation-defined territory; recommend users stick to a single object-like
macro expanding to a string constant.

### 4.6 `#include_next`

GNU extension for wrapper headers. Same as `#include` except the search starts *after* the
directory in which the current file was found. Does not care about `<>` vs `""` and does
not check that the name matches the current file. Only sane use is global fixups
(fixincludes-style), never in a program's own headers.

### 4.7 System headers

Headers found in system directories (or after `#pragma GCC system_header`, or in an
`-isystem` dir) get warnings suppressed, except `#warning`. Macros defined in a system
header also carry partial warning immunity where they are expanded.
`#pragma GCC system_header` affects the rest of the current file only, and does nothing in
the primary source file.

---

## 5. Macros

A macro is a named fragment of tokens. Two kinds: object-like and function-like.
`#define` stores *tokens*, not text. Any valid identifier may be a macro, even a C
keyword. `defined` may never be defined as a macro; nor may C++'s named operators when
compiling C++.

### 5.1 Object-like

`#define BUFFER_SIZE 1024`. Body ends at end of the (already spliced) line. Expansion is
rescanned for further macros, so definition order does not matter for the *body*, only for
the moment of use:

```text
#define TABLESIZE BUFSIZE
#define BUFSIZE 1024
TABLESIZE  ->  BUFSIZE  ->  1024
```

Redefining `BUFSIZE` later changes what `TABLESIZE` expands to. The `#define` for
`TABLESIZE` never looked inside its own body.

### 5.2 Function-like

`#define lang_init() c_init()`. Only expanded when the name is followed by `(`. A bare
mention of the name is left alone — that is what makes `funcptr = foo;` still take the
address of the real function.

Whitespace between the name and `(` *in the definition* makes it object-like:
`#define lang_init () c_init()` + `lang_init()` gives `() c_init()()`. Whitespace between
name and `(` *at the call site* is fine and irrelevant. An invocation may span any number
of lines.

### 5.3 Arguments

- Argument count must match exactly. Empty arguments are legal (`min(,b)` works; `min()`
  for a 2-arg macro is an error).
- Leading/trailing whitespace in each argument is dropped; internal whitespace runs
  collapse to one space.
- Parentheses inside an argument must balance and hide commas. Brackets and braces do NOT:
  `macro(array[x = y, x + 1])` passes two args. (This is why C++ template commas need
  extra parens.)
- Whitespace is not a token, so `foo ()` and `foo ( )` both pass one empty arg.
- Parameters inside string literals are not substituted: `#define foo(x) x, "x"` +
  `foo(bar)` → `bar, "x"`.
- Arguments are fully macro-expanded *before* substitution (see prescan, §5.9), unless the
  parameter is an operand of `#` or `##`.

### 5.4 Stringification `#`

`#param` in a function-like macro body becomes a string constant of the literal text of
the argument, un-expanded.

- Leading/trailing whitespace dropped, internal runs collapse to one space.
- Comments already became whitespace, so they never appear.
- Quotes around embedded string constants get backslash-escaped, and backslashes *inside*
  string/char constants are doubled. A backslash not inside a literal is not doubled:
  `\n` alone stringifies to `"\n"`.
- There is no way to stringify into a *character* constant.
- To stringify the *expansion* of an argument you need the two-level trick:
  `#define xstr(s) str(s)` / `#define str(s) #s`.

### 5.5 Concatenation `##`

Pastes the two adjacent tokens into one. Both operands, if they are parameters, are
substituted un-expanded. Empty argument → the `##` does nothing.

The result must be a single valid token; `x ## +` is a warning and both tokens are
emitted. `##` may not appear at either end of a macro body. It cannot be used to build a
comment (`/` ## `*`), because comments are gone before macros are considered.

Order of evaluation of a chain of `#`/`##` is unspecified by the standard; gcc 3.0+
evaluates `#` and `##` together, strictly left to right. Anyone depending on order should
nest macros instead.

### 5.6 Variadic macros

```text
#define eprintf(...) fprintf(stderr, __VA_ARGS__)
```

Everything after the last named parameter, commas included, becomes one variable argument,
substituted for `__VA_ARGS__`. It is fully macro-expanded like an ordinary argument, and
may be stringified or pasted.

GNU extensions worth supporting for old code:

- Named variable argument: `#define eprintf(args...) fprintf(stderr, args)`. Cannot be
  combined with `__VA_ARGS__` in the same macro.
- Omitting the variable argument entirely is allowed (pedantic warning).
- `, ## __VA_ARGS__` deletes the preceding comma when the variable argument is omitted.
  Only when the token before `##` is literally a comma; passing an *empty* argument is
  different from *omitting* it. When the only parameter is the variadic one, gcc keeps the
  comma in strict-std modes and drops it otherwise. (C++20/C23 replace this mess with
  `__VA_OPT__` — worth adding.)

`__VA_ARGS__` may only appear in the replacement list of a variadic macro.

### 5.7 Predefined macros

Three families (four in C++), all with reserved-ish names.

**Standard:** `__FILE__`, `__LINE__`, `__DATE__` (`"Feb 12 1996"`, day space-padded),
`__TIME__` (`"23:59:01"`), `__STDC__`, `__STDC_VERSION__` (`199409L`/`199901L`/…),
`__STDC_HOSTED__`, `__cplusplus`, `__OBJC__`, `__ASSEMBLER__`.

- `__FILE__` is the path as *opened*, not as spelled in the `#include`.
- Both `__FILE__` and `__LINE__` switch to the included file and revert on return (with
  `__LINE__` then incremented past the `#include` line).
- `__func__`/`__FUNCTION__` are NOT macros — the preprocessor has no idea what function it
  is in. Do not try.
- If the date/time cannot be determined, expand to `"??? ?? ????"` / `"??:??:??"` and warn
  once.

**Common (GNU):** `__COUNTER__` (increments from 0 on each expansion), `__GNUC__` /
`__GNUC_MINOR__` / `__GNUC_PATCHLEVEL__`, `__BASE_FILE__`, `__INCLUDE_LEVEL__`,
`__TIMESTAMP__`, `__STRICT_ANSI__`, `__ELF__`, `__VERSION__`, `__OPTIMIZE__`,
`__CHAR_BIT__`, `__SIZEOF_*__`, `__*_MAX__`, `__SIZE_TYPE__` and friends, `__LP64__`.
These exist so that `limits.h`/`stddef.h` can be written portably.

**System-specific:** `unix`, `__unix__`, `_mips`, `__mips__`, … All expand to 1 so they
work with both `#ifdef` and `#if`. The un-underscored spellings are outside the reserved
namespace and are suppressed under `-ansi`/`-std=`. Always provide the `__x__` parallel of
any `x` we define.

**C++ named operators:** see §6.7.

`cpp -dM` dumps everything predefined; a good conformance test target.

### 5.8 `#undef` and redefinition

`#undef NAME` takes a bare name (no parens even for function-like macros), and is a no-op
if NAME is not a macro. Anything after the name is an error.

Redefining an existing macro is only silent if the new definition is *effectively the
same*:

- Same kind (object vs function-like)
- Identical replacement token list
- Identical parameter list
- Whitespace present in the same places (amount does not matter; comments count as
  whitespace)

Otherwise: warn, and take the new definition. This is what lets two headers define the
same macro without complaint.

### 5.9 Directives inside macro arguments

Formally UB. gcc ≥ 3.2 processes such directives exactly as it would have if the macro
call were not there. If the macro is redefined mid-invocation, the new definition applies
to argument pre-expansion but the old one is used for argument replacement. Pathological
but testable:

```text
#define f(x) x x
f (1
#undef f
#define f 2
f)          ->   1 2 1 2
```

### 5.10 Pitfalls to document (and to test against)

1. **Misnesting.** A macro call can be assembled from part body, part argument:
   `call_with_1(twice)` → `twice(1)` → `(2*(1))`. Bodies need not have balanced parens, so
   a call can begin inside a body and end outside it.
2. **Precedence.** Always parenthesize parameters and the whole body.
   `#define ceil_div(x,y) (x + y - 1) / y` breaks on `ceil_div(b & c, sizeof(int))` and on
   `sizeof ceil_div(1,2)`.
3. **Swallowing the semicolon.** Wrap multi-statement bodies in `do { ... } while (0)` so
   `MACRO(x);` before an `else` stays one statement.
4. **Duplicated side effects.** `min(x+y, foo(z))` calls `foo` twice. The GNU
   statement-expression `({ typeof(X) x_ = (X); ... })` is the only fix in-language.
5. **Self-reference.** A macro whose expansion contains its own name is NOT re-expanded —
   the self-reference is passed through unchanged, which is what stops infinite recursion.
   `#define foo (4 + foo)` expands exactly once. Indirect self-reference counts too:

   ```text
   #define x (4 + y)
   #define y (2 * x)
   x  ->  (4 + (2 * x))
   ```

   Implementation: mark tokens as "blue-painted" for a given macro name so they are never
   reconsidered, including on the second scan. This is the single most important piece of
   state in the expander. Idiom: `#define EPERM EPERM` makes a name testable with `#ifdef`
   while leaving it alone in running text.
6. **Argument prescan.** Arguments are expanded, substituted, then the whole result is
   scanned again. Usually invisible; it matters in three cases — nested calls `f(f(1))`
   work because of it; macros that stringify or paste must be wrapped in another macro to
   get expansion first; and an argument whose expansion contains unshielded commas will be
   re-split on the second scan and blow the argument count (`#define foo a,b` +
   `bar(foo)`).
7. **Newlines in arguments.** A multi-line invocation reports errors on the line where the
   invocation started, not where the bad token was. gcc calls this a bug; we can do better
   by tracking per-token locations.

---

## 6. Conditionals

A conditional selects whether a chunk of tokens reaches the compiler at all. Directives:
`#if`, `#ifdef`, `#ifndef`, `#elif`, `#else`, `#endif`.

### 6.1 Structure

Groups must nest completely, and `#endif` always matches the nearest opener. A conditional
group cannot start in one file and end in another. `#else` may follow any number of
`#elif`s; `#elif` may not follow `#else`. Text after `#else`/`#endif` is invalid C but
accepted with a warning (`-Wendif-labels`, on by default); it never changes which opener is
matched.

### 6.2 Skipped text

Even in a failing branch, the text is still run through phases 1–3, so comments and string
literals inside it must still be lexically well formed. Directives inside it are not
executed, but conditional directives must still be *counted* to find the matching `#endif`.
Unknown directives inside a skipped block are ignored rather than errors.

### 6.3 `#if` expressions

Integer constant expressions only, with these rules:

- Integer constants; character constants interpreted as in normal code (multi-char
  constants: shift left by char width and or-in each char, result typed `int` and
  therefore signed).
- The usual arithmetic, bitwise, shift, comparison and logical operators, with real
  short-circuiting for `&&` and `||`.
- All macros are expanded before evaluation.
- The `defined` operator (see below).
- Any remaining identifier evaluates to 0. Function-like macro names used without parens
  also count as 0. `-Wundef` warns about this.
- NO `sizeof`, NO `enum` constants, NO types, NO casts. They lex as identifiers and become
  0, which usually makes the expression invalid.
- Arithmetic is done in the widest integer type the target knows (64-bit on most gcc
  targets), which is NOT the same rule the compiler uses for constant expressions, so
  results can differ. Signedness follows the usual promotion rules; unsigned constants
  (`123U`) exist.

Nonzero → the branch is taken.

### 6.4 `defined`

`defined NAME` and `defined (NAME)` → 1 if NAME is currently a macro, else 0.
`#if defined MACRO` is exactly `#ifdef MACRO`. Useful for testing several names at once:
`#if defined(__vax__) || defined(__ns16000__)`.

`#if defined BUFSIZE && BUFSIZE >= 1024` can usually be shortened to
`#if BUFSIZE >= 1024` since undefined names are 0.

If `defined` arrives via macro expansion the standard says UB; gcc honours it and warns
under `-pedantic`. Decide and document our choice.

### 6.5 Deleted code

`#if 0` is the correct way to comment out code, because block comments do not nest.
Requirements: the enclosed region must still tokenize (unbalanced single quotes will bite —
apostrophes in English prose are the classic failure), and any conditionals inside must be
balanced. `#ifdef notdef` is folklore and is risky; `#if 0` cannot accidentally become
true.

### 6.6 Why conditionals exist (for the docs, not the code)

Target/OS-specific code that would not even *compile* elsewhere; debug vs release builds of
one source file; and keeping dead code around for reference. Definitions vary via
`-D`/`-U`, system headers, predefined macros, or a generated `config.h`.

### 6.7 C++ named operators

Eleven keywords that are alternate spellings of punctuators and are significant *in the
preprocessor* when compiling C++: they act as operators in `#if`, and they cannot be
defined as macros or poisoned.

```text
and     &&      and_eq  &=      bitand  &       bitor   |
compl   ~       not     !       not_eq  !=      or      ||
or_eq   |=      xor     ^       xor_eq  ^=
```

In C they are plain identifiers; `<iso646.h>` defines them as ordinary macros.

---

## 7. Diagnostics

- `#error <tokens>` — fatal; the rest of the line is the message.
- `#warning <tokens>` — same but preprocessing continues. (GNU extension originally;
  standardized later as `#warning` in C23.)

Neither macro-expands its argument. Internal whitespace runs collapse to a single space.
The line must consist of complete tokens, so the message should be a single string constant
— otherwise an apostrophe starts a char constant and the diagnostic itself fails to lex.

---

## 8. Line control

We must tell the compiler where each token really came from. Everything produced by a macro
expansion is reported at the line where the outermost macro was used.

`#line` has three forms:

- `#line N` — the next line is line N; subsequent lines count from there. N is a
  non-negative decimal constant.
- `#line N "file"` — also changes the reported filename from the next line on. The
  filename IS a real string constant here: backslash escapes are interpreted (unlike
  `#include`). Older gcc got this wrong; 3.1+ is correct.
- `#line <anything>` — macro-expanded, and the result must match one of the above.

`#line` changes `__FILE__` and `__LINE__` from that point on. It does NOT change which
directory `#include "..."` searches first (this changed in gcc 3.0, and the old behaviour
broke generated parsers shipped in tarballs). Primary consumer: code generators like
bison/yacc/flex.

---

## 9. Pragmas

`#pragma` is the standard escape hatch for compiler-specific instructions. A compiler may
attach any meaning it likes to pragmas it does not recognise, and must ignore ones it does
not know. No macro expansion happens on a `#pragma` line. Pragmas we do not handle
ourselves are passed through to the output.

`_Pragma("string-literal")` (C99) is an *operator*, not a directive, so it can be produced
by macro expansion — which `#pragma` cannot. The string is "destringized" (`\\` → `\`,
`\"` → `"`) and then processed as if it had been written as `#pragma <that text>`. Works
with normal or wide string literals.

```text
#define DO_PRAGMA(x) _Pragma (#x)
DO_PRAGMA (GCC dependency "parse.y")
```

gcc does not accept `_Pragma` inside a conditional directive like `#if`.

Pragmas that are the *preprocessor's* business:

- `#pragma once` — see §4.4.
- `#pragma GCC dependency "file" [text]` — warn if `file` is newer than the current file
  (searched along the normal include path). For generated sources.
- `#pragma GCC poison id1 id2 ...` — any later appearance of those identifiers is a hard
  error. Exception: a poisoned identifier appearing in the expansion of a macro that was
  defined *before* the poisoning is fine, so system headers do not trip it.
- `#pragma GCC system_header` — treat the rest of this file as a system header.

---

## 10. Other directives

`#ident "string"` — on some systems copies the string into a special object-file section;
elsewhere ignored. `#sccs` is a synonym. Both deprecated, both from System V, neither
standard nor an official GNU extension.

**The null directive:** a `#` alone on a line (whitespace and comments allowed after it) is
a valid directive that produces nothing. It exists so old code with bare `#` lines emits no
output line.

`#assert` / `#unassert` with the `#predicate(answer)` syntax are an obsolete gcc feature,
superseded by ordinary macros. Support only if we care about ancient code; recommend
against it in any case.

---

## 11. Preprocessor output (textual mode)

Integrated into a compiler, cpp hands over a token stream. Standalone (`-E`), it must
produce text that re-lexes to exactly the same tokens.

- Directive lines become blank lines; comments become spaces (unless `-C`/`-CC`).
- Long runs of blank lines are collapsed.
- Whitespace between tokens collapses to one space, except the first token on a
  non-directive line, which is indented to the column it had in the source.
- Never insert whitespace where there was none, EXCEPT where required to prevent an
  accidental paste (`+` `+` must not become `++`).
- Some directives are duplicated into the output: `#ident` always, `#pragma` if we did not
  consume it, `#define`/`#undef` under `-dD`/`-dU`. When we emit one, the `#` goes in
  column 1 with no space before the directive name; if a macro expansion happens to produce
  something that *looks* like a directive, insert a space after the `#` so it cannot be
  confused with one.

**Linemarkers** carry provenance:

```text
# <linenum> "<filename>" <flags>
```

The filename never contains non-printing characters (octal-escape them). Flags:

| Flag | Meaning |
| --- | --- |
| `1` | start of a new file |
| `2` | returning to a file after an include |
| `3` | following text is from a system header (suppress warnings) |
| `4` | following text should be treated as wrapped in `extern "C"` |

Multiple flags are space-separated and must be in ascending order. Linemarkers are never
emitted inside a string or character constant. As an extension, linemarkers are also
*accepted* on input (like `#line` but with trailing flags). `-P` suppresses linemarkers
entirely.

---

## 12. Traditional mode (`-traditional-cpp`)

Pre-standard semantics. Only worth implementing behind a flag, but the differences are
instructive:

- The input is a text stream, not a token stream; horizontal whitespace, including hard
  tabs, is preserved (this is why it half-works on Makefiles).
- No trigraphs, no digraphs (UB).
- Only `/* */` comments, and only when outside quoted text; `<` in an `#include` also opens
  quoted text. Comments are *deleted*, not replaced with a space, so they act as token-paste
  operators for the compiler's own lexer — but as separators for the preprocessor itself:
  `#if foo/**/bar` is `#if foo bar`.
- Macros expand to text, not tokens. Unmatched quotes are allowed in a replacement list; an
  unterminated literal runs on into the following text.
- `#` and `##` have no special meaning. Stringification is emulated by the fact that
  parameters ARE substituted inside string literals (`#define str(x) "x"`); pasting is
  emulated with an empty comment (`#define suffix(x) foo_/**/x`).
- No recursion protection at all — gcc detects runaway recursion, errors, and moves on.
- No variadic macros.
- Directives are only recognised when `#` is in column 1.
- `__STDC__` is not defined.

`-Wtraditional` warns about constructs that differ: parameters inside string literals,
indented directives, function-like macro names without an argument list, unary plus, and the
`U`/`LL` integer suffixes.

---

## 13. Implementation-defined behaviour and limits

Things we must *choose* and then *document*:

- Source → execution charset mapping
- Which characters may appear in identifiers (`$`, UCNs)
- Collapsing of whitespace runs in textual output
- The numeric value of multi-character character constants in `#if`
- How header files are located
- The filename resulting from a macro-expanded `#include`

Limits (standard minimum → what gcc does):

| Limit | Standard min | gcc |
| --- | --- | --- |
| `#include` nesting | 15 | 200 (to catch runaway recursion) |
| conditional nesting | 63 | memory-bound |
| parenthesis nesting in an expression | 63 | memory-bound |
| significant chars in an identifier | 63 | all of them |
| macros defined at once | 4095 | memory-bound |
| macro parameters / arguments | 127 | 65535 |
| characters on a logical line | 4096 | unlimited (columns past 65535 may be wrong) |
| source file size | unspecified | address-space bound (gcc mmaps the file) |

Rust-side: prefer memory-bound everywhere, but keep a configurable include-depth cap and a
recursion cap so a malicious input cannot OOM the process.

---

## 14. Command line surface worth mirroring

Only the ones that change *behaviour*, not just diagnostics:

- `-D name[=def]` / `-U name` — processed strictly in command-line order. Bare `-D name`
  defines it as 1. The definition is tokenized as if it had appeared in a `#define`.
  Function-like macros are written `-D name(a,b)=...`.
- `-undef` — drop all system-specific and GNU predefines, keep the standard ones.
- `-I`, `-iquote`, `-isystem`, `-idirafter`, `-nostdinc`, `-I-` (deprecated) — search path
  construction, §4.3.
- `-include file` — as if `#include "file"` were the first line of the primary source (but
  searched from the working directory first). `-imacros file` — same, but discard the
  output and keep only the macros. All `-imacros` run before all `-include`; both run after
  all `-D`/`-U`.
- `-M`, `-MM`, `-MD`, `-MMD`, `-MF`, `-MG`, `-MP`, `-MT`, `-MQ` — makefile dependency
  generation. Worth supporting: it is the reason most build systems invoke a preprocessor
  directly. `-MM` skips system headers; `-MP` emits phony targets for each header so
  deleting one does not break the build; `-MG` treats missing headers as generated files
  instead of erroring.
- `-C` / `-CC` — keep comments. `-C` makes comments into tokens in their own right, so a
  comment at the start of a line can stop it being a directive.
- `-fpreprocessed` — input is already preprocessed: no macro expansion, no splicing, no
  trigraphs, most directives skipped; comments still stripped.
- `-fdirectives-only` — handle directives, do not expand macros.
- `-P` — no linemarkers.
- `-dM` / `-dD` / `-dN` / `-dI` / `-dU` — dump macros / definitions / names / includes /
  used macros. `-dM` is the standard way to see all predefines.
- `-std=` / `-ansi`, `-trigraphs`, `-ftabstop=`, `-f*-charset=`.

Environment variables: `CPATH` (like `-I`, after command-line ones), `C_INCLUDE_PATH` /
`CPLUS_INCLUDE_PATH` / `OBJC_INCLUDE_PATH` (like `-isystem`), `DEPENDENCIES_OUTPUT`
(≈ `-MM -MF`), `SUNPRO_DEPENDENCIES` (≈ `-M -MF`). An empty element in any of these means
the current working directory.

---

## 15. Notes for this crate

Suggested split, roughly in dependency order:

- **`source`** — file loading, charset conversion, line map (spliced position → original
  file/line/column). Everything downstream carries a compact `Span` into this, never a
  `String`.
- **`lexer`** — the five pp-token classes, greedy, whitespace-aware. Must record "preceded
  by whitespace" and "first on line" per token — both are needed for stringification, for
  `#` directive detection, and for textual output.
- **`directives`** — the parser for a directive line, over tokens rather than text.
- **`macro`** — definition table, argument collection, substitution, `#`, `##`, the
  hide-set / blue-paint for recursion, prescan ordering.
- **`cond`** — `#if` expression evaluation over the widest integer type, plus the
  skip-scanner that finds the matching `#endif` without executing anything.
- **`include`** — search path chains, guard-macro caching, once-only tracking by canonical
  file identity, include depth limit.
- **`output`** — token-stream API for embedding in a compiler, plus a textual writer that
  emits linemarkers and inserts anti-paste whitespace.
- **`diag`** — every error carries a span; nothing panics on bad input.

Things that will bite, in the order they usually bite:

1. The recursion / hide-set rules (§5.10.5) — get this wrong and everything from
   `<stdio.h>` onward misbehaves.
2. Prescan vs `#`/`##` operand handling (§5.9).
3. Line-map fidelity after splicing, so `__LINE__` and diagnostics agree.
4. Anti-paste whitespace in textual output.
5. Include-guard detection, which is a performance feature, not correctness.

Conformance testing: `gcc -E` on the same input is the oracle. The C standard's own
examples in 6.10.3.5 (the `p[] = "..."` / `x ## s` torture tests) are the cheapest
high-value test suite in existence — start there.
