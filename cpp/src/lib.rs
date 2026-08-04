//! Cpp (C preprocessor) has different responsiblities. The entire file runs through
//! each transformation before next one begins (can happen all at once).
//! *Textual Transformation*
//! 1. The input file is read into memory and broken into lines.
//!    for each file, its EOF is declared/found
//! 2. Continued lines are merged into one long line
//!    A continued line is a line which ends with a backslash '\'
//!    The backslash is removed and the following line is joined with the current one
//!    Line splits may happen even in the middle of a word! but usually
//!    happens at white space
//!    if there is white space between a backslash and the end of a line, that is
//!    still a continued line (compiler can actually raise an error)
//! 3. All comments are replaced with single spaces (blocks and single line comments)
//!
//! Preprocessor tokens fall into five classes:
//! identifiers, preprocessing numbers, string literals, punctuations, others
//!
//! - Identifiers: any sequence of letters, digits or underscores which begin
//!   with a underscore or a letter. The only keyword in regard to preprocessor
//!   is the word 'defined'
//! - Preprocessing numbers: all normal ints, float consts as well as numbers defined
//!   as 'e+', 'e-', 'E+', 'E-', 'p+', 'p-', 'P-', 'P+'. example:
//!   '0xE+12' is a preprocessing number without any meaning and is not the same as
//!   '0xE + 12'
//! - String literals: string consts, char consts, and header file names (#include)
//!   anything as '...' or "..." counts as char and string consts. No limit on their
//!   length but the value of a char const that contains more than one char is
//!   implementation defined. header file names are in "..." or <..> format.
//!   The preprocessor looks for headers in different places based on which one u use.
//!   No string literal may extend past the EOL.
//! - Punctuators: all normal punctuations in ASCII except @, $ and '
//!   All two and three character operators are punctuators as well. There are six
//!   digraphs which are merely alternative ways to spell other punctuators:
//!   Digraphs:   <%  %>   <:  :>   %:  %:%:
//!   Punctuator: {    }   [    ]    #   ##
//! - others: Any other single character is considered 'other'. it will pass unmodified
//!   C compiler will later reject them. in ASCII these are @ $ and '
//!   as well as control characters other that NUL. All characters with the high bit
//!   set are also 'other' (numeric range 0x7F - 0xFF)
//!   In comments, NUL is ignored and in oher places, its usually considered whitespace
//!   THese two are the same:
//!   #define X^@1
//!   #define X  1   (^@ is ASCII NUL)
//!   Within string or character constants, Nuls are preserved and preprocessor emits
//!   a warning
//!
//! *The preprocessing language*
//! The pp language consists of directives to be executed and macros to be expanded:
//!  1. Inclusion of header files. (subtitution)
//!  2. Macro expansion: macros produce fragments of C code. The pp will replace macros
//!     with their definition throughout the progrram.
//!  3. COnditional compilation: you can include or exclude parts of the program
//!     according to various conditions
//!  4. line control: we need to let compiler know about the original line numbers when
//!     rearranging source files and subtituting code.
//!  5. Diagnostics: issuing errors or warnings
//!
//! All of the features above except macro expansion are 'pp directives'. PP directives
//! are lines in the program that start with '#'. Whitespace is allowed before and
//! after #. The '#' is followed by an identifier (the directive name). It specifies
//! the operation to perform (#name). for example, '#define' is the directive that
//! defines macros
//! The '#' which begins a directive cannot come from a macro expansion. also the
//! directive name is not macro expanded.
//! Programs cannot define new pp directives and their names are fixed!
//! Some directives require arguments which come after the directive (same line)
//! and must be sparated from the directive name by whitespace
//! #define <macro name> <intended expansion>
//! a directive cannot cover more than one line, the line may however be continued
//! with 'backslash-newline' or by a block comment which extens past the EOL.
//! in this case, continuation have already merged wit hthe first line to make one
//! long line

pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
