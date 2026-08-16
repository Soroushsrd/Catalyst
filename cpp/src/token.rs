pub enum PPTokenType {
    /// any sequence of letters, digits, or underscores
    /// which begin with _ or a letter
    Ident,
    /// All normal ints and float consts as well as
    /// numbers defined with 'e+', 'e-', 'E+', 'E-',
    /// 'p+', 'p-', 'P-', 'P+'
    /// example: 0xE+12
    /// if a '.' appears in the number, after it a decimal digit is required
    /// after which any number of digits, '_' or numbers can occur
    Number,
    /// String consts. anything as "..." counts as string const
    /// inside "...", a backslash is used to escape chars
    StringLiteral(String),
    /// Char consts. anything as '...' counts as char const
    /// inside '...', a backslash is used to escape chars
    /// prefix forms include 'L', 'u', 'U', 'u8', and R"..."
    CharLiteral,
    /// Header file names (#include). they are in "..." or <...> format
    /// which defines where the preprocessor should look at
    /// inside header names, backslash is an ordinary char
    HeaderFile,
    Punc(Punct),
    /// any other single char is considered 'other'. it passes through unmodified
    /// the compiler will reject it later on
    /// '@', ``, and '$' as well as control characters other than NUL.
    /// all characters with the high bit set are also 'other' (numeric range 0x7F-0xFF)
    /// Nul is usually ignored (^@ is ascii nul)
    Other,
    EOF,
}

/// All normal punctuators in ASCII except '@' and '$' and `` .
/// all two and three char operators are punctuators as well
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Punct {
    // brackets
    LBracket, // [    also <:
    RBracket, // ]    also :>
    LParen,   // (
    RParen,   // )
    LBrace,   // {    also <%
    RBrace,   // }    also %>

    // member access
    Dot,   // .
    Arrow, // ->

    // increment / decrement
    PlusPlus,   // ++
    MinusMinus, // --

    // unary / ambiguous
    Amp,   // &
    Star,  // *
    Plus,  // +
    Minus, // -
    Tilde, // ~
    Bang,  // !

    // arithmetic / bitwise / shift
    Slash,   // /
    Percent, // %
    Shl,     //
    Shr,     // >>
    Caret,   // ^
    Pipe,    // |

    // relational / equality
    Lt,   //
    Gt,   // >
    Le,   // <=
    Ge,   // >=
    EqEq, // ==
    Ne,   // !=

    // logical
    AmpAmp,   // &&
    PipePipe, // ||

    // conditional / statement
    Question, // ?
    Colon,    // :
    Semi,     // ;
    Ellipsis, // ...

    // assignment
    Eq,        // =
    StarEq,    // *=
    SlashEq,   // /=
    PercentEq, // %=
    PlusEq,    // +=
    MinusEq,   // -=
    ShlEq,     // <<=
    ShrEq,     // >>=
    AmpEq,     // &=
    CaretEq,   // ^=
    PipeEq,    // |=

    // separators and the preprocessor's own operators
    Comma,    // ,
    Hash,     // #    also %:
    HashHash, // ##   also %:%:
}
