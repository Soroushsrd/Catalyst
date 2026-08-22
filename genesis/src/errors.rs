use std::{error::Error, fmt::Display};

#[derive(Debug, Clone)]
pub struct PreprocessorErr {
    pub error_type: ErrorType,
    pub message: String,
}

#[derive(Debug, Clone)]
pub enum ErrorType {
    SyntaxError,
    NotIdentifier,
    NotNumber,
    NotStringLiteral,
    NotCharLiteral,
    NotHeaderFile,
    WrongPunctuator,
    MissingToken,
    UnexpectedEOF,
    UnexpectedToken,
}

impl Default for PreprocessorErr {
    fn default() -> Self {
        Self::new(ErrorType::SyntaxError, "")
    }
}

impl Display for PreprocessorErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "\x1b[31merror[{}]\x1b[0m: {}]",
            self.error_code(),
            self.message
        )
    }
}

impl Error for PreprocessorErr {}

impl PreprocessorErr {
    pub fn new(error_type: ErrorType, message: &str) -> Self {
        Self {
            error_type,
            message: message.to_string(),
        }
    }

    fn error_code(&self) -> &str {
        match self.error_type {
            ErrorType::SyntaxError => "PE001",
            ErrorType::NotIdentifier => "PE002",
            ErrorType::NotNumber => "PE003",
            ErrorType::NotStringLiteral => "PE004",
            ErrorType::NotCharLiteral => "PE005",
            ErrorType::NotHeaderFile => "PE006",
            ErrorType::MissingToken => "PE007",
            ErrorType::UnexpectedToken => "PE008",
            ErrorType::WrongPunctuator => "PE009",
            ErrorType::UnexpectedEOF => "PE010",
        }
    }
}
