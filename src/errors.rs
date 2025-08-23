#[derive(Debug, Clone)]
pub struct CompilerError {
    pub error_type: ErrorType,
    pub line: usize,
    pub column: usize,
    pub message: String,
    pub source_line: Option<String>,
    pub suggestion: Option<String>,
}

#[derive(Debug, Clone)]
pub enum ErrorType {
    SyntaxError,
    SemanticError,
    TypeError,
    UndefinedVariable,
    InvalidAssignment,
    MissingToken,
    UnexpectedToken,
    InvalidOperation,
}

impl CompilerError {
    pub fn new(error_type: ErrorType, line: usize, column: usize, message: &str) -> Self {
        Self {
            error_type,
            line,
            column,
            message: message.to_string(),
            source_line: None,
            suggestion: None,
        }
    }

    pub fn with_source_line(mut self, source_line: &str) -> Self {
        self.source_line = Some(source_line.to_string());
        self
    }

    pub fn with_suggestion(mut self, suggestion: &str) -> Self {
        self.suggestion = Some(suggestion.to_string());
        self
    }

    pub fn format_error(&self) -> String {
        let mut output = String::with_capacity(150);
        output.push_str(&format!(
            "\x1b[31merror[{}]\x1b[0m: {}\n",
            self.error_code(),
            self.message
        ));
        output.push_str(&format!(
            "  \x1b[34m-->\x1b[0m line {}:{}\n",
            self.line, self.column
        ));

        if let Some(source_line) = &self.source_line {
            output.push_str("   |\n");
            output.push_str(&format!("{:3}| {}\n", self.line, source_line));

            let spaces = if self.column > 0 { self.column - 1 } else { 0 };
            output.push_str(&format!("   | {}\x1b[31m^\x1b[0m\n", " ".repeat(spaces)));
        }

        if let Some(suggestion) = &self.suggestion {
            output.push_str(&format!("\n\x1b[36mhelp:\x1b[0m {}\n", suggestion));
        }

        output
    }
    fn error_code(&self) -> &str {
        match self.error_type {
            ErrorType::SyntaxError => "E001",
            ErrorType::SemanticError => "E002",
            ErrorType::TypeError => "E003",
            ErrorType::UndefinedVariable => "E004",
            ErrorType::InvalidAssignment => "E005",
            ErrorType::MissingToken => "E006",
            ErrorType::UnexpectedToken => "E007",
            ErrorType::InvalidOperation => "E008",
        }
    }
}
