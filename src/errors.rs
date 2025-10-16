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
    RedefinedVariable,
    UndefinedVariable,
    InvalidAssignment,
    MissingToken,
    UnexpectedToken,
    InvalidOperation,
    UseBeforeDeclaration,
}

impl Default for CompilerError {
    fn default() -> Self {
        Self::new(ErrorType::InvalidOperation, 1, 1, "")
    }
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

    pub fn with_source_line(self, source_line: &str) -> Self {
        CompilerError {
            source_line: Some(source_line.to_string()),
            ..self
        }
    }

    //TODO: how can we comeup with the best suggestions?
    #[allow(dead_code)]
    pub fn with_suggestion(self, suggestion: &str) -> Self {
        CompilerError {
            suggestion: Some(suggestion.to_string()),
            ..self
        }
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
            ErrorType::RedefinedVariable => "E004",
            ErrorType::UndefinedVariable => "E005",
            ErrorType::InvalidAssignment => "E006",
            ErrorType::MissingToken => "E007",
            ErrorType::UnexpectedToken => "E008",
            ErrorType::InvalidOperation => "E009",
            ErrorType::UseBeforeDeclaration => "E010",
        }
    }
}
