use std::path::Path;

use cpp::{
    errors::PreprocessorErr,
    lexer::{Lexer, PPToken},
    reader::Reader,
};

fn run_lexer(path: &Path) -> Result<Vec<PPToken>, Vec<PreprocessorErr>> {
    let source = std::fs::read_to_string(path).expect("failed to read the file");
    let reader = Reader::new(&source);
    let mut lexer = Lexer::new(reader);
    let tokens = lexer.lex();

    if lexer.errors().is_empty() {
        Ok(tokens)
    } else {
        Err(lexer.errors().to_vec())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn integration_lex_torture_header() {
        let path = Path::new("tests/torture.h");
        let result = run_lexer(&path);
        if result.is_ok() {
            println!("Tokens: {:#?}", result.unwrap());
        } else {
            let errs = result.err().unwrap();
            for err in errs {
                println!("err: {err:#?}");
            }
        }
    }

    #[test]
    fn integration_lex_torture_src() {
        let path = Path::new("tests/torture.c");
        let result = run_lexer(&path);
        if result.is_ok() {
            println!("Tokens: {:#?}", result.unwrap());
        } else {
            let errs = result.err().unwrap();
            for err in errs {
                println!("err: {err:#?}");
            }
        }
    }
}
