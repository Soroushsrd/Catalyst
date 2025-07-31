mod lexer;
mod parser;
use std::{fs, io::Result, str::FromStr};

use tracing::info;

use crate::lexer::{Scanner, Token};

fn main() -> Result<()> {
    let input: Vec<String> = std::env::args().collect();
    match input.len() {
        2 => run_file(&input[1])?,
        1 => run_prompt()?,
        _ => info!("Usage: rlox [script]"),
    }
    Ok(())
}

fn run_file(path: &str) -> Result<()> {
    let bytes_str = fs::read_to_string(std::path::PathBuf::from_str(path).unwrap())?;
    run(&bytes_str)?;
    Ok(())
}

fn run_prompt() -> Result<()> {
    let mut buf = String::new();
    loop {
        let n = std::io::stdin().read_line(&mut buf)?;
        if n > 0 {
            run(&buf)?;
        } else {
            break;
        }
    }
    Ok(())
}

fn run(source_code: &str) -> Result<()> {
    let mut scanner = Scanner::new(source_code);
    let tokens: Vec<Token> = scanner.scan_tokens();
    for token in tokens {
        println!("Token: {:?}", token);
    }
    Ok(())
}
