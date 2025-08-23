mod code_generator;
mod errors;
mod lexer;
mod macros;
mod parser;
use std::{
    fs,
    io::{ErrorKind, Result},
    str::FromStr,
};

use tracing::info;

use crate::{
    code_generator::AssemblyGenerator,
    lexer::{Scanner, Token},
    parser::Parser,
};

//WARNING: Fix all string allocations
fn main() -> Result<()> {
    let input: Vec<String> = std::env::args().collect();
    let file_name = std::path::PathBuf::from_str(&input[1]).expect("failed to create a pathbuf");
    let file_name = match file_name
        .file_name()
        .unwrap()
        .to_str()
        .expect("failed to create file name")
        .split_once('.')
    {
        Some((filename, _ext)) => filename,
        None => "output",
    };

    match input.len() {
        2 => run_file(&input[1], file_name)?,
        1 => run_prompt(file_name)?,
        _ => info!("Usage: Catalyst [script]"),
    }
    Ok(())
}

fn run_file(path: &str, file_name: &str) -> Result<()> {
    let bytes_str = fs::read_to_string(std::path::PathBuf::from_str(path).unwrap())?;
    let source_path = std::path::Path::new(path);
    let source_dir = source_path.parent().unwrap_or(std::path::Path::new("."));
    let output_path = source_dir.join(file_name);

    run(&bytes_str, output_path.to_str().unwrap())?;
    Ok(())
}

fn run_prompt(file_name: &str) -> Result<()> {
    let mut buf = String::new();
    loop {
        let n = std::io::stdin().read_line(&mut buf)?;
        if n > 0 {
            run(&buf, file_name)?;
        } else {
            break;
        }
    }
    Ok(())
}

fn compile_to_exe(assembly_file: &str, output_name: &str) -> Result<()> {
    let assemble = std::process::Command::new("as")
        .args(["-64", assembly_file, "-o", "temp.o"])
        .status()?;

    if !assemble.success() {
        println!("failed to assemble");
        return Err(ErrorKind::Other.into());
    }

    let link = std::process::Command::new("ld")
        .args(["temp.o", "-o", output_name])
        .status()?;

    if !link.success() {
        println!("failed to link");
        return Err(ErrorKind::Other.into());
    }

    // std::fs::remove_file("temp.o")?;
    println!("Compilation successful. output file {output_name}");
    Ok(())
}
fn run(source_code: &str, file_name: &str) -> Result<()> {
    let mut scanner = Scanner::new(source_code);
    let tokens: Vec<Token> = scanner.scan_tokens();
    if !scanner.get_errors().is_empty() {
        for error in scanner.get_errors() {
            eprintln!("{}", error.format_error());
        }
    }
    // println!("***TOKENS***");
    // for token in tokens.iter() {
    //     println!("Token: {token:?}");
    // }

    let mut parser = Parser::new(tokens, source_code);
    match parser.parse() {
        Ok(ast) => {
            if !parser.get_errors().is_empty() {
                for error in parser.get_errors() {
                    eprintln!("{}", error.format_error());
                }
            }

            let mut codegen = AssemblyGenerator::new();
            codegen.generate_program(&ast);
            let assembly_file_name = format!("{file_name}.s");
            if let Err(e) = codegen.compile_to_file(&assembly_file_name) {
                println!("failed to write assembly file {e}");
            }
            compile_to_exe(&assembly_file_name, file_name)?;
        }
        Err(errors) => {
            for error in errors {
                eprintln!("{}", error.format_error());
            }
            eprintln!("Compilation failed due to parse errors.");
        }
    }
    Ok(())
}
