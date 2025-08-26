mod code_generator;
mod errors;
mod lexer;
mod macros;
mod parser;
use std::{
    fs,
    io::{ErrorKind, Result},
    path::{Path, PathBuf},
    str::FromStr,
};

use crate::{
    code_generator::AssemblyGenerator,
    lexer::{Scanner, Token},
    parser::Parser,
};

//TODO: use clap for cli purposes

//WARNING: Fix all string allocations
fn main() -> Result<()> {
    let input: Vec<String> = std::env::args().collect();

    match input.len() {
        2 => run_file(&input[1])?,
        _ => println!("Usage: Catalyst [filepath]"),
    }
    Ok(())
}

/// based on the input, extracts file name and then reads the file to a string
/// then passes the file content to be parsed
fn run_file(path: &str) -> Result<()> {
    //TODO: reading the file could be optimized for larger files
    let bytes_str = fs::read_to_string(PathBuf::from_str(path).unwrap())?;

    let file_name = PathBuf::from_str(path).expect("failed to create a pathbuf");
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

    let source_path = Path::new(path).parent().unwrap_or(Path::new("."));

    let output_path = source_path.join(file_name);

    run(&bytes_str, output_path.to_str().unwrap())?;
    Ok(())
}

/// used as the last step to compilation
/// takes in the path to assembly file generated and an output filename
/// using gnu ld and as, assembles and links the final output
fn compile_to_exe(assembly_file: &str, output_name: &str) -> Result<()> {
    let assemble = std::process::Command::new("as")
        .args(["-64", assembly_file, "-o", "temp.o"])
        .status()?;

    if !assemble.success() {
        println!("failed to assemble");
        //TODO: use custom error handling
        return Err(ErrorKind::Other.into());
    }

    let link = std::process::Command::new("ld")
        .args(["temp.o", "-o", output_name])
        .status()?;

    if !link.success() {
        println!("failed to link");
        return Err(ErrorKind::Other.into());
    }

    fs::remove_file("temp.o")?;
    println!("Compilation successful. output file {output_name}");
    Ok(())
}
/// takes in a file name and source code str
/// first parses the source code into its tokens and using the parser module
/// creates an abstract syntax tree. Then, the code generator will generate
/// assembly code based on the AST and save it in the file name
fn run(source_code: &str, file_name: &str) -> Result<()> {
    let mut scanner = Scanner::new(source_code);
    let tokens: Vec<Token> = scanner.scan_tokens();

    if !scanner.get_errors().is_empty() {
        for error in scanner.get_errors() {
            eprintln!("{}", error.format_error());
        }
    }

    println!("***TOKENS***");
    for token in tokens.iter() {
        println!("Token: {token:?}");
    }

    let mut parser = Parser::new(tokens, source_code);
    match parser.parse() {
        Ok(ast) => {
            if !parser.get_errors().is_empty() {
                for error in parser.get_errors() {
                    eprintln!("{}", error.format_error());
                }
            }

            println!("\n***AST***");
            println!("{ast:#?}");
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
