mod code_generator;
mod errors;
mod lexer;
mod macros;
mod parser;
use inkwell::context::Context;

use crate::{
    code_generator::LLVMCodeGenerator,
    lexer::{Scanner, Token},
    parser::Parser,
};

use std::{
    fs,
    io::{ErrorKind, Result},
    path::{Path, PathBuf},
    process::Command,
    str::FromStr,
};

// TODO: use clap for cli purposes

// WARNING: Fix all string allocations
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
fn link_exe(object_file: &str, output_name: &str) -> Result<()> {
    let mut linker_cmd = if cfg!(target_os = "windows") {
        let mut cmd = Command::new("link");
        cmd.args(["/ENTRY:main", "/SUBSYSTEM:CONSOLE"]);
        cmd
    } else {
        let mut cmd = Command::new("ld");
        cmd.args(["-e", "_start"]);
        cmd
    };

    let status = linker_cmd.args([object_file, "-o", output_name]).status()?;

    if !status.success() {
        eprintln!("Linking failed");
        return Err(ErrorKind::Other.into());
    }

    // Clean up object file
    if let Err(e) = fs::remove_file(object_file) {
        eprintln!("Warning: Could not remove object file: {}", e);
    }

    println!("Compilation successful. Output: {}", output_name);
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
                return Err(ErrorKind::InvalidInput.into());
            }

            println!("\n***AST***");
            println!("{ast:#?}");
            let context = Context::create();
            let mut codegen = LLVMCodeGenerator::new(&context, file_name);

            if let Err(e) = codegen.generate_program(&ast) {
                eprintln!("Code generation failed: {e}");
                return Err(ErrorKind::Other.into());
            }

            println!("\n***LLVM IR***");
            codegen.print_ir();

            let object_file_name = format!("{file_name}.o");
            let obj_path = Path::new(&object_file_name);

            if let Err(e) = codegen.compile_to_obj(obj_path) {
                println!("failed to write assembly file {e}");
                return Err(ErrorKind::Other.into());
            }

            link_exe(&object_file_name, file_name)?;
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
