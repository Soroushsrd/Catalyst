// TODO: need something to get the files inside tests/stage_n/valid/*.c files and read their contents
// TODO: when reading the contents, an expected outcome should be parsed which would be used to compare against the program output

use std::{error::Error, path::Path, process::Command};

fn expected_from_source(c_file: &Path) -> i32 {
    let source = std::fs::read_to_string(c_file).expect("file doesnt exist");
    source
        .lines()
        .find_map(|l| l.strip_prefix("// expected "))
        .expect("fixture missing: '// expected value'")
        .trim()
        .parse()
        .expect("expected an int value")
}

fn run_case(c_file: &Path, expected: i32) -> Result<(), Box<dyn Error>> {
    let output_name = match c_file
        .file_name()
        .unwrap()
        .to_str()
        .unwrap()
        .split_once('.')
    {
        Some((file_name, _ext)) => file_name,
        None => "output",
    };
    let output_path = std::env::temp_dir().join(output_name);

    let compile = Command::new(env!("CARGO_BIN_EXE_catalyst"))
        .arg(c_file)
        .arg("-o")
        .arg(&output_path)
        .status()?;

    if !compile.success() {
        return Err(format!("compiler failed on {}", c_file.display()).into());
    }

    let run = Command::new(&output_path).status()?;
    let _ = std::fs::remove_file(&output_path);

    let actual = run.code().ok_or("process killed by signal, no exit code")?;

    if actual != expected {
        return Err(format!("expected {expected}, got {actual}").into());
    }
    Ok(())
}

fn run_files_in_dir(dir: &str) -> Result<(), Box<dyn Error>> {
    for entry in std::fs::read_dir(dir)? {
        let path = entry?.path();
        if path.extension().and_then(|e| e.to_str()) != Some("c") {
            continue;
        }
        let expected_output = expected_from_source(&path);
        run_case(&path, expected_output).unwrap();
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::run_files_in_dir;

    #[test]
    fn test_stage_one_valid() {
        run_files_in_dir("tests/stage_1/valid").unwrap();
    }

    #[test]
    #[should_panic]
    fn test_stage_one_invalid() {
        run_files_in_dir("tests/stage_1/invalid").unwrap();
    }
}
