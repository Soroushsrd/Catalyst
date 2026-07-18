use catalyst::{cmd_args, run_file};
use std::io::Result;

fn main() -> Result<()> {
    crate::run_file(&cmd_args::get().input_file)?;
    Ok(())
}
