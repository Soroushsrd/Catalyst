use std::sync::OnceLock;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct CmdArgs {
    /// Input file to process
    #[arg(index = 1, value_name = "SOURCE_FILE")]
    pub input_file: String,

    /// Output file name
    #[arg(short, long, value_name = "OUTPUT")]
    pub output: Option<String>,
}

/// args singleton
static ARGS: OnceLock<CmdArgs> = OnceLock::new();

/// global read only access to args
pub fn get() -> &'static CmdArgs {
    ARGS.get_or_init(|| CmdArgs::parse())
}

/// just in case you don't need args but you want to make sure they are valid and return if not
pub fn init() {
    ARGS.set(CmdArgs::parse())
        .expect("you are calling cmd_args::init() twice");
}
