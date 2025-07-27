use clap::{Args, Parser, Subcommand};

/// Exu interpreter CLI application struct
#[derive(Parser)]
#[command(version, about, long_about = None)]
#[command(propagate_version = true)]
pub struct Cli {
    /// The path to your .exu source file to be executed
    #[arg(value_parser = verify_file_signature)]
    pub path: Option<String>,

    #[command(subcommand)]
    pub command: Option<Commands>,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Set the interpreter to debug mode
    Debug(DebugArgs),
}

#[derive(Args, Clone)]
pub struct DebugArgs {
    /// The path to your .exu source file to be executed
    #[arg(value_parser = verify_file_signature)]
    pub path: Option<String>,

    /// Display the debugging information of the interpreter
    #[arg(short, long)]
    pub debug_config: bool,

    /// Generate a file containing the tokenized source code
    #[arg(long)]
    pub gen_tokens: bool,

    /// Generate a file containing the equivalent AST of the source code
    #[arg(long)]
    pub gen_ast: bool,

    /// Generate a file containing the compiled bytecode instructions
    #[arg(long)]
    pub gen_instr: bool,

    /// Display the register and heap states of each executed instruction during runtime
    #[arg(long)]
    pub show_vm_states: bool,

    /// Stop the interpreter after the parsing stage
    #[arg(short = 'p', long)]
    pub halt_at_parse: bool,

    /// Stop the interpreter after the typecheck stage
    #[arg(short = 't', long)]
    pub halt_at_typecheck: bool,

    /// Stop the interpreter after the compile stage
    #[arg(short = 'c', long)]
    pub halt_at_compile: bool,
}

/// Verifies if the file path passed has the .exu file extension
fn verify_file_signature(path: &str) -> Result<String, String> {
    if path.ends_with(".exu") {
        Ok(path.to_string())
    } else {
        Err(format!(
            "Invalid file passed. Your source file must end with the \".exu\" file extension."
        ))
    }
}
