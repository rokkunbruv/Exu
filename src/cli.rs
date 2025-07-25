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
    /// Sets the interpreter to debug mode
    Debug(DebugArgs),
}

#[derive(Args, Clone)]
pub struct DebugArgs {
    /// The path to your .exu source file to be executed
    #[arg(value_parser = verify_file_signature)]
    pub path: Option<String>,

    /// Displays the debugging information of the interpreter
    #[arg(short, long)]
    pub debug_info: bool,

    /// Generates a file containing the tokenized source code
    #[arg(long)]
    pub gen_tokens: bool,

    /// Generates a file containing the equivalent AST of the source code
    #[arg(long)]
    pub gen_ast: bool,

    /// Generates a file containing the compiled bytecode instructions
    #[arg(long)]
    pub gen_instr: bool,

    /// Displays the register and heap states of each executed instruction during runtime
    #[arg(long)]
    pub show_vm_states: bool,
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

// Toggle functions to enable/disable features depending on passed flags

pub fn display_debug_info_toggle(cli: &Cli) -> bool {
    if let Some(Commands::Debug(args)) = &cli.command {
        args.debug_info
    } else {
        false
    }
}

pub fn display_vm_states_toggle(cli: &Cli) -> bool {
    if let Some(Commands::Debug(args)) = &cli.command {
        args.show_vm_states
    } else {
        false
    }
}
