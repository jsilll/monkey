use clap::Parser as ClapParser;

use monkey::frontend::lexer::Lexer;
use monkey::frontend::parser::Parser;
use monkey::frontend::typing::TypeChecker;

#[derive(Debug, ClapParser)]
#[clap(author, version, about)]
struct CompilerArgs {
    /// The input file to compile
    pub fname: String,
}

fn main() {
    // Parsing the command line arguments
    let args = CompilerArgs::parse();

    // Reading the source file to memory
    let source = std::fs::read_to_string(&args.fname).unwrap_or_else(|e| {
        eprintln!("Error reading file: {}", e);
        std::process::exit(1);
    });

    // Parsing the source file
    let parser = Parser::new(Lexer::new(&args.fname, &source));
    let file = parser.parse().unwrap_or_else(|e| match e.error {
        monkey::frontend::Error::UnexpectedChar(_) => {
            eprintln!("Lexer error: {}", e);
            std::process::exit(1);
        }
        monkey::frontend::Error::UnexpectedToken(_)
        | monkey::frontend::Error::UnexpectedEof
        | monkey::frontend::Error::InvalidInt => {
            eprintln!("Parser error: {}", e);
            std::process::exit(1);
        }
    });

    // Explicitly dropping the source file to free memory
    drop(source);

    // Type checking the AST
    let type_checker = TypeChecker::new(file);
    let typed_file = type_checker.check().unwrap_or_else(|e| {
        eprintln!("Type error: {}", e);
        std::process::exit(1);
    });

    // Printing the AST
    println!("{:#?}", typed_file);
}
