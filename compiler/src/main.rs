use clap::Parser as ClapParser;

use monkey::frontend::lexer::Lexer;
use monkey::frontend::parser::Parser;

#[derive(Debug, ClapParser)]
#[clap(author, version, about)]
struct CompilerArgs {
    /// The input file to compile
    pub fname: String,
}

fn main() {
    // Reading the source file to memory
    let args = CompilerArgs::parse();
    let source = std::fs::read_to_string(&args.fname).unwrap_or_else(|e| {
        eprintln!("Error reading file: {}", e);
        std::process::exit(1);
    });

    // Parsing the source file
    let parser = Parser::new(Lexer::new(&args.fname, &source));
    let ast = parser.parse().unwrap_or_else(|e| match e.error {
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

    // Printing the AST
    println!("{:#?}", ast);
}
