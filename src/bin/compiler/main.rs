use monkey::common::parsed_ast::Program;

use monkey::frontend::error::LocatedError;
use monkey::frontend::lexer::Lexer;
use monkey::frontend::parser::Parser;

fn parse(fname: &str, source: &str) -> Result<Program, LocatedError> {
    let lexer = Lexer::new(fname, source);
    let mut parser = Parser::new(lexer);
    parser.parse()
}

fn main() {
    let fname = "test/src/1.monkey";
    let source = std::fs::read_to_string(fname).unwrap();

    let program = parse(fname, &source).unwrap_or_else(|e| {
        match e.error {
            monkey::frontend::error::Error::UnexpectedChar(_) => {
                eprintln!("Lexer error: {}", e);
            }

            monkey::frontend::error::Error::UnexpectedToken(_)
            | monkey::frontend::error::Error::UnexpectedEof
            | monkey::frontend::error::Error::InvalidInt => {
                eprintln!("Parser error: {}", e);
            }
        };

        std::process::exit(1);
    });

    println!("{:#?}", program);
}
