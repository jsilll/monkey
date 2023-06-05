use monkey::frontend::error::Error;
use monkey::frontend::lexer::Lexer;
use monkey::frontend::parser::Parser;

fn main() {
    let fname = "test/src/1.monkey";
    let source = std::fs::read_to_string(fname).unwrap();

    let lexer = Lexer::new(fname, &source);
    let mut parser = Parser::new(lexer);

    let program = parser.parse().unwrap_or_else(|e| {
        match e.error {
            Error::UnexpectedChar(_) => {
                eprintln!("Lexer error: {}", e);
            }

            Error::UnexpectedToken(_) | Error::UnexpectedEof | Error::InvalidInt => {
                eprintln!("Parser error: {}", e);
            }
        };
        std::process::exit(1);
    });

    println!("{:#?}", program);
}
