use std::io::Write;

use monkey::frontend::lexer::Lexer;
use monkey::frontend::parser::Parser;

fn main() {
    println!("Monkey REPL");

    let fname = "stdin";
    loop {
        print!(">> ");
        std::io::stdout().flush().unwrap_or_else(|e| {
            eprintln!("Error flushing stdout: {}", e);
            std::process::exit(1);
        });

        let mut source = String::new();
        std::io::stdin().read_line(&mut source).unwrap_or_else(|e| {
            eprintln!("Error reading from stdin: {}", e);
            std::process::exit(1);
        });

        let lexer = Lexer::new(fname, &source);
        let mut parser = Parser::new(lexer);

        match parser.parse() {
            Err(e) => eprintln!("{}", e),
            Ok(ast) => println!("{:#?}", ast),
        }
    }
}
