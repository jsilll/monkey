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
        match Parser::new(lexer).parse() {
            Err(e) => eprintln!("{}", e),
            Ok(program) => match program.statements.len() {
                0 => (),
                _ => println!("{:#?}", program),
            },
        }
    }
}
