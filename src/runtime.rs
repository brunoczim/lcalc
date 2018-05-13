use grammar::Grammar;
use parser::{Lexer, Parser};
use rustyline::{error::ReadlineError, Editor, Result};

pub fn repl<G>(grammar: &G) -> Result<()>
where
    G: Grammar,
{
    let mut editor = Editor::<()>::new();
    loop {
        let line = match editor.readline("> ") {
            Ok(s) => s,
            Err(ReadlineError::Eof) => {
                break Ok(());
            },
            Err(e) => break Err(e),
        };
        let ast = match Parser::new(Lexer::new(&line, grammar)).parse() {
            Ok(expr) => expr,
            Err(e) => {
                eprintln!("Error: {}", e);
                continue;
            },
        };
        println!("{}", ast.eval());
    }
}
