use std::io::{self, BufRead, Write};

use grammar::Grammar;
use parser::{Lexer, Parser};

pub fn repl<G, I, O, E>(
    grammar: &G,
    input: &mut I,
    output: &mut O,
    err: &mut E,
) -> io::Result<()>
where
    G: Grammar,
    I: BufRead,
    O: Write,
    E: Write,
{
    loop {
        output.write(b"> ")?;
        output.flush()?;
        err.flush()?;
        let mut line = String::new();
        if input.read_line(&mut line)? == 0 {
            break Ok(());
        }
        let ast = match Parser::new(Lexer::new(&line, grammar)).parse() {
            Ok(expr) => expr,
            Err(e) => {
                err.write(format!("Error: {}\n", e).as_bytes())?;
                continue;
            },
        };
        output.write(format!("{}\n", ast.eval()).as_bytes())?;
    }
}
