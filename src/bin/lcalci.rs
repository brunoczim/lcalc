extern crate lcalc;
extern crate rustyline;

use lcalc::{
    grammar::Default,
    repl::{help, Error, Repl},
};
use rustyline::error::ReadlineError;
use std::process::exit;

fn syntax_help() -> &'static str {
    concat!(
        "    Syntax for defining lambda:        \\identifer.expression",
        " or λidentifier.expression\n",
        "    Syntax for defining application:   expression expression\n",
        "    Valid expressions:                 identifer or application",
        " or lambda\n",
        "    Syntax for desambiguation:         (expression)\n",
        "    Valid identifiers:                 any character but .\\λ()",
        " or whitespace characters (which are ignored by the lexer)"
    )
}

fn main() {
    let mut repl = Repl::new(Default, Vec::with_capacity(16));
    loop {
        let err = match repl.round("> ") {
            Ok(expr) => {
                println!("{}", expr);
                continue;
            },
            Err(e) => e,
        };
        match err {
            Error::RlError(ref e) => match e {
                ReadlineError::Interrupted => (),
                ReadlineError::Eof => break,
                _ => {
                    eprintln!("{}", err);
                    exit(-1);
                },
            },
            Error::ParseError(_) | Error::BadCommand(_) => {
                eprintln!("{}", err);
                eprintln!("Type :? or :help for help");
            },
            Error::HelpRequested => {
                eprintln!("{}", help());
                eprintln!("{}", syntax_help());
            },
            Error::NoReturn => (),
        }
    }
}
