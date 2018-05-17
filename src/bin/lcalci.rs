extern crate lcalc;
extern crate rustyline;

use lcalc::{
    grammar::Default,
    repl::{help, Error, Repl, Result},
};
use rustyline::error::ReadlineError;
use std::{env::args, process::exit};

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

fn use_result(res: Result) -> bool {
    let err = match res {
        Ok(expr) => {
            println!("{}", expr);
            return true;
        },
        Err(e) => e,
    };
    match err {
        Error::RlError(ref e) => match e {
            ReadlineError::Interrupted => (),
            ReadlineError::Eof => return false,
            _ => {
                eprintln!("{}", err);
                exit(-1);
            },
        },
        Error::ParseError(_) | Error::IOError(_) | Error::BadCommand(_) => {
            eprintln!("{}", err);
            eprintln!("Type :? or :help for help");
        },
        Error::HelpRequested => {
            eprintln!("{}", help());
            eprintln!(
                "Note that you may pass files to be loaded as process \
                 arguments"
            );
            eprintln!("{}", syntax_help());
        },
        Error::NoReturn => (),
    }
    return true;
}

fn main() {
    let mut repl = Repl::new(Default, Vec::with_capacity(16));
    let mut args = args();
    args.next(); // ignore executable name
    for file in args {
        use_result(repl.load(&file));
    }
    while use_result(repl.prompt("> ")) {}
}
