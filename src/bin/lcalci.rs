extern crate lcalc;
extern crate rustyline;

use lcalc::{
    grammar::Default,
    runtime::{Error, Repl},
};
use rustyline::error::ReadlineError;
use std::{collections::HashMap, process::exit};

fn main() {
    let mut repl = Repl::new(Default, HashMap::with_capacity(16));
    loop {
        let err = match repl.round() {
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
                eprintln!("{}", err)
            },
            Error::NoReturn => (),
        }
    }
}
