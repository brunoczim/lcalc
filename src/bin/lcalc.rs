extern crate lcalc;

use lcalc::{grammar::Default, runtime::repl};
use std::{
    fmt::Debug,
    io::{stderr, stdin, stdout, BufReader},
};

fn main() -> Result<(), impl Debug> {
    repl(&Default, &mut BufReader::new(stdin()), &mut stdout(), &mut stderr())
}
