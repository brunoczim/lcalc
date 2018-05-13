extern crate lcalc;

use lcalc::{grammar::Default, runtime::repl};
use std::fmt::Debug;

fn main() -> Result<(), impl Debug> { repl(&Default) }
