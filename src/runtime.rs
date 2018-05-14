use eval::Expr;
use grammar::Grammar;
use parser::{Error as ParseError, Lexer, Parser};
use rustyline::{error::ReadlineError, Editor};
use std::{collections::HashMap, fmt};

pub type Result = ::std::result::Result<Expr, Error>;

#[derive(Debug)]
pub enum Error {
    NoReturn,
    RlError(ReadlineError),
    ParseError(String),
    BadCommand(String),
}

impl fmt::Display for Error {
    fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::NoReturn => write!(fmtr, "empty"),
            Error::RlError(ref e) => write!(fmtr, "readline error: {}", e),
            Error::ParseError(ref e) => write!(fmtr, "parse error: {}", e),
            Error::BadCommand(ref e) => write!(fmtr, "bad command: {}", e),
        }
    }
}

impl<'a> From<ParseError<'a>> for Error {
    fn from(e: ParseError) -> Self { Error::ParseError(e.to_string()) }
}

impl From<ReadlineError> for Error {
    fn from(e: ReadlineError) -> Self { Error::RlError(e) }
}

#[derive(Clone, Debug, Copy)]
enum Cmd {
    Eval,
    Print,
    Exit,
    Quit,
}

impl Cmd {
    fn name(self) -> &'static str {
        match self {
            Cmd::Eval => "eval",
            Cmd::Print => "print",
            Cmd::Exit => "exit",
            Cmd::Quit => "quit",
        }
    }

    fn key(self) -> (&'static str, Self) { (self.name(), self) }
}

#[derive(Debug)]
pub struct Repl<G>
where
    G: Grammar,
{
    grammar: G,
    env: HashMap<String, Expr>,
    editor: Editor<()>,
}

impl<G> Repl<G>
where
    G: Grammar,
{
    pub fn new(grammar: G, env: HashMap<String, Expr>) -> Self {
        Self {
            grammar,
            env,
            editor: Editor::new(),
        }
    }

    pub fn eval(&mut self, src: &str) -> Result {
        Ok(Parser::new(Lexer::new(src, &self.grammar)).parse()?.eval())
    }

    pub fn dispatch_cmd(&mut self, line: &str) -> Result {
        let mut iter = line.char_indices().peekable();
        let mut cmd = String::new();
        loop {
            match iter.peek() {
                Some((_, ch)) => if ch.is_alphabetic() {
                    cmd.push(ch.to_ascii_lowercase());
                } else {
                    break;
                },
                _ => break,
            }
            iter.next();
        }
        let keys = [
            Cmd::Eval.key(),
            Cmd::Print.key(),
            Cmd::Exit.key(),
            Cmd::Quit.key(),
        ];
        let mut idx = None;
        'outer: for &(name, val) in keys.iter() {
            let mut eq = 0;
            let mut it1 = name.chars();
            let mut it2 = cmd.chars();
            loop {
                match it1.next() {
                    Some(ch1) => match it2.next() {
                        Some(ch2) => if ch1 == ch2 {
                            eq += 1;
                        } else {
                            continue 'outer;
                        },
                        _ => break,
                    },
                    _ => if it2.next().is_none() {
                        break;
                    } else {
                        continue 'outer;
                    },
                }
            }
            match idx {
                Some((_, i)) => if eq > i {
                    idx = Some((val, eq));
                },
                _ => idx = Some((val, eq)),
            }
        }
        match idx {
            Some((cmd, _)) => match cmd {
                Cmd::Eval | Cmd::Print => {
                    self.eval(&line[iter.peek().unwrap().0..])
                },
                Cmd::Exit | Cmd::Quit => {
                    Err(Error::RlError(ReadlineError::Eof))
                },
            },
            _ => Err(Error::BadCommand(cmd)),
        }
    }

    pub fn round(&mut self) -> Result {
        let line = self.editor.readline("> ")?;
        self.editor.add_history_entry(&line);
        let trimmed = line.trim();
        match trimmed.chars().next() {
            Some(':') => self.dispatch_cmd(&trimmed[1..]),
            Some(_) => self.eval(trimmed),
            None => Err(Error::NoReturn),
        }
    }

    pub fn grammar(&self) -> &Grammar { &self.grammar }

    pub fn grammar_mut(&mut self) -> &mut Grammar { &mut self.grammar }

    pub fn env(&self) -> &HashMap<String, Expr> { &self.env }

    pub fn env_mut(&mut self) -> &mut HashMap<String, Expr> { &mut self.env }

    pub fn take(self) -> (G, HashMap<String, Expr>) {
        let Self {
            grammar,
            env,
            ..
        } = self;
        (grammar, env)
    }
}
