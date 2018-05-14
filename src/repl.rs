use eval::Expr;
use grammar::Grammar;
use parser::{Error as ParseError, Lexer, Parser, TokenKind};
use rustyline::{error::ReadlineError, Editor};
use std::fmt;

pub type Result = ::std::result::Result<Expr, Error>;

#[derive(Debug)]
pub enum Error {
    NoReturn,
    RlError(ReadlineError),
    ParseError(String),
    BadCommand(String),
    HelpRequested,
}

impl fmt::Display for Error {
    fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::NoReturn => write!(fmtr, "empty"),
            Error::RlError(ref e) => write!(fmtr, "readline error: {}", e),
            Error::ParseError(ref e) => write!(fmtr, "parse error: {}", e),
            Error::BadCommand(ref e) => write!(fmtr, "bad command: {}", e),
            Error::HelpRequested => write!(fmtr, "help requested"),
        }
    }
}

impl<'a> From<ParseError<'a>> for Error {
    fn from(e: ParseError) -> Self { Error::ParseError(e.to_string()) }
}

impl From<ReadlineError> for Error {
    fn from(e: ReadlineError) -> Self { Error::RlError(e) }
}

pub fn help() -> &'static str {
    concat!(
        "This is a lambda calculator REPL.",
        "REPL commands begin with :\n",
        ":help, :?                         shows this message\n",
        ":eval expr, :print expr, expr     evaluates expr and prints\n",
        ":def IDENT expr                   defines IDENT as expr\n",
        ":undef IDENT                      if defined, IDENT is removed\n",
        ":exit, :quit, EOF                 exits the REPL"
    )
}

#[derive(Clone, Debug, Copy)]
enum Cmd {
    Eval,
    Print,
    Def,
    Undef,
    Exit,
    Quit,
    Help,
    Question,
}

impl Cmd {
    fn name(self) -> &'static str {
        match self {
            Cmd::Eval => "eval",
            Cmd::Print => "print",
            Cmd::Def => "def",
            Cmd::Undef => "undef",
            Cmd::Exit => "exit",
            Cmd::Quit => "quit",
            Cmd::Help => "help",
            Cmd::Question => "?",
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
    env: Vec<(String, Expr)>,
    editor: Editor<()>,
}

impl<G> Repl<G>
where
    G: Grammar,
{
    pub fn new(grammar: G, env: Vec<(String, Expr)>) -> Self {
        Self {
            grammar,
            env,
            editor: Editor::new(),
        }
    }

    pub fn eval(&self, src: &str) -> Result {
        let mut expr = Parser::new(Lexer::new(src, &self.grammar)).parse()?;
        for (name, val) in self.env.iter().rev() {
            expr.replace(name, val)
        }
        Ok(expr.eval())
    }

    pub fn define(&mut self, name: String, val: Expr) -> Result {
        let prev = self.env
            .iter()
            .enumerate()
            .find(|(_, elem)| elem.0 == name)
            .map(|(i, _)| i);
        match prev {
            Some(i) => {
                self.env.remove(i);
            },
            _ => (),
        }
        self.env.push((name, val));
        Err(Error::NoReturn)
    }

    pub fn undef(&mut self, name: String) -> Result {
        let prev = self.env
            .iter()
            .enumerate()
            .find(|(_, elem)| elem.0 == name)
            .map(|(i, _)| i);
        match prev {
            Some(i) => {
                self.env.remove(i);
            },
            _ => (),
        }
        Err(Error::NoReturn)
    }

    pub fn dispatch_cmd(&mut self, line: &str) -> Result {
        let mut iter = line.char_indices().peekable();
        let mut cmd = String::new();
        loop {
            match iter.peek() {
                Some(&(_, ch)) => if ch.is_alphabetic() || ch == '?' {
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
            Cmd::Def.key(),
            Cmd::Undef.key(),
            Cmd::Exit.key(),
            Cmd::Quit.key(),
            Cmd::Help.key(),
            Cmd::Question.key(),
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
                        _ => if eq == 0 {
                            continue 'outer;
                        } else {
                            break;
                        },
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

                Cmd::Def => {
                    let (name, val) = {
                        let mut lexer = Lexer::new(
                            &line[iter.peek().unwrap().0..],
                            &self.grammar,
                        );
                        let tok = match lexer.next() {
                            Some(tok) => tok?,
                            _ => {
                                return Err(Error::from(
                                    ParseError::PrematureEof,
                                ))
                            },
                        };
                        if tok.kind != TokenKind::Ident {
                            return Err(Error::from(ParseError::BadToken(tok)));
                        }
                        (tok.contents.into(), Parser::new(lexer).parse()?)
                    };
                    self.define(name, val)
                },

                Cmd::Undef => {
                    let name = {
                        let mut lexer = Lexer::new(
                            &line[iter.peek().unwrap().0..],
                            &self.grammar,
                        );
                        let tok = match lexer.next() {
                            Some(tok) => tok?,
                            _ => {
                                return Err(Error::from(
                                    ParseError::PrematureEof,
                                ))
                            },
                        };
                        if tok.kind != TokenKind::Ident {
                            return Err(Error::from(ParseError::BadToken(tok)));
                        }
                        if let Some(res) = lexer.next() {
                            return Err(Error::from(ParseError::BadToken(res?)));
                        }
                        tok.contents.into()
                    };
                    self.undef(name)
                },

                Cmd::Help | Cmd::Question => Err(Error::HelpRequested),
            },
            _ => Err(Error::BadCommand(cmd)),
        }
    }

    pub fn round(&mut self, prompt: &str) -> Result {
        let line = self.editor.readline(prompt)?;
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

    pub fn env(&self) -> &Vec<(String, Expr)> { &self.env }

    pub fn env_mut(&mut self) -> &mut Vec<(String, Expr)> { &mut self.env }

    pub fn take(self) -> (G, Vec<(String, Expr)>) {
        let Self {
            grammar,
            env,
            ..
        } = self;
        (grammar, env)
    }
}
