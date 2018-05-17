use grammar::Grammar;
use std::{fmt, iter::Peekable, rc::Rc, str::CharIndices};

pub use eval::{
    Expr::{self, *},
    Symbol::*,
};

pub type Result<'a, T> = ::std::result::Result<T, Error<'a>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Error<'a> {
    PrematureEof,
    BadChar(char),
    BadToken(Token<'a>),
}

impl<'a> From<Option<Token<'a>>> for Error<'a> {
    fn from(tok: Option<Token<'a>>) -> Self {
        tok.map_or(Error::PrematureEof, |tok| Error::BadToken(tok))
    }
}

impl<'a> fmt::Display for Error<'a> {
    fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::PrematureEof => write!(fmtr, "unexpected end of input"),
            Error::BadChar(ch) => write!(fmtr, "bad char `{}`", ch),
            Error::BadToken(tok) => write!(fmtr, "bad token {}", tok),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token<'a> {
    pub contents: &'a str,
    pub kind: TokenKind,
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
        write!(fmtr, "`{}` ({})", self.contents, self.kind)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Lambda,
    Ident,
    BodyDel,
    OpenParen,
    CloseParen,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenKind::Lambda => write!(fmtr, "lambda"),
            TokenKind::Ident => write!(fmtr, "identifier"),
            TokenKind::BodyDel => write!(fmtr, "body delimiter"),
            TokenKind::OpenParen => write!(fmtr, "opening parenthesis"),
            TokenKind::CloseParen => write!(fmtr, "closing parenthesis"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lexer<'a, 'b, G>
where
    G: Grammar + 'b,
{
    src: &'a str,
    iter: Peekable<CharIndices<'a>>,
    grammar: &'b G,
}

impl<'a, 'b, G> Lexer<'a, 'b, G>
where
    G: Grammar + 'b,
{
    pub fn new(src: &'a str, grammar: &'b G) -> Self {
        Self {
            src,
            iter: src.char_indices().peekable(),
            grammar,
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<'a, Token<'a>> {
        match self.next() {
            Some(res) => {
                let tok = res?;
                if tok.kind != kind {
                    Err(Error::BadToken(tok))
                } else {
                    Ok(tok)
                }
            },
            _ => Err(Error::PrematureEof),
        }
    }

    pub fn parse(&mut self) -> Result<'a, Expr> {
        let mut parser = Parser {
            lexer: self,
            frames: Vec::with_capacity(16),
            level: 0,
            last_expr: None,
            last_end: None,
        };
        parser.frames.push(Frame::Empty);
        parser.run()?;
        debug_assert_eq!(parser.level, 0);
        parser.last_expr.ok_or(Error::PrematureEof)
    }
}

impl<'a, 'b, G> Iterator for Lexer<'a, 'b, G>
where
    G: Grammar + 'b,
{
    type Item = Result<'a, Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.grammar.is_whitespace(self.iter.peek()?.1) {
            self.iter.next();
        }
        let mut step = 0;
        let &(start, mut ch) = self.iter.peek()?;
        let mut end = start;
        let preds: [(TokenKind, fn(&G, char, usize, &str) -> bool); 5] = [
            (TokenKind::Ident, Grammar::is_ident),
            (TokenKind::Lambda, Grammar::is_lambda),
            (TokenKind::BodyDel, Grammar::is_body_del),
            (TokenKind::OpenParen, Grammar::is_open_paren),
            (TokenKind::CloseParen, Grammar::is_close_paren),
        ];
        for &(kind, pred) in preds.iter() {
            while pred(self.grammar, ch, step, &self.src[start..end]) {
                self.iter.next();
                step += 1;
                match self.iter.peek() {
                    Some(tpl) => {
                        end = tpl.0;
                        ch = tpl.1;
                    },
                    _ => {
                        end = self.src.len();
                        break;
                    },
                };
            }
            if step > 0 {
                return Some(Ok(Token {
                    contents: &self.src[start..end],
                    kind,
                }));
            }
        }
        Some(Err(Error::BadChar(self.iter.peek().unwrap().1)))
    }
}

#[derive(Debug, Clone)]
enum Frame {
    Empty,
    AppArg(Expr),
    AppFun(Expr),
    Lambda(Rc<str>),
}

#[derive(Debug)]
struct Parser<'a, 'b, 'c, G>
where
    'a: 'c,
    'b: 'c,
    G: Grammar + 'b,
{
    lexer: &'c mut Lexer<'a, 'b, G>,
    level: usize,
    last_expr: Option<Expr>,
    last_end: Option<Token<'a>>,
    frames: Vec<Frame>,
}

impl<'a, 'b, 'c, G> Parser<'a, 'b, 'c, G>
where
    'a: 'c,
    'b: 'c,
    G: Grammar + 'b,
{
    fn run(&mut self) -> Result<'a, ()> {
        while let Some(frame) = self.frames.pop() {
            match frame {
                Frame::Empty => self.empty()?,
                Frame::AppArg(other) => self.app_arg(other)?,
                Frame::AppFun(other) => self.app_fun(other)?,
                Frame::Lambda(s) => self.lambda(s)?,
            }
        }
        Ok(())
    }

    fn empty(&mut self) -> Result<'a, ()> {
        let tok = match self.lexer.next() {
            Some(res) => res,
            _ => {
                return if self.level > 0 {
                    Err(Error::PrematureEof)
                } else {
                    Ok(())
                }
            },
        }?;

        match tok.kind {
            TokenKind::Lambda => {
                if self.last_expr.is_none() {
                    let tok = self.lexer.expect(TokenKind::Ident)?;
                    self.lexer.expect(TokenKind::BodyDel)?;
                    self.frames.push(Frame::Lambda(tok.contents.into()));
                    self.frames.push(Frame::Empty);
                    Ok(())
                } else {
                    Err(Error::BadToken(tok))
                }
            },

            TokenKind::OpenParen => {
                match self.last_expr.take() {
                    Some(expr) => {
                        self.frames.push(Frame::AppFun(expr));
                    },
                    _ => self.frames.push(Frame::Empty),
                }
                self.level += 1;
                self.frames.push(Frame::Empty);
                Ok(())
            },

            TokenKind::Ident => {
                let expr = {
                    let lambda = self.frames
                        .iter()
                        .rev()
                        .filter_map(|frame| match frame {
                            Frame::Lambda(s) => Some(s),
                            _ => None,
                        })
                        .find(|s| &***s == tok.contents);
                    Var(match lambda {
                        Some(rc) => Static(rc.clone()),
                        _ => Dyn(tok.contents.into()),
                    })
                };
                self.frames.push(Frame::AppArg(expr));
                Ok(())
            },

            _ => {
                if self.level > 0 && tok.kind == TokenKind::CloseParen {
                    self.last_end = Some(tok);
                    self.level -= 1;
                    Ok(())
                } else {
                    Err(Error::BadToken(tok))
                }
            },
        }
    }

    fn app_arg(&mut self, other: Expr) -> Result<'a, ()> {
        self.last_expr = Some(match self.last_expr.take() {
            Some(e) => App(Box::new(e), Box::new(other)),
            _ => other,
        });
        self.empty()
    }

    fn app_fun(&mut self, other: Expr) -> Result<'a, ()> {
        self.last_expr = Some(match self.last_expr.take() {
            Some(e) => App(Box::new(other), Box::new(e)),
            _ => other,
        });
        self.empty()
    }

    fn lambda(&mut self, arg: Rc<str>) -> Result<'a, ()> {
        match self.last_expr.take() {
            Some(e) => {
                self.last_expr = Some(Lambda(arg, Box::new(e)));
                Ok(())
            },
            _ => Err(self.last_end.into()),
        }
    }
}
