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

    // FIXME: this function is too big.
    pub fn parse(&mut self) -> Result<'a, Expr> {
        #[derive(Debug, Clone)]
        enum Frame {
            Empty,
            AppArg(Expr),
            AppFun(Expr),
            Lambda(Rc<str>),
        }

        let mut frames = Vec::with_capacity(16);
        let mut level = 0;
        let mut last_expr = None;
        let mut last_end = None;

        frames.push(Frame::Empty);

        while let Some(frame) = frames.pop() {
            match frame {
                Frame::Empty => (),
                Frame::AppArg(other) => {
                    last_expr = Some(match last_expr {
                        Some(e) => App(Box::new(e), Box::new(other)),
                        _ => other,
                    });
                },
                Frame::AppFun(other) => {
                    last_expr = Some(match last_expr {
                        Some(e) => App(Box::new(other), Box::new(e)),
                        _ => other,
                    });
                },
                Frame::Lambda(s) => match last_expr {
                    Some(e) => {
                        last_expr = Some(Lambda(s, Box::new(e)));
                        continue;
                    },
                    _ => return Err(last_end.into()),
                },
            }

            let tok = match self.next() {
                Some(res) => res,
                _ => if level > 0 {
                    return Err(Error::PrematureEof);
                } else {
                    continue;
                },
            }?;

            match tok.kind {
                TokenKind::Lambda => {
                    if last_expr.is_none() {
                        let tok = match self.next() {
                            Some(res) => res,
                            _ => return Err(Error::PrematureEof),
                        }?;
                        if tok.kind != TokenKind::Ident {
                            return Err(Error::BadToken(tok));
                        }
                        let arg = tok.contents.into();
                        let tok = match self.next() {
                            Some(res) => res,
                            _ => return Err(Error::PrematureEof),
                        }?;
                        if tok.kind != TokenKind::BodyDel {
                            return Err(Error::BadToken(tok));
                        }
                        frames.push(Frame::Lambda(arg));
                        frames.push(Frame::Empty);
                    } else {
                        return Err(Error::BadToken(tok));
                    }
                },

                TokenKind::OpenParen => {
                    match last_expr {
                        Some(expr) => {
                            frames.push(Frame::AppFun(expr));
                        },
                        _ => frames.push(Frame::Empty),
                    }
                    level += 1;
                    last_expr = None;
                    frames.push(Frame::Empty);
                },

                TokenKind::Ident => {
                    let expr = {
                        let lambda = frames
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
                    frames.push(Frame::AppArg(expr));
                },

                _ => {
                    if level > 0 && tok.kind == TokenKind::CloseParen {
                        last_end = Some(tok);
                        level -= 1;
                    } else {
                        return Err(Error::BadToken(tok));
                    }
                },
            }
        }

        debug_assert_eq!(level, 0);
        last_expr.ok_or(Error::PrematureEof)
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
