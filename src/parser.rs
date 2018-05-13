use eval::Expr::{self, *};
use grammar::Grammar;
use std::{fmt, iter::Peekable, str::CharIndices};

pub type Result<'a, T> = ::std::result::Result<T, Error<'a>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Error<'a> {
    BadChar(char),
    BadToken(Token<'a>),
    PrematureEof(),
}

impl<'a> fmt::Display for Error<'a> {
    fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::BadChar(ch) => write!(fmtr, "bad char `{}`", ch),
            Error::BadToken(tok) => write!(fmtr, "bad token {}", tok),
            Error::PrematureEof() => write!(fmtr, "unexpected end of input"),
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
    G: Grammar,
{
    pub fn new(src: &'a str, grammar: &'b G) -> Self {
        Self {
            src,
            iter: src.char_indices().peekable(),
            grammar,
        }
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
                step += 1;
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

pub struct Parser<'a, 'b, G>
where
    G: Grammar + 'b,
{
    lexer: Lexer<'a, 'b, G>,
}

impl<'a, 'b, G> Parser<'a, 'b, G>
where
    G: Grammar + 'b,
{
    pub fn new(lexer: Lexer<'a, 'b, G>) -> Self {
        Self {
            lexer,
        }
    }

    pub fn take_lexer(self) -> Lexer<'a, 'b, G> { self.lexer }

    pub fn parse(&mut self) -> Result<'a, Expr> { self.parse_expr(None) }

    fn parse_expr(&mut self, end: Option<TokenKind>) -> Result<'a, Expr> {
        let mut expr = None;
        loop {
            let tok = match self.lexer.next() {
                Some(res) => res,
                _ => break expr.ok_or(Error::PrematureEof()),
            }?;
            let other_expr = match tok.kind {
                TokenKind::Lambda => {
                    break if expr.is_none() {
                        self.parse_lambda(end)
                    } else {
                        Err(Error::BadToken(tok))
                    }
                },
                TokenKind::OpenParen => {
                    self.parse_expr(Some(TokenKind::CloseParen))?
                },
                TokenKind::Ident => Var(tok.contents.into()),
                _ => {
                    break if Some(tok.kind) == end {
                        expr.ok_or(Error::BadToken(tok))
                    } else {
                        Err(Error::BadToken(tok))
                    }
                },
            };
            expr = Some(match expr {
                Some(e) => App(Box::new(e), Box::new(other_expr)),
                _ => other_expr,
            });
        }
    }

    fn parse_lambda(&mut self, end: Option<TokenKind>) -> Result<'a, Expr> {
        let tok = match self.lexer.next() {
            Some(res) => res,
            _ => return Err(Error::PrematureEof()),
        }?;
        if tok.kind != TokenKind::Ident {
            return Err(Error::BadToken(tok));
        }
        let arg = tok.contents.into();
        let tok = match self.lexer.next() {
            Some(res) => res,
            _ => return Err(Error::PrematureEof()),
        }?;
        if tok.kind != TokenKind::BodyDel {
            return Err(Error::BadToken(tok));
        }
        let body = self.parse_expr(end)?;
        Ok(Lambda(arg, Box::new(body)))
    }
}
