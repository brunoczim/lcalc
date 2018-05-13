use std::fmt;

pub use self::Expr::*;

#[derive(Debug, Clone)]
pub enum Expr {
    Var(String),
    App(Box<Expr>, Box<Expr>),
    Lambda(String, Box<Expr>),
}

impl Expr {
    pub fn eval(self) -> Self {
        match self {
            Var(name) => Var(name),
            App(fun, arg) => fun.eval().apply(arg.eval()),
            Lambda(arg, body) => Lambda(arg, Box::new(body.eval())),
        }
    }

    pub fn apply(self, arg: Self) -> Self {
        match self {
            Lambda(name, mut body) => {
                body.replace(&name, &arg);
                body.eval()
            },
            val => App(Box::new(val), Box::new(arg)),
        }
    }

    pub fn replace(&mut self, name: &str, val: &Self) {
        match self {
            Var(ref s) => {
                if s == name {
                    *self = val.clone()
                }
            },
            App(ref mut fun, ref mut arg) => {
                fun.replace(name, val);
                arg.replace(name, val);
            },
            Lambda(ref s, ref mut body) => {
                if s != name {
                    body.replace(name, val);
                }
            },
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Var(ref s) => write!(fmtr, "{}", s),
            App(ref fun, ref arg) => {
                write!(
                    fmtr,
                    "{} {}",
                    match **fun {
                        Lambda(_, _) => format!("({})", fun),
                        _ => format!("{}", fun),
                    },
                    match **arg {
                        Var(_) => format!("{}", arg),
                        _ => format!("({})", arg),
                    }
                )
            },
            Lambda(ref arg, ref body) => write!(fmtr, "λ{}.{}", arg, body),
        }
    }
}
