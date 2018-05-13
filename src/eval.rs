use std::fmt;

pub use self::Expr::*;

#[derive(Debug, Clone)]
pub enum Error {
    Undefined(String),
}

impl fmt::Display for Error {
    fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Undefined(ref sym) => {
                write!(fmtr, "Undefined symbol `{}`", sym)
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Var(String),
    App(Box<Expr>, Box<Expr>),
    Lambda(String, Box<Expr>),
}

pub type Result<T> = ::std::result::Result<T, Error>;

impl Expr {
    pub fn eval(&self) -> Result<Self> {
        match self {
            Var(ref s) => Err(Error::Undefined(s.clone())),
            App(ref fun, ref arg) => fun.apply(arg)?.eval(),
            _ => Ok(self.clone()),
        }
    }

    pub fn apply(&self, arg: &Self) -> Result<Self> {
        match self {
            Lambda(ref name, ref body) => Ok(body.replace(name, arg)),
            _ => self.eval()?.apply(arg),
        }
    }

    pub fn replace(&self, name: &str, val: &Self) -> Self {
        match self {
            Var(ref s) => {
                if s == name {
                    val.clone()
                } else {
                    Var(s.clone())
                }
            },
            App(ref fun, ref arg) => {
                App(
                    Box::new(fun.replace(name, val)),
                    Box::new(arg.replace(name, val)),
                )
            },
            Lambda(ref arg, ref body) => {
                if arg == name {
                    val.clone()
                } else {
                    Lambda(arg.clone(), Box::new(body.replace(name, val)))
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
