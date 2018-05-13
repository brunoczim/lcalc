use std::fmt;

pub use self::Expr::*;

#[derive(Debug, Clone)]
pub enum Expr {
    Var(String),
    App(Box<Expr>, Box<Expr>),
    Lambda(String, Box<Expr>),
}

#[derive(Debug, Clone)]
enum Op {
    Eval,
    Swap,
    Apply,
    Lambda(String),
}

fn evaluator(starter: Expr) -> Expr {
    let mut ops = Vec::with_capacity(16);
    let mut exprs = Vec::with_capacity(16);
    ops.push(Op::Eval);
    exprs.push(starter);

    while let Some(op) = ops.pop() {
        match op {
            Op::Eval => {
                match exprs.pop().unwrap() {
                    Var(s) => exprs.push(Var(s)),
                    App(fun, arg) => {
                        exprs.push(*fun);
                        exprs.push(*arg);
                        ops.push(Op::Apply);
                        ops.push(Op::Eval);
                        ops.push(Op::Swap);
                        ops.push(Op::Eval);
                    },
                    Lambda(arg, body) => {
                        exprs.push(*body);
                        ops.push(Op::Lambda(arg));
                        ops.push(Op::Eval);
                    },
                }
            },

            Op::Swap => {
                let x = exprs.pop().unwrap();
                let y = exprs.pop().unwrap();
                exprs.push(x);
                exprs.push(y);
            },

            Op::Apply => {
                let fun = exprs.pop().unwrap();
                let arg = exprs.pop().unwrap();
                match fun {
                    Lambda(name, mut body) => {
                        body.replace(&name, &arg);
                        exprs.push(*body);
                        ops.push(Op::Eval);
                    },
                    _ => exprs.push(App(Box::new(fun), Box::new(arg))),
                }
            },

            Op::Lambda(arg) => {
                let body = exprs.pop().unwrap();
                exprs.push(Lambda(arg, Box::new(body)));
            },
        }
    }

    let result = exprs.pop().unwrap();
    debug_assert!(exprs.len() == 0);
    result
}

impl Expr {
    pub fn eval(self) -> Self { evaluator(self) }

    pub fn apply(self, arg: Self) -> Self {
        evaluator(App(Box::new(self), Box::new(arg)))
    }

    pub fn replace(&mut self, name: &str, val: &Self) {
        let mut stack = Vec::with_capacity(8);
        stack.push(self);
        while let Some(refer) = stack.pop() {
            match refer {
                Var(ref s) => {
                    if s == name {
                        *refer = val.clone()
                    }
                },
                App(ref mut fun, ref mut arg) => {
                    stack.reserve(2);
                    stack.push(fun);
                    stack.push(arg);
                },
                Lambda(ref s, ref mut body) => {
                    if s != name {
                        stack.push(body);
                    }
                },
            }
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
