use std::{fmt, rc::Rc};

pub use self::{Expr::*, Symbol::*};

#[derive(Debug, Clone)]
pub enum Symbol {
    Dyn(String),
    Static(Rc<str>),
}

impl fmt::Display for Symbol {
    fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Dyn(s) => fmtr.write_str(s),
            Static(s) => fmtr.write_str(s),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Var(Symbol),
    App(Box<Expr>, Box<Expr>),
    Lambda(Rc<str>, Box<Expr>),
}

impl Expr {
    pub fn eval(self) -> Self {
        #[derive(Debug, Clone)]
        enum Op {
            Eval,
            Swap,
            Apply,
            Lambda(Rc<str>),
        }

        let mut ops = Vec::with_capacity(16);
        let mut exprs = Vec::with_capacity(16);
        ops.push(Op::Eval);
        exprs.push(self);

        while let Some(op) = ops.pop() {
            match op {
                Op::Eval => match exprs.pop().unwrap() {
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

    pub fn replace(&mut self, name: &Rc<str>, val: &Self) {
        let mut stack = Vec::with_capacity(8);
        stack.push(self);
        while let Some(refer) = stack.pop() {
            match refer {
                Var(sym) => match sym {
                    Dyn(s) => if &**s == &**name {
                        *refer = val.clone()
                    },
                    Static(s) => if Rc::ptr_eq(s, name) {
                        *refer = val.clone()
                    },
                },
                App(ref mut fun, ref mut arg) => {
                    stack.reserve(2);
                    stack.push(fun);
                    stack.push(arg);
                },
                Lambda(_, ref mut body) => {
                    stack.push(body);
                },
            }
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
        enum Op<'a> {
            Display(&'a Expr),
            Write(String),
        }

        let mut ops = Vec::with_capacity(16);
        ops.push(Op::Display(self));

        while let Some(op) = ops.pop() {
            match op {
                Op::Display(e) => match e {
                    Var(x) => write!(fmtr, "{}", x)?,

                    App(fun, arg) => {
                        let mut between = String::new();
                        match **fun {
                            Lambda(_, _) => {
                                fmtr.write_str("(")?;
                                between += ")";
                            },
                            _ => (),
                        }
                        between += " ";
                        match **arg {
                            Var(_) => (),
                            _ => {
                                between += "(";
                                ops.push(Op::Write(")".into()));
                            },
                        }
                        ops.push(Op::Display(arg));
                        ops.push(Op::Write(between));
                        ops.push(Op::Display(fun));
                    },

                    Lambda(arg, body) => {
                        write!(fmtr, "λ{}.", arg)?;
                        ops.push(Op::Display(body));
                    },
                },

                Op::Write(s) => fmtr.write_str(&s)?,
            }
        }
        Ok(())
    }
}
