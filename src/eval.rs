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
            Static(s) => write!(fmtr, "{}", s),
        }
    }
}

#[derive(Debug)]
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
            Apply(Expr),
            ApplyTo(Expr),
            Lambda(Rc<str>),
        }
        let mut ops = Vec::with_capacity(16);
        let mut exprs = Vec::with_capacity(16);

        ops.push(Op::Eval);
        exprs.push(self);

        while let Some(op) = ops.pop() {
            match op {
                Op::Eval => match exprs.pop().unwrap() {
                    Var(var) => exprs.push(Var(var)),

                    App(fun, arg) => {
                        let fun = *fun;
                        match fun {
                            Lambda(s, mut body) => {
                                body.replace(&s, &*arg);
                                ops.push(Op::Eval);
                                exprs.push(*body);
                            },
                            fun => {
                                ops.push(Op::Apply(*arg));
                                ops.push(Op::Eval);
                                exprs.push(fun);
                            },
                        }
                    },

                    Lambda(arg, body) => {
                        ops.push(Op::Lambda(arg));
                        ops.push(Op::Eval);
                        exprs.push(*body);
                    },
                },

                Op::Apply(arg) => match exprs.pop().unwrap().eval() {
                    Lambda(s, mut body) => {
                        body.replace(&s, &arg);
                        ops.push(Op::Eval);
                        exprs.push(*body);
                    },
                    fun => {
                        ops.push(Op::ApplyTo(fun));
                        ops.push(Op::Eval);
                        exprs.push(arg);
                    },
                },

                Op::ApplyTo(fun) => {
                    let arg = exprs.pop().unwrap();
                    exprs.push(App(Box::new(fun), Box::new(arg)));
                },

                Op::Lambda(arg) => {
                    let body = exprs.pop().unwrap();
                    exprs.push(Lambda(arg, Box::new(body)));
                },
            }
        }

        let res = exprs.pop().unwrap();
        debug_assert_eq!(exprs.len(), 0);
        res
    }

    pub fn replace(&mut self, name: &Rc<str>, val: &Self) {
        let mut stack = Vec::with_capacity(8);
        stack.push(self);
        while let Some(refer) = stack.pop() {
            match refer {
                Var(sym) => match sym {
                    Dyn(s) => if &**s == &**name {
                        *refer = val.clone();
                    },
                    Static(s) => if Rc::ptr_eq(s, name) {
                        *refer = val.clone();
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

impl Clone for Expr {
    fn clone(&self) -> Self {
        enum Op<'a> {
            Clone(&'a Expr),
            Apply,
            Lambda(&'a Rc<str>, Rc<str>),
        }
        let mut dests = Vec::with_capacity(16);
        let mut ops = Vec::with_capacity(16);
        ops.push(Op::Clone(self));

        while let Some(op) = ops.pop() {
            match op {
                Op::Clone(e) => match e {
                    Var(sym) => dests.push(Var(match sym {
                        Dyn(s) => Dyn(s.clone()),
                        Static(s) => {
                            let lambda = ops
                                .iter()
                                .filter_map(|x| match x {
                                    Op::Lambda(orig, clone) => {
                                        Some((*orig, clone))
                                    },
                                    _ => None,
                                })
                                .find(|(orig, _)| Rc::ptr_eq(orig, s));
                            match lambda {
                                Some((_, rc)) => Static(rc.clone()),
                                _ => Static(s.clone()),
                            }
                        },
                    })),

                    App(fun, arg) => {
                        ops.push(Op::Apply);
                        ops.push(Op::Clone(fun));
                        ops.push(Op::Clone(arg));
                    },

                    Lambda(arg, body) => {
                        ops.push(Op::Lambda(arg, (&**arg).into()));
                        ops.push(Op::Clone(&**body));
                    },
                },

                Op::Apply => {
                    let fun = dests.pop().unwrap();
                    let arg = dests.pop().unwrap();
                    dests.push(App(Box::new(fun), Box::new(arg)));
                },

                Op::Lambda(_, arg) => {
                    let body = dests.pop().unwrap();
                    dests.push(Lambda(arg, Box::new(body)));
                },
            }
        }

        let res = dests.pop().unwrap();
        debug_assert_eq!(dests.len(), 0);
        res
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
