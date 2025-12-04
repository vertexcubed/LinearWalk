use crate::{Bop, Expr, Literal};

// Simple combinators used for making ASTs by hand


pub fn var(name: &str) -> Expr {
    Expr::Ident(name.to_string())
}
pub fn num(num: i64) -> Expr {
    Expr::Lit(Literal::Int(num))
}
pub fn bool(value: bool) -> Expr {
    Expr::Lit(Literal::Bool(value))
}
pub fn unit() -> Expr {
    Expr::Lit(Literal::Unit)
}

pub fn let_(binding: &str, body: Expr, rest: Expr) -> Expr {
    Expr::Let(binding.to_string(), Box::new(body), Box::new(rest))
}
impl Expr {
    pub fn add(self, rhs: Expr) -> Expr {
        Expr::BinOp(Bop::Add, Box::new(self), Box::new(rhs))
    }
    pub fn sub(self, rhs: Expr) -> Expr {
        Expr::BinOp(Bop::Sub, Box::new(self), Box::new(rhs))
    }
    pub fn mul(self, rhs: Expr) -> Expr {
        Expr::BinOp(Bop::Mul, Box::new(self), Box::new(rhs))
    }
    pub fn div(self, rhs: Expr) -> Expr {
        Expr::BinOp(Bop::Div, Box::new(self), Box::new(rhs))
    }
    pub fn lt(self, rhs: Expr) -> Expr {
        Expr::BinOp(Bop::Lt, Box::new(self), Box::new(rhs))
    }
    pub fn lte(self, rhs: Expr) -> Expr {
        Expr::BinOp(Bop::Lte, Box::new(self), Box::new(rhs))
    }
    pub fn gt(self, rhs: Expr) -> Expr {
        Expr::BinOp(Bop::Gt, Box::new(self), Box::new(rhs))
    }
    pub fn gte(self, rhs: Expr) -> Expr {
        Expr::BinOp(Bop::Gte, Box::new(self), Box::new(rhs))
    }
    pub fn eq(self, rhs: Expr) -> Expr {
        Expr::BinOp(Bop::Eq, Box::new(self), Box::new(rhs))
    }
    pub fn neq(self, rhs: Expr) -> Expr {
        Expr::BinOp(Bop::Neq, Box::new(self), Box::new(rhs))
    }

}

pub fn if_(cond: Expr, then: Expr, else_: Expr) -> Expr {
    Expr::If(Box::new(cond), Box::new(then), Box::new(else_))
}
pub fn func(name: Option<&str>, arg: &str, body: Expr) -> Expr {
    Expr::Closure(name.map(|s| s.to_string()), arg.to_string(), Box::new(body))
}
pub fn app(func: Expr, arg: Expr) -> Expr {
    Expr::App(Box::new(func), Box::new(arg))
}