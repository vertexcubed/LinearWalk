use std::collections::{BTreeMap};
use crate::combinator::*;


#[allow(dead_code)]
mod combinator;

type Identifier = String;

#[derive(Debug)]
enum Expr {
    Lit(Literal),
    Ident(Identifier),
    // let x = 2 in ...
    Let(Identifier, Box<Expr>, Box<Expr>),
    BinOp(Bop, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Closure(Option<Identifier>, Identifier, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
}
#[derive(Debug, Copy, Clone)]
enum Bop {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Neq,
}

#[derive(Debug, Copy, Clone)]
enum Literal {
    Int(i64),
    Bool(bool),
    Unit,
}

#[derive(Debug, Clone)]
enum Value<'src> {
    Int(i64),
    Bool(bool),
    Unit,
    // closure name (if present), arg identifier, expr its pointing to
    Closure(Option<Identifier>, Identifier, &'src Expr, Box<Env<'src>>)
}

#[derive(Debug, Clone, Default)]
struct Env<'src> {
    bindings: BTreeMap<Identifier, Value<'src>>,
}



fn walk<'src>(env: Env<'src>, expr: &'src Expr) -> Value<'src> {
    match expr {
        Expr::Lit(Literal::Int(i)) => Value::Int(*i),
        Expr::Lit(Literal::Bool(b)) => Value::Bool(*b),
        Expr::Lit(Literal::Unit) => Value::Unit,
        Expr::Ident(var) => env.bindings.get(var).expect(format!("Unknown binding: {}", var).as_str()).clone(),
        Expr::Let(binding, value, rest) => {
            let mut new_env = env.clone();
            let value = walk(env, value);
            new_env.bindings.insert(binding.clone(), value);
            walk(new_env, rest)
        }
        Expr::BinOp(op, lhs, rhs) => {
            let left = walk(env.clone(), lhs);
            let right = walk(env, rhs);
            match (op, left, right) {
                (Bop::Add, Value::Int(l), Value::Int(r)) => Value::Int(l + r),
                (Bop::Sub, Value::Int(l), Value::Int(r)) => Value::Int(l - r),
                (Bop::Mul, Value::Int(l), Value::Int(r)) => Value::Int(l * r),
                (Bop::Div, Value::Int(l), Value::Int(r)) => Value::Int(l / r),
                (Bop::Lt, Value::Int(l), Value::Int(r)) => Value::Bool(l < r),
                (Bop::Lte, Value::Int(l), Value::Int(r)) => Value::Bool(l <= r),
                (Bop::Gt, Value::Int(l), Value::Int(r)) => Value::Bool(l > r),
                (Bop::Gte, Value::Int(l), Value::Int(r)) => Value::Bool(l >= r),
                (Bop::Eq, Value::Int(l), Value::Int(r)) => Value::Bool(l == r),
                (Bop::Neq, Value::Int(l), Value::Int(r)) => Value::Bool(l != r),
                _ => panic!("Bad op types")
            }
        }
        Expr::If(cond, then, else_) => {
            let Value::Bool(c) = walk(env.clone(), cond) else { panic!("Can't do if on non-boolean") };
            if c { walk(env, then) } else { walk(env, else_) }
        }
        Expr::Closure(name, arg, body) => {
            Value::Closure(name.clone(), arg.clone(), body.as_ref(), Box::new(env))
        }
        Expr::App(func, arg) => {
            // evaluate argument in current env
            let arg_value = walk(env.clone(), arg);
            // evaluate function in current env
            let Value::Closure(name, arg_id, body, clos_env) = walk(env, func) else {
                panic!("Can't apply to non closure")
            };
            let mut new_env = clos_env.clone();
            if let Some(name) = name {
                new_env.bindings.insert(name.clone(), Value::Closure(Some(name), arg_id.clone(), body, clos_env));
            }
            // bind argument
            new_env.bindings.insert(arg_id, arg_value);
            // evaluate function body
            walk(*new_env, body)
        }
    }
}




fn main() {
    println!("Starting...");


    //  let rec factorial n =
    //      if n <= 1 then
    //          1
    //      else
    //          n * factorial (n - 1)
    //  in
    //  let a = factorial 10 in
    //  a

    let ast = let_(
        "factorial", func(Some("factorial"), "n",
            if_(var("n").lte(num(1)),
                num(1),
                var("n").mul(
                    app(
                        var("factorial"),
                        var("n").sub(num(1)),
                    )
                )
            )
        ),
        let_("a", app(var("factorial"), num(10)),
             var("a")
        )
    );

    let env = Env::default();
    println!("Value: {:?}", walk(env, &ast));
}
