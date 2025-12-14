use std::collections::{HashMap};
use std::time::Instant;
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
    bindings: HashMap<Identifier, Value<'src>>,
}


fn bin_op<'src>(op: Bop, lhs: Value, rhs: Value) -> Value<'src> {
    match (op, lhs, rhs) {
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


fn walk<'src>(env: Env<'src>, expr: &'src Expr) -> Value<'src> {
    match expr {
        Expr::Lit(Literal::Int(i)) => Value::Int(*i),
        Expr::Lit(Literal::Bool(b)) => Value::Bool(*b),
        Expr::Lit(Literal::Unit) => Value::Unit,
        Expr::Ident(var) => env.bindings.get(var).expect(format!("Unknown binding: {}", var).as_str()).clone(),
        Expr::Let(name, body, rest) => {
            let mut new_env = env.clone();
            let value = walk(env, body);
            new_env.bindings.insert(name.clone(), value);
            walk(new_env, rest)
        }
        Expr::BinOp(op, lhs, rhs) => {
            let left = walk(env.clone(), lhs);
            let right = walk(env, rhs);
            bin_op(*op, left, right)
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


#[derive(Debug, Clone)]
struct InterpreterSettings {
    instr_stack_max: usize,
    op_stack_max: usize,
    env_stack_max: usize,
}

const INSTR_STACK_BYTES: usize = 1024 * 1024 * 2;
const OP_STACK_BYTES: usize = 1024 * 1024 * 2;
const ENV_STACK_MAX: usize = 1024 * 1024 * 2;
impl Default for InterpreterSettings {
    fn default() -> Self {
        Self {
            instr_stack_max: INSTR_STACK_BYTES / size_of::<Instruction>(),
            op_stack_max: OP_STACK_BYTES / size_of::<Value>(),
            env_stack_max: ENV_STACK_MAX,
        }
    }
}


#[derive(Debug, Clone, thiserror::Error)]
enum InterpError {
    #[error("Call Stack overflowed")]
    StackOverflow,
    #[error("Operand Stack overflowed")]
    OpStackOverflow,
    #[error("Too many environments")]
    EnvStackOverflow,
}


enum Instruction<'src> {
    ReadExpr(&'src Expr),
    DoBinOp(Bop),
    DoIf(&'src Expr, &'src Expr),
    Bind(Identifier),
    PopEnv,
    Apply,
}


struct Interpreter<'src> {
    instr_stack: Vec<Instruction<'src>>,
    op_stack: Vec<Value<'src>>,
    env_stack: Vec<Env<'src>>,
    settings: InterpreterSettings,
}
impl Default for Interpreter<'_> {
    fn default() -> Self {
        Interpreter::new(InterpreterSettings::default())
    }
}
impl <'src> Interpreter<'src> {
    pub fn new(settings: InterpreterSettings) -> Self {
        Self {
            instr_stack: Vec::new(),
            op_stack: Vec::new(),
            env_stack: Vec::new(),
            settings,
        }
    }

    pub fn run(&mut self, ast: &'src Expr) -> Result<(), InterpError> {
        self.instr_stack.push(Instruction::ReadExpr(ast));
        self.env_stack.push(Env::default());

        // our main loop
        while self.has_instrs() {

            if self.instr_stack.len() > self.settings.instr_stack_max {
                return Err(InterpError::StackOverflow);
            }
            if self.op_stack.len() > self.settings.op_stack_max {
                return Err(InterpError::OpStackOverflow);
            }
            if self.env_stack.len() > self.settings.env_stack_max {
                return Err(InterpError::EnvStackOverflow);
            }



            let instr = self.pop_instr();
            match instr {
                // easy: push a literal onto the operand stack
                Instruction::ReadExpr(Expr::Lit(Literal::Int(i))) => self.push_value(Value::Int(*i)),
                Instruction::ReadExpr(Expr::Lit(Literal::Bool(b))) => self.push_value(Value::Bool(*b)),
                Instruction::ReadExpr(Expr::Lit(Literal::Unit)) => self.push_value(Value::Unit),
                Instruction::ReadExpr(Expr::BinOp(op, lhs, rhs)) => {
                    self.push_instr(Instruction::DoBinOp(*op));
                    self.push_instr(Instruction::ReadExpr(rhs));
                    self.push_instr(Instruction::ReadExpr(lhs));
                }
                Instruction::ReadExpr(Expr::If(cond, then, else_)) => {
                    self.push_instr(Instruction::DoIf(then, else_));
                    self.push_instr(Instruction::ReadExpr(cond));
                }

                Instruction::ReadExpr(Expr::Ident(var)) => {
                    let value = self.env().bindings.get(var).expect(format!("Unknown Binding: {}", var).as_str()).clone();
                    self.push_value(value);
                }
                Instruction::ReadExpr(Expr::Let(name, body, rest)) => {
                    self.push_instr(Instruction::PopEnv);
                    self.push_instr(Instruction::ReadExpr(rest));
                    self.push_instr(Instruction::Bind(name.clone()));
                    self.push_instr(Instruction::ReadExpr(body));
                }


                Instruction::ReadExpr(Expr::Closure(name, arg, body)) => {
                    let current_env = self.env().clone();
                    self.push_value(Value::Closure(name.clone(), arg.clone(), body, Box::new(current_env)));
                }

                Instruction::ReadExpr(Expr::App(func, arg)) => {
                    self.push_instr(Instruction::Apply);
                    self.push_instr(Instruction::ReadExpr(func));
                    self.push_instr(Instruction::ReadExpr(arg));
                }

                // our op stack *should* have two values on it now
                Instruction::DoBinOp(op) => {
                    assert!(self.op_stack.len() >= 2, "Can't do bin op: op stack does not have at least two values.");
                    let rhs = self.pop_value();
                    let lhs = self.pop_value();
                    self.push_value(bin_op(op, lhs, rhs));
                }

                Instruction::DoIf(then, else_) => {
                    let Value::Bool(cond) = self.pop_value() else {
                        panic!("Can't do if on non-boolean")
                    };
                    if cond {
                        self.push_instr(Instruction::ReadExpr(then));
                    }
                    else {
                        self.push_instr(Instruction::ReadExpr(else_));
                    }
                }

                Instruction::Bind(id) => {
                    let value = self.pop_value();
                    let new_env = self.env().clone();
                    self.push_env(new_env);
                    self.env().bindings.insert(id, value);
                }

                Instruction::PopEnv => {
                    self.pop_env();
                }

                Instruction::Apply => {
                    assert!(self.op_stack.len() >= 2, "Can't do application: op stack does not have at least two values.");
                    let Value::Closure(name, arg, body, clos_env) = self.pop_value() else {
                        panic!("Can't apply to non closure");
                    };
                    let arg_value = self.pop_value();
                    self.push_env(*clos_env.clone());
                    if let Some(name) = name {
                        self.env().bindings.insert(name.clone(), Value::Closure(Some(name), arg.clone(), body, clos_env));
                    }
                    self.env().bindings.insert(arg, arg_value);
                    self.instr_stack.push(Instruction::PopEnv);
                    self.instr_stack.push(Instruction::ReadExpr(body));
                }
            }
        }
        Ok(())
    }

    fn push_value(&mut self, value: Value<'src>) {
        self.op_stack.push(value);
    }

    fn pop_value(&mut self) -> Value<'src> {
        self.op_stack.pop().expect("Can't pop value: op stack is empty!")
    }

    fn push_instr(&mut self, instr: Instruction<'src>) {
        self.instr_stack.push(instr);
    }
    fn pop_instr(&mut self) -> Instruction<'src> {
        self.instr_stack.pop().expect("Can't pop value: op stack is empty!")
    }

    fn has_instrs(&self) -> bool {
        self.instr_stack.len() > 0
    }

    fn push_env(&mut self, env: Env<'src>) {
        self.env_stack.push(env);
    }
    fn pop_env(&mut self) -> Env<'src> {
        self.env_stack.pop().expect("Can't pop env: env stack is empty!")
    }

    fn env(&mut self) -> &mut Env<'src> {
        self.env_stack.last_mut().expect("Can't read env: env stack is empty!")
    }
}







fn basic_math() {
    // if 3 + 5 > 6 then 1 * 10 else 0
    let if_  = if_(
        var("x").gt(num(6)),
        num(2).mul(num(10)),
        num(0)
    );

    let ast = let_("x", num(2), if_);


    let mut interpreter = Interpreter::default();
    println!("{:?}", ast);
    interpreter.run(&ast);
    // our final value *should* be the last thing on the operand stack
    println!("Value: {:?}", interpreter.pop_value());
}

fn factorial() {

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
                        var("n"),
                    )
                )
            )
        ),
        let_("a", app(var("factorial"), num(20)),
             var("a")
        )
    );

    // println!("{:?}", walk(Env::default(), &ast));

    let mut interpreter = Interpreter::default();
    if let Err(e) = interpreter.run(&ast) {
        println!("Error while running: {}", e);
        return;
    };
    // our final value *should* be the last thing on the operand stack
    println!("Value: {:?}", interpreter.pop_value());
}


fn fib() {

    //  let rec fib n =
    //      if n <= 1 then 1
    //      else
    //          (fib (n - 1)) + (fib (n - 2))

    let ast = let_(
        "fib",
        func(Some("fib"), "n",
             if_(
                 var("n").lte(num(1)),
                    var("n")
                 ,
                 app(
                     var("fib"), var("n").sub(num(1))
                 ).add(app(
                     var("fib"), var("n").sub(num(2))
                 )),
             )
        ),
        app(var("fib"), num(30))
    );

    // println!("{:?}", walk(Env::default(), &ast));
    // return;


    let mut interpreter = Interpreter::default();
    if let Err(e) = interpreter.run(&ast) {
        println!("Error while running: {}", e);
        return;
    };
    // our final value *should* be the last thing on the operand stack
    println!("Value: {:?}", interpreter.pop_value());
}




fn bottom_up_fib() {
    let ast = let_("fib", func(None, "n",
            if_(var("n").lte(num(1)),
                //then
                var("n"),
                //else
                let_("go", func(Some("go"), "n_two", func(None, "n_one", func(None, "i",
                    if_(var("i").eq(var("n")),
                        var("n_one").add(var("n_two")),
                        app(app(app(var("go"), var("n_one")), (var("n_two").add(var("n_one")))), (var("i").add(num(1))))
                    )
                ))),
                     app(app(app(var("go"), num(0)), num(1)), num(2))
                )
            )
        ),
        app(var("fib"), num(30))
    );

    let mut interpreter = Interpreter::default();
    if let Err(e) = interpreter.run(&ast) {
        println!("Error while running: {}", e);
        return;
    };
    // our final value *should* be the last thing on the operand stack
    println!("Value: {:?}", interpreter.pop_value());
}


fn main() {
    println!("Starting...");
    let now = Instant::now();
    fib();
    let time = now.elapsed().as_secs_f64() * 1000.0;
    println!("Took {}ms", time);
}
