#[macro_use]
use ::core::*;

#[macro_use]
use ::core::errors::*;

#[macro_use]
use ::core::types::*;



use crate::reader::parser::*;

use std::fmt;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::iter;
use std::vec;

pub fn new() -> Result<Interpreter, RuntimeError> {
    Interpreter::new()
}

#[derive(Clone)]
pub struct Interpreter {
    root: Rc<RefCell<Environment>>
}

//Macros
// null == empty list

impl Interpreter {
    pub fn new() -> Result<Interpreter, RuntimeError> {
        let env = try!(Environment::new_root());
        Ok(Interpreter { root: env })
    }

    pub fn run(&self, nodes: &[Node]) -> Result<Value, RuntimeError> {
        let exprs = List::from_nodes(nodes);
        process(exprs, self.root.clone())
    }
}

impl Continuation {
    fn run(self, val: Value) -> Result<Trampoline, RuntimeError> {
        match self {
            Continuation::EvaluateExpressions(rest, env, k) => {
                if !rest.is_empty() {
                    evaluate_expressions(rest, env, k)
                } else {
                    Ok(Trampoline::Run(val, *k))
                }
            },
            Continuation::BeginFunc(rest, env, k) => {
                match val {
                    Value::SpecialForm(f) => {
                        match f {
                            SpecialForm::If => {
                                let (condition, if_expr, else_expr) = try!(rest.unpack3());
                                Ok(Trampoline::Bounce(condition, env.clone(), Continuation::EvaluateIf(if_expr, else_expr, env, k)))
                            },
                            SpecialForm::Define => {
                                let (car, cdr) = shift_or_error!(rest, "Must provide at least two arguments to define");
                                match car {
                                    Value::Symbol(name) => {
                                        let val = try!(cdr.unpack1());
                                        Ok(Trampoline::Bounce(val, env.clone(), Continuation::EvaluateDefine(name, env, k)))
                                    },
                                    Value::List(list) => {
                                        let (caar, cdar) = shift_or_error!(list, "Must provide at least two params in first argument of define");
                                        let name = try!(caar.as_symbol());

                                        let arg_names = try!(cdar.into_iter().map(|v| v.as_symbol()).collect());
                                        let body = cdr;
                                        let f = Function::Scheme(arg_names, body, env.clone());

                                        try!(env.borrow_mut().define(name, Value::Procedure(f)));
                                        Ok(Trampoline::Run(null!(), *k))
                                    },
                                    _ => runtime_error!("Bad argument to define: {:?}", car)
                                }
                            },
                            SpecialForm::Set => {
                                let (name_raw, val) = try!(rest.unpack2());
                                let name = try!(name_raw.as_symbol());
                                Ok(Trampoline::Bounce(val, env.clone(), Continuation::EvaluateSet(name, env, k)))
                            },
                            SpecialForm::Lambda => {
                                let (arg_defns_raw, body) = shift_or_error!(rest, "Must provide at least two arguments to lambda");
                                let arg_defns = try!(arg_defns_raw.as_list());
                                let arg_names = try!(arg_defns.into_iter().map(|v| v.as_symbol()).collect());

                                let f = Function::Scheme(arg_names, body, env);
                                Ok(Trampoline::Run(Value::Procedure(f), *k))
                            },
                            SpecialForm::Let => {
                                let (arg_defns_raw, body) = shift_or_error!(rest, "Must provide at least two arguments to let");
                                let arg_defns = try!(arg_defns_raw.as_list());

                                // Create a new, child environment for the procedure and define the arguments as local variables
                                let proc_env = Environment::new_child(env.clone());

                                // Iterate through the provided arguments, defining them
                                if !arg_defns.is_empty() {
                                    let (first_defn, rest_defns) = shift_or_error!(arg_defns, "Error in let definiton");
                                    let (defn_key, defn_val) = try!(try!(first_defn.as_list()).unpack2());
                                    let name = try!(defn_key.as_symbol());
                                    Ok(Trampoline::Bounce(defn_val, env, Continuation::EvaluateLet(name, rest_defns, body, proc_env, k)))
                                } else {
                                    // Let bindings were empty, just execute the body directly
                                    evaluate_expressions(body, env, k)
                                }
                            },
                            SpecialForm::Quote => {
                                let expr = try!(rest.unpack1());
                                Ok(Trampoline::Run(expr, *k))
                            },
                            SpecialForm::Quasiquote => {
                                let expr = try!(rest.unpack1());
                                match expr {
                                    Value::List(list) => {
                                        match list.shift() {
                                            Some((car, cdr)) => Ok(Trampoline::QuasiBounce(car, env.clone(), Continuation::ContinueQuasiquoting(cdr, List::Null, env, k))),
                                            None => Ok(Trampoline::Run(null!(), *k))
                                        }
                                    },
                                    _ => Ok(Trampoline::Run(expr, *k))
                                }
                            },
                            SpecialForm::Eval => {
                                let expr = try!(rest.unpack1());
                                Ok(Trampoline::Bounce(expr, env.clone(), Continuation::ExecuteEval(env, k)))
                            },
                            SpecialForm::Apply => {
                                let (func, args) = try!(rest.unpack2());
                                Ok(Trampoline::Bounce(func, env.clone(), Continuation::EvaluateApplyArgs(args, env, k)))
                            },
                            SpecialForm::Begin => {
                                match rest.shift() {
                                    Some((car, cdr)) => Ok(Trampoline::Bounce(car, env.clone(), Continuation::EvaluateExpressions(cdr, env, k))),
                                    None => runtime_error!("Must provide at least one argument to a begin statement")
                                }
                            },
                            SpecialForm::And => {
                                match rest.shift() {
                                    Some((car, cdr)) => Ok(Trampoline::Bounce(car, env.clone(), Continuation::EvaluateAnd(cdr, env, k))),
                                    None => Ok(Trampoline::Run(Value::Boolean(true), *k))
                                }
                            },
                            SpecialForm::Or => {
                                match rest.shift() {
                                    Some((car, cdr)) => Ok(Trampoline::Bounce(car, env.clone(), Continuation::EvaluateOr(cdr, env, k))),
                                    None => Ok(Trampoline::Run(Value::Boolean(false), *k))
                                }
                            },
                            SpecialForm::CallCC => {
                                let f = try!(rest.unpack1());
                                Ok(Trampoline::Bounce(f, env, Continuation::ExecuteCallCC(k)))
                            },
                            SpecialForm::DefineSyntaxRule => {
                                let (defn, body) = try!(rest.unpack2());

                                let (name, arg_names_raw) = match try!(defn.as_list()).shift() {
                                    Some((car, cdr)) => (try!(car.as_symbol()), cdr),
                                    None => runtime_error!("Must supply at least two params to first argument in define-syntax-rule")
                                };

                                let arg_names = try!(arg_names_raw.into_iter().map(|v| v.as_symbol()).collect());

                                let m = Value::Macro(arg_names, Box::new(body));
                                try!(env.borrow_mut().define(name, m));
                                Ok(Trampoline::Run(null!(), *k))
                            },
                        }
                    },
                    Value::Macro(arg_names, body) => {
                        let args = rest;
                        if arg_names.len() != args.len() {
                            runtime_error!("Must supply exactly {} arguments to macro: {:?}", arg_names.len(), args);
                        }

                        // Create a lookup table for symbol substitutions
                        let mut substitutions = HashMap::new();
                        for (name, value) in arg_names.into_iter().zip(args.into_iter()) {
                            substitutions.insert(name, value);
                        }

                        // Expand the macro
                        let expanded = expand_macro(*body, &substitutions);

                        // Finished expanding macro, now evaluate the code manually
                        Ok(Trampoline::Bounce(expanded, env, *k))
                    },
                    _ => {
                        match rest.shift() {
                            Some((car, cdr)) => Ok(Trampoline::Bounce(car, env.clone(), Continuation::EvaluateFunc(val, cdr, List::Null, env, k))),
                            None => apply(val, List::Null, k)
                        }
                    }
                }
            },
            Continuation::EvaluateFunc(f, rest, acc, env, k) => {
                let acc2 = acc.unshift(val);
                match rest.shift() {
                    Some((car, cdr)) => Ok(Trampoline::Bounce(car, env.clone(), Continuation::EvaluateFunc(f, cdr, acc2, env, k))),
                    None => apply(f, acc2.reverse(), k)
                }
            },
            Continuation::EvaluateIf(if_expr, else_expr, env, k) => {
                match val {
                    Value::Boolean(false) => Ok(Trampoline::Bounce(else_expr, env, *k)),
                    _ => Ok(Trampoline::Bounce(if_expr, env, *k))
                }
            },
            Continuation::EvaluateDefine(name, env, k) => {
                try!(env.borrow_mut().define(name, val));
                Ok(Trampoline::Run(null!(), *k))
            },
            Continuation::EvaluateSet(name, env, k) => {
                try!(env.borrow_mut().set(name, val));
                Ok(Trampoline::Run(null!(), *k))
            },
            Continuation::EvaluateLet(name, rest, body, env, k) => {
                // Define variable in let scope
                try!(env.borrow_mut().define(name, val));
                match rest.shift() {
                    Some((next_defn, rest_defns)) => {
                        let (defn_key, defn_val) = try!(try!(next_defn.as_list()).unpack2());
                        let name = try!(defn_key.as_symbol());
                        Ok(Trampoline::Bounce(defn_val, env.clone(), Continuation::EvaluateLet(name, rest_defns, body, env, k)))
                    },
                    None => {
                        let inner_env = Environment::new_child(env);
                        evaluate_expressions(body, inner_env, k)
                    }
                }
            },
            Continuation::ContinueQuasiquoting(rest, acc, env, k) => {
                let acc2 = acc.unshift(val);
                match rest.shift() {
                    Some((car, cdr)) => Ok(Trampoline::QuasiBounce(car, env.clone(), Continuation::ContinueQuasiquoting(cdr, acc2, env, k))),
                    None => Ok(Trampoline::Run(acc2.reverse().to_value(), *k))
                }
            },
            Continuation::ExecuteEval(env, k) => {
                Ok(Trampoline::Bounce(val, Environment::get_root(env), *k))
            },
            Continuation::EvaluateApplyArgs(args, env, k) => {
                Ok(Trampoline::Bounce(args, env, Continuation::ExecuteApply(val, k)))
            },
            Continuation::ExecuteApply(f, k) => {
                apply(f, try!(val.as_list()), k)
            },
            Continuation::EvaluateAnd(rest, env, k) => {
                match val {
                    Value::Boolean(false) => Ok(Trampoline::Run(Value::Boolean(false), *k)),
                    _ => {
                        match rest.shift() {
                            Some((car, cdr)) => Ok(Trampoline::Bounce(car, env.clone(), Continuation::EvaluateAnd(cdr, env, k))),
                            None => Ok(Trampoline::Run(val, *k))
                        }
                    }
                }
            },
            Continuation::EvaluateOr(rest, env, k) => {
                match val {
                    Value::Boolean(false) => {
                        match rest.shift() {
                            Some((car, cdr)) => Ok(Trampoline::Bounce(car, env.clone(), Continuation::EvaluateOr(cdr, env, k))),
                            None => Ok(Trampoline::Run(Value::Boolean(false), *k))
                        }
                    },
                    _ => Ok(Trampoline::Run(val, *k))
                }
            },
            Continuation::ExecuteCallCC(k) => {
                apply(val, List::Null.unshift(Value::Continuation(k.clone())), k)
            },
            Continuation::Return => Ok(Trampoline::Land(val))
        }
    }
}

fn apply(val: Value, args: List, k: Box<Continuation>) -> Result<Trampoline, RuntimeError> {
    match val {
        Value::Procedure(f) => {
            match f {
                Function::Scheme(arg_names, body, func_env) => {
                    if arg_names.len() != args.len() {
                        runtime_error!("Must supply exactly {} arguments to function: {:?}", arg_names.len(), args);
                    }

                    // Create a new, child environment for the procedure and define the arguments as local variables
                    let proc_env = Environment::new_child(func_env);
                    for (name, value) in arg_names.into_iter().zip(args.into_iter()) {
                        try!(proc_env.borrow_mut().define(name, value));
                    }

                    // Evaluate procedure body with new environment with procedure environment as parent
                    let inner_env = Environment::new_child(proc_env);
                    evaluate_expressions(body, inner_env, k)
                },
                Function::Native(g) => {
                    let res = try!(primitive(g, args));
                    Ok(Trampoline::Run(res, *k))
                },
            }
        },
        Value::Continuation(k_prime) => {
            Ok(Trampoline::Run(args.to_value(), *k_prime))
        },
        _ => {
            runtime_error!("Don't know how to apply: {:?}", val)
        }
    }
}

fn expand_macro(value: Value, substitutions: &HashMap<String,Value>) -> Value {
    match value {
        Value::Symbol(s) => {
            match substitutions.get(&s) {
                Some(v) => v.clone(),
                None => Value::Symbol(s)
            }
        },
        Value::List(list) => {
            let expanded = list.into_iter().map(|v| expand_macro(v, substitutions)).collect();
            Value::from_vec(expanded)
        },
        other => other
    }
}

fn evaluate_expressions(exprs: List, env: Rc<RefCell<Environment>>, k: Box<Continuation>) -> Result<Trampoline, RuntimeError> {
    match exprs.shift() {
        Some((car, cdr)) => Ok(Trampoline::Bounce(car, env.clone(), Continuation::EvaluateExpressions(cdr, env, k))),
        None => runtime_error!("Trying to evaluate an empty expression list")
    }
}

fn process(exprs: List, env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if exprs.len() == 0 {
        return Ok(null!());
    }

    let mut b = try!(evaluate_expressions(exprs, env, Box::new(Continuation::Return)));
    loop {
        match b {
            // Bounce is the usual execution path. It's used for pretty much everything.
            // Special forms are caught here instead of in env so that they can't be redefined in env.
            Trampoline::Bounce(a, env, k) => {
                b = match a {
                    Value::List(list) => {
                        match list.shift() {
                            Some((car, cdr)) => Trampoline::Bounce(car, env.clone(), Continuation::BeginFunc(cdr, env, Box::new(k))),
                            //TODO: Return an empty list
                            None => runtime_error!("Can't apply an empty list as a function")
                        }
                    },
                    Value::Symbol(ref s) => {
                        let val = match s.as_ref() {
                            "if"     => Value::SpecialForm(SpecialForm::If),
                            "define" => Value::SpecialForm(SpecialForm::Define),
                            "set!"   => Value::SpecialForm(SpecialForm::Set),
                            "lambda" => Value::SpecialForm(SpecialForm::Lambda),
                            "λ"      => Value::SpecialForm(SpecialForm::Lambda),
                            "let"    => Value::SpecialForm(SpecialForm::Let),
                            "quote"  => Value::SpecialForm(SpecialForm::Quote),
                            "quasiquote" => Value::SpecialForm(SpecialForm::Quasiquote),
                            "eval"   => Value::SpecialForm(SpecialForm::Eval),
                            "apply"  => Value::SpecialForm(SpecialForm::Apply),
                            "begin"  => Value::SpecialForm(SpecialForm::Begin),
                            "and"    => Value::SpecialForm(SpecialForm::And),
                            "or"     => Value::SpecialForm(SpecialForm::Or),
                            "call/cc" => Value::SpecialForm(SpecialForm::CallCC),
                            "define-syntax-rule" => Value::SpecialForm(SpecialForm::DefineSyntaxRule),
                            _ => {
                                match env.borrow().get(s) {
                                    Some(v) => v,
                                    None => runtime_error!("Identifier not found: {}", s)
                                }
                            }
                        };
                        try!(k.run(val))
                    },
                    _ => try!(k.run(a))
                }
            },

            // QuasiBounce is for quasiquoting mode -- it just passes the value right through, UNLESS it's of the form (unquote X), in which case it switches back to regular evaluating mode using X as the value.
            Trampoline::QuasiBounce(a, env, k) => {
                b = match a {
                    Value::List(list) => {
                        match list.shift() {
                            Some((car, cdr)) => {
                                match car {
                                    Value::Symbol(ref s) if s == "unquote" => {
                                        let expr = try!(cdr.unpack1());
                                        Trampoline::Bounce(expr, env, k)
                                    },
                                    _ => {
                                        Trampoline::QuasiBounce(car, env.clone(), Continuation::ContinueQuasiquoting(cdr, List::Null, env, Box::new(k)))
                                    }
                                }
                            },
                            None => try!(k.run(null!()))
                        }
                    },
                    _ => try!(k.run(a))
                }
            },

            // Run doesn't evaluate the value, it just runs k with it. It's similar to running inline, but bounces to avoid growing the stack.
            Trampoline::Run(a, k) => {
                b = try!(k.run(a))
            },

            // Land just returns the value. It should only ever be created at the very beginning of process, and will be the last Trampoline value called.
            Trampoline::Land(a) => {
                return Ok(a);
            },
        }
    };
}



fn primitive(f: &'static str, args: List) -> Result<Value, RuntimeError> {
    match f {
        "+" => {
            let sum = try!(args.into_iter().fold(Ok(0), |s, a| match s {
                Ok(z) => Ok(z + try!(a.as_integer())),
                _ => s
            }));
            Ok(Value::Integer(sum))
        },
        "-" => {
            if args.len() != 2 {
                runtime_error!("Must supply exactly two arguments to -: {:?}", args);
            }
            let (l, r) = try!(args.unpack2());
            Ok(Value::Integer(try!(l.as_integer()) - try!(r.as_integer())))
        },
        "*" => {
            let product = try!(args.into_iter().fold(Ok(1), |s, a| match s {
                Ok(z) => Ok(z * try!(a.as_integer())),
                _ => s
            }));
            Ok(Value::Integer(product))
        },
        "/" => {
            if args.len() != 2 {
                runtime_error!("Must supply exactly two arguments to /: {:?}", args);
            }
            let (l, r) = try!(args.unpack2());
            Ok(Value::Integer(try!(l.as_integer()) / try!(r.as_integer())))
        },
        "<" => {
            if args.len() != 2 {
                runtime_error!("Must supply exactly two arguments to <: {:?}", args);
            }
            let (l, r) = try!(args.unpack2());
            Ok(Value::Boolean(try!(l.as_integer()) < try!(r.as_integer())))
        },
        ">" => {
            if args.len() != 2 {
                runtime_error!("Must supply exactly two arguments to >: {:?}", args);
            }
            let (l, r) = try!(args.unpack2());
            Ok(Value::Boolean(try!(l.as_integer()) > try!(r.as_integer())))
        },
        "=" => {
            if args.len() != 2 {
                runtime_error!("Must supply exactly two arguments to =: {:?}", args);
            }
            let (l, r) = try!(args.unpack2());
            Ok(Value::Boolean(try!(l.as_integer()) == try!(r.as_integer())))
        },
        "null?" => {
            if args.len() != 1 {
                runtime_error!("Must supply exactly one argument to null?: {:?}", args);
            }
            let v = try!(args.unpack1());
            match v {
                Value::List(l) => Ok(Value::Boolean(l.is_empty())),
                _ => Ok(Value::Boolean(false))
            }
        },
        "list" => {
            Ok(args.to_value())
        },
        "car" => {
            if args.len() != 1 {
                runtime_error!("Must supply exactly two arguments to car: {:?}", args);
            }
            let l = try!(try!(args.unpack1()).as_list());
            match l.shift() {
                Some((car, _)) => Ok(car),
                None => runtime_error!("Can't run car on an empty list")
            }
        },
        "cdr" => {
            if args.len() != 1 {
                runtime_error!("Must supply exactly two arguments to cdr: {:?}", args);
            }
            let l = try!(try!(args.unpack1()).as_list());
            match l.shift() {
                Some((_, cdr)) => Ok(cdr.to_value()),
                None => runtime_error!("Can't run cdr on an empty list")
            }
        },
        "cons" => {
            if args.len() != 2 {
                runtime_error!("Must supply exactly two arguments to cons: {:?}", args);
            }
            let (elem, list) = try!(args.unpack2());
            Ok(try!(list.as_list()).unshift(elem).to_value())
        },
        "append" => {
            if args.len() != 2 {
                runtime_error!("Must supply exactly two arguments to append: {:?}", args);
            }
            let (list1raw, list2raw) = try!(args.unpack2());
            let list1 = try!(list1raw.as_list());
            let mut list2 = try!(list2raw.as_list());

            for elem in list1.reverse() {
                list2 = list2.unshift(elem)
            }
            Ok(list2.to_value())
        },
        "error" => {
            if args.len() != 1 {
                runtime_error!("Must supply exactly one argument to error: {:?}", args);
            }
            let msg = try!(args.unpack1());
            runtime_error!("{:?}", msg)
        },
        "write" => {
            if args.len() != 1 {
                runtime_error!("Must supply exactly one argument to write: {:?}", args);
            }
            let val = try!(args.unpack1());
            print!("{:?}", val);
            Ok(null!())
        },
        "display" => {
            if args.len() != 1 {
                runtime_error!("Must supply exactly one argument to display: {:?}", args);
            }
            let val = try!(args.unpack1());
            print!("{}", val);
            Ok(null!())
        },
        "displayln" => {
            if args.len() != 1 {
                runtime_error!("Must supply exactly one argument to displayln: {:?}", args);
            }
            let val = try!(args.unpack1());
            println!("{}", val);
            Ok(null!())
        },
        "print" => {
            if args.len() != 1 {
                runtime_error!("Must supply exactly one argument to print: {:?}", args);
            }
            let val = try!(args.unpack1());
            match val {
                Value::Symbol(_) | Value::List(_) => print!("'{:?}", val),
                _ => print!("{:?}", val)
            }
            Ok(null!())
        },
        "newline" => {
            if args.len() != 0 {
                runtime_error!("Must supply exactly zero arguments to newline: {:?}", args);
            }
            println!("");
            Ok(null!())
        },
        _ => {
            runtime_error!("Unknown primitive: {:?}", f)
        }
    }
}

#[cfg(test)]
fn exec(list: List) -> Result<Value, RuntimeError> {
    process(list, try!(Environment::new_root()))
}

#[test]
fn test_add1() {
    // runTest (+ 1 2) => 3
    let i = vec![Value::from_vec(vec![Value::Symbol("+".to_string()),
                                      Value::Integer(1),
                                      Value::Integer(2)])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(3));
}

#[test]
fn test_add2() {
    // runTest (+ (+ 1 2) (+ 3 4)) => 10
    let i = vec![Value::from_vec(vec![Value::Symbol("+".to_string()),
                                      Value::from_vec(vec![Value::Symbol("+".to_string()),
                                                           Value::Integer(1),
                                                           Value::Integer(2)]),
                                      Value::from_vec(vec![Value::Symbol("+".to_string()),
                                                           Value::Integer(3),
                                                           Value::Integer(4)])])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(10));
}

#[test]
fn test_add3() {
    // runTest (+ (+ 1 2) (+ (+ 3 5 6) 4)) => 21
    let i = vec![Value::from_vec(vec![Value::Symbol("+".to_string()),
                                      Value::from_vec(vec![Value::Symbol("+".to_string()),
                                                           Value::Integer(1),
                                                           Value::Integer(2)]),
                                      Value::from_vec(vec![Value::Symbol("+".to_string()),
                                                           Value::from_vec(vec![Value::Symbol("+".to_string()),
                                                                                Value::Integer(3),
                                                                                Value::Integer(5),
                                                                                Value::Integer(6)]),
                                                           Value::Integer(4)])])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(21));
}

#[test]
fn test_subtract1() {
    // runTest (- 3 2) => 1
    let i = vec![Value::from_vec(vec![Value::Symbol("-".to_string()),
                                      Value::Integer(3),
                                      Value::Integer(2)])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(1));
}

#[test]
fn test_if1() {
    // runTest (if (> 1 2) 3 4) => 4
    let i = vec![Value::from_vec(vec![Value::Symbol("if".to_string()),
                                      Value::from_vec(vec![Value::Symbol(">".to_string()),
                                                           Value::Integer(1),
                                                           Value::Integer(2)]),
                                      Value::Integer(3),
                                      Value::Integer(4)])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(4));
}

#[test]
fn test_if2() {
    // runTest (if (> 2 3) (error 4) (error 5)) => null
    let i = vec![Value::from_vec(vec![Value::Symbol("if".to_string()),
                                      Value::from_vec(vec![Value::Symbol(">".to_string()),
                                                           Value::Integer(2),
                                                           Value::Integer(3)]),
                                      Value::from_vec(vec![Value::Symbol("error".to_string()),
                                                           Value::Integer(4)]),
                                      Value::from_vec(vec![Value::Symbol("error".to_string()),
                                                           Value::Integer(5)])])];
    assert_eq!(exec(List::from_vec(i)).unwrap_err().to_string(),
               "RuntimeError: 5");
}

#[test]
fn test_if3() {
    // runTest (if ((if (> 5 4) > <) (+ 1 2) 2) (+ 5 7 8) (+ 9 10 11)) => 20
    let i = vec![Value::from_vec(vec![Value::Symbol("if".to_string()),
                                      Value::from_vec(vec![Value::from_vec(vec![Value::Symbol("if".to_string()),
                                                                                Value::from_vec(vec![Value::Symbol(">".to_string()),
                                                                                                     Value::Integer(5),
                                                                                                     Value::Integer(4)]),
                                                                                Value::Symbol(">".to_string()),
                                                                                Value::Symbol("<".to_string())]),
                                                           Value::from_vec(vec![Value::Symbol("+".to_string()),
                                                                                Value::Integer(1),
                                                                                Value::Integer(2)]),
                                                           Value::Integer(2)]),
                                      Value::from_vec(vec![Value::Symbol("+".to_string()),
                                                           Value::Integer(5),
                                                           Value::Integer(7),
                                                           Value::Integer(8)]),
                                      Value::from_vec(vec![Value::Symbol("+".to_string()),
                                                           Value::Integer(9),
                                                           Value::Integer(10),
                                                           Value::Integer(11)])])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(20));
}

#[test]
fn test_if4() {
    // runTest (if 0 3 4) => 3
    let i = vec![Value::from_vec(vec![Value::Symbol("if".to_string()),
                                      Value::Integer(0),
                                      Value::Integer(3),
                                      Value::Integer(4)])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(3));
}

#[test]
fn test_and1() {
    // runTest (and) => #t
    let i = vec![Value::from_vec(vec![Value::Symbol("and".to_string())])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Boolean(true));
}

#[test]
fn test_and2() {
    // runTest (and #f) => #f
    let i = vec![Value::from_vec(vec![Value::Symbol("and".to_string()),
                                      Value::Boolean(false)])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Boolean(false));
}

#[test]
fn test_and3() {
    // runTest (and #f #t #f) => #f
    let i = vec![Value::from_vec(vec![Value::Symbol("and".to_string()),
                                      Value::Boolean(false),
                                      Value::Boolean(true),
                                      Value::Boolean(false)])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Boolean(false));
}

#[test]
fn test_and4() {
    // runTest (and 0 1) => 1
    let i = vec![Value::from_vec(vec![Value::Symbol("and".to_string()),
                                      Value::Integer(0),
                                      Value::Integer(1)])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(1));
}

#[test]
fn test_and5() {
    // runTest (and #f (error 2)) => #f
    let i = vec![Value::from_vec(vec![Value::Symbol("and".to_string()),
                                      Value::Boolean(false),
                                      Value::from_vec(vec![Value::Symbol("error".to_string()),
                                                           Value::Integer(2)])])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Boolean(false));
}

#[test]
fn test_or1() {
    // runTest (or) => #f
    let i = vec![Value::from_vec(vec![Value::Symbol("or".to_string())])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Boolean(false));
}

#[test]
fn test_or2() {
    // runTest (or #f) => #f
    let i = vec![Value::from_vec(vec![Value::Symbol("or".to_string()),
                                      Value::Boolean(false)])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Boolean(false));
}

#[test]
fn test_or3() {
    // runTest (or #f #t #f) => #t
    let i = vec![Value::from_vec(vec![Value::Symbol("or".to_string()),
                                      Value::Boolean(false),
                                      Value::Boolean(true),
                                      Value::Boolean(false)])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Boolean(true));
}

#[test]
fn test_or4() {
    // runTest (or 0 1) => 0
    let i = vec![Value::from_vec(vec![Value::Symbol("or".to_string()),
                                      Value::Integer(0),
                                      Value::Integer(1)])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(0));
}

#[test]
fn test_or5() {
    // runTest (or #t (error 2)) => #t
    let i = vec![Value::from_vec(vec![Value::Symbol("or".to_string()),
                                      Value::Boolean(true),
                                      Value::from_vec(vec![Value::Symbol("error".to_string()),
                                                           Value::Integer(2)])])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Boolean(true));
}

#[test]
fn test_multiple_statements() {
    // runTest (+ 1 2) (+ 3 4) => 7
    let i = vec![Value::from_vec(vec![Value::Symbol("+".to_string()),
                                      Value::Integer(1),
                                      Value::Integer(2)]),
                 Value::from_vec(vec![Value::Symbol("+".to_string()),
                                      Value::Integer(3),
                                      Value::Integer(4)])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(7));
}

#[test]
fn test_list() {
    // runTest (list 1 2 3) => '(1 2 3)
    let i = vec![Value::from_vec(vec![Value::Symbol("list".to_string()),
                                      Value::Integer(1),
                                      Value::Integer(2),
                                      Value::Integer(3)])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::from_vec(vec![Value::Integer(1),
                                    Value::Integer(2),
                                    Value::Integer(3)]));
}

#[test]
fn test_cons() {
    // runTest (cons 1 (list 2 3)) => '(1 2 3)
    let i = vec![Value::from_vec(vec![Value::Symbol("cons".to_string()),
                                      Value::Integer(1),
                                      Value::from_vec(vec![Value::Symbol("list".to_string()),
                                                           Value::Integer(2),
                                                           Value::Integer(3)])])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::from_vec(vec![Value::Integer(1),
                                    Value::Integer(2),
                                    Value::Integer(3)]));
}

#[test]
fn test_define() {
    // runTest (define x 2) (+ x x) => 4
    let i = vec![Value::from_vec(vec![Value::Symbol("define".to_string()),
                                      Value::Symbol("x".to_string()),
                                      Value::Integer(2)]),
                 Value::from_vec(vec![Value::Symbol("+".to_string()),
                                      Value::Symbol("x".to_string()),
                                      Value::Symbol("x".to_string())])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(4));
}

#[test]
fn test_set() {
    // runTest (define x 2) (set! x 3) (+ x x) => 6
    let i = vec![Value::from_vec(vec![Value::Symbol("define".to_string()),
                                      Value::Symbol("x".to_string()),
                                      Value::Integer(2)]),
                 Value::from_vec(vec![Value::Symbol("set!".to_string()),
                                      Value::Symbol("x".to_string()),
                                      Value::Integer(3)]),
                 Value::from_vec(vec![Value::Symbol("+".to_string()),
                                      Value::Symbol("x".to_string()),
                                      Value::Symbol("x".to_string())])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(6));
}

#[test]
fn test_lambda() {
    // runTest ((lambda (x) (+ x 2)) 3) => 5
    let i = vec![Value::from_vec(vec![Value::from_vec(vec![Value::Symbol("lambda".to_string()),
                                                           Value::from_vec(vec![Value::Symbol("x".to_string())]),
                                                           Value::from_vec(vec![Value::Symbol("+".to_string()),
                                                                                Value::Symbol("x".to_string()),
                                                                                Value::Integer(2)])]),
                                      Value::Integer(3)])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(5));
}

#[test]
fn test_lambda_symbol() {
    // runTest ((λ (x) (+ x 2)) 3) => 5
    let i = vec![Value::from_vec(vec![Value::from_vec(vec![Value::Symbol("λ".to_string()),
                                                           Value::from_vec(vec![Value::Symbol("x".to_string())]),
                                                           Value::from_vec(vec![Value::Symbol("+".to_string()),
                                                                                Value::Symbol("x".to_string()),
                                                                                Value::Integer(2)])]),
                                      Value::Integer(3)])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(5));
}

#[test]
fn test_define_func() {
    // runTest (define (f x) (+ x 2)) (f 3) => 5
    let i = vec![Value::from_vec(vec![Value::Symbol("define".to_string()),
                                      Value::from_vec(vec![Value::Symbol("f".to_string()),
                                                           Value::Symbol("x".to_string())]),
                                      Value::from_vec(vec![Value::Symbol("+".to_string()),
                                                           Value::Symbol("x".to_string()),
                                                           Value::Integer(2)])]),
                 Value::from_vec(vec![Value::Symbol("f".to_string()),
                                      Value::Integer(3)])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(5));
}

#[test]
fn test_define_func2() {
    // runTest (define (noop) (+ 0 0)) (define (f x) (noop) (+ x 2)) ((lambda () (f 3))) => 5
    let i = vec![Value::from_vec(vec![Value::Symbol("define".to_string()),
                                      Value::from_vec(vec![Value::Symbol("noop".to_string())]),
                                      Value::from_vec(vec![Value::Symbol("+".to_string()),
                                                           Value::Integer(0),
                                                           Value::Integer(0)])]),
                 Value::from_vec(vec![Value::Symbol("define".to_string()),
                                      Value::from_vec(vec![Value::Symbol("f".to_string()),
                                                           Value::Symbol("x".to_string())]),
                                      Value::from_vec(vec![Value::Symbol("noop".to_string())]),
                                      Value::from_vec(vec![Value::Symbol("+".to_string()),
                                                           Value::Symbol("x".to_string()),
                                                           Value::Integer(2)])]),
                 Value::from_vec(vec![Value::from_vec(vec![Value::Symbol("lambda".to_string()),
                                                           null!(),
                                                           Value::from_vec(vec![Value::Symbol("f".to_string()),
                                                                                Value::Integer(3)])])])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(5));
}

#[test]
fn test_native_fn_as_value() {
    // runTest + => #<procedure:+>
    let i = vec![Value::Symbol("+".to_string())];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Procedure(Function::Native("+")));
}

#[test]
fn test_dynamic_native_fn() {
    // runTest ((if (> 3 2) + -) 4 3) => 7
    let i = vec![Value::from_vec(vec![Value::from_vec(vec![Value::Symbol("if".to_string()),
                                                           Value::from_vec(vec![Value::Symbol(">".to_string()),
                                                                                Value::Integer(3),
                                                                                Value::Integer(2)]),
                                                           Value::Symbol("+".to_string()),
                                                           Value::Symbol("-".to_string())]),
                                      Value::Integer(4),
                                      Value::Integer(3)])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(7));
}

#[test]
fn test_let_bindings() {
    // runTest (let ((x 3)) (+ x 1)) => 4
    let i = vec![Value::from_vec(vec![Value::Symbol("let".to_string()),
                                      Value::from_vec(vec![Value::from_vec(vec![Value::Symbol("x".to_string()),
                                                                                Value::Integer(3)])]),
                                      Value::from_vec(vec![Value::Symbol("+".to_string()),
                                                           Value::Symbol("x".to_string()),
                                                           Value::Integer(1)])])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(4));
}

#[test]
fn test_quoting() {
    // runTest (quote (1 2)) => (1 2)
    let i = vec![Value::from_vec(vec![Value::Symbol("quote".to_string()),
                                      Value::from_vec(vec![Value::Integer(1),
                                                           Value::Integer(2)])])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::from_vec(vec![Value::Integer(1),
                                    Value::Integer(2)]));
}

#[test]
fn test_quasiquoting() {
    // runTest (quasiquote (2 (unquote (+ 1 2)) 4)) => (2 3 4)
    let i = vec![Value::from_vec(vec![Value::Symbol("quasiquote".to_string()),
                                      Value::from_vec(vec![Value::Integer(2),
                                                           Value::from_vec(vec![Value::Symbol("unquote".to_string()),
                                                                                Value::from_vec(vec![Value::Symbol("+".to_string()),
                                                                                                     Value::Integer(1),
                                                                                                     Value::Integer(2)])]),
                                                           Value::Integer(4)])])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::from_vec(vec![Value::Integer(2),
                                    Value::Integer(3),
                                    Value::Integer(4)]));
}

#[test]
fn test_eval() {
    // runTest (eval (quote (+ 1 2))) => 3
    let i = vec![Value::from_vec(vec![Value::Symbol("eval".to_string()),
                                      Value::from_vec(vec![Value::Symbol("quote".to_string()),
                                                           Value::from_vec(vec![Value::Symbol("+".to_string()),
                                                                                Value::Integer(1),
                                                                                Value::Integer(2)])])])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(3));
}

#[test]
fn test_eval2() {
    // runTest (define (foo x) (eval (quote (+ 1 2))) x) (foo 5) => 5
    let i = vec![Value::from_vec(vec![Value::Symbol("define".to_string()),
                                      Value::from_vec(vec![Value::Symbol("foo".to_string()),
                                                           Value::Symbol("x".to_string())]),
                                      Value::from_vec(vec![Value::Symbol("eval".to_string()),
                                                           Value::from_vec(vec![Value::Symbol("quote".to_string()),
                                                                                Value::from_vec(vec![Value::Symbol("+".to_string()),
                                                                                                     Value::Integer(1),
                                                                                                     Value::Integer(2)])])]),
                                      Value::Symbol("x".to_string())]),
                 Value::from_vec(vec![Value::Symbol("foo".to_string()),
                                      Value::Integer(5)])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(5));
}

#[test]
fn test_apply() {
    // runTest (apply + (quote (1 2 3))) => 6
    let i = vec![Value::from_vec(vec![Value::Symbol("apply".to_string()),
                                      Value::Symbol("+".to_string()),
                                      Value::from_vec(vec![Value::Symbol("quote".to_string()),
                                                           Value::from_vec(vec![Value::Integer(1),
                                                                                Value::Integer(2),
                                                                                Value::Integer(3)])])])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(6));
}

#[test]
fn test_begin() {
    // runTest (define x 1) (begin (set! x 5) (set! x (+ x 2)) x) => 7
    let i = vec![Value::from_vec(vec![Value::Symbol("define".to_string()),
                                      Value::Symbol("x".to_string()),
                                      Value::Integer(1)]),
                 Value::from_vec(vec![Value::Symbol("begin".to_string()),
                                      Value::from_vec(vec![Value::Symbol("set!".to_string()),
                                                           Value::Symbol("x".to_string()),
                                                           Value::Integer(5)]),
                                      Value::from_vec(vec![Value::Symbol("set!".to_string()),
                                                           Value::Symbol("x".to_string()),
                                                           Value::from_vec(vec![Value::Symbol("+".to_string()),
                                                                                Value::Symbol("x".to_string()),
                                                                                Value::Integer(2)])]),
                                      Value::Symbol("x".to_string())])];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(7));
}

#[test]
fn test_callcc() {
    // runTest
    //   (define x 0)
    //   (define (+x n) (set! x (+ x n)))
    //   (define (foo k) (+x 2) (k) (+x 4))
    //   ((lambda ()
    //      (+x 1)
    //      (call/cc foo)
    //      (+x 8)))
    //   x
    // => 11
    let i = vec![Value::from_vec(vec![Value::Symbol("define".to_string()),
                                      Value::Symbol("x".to_string()),
                                      Value::Integer(0)]),
                 Value::from_vec(vec![Value::Symbol("define".to_string()),
                                      Value::from_vec(vec![Value::Symbol("+x".to_string()),
                                                           Value::Symbol("n".to_string())]),
                                      Value::from_vec(vec![Value::Symbol("set!".to_string()),
                                                           Value::Symbol("x".to_string()),
                                                           Value::from_vec(vec![Value::Symbol("+".to_string()),
                                                                                Value::Symbol("x".to_string()),
                                                                                Value::Symbol("n".to_string())])])]),
                 Value::from_vec(vec![Value::Symbol("define".to_string()),
                                      Value::from_vec(vec![Value::Symbol("foo".to_string()),
                                                           Value::Symbol("k".to_string())]),
                                      Value::from_vec(vec![Value::Symbol("+x".to_string()),
                                                           Value::Integer(2)]),
                                      Value::from_vec(vec![Value::Symbol("k".to_string())]),
                                      Value::from_vec(vec![Value::Symbol("+x".to_string()),
                                                           Value::Integer(4)])]),
                 Value::from_vec(vec![Value::from_vec(vec![Value::Symbol("lambda".to_string()),
                                                           null!(),
                                                           Value::from_vec(vec![Value::Symbol("+x".to_string()),
                                                                                Value::Integer(1)]),
                                                           Value::from_vec(vec![Value::Symbol("call/cc".to_string()),
                                                                                Value::Symbol("foo".to_string())]),
                                                           Value::from_vec(vec![Value::Symbol("+x".to_string()),
                                                                                Value::Integer(8)])])]),
                 Value::Symbol("x".to_string())];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(11));
}

#[test]
fn test_macros() {
    // runTest (define-syntax-rule (incr x) (set! x (+ x 1))) (define a 1) (incr a) a => 2
    let i = vec![Value::from_vec(vec![Value::Symbol("define-syntax-rule".to_string()),
                                      Value::from_vec(vec![Value::Symbol("incr".to_string()),
                                                           Value::Symbol("x".to_string())]),
                                      Value::from_vec(vec![Value::Symbol("set!".to_string()),
                                                           Value::Symbol("x".to_string()),
                                                           Value::from_vec(vec![Value::Symbol("+".to_string()),
                                                                                Value::Symbol("x".to_string()),
                                                                                Value::Integer(1)])])]),
                 Value::from_vec(vec![Value::Symbol("define".to_string()),
                                      Value::Symbol("a".to_string()),
                                      Value::Integer(1)]),
                 Value::from_vec(vec![Value::Symbol("incr".to_string()),
                                      Value::Symbol("a".to_string())]),
                 Value::Symbol("a".to_string())];
    assert_eq!(exec(List::from_vec(i)).unwrap(),
               Value::Integer(2));
}

#[test]
fn test_list_iter() {
    let l = List::Cell(
        Box::new(Value::Integer(1)),
        Box::new(List::Cell(
            Box::new(Value::Integer(2)),
            Box::new(List::Cell(
                Box::new(Value::Integer(3)),
                Box::new(List::Null))))));
    let mut x = 0;
    for i in l {
        x += 1;
        assert_eq!(i, Value::Integer(x));
    }
    assert_eq!(x, 3);
}

#[test]
fn test_list_to_string() {
    let l = List::Cell(
        Box::new(Value::Integer(1)),
        Box::new(List::Cell(
            Box::new(Value::Integer(2)),
            Box::new(List::Cell(
                Box::new(Value::Integer(3)),
                Box::new(List::Null))))));
    assert_eq!(l.to_string(), "(1 2 3)");
}
