use ::core::errors::*;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;
use std::iter;
use std::vec;


struct Nil;

#[macro_export]
macro_rules! null { () => (List::Null.to_value()) }


#[derive(PartialEq, Clone)]
pub enum Value {
    Symbol(String),
    Integer(i64),
    Boolean(bool),
    String(String),
    List(List),
    Procedure(Function),
    SpecialForm(SpecialForm),
    Macro(Vec<String>, Box<Value>),
    Continuation(Box<Continuation>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Node {
    Identifier(String),
    Integer(i64),
    Boolean(bool),
    String(String),
    List(Vec<Node>),
}

impl Value {
    pub fn from_vec(vec: Vec<Value>) -> Value {
        List::from_vec(vec).to_value()
    }

    pub fn from_node(node: &Node) -> Value {
        match *node {
            Node::Identifier(ref val) => Value::Symbol(val.clone()),
            Node::Integer(val) => Value::Integer(val),
            Node::Boolean(val) => Value::Boolean(val),
            Node::String(ref val) => Value::String(val.clone()),
            Node::List(ref nodes) => Value::List(List::from_nodes(&nodes))
        }
    }

    pub fn as_symbol(self) -> Result<String, RuntimeError> {
        match self {
            Value::Symbol(s) => Ok(s),
            _ => runtime_error!("Expected a symbol value: {:?}", self)
        }
    }

    pub fn as_integer(self) -> Result<i64, RuntimeError> {
        match self {
            Value::Integer(i) => Ok(i),
            _ => runtime_error!("Expected an integer value: {:?}", self)
        }
    }

    // fn as_boolean(self) -> Result<bool, RuntimeError> {
    //     match self {
    //         Value::Boolean(b) => Ok(b),
    //         _ => runtime_error!("Expected a boolean value: {:?}", self)
    //     }
    // }

    // fn as_string(self) -> Result<String, RuntimeError> {
    //     match self {
    //         Value::String(s) => Ok(s),
    //         _ => runtime_error!("Expected a string value: {:?}", self)
    //     }
    // }

    pub fn as_list(self) -> Result<List, RuntimeError> {
        match self {
            Value::List(l) => Ok(l),
            _ => runtime_error!("Expected a list value: {:?}", self)
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Symbol(ref val) => write!(f, "{}", val),
            Value::Integer(val)    => write!(f, "{}", val),
            Value::Boolean(val)    => write!(f, "#{}", if val { "t" } else { "f" }),
            Value::String(ref val) => write!(f, "{}", val),
            Value::List(ref list)  => write!(f, "{}", list),
            Value::Procedure(_)    => write!(f, "#<procedure>"),
            Value::SpecialForm(_)  => write!(f, "#<special_form>"),
            Value::Continuation(_) => write!(f, "#<continuation>"),
            Value::Macro(_,_)      => write!(f, "#<macro>"),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::String(ref val) => write!(f, "\"{}\"", val),
            Value::List(ref list)  => write!(f, "{:?}", list),
            _                      => write!(f, "{}", self)
        }
    }
}



#[derive(PartialEq, Clone)]
pub enum List {
    Cell(Box<Value>, Box<List>),
    Null
}

impl List {
    pub fn from_vec(mut vec: Vec<Value>) -> List {
        if vec.len() > 0 {
            let mut out = List::Null;
            while vec.len() > 0 {
                let v = vec.pop().unwrap();
                out = List::Cell(Box::new(v), Box::new(out));
            }
            out
        } else {
            List::Null
        }
    }

    pub fn from_nodes(nodes: &[Node]) -> List {
        let vec = nodes.iter().map(Value::from_node).collect();
        List::from_vec(vec)
    }

    pub fn is_empty(&self) -> bool {
        self == &List::Null
    }

    pub fn shift(self) -> Option<(Value, List)> {
        match self {
            List::Cell(car, cdr) => Some((*car, *cdr)),
            List::Null => None
        }
    }

    pub fn unshift(self, car: Value) -> List {
        List::Cell(Box::new(car), Box::new(self))
    }

    pub fn len(&self) -> usize {
        match self {
            &List::Cell(_, ref cdr) => 1 + cdr.len(),
            &List::Null => 0
        }
    }

    pub fn unpack1(self) -> Result<Value, RuntimeError> {
        let (car, cdr) = shift_or_error!(self, "Expected list of length 1, but was empty");
        if !cdr.is_empty() { runtime_error!("Expected list of length 1, but it had more elements") }
        Ok(car)
    }

    pub fn unpack2(self) -> Result<(Value, Value), RuntimeError> {
        let (car, cdr) = shift_or_error!(self, "Expected list of length 2, but was empty");
        let (cadr, cddr) = shift_or_error!(cdr, "Expected list of length 2, but was length 1");
        if !cddr.is_empty() { runtime_error!("Expected list of length 2, but it had more elements") }
        Ok((car, cadr))
    }

    pub fn unpack3(self) -> Result<(Value, Value, Value), RuntimeError> {
        let (car, cdr) = shift_or_error!(self, "Expected list of length 3, but was empty");
        let (cadr, cddr) = shift_or_error!(cdr, "Expected list of length 3, but was length 1");
        let (caddr, cdddr) = shift_or_error!(cddr, "Expected list of length 3, but was length 2");
        if !cdddr.is_empty() { runtime_error!("Expected list of length 3, but it had more elements") }
        Ok((car, cadr, caddr))
    }

    pub fn reverse(self) -> List {
        let mut out = List::Null;
        for v in self {
            out = out.unshift(v)
        }
        out
    }

    pub fn to_value(self) -> Value {
        Value::List(self)
    }

    pub fn to_vec(self) -> Vec<Value> {
        let mut out = vec![];
        let mut l = self;
        loop {
            match l.shift() {
                Some((car, cdr)) => {
                    out.push(car);
                    l = cdr;
                },
                None => break
            }
        }
        out
    }
}

impl iter::IntoIterator for List {
    type Item = Value;
    type IntoIter = vec::IntoIter<Value>;

    fn into_iter(self) -> Self::IntoIter {
        self.to_vec().into_iter()
    }
}

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let strs: Vec<String> = self.clone().into_iter().map(|v| format!("{}", v)).collect();
        write!(f, "({})", &strs.join(" "))
    }
}

impl fmt::Debug for List {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let strs: Vec<String> = self.clone().into_iter().map(|v| format!("{:?}", v)).collect();
        write!(f, "({})", &strs.join(" "))
    }
}

#[derive(Clone, PartialEq)]
pub enum Function {
    Scheme(Vec<String>, List, Rc<RefCell<Environment>>),
    Native(&'static str),
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Function::Scheme(_, _, _) => write!(f, "#<procedure>"),
            Function::Native(ref s) => write!(f, "#<procedure:{}>", s),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum SpecialForm {
    If,
    Define,
    Set,
    Lambda,
    Let,
    Quote,
    Quasiquote,
    Eval,
    Apply,
    Begin,
    And,
    Or,
    CallCC,
    DefineSyntaxRule,
}

pub enum Trampoline {
    Bounce(Value, Rc<RefCell<Environment>>, Continuation),
    QuasiBounce(Value, Rc<RefCell<Environment>>, Continuation),
    Run(Value, Continuation),
    Land(Value),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Continuation {
    EvaluateExpressions(List, Rc<RefCell<Environment>>, Box<Continuation>),
    BeginFunc(List, Rc<RefCell<Environment>>, Box<Continuation>),
    EvaluateIf(Value, Value, Rc<RefCell<Environment>>, Box<Continuation>),
    EvaluateDefine(String, Rc<RefCell<Environment>>, Box<Continuation>),
    EvaluateSet(String, Rc<RefCell<Environment>>, Box<Continuation>),
    EvaluateFunc(Value, List, List, Rc<RefCell<Environment>>, Box<Continuation>),
    EvaluateLet(String, List, List, Rc<RefCell<Environment>>, Box<Continuation>),
    ContinueQuasiquoting(List, List, Rc<RefCell<Environment>>, Box<Continuation>),
    ExecuteEval(Rc<RefCell<Environment>>, Box<Continuation>),
    EvaluateApplyArgs(Value, Rc<RefCell<Environment>>, Box<Continuation>),
    ExecuteApply(Value, Box<Continuation>),
    EvaluateAnd(List, Rc<RefCell<Environment>>, Box<Continuation>),
    EvaluateOr(List, Rc<RefCell<Environment>>, Box<Continuation>),
    ExecuteCallCC(Box<Continuation>),
    Return,
}


#[derive(PartialEq)]
pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Value>,
}

impl fmt::Debug for Environment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.parent {
            Some(ref parent) => write!(f, "{:?}, {:?}", self.values, parent.borrow()),
            None => write!(f, "{:?} ", self.values)
        }
    }
}

impl Environment {
    pub fn new_root() -> Result<Rc<RefCell<Environment>>, RuntimeError> {
        let mut env = Environment { parent: None, values: HashMap::new() };
        try!(env.define("+".to_string(), Value::Procedure(Function::Native("+"))));
        try!(env.define("-".to_string(), Value::Procedure(Function::Native("-"))));
        try!(env.define("*".to_string(), Value::Procedure(Function::Native("*"))));
        try!(env.define("/".to_string(), Value::Procedure(Function::Native("/"))));
        try!(env.define("<".to_string(), Value::Procedure(Function::Native("<"))));
        try!(env.define(">".to_string(), Value::Procedure(Function::Native(">"))));
        try!(env.define("=".to_string(), Value::Procedure(Function::Native("="))));
        try!(env.define("null?".to_string(), Value::Procedure(Function::Native("null?"))));
        try!(env.define("list".to_string(), Value::Procedure(Function::Native("list"))));
        try!(env.define("car".to_string(), Value::Procedure(Function::Native("car"))));
        try!(env.define("cdr".to_string(), Value::Procedure(Function::Native("cdr"))));
        try!(env.define("cons".to_string(), Value::Procedure(Function::Native("cons"))));
        try!(env.define("append".to_string(), Value::Procedure(Function::Native("append"))));
        try!(env.define("error".to_string(), Value::Procedure(Function::Native("error"))));
        try!(env.define("write".to_string(), Value::Procedure(Function::Native("write"))));
        try!(env.define("display".to_string(), Value::Procedure(Function::Native("display"))));
        try!(env.define("displayln".to_string(), Value::Procedure(Function::Native("displayln"))));
        try!(env.define("print".to_string(), Value::Procedure(Function::Native("print"))));
        try!(env.define("newline".to_string(), Value::Procedure(Function::Native("newline"))));
        Ok(Rc::new(RefCell::new(env)))
    }

    pub fn new_child(parent: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        let env = Environment { parent: Some(parent), values: HashMap::new() };
        Rc::new(RefCell::new(env))
    }

    // Define a variable at the current level
    // If key is not defined in the current env, set it
    // If key is already defined in the current env, return runtime error
    // (So if key is defined at a higher level, still define it at the current level)
    pub fn define(&mut self, key: String, value: Value) -> Result<(), RuntimeError> {
        if self.values.contains_key(&key) {
            runtime_error!("Duplicate define: {:?}", key)
        } else {
            self.values.insert(key, value);
            Ok(())
        }
    }

    // Set a variable to a value, at any level in the env, or throw a runtime error if it isn't defined at all
    pub fn set(&mut self, key: String, value: Value) -> Result<(), RuntimeError>  {
        if self.values.contains_key(&key) {
            self.values.insert(key, value);
            Ok(())
        } else {
            // Recurse up the environment tree until a value is found or the end is reached
            match self.parent {
                Some(ref parent) => parent.borrow_mut().set(key, value),
                None => runtime_error!("Can't set! an undefined variable: {:?}", key)
            }
        }
    }

    pub fn get(&self, key: &String) -> Option<Value> {
        match self.values.get(key) {
            Some(val) => Some(val.clone()),
            None => {
                // Recurse up the environment tree until a value is found or the end is reached
                match self.parent {
                    Some(ref parent) => parent.borrow().get(key),
                    None => None
                }
            }
        }
    }

    pub fn get_root(env_ref: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        let env = env_ref.borrow();
        match env.parent {
            Some(ref parent) => Environment::get_root(parent.clone()),
            None => env_ref.clone()
        }
    }
}

