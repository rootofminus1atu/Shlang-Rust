use std::collections::*;

use crate::spans::*;

pub struct AST {
    functions: HashMap<String, Function>,
    variables: HashMap<String, Value>,
    structs: HashMap<String, Struct>,
}
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Null,
    Void,
    Num(f64),
    Bool(bool),
    Str(String),
    Function(Function),
    BuiltinFunc(BuiltinFunc),
    Struct(Struct),
    StructRef(u32),
}
pub type TypedValue = (Value, Type);
pub type ValueStream = Vec<Value>;
impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            Value::Bool(_) => Type::Bool,
            Value::BuiltinFunc(_) | Value::Function(_) => Type::Function,
            Value::Null => Type::Null,
            Value::Void => Type::Void,
            Value::Str(_) => Type::Str,
            Value::Num(_) => Type::Num,

            Value::Struct(s) => Type::UserDefined(s.id.clone()),
            Value::StructRef(id) => Type::Ref(*id),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    pub id: String,
    pub env: Scope,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub block: Block,
    pub args: Vec<String>,
}
impl Function {
    pub fn new(block: Block, args: Vec<String>) -> Self {
        Self { block, args }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BuiltinFunc {
    pub function: fn(HashMap<String, Value>, Vec<Value>) -> Value,
    pub arg_size: i16,
}
impl From<Function> for Value {
    fn from(x: Function) -> Self {
        Value::Function(x)
    }
}
#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Null,
    Void,
    Num,
    Bool,
    Str,
    Function,

    Ref(u32),
    UserDefined(String),
}
impl Type {
    pub fn is_void(&self) -> bool {
        self == &Self::Void
    }
    pub fn is_numeric(&self) -> bool {
        matches!(self, Self::Bool | Self::Num)
    }
}
pub type Block = Vec<Spanned<Expr>>;
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Statement(Statement),
    Value(Value),
    BinaryNode(BinaryOp, Spanned<Box<Expr>>, Spanned<Box<Expr>>),
    UnaryNode(UnaryOp, Spanned<Box<Expr>>),
    Variable(String),
    Call(Spanned<Box<Expr>>, Vec<Spanned<Expr>>),
    Branch(Spanned<Box<Expr>>, Block, Option<Block>),
    Loop(Block),
    While(Spanned<Box<Expr>>, Block),
    DoBlock(Block),
    Constructor(String, HashMap<String, Spanned<Expr>>),
    StructDef(String, Vec<Spanned<Field>>),
    FieldAccess {
        target: Spanned<Box<Expr>>,
        requested: Spanned<Box<Expr>>,
    },
}
impl Expr {
    pub fn to_spanned(self, span: Span) -> Spanned<Self> {
        Spanned::new(self, span)
    }
}
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Return(Spanned<Box<Expr>>),
    Break,
    Continue,
    Declaration(String, Spanned<Box<Expr>>),
    Assignment(Spanned<Box<Expr>>, Spanned<Box<Expr>>),
}
impl Statement {
    pub fn to_exprspan(self, span: Span) -> Spanned<Expr> {
        Spanned::new(Expr::Statement(self), span)
    }
}
impl From<Statement> for Expr {
    fn from(value: Statement) -> Self {
        return Expr::Statement(value);
    }
}
#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    ADD,
    SUBTRACT,
    DIVIDE,
    MULTIPLY,
    MODULO,
    AND,
    OR,
    ISEQUAL,
    ISDIFERENT,
    GREATER,
    LESSER,
    GREATER_EQUAL,
    LESSER_EQUAL,
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    NEGATIVE,
    NOT,
}
#[derive(Clone, Debug, PartialEq)]
pub enum Field {
    Declaration(String, Spanned<Expr>),
    StructDef(String, Vec<Spanned<Field>>),
}

pub type VarMap = HashMap<String, Value>;
#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    pub parent: Option<Box<Scope>>,
    pub vars: VarMap,
    pub structs: HashMap<String, Struct>,
}

impl Scope {
    pub fn get_var(&self, var_name: &String) -> Option<Value> {
        if let Some(var) = self.vars.get(var_name) {
            return Some(var.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.get_var(var_name);
        }
        None
    }
    pub fn get_struct(&self, struct_name: &String) -> Option<Struct> {
        if let Some(obj) = self.structs.get(struct_name) {
            return Some(obj.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.get_struct(struct_name);
        }
        None
    }
    pub fn define(&mut self, var_name: String, val: Value) {
        if let Value::Struct(obj) = &val {
            self.structs.insert(var_name.clone(), obj.clone());
        }
        self.vars.insert(var_name, val);
    }
    pub fn new(parent: Option<Box<Scope>>, vars: VarMap, structs: HashMap<String, Struct>) -> Self {
        Scope {
            parent,
            vars,
            structs,
        }
    }
    pub fn new_child_in(parent: Scope) -> Self {
        Scope {
            parent: Some(Box::new(parent)),
            vars: HashMap::from([]),
            structs: HashMap::from([]),
        }
    }
    pub fn assign(&mut self, var_name: String, value: Value) -> Option<Value> {
        if let Some(var) = self.vars.get_mut(&var_name) {
            *var = value;
            return Some(var.clone());
        }
        if let Some(parent) = &mut self.parent {
            return parent.assign(var_name, value);
        }
        None
    }
}
