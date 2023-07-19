use crate::spans::*;
use std::collections::*;
#[derive(Clone, Debug, PartialEq)]

pub enum Control {
    Return(Box<Value>, Type),
    Result(Box<Value>, Type),
    Break,
}
impl From<Control> for Value {
    fn from(x: Control) -> Self {
        Value::Control(x)
    }
}
pub trait Conv{
    fn to_node(&self, span: Span)->Node;
}
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Null,
    Void,
    Control(Control),
    Num(f64),
    Bool(bool),
    Str(String),
    Function(Function),
    BuiltinFunc(BuiltinFunc),
}
impl Value {
    pub fn get_type(&self) -> Type {
        match *self {
            Value::Bool(_) => return Type::Bool,
            Value::BuiltinFunc(_) | Value::Function(_) => return Type::Function,
            Value::Null => return Type::Null,
            Value::Void => return Type::Void,
            Value::Str(_) => return Type::Str,
            Value::Num(_) => return Type::Num,
            Value::Control(_) => return Type::Never,
        }
    }
}
impl Conv for Value {
    fn to_node(&self, span: Span)->Node {
        Node::value(self.clone(), span)
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
    Never,
}
impl Type {
    pub fn is_void(&self) -> bool {
        self == &Self::Void
    }
    pub fn is_numeric(&self) -> bool {
        match self {
            Self::Bool | Self::Num => return true,
            _ => return false,
        }
    }
}
pub type TypedValue = (Value, Type);
#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    block: Box<Block>,
    args: Vec<String>,
}
type ValueStream = Vec<Value>;
#[derive(Clone, Debug, PartialEq)]
pub struct BuiltinFunc {
    function: fn(ValueStream) -> Value,
    arg_size: i16,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Value(Spanned<Box<Value>>),
    Block(Spanned<Block>),
    DoBlock(Spanned<DoBlock>),
    BinaryNode(Spanned<BinaryNode>),
    UnaryNode(Spanned<UnaryNode>),
    ResultNode(Spanned<Box<Node>>),
    ReturnNode(Spanned<Box<Node>>),
    BreakNode(Span),
    Declaration(Spanned<Declaration>),
    Assignment(Spanned<Assignment>),
    Variable(Spanned<Variable>),
    Call(Spanned<Call>),
    Branch(Spanned<Branch>),
    Loop(Spanned<Loop>),
    While(Spanned<While>),
}
 
impl Node {
    pub fn block(body: NodeStream, span: Span) -> Self {
        return Node::Block(Spanned {
            unspanned: Block {
                body: Box::new(body),
            },
            span,
        });
    }
    pub fn value(value: Value, span: Span) -> Self {
        Node::Value(Spanned {
            unspanned: Box::new(value),
            span,
        })
    }
}

macro_rules! nodes_from {
    ($($name:ident)*) => {
        $(
            impl Conv for $name {
                fn to_node(&self,span:Span) -> Node {
                    Node::$name(Spanned {
                        unspanned: self.clone(),
                        span
                    })
                }
            }
        )*
    }
}
nodes_from! { UnaryNode DoBlock BinaryNode Call Variable Assignment Declaration Branch While Loop Block}
pub type NodeStream = Vec<Node>;
pub type NodeRef = Box<Node>;

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub body: Box<NodeStream>,
}
pub type BlockRef = Box<Block>;
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
pub struct BinaryNode {
    pub kind: BinaryOp,
    pub left: NodeRef,
    pub right: NodeRef,
}
#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    NEGATIVE,
    NOT,
}
#[derive(Clone, Debug, PartialEq)]
pub struct UnaryNode {
    pub kind: UnaryOp,
    pub object: NodeRef,
}
#[derive(Clone, Debug, PartialEq)]
pub struct Declaration {
    pub var_name: String,
    pub value: NodeRef,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub var_name: String,
    pub value: NodeRef,
}
#[derive(Clone, Debug, PartialEq)]
pub struct Variable {
    pub name: String,
}
#[derive(Clone, Debug, PartialEq)]
pub struct Call {
    pub callee: NodeRef,
    pub args: Box<NodeStream>,
}
#[derive(Clone, Debug, PartialEq)]
pub struct DoBlock {
    pub body: Box<NodeStream>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Branch {
    pub condition: NodeRef,
    pub if_block: BlockRef,
    pub else_block: Option<BlockRef>,
}
#[derive(Clone, Debug, PartialEq)]
pub struct Loop {
    pub proc: BlockRef,
}
#[derive(Clone, Debug, PartialEq)]
pub struct While {
    pub condition: NodeRef,
    pub proc: BlockRef,
}
#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    pub parent: Option<Box<Scope>>,
    pub var_map: HashMap<String, Value>,
}

impl Scope {
    pub fn get_var(&self, var_name: String) -> Option<Value> {
        if let Some(var) = self.var_map.get(&var_name) {
            return Some(var.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.get_var(var_name);
        }

        None
    }
    pub fn define(&mut self, var_name: String, val: Value) {
        dbg!(self.var_map.insert(var_name, val));
        dbg!(&self.var_map);
    }
    pub fn new(parent: Option<Box<Scope>>, var_map: HashMap<String, Value>) -> Self {
        Scope { parent, var_map }
    }
    pub fn travel(&mut self) {
        let Some(parent) = self.parent.clone() else {panic!()};
        let grandpa = parent.clone().parent;
        self.parent = grandpa;
        self.var_map = parent.var_map;
    }
    pub fn assign(&mut self, var_name: String, value: Value) -> Option<Value> {
        if let Some(var) = self.var_map.get_mut(&var_name) {
            *var = value;
            return Some(var.clone());
        }
        if let Some(parent) = &mut self.parent {
            return parent.assign(var_name, value);
        }
        None
    }
}
