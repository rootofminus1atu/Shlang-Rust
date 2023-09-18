use std::collections::*;

use crate::spans::*;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct AST {
    pub vardefs: HashMap<String, Expr>,
    pub funcdefs: HashMap<String, Function>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Null,
    Void,
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Function(Function),
    BuiltinFunc(BuiltinFunc),
}

impl Value {
    pub fn to_nodespan(self, span: Span) -> Spanned<Expr> {
        Spanned::new(Expr::Value(Box::new(self)), span)
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub block: Vec<Spanned<Expr>>,
    pub args: Vec<(String, Type)>,
    pub ret_type: Type,
}
#[derive(Clone, Debug, PartialEq)]
pub struct BuiltinFunc {
    pub function: fn((Value, Type), Vec<Value>) -> Value,
    pub inf: bool,
    pub ptypes: Vec<Type>,
    pub ret_type: Type,
}
#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Null,
    Float,
    Int,
    Bool,
    Str,
    Function,
    Any,
}
#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Statemnt(Statemnt),
    Expr(Expr),
}
#[derive(Clone, Debug, PartialEq)]
pub enum Statemnt {
    VarDef(VarDef),
    VarAssignment(VarAssign),
    ExprStatemnt(Spanned<Expr>),
    ReturnNode(Box<Spanned<Expr>>),
    BreakNode,
    ContinueNode,
}
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Value(Box<Value>),
    Block(Body),
    BinaryNode(BinaryNode),
    UnaryNode(Box<Spanned<Expr>>, UnaryOp),
    Variable(String),
    Call(Call),
    Branch(Branch),
    Loop(Vec<Spanned<Node>>),
    While(Box<Spanned<Expr>>, Vec<Spanned<Node>>),
    DoBlock(Vec<Spanned<Node>>),
}
impl From<Value> for Expr {
    fn from(x: Value) -> Self {
        Expr::Value(Box::new(x))
    }
}
pub type Body = Vec<Spanned<Expr>>;
pub type NodeRef = Box<Spanned<Expr>>;
// macro_rules! nodes_from {
//     ($($name:ident)*) => {
//         $(
//             impl ::core::convert::From<$name> for Node {
//                 fn from(node: $name) -> Self {
//                     Self::$name(node)
//                 }
//             }
//             impl $name {
//                 fn to_nodespan(self,span:Span) -> Spanned<Node> {
//                     Spanned::new(Node::$name(self.clone()),span)
//                 }

//             }
//         )*
//     }
// }
// nodes_from! { UnaryNode BinaryNode Call Assignment Declaration Branch}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Divide,
    Multiply,
    Modulo,
    And,
    Or,
    IsEqual,
    IsDiferent,
    Greater,
    Lesser,
    GreaterOrEqual,
    LesserOrEqual,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinaryNode {
    pub kind: BinaryOp,
    pub left: Box<Spanned<Expr>>,
    pub right: Box<Spanned<Expr>>,
}
#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    NEGATIVE,
    NOT,
}
#[derive(Clone, Debug, PartialEq)]
pub struct VarDef {
    pub var_name: String,
    pub var_type: Type,
    pub value: Box<Spanned<Expr>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarAssign {
    pub target: Box<Spanned<Expr>>,
    pub value: Box<Spanned<Expr>>,
}
#[derive(Clone, Debug, PartialEq)]
pub struct Call {
    pub callee: Box<Spanned<Expr>>,
    pub args: Vec<Spanned<Expr>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Branch {
    pub condition: Box<Spanned<Expr>>,
    pub if_block: Vec<Spanned<Node>>,
    pub else_block: Option<Vec<Spanned<Node>>>,
}
