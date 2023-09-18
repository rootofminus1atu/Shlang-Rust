use crate::ast_nodes;
use crate::lang_errors::*;
use crate::spans::*;
use ast_nodes::*;
use std::collections::HashMap;
use std::*;
type IError = InterpreterError;
#[derive(Debug)]
pub struct Interpreter {
    program: Vec<Spanned<Expr>>,
    heap: HashMap<u32, Value>,
}
impl Interpreter {
    pub fn new(program: Vec<Spanned<Expr>>) -> Self {
        Self {
            program,
            heap: HashMap::from([]),
        }
    }
}
