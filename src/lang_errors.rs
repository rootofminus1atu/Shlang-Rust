use crate::ast_nodes::*;
use crate::{spans::*, tokens::*};
use colored::*;
#[derive(Clone)]

pub struct ErrorBuilder {
    pub input: String,
}
impl ErrorBuilder {
    pub fn new(input: String) -> Self {
        Self { input }
    }
    pub fn emit(&self, msg: &str, span: Span) {
        eprintln!("{}", self.build(msg, span));
    }
    pub fn panic_emit(&self, msg: &str, span: Span) {
        self.emit(msg, span);
        panic!()
    }
    pub fn build(&self, msg: &str, span: Span) -> String {
        let position = self.line_pos(span);
        let (start, stop) = (
            self.input[..span.0].to_string(),
            self.input[span.1..].to_string(),
        );
        let marked = format!(
            "{start}{}{stop}",
            self.input[span.0..span.1].to_string().red()
        );
        let lines: Vec<&str> = marked.lines().collect();
        let line = lines[position - 1];
        format!("{} {msg}\n{position} {} {line}", "ERROR!".red(), "|".blue(),)
    }
    pub fn line_pos(&self, span: Span) -> usize {
        return self.input[..span.0]
            .chars()
            .filter(|ch| *ch == '\n')
            .count()
            + 1;
    }
}
pub trait LangError {
    fn print_msg(&self, err_out: ErrorBuilder);
}
impl<T: ToString> LangError for Spanned<T> {
    fn print_msg(&self, err_out: ErrorBuilder) {
        err_out.emit(self.unspanned.to_string().as_str(), self.span);
    }
}
#[derive(Clone, Debug)]
pub enum ParseError {
    InvalidToken(TokenType, TokenType),
    UnexpectedToken,
    UnexpectedToplevel,
    UnterminatedParetheses,
    UnexpectedStreamEnd,
    UnexpectedFieldNode(Expr),
}
impl ToString for ParseError {
    fn to_string(&self) -> String {
        use ParseError as PErr;
        match &self {
            PErr::InvalidToken(expected, got) => {
                format!("expected token {expected:?} but got token {:?}", got)
            }
            PErr::UnexpectedToken => "Unexpected token".to_string(),
            PErr::UnexpectedToplevel => "Unexpected token at toplevel".to_string(),
            PErr::UnexpectedStreamEnd => {
                "Expected To find another token but none was found".to_string()
            }
            PErr::UnterminatedParetheses => "Unterminated parentheses".to_string(),
            PErr::UnexpectedFieldNode(_) => "Invalid Node in struct feilds".to_string(),
        }
    }
}
pub type SpannedParseErr = Spanned<ParseError>;

#[derive(Debug)]
pub enum InterpreterError {
    MixedTypes(Type, Type),
    InvalidReturnType(Type, Type),
    InvalidType(Vec<Type>, Type),
    InvalidControl,
    VoidAssignment,
    NonExistentVar(String),
    InvalidAssignment(String),
    InvalidConstructor,
    InvalidOp(BinaryOp, Type),
    InvalidArgSize(u32, u32),
    InvalidBinary(Type),
}
pub type SpannedInterpreterErr = Spanned<InterpreterError>;

impl ToString for InterpreterError {
    fn to_string(&self) -> String {
        use InterpreterError as IErr;
        match &self {
            IErr::MixedTypes(first, last) => format!("Mixed types: {first:?} and {last:?}"),
            IErr::InvalidReturnType(first, got) => {
                format!("Invalid return type: expected {first:?} but got {got:?}")
            }
            IErr::InvalidType(accepted, got) => {
                let opts = String::from_iter(
                    format!("{accepted:?}")
                        .chars()
                        .filter(|c| c != &'[' && c != &']'),
                )
                .replace(',', " or ");
                format!("Invalid Types expected: {opts:?} but got {got:?}")
            }
            IErr::InvalidControl => "Invalid control flow node".to_string(),
            IErr::VoidAssignment => "Attempted to assign void to a variable".to_string(),
            IErr::NonExistentVar(name) => {
                "Couldnt find variable with name: ".to_string() + name.as_str()
            }
            IErr::InvalidAssignment(name) => {
                "Attempted to assign to non existent variable with name: ".to_string()
                    + name.as_str()
            }
            IErr::InvalidConstructor => "Attempted to construct a non existent struct".to_string(),
            IErr::InvalidOp(op, inv) => format!("Cant do {op:?} operation with type {inv:?}"),
            IErr::InvalidArgSize(expected, got) => format!(
                "Invalid args size expected {expected:?} arguments but got {got:?} arguments"
            ),
            IErr::InvalidBinary(got) => format!("Invalid type in binary operation: {:?}", got),
        }
    }
}
