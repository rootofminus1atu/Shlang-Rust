use crate::ast_nodes;
use crate::defaults;
use crate::errors::LangError;
use crate::spans::*;
use ast_nodes::*;
use std::collections::HashMap;
use std::*;
const VOID: TypedValue = (Value::Void, Type::Void);
pub struct Interpreter {
    program: Block,
    current: Scope,
    errors: LangError,
}
impl Interpreter {
    pub fn new(program: Block, source: String) -> Self {
        let current = Scope {
            parent: None,

            var_map: defaults::var_map(),
        };
        return Interpreter {
            program,
            current,
            errors: LangError { input: source },
        };
    }
    pub fn execute(&mut self) {
        self.eval_block(self.program.clone());
    }

    fn eval_block(&mut self, block: Block) -> TypedValue {
        let cur = Some(Box::new(self.current.clone()));
        let new_scope = Scope::new(cur, HashMap::from([]));
        self.current = new_scope;
        if block.body.len() == 0 {
            return VOID;
        }
        let body = *block.body;
        for node in body {
            let Value::Control(result) = self.eval_node(node).0 else {continue;};
            let parent = self.current.parent.clone();
            self.current = *parent.expect("Invalid control outside block");
            return (Value::Control(result), Type::Never);
        }

        let parent = self.current.parent.clone();
        self.current = *parent.expect("Invalid control outside block");
        return VOID;
    }
    fn num_convert(&self, num: Value, span: Span) -> (f64, bool) {
        match num {
            Value::Num(val) => (val, val != 0.0),
            Value::Bool(cond) => (cond as i8 as f64, cond),
            invalid => {
                panic!("Invalid type{invalid:?}");
            }
        }
    }
    fn unary_calc(&self, kind: UnaryOp, span: Span, target: TypedValue) -> TypedValue {
        match kind {
            UnaryOp::NEGATIVE => {
                let Value::Num(val) = target.0 else {panic!("Invalid Type expected Num but got {:?}",target.1)};
                return (Value::Num(-val), Type::Num);
            }
            UnaryOp::NOT => {
                return (Value::Bool(!self.num_convert(target.0, span).1), Type::Bool);
            }
        }
    }
    fn binary_calc(&self, kind: BinaryOp, left_val: Value, right_val: Value, span: Span) -> Value {
        let (left, left_bool) = self.num_convert(left_val, span);
        let (right, right_bool) = self.num_convert(right_val, span);
        match kind {
            BinaryOp::ADD => Value::Num(left + right),
            BinaryOp::SUBTRACT => Value::Num(left - right),
            BinaryOp::MULTIPLY => Value::Num(left * right),
            BinaryOp::DIVIDE => Value::Num(left / right),
            BinaryOp::MODULO => Value::Num(left % right),
            BinaryOp::GREATER => Value::Bool(left > right),
            BinaryOp::GREATER_EQUAL => Value::Bool(left >= right),
            BinaryOp::LESSER => Value::Bool(left < right),
            BinaryOp::LESSER_EQUAL => Value::Bool(left <= right),
            BinaryOp::AND => Value::Bool(left_bool && right_bool),
            BinaryOp::OR => Value::Bool(left_bool || right_bool),
            BinaryOp::ISEQUAL => Value::Bool(left == right),
            BinaryOp::ISDIFERENT => Value::Bool(left != right),
        }
    }
    fn str_calc(&mut self, kind: BinaryOp, left_val: Value, right_val: Value, span: Span) -> Value {
        let Value::Str(left) = left_val else {panic!()};
        let Value::Str(right) = right_val else {panic!()};

        match kind {
            BinaryOp::ADD => Value::Str(left + &right),
            BinaryOp::GREATER => Value::Bool(left > right),
            BinaryOp::GREATER_EQUAL => Value::Bool(left >= right),
            BinaryOp::LESSER => Value::Bool(left < right),
            BinaryOp::LESSER_EQUAL => Value::Bool(left <= right),
            BinaryOp::ISEQUAL => Value::Bool(left == right),
            BinaryOp::ISDIFERENT => Value::Bool(left != right),
            op => {
                self.errors.emit(
                    format!("Cant do {op:?} operation with strings").as_str(),
                    span,
                );
                panic!();
            }
        }
    }

    fn eval_binary_node(&mut self, bin_op: BinaryNode, span: Span) -> TypedValue {
        let (left, left_type) = self.eval_node(*bin_op.left);
        let (right, right_type) = self.eval_node(*bin_op.right);
        if left_type != right_type {
            let are_numerical = left_type.is_numeric() && right_type.is_numeric();
            match bin_op.kind {
                BinaryOp::ISDIFERENT => return (Value::Bool(true), Type::Bool),
                BinaryOp::ISEQUAL => return (Value::Bool(true), Type::Bool),
                _ => {
                    if !are_numerical {
                        self.errors
                            .emit(format!("mixed types: {left:?} {right:?}").as_str(), span);
                        panic!();
                    }
                }
            }
        }
        match left_type {
            Type::Num | Type::Bool => {
                return (self.binary_calc(bin_op.kind, left, right, span), left_type)
            }
            Type::Str => return (self.str_calc(bin_op.kind, left, right, span), left_type),
            invalid => panic!("Invalid type in binary operation:{invalid:?}"),
        }
    }
    fn unwrap_var(&mut self, value: TypedValue, span: Span) -> TypedValue {
        if value.1 == Type::Void {
            self.errors.emit("Cannot assign void to variable", span);
            panic!();
        }
        let Value::Control(control) = value.0 else {
            return value;
        };
        let Control::Result(res_val,res_type) = control else {
            self.errors.emit("Unexpected controlflow node", span);
            panic!();
        };
        if res_type == Type::Void {
            self.errors.emit("Cannot assign void to variable", span);
            panic!();
        }
        return (*res_val, res_type);
    }

    fn declare(&mut self, request: Declaration, span: Span) {
        let init_val = self.eval_node(*request.value);
        let unwraped = self.unwrap_var(init_val, span).0;
        self.current.define(request.var_name, unwraped);
    }
    fn assign(&mut self, request: Assignment, span: Span) {
        let init_val = self.eval_node(*request.value);
        let unwraped = self.unwrap_var(init_val, span).0;
        if self.current.assign(request.var_name, unwraped).is_none() {
            self.errors
                .emit("Attempted to assign to a non existent variable", span);
            panic!();
        }
    }
    fn eval_variable(&mut self, var: Variable, span: Span) -> TypedValue {
        let Some(result) = self.current.get_var(var.name) else {
            self.errors.emit("undefined variable", span);
            panic!();
        };
        let res_type = result.get_type();
        return (result, res_type.clone());
    }
    fn eval_node(&mut self, expr: Node) -> TypedValue {
        match expr {
            Node::Value(val) => (*val.unspanned.clone(), val.unspanned.get_type()),
            Node::Block(body) => self.eval_block(body.unspanned),
            Node::Variable(var) => self.eval_variable(var.unspanned, var.span),
            Node::Call(request) => {
                todo!()
            }
            Node::UnaryNode(unary_op) => {
                let (target, target_type) = self.eval_node(*unary_op.unspanned.object);
                self.unary_calc(
                    unary_op.unspanned.kind,
                    unary_op.span,
                    (target, target_type),
                )
            }
            Node::BinaryNode(bin_op) => self.eval_binary_node(bin_op.unspanned, bin_op.span),
            Node::Branch(branch) => {
                todo!()
            }
            Node::ReturnNode(expr) => {
                let evaluated = self.eval_node(*expr.unspanned);
                let result: Value = Control::Return(Box::new(evaluated.0), evaluated.1).into();
                (result, Type::Never)
            }
            Node::ResultNode(expr) => {
                let evaluated = self.eval_node(*expr.unspanned);
                let result: Value = Control::Result(Box::new(evaluated.0), evaluated.1).into();
                (result, Type::Never)
            }
            Node::BreakNode(_) => (Control::Break.into(), Type::Never),
            Node::Declaration(declaration) => {
                self.declare(declaration.unspanned, declaration.span);
                VOID
            }
            Node::Assignment(ass) => {
                self.assign(ass.unspanned, ass.span);
                VOID
            }
            Node::While(obj) => {
                todo!()
            }
            Node::Loop(obj) => {
                todo!()
            }
            _ => {
                todo!()
            }
        }
    }
}
