use crate::ast_nodes;
use crate::Interpreter;
use crate::Parser;
use ast_nodes::*;
use std::collections::HashMap;
use std::io;
use std::io::Write;

const NULL: Value = Value::Null;
pub fn default_scope() -> Scope {
    Scope {
        parent: None,
        vars: var_map(),
        structs: HashMap::from([]),
    }
}
pub fn var_map() -> VarMap {
    HashMap::from([
        (
            "input".to_string(),
            (
                Value::BuiltinFunc(BuiltinFunc {
                    function: input_builtin,
                    inf: true,
                    ptypes: vec![Type::Any],
                    ret_type: Type::Str,
                }),
                Type::Function,
            ),
        ),
        (
            "println".to_string(),
            (
                Value::BuiltinFunc(BuiltinFunc {
                    function: println_builtin,
                    inf: true,
                    ptypes: vec![Type::Any],
                    ret_type: Type::Null,
                }),
                Type::Function,
            ),
        ),
        (
            "print".to_string(),
            (
                Value::BuiltinFunc(BuiltinFunc {
                    function: print_builtin,
                    inf: true,
                    ptypes: vec![Type::Any],
                    ret_type: Type::Null,
                }),
                Type::Function,
            ),
        ),
        (
            "parse_num".to_string(),
            (
                Value::BuiltinFunc(BuiltinFunc {
                    function: parse_num,
                    inf: false,
                    ptypes: vec![Type::Str],
                    ret_type: Type::Any,
                }),
                Type::Function,
            ),
        ),
        (
            "to_str".to_string(),
            (
                Value::BuiltinFunc(BuiltinFunc {
                    function: to_str,
                    inf: false,
                    ptypes: vec![Type::Any],
                    ret_type: Type::Any,
                }),
                Type::Function,
            ),
        ),
        (
            "typeof".to_string(),
            (
                Value::BuiltinFunc(BuiltinFunc {
                    function: typeof_node,
                    inf: false,
                    ptypes: vec![Type::Any],
                    ret_type: Type::Str,
                }),
                Type::Function,
            ),
        ),
        (
            "eval".to_string(),
            (
                Value::BuiltinFunc(BuiltinFunc {
                    function: eval,
                    inf: false,
                    ptypes: vec![Type::Str],
                    ret_type: Type::Any,
                }),
                Type::Function,
            ),
        ),
    ])
}
pub fn str_struct() -> Struct {
    let env = str_structmap();
    Struct {
        id: "str".to_string(),
        env: Scope::new(None, env, HashMap::from([])),
    }
}
pub fn num_struct() -> Struct {
    let env = num_structmap();
    Struct {
        id: "num".to_string(),
        env: Scope::new(None, env, HashMap::from([])),
    }
}
pub fn num_structmap() -> VarMap {
    HashMap::from([(
        "to_string".to_string(),
        (
            Value::BuiltinFunc(BuiltinFunc {
                function: num_to_str,
                inf: false,
                ptypes: vec![],
                ret_type: Type::Str,
            }),
            Type::Function,
        ),
    )])
}
pub fn str_structmap() -> VarMap {
    HashMap::from([
        (
            "parse_num".to_string(),
            (
                Value::BuiltinFunc(BuiltinFunc {
                    function: parse_num,
                    inf: false,
                    ptypes: vec![],
                    ret_type: Type::Any,
                }),
                Type::Function,
            ),
        ),
        (
            "substr".to_string(),
            (
                Value::BuiltinFunc(BuiltinFunc {
                    function: substr_method,
                    inf: false,
                    ptypes: vec![Type::Num, Type::Num],
                    ret_type: Type::Str,
                }),
                Type::Function,
            ),
        ),
        (
            "char_at".to_string(),
            (
                Value::BuiltinFunc(BuiltinFunc {
                    function: char_at_method,
                    inf: false,
                    ptypes: vec![Type::Num],
                    ret_type: Type::Str,
                }),
                Type::Function,
            ),
        ),
    ])
}

pub fn parse_num_method(this: TypedValue, _: ValueStream) -> Value {
    let Value::Str(value) = this.0 else { panic!() };
    let parsed: Result<f64, _> = String::from_iter(value.chars().filter(|&c| c != '_')).parse();
    let Ok(result) = parsed else {return NULL;};
    Value::Num(result)
}
pub fn num_to_str(this: TypedValue, _: ValueStream) -> Value {
    let Value::Num(value) = this.0 else {panic!()};
    Value::Str(format!("{value}"))
}
pub fn substr_method(this: TypedValue, args: ValueStream) -> Value {
    let Value::Str(value) = this.0 else { panic!() };
    let [Value::Num(start), Value::Num(end)] = args[..2] else { return NULL; };
    let sub = &value[start as usize..end as usize];
    Value::Str(sub.to_string())
}

pub fn char_at_method(this: TypedValue, args: ValueStream) -> Value {
    let Value::Str(value) = this.0 else { panic!() };
    let Value::Num(index) = &args[0] else {return NULL;};

    value
        .chars()
        .nth(*index as usize)
        .map(|c| Value::Str(c.to_string()))
        .unwrap_or(Value::Null)
}

pub fn val_to_str(val: &Value) -> String {
    match val {
        Value::Num(num) => num.to_string(),
        Value::Bool(cond) => cond.to_string(),
        Value::Str(txt) => txt.to_string(),
        Value::Null => "null".to_string(),
        Value::Void => "void".to_string(),
        Value::Control(val) => format!("{val:?}"),
        _ => "unnamed".to_string(),
    }
}

pub fn println_builtin(_: TypedValue, args: ValueStream) -> Value {
    if args.is_empty() {
        println!();
    }
    let mut out = "".to_string();
    for val in args {
        out += format!(" {}", val_to_str(&val)).as_str();
    }
    out = out.trim().to_string();
    println!("{out}");
    Value::Null
}
pub fn print_builtin(_: TypedValue, args: ValueStream) -> Value {
    if args.is_empty() {
        print!("");
        io::stdout().flush().unwrap();
        return Value::Null;
    }
    let mut out = "".to_string();
    for val in args {
        out += format!(" {}", val_to_str(&val)).as_str();
    }
    out = out.trim().to_string();
    print!("{out}");
    io::stdout().flush().unwrap();
    Value::Null
}

pub fn typeof_node(_: TypedValue, args: ValueStream) -> Value {
    let out = args[0].get_type().to_string();
    Value::Str(out)
}
pub fn parse_num(_: TypedValue, args: ValueStream) -> Value {
    let Value::Str(input) = &args[0] else {return NULL;};
    Value::Num(input.parse().unwrap())
}
pub fn to_str(_: TypedValue, args: ValueStream) -> Value {
    Value::Str(val_to_str(&args[0]))
}
pub fn input_builtin(_: TypedValue, args: ValueStream) -> Value {
    if !args.is_empty() {
        let message = &args[0];
        print!("{}", val_to_str(message));
        io::stdout().flush().unwrap();
    }
    let mut result = String::new();
    let read = io::stdin().read_line(&mut result);
    if read.is_err() {
        result = "".to_string()
    }
    return Value::Str(String::from(result.trim()));
}
pub fn eval(_: TypedValue, args: ValueStream) -> Value {
    let Value::Str(source) = &args[0] else {return Value::Str("Expected a string".to_string());};
    let mut parser = Parser::new(source.as_str());
    let ast_result = parser.batch_parse_expr();
    let Ok(ast) = ast_result else {
        return Value::Null;
    };

    let Ok(result) = Interpreter::execute_node(ast) else {return Value::Null;};
    let Value::Control(nevah) = result.0 else {return result.0;};

    match nevah {
        Control::Result(val, _) => *val,
        Control::Return(val, _) => *val,
        _ => Value::Null,
    }
}
