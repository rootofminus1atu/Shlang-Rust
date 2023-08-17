use crate::ast_nodes;
use ast_nodes::*;
use std::collections::HashMap;
use std::io;
use std::io::Write;
pub fn default_scope() -> Scope {
    Scope {
        parent: None,
        vars: var_map(),
        structs: HashMap::from([]),
    }
}
pub fn var_map() -> VarMap {
    let map = HashMap::from([
        ("noice".to_string(), Value::Num(69.0)),
        (
            "input".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: input_builtin,
                arg_size: -1,
            }),
        ),
        (
            "println".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: println_builtin,
                arg_size: -1,
            }),
        ),
        (
            "print".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: print_builtin,
                arg_size: -1,
            }),
        ),
        (
            "parse_num".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: parse_num,
                arg_size: 1,
            }),
        ),
        (
            "to_str".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: to_str,
                arg_size: 1,
            }),
        ),
        (
            "typeof".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: typeof_node,
                arg_size: 1,
            }),
        ),
    ]);
    map
}
pub fn str_struct(val: String) -> Struct {
    let env = str_structmap(val);
    Struct {
        id: "str".to_string(),
        env: Scope::new(None, env, HashMap::from([])),
    }
}
pub fn num_struct(val: f64) -> Struct {
    let env = num_structmap(val);
    Struct {
        id: "num".to_string(),
        env: Scope::new(None, env, HashMap::from([])),
    }
}
pub fn num_structmap(val: f64) -> VarMap {
    return HashMap::from([
        ("v".to_string(), Value::Num(val)),
        (
            "to_string".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: num_to_str,
                arg_size: 0,
            }),
        ),
    ]);
}
pub fn str_structmap(val: String) -> VarMap {
    return HashMap::from([
        ("nice".to_string(), Value::Num(69.0)),
        ("v".to_string(), Value::Str(val)),
        (
            "parse_num".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: parse_num_method,
                arg_size: 0,
            }),
        ),
        (
            "substr".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: substr_method,
                arg_size: 2,
            }),
        ),
        (
            "char_at".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: char_at_method,
                arg_size: 1,
            }),
        ),
    ]);
}

pub fn parse_num_method(scope: VarMap, _: ValueStream) -> Value {
    let Some(Value::Str(value)) = scope.get("v") else {panic!()};
    let parsed: Result<f64, _> = value.parse();
    let Ok(result) = parsed else {return Value::Null;};
    return Value::Num(result);
}
pub fn num_to_str(scope: VarMap, _: ValueStream) -> Value {
    let Some(Value::Num(value)) = scope.get("v") else {panic!()};
    return Value::Str(format!("{value}"));
}
pub fn substr_method(scope: VarMap, args: ValueStream) -> Value {
    let Some(Value::Str(value)) = scope.get("v") else { panic!() };
    let [Value::Num(start), Value::Num(end)] = args[..2] else { panic!() };
    let sub = &value[start as usize..end as usize];
    Value::Str(sub.to_string())
}

pub fn char_at_method(scope: VarMap, args: ValueStream) -> Value {
    let Some(Value::Str(value)) = scope.get("v") else { panic!() };
    let Value::Num(index) = &args[0] else {panic!()};

    value
        .chars()
        .nth(*index as usize)
        .map(|c| Value::Str(c.to_string()))
        .unwrap_or(Value::Null)
}

fn val_to_str(val: &Value) -> String {
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

pub fn println_builtin(_: VarMap, args: ValueStream) -> Value {
    if args.is_empty() {
        println!("");
    }
    let mut out = "".to_string();
    for val in args {
        out += format!(" {}", val_to_str(&val)).as_str();
    }
    out = out.trim().to_string();
    println!("{out}");
    return Value::Void;
}
pub fn print_builtin(_: VarMap, args: ValueStream) -> Value {
    if args.is_empty() {
        print!("");
        io::stdout().flush().unwrap();
        return Value::Void;
    }
    let mut out = "".to_string();
    for val in args {
        out += format!(" {}", val_to_str(&val)).as_str();
    }
    out = out.trim().to_string();
    print!("{out}");
    io::stdout().flush().unwrap();
    return Value::Void;
}
pub fn typeof_node(_: VarMap, args: ValueStream) -> Value {
    use Type::*;
    let out = match args[0].get_type() {
        Void => "void",
        Null => "null",
        Function => "func",
        Never => "!",
        Bool => "bool",
        Num => "num",
        Str => "str",
        UserDefined(id) => return Value::Str(id.to_string()),
        Ref(_) => "ref",
    }
    .to_string();
    return Value::Str(out);
}
pub fn parse_num(_: VarMap, args: ValueStream) -> Value {
    let Value::Str(input) = &args[0] else {panic!()};
    Value::Num(input.parse().unwrap())
}
pub fn to_str(_: VarMap, args: ValueStream) -> Value {
    return Value::Str(val_to_str(&args[0]));
}
pub fn input_builtin(_: VarMap, args: ValueStream) -> Value {
    if !args.is_empty() {
        let message = &args[0];
        print!("{}", val_to_str(&message));
        io::stdout().flush().unwrap();
    }
    let mut result = String::new();
    let read = io::stdin().read_line(&mut result);
    if read.is_err() {
        result = "".to_string()
    }
    return Value::Str(String::from(result.trim()));
}
