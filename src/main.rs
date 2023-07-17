use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::*;
pub mod AstNodes;
pub mod Defaults;
pub mod Runner;
pub mod Lexer;
pub mod TokParser;
pub mod Token;
pub mod tests;
use Lexer::*;
use TokParser::Parser;
use Runner::Interpreter;
fn input(message: &str) -> String {
    print!("{message} ");
    io::stdout().flush().unwrap();
    let mut result = String::new();
    io::stdin()
        .read_line(&mut result)
        .expect("didnt receive input:");
    return String::from(result.trim());
}
fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() <= 1 {
        full_rpl();
        return;
    }
    let file_path = &args[1];
    let source = fs::read_to_string(file_path).expect("Should have been able to read the file");
    let mut parser = Parser::new(source.as_str());
    println!("{:#?}", parser.batch_parse());
}
fn lexer_rpl() {
    loop {
        let source = input(">: ");
        let mut scan = Scanner::new(&source);
        let mut parser = Parser::new(source.as_str());
        loop {
            let tok = scan.next();
            if tok.is_none() {
                break;
            }
            let some = tok.unwrap();
            println!("{:?} | {:?}", some.clone(), parser.text(some));
        }
    }
}
fn rpl() {
    loop {
        let source = input(">: ");
        let scan = Scanner::new(&source);
        let mut parser = Parser::new(source.as_str());
        println!("{:#?}", parser.batch_parse());
    }
}
fn full_rpl() {
    loop {
        let source = input(">: ");
        let mut parser = Parser::new(source.as_str());
        let ast = parser.batch_parse();
        println!("{:#?}", &ast);
        let mut interpreter = Interpreter::new(ast);
        interpreter.execute()
    }
}
