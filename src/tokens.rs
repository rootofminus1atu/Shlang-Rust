use crate::spans::*;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    Str,
    Num,
    Int,
    Float,
    Ident,
    Semicolon,
    Plus,
    Minus,
    Start,
    Slash,
    Lparen,
    Rparen,
    LBrace,
    RBrace,
    LBrack,
    Rbrack,
    Equal,
    Dot,
    Lesser,
    Greater,
    GreaterEqual,
    LesserEqual,
    Comma,
    Colon,
    Bang,
    Percent,
    DoubleEqual,
    BangEqual,
    And,
    Not,
    Or,
    If,
    Else,
    Func,
    Return,
    Loop,
    While,
    Break,
    False,
    True,
    Var,
    Do,
    Ampersand,
    Pipe,
    Null,
    Struct,
    Continue,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    New,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenType,
    pub span: (usize, usize),
}
pub trait TokenEq {
    fn is(&self, kind: &TokenType) -> bool;
    fn isnt(&self, kind: &TokenType) -> bool;
}
impl Token {
    pub fn new(kind: TokenType, span: Span) -> Self {
        Token { kind, span }
    }
}
impl TokenEq for Token {
    fn is(&self, kind: &TokenType) -> bool {
        &self.kind == kind
    }
    fn isnt(&self, kind: &TokenType) -> bool {
        &self.kind != kind
    }
}
type MaybeToken = Option<Token>;
impl TokenEq for MaybeToken {
    fn is(&self, kind: &TokenType) -> bool {
        match self.clone() {
            Some(tok) => &tok.kind == kind,
            None => false,
        }
    }
    fn isnt(&self, kind: &TokenType) -> bool {
        match self.clone() {
            Some(tok) => &tok.kind != kind,
            None => false,
        }
    }
}
pub fn map_keyword(text: String) -> Option<TokenType> {
    match text.as_str() {
        "true" => Some(TokenType::True),
        "false" => Some(TokenType::False),
        "if" => Some(TokenType::If),
        "else" => Some(TokenType::Else),
        "func" => Some(TokenType::Func),
        "return" => Some(TokenType::Return),
        "loop" => Some(TokenType::Loop),
        "while" => Some(TokenType::While),
        "break" => Some(TokenType::Break),
        "var" => Some(TokenType::Var),
        "and" => Some(TokenType::And),
        "not" => Some(TokenType::Not),
        "or" => Some(TokenType::Or),
        "do" => Some(TokenType::Do),
        "null" => Some(TokenType::Null),
        "struct" => Some(TokenType::Struct),
        "continue" => Some(TokenType::Continue),
        "new" => Some(TokenType::New),
        _ => None,
    }
}
