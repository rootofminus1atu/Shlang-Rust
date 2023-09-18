use crate::spans::*;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    Str,
    Int,
    Float,
    Identifier,
    Semicolon,
    Plus,
    Minus,
    Star,
    Slash,
    Lparen,
    Rparen,
    LBrace,
    Rbrace,
    Lbrack,
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
    Continue,
    Loop,
    While,
    Break,
    False,
    True,
    Var,
    Do,
    Ampersand,
    DoubleAmper,
    Pipe,
    DoublePipe,
    Null,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenType,
    pub span: (usize, usize),
}

impl Token {
    pub fn new(kind: TokenType, span: Span) -> Self {
        Token { kind, span }
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
        "continue" => Some(TokenType::Continue),
        _ => None,
    }
}
