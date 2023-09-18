use std::collections::HashMap;
use std::iter::Peekable;

use crate::ast_nodes::*;
use crate::lang_errors::*;
use crate::spans::Spanned as Spn;
use crate::spans::*;
use crate::token_lexer::Lexer;
use crate::tokens::*;
use crate::ParseError as PErr;
#[derive(Clone)]
pub struct TokenIter<'input> {
    lexer: Lexer<'input>,
}

impl<'input> TokenIter<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            lexer: Lexer::new(input),
        }
    }
}

impl<'input> Iterator for TokenIter<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next_tok()
    }
}
#[derive(Clone)]
pub struct Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    input: &'input str,
    tokens: Peekable<I>,
}

impl<'input> Parser<'input, TokenIter<'input>> {
    pub fn new(input: &'input str) -> Parser<'input, TokenIter<'input>> {
        Parser {
            input,
            tokens: TokenIter::new(input).peekable(),
        }
    }
    // converts token spans into text
    pub fn text(&mut self, token: &Token) -> String {
        self.input[token.span.0..token.span.1].to_string()
    }
    pub fn filtered_text(&mut self, token: &Token, filter: char) -> String {
        String::from_iter(self.text(token).chars().filter(|c| c != &filter))
    }
    pub fn parse_float(&mut self, token: &Token) -> Value {
        Value::Float(self.filtered_text(token, '_').parse().unwrap())
    }
    pub fn parse_int(&mut self, token: &Token) -> Value {
        Value::Int(self.filtered_text(token, '_').parse().unwrap())
    }

    // A hack used to fix most escape charaters
    pub fn escaped_text(&mut self, token: &Token) -> String {
        self.input[token.span.0..token.span.1]
            .to_string()
            .replace("\\n", "\n")
            .replace("\\t", "\t")
            .replace("\\r", "\r")
            .replace("\\0", "\0")
            .replace("\\\"", "\"")
    }
    // peeks the current token
    fn peek(&mut self) -> Option<Token> {
        self.tokens.peek().cloned()
    }
    // peeks the current token and if none was found it prints and returns an error
    // this is used for expressions that require the existence of a current token
    fn peek_some(&mut self) -> Result<Token, Spn<PErr>> {
        let Some(peeked) = self.tokens.peek().cloned() else {
            return Err(PErr::UnexpectedStreamEnd.to_spanned((0,0)));
        };
        Ok(peeked)
    }
    // advances to the next token
    fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }
    // checks if a token is the expected token and if it isnt returns an error
    // this is used for checking if certain expressions are valid
    fn check_valid(&mut self, expected: TokenType, token: Token) -> Result<(), Spn<PErr>> {
        if token.kind == expected {
            return Ok(());
        }
        Err(PErr::InvalidToken(expected, token.kind).to_spanned(token.span))
    }
    // peeks the current token and checks if it is the same as the expected token returning an error if it isnt
    // this is also used for validating expressions
    fn expect(&mut self, expected: TokenType) -> Result<Token, Spn<PErr>> {
        let token: Token = self.peek_some()?;
        self.check_valid(expected, token.clone())?;
        Ok(token)
    }
    fn consume(&mut self, expected: TokenType) -> Result<Token, Spn<PErr>> {
        let token = self.expect(expected)?;
        self.next();
        Ok(token)
    }
    fn parse_paren(&mut self) -> Result<Spn<Expr>, Spn<PErr>> {
        let paren = self.next().unwrap();
        let expr = self.parse_expr(0)?;
        let Some(end) = self.peek() else {
            return Err(PErr::UnterminatedParetheses.to_spanned(paren.span));
        };
        self.check_valid(TokenType::Rparen, end)?;
        Ok(expr)
    }
    fn parse_atom(&mut self, peeked: &Token) -> Result<Spn<Expr>, Spn<PErr>> {
        match &peeked.kind {
            TokenType::Str => Ok(Value::Str(self.escaped_text(peeked)).to_nodespan(peeked.span)),
            TokenType::Int => Ok(self.parse_int(peeked).to_nodespan(peeked.span)),
            TokenType::Float => Ok(self.parse_float(peeked).to_nodespan(peeked.span)),
            TokenType::False => Ok(Value::Bool(false).to_nodespan(peeked.span)),
            TokenType::True => Ok(Value::Bool(true).to_nodespan(peeked.span)),
            TokenType::Null => Ok(Value::Null.to_nodespan(peeked.span)),
            TokenType::Lparen => self.parse_paren(),
            _ => todo!(),
        }
    }
    fn prefix_binding_power(&self, op: TokenType) -> ((), u8) {
        match op {
            TokenType::Plus | TokenType::Minus => ((), 5),
            _ => panic!("bad op: {:?}", op),
        }
    }
    fn postfix_binding_power(&self, op: TokenType) -> Option<(u8, ())> {
        Some(match op {
            TokenType::Bang => (2, ()),
            _ => return None,
        })
    }
    fn infix_binding_power(&self, op: TokenType) -> Option<(u8, u8)> {
        Some(match op {
            TokenType::Plus | TokenType::Minus => (1, 2),
            TokenType::Star | TokenType::Slash => (3, 4),
            _ => return None,
        })
    }
    fn bin_op(op_tok: TokenType, lhs: Spanned<Expr>, rhs: Spanned<Expr>) -> Option<Spanned<Expr>> {
        let op = match op_tok {
            TokenType::Plus => BinaryOp::Add,
            TokenType::Slash => BinaryOp::Divide,
            TokenType::And | TokenType::DoubleAmper => BinaryOp::And,
            TokenType::Or | TokenType::DoublePipe => BinaryOp::Or,
            TokenType::Minus => BinaryOp::Subtract,
            TokenType::Star => BinaryOp::Multiply,
            TokenType::BangEqual => BinaryOp::IsDiferent,
            TokenType::DoubleEqual => BinaryOp::IsEqual,
            TokenType::Greater => BinaryOp::Greater,
            TokenType::Lesser => BinaryOp::Lesser,
            TokenType::LesserEqual => BinaryOp::LesserOrEqual,
            TokenType::GreaterEqual => BinaryOp::GreaterOrEqual,
            TokenType::Percent => BinaryOp::Modulo,
            _ => return None,
        };
        Some(
            Expr::BinaryNode(BinaryNode {
                kind: op,
                left: Box::new(lhs.clone()),
                right: Box::new(rhs.clone()),
            })
            .to_spanned((lhs.span.0, rhs.span.1)),
        )
    }
    fn parse_op(op_tok: TokenType, lhs: Spanned<Expr>, rhs: Spanned<Expr>) -> Spanned<Expr> {
        todo!()
    }
    pub fn parse_expr(&mut self, min_bp: u8) -> Result<Spn<Expr>, Spn<PErr>> {
        let lhs_tok = self.peek().unwrap();
        let mut lhs = self.parse_atom(&lhs_tok)?;
        self.next();
        while let Some(op_tok) = self.peek() {
            if !matches!(
                op_tok.kind,
                TokenType::Plus | TokenType::Minus | TokenType::Star | TokenType::Slash
            ) {
                panic!()
            }
            if let Some((l_bp, r_bp)) = self.infix_binding_power(op_tok.kind) {
                if l_bp < min_bp {
                    break;
                }
                self.next();
                let rhs = self.parse_expr(r_bp)?;
                continue;
            }
        }
        Ok(lhs)
    }

    fn parse_top(&mut self) -> Result<AST, Spanned<PErr>> {
        let mut ast = AST::default();
        while let Some(next) = self.next() {
            match next.kind {
                TokenType::Func => {
                    todo!()
                }

                TokenType::Var => {
                    todo!()
                }
                _ => return Err(PErr::UnexpectedToplevel.to_spanned(next.span)),
            }
        }
        todo!()
    }
}
