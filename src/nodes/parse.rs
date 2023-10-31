use std::collections::HashMap;
use std::iter::Peekable;

use super::ast_nodes::*;
use crate::lang_errors::*;
use crate::spans::*;
use crate::token_lexer::Lexer;
use crate::tokens::*;

use super::ast_nodes::Block;

#[derive(Clone)]
pub struct TokenIter<'input> {
    lexer: Lexer<'input>,
}
type ExprSpan = Spanned<Expr>;
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

type TokenResult = Result<Token, ParseError>;
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
    pub fn parse_num(&mut self, token: &Token) -> ExprSpan {
        let mut text = self.input[token.span.0..token.span.1].to_string();
        let idk: Vec<_> = text.chars().filter(|c| c != &'_').collect();
        text = String::from_iter(idk);
        let val = Value::Num(text.parse().unwrap());
        self.build_expr(val, token.span)
    }
    pub fn parse_string(&mut self, token: &Token) -> ExprSpan {
        let text = self.input[token.span.0..token.span.1].to_string();
        self.build_expr(Value::Str(text), token.span)
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
    fn peek_some(&mut self) -> Result<Token, ParseError> {
        let Some(peeked) = self.tokens.peek().cloned() else {
            return Err(ParseError::UnexpectedStreamEnd);
        };
        Ok(peeked)
    }
    // advances to the next token
    fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }
    // checks if a token is the expected token and if it isnt returns an error
    // this is used for checking if certain expressions are valid
    fn check_valid(&mut self, expected: TokenType, token: Token) -> Result<(), ParseError> {
        if token.is(&expected) {
            return Ok(());
        }
        Err(ParseError::InvalidToken(expected, token))
    }
    // peeks the current token and checks if it is the same as the expected token returning an error if it isnt
    // this is also used for validating expressions
    fn expect(&mut self, expected: TokenType) -> Result<Token, ParseError> {
        let token: Token = self.peek_some()?;
        self.check_valid(expected, token.clone())?;
        Ok(token)
    }
    fn consume(&mut self, expected: TokenType) -> Result<Token, ParseError> {
        let token = self.expect(expected)?;
        self.next();
        Ok(token)
    }
    // this is used for expressions with blocks like if
    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let first = self.expect(TokenType::LBrace)?;
        let mut body: Block = vec![];
        self.next().unwrap();
        let token = self.peek_some()?;
        if token.is(&TokenType::RBrace) {
            return Ok(body);
        }
        loop {
            let expr = self.parse_expr()?;
            body.push(expr);
            if self.peek().is(&TokenType::RBrace) {
                break;
            }
        }
        Ok(body)
    }
    // These Parse variable definitions/declaration
    // Parses toplevel expressions
    pub fn parse_top(&mut self) -> Result<AST, ParseError> {
        let mut vars: HashMap<String, Spanned<Expr>> = HashMap::from([]);
        let mut funcs: HashMap<String, Function> = HashMap::from([]);

        while let Some(peeked) = self.peek() {
            match peeked.clone().kind {
                TokenType::Var => {
                    let Expr::Statement(Statement::Declaration(name, val)) =
                        self.parse_vardef()?.unspanned else {panic!()};
                    vars.insert(name, val);
                }
                TokenType::Func => {
                    todo!()
                }
                TokenType::Struct => {
                    todo!()
                }
                _ => return Err(ParseError::UnexpectedToplevel(peeked)),
            }
        }
        todo!()
    }
    fn build_expr(&mut self, val: Value, span: Span) -> ExprSpan {
        Spanned::new(Expr::Value(val), span)
    }
    fn compound_assignment(
        &mut self,
        kind: BinaryOp,
        var: ExprSpan,
        value: ExprSpan,
        span: Span,
    ) -> Result<ExprSpan, ParseError> {
        Ok(Statement::Assignment(
            var.clone().box_unspanned(),
            self.binary_node(kind, var, value, span)?.box_unspanned(),
        )
        .to_exprspan(span))
    }
    fn parse_assignment(&mut self, target: ExprSpan, token: Token) -> Result<ExprSpan, ParseError> {
        let last = self.peek_some()?;
        self.next();
        let value = self.parse_expr()?;
        let span = (token.span.0, last.span.1);
        match last.kind {
            TokenType::PlusEqual => {
                return self.compound_assignment(BinaryOp::ADD, target, value, span)
            }
            TokenType::MinusEqual => {
                return self.compound_assignment(BinaryOp::SUBTRACT, target, value, span)
            }
            TokenType::SlashEqual => {
                return self.compound_assignment(BinaryOp::DIVIDE, target, value, span)
            }
            TokenType::StarEqual => {
                return self.compound_assignment(BinaryOp::MULTIPLY, target, value, span)
            }
            _ => {}
        }
        Ok(Statement::Assignment(target.box_unspanned(), value.box_unspanned()).to_exprspan(span))
    }
    fn binary_node(
        &self,
        kind: BinaryOp,
        left: ExprSpan,
        right: ExprSpan,
        span: Span,
    ) -> Result<ExprSpan, ParseError> {
        Ok(Spanned::new(
            Expr::BinaryNode(kind, left.box_unspanned(), right.box_unspanned()),
            span,
        ))
    }
    fn unary_operator(&mut self, kind: UnaryOp) -> Result<ExprSpan, ParseError> {
        let token = self.peek();
        self.next();
        let right = self.parse_atom(token.as_ref())?;
        Ok(Expr::UnaryNode(kind, right.box_unspanned()).to_spanned(token.unwrap().span))
    }
    pub fn parse_expr(&mut self) -> Result<ExprSpan, ParseError> {
        let left = self.or_prec()?;
        let Some(op) = self.peek() else {return Ok(left);};
        match op.kind {
            TokenType::Equal
            | TokenType::PlusEqual
            | TokenType::MinusEqual
            | TokenType::StarEqual
            | TokenType::SlashEqual => self.parse_assignment(left, op),
            _ => Ok(left),
        }
    }

    // a level of precedence for logic or
    fn or_prec(&mut self) -> Result<ExprSpan, ParseError> {
        let left = self.and_prec()?;
        let Some(op) = self.peek() else {return Ok(left);};
        if op.isnt(&TokenType::Or) && op.isnt(&TokenType::Pipe) {
            return Ok(left);
        }
        self.next();
        let result = self.or_prec()?;
        self.binary_node(BinaryOp::OR, left, result, op.span)
    }
    // a level of precedence for logic and
    fn and_prec(&mut self) -> Result<ExprSpan, ParseError> {
        let left = self.eq_prec()?;
        let Some(op) = self.peek() else {return Ok(left);};
        if op.isnt(&TokenType::And) && op.isnt(&TokenType::Ampersand) {
            return Ok(left);
        }
        self.next();
        let result = self.and_prec()?;
        self.binary_node(BinaryOp::AND, left, result, op.span)
    }
    // a level of precedence for:
    // >= > < <= == !=
    fn eq_prec(&mut self) -> Result<ExprSpan, ParseError> {
        let left = self.add_prec()?;
        let Some(op) = self.peek() else {return Ok(left);};
        let kind = match op.kind {
            TokenType::Greater => BinaryOp::GREATER,
            TokenType::GreaterEqual => BinaryOp::GREATER_EQUAL,
            TokenType::Lesser => BinaryOp::LESSER,
            TokenType::LesserEqual => BinaryOp::LESSER_EQUAL,
            TokenType::DoubleEqual => BinaryOp::ISEQUAL,
            TokenType::BangEqual => BinaryOp::ISDIFERENT,
            _ => return Ok(left),
        };
        self.next();
        let result = self.eq_prec()?;
        self.binary_node(kind, left, result, op.span)
    }
    // a level of precedence for:
    // + -
    fn add_prec(&mut self) -> Result<ExprSpan, ParseError> {
        let left = self.prod_prec()?;
        let Some(op) = self.peek() else {return Ok(left);};
        let kind = match op.kind {
            TokenType::Plus => BinaryOp::ADD,
            TokenType::Minus => BinaryOp::SUBTRACT,
            _ => return Ok(left),
        };
        self.next();
        let result = self.add_prec()?;
        self.binary_node(kind, left, result, op.span)
    }
    // a level of precedence for:
    // * / %
    fn prod_prec(&mut self) -> Result<ExprSpan, ParseError> {
        let left = self.primary_prec()?;
        let Some(op) = self.peek() else {return Ok(left);};
        let kind = match op.kind {
            TokenType::Slash => BinaryOp::DIVIDE,
            TokenType::Start => BinaryOp::MULTIPLY,
            TokenType::Percent => BinaryOp::MODULO,
            _ => return Ok(left),
        };
        self.next();
        let result = self.prod_prec()?;
        self.binary_node(kind, left, result, op.span)
    }

    fn primary_prec(&mut self) -> Result<ExprSpan, ParseError> {
        let value = self.peek();
        self.next();
        let left = self.parse_atom(value.as_ref())?;
        self.primary_ops(left)
    }
    fn primary_ops(&mut self, left: ExprSpan) -> Result<ExprSpan, ParseError> {
        let Some(op) = self.peek() else {return Ok(left);};
        match op.kind {
            TokenType::Lparen => self.parse_call(left),
            TokenType::Dot => self.parse_field_access(left, op.span),
            _ => Ok(left),
        }
    }
    fn parse_field_access(&mut self, target: ExprSpan, span: Span) -> Result<ExprSpan, ParseError> {
        self.next();
        let requested = self.primary_prec()?.box_unspanned();

        Ok(Expr::FieldAccess {
            target: target.box_unspanned(),
            requested,
        }
        .to_spanned(span))
    }
    fn parse_expr_list(
        &mut self,
        token: &Token,
        closing_tok: TokenType,
    ) -> Result<Block, ParseError> {
        let mut token = token.clone();
        let mut params: Block = vec![];
        while token.isnt(&closing_tok) {
            if self.peek().is(&closing_tok) {
                break;
            }
            let expr = self.parse_expr()?;
            token = self.peek_some()?;
            params.push(expr);
            if token.is(&closing_tok) {
                break;
            }
            if token.is(&TokenType::Comma) {
                self.next();
                continue;
            }
            return Err(ParseError::UnexpectedToken(token));
        }
        Ok(params)
    }
    fn parse_call(&mut self, callee: ExprSpan) -> Result<ExprSpan, ParseError> {
        let first = self.next().unwrap();
        let token = self.peek_some()?;
        let params = self.parse_expr_list(&token, TokenType::Rparen)?;
        let last = self.peek_some()?;
        self.next();
        let call = Spanned::new(
            Expr::Call(callee.box_unspanned(), params),
            (first.span.0, last.span.1),
        );
        self.primary_ops(call)
    }
    fn parse_do(&mut self) -> Result<ExprSpan, ParseError> {
        let first = self.expect(TokenType::LBrace)?;
        let block = self.parse_block()?;
        let last = self.peek_some()?;
        self.next();
        let span = (first.span.0, last.span.1);
        Ok(Expr::DoBlock(block).to_spanned(span))
    }
    fn parse_loop(&mut self) -> Result<ExprSpan, ParseError> {
        let first = self.expect(TokenType::LBrace)?;
        let block = self.parse_block()?;
        let last = self.peek_some()?;
        self.next();
        let span = (first.span.0, last.span.1);
        Ok(Expr::Loop(block).to_spanned(span))
    }
    fn parse_elif(
        &mut self,
        condition: ExprSpan,
        if_block: Block,
        span: Span,
    ) -> Result<ExprSpan, ParseError> {
        self.next();
        let elif = self.parse_branch()?;
        let elif_span = elif.span;
        let elif_block: Block = vec![elif];

        Ok(Expr::Branch(condition.box_unspanned(), if_block, Some(elif_block)).to_spanned(span))
    }
    // parses if expressions

    fn parse_branch(&mut self) -> Result<ExprSpan, ParseError> {
        let first = self.peek_some()?;
        let condition = self.parse_expr()?;
        let last = self.peek_some()?;
        let if_block = self.parse_block()?;
        self.next();
        let span = (first.span.0, last.span.1);
        let Some(else_branch) = self.peek() else {
            self.next();
            return Ok(Expr::Branch(condition.box_unspanned(),if_block, None).to_spanned(span));
        };
        if else_branch.isnt(&TokenType::Else) {
            return Ok(Expr::Branch(condition.box_unspanned(), if_block, None).to_spanned(span));
        }
        self.next();
        if self.peek_some()?.is(&TokenType::If) {
            return self.parse_elif(condition, if_block, span);
        }
        let else_block = self.parse_block()?;
        self.next();
        Ok(Expr::Branch(condition.box_unspanned(), if_block, Some(else_block)).to_spanned(span))
    }
    fn struct_params(&mut self) -> Result<HashMap<String, ExprSpan>, ParseError> {
        self.consume(TokenType::LBrace)?;
        let token = self.peek_some()?;
        let mut body: HashMap<String, ExprSpan> = HashMap::from([]);
        if token.is(&TokenType::RBrace) {
            return Ok(body);
        }
        loop {
            let target = self.consume(TokenType::Ident)?;
            self.consume(TokenType::Colon)?;
            let expr = self.parse_expr()?;
            body.insert(self.text(&target), expr);
            if self.peek().is(&TokenType::Comma) {
                self.next();
            }
            if self.peek().is(&TokenType::RBrace) {
                break;
            }
        }
        Ok(body)
    }
    fn parse_paren(&mut self, paren: Token) -> Result<ExprSpan, ParseError> {
        let expr = self.parse_expr()?;
        let Some(end) = self.peek() else {
            return Err(ParseError::UnterminatedParetheses(paren));
        };
        self.check_valid(TokenType::Rparen, end)?;
        self.next();
        Ok(expr)
    }
    fn parse_while_loop(&mut self) -> Result<ExprSpan, ParseError> {
        let first = self.peek_some()?;
        let condition = self.parse_expr()?;
        let last = self.peek_some()?;
        let proc = self.parse_block()?;
        self.next();
        Ok(Expr::While(condition.box_unspanned(), proc).to_spanned((first.span.0, last.span.1)))
    }
    fn parse_constructor(&mut self) -> Result<ExprSpan, ParseError> {
        let first = self.peek_some()?;
        let ident = self.consume(TokenType::Ident)?;
        let name = self.text(&ident);
        let params = self.struct_params()?;
        let last = self.peek_some()?;
        self.next();
        let span = (first.span.0, last.span.1);
        Ok(Expr::Constructor(name, params).to_spanned(span))
    }
    // These Parse variable definitions/declarations
    fn empty_var_decl(&mut self, first: Token, var_name: String) -> ExprSpan {
        let Some(last) = self.peek() else {todo!()};

        Statement::Declaration(
            var_name,
            self.build_expr(Value::Null, first.span).box_unspanned(),
        )
        .to_exprspan((first.span.0, last.span.1))

        // Declaration {
        //     var_name,
        //     value: Value::Null.to_nodespan(first.span).boxed(),
        // }
        // .to_nodespan((first.span.0, last.span.1))
    }
    fn var_decl(
        &mut self,
        var_name: String,
        name_ident: Token,
    ) -> Result<Spanned<Expr>, ParseError> {
        let last = self.next();
        let val = self.parse_expr()?;

        Ok(Statement::Declaration(var_name, val.box_unspanned())
            .to_exprspan((name_ident.span.0, last.unwrap().span.1)))
    }
    fn parse_vardef(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let ident = self.expect(TokenType::Ident)?;
        let var_name = self.text(&ident);
        self.next();
        match self.peek_some()?.kind {
            TokenType::Semicolon => return Ok(self.empty_var_decl(ident, var_name)),
            TokenType::Equal => return self.var_decl(var_name, ident),
            _ => {}
        }
        Err(ParseError::UnexpectedToken(self.peek_some()?))
    }
    // This function parses the parameters of function definitions aka: func >(one,two)<
    fn parse_func_params(&mut self) -> Result<Vec<String>, ParseError> {
        self.next();
        let mut token = self.peek_some()?;
        let mut params: Vec<String> = vec![];
        while token.isnt(&TokenType::Rparen) {
            if self.peek().is(&TokenType::Rparen) {
                break;
            }
            let ident: Token = self.expect(TokenType::Ident)?;
            let var_name = self.text(&ident);
            self.next();
            token = self.peek_some()?;
            params.push(var_name);
            match token.kind {
                TokenType::Rparen => break,
                TokenType::Comma => {
                    self.next();
                    continue;
                }
                _ => {}
            }
            return Err(ParseError::UnexpectedToken(token));
        }
        self.next();
        Ok(params)
    }

    // This creates a function object and creates a Declaration Node
    // this is so it can then be cast into a variable
    fn parse_named_func(&mut self, name_ident: &Token) -> Result<Spanned<Expr>, ParseError> {
        let func_name = self.text(name_ident);
        self.next();
        let params = self.parse_func_params()?;
        let last = self.peek_some()?;
        let func: Value = Function::new(self.parse_block()?, params).into();

        let func_span = (name_ident.span.0, last.span.1);
        Ok(
            Statement::Declaration(func_name, self.build_expr(func, func_span).box_unspanned())
                .to_exprspan(func_span),
        )
    }
    // This creates the function object which is passed as a value
    fn build_func(&mut self) -> Result<Value, ParseError> {
        let params = self.parse_func_params()?;
        let func_block = self.parse_block()?;
        Ok(Function::new(func_block, params).into())
    }
    // This uses build_func to create the function and then converts it into a nodespan
    // this is so it can be used in a block
    fn parse_anon_func(&mut self, first: &Token) -> Result<Spanned<Expr>, ParseError> {
        let func = self.build_func()?;
        let last = self.peek_some()?;
        let span = (first.span.0, last.span.1);
        Ok(self.build_expr(func, span))
    }
    // Takes the aformentioned function and combines them to alow the current function syntax
    fn parse_funcdef(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let first = self.peek_some()?;
        match first.kind {
            TokenType::Ident => return self.parse_named_func(&first),
            TokenType::Lparen => return self.parse_anon_func(&first),
            _ => {}
        };
        Err(ParseError::UnexpectedToken(first))
    }

    fn parse_atom(&mut self, peeked: Option<&Token>) -> Result<Spanned<Expr>, ParseError> {
        let Some(value) = peeked else {return Err(ParseError::UnexpectedStreamEnd);};

        match &value.kind {
            TokenType::True => Ok(self.build_expr(Value::Bool(true), value.span)),
            TokenType::False => Ok(self.build_expr(Value::Bool(false), value.span)),
            TokenType::Num => Ok(self.parse_num(value)),
            TokenType::Str => Ok(self.parse_string(value)),
            TokenType::Ident => Ok(Spanned::new(Expr::Variable(self.text(value)), value.span)),
            TokenType::Null => Ok(self.build_expr(Value::Null, value.span)),
            TokenType::Var => self.parse_vardef(),
            TokenType::Func => {
                let func = self.parse_funcdef()?;
                self.next();
                Ok(func)
            }
            TokenType::While => self.parse_while_loop(),
            TokenType::If => self.parse_branch(),
            TokenType::Do => self.parse_do(),
            TokenType::Loop => self.parse_loop(),
            TokenType::Return => {
                todo!()
                // let expr = self.parse_expr()?;
                // if expr.unspanned == Node::DontResult {
                //     return Ok(Node::ReturnNode(Value::Null.to_nodespan(expr.span).boxed())
                //         .to_spanned(value.span));
                // }
                // Ok(Statement::Return(expr.box_unspanned()).to_exprspan(value.span))
            }
            TokenType::Break => Ok(Statement::Break.to_exprspan(value.span)),
            TokenType::Not | TokenType::Bang => self.unary_operator(UnaryOp::NOT),
            TokenType::Minus => self.unary_operator(UnaryOp::NEGATIVE),
            TokenType::Lparen => self.parse_paren(value.clone()),
            TokenType::New => self.parse_constructor(),
            _ => Err(ParseError::UnexpectedToken(value.clone())),
        }
    }
}
