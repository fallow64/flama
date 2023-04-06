use std::{mem, path::PathBuf, rc::Rc};

use crate::{
    check::types::Type,
    lexer::{
        token::{Span, Token, TokenType},
        Lexer,
    },
    parser::ast::BinaryExpr,
    ErrorType, FlamaError, FlamaResult,
};

use self::ast::{
    new_node_ptr, AssignExpr, BlockStmt, BreakStmt, CallExpr, ConstItem, ContinueStmt, EventItem,
    Expression, ExpressionStmt, FunctionItem, FunctionSignature, GetExpr, Identifier, IfStmt, Item,
    LetStmt, ListExpr, LiteralExpr, NameExpr, Parameter, PrintStmt, Program, ReturnStmt, SetExpr,
    Statement, TypeExpression, UnaryExpr, VariableType, WhileStmt,
};

pub mod ast;
pub mod ast_printer;
pub mod visitor;

pub struct Parser {
    lexer: Lexer,
    queued_errors: Vec<FlamaError>,
    source_path: Rc<PathBuf>,
    loop_depth: usize,
    in_function: bool,

    current_token: Option<Token>,
    peek_token: Option<Token>,
}

impl Parser {
    pub fn new(lexer: Lexer, source_path: Rc<PathBuf>) -> Self {
        let mut parser = Parser {
            lexer,
            queued_errors: vec![],
            source_path,
            loop_depth: 0,
            in_function: false,
            current_token: None,
            peek_token: None,
        };
        parser.advance();
        parser.advance();

        parser
    }

    pub fn parse_program(self) -> Result<Program, Vec<FlamaError>> {
        let source_path = self.source_path.clone();
        let mut items = vec![];
        let mut errors = vec![];

        let mut signatures = vec![];
        for result in self {
            match result {
                Ok(item) => {
                    if let Item::Function(function) = item.clone() {
                        signatures.push(function.borrow().signature.clone())
                    }
                    items.push(item);
                }
                Err(error) => errors.push(error),
            }
        }

        if errors.is_empty() {
            Ok(Program {
                signatures,
                items,
                path: source_path,
            })
        } else {
            Err(errors)
        }
    }

    // ------------------------- SECTIONS -------------------------

    /// Parses the next item.
    fn parse_item(&mut self) -> FlamaResult<Item> {
        let err = match self
            .current_token
            .as_ref()
            .expect("called parse_item(), but is at end")
            .ttype
        {
            TokenType::Event => match self.parse_event_item() {
                Ok(event) => return Ok(Item::Event(new_node_ptr(event))),
                Err(err) => err,
            },
            TokenType::Function => match self.parse_function_item() {
                Ok(function) => return Ok(Item::Function(new_node_ptr(function))),
                Err(err) => err,
            },
            TokenType::Const => match self.parse_const_item() {
                Ok(constant) => return Ok(Item::Constant(new_node_ptr(constant))),
                Err(err) => err,
            },
            _ => self
                .consume_multiple(&[TokenType::Event, TokenType::Function])
                .expect_err("update consume_multiple()"),
        };

        self.synchronize();

        Err(err)
    }

    /// `"event" <name> <block>`
    fn parse_event_item(&mut self) -> FlamaResult<EventItem> {
        let init = self.consume(TokenType::Event)?;
        let name = self
            .consume_multiple(&[TokenType::Identifier, TokenType::String])?
            .into();
        let block = new_node_ptr(self.parse_block_stmt()?);
        Ok(EventItem {
            init,
            name,
            body: block,
        })
    }

    /// `"fun" <name> "(" <params> ")" <block>`
    fn parse_function_item(&mut self) -> FlamaResult<FunctionItem> {
        let init = self.consume(TokenType::Function)?;
        let name = self.consume(TokenType::Identifier)?.into();

        self.consume(TokenType::LParen)?; // consume the (
        let params = self.get_parameters(TokenType::RParen)?;
        self.advance(); // consume the )

        let return_type = if self.is_match(TokenType::Arrow) {
            self.advance(); // consume the ->
            Some(self.parse_type_expression()?)
        } else {
            None
        };
        self.in_function = true;

        let mut stmts = vec![];
        self.consume(TokenType::LBrace)?;
        while !self.is_match(TokenType::RBrace) {
            stmts.push(self.parse_declaration()?);
        }
        self.consume(TokenType::RBrace)?;
        self.in_function = false;

        Ok(FunctionItem {
            init,
            stmts,
            signature: FunctionSignature {
                name,
                params,
                return_type,
            },
        })
    }

    fn parse_const_item(&mut self) -> FlamaResult<ConstItem> {
        let init = self.consume(TokenType::Const)?;
        let name = self.consume(TokenType::Identifier)?.into();

        let type_annotation = if self.is_match(TokenType::Colon) {
            self.advance();
            Some(self.parse_type_expression()?)
        } else {
            None
        };

        self.consume(TokenType::Assign)?;
        let value = self.parse_expression()?;
        self.consume(TokenType::SemiColon)?;

        Ok(ConstItem {
            init,
            type_annotation,
            name,
            value,
        })
    }

    // ------------------------- STATEMENTS -------------------------

    fn parse_declaration(&mut self) -> FlamaResult<Statement> {
        match self
            .current_token
            .as_ref()
            .expect("called parse_declaration(), but is at end")
            .ttype
        {
            TokenType::Let | TokenType::Local | TokenType::Save | TokenType::Game => {
                Ok(Statement::Let(new_node_ptr(self.parse_let_stmt()?)))
            }
            _ => Ok(self.parse_statement()?),
        }
    }

    /// Parses the next statement.
    fn parse_statement(&mut self) -> FlamaResult<Statement> {
        match self
            .current_token
            .as_ref()
            .expect("called parse_statement(), but is at end")
            .ttype
        {
            TokenType::LBrace => Ok(Statement::Block(new_node_ptr(self.parse_block_stmt()?))),
            TokenType::Print => Ok(Statement::Print(new_node_ptr(self.parse_print_stmt()?))),
            TokenType::If => Ok(Statement::If(new_node_ptr(self.parse_if_stmt()?))),
            TokenType::While => Ok(Statement::While(new_node_ptr(self.parse_while_stmt()?))),
            TokenType::Continue => Ok(Statement::Continue(new_node_ptr(
                self.parse_continue_stmt()?,
            ))),
            TokenType::Break => Ok(Statement::Break(new_node_ptr(self.parse_break_stmt()?))),
            TokenType::Return => Ok(Statement::Return(new_node_ptr(self.parse_return_stmt()?))),
            _ => Ok(Statement::Expression(new_node_ptr(
                self.parse_expression_stmt()?,
            ))),
        }
    }

    /// `"{" <stmt>* "}"`
    fn parse_block_stmt(&mut self) -> FlamaResult<BlockStmt> {
        let init = self.consume(TokenType::LBrace)?;
        let mut stmts = vec![];

        while !self.is_match(TokenType::RBrace) {
            stmts.push(self.parse_declaration()?);
        }
        self.advance(); // consume the }
        Ok(BlockStmt {
            init,
            statements: stmts,
        })
    }

    /// `"print" <expr> ";"`
    fn parse_print_stmt(&mut self) -> FlamaResult<PrintStmt> {
        let init = self.consume(TokenType::Print)?;
        let value = self.parse_expression()?;

        self.consume(TokenType::SemiColon)?;
        Ok(PrintStmt { init, value })
    }

    /// `"if" <expr> <block> ("else" <block>)?`
    fn parse_if_stmt(&mut self) -> FlamaResult<IfStmt> {
        let init = self.consume(TokenType::If)?;

        self.consume(TokenType::LParen)?;
        let condition = self.parse_expression()?;
        self.consume(TokenType::RParen)?;

        let body = self.parse_statement()?;
        let alternative = if self.is_match(TokenType::Else) {
            self.advance();
            Some(self.parse_statement()?)
        } else {
            None
        };

        Ok(IfStmt {
            init,
            condition,
            body,
            alternative,
        })
    }

    /// `"while" <expr> <block>`
    fn parse_while_stmt(&mut self) -> FlamaResult<WhileStmt> {
        let init = self.consume(TokenType::While)?;

        self.loop_depth += 1;

        self.consume(TokenType::LParen)?;
        let condition = self.parse_expression()?;
        self.consume(TokenType::RParen)?;

        let body = self.parse_statement()?;

        self.loop_depth -= 1;

        Ok(WhileStmt {
            init,
            condition,
            body,
        })
    }

    /// `"continue" ";"`
    fn parse_continue_stmt(&mut self) -> FlamaResult<ContinueStmt> {
        let init = self.consume(TokenType::Continue)?;
        self.consume(TokenType::SemiColon)?;

        if self.loop_depth == 0 {
            return Err(
                self.make_error(init.span, "continue statement outside of loop".to_string())
            );
        }

        Ok(ContinueStmt { init })
    }

    /// `"break" ";"`
    fn parse_break_stmt(&mut self) -> FlamaResult<BreakStmt> {
        let init = self.consume(TokenType::Break)?;
        self.consume(TokenType::SemiColon)?;

        if self.loop_depth == 0 {
            return Err(self.make_error(init.span, "break statement outside of loop".to_string()));
        }

        Ok(BreakStmt { init })
    }

    /// `"return" <expr>? ";"`
    fn parse_return_stmt(&mut self) -> FlamaResult<ReturnStmt> {
        let init = self.consume(TokenType::Return)?;

        let value = if self.is_match(TokenType::SemiColon) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.consume(TokenType::SemiColon)?;

        if !self.in_function {
            return Err(self.make_error(
                init.span,
                "return statement outside of function".to_string(),
            ));
        }

        Ok(ReturnStmt { init, value })
    }

    /// `"let" <name> (":" <type>)? ("=" <expr>)? ";"`
    fn parse_let_stmt(&mut self) -> FlamaResult<LetStmt> {
        let init = self.consume_multiple(&[
            TokenType::Let,
            TokenType::Save,
            TokenType::Local,
            TokenType::Save,
            TokenType::Game,
        ])?;

        let kind = VariableType::from_ttype(init.ttype).unwrap();

        let name = self.consume(TokenType::Identifier)?.into();

        let type_annotation = if self.is_match(TokenType::Colon) {
            self.advance();
            Some(self.parse_type_expression()?)
        } else {
            None
        };

        let value = if self.is_match(TokenType::Assign) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.consume(TokenType::SemiColon)?;

        Ok(LetStmt {
            init,
            name,
            type_annotation,
            value,
            kind,
        })
    }

    /// `<expr> ";"`
    fn parse_expression_stmt(&mut self) -> FlamaResult<ExpressionStmt> {
        let value = self.parse_expression()?;
        self.consume(TokenType::SemiColon)?;
        Ok(ExpressionStmt { expression: value })
    }

    // ------------------------- EXPRESSIONS -------------------------

    /// Parses the next expression.
    fn parse_expression(&mut self) -> FlamaResult<Expression> {
        self.parse_assignment()
    }

    /// `<or> ( "=" <or> )*`
    fn parse_assignment(&mut self) -> FlamaResult<Expression> {
        let mut expr = self.parse_or()?;
        if self.is_matches(&[TokenType::Assign]) {
            let operator = self.advance().unwrap();
            let rhs = self.parse_or()?;

            expr = match expr {
                Expression::Name(n) => Expression::Assign(new_node_ptr(AssignExpr {
                    init: operator,
                    name: n.borrow().init.clone(),
                    value: rhs,
                    typ: None,
                })),
                Expression::Get(g) => Expression::Set(new_node_ptr(SetExpr {
                    init: operator,
                    object: g.borrow().object.clone(),
                    name: g.borrow().name.clone(),
                    value: rhs,
                    typ: None,
                })),
                _ => {
                    return Err(
                        self.make_error(operator.span, "invalid assignment target".to_string())
                    )
                }
            }
        }

        Ok(expr)
    }

    /// `<and> ( "||" <and> )*`
    fn parse_or(&mut self) -> FlamaResult<Expression> {
        let mut expr = self.parse_and()?;
        while self.is_matches(&[TokenType::Or]) {
            let operator = self.advance().unwrap();
            let rhs = self.parse_and()?;

            let bin_op = operator
                .ttype
                .get_operator_binary()
                .expect("not binary operator, but is match");

            expr = Expression::Binary(new_node_ptr(BinaryExpr {
                init: operator,
                left: expr,
                operator: bin_op,
                right: rhs,
                typ: None,
            }));
        }

        Ok(expr)
    }

    /// `<equality> ( "&&" <equality> )*`
    fn parse_and(&mut self) -> FlamaResult<Expression> {
        let mut expr = self.parse_equality()?;
        while self.is_matches(&[TokenType::And]) {
            let operator = self.advance().unwrap();
            let rhs = self.parse_equality()?;

            let bin_op = operator
                .ttype
                .get_operator_binary()
                .expect("not binary operator, but is match");

            expr = Expression::Binary(new_node_ptr(BinaryExpr {
                init: operator,
                left: expr,
                operator: bin_op,
                right: rhs,
                typ: None,
            }));
        }

        Ok(expr)
    }

    /// `<relational> ( ("==" | "!=") <relational> )*`
    fn parse_equality(&mut self) -> FlamaResult<Expression> {
        let mut expr = self.parse_relational()?;
        while self.is_matches(&[TokenType::Equals, TokenType::NotEq]) {
            let operator = self.advance().unwrap();
            let rhs = self.parse_relational()?;

            let bin_op = operator
                .ttype
                .get_operator_binary()
                .expect("not binary operator, but is match");

            expr = Expression::Binary(new_node_ptr(BinaryExpr {
                init: operator,
                left: expr,
                operator: bin_op,
                right: rhs,
                typ: None,
            }));
        }

        Ok(expr)
    }

    /// `<term> ( ("<" | "<=" | ">" | ">=") <term> )*`
    fn parse_relational(&mut self) -> FlamaResult<Expression> {
        let mut expr = self.parse_term()?;
        while self.is_matches(&[
            TokenType::Greater,
            TokenType::GreaterEq,
            TokenType::Less,
            TokenType::LessEq,
        ]) {
            let operator = self.advance().unwrap();
            let rhs = self.parse_term()?;

            let bin_op = operator
                .ttype
                .get_operator_binary()
                .expect("not binary operator, but is match");

            expr = Expression::Binary(new_node_ptr(BinaryExpr {
                init: operator,
                left: expr,
                operator: bin_op,
                right: rhs,
                typ: None,
            }));
        }

        Ok(expr)
    }

    /// `<factor> ( ("+" | "-") <factor> )*`
    fn parse_term(&mut self) -> FlamaResult<Expression> {
        let mut expr = self.parse_factor()?;
        while self.is_matches(&[TokenType::Plus, TokenType::Minus]) {
            let operator = self.advance().unwrap();
            let rhs = self.parse_factor()?;

            let bin_op = operator
                .ttype
                .get_operator_binary()
                .expect("not binary operator, but is match");

            expr = Expression::Binary(new_node_ptr(BinaryExpr {
                init: operator,
                left: expr,
                operator: bin_op,
                right: rhs,
                typ: None,
            }));
        }

        Ok(expr)
    }

    /// `<unary> ( ("*" | "/" | "%") <unary> )*`
    fn parse_factor(&mut self) -> FlamaResult<Expression> {
        let mut expr = self.parse_unary()?;
        while self.is_matches(&[TokenType::Star, TokenType::Slash, TokenType::Modulo]) {
            let operator = self.advance().unwrap();
            let rhs = self.parse_unary()?;

            let bin_op = operator
                .ttype
                .get_operator_binary()
                .expect("not binary operator, but is match");

            expr = Expression::Binary(new_node_ptr(BinaryExpr {
                init: operator,
                left: expr,
                operator: bin_op,
                right: rhs,
                typ: None,
            }));
        }

        Ok(expr)
    }

    /// `(("!" | "-" | "+") <unary>) | <call>`
    fn parse_unary(&mut self) -> FlamaResult<Expression> {
        if self.is_matches(&[TokenType::Plus, TokenType::Minus, TokenType::Not]) {
            let operator = self.advance().unwrap();
            let rhs = self.parse_unary()?;

            let un_op = operator
                .ttype
                .get_operator_unary()
                .expect("not unary operator, but is match");

            let expr = Expression::Unary(new_node_ptr(UnaryExpr {
                init: operator,
                operator: un_op,
                right: rhs,
                typ: None,
            }));

            Ok(expr)
        } else {
            self.parse_call()
        }
    }

    /// `<primary> ( ( "(" <expression>* ")" ) | ("." <identifier> ) )*`
    fn parse_call(&mut self) -> FlamaResult<Expression> {
        let mut expr = self.parse_primary()?;

        // handle calling functions and "get" expressions
        loop {
            if self.is_match(TokenType::Dot) {
                let init = self.consume(TokenType::Dot)?;
                let name: Identifier = self.consume(TokenType::Identifier)?.into();

                expr = Expression::Get(new_node_ptr(GetExpr {
                    init,
                    object: expr,
                    name,
                    typ: None,
                }));
            } else if self.is_match(TokenType::LParen) {
                let init = self.consume(TokenType::LParen)?;

                let args = self.get_arguments(TokenType::RParen)?;

                self.consume(TokenType::RParen)?;

                expr = Expression::Call(new_node_ptr(CallExpr {
                    init,
                    callee: expr,
                    args,
                    typ: None,
                }));
            } else {
                break;
            }
        }

        Ok(expr)
    }

    /// `"(" <expression> ")" | <string> | <number> | <identifier> | ("true" | "false")`
    fn parse_primary(&mut self) -> FlamaResult<Expression> {
        let init = self
            .advance()
            .expect("called parse_primary(), but is at end");
        match init.ttype {
            TokenType::LParen => {
                let expr = self.parse_expression()?;
                self.consume(TokenType::RParen)?;
                Ok(expr)
            }
            TokenType::String => {
                let lexeme = &init.lexeme[1..init.lexeme.len() - 1];
                let unescaped = Self::unescape_string(lexeme);

                let expr = Expression::Literal(new_node_ptr(LiteralExpr {
                    init,
                    kind: unescaped.into(),
                    typ: None,
                }));
                Ok(expr)
            }
            TokenType::Number => {
                let number = init.lexeme.parse::<f64>().expect("invalid number parsed?");

                let expr = Expression::Literal(new_node_ptr(LiteralExpr {
                    init,
                    kind: number.into(),
                    typ: None,
                }));
                Ok(expr)
            }
            TokenType::Identifier => {
                let name = init.clone().into();

                let expr = Expression::Name(new_node_ptr(NameExpr {
                    init,
                    name,
                    typ: None,
                }));
                Ok(expr)
            }
            TokenType::True | TokenType::False => {
                let value = init.ttype == TokenType::True;

                let expr = Expression::Literal(new_node_ptr(LiteralExpr {
                    init,
                    kind: value.into(),
                    typ: None,
                }));
                Ok(expr)
            }
            TokenType::LBracket => {
                let elements = self.get_arguments(TokenType::RBracket)?;

                self.consume(TokenType::RBracket)?;

                let expr = Expression::List(new_node_ptr(ListExpr {
                    init,
                    elements,
                    typ: None,
                }));

                Ok(expr)
            }
            _ => Err(self.make_error(init.span, "expected expression".to_string())),
        }
    }

    // ------------------------- TYPE EXPRESSIONS -------------------------

    fn parse_type_expression(&mut self) -> FlamaResult<TypeExpression> {
        let init = self
            .advance()
            .expect("called parse_type_expression(), but is at end");

        let typ = match init.ttype {
            TokenType::TypeNumber => Type::Number,
            TokenType::TypeString => Type::String,
            TokenType::TypeBoolean => Type::Boolean,
            TokenType::TypeList => {
                self.consume(TokenType::Less)?;
                let inner = self.parse_type_expression()?;
                self.consume(TokenType::Greater)?;
                Type::List(Box::new(inner.typ))
            }
            TokenType::Identifier => Type::Identifier(init.clone().into()),
            _ => {
                return Err(self.make_error_expected(
                    &[
                        TokenType::TypeNumber,
                        TokenType::TypeString,
                        TokenType::TypeBoolean,
                        TokenType::TypeList,
                        TokenType::Identifier,
                    ],
                    self.current_token.as_ref().map(|tok| tok.ttype),
                ))
            }
        };

        Ok(TypeExpression { init, typ })
    }

    // ------------------------- HELPER FUNCTIONS -------------------------

    fn synchronize(&mut self) {
        while self.current_token.is_some() {
            if self.advance().unwrap().ttype == TokenType::SemiColon {
                return;
            }

            match self.peek_token.as_ref() {
                Some(tok) => match tok.ttype {
                    TokenType::Function
                    | TokenType::Let
                    | TokenType::If
                    | TokenType::While
                    | TokenType::Print
                    | TokenType::Event
                    | TokenType::Return => return,
                    _ => (),
                },
                None => (),
            }

            self.advance();
        }
    }

    /// Returns a vector of parameters. Does not consume the opening or closing tokens.
    fn get_parameters(&mut self, ending_type: TokenType) -> FlamaResult<Vec<Parameter>> {
        let mut params = vec![];

        while !self.is_match(ending_type) {
            let name = self.consume(TokenType::Identifier)?.into();
            self.consume(TokenType::Colon)?;
            let type_annotation = self.parse_type_expression()?;

            params.push(Parameter {
                name,
                type_annotation,
            });

            if !self.is_match(ending_type) {
                self.consume(TokenType::Comma)?;
            }
        }
        Ok(params)
    }

    /// Returns a vector of expressions. Does not consume the opening or closing tokens.
    fn get_arguments(&mut self, ending_type: TokenType) -> FlamaResult<Vec<Expression>> {
        let mut args = vec![];

        while !self.is_match(ending_type) {
            let expr = self.parse_expression()?;
            args.push(expr);

            if !self.is_match(ending_type) {
                self.consume(TokenType::Comma)?;
            }
        }

        Ok(args)
    }

    /// Returns true if the current token is one of the given types.
    fn is_matches(&self, ttypes: &[TokenType]) -> bool {
        for ttype in ttypes {
            if self.is_match(*ttype) {
                return true;
            }
        }
        false
    }

    /// Returns true if the current token is of the given type.
    fn is_match(&self, ttype: TokenType) -> bool {
        self.current_token
            .as_ref()
            .map_or(false, |tok| tok.ttype == ttype)
    }

    /// Consumes the current token if it is one of the given types and returns it.
    fn consume_multiple(&mut self, ttypes: &[TokenType]) -> FlamaResult<Token> {
        if let Some(tok) = self.current_token.as_ref() {
            if ttypes.contains(&tok.ttype) {
                let consumed = self.advance().unwrap();
                Ok(consumed)
            } else {
                Err(self.make_error_expected(ttypes, Some(tok.ttype)))
            }
        } else {
            Err(self.make_error_expected(ttypes, None))
        }
    }

    /// Consumes the current token if it is of the given type and returns it.
    fn consume(&mut self, ttype: TokenType) -> FlamaResult<Token> {
        if let Some(tok) = self.current_token.as_ref() {
            if tok.ttype == ttype {
                let consumed = self.advance().unwrap();
                Ok(consumed)
            } else {
                Err(self.make_error_expected(&[ttype], Some(tok.ttype)))
            }
        } else {
            Err(self.make_error_expected(&[ttype], None))
        }
    }

    /// Advances the parser by one token and returns the previous token.
    fn advance(&mut self) -> Option<Token> {
        let next_token = self.lexer.next().map_or(None, |result| match result {
            Ok(tok) => Some(tok),
            Err(err) => {
                self.queued_errors.push(err);
                None
            }
        });

        mem::replace(
            &mut self.current_token,
            mem::replace(&mut self.peek_token, next_token),
        )
    }

    /// Unescapes a string literal.
    fn unescape_string(s: &str) -> String {
        let mut result = String::new();
        let mut chars = s.chars().peekable();

        while let Some(c) = chars.next() {
            if c == '\\' {
                match chars.next() {
                    Some('n') => result.push('\n'),
                    Some('r') => result.push('\r'),
                    Some('t') => result.push('\t'),
                    Some('\'') => result.push('\''),
                    Some('\"') => result.push('\"'),
                    Some('\\') => result.push('\\'),
                    Some(c) => {
                        result.push('\\');
                        result.push(c)
                    }
                    None => break,
                }
            } else {
                result.push(c);
            }
        }

        result
    }

    // errors

    /// Creates an error for an unexpected token.
    fn make_error_expected(&self, ttypes: &[TokenType], got: Option<TokenType>) -> FlamaError {
        let span = if let Some(tok) = &self.current_token {
            tok.span
        } else {
            Span::default()
        };

        let mut msgs: Vec<String> = ttypes.iter().map(|ttype| ttype.to_string()).collect();

        let error_message = match msgs.len() {
            0 => "none".to_string(),
            1 => msgs.pop().unwrap(),
            2 => format!("{} or {}", msgs[0], msgs[1]),
            _ => {
                let last = msgs.pop().unwrap();

                let seperated = msgs.join(", ");

                format!("{}, or {}", seperated, last)
            }
        };

        self.make_error(
            span,
            format!(
                "expected {}, but got {}",
                error_message,
                got.map_or("none".to_string(), |t| t.to_string())
            ),
        )
    }

    /// Creates an error with type `ErrorType::Parsing`.
    fn make_error(&self, span: Span, message: String) -> FlamaError {
        FlamaError {
            message,
            span,
            error_type: ErrorType::Parsing,
            source_path: self.source_path.clone(),
        }
    }
}

impl Iterator for Parser {
    type Item = FlamaResult<Item>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_token.is_none() {
            None
        } else if let Some(err) = self.queued_errors.pop() {
            Some(Err(err))
        } else {
            Some(self.parse_item())
        }
    }
}

mod tests {
    #[test]
    fn test_unescape_string() {
        assert_eq!(super::Parser::unescape_string("hello"), "hello");
        assert_eq!(super::Parser::unescape_string("hello\\n"), "hello\n");
        assert_eq!(
            super::Parser::unescape_string("\\r \\t \\' \\\" \\\\ \\a"),
            "\r \t \' \" \\ \\a"
        );
    }
}
