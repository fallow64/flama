use std::rc::Rc;

use crate::lexer::token::{Span, Spanned, Token};

pub type NodePtr<T> = Rc<T>;

pub type Program = Vec<NodePtr<Declaration>>;

// creates a new `NodePtr` from a value
// create them like this incase we need to change the implementation
pub fn new_node_ptr<T>(val: T) -> NodePtr<T> {
    Rc::new(val)
}

// ------------------------- EXPRESSIONS -------------------------

#[derive(Debug)]
pub enum Expression {
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Literal(LiteralExpr),
    Name(NameExpr),
    Call(CallExpr),
    Assign(AssignExpr),
    Get(GetExpr),
    Set(SetExpr),
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub init: Token,
    pub operator: UnaryOperator,
    pub right: NodePtr<Expression>,
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub init: Token,
    pub left: NodePtr<Expression>,
    pub operator: BinaryOperator,
    pub right: NodePtr<Expression>,
}

#[derive(Debug)]
pub struct LiteralExpr {
    pub init: Token,
    pub kind: LiteralKind,
}

#[derive(Debug)]
pub struct NameExpr {
    pub init: Token,
    pub name: String,
}

#[derive(Debug)]
pub struct CallExpr {
    pub init: Token,
    pub callee: NodePtr<Expression>,
    pub args: Vec<NodePtr<Expression>>,
}

#[derive(Debug)]
pub struct AssignExpr {
    pub init: Token,
    pub name: Token,
    pub value: NodePtr<Expression>,
}

#[derive(Debug)]
pub struct GetExpr {
    pub init: Token,
    pub object: NodePtr<Expression>,
    pub name: Token,
}

#[derive(Debug)]
pub struct SetExpr {
    pub init: Token,
    pub object: NodePtr<Expression>,
    pub name: Token,
    pub value: NodePtr<Expression>,
}

// ------------------------- STATEMENTS -------------------------

#[derive(Debug)]
pub enum Statement {
    Block(BlockStmt),
    Print(PrintStmt),
    If(IfStmt),
    While(WhileStmt),
    Continue(ContinueStmt),
    Break(BreakStmt),
    Return(ReturnStmt),
    Let(LetStmt),
    Expression(ExpressionStmt),
}

#[derive(Debug)]
pub struct BlockStmt {
    pub init: Token,
    pub stmts: Vec<NodePtr<Statement>>,
}

#[derive(Debug)]
pub struct PrintStmt {
    pub init: Token,
    pub values: Vec<NodePtr<Expression>>,
}

#[derive(Debug)]
pub struct IfStmt {
    pub init: Token,
    pub condition: NodePtr<Expression>,
    pub consequence: NodePtr<BlockStmt>,
    pub alternative: Option<NodePtr<BlockStmt>>,
}

#[derive(Debug)]
pub struct WhileStmt {
    pub init: Token,
    pub condition: NodePtr<Expression>,
    pub consequence: NodePtr<BlockStmt>,
}

#[derive(Debug)]
pub struct ContinueStmt {
    pub init: Token,
}

#[derive(Debug)]
pub struct BreakStmt {
    pub init: Token,
}

#[derive(Debug)]
pub struct ReturnStmt {
    pub init: Token,
    pub value: Option<NodePtr<Expression>>,
}

#[derive(Debug)]
pub struct LetStmt {
    pub init: Token,
    pub name: Token,
    pub type_annotation: Option<TypeExpression>,
    pub value: Option<NodePtr<Expression>>,
}

#[derive(Debug)]
pub struct ExpressionStmt {
    pub value: NodePtr<Expression>,
}

// ------------------------- DECLARATIONS -------------------------

#[derive(Debug)]
pub enum Declaration {
    Event(EventDecl),
    Function(FunctionDecl),
}

#[derive(Debug)]
pub struct EventDecl {
    pub init: Token,
    pub name: Token,
    pub block: NodePtr<BlockStmt>,
}

#[derive(Debug)]
pub struct FunctionDecl {
    pub init: Token,
    pub signature: FunctionSignature,
    pub block: NodePtr<BlockStmt>,
}

// ------------------------- TYPED EXPRESSIONS -------------------------

#[derive(Debug)]
pub enum TypeExpression {
    // TODO: node pointers?
    Number(Token),
    String(Token),
    Boolean(Token),
    Identifier(Token),
}

// ------------------------ TRAITS --------------------------
pub trait Visitor {
    type ExpressionOutput;
    type StatementOutput;
    type DeclarationOutput;
    type TypeExpressionOutput;

    fn visit_expression(&mut self, expr: &Expression) -> Self::ExpressionOutput;
    fn visit_statement(&mut self, stmt: &Statement) -> Self::StatementOutput;
    fn visit_declaration(&mut self, decl: &Declaration) -> Self::DeclarationOutput;
    fn visit_type_expression(&mut self, type_expr: &TypeExpression) -> Self::TypeExpressionOutput;
}

// ------------------------ SPANNED IMPLEMENTATIONS --------------------------

impl Spanned for Expression {
    fn span(&self) -> Span {
        match self {
            Expression::Unary(expr) => expr.init.span,
            Expression::Binary(expr) => expr.init.span,
            Expression::Literal(literal) => literal.init.span,
            Expression::Name(expr) => expr.init.span,
            Expression::Call(expr) => expr.init.span,
            Expression::Assign(expr) => expr.init.span,
            Expression::Get(expr) => expr.init.span,
            Expression::Set(expr) => expr.init.span,
        }
    }
}

impl Spanned for Statement {
    fn span(&self) -> Span {
        match self {
            Statement::Block(stmt) => stmt.init.span,
            Statement::Print(stmt) => stmt.init.span,
            Statement::If(stmt) => stmt.init.span,
            Statement::While(stmt) => stmt.init.span,
            Statement::Continue(stmt) => stmt.init.span,
            Statement::Break(stmt) => stmt.init.span,
            Statement::Return(stmt) => stmt.init.span,
            Statement::Let(stmt) => stmt.init.span,
            Statement::Expression(stmt) => stmt.value.span(),
        }
    }
}

impl Spanned for Declaration {
    fn span(&self) -> Span {
        match self {
            Declaration::Event(sect) => sect.init.span,
            Declaration::Function(sect) => sect.init.span,
        }
    }
}

impl Spanned for TypeExpression {
    fn span(&self) -> Span {
        match self {
            TypeExpression::Number(s) => s.span,
            TypeExpression::String(s) => s.span,
            TypeExpression::Boolean(s) => s.span,
            TypeExpression::Identifier(s) => s.span,
        }
    }
}

// ------------------------- UTILITY -------------------------

#[derive(Debug)]
pub struct Parameter {
    pub name: Token,
    pub type_annotation: TypeExpression,
}

#[derive(Debug)]
pub struct FunctionSignature {
    pub name: Token,
    pub params: Vec<Parameter>,
    pub return_type: Option<TypeExpression>,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Identity,
    Negate,
    Not,
    Increment,
    Decrement,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Divide,
    Multiply,
    Modulo,
    Equals,
    NotEq,
    Greater,
    GreaterEq,
    Less,
    LessEq,
    Or,
    And,
    Assign,
}

#[derive(Debug)]
pub enum LiteralKind {
    Number(f64),
    String(String),
    Boolean(bool),
}

impl From<f64> for LiteralKind {
    fn from(value: f64) -> Self {
        LiteralKind::Number(value)
    }
}

impl From<String> for LiteralKind {
    fn from(value: String) -> Self {
        LiteralKind::String(value)
    }
}

impl From<bool> for LiteralKind {
    fn from(value: bool) -> Self {
        LiteralKind::Boolean(value)
    }
}
