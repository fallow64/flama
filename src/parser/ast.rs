use std::{
    cell::RefCell,
    fmt::{Display, Formatter},
    rc::Rc,
};

use crate::lexer::token::{Span, Spanned, Token, TokenType};

pub type NodePtr<T> = Rc<RefCell<T>>;

// TODO: Program should be a struct with the different types, path, etc...
pub type Program = Vec<NodePtr<Item>>;

// creates a new `NodePtr` from a value
// create them like this incase we need to change the implementation
pub fn new_node_ptr<T>(val: T) -> NodePtr<T> {
    Rc::new(RefCell::new(val))
}

// ------------------------- EXPRESSIONS -------------------------

#[derive(Debug, Clone)]
pub enum Expression {
    Unary(NodePtr<UnaryExpr>),
    Binary(NodePtr<BinaryExpr>),
    Literal(NodePtr<LiteralExpr>),
    Name(NodePtr<NameExpr>),
    Call(NodePtr<CallExpr>),
    Assign(NodePtr<AssignExpr>),
    Get(NodePtr<GetExpr>),
    Set(NodePtr<SetExpr>),
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub init: Token,
    pub operator: UnaryOperator,
    pub right: Expression,
    pub typ: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub init: Token,
    pub left: Expression,
    pub operator: BinaryOperator,
    pub right: Expression,
    pub typ: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct LiteralExpr {
    pub init: Token,
    pub kind: LiteralKind,
    pub typ: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct NameExpr {
    pub init: Token,
    pub name: Identifier,
    pub typ: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub init: Token,
    pub callee: Expression,
    pub args: Vec<Expression>,
    pub typ: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct AssignExpr {
    pub init: Token,
    pub name: Token,
    pub value: Expression,
    pub typ: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct GetExpr {
    pub init: Token,
    pub object: Expression,
    pub name: Token,
    pub typ: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct SetExpr {
    pub init: Token,
    pub object: Expression,
    pub name: Token,
    pub value: Expression,
    pub typ: Option<Type>,
}

// ------------------------- STATEMENTS -------------------------

#[derive(Debug, Clone)]
pub enum Statement {
    Block(NodePtr<BlockStmt>),
    Print(NodePtr<PrintStmt>),
    If(NodePtr<IfStmt>),
    While(NodePtr<WhileStmt>),
    Continue(NodePtr<ContinueStmt>),
    Break(NodePtr<BreakStmt>),
    Return(NodePtr<ReturnStmt>),
    Let(NodePtr<LetStmt>),
    Expression(NodePtr<ExpressionStmt>),
}

#[derive(Debug, Clone)]
pub struct BlockStmt {
    pub init: Token,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct PrintStmt {
    pub init: Token,
    pub values: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub init: Token,
    pub condition: Expression,
    pub body: Statement,
    pub alternative: Option<Statement>,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub init: Token,
    pub condition: Expression,
    pub body: Statement,
}

#[derive(Debug, Clone)]
pub struct ContinueStmt {
    pub init: Token,
}

#[derive(Debug, Clone)]
pub struct BreakStmt {
    pub init: Token,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub init: Token,
    pub value: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub init: Token,
    pub name: Identifier,
    pub type_annotation: Option<TypeExpression>,
    pub value: Option<Expression>,
    pub kind: VariableType,
}

#[derive(Debug, Clone)]
pub struct ExpressionStmt {
    pub expression: Expression,
}

// ------------------------- SECTIONS -------------------------

#[derive(Debug, Clone)]
pub enum Item {
    Event(NodePtr<EventItem>),
    Function(NodePtr<FunctionItem>),
    Constant(NodePtr<ConstItem>),
}

#[derive(Debug, Clone)]
pub struct EventItem {
    pub init: Token,
    pub name: Identifier,
    pub body: NodePtr<BlockStmt>,
}

#[derive(Debug, Clone)]
pub struct FunctionItem {
    pub init: Token,
    pub signature: FunctionSignature,
    pub body: NodePtr<BlockStmt>,
}

#[derive(Debug, Clone)]
pub struct ConstItem {
    pub init: Token,
    pub name: Identifier,
    pub type_annotation: Option<TypeExpression>,
    pub value: Expression,
}

// ------------------------- TYPED EXPRESSIONS -------------------------

#[derive(Debug, Clone)]
pub struct TypeExpression {
    pub init: Token,
    pub typ: Type,
    // Number(Token),
    // String(Token),
    // Boolean(Token),
    // Identifier(Token),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Default)]
pub enum Type {
    Number,
    String,
    Boolean,
    Identifier(Identifier),
    Vector,
    #[default]
    Void,
}

impl From<LiteralKind> for Type {
    fn from(kind: LiteralKind) -> Self {
        match kind {
            LiteralKind::Number(_) => Type::Number,
            LiteralKind::String(_) => Type::String,
            LiteralKind::Boolean(_) => Type::Boolean,
            LiteralKind::Vector(_, _, _) => Type::Vector,
        }
    }
}

impl Type {
    #[allow(dead_code)]
    pub fn is_primitive(&self) -> bool {
        matches!(self, Type::Number | Type::String | Type::Boolean)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Number => write!(f, "number"),
            Type::String => write!(f, "string"),
            Type::Boolean => write!(f, "boolean"),
            Type::Identifier(id) => write!(f, "{}", id),
            Type::Vector => write!(f, "vector"),
            Type::Void => write!(f, "void"),
        }
    }
}

impl Display for TypeExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.init.lexeme)
    }
}

// ------------------------ IMPLEMENTATIONS --------------------------

impl Expression {
    pub fn get_type(&self) -> Option<Type> {
        match self {
            Expression::Unary(expr) => expr.borrow().typ.clone(),
            Expression::Binary(expr) => expr.borrow().typ.clone(),
            Expression::Literal(literal) => literal.borrow().typ.clone(),
            Expression::Name(expr) => expr.borrow().typ.clone(),
            Expression::Call(expr) => expr.borrow().typ.clone(),
            Expression::Assign(expr) => expr.borrow().typ.clone(),
            Expression::Get(expr) => expr.borrow().typ.clone(),
            Expression::Set(expr) => expr.borrow().typ.clone(),
        }
    }
}

impl Spanned for Expression {
    fn span(&self) -> Span {
        match self {
            Expression::Unary(expr) => expr.borrow().init.span,
            Expression::Binary(expr) => expr.borrow().init.span,
            Expression::Literal(literal) => literal.borrow().init.span,
            Expression::Name(expr) => expr.borrow().init.span,
            Expression::Call(expr) => expr.borrow().init.span,
            Expression::Assign(expr) => expr.borrow().init.span,
            Expression::Get(expr) => expr.borrow().init.span,
            Expression::Set(expr) => expr.borrow().init.span,
        }
    }
}

impl Spanned for Statement {
    fn span(&self) -> Span {
        match self {
            Statement::Block(stmt) => stmt.borrow().init.span,
            Statement::Print(stmt) => stmt.borrow().init.span,
            Statement::If(stmt) => stmt.borrow().init.span,
            Statement::While(stmt) => stmt.borrow().init.span,
            Statement::Continue(stmt) => stmt.borrow().init.span,
            Statement::Break(stmt) => stmt.borrow().init.span,
            Statement::Return(stmt) => stmt.borrow().init.span,
            Statement::Let(stmt) => stmt.borrow().init.span,
            Statement::Expression(stmt) => stmt.borrow().expression.span(),
        }
    }
}

impl Spanned for Item {
    fn span(&self) -> Span {
        match self {
            Item::Event(sect) => sect.borrow().init.span,
            Item::Function(sect) => sect.borrow().init.span,
            Item::Constant(sect) => sect.borrow().init.span,
        }
    }
}

impl Spanned for TypeExpression {
    fn span(&self) -> Span {
        self.init.span
    }
}

// ------------------------- UTILITY -------------------------

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: Token,
    pub type_annotation: TypeExpression,
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: Identifier,
    pub params: Vec<Parameter>,
    pub return_type: Option<TypeExpression>,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Identity,
    Negate,
    Not,
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Identity => write!(f, "+"),
            UnaryOperator::Negate => write!(f, "-"),
            UnaryOperator::Not => write!(f, "!"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
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

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "+"),
            BinaryOperator::Subtract => write!(f, "-"),
            BinaryOperator::Divide => write!(f, "/"),
            BinaryOperator::Multiply => write!(f, "*"),
            BinaryOperator::Modulo => write!(f, "%"),
            BinaryOperator::Equals => write!(f, "=="),
            BinaryOperator::NotEq => write!(f, "!="),
            BinaryOperator::Greater => write!(f, ">"),
            BinaryOperator::GreaterEq => write!(f, ">="),
            BinaryOperator::Less => write!(f, "<"),
            BinaryOperator::LessEq => write!(f, "<="),
            BinaryOperator::Or => write!(f, "||"),
            BinaryOperator::And => write!(f, "&&"),
            BinaryOperator::Assign => write!(f, "="),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LiteralKind {
    Number(f64),
    String(String),
    Boolean(bool),
    Vector(f64, f64, f64),
}

impl Display for LiteralKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralKind::Number(n) => write!(f, "{}", n),
            LiteralKind::String(s) => write!(f, "\"{}\"", s),
            LiteralKind::Boolean(b) => write!(f, "{}", b),
            LiteralKind::Vector(x, y, z) => write!(f, "<{}, {}, {}>", x, y, z),
        }
    }
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

impl From<(f64, f64, f64)> for LiteralKind {
    fn from(value: (f64, f64, f64)) -> Self {
        LiteralKind::Vector(value.0, value.1, value.2)
    }
}

#[derive(Debug, Clone)]
pub enum VariableType {
    Save,
    Local,
    Game,
    Let,
}

impl VariableType {
    pub fn from_ttype(ttype: TokenType) -> Option<VariableType> {
        match ttype {
            TokenType::Save => Some(VariableType::Save),
            TokenType::Local => Some(VariableType::Local),
            TokenType::Game => Some(VariableType::Game),
            TokenType::Let => Some(VariableType::Let),
            _ => None,
        }
    }
}

impl Display for VariableType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            VariableType::Save => write!(f, "save"),
            VariableType::Local => write!(f, "local"),
            VariableType::Game => write!(f, "game"),
            VariableType::Let => write!(f, "let"),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl From<Token> for Identifier {
    fn from(token: Token) -> Self {
        Identifier {
            name: token.lexeme,
            span: token.span,
        }
    }
}
