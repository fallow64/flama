use std::{
    cell::RefCell,
    fmt::{Display, Formatter},
    path::PathBuf,
    rc::Rc,
};

use crate::{
    check::types::Type,
    lexer::token::{Span, Spanned, Token, TokenType},
};

pub type NodePtr<T> = Rc<RefCell<T>>;

pub struct Program {
    pub signatures: Vec<FunctionSignature>,
    pub typedefs: Vec<NodePtr<StructItem>>,
    pub items: Vec<Item>,
    pub path: Rc<PathBuf>,
}

// Creates a new NodePtr<T>. This is a convenience function incase
// we change the implementation of NodePtr<T> in the future.
pub fn new_node_ptr<T>(val: T) -> NodePtr<T> {
    Rc::new(RefCell::new(val))
}

// ------------------------- EXPRESSIONS -------------------------

/// An expression is a node in the AST that can be evaluated to a value.
#[derive(Debug, Clone)]
pub enum Expression {
    Unary(NodePtr<UnaryExpr>),
    Binary(NodePtr<BinaryExpr>),
    Literal(NodePtr<LiteralExpr>),
    List(NodePtr<ListExpr>),
    Name(NodePtr<NameExpr>),
    Instanciate(NodePtr<InstanciateExpr>),
    Call(NodePtr<CallExpr>),
    Assign(NodePtr<AssignExpr>),
    Subscript(NodePtr<SubscriptExpr>),
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
pub struct ListExpr {
    pub init: Token,
    pub elements: Vec<Expression>,
    pub typ: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct NameExpr {
    pub init: Token,
    pub name: Identifier,
    pub typ: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct InstanciateExpr {
    pub init: Token,
    pub name: Identifier,
    pub fields: Vec<(Identifier, Expression)>,
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
    pub name: Identifier,
    pub value: Expression,
    pub typ: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct SubscriptExpr {
    pub init: Token,
    pub object: Expression,
    pub index: Expression,
    pub typ: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct GetExpr {
    pub init: Token,
    pub object: Expression,
    pub name: Identifier,
    pub typ: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct SetExpr {
    pub init: Token,
    pub object: Expression,
    pub name: Identifier,
    pub value: Expression,
    pub typ: Option<Type>,
}

// ------------------------- STATEMENTS -------------------------

/// A statement is a node in the AST that does not evaluate to a value, but rather performs some side effect.
#[derive(Debug, Clone)]
pub enum Statement {
    Block(NodePtr<BlockStmt>),
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

// ------------------------- ITEMS -------------------------

/// An item is a node in the AST that is either directly compiled or represents higher-level features.
#[derive(Debug, Clone)]
pub enum Item {
    Event(NodePtr<EventItem>),
    Function(NodePtr<FunctionItem>),
    Struct(NodePtr<StructItem>),
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
    pub stmts: Vec<Statement>,
    pub signature: FunctionSignature,
}

#[derive(Debug, Clone)]
pub struct StructItem {
    pub init: Token,
    pub name: Identifier,
    pub fields: Vec<(Identifier, TypeExpression)>,
}

// ------------------------- TYPED EXPRESSIONS -------------------------

/// A type expressions is an explicit type annotation in the source code.
/// `init` is not an `Identifier` because it can be a primitive type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeExpression {
    pub init: Token,
    pub typ: Type,
}

impl Display for TypeExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.init.lexeme)
    }
}

// ------------------------ IMPLS --------------------------

impl Expression {
    /// Returns the type of the expression, if it has one.
    pub fn get_type(&self) -> Option<Type> {
        match self {
            Expression::Unary(expr) => expr.borrow().typ.clone(),
            Expression::Binary(expr) => expr.borrow().typ.clone(),
            Expression::Literal(expr) => expr.borrow().typ.clone(),
            Expression::List(expr) => expr.borrow().typ.clone(),
            Expression::Name(expr) => expr.borrow().typ.clone(),
            Expression::Instanciate(expr) => expr.borrow().typ.clone(),
            Expression::Call(expr) => expr.borrow().typ.clone(),
            Expression::Assign(expr) => expr.borrow().typ.clone(),
            Expression::Subscript(expr) => expr.borrow().typ.clone(),
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
            Expression::Literal(expr) => expr.borrow().init.span,
            Expression::List(expr) => expr.borrow().init.span,
            Expression::Name(expr) => expr.borrow().init.span,
            Expression::Instanciate(expr) => expr.borrow().init.span,
            Expression::Call(expr) => expr.borrow().init.span,
            Expression::Assign(expr) => expr.borrow().init.span,
            Expression::Subscript(expr) => expr.borrow().init.span,
            Expression::Get(expr) => expr.borrow().init.span,
            Expression::Set(expr) => expr.borrow().init.span,
        }
    }
}

impl Spanned for Statement {
    fn span(&self) -> Span {
        match self {
            Statement::Block(stmt) => stmt.borrow().init.span,
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
            Item::Struct(sect) => sect.borrow().init.span,
        }
    }
}

impl Spanned for TypeExpression {
    fn span(&self) -> Span {
        self.init.span
    }
}

// ------------------------- UTILITY -------------------------

/// A parameter is a name and type annotation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
    pub name: Identifier,
    pub type_annotation: TypeExpression,
}

/// A field is a name and value.
#[derive(Debug, Clone)]
pub struct Field {
    pub name: Identifier,
    pub value: Expression,
}

impl Display for Parameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.type_annotation)
    }
}

/// A function signature is a name, a list of parameters, and an optional return type.
/// `return_type` is not `Type::Void` because it needs a TypeExpression, which is explicit.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct FunctionSignature {
    pub name: Identifier,
    pub params: Vec<(Identifier, TypeExpression)>,
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
        }
    }
}

impl BinaryOperator {
    /// Returns true if an operator is able to be used in `%math`.
    pub fn is_math_safe(&self) -> bool {
        matches!(
            self,
            BinaryOperator::Add
                | BinaryOperator::Subtract
                | BinaryOperator::Multiply
                | BinaryOperator::Divide
                | BinaryOperator::Modulo
        )
    }
}

/// A literal is a value that can be evaluated at compile time.
#[derive(Debug, Clone)]
pub enum LiteralKind {
    Number(f64),
    String(String),
    Boolean(bool),
}

impl Display for LiteralKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralKind::Number(n) => write!(f, "{}", n),
            LiteralKind::String(s) => write!(f, "\"{}\"", s),
            LiteralKind::Boolean(b) => write!(f, "{}", b),
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

/// The differing types of variables.
/// `Let` is the preffered type, but the others have DF correlations.
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

/// An identifier is a name and a span.
/// Differentiated from Token for convenience.
#[derive(Debug, Clone, Hash, PartialEq, Eq, Default, PartialOrd, Ord)]
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
