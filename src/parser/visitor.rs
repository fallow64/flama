use crate::error::FlamaResult;

use super::ast::{
    AssignExpr, BinaryExpr, BlockStmt, BreakStmt, CallExpr, ContinueStmt, EventItem, Expression,
    ExpressionStmt, FunctionItem, GetExpr, IfStmt, InstanciateExpr, Item, LetStmt, ListExpr,
    LiteralExpr, NameExpr, Node, ReturnStmt, SetExpr, Statement, StructItem, SubscriptExpr,
    UnaryExpr, WhileStmt,
};

/// A trait for visiting AST nodes.
/// This could be seperated out into 3 seperate traits, but that adds
/// more hastle when 99% of the time you want to visit all 3.
pub trait Visitor {
    type ExpressionOutput;
    type StatementOutput;
    type ItemOutput;

    fn visit_unary_expr(&mut self, expr: Node<UnaryExpr>)
        -> FlamaResult<Self::ExpressionOutput>;
    fn visit_binary_expr(
        &mut self,
        expr: Node<BinaryExpr>,
    ) -> FlamaResult<Self::ExpressionOutput>;
    fn visit_literal_expr(
        &mut self,
        expr: Node<LiteralExpr>,
    ) -> FlamaResult<Self::ExpressionOutput>;
    fn visit_list_expr(&mut self, expr: Node<ListExpr>) -> FlamaResult<Self::ExpressionOutput>;
    fn visit_name_expr(&mut self, expr: Node<NameExpr>) -> FlamaResult<Self::ExpressionOutput>;
    fn visit_instanciate_expr(
        &mut self,
        expr: Node<InstanciateExpr>,
    ) -> FlamaResult<Self::ExpressionOutput>;
    fn visit_call_expr(&mut self, expr: Node<CallExpr>) -> FlamaResult<Self::ExpressionOutput>;
    fn visit_assign_expr(
        &mut self,
        expr: Node<AssignExpr>,
    ) -> FlamaResult<Self::ExpressionOutput>;
    fn visit_subscript_expr(
        &mut self,
        expr: Node<SubscriptExpr>,
    ) -> FlamaResult<Self::ExpressionOutput>;
    fn visit_get_expr(&mut self, expr: Node<GetExpr>) -> FlamaResult<Self::ExpressionOutput>;
    fn visit_set_expr(&mut self, expr: Node<SetExpr>) -> FlamaResult<Self::ExpressionOutput>;

    fn visit_block_stmt(&mut self, stmt: Node<BlockStmt>) -> FlamaResult<Self::StatementOutput>;
    fn visit_if_stmt(&mut self, stmt: Node<IfStmt>) -> FlamaResult<Self::StatementOutput>;
    fn visit_while_stmt(&mut self, stmt: Node<WhileStmt>) -> FlamaResult<Self::StatementOutput>;
    fn visit_continue_stmt(
        &mut self,
        stmt: Node<ContinueStmt>,
    ) -> FlamaResult<Self::StatementOutput>;
    fn visit_break_stmt(&mut self, stmt: Node<BreakStmt>) -> FlamaResult<Self::StatementOutput>;
    fn visit_return_stmt(
        &mut self,
        stmt: Node<ReturnStmt>,
    ) -> FlamaResult<Self::StatementOutput>;
    fn visit_let_stmt(&mut self, stmt: Node<LetStmt>) -> FlamaResult<Self::StatementOutput>;
    fn visit_expression_stmt(
        &mut self,
        stmt: Node<ExpressionStmt>,
    ) -> FlamaResult<Self::StatementOutput>;

    fn visit_event_item(&mut self, decl: Node<EventItem>) -> FlamaResult<Self::ItemOutput>;
    fn visit_function_item(&mut self, decl: Node<FunctionItem>)
        -> FlamaResult<Self::ItemOutput>;
    fn visit_struct_item(&mut self, decl: Node<StructItem>) -> FlamaResult<Self::ItemOutput>;
}

/// Double dispatch for visiting expression AST nodes.
pub trait ExpressionVisitable {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput>;
}

/// Double dispatch for visiting statement AST nodes.
pub trait StatementVisitable {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput>;
}

/// Double dispatch for visiting item AST nodes.
pub trait ItemVisitable {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ItemOutput>;
}

impl ExpressionVisitable for Expression {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        match self {
            Expression::Unary(expr) => visitor.visit_unary_expr(expr.clone()),
            Expression::Binary(expr) => visitor.visit_binary_expr(expr.clone()),
            Expression::Literal(expr) => visitor.visit_literal_expr(expr.clone()),
            Expression::List(expr) => visitor.visit_list_expr(expr.clone()),
            Expression::Name(expr) => visitor.visit_name_expr(expr.clone()),
            Expression::Instanciate(expr) => visitor.visit_instanciate_expr(expr.clone()),
            Expression::Call(expr) => visitor.visit_call_expr(expr.clone()),
            Expression::Assign(expr) => visitor.visit_assign_expr(expr.clone()),
            Expression::Subscript(expr) => visitor.visit_subscript_expr(expr.clone()),
            Expression::Get(expr) => visitor.visit_get_expr(expr.clone()),
            Expression::Set(expr) => visitor.visit_set_expr(expr.clone()),
        }
    }
}

impl ExpressionVisitable for Node<Expression> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        self.borrow().accept(visitor)
    }
}

impl StatementVisitable for Statement {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput> {
        match self {
            Statement::Block(stmt) => visitor.visit_block_stmt(stmt.clone()),
            Statement::If(stmt) => visitor.visit_if_stmt(stmt.clone()),
            Statement::While(stmt) => visitor.visit_while_stmt(stmt.clone()),
            Statement::Continue(stmt) => visitor.visit_continue_stmt(stmt.clone()),
            Statement::Break(stmt) => visitor.visit_break_stmt(stmt.clone()),
            Statement::Return(stmt) => visitor.visit_return_stmt(stmt.clone()),
            Statement::Let(stmt) => visitor.visit_let_stmt(stmt.clone()),
            Statement::Expression(stmt) => visitor.visit_expression_stmt(stmt.clone()),
        }
    }
}

impl StatementVisitable for Node<Statement> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput> {
        self.borrow().accept(visitor)
    }
}

impl ItemVisitable for Item {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ItemOutput> {
        match self {
            Item::Event(item) => visitor.visit_event_item(item.clone()),
            Item::Function(item) => visitor.visit_function_item(item.clone()),
            Item::Struct(item) => visitor.visit_struct_item(item.clone()),
        }
    }
}

impl ItemVisitable for Node<Item> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ItemOutput> {
        self.borrow().accept(visitor)
    }
}

// ------------------------ VISITOR IMPLEMENTATIONS --------------------------

impl ExpressionVisitable for Node<UnaryExpr> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        visitor.visit_unary_expr(self.clone())
    }
}

impl ExpressionVisitable for Node<BinaryExpr> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        visitor.visit_binary_expr(self.clone())
    }
}

impl ExpressionVisitable for Node<LiteralExpr> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        visitor.visit_literal_expr(self.clone())
    }
}

impl ExpressionVisitable for Node<ListExpr> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        visitor.visit_list_expr(self.clone())
    }
}

impl ExpressionVisitable for Node<NameExpr> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        visitor.visit_name_expr(self.clone())
    }
}

impl ExpressionVisitable for Node<InstanciateExpr> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        visitor.visit_instanciate_expr(self.clone())
    }
}

impl ExpressionVisitable for Node<CallExpr> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        visitor.visit_call_expr(self.clone())
    }
}

impl ExpressionVisitable for Node<AssignExpr> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        visitor.visit_assign_expr(self.clone())
    }
}

impl ExpressionVisitable for Node<SubscriptExpr> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        visitor.visit_subscript_expr(self.clone())
    }
}

impl ExpressionVisitable for Node<GetExpr> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        visitor.visit_get_expr(self.clone())
    }
}

impl ExpressionVisitable for Node<SetExpr> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        visitor.visit_set_expr(self.clone())
    }
}

impl StatementVisitable for Node<BlockStmt> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput> {
        visitor.visit_block_stmt(self.clone())
    }
}

impl StatementVisitable for Node<IfStmt> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput> {
        visitor.visit_if_stmt(self.clone())
    }
}

impl StatementVisitable for Node<WhileStmt> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput> {
        visitor.visit_while_stmt(self.clone())
    }
}

impl StatementVisitable for Node<ContinueStmt> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput> {
        visitor.visit_continue_stmt(self.clone())
    }
}

impl StatementVisitable for Node<BreakStmt> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput> {
        visitor.visit_break_stmt(self.clone())
    }
}

impl StatementVisitable for Node<ReturnStmt> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput> {
        visitor.visit_return_stmt(self.clone())
    }
}

impl StatementVisitable for Node<LetStmt> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput> {
        visitor.visit_let_stmt(self.clone())
    }
}

impl StatementVisitable for Node<ExpressionStmt> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput> {
        visitor.visit_expression_stmt(self.clone())
    }
}

impl ItemVisitable for Node<EventItem> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ItemOutput> {
        visitor.visit_event_item(self.clone())
    }
}

impl ItemVisitable for Node<FunctionItem> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ItemOutput> {
        visitor.visit_function_item(self.clone())
    }
}

impl ItemVisitable for Node<StructItem> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ItemOutput> {
        visitor.visit_struct_item(self.clone())
    }
}
