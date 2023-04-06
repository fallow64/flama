use crate::FlamaResult;

use super::ast::{
    AssignExpr, BinaryExpr, BlockStmt, BreakStmt, CallExpr, ConstItem, ContinueStmt, EventItem,
    Expression, ExpressionStmt, FunctionItem, GetExpr, IfStmt, Item, LetStmt, ListExpr,
    LiteralExpr, NameExpr, NodePtr, PrintStmt, ReturnStmt, SetExpr, Statement, UnaryExpr,
    WhileStmt,
};

pub trait Visitor {
    type ExpressionOutput;
    type StatementOutput;
    type ItemOutput;

    fn visit_unary_expr(&mut self, expr: NodePtr<UnaryExpr>)
        -> FlamaResult<Self::ExpressionOutput>;
    fn visit_binary_expr(
        &mut self,
        expr: NodePtr<BinaryExpr>,
    ) -> FlamaResult<Self::ExpressionOutput>;
    fn visit_literal_expr(
        &mut self,
        expr: NodePtr<LiteralExpr>,
    ) -> FlamaResult<Self::ExpressionOutput>;
    fn visit_list_expr(&mut self, expr: NodePtr<ListExpr>) -> FlamaResult<Self::ExpressionOutput>;
    fn visit_name_expr(&mut self, expr: NodePtr<NameExpr>) -> FlamaResult<Self::ExpressionOutput>;
    fn visit_call_expr(&mut self, expr: NodePtr<CallExpr>) -> FlamaResult<Self::ExpressionOutput>;
    fn visit_assign_expr(
        &mut self,
        expr: NodePtr<AssignExpr>,
    ) -> FlamaResult<Self::ExpressionOutput>;
    fn visit_get_expr(&mut self, expr: NodePtr<GetExpr>) -> FlamaResult<Self::ExpressionOutput>;
    fn visit_set_expr(&mut self, expr: NodePtr<SetExpr>) -> FlamaResult<Self::ExpressionOutput>;

    fn visit_block_stmt(&mut self, stmt: NodePtr<BlockStmt>) -> FlamaResult<Self::StatementOutput>;
    fn visit_print_stmt(&mut self, stmt: NodePtr<PrintStmt>) -> FlamaResult<Self::StatementOutput>;
    fn visit_if_stmt(&mut self, stmt: NodePtr<IfStmt>) -> FlamaResult<Self::StatementOutput>;
    fn visit_while_stmt(&mut self, stmt: NodePtr<WhileStmt>) -> FlamaResult<Self::StatementOutput>;
    fn visit_continue_stmt(
        &mut self,
        stmt: NodePtr<ContinueStmt>,
    ) -> FlamaResult<Self::StatementOutput>;
    fn visit_break_stmt(&mut self, stmt: NodePtr<BreakStmt>) -> FlamaResult<Self::StatementOutput>;
    fn visit_return_stmt(
        &mut self,
        stmt: NodePtr<ReturnStmt>,
    ) -> FlamaResult<Self::StatementOutput>;
    fn visit_let_stmt(&mut self, stmt: NodePtr<LetStmt>) -> FlamaResult<Self::StatementOutput>;
    fn visit_expression_stmt(
        &mut self,
        stmt: NodePtr<ExpressionStmt>,
    ) -> FlamaResult<Self::StatementOutput>;

    fn visit_event_item(&mut self, decl: NodePtr<EventItem>) -> FlamaResult<Self::ItemOutput>;
    fn visit_function_item(&mut self, decl: NodePtr<FunctionItem>)
        -> FlamaResult<Self::ItemOutput>;
    fn visit_const_item(&mut self, decl: NodePtr<ConstItem>) -> FlamaResult<Self::ItemOutput>;
}

pub trait ExpressionVisitable {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput>;
}

pub trait StatementVisitable {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput>;
}

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
            Expression::Call(expr) => visitor.visit_call_expr(expr.clone()),
            Expression::Assign(expr) => visitor.visit_assign_expr(expr.clone()),
            Expression::Get(expr) => visitor.visit_get_expr(expr.clone()),
            Expression::Set(expr) => visitor.visit_set_expr(expr.clone()),
        }
    }
}

impl ExpressionVisitable for NodePtr<Expression> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        self.borrow().accept(visitor)
    }
}

impl StatementVisitable for Statement {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput> {
        match self {
            Statement::Block(stmt) => visitor.visit_block_stmt(stmt.clone()),
            Statement::Print(stmt) => visitor.visit_print_stmt(stmt.clone()),
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

impl StatementVisitable for NodePtr<Statement> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput> {
        self.borrow().accept(visitor)
    }
}

impl ItemVisitable for Item {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ItemOutput> {
        match self {
            Item::Event(item) => visitor.visit_event_item(item.clone()),
            Item::Function(item) => visitor.visit_function_item(item.clone()),
            Item::Constant(item) => visitor.visit_const_item(item.clone()),
        }
    }
}

impl ItemVisitable for NodePtr<Item> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ItemOutput> {
        self.borrow().accept(visitor)
    }
}

// ------------------------ VISITOR IMPLEMENTATIONS --------------------------

impl ExpressionVisitable for NodePtr<UnaryExpr> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        visitor.visit_unary_expr(self.clone())
    }
}

impl ExpressionVisitable for NodePtr<BinaryExpr> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        visitor.visit_binary_expr(self.clone())
    }
}

impl ExpressionVisitable for NodePtr<LiteralExpr> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        visitor.visit_literal_expr(self.clone())
    }
}

impl ExpressionVisitable for NodePtr<ListExpr> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        visitor.visit_list_expr(self.clone())
    }
}

impl ExpressionVisitable for NodePtr<NameExpr> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        visitor.visit_name_expr(self.clone())
    }
}

impl ExpressionVisitable for NodePtr<CallExpr> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        visitor.visit_call_expr(self.clone())
    }
}

impl ExpressionVisitable for NodePtr<AssignExpr> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        visitor.visit_assign_expr(self.clone())
    }
}

impl ExpressionVisitable for NodePtr<GetExpr> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        visitor.visit_get_expr(self.clone())
    }
}

impl ExpressionVisitable for NodePtr<SetExpr> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ExpressionOutput> {
        visitor.visit_set_expr(self.clone())
    }
}

impl StatementVisitable for NodePtr<BlockStmt> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput> {
        visitor.visit_block_stmt(self.clone())
    }
}

impl StatementVisitable for NodePtr<PrintStmt> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput> {
        visitor.visit_print_stmt(self.clone())
    }
}

impl StatementVisitable for NodePtr<IfStmt> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput> {
        visitor.visit_if_stmt(self.clone())
    }
}

impl StatementVisitable for NodePtr<WhileStmt> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput> {
        visitor.visit_while_stmt(self.clone())
    }
}

impl StatementVisitable for NodePtr<ContinueStmt> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput> {
        visitor.visit_continue_stmt(self.clone())
    }
}

impl StatementVisitable for NodePtr<BreakStmt> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput> {
        visitor.visit_break_stmt(self.clone())
    }
}

impl StatementVisitable for NodePtr<ReturnStmt> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput> {
        visitor.visit_return_stmt(self.clone())
    }
}

impl StatementVisitable for NodePtr<LetStmt> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput> {
        visitor.visit_let_stmt(self.clone())
    }
}

impl StatementVisitable for NodePtr<ExpressionStmt> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::StatementOutput> {
        visitor.visit_expression_stmt(self.clone())
    }
}

impl ItemVisitable for NodePtr<EventItem> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ItemOutput> {
        visitor.visit_event_item(self.clone())
    }
}

impl ItemVisitable for NodePtr<FunctionItem> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ItemOutput> {
        visitor.visit_function_item(self.clone())
    }
}

impl ItemVisitable for NodePtr<ConstItem> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> FlamaResult<V::ItemOutput> {
        visitor.visit_const_item(self.clone())
    }
}
