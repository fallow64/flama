use std::rc::Rc;

use crate::{
    parser::{
        ast::{
            AssignExpr, BinaryExpr, BlockStmt, BreakStmt, CallExpr, ConstItem, ContinueStmt,
            EventItem, ExpressionStmt, FunctionItem, GetExpr, IfStmt, LetStmt, LiteralExpr,
            LiteralKind, NameExpr, NodePtr, PrintStmt, Program, ReturnStmt, SetExpr, UnaryExpr,
            WhileStmt,
        },
        visitor::{ExpressionVisitable, ItemVisitable, StatementVisitable, Visitor},
    },
    FlamaResult,
};

pub struct TestLayer;

impl TestLayer {
    pub fn test(program: Rc<Program>) -> FlamaResult<()> {
        let mut test_layer = TestLayer {};

        for item in program.iter() {
            item.accept(&mut test_layer)?;
        }

        Ok(())
    }
}

impl Visitor for TestLayer {
    type ExpressionOutput = ();
    type StatementOutput = ();
    type ItemOutput = ();

    fn visit_unary_expr(
        &mut self,
        expr: NodePtr<UnaryExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        expr.borrow().right.accept(self)?;
        Ok(())
    }

    fn visit_binary_expr(
        &mut self,
        expr: NodePtr<BinaryExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        expr.borrow().left.accept(self)?;
        expr.borrow().right.accept(self)?;
        Ok(())
    }

    fn visit_literal_expr(
        &mut self,
        expr: NodePtr<LiteralExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        if let LiteralKind::Number(n) = &mut expr.borrow_mut().kind {
            *n += 100.0;
        }
        Ok(())
    }

    fn visit_name_expr(&mut self, _expr: NodePtr<NameExpr>) -> FlamaResult<Self::ExpressionOutput> {
        Ok(())
    }

    fn visit_call_expr(&mut self, expr: NodePtr<CallExpr>) -> FlamaResult<Self::ExpressionOutput> {
        for arg in expr.borrow().args.iter() {
            arg.accept(self)?;
        }

        expr.borrow().callee.accept(self)?;
        Ok(())
    }

    fn visit_assign_expr(
        &mut self,
        expr: NodePtr<AssignExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        expr.borrow().value.accept(self)?;
        Ok(())
    }

    fn visit_get_expr(&mut self, expr: NodePtr<GetExpr>) -> FlamaResult<Self::ExpressionOutput> {
        expr.borrow().object.accept(self)?;
        Ok(())
    }

    fn visit_set_expr(&mut self, expr: NodePtr<SetExpr>) -> FlamaResult<Self::ExpressionOutput> {
        expr.borrow().object.accept(self)?;
        expr.borrow().value.accept(self)?;
        Ok(())
    }

    fn visit_block_stmt(&mut self, stmt: NodePtr<BlockStmt>) -> FlamaResult<Self::StatementOutput> {
        for stmt in stmt.borrow().statements.iter() {
            stmt.accept(self)?;
        }

        Ok(())
    }

    fn visit_print_stmt(&mut self, stmt: NodePtr<PrintStmt>) -> FlamaResult<Self::StatementOutput> {
        for value in &stmt.borrow().values {
            value.accept(self)?;
        }
        Ok(())
    }

    fn visit_if_stmt(&mut self, stmt: NodePtr<IfStmt>) -> FlamaResult<Self::StatementOutput> {
        stmt.borrow().condition.accept(self)?;
        stmt.borrow().body.accept(self)?;
        if let Some(alternative) = &stmt.borrow().alternative {
            alternative.accept(self)?;
        }
        Ok(())
    }

    fn visit_while_stmt(&mut self, stmt: NodePtr<WhileStmt>) -> FlamaResult<Self::StatementOutput> {
        stmt.borrow().condition.accept(self)?;
        stmt.borrow().body.accept(self)?;
        Ok(())
    }

    fn visit_continue_stmt(
        &mut self,
        _stmt: NodePtr<ContinueStmt>,
    ) -> FlamaResult<Self::StatementOutput> {
        Ok(())
    }

    fn visit_break_stmt(
        &mut self,
        _stmt: NodePtr<BreakStmt>,
    ) -> FlamaResult<Self::StatementOutput> {
        Ok(())
    }

    fn visit_return_stmt(
        &mut self,
        stmt: NodePtr<ReturnStmt>,
    ) -> FlamaResult<Self::StatementOutput> {
        if let Some(value) = &stmt.borrow().value {
            value.accept(self)?;
        }
        Ok(())
    }

    fn visit_let_stmt(&mut self, stmt: NodePtr<LetStmt>) -> FlamaResult<Self::StatementOutput> {
        if let Some(value) = &stmt.borrow().value {
            value.accept(self)?;
        }
        Ok(())
    }

    fn visit_expression_stmt(
        &mut self,
        stmt: NodePtr<ExpressionStmt>,
    ) -> FlamaResult<Self::StatementOutput> {
        stmt.borrow().expression.accept(self)?;
        Ok(())
    }

    fn visit_event_item(&mut self, item: NodePtr<EventItem>) -> FlamaResult<Self::ItemOutput> {
        item.borrow().body.accept(self)?;
        Ok(())
    }

    fn visit_function_item(
        &mut self,
        item: NodePtr<FunctionItem>,
    ) -> FlamaResult<Self::ItemOutput> {
        for stmt in &item.borrow().stmts {
            stmt.accept(self)?;
        }

        Ok(())
    }

    fn visit_const_item(&mut self, item: NodePtr<ConstItem>) -> FlamaResult<Self::ItemOutput> {
        item.borrow().value.accept(self)?;
        Ok(())
    }
}
