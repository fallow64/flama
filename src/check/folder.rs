// still TODO, not in cargo project
use std::{cell::RefCell, rc::Rc};

use crate::{
    parser::{
        ast::{
            AssignExpr, BinaryExpr, BinaryOperator, BlockStmt, BreakStmt, CallExpr, ContinueStmt,
            EventItem, Expression, ExpressionStmt, FunctionItem, GetExpr, IfStmt, InstanciateExpr,
            LetStmt, ListExpr, LiteralExpr, LiteralKind, NameExpr, NodePtr, PrintStmt, ReturnStmt,
            SetExpr, StructItem, UnaryExpr, WhileStmt,
        },
        visitor::{ExpressionVisitable, StatementVisitable, Visitor},
    },
    FlamaResult,
};

pub struct ConstantFolder;

impl Visitor for ConstantFolder {
    type ExpressionOutput = ();
    type StatementOutput = ();
    type ItemOutput = ();

    fn visit_unary_expr(
        &mut self,
        expr: NodePtr<UnaryExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        todo!()
    }

    fn visit_binary_expr(
        &mut self,
        expr: NodePtr<BinaryExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        match (expr.borrow().left, expr.borrow().right) {
            (Expression::Literal(left), Expression::Literal(right)) => {
                let left = left.borrow();
                let right = right.borrow();

                let result = match (left.kind, right.kind) {
                    (LiteralKind::Number(left), LiteralKind::Number(right)) => {
                        match expr.borrow().operator {
                            BinaryOperator::Add => LiteralKind::Number(left + right),
                            BinaryOperator::Subtract => LiteralKind::Number(left - right),
                            BinaryOperator::Multiply => LiteralKind::Number(left * right),
                            BinaryOperator::Divide => LiteralKind::Number(left / right),
                            BinaryOperator::Equals => LiteralKind::Boolean(left == right),
                            BinaryOperator::NotEq => LiteralKind::Boolean(left != right),
                            BinaryOperator::Greater => LiteralKind::Boolean(left > right),
                            BinaryOperator::GreaterEq => LiteralKind::Boolean(left >= right),
                            BinaryOperator::Less => LiteralKind::Boolean(left < right),
                            BinaryOperator::LessEq => LiteralKind::Boolean(left <= right),
                            BinaryOperator::Modulo => LiteralKind::Number(left % right),
                            _ => unreachable!(), // and, or, and set
                        }
                    }
                    (LiteralKind::Boolean(left), LiteralKind::Boolean(right)) => {
                        match expr.borrow().operator {
                            BinaryOperator::Equals => LiteralKind::Boolean(left == right),
                            BinaryOperator::NotEq => LiteralKind::Boolean(left != right),
                            _ => unreachable!(),
                        }
                    }
                    _ => unreachable!(),
                };
            }
            _ => {}
        }

        Ok(())
    }

    fn visit_literal_expr(
        &mut self,
        _expr: NodePtr<LiteralExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        Ok(())
    }

    fn visit_list_expr(&mut self, expr: NodePtr<ListExpr>) -> FlamaResult<Self::ExpressionOutput> {
        for element in &expr.borrow().elements {
            element.accept(self)?;
        }

        Ok(())
    }

    fn visit_name_expr(&mut self, _expr: NodePtr<NameExpr>) -> FlamaResult<Self::ExpressionOutput> {
        Ok(())
    }

    fn visit_instanciate_expr(
        &mut self,
        expr: NodePtr<InstanciateExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        for (_, expr) in &expr.borrow().fields {
            expr.accept(self)?;
        }

        Ok(())
    }

    fn visit_call_expr(&mut self, expr: NodePtr<CallExpr>) -> FlamaResult<Self::ExpressionOutput> {
        expr.borrow().callee.accept(self)?;
        for argument in &expr.borrow().args {
            argument.accept(self)?;
        }

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
        for statement in &stmt.borrow().statements {
            statement.accept(self)?;
        }

        Ok(())
    }

    fn visit_print_stmt(&mut self, stmt: NodePtr<PrintStmt>) -> FlamaResult<Self::StatementOutput> {
        stmt.borrow().value.accept(self)?;

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
        if let Some(expr) = &stmt.borrow().value {
            expr.accept(self)?;
        }

        Ok(())
    }

    fn visit_let_stmt(&mut self, stmt: NodePtr<LetStmt>) -> FlamaResult<Self::StatementOutput> {
        if let Some(expr) = &stmt.borrow().value {
            expr.accept(self)?;
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

    fn visit_event_item(&mut self, decl: NodePtr<EventItem>) -> FlamaResult<Self::ItemOutput> {
        decl.borrow().body.accept(self)?;

        Ok(())
    }

    fn visit_function_item(
        &mut self,
        decl: NodePtr<FunctionItem>,
    ) -> FlamaResult<Self::ItemOutput> {
        for stmt in &decl.borrow().stmts {
            stmt.accept(self)?;
        }

        Ok(())
    }

    fn visit_struct_item(&mut self, _decl: NodePtr<StructItem>) -> FlamaResult<Self::ItemOutput> {
        Ok(())
    }
}
