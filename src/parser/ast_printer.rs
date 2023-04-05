use std::rc::Rc;

use crate::{
    parser::{
        ast::{
            AssignExpr, BinaryExpr, BlockStmt, BreakStmt, CallExpr, ContinueStmt, EventItem,
            ExpressionStmt, FunctionItem, GetExpr, IfStmt, LetStmt, LiteralExpr, NameExpr, NodePtr,
            PrintStmt, Program, ReturnStmt, SetExpr, UnaryExpr, WhileStmt,
        },
        visitor::{ExpressionVisitable, StatementVisitable, Visitor},
    },
    FlamaResult, FlamaResults,
};

use super::{
    ast::{ConstItem, Statement},
    visitor::ItemVisitable,
};

pub fn print(program: Rc<Program>) -> FlamaResults<()> {
    let mut printer = Printer { indent: 0 };

    let mut errs = vec![];
    for item in &program.items {
        if let Err(err) = item.accept(&mut printer) {
            errs.push(err);
        }
    }

    if errs.is_empty() {
        Ok(())
    } else {
        Err(errs)
    }
}

struct Printer {
    indent: usize,
}

impl Visitor for Printer {
    type ExpressionOutput = String;
    type StatementOutput = String;
    type ItemOutput = String;

    fn visit_unary_expr(
        &mut self,
        expr: NodePtr<UnaryExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        Ok(format!(
            "({}{})",
            expr.borrow().operator.to_string(),
            expr.borrow().right.accept(self)?
        ))
    }

    fn visit_binary_expr(
        &mut self,
        expr: NodePtr<BinaryExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        Ok(format!(
            "({} {} {})",
            expr.borrow().left.accept(self)?,
            expr.borrow().operator.to_string(),
            expr.borrow().right.accept(self)?
        ))
    }

    fn visit_literal_expr(
        &mut self,
        expr: NodePtr<LiteralExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        Ok(expr.borrow().kind.to_string())
    }

    fn visit_name_expr(&mut self, expr: NodePtr<NameExpr>) -> FlamaResult<Self::ExpressionOutput> {
        Ok(expr.borrow().name.name.clone())
    }

    fn visit_call_expr(&mut self, expr: NodePtr<CallExpr>) -> FlamaResult<Self::ExpressionOutput> {
        let arguments = expr
            .borrow()
            .args
            .iter()
            .map(|arg| arg.accept(self))
            .collect::<FlamaResult<Vec<_>>>()?
            .join(", ");

        Ok(format!(
            "{}({})",
            expr.borrow().callee.accept(self)?,
            arguments
        ))
    }

    fn visit_assign_expr(
        &mut self,
        expr: NodePtr<AssignExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        Ok(format!(
            "{} = {}",
            expr.borrow().name.lexeme,
            expr.borrow().value.accept(self)?
        ))
    }

    fn visit_get_expr(&mut self, expr: NodePtr<GetExpr>) -> FlamaResult<Self::ExpressionOutput> {
        Ok(format!(
            "{}.{}",
            expr.borrow().object.accept(self)?,
            expr.borrow().name.lexeme
        ))
    }

    fn visit_set_expr(&mut self, expr: NodePtr<SetExpr>) -> FlamaResult<Self::ExpressionOutput> {
        Ok(format!(
            "{}.{} = {}",
            expr.borrow().object.accept(self)?,
            expr.borrow().name.lexeme,
            expr.borrow().value.accept(self)?
        ))
    }

    fn visit_block_stmt(&mut self, stmt: NodePtr<BlockStmt>) -> FlamaResult<Self::StatementOutput> {
        let mut output = String::from("{\n");
        self.indent += 4;

        for statement in &stmt.borrow().statements {
            output.push_str(" ".repeat(self.indent).as_str());
            output.push_str(&statement.accept(self)?);
            output.push_str("\n");
        }

        self.indent -= 4;
        output.push_str(" ".repeat(self.indent).as_str());
        output.push_str("}");

        Ok(output)
    }

    fn visit_print_stmt(&mut self, stmt: NodePtr<PrintStmt>) -> FlamaResult<Self::StatementOutput> {
        let values = stmt
            .borrow()
            .values
            .iter()
            .map(|val| val.accept(self))
            .collect::<FlamaResult<Vec<_>>>()?
            .join(", ");

        Ok(format!("print({});", values))
    }

    fn visit_if_stmt(&mut self, stmt: NodePtr<IfStmt>) -> FlamaResult<Self::StatementOutput> {
        Ok(format!(
            "if ({}) {}",
            stmt.borrow().condition.accept(self)?,
            stmt.borrow().body.accept(self)?
        ))
    }

    fn visit_while_stmt(&mut self, stmt: NodePtr<WhileStmt>) -> FlamaResult<Self::StatementOutput> {
        Ok(format!(
            "while ({}) {}",
            stmt.borrow().condition.accept(self)?,
            stmt.borrow().body.accept(self)?
        ))
    }

    fn visit_continue_stmt(
        &mut self,
        _stmt: NodePtr<ContinueStmt>,
    ) -> FlamaResult<Self::StatementOutput> {
        Ok("continue;".to_string())
    }

    fn visit_break_stmt(
        &mut self,
        _stmt: NodePtr<BreakStmt>,
    ) -> FlamaResult<Self::StatementOutput> {
        Ok("break;".to_string())
    }

    fn visit_return_stmt(
        &mut self,
        stmt: NodePtr<ReturnStmt>,
    ) -> FlamaResult<Self::StatementOutput> {
        let value = match &stmt.borrow().value {
            Some(val) => format!(" {}", val.accept(self)?),
            None => "".to_string(),
        };

        Ok(format!("return{};", value))
    }

    fn visit_let_stmt(&mut self, stmt: NodePtr<LetStmt>) -> FlamaResult<Self::StatementOutput> {
        let keyword = stmt.borrow().kind.to_string();

        let type_annotation = match &stmt.borrow().type_annotation {
            Some(type_annotation) => format!(": {}", type_annotation.to_string()),
            None => "".to_string(),
        };

        let value = match &stmt.borrow().value {
            Some(val) => format!(" = {}", val.accept(self)?),
            None => "".to_string(),
        };

        Ok(format!(
            "{} {}{}{};",
            keyword,
            stmt.borrow().name.name,
            type_annotation,
            value
        ))
    }

    fn visit_expression_stmt(
        &mut self,
        stmt: NodePtr<ExpressionStmt>,
    ) -> FlamaResult<Self::StatementOutput> {
        Ok(format!("{};", stmt.borrow().expression.accept(self)?))
    }

    fn visit_event_item(&mut self, item: NodePtr<EventItem>) -> FlamaResult<Self::ItemOutput> {
        Ok(format!(
            "event {} {}",
            item.borrow().name.name,
            item.borrow().body.accept(self)?
        ))
    }

    fn visit_function_item(
        &mut self,
        item: NodePtr<FunctionItem>,
    ) -> FlamaResult<Self::ItemOutput> {
        Ok(format!(
            "fn {} {}",
            item.borrow().signature.name.name,
            self.visit_multiple(&item.borrow().stmts)?
        ))
    }

    fn visit_const_item(&mut self, item: NodePtr<ConstItem>) -> FlamaResult<Self::ItemOutput> {
        let type_annotation = match &item.borrow().type_annotation {
            Some(type_annotation) => format!(": {}", type_annotation.to_string()),
            None => "".to_string(),
        };

        Ok(format!(
            "const {}{} = {}",
            item.borrow().name.name,
            type_annotation,
            item.borrow().value.accept(self)?
        ))
    }
}

impl Printer {
    fn visit_multiple<'a>(&mut self, stmts: &'a Vec<Statement>) -> FlamaResult<String> {
        let mut output = String::from("{\n");
        self.indent += 4;

        for stmt in stmts {
            output.push_str(" ".repeat(self.indent).as_str());
            output.push_str(&stmt.accept(self)?);
            output.push_str("\n");
        }
        output.push_str("}");

        self.indent -= 4;

        Ok(output)
    }
}
