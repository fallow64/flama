use std::rc::Rc;

use crate::{
    error::{FlamaResult, FlamaResults},
    parser::{
        ast::{
            AssignExpr, BinaryExpr, BlockStmt, BreakStmt, CallExpr, ContinueStmt, EventItem,
            ExpressionStmt, FunctionItem, GetExpr, IfStmt, InstanciateExpr, LetStmt, ListExpr,
            LiteralExpr, NameExpr, NodePtr, PrintStmt, Program, ReturnStmt, SetExpr, Statement,
            StructItem, UnaryExpr, WhileStmt,
        },
        visitor::{ExpressionVisitable, ItemVisitable, StatementVisitable, Visitor},
    },
};

/// Helper struct for visting the AST and printing it.
pub struct Printer {
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
            expr.borrow().operator,
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
            expr.borrow().operator,
            expr.borrow().right.accept(self)?
        ))
    }

    fn visit_literal_expr(
        &mut self,
        expr: NodePtr<LiteralExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        Ok(expr.borrow().kind.to_string())
    }

    fn visit_list_expr(&mut self, expr: NodePtr<ListExpr>) -> FlamaResult<Self::ExpressionOutput> {
        let elements = expr
            .borrow()
            .elements
            .iter()
            .map(|element| element.accept(self))
            .collect::<FlamaResult<Vec<_>>>()?
            .join(", ");

        Ok(format!("[{}]", elements))
    }

    fn visit_name_expr(&mut self, expr: NodePtr<NameExpr>) -> FlamaResult<Self::ExpressionOutput> {
        Ok(expr.borrow().name.name.clone())
    }

    fn visit_instanciate_expr(
        &mut self,
        expr: NodePtr<InstanciateExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        let mut output = expr.borrow().name.name.clone();
        output.push_str(" {\n");
        self.indent += 4;

        for (name, expr) in expr.borrow().fields.iter() {
            output.push_str(" ".repeat(self.indent).as_str());
            output.push_str(&name.name);
            output.push_str(": ");
            output.push_str(&expr.accept(self)?);
            output.push('\n');
        }
        self.indent -= 4;
        output.push_str(" ".repeat(self.indent).as_str());
        output.push('}');

        Ok(output)
        // todo!()
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
            expr.borrow().name.name,
            expr.borrow().value.accept(self)?
        ))
    }

    fn visit_get_expr(&mut self, expr: NodePtr<GetExpr>) -> FlamaResult<Self::ExpressionOutput> {
        Ok(format!(
            "{}.{}",
            expr.borrow().object.accept(self)?,
            expr.borrow().name.name
        ))
    }

    fn visit_set_expr(&mut self, expr: NodePtr<SetExpr>) -> FlamaResult<Self::ExpressionOutput> {
        Ok(format!(
            "{}.{} = {}",
            expr.borrow().object.accept(self)?,
            expr.borrow().name.name,
            expr.borrow().value.accept(self)?
        ))
    }

    fn visit_block_stmt(&mut self, stmt: NodePtr<BlockStmt>) -> FlamaResult<Self::StatementOutput> {
        self.visit_multiple(&stmt.borrow().statements)
    }

    fn visit_print_stmt(&mut self, stmt: NodePtr<PrintStmt>) -> FlamaResult<Self::StatementOutput> {
        let value = stmt.borrow().value.accept(self)?;

        Ok(format!("print {};", value))
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
            Some(type_annotation) => format!(": {}", type_annotation),
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
            "fn {}({}) {}",
            item.borrow().signature.name.name,
            item.borrow()
                .signature
                .params
                .iter()
                .map(|(name, typ)| format!("{}: {}", name.name, typ))
                .collect::<Vec<_>>()
                .join(", "),
            self.visit_multiple(&item.borrow().stmts)?
        ))
    }

    fn visit_struct_item(&mut self, decl: NodePtr<StructItem>) -> FlamaResult<Self::ItemOutput> {
        let mut output = String::from("struct ");

        output.push_str(&decl.borrow().name.name);
        output.push_str(" {\n");

        for (ident, typ) in &decl.borrow().fields {
            output.push_str(" ".repeat(self.indent + 4).as_str());
            output.push_str(&ident.name);
            output.push_str(": ");
            output.push_str(&typ.to_string());
            output.push_str(",\n");
        }

        output.push_str(" ".repeat(self.indent).as_str());
        output.push('}');

        Ok(output)
    }
}

impl Printer {
    /// Prints the given program to `stdout`.
    pub fn print(program: Rc<Program>) -> FlamaResults<()> {
        let mut printer = Printer { indent: 0 };

        let mut errs = vec![];
        for item in &program.items {
            match item.accept(&mut printer) {
                Ok(output) => println!("{}", output),
                Err(err) => errs.push(err),
            }
        }

        if errs.is_empty() {
            Ok(())
        } else {
            Err(errs)
        }
    }

    fn visit_multiple(&mut self, stmts: &Vec<Statement>) -> FlamaResult<String> {
        let mut output = String::from("{\n");
        self.indent += 4;

        for stmt in stmts {
            output.push_str(" ".repeat(self.indent).as_str());
            output.push_str(&stmt.accept(self)?);
            output.push('\n');
        }

        self.indent -= 4;
        output.push_str(" ".repeat(self.indent).as_str());
        output.push('}');

        Ok(output)
    }
}
