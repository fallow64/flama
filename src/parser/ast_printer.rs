use crate::{
    parser::{
        ast::{
            AssignExpr, BinaryExpr, BlockStmt, BreakStmt, CallExpr, ContinueStmt, EventDecl,
            ExpressionStmt, FunctionDecl, GetExpr, IfStmt, LetStmt, LiteralExpr, NameExpr, NodePtr,
            PrintStmt, Program, ReturnStmt, SetExpr, UnaryExpr, WhileStmt,
        },
        visitor::{DeclarationVisitable, ExpressionVisitable, StatementVisitable, Visitor},
    },
    FlamaResult,
};

pub fn print(program: Program) -> FlamaResult<()> {
    let mut printer = Printer { indent: 0 };

    for declaration in program.iter() {
        println!("{}", declaration.borrow().accept(&mut printer)?);
    }

    Ok(())
}

struct Printer {
    indent: usize,
}

impl Visitor for Printer {
    type ExpressionOutput = String;
    type StatementOutput = String;
    type DeclarationOutput = String;

    fn visit_unary_expr(
        &mut self,
        expr: NodePtr<UnaryExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        Ok(format!(
            "({} {})",
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
        Ok(expr.borrow().name.clone())
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
            "if {} {}",
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
        let type_annotation = match &stmt.borrow().type_annotation {
            Some(type_annotation) => format!(": {}", type_annotation.to_string()),
            None => "".to_string(),
        };

        let value = match &stmt.borrow().value {
            Some(val) => format!(" = {}", val.accept(self)?),
            None => "".to_string(),
        };

        Ok(format!(
            "let {}{}{};",
            stmt.borrow().name.lexeme,
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

    fn visit_event_decl(
        &mut self,
        decl: NodePtr<EventDecl>,
    ) -> FlamaResult<Self::DeclarationOutput> {
        Ok(format!(
            "event {} {}",
            decl.borrow().name.lexeme,
            decl.borrow().body.accept(self)?
        ))
    }

    fn visit_function_decl(
        &mut self,
        decl: NodePtr<FunctionDecl>,
    ) -> FlamaResult<Self::DeclarationOutput> {
        Ok(format!(
            "fn {} {}",
            decl.borrow().signature.name.lexeme,
            decl.borrow().body.accept(self)?
        ))
    }
}