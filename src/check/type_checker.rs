use std::{path::PathBuf, rc::Rc};

use crate::{
    check::{environment::Environment, types::Type},
    lexer::token::{Span, Spanned},
    parser::{
        ast::{
            AssignExpr, BinaryExpr, BinaryOperator, BlockStmt, BreakStmt, CallExpr, ContinueStmt,
            EventItem, ExpressionStmt, FunctionItem, FunctionSignature, GetExpr, IfStmt, LetStmt,
            ListExpr, LiteralExpr, NameExpr, NodePtr, PrintStmt, Program, ReturnStmt, SetExpr,
            UnaryExpr, UnaryOperator, WhileStmt,
        },
        visitor::{ExpressionVisitable, ItemVisitable, StatementVisitable, Visitor},
    },
    ErrorType, FlamaError, FlamaResult, FlamaResults,
};

/// The type checker.
///
/// The type checker is responsible for checking and inferring the types of expressions and statements.
pub struct TypeChecker {
    environment: Environment<String, Type>,
    source_path: Rc<PathBuf>,
    return_type: Type,
}

impl Visitor for TypeChecker {
    type ExpressionOutput = Type;
    type StatementOutput = ();
    type ItemOutput = ();

    fn visit_unary_expr(
        &mut self,
        expr: NodePtr<UnaryExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        let typ = expr.borrow().right.accept(self)?;

        match (&expr.borrow().operator, typ.clone()) {
            (UnaryOperator::Identity, Type::Number) => Ok(Type::Number),
            (UnaryOperator::Negate, Type::Number) => Ok(Type::Number),
            (UnaryOperator::Not, Type::Boolean) => Ok(Type::Boolean),
            (op, typ) => Err(self.error(
                format!("operator '{}' not supported on type {}", op, typ),
                expr.borrow().init.span,
            )),
        }
    }

    fn visit_binary_expr(
        &mut self,
        expr: NodePtr<BinaryExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        let typ1 = expr.borrow().left.accept(self)?;
        let typ2 = expr.borrow().right.accept(self)?;

        match (&expr.borrow().operator, typ1, typ2) {
            (BinaryOperator::Add, Type::Number, Type::Number) => Ok(Type::Number),
            (BinaryOperator::Add, Type::String, Type::String) => Ok(Type::String),
            (BinaryOperator::Add, Type::String, Type::Number) => Ok(Type::String),
            (BinaryOperator::Add, Type::Number, Type::String) => Ok(Type::String),
            (BinaryOperator::Subtract, Type::Number, Type::Number) => Ok(Type::Number),
            (BinaryOperator::Multiply, Type::Number, Type::Number) => Ok(Type::Number),
            (BinaryOperator::Divide, Type::Number, Type::Number) => Ok(Type::Number),
            (BinaryOperator::Modulo, Type::Number, Type::Number) => Ok(Type::Number),
            (BinaryOperator::Equals, a, b) => {
                if a == b {
                    Ok(Type::Boolean)
                } else {
                    Err(self.expected_error(&a, &b, expr.borrow().init.span))
                }
            }
            (BinaryOperator::NotEq, a, b) => {
                if a == b {
                    Ok(Type::Boolean)
                } else {
                    Err(self.expected_error(&a, &b, expr.borrow().init.span))
                }
            }
            (BinaryOperator::Less, Type::Number, Type::Number) => Ok(Type::Boolean),
            (BinaryOperator::LessEq, Type::Number, Type::Number) => Ok(Type::Boolean),
            (BinaryOperator::Greater, Type::Number, Type::Number) => Ok(Type::Boolean),
            (BinaryOperator::GreaterEq, Type::Number, Type::Number) => Ok(Type::Boolean),
            (BinaryOperator::And, Type::Boolean, Type::Boolean) => Ok(Type::Boolean),
            (BinaryOperator::Or, Type::Boolean, Type::Boolean) => Ok(Type::Boolean),
            (BinaryOperator::Assign, a, b) => {
                // makes sure that the assigned to variable is the same type as the value
                if a == b {
                    Ok(a)
                } else {
                    Err(self.expected_error(&a, &b, expr.borrow().init.span))
                }
            }
            (op, typ1, typ2) => Err(self.error(
                format!(
                    "operator '{}' not supported on types {} and {}",
                    op, typ1, typ2
                ),
                expr.borrow().init.span,
            )),
        }
    }

    fn visit_literal_expr(
        &mut self,
        expr: NodePtr<LiteralExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        let typ: Type = expr.borrow().kind.clone().into();

        expr.borrow_mut().typ = Some(typ.clone());
        Ok(typ)
    }

    fn visit_list_expr(&mut self, expr: NodePtr<ListExpr>) -> FlamaResult<Self::ExpressionOutput> {
        if expr.borrow().elements.is_empty() {
            Ok(Type::List(Box::new(Type::Any)))
        } else {
            let first_type = expr.borrow().elements[0].accept(self)?;

            for element in expr.borrow().elements[1..].iter() {
                let element_type = element.accept(self)?;

                if first_type != element_type {
                    return Err(self.error(
                        format!(
                            "expected list element to be of type {}, but got {}",
                            first_type, element_type
                        ),
                        element.span(),
                    ));
                }
            }

            Ok(Type::List(Box::new(first_type)))
        }
    }

    fn visit_name_expr(&mut self, expr: NodePtr<NameExpr>) -> FlamaResult<Self::ExpressionOutput> {
        let name = expr.borrow().name.clone();
        let value_type = self.environment.get(&name.name);

        if value_type.is_none() {
            return Err(self.undefined_variable_error(&name.name, name.span));
        } else {
            Ok(value_type.unwrap().clone())
        }
    }

    fn visit_call_expr(&mut self, expr: NodePtr<CallExpr>) -> FlamaResult<Self::ExpressionOutput> {
        let callee_type = expr.borrow().callee.accept(self)?;
        if let Type::Function(signature) = callee_type {
            let mut args = Vec::new();

            for arg in expr.borrow().args.iter() {
                args.push(arg.accept(self)?);
            }

            if args.len() != signature.params.len() {
                return Err(self.arity_error(
                    &signature.name.name,
                    signature.params.len(),
                    args.len(),
                    expr.borrow().init.span,
                ));
            }

            for (i, arg) in args.iter().enumerate() {
                let expected_type = &signature.params[i].type_annotation.typ;
                if arg != expected_type {
                    return Err(self.expected_error(&expected_type, &arg, expr.borrow().init.span));
                }
            }

            Ok(signature.return_type.map_or(Type::Void, |rt| rt.typ))
        } else {
            Err(self.expected_error(
                &Type::Function(Box::new(FunctionSignature::default())),
                &callee_type,
                expr.borrow().init.span,
            ))
        }
    }

    fn visit_assign_expr(
        &mut self,
        expr: NodePtr<AssignExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        let value_type = expr.borrow().value.accept(self)?;
        let var_type = self.environment.get(&expr.borrow().name.lexeme);

        if var_type.is_none() {
            return Err(
                self.undefined_variable_error(&expr.borrow().name.lexeme, expr.borrow().name.span)
            );
        }

        let var_type = var_type.unwrap();

        if var_type == &value_type {
            Ok(value_type)
        } else {
            Err(self.expected_error(var_type, &value_type, expr.borrow().init.span))
        }
    }

    fn visit_get_expr(&mut self, expr: NodePtr<GetExpr>) -> FlamaResult<Self::ExpressionOutput> {
        let _object_type = expr.borrow().object.accept(self)?;
        todo!()
    }

    fn visit_set_expr(&mut self, expr: NodePtr<SetExpr>) -> FlamaResult<Self::ExpressionOutput> {
        expr.borrow().object.accept(self)?;
        expr.borrow().value.accept(self)?;
        todo!()
    }

    fn visit_block_stmt(&mut self, stmt: NodePtr<BlockStmt>) -> FlamaResult<Self::StatementOutput> {
        self.environment.push();
        for statement in &stmt.borrow().statements {
            statement.accept(self)?;
        }
        self.environment.pop();

        Ok(())
    }

    fn visit_print_stmt(&mut self, stmt: NodePtr<PrintStmt>) -> FlamaResult<Self::StatementOutput> {
        // any types allowed in print stmt
        let typ = stmt.borrow().value.accept(self)?;
        println!("{}", typ);

        Ok(())
    }

    fn visit_if_stmt(&mut self, stmt: NodePtr<IfStmt>) -> FlamaResult<Self::StatementOutput> {
        let condition_type = stmt.borrow().condition.accept(self)?;
        if !matches!(condition_type, Type::Boolean) {
            return Err(self.expected_error(
                &Type::Boolean,
                &condition_type,
                stmt.borrow().condition.span(),
            ));
        }

        stmt.borrow().body.accept(self)?;
        if let Some(alternative) = &stmt.borrow().alternative {
            alternative.accept(self)?;
        }

        Ok(())
    }

    fn visit_while_stmt(&mut self, stmt: NodePtr<WhileStmt>) -> FlamaResult<Self::StatementOutput> {
        let condition_type = stmt.borrow().condition.accept(self)?;
        if !matches!(condition_type, Type::Boolean) {
            return Err(self.expected_error(
                &Type::Boolean,
                &condition_type,
                stmt.borrow().condition.span(),
            ));
        }

        stmt.borrow().body.accept(self)?;

        Ok(())
    }

    fn visit_continue_stmt(
        &mut self,
        _: NodePtr<ContinueStmt>,
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
        let value_type = if let Some(value) = &stmt.borrow().value {
            value.accept(self)?
        } else {
            Type::default()
        };

        if value_type != self.return_type {
            return Err(self.expected_error(
                &self.return_type,
                &value_type,
                stmt.borrow().init.span,
            ));
        }

        Ok(())
    }

    fn visit_let_stmt(&mut self, stmt: NodePtr<LetStmt>) -> FlamaResult<Self::StatementOutput> {
        let type_annotation = stmt.borrow().type_annotation.clone().map(|t| t.typ);
        let name = stmt.borrow().name.name.clone();

        let value_type = if let Some(value_type) = &stmt.borrow().value {
            Some(value_type.accept(self)?)
        } else {
            None
        };

        let typ = match (type_annotation, value_type) {
            (Some(type_annotation), Some(value_type)) => {
                if type_annotation != value_type {
                    return Err(self.expected_error(
                        &type_annotation,
                        &value_type,
                        stmt.borrow().value.as_ref().unwrap().span(),
                    ));
                } else {
                    type_annotation
                }
            }
            (None, None) => return Err(self.cannot_infer_error(&name, stmt.borrow().init.span)),
            (Some(type_annotation), None) => type_annotation,
            (None, Some(value_type)) => value_type,
        };

        if let Type::Void = typ {
            return Err(self.cannot_infer_error(&name, stmt.borrow().name.span));
        }

        self.environment.define(name, typ);

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
        // signature into scope already handled in Self::parse()

        self.environment.push();
        self.return_type = decl
            .borrow()
            .signature
            .return_type
            .clone()
            .map(|te| te.typ)
            .unwrap_or(Type::default());

        for param in &decl.borrow().signature.params {
            self.environment
                .define(param.name.name.clone(), param.type_annotation.typ.clone());
        }

        for stmt in &decl.borrow().stmts {
            stmt.accept(self)?;
        }
        self.return_type = Type::default();
        self.environment.pop();

        Ok(())
    }
}

impl TypeChecker {
    pub fn new(source_path: Rc<PathBuf>) -> Self {
        Self {
            environment: Environment::new(),
            source_path,
            return_type: Type::default(),
        }
    }

    pub fn check(program: Rc<Program>, source_path: Rc<PathBuf>) -> FlamaResults<()> {
        let mut type_checker = TypeChecker::new(source_path);

        for signature in &program.signatures {
            type_checker.environment.define(
                signature.name.name.clone(),
                Type::Function(Box::new(signature.clone())),
            );
        }

        let mut errs = vec![];
        for item in &program.items {
            if let Err(err) = item.accept(&mut type_checker) {
                errs.push(err);
            }
        }

        if errs.is_empty() {
            Ok(())
        } else {
            Err(errs)
        }
    }

    fn error(&self, message: String, span: Span) -> FlamaError {
        FlamaError {
            message,
            span,
            error_type: ErrorType::Type,
            source_path: self.source_path.clone(),
        }
    }

    fn expected_error(&self, expected: &Type, actual: &Type, span: Span) -> FlamaError {
        self.error(
            format!("expected type {}, but got type {}", expected, actual),
            span,
        )
    }

    fn undefined_variable_error(&self, name: &String, span: Span) -> FlamaError {
        self.error(format!("undefined variable '{}'", name), span)
    }

    fn cannot_infer_error(&self, variable: &String, span: Span) -> FlamaError {
        self.error(
            format!("cannot infer type for variable '{}'", variable),
            span,
        )
    }

    fn arity_error(
        &self,
        func_name: &String,
        expected: usize,
        actual: usize,
        span: Span,
    ) -> FlamaError {
        self.error(
            format!(
                "expected {} arguments for function '{}', but got {}",
                expected, func_name, actual
            ),
            span,
        )
    }
}
