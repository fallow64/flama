use std::{collections::HashMap, path::PathBuf, rc::Rc};

use crate::{
    check::{environment::Environment, types::Type},
    error::{ErrorType, FlamaError, FlamaResult, FlamaResults},
    lexer::token::{Span, Spanned},
    parser::{
        ast::{
            AssignExpr, BinaryExpr, BinaryOperator, BlockStmt, BreakStmt, CallExpr, ContinueStmt,
            EventItem, ExpressionStmt, FunctionItem, GetExpr, IfStmt, InstanciateExpr, LetStmt,
            ListExpr, LiteralExpr, NameExpr, Node, Program, ReturnStmt, SetExpr, StructItem,
            SubscriptExpr, UnaryExpr, UnaryOperator, VariableType, WhileStmt,
        },
        visitor::{ExpressionVisitable, ItemVisitable, StatementVisitable, Visitor},
    },
};

// note: this file is littered with `clone()` calls, which is not ideal, but quite frankly I don't wanna deal with it right now
// also, Type::Identifier really messes with this

/// The type checker.
///
/// The type checker is responsible for checking and inferring the types of expressions and statements.
pub struct TypeChecker {
    environment: Environment<String, Type>,
    typedefs: Environment<String, Type>,
    source_path: Rc<PathBuf>,
    return_type: Type,
}

impl Visitor for TypeChecker {
    type ExpressionOutput = Type;
    type StatementOutput = ();
    type ItemOutput = ();

    fn visit_unary_expr(
        &mut self,
        expr: Node<UnaryExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        let right_type = expr.borrow().right.accept(self)?;

        let typ = match (&expr.borrow().operator, right_type) {
            (UnaryOperator::Identity, Type::Number) => Type::Number,
            (UnaryOperator::Negate, Type::Number) => Type::Number,
            (UnaryOperator::Not, Type::Boolean) => Type::Boolean,
            (op, typ) => {
                return Err(self.error(
                    format!("operator '{}' not supported on type {}", op, typ),
                    expr.borrow().init.span,
                ))
            }
        };
        expr.borrow_mut().typ = Some(typ.clone());
        Ok(typ)
    }

    fn visit_binary_expr(
        &mut self,
        expr: Node<BinaryExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        let left_type = expr.borrow().left.accept(self)?;
        let right_type = expr.borrow().right.accept(self)?;

        let typ = match (&expr.borrow().operator, left_type, right_type) {
            (BinaryOperator::Add, Type::Number, Type::Number) => Type::Number,
            (BinaryOperator::Add, Type::String, Type::String) => Type::String,
            (BinaryOperator::Add, Type::String, Type::Number) => Type::String,
            (BinaryOperator::Add, Type::Number, Type::String) => Type::String,
            (BinaryOperator::Subtract, Type::Number, Type::Number) => Type::Number,
            (BinaryOperator::Multiply, Type::Number, Type::Number) => Type::Number,
            (BinaryOperator::Divide, Type::Number, Type::Number) => Type::Number,
            (BinaryOperator::Modulo, Type::Number, Type::Number) => Type::Number,
            (BinaryOperator::Equals, a, b) => {
                if a != b {
                    return Err(self.expected_error(&a, &b, expr.borrow().init.span));
                }
                Type::Boolean
            }
            (BinaryOperator::NotEq, a, b) => {
                if a != b {
                    return Err(self.expected_error(&a, &b, expr.borrow().init.span));
                }
                Type::Boolean
            }
            (BinaryOperator::Less, Type::Number, Type::Number) => Type::Boolean,
            (BinaryOperator::LessEq, Type::Number, Type::Number) => Type::Boolean,
            (BinaryOperator::Greater, Type::Number, Type::Number) => Type::Boolean,
            (BinaryOperator::GreaterEq, Type::Number, Type::Number) => Type::Boolean,
            (BinaryOperator::And, Type::Boolean, Type::Boolean) => Type::Boolean,
            (BinaryOperator::Or, Type::Boolean, Type::Boolean) => Type::Boolean,
            (op, typ1, typ2) => {
                return Err(self.error(
                    format!(
                        "operator '{}' not supported on types {} and {}",
                        op, typ1, typ2
                    ),
                    expr.borrow().init.span,
                ))
            }
        };

        expr.borrow_mut().typ = Some(typ.clone());
        Ok(typ)
    }

    fn visit_literal_expr(
        &mut self,
        expr: Node<LiteralExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        let typ: Type = expr.borrow().kind.clone().into();

        expr.borrow_mut().typ = Some(typ.clone());
        Ok(typ)
    }

    fn visit_list_expr(&mut self, expr: Node<ListExpr>) -> FlamaResult<Self::ExpressionOutput> {
        let typ = if expr.borrow().elements.is_empty() {
            Type::List(Box::new(Type::Any))
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

            Type::List(Box::new(first_type))
        };

        expr.borrow_mut().typ = Some(typ.clone());
        Ok(typ)
    }

    fn visit_name_expr(&mut self, expr: Node<NameExpr>) -> FlamaResult<Self::ExpressionOutput> {
        let name = expr.borrow().name.clone();
        let value_type = self.environment.get(&name.name);

        match value_type {
            Some(typ) => {
                expr.borrow_mut().typ = Some(typ.clone());
                Ok(typ.clone())
            }
            None => Err(self.undefined_variable_error(&name.name, name.span)),
        }
    }

    fn visit_instanciate_expr(
        &mut self,
        expr: Node<InstanciateExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        let inst_name = expr.borrow().name.clone();

        // for the purpose of this function, struct fields will be referred to as params, and the instanciation fields are just fields
        let params = match self.typedefs.get(&inst_name.name) {
            Some(Type::Struct(_, params)) => params,
            Some(_) => panic!("non-struct type in typedefs, update this"),
            None => return Err(self.undefined_type_error(&inst_name.name, inst_name.span)),
        }
        .clone();

        // ensure that all `params` keys are in fields
        // expr.fields is of type Vec<(String, Expression)>
        for (param_name, _) in params.iter() {
            if !expr
                .borrow()
                .fields
                .iter()
                .any(|(field_name, _)| field_name.name == *param_name)
            {
                return Err(self.error(
                    format!(
                        "expected field '{}' to be defined in instanciation of struct '{}'",
                        param_name, inst_name.name
                    ),
                    inst_name.span,
                ));
            }
        }

        // check that each field is actually a parameter
        for (field_name, field_expr) in expr.borrow().fields.iter() {
            if !params.contains_key(&field_name.name) {
                return Err(self.error(
                    format!(
                        "expected field '{}' to be defined in instanciation of struct '{}'",
                        field_name, inst_name.name
                    ),
                    inst_name.span,
                ));
            }

            // check that the field is the correct type
            let field_type = field_expr.accept(self)?;
            let expected_type = &params[&field_name.name];

            if &field_type != expected_type {
                return Err(self.expected_error(expected_type, &field_type, field_expr.span()));
            }
        }

        expr.borrow_mut().typ = Some(self.typedefs.get(&inst_name.name).cloned().unwrap());
        Ok(self.typedefs.get(&inst_name.name).cloned().unwrap())
    }

    fn visit_call_expr(&mut self, expr: Node<CallExpr>) -> FlamaResult<Self::ExpressionOutput> {
        let callee_type = expr.borrow().callee.accept(self)?;

        let typ = match callee_type {
            Type::Function(func_type) => {
                let mut args = Vec::new();

                for arg in expr.borrow().args.iter() {
                    args.push(arg.accept(self)?);
                }

                if args.len() != func_type.params.len() {
                    return Err(self.arity_error(
                        func_type.params.len(),
                        args.len(),
                        expr.borrow().init.span,
                    ));
                }

                for (i, arg) in args.into_iter().enumerate() {
                    let expected_type = self.type_identifier_to(func_type.params[i].clone())?;
                    if arg != expected_type {
                        return Err(self.expected_error(
                            &expected_type,
                            &arg,
                            expr.borrow().init.span,
                        ));
                    }
                }

                self.type_identifier_to(func_type.return_type.unwrap_or(Type::Void))?
            }
            _ => {
                return Err(self.expected_error(
                    &Type::Function(Box::default()),
                    &callee_type,
                    expr.borrow().init.span,
                ))
            }
        };

        expr.borrow_mut().typ = Some(typ.clone());
        Ok(typ)
    }

    fn visit_assign_expr(
        &mut self,
        expr: Node<AssignExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        let value_type = expr.borrow().value.accept(self)?;
        let var_type = self.environment.get(&expr.borrow().name.name);
        let var_type = var_type.unwrap();

        if var_type == &value_type {
            Ok(value_type)
        } else {
            Err(self.expected_error(var_type, &value_type, expr.borrow().init.span))
        }
    }

    fn visit_get_expr(&mut self, expr: Node<GetExpr>) -> FlamaResult<Self::ExpressionOutput> {
        let object_type = expr.borrow().object.accept(self)?;

        let typ = match object_type {
            Type::Struct(_, fields) => {
                let field_name = expr.borrow().name.name.clone();
                let field_type = fields.get(&field_name);

                if field_type.is_none() {
                    return Err(self.error(
                        format!(
                            "type '{}' has no field '{}'",
                            expr.borrow().object.get_type().unwrap_or(Type::None),
                            field_name
                        ),
                        expr.borrow().init.span,
                    ));
                }

                field_type.unwrap().clone()
            }
            _ => {
                return Err(self.error(
                    format!("type '{}' is not a struct", object_type),
                    expr.borrow().init.span,
                ));
            }
        };

        expr.borrow_mut().typ = Some(typ.clone());
        Ok(typ)
    }

    fn visit_set_expr(&mut self, expr: Node<SetExpr>) -> FlamaResult<Self::ExpressionOutput> {
        expr.borrow().object.accept(self)?;
        expr.borrow().value.accept(self)?;
        todo!()
    }

    fn visit_subscript_expr(
        &mut self,
        expr: Node<SubscriptExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        let object_type = expr.borrow().object.accept(self)?;
        let index_type = expr.borrow().index.accept(self)?;

        if index_type != Type::Number {
            return Err(self.expected_error(&Type::Number, &index_type, expr.borrow().init.span));
        }

        let typ = match object_type {
            Type::List(inner) => *inner,
            _ => {
                return Err(self.expected_error(
                    &Type::List(Box::new(Type::Any)),
                    &object_type,
                    expr.borrow().init.span,
                ))
            }
        };

        expr.borrow_mut().typ = Some(typ.clone());
        Ok(typ)
    }

    fn visit_block_stmt(&mut self, stmt: Node<BlockStmt>) -> FlamaResult<Self::StatementOutput> {
        self.environment.push();
        for statement in &stmt.borrow().statements {
            statement.accept(self)?;
        }
        self.environment.pop();

        Ok(())
    }

    fn visit_if_stmt(&mut self, stmt: Node<IfStmt>) -> FlamaResult<Self::StatementOutput> {
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

    fn visit_while_stmt(&mut self, stmt: Node<WhileStmt>) -> FlamaResult<Self::StatementOutput> {
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
        _: Node<ContinueStmt>,
    ) -> FlamaResult<Self::StatementOutput> {
        Ok(())
    }

    fn visit_break_stmt(
        &mut self,
        _stmt: Node<BreakStmt>,
    ) -> FlamaResult<Self::StatementOutput> {
        Ok(())
    }

    fn visit_return_stmt(
        &mut self,
        stmt: Node<ReturnStmt>,
    ) -> FlamaResult<Self::StatementOutput> {
        let value_type = if let Some(value) = &stmt.borrow().value {
            value.accept(self)?
        } else {
            Type::Void
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

    fn visit_let_stmt(&mut self, stmt: Node<LetStmt>) -> FlamaResult<Self::StatementOutput> {
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
            (Some(type_annotation), None) => {
                // other types than `let` can be uninitialized because storing data
                if type_annotation.is_primitive() && stmt.borrow().kind == VariableType::Let {
                    return Err(self.error(
                        format!(
                            "type '{}' is a primitive/struct, and must be initialized with a value",
                            type_annotation
                        ),
                        stmt.borrow().type_annotation.as_ref().unwrap().span(),
                    ));
                }

                type_annotation
            }
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
        stmt: Node<ExpressionStmt>,
    ) -> FlamaResult<Self::StatementOutput> {
        stmt.borrow().expression.accept(self)?;
        Ok(())
    }

    fn visit_event_item(&mut self, decl: Node<EventItem>) -> FlamaResult<Self::ItemOutput> {
        decl.borrow().body.accept(self)?;
        Ok(())
    }

    fn visit_function_item(
        &mut self,
        decl: Node<FunctionItem>,
    ) -> FlamaResult<Self::ItemOutput> {
        // signature into scope already handled in Self::parse()

        self.environment.push();
        let typ = decl
            .borrow()
            .signature
            .return_type
            .clone()
            .map(|te| te.typ)
            .unwrap_or(Type::Void);
        self.return_type = self.type_identifier_to(typ)?;

        for param in &decl.borrow().signature.params {
            self.environment.define(
                param.0.name.clone(),
                self.type_identifier_to(param.1.typ.clone())?,
            );
        }

        for stmt in &decl.borrow().stmts {
            stmt.accept(self)?;
        }

        self.environment.pop();

        Ok(())
    }

    fn visit_struct_item(&mut self, _decl: Node<StructItem>) -> FlamaResult<Self::ItemOutput> {
        Ok(())
    }
}

impl TypeChecker {
    pub fn new(source_path: Rc<PathBuf>) -> Self {
        Self {
            environment: Environment::new(),
            typedefs: Environment::new(),
            source_path,
            return_type: Type::Void,
        }
    }

    pub fn check(program: Rc<Program>, source_path: Rc<PathBuf>) -> FlamaResults<()> {
        let mut type_checker = TypeChecker::new(source_path);
        let mut errs = vec![];

        // preliminary definitions

        for signature in &program.signatures {
            type_checker.environment.define(
                signature.name.name.clone(),
                Type::Function(Box::new(signature.clone().into())),
            );
        }

        for strukt in &program.typedefs {
            let map = strukt
                .borrow()
                .fields
                .iter()
                .map(|field| match field.1.typ {
                    // the parser does not know about previous typedefs, so we need to look them up.
                    Type::Identifier(ref ident) => (
                        field.0.name.clone(),
                        match type_checker.typedefs.get(&ident.name) {
                            Some(t) => t.clone(),
                            None => {
                                errs.push(
                                    type_checker.undefined_type_error(&ident.name, ident.span),
                                );
                                Type::None
                            }
                        },
                    ),
                    _ => (field.0.name.clone(), field.1.typ.clone()),
                })
                .collect::<HashMap<String, Type>>();

            type_checker.typedefs.define(
                strukt.borrow().name.name.clone(),
                Type::Struct(strukt.borrow().name.name.clone(), map),
            )
        }

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

    /// Replaces Type::Identifier with the actual type.
    fn type_identifier_to(&self, typ: Type) -> FlamaResult<Type> {
        if let Type::Identifier(ident) = typ {
            match self.typedefs.get(&ident.name) {
                Some(t) => Ok(t.clone()),
                None => Err(self.undefined_type_error(&ident.name, ident.span)),
            }
        } else {
            Ok(typ)
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
            format!("expected type '{}', but got type '{}'", expected, actual),
            span,
        )
    }

    fn undefined_variable_error(&self, name: &String, span: Span) -> FlamaError {
        FlamaError {
            message: format!("undefined variable '{}'", name),
            span,
            error_type: ErrorType::Name,
            source_path: self.source_path.clone(),
        }
    }

    fn undefined_type_error(&self, name: &String, span: Span) -> FlamaError {
        self.error(format!("undefined type '{}'", name), span)
    }

    fn cannot_infer_error(&self, variable: &String, span: Span) -> FlamaError {
        self.error(
            format!("cannot infer type for variable '{}'", variable),
            span,
        )
    }

    fn arity_error(&self, expected: usize, actual: usize, span: Span) -> FlamaError {
        self.error(
            format!("expected {} arguments, but got {}", expected, actual),
            span,
        )
    }
}
