use std::{mem, rc::Rc};

use crate::{
    check::{environment::Environment, types::Type},
    error::FlamaResult,
    parser::{
        ast::{
            AssignExpr, BinaryExpr, BinaryOperator, BlockStmt, BreakStmt, CallExpr, ContinueStmt,
            EventItem, Expression, ExpressionStmt, FunctionItem, GetExpr, IfStmt, InstanciateExpr,
            LetStmt, ListExpr, LiteralExpr, LiteralKind, NameExpr, NodePtr, Program, ReturnStmt,
            SetExpr, StructItem, UnaryExpr, UnaryOperator, WhileStmt,
        },
        visitor::{ExpressionVisitable, ItemVisitable, StatementVisitable, Visitor},
    },
};

use super::{
    builder::{self, Args, BlockInfo, CodeBlock, CodeTemplate, CodeValue, Info, VariableScope},
    namer::Namer,
};

pub struct Compiler {
    pub templates: Vec<CodeTemplate>,
    pub current_stack: Vec<CodeBlock>,
    /// A mapping of variable names to their code values.
    pub environment: Environment<String, CodeValue>,
    pub namer: Namer,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            templates: vec![],
            current_stack: vec![],
            environment: Environment::new(),
            namer: Namer::new(),
        }
    }

    pub fn compile_program(program: Rc<Program>) -> Vec<CodeTemplate> {
        let mut compiler = Compiler::new();

        for signature in program.signatures.iter() {
            compiler.environment.define(
                signature.name.name.clone(),
                signature.name.name.clone().into(),
            );
        }

        for item in program.items.iter() {
            item.accept(&mut compiler).unwrap();
        }

        compiler.templates
    }

    fn get_rv() -> CodeValue {
        CodeValue::Variable {
            name: "$rv".to_string(),
            scope: VariableScope::default(),
        }
    }

    fn get_args() -> CodeValue {
        CodeValue::Variable {
            name: "$args".to_string(),
            scope: VariableScope::default(),
        }
    }

    fn get_scope_count() -> CodeValue {
        CodeValue::Variable {
            name: "$sc".to_string(),
            scope: VariableScope::default(),
        }
    }

    fn get_scope_count_prefix() -> CodeValue {
        "SC\\%var($sc)".to_string().into()
    }

    fn get_scope_count_var(var: &str) -> CodeValue {
        CodeValue::Variable {
            name: format!("SC\\%var($sc)_{}", var),
            scope: VariableScope::default(),
        }
    }

    fn push_scope(&mut self) {
        self.current_stack.push(
            BlockInfo::SetVariable {
                args: Args {
                    items: vec![Self::get_scope_count().as_item(0)],
                },
                action: "+=".to_string(),
            }
            .into(),
        );
        self.current_stack.push(
            BlockInfo::SetVariable {
                args: Args {
                    items: vec![
                        Self::get_scope_count_prefix().as_item(0),
                        CodeValue::Tag {
                            option: "Any part of name".to_string(),
                            tag: "Match Requirement".to_string(),
                            action: "PurgeVars".to_string(),
                            block: "set_var".to_string(),
                        }
                        .as_item(25),
                        CodeValue::Tag {
                            option: "False".to_string(),
                            tag: "Ignore Case".to_string(),
                            action: "PurgeVars".to_string(),
                            block: "set_var".to_string(),
                        }
                        .as_item(26),
                    ],
                },
                action: "PurgeVars".to_string(),
            }
            .into(),
        );
    }

    fn pop_scope(&mut self) {
        self.current_stack.push(
            BlockInfo::SetVariable {
                args: Args {
                    items: vec![
                        Self::get_scope_count_prefix().as_item(0),
                        CodeValue::Tag {
                            option: "Any part of name".to_string(),
                            tag: "Match Requirement".to_string(),
                            action: "PurgeVars".to_string(),
                            block: "set_var".to_string(),
                        }
                        .as_item(25),
                        CodeValue::Tag {
                            option: "False".to_string(),
                            tag: "Ignore Case".to_string(),
                            action: "PurgeVars".to_string(),
                            block: "set_var".to_string(),
                        }
                        .as_item(26),
                    ],
                },
                action: "PurgeVars".to_string(),
            }
            .into(),
        );

        self.current_stack.push(
            BlockInfo::SetVariable {
                args: Args {
                    items: vec![Self::get_scope_count().as_item(0)],
                },
                action: "-=".to_string(),
            }
            .into(),
        );
    }
}

impl Visitor for Compiler {
    type ExpressionOutput = CodeValue;
    type StatementOutput = ();
    type ItemOutput = ();

    fn visit_unary_expr(
        &mut self,
        expr: NodePtr<UnaryExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        let val = expr.accept(self)?;
        match (&expr.borrow().operator, val) {
            (UnaryOperator::Identity, val) => Ok(val),
            (UnaryOperator::Negate, CodeValue::Number { name }) => Ok(CodeValue::Number {
                name: format!("%math(0-{})", name),
            }),
            (UnaryOperator::Not, CodeValue::Number { name }) => Ok(CodeValue::Number {
                name: format!("%math(0-1*{}+1)", name),
            }),
            _ => unreachable!(),
        }
    }

    fn visit_binary_expr(
        &mut self,
        expr: NodePtr<BinaryExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        let typ1 = expr.borrow().left.get_type().unwrap();
        let typ2 = expr.borrow().right.get_type().unwrap();
        let val1 = expr.borrow().left.accept(self)?;
        let val2 = expr.borrow().right.accept(self)?;

        if expr.borrow().operator.is_math_safe() {
            return Ok(CodeValue::Number {
                name: format!(
                    "%math({}{}{})",
                    val1.as_string().unwrap(),
                    expr.borrow().operator,
                    val2.as_string().unwrap()
                ),
            });
        }

        let mut result_action: String;

        match (typ1, &expr.borrow().operator, typ2) {
            (Type::String, BinaryOperator::Add, Type::String) => {
                return Ok(CodeValue::Text {
                    name: format!("{}{}", val1.as_string().unwrap(), val2.as_string().unwrap()),
                })
            }
            _ => result_action = expr.borrow().operator.to_string(),
        }

        if result_action == "==" {
            result_action = "=".to_string();
        }

        let result_var = self.namer.get_rand_var("binop");
        self.current_stack.push(
            BlockInfo::IfVariable {
                args: Args {
                    items: vec![val1.as_item(0), val2.as_item(1)],
                },
                action: result_action,
            }
            .into(),
        );
        self.current_stack.push(builder::open_bracket());
        self.current_stack.push(builder::set_var(
            result_var.clone(),
            CodeValue::Number {
                name: "1".to_string(),
            },
        ));
        self.current_stack.push(builder::close_bracket());
        self.current_stack.push(builder::else_block());
        self.current_stack.push(builder::open_bracket());
        self.current_stack
            .push(builder::set_var(result_var.clone(), 0.into()));
        self.current_stack.push(builder::close_bracket());

        Ok(result_var)
    }

    fn visit_literal_expr(
        &mut self,
        expr: NodePtr<LiteralExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        match &expr.borrow().kind {
            LiteralKind::Number(n) => Ok(CodeValue::Number {
                name: n.to_string(),
            }),
            LiteralKind::String(s) => Ok(s.clone().into()),
            LiteralKind::Boolean(b) => Ok(CodeValue::Number {
                name: if b == &true {
                    "1".to_string()
                } else {
                    "0".to_string()
                },
            }),
        }
    }

    fn visit_list_expr(&mut self, expr: NodePtr<ListExpr>) -> FlamaResult<Self::ExpressionOutput> {
        let list_var = self.namer.get_rand_var("list");
        let mut values = vec![list_var.clone().as_item(0)];
        for (i, element) in expr.borrow().elements.iter().enumerate() {
            values.push(element.accept(self)?.as_item((i as i32) + 1));
        }

        self.current_stack.push(
            BlockInfo::SetVariable {
                args: Args { items: values },
                action: "CreateList".to_string(),
            }
            .into(),
        );

        Ok(list_var)
    }

    fn visit_name_expr(&mut self, expr: NodePtr<NameExpr>) -> FlamaResult<Self::ExpressionOutput> {
        Ok(self
            .environment
            .get(&expr.borrow().name.name)
            .unwrap()
            .clone())
    }

    fn visit_instanciate_expr(
        &mut self,
        _expr: NodePtr<InstanciateExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        todo!()
    }

    fn visit_call_expr(&mut self, expr: NodePtr<CallExpr>) -> FlamaResult<Self::ExpressionOutput> {
        let mut args = vec![];
        for arg in expr.borrow().args.iter() {
            args.push((arg.accept(self)?, arg.get_type().unwrap()));
        }

        if let Some(builtin) = &expr.borrow().builtin {
            let base = if let Expression::Get(ge) = &expr.borrow().callee {
                Some((
                    ge.borrow().object.accept(self)?,
                    ge.borrow().object.get_type().as_ref().unwrap().clone(),
                ))
            } else {
                None
            };

            let val = builtin.compile(self, base, args).unwrap_or(0.into());
            return Ok(val);
        } else {
            let mut args = vec![Self::get_args().as_item(0)];
            for (i, arg) in expr.borrow().args.iter().enumerate() {
                args.push(arg.accept(self)?.as_item((i as i32) + 1));
            }

            self.current_stack.push(
                BlockInfo::SetVariable {
                    args: Args { items: args },
                    action: "CreateList".to_string(),
                }
                .into(),
            );

            let callee = expr.borrow().callee.accept(self)?;
            self.current_stack.push(
                BlockInfo::CallFunction {
                    args: Args::default(),
                    data: callee.as_string().unwrap(),
                }
                .into(),
            );

            Ok(Self::get_rv())
        }
    }

    fn visit_assign_expr(
        &mut self,
        expr: NodePtr<AssignExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        let value = expr.borrow().value.accept(self)?;
        let name = self
            .environment
            .get(&expr.borrow().name.name)
            .unwrap()
            .clone();

        self.current_stack
            .push(builder::set_var(name, value.clone()));

        Ok(value)
    }

    fn visit_get_expr(&mut self, _: NodePtr<GetExpr>) -> FlamaResult<Self::ExpressionOutput> {
        todo!()
    }

    fn visit_set_expr(&mut self, _: NodePtr<SetExpr>) -> FlamaResult<Self::ExpressionOutput> {
        todo!()
    }

    fn visit_block_stmt(&mut self, stmt: NodePtr<BlockStmt>) -> FlamaResult<Self::StatementOutput> {
        // self.push_scope();

        for statement in &stmt.borrow().statements {
            statement.accept(self)?;
        }

        // self.pop_scope();

        Ok(())
    }

    fn visit_if_stmt(&mut self, stmt: NodePtr<IfStmt>) -> FlamaResult<Self::StatementOutput> {
        let condition = stmt.borrow().condition.accept(self)?;

        self.current_stack.push(builder::if_var(
            condition,
            CodeValue::Number {
                name: "1".to_string(),
            },
        ));

        self.current_stack.push(builder::open_bracket());
        self.push_scope();
        stmt.borrow().body.accept(self)?;
        self.pop_scope();
        self.current_stack.push(builder::close_bracket());

        if let Some(alt) = &stmt.borrow().alternative {
            self.current_stack.push(builder::else_block());
            self.current_stack.push(builder::open_bracket());
            self.push_scope();
            alt.accept(self)?;
            self.pop_scope();
            self.current_stack.push(builder::close_bracket());
        }

        Ok(())
    }

    fn visit_while_stmt(&mut self, stmt: NodePtr<WhileStmt>) -> FlamaResult<Self::StatementOutput> {
        let condition = stmt.borrow().condition.accept(self)?;

        self.current_stack.push(
            BlockInfo::Repeat {
                args: Args {
                    items: vec![
                        condition.as_item(0),
                        CodeValue::Number {
                            name: "1".to_string(),
                        }
                        .as_item(1),
                    ],
                },
                action: "While".to_string(),
                sub_action: Some("=".to_string()),
            }
            .into(),
        );

        self.current_stack.push(builder::open_repeat_bracket());
        stmt.borrow().body.accept(self)?;
        self.current_stack.push(builder::close_repeat_bracket());

        Ok(())
    }

    fn visit_continue_stmt(
        &mut self,
        _: NodePtr<ContinueStmt>,
    ) -> FlamaResult<Self::StatementOutput> {
        self.current_stack.push(
            BlockInfo::Control {
                args: Args::default(),
                action: "Skip".to_string(),
            }
            .into(),
        );

        Ok(())
    }

    fn visit_break_stmt(&mut self, _: NodePtr<BreakStmt>) -> FlamaResult<Self::StatementOutput> {
        self.current_stack.push(
            BlockInfo::Control {
                args: Args::default(),
                action: "StopRepeat".to_string(),
            }
            .into(),
        );

        Ok(())
    }

    fn visit_return_stmt(
        &mut self,
        stmt: NodePtr<ReturnStmt>,
    ) -> FlamaResult<Self::StatementOutput> {
        if let Some(value) = &stmt.borrow().value {
            let value = value.accept(self)?;

            self.current_stack
                .push(builder::set_var(Self::get_rv(), value))
        } else {
            self.current_stack
                .push(builder::set_var(Self::get_rv(), 0.into()));
        }

        Ok(())
    }

    fn visit_let_stmt(&mut self, stmt: NodePtr<LetStmt>) -> FlamaResult<Self::StatementOutput> {
        let name = stmt.borrow().name.name.clone();

        let to_set = Self::get_scope_count_var(&name);

        let value = if let Some(value) = &stmt.borrow().value {
            value.accept(self)?
        } else {
            0.into()
        };

        self.current_stack
            .push(builder::set_var(to_set.clone(), value));

        self.environment.define(name, to_set);

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
        self.current_stack.push(
            BlockInfo::PlayerEvent {
                args: Args::default(),
                action: decl.borrow().name.name.clone(),
            }
            .into(),
        );
        decl.borrow().body.accept(self)?;

        let stack = mem::replace(&mut self.current_stack, Vec::new());
        self.templates.push(CodeTemplate { blocks: stack });
        // self.current_stack

        Ok(())
    }

    fn visit_function_item(
        &mut self,
        decl: NodePtr<FunctionItem>,
    ) -> FlamaResult<Self::ItemOutput> {
        self.current_stack.push(CodeBlock {
            id: "block".to_string(),
            info: Info::BlockInfo(BlockInfo::Function {
                args: Args::default(),
                data: decl.borrow().signature.name.name.clone(),
            }),
        });
        self.push_scope();

        for (i, param) in decl.borrow().signature.params.iter().enumerate() {
            let name = param.0.name.clone();
            let to_set = Self::get_scope_count_var(&name);

            self.environment.define(name, to_set.clone());
            self.current_stack.push(
                BlockInfo::SetVariable {
                    args: Args {
                        items: vec![
                            to_set.as_item(0),
                            Self::get_args().as_item(1),
                            CodeValue::Number {
                                name: (i + 1).to_string(),
                            }
                            .as_item(2),
                        ],
                    },
                    action: "GetListValue".to_string(),
                }
                .into(),
            )
        }

        for stmt in &decl.borrow().stmts {
            stmt.accept(self)?;
        }
        self.pop_scope();

        let stack = mem::replace(&mut self.current_stack, Vec::new());
        self.templates.push(CodeTemplate { blocks: stack });

        Ok(())
    }

    fn visit_struct_item(&mut self, _: NodePtr<StructItem>) -> FlamaResult<Self::ItemOutput> {
        Ok(())
    }
}
