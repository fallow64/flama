pub mod builder;
pub mod namer;

use std::{mem, rc::Rc};

use crate::{
    check::{environment::Environment, types::Type},
    error::FlamaResult,
    parser::{
        ast::{
            AssignExpr, BinaryExpr, BinaryOperator, BlockStmt, BreakStmt, CallExpr, ContinueStmt,
            EventItem, ExpressionStmt, FunctionItem, GetExpr, Identifier, IfStmt, InstanciateExpr,
            LetStmt, ListExpr, LiteralExpr, LiteralKind, NameExpr, NodePtr, Program, ReturnStmt,
            SetExpr, StructItem, SubscriptExpr, UnaryExpr, UnaryOperator, VariableType, WhileStmt,
        },
        visitor::{ExpressionVisitable, ItemVisitable, StatementVisitable, Visitor},
    },
};

use self::{
    builder::{Args, BlockInfo, CodeBlock, CodeTemplate, CodeValue, Info, VariableScope},
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

    /// Compile the program, returning a vector of code templates.
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

    /// Gets the return value of a function.
    fn get_rv() -> CodeValue {
        CodeValue::Variable {
            name: "$rv".to_string(),
            scope: VariableScope::default(),
        }
    }

    /// Gets the arguments that are passed into a function.
    fn get_args() -> CodeValue {
        CodeValue::Variable {
            name: "$args".to_string(),
            scope: VariableScope::default(),
        }
    }

    /// Gets the scope count.
    fn get_scope_count() -> CodeValue {
        CodeValue::Variable {
            name: "$sc".to_string(),
            scope: VariableScope::default(),
        }
    }

    /// Returns a `SC\%var($sc)` variable.
    /// Used for clearing the scope.
    fn get_scope_count_prefix() -> CodeValue {
        "SC\\%var($sc)".to_string().into()
    }

    // Returns a `SC\%var($sc)_<var>` variable.
    // Used for scoping purposes
    fn get_scope_count_var(var: &str) -> CodeValue {
        CodeValue::Variable {
            name: format!("SC\\%var($sc)_{}", var),
            scope: VariableScope::default(),
        }
    }

    /// Increments `$sc` and clears the scope.
    fn push_scope(&mut self) {
        self.current_stack.push(
            BlockInfo::SetVariable {
                args: vec![Self::get_scope_count().into_item(0)].into(),
                action: "+=".to_string(),
            }
            .into(),
        );
        self.current_stack.push(
            BlockInfo::SetVariable {
                args: vec![
                    Self::get_scope_count_prefix().into_item(0),
                    CodeValue::Tag {
                        option: "Any part of name".to_string(),
                        tag: "Match Requirement".to_string(),
                        action: "PurgeVars".to_string(),
                        block: "set_var".to_string(),
                    }
                    .into_item(25),
                    CodeValue::Tag {
                        option: "False".to_string(),
                        tag: "Ignore Case".to_string(),
                        action: "PurgeVars".to_string(),
                        block: "set_var".to_string(),
                    }
                    .into_item(26),
                ]
                .into(),
                action: "PurgeVars".to_string(),
            }
            .into(),
        );
    }

    /// Clears the scope and decreases `$sc`.
    fn pop_scope(&mut self) {
        self.current_stack.push(
            BlockInfo::SetVariable {
                args: vec![
                    Self::get_scope_count_prefix().into_item(0),
                    CodeValue::Tag {
                        option: "Any part of name".to_string(),
                        tag: "Match Requirement".to_string(),
                        action: "PurgeVars".to_string(),
                        block: "set_var".to_string(),
                    }
                    .into_item(25),
                    CodeValue::Tag {
                        option: "False".to_string(),
                        tag: "Ignore Case".to_string(),
                        action: "PurgeVars".to_string(),
                        block: "set_var".to_string(),
                    }
                    .into_item(26),
                ]
                .into(),
                action: "PurgeVars".to_string(),
            }
            .into(),
        );

        self.current_stack.push(
            BlockInfo::SetVariable {
                args: vec![Self::get_scope_count().into_item(0)].into(),
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
                // yippee no order of operations
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
            // TODO: This is a hack, but it works for now
            result_action = "=".to_string();
        }

        let result_var = self.namer.get_rand_var("binop");
        self.current_stack.push(
            BlockInfo::IfVariable {
                args: vec![val1.into_item(0), val2.into_item(1)].into(),
                action: result_action,
            }
            .into(),
        );
        self.current_stack.push(builder::open_bracket());
        self.current_stack
            .push(builder::set_var(result_var.clone(), 1.into()));
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
        let mut values = vec![list_var.clone().into_item(0)];
        for (i, element) in expr.borrow().elements.iter().enumerate() {
            values.push(element.accept(self)?.into_item((i as i32) + 1));
        }

        self.current_stack.push(
            BlockInfo::SetVariable {
                args: values.into(),
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

        let mut args = vec![Self::get_args().into_item(0)];
        for (i, arg) in expr.borrow().args.iter().enumerate() {
            args.push(arg.accept(self)?.into_item((i as i32) + 1));
        }

        self.current_stack.push(
            BlockInfo::SetVariable {
                args: args.into(),
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

    fn visit_subscript_expr(
        &mut self,
        expr: NodePtr<SubscriptExpr>,
    ) -> FlamaResult<Self::ExpressionOutput> {
        let result = self.namer.get_rand_var("subscript");
        let object = expr.borrow().object.accept(self)?;
        let index = expr.borrow().index.accept(self)?;
        self.current_stack.push(
            BlockInfo::SetVariable {
                args: vec![
                    result.clone().into_item(0),
                    object.into_item(1),
                    index.add_one().into_item(2),
                ]
                .into(),
                action: "GetListValue".to_string(),
            }
            .into(),
        );

        Ok(result)
    }

    fn visit_get_expr(&mut self, expr: NodePtr<GetExpr>) -> FlamaResult<Self::ExpressionOutput> {
        let result = self.namer.get_rand_var("get");
        let object = expr.borrow().object.accept(self)?;
        self.current_stack.push(
            BlockInfo::SetVariable {
                args: vec![
                    result.clone().into_item(0),
                    object.into_item(1),
                    <Identifier as Into<CodeValue>>::into(expr.borrow().name.clone()).into_item(2),
                ]
                .into(),
                action: "GetDictValue".to_string(),
            }
            .into(),
        );

        Ok(result)
    }

    fn visit_set_expr(&mut self, _: NodePtr<SetExpr>) -> FlamaResult<Self::ExpressionOutput> {
        todo!()
    }

    fn visit_block_stmt(&mut self, stmt: NodePtr<BlockStmt>) -> FlamaResult<Self::StatementOutput> {
        self.push_scope();

        for statement in &stmt.borrow().statements {
            statement.accept(self)?;
        }

        self.pop_scope();

        Ok(())
    }

    fn visit_if_stmt(&mut self, stmt: NodePtr<IfStmt>) -> FlamaResult<Self::StatementOutput> {
        let condition = stmt.borrow().condition.accept(self)?;

        self.current_stack
            .push(builder::if_var(condition, 1.into()));

        self.current_stack.push(builder::open_bracket());
        stmt.borrow().body.accept(self)?;
        self.current_stack.push(builder::close_bracket());

        if let Some(alt) = &stmt.borrow().alternative {
            self.current_stack.push(builder::else_block());
            self.current_stack.push(builder::open_bracket());
            alt.accept(self)?;
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
                        condition.into_item(0),
                        <i32 as Into<CodeValue>>::into(1).into_item(1),
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

        // is this really the best way to do this?
        let to_set = match stmt.borrow().kind {
            VariableType::Save => CodeValue::Variable {
                name: name.clone(),
                scope: VariableScope::Save,
            },
            VariableType::Local => CodeValue::Variable {
                name: name.clone(),
                scope: VariableScope::Local,
            },
            VariableType::Game => CodeValue::Variable {
                name: name.clone(),
                scope: VariableScope::Global,
            },
            VariableType::Let => Self::get_scope_count_var(&name),
        };

        if let Some(value) = &stmt.borrow().value {
            let code_value = value.accept(self)?;
            self.current_stack
                .push(builder::set_var(to_set.clone(), code_value));
        } else if stmt.borrow().kind == VariableType::Let {
            // don't use this for global, save, and local because we might want to use data and not replace it
            self.current_stack
                .push(builder::set_var(to_set.clone(), 0.into()));
        }

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

        let stack = mem::take(&mut self.current_stack);
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
                            to_set.into_item(0),
                            Self::get_args().into_item(1),
                            CodeValue::Number {
                                name: (i + 1).to_string(),
                            }
                            .into_item(2),
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

        let stack = mem::take(&mut self.current_stack);
        self.templates.push(CodeTemplate { blocks: stack });

        Ok(())
    }

    fn visit_struct_item(&mut self, _: NodePtr<StructItem>) -> FlamaResult<Self::ItemOutput> {
        Ok(())
    }
}
