mod html;
mod terminal;

use std::collections::HashMap;

use by_address::ByAddress;
use itertools::Itertools;

pub use html::HtmlFormatter;
pub use terminal::TerminalFormatter;

use crate::ast::*;
use crate::ExecutionEvent;
use crate::VariableMapping;

use crate::Value;

#[derive(PartialEq, Eq)]
pub enum DecorationType {
    InactiveVariable(usize),
    ActiveVariable(usize),
    ActiveExpr,
    Keyword,
    Type,
    Value,
    Operator,
    Comment,
    Arrow,
    Alert,
    None,
}

#[derive(Clone)]
pub enum ExprState {
    Inactive,
    Active,
    Replaced(Value),
}

#[derive(Clone, PartialEq, Eq)]
pub enum StatementState {
    Active,
    Inactive,
}

pub trait AstDecorator {
    fn var_value(&self, _var: &Ident) -> Option<Value> {
        None
    }

    fn var_decoration(&self, _var: &Ident) -> DecorationType {
        DecorationType::InactiveVariable(0)
    }

    fn expression_state(&self, _expr: &Expr) -> ExprState {
        ExprState::Inactive
    }

    fn statement_state(&self, _stmt: &Statement) -> StatementState {
        StatementState::Inactive
    }
}

pub struct NoopDecorator {}

impl NoopDecorator {
    pub fn new() -> NoopDecorator {
        NoopDecorator {}
    }
}

impl Default for NoopDecorator {
    fn default() -> Self {
        Self::new()
    }
}

impl AstDecorator for NoopDecorator {}

pub struct VarMappingDecorator<'a> {
    var_mapping: VariableMapping<'a>,
    known_vars: HashMap<ByAddress<&'a Ident>, usize>,
}

impl<'a> VarMappingDecorator<'a> {
    pub fn new(
        var_mapping: VariableMapping<'a>,
        var_order: &[&'a Ident],
    ) -> VarMappingDecorator<'a> {
        let mut known_vars = HashMap::new();
        for (id, var) in var_order.iter().cloned().enumerate() {
            known_vars.insert(ByAddress(var), id);
        }
        VarMappingDecorator {
            var_mapping,
            known_vars,
        }
    }
}

impl<'a> AstDecorator for VarMappingDecorator<'a> {
    fn var_decoration(&self, var: &Ident) -> DecorationType {
        let actual_var = self.var_mapping.get_var(var);
        let id = self
            .known_vars
            .get(&ByAddress(actual_var))
            .expect(&*format!("Unknown variable {}", var));
        DecorationType::InactiveVariable(*id)
    }
}

pub struct ExecutionDecorator<'a> {
    var_mapping_decorator: VarMappingDecorator<'a>,
    expr_states: HashMap<ByAddress<&'a Expr>, ExprState>,
    var_values: HashMap<ByAddress<&'a Ident>, Value>,
    active_statement: Option<ByAddress<&'a Statement>>,
}

impl<'a> ExecutionDecorator<'a> {
    pub fn new(
        var_mapping: VariableMapping<'a>,
        var_order: &[&'a Ident],
    ) -> ExecutionDecorator<'a> {
        let var_mapping_decorator = VarMappingDecorator::new(var_mapping, var_order);
        ExecutionDecorator {
            var_mapping_decorator,
            var_values: HashMap::new(),
            expr_states: HashMap::new(),
            active_statement: None,
        }
    }

    pub fn process_event(&mut self, event: &ExecutionEvent<'a>) {
        match event {
            ExecutionEvent::ExprEval(expr) => {
                self.expr_states.insert(ByAddress(expr), ExprState::Active);
            }
            ExecutionEvent::ExprReplaced(expr, val) => {
                self.expr_states
                    .insert(ByAddress(expr), ExprState::Replaced(val.clone()));
            }
            ExecutionEvent::Assign(var, val) => {
                self.var_values.insert(ByAddress(var), val.clone());
            }
            ExecutionEvent::Return(_) => {
                self.active_statement = None;
                self.expr_states.clear();
            }
            ExecutionEvent::StatementActive(stmt) => {
                self.active_statement = Some(ByAddress(stmt));
                self.expr_states.clear();
            }
        }
    }
}

impl<'a> AstDecorator for ExecutionDecorator<'a> {
    fn var_decoration(&self, var: &Ident) -> DecorationType {
        self.var_mapping_decorator.var_decoration(var)
    }

    fn var_value(&self, var: &Ident) -> Option<Value> {
        self.var_values.get(&ByAddress(var)).cloned()
    }

    fn expression_state(&self, expr: &Expr) -> ExprState {
        self.expr_states
            .get(&ByAddress(expr))
            .cloned()
            .unwrap_or(ExprState::Inactive)
    }

    fn statement_state(&self, stmt: &Statement) -> StatementState {
        if self.active_statement == Some(ByAddress(stmt)) {
            StatementState::Active
        } else {
            StatementState::Inactive
        }
    }
}

pub trait AstFormatter<D>
where
    D: AstDecorator,
{
    fn apply_decoration(&self, _dec: &DecorationType, data: String) -> String {
        data
    }

    fn fmt_nl(&self, _dec: &D) -> String {
        "\n".into()
    }

    fn fmt_line(&self, indent: usize, statement_state: StatementState, line: String) -> String {
        format!(
            "{}{:<2$}{line}\n",
            if statement_state == StatementState::Active {
                ">"
            } else {
                " "
            },
            "",
            indent * 2
        )
    }

    fn fmt_value(&self, value: &Value) -> String {
        match value {
            Value::Integer(v) => format!("{v}"),
            Value::Float(v) => format!("{v}"),
            Value::String(v) => format!("\"{v}\""),
            Value::Bool(b) => {
                if *b {
                    "true"
                } else {
                    "false"
                }
            }
            .into(),
            Value::None => "(uninit)".into(),
        }
    }

    fn fmt_decl(&self, ident: String, ty: String, value: Option<String>) -> String {
        if let Some(value) = value {
            format!("{ident}: {ty} > {value}")
        } else {
            format!("{ident}: {ty}")
        }
    }

    fn fmt_assign(&self) -> String {
        "<-".into()
    }

    fn fmt_fnret(&self) -> String {
        "->".into()
    }

    fn fmt_dots(&self) -> String {
        "...".into()
    }

    fn op_sym(&self, op: &Op) -> String {
        match op {
            Op::Sum => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Div => "/",
            Op::Mod => "%",
            Op::Le => "<=",
            Op::Lt => "<",
            Op::Ge => ">=",
            Op::Gt => ">",
            Op::Eq => "==",
            Op::Ne => "!=",
            Op::And => "and",
            Op::Or => "or",
        }
        .into()
    }

    fn type_to_str(&self, ty: &Type) -> String {
        match ty {
            Type::Bool => "bool".into(),
            Type::Void => "".into(),
            Type::String => "string".into(),
            Type::Integer => "integer".into(),
            Type::Float => "float".into(),
            Type::Array(ty) => self.type_to_str(&**ty) + "[]",
        }
    }

    fn format_variable(&self, var: &Ident, dec: &D) -> String {
        self.apply_decoration(&dec.var_decoration(var), var.clone())
    }

    fn format_comment(&self, comm: &Comment) -> String {
        match comm {
            None => "".into(),
            Some(c) => format!(
                " {}",
                self.apply_decoration(&DecorationType::Comment, c.into())
            ),
        }
    }

    fn format_array(&self, arr: &[Expr], dec: &D) -> String {
        format!(
            "[{}]",
            arr.iter()
                .map(|expr| self.format_expr(expr, dec))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }

    fn format_tuple(&self, tuple: &Tuple, dec: &D) -> String {
        format!(
            "({})",
            match tuple {
                Tuple::ExprTuple(ve) => ve
                    .iter()
                    .map(|expr| self.format_expr(expr, dec))
                    .collect::<Vec<String>>(),
                Tuple::DeclTuple(ve) => ve
                    .iter()
                    .map(|decl| format!(
                        "{} {}",
                        self.apply_decoration(&DecorationType::Keyword, String::from("variable")),
                        self.format_decl(decl, dec)
                    ))
                    .collect::<Vec<String>>(),
            }
            .join(", ")
        )
    }

    fn format_decl(&self, decl: &Decl, dec: &D) -> String {
        let ident = self.format_variable(&decl.ident, dec);
        let ty = self.apply_decoration(&DecorationType::Type, self.type_to_str(&decl.ty));
        let val = match &decl.val {
            None => String::from(""),
            Some(expr) => format!(
                " {} {}",
                self.apply_decoration(&DecorationType::Arrow, self.fmt_assign()),
                self.format_expr(expr, dec)
            ),
        };
        format!("{ident}: {ty}{val}")
    }

    fn format_expr(&self, expr: &Expr, dec: &D) -> String {
        let expr_state = dec.expression_state(expr);
        if let ExprState::Replaced(val) = expr_state {
            return self.apply_decoration(&DecorationType::Value, self.fmt_value(&val));
        }
        let expr = match expr {
            Expr::Ref(ident) => self.format_variable(ident, dec),
            Expr::Integer(i) => self.apply_decoration(&DecorationType::Value, format!("{}", i)),
            Expr::Float(f) => self.apply_decoration(&DecorationType::Value, format!("{}", f)),
            Expr::Op(e1, op, e2) => format!(
                "{} {} {}",
                self.format_expr(e1, dec),
                self.apply_decoration(&DecorationType::Operator, self.op_sym(op)),
                self.format_expr(e2, dec),
            ),
            Expr::String(s) => self.apply_decoration(&DecorationType::Value, s.clone()),
            Expr::Array(ve) => self.format_array(ve, dec),
            Expr::Tuple(tuple) => self.format_tuple(tuple, dec),
            Expr::Parens(expr) => format!("({})", self.format_expr(expr, dec)),
            Expr::Not(expr) => format!("not {}", self.format_expr(expr, dec)),
            Expr::ArrayIndex(ae, ie) => format!(
                "{}[{}]",
                self.format_expr(ae, dec),
                self.format_expr(ie, dec)
            ),
            Expr::FunctionCall(f, args) => format!(
                "{}({})",
                f,
                args.iter().map(|x| self.format_expr(x, dec)).format(", ")
            ),
        };
        let decoration = match expr_state {
            ExprState::Active => DecorationType::ActiveExpr,
            ExprState::Inactive => DecorationType::None,
            _ => unreachable!(),
        };
        self.apply_decoration(&decoration, expr)
    }

    fn format_statement(
        &self,
        stmt: &Statement,
        indent: usize,
        comment: String,
        dec: &D,
    ) -> String {
        let statement_state = dec.statement_state(stmt);
        let keyword = |k: &'static str| self.apply_decoration(&DecorationType::Keyword, k.into());
        match stmt {
            Statement::SilentBlank => "".into(),
            Statement::Blank => self.fmt_line(
                indent,
                statement_state,
                format!(
                    "{}{comment}",
                    self.apply_decoration(&DecorationType::Alert, String::from("[???]")),
                ),
            ),
            Statement::Decl(decl) => self.fmt_line(
                indent,
                statement_state,
                format!(
                    "{} {}{comment}",
                    keyword("variable"),
                    self.format_decl(decl, dec)
                ),
            ),
            Statement::Assign(e1, e2) => self.fmt_line(
                indent,
                statement_state,
                format!(
                    "{} {} {}{comment}",
                    self.format_expr(e1, dec),
                    self.apply_decoration(
                        &DecorationType::Operator,
                        self.apply_decoration(&DecorationType::Arrow, self.fmt_assign())
                    ),
                    self.format_expr(e2, dec),
                ),
            ),
            Statement::While(cond, block, comm1, comm2) => {
                format!(
                    "{}{}{}",
                    self.fmt_line(
                        indent,
                        statement_state,
                        format!(
                            "{} {} {} {}",
                            keyword("while"),
                            self.format_expr(cond, dec),
                            keyword("do"),
                            self.format_comment(comm1)
                        ),
                    ),
                    self.format_block(block, indent + 1, dec),
                    self.fmt_line(
                        indent,
                        StatementState::Inactive,
                        format!("{} {}", keyword("end while"), self.format_comment(comm2))
                    )
                )
            }
            Statement::For(decl, left, right, included, block, comm1, comm2) => {
                format!(
                    "{}{}{}",
                    self.fmt_line(
                        indent,
                        statement_state,
                        format!(
                            "{} {} {} [{} {} {}{} {} {}",
                            keyword("for"),
                            self.format_variable(&decl.ident, dec),
                            keyword("in"),
                            self.format_expr(left, dec),
                            self.fmt_dots(),
                            self.format_expr(right, dec),
                            if *included { "]" } else { ")" },
                            keyword("do"),
                            self.format_comment(comm1)
                        )
                    ),
                    self.format_block(block, indent + 1, dec),
                    self.fmt_line(
                        indent,
                        StatementState::Inactive,
                        format!("{} {}", keyword("end for"), self.format_comment(comm2))
                    )
                )
            }
            Statement::If(cond, block1, block2, comm1, comm2, comm3) => {
                let if_line = self.fmt_line(
                    indent,
                    statement_state,
                    format!(
                        "{} {} {} {}",
                        keyword("if"),
                        self.format_expr(cond, dec),
                        keyword("then"),
                        self.format_comment(comm1)
                    ),
                );
                let block1 = self.format_block(block1, indent + 1, dec);
                let block2 = if !block2.is_empty() {
                    self.fmt_line(
                        indent,
                        StatementState::Inactive,
                        format!("{} {}", keyword("else"), self.format_comment(comm2)),
                    ) + &self.format_block(block2, indent + 1, dec)
                } else {
                    "".into()
                };
                format!(
                    "{}{}{}{}",
                    if_line,
                    block1,
                    block2,
                    self.fmt_line(
                        indent,
                        StatementState::Inactive,
                        format!("{} {}", keyword("end if"), self.format_comment(comm3))
                    )
                )
            }
            Statement::Return(ret) => self.fmt_line(
                indent,
                statement_state,
                format!(
                    "{} {}{comment}",
                    keyword("return"),
                    if ret.is_some() {
                        self.format_expr(ret.as_ref().unwrap(), dec)
                    } else {
                        String::from("")
                    }
                ),
            ),
            Statement::Expr(expr) => self.fmt_line(
                indent,
                statement_state,
                format!("{}{comment}", self.format_expr(expr, dec)),
            ),
        }
    }

    fn format_line(&self, line: &Line, indent: usize, dec: &D) -> String {
        if let Some(stmt) = &line.statement {
            self.format_statement(stmt, indent, self.format_comment(&line.comment), dec)
        } else {
            self.fmt_line(
                indent,
                StatementState::Inactive,
                self.format_comment(&line.comment),
            )
        }
    }

    fn format_block(&self, block: &[Line], indent: usize, dec: &D) -> String {
        format!(
            "{}",
            block
                .iter()
                .map(|l| self.format_line(l, indent, dec))
                .format("")
        )
    }

    fn format_fn(&self, function: &Func, indent: usize, dec: &D) -> String {
        let keyword = |k: &'static str| self.apply_decoration(&DecorationType::Keyword, k.into());
        let (cf, cl) = &function.comments;
        let ret = if function.ret == Type::Void {
            "".into()
        } else {
            format!(
                " {} {}",
                self.apply_decoration(&DecorationType::Arrow, self.fmt_fnret()),
                self.apply_decoration(&DecorationType::Type, self.type_to_str(&function.ret))
            )
        };
        format!(
            "{}{}{}",
            self.fmt_line(
                indent,
                StatementState::Inactive,
                format!(
                    "{} {}({}){} {}",
                    keyword("function"),
                    function.ident,
                    function
                        .args
                        .iter()
                        .map(|x| self.format_decl(x, dec))
                        .format(", "),
                    ret,
                    self.format_comment(cf)
                )
            ),
            self.format_block(&function.body, indent + 1, dec),
            self.fmt_line(
                indent,
                StatementState::Inactive,
                format!("{} {}", keyword("end function"), self.format_comment(cl)),
            )
        )
    }

    fn format_item(&self, item: &Item, indent: usize, add_newline: bool, dec: &D) -> String {
        format!(
            "{}{}",
            if add_newline {
                self.fmt_nl(dec)
            } else {
                String::from("")
            },
            match item {
                Item::Func(function) => {
                    self.format_fn(function, indent, dec)
                }
                Item::Line(line) => {
                    self.format_line(line, indent, dec)
                }
            }
        )
    }

    fn format_program(&self, items: &[Item], indent: usize, dec: &D) -> String {
        let mut last_was_line = false;
        items
            .iter()
            .enumerate()
            .map(|(cnt, item)| {
                let needs_newline = cnt > 0
                    && match item {
                        Item::Line(_) => !last_was_line,
                        Item::Func(_) => true,
                    };
                last_was_line = matches!(item, &Item::Line(_));
                self.format_item(item, indent, needs_newline, dec)
            })
            .join("")
    }
}

pub struct ASCIIFormatter {}

impl ASCIIFormatter {
    pub fn new() -> ASCIIFormatter {
        ASCIIFormatter {}
    }
}

impl Default for ASCIIFormatter {
    fn default() -> Self {
        Self::new()
    }
}

impl<D: AstDecorator> AstFormatter<D> for ASCIIFormatter {}
