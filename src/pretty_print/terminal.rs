use crate::ast::Op;
use crate::pretty_print::{AstDecorator, AstFormatter, DecorationType, StatementState};
use itertools::Itertools;

pub struct TerminalFormatter {}

impl TerminalFormatter {
    pub fn new() -> TerminalFormatter {
        TerminalFormatter {}
    }
}

impl Default for TerminalFormatter {
    fn default() -> Self {
        Self::new()
    }
}

const VAR_COLORS: &[&str] = &[
    "\x1b[34m", "\x1b[31m", "\x1b[32m", "\x1b[33m", "\x1b[35m", "\x1b[36m", "\x1b[37m",
];
const BRIGHTBLUE: &str = "\x1b[94m";
const BRIGHTGREEN: &str = "\x1b[92m";
const RED: &str = "\x1b[31m";
const BOLD: &str = "\x1b[1m";
const THIN: &str = "\x1b[2m";
const NORMAL_WEIGHT: &str = "\x1b[22m";
const CURSIVE: &str = "\x1b[3m";
const NO_CURSIVE: &str = "\x1b[23m";
const UNDERLINE: &str = "\x1b[4m";
const NO_UNDERLINE: &str = "\x1b[24m";
const CLEAR_COLOR: &str = "\x1b[39m";
//const CLEAR: & str = "\x1b[0m";

impl<D: AstDecorator> AstFormatter<D> for TerminalFormatter {
    fn apply_decoration(&self, dec: &DecorationType, data: String) -> String {
        match dec {
            DecorationType::InactiveVariable(x) => {
                format!(
                    "{}{BOLD}{data}{CLEAR_COLOR}{NORMAL_WEIGHT}",
                    VAR_COLORS[x % VAR_COLORS.len()]
                )
            }
            DecorationType::ActiveVariable(x) => {
                format!(
                    "{UNDERLINE}{}{NO_UNDERLINE}",
                    <TerminalFormatter as AstFormatter<D>>::apply_decoration(
                        self,
                        &DecorationType::InactiveVariable(*x),
                        data
                    )
                )
            }
            DecorationType::ActiveExpr => format!("{UNDERLINE}{data}{NO_UNDERLINE}"),
            DecorationType::Keyword => format!("{BOLD}{data}{NORMAL_WEIGHT}"),
            DecorationType::Type => format!("{CURSIVE}{data}{NO_CURSIVE}"),
            DecorationType::Value => format!("{BRIGHTBLUE}{data}{CLEAR_COLOR}"),
            DecorationType::Operator => data,
            DecorationType::Comment => format!("{BRIGHTGREEN}{data}{CLEAR_COLOR}"),
            DecorationType::Arrow => data,
            DecorationType::Alert => format!("{RED}{data}{CLEAR_COLOR}"),
            DecorationType::None => data,
        }
    }

    fn fmt_line(&self, indent: usize, statement_state: StatementState, line: String) -> String {
        format!(
            "{}{THIN}{}{NORMAL_WEIGHT}{line}\n",
            if statement_state == StatementState::Active {
                ">"
            } else {
                " "
            },
            (0..indent).map(|_| "│ ").format("")
        )
    }

    fn fmt_decl(&self, ident: String, ty: String, value: Option<String>) -> String {
        if let Some(value) = value {
            format!("{ident}: {ty} ▸ {value}")
        } else {
            format!("{ident}: {ty}")
        }
    }

    fn fmt_assign(&self) -> String {
        "←".into()
    }

    fn fmt_fnret(&self) -> String {
        "→".into()
    }

    fn op_sym(&self, op: &Op) -> String {
        match op {
            Op::Sum => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Div => "/",
            Op::Mod => "MOD",
            Op::Le => "≤",
            Op::Lt => "<",
            Op::Ge => "≥",
            Op::Gt => ">",
            Op::Eq => "==",
            Op::Ne => "≠",
            Op::And => "and",
            Op::Or => "or",
        }
        .into()
    }
}
