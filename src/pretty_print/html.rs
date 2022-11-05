use std::cell::RefCell;

use itertools::Itertools;

use crate::ast::Op;
use crate::pretty_print::{AstDecorator, AstFormatter, DecorationType, StatementState};

pub struct HtmlFormatter {
    current_indent: RefCell<usize>,
}

impl HtmlFormatter {
    pub fn new() -> HtmlFormatter {
        HtmlFormatter {
            current_indent: RefCell::new(0),
        }
    }

    fn syntax_span(&self, class: &str, data: String) -> String {
        format!(r#"<span class="srs-syntax {class}">{data}</span>"#)
    }
}

impl Default for HtmlFormatter {
    fn default() -> Self {
        Self::new()
    }
}

impl<D: AstDecorator> AstFormatter<D> for HtmlFormatter {
    fn apply_decoration(&self, dec: &DecorationType, data: String) -> String {
        match dec {
            DecorationType::InactiveVariable(i) => {
                format!(r#"<span class="srs-syntax var" data-var={i}>{data}</span>"#)
            }
            DecorationType::ActiveVariable(i) => {
                format!(r#"<span class="srs-syntax var active-var" data-var={i}>{data}</span>"#)
            }
            DecorationType::ActiveExpr => self.syntax_span("active-expr", data),
            DecorationType::Keyword => self.syntax_span("keyword", data),
            DecorationType::Type => self.syntax_span("type", data),
            DecorationType::Value => self.syntax_span("value", data),
            DecorationType::Operator => self.syntax_span("operator", data),
            DecorationType::Comment => self.syntax_span("comment", data),
            DecorationType::Arrow => self.syntax_span("arrow", data),
            DecorationType::Alert => self.syntax_span("alert", data),
            DecorationType::None => data,
        }
    }

    fn fmt_nl(&self, _dec: &D) -> String {
        //String::from("<br>")
        "".into() // As of now, the parser doesn't skip newlines, so this isn't necessary
    }

    fn fmt_line(&self, indent: usize, statement_state: StatementState, line: String) -> String {
        let mut last_indent = self.current_indent.borrow_mut();
        let mut pieces = vec![];
        while *last_indent > indent {
            pieces.push(r#"</span>"#);
            *last_indent -= 1;
        }
        while *last_indent < indent {
            pieces.push(r#"<span class="srs-syntax block">"#);
            *last_indent += 1;
        }
        format!(
            r#"{}<span class="srs-syntax line {}">{line}</span>"#,
            pieces.iter().format(""),
            if statement_state == StatementState::Active {
                "active-stmt"
            } else {
                ""
            }
        )
    }

    fn fmt_decl(&self, ident: String, ty: String, value: Option<String>) -> String {
        if let Some(value) = value {
            format!("{ident}: {ty} &rsaquo; {value}")
        } else {
            format!("{ident}: {ty}")
        }
    }

    fn fmt_assign(&self) -> String {
        "&larr;".into()
    }

    fn fmt_fnret(&self) -> String {
        "&rarr;".into()
    }

    fn fmt_dots(&self) -> String {
        "&hellip;".into()
    }

    fn op_sym(&self, op: &Op) -> String {
        match op {
            Op::Sum => "&plus;",
            Op::Sub => "&minus;",
            Op::Mul => "&times;",
            Op::Div => "&frasl;",
            Op::Mod => "mod",
            Op::Le => "&le;",
            Op::Lt => "&lt;",
            Op::Ge => "&ge;",
            Op::Gt => "&gt;",
            Op::Eq => "&equals;",
            Op::Ne => "&ne;",
            Op::And => "and ",
            Op::Or => "or",
        }
        .into()
    }
}
