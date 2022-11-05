use std::collections::HashMap;

use ast::{Expr, Ident, Item, Line, Statement};

use by_address::ByAddress;
use eval::execute_fn;
use grammar::{BlockParser, ProgramParser};

use lalrpop_util::{lexer::Token, ParseError};
use pretty_print::{
    AstFormatter, ExecutionDecorator, HtmlFormatter, TerminalFormatter, VarMappingDecorator,
};
use typecheck::typecheck_and_varmap;

use wasm_bindgen::prelude::*;

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(#[allow(unknown_lints,clippy,clippy::all,warnings)] pub grammar); // synthesized by LALRPOP

pub mod ast;
pub mod eval;
pub mod pretty_print;
pub mod typecheck;

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),
    None,
}

#[derive(Clone, Debug)]
pub struct VariableMapping<'a> {
    mapping: HashMap<ByAddress<&'a Ident>, &'a Ident>,
}

impl<'a> VariableMapping<'a> {
    pub fn new() -> VariableMapping<'a> {
        VariableMapping {
            mapping: HashMap::new(),
        }
    }

    /// Returns the definition that the given variable reference refers to, if known, or the
    /// original reference otherwise.
    pub fn get_var(&self, var_ref: &'a Ident) -> &'a Ident {
        self.mapping
            .get(&ByAddress(var_ref))
            .cloned()
            .unwrap_or(var_ref)
    }
}

impl<'a> Default for VariableMapping<'a> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub enum ExecutionEvent<'a> {
    ExprEval(&'a Expr),
    ExprReplaced(&'a Expr, Value),
    Assign(&'a Ident, Value),
    Return(Option<Value>),
    StatementActive(&'a Statement),
}

fn trace_exec<'a, T: AstFormatter<ExecutionDecorator<'a>>>(
    formatter: T,
    program: &'a [Item],
    events: Vec<ExecutionEvent<'a>>,
    mapping: VariableMapping<'a>,
    order: Vec<&'a Ident>,
) -> Vec<String> {
    let mut decorator = ExecutionDecorator::new(mapping, &order);
    events
        .into_iter()
        .map(|event| {
            decorator.process_event(&event);
            formatter.format_program(program, /*indent=*/ 0, &decorator)
        })
        .collect()
}

trait CodeParser {
    type OutputElement;
    fn parse<'input>(
        &self,
        input: &'input str,
    ) -> Result<Vec<Self::OutputElement>, ParseError<usize, Token<'input>, &'static str>>;
}

impl CodeParser for ProgramParser {
    type OutputElement = Item;
    fn parse<'input>(
        &self,
        input: &'input str,
    ) -> Result<Vec<Self::OutputElement>, ParseError<usize, Token<'input>, &'static str>> {
        self.parse(input)
    }
}

impl CodeParser for BlockParser {
    type OutputElement = Line;
    fn parse<'input>(
        &self,
        input: &'input str,
    ) -> Result<Vec<Self::OutputElement>, ParseError<usize, Token<'input>, &'static str>> {
        self.parse(input)
    }
}

// Needs semicolons.
fn parse<T: CodeParser>(code: String, parser: T) -> Vec<T::OutputElement> {
    let src = code.replace('\n', ";\n"); // Add semicolons, as lalrpop ignores whitespace by default.

    let elements = parser.parse(&src);
    match elements {
        Ok(e) => e,
        Err(e) => {
            match e {
                ParseError::InvalidToken { location: loc } => {
                    println!(
                        "{}\x1b[31;1m{}\x1b[;m{}",
                        &src[..loc],
                        &src[loc..loc + 1],
                        &src[loc + 1..]
                    );
                }
                _ => print!("{src}"),
            };
            panic!("{:?}", e);
        }
    }
}

fn parse_block(code: String) -> Vec<Line> {
    parse(code, BlockParser::new())
}

fn parse_program(code: String) -> Vec<Item> {
    parse(code, ProgramParser::new())
}

pub fn format_snippet(
    code: String,
    base_code: String,
    fn_name: String,
    id: usize,
    indent: usize,
    emit_html: bool,
) -> String {
    let block = parse_block(code);
    let program = parse_program(base_code);
    let mut replacement = vec![None; id];
    replacement.push(Some(&block));
    let (mapping, order) = typecheck_and_varmap(&program, replacement).unwrap();
    let dec = VarMappingDecorator::new(mapping, &order);
    program
        .iter()
        .find_map(|item| {
            if let Item::Func(func) = item {
                if func.ident != fn_name {
                    return None;
                }
                Some(if emit_html {
                    HtmlFormatter::new().format_block(&block, indent, &dec)
                } else {
                    TerminalFormatter::new().format_block(&block, indent, &dec)
                })
            } else {
                None
            }
        })
        .expect("Function not found")
}

pub fn format_code(code: String, indent: usize, emit_html: bool) -> String {
    let program = parse_program(code);
    let (mapping, order) = typecheck_and_varmap(&program, vec![]).unwrap();
    let dec = VarMappingDecorator::new(mapping, &order);
    if emit_html {
        HtmlFormatter::new().format_program(&program, indent, &dec)
    } else {
        TerminalFormatter::new().format_program(&program, indent, &dec)
    }
}

pub fn eval_on_args(
    code: String,
    fn_name: String,
    args: Vec<Value>,
    emit_html: bool,
) -> Vec<String> {
    let program = parse_program(code);
    let (mapping, order) = typecheck_and_varmap(&program, vec![]).unwrap();
    let events = program
        .iter()
        .find_map(|item| {
            if let Item::Func(func) = item {
                if func.ident != fn_name {
                    return None;
                }
                // TODO: handle other fns.
                if func.ident != fn_name {
                    return None;
                }
                let execution_events = execute_fn(func, &mapping, args.clone()).unwrap();
                Some(execution_events)
            } else {
                None
            }
        })
        .unwrap();

    if emit_html {
        trace_exec(HtmlFormatter::new(), &program, events, mapping, order)
    } else {
        trace_exec(TerminalFormatter::new(), &program, events, mapping, order)
    }
}

#[wasm_bindgen]
pub fn eval_from_js(code: String, fn_name: String, args: Vec<i64>) -> js_sys::Array {
    eval_on_args(
        code,
        fn_name,
        args.into_iter().map(Value::Integer).collect(),
        /*emit_html*/ true,
    )
    .into_iter()
    .map(JsValue::from)
    .collect()
}
