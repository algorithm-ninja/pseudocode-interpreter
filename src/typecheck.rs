use std::collections::HashMap;

use crate::ast::{Decl, Expr, Func, Ident, Item, Line, Statement, Tuple};

use crate::VariableMapping;

use by_address::ByAddress;

// TODO
#[derive(Debug)]
pub struct TypeCheckErrors {
    error: String,
}

#[derive(Debug)]
struct TypeCheckState<'a> {
    decl_stack: Vec<HashMap<String, &'a Ident>>,
    var_order: Vec<&'a Ident>,
    var_mapping: VariableMapping<'a>,
    blank_count: usize,
    blank_replacements: Vec<Option<&'a Vec<Line>>>,
}

fn typecheck_expr<'a, 'b>(
    expr: &'a Expr,
    state: &mut TypeCheckState<'b>,
) -> Result<(), TypeCheckErrors>
where
    'a: 'b,
{
    match expr {
        Expr::Ref(id) => {
            let var = state
                .decl_stack
                .iter()
                .rev()
                .find_map(|mapping| mapping.get(id))
                .cloned();
            if let Some(var) = var {
                state.var_mapping.mapping.insert(ByAddress(id), var);
            } else {
                return Err(TypeCheckErrors {
                    error: format!("Unknown variable {id}"),
                });
            }
        }
        Expr::Integer(_) => {}
        Expr::Float(_) => {}
        Expr::String(_) => {}
        Expr::Array(ve) => {
            for e in ve.iter() {
                typecheck_expr(e, state)?
            }
        }
        Expr::Tuple(tuple) => {
            match tuple {
                Tuple::ExprTuple(ve) => {
                    for e in ve.iter() {
                        typecheck_expr(e, state)?
                    }
                },
                Tuple::DeclTuple(ve) => {
                    for d in ve.iter() {
                        typecheck_decl(d, state)?
                    }
                },
            }
        }
        Expr::Parens(be) => {
            typecheck_expr(&*be, state)?;
        }
        Expr::Op(e1, _, e2) => {
            typecheck_expr(&*e1, state)?;
            typecheck_expr(&*e2, state)?;
        }
        Expr::Not(be) => {
            typecheck_expr(&*be, state)?;
        }
        Expr::ArrayIndex(e1, e2) => {
            typecheck_expr(&*e1, state)?;
            typecheck_expr(&*e2, state)?;
        }
        Expr::FunctionCall(_, args) => {
            for e in args.iter() {
                typecheck_expr(e, state)?
            }
        }
    };
    Ok(())
}

fn typecheck_decl<'a, 'b>(
    decl: &'a Decl,
    state: &mut TypeCheckState<'b>,
) -> Result<(), TypeCheckErrors>
where
    'a: 'b,
{
    state.var_order.push(&decl.ident);
    state
        .decl_stack
        .last_mut()
        .expect("Decl stack cannot be empty")
        .insert(decl.ident.clone(), &decl.ident);
    if let Some(expr) = &decl.val {
        typecheck_expr(&expr, state)?
    }
    Ok(())
}

fn typecheck_statement<'a, 'b>(
    statement: &'a Statement,
    state: &mut TypeCheckState<'b>,
) -> Result<(), TypeCheckErrors>
where
    'a: 'b,
{
    match statement {
        Statement::SilentBlank | Statement::Blank => {
            let replacement = state
                .blank_replacements
                .get(state.blank_count)
                .cloned()
                .flatten();

            state.blank_count += 1;

            if let Some(replacement) = replacement {
                typecheck_block(replacement, state)?;
            }
        }
        Statement::Decl(d) => typecheck_decl(d, state)?,
        Statement::Assign(e1, e2) => {
            typecheck_expr(e1, state)?;
            typecheck_expr(e2, state)?
        }
        Statement::If(e, b1, b2, _, _, _) => {
            typecheck_expr(e, state)?;
            typecheck_block(b1, state)?;
            typecheck_block(b2, state)?;
        }
        Statement::While(e, b, _, _) => {
            typecheck_expr(e, state)?;
            typecheck_block(b, state)?;
        }
        Statement::For(d, l, r, _, b, _, _) => {
            typecheck_decl(d, state)?;
            typecheck_expr(l, state)?;
            typecheck_expr(r, state)?;
            typecheck_block(b, state)?;
        }
        Statement::Return(r) => {
            if r.is_some() {
                typecheck_expr(r.as_ref().unwrap(), state)?
            }
        }
        Statement::Expr(e) => typecheck_expr(e, state)?,
    };
    Ok(())
}

fn typecheck_line<'a, 'b>(
    line: &'a Line,
    state: &mut TypeCheckState<'b>,
) -> Result<(), TypeCheckErrors>
where
    'a: 'b,
{
    if let Some(statement) = &line.statement {
        typecheck_statement(statement, state)?
    }
    Ok(())
}

fn typecheck_block<'a, 'b>(
    block: &'a Vec<Line>,
    state: &mut TypeCheckState<'b>,
) -> Result<(), TypeCheckErrors>
where
    'a: 'b,
{
    state.decl_stack.push(HashMap::new());
    for line in block {
        typecheck_line(line, state)?
    }
    state.decl_stack.pop();
    Ok(())
}

fn typecheck_function<'a, 'b>(
    function: &'a Func,
    state: &mut TypeCheckState<'b>,
) -> Result<(), TypeCheckErrors>
where
    'a: 'b,
{
    state.decl_stack.push(HashMap::new());
    for decl in function.args.iter() {
        typecheck_decl(decl, state)?;
    }
    for line in function.body.iter() {
        typecheck_line(line, state)?
    }
    state.decl_stack.pop();
    Ok(())
}

fn typecheck_item<'a, 'b>(
    item: &'a Item,
    state: &mut TypeCheckState<'b>,
) -> Result<(), TypeCheckErrors>
where
    'a: 'b,
{
    match item {
        Item::Line(line) => typecheck_line(line, state)?,
        Item::Func(func) => typecheck_function(func, state)?,
    }
    Ok(())
}

pub fn typecheck_and_varmap<'a>(
    items: &'a Vec<Item>,
    blank_replacements: Vec<Option<&'a Vec<Line>>>,
) -> Result<(VariableMapping<'a>, Vec<&'a Ident>), TypeCheckErrors> {
    // TODO: actually typecheck.
    let mut state = TypeCheckState {
        decl_stack: vec![],
        var_order: vec![],
        var_mapping: VariableMapping::new(),
        blank_count: 0,
        blank_replacements,
    };

    state.decl_stack.push(HashMap::new());
    for item in items {
        typecheck_item(item, &mut state)?;
    }
    state.decl_stack.pop();

    Ok((state.var_mapping, state.var_order))
}
