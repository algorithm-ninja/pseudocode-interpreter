use std::collections::HashMap;

use crate::ast::{Decl, Expr, Func, Ident, Line, Op, Statement};

use crate::{ExecutionEvent, Value, VariableMapping};

use by_address::ByAddress;

// TODO
#[allow(dead_code)]
#[derive(Debug)]
pub struct ExecutionErrors {
    error: String,
}

#[derive(Debug)]
struct ExecutionState<'a> {
    var_mapping: VariableMapping<'a>,
    var_value: HashMap<ByAddress<&'a Ident>, Value>,
    events: Vec<ExecutionEvent<'a>>,
}

impl<'a> ExecutionState<'a> {
    fn add_expr_eval<'b>(&mut self, e: &'b Expr, v: Value)
    where
        'b: 'a,
    {
        self.events.push(ExecutionEvent::ExprEval(e));
        self.events.push(ExecutionEvent::ExprReplaced(e, v));
    }
    fn assign<'b>(&mut self, i: &'b Ident, v: Value) -> Result<(), ExecutionErrors>
    where
        'b: 'a,
    {
        // TODO: typecheck?
        self.events.push(ExecutionEvent::Assign(i, v.clone()));
        self.var_value.insert(ByAddress(i), v);
        Ok(())
    }
}

fn apply_op(v1: Value, op: &Op, v2: Value) -> Result<Value, ExecutionErrors> {
    Ok(match (v1, op, v2) {
        (Value::Integer(a), Op::Sum, Value::Integer(b)) => Value::Integer(a + b),
        (Value::Integer(a), Op::Sub, Value::Integer(b)) => Value::Integer(a - b),
        (Value::Integer(a), Op::Mul, Value::Integer(b)) => Value::Integer(a * b),
        (Value::Integer(a), Op::Lt, Value::Integer(b)) => Value::Bool(a < b),
        (Value::Integer(a), Op::Ge, Value::Integer(b)) => Value::Bool(a >= b),
        (Value::Integer(a), Op::Gt, Value::Integer(b)) => Value::Bool(a > b),
        _ => unimplemented!(),
    })
}

fn execute_expr<'a, 'b>(
    expr: &'a Expr,
    state: &mut ExecutionState<'b>,
) -> Result<Value, ExecutionErrors>
where
    'a: 'b,
{
    match expr {
        Expr::Ref(id) => {
            let var_id = state.var_mapping.get_var(id);
            let value = state.var_value.get(&ByAddress(var_id)).unwrap().clone();
            if matches!(value, Value::None) {
                return Err(ExecutionErrors {
                    error: format!("referenced uninitialized variable {id}"),
                });
            }
            state.add_expr_eval(expr, value.clone());
            Ok(value)
        }
        Expr::Integer(x) => Ok(Value::Integer(*x)),
        Expr::Float(x) => Ok(Value::Float(*x)),
        Expr::String(_) => {
            unimplemented!()
        }
        Expr::Array(_ve) => {
            unimplemented!()
        }
        Expr::Tuple(_tuple) => {
            unimplemented!()
        }
        Expr::Parens(be) => execute_expr(be, state),
        Expr::Op(e1, op, e2) => {
            let v1 = execute_expr(e1, state)?;
            let v2 = execute_expr(e2, state)?;
            let val = apply_op(v1, op, v2)?;
            state.add_expr_eval(expr, val.clone());
            Ok(val)
        }
        Expr::Not(_) => {
            unimplemented!()
        }
        Expr::ArrayIndex(_, _) => {
            unimplemented!()
        }
        Expr::FunctionCall(_, _) => {
            unimplemented!()
        }
    }
}

fn execute_decl<'a, 'b>(
    decl: &'a Decl,
    state: &mut ExecutionState<'b>,
) -> Result<(), ExecutionErrors>
where
    'a: 'b,
{
    state.assign(&decl.ident, Value::None)?;
    Ok(())
}

fn execute_statement<'a, 'b>(
    statement: &'a Statement,
    state: &mut ExecutionState<'b>,
) -> Result<(), ExecutionErrors>
where
    'a: 'b,
{
    state
        .events
        .push(ExecutionEvent::StatementActive(statement));
    match statement {
        Statement::SilentBlank | Statement::Blank => (),
        Statement::Decl(d) => execute_decl(d, state)?,
        Statement::Assign(e1, e2) => {
            let val = execute_expr(e2, state)?;
            if let Expr::Ref(id) = e1 {
                let var = state.var_mapping.get_var(id);
                state.assign(var, val)?;
            }
        }
        Statement::If(e, b1, b2, _, _, _) => {
            let cond_val = execute_expr(e, state)?;
            let cond_val = if let Value::Bool(val) = cond_val {
                val
            } else {
                // Should be typechecked to never happen.
                panic!("Typechecking inconsistent");
            };
            if cond_val {
                execute_block(b1, state)?;
            } else {
                execute_block(b2, state)?;
            }
        }
        Statement::While(e, b, _, _) => {
            loop {
                let cond_val = execute_expr(e, state)?;
                let cond_val = if let Value::Bool(val) = cond_val {
                    val
                } else {
                    // Should be typechecked to never happen.
                    panic!("Typechecking inconsistent");
                };
                if cond_val {
                    execute_block(b, state)?;
                } else {
                    break;
                }
                state
                    .events
                    .push(ExecutionEvent::StatementActive(statement));
            }
        }
        Statement::For(d, l, r, inc, b, _, _) => {
            execute_decl(d, state)?;
            let val_l = match execute_expr(l, state)? {
                Value::Integer(v) => v,
                _ => 0,
            };
            let val_r = match execute_expr(r, state)? {
                Value::Integer(v) => v - (if *inc { 0 } else { 1 }),
                _ => 0,
            };
            for i in val_l..val_r {
                state.assign(&d.ident, Value::Integer(i))?;
                execute_block(b, state)?;
            }
        }
        Statement::Return(r) => {
            if r.is_some() {
                let val = execute_expr(r.as_ref().unwrap(), state)?;
                // TODO: this might need more work for function calls.
                state.events.push(ExecutionEvent::Return(Some(val)));
            } else {
                state.events.push(ExecutionEvent::Return(None));
            }
        }
        Statement::Expr(e) => {
            execute_expr(e, state)?;
        }
    };
    Ok(())
}

fn execute_line<'a, 'b>(
    line: &'a Line,
    state: &mut ExecutionState<'b>,
) -> Result<(), ExecutionErrors>
where
    'a: 'b,
{
    if line.statement.is_some() {
        execute_statement(line.statement.as_ref().unwrap(), state)?
    }
    Ok(())
}

fn execute_block<'a, 'b>(
    block: &'a Vec<Line>,
    state: &mut ExecutionState<'b>,
) -> Result<(), ExecutionErrors>
where
    'a: 'b,
{
    for line in block {
        execute_line(line, state)?
    }
    Ok(())
}

pub fn execute_fn<'a, 'b>(
    function: &'a Func,
    var_mapping: &'b VariableMapping<'a>,
    args: Vec<Value>,
) -> Result<Vec<ExecutionEvent<'a>>, ExecutionErrors> {
    let mut state = ExecutionState {
        var_mapping: var_mapping.clone(),
        var_value: HashMap::new(),
        events: vec![],
    };

    if args.len() != function.args.len() {
        return Err(ExecutionErrors {
            error: "Invalid number of arguments.".into(),
        });
    }

    for (arg, val) in function.args.iter().zip(args.into_iter()) {
        state.assign(&arg.ident, val)?;
    }

    execute_block(&function.body, &mut state)?;

    Ok(state.events)
}
