use std::{collections::HashMap, fmt::Debug, rc::Rc, sync::Arc};

use im::{OrdSet, Vector};

use crate::{
    error::Error,
    eval::{CompiledProgram, Instruction, Ip},
    value::{LValue, RValue},
};

use super::ast::*;

pub type Result<T, A> = std::result::Result<T, Error<A>>;

fn expect_type<A: Ast, NT: Debug + AstNode<NT> + Clone>(
    types: &[&Type<A>],
    node: &Node<A, NT>,
    node_type: &Type<A>,
) -> Result<(), A> {
    for t in types {
        if t.is_same(node_type)? {
            return Ok(());
        }
    }
    Err(Error::TypeError(
        node.id,
        node.info.clone(),
        node_type.clone(),
        types.iter().cloned().cloned().collect(),
    ))
}

#[derive(Debug)]
enum LValueStorageLocation {
    Local(usize),
    Global(usize),
}

#[derive(Debug)]
struct ProgramCompilationState<'a, A: Ast> {
    fn_entry_point: HashMap<FnIndex, Ip>,
    vardecl_to_location: HashMap<VarIndex, LValueStorageLocation>,
    ini_entry_point: Ip,
    instructions: Vec<Instruction<'a, A>>,
    program: &'a Program<A>,
}

macro_rules! pop_variant {
    ($p:pat_param, $state:expr) => {
        let $p = $state.lvalues.pop().unwrap() else { unreachable!() };
    };
}

impl<'a, A: Ast> ProgramCompilationState<'a, A> {
    fn add_placeholder(&mut self) -> Ip {
        let s = self.instructions.len();
        self.instructions.push(Instruction::placeholder());
        s
    }

    fn compile_expr(
        &mut self,
        expr: &'a Node<A, Expr<A>>,
        local_lvalues: usize,
        mut current: Ip,
    ) -> Result<(Ip, Type<A>), A> {
        let mut next = self.add_placeholder();
        let ty = match expr.unwrap() {
            Expr::Ref(var) => {
                match *self.vardecl_to_location.get(var).unwrap() {
                    LValueStorageLocation::Local(l) => {
                        self.instructions[current].set(move |state| {
                            state.lvalues.push(
                                state.lvalues[state.lvalues.len() - local_lvalues + l].clone(),
                            );
                            Ok(Some(next))
                        });
                    }
                    LValueStorageLocation::Global(l) => {
                        self.instructions[current].set(move |state| {
                            state.lvalues.push(state.lvalues[l].clone());
                            Ok(Some(next))
                        });
                    }
                }
                self.program.var(*var).ty.get_contents().unwrap().clone()
            }

            Expr::Integer(i) => {
                self.instructions[current].set(move |state| {
                    state.lvalues.push(LValue::Integer(*i));
                    Ok(Some(next))
                });
                Type::Integer
            }

            Expr::Float(i) => {
                self.instructions[current].set(move |state| {
                    state.lvalues.push(LValue::Float(*i));
                    Ok(Some(next))
                });
                Type::Float
            }

            Expr::Bool(i) => {
                self.instructions[current].set(move |state| {
                    state.lvalues.push(LValue::Bool(*i));
                    Ok(Some(next))
                });
                Type::Bool
            }
            Expr::String(i) => {
                self.instructions[current].set(move |state| {
                    state.lvalues.push(LValue::String(Arc::new(i.clone())));
                    Ok(Some(next))
                });
                Type::String
            }

            Expr::Range(e1, e2, range_ty) => {
                let (n, t) = self.compile_expr(e1, local_lvalues, current)?;
                expect_type(&[&Type::Integer], e1, &t)?;
                let (n, t) = self.compile_expr(e2, local_lvalues + 1, n)?;
                expect_type(&[&Type::Integer], e2, &t)?;

                if *range_ty == RangeType::HalfOpen {
                    self.instructions[n].set(move |state| {
                        pop_variant!(LValue::Integer(e), state);
                        pop_variant!(LValue::Integer(b), state);
                        state
                            .lvalues
                            .push(LValue::Array((b..e).map(LValue::Integer).collect()));
                        Ok(Some(next))
                    });
                } else {
                    self.instructions[n].set(move |state| {
                        pop_variant!(LValue::Integer(e), state);
                        pop_variant!(LValue::Integer(b), state);
                        state
                            .lvalues
                            .push(LValue::Array((b..=e).map(LValue::Integer).collect()));
                        Ok(Some(next))
                    });
                }
                Type::Array(Box::new(Node::new_with_defaults(Type::Integer)))
            }

            Expr::BinaryOp(e1, op, e2) => {
                let (n, t1) = self.compile_expr(e1, local_lvalues, current)?;
                let (n, t2) = self.compile_expr(e2, local_lvalues + 1, n)?;
                expect_type(&[&t1], e2, &t2)?;
                let ty = match op {
                    BinaryOp::Le
                    | BinaryOp::Ge
                    | BinaryOp::Gt
                    | BinaryOp::Lt
                    | BinaryOp::Eq
                    | BinaryOp::Ne => Type::Bool,
                    BinaryOp::Sum => {
                        expect_type(&[&Type::Integer, &Type::Float, &Type::String], e1, &t1)?;
                        t1.clone()
                    }
                    BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                        expect_type(&[&Type::Integer, &Type::Float], e1, &t1)?;
                        t1.clone()
                    }
                    BinaryOp::Mod => {
                        expect_type(&[&Type::Integer], e1, &t1)?;
                        t1.clone()
                    }
                    BinaryOp::And | BinaryOp::Or => {
                        expect_type(&[&Type::Bool], e1, &t1)?;
                        t1.clone()
                    }
                };
                // TODO(veluca): catch arithmetic errors.
                match (&t1, op, &t2) {
                    (Type::String, BinaryOp::Sum, Type::String) => {
                        self.instructions[n].set(move |state| {
                            pop_variant!(LValue::String(op2), state);
                            pop_variant!(LValue::String(op1), state);
                            state
                                .lvalues
                                .push(LValue::String(Arc::new((*op1).clone() + &*op2)));
                            Ok(Some(next))
                        });
                    }
                    (Type::Integer, BinaryOp::Sum, Type::Integer) => {
                        self.instructions[n].set(move |state| {
                            pop_variant!(LValue::Integer(op2), state);
                            pop_variant!(LValue::Integer(op1), state);
                            state.lvalues.push(LValue::Integer(op1 + op2));
                            Ok(Some(next))
                        });
                    }
                    (Type::Integer, BinaryOp::Sub, Type::Integer) => {
                        self.instructions[n].set(move |state| {
                            pop_variant!(LValue::Integer(op2), state);
                            pop_variant!(LValue::Integer(op1), state);
                            state.lvalues.push(LValue::Integer(op1 - op2));
                            Ok(Some(next))
                        });
                    }
                    (Type::Integer, BinaryOp::Mul, Type::Integer) => {
                        self.instructions[n].set(move |state| {
                            pop_variant!(LValue::Integer(op2), state);
                            pop_variant!(LValue::Integer(op1), state);
                            state.lvalues.push(LValue::Integer(op1 * op2));
                            Ok(Some(next))
                        });
                    }
                    (Type::Integer, BinaryOp::Div, Type::Integer) => {
                        self.instructions[n].set(move |state| {
                            pop_variant!(LValue::Integer(op2), state);
                            pop_variant!(LValue::Integer(op1), state);
                            state.lvalues.push(LValue::Integer(op1 / op2));
                            Ok(Some(next))
                        });
                    }
                    (Type::Integer, BinaryOp::Mod, Type::Integer) => {
                        self.instructions[n].set(move |state| {
                            pop_variant!(LValue::Integer(op2), state);
                            pop_variant!(LValue::Integer(op1), state);
                            state.lvalues.push(LValue::Integer(op1 % op2));
                            Ok(Some(next))
                        });
                    }
                    (Type::Float, BinaryOp::Sum, Type::Float) => {
                        self.instructions[n].set(move |state| {
                            pop_variant!(LValue::Float(op2), state);
                            pop_variant!(LValue::Float(op1), state);
                            state.lvalues.push(LValue::Float(op1 + op2));
                            Ok(Some(next))
                        });
                    }
                    (Type::Float, BinaryOp::Sub, Type::Float) => {
                        self.instructions[n].set(move |state| {
                            pop_variant!(LValue::Float(op2), state);
                            pop_variant!(LValue::Float(op1), state);
                            state.lvalues.push(LValue::Float(op1 - op2));
                            Ok(Some(next))
                        });
                    }
                    (Type::Float, BinaryOp::Mul, Type::Float) => {
                        self.instructions[n].set(move |state| {
                            pop_variant!(LValue::Float(op2), state);
                            pop_variant!(LValue::Float(op1), state);
                            state.lvalues.push(LValue::Float(op1 * op2));
                            Ok(Some(next))
                        });
                    }
                    (Type::Float, BinaryOp::Div, Type::Float) => {
                        self.instructions[n].set(move |state| {
                            pop_variant!(LValue::Float(op2), state);
                            pop_variant!(LValue::Float(op1), state);
                            state.lvalues.push(LValue::Float(op1 / op2));
                            Ok(Some(next))
                        });
                    }
                    (_, BinaryOp::Lt, _) => {
                        self.instructions[n].set(move |state| {
                            let op2 = state.lvalues.pop().unwrap();
                            let op1 = state.lvalues.pop().unwrap();
                            state.lvalues.push(LValue::Bool(op1 < op2));
                            Ok(Some(next))
                        });
                    }
                    (_, BinaryOp::Le, _) => {
                        self.instructions[n].set(move |state| {
                            let op2 = state.lvalues.pop().unwrap();
                            let op1 = state.lvalues.pop().unwrap();
                            state.lvalues.push(LValue::Bool(op1 <= op2));
                            Ok(Some(next))
                        });
                    }
                    (_, BinaryOp::Gt, _) => {
                        self.instructions[n].set(move |state| {
                            let op2 = state.lvalues.pop().unwrap();
                            let op1 = state.lvalues.pop().unwrap();
                            state.lvalues.push(LValue::Bool(op1 > op2));
                            Ok(Some(next))
                        });
                    }
                    (_, BinaryOp::Ge, _) => {
                        self.instructions[n].set(move |state| {
                            let op2 = state.lvalues.pop().unwrap();
                            let op1 = state.lvalues.pop().unwrap();
                            state.lvalues.push(LValue::Bool(op1 >= op2));
                            Ok(Some(next))
                        });
                    }
                    (_, BinaryOp::Eq, _) => {
                        self.instructions[n].set(move |state| {
                            let op2 = state.lvalues.pop().unwrap();
                            let op1 = state.lvalues.pop().unwrap();
                            state.lvalues.push(LValue::Bool(op1 == op2));
                            Ok(Some(next))
                        });
                    }
                    (_, BinaryOp::Ne, _) => {
                        self.instructions[n].set(move |state| {
                            let op2 = state.lvalues.pop().unwrap();
                            let op1 = state.lvalues.pop().unwrap();
                            state.lvalues.push(LValue::Bool(op1 != op2));
                            Ok(Some(next))
                        });
                    }
                    (_, BinaryOp::And, _) => {
                        self.instructions[n].set(move |state| {
                            pop_variant!(LValue::Bool(op2), state);
                            pop_variant!(LValue::Bool(op1), state);
                            state.lvalues.push(LValue::Bool(op1 && op2));
                            Ok(Some(next))
                        });
                    }
                    (_, BinaryOp::Or, _) => {
                        self.instructions[n].set(move |state| {
                            pop_variant!(LValue::Bool(op2), state);
                            pop_variant!(LValue::Bool(op1), state);
                            state.lvalues.push(LValue::Bool(op1 || op2));
                            Ok(Some(next))
                        });
                    }
                    _ => unreachable!("{:?} - {:?} {:?} {:?}", expr, t1, op, t2),
                }
                ty
            }

            // TODO(veluca): consider making Output a statement.
            Expr::Output(expr) => {
                let (n, _) = self.compile_expr(expr, local_lvalues, current)?;
                self.instructions[n].set(move |state| {
                    let arg = state.lvalues.pop().unwrap();
                    // TODO(veluca): prettier printing
                    state.stdout.push(format!("{:?}", arg));
                    state.lvalues.push(LValue::Void);
                    Ok(Some(next))
                });
                Type::Void
            }

            Expr::FunctionCall(f, args) => {
                let entry = *self.fn_entry_point.get(f).unwrap();
                let f = self.program.fun(*f);
                if args.len() != f.args.len() {
                    return Err(Error::WrongArgumentNumber(
                        expr.id,
                        expr.info.clone(),
                        f.ident.clone(),
                    ));
                }
                for (i, (ex, var)) in args.iter().zip(f.args.iter()).enumerate() {
                    let (n, t) = self.compile_expr(ex, local_lvalues + i, current)?;
                    expect_type(&[self.program.var(*var).ty.get_contents()?], ex, &t)?;
                    current = n;
                }
                self.instructions[current].set(move |state| {
                    // The stack is already set up properly, except for the return addr.
                    // Upon return, the callee will put the return value in the top position of the
                    // stack.
                    state.ip.push(next);
                    Ok(Some(entry))
                });
                if let Some(ret) = &f.ret {
                    ret.get_contents()?.clone()
                } else {
                    Type::Void
                }
            }

            Expr::Array(a) => {
                let mut ty = None;
                for (i, ex) in a.iter().enumerate() {
                    let (n, t) = self.compile_expr(ex, local_lvalues + i, current)?;
                    if let Some(ty) = &ty {
                        expect_type(&[ty], ex, &t)?;
                    }
                    ty = Some(t);
                    current = n;
                }
                if ty.is_none() {
                    todo!() // what do we do here?
                }

                let num = a.len();
                self.instructions[current].set(move |state| {
                    let mut lval = Vector::new();
                    for _ in 0..num {
                        lval.push_front(state.lvalues.pop().unwrap());
                    }
                    state.lvalues.push(LValue::Array(lval));
                    Ok(Some(next))
                });

                Type::Array(Box::new(Node::new_with_defaults(ty.unwrap())))
            }

            Expr::Set(s) => {
                let mut ty = None;
                for (i, ex) in s.iter().enumerate() {
                    let (n, t) = self.compile_expr(ex, local_lvalues + i, current)?;
                    if let Some(ty) = &ty {
                        expect_type(&[ty], ex, &t)?;
                    }
                    ty = Some(t);
                    current = n;
                }
                if ty.is_none() {
                    todo!() // what do we do here?
                }

                let num = s.len();
                self.instructions[current].set(move |state| {
                    let mut lval = OrdSet::new();
                    for _ in 0..num {
                        lval.insert(state.lvalues.pop().unwrap());
                    }
                    state.lvalues.push(LValue::Set(lval));
                    Ok(Some(next))
                });

                Type::Set(Box::new(Node::new_with_defaults(ty.unwrap())))
            }

            Expr::Map(s) => {
                let mut ty: Option<(Type<A>, Type<A>)> = None;
                for (i, ex) in s.iter().enumerate() {
                    let (n, t0) = self.compile_expr(&ex.0, local_lvalues + 2 * i, current)?;
                    let (n, t1) = self.compile_expr(&ex.1, local_lvalues + 2 * i + 1, n)?;
                    if let Some(ty) = &ty {
                        expect_type(&[&ty.0], &ex.0, &t0)?;
                        expect_type(&[&ty.1], &ex.1, &t1)?;
                    }
                    ty = Some((t0, t1));
                    current = n;
                }
                if ty.is_none() {
                    todo!() // what do we do here?
                }

                let num = s.len();
                self.instructions[current].set(move |state| {
                    let mut lval = im::HashMap::new();
                    for _ in 0..num {
                        let value = state.lvalues.pop().unwrap();
                        let key = state.lvalues.pop().unwrap();
                        lval.insert(key, value);
                    }
                    state.lvalues.push(LValue::Map(lval));
                    Ok(Some(next))
                });

                let ty = ty.unwrap();
                Type::Map(
                    Box::new(Node::new_with_defaults(ty.0)),
                    Box::new(Node::new_with_defaults(ty.1)),
                )
            }

            Expr::Tuple(a) => {
                let mut types = vec![];
                for (i, ex) in a.iter().enumerate() {
                    let (n, t) = self.compile_expr(ex, local_lvalues + i, current)?;
                    types.push(Node::new_with_defaults(t));
                    current = n;
                }

                let num = a.len();
                self.instructions[current].set(move |state| {
                    let mut lval = Vector::new();
                    for _ in 0..num {
                        lval.push_front(state.lvalues.pop().unwrap());
                    }
                    state.lvalues.push(LValue::Tuple(lval));
                    Ok(Some(next))
                });

                Type::Tuple(types)
            }

            Expr::NamedTuple(a) => {
                let mut types = vec![];
                for (i, ex) in a.iter().enumerate() {
                    let (n, t) = self.compile_expr(&ex.1, local_lvalues + i, current)?;
                    types.push((ex.0.clone(), Node::new_with_defaults(t)));
                    current = n;
                }

                let num = a.len();
                self.instructions[current].set(move |state| {
                    let mut lval = Vector::new();
                    for _ in 0..num {
                        lval.push_front(state.lvalues.pop().unwrap());
                    }
                    state.lvalues.push(LValue::Tuple(lval));
                    Ok(Some(next))
                });

                Type::NamedTuple(types)
            }

            Expr::Parens(e) => {
                let (n, t) = self.compile_expr(e, local_lvalues, current)?;
                // This generates an unused expression.
                next = n;
                t
            }

            Expr::Not(e) => {
                let (n, t) = self.compile_expr(e, local_lvalues, current)?;
                expect_type(&[&Type::Bool], e, &t)?;
                self.instructions[n].set(move |state| {
                    pop_variant!(LValue::Bool(b), state);
                    state.lvalues.push(LValue::Bool(!b));
                    Ok(Some(next))
                });
                Type::Bool
            }
            Expr::ArrayIndex(aexpr, iexpr) => {
                let (n, aty) = self.compile_expr(aexpr, local_lvalues, current)?;
                let (n, ity) = self.compile_expr(iexpr, local_lvalues + 1, n)?;
                expect_type(&[&Type::Integer], iexpr, &ity)?;
                self.instructions[n].set(move |state| {
                    pop_variant!(LValue::Integer(i), state);
                    pop_variant!(LValue::Array(lval), state);
                    if (i < 0) || (i as usize) > lval.len() {
                        return Err(Error::ArrayOutOfBounds(
                            expr.id,
                            expr.info.clone(),
                            i,
                            lval.len(),
                        ));
                    }
                    let i = i as usize;
                    if i < lval.len() {
                        state.lvalues.push(lval[i].clone());
                    }
                    Ok(Some(next))
                });
                if let Type::Array(inner) = aty {
                    inner.get_contents()?.clone()
                } else {
                    return Err(Error::ExpectedArray(aexpr.id, aexpr.info.clone()));
                }
            }
            Expr::TupleField(texpr, idx) => {
                let (n, tty) = self.compile_expr(texpr, local_lvalues, current)?;
                self.instructions[n].set(move |state| {
                    pop_variant!(LValue::Tuple(lval), state);
                    state.lvalues.push(lval[*idx].clone());
                    Ok(Some(next))
                });
                if let Type::Tuple(inner) = tty {
                    if *idx >= inner.len() {
                        return Err(Error::InvalidTupleField(expr.id, expr.info.clone(), *idx));
                    }
                    inner[*idx].get_contents()?.clone()
                } else {
                    return Err(Error::ExpectedTuple(expr.id, expr.info.clone()));
                }
            }
            Expr::NamedTupleField(texpr, field) => {
                let (n, tty) = self.compile_expr(texpr, local_lvalues, current)?;
                let (ty, idx) = if let Type::NamedTuple(inner) = tty {
                    if let Some((pos, x)) = inner
                        .iter()
                        .enumerate()
                        .find(|(_, x)| x.0.name == field.name)
                    {
                        (x.1.get_contents()?.clone(), pos)
                    } else {
                        return Err(Error::InvalidNamedTupleField(
                            expr.id,
                            expr.info.clone(),
                            field.name.clone(),
                        ));
                    }
                } else {
                    return Err(Error::ExpectedNamedTuple(expr.id, expr.info.clone()));
                };
                self.instructions[n].set(move |state| {
                    pop_variant!(LValue::NamedTuple(lval), state);
                    state.lvalues.push(lval[idx].clone());
                    Ok(Some(next))
                });
                ty
            }
            Expr::MethodCall(_, _, _) => todo!(),
        };
        Ok((next, ty.canonical_type()?))
    }

    fn compile_expr_rvalue(
        &mut self,
        expr: &'a Node<A, Expr<A>>,
        local_lvalues: usize,
        current: Ip,
    ) -> Result<(Ip, Type<A>), A> {
        let next = self.add_placeholder();
        let ty = match expr.get_contents()? {
            Expr::Ref(var) => {
                match *self.vardecl_to_location.get(var).unwrap() {
                    LValueStorageLocation::Local(l) => {
                        self.instructions[current].set(move |state| {
                            state.rvalues.push(RValue {
                                lstack_pos: state.lvalues.len() - local_lvalues + l,
                                indices: vec![],
                            });
                            Ok(Some(next))
                        });
                    }
                    LValueStorageLocation::Global(l) => {
                        self.instructions[current].set(move |state| {
                            state.rvalues.push(RValue {
                                lstack_pos: l,
                                indices: vec![],
                            });
                            Ok(Some(next))
                        });
                    }
                }
                self.program.var(*var).ty.get_contents().unwrap().clone()
            }
            Expr::ArrayIndex(aexpr, iexpr) => {
                let (n, aty) = self.compile_expr_rvalue(aexpr, local_lvalues, current)?;
                let (n, ity) = self.compile_expr(iexpr, local_lvalues, n)?;
                expect_type(&[&Type::Integer], iexpr, &ity)?;
                self.instructions[n].set(move |state| {
                    let mut rval = state.rvalues.pop().unwrap();
                    pop_variant!(LValue::Integer(i), state);
                    rval.indices.push(i);
                    state.rvalues.push(rval);
                    Ok(Some(next))
                });
                if let Type::Array(inner) = aty {
                    inner.get_contents()?.clone()
                } else {
                    return Err(Error::ExpectedArray(aexpr.id, aexpr.info.clone()));
                }
            }
            Expr::TupleField(texpr, idx) => {
                let (n, tty) = self.compile_expr_rvalue(texpr, local_lvalues, current)?;
                self.instructions[n].set(move |state| {
                    let mut rval = state.rvalues.pop().unwrap();
                    rval.indices.push(*idx as i64);
                    state.rvalues.push(rval);
                    Ok(Some(next))
                });
                if let Type::Tuple(inner) = tty {
                    if *idx >= inner.len() {
                        return Err(Error::InvalidTupleField(expr.id, expr.info.clone(), *idx));
                    }
                    inner[*idx].get_contents()?.clone()
                } else {
                    return Err(Error::ExpectedTuple(expr.id, expr.info.clone()));
                }
            }
            Expr::NamedTupleField(texpr, field) => {
                let (n, tty) = self.compile_expr_rvalue(texpr, local_lvalues, current)?;
                let (ty, idx) = if let Type::NamedTuple(inner) = tty {
                    if let Some((pos, x)) = inner
                        .iter()
                        .enumerate()
                        .find(|(_, x)| x.0.name == field.name)
                    {
                        (x.1.get_contents()?.clone(), pos)
                    } else {
                        return Err(Error::InvalidNamedTupleField(
                            expr.id,
                            expr.info.clone(),
                            field.name.clone(),
                        ));
                    }
                } else {
                    return Err(Error::ExpectedNamedTuple(expr.id, expr.info.clone()));
                };
                self.instructions[n].set(move |state| {
                    let mut rval = state.rvalues.pop().unwrap();
                    rval.indices.push(idx as i64);
                    state.rvalues.push(rval);
                    Ok(Some(next))
                });
                ty
            }
            Expr::Tuple(_) => todo!(),
            Expr::NamedTuple(_) => todo!(),
            _ => {
                return Err(Error::NotAssignable(expr.id, expr.info.clone()));
            }
        };
        Ok((next, ty.canonical_type()?))
    }

    fn compile_block<'b>(
        &mut self,
        block: &'a Block<A>,
        mut current: Ip,
        num_local_vars: usize,
        ret_type: &'b Option<&'a Type<A>>,
    ) -> Result<Ip, A> {
        let mut num_block_vars = 0;

        for stmt in block.statements.iter() {
            let numv = num_block_vars + num_local_vars;
            match stmt.get_contents()? {
                Statement::Decl(decl) => {
                    let loc = LValueStorageLocation::Local(numv);
                    num_block_vars += 1;
                    current = self.compile_vardecl(*decl, numv, loc, current)?
                }
                Statement::Expr(expr) => {
                    let (next, _) = self.compile_expr(expr, numv, current)?;
                    current = self.add_placeholder();
                    self.instructions[next].set(move |state| {
                        // Remove the value of the expression from the stack.
                        state.lvalues.pop();
                        Ok(Some(current))
                    });
                }
                Statement::If(expr, block_true, block_false) => {
                    let (next, ty) = self.compile_expr(expr, numv, current)?;
                    expect_type(&[&Type::Bool], expr, &ty)?;
                    let placeholder_t = self.add_placeholder();
                    let placeholder_f = self.add_placeholder();
                    self.instructions[next].set(move |state| {
                        pop_variant!(LValue::Bool(b), state);
                        if b {
                            Ok(Some(placeholder_t))
                        } else {
                            Ok(Some(placeholder_f))
                        }
                    });
                    let end_bt = self.compile_block(block_true, placeholder_t, numv, ret_type)?;
                    let end_bf = self.compile_block(block_false, placeholder_f, numv, ret_type)?;
                    current = self.add_placeholder();
                    self.instructions[end_bf].set(move |_| Ok(Some(current)));
                    self.instructions[end_bt].set(move |_| Ok(Some(current)));
                }
                Statement::Comment(_) => {}
                Statement::Assign(rexpr, expr) => {
                    let (next, rty) = self.compile_expr_rvalue(rexpr, numv, current)?;
                    let (next, lty) = self.compile_expr(expr, numv, next)?;
                    expect_type(&[&rty], expr, &lty)?;
                    current = self.add_placeholder();
                    self.instructions[next].set(move |state| {
                        let rval = state.rvalues.pop().unwrap();
                        let lval = state.lvalues.pop().unwrap();
                        let mut current_value = &mut state.lvalues[rval.lstack_pos];
                        for x in rval.indices.iter().rev() {
                            match current_value {
                                LValue::Array(arr) => {
                                    if (*x < 0) || (*x as usize) > arr.len() {
                                        return Err(Error::ArrayOutOfBounds(
                                            stmt.id,
                                            stmt.info.clone(),
                                            *x,
                                            arr.len(),
                                        ));
                                    }
                                    current_value = &mut arr[*x as usize];
                                }
                                LValue::Tuple(vals) => {
                                    current_value = &mut vals[*x as usize];
                                }
                                LValue::NamedTuple(vals) => {
                                    current_value = &mut vals[*x as usize];
                                }
                                // TODO(veluca): tuple/namedtuple rvalues.
                                _ => unreachable!(),
                            }
                        }
                        *current_value = lval;
                        Ok(Some(current))
                    });
                }
                Statement::Return(expr) => {
                    if let (Some(expr), Some(ty)) = (expr, ret_type) {
                        let (next, ety) = self.compile_expr(expr, numv, current)?;
                        expect_type(&[ty], expr, &ety)?;
                        self.instructions[next].set(move |state| {
                            // Take the return value from the top of the stack, remove local
                            // variables, then put the return value back on the stack.
                            let retval = state.lvalues.pop().unwrap();
                            for _ in 0..numv {
                                state.lvalues.pop();
                            }
                            state.lvalues.push(retval);
                            Ok(None)
                        });
                        current = self.add_placeholder();
                    } else if expr.is_none() && ret_type.is_none() {
                        self.instructions[current].set(move |state| {
                            for _ in 0..numv {
                                state.lvalues.pop();
                            }
                            state.lvalues.push(LValue::Void);
                            Ok(None)
                        });
                        current = self.add_placeholder();
                    } else if let Some(expr) = expr {
                        return Err(Error::ReturnHasValue(expr.id, expr.info.clone()));
                    } else {
                        return Err(Error::ReturnNoValue(stmt.id, stmt.info.clone()));
                    }
                }
                Statement::While(expr, block) => {
                    let (next, ty) = self.compile_expr(expr, numv, current)?;
                    expect_type(&[&Type::Bool], expr, &ty)?;
                    let placeholder_t = self.add_placeholder();
                    let placeholder_f = self.add_placeholder();
                    self.instructions[next].set(move |state| {
                        pop_variant!(LValue::Bool(b), state);
                        if b {
                            Ok(Some(placeholder_t))
                        } else {
                            Ok(Some(placeholder_f))
                        }
                    });
                    let end_block = self.compile_block(block, placeholder_t, numv, ret_type)?;
                    self.instructions[end_block].set(move |_| Ok(Some(current)));
                    current = placeholder_f;
                }
                Statement::For(var, expr, block) => {
                    let vty = self.program.var(*var).ty.clone();
                    let (next, ty) = self.compile_expr(expr, numv, current)?;
                    expect_type(&[&Type::Array(Box::new(vty))], expr, &ty)?;
                    self.vardecl_to_location
                        .insert(*var, LValueStorageLocation::Local(numv + 2));
                    let check_iter = self.add_placeholder();
                    self.instructions[next].set(move |state| {
                        // Iteration index
                        state.lvalues.push(LValue::Integer(0));
                        // Local variable
                        state.lvalues.push(LValue::Void);
                        Ok(Some(check_iter))
                    });
                    let placeholder_iter = self.add_placeholder();
                    let placeholder_done = self.add_placeholder();
                    self.instructions[check_iter].set(move |state| {
                        state.lvalues.pop(); // local variable
                        pop_variant!(LValue::Integer(idx), state);
                        pop_variant!(LValue::Array(arr), state);
                        let idx = idx as usize;
                        if idx < arr.len() {
                            let val = arr[idx].clone();
                            state.lvalues.push(LValue::Array(arr));
                            state.lvalues.push(LValue::Integer(idx as i64 + 1));
                            state.lvalues.push(val);
                            Ok(Some(placeholder_iter))
                        } else {
                            Ok(Some(placeholder_done))
                        }
                    });
                    let end_block = self.compile_block(
                        block,
                        placeholder_iter,
                        // We have on the stack the array, iteration index and iteration variable.
                        numv + 3,
                        ret_type,
                    )?;
                    self.instructions[end_block].set(move |_| Ok(Some(check_iter)));
                    current = placeholder_done;
                }
            }
        }

        let next = self.add_placeholder();
        // Clean up block variables.
        self.instructions[current].set(move |state| {
            for _ in 0..num_block_vars {
                state.lvalues.pop();
            }
            Ok(Some(next))
        });
        Ok(next)
    }

    fn compile_fun(&mut self, fun: FnIndex) -> Result<(), A> {
        let current = *self.fn_entry_point.get(&fun).unwrap();
        let fun = self.program.fun(fun);
        for (i, arg) in fun.args.iter().enumerate() {
            self.vardecl_to_location
                .insert(*arg, LValueStorageLocation::Local(i));
        }
        let num_args = fun.args.len();
        let ret = fun.ret.as_ref().map(|x| x.get_contents()).transpose()?;
        let next = self.compile_block(&fun.body, current, num_args, &ret)?;
        if fun.ret.is_some() {
            self.instructions[next].set(|_| Err(Error::DidNotReturn(fun.ident.clone())));
        } else {
            self.instructions[next].set(move |state| {
                for _ in 0..num_args {
                    state.lvalues.pop();
                }
                state.lvalues.push(LValue::Void);
                Ok(None)
            });
        }
        Ok(())
    }

    fn compile_vardecl(
        &mut self,
        idx: VarIndex,
        num_local_vars: usize,
        loc: LValueStorageLocation,
        current: Ip,
    ) -> Result<Ip, A> {
        self.vardecl_to_location.insert(idx, loc);
        let decl = self.program.var(idx);
        if let Some(expr) = &decl.val {
            let (next, ty) = self.compile_expr(expr, num_local_vars, current)?;
            expect_type(&[decl.ty.get_contents()?], expr, &ty)?;
            Ok(next)
        } else {
            let next = self.add_placeholder();
            self.instructions[current].set(move |state| {
                state
                    .lvalues
                    .push(LValue::new_for_type(decl.ty.get_contents()?));
                Ok(Some(next))
            });
            Ok(next)
        }
    }
}

pub fn compile<A: Ast>(program: &Program<A>) -> Result<Rc<CompiledProgram<'_, A>>, A> {
    let mut state = ProgramCompilationState {
        fn_entry_point: HashMap::new(),
        vardecl_to_location: HashMap::new(),
        ini_entry_point: 0,
        instructions: vec![],
        program,
    };

    state.ini_entry_point = state.add_placeholder();

    // Create entry/cleanup blocks for functions.
    for item in program.items.iter() {
        if let Item::Fn(f) = item.get_contents()? {
            let placeholder = state.add_placeholder();
            state.fn_entry_point.insert(*f, placeholder);
        }
    }

    // Create initialization block(s).
    let mut num_global_vars = 0;
    let mut current = state.ini_entry_point;
    for item in program.items.iter() {
        if let Item::GlobalVar(var) = item.get_contents()? {
            let loc = LValueStorageLocation::Global(num_global_vars);
            num_global_vars += 1;
            current = state.compile_vardecl(*var, 0, loc, current)?;
        }
    }

    // Compile functions.
    for item in program.items.iter() {
        if let Item::Fn(f) = item.get_contents()? {
            state.compile_fun(*f)?;
        }
    }

    state.instructions[current].set(|_| Ok(None));

    Ok(Rc::new(CompiledProgram {
        ini_entry_point: state.ini_entry_point,
        fn_entry_point: state.fn_entry_point,
        ast: program,
        instructions: state.instructions,
    }))
}
