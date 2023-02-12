use std::{collections::HashMap, fmt::Debug, rc::Rc, sync::Arc, vec};

use ordered_float::NotNan;

use crate::{
    error::Error,
    eval::{CompiledProgram, DebugInfo, Instruction, Ip, ProgramState, TemporaryIndex},
    value::{LValue, RValue},
};

use super::ast::*;

mod ops_support;

use ops_support::*;

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
        Types(types.iter().cloned().cloned().collect()),
    ))
}

#[derive(Debug)]
enum LValueStorageLocation {
    // Offset from top of stack.
    Local(usize),
    // Offset from 0 position.
    Global(usize),
}

#[derive(Debug, PartialEq, Eq)]
enum LValueStackEntry<'a, A: Ast> {
    Temporary(&'a Node<A, Expr<A>>),
    Variable(VarIndex),
}

#[derive(Debug)]
struct StackState<'a, A: Ast> {
    fun: Option<&'a Node<A, Item<A>>>,
    lstack_entries: Vec<LValueStackEntry<'a, A>>,
    rstack_entries: Vec<&'a Node<A, Expr<A>>>,
}

impl<'a, A: Ast> StackState<'a, A> {
    fn debug_info(
        &self,
        current_expr: Option<&'a Node<A, Expr<A>>>,
        num_global_vars: usize,
    ) -> DebugInfo<'a, A> {
        let variables = self
            .lstack_entries
            .iter()
            .enumerate()
            .skip(num_global_vars)
            .filter_map(|(i, entry)| {
                if let LValueStackEntry::Variable(v) = entry {
                    Some((*v, self.lstack_entries.len() - i - 1))
                } else {
                    None
                }
            })
            .collect();
        let temporaries = self
            .lstack_entries
            .iter()
            .enumerate()
            .skip(num_global_vars)
            .filter_map(|(i, entry)| {
                if let LValueStackEntry::Temporary(v) = entry {
                    Some((
                        *v,
                        TemporaryIndex::LValue(self.lstack_entries.len() - i - 1),
                    ))
                } else {
                    None
                }
            })
            .chain(self.rstack_entries.iter().enumerate().map(|(i, entry)| {
                (
                    *entry,
                    TemporaryIndex::RValue(self.rstack_entries.len() - i - 1),
                )
            }))
            .collect();
        DebugInfo {
            fun: self.fun,
            current_expr,
            variables,
            temporaries,
        }
    }

    fn get_var_loc(&self, var: VarIndex) -> usize {
        self.lstack_entries
            .iter()
            .enumerate()
            .find_map(|(i, x)| {
                if matches!(x, LValueStackEntry::Variable(v) if *v == var) {
                    Some(i)
                } else {
                    None
                }
            })
            .unwrap()
    }

    fn num_local_lvalues(&self, num_global_vars: usize) -> usize {
        self.lstack_entries.len() - num_global_vars
    }
}

#[derive(Debug)]
struct ProgramCompilationState<'a, A: Ast> {
    fn_entry_point: HashMap<FnIndex, Ip>,
    stack_state: StackState<'a, A>,
    ini_entry_point: Ip,
    num_global_vars: usize,
    instructions: Vec<Instruction<'a, A>>,
    instruction_debug_info: Vec<DebugInfo<'a, A>>,
    program: &'a Program<A>,
    current: usize,
}

impl<'a, A: Ast> ProgramCompilationState<'a, A> {
    /// Adds an instruction and the corresponding debug information at current program state.
    /// Creates a new instruction with address given by `next_instruction()`.
    fn add_instruction<F>(&mut self, f: F, expr: Option<&'a Node<A, Expr<A>>>)
    where
        F: Fn(&mut ProgramState<'a, A>) -> Result<Option<Ip>, A> + 'a,
    {
        self.instructions[self.current] = Instruction::new(f);
        self.instruction_debug_info[self.current] =
            self.stack_state.debug_info(expr, self.num_global_vars);
        self.current = self.add_placeholder();
    }

    /// Gets the address of the next instruction that will be added.
    fn next_instruction(&self) -> Ip {
        self.instructions.len()
    }

    /// Adds a placeholder instruction that must be replaced (as must its debug info), and returns
    /// its address. Use sparingly.
    fn add_placeholder(&mut self) -> Ip {
        let s = self.next_instruction();
        self.instructions
            .push(Instruction::new(|_| unreachable!("unreplaced placeholder")));
        self.instruction_debug_info.push(DebugInfo {
            fun: None,
            current_expr: None,
            variables: HashMap::new(),
            temporaries: HashMap::new(),
        });
        s
    }

    /// Adds a function call; function inputs are already assumed to be present on the stack.
    fn add_fncall(&mut self, fun: FnIndex, expr: &'a Node<A, Expr<A>>) {
        let entry = *self.fn_entry_point.get(&fun).unwrap();
        let f = self.program.fun(fun);
        let next = self.next_instruction();
        self.add_instruction(
            move |state| {
                // The stack is already set up properly, except for the return addr.
                // Upon return, the callee will put the return value in the top position of the
                // stack.
                state.ip.push(next);
                Ok(Some(entry))
            },
            Some(expr),
        );
        for _ in 0..f.args.len() {
            assert!(matches!(
                self.stack_state.lstack_entries.pop().unwrap(),
                LValueStackEntry::Temporary(_)
            ));
        }
        self.stack_state
            .lstack_entries
            .push(LValueStackEntry::Temporary(expr));
    }

    /// Adds an operation that consumes some inputs and produces some outputs, and does not
    /// otherwise alter program state in any way except possibly by changing the value of some
    /// entries in the stack.
    /// The output of the function will be recorded in the debug information.
    fn add_operation<In: OperationInput<'a, A> + 'a, Out: OperationOutput<'a, A> + 'a, F>(
        &mut self,
        op: F,
        input: In::InputInfo,
        expr: Option<&'a Node<A, Expr<A>>>,
    ) where
        F: Fn(&mut ProgramState<'a, A>, In) -> Result<Out, A> + 'a,
    {
        let num_lvals = In::count_lvalues(&input);
        let num_rvals = In::count_rvalues(&input);
        let next = self.next_instruction();
        self.add_instruction(
            move |state| {
                let input = In::get(&input, state);
                let output = op(state, input)?;
                output.push(state);
                Ok(Some(next))
            },
            expr,
        );

        for _ in 0..num_lvals {
            assert!(matches!(
                self.stack_state.lstack_entries.pop().unwrap(),
                LValueStackEntry::Temporary(_)
            ));
        }
        for _ in 0..num_rvals {
            self.stack_state.rstack_entries.pop().unwrap();
        }
        match Out::OUTPUT_TYPE {
            OperationOutputType::LValue => {
                self.stack_state
                    .lstack_entries
                    .push(LValueStackEntry::Temporary(expr.unwrap()));
            }
            OperationOutputType::RValue => {
                self.stack_state.rstack_entries.push(expr.unwrap());
            }
            OperationOutputType::Neither => {
                // Nothing to do
            }
        }
    }

    /// Adds a variable without consuming any stack values.
    fn add_variable<F: Fn(&ProgramState<'a, A>) -> LValue + 'a>(&mut self, op: F, var: VarIndex) {
        let next = self.next_instruction();
        self.add_instruction(
            move |state| {
                state.lvalues.push(op(state));
                Ok(Some(next))
            },
            None,
        );
        self.stack_state
            .lstack_entries
            .push(LValueStackEntry::Variable(var));
    }

    /// Adds a terminating instruction, i.e. an instruction that is not followed by another one.
    /// It is still valid to add instructions after this call, but those instructions will be
    /// unreachable.
    fn add_terminating_instruction<F>(&mut self, f: F)
    where
        F: Fn() -> Result<(), A> + 'a,
    {
        self.add_instruction(
            move |_| {
                f()?;
                Ok(None)
            },
            None,
        );
    }

    /// Adds a branch in control flow; this reads a boolean value from the stack and creates
    /// instruction sequences for both the true and the false branch.
    /// The functions that create the branches are passed the address of the placeholder
    /// instruction that subsequent instructions will be added after, and may return any valid
    /// instruction as long as it is guaranteed that the stack state at that instruction is
    /// compatible with the program state immediately after the boolean value is removed from the
    /// stack.
    fn add_branch<F, G>(&mut self, true_block: F, false_block: G) -> Result<(), A>
    where
        F: FnOnce(&mut Self, usize) -> Result<usize, A>,
        G: FnOnce(&mut Self, usize) -> Result<usize, A>,
    {
        let after = self.next_instruction();
        let placeholder_t = after + 1;
        let placeholder_f = after + 2;
        self.add_instruction(
            move |state| {
                let b = bool::get(&(), state);
                if b {
                    Ok(Some(placeholder_t))
                } else {
                    Ok(Some(placeholder_f))
                }
            },
            None,
        );

        // Remove the bool from the stack.
        assert!(matches!(
            self.stack_state.lstack_entries.pop().unwrap(),
            LValueStackEntry::Temporary(_)
        ));

        // Add placeholders for placeholder_t and placeholder_f.
        assert!(self.next_instruction() == placeholder_t);
        self.add_placeholder();
        assert!(self.next_instruction() == placeholder_f);
        self.add_placeholder();

        self.current = placeholder_t;
        let eob = true_block(self, after)?;
        self.add_instruction(move |_| Ok(Some(eob)), None);
        // Remove the unnecessary instruction.
        self.instructions.pop();
        self.instruction_debug_info.pop();

        self.current = placeholder_f;
        let eob = false_block(self, after)?;
        self.add_instruction(move |_| Ok(Some(eob)), None);
        // Remove the unnecessary instruction.
        self.instructions.pop();
        self.instruction_debug_info.pop();

        self.current = after;
        Ok(())
    }

    /// Adds a return instruction, which removes local variables from the stack and leaves the
    /// (top-most) value to be returned, if any.
    fn add_return(&mut self, has_retval: bool) {
        let num_local_lvalues = self.stack_state.num_local_lvalues(self.num_global_vars);
        if has_retval {
            self.add_instruction(
                move |state| {
                    let ret = state.lvalues.pop().unwrap();
                    for _ in 0..(num_local_lvalues - 1) {
                        state.lvalues.pop().unwrap();
                    }
                    state.lvalues.push(ret);
                    Ok(None)
                },
                None,
            );
            // We only remove the return value from the stack here, as this stack representation is
            // only used within the same function.
            assert!(matches!(
                self.stack_state.lstack_entries.pop().unwrap(),
                LValueStackEntry::Temporary(_)
            ));
        } else {
            self.add_instruction(
                move |state| {
                    for _ in 0..num_local_lvalues {
                        state.lvalues.pop().unwrap();
                    }
                    state.lvalues.push(LValue::Void);
                    Ok(None)
                },
                None,
            );
        }
    }

    /// Promotes the LValue on top of the stack to a variable.
    fn promote_to_variable(&mut self, var: VarIndex) {
        self.stack_state.lstack_entries.pop();
        self.stack_state
            .lstack_entries
            .push(LValueStackEntry::Variable(var));
    }

    /// Adds an operation that removes the specified number of values from the stack, ensuring that
    /// they are variables.
    fn cleanup_variables(&mut self, count: usize) {
        let next = self.next_instruction();
        self.add_instruction(
            move |state| {
                for _ in 0..count {
                    state.lvalues.pop().unwrap();
                }
                Ok(Some(next))
            },
            None,
        );
        for _ in 0..count {
            assert!(matches!(
                self.stack_state.lstack_entries.pop().unwrap(),
                LValueStackEntry::Variable(_)
            ));
        }
    }

    fn get_var_loc(&self, var: VarIndex) -> LValueStorageLocation {
        let loc = self.stack_state.get_var_loc(var);
        if loc < self.num_global_vars {
            LValueStorageLocation::Global(loc)
        } else {
            LValueStorageLocation::Local(self.stack_state.lstack_entries.len() - 1 - loc)
        }
    }

    fn compile_expr(&mut self, expr: &'a Node<A, Expr<A>>) -> Result<Type<A>, A> {
        let ty = match expr.unwrap() {
            Expr::Ref(var) => {
                match self.get_var_loc(*var) {
                    LValueStorageLocation::Local(l) => {
                        self.add_operation(
                            move |state, _: ()| {
                                Ok(state.lvalues[state.lvalues.len() - l - 1].clone())
                            },
                            (),
                            Some(expr),
                        );
                    }
                    LValueStorageLocation::Global(l) => {
                        self.add_operation(
                            move |state, _: ()| Ok(state.lvalues[l].clone()),
                            (),
                            Some(expr),
                        );
                    }
                }
                self.program.var(*var).ty.get_contents().unwrap().clone()
            }

            Expr::Integer(i) => {
                self.add_operation(move |_, _: ()| Ok(*i), (), Some(expr));
                Type::Integer
            }

            Expr::Float(i) => {
                self.add_operation(move |_, _: ()| Ok(*i), (), Some(expr));
                Type::Float
            }

            Expr::Bool(i) => {
                self.add_operation(move |_, _: ()| Ok(*i), (), Some(expr));
                Type::Bool
            }

            Expr::String(i) => {
                let arci = Arc::new(i.clone());
                self.add_operation(move |_, _: ()| Ok(arci.clone()), (), Some(expr));
                Type::String
            }

            Expr::Range(e1, e2, range_ty) => {
                let t = self.compile_expr(e1)?;
                expect_type(&[&Type::Integer], e1, &t)?;
                let t = self.compile_expr(e2)?;
                expect_type(&[&Type::Integer], e2, &t)?;

                if *range_ty == RangeType::HalfOpen {
                    self.add_operation(
                        move |_, (b, e)| Ok(LValue::Array((b..e).map(LValue::Integer).collect())),
                        (),
                        Some(expr),
                    );
                } else {
                    self.add_operation(
                        move |_, (b, e)| Ok(LValue::Array((b..=e).map(LValue::Integer).collect())),
                        (),
                        Some(expr),
                    );
                }
                Type::Array(Box::new(Node::new_with_defaults(Type::Integer)))
            }

            Expr::UnaryOp(op, e) => {
                let t = self.compile_expr(e)?;

                match op {
                    UnaryOp::Not => expect_type(&[&Type::Bool], e, &t)?,
                    UnaryOp::Neg => expect_type(&[&Type::Integer, &Type::Float], e, &t)?,
                };

                match (op, &t) {
                    (UnaryOp::Not, _) => {
                        self.add_operation(move |_, b: bool| Ok(!b), (), Some(expr));
                    }
                    (UnaryOp::Neg, Type::Integer) => {
                        self.add_operation(
                            move |_, x: i64| {
                                x.checked_neg().ok_or_else(|| {
                                    Error::UnaryOverflow(expr.id, expr.info.clone(), *op, x)
                                })
                            },
                            (),
                            Some(expr),
                        );
                    }
                    (UnaryOp::Neg, Type::Float) => {
                        self.add_operation(move |_, x: NotNan<f64>| Ok(-x), (), Some(expr));
                    }
                    _ => unreachable!("{:?} - {:?} {:?}", expr, op, t),
                };

                t
            }

            Expr::BinaryOp(e1, op, e2) => {
                let t1 = self.compile_expr(e1)?;
                let t2 = self.compile_expr(e2)?;
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
                match (&t1, op, &t2) {
                    (Type::String, BinaryOp::Sum, Type::String) => {
                        self.add_operation(
                            move |_, (s1, s2): (Arc<String>, Arc<String>)| {
                                Ok(Arc::new((*s1).clone() + &*s2))
                            },
                            (),
                            Some(expr),
                        );
                    }
                    (Type::Integer, BinaryOp::Sum, Type::Integer) => {
                        self.add_operation(
                            move |_, (i1, i2): (i64, i64)| {
                                i1.checked_add(i2).ok_or_else(|| {
                                    Error::Overflow(expr.id, expr.info.clone(), *op, i1, i2)
                                })
                            },
                            (),
                            Some(expr),
                        );
                    }
                    (Type::Integer, BinaryOp::Sub, Type::Integer) => {
                        self.add_operation(
                            move |_, (i1, i2): (i64, i64)| {
                                i1.checked_sub(i2).ok_or_else(|| {
                                    Error::Overflow(expr.id, expr.info.clone(), *op, i1, i2)
                                })
                            },
                            (),
                            Some(expr),
                        );
                    }
                    (Type::Integer, BinaryOp::Mul, Type::Integer) => {
                        self.add_operation(
                            move |_, (i1, i2): (i64, i64)| {
                                i1.checked_mul(i2).ok_or_else(|| {
                                    Error::Overflow(expr.id, expr.info.clone(), *op, i1, i2)
                                })
                            },
                            (),
                            Some(expr),
                        );
                    }
                    (Type::Integer, BinaryOp::Div, Type::Integer) => {
                        self.add_operation(
                            move |_, (i1, i2): (i64, i64)| {
                                if i2 == 0 {
                                    return Err(Error::DivisionByZero(expr.id, expr.info.clone()));
                                }
                                i1.checked_div(i2).ok_or_else(|| {
                                    Error::Overflow(expr.id, expr.info.clone(), *op, i1, i2)
                                })
                            },
                            (),
                            Some(expr),
                        );
                    }
                    (Type::Integer, BinaryOp::Mod, Type::Integer) => {
                        self.add_operation(
                            move |_, (i1, i2): (i64, i64)| {
                                if i2 == 0 {
                                    return Err(Error::DivisionByZero(expr.id, expr.info.clone()));
                                }
                                i1.checked_rem(i2).ok_or_else(|| {
                                    Error::Overflow(expr.id, expr.info.clone(), *op, i1, i2)
                                })
                            },
                            (),
                            Some(expr),
                        );
                    }
                    (Type::Float, BinaryOp::Sum, Type::Float) => {
                        self.add_operation(
                            move |_, (f1, f2): (NotNan<f64>, NotNan<f64>)| Ok(f1 + f2),
                            (),
                            Some(expr),
                        );
                    }
                    (Type::Float, BinaryOp::Sub, Type::Float) => {
                        self.add_operation(
                            move |_, (f1, f2): (NotNan<f64>, NotNan<f64>)| Ok(f1 - f2),
                            (),
                            Some(expr),
                        );
                    }
                    (Type::Float, BinaryOp::Mul, Type::Float) => {
                        self.add_operation(
                            move |_, (f1, f2): (NotNan<f64>, NotNan<f64>)| Ok(f1 * f2),
                            (),
                            Some(expr),
                        );
                    }
                    (Type::Float, BinaryOp::Div, Type::Float) => {
                        self.add_operation(
                            move |_, (f1, f2): (NotNan<f64>, NotNan<f64>)| Ok(f1 / f2),
                            (),
                            Some(expr),
                        );
                    }
                    (_, BinaryOp::Lt, _) => {
                        self.add_operation(
                            move |_, (f1, f2): (LValue, LValue)| Ok(f1 < f2),
                            (),
                            Some(expr),
                        );
                    }
                    (_, BinaryOp::Le, _) => {
                        self.add_operation(
                            move |_, (f1, f2): (LValue, LValue)| Ok(f1 <= f2),
                            (),
                            Some(expr),
                        );
                    }
                    (_, BinaryOp::Gt, _) => {
                        self.add_operation(
                            move |_, (f1, f2): (LValue, LValue)| Ok(f1 > f2),
                            (),
                            Some(expr),
                        );
                    }
                    (_, BinaryOp::Ge, _) => {
                        self.add_operation(
                            move |_, (f1, f2): (LValue, LValue)| Ok(f1 >= f2),
                            (),
                            Some(expr),
                        );
                    }
                    (_, BinaryOp::Eq, _) => {
                        self.add_operation(
                            move |_, (f1, f2): (LValue, LValue)| Ok(f1 == f2),
                            (),
                            Some(expr),
                        );
                    }
                    (_, BinaryOp::Ne, _) => {
                        self.add_operation(
                            move |_, (f1, f2): (LValue, LValue)| Ok(f1 != f2),
                            (),
                            Some(expr),
                        );
                    }
                    (Type::Bool, BinaryOp::And, Type::Bool) => {
                        self.add_operation(move |_, (b1, b2)| Ok(b1 && b2), (), Some(expr));
                    }
                    (Type::Bool, BinaryOp::Or, Type::Bool) => {
                        self.add_operation(move |_, (b1, b2)| Ok(b1 || b2), (), Some(expr));
                    }
                    _ => unreachable!("{:?} - {:?} {:?} {:?}", expr, t1, op, t2),
                }
                ty
            }

            // TODO(veluca): consider making Output a statement.
            Expr::Output(iexpr) => {
                self.compile_expr(iexpr)?;
                self.add_operation(
                    move |_, v: LValue| {
                        let out = format!("{v}");
                        Ok((out, LValue::Void))
                    },
                    (),
                    Some(expr),
                );
                Type::Void
            }

            Expr::Min(e1, e2) => {
                let t1 = self.compile_expr(e1)?;
                let t2 = self.compile_expr(e2)?;

                expect_type(&[&t1], e2, &t2)?;

                self.add_operation(
                    move |_, (x, y): (LValue, LValue)| Ok(x.min(y)),
                    (),
                    Some(expr),
                );
                t1
            }

            Expr::Max(e1, e2) => {
                let t1 = self.compile_expr(e1)?;
                let t2 = self.compile_expr(e2)?;

                expect_type(&[&t1], e2, &t2)?;

                self.add_operation(
                    move |_, (x, y): (LValue, LValue)| Ok(x.max(y)),
                    (),
                    Some(expr),
                );
                t1
            }

            Expr::Repeat(v, n) => {
                let tv = self.compile_expr(v)?;
                let tn = self.compile_expr(n)?;

                expect_type(&[&Type::Integer], n, &tn)?;

                self.add_operation(
                    |_, (v, n): (LValue, i64)| {
                        if n >= 0 {
                            Ok(LValue::Array((0..n).map(|_| v.clone()).collect()))
                        } else {
                            Err(Error::RepeatNegativeAmount(expr.id, expr.info.clone(), n))
                        }
                    },
                    (),
                    Some(expr),
                );

                Type::Array(Box::new(Node::new_with_defaults(tv)))
            }

            Expr::HasString => {
                self.add_operation(
                    |state, _: ()| Ok(state.stdin_pos < state.stdin.len()),
                    (),
                    Some(expr),
                );
                Type::Bool
            }

            Expr::NextString => {
                self.add_operation(
                    |state, _: ()| {
                        let Some(value) = state.stdin.get(state.stdin_pos) else {
                            return Err(Error::NextStringFailed(expr.id, expr.info.clone()))
                        };
                        state.stdin_pos += 1;
                        Ok(Arc::new(value.clone()))
                    },
                    (),
                    Some(expr),
                );
                Type::String
            }

            Expr::HasInt => {
                self.add_operation(
                    |state, _: ()| {
                        Ok(state
                            .stdin
                            .get(state.stdin_pos)
                            .map_or(false, |x| x.parse::<i64>().is_ok()))
                    },
                    (),
                    Some(expr),
                );
                Type::Bool
            }

            Expr::NextInt => {
                self.add_operation(
                    |state, _: ()| {
                        let Some(value) = state.stdin.get(state.stdin_pos) else {
                            return Err(Error::NextIntFailed(expr.id, expr.info.clone()))
                        };
                        let Ok(value) = value.parse::<i64>() else {
                            return Err(Error::NextIntParsingFailed(expr.id, expr.info.clone(), value.clone()))
                        };
                        state.stdin_pos += 1;
                        Ok(value)
                    },
                    (),
                    Some(expr),
                );
                Type::Integer
            }

            Expr::ToString(e) => {
                self.compile_expr(e)?;
                self.add_operation(
                    |_, val: LValue| Ok(LValue::String(Arc::new(val.to_string()))),
                    (),
                    Some(expr),
                );
                Type::String
            }

            Expr::FunctionCall(fun, args) => {
                let f = self.program.fun(*fun);
                if args.len() != f.args.len() {
                    return Err(Error::WrongArgumentNumber(
                        expr.id,
                        expr.info.clone(),
                        f.ident.clone(),
                    ));
                }
                for (ex, var) in args.iter().zip(f.args.iter()) {
                    let t = self.compile_expr(ex)?;
                    expect_type(&[self.program.var(*var).ty.get_contents()?], ex, &t)?;
                }
                self.add_fncall(*fun, expr);
                if let Some(ret) = &f.ret {
                    ret.get_contents()?.clone()
                } else {
                    Type::Void
                }
            }

            Expr::Array(a) => {
                let mut ty = None;
                for ex in a.iter() {
                    let t = self.compile_expr(ex)?;
                    if let Some(ty) = &ty {
                        expect_type(&[ty], ex, &t)?;
                    }
                    ty = Some(t);
                }
                if ty.is_none() {
                    return Err(Error::EmptyArray(expr.id, expr.info.clone()));
                }

                self.add_operation(
                    move |_, v: Vec<LValue>| Ok(LValue::Array(v.into_iter().collect())),
                    a.len(),
                    Some(expr),
                );

                Type::Array(Box::new(Node::new_with_defaults(ty.unwrap())))
            }

            Expr::Set(s) => {
                let mut ty = None;
                for ex in s.iter() {
                    let t = self.compile_expr(ex)?;
                    if let Some(ty) = &ty {
                        expect_type(&[ty], ex, &t)?;
                    }
                    ty = Some(t);
                }
                if ty.is_none() {
                    return Err(Error::EmptySet(expr.id, expr.info.clone()));
                }

                self.add_operation(
                    move |_, v: Vec<LValue>| Ok(LValue::Set(v.into_iter().collect())),
                    s.len(),
                    Some(expr),
                );

                Type::Set(Box::new(Node::new_with_defaults(ty.unwrap())))
            }

            Expr::Map(s) => {
                let mut ty: Option<(Type<A>, Type<A>)> = None;
                for ex in s.iter() {
                    let t0 = self.compile_expr(&ex.0)?;
                    let t1 = self.compile_expr(&ex.1)?;
                    if let Some(ty) = &ty {
                        expect_type(&[&ty.0], &ex.0, &t0)?;
                        expect_type(&[&ty.1], &ex.1, &t1)?;
                    }
                    ty = Some((t0, t1));
                }
                if ty.is_none() {
                    return Err(Error::EmptyMap(expr.id, expr.info.clone()));
                }

                self.add_operation(
                    move |_, v: Vec<(LValue, LValue)>| Ok(LValue::Map(v.into_iter().collect())),
                    s.len(),
                    Some(expr),
                );

                let ty = ty.unwrap();
                Type::Map(
                    Box::new(Node::new_with_defaults(ty.0)),
                    Box::new(Node::new_with_defaults(ty.1)),
                )
            }

            Expr::Tuple(a) => {
                let mut types = vec![];
                for ex in a.iter() {
                    let t = self.compile_expr(ex)?;
                    types.push(Node::new_with_defaults(t));
                }

                self.add_operation(
                    move |_, v: Vec<LValue>| Ok(LValue::Tuple(v.into_iter().collect())),
                    a.len(),
                    Some(expr),
                );

                Type::Tuple(types)
            }

            Expr::NamedTuple(a) => {
                let mut types = vec![];
                for ex in a.iter() {
                    let t = self.compile_expr(&ex.1)?;
                    types.push((ex.0.clone(), Node::new_with_defaults(t)));
                }

                self.add_operation(
                    move |_, v: Vec<LValue>| Ok(LValue::NamedTuple(v.into_iter().collect())),
                    a.len(),
                    Some(expr),
                );

                Type::NamedTuple(types)
            }

            Expr::Parens(e) => self.compile_expr(e)?,

            Expr::ArrayIndex(aexpr, iexpr) => {
                let aty = self.compile_expr(aexpr)?;
                let ity = self.compile_expr(iexpr)?;
                expect_type(&[&Type::Integer], iexpr, &ity)?;

                self.add_operation(
                    move |_, (a, i): (LValue, i64)| {
                        let LValue::Array(lval) = a else {
                            panic!("Not an array");
                        };
                        if (i < 0) || (i as usize) >= lval.len() {
                            return Err(Error::ArrayOutOfBounds(
                                expr.id,
                                expr.info.clone(),
                                i,
                                lval.len(),
                            ));
                        }
                        Ok(lval[i as usize].clone())
                    },
                    (),
                    Some(expr),
                );

                if let Type::Array(inner) = aty {
                    inner.get_contents()?.clone()
                } else {
                    return Err(Error::ExpectedArray(aexpr.id, aexpr.info.clone()));
                }
            }

            Expr::TupleField(texpr, idx) => {
                let tty = self.compile_expr(texpr)?;

                self.add_operation(
                    move |_, a: LValue| {
                        let LValue::Tuple(lval) = a else {
                            panic!("Not a tuple");
                        };
                        Ok(lval[*idx].clone())
                    },
                    (),
                    Some(expr),
                );

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
                let tty = self.compile_expr(texpr)?;
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

                self.add_operation(
                    move |_, a: LValue| {
                        let LValue::NamedTuple(lval) = a else {
                            panic!("Not a tuple");
                        };
                        Ok(lval[idx].clone())
                    },
                    (),
                    Some(expr),
                );
                ty
            }
            Expr::MethodCall(_, _, _) => todo!(),
        };
        ty.canonical_type()
    }

    fn compile_expr_rvalue(&mut self, expr: &'a Node<A, Expr<A>>) -> Result<Type<A>, A> {
        let ty = match expr.get_contents()? {
            Expr::Ref(var) => {
                match self.get_var_loc(*var) {
                    LValueStorageLocation::Local(l) => {
                        self.add_operation(
                            move |state, _: ()| {
                                Ok(RValue::new(
                                    (state.lvalues.len() - l - 1, var.clone()),
                                    vec![],
                                ))
                            },
                            (),
                            Some(expr),
                        );
                    }
                    LValueStorageLocation::Global(l) => {
                        self.add_operation(
                            move |_, _: ()| Ok(RValue::new((l, var.clone()), vec![])),
                            (),
                            Some(expr),
                        );
                    }
                }
                self.program.var(*var).ty.get_contents().unwrap().clone()
            }
            Expr::ArrayIndex(aexpr, iexpr) => {
                let aty = self.compile_expr_rvalue(aexpr)?;
                let ity = self.compile_expr(iexpr)?;
                expect_type(&[&Type::Integer], iexpr, &ity)?;
                self.add_operation(
                    move |_, (mut a, i): (RValue, i64)| {
                        let RValue::Single(inner) = &mut a else { panic!("Multiple rvalues"); };
                        inner.indices.push(i);
                        Ok(a)
                    },
                    (),
                    Some(expr),
                );
                if let Type::Array(inner) = aty {
                    inner.get_contents()?.clone()
                } else {
                    return Err(Error::ExpectedArray(aexpr.id, aexpr.info.clone()));
                }
            }
            Expr::TupleField(texpr, idx) => {
                let tty = self.compile_expr_rvalue(texpr)?;
                self.add_operation(
                    move |_, mut a: RValue| {
                        let RValue::Single(inner) = &mut a else {
                            return Err(Error::TemporaryTupleAccess(expr.id, expr.info.clone()))
                        };
                        inner.indices.push(*idx as i64);
                        Ok(a)
                    },
                    (),
                    Some(expr),
                );
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
                let tty = self.compile_expr_rvalue(texpr)?;
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
                self.add_operation(
                    move |_, mut a: RValue| {
                        let RValue::Single(inner) = &mut a else { panic!("Multiple rvalues"); };
                        inner.indices.push(idx as i64);
                        Ok(a)
                    },
                    (),
                    Some(expr),
                );
                ty
            }
            Expr::Tuple(fields) => {
                let mut types = Vec::new();
                for field in fields.iter() {
                    let t = self.compile_expr_rvalue(field)?;
                    types.push(Node::new_with_defaults(t));
                }

                self.add_operation(
                    |_, v: Vec<RValue>| {
                        if v.iter().any(|item| matches!(item, RValue::Tuple(_))) {
                            return Err(Error::NestedTemporaryTuple(expr.id, expr.info.clone()));
                        }
                        Ok(RValue::Tuple(v))
                    },
                    fields.len(),
                    Some(expr),
                );

                Type::Tuple(types)
            }
            Expr::NamedTuple(_) => todo!(),
            _ => {
                return Err(Error::NotAssignable(expr.id, expr.info.clone()));
            }
        };
        ty.canonical_type()
    }

    fn compile_block<'b>(
        &mut self,
        block: &'a Block<A>,
        ret_type: &'b Option<&'a Type<A>>,
    ) -> Result<(), A> {
        let mut num_block_vars = 0;

        for stmt in block.statements.iter() {
            match stmt.get_contents()? {
                Statement::Decl(decl) => {
                    num_block_vars += 1;
                    self.compile_vardecl(*decl)?
                }
                Statement::Expr(expr) => {
                    self.compile_expr(expr)?;
                    // Remove the value of the expression from the stack.
                    self.add_operation(|_, _: LValue| Ok(()), (), Some(expr));
                }
                Statement::If(expr, block_true, block_false) => {
                    let ty = self.compile_expr(expr)?;
                    expect_type(&[&Type::Bool], expr, &ty)?;
                    self.add_branch(
                        |state, end| {
                            state.compile_block(block_true, ret_type)?;
                            Ok(end)
                        },
                        |state, end| {
                            state.compile_block(block_false, ret_type)?;
                            Ok(end)
                        },
                    )?;
                }
                Statement::Comment(_) => {}
                Statement::Assign(rexpr, expr) => {
                    let rty = self.compile_expr_rvalue(rexpr)?;
                    let lty = self.compile_expr(expr)?;
                    expect_type(&[&rty], expr, &lty)?;
                    self.add_operation(
                        move |state, (rval, lval): (RValue, LValue)| {
                            let (rvals, lvals) = match (rval, lval) {
                                (RValue::Single(r), l) => (vec![RValue::Single(r)], vec![l]),
                                (RValue::Tuple(r), LValue::Tuple(l)) =>
                                    (r, l.into_iter().collect()),
                                _ => unreachable!(),
                            };

                            for (entry, lentry) in rvals.iter().zip(lvals.into_iter()) {
                                let RValue::Single(entry) = entry else { panic!("Nested rvalue tuples"); };

                                let mut current_value = &mut state.lvalues[entry.lstack_pos.0];
                                for x in entry.indices.iter().rev() {
                                    match current_value {
                                        LValue::Array(arr) => {
                                            if (*x < 0) || (*x as usize) >= arr.len() {
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
                                        // TODO(veluca): namedtuple rvalues.
                                        _ => unreachable!(),
                                    }
                                }
                                *current_value = lentry;
                            }

                            Ok(())
                        },
                        (),
                        None,
                    );
                }
                Statement::Return(expr) => {
                    if let (Some(expr), Some(ty)) = (expr, ret_type) {
                        let ety = self.compile_expr(expr)?;
                        expect_type(&[ty], expr, &ety)?;
                        self.add_return(true);
                    } else if expr.is_none() && ret_type.is_none() {
                        self.add_return(false);
                    } else if let Some(expr) = expr {
                        return Err(Error::ReturnHasValue(expr.id, expr.info.clone()));
                    } else {
                        return Err(Error::ReturnNoValue(stmt.id, stmt.info.clone()));
                    }
                }
                Statement::While(expr, block) => {
                    let start = self.current;
                    let ty = self.compile_expr(expr)?;
                    expect_type(&[&Type::Bool], expr, &ty)?;
                    self.add_branch(
                        |state, _| {
                            state.compile_block(block, ret_type)?;
                            Ok(start)
                        },
                        |_, end| Ok(end),
                    )?;
                }
                Statement::For(var, expr, block, idx_var, check_placeholder_expr) => {
                    let vdecl = self.program.var(*var);
                    let vty = vdecl.ty.clone();
                    let ty = self.compile_expr(expr)?;
                    expect_type(&[&Type::Array(Box::new(vty))], expr, &ty)?;
                    // We now have the array at the top of the stack; add the starting index.
                    self.add_variable(|_| LValue::Integer(0), *idx_var);
                    let loop_start = self.current;
                    self.add_operation(
                        |state, ()| {
                            let numl = state.lvalues.len();
                            let LValue::Integer(idx) = &state.lvalues[numl - 1] else { panic!("index is not integer") };
                            let LValue::Array(arr) = &state.lvalues[numl - 2] else { panic!("array is not an array") };
                            Ok((*idx as usize) < arr.len())
                        },
                        (),
                        Some(check_placeholder_expr),
                    );
                    self.add_branch(
                        |state, _| {
                            // Add the variable on the stack.
                            state.add_variable(
                                |state| {
                                    let numl = state.lvalues.len();
                                    let LValue::Integer(idx) = &state.lvalues[numl - 1] else { panic!("index is not integer") };
                                    let LValue::Array(arr) = &state.lvalues[numl - 2] else { panic!("array is not an array") };
                                    arr[*idx as usize].clone()
                                },
                                *var,
                            );

                            state.compile_block(block, ret_type)?;

                            // Remove the variable from the stack.
                            state.cleanup_variables(1);
                            // Increment iteration index
                            state.add_operation(
                                |state, ()| {
                                    let LValue::Integer(idx) = state.lvalues.last_mut().unwrap() else { panic!("index is not integer") };
                                    *idx += 1;
                                    Ok(())
                                },
                                (),
                                None,
                            );
                            Ok(loop_start)
                        },
                        |state, end| {
                            // Remove idx and array.
                            state.cleanup_variables(1);
                            state.add_operation(|_, _: LValue| Ok(()), (), None);
                            Ok(end)
                        },
                    )?;
                }
            }
        }

        self.cleanup_variables(num_block_vars);
        Ok(())
    }

    fn compile_fun(&mut self, fun: FnIndex) -> Result<(), A> {
        self.current = *self.fn_entry_point.get(&fun).unwrap();
        let fun = self.program.fun(fun);
        for arg in fun.args.iter() {
            self.stack_state
                .lstack_entries
                .push(LValueStackEntry::Variable(*arg));
        }
        let num_args = fun.args.len();
        let ret = fun.ret.as_ref().map(|x| x.get_contents()).transpose()?;
        self.compile_block(&fun.body, &ret)?;
        if fun.ret.is_none() {
            self.add_return(false);
        }
        self.add_terminating_instruction(|| Err(Error::DidNotReturn(fun.ident.clone())));
        self.cleanup_variables(num_args);
        Ok(())
    }

    fn compile_vardecl(&mut self, idx: VarIndex) -> Result<(), A> {
        let decl = self.program.var(idx);
        if let Some(expr) = &decl.val {
            let ty = self.compile_expr(expr)?;
            expect_type(&[decl.ty.get_contents()?], expr, &ty)?;
        } else {
            let val = LValue::new_for_type(decl.ty.get_contents()?);
            self.add_variable(move |_| val.clone(), idx);
        }
        self.promote_to_variable(idx);
        Ok(())
    }
}

pub fn compile<A: Ast>(program: &Program<A>) -> Result<Rc<CompiledProgram<'_, A>>, A> {
    let mut state = ProgramCompilationState {
        fn_entry_point: HashMap::new(),
        ini_entry_point: 0,
        num_global_vars: 0,
        stack_state: StackState {
            fun: None,
            lstack_entries: vec![],
            rstack_entries: vec![],
        },
        instructions: vec![],
        instruction_debug_info: vec![],
        program,
        current: 0,
    };

    state.ini_entry_point = state.add_placeholder();

    // Create entry/cleanup blocks for functions.
    for item in program.items.iter() {
        if let Item::Fn(f) = item.get_contents()? {
            let placeholder = state.add_placeholder();
            state.fn_entry_point.insert(*f, placeholder);
        }
    }

    state.current = state.ini_entry_point;

    let mut num_global_vars = 0;

    // Create initialization block(s).
    for item in program.items.iter() {
        if let Item::GlobalVar(var) = item.get_contents()? {
            state.compile_vardecl(*var)?;
            num_global_vars += 1;
        }
    }

    let main = program
        .items
        .iter()
        .filter_map(|x| match x.get_contents() {
            Ok(Item::Fn(i)) => Some(*i),
            _ => None,
        })
        .find(|x| program.fun(*x).ident.name == "main")
        .ok_or_else(|| {
            Error::<A>::UnrecognizedFunction(Ident {
                name: "main".into(),
                info: A::NodeInfo::default(),
            })
        })?;

    {
        let main_fun = program.fun(main);
        if !main_fun.args.is_empty() || main_fun.ret.is_some() {
            return Err(Error::InvalidMainSignature(main_fun.ident.clone()));
        }
    }

    state.add_fncall(main, &program.entry_placeholder);

    state.add_terminating_instruction(|| Ok(()));

    // We now have one value on the global stack that is not accounted for (the return value of
    // main()). Therefore, we increase the count of global variables by one.
    state.num_global_vars = num_global_vars + 1;

    // Compile functions.
    for item in program.items.iter() {
        if let Item::Fn(f) = item.get_contents()? {
            state.stack_state.fun = Some(item);
            state.compile_fun(*f)?;
        }
    }

    Ok(Rc::new(CompiledProgram {
        ini_entry_point: state.ini_entry_point,
        fn_entry_point: state.fn_entry_point,
        ast: program,
        instruction_debug_info: state.instruction_debug_info,
        instructions: state.instructions,
    }))
}
