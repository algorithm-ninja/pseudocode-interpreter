#![allow(unused_assignments)]

use std::{fmt::Debug, sync::Arc};

use im::{HashMap, Vector};

use crate::{
    ast::*,
    error::Error,
    value::{LValue, RValue},
};

#[derive(Clone, Debug)]
struct Scope {
    variables: HashMap<VarIndex, LValue>,
}

#[derive(Clone, Debug)]
struct CheckpointableStack<T: Clone + Debug> {
    stack: Vector<T>,
    checkpoints: Vector<usize>,
    stack_len: usize,
}

impl<T: Clone + Debug> CheckpointableStack<T> {
    fn new() -> CheckpointableStack<T> {
        CheckpointableStack {
            stack: Vector::new(),
            checkpoints: Vector::new(),
            stack_len: 0,
        }
    }

    fn checkpoint(&mut self) {
        self.checkpoints.push_back(self.stack_len)
    }

    fn rollback(&mut self) {
        assert!(!self.checkpoints.is_empty());
        self.stack_len = self.checkpoints.pop_back().unwrap()
    }

    fn get(&self, pos: usize) -> Option<&T> {
        assert!(!self.checkpoints.is_empty());
        let pos = *self.checkpoints.back().unwrap() + pos;
        if pos < self.stack_len {
            self.stack.get(pos)
        } else {
            None
        }
    }

    fn push(&mut self, val: T, pos: usize) {
        assert!(!self.checkpoints.is_empty());
        let pos = *self.checkpoints.back().unwrap() + pos;
        assert_eq!(pos, self.stack_len);
        self.stack_len += 1;
        if pos < self.stack.len() {
            self.stack[pos] = val;
        } else {
            self.stack.push_back(val)
        }
    }
}

#[derive(Clone, Debug)]
enum EvalStackElement<'a, A: Ast> {
    ExprLValue(&'a Node<A, Expr<A>>, usize),
    ExprRValue(&'a Node<A, Expr<A>>, usize),
    Statement(&'a Block<A>, usize, usize),
}

/// Copying a ProgramState is cheap; to keep track of past states, just clone the instance before
/// calling a mutating method.
#[derive(Clone, Debug)]
pub struct ProgramState<'a, A: Ast> {
    program: &'a Program<A>,
    global_variables: Scope,
    local_variables: Vector<Scope>,
    // Contains the *next* expression/statement to be evaluated.
    eval_stack: Vector<EvalStackElement<'a, A>>,
    evaluated_expression_lvalues: CheckpointableStack<LValue>,
    evaluated_expression_rvalues: CheckpointableStack<RValue>,
    stdout: Vector<String>,
}

pub type Result<T, A> = std::result::Result<T, Error<A>>;

fn check_fun_ret<A: Ast>(fun: &FnDecl<A>, ty: &Type<A>) -> Result<(), A> {
    if let Some(t) = &fun.ret {
        if !t.unwrap().is_same(ty)? {
            return Err(Error::TypeError(
                t.id,
                t.info.clone(),
                t.get_contents()?.clone(),
                vec![ty.clone()],
            ));
        }
    } else if !ty.is_same(&Type::Void)? {
        return Err(Error::TypeError(
            0,
            A::NodeInfo::default(),
            Type::Void,
            vec![ty.clone()],
        ));
    }

    Ok(())
}

macro_rules! er {
    ($s:expr, $idx: expr, $e:expr) => {{
        let i = $idx;
        $idx += 1;
        if let Some(x) = $s.get_cached_rvalue(i) {
            x
        } else {
            $s.start_eval(EvalStackElement::ExprRValue($e, i));
            return Ok(None);
        }
    }};
}

macro_rules! el {
    ($s:expr, $idx: expr, $e:expr) => {{
        let i = $idx;
        $idx += 1;
        if let Some(x) = $s.get_cached_lvalue(i) {
            x
        } else {
            $s.start_eval(EvalStackElement::ExprLValue($e, i));
            return Ok(None);
        }
    }};
}

impl<'a, A: Ast> ProgramState<'a, A> {
    fn start_eval(&mut self, to_eval: EvalStackElement<'a, A>) {
        self.eval_stack.push_back(to_eval);
        self.evaluated_expression_rvalues.checkpoint();
        self.evaluated_expression_lvalues.checkpoint();
    }

    fn finish_eval(&mut self) -> EvalStackElement<'a, A> {
        self.evaluated_expression_rvalues.rollback();
        self.evaluated_expression_lvalues.rollback();
        self.eval_stack.pop_back().unwrap()
    }

    fn start_block(&mut self, blk: &'a Block<A>, vars: &[VarIndex], values: &[LValue]) {
        assert!(vars.len() == values.len());
        self.local_variables.push_back(Scope {
            variables: vars
                .iter()
                .zip(values.iter())
                .map(|(v, val)| (*v, val.clone()))
                .collect(),
        });
        self.start_eval(EvalStackElement::Statement(blk, 0, 0));
    }

    fn finish_block(&mut self) {
        assert!(matches!(
            self.eval_stack.back(),
            Some(EvalStackElement::Statement(_, _, _))
        ));
        self.finish_eval();
        self.local_variables.pop_back();
    }

    fn finish_lvalue_eval(&mut self, lvalue: LValue) {
        assert!(matches!(
            self.eval_stack.back(),
            Some(EvalStackElement::ExprLValue(_, _))
        ));
        if let EvalStackElement::ExprLValue(_, idx) = self.finish_eval() {
            self.evaluated_expression_lvalues.push(lvalue, idx);
        }
    }

    fn get_cached_lvalue(&mut self, idx: usize) -> Option<LValue> {
        self.evaluated_expression_lvalues.get(idx).cloned()
    }

    fn finish_rvalue_eval(&mut self, rvalue: RValue) {
        assert!(matches!(
            self.eval_stack.back(),
            Some(EvalStackElement::ExprRValue(_, _))
        ));
        if let EvalStackElement::ExprRValue(_, idx) = self.finish_eval() {
            self.evaluated_expression_rvalues.push(rvalue, idx);
        }
    }

    fn get_cached_rvalue(&mut self, idx: usize) -> Option<RValue> {
        self.evaluated_expression_rvalues.get(idx).cloned()
    }

    fn check_callee(&mut self, lvalue: LValue) -> Result<Option<LValue>, A> {
        if self.eval_stack.is_empty() {
            return Ok(Some(lvalue));
        }
        let back = self.eval_stack.back_mut().unwrap();
        if let EvalStackElement::ExprLValue(expr, _) = back {
            if let Expr::FunctionCall(f, _) = expr.unwrap() {
                if self.program.fun(*f).ret.is_some() && lvalue == LValue::Void {
                    return Err(Error::DidNotReturn(expr.id, expr.info.clone()));
                }
                self.finish_lvalue_eval(lvalue);
            }
        }
        Ok(None)
    }

    fn next_loop_iter(&mut self, len: usize) -> Option<usize> {
        if let EvalStackElement::Statement(_, _, ref mut lp) = self.eval_stack.back_mut().unwrap() {
            let next = *lp;
            if next < len {
                *lp += 1;
                Some(next)
            } else {
                None
            }
        } else {
            unreachable!();
        }
    }

    fn clear_statement(&mut self) {
        self.evaluated_expression_rvalues.rollback();
        self.evaluated_expression_lvalues.rollback();
        self.evaluated_expression_rvalues.checkpoint();
        self.evaluated_expression_lvalues.checkpoint();
    }

    fn next_statement(&mut self) {
        if let EvalStackElement::Statement(_, ref mut pos, ref mut lp) =
            self.eval_stack.back_mut().unwrap()
        {
            *pos += 1;
            *lp = 0;
            self.clear_statement();
        }
    }

    fn get_variable(&mut self, var: &VarIndex) -> &mut LValue {
        if let Some(x) = self.global_variables.variables.get_mut(var) {
            x
        } else {
            self.local_variables
                .iter_mut()
                .rev()
                .find_map(|x| x.variables.get_mut(var))
                .unwrap()
        }
    }

    /// Starts a program from scratch, inizializing global variables to their default values.
    pub fn new(program: &'a Program<A>) -> ProgramState<'a, A> {
        let mut vars = HashMap::new();
        for item in program.items.iter() {
            if let Item::GlobalVar(v) = *item.unwrap() {
                let vd = program.var(v);
                // Global variables cannot be inizialized (this is checked by the parser).
                assert!(vd.val.is_none());
                vars.insert(v, LValue::new_for_type(vd.ty.unwrap()));
            }
        }

        ProgramState {
            program,
            global_variables: Scope { variables: vars },
            local_variables: Vector::new(),
            eval_stack: Vector::new(),
            evaluated_expression_rvalues: CheckpointableStack::new(),
            evaluated_expression_lvalues: CheckpointableStack::new(),
            stdout: Vector::new(),
        }
    }

    /// Starts evaluation of the given function on the given arguments. Will error out if already
    /// evaluating a function.
    pub fn evaluate_fun(
        &mut self,
        fun: &str,
        args: &[LValue],
        ret_type: &Type<A>,
    ) -> Result<(), A> {
        let fname = fun;
        let fun = self.program.items.iter().find_map(|x| {
            if let Item::Fn(f) = *x.unwrap() {
                let f = self.program.fun(f);
                if f.ident.name == fun {
                    return Some(f);
                }
            }
            None
        });
        let fun = if let Some(fun) = fun {
            fun
        } else {
            return Err(Error::UnrecognizedFunction(Ident {
                name: fname.to_owned(),
                info: A::NodeInfo::default(),
            }));
        };

        check_fun_ret(fun, ret_type)?;

        if !args.is_empty() || !fun.args.is_empty() {
            todo!();
        }

        assert!(self.eval_stack.is_empty());

        self.start_block(&fun.body, &fun.args[..], args);
        Ok(())
    }

    /// Executes one evaluation step, returning None if evaluation of the top-level function is not
    /// yet done, and its return value if it is.
    pub fn eval_step(&mut self) -> Result<Option<LValue>, A> {
        assert!(!self.eval_stack.is_empty());
        match self.eval_stack.back().unwrap() {
            EvalStackElement::Statement(blk, pos, _) => self.eval_block_pos(blk, *pos),
            EvalStackElement::ExprLValue(expr, _) => self.eval_lvalue(expr),
            EvalStackElement::ExprRValue(expr, _) => self.eval_rvalue(expr),
        }
    }

    fn eval_block_pos(&mut self, blk: &'a Block<A>, pos: usize) -> Result<Option<LValue>, A> {
        if pos >= blk.statements.len() {
            self.finish_block();
            return self.check_callee(LValue::Void);
        }
        let (id, info) = (blk.statements[pos].id, blk.statements[pos].info.clone());

        let mut eridx = 0;
        let mut elidx = 0;

        match blk.statements[pos].unwrap() {
            Statement::Comment(_) => {}
            Statement::Decl(v) => {
                let d = self.program.var(*v);
                let init = if let Some(val) = &d.val {
                    el!(self, elidx, val)
                } else {
                    LValue::new_for_type(d.ty.unwrap())
                };
                self.local_variables
                    .back_mut()
                    .unwrap()
                    .variables
                    .insert(*v, init);
            }
            Statement::Assign(to, expr) => {
                let val = el!(self, elidx, expr);
                let dest = er!(self, eridx, to);
                let mut current_value = self.get_variable(&dest.vardecl);
                for idx in dest.indices.iter().rev() {
                    match (idx, current_value) {
                        (LValue::Integer(x), LValue::Array(arr)) => {
                            if *x < 0 || *x as usize >= arr.len() {
                                return Err(Error::ArrayOutOfBounds(id, info, *x, arr.len()));
                            }
                            current_value = &mut arr[*x as usize];
                        }
                        (LValue::Integer(x), LValue::Tuple(vals)) => {
                            current_value = &mut vals[*x as usize];
                        }
                        (LValue::String(x), LValue::NamedTuple(vals)) => {
                            current_value = vals.get_mut(&**x).unwrap();
                        }
                        // TODO: tuple/namedtuple rvalues.
                        _ => unreachable!(),
                    }
                }
                *current_value = val;
            }
            Statement::If(expr, blk1, blk2) => {
                let cond = el!(self, elidx, expr);
                // We are done with this statement. Clear the eval state and ensure that next
                // call to eval_step starts from the following statement.
                self.next_statement();
                if let LValue::Bool(c) = cond {
                    if c {
                        self.start_block(blk1, &[], &[]);
                        return Ok(None);
                    } else {
                        self.start_block(blk2, &[], &[]);
                        return Ok(None);
                    }
                } else {
                    unreachable!()
                }
            }
            Statement::While(expr, blk) => {
                let cond = el!(self, elidx, expr);
                // Clear evaluation of this expression, so that it gets re-evaluated next time.
                self.clear_statement();
                if let LValue::Bool(c) = cond {
                    if c {
                        self.start_block(blk, &[], &[]);
                        return Ok(None);
                    }
                } else {
                    unreachable!()
                }
            }
            Statement::For(var, expr, blk) => {
                let arr = el!(self, elidx, expr);

                if let LValue::Array(a) = arr {
                    if let Some(idx) = self.next_loop_iter(a.len()) {
                        let val = a[idx].clone();
                        self.start_block(blk, &[*var], &[val]);
                        return Ok(None);
                    }
                } else {
                    unreachable!()
                }
            }
            Statement::Expr(expr) => {
                el!(self, elidx, expr);
            }
            Statement::Return(expr) => {
                let rval = if let Some(expr) = expr {
                    el!(self, elidx, expr)
                } else {
                    LValue::Void
                };
                // Pop all statements from the stack until we encounter the callee.
                while matches!(
                    self.eval_stack.back(),
                    Some(EvalStackElement::Statement(_, _, _))
                ) {
                    self.finish_block();
                }
                return self.check_callee(rval);
            }
        }
        self.next_statement();
        Ok(None)
    }

    fn eval_lvalue(&mut self, expr: &'a Node<A, Expr<A>>) -> Result<Option<LValue>, A> {
        let (id, info) = (expr.id, expr.info.clone());

        let mut elidx = 0;

        let val = match expr.unwrap() {
            Expr::Ref(var) => self.get_variable(var).clone(),
            Expr::Integer(i) => LValue::Integer(*i),
            Expr::Float(i) => LValue::Float(*i),
            Expr::Bool(i) => LValue::Bool(*i),
            Expr::String(i) => LValue::String(Arc::new(i.clone())),
            Expr::Array(a) => {
                let vals = a
                    .iter()
                    .map(|x| Ok(Some(el!(self, elidx, x))))
                    .collect::<Result<Option<_>, A>>()?;
                if let Some(vals) = vals {
                    LValue::Array(vals)
                } else {
                    return Ok(None);
                }
            }
            Expr::Set(a) => {
                let vals = a
                    .iter()
                    .map(|x| Ok(Some(el!(self, elidx, x))))
                    .collect::<Result<Option<_>, A>>()?;
                if let Some(vals) = vals {
                    LValue::Set(vals)
                } else {
                    return Ok(None);
                }
            }
            Expr::Map(a) => {
                let vals = a
                    .iter()
                    .map(|(x, y)| Ok(Some((el!(self, elidx, x), el!(self, elidx, y)))))
                    .collect::<Result<Option<_>, A>>()?;
                if let Some(vals) = vals {
                    LValue::Map(vals)
                } else {
                    return Ok(None);
                }
            }
            Expr::Tuple(a) => {
                let vals = a
                    .iter()
                    .map(|x| Ok(Some(el!(self, elidx, x))))
                    .collect::<Result<Option<_>, A>>()?;
                if let Some(vals) = vals {
                    LValue::Tuple(vals)
                } else {
                    return Ok(None);
                }
            }
            Expr::NamedTuple(a) => {
                let vals = a
                    .iter()
                    .map(|(n, x)| Ok(Some((n.name.clone(), el!(self, elidx, x)))))
                    .collect::<Result<Option<_>, A>>()?;
                if let Some(vals) = vals {
                    LValue::NamedTuple(vals)
                } else {
                    return Ok(None);
                }
            }
            Expr::Parens(a) => {
                el!(self, elidx, a)
            }
            Expr::Range(e1, e2, ty) => {
                let e1 = el!(self, elidx, e1);
                let e2 = el!(self, elidx, e2);
                if let LValue::Integer(e1) = e1 {
                    if let LValue::Integer(e2) = e2 {
                        if *ty == RangeType::HalfOpen {
                            LValue::Array((e1..e2).map(LValue::Integer).collect())
                        } else {
                            LValue::Array((e1..=e2).map(LValue::Integer).collect())
                        }
                    } else {
                        unreachable!()
                    }
                } else {
                    unreachable!()
                }
            }
            Expr::Not(e) => {
                let e = el!(self, elidx, e);
                if let LValue::Bool(e) = e {
                    LValue::Bool(!e)
                } else {
                    unreachable!()
                }
            }
            Expr::BinaryOp(e1, op, e2) => {
                let e1 = el!(self, elidx, e1);
                let e2 = el!(self, elidx, e2);
                // TODO: catch arithmetic errors.
                match (e1, op, e2) {
                    (x, BinaryOp::Gt, y) => LValue::Bool(x > y),
                    (x, BinaryOp::Lt, y) => LValue::Bool(x < y),
                    (x, BinaryOp::Ge, y) => LValue::Bool(x >= y),
                    (x, BinaryOp::Le, y) => LValue::Bool(x <= y),
                    (x, BinaryOp::Eq, y) => LValue::Bool(x == y),
                    (x, BinaryOp::Ne, y) => LValue::Bool(x != y),
                    (LValue::Integer(x), BinaryOp::Sum, LValue::Integer(y)) => {
                        LValue::Integer(x + y)
                    }
                    (LValue::Float(x), BinaryOp::Sum, LValue::Float(y)) => LValue::Float(x + y),
                    (LValue::String(x), BinaryOp::Sum, LValue::String(y)) => {
                        LValue::String(Arc::new((*x).clone() + &y))
                    }
                    (LValue::Integer(x), BinaryOp::Sub, LValue::Integer(y)) => {
                        LValue::Integer(x - y)
                    }
                    (LValue::Float(x), BinaryOp::Sub, LValue::Float(y)) => LValue::Float(x - y),
                    (LValue::Integer(x), BinaryOp::Mul, LValue::Integer(y)) => {
                        LValue::Integer(x * y)
                    }
                    (LValue::Float(x), BinaryOp::Mul, LValue::Float(y)) => LValue::Float(x * y),
                    (LValue::Integer(x), BinaryOp::Div, LValue::Integer(y)) => {
                        LValue::Integer(x / y)
                    }
                    (LValue::Float(x), BinaryOp::Div, LValue::Float(y)) => LValue::Float(x / y),
                    (LValue::Integer(x), BinaryOp::Mod, LValue::Integer(y)) => {
                        LValue::Integer(x % y)
                    }
                    (LValue::Bool(x), BinaryOp::And, LValue::Bool(y)) => LValue::Bool(x && y),
                    (LValue::Bool(x), BinaryOp::Or, LValue::Bool(y)) => LValue::Bool(x || y),
                    _ => unreachable!(),
                }
            }
            Expr::ArrayIndex(arr, idx) => {
                let arr = el!(self, elidx, arr);
                let idx = el!(self, elidx, idx);
                if let LValue::Array(arr) = arr {
                    if let LValue::Integer(x) = idx {
                        if x < 0 || x as usize >= arr.len() {
                            return Err(Error::ArrayOutOfBounds(id, info, x, arr.len()));
                        }
                        arr[x as usize].clone()
                    } else {
                        unreachable!()
                    }
                } else {
                    unreachable!()
                }
            }
            Expr::TupleField(tpl, idx) => {
                let tpl = el!(self, elidx, tpl);
                if let LValue::Tuple(tpl) = tpl {
                    tpl[*idx].clone()
                } else {
                    unreachable!()
                }
            }
            Expr::NamedTupleField(tpl, field) => {
                let tpl = el!(self, elidx, tpl);
                if let LValue::NamedTuple(tpl) = tpl {
                    tpl.get(&field.name).unwrap().clone()
                } else {
                    unreachable!()
                }
            }
            Expr::Output(expr) => {
                let expr = el!(self, elidx, expr);
                // TODO: prettier printing
                self.stdout.push_back(format!("{:?}", expr));
                LValue::Void
            }
            Expr::MethodCall(_, _, _) => {
                todo!()
            }
            Expr::FunctionCall(f, args) => {
                let args = args
                    .iter()
                    .map(|x| Ok(Some(el!(self, elidx, x))))
                    .collect::<Result<Option<Vec<_>>, A>>()?;
                if let Some(vals) = args {
                    let f = self.program.fun(*f);
                    self.start_block(&f.body, &f.args[..], &vals[..]);
                }
                return Ok(None);
            }
        };
        self.finish_lvalue_eval(val);
        Ok(None)
    }

    fn eval_rvalue(&mut self, expr: &'a Node<A, Expr<A>>) -> Result<Option<LValue>, A> {
        let mut eridx = 0;
        let mut elidx = 0;

        let rvalue = match expr.unwrap() {
            Expr::Ref(var) => RValue {
                vardecl: *var,
                indices: Vector::new(),
            },
            Expr::ArrayIndex(aexpr, iexpr) => {
                let mut arr = er!(self, eridx, aexpr);
                let idx = el!(self, elidx, iexpr);
                arr.indices.push_back(idx);
                arr
            }
            Expr::TupleField(aexpr, idx) => {
                let mut arr = er!(self, eridx, aexpr);
                arr.indices.push_back(LValue::Integer(*idx as i64));
                arr
            }
            Expr::NamedTupleField(aexpr, field) => {
                let mut arr = er!(self, eridx, aexpr);
                arr.indices
                    .push_back(LValue::String(Arc::new(field.name.clone())));
                arr
            }
            Expr::Tuple(_) => todo!(),
            Expr::NamedTuple(_) => todo!(),
            _ => unreachable!(),
        };
        self.finish_rvalue_eval(rvalue);
        Ok(None)
    }

    pub fn stdout(&self) -> &Vector<String> {
        &self.stdout
    }
}
