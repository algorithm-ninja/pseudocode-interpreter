use std::sync::Arc;

use by_address::ByAddress;
use im::{HashMap, Vector};

use crate::{
    ast::{
        Ast, BinaryOp, Block, Expr, FnDecl, Ident, Item, Node, Program, RangeType, Statement, Type,
        VarDecl,
    },
    error::Error,
    value::{LValue, RValue},
};

#[derive(Clone, Debug)]
struct Scope<'a, A: Ast> {
    variables: HashMap<ByAddress<&'a VarDecl<A>>, LValue>,
}

#[derive(Clone, Debug)]
struct StatementStack<'a, A: Ast> {
    evaluated_expression_lvalues: HashMap<ByAddress<&'a Node<A, Expr<A>>>, LValue>,
    evaluated_expression_rvalues: HashMap<ByAddress<&'a Node<A, Expr<A>>>, RValue<'a, A>>,
    loop_pos: usize,
}

impl<'a, A: Ast> StatementStack<'a, A> {
    fn new() -> StatementStack<'a, A> {
        StatementStack {
            evaluated_expression_lvalues: HashMap::new(),
            evaluated_expression_rvalues: HashMap::new(),
            loop_pos: 0,
        }
    }
}

#[derive(Clone, Debug)]
enum EvalStackElement<'a, A: Ast> {
    ExprLValue(&'a Node<A, Expr<A>>),
    ExprRValue(&'a Node<A, Expr<A>>),
    Statement(&'a Block<A>, usize),
}

/// Copying a ProgramState is cheap; to keep track of past states, just clone the instance before
/// calling a mutating method.
#[derive(Clone, Debug)]
pub struct ProgramState<'a, A: Ast> {
    program: &'a Program<A>,
    global_variables: Scope<'a, A>,
    local_variables: Vector<Scope<'a, A>>,
    // Contains the *next* expression/statement to be evaluated.
    eval_stack: Vector<EvalStackElement<'a, A>>,
    statement_stack: Vector<StatementStack<'a, A>>,
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
    } else {
        if !ty.is_same(&Type::Void)? {
            return Err(Error::TypeError(
                0,
                A::NodeInfo::default(),
                Type::Void,
                vec![ty.clone()],
            ));
        }
    }
    Ok(())
}

macro_rules! er {
    ($s:expr, $e:expr) => {
        if let Some(x) = $s
            .statement_stack
            .back()
            .unwrap()
            .evaluated_expression_rvalues
            .get(&ByAddress($e))
        {
            x.clone()
        } else {
            $s.eval_stack.push_back(EvalStackElement::ExprRValue($e));
            return Ok(None);
        }
    };
}

macro_rules! el {
    ($s:expr, $e:expr) => {
        if let Some(x) = $s
            .statement_stack
            .back()
            .unwrap()
            .evaluated_expression_lvalues
            .get(&ByAddress($e))
        {
            x.clone()
        } else {
            $s.eval_stack.push_back(EvalStackElement::ExprLValue($e));
            return Ok(None);
        }
    };
}

impl<'a, A: Ast> ProgramState<'a, A> {
    /// Starts a program from scratch, inizializing global variables to their default values.
    pub fn new(program: &'a Program<A>) -> ProgramState<'a, A> {
        let mut vars = HashMap::new();
        for item in program.items.iter() {
            if let Item::GlobalVar(v) = item.unwrap() {
                // Global variables cannot be inizialized (this is checked by the parser).
                assert!(v.val.is_none());
                vars.insert(ByAddress(&**v), LValue::new_for_type(v.ty.unwrap()));
            }
        }

        ProgramState {
            program,
            global_variables: Scope { variables: vars },
            local_variables: Vector::new(),
            eval_stack: Vector::new(),
            statement_stack: Vector::new(),
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
            if let Item::Fn(f) = x.unwrap() {
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

        let fargs = fun.args.iter().collect::<Vec<_>>();

        self.start_block(&fun.body, &fargs[..], args);
        Ok(())
    }

    /// Executes one evaluation step, returning None if evaluation of the top-level function is not
    /// yet done, and its return value if it is.
    pub fn eval_step(&mut self) -> Result<Option<LValue>, A> {
        assert!(!self.eval_stack.is_empty());
        match self.eval_stack.back().unwrap() {
            EvalStackElement::Statement(blk, pos) => self.eval_block_pos(blk, *pos),
            EvalStackElement::ExprLValue(expr) => self.eval_lvalue(expr),
            EvalStackElement::ExprRValue(expr) => self.eval_rvalue(expr),
        }
    }

    fn start_block(&mut self, blk: &'a Block<A>, vars: &[&'a Arc<VarDecl<A>>], values: &[LValue]) {
        assert!(vars.len() == values.len());
        self.eval_stack
            .push_back(EvalStackElement::Statement(&blk, 0));
        self.local_variables.push_back(Scope {
            variables: vars
                .iter()
                .zip(values.iter())
                .map(|(v, val)| (ByAddress(&***v), val.clone()))
                .collect(),
        });
        self.statement_stack.push_back(StatementStack::new());
    }

    fn eval_block_pos(&mut self, blk: &'a Block<A>, pos: usize) -> Result<Option<LValue>, A> {
        if pos >= blk.statements.len() {
            self.statement_stack.pop_back();
            return self.pop_stack(LValue::Void);
        }
        let (id, info) = (blk.statements[pos].id, blk.statements[pos].info.clone());

        let statement_done = |s: &mut Self| {
            if let EvalStackElement::Statement(_, ref mut pos) = s.eval_stack.back_mut().unwrap() {
                s.statement_stack.pop_back();
                *pos += 1;
                s.statement_stack.push_back(StatementStack::new());
            }
        };
        match blk.statements[pos].unwrap() {
            Statement::Comment(_) => {}
            Statement::Decl(d) => {
                let init = if let Some(val) = &d.val {
                    el!(self, val)
                } else {
                    LValue::new_for_type(&d.ty.unwrap())
                };
                self.local_variables
                    .back_mut()
                    .unwrap()
                    .variables
                    .insert(ByAddress(&**d), init);
            }
            Statement::Assign(to, expr) => {
                let val = el!(self, expr);
                let dest = er!(self, to);
                let addr = &ByAddress(dest.vardecl);
                let mut current_value =
                    if let Some(x) = self.global_variables.variables.get_mut(addr) {
                        x
                    } else {
                        self.local_variables
                            .iter_mut()
                            .rev()
                            .find_map(|x| x.variables.get_mut(addr))
                            .unwrap()
                    };
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
                let cond = el!(self, expr);
                // We are done with this statement. Clear the eval state and ensure that next
                // iteration starts from the following statement.
                statement_done(self);
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
                let cond = el!(self, expr);
                // Clear evaluation of this expression, so that it gets re-evaluated next time.
                self.statement_stack.pop_back();
                self.statement_stack.push_back(StatementStack::new());
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
                let arr = el!(self, expr);

                if let LValue::Array(a) = arr {
                    if self.statement_stack.back().unwrap().loop_pos < a.len() {
                        let val = a[self.statement_stack.back().unwrap().loop_pos].clone();
                        self.statement_stack.back_mut().unwrap().loop_pos += 1;
                        self.start_block(blk, &[var], &[val]);
                        return Ok(None);
                    }
                } else {
                    unreachable!()
                }
            }
            Statement::Expr(expr) => {
                el!(self, expr);
            }
            Statement::Return(expr) => {
                let rval = if let Some(expr) = expr {
                    el!(self, expr)
                } else {
                    LValue::Void
                };
                return self.pop_stack(rval);
            }
        }
        statement_done(self);
        Ok(None)
    }

    fn eval_lvalue(&mut self, expr: &'a Node<A, Expr<A>>) -> Result<Option<LValue>, A> {
        let (id, info) = (expr.id, expr.info.clone());
        let val = match expr.unwrap() {
            Expr::Ref(var) => {
                let var_arc = var.upgrade().unwrap();
                // TODO(veluca): no. Get rid of Arcs.
                let addr = &ByAddress(unsafe { std::mem::transmute(&*var_arc) });
                if let Some(x) = self.global_variables.variables.get(addr) {
                    x.clone()
                } else {
                    self.local_variables
                        .iter()
                        .rev()
                        .find_map(|x| x.variables.get(addr))
                        .unwrap()
                        .clone()
                }
            }
            Expr::Integer(i) => LValue::Integer(*i),
            Expr::Float(i) => LValue::Float(*i),
            Expr::Bool(i) => LValue::Bool(*i),
            Expr::String(i) => LValue::String(Arc::new(i.clone())),
            Expr::Array(a) => {
                let vals = a
                    .iter()
                    .map(|x| Ok(Some(el!(self, x))))
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
                    .map(|x| Ok(Some(el!(self, x))))
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
                    .map(|(x, y)| Ok(Some((el!(self, x), el!(self, y)))))
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
                    .map(|x| Ok(Some(el!(self, x))))
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
                    .map(|(n, x)| Ok(Some((n.name.clone(), el!(self, x)))))
                    .collect::<Result<Option<_>, A>>()?;
                if let Some(vals) = vals {
                    LValue::NamedTuple(vals)
                } else {
                    return Ok(None);
                }
            }
            Expr::Parens(a) => {
                el!(self, a)
            }
            Expr::Range(e1, e2, ty) => {
                let e1 = el!(self, e1);
                let e2 = el!(self, e2);
                if let LValue::Integer(e1) = e1 {
                    if let LValue::Integer(e2) = e2 {
                        if *ty == RangeType::HalfOpen {
                            LValue::Array((e1..e2).map(|x| LValue::Integer(x)).collect())
                        } else {
                            LValue::Array((e1..=e2).map(|x| LValue::Integer(x)).collect())
                        }
                    } else {
                        unreachable!()
                    }
                } else {
                    unreachable!()
                }
            }
            Expr::Not(e) => {
                let e = el!(self, e);
                if let LValue::Bool(e) = e {
                    LValue::Bool(!e)
                } else {
                    unreachable!()
                }
            }
            Expr::BinaryOp(e1, op, e2) => {
                let e1 = el!(self, e1);
                let e2 = el!(self, e2);
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
                let arr = el!(self, arr);
                let idx = el!(self, idx);
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
                let tpl = el!(self, tpl);
                if let LValue::Tuple(tpl) = tpl {
                    tpl[*idx].clone()
                } else {
                    unreachable!()
                }
            }
            Expr::NamedTupleField(tpl, field) => {
                let tpl = el!(self, tpl);
                if let LValue::NamedTuple(tpl) = tpl {
                    tpl.get(&field.name).unwrap().clone()
                } else {
                    unreachable!()
                }
            }
            Expr::MethodCall(_, _, _) => {
                todo!()
            }
            Expr::Output(expr) => {
                let expr = el!(self, expr);
                // TODO: prettier printing
                self.stdout.push_back(format!("{:?}", expr));
                LValue::Void
            }
            Expr::FunctionCall(f, args) => {
                let args = args
                    .iter()
                    .map(|x| Ok(Some(el!(self, x))))
                    .collect::<Result<Option<Vec<_>>, A>>()?;
                if let Some(vals) = args {
                    let f = f.upgrade().unwrap();
                    // TODO(veluca): no. Get rid of Arcs.
                    let fb = &*f;
                    let fb = unsafe { std::mem::transmute::<_, &'a FnDecl<A>>(fb) };
                    let fargs = fb.args.iter().collect::<Vec<_>>();
                    self.start_block(&fb.body, &fargs[..], &vals[..]);
                }
                return Ok(None);
            }
        };
        self.statement_stack
            .back_mut()
            .unwrap()
            .evaluated_expression_lvalues
            .insert(ByAddress(expr), val);
        self.pop_stack(LValue::Void)
    }

    fn eval_rvalue(&mut self, expr: &'a Node<A, Expr<A>>) -> Result<Option<LValue>, A> {
        let rval = match expr.unwrap() {
            Expr::Ref(var) => {
                let var_arc = var.upgrade().unwrap();
                // TODO(veluca): no. Get rid of Arcs.
                RValue {
                    vardecl: unsafe { std::mem::transmute(&*var_arc) },
                    indices: Vector::new(),
                }
            }
            Expr::ArrayIndex(aexpr, iexpr) => {
                let mut arr = er!(self, aexpr);
                let idx = el!(self, iexpr);
                arr.indices.push_back(idx);
                arr
            }
            Expr::TupleField(aexpr, idx) => {
                let mut arr = er!(self, aexpr);
                arr.indices.push_back(LValue::Integer(*idx as i64));
                arr
            }
            Expr::NamedTupleField(aexpr, field) => {
                let mut arr = er!(self, aexpr);
                arr.indices
                    .push_back(LValue::String(Arc::new(field.name.clone())));
                arr
            }
            Expr::Tuple(_) => todo!(),
            Expr::NamedTuple(_) => todo!(),
            _ => unreachable!(),
        };
        self.statement_stack
            .back_mut()
            .unwrap()
            .evaluated_expression_rvalues
            .insert(ByAddress(expr), rval);
        self.pop_stack(LValue::Void)
    }

    fn pop_stack(&mut self, lvalue: LValue) -> Result<Option<LValue>, A> {
        let last = self.eval_stack.pop_back();
        if let EvalStackElement::Statement(_, _) = last.unwrap() {
            if self.eval_stack.is_empty() {
                return Ok(Some(lvalue));
            }
            let back = self.eval_stack.back_mut().unwrap();
            match back {
                EvalStackElement::Statement(_, _) => {}
                EvalStackElement::ExprRValue(_) => {}
                EvalStackElement::ExprLValue(expr) => {
                    if let Expr::FunctionCall(f, _) = expr.unwrap() {
                        if f.upgrade().unwrap().ret.is_some() && lvalue == LValue::Void {
                            return Err(Error::DidNotReturn(expr.id, expr.info.clone()));
                        }
                        self.statement_stack
                            .back_mut()
                            .unwrap()
                            .evaluated_expression_lvalues
                            .insert(ByAddress(expr), lvalue);
                        return self.pop_stack(LValue::Void);
                    }
                }
            }
        }
        Ok(None)
    }

    pub fn stdout(&self) -> &Vector<String> {
        &self.stdout
    }
}
