use std::{
    collections::HashMap,
    fmt::{Debug, Formatter},
    rc::Rc,
};

use crate::{
    ast::{Ast, Expr, FnIndex, Ident, Item, Node, Program, VarIndex},
    error::Error,
    value::{LValue, RValue},
};

pub type Result<T, A> = std::result::Result<T, Error<A>>;

pub type Ip = usize;

type InstructionFn<'a, A> = Box<dyn Fn(&mut ProgramState<'a, A>) -> Result<Option<Ip>, A> + 'a>;

pub struct Instruction<'a, A: Ast> {
    run: InstructionFn<'a, A>,
}

impl<'a, A: Ast> Instruction<'a, A> {
    pub fn new<F>(f: F) -> Instruction<'a, A>
    where
        F: Fn(&mut ProgramState<'a, A>) -> Result<Option<Ip>, A> + 'a,
    {
        Instruction { run: Box::new(f) }
    }
}

impl<'a, A: Ast> Debug for Instruction<'a, A> {
    fn fmt(&self, _: &mut Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        // TODO(veluca): consider actually printing something.
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct ProgramState<'a, A: Ast> {
    pub ip: Vec<Ip>,
    pub lvalues: Vec<LValue>,
    pub rvalues: Vec<RValue>,
    pub stdin: Vec<String>,
    pub stdin_pos: usize,
    pub stdout: Vec<String>,
    program: Rc<CompiledProgram<'a, A>>,
}

#[derive(Debug)]
pub enum ExprValue {
    LValue(LValue),
    RValue(RValue),
}

#[derive(Debug)]
pub struct StackFrame<'a, A: Ast> {
    pub fun: Option<&'a Node<A, Item<A>>>,
    pub current_expr: Option<&'a Node<A, Expr<A>>>,
    pub variables: HashMap<VarIndex, LValue>,
    pub temporaries: HashMap<&'a Node<A, Expr<A>>, ExprValue>,
}

#[derive(Debug)]
pub enum TemporaryIndex {
    LValue(usize),
    RValue(usize),
}

#[derive(Debug)]
pub struct DebugInfo<'a, A: Ast> {
    pub fun: Option<&'a Node<A, Item<A>>>,
    pub current_expr: Option<&'a Node<A, Expr<A>>>,
    pub variables: HashMap<VarIndex, usize>,
    pub temporaries: HashMap<&'a Node<A, Expr<A>>, TemporaryIndex>,
}

impl<'a, A: Ast> DebugInfo<'a, A> {
    pub fn num_lstack(&self) -> usize {
        self.variables.len()
            + self
                .temporaries
                .iter()
                .filter(|(_, x)| matches!(x, TemporaryIndex::LValue(_)))
                .count()
    }

    pub fn num_rstack(&self) -> usize {
        self.temporaries.len() + self.variables.len() - self.num_lstack()
    }
}

#[derive(Debug)]
pub struct CompiledProgram<'a, A: Ast> {
    pub instructions: Vec<Instruction<'a, A>>,
    pub ini_entry_point: usize,
    pub fn_entry_point: HashMap<FnIndex, usize>,
    pub instruction_debug_info: Vec<DebugInfo<'a, A>>,
    pub global_vars_debug_info: DebugInfo<'a, A>,
    pub ast: &'a Program<A>,
}

impl<'a, A: Ast> ProgramState<'a, A> {
    pub fn new(program: Rc<CompiledProgram<'a, A>>, input: &str) -> Result<ProgramState<'a, A>, A> {
        let input_tokens = input
            .split([' ', '\t', '\n', '\r'])
            .filter(|x| !x.is_empty())
            .map(|x| x.to_owned())
            .collect();

        let mut program_state = ProgramState {
            ip: vec![],
            lvalues: vec![],
            rvalues: vec![],
            stdin: input_tokens,
            stdin_pos: 0,
            stdout: vec![],
            program: program.clone(),
        };

        // Set up things to run initialization.
        program_state.ip.push(program.ini_entry_point);

        Ok(program_state)
    }

    /// Returns true if eval is done.
    pub fn eval_step(&mut self) -> Result<bool, A> {
        self.eval_step_impl(&self.program.clone())
    }

    fn eval_step_impl(&mut self, program: &Rc<CompiledProgram<'a, A>>) -> Result<bool, A> {
        let ip = self.ip.pop();
        let Some(ip) = ip else {
            return Ok(true);
        };
        let new_ip = (program.instructions[ip].run)(self)?;
        if let Some(new_ip) = new_ip {
            self.ip.push(new_ip);
        }
        Ok(false)
    }

    /// Starts evaluation of the given function on the given arguments. Will error out if already
    /// evaluating a function, or global initialization is not done.
    pub fn evaluate_fun(&mut self, fun: &str, args: &[LValue]) -> Result<(), A> {
        assert!(self.ip.is_empty());

        let fname = fun;
        let Some(idx) = self.program.ast.items.iter().find_map(|x| {
            if let Item::Fn(idx) = *x.unwrap() {
                let f = self.program.ast.fun(idx);
                if f.ident.name == fun {
                    return Some(idx);
                }
            }
            None
        }) else {
            return Err(Error::UnrecognizedFunction(Ident {
                name: fname.to_owned(),
                info: A::NodeInfo::default(),
            }));

        };

        let fun = self.program.ast.fun(idx);

        if !args.is_empty() || !fun.args.is_empty() || fun.ret.is_some() {
            unimplemented!();
        }

        self.ip
            .push(*self.program.fn_entry_point.get(&idx).unwrap());
        Ok(())
    }

    /// Returns the return value of the top-level function that has just finished evaluating.
    pub fn get_fun_return(&mut self) -> LValue {
        unimplemented!()
    }

    pub fn stdout(&self) -> &Vec<String> {
        &self.stdout
    }

    pub fn stack_frames(&self) -> Vec<StackFrame<'a, A>> {
        let mut num_lvalues = self.lvalues.len();
        let mut num_rvalues = self.rvalues.len();
        let mut ret = vec![];

        let get_lvalue = |is_leaf: bool, dinfo_offset: usize, num_lvalues: usize| {
            if !is_leaf && dinfo_offset == 0 {
                return None;
            }
            let index = if is_leaf {
                dinfo_offset
            } else {
                dinfo_offset - 1
            };
            Some(self.lvalues[num_lvalues - index - 1].clone())
        };

        let mut frame_from_deb_info = |debug_info: &DebugInfo<'a, A>, is_leaf: bool| {
            let variables: HashMap<_, _> = debug_info
                .variables
                .iter()
                .filter_map(|(k, v)| get_lvalue(is_leaf, *v, num_lvalues).map(|x| (*k, x)))
                .collect();

            let temporaries: HashMap<_, _> = debug_info
                .temporaries
                .iter()
                .filter_map(|(k, v)| match v {
                    TemporaryIndex::LValue(lv) => {
                        get_lvalue(is_leaf, *lv, num_lvalues).map(|x| (*k, ExprValue::LValue(x)))
                    }
                    TemporaryIndex::RValue(rv) => Some((
                        *k,
                        ExprValue::RValue(self.rvalues[num_rvalues - rv - 1].clone()),
                    )),
                })
                .collect();

            let frame = StackFrame {
                fun: debug_info.fun,
                variables,
                temporaries,
                current_expr: if is_leaf {
                    None
                } else {
                    debug_info.current_expr
                },
            };
            // Anything not at the bottom of the stack has an extra entry for the return value of
            // the callee.
            num_lvalues = num_lvalues
                .checked_sub(debug_info.num_lstack() - !is_leaf as usize)
                .unwrap();
            num_rvalues = num_rvalues.checked_sub(debug_info.num_rstack()).unwrap();
            frame
        };

        for (idx, ip) in self.ip.iter().rev().enumerate() {
            let debug_info = &self.program.instruction_debug_info[*ip];
            ret.push(frame_from_deb_info(debug_info, idx == 0));
        }

        ret.push(frame_from_deb_info(
            &self.program.global_vars_debug_info,
            true,
        ));

        assert!(num_rvalues == 0);
        assert!(num_lvalues == 0);

        // Return stack frames top to bottom.
        ret.reverse();
        ret
    }
}
