use std::{
    collections::HashMap,
    fmt::{Debug, Formatter},
    rc::Rc,
};

use crate::{
    ast::{Ast, FnIndex, Ident, Item, Program},
    error::Error,
    value::{LValue, RValue},
};

pub type Result<T, A> = std::result::Result<T, Error<A>>;

pub type Ip = usize;

#[derive(Clone, Debug)]
pub struct ProgramState<'a, A: Ast> {
    pub ip: Vec<Ip>,
    pub lvalues: Vec<LValue>,
    pub rvalues: Vec<RValue>,
    pub stdout: Vec<String>,
    program: Rc<CompiledProgram<'a, A>>,
}

type InstructionFn<'a, A> = Box<dyn Fn(&mut ProgramState<'a, A>) -> Result<Option<Ip>, A> + 'a>;

pub struct Instruction<'a, A: Ast> {
    run: InstructionFn<'a, A>,
}

impl<'a, A: Ast> Instruction<'a, A> {
    pub fn placeholder() -> Instruction<'a, A> {
        Instruction {
            run: Box::new(|_| unreachable!("unreplaced placeholder")),
        }
    }
    pub fn set<F>(&mut self, f: F)
    where
        F: Fn(&mut ProgramState<'a, A>) -> Result<Option<Ip>, A> + 'a,
    {
        self.run = Box::new(f);
    }
}

impl<'a, A: Ast> Debug for Instruction<'a, A> {
    fn fmt(&self, _: &mut Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        // TODO(veluca): consider actually printing something.
        Ok(())
    }
}

#[derive(Debug)]
pub struct CompiledProgram<'a, A: Ast> {
    pub instructions: Vec<Instruction<'a, A>>,
    pub ini_entry_point: usize,
    pub fn_entry_point: HashMap<FnIndex, usize>,
    pub ast: &'a Program<A>,
}

impl<'a, A: Ast> ProgramState<'a, A> {
    pub fn new(program: Rc<CompiledProgram<'a, A>>) -> Result<ProgramState<'a, A>, A> {
        let mut program_state = ProgramState {
            ip: vec![],
            lvalues: vec![],
            rvalues: vec![],
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
}
