use by_address::ByAddress;
use im::{HashMap, Vector};

use crate::{
    ast::{Ast, Block, Expr, Node, Program, VarDecl},
    error::Error,
    value::{LValue, RValue},
};

#[derive(Clone, Debug)]
struct Scope<'a, A: Ast> {
    variables: HashMap<ByAddress<&'a VarDecl<A>>, LValue>,
}

#[derive(Clone, Debug)]
enum EvalStackElement<'a, A: Ast> {
    Expr(&'a Node<A, Expr<A>>),
    Statement(&'a Block<A>, usize),
}

/// Copying a ProgramState is cheap; to keep track of past states, just clone the instance before
/// calling a mutating method.
#[derive(Clone, Debug)]
pub struct ProgramState<'a, A: Ast> {
    program: &'a Program<A>,
    global_variables: Scope<'a, A>,
    local_variables: Vector<Scope<'a, A>>,
    evaluated_expression_lvalues: HashMap<ByAddress<&'a Node<A, Expr<A>>>, LValue>,
    evaluated_expression_rvalues: HashMap<ByAddress<&'a Node<A, Expr<A>>>, RValue<'a, A>>,
    eval_stack: Vector<EvalStackElement<'a, A>>,
}

pub type Result<T, A> = std::result::Result<T, Error<A>>;

impl<'a, A: Ast> ProgramState<'a, A> {
    /// Starts a program from scratch, inizializing global variables to their default values.
    pub fn new(program: &'a Program<A>) -> ProgramState<'a, A> {
        todo!()
    }

    /// Starts evaluation of the given function on the given arguments. Will error out if already
    /// evaluating a function.
    pub fn evaluate_fun(&mut self, fun: &str, args: &[LValue]) -> Result<(), A> {
        todo!()
    }

    /// Executes one evaluation step, returning None if evaluation of the top-level function is not
    /// yet done, and its return value if it is.
    pub fn eval_step(&mut self) -> Result<Option<LValue>, A> {
        todo!()
    }
}
