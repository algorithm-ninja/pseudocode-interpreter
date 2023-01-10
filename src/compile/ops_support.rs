use std::sync::Arc;

use ordered_float::NotNan;

use crate::{
    ast::Ast,
    eval::ProgramState,
    value::{LValue, RValue},
};

pub trait OperationInput<'a, A: Ast> {
    type InputInfo;
    fn count_lvalues(info: &Self::InputInfo) -> usize;
    fn count_rvalues(info: &Self::InputInfo) -> usize;
    fn get(info: &Self::InputInfo, state: &mut ProgramState<'a, A>) -> Self;
}

macro_rules! pop_variant {
    ($p:pat_param, $state:expr) => {
        let $p = $state.lvalues.pop().unwrap() else { unreachable!() };
    };
}

impl<'a, A: Ast> OperationInput<'a, A> for LValue {
    type InputInfo = ();
    fn count_lvalues(_: &Self::InputInfo) -> usize {
        1
    }
    fn count_rvalues(_: &Self::InputInfo) -> usize {
        0
    }
    fn get(_: &Self::InputInfo, state: &mut ProgramState<'a, A>) -> Self {
        state.lvalues.pop().unwrap()
    }
}

impl<'a, A: Ast> OperationInput<'a, A> for RValue {
    type InputInfo = ();
    fn count_lvalues(_: &Self::InputInfo) -> usize {
        0
    }
    fn count_rvalues(_: &Self::InputInfo) -> usize {
        1
    }
    fn get(_: &Self::InputInfo, state: &mut ProgramState<'a, A>) -> Self {
        state.rvalues.pop().unwrap()
    }
}

impl<'a, A: Ast> OperationInput<'a, A> for () {
    type InputInfo = ();
    fn count_lvalues(_: &Self::InputInfo) -> usize {
        0
    }
    fn count_rvalues(_: &Self::InputInfo) -> usize {
        0
    }
    fn get(_: &Self::InputInfo, _: &mut ProgramState<'a, A>) -> Self {}
}

impl<'a, A: Ast> OperationInput<'a, A> for bool {
    type InputInfo = ();
    fn count_lvalues(_: &Self::InputInfo) -> usize {
        1
    }
    fn count_rvalues(_: &Self::InputInfo) -> usize {
        0
    }
    fn get(_: &Self::InputInfo, state: &mut ProgramState<'a, A>) -> Self {
        pop_variant!(LValue::Bool(b), state);
        b
    }
}

impl<'a, A: Ast> OperationInput<'a, A> for i64 {
    type InputInfo = ();
    fn count_lvalues(_: &Self::InputInfo) -> usize {
        1
    }
    fn count_rvalues(_: &Self::InputInfo) -> usize {
        0
    }
    fn get(_: &Self::InputInfo, state: &mut ProgramState<'a, A>) -> Self {
        pop_variant!(LValue::Integer(b), state);
        b
    }
}

impl<'a, A: Ast> OperationInput<'a, A> for NotNan<f64> {
    type InputInfo = ();
    fn count_lvalues(_: &Self::InputInfo) -> usize {
        1
    }
    fn count_rvalues(_: &Self::InputInfo) -> usize {
        0
    }
    fn get(_: &Self::InputInfo, state: &mut ProgramState<'a, A>) -> Self {
        pop_variant!(LValue::Float(b), state);
        b
    }
}

impl<'a, A: Ast> OperationInput<'a, A> for Arc<String> {
    type InputInfo = ();
    fn count_lvalues(_: &Self::InputInfo) -> usize {
        1
    }
    fn count_rvalues(_: &Self::InputInfo) -> usize {
        0
    }
    fn get(_: &Self::InputInfo, state: &mut ProgramState<'a, A>) -> Self {
        pop_variant!(LValue::String(b), state);
        b
    }
}

impl<
        'a,
        A: Ast,
        T: OperationInput<'a, A, InputInfo = ()>,
        U: OperationInput<'a, A, InputInfo = ()>,
    > OperationInput<'a, A> for (T, U)
{
    type InputInfo = ();
    fn count_lvalues(_: &Self::InputInfo) -> usize {
        T::count_lvalues(&()) + U::count_lvalues(&())
    }
    fn count_rvalues(_: &Self::InputInfo) -> usize {
        T::count_rvalues(&()) + U::count_rvalues(&())
    }
    fn get(_: &Self::InputInfo, state: &mut ProgramState<'a, A>) -> Self {
        // We are popping from a stack, so pop the second argument first.
        let in1 = U::get(&(), state);
        let in0 = T::get(&(), state);
        (in0, in1)
    }
}

impl<'a, A: Ast, T: OperationInput<'a, A, InputInfo = ()>> OperationInput<'a, A> for Vec<T> {
    type InputInfo = usize;
    fn count_lvalues(info: &Self::InputInfo) -> usize {
        T::count_lvalues(&()) * info
    }
    fn count_rvalues(info: &Self::InputInfo) -> usize {
        T::count_rvalues(&()) * info
    }
    fn get(info: &Self::InputInfo, state: &mut ProgramState<'a, A>) -> Self {
        let mut vec: Vec<_> = (0..*info).map(|_| T::get(&(), state)).collect();
        vec.reverse();
        vec
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum OperationOutputType {
    LValue,
    RValue,
    Neither,
}

pub trait OperationOutput<'a, A: Ast> {
    const OUTPUT_TYPE: OperationOutputType;
    fn push(self, state: &mut ProgramState<'a, A>);
}

impl<'a, A: Ast> OperationOutput<'a, A> for () {
    const OUTPUT_TYPE: OperationOutputType = OperationOutputType::Neither;
    fn push(self, _: &mut ProgramState<'a, A>) {}
}

impl<'a, A: Ast> OperationOutput<'a, A> for LValue {
    const OUTPUT_TYPE: OperationOutputType = OperationOutputType::LValue;
    fn push(self, state: &mut ProgramState<'a, A>) {
        state.lvalues.push(self)
    }
}

impl<'a, A: Ast> OperationOutput<'a, A> for bool {
    const OUTPUT_TYPE: OperationOutputType = OperationOutputType::LValue;
    fn push(self, state: &mut ProgramState<'a, A>) {
        state.lvalues.push(LValue::Bool(self))
    }
}

impl<'a, A: Ast> OperationOutput<'a, A> for i64 {
    const OUTPUT_TYPE: OperationOutputType = OperationOutputType::LValue;
    fn push(self, state: &mut ProgramState<'a, A>) {
        state.lvalues.push(LValue::Integer(self))
    }
}

impl<'a, A: Ast> OperationOutput<'a, A> for NotNan<f64> {
    const OUTPUT_TYPE: OperationOutputType = OperationOutputType::LValue;
    fn push(self, state: &mut ProgramState<'a, A>) {
        state.lvalues.push(LValue::Float(self))
    }
}

impl<'a, A: Ast> OperationOutput<'a, A> for Arc<String> {
    const OUTPUT_TYPE: OperationOutputType = OperationOutputType::LValue;
    fn push(self, state: &mut ProgramState<'a, A>) {
        state.lvalues.push(LValue::String(self))
    }
}

impl<'a, A: Ast> OperationOutput<'a, A> for RValue {
    const OUTPUT_TYPE: OperationOutputType = OperationOutputType::RValue;
    fn push(self, state: &mut ProgramState<'a, A>) {
        state.rvalues.push(self)
    }
}

impl<'a, A: Ast> OperationOutput<'a, A> for String {
    const OUTPUT_TYPE: OperationOutputType = OperationOutputType::Neither;
    fn push(self, state: &mut ProgramState<'a, A>) {
        state.stdout.push(self);
    }
}

impl<'a, A: Ast, T: OperationOutput<'a, A>, U: OperationOutput<'a, A>> OperationOutput<'a, A>
    for (T, U)
{
    const OUTPUT_TYPE: OperationOutputType =
        if matches!(T::OUTPUT_TYPE, OperationOutputType::Neither) {
            U::OUTPUT_TYPE
        } else if matches!(U::OUTPUT_TYPE, OperationOutputType::Neither) {
            T::OUTPUT_TYPE
        } else {
            panic!("Cannot return multiple L or R values")
        };

    fn push(self, state: &mut ProgramState<'a, A>) {
        let (t, u) = self;
        t.push(state);
        u.push(state);
    }
}
