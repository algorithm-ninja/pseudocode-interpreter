use std::sync::Arc;

use im::{HashMap, OrdSet, Vector};
use ordered_float::NotNan;

use crate::ast::{Ast, VarDecl};

#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum LValue {
    Integer(i64),
    Float(NotNan<f64>),
    String(Arc<String>),
    Bool(bool),
    Array(Vector<LValue>),
    Set(OrdSet<LValue>),
    Map(HashMap<LValue, LValue>),
    Tuple(Vector<LValue>),
    NamedTuple(HashMap<String, LValue>),
    Void, // Represents no value.
}

#[derive(Clone, Debug)]
pub struct RValue<'a, A: Ast> {
    pub vardecl: &'a VarDecl<A>,
    pub indices: Vector<LValue>,
}
