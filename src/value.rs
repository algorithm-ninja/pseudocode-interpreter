use std::sync::Arc;

use im::{HashMap, OrdSet, Vector};
use ordered_float::NotNan;

use crate::ast::{Ast, Type, VarDecl};

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

impl LValue {
    pub fn new_for_type<A: Ast>(ty: &Type<A>) -> LValue {
        let ty = ty.canonical_type().unwrap();
        match ty {
            Type::Bool => LValue::Bool(false),
            Type::Integer => LValue::Integer(0),
            Type::Float => LValue::Float(NotNan::new(0.0).unwrap()),
            Type::String => LValue::String(Arc::new(String::new())),
            Type::Array(_) => LValue::Array(Vector::new()),
            Type::Set(_) => LValue::Set(OrdSet::new()),
            Type::Map(_, _) => LValue::Map(HashMap::new()),
            Type::Tuple(types) => LValue::Tuple(
                types
                    .iter()
                    .map(|t| LValue::new_for_type(t.unwrap()))
                    .collect(),
            ),

            Type::NamedTuple(names_and_types) => LValue::NamedTuple(
                names_and_types
                    .iter()
                    .map(|(n, t)| (n.name.to_owned(), LValue::new_for_type(t.unwrap())))
                    .collect(),
            ),
            _ => unreachable!("Invalid type"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct RValue<'a, A: Ast> {
    // TODO: tuple/namedtuple rvalues.
    pub vardecl: &'a VarDecl<A>,
    pub indices: Vector<LValue>,
}
