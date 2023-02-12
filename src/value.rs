use std::{fmt::Display, sync::Arc};

use im::{HashMap, OrdSet, Vector};
use itertools::Itertools;
use ordered_float::NotNan;

use crate::ast::{Ast, Type, VarIndex};

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
    NamedTuple(Vector<LValue>),
    // Represents no value. It is either an uninitialized stack entry, or the return value of a
    // top-level function.
    Void,
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
                    .map(|(_, t)| LValue::new_for_type(t.unwrap()))
                    .collect(),
            ),
            _ => unreachable!("Invalid type"),
        }
    }
}

impl Default for LValue {
    fn default() -> Self {
        LValue::Void
    }
}

impl Display for LValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LValue::Integer(value) => write!(f, "{value}"),
            LValue::Float(value) => write!(f, "{value}"),
            LValue::String(value) => write!(f, "{value}"),
            LValue::Bool(value) => write!(f, "{value}"),
            LValue::Array(value) => {
                write!(f, "[{}]", value.iter().map(|x| x.to_string()).join(","))
            }
            LValue::Set(value) => {
                write!(f, "{{{}}}", value.iter().map(|x| x.to_string()).join(","))
            }
            LValue::Map(value) => {
                write!(
                    f,
                    "{{{}}}",
                    value.iter().map(|(k, v)| format!("{k}->{v}")).join(",")
                )
            }
            LValue::Tuple(value) | LValue::NamedTuple(value) => {
                write!(f, "({})", value.iter().map(|x| x.to_string()).join(","))
            }
            LValue::Void => write!(f, "Void"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct RValueEntry {
    pub lstack_pos: (usize, VarIndex),
    // TODO(veluca): consider using a small-vector.
    pub indices: Vec<i64>,
}

#[derive(Clone, Debug)]
pub enum RValue {
    Single(RValueEntry),
    Tuple(Vec<RValue>),
}

impl RValue {
    pub fn new(lstack_pos: (usize, VarIndex), indices: Vec<i64>) -> Self {
        Self::Single(RValueEntry {
            lstack_pos,
            indices,
        })
    }
}
