use thiserror::Error;

use crate::{
    ast::{Ast, Ident, Type},
    parse::Token,
};

#[derive(Error, Debug)]
pub enum Error<A: Ast> {
    #[error("parse error: expected one of {0:?}, found {1:?} at position {2:?}")]
    ParseError(Vec<Token>, String, A::NodeInfo),
    #[error("parse error: {1:?} at position {2:?} is invalid, {0}")]
    GenericParseError(String, String, A::NodeInfo),
    #[error("parse error: unrecognized token {0} at position {1:?}")]
    UnrecognizedToken(String, A::NodeInfo),
    #[error("error: unrecognized variable {0:?}")]
    UnrecognizedVariable(Ident<A>),
    #[error("error: unrecognized function {0:?}")]
    UnrecognizedFunction(Ident<A>),
    #[error("error: unrecognized type {0:?}")]
    UnrecognizedType(Ident<A>),
    #[error("error: duplicate variable {0:?}, previous definition is at {1:?}")]
    DuplicateVariable(Ident<A>, Ident<A>),
    #[error("error: duplicate function {0:?}, previous definition is at {1:?}")]
    DuplicateFunction(Ident<A>, Ident<A>),
    #[error("error: duplicate type {0:?}, previous definition is at {1:?}")]
    DuplicateType(Ident<A>, Ident<A>),
    #[error("missing node: {0:?} ({1:?})")]
    MissingNode(usize, A::NodeInfo),
    #[error("type error: node {0:?} ({1:?}) has an unexpected type {2:?} (expected: {3:?})")]
    TypeError(usize, A::NodeInfo, Type<A>, Vec<Type<A>>),
    #[error("type error: return statement with a value {0:?} ({1:?}) where none was expected")]
    ReturnHasValueError(usize, A::NodeInfo),
    #[error("type error: return statement without a value {0:?} ({1:?}) where one was expected")]
    ReturnNoValueError(usize, A::NodeInfo),
}

impl<A: Ast> Error<A> {
    pub fn get_error_location(&self) -> A::NodeInfo {
        match self {
            Error::ParseError(_, _, n) => n,
            Error::GenericParseError(_, _, n) => n,
            Error::UnrecognizedToken(_, n) => n,
            Error::UnrecognizedVariable(Ident { name: _, info: n }) => n,
            Error::UnrecognizedFunction(Ident { name: _, info: n }) => n,
            Error::UnrecognizedType(Ident { name: _, info: n }) => n,
            Error::DuplicateVariable(Ident { name: _, info: n }, _) => n,
            Error::DuplicateFunction(Ident { name: _, info: n }, _) => n,
            Error::DuplicateType(Ident { name: _, info: n }, _) => n,
            Error::MissingNode(_, n) => n,
            Error::TypeError(_, n, _, _) => n,
            Error::ReturnNoValueError(_, n) => n,
            Error::ReturnHasValueError(_, n) => n,
        }
        .clone()
    }
}
