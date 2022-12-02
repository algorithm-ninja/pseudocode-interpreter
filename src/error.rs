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
    #[error("type error: node {0:?} ({1:?}) has an unexpected type (expected: {2:?})")]
    TypeError(usize, A::NodeInfo, Vec<Type<A>>),
    #[error("type error: return statement with a value {0:?} ({1:?}) where none was expected")]
    ReturnHasValueError(usize, A::NodeInfo),
    #[error("type error: return statement without a value {0:?} ({1:?}) where one was expected")]
    ReturnNoValueError(usize, A::NodeInfo),
}
