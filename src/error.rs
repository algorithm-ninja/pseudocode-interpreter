use thiserror::Error;

use crate::{
    ast::{Ast, BinaryOp, Ident, Type, UnaryOp},
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
    #[error("internal parse error: {0}")]
    PlaceholderParseError(String),
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
    #[error("error: cannot access field of temporary tuple at {0:?} ({1:?})")]
    TemporaryTupleAccess(usize, A::NodeInfo),
    #[error("error: cannot build nested temporary tuple at {0:?} ({1:?})")]
    NestedTemporaryTuple(usize, A::NodeInfo),
    #[error("missing node: {0:?} ({1:?})")]
    MissingNode(usize, A::NodeInfo),
    #[error("type error: node {0:?} ({1:?}) has an unexpected type {2:?} (expected: {3:?})")]
    TypeError(usize, A::NodeInfo, Type<A>, Vec<Type<A>>),
    #[error("type error: return statement with a value {0:?} ({1:?}) where none was expected")]
    ReturnHasValue(usize, A::NodeInfo),
    #[error("type error: return statement without a value {0:?} ({1:?}) where one was expected")]
    ReturnNoValue(usize, A::NodeInfo),
    #[error("type error: expression {0:?} ({1:?}) cannot be assigned to")]
    NotAssignable(usize, A::NodeInfo),
    #[error("type error: expected array at {0:?} ({1:?})")]
    ExpectedArray(usize, A::NodeInfo),
    #[error("type error: expected tuple at {0:?} ({1:?})")]
    ExpectedTuple(usize, A::NodeInfo),
    #[error("type error: expected named tuple at {0:?} ({1:?})")]
    ExpectedNamedTuple(usize, A::NodeInfo),
    #[error("type error: invalid tuple field {2} at {0:?} ({1:?})")]
    InvalidTupleField(usize, A::NodeInfo, usize),
    #[error("type error: invalid tuple field {2} at {0:?} ({1:?})")]
    InvalidNamedTupleField(usize, A::NodeInfo, String),
    #[error("type error: invalid number of arguments when calling {2:?} at {0:?} ({1:?})")]
    WrongArgumentNumber(usize, A::NodeInfo, Ident<A>),
    #[error("runtime error: function {0:?} did not return)")]
    DidNotReturn(Ident<A>),
    #[error("runtime error: array out of bounds at {0:?} ({1:?}) - index is {2}, len is {3}")]
    ArrayOutOfBounds(usize, A::NodeInfo, i64, usize),
    #[error("runtime error: result of {3} {2:?} {4} would overflow at {0:?} ({1:?})")]
    Overflow(usize, A::NodeInfo, BinaryOp, i64, i64),
    #[error("runtime error: result of {2:?} {3} would overflow at {0:?} ({1:?})")]
    UnaryOverflow(usize, A::NodeInfo, UnaryOp, i64),
    #[error("runtime error: division by zero at {0:?} ({1:?})")]
    DivisionByZero(usize, A::NodeInfo),
    #[error("runtime error: cannot repeat {2:?} times at {0:?} ({1:?})")]
    RepeatNegativeAmount(usize, A::NodeInfo, i64),
    #[error("runtime error: next_string called at {0:?} ({1:?}) with no input left")]
    NextStringFailed(usize, A::NodeInfo),
    #[error("runtime error: next_int called at {0:?} ({1:?}) with no input left")]
    NextIntFailed(usize, A::NodeInfo),
    #[error(
        "runtime error: next_int called at {0:?} ({1:?}) but no integer could be parsed from {2:?}"
    )]
    NextIntParsingFailed(usize, A::NodeInfo, String),
}

impl<A: Ast> Error<A> {
    pub fn get_error_location(&self) -> A::NodeInfo {
        match self {
            Error::ParseError(_, _, n) => n,
            Error::GenericParseError(_, _, n) => n,
            Error::PlaceholderParseError(_) => {
                panic!("A PlaceholderParseError should never be returned")
            }
            Error::UnrecognizedToken(_, n) => n,
            Error::UnrecognizedVariable(Ident { name: _, info: n }) => n,
            Error::UnrecognizedFunction(Ident { name: _, info: n }) => n,
            Error::UnrecognizedType(Ident { name: _, info: n }) => n,
            Error::DuplicateVariable(Ident { name: _, info: n }, _) => n,
            Error::DuplicateFunction(Ident { name: _, info: n }, _) => n,
            Error::DuplicateType(Ident { name: _, info: n }, _) => n,
            Error::TemporaryTupleAccess(_, n) => n,
            Error::NestedTemporaryTuple(_, n) => n,
            Error::MissingNode(_, n) => n,
            Error::TypeError(_, n, _, _) => n,
            Error::ReturnNoValue(_, n) => n,
            Error::ReturnHasValue(_, n) => n,
            Error::NotAssignable(_, n) => n,
            Error::ExpectedArray(_, n) => n,
            Error::ExpectedTuple(_, n) => n,
            Error::ExpectedNamedTuple(_, n) => n,
            Error::InvalidTupleField(_, n, _) => n,
            Error::InvalidNamedTupleField(_, n, _) => n,
            Error::WrongArgumentNumber(_, n, _) => n,
            Error::DidNotReturn(Ident { name: _, info: n }) => n,
            Error::ArrayOutOfBounds(_, n, _, _) => n,
            Error::Overflow(_, n, _, _, _) => n,
            Error::UnaryOverflow(_, n, _, _) => n,
            Error::DivisionByZero(_, n) => n,
            Error::RepeatNegativeAmount(_, n, _) => n,
            Error::NextStringFailed(_, n) => n,
            Error::NextIntFailed(_, n) => n,
            Error::NextIntParsingFailed(_, n, _) => n,
        }
        .clone()
    }
}
