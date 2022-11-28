use std::fmt::Debug;
use std::ops::Range;

pub use crate::ast::*;
pub use crate::error::Error;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TextAst;

impl Ast for TextAst {
    type NodeWrapper<T: Debug + GetNode<T>> = T;
    type NodeInfo = Range<usize>;
}

pub type Result<T> = std::result::Result<T, Error<TextAst>>;
