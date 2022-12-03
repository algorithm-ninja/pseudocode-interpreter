use std::{cell::RefCell, fmt::Debug, sync::Arc};

use crate::error::Error;

pub trait AstNode<T> {
    fn get(&self) -> Option<&T>;
    fn new(val: T) -> Self;
}

pub trait Ast: Debug + Clone {
    type NodeWrapper<T: Debug + Clone + AstNode<T>>: Debug + Clone + AstNode<T>;
    type NodeInfo: Debug + Clone + Default;
}

#[derive(Debug, Clone)]
pub struct Ident<A: Ast> {
    pub name: String,
    pub info: A::NodeInfo,
}

#[derive(Debug, Clone)]
pub struct Node<A: Ast, T: Debug + AstNode<T> + Clone> {
    pub id: usize,
    pub info: A::NodeInfo,
    pub contents: A::NodeWrapper<T>,
}

impl<A: Ast, T: Debug + AstNode<T> + Clone> Node<A, T> {
    pub fn get_contents(&self) -> Result<&T, Error<A>> {
        self.contents
            .get()
            .ok_or(Error::MissingNode(self.id, self.info.clone()))
    }
    pub fn new_with_defaults(contents: T) -> Node<A, T> {
        Self::new(contents, 0, A::NodeInfo::default())
    }
    pub fn new(contents: T, id: usize, info: A::NodeInfo) -> Node<A, T> {
        Node {
            id,
            info,
            contents: A::NodeWrapper::<T>::new(contents),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Sum,
    Sub,
    Mul,
    Div,
    Mod,
    Le,
    Lt,
    Ge,
    Gt,
    Eq,
    Ne,
    And,
    Or,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RangeType {
    HalfOpen,
    Closed,
}

type TypeNode<A> = Node<A, Type<A>>;

// Valid types for a variable or expression to have.
#[derive(Debug, Clone)]
pub enum Type<A: Ast> {
    Integer,
    Float,
    String,
    Bool,
    Array(Box<TypeNode<A>>),
    Set(Box<TypeNode<A>>),
    Map(Box<TypeNode<A>>, Box<TypeNode<A>>),
    Tuple(Vec<TypeNode<A>>),
    NamedTuple(Vec<(Ident<A>, TypeNode<A>)>),
    NamedType(Arc<TypeDecl<A>>),
    Void, // TODO(veluca): is this the best way to handle non-values?
}

impl<A: Ast> Type<A> {
    pub fn canonical_type(&self) -> Result<&Type<A>, Error<A>> {
        if let Type::NamedType(t) = self {
            t.ty.get_contents()?.canonical_type()
        } else {
            Ok(self)
        }
    }

    pub fn is_same(&self, other: &Type<A>) -> Result<bool, Error<A>> {
        if let Type::NamedType(t) = self {
            return t.ty.get_contents()?.is_same(other);
        }
        if let Type::NamedType(t) = other {
            return t.ty.get_contents()?.is_same(self);
        }
        match (self, other) {
            (Type::Integer, Type::Integer) => Ok(true),
            (Type::Float, Type::Float) => Ok(true),
            (Type::String, Type::String) => Ok(true),
            (Type::Bool, Type::Bool) => Ok(true),
            (Type::Array(x), Type::Array(y)) => x.get_contents()?.is_same(y.get_contents()?),
            (Type::Set(x), Type::Set(y)) => x.get_contents()?.is_same(y.get_contents()?),
            (Type::Map(x1, x2), Type::Map(y1, y2)) => x1
                .get_contents()?
                .is_same(y1.get_contents()?)
                .and_then(|b| {
                    if !b {
                        Ok(b)
                    } else {
                        x2.get_contents()?.is_same(y2.get_contents()?)
                    }
                }),
            (Type::Tuple(x), Type::Tuple(y)) => {
                if x.len() != y.len() {
                    return Ok(false);
                }
                x.iter()
                    .zip(y.iter())
                    .map(|(x, y)| x.get_contents()?.is_same(y.get_contents()?))
                    .try_fold(true, |b, r| Ok(b && r?))
            }
            (Type::NamedTuple(x), Type::NamedTuple(y)) => {
                if x.len() != y.len() {
                    return Ok(false);
                }
                x.iter()
                    .zip(y.iter())
                    .map(|(x, y)| {
                        if x.0.name != y.0.name {
                            Ok(false)
                        } else {
                            x.1.get_contents()?.is_same(y.1.get_contents()?)
                        }
                    })
                    .try_fold(true, |b, r| Ok(b && r?))
            }
            _ => Ok(false),
        }
    }
}

type ExprNode<A> = Node<A, Expr<A>>;

#[derive(Debug, Clone)]
pub enum Expr<A: Ast> {
    Ref(Arc<VarDecl<A>>),
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Array(Vec<ExprNode<A>>),
    Set(Vec<ExprNode<A>>),
    Map(Vec<(ExprNode<A>, ExprNode<A>)>),
    Tuple(Vec<ExprNode<A>>),
    NamedTuple(Vec<(Ident<A>, ExprNode<A>)>),
    Range(Box<ExprNode<A>>, Box<ExprNode<A>>, RangeType),
    Parens(Box<ExprNode<A>>),
    BinaryOp(Box<ExprNode<A>>, BinaryOp, Box<ExprNode<A>>),
    Not(Box<ExprNode<A>>),
    ArrayIndex(Box<ExprNode<A>>, Box<ExprNode<A>>),
    FunctionCall(Arc<RefCell<FnDecl<A>>>, Vec<ExprNode<A>>),
    MethodCall(Box<ExprNode<A>>, Ident<A>, Vec<ExprNode<A>>),
    Output(Box<ExprNode<A>>),
    TupleField(Box<ExprNode<A>>, usize),
    NamedTupleField(Box<ExprNode<A>>, Ident<A>),
}

#[derive(Debug, Clone)]
pub enum Statement<A: Ast> {
    Decl(Arc<VarDecl<A>>),
    Comment(String),
    Assign(ExprNode<A>, ExprNode<A>),
    If(ExprNode<A>, Block<A>, Block<A>),
    While(ExprNode<A>, Block<A>),
    For(Arc<VarDecl<A>>, ExprNode<A>, Block<A>),
    Return(Option<ExprNode<A>>),
    Expr(ExprNode<A>),
}

#[derive(Debug, Clone)]
pub enum Item<A: Ast> {
    Comment(String),
    GlobalVar(Arc<VarDecl<A>>),
    Fn(Arc<RefCell<FnDecl<A>>>), // TODO(veluca): find a better way.
    Type(Arc<TypeDecl<A>>),
}

// Declaration of a type alias.
#[derive(Debug)]
pub struct TypeDecl<A: Ast> {
    pub ident: Ident<A>,
    pub ty: TypeNode<A>,
}

#[derive(Debug, Clone)]
pub struct Block<A: Ast> {
    pub statements: Vec<Node<A, Statement<A>>>,
}

#[derive(Debug)]
pub struct VarDecl<A: Ast> {
    pub ident: Ident<A>,
    pub ty: TypeNode<A>,
    pub val: Option<ExprNode<A>>,
}

#[derive(Debug)]
pub struct FnDecl<A: Ast> {
    pub ident: Ident<A>,
    pub args: Vec<Arc<VarDecl<A>>>,
    pub ret: Option<TypeNode<A>>,
    pub body: Block<A>,
}

#[derive(Debug)]
pub struct Program<A: Ast> {
    pub items: Vec<Node<A, Item<A>>>,
}

impl<A: Ast> AstNode<Type<A>> for Type<A> {
    fn get(&self) -> Option<&Type<A>> {
        Some(self)
    }
    fn new(t: Self) -> Self {
        t
    }
}

impl<A: Ast> AstNode<Expr<A>> for Expr<A> {
    fn get(&self) -> Option<&Expr<A>> {
        Some(self)
    }
    fn new(t: Self) -> Self {
        t
    }
}

impl<A: Ast> AstNode<Statement<A>> for Statement<A> {
    fn get(&self) -> Option<&Statement<A>> {
        Some(self)
    }
    fn new(t: Self) -> Self {
        t
    }
}

impl<A: Ast> AstNode<Item<A>> for Item<A> {
    fn get(&self) -> Option<&Item<A>> {
        Some(self)
    }
    fn new(t: Self) -> Self {
        t
    }
}

impl<T: AstNode<T>> AstNode<T> for Option<T> {
    fn get(&self) -> Option<&T> {
        self.as_ref().and_then(|f| f.get())
    }
    fn new(t: T) -> Self {
        Some(t)
    }
}
