use std::{cell::RefCell, fmt::Debug, rc::Rc};

use crate::error::Error;

pub trait GetNode<T> {
    fn get(&self) -> Option<&T>;
}

pub trait Ast: Debug + Clone {
    type NodeWrapper<T: Debug + GetNode<T>>: Debug + GetNode<T>;
    type NodeInfo: Debug + Clone;
}

#[derive(Debug, Clone)]
pub struct Ident<A: Ast> {
    pub name: String,
    pub info: A::NodeInfo,
}

#[derive(Debug)]
pub struct Node<A: Ast, T: Debug + GetNode<T>> {
    pub id: usize,
    pub info: A::NodeInfo,
    pub contents: A::NodeWrapper<T>,
}

impl<A: Ast, T: Debug + GetNode<T>> Node<A, T> {
    fn get_contents(&self) -> Result<&T, Error<A>> {
        self.contents
            .get()
            .ok_or_else(|| Error::MissingNode(self.id))
    }
}

#[derive(Debug)]
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

impl BinaryOp {
    pub fn precedence(&self) -> usize {
        match self {
            BinaryOp::And | BinaryOp::Or => 0,
            BinaryOp::Ne
            | BinaryOp::Eq
            | BinaryOp::Ge
            | BinaryOp::Gt
            | BinaryOp::Le
            | BinaryOp::Lt => 1,
            BinaryOp::Sub | BinaryOp::Sum => 2,
            BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 3,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum RangeType {
    HalfOpen,
    Closed,
}

// Valid types for a variable or expression to have.
#[derive(Debug)]
pub enum Type<A: Ast> {
    Integer,
    Float,
    String,
    Bool,
    Array(Box<Node<A, Type<A>>>),
    Set(Box<Node<A, Type<A>>>),
    Map(Box<Node<A, Type<A>>>, Box<Node<A, Type<A>>>),
    Tuple(Vec<Node<A, Type<A>>>),
    NamedTuple(Vec<(Ident<A>, Node<A, Type<A>>)>),
    NamedType(Rc<TypeDecl<A>>),
}

impl<A: Ast> Type<A> {
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

#[derive(Debug)]
pub enum Expr<A: Ast> {
    Ref(Rc<VarDecl<A>>),
    Integer(i64),
    Float(f64),
    String(String),
    Array(Vec<Node<A, Expr<A>>>),
    Set(Vec<Node<A, Expr<A>>>),
    Map(Vec<(Node<A, Expr<A>>, Node<A, Expr<A>>)>),
    Tuple(Vec<Node<A, Expr<A>>>),
    Range(Box<Node<A, Expr<A>>>, Box<Node<A, Expr<A>>>, RangeType),
    Parens(Box<Node<A, Expr<A>>>),
    BinaryOp(Box<Node<A, Expr<A>>>, BinaryOp, Box<Node<A, Expr<A>>>),
    Not(Box<Node<A, Expr<A>>>),
    ArrayIndex(Box<Node<A, Expr<A>>>, Box<Node<A, Expr<A>>>),
    FunctionCall(Rc<RefCell<FnDecl<A>>>, Vec<Node<A, Expr<A>>>),
    MethodCall(Box<Node<A, Expr<A>>>, Ident<A>, Vec<Node<A, Expr<A>>>),
    Output(Box<Node<A, Expr<A>>>),
    TupleField(Box<Node<A, Expr<A>>>, usize),
    NamedTupleField(Box<Node<A, Expr<A>>>, Ident<A>),
}

#[derive(Debug)]
pub enum Statement<A: Ast> {
    Decl(Rc<VarDecl<A>>),
    Comment(String),
    Assign(Node<A, Expr<A>>, Node<A, Expr<A>>),
    If(Node<A, Expr<A>>, Block<A>, Block<A>),
    While(Node<A, Expr<A>>, Block<A>),
    For(VarDecl<A>, Node<A, Expr<A>>, Node<A, Expr<A>>, Block<A>),
    Return(Option<Node<A, Expr<A>>>),
    Expr(Node<A, Expr<A>>),
}

#[derive(Debug)]
pub enum Item<A: Ast> {
    Comment(String),
    GlobalVar(Rc<VarDecl<A>>),
    Fn(Rc<RefCell<FnDecl<A>>>), // TODO(veluca): find a better way.
    Type(Rc<TypeDecl<A>>),
}

// Declaration of a type alias.
#[derive(Debug)]
pub struct TypeDecl<A: Ast> {
    pub ident: Ident<A>,
    pub ty: Node<A, Type<A>>,
}

#[derive(Debug)]
pub struct Block<A: Ast> {
    pub statements: Vec<Node<A, Statement<A>>>,
}

#[derive(Debug)]
pub struct VarDecl<A: Ast> {
    pub ident: Ident<A>,
    pub ty: Node<A, Type<A>>,
    pub val: Option<Node<A, Expr<A>>>,
}

#[derive(Debug)]
pub struct FnDecl<A: Ast> {
    pub ident: Ident<A>,
    pub args: Vec<VarDecl<A>>,
    pub ret: Option<Node<A, Type<A>>>,
    pub body: Block<A>,
}

#[derive(Debug)]
pub struct Program<A: Ast> {
    pub items: Vec<Node<A, Item<A>>>,
}

impl<A: Ast> GetNode<Type<A>> for Type<A> {
    fn get(&self) -> Option<&Type<A>> {
        Some(self)
    }
}

impl<A: Ast> GetNode<Expr<A>> for Expr<A> {
    fn get(&self) -> Option<&Expr<A>> {
        Some(self)
    }
}

impl<A: Ast> GetNode<Statement<A>> for Statement<A> {
    fn get(&self) -> Option<&Statement<A>> {
        Some(self)
    }
}

impl<A: Ast> GetNode<Item<A>> for Item<A> {
    fn get(&self) -> Option<&Item<A>> {
        Some(self)
    }
}

impl<T: GetNode<T>> GetNode<T> for Option<T> {
    fn get(&self) -> Option<&T> {
        self.as_ref().and_then(|f| f.get())
    }
}
