use std::fmt::Debug;

pub type Ident = String;
pub type Comment = Option<String>;

// TODO: conversions, structs

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Integer,
    Float,
    String,
    Bool,
    Array(Box<Type>),
    Void,
}

#[derive(Debug)]
pub enum Op {
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

#[derive(Debug)]
pub enum Tuple {
    ExprTuple(Vec<Expr>),
    DeclTuple(Vec<Decl>),
}

#[derive(Debug)]
pub enum Expr {
    Ref(Ident),
    Integer(i64),
    Float(f64), // TODO
    String(String),
    Array(Vec<Expr>), // TODO
    Tuple(Tuple),
    Parens(Box<Expr>),
    Op(Box<Expr>, Op, Box<Expr>),
    Not(Box<Expr>),
    ArrayIndex(Box<Expr>, Box<Expr>),
    FunctionCall(Ident, Vec<Expr>),
}

#[derive(Debug)]
pub enum Statement {
    SilentBlank,
    Blank,
    Decl(Decl),
    Assign(Expr, Expr),
    If(Expr, Vec<Line>, Vec<Line>, Comment, Comment, Comment),
    While(Expr, Vec<Line>, Comment, Comment),
    For(Decl, Expr, Expr, bool, Vec<Line>, Comment, Comment),
    Return(Option<Expr>),
    Expr(Expr),
}

#[derive(Debug)]
pub enum Item {
    Func(Func),
    Line(Line),
}

#[derive(Debug)]
pub struct Line {
    pub statement: Option<Statement>,
    pub comment: Comment,
}

#[derive(Debug)]
pub struct Decl {
    pub ident: Ident,
    pub ty: Type,
    pub val: Option<Expr>,
}

#[derive(Debug)]
pub struct Func {
    pub ident: String,
    pub args: Vec<Decl>,
    pub ret: Type,
    pub body: Vec<Line>,
    pub comments: (Comment, Comment),
}
