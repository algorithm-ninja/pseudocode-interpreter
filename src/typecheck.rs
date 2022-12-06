use std::fmt::Debug;

use crate::error::Error;

use super::ast::*;

pub type Result<T, A> = std::result::Result<T, Error<A>>;

#[derive(PartialEq, Eq, Clone, Copy)]
enum ValueType {
    LValue,
    RValue,
}

fn expect_type<A: Ast, NT: Debug + AstNode<NT> + Clone>(
    types: &[&Type<A>],
    node: &Node<A, NT>,
    node_type: &Type<A>,
) -> Result<(), A> {
    for t in types {
        if t.is_same(node_type)? {
            return Ok(());
        }
    }
    Err(Error::TypeError(
        node.id,
        node.info.clone(),
        node_type.clone(),
        types.iter().cloned().cloned().collect(),
    ))
}

fn check_expr_has_type<A: Ast>(
    program: &Program<A>,
    to_type: &Type<A>,
    from: &Node<A, Expr<A>>,
) -> Result<(), A> {
    let ty = typecheck_expr(program, from, ValueType::LValue)?;
    expect_type(&[to_type], from, &ty)
}

fn typecheck_expr<A: Ast>(
    program: &Program<A>,
    expr: &Node<A, Expr<A>>,
    value_type: ValueType,
) -> Result<Type<A>, A> {
    let check_is_lvalue = || {
        if value_type == ValueType::RValue {
            return Err(Error::NotAssignable(expr.id, expr.info.clone()));
        }
        Ok(())
    };
    type ExprNode<A> = Node<A, Expr<A>>;
    let get_same_type = |exprs: &Vec<ExprNode<A>>| {
        if exprs.is_empty() {
            todo!(); // Figure out what to do here
        }
        let ty1 = typecheck_expr(program, &exprs[0], value_type)?;
        for e in exprs.iter().skip(1) {
            check_expr_has_type(program, &ty1, e)?;
        }
        Ok(ty1)
    };
    let get_same_types = |exprs: &Vec<(ExprNode<A>, ExprNode<A>)>| {
        if exprs.is_empty() {
            todo!(); // Figure out what to do here
        }
        let ty1first = typecheck_expr(program, &exprs[0].0, value_type)?;
        let ty1second = typecheck_expr(program, &exprs[0].1, value_type)?;
        for e in exprs.iter().skip(1) {
            check_expr_has_type(program, &ty1first, &e.0)?;
            check_expr_has_type(program, &ty1second, &e.1)?;
        }
        Ok((ty1first, ty1second))
    };

    let ty = match expr.get_contents()? {
        Expr::Ref(var) => program.var(*var).ty.get_contents()?.clone(),
        Expr::Integer(_) => {
            check_is_lvalue()?;
            Type::Integer
        }
        Expr::Float(_) => {
            check_is_lvalue()?;
            Type::Float
        }
        Expr::String(_) => {
            check_is_lvalue()?;
            Type::String
        }
        Expr::Bool(_) => {
            check_is_lvalue()?;
            Type::Bool
        }
        Expr::Array(exprs) => {
            check_is_lvalue()?;
            Type::Array(Box::new(Node::new_with_defaults(get_same_type(exprs)?)))
        }
        Expr::Set(exprs) => {
            check_is_lvalue()?;
            Type::Set(Box::new(Node::new_with_defaults(get_same_type(exprs)?)))
        }
        Expr::Map(exprs) => {
            check_is_lvalue()?;
            let (ty1, ty2) = get_same_types(exprs)?;
            Type::Map(
                Box::new(Node::new_with_defaults(ty1)),
                Box::new(Node::new_with_defaults(ty2)),
            )
        }
        Expr::Tuple(fields) => Type::Tuple(
            fields
                .iter()
                .map(|x| typecheck_expr(program, x, value_type).map(|x| Node::new_with_defaults(x)))
                .collect::<Result<Vec<_>, _>>()?,
        ),
        Expr::NamedTuple(fields) => Type::NamedTuple(
            fields
                .iter()
                .map(|x| {
                    typecheck_expr(program, &x.1, value_type)
                        .map(|y| (x.0.clone(), Node::new_with_defaults(y)))
                })
                .collect::<Result<Vec<_>, _>>()?,
        ),
        Expr::Range(e1, e2, _) => {
            check_is_lvalue()?;
            check_expr_has_type(program, &Type::Integer, e1)?;
            check_expr_has_type(program, &Type::Integer, e2)?;
            Type::Array(Box::new(Node::new_with_defaults(Type::Integer)))
        }
        Expr::Parens(e) => typecheck_expr(program, e, value_type)?,
        Expr::Not(e) => {
            check_expr_has_type(program, &Type::Bool, e)?;
            Type::Bool
        }
        Expr::BinaryOp(e1, op, e2) => {
            check_is_lvalue()?;
            let ty1 = typecheck_expr(program, e1, ValueType::LValue)?;
            check_expr_has_type(program, &ty1, e2)?;
            match op {
                BinaryOp::Le
                | BinaryOp::Ge
                | BinaryOp::Gt
                | BinaryOp::Lt
                | BinaryOp::Eq
                | BinaryOp::Ne => Type::Bool,
                BinaryOp::Sum => {
                    expect_type(&[&Type::Integer, &Type::Float, &Type::String], e1, &ty1)?;
                    ty1
                }
                BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                    expect_type(&[&Type::Integer, &Type::Float], e1, &ty1)?;
                    ty1
                }
                BinaryOp::Mod => {
                    expect_type(&[&Type::Integer], e1, &ty1)?;
                    ty1
                }
                BinaryOp::And | BinaryOp::Or => {
                    expect_type(&[&Type::Bool], e1, &ty1)?;
                    ty1
                }
            }
        }
        Expr::ArrayIndex(expr1, expr2) => {
            check_expr_has_type(program, &Type::Integer, expr2)?;
            if let Type::Array(inner) = typecheck_expr(program, expr1, value_type)? {
                inner.get_contents()?.clone()
            } else {
                return Err(Error::ExpectedArray(expr.id, expr.info.clone()));
            }
        }
        Expr::Output(e) => {
            check_is_lvalue()?;
            typecheck_expr(program, e, ValueType::LValue)?;
            Type::Void
        }
        Expr::MethodCall(_, _, _) => {
            // TODO(veluca): define methods.
            todo!()
        }
        Expr::FunctionCall(f, args) => {
            let f = program.fun(*f);
            if args.len() != f.args.len() {
                return Err(Error::WrongArgumentNumber(
                    expr.id,
                    expr.info.clone(),
                    f.ident.clone(),
                ));
            }
            for (expr, def) in args.iter().zip(f.args.iter()) {
                check_expr_has_type(program, program.var(*def).ty.get_contents()?, expr)?;
            }
            if let Some(t) = &f.ret {
                t.get_contents()?.clone()
            } else {
                Type::Void
            }
        }
        Expr::TupleField(expr, idx) => {
            if let Type::Tuple(inner) = typecheck_expr(program, expr, value_type)? {
                if inner.len() >= *idx {
                    return Err(Error::InvalidTupleField(expr.id, expr.info.clone(), *idx));
                }
                inner[*idx].get_contents()?.clone()
            } else {
                return Err(Error::ExpectedTuple(expr.id, expr.info.clone()));
            }
        }
        Expr::NamedTupleField(expr, name) => {
            if let Type::NamedTuple(inner) = typecheck_expr(program, expr, value_type)? {
                if let Some(x) = inner.iter().find(|x| x.0.name == name.name) {
                    x.1.get_contents()?.clone()
                } else {
                    return Err(Error::InvalidNamedTupleField(
                        expr.id,
                        expr.info.clone(),
                        name.name.clone(),
                    ));
                }
            } else {
                return Err(Error::ExpectedNamedTuple(expr.id, expr.info.clone()));
            }
        }
    };
    ty.canonical_type()
}

fn typecheck_statement<A: Ast>(
    program: &Program<A>,
    statement: &Node<A, Statement<A>>,
    ret_type: Option<&Type<A>>,
) -> Result<(), A> {
    match statement.get_contents()? {
        Statement::Comment(_) => Ok(()),
        Statement::If(boolexpr, b1, b2) => {
            check_expr_has_type(program, &Type::Bool, boolexpr)?;
            for s in b1.statements.iter().chain(b2.statements.iter()) {
                typecheck_statement(program, s, ret_type)?;
            }
            Ok(())
        }
        Statement::While(boolexpr, block) => {
            check_expr_has_type(program, &Type::Bool, boolexpr)?;
            for s in block.statements.iter() {
                typecheck_statement(program, s, ret_type)?;
            }
            Ok(())
        }
        Statement::For(var, expr, block) => {
            for s in block.statements.iter() {
                typecheck_statement(program, s, ret_type)?;
            }
            check_expr_has_type(
                program,
                &Type::Array(Box::new(program.var(*var).ty.clone())),
                expr,
            )
        }
        Statement::Decl(var) => program
            .var(*var)
            .val
            .as_ref()
            .map(|expr| check_expr_has_type(program, program.var(*var).ty.get_contents()?, expr))
            .transpose()
            .map(|_| ()),
        Statement::Assign(expr_to, expr) => {
            let type_to = typecheck_expr(program, expr_to, ValueType::RValue)?;
            check_expr_has_type(program, &type_to, expr)
        }
        Statement::Expr(expr) => typecheck_expr(program, expr, ValueType::LValue).map(|_| ()),
        Statement::Return(expr) => {
            if let (Some(expr), Some(ty)) = (expr, ret_type) {
                check_expr_has_type(program, ty, expr)
            } else if expr.is_none() && ret_type.is_none() {
                Ok(())
            } else if let Some(expr) = expr {
                Err(Error::ReturnHasValue(expr.id, expr.info.clone()))
            } else {
                Err(Error::ReturnNoValue(statement.id, statement.info.clone()))
            }
        }
    }
}

fn typecheck_item<A: Ast>(program: &Program<A>, item: &Item<A>) -> Result<(), A> {
    match item {
        Item::Type(_) => Ok(()),
        Item::Comment(_) => Ok(()),
        Item::GlobalVar(v) => {
            let v = program.var(*v);
            if let Some(val) = &v.val {
                check_expr_has_type(program, v.ty.get_contents()?, val)
            } else {
                Ok(())
            }
        }
        Item::Fn(f) => {
            let f = program.fun(*f);
            for stmt in f.body.statements.iter() {
                typecheck_statement(
                    program,
                    stmt,
                    f.ret.as_ref().map(|x| x.get_contents()).transpose()?,
                )?
            }
            Ok(())
        }
    }
}

pub fn typecheck<A: Ast>(program: &Program<A>) -> Result<(), A> {
    for i in program.items.iter() {
        typecheck_item(program, i.get_contents()?)?;
    }
    Ok(())
}
