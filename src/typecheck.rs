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
        types.into_iter().cloned().cloned().collect(),
    ))
}

fn check_expr_has_type<A: Ast>(to_type: &Type<A>, from: &Node<A, Expr<A>>) -> Result<(), A> {
    let ty = typecheck_expr(from, ValueType::LValue)?;
    expect_type(&vec![to_type], from, &ty)
}

fn typecheck_expr<A: Ast>(expr: &Node<A, Expr<A>>, value_type: ValueType) -> Result<Type<A>, A> {
    let check_is_lvalue = || {
        if value_type == ValueType::RValue {
            return Err(Error::NotAssignable(expr.id, expr.info.clone()));
        }
        Ok(())
    };
    let get_same_type = |exprs: &Vec<Node<A, Expr<A>>>| {
        if exprs.is_empty() {
            todo!(); // Figure out what to do here
        }
        let ty1 = typecheck_expr(&exprs[0], value_type)?;
        for e in exprs.iter().skip(1) {
            check_expr_has_type(&ty1, e)?;
        }
        Ok(ty1)
    };
    let get_same_types = |exprs: &Vec<(Node<A, Expr<A>>, Node<A, Expr<A>>)>| {
        if exprs.is_empty() {
            todo!(); // Figure out what to do here
        }
        let ty1first = typecheck_expr(&exprs[0].0, value_type)?;
        let ty1second = typecheck_expr(&exprs[0].1, value_type)?;
        for e in exprs.iter().skip(1) {
            check_expr_has_type(&ty1first, &e.0)?;
            check_expr_has_type(&ty1second, &e.1)?;
        }
        Ok((ty1first, ty1second))
    };

    let ty = match expr.get_contents()? {
        Expr::Ref(var) => var.ty.get_contents()?.clone(),
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
                .map(|x| typecheck_expr(x, value_type).map(|x| Node::new_with_defaults(x)))
                .collect::<Result<Vec<_>, _>>()?,
        ),
        Expr::NamedTuple(fields) => Type::NamedTuple(
            fields
                .iter()
                .map(|x| {
                    typecheck_expr(&x.1, value_type)
                        .map(|y| (x.0.clone(), Node::new_with_defaults(y)))
                })
                .collect::<Result<Vec<_>, _>>()?,
        ),
        Expr::Range(e1, e2, _) => {
            check_is_lvalue()?;
            check_expr_has_type(&Type::Integer, e1)?;
            check_expr_has_type(&Type::Integer, e2)?;
            Type::Array(Box::new(Node::new_with_defaults(Type::Integer)))
        }
        Expr::Parens(e) => typecheck_expr(e, value_type)?,
        Expr::Not(e) => {
            check_expr_has_type(&Type::Bool, e)?;
            Type::Bool
        }
        Expr::BinaryOp(e1, op, e2) => {
            check_is_lvalue()?;
            let ty1 = typecheck_expr(e1, ValueType::LValue)?;
            check_expr_has_type(&ty1, e2)?;
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
            check_expr_has_type(&Type::Integer, expr2)?;
            if let Type::Array(inner) = typecheck_expr(expr1, value_type)? {
                inner.get_contents()?.clone()
            } else {
                return Err(Error::ExpectedArray(expr.id, expr.info.clone()));
            }
        }
        Expr::Output(e) => {
            check_is_lvalue()?;
            typecheck_expr(e, ValueType::LValue)?;
            Type::Void
        }
        Expr::MethodCall(_, _, _) => {
            // TODO(veluca): define methods.
            todo!()
        }
        Expr::FunctionCall(f, args) => {
            if args.len() != f.borrow().args.len() {
                return Err(Error::WrongArgumentNumber(
                    expr.id,
                    expr.info.clone(),
                    f.borrow().ident.clone(),
                ));
            }
            for (expr, def) in args.iter().zip(f.borrow().args.iter()) {
                check_expr_has_type(def.ty.get_contents()?, expr)?;
            }
            if let Some(t) = &f.borrow().ret {
                t.get_contents()?.clone()
            } else {
                Type::Void
            }
        }
        Expr::TupleField(expr, idx) => {
            if let Type::Tuple(inner) = typecheck_expr(expr, value_type)? {
                if inner.len() >= *idx {
                    return Err(Error::InvalidTupleField(expr.id, expr.info.clone(), *idx));
                }
                inner[*idx].get_contents()?.clone()
            } else {
                return Err(Error::ExpectedTuple(expr.id, expr.info.clone()));
            }
        }
        Expr::NamedTupleField(expr, name) => {
            if let Type::NamedTuple(inner) = typecheck_expr(expr, value_type)? {
                if let Some(x) = inner.iter().filter(|x| x.0.name == name.name).next() {
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
    Ok(ty.canonical_type()?.clone())
}

fn typecheck_statement<A: Ast>(
    statement: &Node<A, Statement<A>>,
    ret_type: Option<&Type<A>>,
) -> Result<(), A> {
    match statement.get_contents()? {
        Statement::Comment(_) => Ok(()),
        Statement::If(boolexpr, b1, b2) => {
            check_expr_has_type(&Type::Bool, boolexpr)?;
            for s in b1.statements.iter().chain(b2.statements.iter()) {
                typecheck_statement(s, ret_type)?;
            }
            Ok(())
        }
        Statement::While(boolexpr, block) => {
            check_expr_has_type(&Type::Bool, boolexpr)?;
            for s in block.statements.iter() {
                typecheck_statement(s, ret_type)?;
            }
            Ok(())
        }
        Statement::For(var, expr, block) => {
            for s in block.statements.iter() {
                typecheck_statement(s, ret_type)?;
            }
            check_expr_has_type(&Type::Array(Box::new(var.ty.clone())), expr)
        }
        Statement::Decl(var) => var
            .val
            .as_ref()
            .map(|expr| Ok(check_expr_has_type(var.ty.get_contents()?, expr)?))
            .transpose()
            .map(|_| ()),
        Statement::Assign(expr_to, expr) => {
            let type_to = typecheck_expr(expr_to, ValueType::RValue)?;
            check_expr_has_type(&type_to, expr)
        }
        Statement::Expr(expr) => typecheck_expr(expr, ValueType::LValue).map(|_| ()),
        Statement::Return(expr) => {
            if let (Some(expr), Some(ty)) = (expr, ret_type) {
                check_expr_has_type(ty, expr)
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

fn typecheck_item<A: Ast>(item: &Item<A>) -> Result<(), A> {
    match item {
        Item::Type(_) => Ok(()),
        Item::Comment(_) => Ok(()),
        Item::GlobalVar(v) => {
            if let Some(val) = &v.val {
                check_expr_has_type(v.ty.get_contents()?, &val)
            } else {
                Ok(())
            }
        }
        Item::Fn(f) => {
            let f = f.borrow();
            for stmt in f.body.statements.iter() {
                typecheck_statement(
                    stmt,
                    f.ret.as_ref().map(|x| Ok(x.get_contents()?)).transpose()?,
                )?
            }
            Ok(())
        }
    }
}

pub fn typecheck<A: Ast>(program: &Program<A>) -> Result<(), A> {
    for i in program.items.iter() {
        typecheck_item(i.get_contents()?)?;
    }
    Ok(())
}
