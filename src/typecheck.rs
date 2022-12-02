use std::fmt::Debug;

use crate::error::Error;

use super::ast::*;

pub type Result<T, A> = std::result::Result<T, Error<A>>;

#[derive(PartialEq, Eq)]
enum ValueType {
    LValue,
    RValue,
}

fn expect_type<A: Ast, NT: Debug + GetNode<NT> + Clone>(
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
        types.into_iter().cloned().cloned().collect(),
    ))
}

fn check_expr_has_type<A: Ast>(to_type: &Type<A>, from: &Node<A, Expr<A>>) -> Result<(), A> {
    let ty = typecheck_expr(from.get_contents()?, ValueType::LValue)?;
    expect_type(&vec![to_type], from, &ty)
}

fn typecheck_expr<A: Ast>(expr: &Expr<A>, value_type: ValueType) -> Result<Type<A>, A> {
    // TODO
    Ok(Type::Integer)
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
            let type_to = typecheck_expr(expr_to.get_contents()?, ValueType::RValue)?;
            check_expr_has_type(&type_to, expr)
        }
        Statement::Expr(expr) => {
            typecheck_expr(expr.get_contents()?, ValueType::LValue).map(|_| ())
        }
        Statement::Return(expr) => {
            if let (Some(expr), Some(ty)) = (expr, ret_type) {
                check_expr_has_type(ty, expr)
            } else if expr.is_none() && ret_type.is_none() {
                Ok(())
            } else if let Some(expr) = expr {
                Err(Error::ReturnHasValueError(expr.id, expr.info.clone()))
            } else {
                Err(Error::ReturnNoValueError(
                    statement.id,
                    statement.info.clone(),
                ))
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
