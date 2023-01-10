use crate::parse::expr::parse_expr;

use super::common::*;
use super::lexer::*;
use super::state::*;
use super::ty::*;

// TODO(veluca): think about multiple declarations per statement.
pub fn parse_var_decl(
    parser_state: &mut ParserState,
    allow_init: bool,
) -> Result<VarDecl<TextAst>> {
    parser_state.require(Token::Variable)?;
    let ident = parser_state.ident()?;
    parser_state.require(Token::Colon)?;
    let ty = parse_type(parser_state)?;
    let val = if !allow_init || parser_state.peek()?.0 == Token::Newline {
        None
    } else {
        parser_state.require(Token::Assign)?;
        Some(parse_expr(parser_state)?)
    };
    parser_state.require(Token::Newline)?;
    Ok(VarDecl { ident, ty, val })
}

pub fn parse_block(
    parser_state: &mut ParserState,
    extra_decls: &Vec<VarDecl<TextAst>>,
) -> Result<(Vec<VarIndex>, Block<TextAst>)> {
    let mut statements = vec![];
    parser_state.start_scope();
    let mut extra_decl_idxs = vec![];
    for var in extra_decls {
        extra_decl_idxs.push(parser_state.add_var(var.clone())?);
    }
    loop {
        match parser_state.peek()? {
            (Token::Variable, _, _) => {
                statements.push(parser_state.node(|parser_state| {
                    let v = parse_var_decl(parser_state, /*allow_init=*/ true)?;
                    let v = parser_state.add_var(v)?;
                    Ok(Statement::Decl(v))
                })?);
            }
            (Token::Comment, _, s) => {
                let s = s[2..].to_owned();
                statements.push(parser_state.node(|parser_state| {
                    parser_state.require(Token::Comment)?;
                    parser_state.require(Token::Newline)?;
                    Ok(Statement::Comment(s))
                })?);
            }
            (Token::Newline, _, _) => {
                parser_state.require(Token::Newline)?;
            }
            (Token::If, _, _) => {
                statements.push(parser_state.node(|parser_state| {
                    parser_state.require(Token::If)?;
                    let cond = parse_expr(parser_state)?;
                    parser_state.require(Token::Then)?;
                    parser_state.require(Token::Newline)?;
                    let then_block = parse_block(parser_state, &vec![])?.1;
                    let else_block = if parser_state.peek()?.0 == Token::Else {
                        parser_state.require(Token::Else)?;
                        parser_state.require(Token::Newline)?;
                        parse_block(parser_state, &vec![])?.1
                    } else {
                        Block { statements: vec![] }
                    };
                    parser_state.require(Token::End)?;
                    parser_state.require(Token::If)?;
                    parser_state.require(Token::Newline)?;
                    Ok(Statement::If(cond, then_block, else_block))
                })?);
            }
            (Token::While, _, _) => {
                statements.push(parser_state.node(|parser_state| {
                    parser_state.require(Token::While)?;
                    let cond = parse_expr(parser_state)?;
                    parser_state.require(Token::Do)?;
                    parser_state.require(Token::Newline)?;
                    let block = parse_block(parser_state, &vec![])?.1;
                    parser_state.require(Token::End)?;
                    parser_state.require(Token::While)?;
                    parser_state.require(Token::Newline)?;
                    Ok(Statement::While(cond, block))
                })?);
            }
            (Token::For, _, _) => {
                statements.push(parser_state.node(|parser_state| {
                    parser_state.require(Token::For)?;
                    let ident = parser_state.ident()?;
                    let ty = if parser_state.peek()?.0 == Token::In {
                        parser_state.node(|_| Ok(Type::Integer))?
                    } else {
                        parser_state.require(Token::Colon)?;
                        parse_type(parser_state)?
                    };
                    let var = VarDecl {
                        ident: ident.clone(),
                        ty,
                        val: None,
                    };
                    parser_state.require(Token::In)?;
                    let arr = parse_expr(parser_state)?;
                    parser_state.require(Token::Do)?;
                    parser_state.require(Token::Newline)?;
                    let (decl, block) = parse_block(parser_state, &vec![var])?;
                    parser_state.require(Token::End)?;
                    parser_state.require(Token::For)?;
                    parser_state.require(Token::Newline)?;

                    // Create a temporary variable for holding the iteration index.
                    parser_state.start_scope();
                    let ty = parser_state.node(|_| Ok(Type::Integer))?;
                    let placeholder_var_idx = parser_state.add_var(VarDecl {
                        ident,
                        ty,
                        val: None,
                    })?;
                    parser_state.end_scope();
                    Ok(Statement::For(
                        decl[0],
                        arr,
                        block,
                        // placeholder for iteration index
                        placeholder_var_idx,
                        // placeholder for end-of-loop condition
                        parser_state.node(|_| Ok(Expr::Bool(true)))?,
                    ))
                })?);
            }
            (Token::Return, _, _) => {
                statements.push(parser_state.node(|parser_state| {
                    parser_state.require(Token::Return)?;
                    // TODO(veluca): consider figuring out from the current function whether we want to
                    // return something or not.
                    let ret = if parser_state.peek()?.0 == Token::Newline {
                        parser_state.require(Token::Newline)?;
                        None
                    } else {
                        Some(parse_expr(parser_state)?)
                    };
                    Ok(Statement::Return(ret))
                })?);
            }
            (Token::End, _, _) | (Token::Eos, _, _) | (Token::Else, _, _) => {
                break;
            }
            (_, _, _) => {
                // All other cases (Assign and Expr statements) begin with an expression.
                statements.push(parser_state.node(|parser_state| {
                    let expr = parse_expr(parser_state)?;
                    if parser_state.peek()?.0 == Token::Assign {
                        parser_state.require(Token::Assign)?;
                        let assignee = parse_expr(parser_state)?;
                        parser_state.require(Token::Newline)?;
                        Ok(Statement::Assign(expr, assignee))
                    } else {
                        parser_state.require(Token::Newline)?;
                        Ok(Statement::Expr(expr))
                    }
                })?);
            }
        };
    }
    parser_state.end_scope();
    Ok((extra_decl_idxs, Block { statements }))
}

pub fn parse_fn_decl(parser_state: &mut ParserState) -> Result<FnDecl<TextAst>> {
    parser_state.require(Token::Function)?;
    let ident = parser_state.ident()?;
    parser_state.require(Token::OpenP)?;
    let args = parse_comma_separated(parser_state, Token::ClosedP, |ps| {
        let ident = ps.ident()?;
        ps.require(Token::Colon)?;
        Ok(VarDecl {
            ident,
            ty: parse_type(ps)?,
            val: None,
        })
    })?
    .0;
    let ret = if parser_state.peek()?.0 == Token::Arrow {
        parser_state.require(Token::Arrow)?;
        Some(parse_type(parser_state)?)
    } else {
        None
    };
    parser_state.require(Token::Newline)?;
    let (decls, body) = parse_block(parser_state, &args)?;
    parser_state.require(Token::End)?;
    parser_state.require(Token::Function)?;
    parser_state.require(Token::Newline)?;
    Ok(FnDecl {
        ident,
        args: decls,
        ret,
        body,
    })
}
