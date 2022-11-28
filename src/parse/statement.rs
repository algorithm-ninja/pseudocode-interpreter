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
    assert!(!allow_init, "Not yet implemented");
    parser_state.require(Token::Newline)?;
    Ok(VarDecl {
        ident,
        ty,
        val: None,
    })
}

pub fn parse_block(parser_state: &mut ParserState) -> Result<Block<TextAst>> {
    loop {
        // TODO
        if parser_state.done() {
            break;
        }
        if parser_state.peek()?.0 == Token::End && parser_state.peek_next() == Token::Function {
            break;
        }
        parser_state.advance()?;
    }
    Ok(Block { statements: vec![] })
}

pub fn parse_fn_decl(parser_state: &mut ParserState) -> Result<FnDecl<TextAst>> {
    parser_state.require(Token::Function)?;
    let ident = parser_state.ident()?;
    parser_state.require(Token::OpenP)?;
    let mut args = vec![];
    loop {
        let ident = parser_state.ident()?;
        parser_state.require(Token::Colon)?;
        args.push(VarDecl {
            ident,
            ty: parse_type(parser_state)?,
            val: None,
        });
        if parser_state.peek()?.0 == Token::ClosedP {
            parser_state.require(Token::ClosedP)?;
            break;
        }
        parser_state.require(Token::Comma)?;
        // This check allows trailing commas.
        if parser_state.peek()?.0 == Token::ClosedP {
            parser_state.require(Token::ClosedP)?;
            break;
        }
    }
    let ret = if parser_state.peek()?.0 == Token::Arrow {
        parser_state.require(Token::Arrow)?;
        Some(parse_type(parser_state)?)
    } else {
        None
    };
    parser_state.require(Token::Newline)?;
    let body = parse_block(parser_state)?;
    parser_state.require(Token::End)?;
    parser_state.require(Token::Function)?;
    parser_state.require(Token::Newline)?;
    Ok(FnDecl {
        ident,
        args,
        ret,
        body,
    })
}
