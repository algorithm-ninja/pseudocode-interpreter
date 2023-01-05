use crate::error::Error;

mod common;
mod expr;
mod lexer;
mod state;
mod statement;
mod ty;

pub use lexer::Token;

pub use common::TextAst;
use common::*;
use state::*;
use statement::*;
use ty::*;

pub fn parse(src: &str) -> Result<Program<TextAst>> {
    let mut parser_state = ParserState::new(src);

    let mut out = Program {
        items: vec![],
        funs: vec![],
        vars: vec![],
    };

    while !parser_state.done() {
        match parser_state.peek()? {
            (Token::Type, _, _) => {
                out.items.push(parser_state.node(|parser_state| {
                    let decl = parse_type_decl(parser_state)?;
                    let decl = parser_state.add_type(decl)?;
                    Ok(Item::Type(decl))
                })?);
            }
            (Token::Variable, _, _) => {
                out.items.push(parser_state.node(|parser_state| {
                    let decl = parse_var_decl(parser_state, /*allow_init=*/ false)?;
                    let decl = parser_state.add_var(decl)?;
                    Ok(Item::GlobalVar(decl))
                })?);
            }
            (Token::Comment, _, s) => {
                let s = s[2..].to_owned();
                out.items.push(parser_state.node(|parser_state| {
                    parser_state.require(Token::Comment)?;
                    parser_state.require(Token::Newline)?;
                    Ok(Item::Comment(s))
                })?);
            }
            (Token::Newline, _, _) => {
                parser_state.require(Token::Newline)?;
            }
            (Token::Function, _, _) => {
                out.items.push(parser_state.node(|parser_state| {
                    let decl = parse_fn_decl(parser_state)?;
                    let decl = parser_state.add_fn(decl)?;
                    Ok(Item::Fn(decl))
                })?);
            }
            (_, r, s) => {
                return Err(Error::ParseError(
                    vec![
                        Token::Type,
                        Token::Newline,
                        Token::Function,
                        Token::Variable,
                        Token::Comment,
                    ],
                    s.to_owned(),
                    r,
                ));
            }
        }
    }

    parser_state.finalize(&mut out)?;

    Ok(out)
}
