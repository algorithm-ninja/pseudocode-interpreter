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
                let id = parser_state.start_node();
                let decl = parse_type_decl(&mut parser_state)?;
                let decl = parser_state.add_type(decl)?;
                out.items.push(parser_state.end_node(id, Item::Type(decl)));
            }
            (Token::Variable, _, _) => {
                let id = parser_state.start_node();
                let decl = parse_var_decl(&mut parser_state, /*allow_init=*/ false)?;
                let decl = parser_state.add_var(decl)?;
                out.items
                    .push(parser_state.end_node(id, Item::GlobalVar(decl)));
            }
            (Token::Comment, _, s) => {
                let s = s[2..].to_owned();
                let id = parser_state.start_node();
                parser_state.require(Token::Comment)?;
                parser_state.require(Token::Newline)?;
                out.items.push(parser_state.end_node(id, Item::Comment(s)));
            }
            (Token::Newline, _, _) => {
                parser_state.require(Token::Newline)?;
            }
            (Token::Function, _, _) => {
                let id = parser_state.start_node();
                let decl = parse_fn_decl(&mut parser_state)?;
                let decl = parser_state.add_fn(decl)?;
                out.items.push(parser_state.end_node(id, Item::Fn(decl)));
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
