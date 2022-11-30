use super::common::*;
use super::lexer::*;
use super::state::*;

pub fn parse_type(parser_state: &mut ParserState) -> Result<Node<TextAst, Type<TextAst>>> {
    let id = parser_state.start_node();
    let mut ty = match parser_state.advance()? {
        (Token::Integer, _, _) => Type::Integer,
        (Token::Float, _, _) => Type::Float,
        (Token::String, _, _) => Type::String,
        (Token::OpenP, _, _) => {
            // Named tuples have an identifier followed by a colon, and they are the only types
            // that have a colon as the second token.
            if parser_state.peek_next() == Token::Colon {
                let mut names_and_types = vec![];
                loop {
                    let ident = parser_state.ident()?;
                    parser_state.require(Token::Colon)?;
                    names_and_types.push((ident, parse_type(parser_state)?));
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
                Type::NamedTuple(names_and_types)
            } else {
                let mut types = vec![];
                loop {
                    types.push(parse_type(parser_state)?);
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
                if types.len() <= 1 {
                    parser_state.fail_node(id, "Type tuples need at least 2 elements".into())?;
                }
                Type::Tuple(types)
            }
        }
        (Token::OpenBr, _, _) => {
            let ty = parse_type(parser_state)?;
            match parser_state.advance()? {
                (Token::ClosedBr, _, _) => Type::Set(Box::new(ty)),
                (Token::Arrow, _, _) => {
                    let ty2 = parse_type(parser_state)?;
                    parser_state.require(Token::ClosedBr)?;
                    Type::Map(Box::new(ty), Box::new(ty2))
                }
                (_, r, s) => {
                    return Err(Error::ParseError(
                        vec![Token::ClosedBr, Token::Arrow],
                        s.to_owned(),
                        r,
                    ));
                }
            }
        }
        (Token::Ident, r, s) => {
            let ident = Ident {
                name: s.to_owned(),
                info: r,
            };
            Type::NamedType(parser_state.find_type(ident)?)
        }
        (_, r, s) => {
            return Err(Error::ParseError(
                vec![
                    Token::Integer,
                    Token::Float,
                    Token::String,
                    Token::Bool,
                    Token::OpenP,
                    Token::OpenBr,
                    Token::Ident,
                ],
                s.to_owned(),
                r,
            ));
        }
    };
    while parser_state.peek()?.0 == Token::OpenSq {
        let new_id = parser_state.clone_node(id);
        ty = Type::Array(Box::new(parser_state.end_node(new_id, ty)));
        parser_state.require(Token::OpenSq)?;
        parser_state.require(Token::ClosedSq)?;
    }
    Ok(parser_state.end_node(id, ty))
}

pub fn parse_type_decl(parser_state: &mut ParserState) -> Result<TypeDecl<TextAst>> {
    parser_state.require(Token::Type)?;
    let ident = parser_state.ident()?;
    parser_state.require(Token::Colon)?;
    let ty = parse_type(parser_state)?;
    parser_state.require(Token::Newline)?;
    Ok(TypeDecl { ident, ty })
}
