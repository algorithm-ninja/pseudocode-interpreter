use super::common::*;
use super::lexer::*;
use super::state::*;

pub fn parse_type(parser_state: &mut ParserState) -> Result<Node<TextAst, Type<TextAst>>> {
    parser_state.node(|parser_state| {
        let mut ty = match parser_state.advance()? {
            (Token::Integer, _, _) => Type::Integer,
            (Token::Float, _, _) => Type::Float,
            (Token::String, _, _) => Type::String,
            (Token::Bool, _, _) => Type::Bool,
            (Token::OpenP, _, _) => {
                // Named tuples have an identifier followed by a colon, and they are the only types
                // that have a colon as the second token.
                if parser_state.peek_next() == Token::Colon {
                    Type::NamedTuple(
                        parse_comma_separated(parser_state, Token::ClosedP, |ps| {
                            let ident = ps.ident()?;
                            ps.require(Token::Colon)?;
                            Ok((ident, parse_type(ps)?))
                        })?
                        .0,
                    )
                } else {
                    let types = parse_comma_separated(parser_state, Token::ClosedP, parse_type)?.0;
                    if types.len() <= 1 {
                        return Err(Error::PlaceholderParseError(
                            "Type tuples need at least 2 elements".into(),
                        ));
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
            ty = Type::Array(Box::new(parser_state.clone_current_node(|_| Ok(ty))?));
            parser_state.require(Token::OpenSq)?;
            parser_state.require(Token::ClosedSq)?;
        }
        Ok(ty)
    })
}

pub fn parse_type_decl(parser_state: &mut ParserState) -> Result<TypeDecl<TextAst>> {
    parser_state.require(Token::Type)?;
    let ident = parser_state.ident()?;
    parser_state.require(Token::Colon)?;
    let ty = parse_type(parser_state)?;
    parser_state.require(Token::Newline)?;
    Ok(TypeDecl { ident, ty })
}
