use super::common::*;
use super::lexer::*;
use super::state::*;

fn parse_expr_list(
    parser_state: &mut ParserState,
    closing_delim: Token,
) -> Result<Vec<Node<TextAst, Expr<TextAst>>>> {
    let mut exprs = vec![];
    loop {
        exprs.push(parse_expr(parser_state)?);
        if parser_state.peek()?.0 == closing_delim {
            parser_state.require(closing_delim)?;
            break;
        }
        parser_state.require(Token::Comma)?;
        // This check allows trailing commas.
        if parser_state.peek()?.0 == closing_delim {
            parser_state.require(closing_delim)?;
            break;
        }
    }
    Ok(exprs)
}

pub fn parse_expr(parser_state: &mut ParserState) -> Result<Node<TextAst, Expr<TextAst>>> {
    let id = parser_state.start_node();
    let mut expr = match parser_state.peek()? {
        (Token::IntegerLit, _, _) => Expr::Integer(parser_state.integer_lit()?),
        (Token::FloatLit, _, _) => Expr::Float(parser_state.float_lit()?),
        (Token::StringLit, _, s) => {
            let s = s.to_owned();
            parser_state.require(Token::StringLit)?;
            Expr::String(s) // TODO escaping
        }
        (Token::True, _, _) => {
            parser_state.require(Token::True)?;
            Expr::Bool(true)
        }
        (Token::False, _, _) => {
            parser_state.require(Token::False)?;
            Expr::Bool(false)
        }
        (Token::Ident, _, _) => {
            let ident = parser_state.ident()?;
            if parser_state.peek()?.0 == Token::OpenP {
                // This is a function call.
                let fun = parser_state.find_fn(ident);
                parser_state.require(Token::OpenP)?;
                let args = parse_expr_list(parser_state, Token::ClosedP)?;
                Expr::FunctionCall(fun, args)
            } else {
                // Variable reference.
                Expr::Ref(parser_state.find_var(ident)?)
            }
        }
        (Token::OpenSq, _, _) => {
            // TODO: ranges
            parser_state.require(Token::OpenSq)?;
            Expr::Array(parse_expr_list(parser_state, Token::ClosedSq)?)
        }

        (_, r, s) => {
            return Err(Error::ParseError(
                vec![
                    Token::Ident,
                    Token::IntegerLit,
                    Token::FloatLit,
                    Token::StringLit,
                    Token::False,
                    Token::True,
                    Token::OpenSq,
                    Token::OpenBr,
                    Token::OpenP,
                    Token::Not,
                    Token::Output,
                ],
                s.to_owned(),
                r,
            ));
        }
    };
    loop {
        match parser_state.peek()? {
            // Indexing
            (Token::OpenSq, _, _) => {
                let new_id = parser_state.clone_node(id);
                expr = {
                    let be = Box::new(parser_state.end_node(new_id, expr));
                    parser_state.require(Token::OpenSq)?;
                    Expr::ArrayIndex(be, Box::new(parse_expr(parser_state)?))
                };
                parser_state.require(Token::ClosedSq)?;
            }
            // Tuple/member access
            (Token::Period, _, _) => {
                let new_id = parser_state.clone_node(id);
                expr = {
                    let be = Box::new(parser_state.end_node(new_id, expr));
                    parser_state.require(Token::Period)?;
                    if parser_state.peek_next() == Token::OpenP {
                        // method call
                        let method = parser_state.ident()?;
                        parser_state.require(Token::OpenP)?;
                        let args = parse_expr_list(parser_state, Token::ClosedP)?;
                        Expr::MethodCall(be, method, args)
                    } else if parser_state.peek()?.0 == Token::IntegerLit {
                        let index = parser_state.integer_lit()?;
                        if index < 0 {
                            // TODO
                            return Err(Error::ParseError(vec![], "-".to_owned(), 0..0));
                        }
                        let index = index as usize;
                        Expr::TupleField(be, index)
                    } else {
                        let ident = parser_state.ident()?;
                        Expr::NamedTupleField(be, ident)
                    }
                }
            }
            (_, _, _) => {
                break;
            }
        }
    }
    Ok(parser_state.end_node(id, expr))
}
