use super::common::*;
use super::lexer::*;
use super::state::*;

fn parse_expr_list(
    parser_state: &mut ParserState,
    closing_delim: Token,
) -> Result<(Vec<Node<TextAst, Expr<TextAst>>>, bool)> {
    let mut exprs = vec![];
    let mut has_comma = false;
    loop {
        exprs.push(parse_expr(parser_state)?);
        if parser_state.peek()?.0 == closing_delim {
            parser_state.require(closing_delim)?;
            break;
        }
        parser_state.require(Token::Comma)?;
        has_comma = true;
        // This check allows trailing commas.
        if parser_state.peek()?.0 == closing_delim {
            parser_state.require(closing_delim)?;
            break;
        }
    }
    Ok((exprs, has_comma))
}

fn get_operation(token: Token) -> Option<(BinaryOp, usize)> {
    match token {
        Token::And => Some((BinaryOp::And, 1)),
        Token::Or => Some((BinaryOp::Or, 1)),
        Token::Ne => Some((BinaryOp::Ne, 2)),
        Token::Ge => Some((BinaryOp::Ge, 2)),
        Token::Le => Some((BinaryOp::Le, 2)),
        Token::Gt => Some((BinaryOp::Gt, 2)),
        Token::Lt => Some((BinaryOp::Lt, 2)),
        Token::Sum => Some((BinaryOp::Sum, 3)),
        Token::Sub => Some((BinaryOp::Sub, 3)),
        Token::Mul => Some((BinaryOp::Mul, 4)),
        Token::Div => Some((BinaryOp::Div, 4)),
        Token::Mod => Some((BinaryOp::Mod, 4)),
        _ => None,
    }
}

fn parse_expr_with_precedence(
    parser_state: &mut ParserState,
    left_precedence: usize,
) -> Result<Node<TextAst, Expr<TextAst>>> {
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
                let (args, _) = parse_expr_list(parser_state, Token::ClosedP)?;
                Expr::FunctionCall(fun, args)
            } else {
                // Variable reference.
                Expr::Ref(parser_state.find_var(ident)?)
            }
        }
        (Token::OpenSq, _, _) => {
            let mut parse_range = || {
                parser_state.require(Token::OpenSq)?;

                let expr1 = parse_expr(parser_state)?;
                parser_state.require(Token::Range)?;
                let expr2 = parse_expr(parser_state)?;
                if parser_state.peek()?.0 == Token::ClosedSq {
                    parser_state.require(Token::ClosedSq)?;
                    Ok(Expr::Range(
                        Box::new(expr1),
                        Box::new(expr2),
                        RangeType::Closed,
                    ))
                } else {
                    parser_state.require(Token::ClosedP)?;
                    Ok(Expr::Range(
                        Box::new(expr1),
                        Box::new(expr2),
                        RangeType::HalfOpen,
                    ))
                }
            };

            let range: Result<_> = parse_range();

            if let Ok(range) = range {
                range
            } else {
                parser_state.rollback_node(id);
                parser_state.require(Token::OpenSq)?;
                Expr::Array(parse_expr_list(parser_state, Token::ClosedSq)?.0)
            }
        }
        (Token::OpenP, _, _) => {
            parser_state.require(Token::OpenP)?;
            if parser_state.peek_next() == Token::Colon {
                let mut names_and_values = vec![];
                loop {
                    let ident = parser_state.ident()?;
                    parser_state.require(Token::Colon)?;
                    names_and_values.push((ident, parse_expr(parser_state)?));
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
                Expr::NamedTuple(names_and_values)
            } else {
                let (tuple_elems, has_comma) = parse_expr_list(parser_state, Token::ClosedP)?;
                if (tuple_elems.len() == 1 && has_comma) || tuple_elems.is_empty() {
                    parser_state.fail_node(id, "cannot have tuples of <= one element".into())?;
                }
                if tuple_elems.len() == 1 {
                    Expr::Parens(Box::new(tuple_elems.into_iter().next().unwrap()))
                } else {
                    Expr::Tuple(tuple_elems)
                }
            }
        }
        (Token::OpenBr, _, _) => {
            let mut parse_map = || {
                parser_state.require(Token::OpenBr)?;

                let mut keys_and_values = vec![];
                loop {
                    let key = parse_expr(parser_state)?;
                    parser_state.require(Token::Arrow)?;
                    keys_and_values.push((key, parse_expr(parser_state)?));
                    if parser_state.peek()?.0 == Token::ClosedBr {
                        parser_state.require(Token::ClosedBr)?;
                        break;
                    }
                    parser_state.require(Token::Comma)?;
                    // This check allows trailing commas.
                    if parser_state.peek()?.0 == Token::ClosedBr {
                        parser_state.require(Token::ClosedBr)?;
                        break;
                    }
                }
                Ok(Expr::Map(keys_and_values))
            };

            let map: Result<_> = parse_map();

            if let Ok(map) = map {
                map
            } else {
                parser_state.rollback_node(id);
                parser_state.require(Token::OpenBr)?;
                Expr::Set(parse_expr_list(parser_state, Token::ClosedBr)?.0)
            }
        }
        (Token::Output, _, _) => {
            parser_state.require(Token::Output)?;
            parser_state.require(Token::OpenP)?;
            let expr = parse_expr(parser_state)?;
            parser_state.require(Token::ClosedP)?;
            Expr::Output(Box::new(expr))
        }
        (Token::Not, _, _) => {
            parser_state.require(Token::Not)?;
            let expr = parse_expr_with_precedence(parser_state, usize::MAX)?;
            Expr::Not(Box::new(expr))
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
                        let (args, _) = parse_expr_list(parser_state, Token::ClosedP)?;
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
            (tok, _, _) if get_operation(tok).is_some() => {
                let (op, prec) = get_operation(tok).unwrap();
                if left_precedence >= prec {
                    // Left operator binds at least as strongly as the next operator. Return to caller and
                    // let them get the current expression.
                    break;
                }
                let new_id = parser_state.clone_node(id);
                expr = {
                    let be = Box::new(parser_state.end_node(new_id, expr));
                    parser_state.require(tok)?;
                    let next_expr = Box::new(parse_expr_with_precedence(parser_state, prec)?);
                    Expr::BinaryOp(be, op, next_expr)
                }
            }
            (_, _, _) => {
                break;
            }
        }
    }
    Ok(parser_state.end_node(id, expr))
}

pub fn parse_expr(parser_state: &mut ParserState) -> Result<Node<TextAst, Expr<TextAst>>> {
    parse_expr_with_precedence(parser_state, 0)
}
