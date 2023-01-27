use super::common::*;
use super::lexer::*;

use std::sync::Weak;
use std::{collections::HashMap, fmt::Debug, ops::Range, sync::Arc};

use logos::Logos;
use ordered_float::NotNan;

#[derive(Debug)]
struct ScopeState {
    types: HashMap<String, Arc<TypeDecl<TextAst>>>,
    variables: HashMap<String, VarIndex>,
    functions: HashMap<String, FnIndex>,
}

#[derive(Debug)]
struct NodeState {
    start_pos: usize,
    can_rollback: bool,
}

impl ScopeState {
    fn new() -> ScopeState {
        ScopeState {
            types: HashMap::new(),
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct ParserState<'a> {
    input: &'a str,
    tokens: Vec<(Token, Range<usize>)>,
    input_pos: usize,
    next_node_id: usize,
    scope_state: Vec<ScopeState>,
    node_state: Vec<NodeState>,
    vars: Vec<VarDecl<TextAst>>,
    funs: Vec<FnDecl<TextAst>>,
    unknown_funs: HashMap<String, FnIndex>,
}

impl<'a> ParserState<'a> {
    pub fn new(input: &'a str) -> ParserState<'a> {
        let mut tokens: Vec<_> = Token::lexer(input).spanned().collect();
        tokens.push((Token::Newline, input.len()..input.len()));
        tokens.push((Token::Eos, input.len()..input.len()));
        ParserState {
            input,
            tokens,
            input_pos: 0,
            next_node_id: 0,
            scope_state: vec![ScopeState::new()],
            node_state: vec![],
            vars: vec![],
            funs: vec![],
            unknown_funs: HashMap::new(),
        }
    }

    pub fn done(&self) -> bool {
        self.tokens[self.input_pos].0 == Token::Eos
    }

    fn tok(&self) -> Result<(Token, Range<usize>)> {
        let (tok, r) = self.tokens[self.input_pos].clone();
        if tok == Token::Error {
            return Err(Error::UnrecognizedToken(
                self.input[r.clone()].to_owned(),
                r,
            ));
        }
        Ok((tok, r))
    }

    pub fn peek(&self) -> Result<(Token, Range<usize>, &str)> {
        let (tok, r) = self.tok()?;
        Ok((tok, r.clone(), &self.input[r]))
    }

    pub fn peek_next(&self) -> Token {
        self.tokens
            .get(self.input_pos + 1)
            .map(|x| x.0)
            .unwrap_or(Token::Error)
    }

    pub fn advance(&mut self) -> Result<(Token, Range<usize>, &str)> {
        let (tok, r) = self.tok()?;
        if tok != Token::Eos {
            self.input_pos += 1;
        }
        Ok((tok, r.clone(), &self.input[r]))
    }

    pub fn require(&mut self, req_tok: Token) -> Result<(Range<usize>, &str)> {
        let (tok, r) = self.tok()?;
        let s = &self.input[r.clone()];
        if tok != req_tok {
            return Err(Error::ParseError(vec![req_tok], s.to_owned(), r));
        }
        if tok != Token::Eos {
            self.input_pos += 1;
        }
        Ok((r, s))
    }

    pub fn ident(&mut self) -> Result<Ident<TextAst>> {
        let (r, s) = self.require(Token::Ident)?;
        Ok(Ident {
            name: s.to_owned(),
            info: r,
        })
    }

    pub fn integer_lit(&mut self) -> Result<i64> {
        let (r, s) = self.require(Token::IntegerLit)?;
        s.parse()
            .map_err(|_| Error::ParseError(vec![Token::IntegerLit], s.to_owned(), r.clone()))
    }

    pub fn float_lit(&mut self) -> Result<NotNan<f64>> {
        let (r, s) = self.require(Token::FloatLit)?;
        s.parse()
            .map_err(|_| Error::ParseError(vec![Token::IntegerLit], s.to_owned(), r.clone()))
    }

    fn disallow_rollback(&mut self) {
        for n in &mut self.node_state {
            n.can_rollback = false;
        }
    }

    pub fn start_scope(&mut self) {
        self.disallow_rollback();
        self.scope_state.push(ScopeState::new());
    }

    pub fn end_scope(&mut self) {
        self.disallow_rollback();
        self.scope_state
            .pop()
            .expect("Programming error: closing a scope while none are open");
    }

    pub fn find_fn(&mut self, ident: Ident<TextAst>) -> FnIndex {
        for s in self.scope_state.iter().rev() {
            if let Some(x) = s.functions.get(&ident.name) {
                return *x;
            }
        }
        if let Some(x) = self.unknown_funs.get(&ident.name) {
            return *x;
        }
        let fd = FnDecl {
            ident: ident.clone(),
            args: vec![],
            body: Block { statements: vec![] },
            ret: None,
        };
        let idx = FnIndex(self.funs.len());
        self.funs.push(fd);
        self.unknown_funs.insert(ident.name, idx);
        idx
    }

    pub fn add_fn(&mut self, decl: FnDecl<TextAst>) -> Result<FnIndex> {
        self.disallow_rollback();
        let scope = self.scope_state.last_mut().unwrap();
        if let Some(prev) = scope.functions.get(&decl.ident.name) {
            return Err(Error::DuplicateFunction(
                decl.ident.clone(),
                self.funs[prev.0].ident.clone(),
            ));
        }

        let name = decl.ident.name.clone();
        let idx = if self.unknown_funs.contains_key(&decl.ident.name) {
            let idx = self.unknown_funs.remove(&decl.ident.name).unwrap();
            self.funs[idx.0] = decl;
            idx
        } else {
            let idx = FnIndex(self.funs.len());
            self.funs.push(decl);
            idx
        };
        scope.functions.insert(name, idx);
        Ok(idx)
    }

    pub fn find_var(&mut self, ident: Ident<TextAst>) -> Result<VarIndex> {
        for s in self.scope_state.iter().rev() {
            if let Some(x) = s.variables.get(&ident.name) {
                return Ok(*x);
            }
        }
        Err(Error::UnrecognizedVariable(ident))
    }

    pub fn add_var(&mut self, decl: VarDecl<TextAst>) -> Result<VarIndex> {
        self.disallow_rollback();
        let scope = self.scope_state.last_mut().unwrap();
        if let Some(prev) = scope.variables.get(&decl.ident.name) {
            return Err(Error::DuplicateVariable(
                decl.ident.clone(),
                self.vars[prev.0].ident.clone(),
            ));
        }
        let idx = VarIndex(self.vars.len());
        let name = decl.ident.name.clone();
        self.vars.push(decl);
        scope.variables.insert(name, idx);
        Ok(idx)
    }

    pub fn find_type(&mut self, ident: Ident<TextAst>) -> Result<Weak<TypeDecl<TextAst>>> {
        for s in self.scope_state.iter().rev() {
            if let Some(x) = s.types.get(&ident.name) {
                return Ok(Arc::downgrade(x));
            }
        }
        Err(Error::UnrecognizedType(ident))
    }

    pub fn add_type(&mut self, decl: TypeDecl<TextAst>) -> Result<Arc<TypeDecl<TextAst>>> {
        self.disallow_rollback();
        let scope = self.scope_state.last_mut().unwrap();
        if let Some(prev) = scope.types.get(&decl.ident.name) {
            return Err(Error::DuplicateType(decl.ident.clone(), prev.ident.clone()));
        }
        let name = decl.ident.name.clone();
        scope.types.insert(name.clone(), Arc::new(decl));
        Ok(scope.types.get(&name).unwrap().clone())
    }

    fn node_impl<T: Debug + AstNode<T> + Clone, F>(
        &mut self,
        create: F,
        start_pos: usize,
    ) -> Result<Node<TextAst, T>>
    where
        F: FnOnce(&mut Self) -> Result<T>,
    {
        self.node_state.push(NodeState {
            start_pos,
            can_rollback: true,
        });
        let res = create(self);
        let node = self.node_state.pop().unwrap();
        if let Ok(res) = res {
            assert!(
                self.input_pos >= 1,
                "Programming error: closing a node with no content"
            );
            let start = self.tokens[node.start_pos].1.start;
            let end = self.tokens[self.input_pos - 1].1.end;
            let id = self.next_node_id;
            self.next_node_id += 1;
            Ok(Node {
                id,
                contents: res,
                info: start..end,
            })
        } else {
            let err = res.unwrap_err();
            let start = self.tokens[node.start_pos].1.start;
            let end = self.tokens[self.input_pos].1.end;
            if let Error::PlaceholderParseError(msg) = err {
                Err(Error::GenericParseError(
                    msg,
                    self.input[start..end].to_owned(),
                    start..end,
                ))
            } else {
                Err(err)
            }
        }
    }

    pub fn node<T: Debug + AstNode<T> + Clone, F>(&mut self, create: F) -> Result<Node<TextAst, T>>
    where
        F: FnOnce(&mut Self) -> Result<T>,
    {
        self.node_impl(create, self.input_pos)
    }

    pub fn clone_current_node<T: Debug + AstNode<T> + Clone, F>(
        &mut self,
        create: F,
    ) -> Result<Node<TextAst, T>>
    where
        F: FnOnce(&mut Self) -> Result<T>,
    {
        let pos = self.node_state.last().unwrap().start_pos;
        self.node_impl(create, pos)
    }

    pub fn rollback_current_node(&mut self) {
        let node = self.node_state.pop().unwrap();
        self.input_pos = node.start_pos;
        self.node_state.push(node);
    }

    pub fn finalize(&mut self, program: &mut Program<TextAst>) -> Result<()> {
        assert!(
            self.node_state.is_empty(),
            "Programming error: some nodes were never closed"
        );
        assert!(
            self.scope_state.len() == 1,
            "Programming error: some scopes were never closed"
        );

        if let Some((_, f)) = self.unknown_funs.iter().next() {
            return Err(Error::UnrecognizedFunction(self.funs[f.0].ident.clone()));
        }

        program.funs = self.funs.drain(..).collect();
        program.vars = self.vars.drain(..).collect();

        Ok(())
    }
}

pub fn parse_comma_separated<T, F>(
    parser_state: &mut ParserState,
    closing_delim: Token,
    parse_one: F,
) -> Result<(Vec<T>, bool)>
where
    F: Fn(&mut ParserState) -> Result<T>,
{
    if parser_state.peek()?.0 == closing_delim {
        parser_state.require(closing_delim)?;
        return Ok((vec![], false));
    }
    let mut exprs = vec![];
    let mut has_comma = false;
    loop {
        exprs.push(parse_one(parser_state)?);
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
