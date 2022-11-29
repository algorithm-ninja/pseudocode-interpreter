use super::common::*;
use super::lexer::*;
use std::{cell::RefCell, collections::HashMap, fmt::Debug, ops::Range, rc::Rc};

use logos::Logos;

#[derive(Debug)]
struct ScopeState {
    types: HashMap<String, Rc<TypeDecl<TextAst>>>,
    variables: HashMap<String, Rc<VarDecl<TextAst>>>,
    functions: HashMap<String, Rc<RefCell<FnDecl<TextAst>>>>,
}

#[derive(Debug)]
struct NodeState {
    start_pos: usize,
    id: usize,
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
    // All functions that are not yet fully defined.
    unknown_functions: HashMap<String, Rc<RefCell<FnDecl<TextAst>>>>,
}

impl<'a> ParserState<'a> {
    pub fn new(input: &'a str) -> ParserState<'a> {
        let mut tokens: Vec<_> = Token::lexer(input).spanned().collect();
        tokens.push((Token::Eos, input.len()..input.len()));
        ParserState {
            input,
            tokens,
            input_pos: 0,
            next_node_id: 0,
            scope_state: vec![ScopeState::new()],
            node_state: vec![],
            unknown_functions: HashMap::new(),
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

    pub fn float_lit(&mut self) -> Result<f64> {
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

    pub fn find_fn(&mut self, ident: Ident<TextAst>) -> Rc<RefCell<FnDecl<TextAst>>> {
        for s in self.scope_state.iter().rev() {
            if let Some(x) = s.functions.get(&ident.name) {
                return x.clone();
            }
        }
        if let Some(x) = self.unknown_functions.get(&ident.name) {
            return x.clone();
        }
        let fd = Rc::new(RefCell::new(FnDecl {
            ident: ident.clone(),
            args: vec![],
            body: Block { statements: vec![] },
            ret: None,
        }));
        self.unknown_functions.insert(ident.name, fd.clone());
        fd
    }

    pub fn add_fn(&mut self, decl: FnDecl<TextAst>) -> Result<Rc<RefCell<FnDecl<TextAst>>>> {
        self.disallow_rollback();
        let scope = self.scope_state.last_mut().unwrap();
        if let Some(prev) = scope.functions.get(&decl.ident.name) {
            return Err(Error::DuplicateFunction(
                decl.ident.clone(),
                prev.borrow().ident.clone(),
            ));
        }

        let name = decl.ident.name.clone();
        let decl = if self.unknown_functions.contains_key(&decl.ident.name) {
            *self.unknown_functions.get_mut(&name).unwrap().borrow_mut() = decl;
            self.unknown_functions.remove(&name).unwrap()
        } else {
            Rc::new(RefCell::new(decl))
        };
        scope.functions.insert(name.clone(), decl);
        Ok(scope.functions.get(&name).unwrap().clone())
    }

    pub fn find_var(&mut self, ident: Ident<TextAst>) -> Result<Rc<VarDecl<TextAst>>> {
        for s in self.scope_state.iter().rev() {
            if let Some(x) = s.variables.get(&ident.name) {
                return Ok(x.clone());
            }
        }
        Err(Error::UnrecognizedVariable(ident))
    }

    pub fn add_var(&mut self, decl: Rc<VarDecl<TextAst>>) -> Result<Rc<VarDecl<TextAst>>> {
        self.disallow_rollback();
        let scope = self.scope_state.last_mut().unwrap();
        if let Some(prev) = scope.functions.get(&decl.ident.name) {
            return Err(Error::DuplicateVariable(
                decl.ident.clone(),
                prev.borrow().ident.clone(),
            ));
        }
        let name = decl.ident.name.clone();
        scope.variables.insert(name.clone(), decl);
        Ok(scope.variables.get(&name).unwrap().clone())
    }

    pub fn find_type(&mut self, ident: Ident<TextAst>) -> Result<Rc<TypeDecl<TextAst>>> {
        for s in self.scope_state.iter().rev() {
            if let Some(x) = s.types.get(&ident.name) {
                return Ok(x.clone());
            }
        }
        Err(Error::UnrecognizedType(ident))
    }

    pub fn add_type(&mut self, decl: TypeDecl<TextAst>) -> Result<Rc<TypeDecl<TextAst>>> {
        self.disallow_rollback();
        let scope = self.scope_state.last_mut().unwrap();
        if let Some(prev) = scope.functions.get(&decl.ident.name) {
            return Err(Error::DuplicateType(
                decl.ident.clone(),
                prev.borrow().ident.clone(),
            ));
        }
        let name = decl.ident.name.clone();
        scope.types.insert(name.clone(), Rc::new(decl));
        Ok(scope.types.get(&name).unwrap().clone())
    }

    pub fn start_node(&mut self) -> usize {
        let id = self.next_node_id;
        self.next_node_id += 1;
        self.node_state.push(NodeState {
            start_pos: self.input_pos,
            id,
            can_rollback: true,
        });
        id
    }

    pub fn clone_node(&mut self, id: usize) -> usize {
        let node = self
            .node_state
            .last()
            .expect("Programming error: cloning a node while none are open");
        assert!(node.id == id, "Programming error: cloning the wrong node");
        let id = self.next_node_id;
        self.next_node_id += 1;
        self.node_state.push(NodeState {
            start_pos: node.start_pos,
            id,
            can_rollback: true,
        });
        id
    }

    pub fn end_node<T: Debug + GetNode<T>>(&mut self, id: usize, contents: T) -> Node<TextAst, T> {
        let node = self
            .node_state
            .pop()
            .expect("Programming error: closing a node while none are open");
        assert!(node.id == id, "Programming error: closing the wrong node");
        let start = self.tokens[node.start_pos].1.start;
        let end = self.tokens[self.input_pos].1.end;
        Node {
            id: node.id,
            contents,
            info: start..end,
        }
    }

    pub fn rollback_node(&mut self, id: usize) {
        let node = self
            .node_state
            .pop()
            .expect("Programming error: rolling back a node while none are open");
        assert!(
            node.id == id,
            "Programming error: rolling back the wrong node"
        );
        self.input_pos = node.start_pos;
    }

    pub fn finalize(&self) -> Result<()> {
        assert!(
            self.node_state.is_empty(),
            "Programming error: some nodes were never closed"
        );
        assert!(
            self.scope_state.len() == 1,
            "Programming error: some scopes were never closed"
        );

        if let Some((_, f)) = self.unknown_functions.iter().next() {
            return Err(Error::UnrecognizedFunction(f.borrow().ident.clone()));
        }
        Ok(())
    }
}
