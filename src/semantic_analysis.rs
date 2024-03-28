use std::collections::HashMap;

use chumsky::container::Container;

use crate::parser::{Expr, File, OuterStmt, Stmt, Type};

#[derive(Debug)]
pub enum SemanticErrorType {
    UndefinedVariable(String),
    TypeMismatch { expected: String, found: String },
    Redeclaration(String),
    InvalidFunctionCall { name: String, reason: String },
}

#[derive(Debug)]
pub struct SemanticError {
    typ: SemanticErrorType, // ...
}

impl SemanticError {}

pub enum SymbolType {
    Integer,
    Float,
    Char,
    Void,
    Pointer(Box<SymbolType>),
    Array(Box<SymbolType>),
    Function {
        args: Vec<Box<SymbolType>>,
        ret: Box<SymbolType>,
    },
    // Type(String)
}

impl SymbolType {
    pub fn from(t: Type) -> Self {
        match t {
            Type::Int => SymbolType::Integer,
            Type::Float => SymbolType::Float,
            Type::Char => SymbolType::Char,
            Type::Void => SymbolType::Void,
            Type::Array(t) => SymbolType::Array(Box::new(SymbolType::from(*t))),
            Type::Pointer(t) => SymbolType::Pointer(Box::new(SymbolType::from(*t))),
            Type::Custom(_) => todo!(),
        }
    }
}

pub struct Symbol {
    name: String,
    typ: SymbolType,
}

impl Symbol {
    pub fn from_type(t: Type, n: String) -> Self {
        Self {
            name: n,
            typ: SymbolType::from(t),
        }
    }
}

pub struct SymbolTable<'a> {
    symbols: HashMap<String, Symbol>,
    parent: Option<Box<&'a SymbolTable<'a>>>,
}

impl<'a> SymbolTable<'a> {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_child_of(other: &'a Self) -> Self {
        Self {
            symbols: HashMap::new(),
            parent: Some(Box::new(other)),
        }
    }

    pub fn has(&self, name: &String) -> bool {
        if self.symbols.contains_key(name) {
            true
        } else if self.parent.is_some() {
            self.parent.as_ref().unwrap().has(name)
        } else {
            false
        }
    }

    pub fn get(&self, name: &String) -> Option<&Symbol> {
        if self.symbols.contains_key(name) {
            self.symbols.get(name)
        } else if self.parent.is_some() {
            self.parent.as_ref().unwrap().get(name)
        } else {
            None
        }
    }

    pub fn add(&mut self, entry: (String, Symbol)) {
        self.symbols.push(entry);
    }
}

pub trait Analyzable {
    fn analyze(&mut self, symbs: &mut SymbolTable) -> Result<(), Vec<SemanticError>>;
}

impl Analyzable for Type {
    fn analyze(&mut self, symbs: &mut SymbolTable) -> Result<(), Vec<SemanticError>> {
        match self {
            Type::Int | Type::Float | Type::Char | Type::Void => Ok(()),
            Type::Array(t) => t.analyze(symbs),
            Type::Pointer(t) => t.analyze(symbs),
            // TODO: Figure out how to handle custom types (structs etc)
            Type::Custom(_) => todo!(),
        }
    }
}

impl Analyzable for OuterStmt {
    fn analyze(&mut self, symbs: &mut SymbolTable) -> Result<(), Vec<SemanticError>> {
        let mut errs: Vec<SemanticError> = vec![];
        match self {
            OuterStmt::FnDef {
                name,
                args,
                ret,
                body,
            } => {
                if symbs.symbols.contains_key(name) {
                    errs.push(SemanticError {
                        typ: SemanticErrorType::Redeclaration(name.clone()),
                    });
                } else {
                    symbs.symbols.push((
                        name.clone(),
                        Symbol {
                            name: name.clone(),
                            typ: SymbolType::Function {
                                args: args
                                    .clone()
                                    .into_iter()
                                    .map(|t| Box::new(SymbolType::from(t.1)))
                                    .collect(),
                                ret: Box::new(SymbolType::from(ret.clone())),
                            },
                        },
                    ));
                };

                let mut func_symbs = SymbolTable::new_child_of(symbs);
                args.into_iter().for_each(|(s, t)| {
                    func_symbs.add((s.clone(), Symbol::from_type(t.clone(), s.clone())))
                });
                body.into_iter().for_each(|s| {
                    if let Stmt::ExprStmt(Expr::Ret(r)) = s {
                        // TODO: Check r for errors, get r's type, make sure matches fn ret
                        // if r.
                    } else {
                        if let Err(mut es) = s.analyze(&mut func_symbs) {
                            errs.append(&mut es);
                        }
                    }
                });

                if errs.is_empty() {
                    Ok(())
                } else {
                    Err(errs)
                }
            }
            OuterStmt::StructDef { name, args } => todo!(),
            OuterStmt::GlobalVarDecl { name, typ, expr } => todo!(),
        }
    }
}

impl Analyzable for Stmt {
    fn analyze(&mut self, symbs: &mut SymbolTable) -> Result<(), Vec<SemanticError>> {
        match self {
            Stmt::ForStmt {
                init,
                cond,
                after,
                body,
            } => todo!(),
            Stmt::ExprStmt(_) => todo!(),
            Stmt::StmtList(_) => todo!(),
            Stmt::VarDecl { name, typ, expr } => todo!(),
        }
    }
}

impl Analyzable for Expr {
    fn analyze(&mut self, symbs: &mut SymbolTable) -> Result<(), Vec<SemanticError>> {
        match self {
            // TODO: check types are correct recursively through "get_type"
            Expr::ParserNone => todo!(),
            Expr::IntLit(_)
            | Expr::FloatLit(_)
            | Expr::StringLit(_)
            | Expr::CharLit(_)
            | Expr::Null => Ok(()),
            Expr::Var(_) => todo!(),
            Expr::Neg(_) => todo!(),
            Expr::Ref(_) => todo!(),
            Expr::Deref(_) => todo!(),
            Expr::Inc(_) => todo!(),
            Expr::Dec(_) => todo!(),
            Expr::Add(_, _) => todo!(),
            Expr::Sub(_, _) => todo!(),
            Expr::Mult(_, _) => todo!(),
            Expr::Div(_, _) => todo!(),
            Expr::Mod(_, _) => todo!(),
            Expr::Gt(_, _) => todo!(),
            Expr::Lt(_, _) => todo!(),
            Expr::Eq(_, _) => todo!(),
            Expr::Neq(_, _) => todo!(),
            Expr::Geq(_, _) => todo!(),
            Expr::Leq(_, _) => todo!(),
            Expr::Index(_, _) => todo!(),
            Expr::Assign(_, _) => todo!(),
            Expr::Ret(_) => todo!(),
            Expr::FnCall { expr, args } => todo!(),
            Expr::Member { expr, name } => todo!(),
            Expr::If {
                iff,
                elseifs,
                elsee,
            } => todo!(),
        }
    }
}

impl Expr {
    // TODO: recursively check types
    pub fn get_type(&self, symbs: &mut SymbolTable) -> SymbolType {
        todo!()
    }
}

pub fn semantic_analysis(f: &mut File) -> Vec<SemanticError> {
    let mut table = SymbolTable {
        symbols: HashMap::new(),
        parent: None,
    };
    let mut errs: Vec<SemanticError> = vec![];
    for stmt in f.stmts.iter_mut() {
        if let Err(mut es) = stmt.analyze(&mut table) {
            errs.append(&mut es);
        }
    }
    errs
}
