use std::{ collections::HashMap, fmt::Display, ops::Add};

use chumsky::container::Container;

use crate::parser::{Expr, File, OuterStmt, Stmt, Type};

#[derive(Debug)]
pub enum SemanticErrorType {
    UndefinedVariable(String),
    Redeclaration(String),

    // Same????
    TypeMismatch { expected: SymbolType, found: SymbolType },
    TypeConflict { lhs: SymbolType, rhs: SymbolType },

    InvalidFunctionCall { name: String, reason: String },
    InvalidAssignmentLhs(SymbolType), 
    InvalidDeref(SymbolType),
    InvalidIndexation(SymbolType),
    InvalidType { expected_one_of: Vec<SymbolType>, found: SymbolType },
}

#[derive(Debug)]
pub struct SemanticError {
    typ: SemanticErrorType, // ...
}

impl SemanticError {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolType {
    VoidPtr,
    Integer,
    Float,
    Char,
    Void,
    // Inferred,
    Variadic,
    Pointer(Box<SymbolType>),
    Array(Box<SymbolType>),
    Function {
        args: Vec<Box<SymbolType>>,
        ret: Box<SymbolType>,
    },
    // Type(String)
}

impl Display for SymbolType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Token::Error(s) => write!(f, "<error:{s}>"),
        match self {
            SymbolType::VoidPtr => write!(f, "&void"),
            SymbolType::Integer => write!(f, "int"),
            SymbolType::Float => write!(f, "float"),
            SymbolType::Char => write!(f, "char"),
            SymbolType::Void => write!(f, "void"),
            SymbolType::Pointer(t) => write!(f, "&{}", t),
            SymbolType::Array(t) => write!(f, "{}[]", t),
            SymbolType::Variadic => write!(f, " . . . "),
            SymbolType::Function { args, ret } => write!(
                f,
                "fn({}){}",
                args.into_iter()
                    .map(|arg| arg.to_string())
                    .fold(String::new(), |acc, i| acc.add(", ").add(i.as_str())),
                ret
            ),
        }
    }
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

    pub fn new(st: SymbolType, n: String) -> Self {
        Self {
            name: n,
            typ: st
        }
    }
    
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

    pub fn get(&self, name: &String) -> Result<&Symbol, SemanticError> {
        if let Some(s) = self.symbols.get(name) {
            Ok(s)
        } else if let Some(p) = self.parent.as_ref() {
            p.get(name)
        } else {
            Err(SemanticError {
                typ: SemanticErrorType::UndefinedVariable(name.to_string()),
            })
        }
    }

    pub fn add(&mut self, symbol: Symbol) {
        self.symbols.push((symbol.name.to_owned(), symbol));
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
                    func_symbs.add(Symbol::from_type(t.clone(), s.clone()))
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
        let mut errs = vec![];
        match self {
            Stmt::ForStmt {
                init,
                cond,
                after,
                body,
            } => {
                let mut for_symbs = SymbolTable::new_child_of(symbs);
                // FIXME: Very repetitive, these could probably be made into functions
                if let Err(mut es) = init.analyze(&mut for_symbs) {
                    errs.append(&mut es);
                }
                if let Err(mut es) = cond.analyze(&mut for_symbs) {
                    errs.append(&mut es);
                }
                if let Err(mut es) = after.analyze(&mut for_symbs) {
                    errs.append(&mut es);
                }
                for stmt in body {
                    if let Err(mut es) = stmt.analyze(&mut for_symbs) {
                        errs.append(&mut es);
                    }
                }
                // FIXME: that includes this guy (repetitive)
                if errs.is_empty() {
                    Ok(())
                } else {
                    Err(errs)
                }

            },
            Stmt::ExprStmt(e) => e.analyze(symbs),
            Stmt::StmtList(stms) => {
            let x =  stms.into_iter()
                .map(|s| match s.analyze(symbs) {
                    Ok(()) => vec![],
                    Err(e) => e
                })
                .flatten().collect::<Vec<_>>();
                if x.is_empty() {
                    Ok(())
                } else {
                    Err(x)
                }
            }
            Stmt::VarDecl { name, typ, expr } => {
                if symbs.has(name) {
                    errs.push(SemanticError { typ: SemanticErrorType::Redeclaration(name.to_owned()) });
                }
                if let Err(mut es) = expr.analyze(symbs) {
                    errs.append(&mut es);
                    // Return early, rest is VERY dependant on expr's type
                } else {
                    let rtyp = expr.get_type(symbs).unwrap();
                    let expected = match typ {
                        Some(t) => SymbolType::from(t.to_owned()),
                        None => rtyp.clone(),
                    };
                    if expected != expr.get_type(symbs).unwrap() {
                        errs.push(SemanticError { typ: SemanticErrorType::TypeMismatch { expected: expected.to_owned(), found: rtyp }})
                    }
                    symbs.add(Symbol::new(expected, name.to_owned()))
                }
                if errs.is_empty() {
                    Ok(())
                } else {
                    Err(errs)
                }
            }
        }
    }
}

impl Analyzable for Expr {
    fn analyze(&mut self, symbs: &mut SymbolTable) -> Result<(), Vec<SemanticError>> {
        if let Err(e) = self.get_type(symbs) {
            return Err(vec![e]);
        }
        let typ = self.get_type(symbs).unwrap();
        match self {
            // TODO: check types are correct recursively through "get_type"
            Expr::ParserNone
            | Expr::IntLit(_)
            | Expr::FloatLit(_)
            | Expr::StringLit(_)
            | Expr::CharLit(_)
            | Expr::Null
            | Expr::Neg(_)
            | Expr::Inc(_)
            | Expr::Dec(_)
            | Expr::Add(_, _)
            | Expr::Sub(_, _)
            | Expr::Mult(_, _)
            | Expr::Div(_, _)
            | Expr::Mod(_, _)
            | Expr::Gt(_, _)
            | Expr::Lt(_, _)
            | Expr::Eq(_, _)
            | Expr::Neq(_, _)
            | Expr::Geq(_, _)
            | Expr::Leq(_, _)
            // TODO: aanythign more for reffing / dereffing and gang???
            | Expr::FnCall { .. }
            | Expr::Ret(_) 
            | Expr::Index(_, _)
            | Expr::Assign(_, _)
            | Expr::Ref(_)
            | Expr::Deref(_) => Ok(()),
            Expr::Var(n) => {
                if symbs.has(n) {
                    Ok(())
                } else {
                    Err(vec![SemanticError {
                        typ: SemanticErrorType::UndefinedVariable(n.to_string()),
                    }])
                }
            }
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
    pub fn get_type(&self, symbs: &SymbolTable) -> Result<SymbolType, SemanticError> {
        match self {
            Expr::ParserNone => Ok(SymbolType::Void),
            // Err(SemanticError { typ: SemanticErrorType::TypeMismatch { expected: , found:  } }),
            Expr::IntLit(_) => Ok(SymbolType::Integer),
            Expr::FloatLit(_) => Ok(SymbolType::Float),
            Expr::StringLit(_) => Ok(SymbolType::Pointer(Box::new(SymbolType::Char))),
            Expr::CharLit(_) => Ok(SymbolType::Char),
            Expr::Null => Ok(SymbolType::VoidPtr),
            Expr::Var(s) => match symbs.get(s) {
                Ok(symb) => Ok(symb.typ.clone()),
                Err(e) => Err(e),
            },
            Expr::Inc(e) | Expr::Dec(e) | Expr::Neg(e) => match e.get_type(symbs)? {
                SymbolType::Integer | SymbolType::Float => Ok(e.get_type(symbs)?),
                SymbolType::Char
                | SymbolType::VoidPtr
                | SymbolType::Array(_)
                | SymbolType::Function { .. }
                | SymbolType::Void
                // TODO: Is this right?
                // | SymbolType::Inferred
                | SymbolType::Variadic
                | SymbolType::Pointer(_) => Err(SemanticError {
                    typ: SemanticErrorType::InvalidType { expected_one_of: vec![SymbolType::Integer, SymbolType::Float], found: e.get_type(symbs)? }
                }),
            },
            Expr::Ref(e) => Ok(SymbolType::Pointer(Box::new(e.get_type(symbs)?))),
            Expr::Deref(e) => {
                if let Expr::Ref(i) = *e.clone() {
                    Ok(i.get_type(symbs)?)
                } else {
                    Err(SemanticError {
                        typ: SemanticErrorType::InvalidDeref(e.get_type(symbs)?)
                    })
                }
            }
            Expr::Add(l, r)
            | Expr::Sub(l, r)
            | Expr::Mult(l, r)
            | Expr::Div(l, r)
            | Expr::Mod(l, r) => {
                let (lt, rt) = (l.get_type(symbs)?, r.get_type(symbs)?);
                if lt != SymbolType::Integer || lt != SymbolType::Float {
                    Err(SemanticError {
                        typ: SemanticErrorType::InvalidType { expected_one_of: vec![SymbolType::Integer, SymbolType::Float], found: lt } 
                    })
                } else if rt != SymbolType::Integer || rt != SymbolType::Float {
                    Err(SemanticError {
                        typ: SemanticErrorType::InvalidType { expected_one_of: vec![SymbolType::Integer, SymbolType::Float], found: lt } 
                    })
                } else if rt != lt {
                    Err(SemanticError {
                        typ: SemanticErrorType::TypeConflict {
                            lhs: lt,
                            rhs: rt,
                        },
                    })
                } else {
                    Ok(lt)
                }
            }

            Expr::Gt(l, r)
            | Expr::Lt(l, r)
            | Expr::Eq(l, r)
            | Expr::Neq(l, r)
            | Expr::Geq(l, r)
            | Expr::Leq(l, r) => {
                // TODO: Stricter checking of types here, structs should prob not be =='d by default
                let (lt, rt) = (l.get_type(symbs)?, r.get_type(symbs)?);
                if rt != lt {
                    Err(SemanticError {
                        typ: SemanticErrorType::TypeConflict {
                            lhs: lt,
                            rhs: rt,
                        },
                    })
                } else {
                    Ok(lt)
                }
            }
            // TODO: "un-indentify" this, wtf happened here?
            // TODO: What about inner indexes and
            Expr::Index(e, i) => {
                if let Expr::Var(s) = *e.clone() {
                    match symbs.get(&s) {
                        Ok(s) => {
                            if let SymbolType::Array(_) = s.typ.clone() {
                                // TODO: Check if this check fits here, or should be moved to analyze
                                if let SymbolType::Integer = i.get_type(symbs)? {
                                    Ok(s.typ.clone())
                                } else {
                                    Err(SemanticError {
                                        typ: SemanticErrorType::TypeMismatch {
                                            expected: SymbolType::Integer,
                                            found: i.get_type(symbs)?,
                                        },
                                    })
                                }
                            } else {
                                Err(SemanticError {
                                    typ: SemanticErrorType::InvalidIndexation(e.get_type(symbs)?) 
                                })
                            }
                        }
                        Err(e) => Err(e),
                    }
                } else {
                                Err(SemanticError {
                                    typ: SemanticErrorType::InvalidIndexation(e.get_type(symbs)?) 
                                })
                }
            }
            Expr::Assign(v, e) => match *v.clone() {
                Expr::ParserNone
                | Expr::IntLit(_)
                | Expr::FloatLit(_)
                | Expr::StringLit(_)
                | Expr::CharLit(_)
                | Expr::Null
                | Expr::Neg(_)
                | Expr::Ref(_)
                | Expr::Deref(_)
                | Expr::Inc(_)
                | Expr::Dec(_)
                | Expr::Add(_, _)
                | Expr::Sub(_, _)
                | Expr::Mult(_, _)
                | Expr::Div(_, _)
                | Expr::Mod(_, _)
                | Expr::Gt(_, _)
                | Expr::Lt(_, _)
                | Expr::Eq(_, _)
                | Expr::Neq(_, _)
                | Expr::Geq(_, _)
                | Expr::Leq(_, _)
                | Expr::Ret(_)
                | Expr::FnCall { .. }
                | Expr::Assign(_, _)
                | Expr::If { .. } => Err(SemanticError {
                    typ: SemanticErrorType::InvalidIndexation(v.get_type(symbs)?)
                }),
                Expr::Member { .. } | Expr::Index(_, _) | Expr::Var(_) => {
                    let (lt, rt) = (v.get_type(symbs)?, e.get_type(symbs)?);
                    if lt == rt {
                        Ok(lt)
                    } else {
                        Err(SemanticError {
                            typ: SemanticErrorType::TypeMismatch {
                                expected: lt,
                                found: rt,
                            },
                        })
                    }
                }
            },
            Expr::Ret(i) => Ok(i.get_type(symbs)?),
            Expr::FnCall { expr, args } => expr.get_type(symbs),
            Expr::Member { expr, name } => todo!(),
            // TODO: This one is kiiiinda tricky...
            //   Implicit ret??? Separate ret for ifs??? 
            Expr::If {
                iff,
                elseifs,
                elsee,
            } => Ok(SymbolType::Void)
        }
    }
}

pub fn semantic_analysis(f: &mut File) -> Vec<SemanticError> {
    let mut table = SymbolTable::new();

    table.add(Symbol { 
        name: "printf".to_string(), 
        typ: SymbolType::Function { 
            args: vec![ Box::new(SymbolType::Pointer(Box::new(SymbolType::Char))), 
                        Box::new(SymbolType::Variadic)], 
            ret: Box::new(SymbolType::Integer) 
        }  
    });
    
    let mut errs: Vec<SemanticError> = vec![];
    for stmt in f.stmts.iter_mut() {
        if let Err(mut es) = stmt.analyze(&mut table) {
            errs.append(&mut es);
        }
    }
    errs
}
