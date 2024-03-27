use std::collections::HashMap;

use chumsky::container::Container;

use crate::parser::{Expr, File, OuterStmt, Type};

enum SymbolType {
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
}

struct Symbol {
    name: String,
    typ: SymbolType,
}

struct SymbolTable {
    symbols: HashMap<String, Symbol>,
    parent: Option<Box<SymbolTable>>,
}

pub trait Analyzable {
    // fn analyze(self, symbs: Rc<Cell<SymbolTable>>) -> Self;
    fn analyze(self, symbs: &mut SymbolTable) -> Self;
}

impl Analyzable for Type {
    fn analyze(self, symbs: &mut SymbolTable) -> Self {
        match self {
            Type::Int => todo!(),
            Type::Float => todo!(),
            Type::Char => todo!(),
            Type::Void => todo!(),
            Type::Custom(_) => todo!(),
            Type::Array(_) => todo!(),
            Type::Pointer(_) => todo!(),
        }
    }
}

impl Analyzable for OuterStmt {
    // fn analyze(self, symbs: Rc<Cell<SymbolTable>>) -> Self {
    //     todo!()
    // }
    fn analyze(self, symbs: &mut SymbolTable) -> Self {
        match self {
            OuterStmt::FnDef {
                name,
                args,
                ret,
                body,
            } => {
                if symbs.symbols.contains_key(&name) {
                    todo!("error message and return")
                }
                symbs.symbols.push((name,))
            }
            OuterStmt::StructDef { name, args } => todo!(),
            OuterStmt::GlobalVarDecl { name, typ, expr } => todo!(),
        }
    }
}

pub fn semantic_analysis(f: File) -> File {
    let includes = f.includes;
    let mut table = SymbolTable {
        symbols: HashMap::new(),
        parent: None,
    };
    // Rc is probably overkill, but it is possible if needed
    // let mut symbols = Rc::new(Cell::new(table));
    let globs = f
        .stmts
        .into_iter()
        // .map(|g| g.analyze(symbols.clone()))
        .map(|g| g.analyze(&mut table))
        .collect();

    File {
        includes,
        stmts: globs,
    }
}
