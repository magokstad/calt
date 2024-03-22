use chumsky::{error::Simple, primitive::just, select, Parser};

use crate::lexer::Token;

pub struct File {
    imports: Vec<Import>,
    stmts: Vec<Stmt>,
}

pub struct Import {}

pub enum Stmt {
    FnDef {
        name: String,
        args: Vec<(String, Type)>,
        body: Vec<Expr>,
    },
    StructDef {
        name: String,
        args: Vec<(String, Type)>,
    },
    EnumDef {
        name: String,
        args: Vec<(String, Type)>,
    },
}

pub enum Expr {
    IntLit(String),
    FloatLit(String),
    StringLit(String),
    CharLit(String),

    Var(String),

    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mult(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),

    Index(Box<Expr>, Box<Expr>),

    FnCall { name: String, args: Vec<Expr> },
}

pub enum Type {
    Int,
    Float,
    String,
    Char,
    Custom(String),
    Array(Box<Type>),
    Pointer(Box<Type>),
    // Compund(),
}

pub fn parser() -> impl Parser<Token, Expr, Error = Simple<Token>> {
    let literal = select! {
        Token::IntLit(s) => Expr::IntLit(s),
        Token::FloatLit(s) => Expr::FloatLit(s)
        Token::FloatLit(s) => Expr::FloatLit(s)
        Token::FloatLit(s) => Expr::FloatLit(s)
    };

    basics
}
