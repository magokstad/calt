use chumsky::{
    combinator::SeparatedBy,
    error::Simple,
    primitive::just,
    recursive::{self, recursive},
    select, Parser,
};

use crate::lexer::Token;

#[derive(Debug)]
pub struct File {
    includes: Vec<Include>,
    stmts: Vec<OuterStmt>,
}

#[derive(Debug)]
pub struct Include {}

#[derive(Debug)]
pub enum OuterStmt {
    FnDef {
        name: String,
        args: Vec<(String, Type)>,
        body: Vec<Stmt>,
    },
    StructDef {
        name: String,
        args: Vec<(String, Type)>,
    },
    EnumDef {
        name: String,
        args: Vec<(String, Type)>,
    },
    GlobalVarDecl {
        name: String,
        typ: Option<Type>,
        expr: Expr,
    },
}

#[derive(Clone, Debug)]
pub enum Stmt {
    // ForStmt {
    //     init:
    // },
    ExprStmt(Expr),
}

#[derive(Clone, Debug)]
pub enum Expr {
    // Stinky hack
    ParserNone,

    IntLit(String),
    FloatLit(String),
    StringLit(String),
    CharLit(String),

    Null,

    Var(String),

    Neg(Box<Expr>),
    Ref(Box<Expr>),
    Deref(Box<Expr>),

    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mult(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),

    Gt(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Neq(Box<Expr>, Box<Expr>),
    Geq(Box<Expr>, Box<Expr>),
    Leq(Box<Expr>, Box<Expr>),

    Index(Box<Expr>, Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),

    FnCall {
        expr: Box<Expr>,
        args: Vec<Box<Expr>>,
    },
    Member {
        expr: Box<Expr>,
        name: String,
    },
    If {
        iff: (Box<Expr>, Vec<Stmt>),
        elseifs: Vec<(Box<Expr>, Vec<Stmt>)>,
        elsee: Vec<Stmt>,
    },
}

impl Expr {
    fn replace_parse_temp(self, new_expr: Expr) -> Expr {
        match self {
            Expr::Member { expr, name } => Expr::Member {
                expr: Box::new(new_expr),
                name,
            },
            Expr::FnCall { expr, args } => Expr::FnCall {
                expr: Box::new(new_expr),
                args,
            },
            Expr::Index(el, er) => Expr::Index(Box::new(new_expr), er),
            _ => Expr::ParserNone,
        }
    }
}

type UnaryOpFn = fn(Box<Expr>) -> Expr;
type BinaryOpFn = fn(Box<Expr>, Box<Expr>) -> Expr;

#[derive(Debug)]
pub enum Type {
    Int,
    Float,
    String,
    Char,
    Void,
    Custom(String),
    Array(Box<Type>),
    Pointer(Box<Type>),
    // Compund(),
}

pub fn parser() -> impl Parser<Token, File, Error = Simple<Token>> {
    let literal = select! {
        Token::IntLit(s) => Expr::IntLit(s),
        Token::FloatLit(s) => Expr::FloatLit(s),
        Token::StringLit(s) => Expr::StringLit(s),
        Token::CharLit(s) => Expr::CharLit(s)
    };

    let unary_op = select! {
        Token::Minus => Expr::Neg as UnaryOpFn,
        Token::Ampersand => Expr::Ref as UnaryOpFn,
        Token::Asterisk => Expr::Deref as UnaryOpFn,
    };

    let bin_op_strong = select! {
        Token::Asterisk => Expr::Mult as BinaryOpFn,
        Token::Slash => Expr::Div as BinaryOpFn,
        Token::Percent => Expr::Mod as BinaryOpFn,
    };

    let bin_op_weak = select! {
        Token::Plus => Expr::Add as BinaryOpFn,
        Token::Minus => Expr::Sub as BinaryOpFn,
    };

    let comp_strong = select! {
        Token::LessEqual => Expr::Leq as BinaryOpFn,
        Token::Less => Expr::Lt as BinaryOpFn,
        Token::GreaterEqual => Expr::Geq as BinaryOpFn,
        Token::Greater => Expr::Gt as BinaryOpFn,
    };

    let comp_weak = select! {
        Token::EqualEqual => Expr::Eq as BinaryOpFn,
        Token::NotEqual => Expr::Neq as BinaryOpFn,
    };

    let assign = select! {
        Token::Equal => Expr::Assign as BinaryOpFn
    };

    let var = select! {Token::Name(n) => Expr::Var(n)};
    let name = select! {Token::Name(n) => n};

    let stmt = recursive(|stmt| {
        let expr = recursive(|expr| {
            let inner = expr
                .clone()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen));

            let atom = literal.or(var).or(inner);

            // let iff = just(Token::If)
            //     .ignore_then(expr)
            //     .then(stmt.repeated().separated_by(just(Token::Newline)).delimited_by(Token, )).then(just(Token::Else).ignore_then(Token::If).ignore_then());

            let subscription = atom
                .clone()
                .delimited_by(just(Token::LeftBracket), just(Token::RightBracket))
                .map(|r| Expr::Index(Box::new(Expr::ParserNone), Box::new(r)));

            let func_call = atom
                .clone()
                .separated_by(just(Token::Comma))
                .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                .map(|rs| Expr::FnCall {
                    expr: Box::new(Expr::ParserNone),
                    args: rs.into_iter().map(|x| Box::new(x)).collect(),
                });

            let member_access = just(Token::Dot).ignore_then(name).map(|rhs| Expr::Member {
                expr: Box::new(Expr::ParserNone),
                name: rhs,
            });

            let first = atom
                .clone()
                .then(member_access.or(func_call).or(subscription).repeated())
                .foldl(|exp, firs| firs.replace_parse_temp(exp));

            // C operator precedence:
            // https://en.cppreference.com/w/c/language/operator_precedence

            let second = unary_op
                .repeated()
                .then(first.clone())
                .foldr(|op, rhs| op(Box::new(rhs)));

            let third = second
                .clone()
                .then(bin_op_strong.then(second.clone()).repeated())
                .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

            let fourth = third
                .clone()
                .then(bin_op_weak.then(third.clone()).repeated())
                .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

            // skipping fifth for now
            let fifth = fourth;

            let sixth = fifth
                .clone()
                .then(comp_strong.then(fifth.clone()).repeated())
                .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

            let seventh = sixth
                .clone()
                .then(comp_weak.then(sixth.clone()).repeated())
                .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

            // skipping to thirteenth
            let thirteenth = seventh.clone();
            let fourteenth = thirteenth
                .clone()
                .then(assign.then(thirteenth.clone()).or_not())
                .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

            fourteenth
        });
        stmt.or(expr.clone())
    });

    // Temporary
    stmt.map(|s| File {
        includes: vec![],
        stmts: vec![OuterStmt::GlobalVarDecl {
            name: String::from("test"),
            typ: None,
            expr: s,
        }],
    })
}
