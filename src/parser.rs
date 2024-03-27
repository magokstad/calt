use chumsky::{
    error::Rich,
    extra,
    input::{Input, ValueInput},
    pratt::{infix, left, postfix, prefix},
    primitive::{choice, just},
    recursive::{recursive, Recursive},
    select,
    span::SimpleSpan,
    IterParser, Parser,
};

use crate::lexer::Token;

#[derive(Clone, Debug)]
pub struct File {
    includes: Vec<Include>,
    stmts: Vec<OuterStmt>,
}

#[derive(Clone, Debug)]
pub struct Include {}

#[derive(Clone, Debug)]
pub enum OuterStmt {
    FnDef {
        name: String,
        args: Vec<(String, Type)>,
        ret: Type,
        body: Vec<Stmt>,
    },
    StructDef {
        name: String,
        args: Vec<(String, Type)>,
    },
    // EnumDef {
    //     name: String,
    //     args: Vec<(String, Type)>,
    // },
    GlobalVarDecl {
        name: String,
        typ: Option<Type>,
        expr: Expr,
    },
}

#[derive(Clone, Debug)]
pub enum Stmt {
    ForStmt {
        init: Box<Stmt>,
        cond: Box<Expr>,
        after: Box<Stmt>,
        body: Vec<Stmt>,
    },
    WhileStmt {
        cond: Box<Expr>,
        body: Vec<Stmt>,
    },
    ExprStmt(Vec<Expr>),
    VarDecl {
        name: String,
        typ: Option<Type>,
        expr: Expr,
    },
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

    Break,
    Continue,

    Ret(Box<Expr>),

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
                expr: {
                    drop(expr);
                    Box::new(new_expr)
                },
                name,
            },
            Expr::FnCall { expr, args } => Expr::FnCall {
                expr: {
                    drop(expr);
                    Box::new(new_expr)
                },
                args,
            },
            Expr::Index(_, e) => Expr::Index(Box::new(new_expr), e),
            _ => Expr::ParserNone,
        }
    }
}

type UnaryOpFn = fn(Box<Expr>) -> Expr;
type BinaryOpFn = fn(Box<Expr>, Box<Expr>) -> Expr;

#[derive(Clone, Debug)]
pub enum Type {
    Int,
    Float,
    Char,
    Void,
    Custom(String),
    Array(Box<Type>),
    Pointer(Box<Type>),
    // Compund(),
}

type UnaryTypeFn = fn(Box<Type>) -> Type;

pub fn type_parser<'a, I>() -> impl Parser<'a, I, Type, extra::Err<Rich<'a, Token<'a>>>> + Clone
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let base_type = select! {
        Token::Int => Type::Int,
        Token::Float => Type::Float,
        Token::Char => Type::Char,
        Token::Void => Type::Void,
        Token::Name(s) => Type::Custom(s)
    };

    let ref_type = select! {
        Token::Ampersand => Type::Pointer as UnaryTypeFn,
        // Token::Asterisk => Type::Pointer as UnaryTypeFn
    };

    // let typ = recursive(|_| {
    let refer = ref_type
        .repeated()
        .foldr(base_type, |op, rhs| op(Box::new(rhs)));

    let array = refer.foldl(
        just(Token::LeftBracket)
            .ignore_then(just(Token::RightBracket))
            .repeated(),
        |typ, _| Type::Array(Box::new(typ)),
    );
    // array
    // });

    // remove if changing typ to include more types
    let typ = array;

    typ
}

pub fn stmt_parser<'a, I>() -> impl Parser<'a, I, Stmt, extra::Err<Rich<'a, Token<'a>>>> + Clone
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    println!("expr outer");
    let literal = select! {
        Token::IntLit(s) => Expr::IntLit(s),
        Token::FloatLit(s) => Expr::FloatLit(s),
        Token::StringLit(s) => Expr::StringLit(s),
        Token::CharLit(s) => Expr::CharLit(s)
    }
    .labelled("literal");

    let unary_op = select! {
        Token::Minus => Expr::Neg as UnaryOpFn,
        Token::Ampersand => Expr::Ref as UnaryOpFn,
        Token::Asterisk => Expr::Deref as UnaryOpFn,
    }
    .labelled("unary");

    let bin_op_strong = select! {
        Token::Asterisk => Expr::Mult as BinaryOpFn,
        Token::Slash => Expr::Div as BinaryOpFn,
        Token::Percent => Expr::Mod as BinaryOpFn,
    }
    .labelled("binary_strong");

    let bin_op_weak = select! {
        Token::Plus => Expr::Add as BinaryOpFn,
        Token::Minus => Expr::Sub as BinaryOpFn,
    }
    .labelled("binary_weak");

    let comp_strong = select! {
        Token::LessEqual => Expr::Leq as BinaryOpFn,
        Token::Less => Expr::Lt as BinaryOpFn,
        Token::GreaterEqual => Expr::Geq as BinaryOpFn,
        Token::Greater => Expr::Gt as BinaryOpFn,
    }
    .labelled("comp_strong");

    let comp_weak = select! {
        Token::EqualEqual => Expr::Eq as BinaryOpFn,
        Token::NotEqual => Expr::Neq as BinaryOpFn,
    };

    let assign = select! {
        Token::Equal => Expr::Assign as BinaryOpFn
    };

    let var = select! {Token::Name(n) => Expr::Var(n)}.labelled("var");
    let name = select! {Token::Name(n) => n}.labelled("name");
    let null = select! {Token::Null => Expr::Null}.labelled("null");

    // C operator precedence:
    // https://en.cppreference.com/w/c/language/operator_precedence

    // TODO: With new parsers, this might be easier to do some other way w/o replace_parse_temp
    let mut stmt = Recursive::declare();

    let expr = recursive(|expr| {
        let inner = expr
            .clone()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen));
        let ret = just(Token::Ret).ignore_then(expr.clone());

        let atom = literal.or(var).or(null).or(ret).or(inner);

        let stmt_list = stmt
            .clone()
            .separated_by(just(Token::SemiColon))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LeftBrace), just(Token::RightBrace));

        let subscription = expr
            .clone()
            .delimited_by(just(Token::LeftBracket), just(Token::RightBracket))
            .map(|r| Expr::Index(Box::new(Expr::ParserNone), Box::new(r)));

        let func_call = expr
            .clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen))
            .map(|rs| Expr::FnCall {
                expr: Box::new(Expr::ParserNone),
                args: rs.into_iter().map(|x| Box::new(x)).collect(),
            });

        let member_access = just(Token::Dot).ignore_then(name).map(|rhs| Expr::Member {
            expr: Box::new(Expr::ParserNone),
            name: rhs,
        });

        let first = atom.clone().foldl(
            choice((member_access, func_call, subscription)).repeated(),
            |exp, firs| firs.replace_parse_temp(exp),
        );

        // let iff = just(Token::If)
        //     .ignore_then(expr.clone())
        //     .then(stmt_list.clone())
        //     .then(
        //         just(Token::Else)
        //             .ignore_then(just(Token::If))
        //             .ignore_then(expr.clone())
        //             .then(stmt_list.clone())
        //             .repeated()
        //             .collect::<Vec<_>>(),
        //     )
        //     .then(just(Token::Else).ignore_then(stmt_list.clone()).or_not())
        //     .map(|(((exp, l1), l2a), ol3)| Expr::If {
        //         iff: (Box::new(exp), l1),
        //         elseifs: l2a.into_iter().map(|(e, vs)| (Box::new(e), vs)).collect(),
        //         elsee: ol3.unwrap_or(vec![]),
        //     });

        let end = first.pratt((
            prefix(8, unary_op, |op: UnaryOpFn, rhs| op(Box::new(rhs))),
            infix(left(7), bin_op_strong, |lhs, op: BinaryOpFn, rhs| {
                op(Box::new(lhs), Box::new(rhs))
            }),
            infix(left(6), bin_op_weak, |lhs, op: BinaryOpFn, rhs| {
                op(Box::new(lhs), Box::new(rhs))
            }),
            infix(left(5), comp_strong, |lhs, op: BinaryOpFn, rhs| {
                op(Box::new(lhs), Box::new(rhs))
            }),
            infix(left(4), comp_weak, |lhs, op: BinaryOpFn, rhs| {
                op(Box::new(lhs), Box::new(rhs))
            }),
            infix(left(3), assign, |lhs, op: BinaryOpFn, rhs| {
                op(Box::new(lhs), Box::new(rhs))
            }),
        ));

        end
    });

    // let name = select! {Token::Name(n) => n};
    stmt.define({
        let expr_stmt = expr
            .clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .map(|e| Stmt::ExprStmt(e));

        let var_decl = name
            .then(just(Token::Colon).ignore_then(type_parser()).or_not())
            .then_ignore(just(Token::ColEqual))
            .then(expr.clone())
            .map(|((name, typ), expr)| Stmt::VarDecl { name, typ, expr });

        let for_stmt = just(Token::For)
            .ignore_then(expr_stmt.clone())
            .then_ignore(just(Token::SemiColon))
            .then(expr.clone())
            .then_ignore(just(Token::SemiColon))
            .then(expr_stmt.clone())
            .then(
                stmt.clone()
                    .separated_by(just(Token::SemiColon))
                    .collect::<Vec<_>>(),
            )
            .map(|(((i, c), a), b)| Stmt::ForStmt {
                init: Box::new(i),
                cond: Box::new(c),
                after: Box::new(a),
                body: b,
            });

        var_decl.or(expr_stmt).or(for_stmt)
    });

    stmt
}

// pub fn stmt_parser<'a, I>() -> impl Parser<'a, I, Stmt, extra::Err<Rich<'a, Token<'a>>>> + Clone
// where
//     I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
// {

//     stmt
// }

pub fn parser<'a, I>() -> impl Parser<'a, I, File, extra::Err<Rich<'a, Token<'a>>>> + Clone
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let name = select! {Token::Name(n) => n}.labelled("name");

    let name_type_list = name
        .then_ignore(just(Token::Colon))
        .then(type_parser())
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>();

    let fn_def = just(Token::Fn)
        .ignore_then(name)
        .then(
            name_type_list
                .clone()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
        )
        .then(type_parser())
        .then(
            stmt_parser()
                .separated_by(just(Token::SemiColon))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
        )
        .map(|(((name, args), ret), body)| OuterStmt::FnDef {
            name,
            args,
            ret,
            body,
        });

    // Temporary
    fn_def.map(|f| File {
        includes: vec![],
        stmts: vec![
            // OuterStmt::
            f,
            // OuterStmt::GlobalVarDecl {
            //     name: "asd".to_string(),
            //     typ: None,
            //     expr: f,
            // },
        ],
    })
}

// pub fn parser<'a, I>() -> impl Parser<'a, I, File, extra::Err<Rich<'a, Token<'a>>>>
// where
//     I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
// {
//     // pub fn parser() -> impl Parser<Token, File, Error = Simple<Token>> {
//     let literal = select! {
//         Token::IntLit(s) => Expr::IntLit(s),
//         Token::FloatLit(s) => Expr::FloatLit(s),
//         Token::StringLit(s) => Expr::StringLit(s),
//         Token::CharLit(s) => Expr::CharLit(s)
//     };

//     let unary_op = select! {
//         Token::Minus => Expr::Neg as UnaryOpFn,
//         Token::Ampersand => Expr::Ref as UnaryOpFn,
//         Token::Asterisk => Expr::Deref as UnaryOpFn,
//     };

//     let bin_op_strong = select! {
//         Token::Asterisk => Expr::Mult as BinaryOpFn,
//         Token::Slash => Expr::Div as BinaryOpFn,
//         Token::Percent => Expr::Mod as BinaryOpFn,
//     };

//     let bin_op_weak = select! {
//         Token::Plus => Expr::Add as BinaryOpFn,
//         Token::Minus => Expr::Sub as BinaryOpFn,
//     };

//     let comp_strong = select! {
//         Token::LessEqual => Expr::Leq as BinaryOpFn,
//         Token::Less => Expr::Lt as BinaryOpFn,
//         Token::GreaterEqual => Expr::Geq as BinaryOpFn,
//         Token::Greater => Expr::Gt as BinaryOpFn,
//     };

//     // let comp_weak = select! {
//     //     Token::EqualEqual => Expr::Eq as BinaryOpFn,
//     //     Token::NotEqual => Expr::Neq as BinaryOpFn,
//     // };

//     // let assign = select! {
//     //     Token::Equal => Expr::Assign as BinaryOpFn
//     // };

//     let var = select! {Token::Name(n) => Expr::Var(n)};
//     let name = select! {Token::Name(n) => n};
//     let null = select! {Token::Null => Expr::Null};

//     let base_type = select! {
//         Token::Int => Type::Int,
//         Token::Float => Type::Float,
//         Token::Char => Type::Char,
//         Token::Void => Type::Void,
//         Token::Name(s) => Type::Custom(s)
//     };

//     let ref_type = select! {
//         Token::Ampersand => Type::Pointer as UnaryTypeFn,
//         // Token::Asterisk => Type::Pointer as UnaryTypeFn
//     };

//     // let typ = recursive(|_| {
//     let refer = ref_type
//         .repeated()
//         .foldr(base_type, |op, rhs| op(Box::new(rhs)));

//     let array = refer.foldl(
//         just(Token::LeftBracket)
//             .ignore_then(just(Token::RightBracket))
//             .repeated(),
//         |typ, _| Type::Array(Box::new(typ)),
//     );
//     // array
//     // });

//     // remove if changing typ to include more types
//     let typ = array;

//     // Hack to get expr in this scope
//     let mut expr_maybe = None;
//     let stmt = recursive(|stmt| {
//         let expr = recursive(|expr| {
//             let inner = expr
//                 .clone()
//                 .delimited_by(just(Token::LeftParen), just(Token::RightParen));

//             let ret = just(Token::Ret).ignore_then(expr.clone());

//             let atom = literal.or(var).or(null).or(ret).or(inner);

//             // let stmt_list = stmt
//             //     .clone()
//             //     .separated_by(just(Token::SemiColon))
//             //     .collect::<Vec<_>>()
//             //     .delimited_by(just(Token::LeftBrace), just(Token::RightBrace));

//             let subscription = expr
//                 .clone()
//                 .delimited_by(just(Token::LeftBracket), just(Token::RightBracket))
//                 .map(|r| Expr::Index(Box::new(Expr::ParserNone), Box::new(r)));

//             let func_call = expr
//                 .clone()
//                 .separated_by(just(Token::Comma))
//                 .collect::<Vec<_>>()
//                 .delimited_by(just(Token::LeftParen), just(Token::RightParen))
//                 .map(|rs| Expr::FnCall {
//                     expr: Box::new(Expr::ParserNone),
//                     args: rs.into_iter().map(|x| Box::new(x)).collect(),
//                 });

//             let member_access = just(Token::Dot).ignore_then(name).map(|rhs| Expr::Member {
//                 expr: Box::new(Expr::ParserNone),
//                 name: rhs,
//             });

//             // C operator precedence:
//             // https://en.cppreference.com/w/c/language/operator_precedence

//             // TODO: With new parsers, this might be easier to do some other way w/o replace_parse_temp
//             let first = atom.clone().foldl(
//                 member_access.or(func_call).or(subscription).repeated(),
//                 |exp, firs| firs.replace_parse_temp(exp),
//             );

//             let second = unary_op
//                 .repeated()
//                 .foldr(first.clone(), |op, rhs| op(Box::new(rhs)));

//             let third = second.clone().foldl(
//                 bin_op_strong.then(second.clone()).repeated(),
//                 |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
//             );

//             let fourth = third.clone().foldl(
//                 bin_op_weak.then(third.clone()).repeated(),
//                 |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
//             );

//             // skipping fifth for now
//             let fifth = fourth;

//             let sixth = fifth.clone().foldl(
//                 comp_strong.then(fifth.clone()).repeated(),
//                 |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
//             );

//             // let seventh = sixth.clone().foldl(
//             //     comp_weak.then(sixth.clone()).repeated(),
//             //     |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
//             // );

//             // // skipping to thirteenth
//             // let thirteenth = seventh.clone();
//             // let fourteenth = thirteenth.clone().foldl(
//             //     assign.then(thirteenth.clone()).repeated(),
//             //     |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
//             // );

//             // let iff = just(Token::If)
//             //     .ignore_then(expr.clone())
//             //     .then(stmt_list.clone())
//             //     .then(
//             //         just(Token::Else)
//             //             .ignore_then(just(Token::If))
//             //             .ignore_then(expr.clone())
//             //             .then(stmt_list.clone())
//             //             .repeated()
//             //             .collect::<Vec<_>>(),
//             //     )
//             //     .then(just(Token::Else).ignore_then(stmt_list.clone()).or_not())
//             //     .map(|(((exp, l1), l2a), ol3)| Expr::If {
//             //         iff: (Box::new(exp), l1),
//             //         elseifs: l2a.into_iter().map(|(e, vs)| (Box::new(e), vs)).collect(),
//             //         elsee: ol3.unwrap_or(vec![]),
//             //     });

//             // fourteenth //.or(iff)
//             sixth
//         });
//         // Hack to get expr in outer scope
//         expr_maybe = Some(expr.clone());

//         let expr_stmt = expr
//             .clone()
//             .separated_by(just(Token::Comma))
//             .collect::<Vec<_>>()
//             .map(|e| Stmt::ExprStmt(e));

//         let var_decl = name
//             .then(just(Token::Colon).ignore_then(typ.clone()).or_not())
//             .then_ignore(just(Token::ColEqual))
//             .then(expr.clone())
//             .map(|((name, typ), expr)| Stmt::VarDecl { name, typ, expr });

//         // let for_stmt = just(Token::For)
//         //     .ignore_then(expr_stmt.clone())
//         //     .then_ignore(just(Token::SemiColon))
//         //     .then(expr.clone())
//         //     .then_ignore(just(Token::SemiColon))
//         //     .then(expr_stmt.clone())
//         //     .then(
//         //         stmt.clone()
//         //             .separated_by(just(Token::SemiColon))
//         //             .collect::<Vec<_>>(),
//         //     )
//         //     .map(|(((i, c), a), b)| Stmt::ForStmt {
//         //         init: Box::new(i),
//         //         cond: Box::new(c),
//         //         after: Box::new(a),
//         //         body: b,
//         //     });

//         var_decl.or(expr_stmt)
//         // .or(for_stmt)
//     });
//     // Hack to get expr in this scope
//     let expr = expr_maybe.unwrap();

//     // let name_type_list = name
//     //     .then_ignore(just(Token::Colon))
//     //     .then(typ.clone())
//     //     .separated_by(just(Token::Comma))
//     //     .collect::<Vec<_>>();

//     // let fn_def = just(Token::Fn)
//     //     .ignore_then(name)
//     //     .then(
//     //         name_type_list
//     //             .clone()
//     //             .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
//     //     )
//     //     .then(typ.clone())
//     //     .then(
//     //         stmt.clone()
//     //             .separated_by(just(Token::SemiColon))
//     //             .collect::<Vec<_>>()
//     //             .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
//     //     )
//     //     .map(|(((name, args), ret), body)| OuterStmt::FnDef {
//     //         name,
//     //         args,
//     //         ret,
//     //         body,
//     //     });

//     // Temporary
//     expr.map(|f| File {
//         includes: vec![],
//         stmts: vec![OuterStmt::GlobalVarDecl {
//             name: "asd".to_string(),
//             typ: None,
//             expr: f,
//         }],
//     })
// }
