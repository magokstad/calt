use std::fmt;

use logos::{Lexer, Logos, Skip};

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexingError {
    // InvalidInteger(String),
    #[default]
    UnknownSymbol,
}

fn update_extra<'a>(lex: &mut Lexer<'a, Token<'a>>) {
    let (_pf, pt, pl) = lex.extras.last().unwrap_or(&(0, 1, 1));
    lex.extras.push((*pt, lex.span().end, pl + 1));
}

fn newline_callback<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Skip {
    update_extra(lex);
    Skip
}

fn comment_callback<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Skip {
    update_extra(lex);
    lex.slice().to_owned();
    Skip
}

#[derive(Logos, Debug, PartialEq, Eq, Hash, Clone)]
#[logos(skip r"[ \t\f]+")]
#[logos(error = LexingError)]
#[logos(extras = Vec<(usize, usize, usize)>)]
pub enum Token<'a> {
    Error(&'a str),

    #[token("\n", newline_callback)]
    Newline,

    #[token("=")]
    Equal,
    #[token(":=")]
    ColEqual,

    #[token("==")]
    EqualEqual,
    #[token("!=")]
    NotEqual,
    #[token("<=")]
    LessEqual,
    #[token(">=")]
    GreaterEqual,
    #[token("<")]
    Less,
    #[token(">")]
    Greater,

    #[token("{")]
    LeftBrace,
    #[token("[")]
    LeftBracket,
    #[token("(")]
    LeftParen,
    #[token("}")]
    RightBrace,
    #[token("]")]
    RightBracket,
    #[token(")")]
    RightParen,

    #[token(":")]
    Colon,
    #[token(";")]
    SemiColon,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,

    #[token("!")]
    Exclamation,
    #[token("?")]
    Question,
    #[token("&")]
    Ampersand,
    #[token("*")]
    Asterisk,
    #[token("/")]
    Slash,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("%")]
    Percent,

    #[token("++")]
    Increment,
    #[token("--")]
    Decrement,
    #[token("+=")]
    IncrementBy,
    #[token("-=")]
    DecrementBy,

    #[token("for")]
    For,
    #[token("while")]
    While,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("struct")]
    Struct,
    #[token("enum")]
    Enum,
    #[token("ifc")]
    #[token("interface")]
    Interface,
    #[token("fn")]
    Fn,
    #[token("ret")]
    Ret,
    #[token("tail")]
    #[token("tfn")]
    Tail,

    #[token("null")]
    Null,
    #[token("void")]
    Void,
    #[token("int")]
    Int,
    #[token("float")]
    Float,
    #[token("char")]
    Char,

    #[regex(r"///.*\n", comment_callback)]
    DocString,
    // DocString(String),
    #[regex(r"//.*\n", comment_callback)]
    Comment,
    // Comment(String),
    #[regex(r"[A-Za-z][A-Za-z0-9_]*", |lex| lex.slice().to_owned())]
    Name(String),

    // Consider keeping as string?
    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)(?:[eE][+-]?\d+)?", |lex| lex.slice().to_owned())]
    FloatLit(String),

    // Consider keeping as string?
    #[regex(r"-?(?:0b[10]*|0x[AaBbCcDdEeFf\d]*|0|[1-9]\d*)", |lex| lex.slice().to_owned())]
    IntLit(String),

    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, |lex| lex.slice().to_owned())]
    StringLit(String),

    #[regex(r"'([^'\\]|\\.)'", |lex| lex.slice().to_owned())]
    CharLit(String),
}
impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Error(s) => write!(f, "<error:{s}>"),
            Token::Newline => write!(f, "\n"),
            Token::Equal => write!(f, "="),
            Token::ColEqual => write!(f, ":="),
            Token::EqualEqual => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::LessEqual => write!(f, "<="),
            Token::GreaterEqual => write!(f, ">="),
            Token::Less => write!(f, "<"),
            Token::Greater => write!(f, ">"),
            Token::LeftBrace => write!(f, "{{"),
            Token::LeftBracket => write!(f, "["),
            Token::LeftParen => write!(f, "("),
            Token::RightBrace => write!(f, "}}"),
            Token::RightBracket => write!(f, "]"),
            Token::RightParen => write!(f, ")"),
            Token::Colon => write!(f, ":"),
            Token::SemiColon => write!(f, ";"),
            Token::Dot => write!(f, "."),
            Token::Comma => write!(f, ","),
            Token::Exclamation => write!(f, "!"),
            Token::Question => write!(f, "?"),
            Token::Ampersand => write!(f, "&"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Percent => write!(f, "%"),
            Token::Increment => write!(f, "++"),
            Token::Decrement => write!(f, "--"),
            Token::IncrementBy => write!(f, "+="),
            Token::DecrementBy => write!(f, "-="),
            Token::For => write!(f, "for"),
            Token::While => write!(f, "while"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Struct => write!(f, "struct"),
            Token::Enum => write!(f, "enum"),
            Token::Interface => write!(f, "ifc"),
            Token::Fn => write!(f, "fn"),
            Token::Ret => write!(f, "ret"),
            Token::Tail => write!(f, "tfn"),
            Token::Null => write!(f, "null"),
            Token::Void => write!(f, "void"),
            Token::Int => write!(f, "int"),
            Token::Float => write!(f, "float"),
            Token::Char => write!(f, "char"),
            Token::DocString => write!(f, "<docstring>"),
            Token::Comment => write!(f, "<comment>"),
            Token::Name(s) => write!(f, "{s}"),
            Token::FloatLit(s) => write!(f, "{s}"),
            Token::IntLit(s) => write!(f, "{s}"),
            Token::StringLit(s) => write!(f, "{s}"),
            Token::CharLit(s) => write!(f, "{s}"),
        }
    }
}
