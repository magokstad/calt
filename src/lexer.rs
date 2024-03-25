use logos::{Lexer, Logos, Skip};

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexingError {
    // InvalidInteger(String),
    #[default]
    UnknownSymbol,
}

fn update_extra(lex: &mut Lexer<Token>) {
    let (_pf, pt, pl) = lex.extras.last().unwrap_or(&(0, 1, 1));
    lex.extras.push((*pt, lex.span().end, pl + 1));
}

fn newline_callback(lex: &mut Lexer<Token>) -> Skip {
    update_extra(lex);
    Skip
}

fn comment_callback(lex: &mut Lexer<Token>) -> String {
    update_extra(lex);
    lex.slice().to_owned()
}

#[derive(Logos, Debug, PartialEq, Eq, Hash, Clone)]
#[logos(skip r"[ \t\f]+")]
#[logos(error = LexingError)]
#[logos(extras = Vec<(usize, usize, usize)>)]
pub enum Token {
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
    #[token("char")]
    Char,

    #[regex(r"///.*\n", comment_callback)]
    DocString(String),
    #[regex(r"//.*\n", comment_callback)]
    Comment(String),

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
