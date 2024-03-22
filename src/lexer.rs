use logos::Logos;

#[derive(Logos, Debug, PartialEq, Eq, Hash, Clone)]
#[logos(skip r"[ \t\f\n]+")]
pub enum Token {
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

    #[token("for")]
    For,
    #[token("while")]
    While,
    #[token("struct")]
    Struct,
    #[token("enum")]
    Enum,
    #[token("interface")]
    Interface,
    #[token("fn")]
    Fn,
    #[token("ret")]
    Ret,
    #[token("tail")]
    Tail,

    #[token("null")]
    Null,
    #[token("void")]
    Void,
    #[token("int")]
    Int,
    #[token("char")]
    Char,

    #[regex(r"///.*\n", |lex| lex.slice().to_owned())]
    DocString(String),
    #[regex(r"//.*\n", |lex| lex.slice().to_owned())]
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
