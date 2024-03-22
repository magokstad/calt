use lexer::Token;
use logos::Logos;

mod lexer;
mod parser;

fn main() {
    let lex = Token::lexer(include_str!("../design/initial.calt"));
    for res in lex {
        println!("{res:?}");
    }
}
