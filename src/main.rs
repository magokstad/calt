use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::Parser;
use lexer::Token;
use logos::Logos;

use crate::lexer::LexingError;

mod lexer;
mod parser;

fn main() {
    let src = include_str!("../design/initial.calt");
    let lex = Token::lexer(src);
    let extras = lex.extras.clone();
    let tokens = lex
        .spanned()
        .filter_map(|(t, s)| match t {
            Ok(t) => Some(t),
            Err(e) => {
                let mut line = 0;
                for i in &extras {
                    if i.0 >= s.start && i.1 < s.end {
                        line = i.2;
                        break;
                    }
                }
                match e {
                    LexingError::UnknownSymbol => {
                        println!("Unknown Symbol at {}:{}", line, s.start)
                    }
                };
                None
            }
        })
        .collect::<Vec<Token>>();

    let parse = parser::parser().parse(tokens);
    match parse {
        Ok(p) => println!("{:#?}", parse),
        Err(e) => println!("{}", e),
    };
}
