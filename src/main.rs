use chumsky::Parser;
use lexer::Token;
use logos::Logos;

use crate::lexer::LexingError;

mod lexer;
mod parser;

fn main() {
    let lex = Token::lexer(include_str!("../design/initial.calt"));
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
    // for res in lex {
    //     println!("{res:?}");
    // }
    println!("Ok");
    let parse = parser::parser().parse(tokens);
    println!("{:?}", parse);
}
