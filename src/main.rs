use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{
    input::{Input, Stream},
    Parser,
};
use lexer::Token;
use logos::Logos;

use crate::codegen::codegen;

mod codegen;
mod lexer;
mod parser;
mod semantic_analysis;

fn main() {
    let src = include_str!("../design/initial.calt");
    let lex = Token::lexer(src);
    let token_iter = lex.spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(_) => (Token::Error("generic"), span.into()),
    });

    let token_stream = Stream::from_iter(token_iter).spanned((src.len()..src.len()).into());
    let parse = parser::parser().parse(token_stream);
    let (ast, errs) = parse.into_output_errors();
    println!("{:#?}", ast);
    for err in errs {
        Report::build(ReportKind::Error, (), err.span().start)
            .with_code(3)
            .with_message(err.to_string())
            .with_label(
                Label::new(err.span().into_range())
                    .with_message(err.reason().to_string())
                    .with_color(Color::Red),
            )
            .finish()
            .eprint(Source::from(src))
            .unwrap();
    }
    if let Some(ast) = ast {
        codegen(ast);
    }
}
