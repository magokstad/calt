use crate::{parser::File, semantic_analysis::semantic_analysis};

pub fn codegen(f: File) -> String {
    let mut x = f;
    let errs = semantic_analysis(&mut x);
    println!("{:#?}", errs);
    String::new()
}
