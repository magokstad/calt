use crate::{parser::File, semantic_analysis::semantic_analysis};

pub fn codegen(f: File) -> String {
    let mut x = f;
    semantic_analysis(&mut x);
    String::new()
}
