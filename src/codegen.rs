use crate::{parser::File, semantic_analysis::semantic_analysis};

pub fn codegen(f: File) -> String {
    let f = semantic_analysis(f);
    String::new()
}
