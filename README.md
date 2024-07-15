# Calt (name WIP)

A compiler in Rust for a custom programming language inspired by C, but with modern syntax similar to Go.
The project is in early development, but lexing, parsing and static analysis of basic language features works.

### Idea

The idea is to use the Logos crate for lexing, the Chumsky crate for parsing, then do custom static analysis, and lastly 
code generation from the AST into [QBE intermediate language](https://c9x.me/compile/doc/il.html). 
The [QBE compiler backend](https://c9x.me/compile/) then handles the rest of the compilation into machine code. Making a 
custom compiler backend could also be an option, but is usually complicated enough to be their own large projects 
(i.e [LLVM](https://llvm.org/)).

A sample file used for early testing can be found in `design/initial.calt` which also shows the syntax of the language.

### Getting started

Command line arguments are planned to be supported with the [CLAP crate](https://docs.rs/clap/latest/clap/), 
but is not the main priority as of now. Simply clone the github repo and run with `cargo run` 
