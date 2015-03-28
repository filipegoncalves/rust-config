#![feature(rustc_private)]
#![feature(collections, str_char)]
#![feature(plugin)]
#![plugin(peg_syntax_ext)]

extern crate syntax;

pub mod parser;

// TODO Implement wrapper methods around parser and expose a nice public API
