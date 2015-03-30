#![feature(rustc_private)]
#![feature(collections, str_char)]
#![feature(plugin)]
#![plugin(peg_syntax_ext)]

extern crate syntax;

// TODO Think whether this should really be public
// This is only pub to avoid compiler warnings for now
pub mod parser;
pub mod reader;
pub mod error;

// TODO Implement wrapper methods around parser and expose a nice public API
