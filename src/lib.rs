#![feature(rustc_private)]
#![feature(collections, str_char)]
#![feature(plugin)]
#![plugin(peg_syntax_ext)]

extern crate syntax;

mod parser;

pub mod types;
pub mod error;
pub mod reader;
