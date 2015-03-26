#![feature(rustc_private)]
#![feature(collections, str_char)]
#![feature(plugin)]
#![plugin(peg_syntax_ext)]

extern crate syntax;

pub mod parser;

#[cfg(test)] use parser::{SettingsList, Setting, Value, ScalarValue, ArrayValue, ListValue};

#[test]
fn it_works() {
    let mut expected = SettingsList::new();
    expected.insert("my_test".to_string(), Setting::new("my_test".to_string(), Value::Svalue(ScalarValue::Boolean(true))));
    assert_eq!(parser::parse("my_test = true;"), Ok(expected));
}
