//! The core parser module.
//! Upon successfully parsing a configuration, a `Config` is created. Conceptually, a
//! `Config` consits of a `SettingsList`, which is a map that binds a `Setting` name to a `Value`.
//!
//! This map is the basis for the rest of the library. The public library API is nothing more
//! than a simple set of wrappers to make it easier to manage a the `SettingsList` inside
//! a `Config`.
//!
//! When a parse call is invoked, a settings list is built as input is read. It is not expected that
//! library users manipulate or otherwise deal directly with these internal data structures.
//!
//! Most of the setting types allowed in a configuration will pretty much map to either a Rust
//! primitive type or a container.
//!
//! # Examples
//! This example shows how to create a settings list and store a `Boolean` scalar value named
//! `my_setting` with the boolean value `true`. This is how the parser works internally.
//!
//! The first step is to create a new, empty settings list:
//!
//! ```
//! use config::types::SettingsList;
//! # use config::types::ScalarValue;
//! # use config::types::Value;
//! # use config::types::Setting;
//!
//! let mut my_settings_list = SettingsList::new();
//! # let setting_name = "my_setting".to_string();
//! # let a_scalar = ScalarValue::Boolean(true);
//! # let setting_value = Value::Svalue(a_scalar);
//! # let my_setting = Setting::new(setting_name.clone(), setting_value);
//! # my_settings_list.insert(setting_name, my_setting);
//! ```
//!
//! Next, we define the setting name as `my_setting`:
//!
//! ```
//! # use config::types::SettingsList;
//! # use config::types::ScalarValue;
//! # use config::types::Value;
//! # use config::types::Setting;
//!
//! # let mut my_settings_list = SettingsList::new();
//! let setting_name = "my_setting".to_string();
//! # let a_scalar = ScalarValue::Boolean(true);
//! # let setting_value = Value::Svalue(a_scalar);
//! # let my_setting = Setting::new(setting_name.clone(), setting_value);
//! # my_settings_list.insert(setting_name, my_setting);
//! ```
//!
//! Then, we create a boolean scalar value holding `true`:
//!
//! ```
//! # use config::types::SettingsList;
//! use config::types::ScalarValue;
//! # use config::types::Value;
//! # use config::types::Setting;
//!
//! # let mut my_settings_list = SettingsList::new();
//! # let setting_name = "my_setting".to_string();
//! let a_scalar = ScalarValue::Boolean(true);
//! # let setting_value = Value::Svalue(a_scalar);
//! # let my_setting = Setting::new(setting_name.clone(), setting_value);
//! # my_settings_list.insert(setting_name, my_setting);
//! ```
//!
//! ... and we wrap it in a `Value`, because settings store generic values:
//!
//! ```
//! # use config::types::SettingsList;
//! # use config::types::ScalarValue;
//! use config::types::Value;
//! # use config::types::Setting;
//!
//! # let mut my_settings_list = SettingsList::new();
//! # let setting_name = "my_setting".to_string();
//! # let a_scalar = ScalarValue::Boolean(true);
//! let setting_value = Value::Svalue(a_scalar);
//! # let my_setting = Setting::new(setting_name.clone(), setting_value);
//! # my_settings_list.insert(setting_name, my_setting);
//! ```
//!
//! And finally, we create the new setting:
//!
//! ```
//! # use config::types::SettingsList;
//! # use config::types::ScalarValue;
//! # use config::types::Value;
//! use config::types::Setting;
//!
//! # let mut my_settings_list = SettingsList::new();
//! # let setting_name = "my_setting".to_string();
//! # let a_scalar = ScalarValue::Boolean(true);
//! # let setting_value = Value::Svalue(a_scalar);
//! let my_setting = Setting::new(setting_name.clone(), setting_value);
//! # my_settings_list.insert(setting_name, my_setting);
//! ```
//! ... and insert it into the settings list:
//!
//! ```
//! # use config::types::SettingsList;
//! # use config::types::ScalarValue;
//! # use config::types::Value;
//! # use config::types::Setting;
//!
//! # let mut my_settings_list = SettingsList::new();
//! # let setting_name = "my_setting".to_string();
//! # let a_scalar = ScalarValue::Boolean(true);
//! # let setting_value = Value::Svalue(a_scalar);
//! # let my_setting = Setting::new(setting_name.clone(), setting_value);
//! my_settings_list.insert(setting_name, my_setting);
//! ```
//!
//! Here's the complete example:
//! ```
//! use config::types::SettingsList;
//! use config::types::ScalarValue;
//! use config::types::Value;
//! use config::types::Setting;
//!
//! let mut my_settings_list = SettingsList::new();
//! let setting_name = "my_setting".to_string();
//! let a_scalar = ScalarValue::Boolean(true);
//! let setting_value = Value::Svalue(a_scalar);
//! let my_setting = Setting::new(setting_name.clone(), setting_value);
//! my_settings_list.insert(setting_name, my_setting);
//! ```
//!
//! As a final step, the parser creates a `Config` out of the `SettingsList`.
//! This is what the user sees and interacts with. It is as simple as:
//!
//! ```
//! # use config::types::SettingsList;
//! # use config::types::ScalarValue;
//! # use config::types::Value;
//! # use config::types::Setting;
//! use config::types::Config;
//!
//! # let mut my_settings_list = SettingsList::new();
//! # let setting_name = "my_setting".to_string();
//! # let a_scalar = ScalarValue::Boolean(true);
//! # let setting_value = Value::Svalue(a_scalar);
//! # let my_setting = Setting::new(setting_name.clone(), setting_value);
//! # my_settings_list.insert(setting_name, my_setting);
//! let my_config = Config::new(my_settings_list);
//! ```
//!

use std::str;
use std::str::from_utf8;
use std::str::FromStr;
use std::iter;

use types::{SettingsList, Setting, Value, ScalarValue, ArrayValue, ListValue, Config};

use nom::{alpha, alphanumeric, digit, multispace, not_line_ending};
use nom::IResult;
use nom::Err::Code;
use nom::IResult::*;

pub type ParseError = u32;

/// Parses a configuration from a `&str`.
/// An error is returned in case of syntax error.
pub fn parse(config: &str) -> Result<Config, ParseError> {
    // TODO Descriptive errors
    match conf(&config.as_bytes()[..]) {
        Done(_, c) => Ok(c),
        _          => Err(0)
    }
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~ The parsers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// ~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~ Top-level parser ~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~
named!(conf<&[u8], Config>,
       chain!(
           c: alt!(map_res!(settings_list,
                            |l| -> Result<Config, ()> { Ok(Config::new(l)) }) |
                   map_res!(blanks,
                            |_| -> Result<Config, ()> { Ok(Config::new(SettingsList::new())) })) ~
           eof,
           || { c }));

// ~~~~~~~~~~~~~~~~~~~~~
// ~~~ Settings List ~~~
// ~~~~~~~~~~~~~~~~~~~~~
named!(settings_list<&[u8], SettingsList>,
       map_res!(settings_list_elems,
                |s: Vec<Setting>| -> Result<SettingsList, ()> {
                    let mut res = SettingsList::new();
                    for setting in s.into_iter() {
                        res.insert(setting.name.clone(), setting);
                    }
                    Ok(res) }));

named!(settings_list_elems<&[u8], Vec<Setting> >, many1!(setting));

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~ Setting parser and auxiliary parsers ~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
named!(setting<&[u8], Setting>,
       chain!(
           blanks? ~
           name: setting_name ~
           blanks? ~
           alt!(tag!(":") | tag!("=")) ~
           blanks? ~
           v: value ~
           blanks? ~
           tag!(";") ~
           blanks?,
           || { Setting::new(name, v) }));

// Matches a setting name of the form [a-zA-Z][-a-zA-Z0-9_]*
named!(setting_name<&[u8], String>,
       chain!(
           h: map_res!(alpha, from_utf8) ~
           t: many0!(map_res!(alt!(tag!("-") | tag!("_") | alphanumeric), from_utf8)),
           || {
               t.into_iter().fold(h.to_string(), |mut accum, slice| {
                   accum.push_str(slice);
                   accum
               })}));

// ~~~~~~~~~~~~~~~~~~~~~
// ~~~ Values parser ~~~
// ~~~~~~~~~~~~~~~~~~~~~
named!(value<&[u8], Value>,
       alt!(
           map_res!(scalar_value, |sv| -> Result<Value, ()> { Ok(Value::Svalue(sv)) }) |
           array |
           list |
           map_res!(group, |g| -> Result<Value, ()> { Ok(Value::Group(g)) })));

// ~~~~~~~~~~~~~~~~~~~~~
// ~~~ Scalar Values ~~~
// ~~~~~~~~~~~~~~~~~~~~~
named!(scalar_value<&[u8], ScalarValue>,
       alt!(
           boolean_scalar_value |
           flt_scalar_value |
           int_scalar_value |
           str_scalar_value));

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~ Array parser and auxiliary parsers ~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
named!(array<&[u8], Value>,
       alt!(
           chain!(tag!("[") ~ blanks? ~ tag!("]"),
                  || { Value::Array(Vec::new()) })
           |
           chain!(
               tag!("[") ~
               e: boolean_array_elements ~
               tag!("]"),
               || { Value::Array(e) })
           |
           chain!(
               tag!("[") ~
               e: str_array_elements ~
               tag!("]"),
               || { Value::Array(e) })
           |
           chain!(
               tag!("[") ~
               e: flt64_array_elements ~
               tag!("]"),
               || { Value::Array(e) })
           |
           chain!(
               tag!("[") ~
               e: flt32_array_elements ~
               tag!("]"),
               || { Value::Array(e) })
           |
           chain!(
               tag!("[") ~
               e: int64_array_elements ~
               tag!("]"),
               || { Value::Array(e) })
           |
           chain!(
               tag!("[") ~
               e: int32_array_elements ~
               tag!("]"),
               || { Value::Array(e) })));

macro_rules! array_elems {
    ($name:ident, $parser:ident) => (
        named!($name<&[u8], ArrayValue>,
               chain!(
                   blanks? ~
                   first: $parser ~
                   rest:  many0!(chain!(
                                     blanks? ~
                                     tag!(",") ~
                                     blanks? ~
                                     v: $parser,
                                     || { v } )) ~
                   blanks?,
                   || {
                       let mut res = Vec::new();
                       res.push(Value::Svalue(first));
                       res.extend(rest.into_iter().map(|v| Value::Svalue(v)));
                       res
                      }));
        );
}

array_elems!(boolean_array_elements, boolean_scalar_value);
array_elems!(str_array_elements, str_scalar_value);
array_elems!(flt64_array_elements, flt64_scalar_value);
array_elems!(flt32_array_elements, flt32_scalar_value);
array_elems!(int64_array_elements, int64_scalar_value);
array_elems!(int32_array_elements, int32_scalar_value);

// ~~~~~~~~~~~~~
// ~~~ Lists ~~~
// ~~~~~~~~~~~~~
named!(list<&[u8], Value>,
       alt!(
           chain!(tag!("(") ~ blanks? ~ tag!(")"), || { Value::List(ListValue::new()) })
           |
           chain!(
               tag!("(") ~
                   blanks? ~
                   first: value ~
                   blanks? ~
                   rest: many0!(chain!(
                                    blanks? ~
                                    tag!(",") ~
                                    blanks? ~
                                    v: value,
                                    || { v })) ~
                   blanks? ~
                   tag!(")"),
                   || {
                       let mut res = ListValue::new();
                       res.push(first);
                       res.extend(rest.into_iter());
                       Value::List(res)
                      })));

// ~~~~~~~~~~~~~
// ~~~ Group ~~~
// ~~~~~~~~~~~~~
named!(group<&[u8], SettingsList>,
       chain!(
           tag!("{") ~
           l: settings_list ~
           tag!("}"),
           || { l }));              

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~ Boolean values parser and auxiliary parsers ~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
named!(boolean_scalar_value<&[u8], ScalarValue>, alt!(bool_true_value | bool_false_value));

named!(bool_true_value<&[u8], ScalarValue>,
       alt!(chain!(
               alt!(tag!("T") | tag!("t")) ~
               alt!(tag!("R") | tag!("r")) ~
               alt!(tag!("U") | tag!("u")) ~
               alt!(tag!("E") | tag!("e")),
               || { ScalarValue::Boolean(true) } )
            |
            chain!(
               alt!(tag!("Y") | tag!("y")) ~
               alt!(tag!("E") | tag!("e")) ~
               alt!(tag!("S") | tag!("s")),
               || { ScalarValue::Boolean(true) } )));

named!(bool_false_value<&[u8], ScalarValue>,
       alt!(chain!(
               alt!(tag!("F") | tag!("f")) ~
               alt!(tag!("A") | tag!("a")) ~
               alt!(tag!("L") | tag!("l")) ~
               alt!(tag!("S") | tag!("s")) ~
               alt!(tag!("E") | tag!("e")),
               || { ScalarValue::Boolean(false) } )
            |
            chain!(
               alt!(tag!("N") | tag!("n")) ~
               alt!(tag!("O") | tag!("o")),
               || { ScalarValue::Boolean(false) } )));

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~ String parser and auxiliary parsers ~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
named!(str_scalar_value<&[u8], ScalarValue>,
       chain!(
           strs: many1!(chain!(
                        s: string_literal ~
                        blanks?,
                        || { s })),
           || {
               ScalarValue::Str(
                   strs.into_iter().fold(String::new(), |mut accum, str_i| {
                       accum.push_str(&str_i[..]);
                       accum
                   }))}));

named!(not_escaped_seq<&[u8], &[u8]>, take_until_either!(&b"\\\""[..]));
named!(escaped_seq, alt!(tag!("\\r") | tag!("\\n") | tag!("\\t") | tag!("\\\"") | tag!("\\\\")));
named!(string_literal<&[u8], String>,
       chain!(
           tag!("\"") ~
           s: many0!(map_res!(alt!(escaped_seq | not_escaped_seq), from_utf8)) ~
           tag!("\""),
           || {
               str_lit(&s.into_iter().fold(String::new(),
                                           |mut accum, slice| {
                                               accum.push_str(slice);
                                               accum
                                           })[..])}));

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~ Integer values parser and auxiliary parsers ~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
named!(int_scalar_value<&[u8], ScalarValue>,
       alt!(int64_scalar_value | int32_scalar_value));

// Tentative parser to match Integer32 scalar values
// Returns an `Err` if `parse::<i32>()` fails
// [+-][0-9]+
named!(int32_scalar_value_tentative<&[u8], Result<i32, <i32 as FromStr>::Err> >,
       chain!(
           s: map_res!(alt!(tag!("+") | tag!("-")), from_utf8)? ~
           v: map_res!(digit, from_utf8),
           || {
               // Rust's parse::<i32>() and i64 do not accept a leading '+'
               let sign = if s.unwrap_or("+") == "+" { "" } else { "-" };
               (&format!("{}{}", sign, v)[..]).parse::<i32>()}));

// Tentative parser to match Integer64 scalar values
// Returns an `Err` if `parse::<i64>()` fails
// [+-][0-9]+L
named!(int64_scalar_value_tentative<&[u8], Result<i64, <i64 as FromStr>::Err> >,
       chain!(
           s: map_res!(alt!(tag!("+") | tag!("-")), from_utf8)? ~
           v: map_res!(digit, from_utf8) ~
           tag!("L"),
           || {
               // Rust's parse::<i32>() and i64 do not accept a leading '+'
               let sign = if s.unwrap_or("+") == "+" { "" } else { "-" };
               (&format!("{}{}", sign, v)[..]).parse::<i64>()}));

// Transforms a possible `Err` returned by `parse::<i32>()` into a parse `Error`
named!(int32_scalar_value<&[u8], ScalarValue>,
       map_res!(int32_scalar_value_tentative,
                |r: Result<i32, <i32 as FromStr>::Err>| {
                    r.map(|v| ScalarValue::Integer32(v))
                }));

// Transforms a possible `Err` returned by `parse::<i64>()` into a parse `Error`
named!(int64_scalar_value<&[u8], ScalarValue>,
       map_res!(int64_scalar_value_tentative,
                |r: Result<i64, <i64 as FromStr>::Err>| {
                    r.map(|v| ScalarValue::Integer64(v))
                }));

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~ Floating point values parser and auxiliary parsers ~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
named!(flt_scalar_value<&[u8], ScalarValue>,
       alt!(flt64_scalar_value | flt32_scalar_value));

// Auxiliary parser for floats with digits before .
// [0-9]+\.[0-9]*
named!(flt_base_w_digits_bef_dot<&[u8], (&str, &str)>,
       chain!(
           b: map_res!(digit, from_utf8) ~
           tag!(".") ~
           a: map_res!(digit, from_utf8)?,
           || { (b, a.unwrap_or("")) }));

// Auxiliary parser for floats with no digits before .
// \.[0-9]+
named!(flt_base_no_digits_bef_dot<&[u8], (&str, &str)>,
       chain!(
           tag!(".") ~
           a: map_res!(digit, from_utf8),
           || { ("", a) }));

// Auxiliary parser to match the base of a float
// [0-9]+\.[0-9]* | \.[0-9]+
named!(flt_base<&[u8], (&str, &str)>,
       alt!(flt_base_w_digits_bef_dot | flt_base_no_digits_bef_dot));

// Auxiliary parser to match the exponent of a float
// [eE][+-]?[0-9]+
named!(flt_exponent<&[u8], (&str, &str)>,
       chain!(
           alt!(tag!("e") | tag!("E")) ~
           s: map_res!(alt!(tag!("+") | tag!("-")), from_utf8)? ~
           v: map_res!(digit, from_utf8),
           || { (s.unwrap_or("+"), v) }));

// Tentative parser to match Floating32 scalar values
// Returns an `Err` if `parse::<f32>()` fails
// [+-]?([0-9]+\.[0-9]* | \.[0-9]+)([eE][+-]?[0-9]+)?
named!(flt32_scalar_value_tentative<&[u8], Result<f32, <f32 as FromStr>::Err> >,
       chain!(
           s: map_res!(alt!(tag!("+") | tag!("-")), from_utf8)? ~
           b: flt_base ~
           e: flt_exponent?,
           || {
               let (base_bef, base_after) = b;
               let (exp_sign, exp_val) = e.unwrap_or(("+", "0"));
               // Rust's parse::<f32>() and f64 do not accept a leading '+'
               let sign = if s.unwrap_or("+") == "+" { "" } else { "-" };
               (&format!("{}{}.{}e{}{}", sign, base_bef, base_after,
                         exp_sign, exp_val)[..]).parse::<f32>()}));

// Tentative parser to match Floating64 scalar values
// Returns an Err if parse::<f64>() fails
// [+-]?([0-9]+\.[0-9]* | \.[0-9]+)([eE][+-]?[0-9]+)?L
named!(flt64_scalar_value_tentative<&[u8], Result<f64, <f64 as FromStr>::Err> >,
       chain!(
           s: map_res!(alt!(tag!("+") | tag!("-")), from_utf8)? ~
           b: flt_base ~
           e: flt_exponent? ~
           tag!("L"),
           || {
               let (base_bef, base_after) = b;
               let (exp_sign, exp_val) = e.unwrap_or(("+", "0"));
               // Rust's parse::<f32>() and f64 do not accept a leading '+'
               let sign = if s.unwrap_or("+") == "+" { "" } else { "-" };
               (&format!("{}{}.{}e{}{}", sign, base_bef, base_after,
                         exp_sign, exp_val)[..]).parse::<f64>()}));

// Transforms a possible `Err` returned by `parse::<f32>()` into a parse `Error`
named!(flt32_scalar_value<&[u8], ScalarValue>,
       map_res!(flt32_scalar_value_tentative,
                |r: Result<f32, <f32 as FromStr>::Err>| {
                    r.map(|v| ScalarValue::Floating32(v))
                }));

// Transforms a possible `Err` returned by `parse::<f64>()` into a parse `Error`
named!(flt64_scalar_value<&[u8], ScalarValue>,
       map_res!(flt64_scalar_value_tentative,
                |r: Result<f64, <f64 as FromStr>::Err>| {
                    r.map(|v| ScalarValue::Floating64(v))
                }));

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~ Parser to ignore useless stuff: comments, new lines, ... ~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
named!(blanks,
       chain!(
           many0!(alt!(multispace | comment_one_line | comment_block)),
           || { &b""[..] }));

// Auxiliary parser to ignore newlines
// NOTE: In some cases, this parser is combined with others that use `not_line_ending`
//       However, `not_line_ending` won't match `\u{2028}` or `\u{2029}`
named!(eol,
       alt!(tag!("\r\n") | tag!("\n") | tag!("\u{2028}") | tag!("\u{2029}")));

// Auxiliary parser to ignore one-line comments
named!(comment_one_line,
       chain!(
           alt!(tag!("//") | tag!("#")) ~
           not_line_ending? ~
           alt!(eol | eof),
           || { &b""[..] }));

// Auxiliary parser to ignore block comments
named!(comment_block,
       chain!(
           tag!("/*") ~
           take_until_and_consume!(&b"*/"[..]),
           || { &b""[..] }));

// This parser is successful only if the input is over
fn eof(input:&[u8]) -> IResult<&[u8], &[u8]> {
    if input.len() == 0 {
        Done(input, input)
    } else {
        Error(Code(0))
    }
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~ End of parsers section ~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// The following code to unescape string literals was copied from Rust's source
// It's in src/libsyntax/parse/mod.rs
// We use it here to avoid using the `rustc_private` feature, which would break the build on the
// beta channel

/// Parse a string representing a character literal into its final form.
/// Rather than just accepting/rejecting a given literal, unescapes it as
/// well. Can take any slice prefixed by a character escape. Returns the
/// character and the number of characters consumed.
fn char_lit(lit: &str) -> (char, isize) {
    use std::char;

    let mut chars = lit.chars();
    let c = match (chars.next(), chars.next()) {
        (Some(c), None) if c != '\\' => return (c, 1),
        (Some('\\'), Some(c)) => match c {
            '"' => Some('"'),
            'n' => Some('\n'),
            'r' => Some('\r'),
            't' => Some('\t'),
            '\\' => Some('\\'),
            '\'' => Some('\''),
            '0' => Some('\0'),
            _ => { None }
        },
        _ => panic!("lexer accepted invalid char escape `{}`", lit)
    };

    match c {
        Some(x) => return (x, 2),
        None => { }
    }

    let msg = format!("lexer should have rejected a bad character escape {}", lit);
    let msg2 = &msg[..];

    fn esc(len: usize, lit: &str) -> Option<(char, isize)> {
        u32::from_str_radix(&lit[2..len], 16).ok()
        .and_then(char::from_u32)
        .map(|x| (x, len as isize))
    }

    let unicode_escape = || -> Option<(char, isize)> {
        if lit.as_bytes()[2] == b'{' {
            let idx = lit.find('}').expect(msg2);
            let subslice = &lit[3..idx];
            u32::from_str_radix(subslice, 16).ok()
                .and_then(char::from_u32)
                .map(|x| (x, subslice.chars().count() as isize + 4))
        } else {
            esc(6, lit)
        }
    };

    // Unicode escapes
    return match lit.as_bytes()[1] as char {
        'x' | 'X' => esc(4, lit),
        'u' => unicode_escape(),
        'U' => esc(10, lit),
        _ => None,
    }.expect(msg2);
}

/// Parse a string representing a string literal into its final form. Does
/// unescaping.
fn str_lit(lit: &str) -> String {
    let mut res = String::with_capacity(lit.len());

    // FIXME #8372: This could be a for-loop if it didn't borrow the iterator
    let error = |i| format!("lexer should have rejected {} at {}", lit, i);

    /// Eat everything up to a non-whitespace
    fn eat<'a>(it: &mut iter::Peekable<str::CharIndices<'a>>) {
        loop {
            match it.peek().map(|x| x.1) {
                Some(' ') | Some('\n') | Some('\r') | Some('\t') => {
                    it.next();
                },
                _ => { break; }
            }
        }
    }

    let mut chars = lit.char_indices().peekable();
    loop {
        match chars.next() {
            Some((i, c)) => {
                match c {
                    '\\' => {
                        let ch = chars.peek().unwrap_or_else(|| {
                            panic!("{}", error(i))
                        }).1;

                        if ch == '\n' {
                            eat(&mut chars);
                        } else if ch == '\r' {
                            chars.next();
                            let ch = chars.peek().unwrap_or_else(|| {
                                panic!("{}", error(i))
                            }).1;

                            if ch != '\n' {
                                panic!("lexer accepted bare CR");
                            }
                            eat(&mut chars);
                        } else {
                            // otherwise, a normal escape
                            let (c, n) = char_lit(&lit[i..]);
                            for _ in 0..n - 1 { // we don't need to move past the first \
                                chars.next();
                            }
                            res.push(c);
                        }
                    },
                    '\r' => {
                        let ch = chars.peek().unwrap_or_else(|| {
                            panic!("{}", error(i))
                        }).1;

                        if ch != '\n' {
                            panic!("lexer accepted bare CR");
                        }
                        chars.next();
                        res.push('\n');
                    }
                    c => res.push(c),
                }
            },
            None => break
        }
    }

    res.shrink_to_fit(); // probably not going to do anything, unless there was an escape.
    res
}

#[cfg(test)]
mod test {
    use super::{setting_name, setting, settings_list};
    use super::{escaped_seq, not_escaped_seq, string_literal, str_scalar_value};
    use super::{eol, comment_one_line, comment_block, blanks};
    use super::{flt_base_w_digits_bef_dot, flt_base_no_digits_bef_dot, flt_base, flt_exponent};
    use super::{flt32_scalar_value, flt64_scalar_value, flt_scalar_value};
    use super::{int32_scalar_value, int64_scalar_value, int_scalar_value};
    use super::{bool_true_value, bool_false_value, boolean_scalar_value, scalar_value};
    use super::{boolean_array_elements, str_array_elements};
    use super::{flt64_array_elements, flt32_array_elements};
    use super::{int64_array_elements, int32_array_elements};
    use super::{value, array, group, list};
    use super::conf;

    use nom::ErrorCode;
    use nom::Err::{Code, Position};
    use nom::IResult::*;

    use types::{Setting, SettingsList, Value, ScalarValue, Config};

    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~ Parser components tests ~~~
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    #[test]
    fn setting_name_alpha() {
        let a_setting = &b"miles"[..];
        let res = setting_name(a_setting);
        assert_eq!(res, Done(&b""[..], "miles".to_string()));
    }

    #[test]
    fn setting_name_alphanumeric() {
        let a_setting = &b"ipv4_address-test"[..];
        let res = setting_name(a_setting);
        assert_eq!(res, Done(&b""[..], "ipv4_address-test".to_string()));
    }

    #[test]
    fn setting_name_ends_with_symbols() {
        let a_setting = &b"a_test__"[..];
        let res = setting_name(a_setting);
        assert_eq!(res, Done(&b""[..], "a_test__".to_string()));

        let a_setting2 = &b"b-_-"[..];
        let res2 = setting_name(a_setting2);
        assert_eq!(res2, Done(&b""[..], "b-_-".to_string()));
    }

    #[test]
    fn setting_name_ends_with_num() {
        let a_setting = &b"a_test_1234"[..];
        let res = setting_name(a_setting);
        assert_eq!(res, Done(&b""[..], "a_test_1234".to_string()));
    }

    #[test]
    fn setting_name_bad_num_prefix() {
        let a_setting = &b"12_xxx"[..];
        let res = setting_name(a_setting);
        assert_eq!(res, Error(Position(ErrorCode::Alpha as u32, b"12_xxx")));
    }

    #[test]
    fn setting_name_bad_symbol_prefix() {
        let a_setting = &b"__not_allowed"[..];
        let res = setting_name(a_setting);
        assert_eq!(res, Error(Position(ErrorCode::Alpha as u32, b"__not_allowed")));
    }

    #[test]
    fn single_setting() {
        let a_setting = &b"a_test__2 = \n\n\t19L;\r\n"[..];
        let res = setting(a_setting);
        assert_eq!(res, Done(&b""[..], Setting::new(
            "a_test__2".to_string(),
            Value::Svalue(ScalarValue::Integer64(19)))));
    }

    #[test]
    fn single_setting2() {
        let a_setting = &b"mysql_user:\"root\";"[..];
        let res = setting(a_setting);
        assert_eq!(res, Done(&b""[..], Setting::new(
            "mysql_user".to_string(),
            Value::Svalue(ScalarValue::Str("root".to_string())))));
    }

    #[test]
    fn single_setting3() {
        let a_setting = &b"\n\tcountry   : \"USA\"\n\n;"[..];
        let res = setting(a_setting);
        assert_eq!(res, Done(&b""[..], Setting::new(
            "country".to_string(),
            Value::Svalue(ScalarValue::Str("USA".to_string())))));
    }

    #[test]
    fn single_setting_list() {
        let a_setting = &b"codes: (1, 2, 3);"[..];
        let res = setting(a_setting);
        assert_eq!(res, Done(&b""[..], Setting::new(
            "codes".to_string(),
            Value::List(vec![
                Value::Svalue(ScalarValue::Integer32(1)),
                Value::Svalue(ScalarValue::Integer32(2)),
                Value::Svalue(ScalarValue::Integer32(3))]))));
    }

    #[test]
    fn single_setting_array() {
        let a_setting = &b"codes: [1, 2, 3];"[..];
        let res = setting(a_setting);
        assert_eq!(res, Done(&b""[..], Setting::new(
            "codes".to_string(),
            Value::Array(vec![
                Value::Svalue(ScalarValue::Integer32(1)),
                Value::Svalue(ScalarValue::Integer32(2)),
                Value::Svalue(ScalarValue::Integer32(3))]))));
    }

    #[test]
    fn single_setting_group() {
        let a_setting = &b"misc: { x = 1; y = 2; };"[..];
        let res = setting(a_setting);

        let mut grp = SettingsList::new();
        grp.insert("x".to_string(),
                   Setting::new("x".to_string(), Value::Svalue(ScalarValue::Integer32(1))));
        grp.insert("y".to_string(),
                   Setting::new("y".to_string(), Value::Svalue(ScalarValue::Integer32(2))));

        assert_eq!(res, Done(&b""[..], Setting::new("misc".to_string(), Value::Group(grp))));
    }

    #[test]
    fn settings_lst() {
        let settings = &b"setting1: 1; setting2\n\n=\n2 ;\tsetting3=\"bye\";\n"[..];
        let res = settings_list(settings);

        let mut expected = SettingsList::new();
        expected.insert("setting1".to_string(),
                        Setting::new("setting1".to_string(),
                                     Value::Svalue(ScalarValue::Integer32(1))));
        expected.insert("setting2".to_string(),
                        Setting::new("setting2".to_string(),
                                     Value::Svalue(ScalarValue::Integer32(2))));
        expected.insert("setting3".to_string(),
                        Setting::new("setting3".to_string(),
                                     Value::Svalue(ScalarValue::Str("bye".to_string()))));

        assert_eq!(res, Done(&b""[..], expected));
    }

    #[test]
    fn not_escaped_sequence() {
        let a_sequence = &b"a regular string without escaped sequences\""[..];
        let res = not_escaped_seq(a_sequence);
        assert_eq!(res, Done(&b"\""[..], &a_sequence[..a_sequence.len()-1]));
    }

    #[test]
    fn not_escaped_sequence2() {
        let a_sequence = &b"a string with \\t an escape\""[..];
        let res = not_escaped_seq(a_sequence);
        assert_eq!(res, Done(&b"\\t an escape\""[..], &a_sequence[0..14]));
    }

    #[test]
    fn not_escaped_sequence3() {
        let a_sequence = &b"\\nthis time we started with a newline\""[..];
        let res = not_escaped_seq(a_sequence);
        assert_eq!(res, Done(a_sequence, &b""[..]));
    }

    #[test]
    fn not_escaped_sequence4() {
        let a_sequence = &b"\""[..];
        let res = not_escaped_seq(a_sequence);
        assert_eq!(res, Done(a_sequence, &b""[..]));
    }

    #[test]
    fn escaped_sequence1() {
        let an_escaped_seq = &b"\\t"[..];
        let res = escaped_seq(an_escaped_seq);
        assert_eq!(res, Done(&b""[..], an_escaped_seq));
    }

    #[test]
    fn escaped_sequence2() {
        let an_escaped_seq = &b"\\\\"[..];
        let res = escaped_seq(an_escaped_seq);
        assert_eq!(res, Done(&b""[..], an_escaped_seq));
    }

    #[test]
    fn escaped_sequence3() {
        let an_escaped_seq = &b"\\\"test"[..];
        let res = escaped_seq(an_escaped_seq);
        assert_eq!(res, Done(&b"test"[..], &an_escaped_seq[0..2]));
    }

    #[test]
    fn bad_escape_sequence1() {
        let bad_escaped_seq = &b"\\q"[..];
        let res = escaped_seq(bad_escaped_seq);
        assert_eq!(res, Error(Position(ErrorCode::Alt as u32, b"\\q")));
    }

    #[test]
    fn bad_escape_sequence2() {
        let bad_escape_seq = &b"aaa"[..];
        let res = escaped_seq(bad_escape_seq);
        assert_eq!(res, Error(Position(ErrorCode::Alt as u32, b"aaa")));
    }

    #[test]
    fn empty_str_lit() {
        let input = &b"\"\""[..];
        let res = string_literal(input);
        assert_eq!(res, Done(&b""[..], String::new()));        
    }

    #[test]
    fn simple_str_lit() {
        let input = &b"\"a string literal\""[..];
        let res = string_literal(input);
        assert_eq!(res, Done(&b""[..], "a string literal".to_string()));
    }

    #[test]
    fn str_lit_with_escapes() {
        let input = &b"\"And he said: \\\"Hello, world!\\\"\\n\""[..];
        let res = string_literal(input);
        assert_eq!(res, Done(&b""[..], "And he said: \"Hello, world!\"\n".to_string()));
    }

    #[test]
    fn str_lit_with_escapes2() {
        let input = &b"\"And he said: \\\"Hello, world!\\\"\""[..];
        let res = string_literal(input);
        assert_eq!(res, Done(&b""[..], "And he said: \"Hello, world!\"".to_string()));
    }

    #[test]
    fn str_lit_with_escapes3() {
        let input = &b"\"A string\\twith a tab\""[..];
        let res = string_literal(input);
        assert_eq!(res, Done(&b""[..], "A string\twith a tab".to_string()));
    }

    #[test]
    fn str_lit_with_escapes4() {
        let input = &b"\"A backslash in quotes: \\\"\\\\\\\"\""[..];
        let res = string_literal(input);
        assert_eq!(res, Done(&b""[..], "A backslash in quotes: \"\\\"".to_string()));
    }

    #[test]
    fn str_lit_with_escapes5() {
        let input = &b"\"escaped_str=\\\"Just a \\\\\\\"test\\\\\\\" with escapes.\\\";\""[..];
        let res = string_literal(input);
        assert_eq!(res, Done(&b""[..],
                             "escaped_str=\"Just a \\\"test\\\" with escapes.\";".to_string()));
    }

    #[test]
    fn single_str_scalar_value() {
        let input = &b"\"a string literal\""[..];
        let res = str_scalar_value(input);
        assert_eq!(res, Done(&b""[..], ScalarValue::Str("a string literal".to_string())));
    }

    #[test]
    fn single_str_scalar_value2() {
        let input = &b"\"A backslash in quotes: \\\"\\\\\\\"\""[..];
        let res = str_scalar_value(input);
        assert_eq!(res, Done(&b""[..], ScalarValue::Str(
            "A backslash in quotes: \"\\\"".to_string())));
    }

    #[test]
    fn single_str_scalar_value3() {
        let input = &b"\"escaped_str=\\\"Just a \\\\\\\"test\\\\\\\" with escapes.\\\";\""[..];
        let res = str_scalar_value(input);
        assert_eq!(res, Done(&b""[..], ScalarValue::Str(
            "escaped_str=\"Just a \\\"test\\\" with escapes.\";".to_string())));
    }

    #[test]
    fn multi_str_scalar_value() {
        let input = &b"\"string\"\n\n\" literals\"\t\" to\"    \" the rescue\""[..];
        let res = str_scalar_value(input);
        assert_eq!(res, Done(&b""[..], ScalarValue::Str(
            "string literals to the rescue".to_string())));
    }

    #[test]
    fn multi_str_scalar_value2() {
        let input = &concat!("\"escaped_\"\"str=\\\"Just a \\\\\"\r\n\r\n   \"\\\"te\"\t\t  ",
                             "\t\t\"st\\\\\\\" \"\n\"wi\"\"th escapes.\\\";\"").as_bytes()[..];
        let res = str_scalar_value(input);
        assert_eq!(res, Done(&b""[..], ScalarValue::Str(
            "escaped_str=\"Just a \\\"test\\\" with escapes.\";".to_string())));
    }

    #[test]
    fn multi_str_scalar_value3() {
        let input = &b"\"string\"\n//Test\n\" literals\"\t\" to\"    \" the rescue\""[..];
        let res = str_scalar_value(input);
        assert_eq!(res, Done(&b""[..], ScalarValue::Str(
            "string literals to the rescue".to_string())));
    }

    #[test]
    fn multi_str_scalar_value4() {
        let input = &b"\"string\"/*test*/\" literals\"\t\" to\" /**/   \" the rescue\""[..];
        let res = str_scalar_value(input);
        assert_eq!(res, Done(&b""[..], ScalarValue::Str(
            "string literals to the rescue".to_string())));
    }

    #[test]
    fn end_of_line1() {
        let input = &b"a test\n"[..];
        let res = eol(input);
        assert_eq!(res, Error(Position(ErrorCode::Alt as u32, b"a test\n")));
    }

    #[test]
    fn end_of_line2() {
        let input = &b"\r\n"[..];
        let res = eol(input);
        assert_eq!(res, Done(&b""[..], input));
    }

    #[test]
    fn end_of_line3() {
        let input = &b"\n"[..];
        let res = eol(input);
        assert_eq!(res, Done(&b""[..], input));
    }

    #[test]
    fn end_of_line4() {
        let input = &"\u{2028}\n\n".as_bytes()[..];
        let res = eol(input);
        assert_eq!(res, Done(&b"\n\n"[..], &"\u{2028}".as_bytes()[..]));
    }

    #[test]
    fn one_line_comment_bad() {
        let input = &b"not a comment // see?"[..];
        let res = comment_one_line(input);
        assert_eq!(res, Error(Position(ErrorCode::Alt as u32, b"not a comment // see?")));
    }

    #[test]
    fn one_line_comment() {
        let input = &b"//\r\n"[..];
        let res = comment_one_line(input);
        assert_eq!(res, Done(&b""[..], &b""[..]));
    }

    #[test]
    fn one_line_comment2() {
        let input = &b"// A one-line comment example.\n\n\n"[..];
        let res = comment_one_line(input);
        assert_eq!(res, Done(&b"\n\n"[..], &b""[..]));
    }

    #[test]
    fn one_line_comment3() {
        let input = &b"//////////Another comment. One-liners don't // // nest\n"[..];
        let res = comment_one_line(input);
        assert_eq!(res, Done(&b""[..], &b""[..]));
    }

    #[test]
    fn one_line_comment4() {
        let input = &b"# Comments can also start with a `#`.\n"[..];
        let res = comment_one_line(input);
        assert_eq!(res, Done(&b""[..], &b""[..]));
    }

    #[test]
    fn comment_blk_bad() {
        let input = &b"not a comment /* see?"[..];
        let res = comment_block(input);
        assert_eq!(res, Error(Position(ErrorCode::Tag as u32, b"not a comment /* see?")));
    }

    #[test]
    fn comment_blk() {
        let input = &b"/**/"[..];
        let res = comment_block(input);
        assert_eq!(res, Done(&b""[..], &b""[..]));
    }

    #[test]
    fn comment_blk2() {
        let input = &b"/* Comments do /* not /****nest*/"[..];
        let res = comment_block(input);
        assert_eq!(res, Done(&b""[..], &b""[..]));
    }

    #[test]
    fn comment_blk3() {
        let input = &b"/* A\ncomment\nwith\nmultiple\nlines\n\t\t\r\nand\nspaces*****/\ntest"[..];
        let res = comment_block(input);
        assert_eq!(res, Done(&b"\ntest"[..], &b""[..]));
    }

    #[test]
    fn comment_blk4() {
        let input = &b"/*/*/*Again, they don't \n*nest*/See? We're not in a comment now."[..];
        let res = comment_block(input);
        // Hah! A trap :) The comment ends with the first occurrence of `*/`
        assert_eq!(res, Done(&b"*Again, they don't \n*nest*/See? We're not in a comment now."[..],
                             &b""[..]));
    }

    #[test]
    fn comment_blk5() {
        let input = &b"/*/*\n *Again, they don't \n *nest*/See? We're not in a comment now."[..];
        let res = comment_block(input);
        assert_eq!(res, Done(&b"See? We're not in a comment now."[..], &b""[..]));
    }

    #[test]
    fn blanks_mixed() {
        let input = &b"   \n\n"[..];
        let res = blanks(input);
        assert_eq!(res, Done(&b""[..], &b""[..]));
    }

    #[test]
    fn blanks_mixed2() {
        let input = &b"   "[..];
        let res = blanks(input);
        assert_eq!(res, Done(&b""[..], &b""[..]));
    }

    #[test]
    fn blanks_mixed3() {
        let input = &b"// A test\n  \t/* a \ncomment\n\n*//**///Hello\n\n\n\n"[..];
        let res = blanks(input);
        assert_eq!(res, Done(&b""[..], &b""[..]));
    }

    #[test]
    fn blanks_mixed4() {
        let input = &b"\n\n\t\t         // A test\n  \t/* a \ncomment\n\n*//**///Hello\n\n\n\n"[..];
        let res = blanks(input);
        assert_eq!(res, Done(&b""[..], &b""[..]));
    }

    #[test]
    fn blanks_mixed5() {
        let input = &b"\n\n\t\t         // A test\n  \t/* a \ncomment\n\n*//**///Hello\nXPTO\n"[..];
        let res = blanks(input);
        assert_eq!(res, Done(&b"XPTO\n"[..], &b""[..]));
    }

    #[test]
    fn blanks_mixed6() {
        let input = &b"// A test\n"[..];
        let res = blanks(input);
        assert_eq!(res, Done(&b""[..], &b""[..]));
    }

    #[test]
    fn blanks_mixed7() {
        let input = &b"### A test\naaa"[..];
        let res = blanks(input);
        assert_eq!(res, Done(&b"aaa"[..], &b""[..]));
    }

    #[test]
    fn blanks_mixed8() {
        let input = &b"/************/\r\n\r\n\r\n\t"[..];
        let res = blanks(input);
        assert_eq!(res, Done(&b""[..], &b""[..]));
    }

    #[test]
    fn flt_base_w_digits_before_dot() {
        let input = &b".4435"[..];
        let res = flt_base_w_digits_bef_dot(input);
        assert_eq!(res, Error(Position(ErrorCode::Digit as u32, b".4435")));
    }

    #[test]
    fn flt_base_w_digits_before_dot2() {
        let input = &b"0.4435"[..];
        let res = flt_base_w_digits_bef_dot(input);
        assert_eq!(res, Done(&b""[..], ("0", "4435")));
    }

    #[test]
    fn flt_base_w_digits_before_dot3() {
        let input = &b"19570.4435"[..];
        let res = flt_base_w_digits_bef_dot(input);
        assert_eq!(res, Done(&b""[..], ("19570", "4435")));
    }

    #[test]
    fn flt_base_w_digits_before_dot4() {
        let input = &b"19570."[..];
        let res = flt_base_w_digits_bef_dot(input);
        assert_eq!(res, Done(&b""[..], ("19570", "")));
    }

    #[test]
    fn flt_base_w_digits_before_dot5() {
        let input = &b"0.0"[..];
        let res = flt_base_w_digits_bef_dot(input);
        assert_eq!(res, Done(&b""[..], ("0", "0")));
    }

    #[test]
    fn flt_base_w_digits_before_dot6() {
        let input = &b"0.0\nmore input"[..];
        let res = flt_base_w_digits_bef_dot(input);
        assert_eq!(res, Done(&b"\nmore input"[..], ("0", "0")));
    }

    #[test]
    fn flt_base_no_digits_before_dot() {
        let input = &b"0.0"[..];
        let res = flt_base_no_digits_bef_dot(input);
        assert_eq!(res, Error(Position(ErrorCode::Tag as u32, b"0.0")));
    }

    #[test]
    fn flt_base_no_digits_before_dot2() {
        let input = &b".0"[..];
        let res = flt_base_no_digits_bef_dot(input);
        assert_eq!(res, Done(&b""[..], ("", "0")));
    }

    #[test]
    fn flt_base_no_digits_before_dot3() {
        let input = &b".\n"[..];
        let res = flt_base_no_digits_bef_dot(input);
        assert_eq!(res, Error(Position(ErrorCode::Digit as u32, b"\n" /* dot is consumed */)));
    }

    #[test]
    fn flt_base_no_digits_before_dot4() {
        let input = &b".456897"[..];
        let res = flt_base_no_digits_bef_dot(input);
        assert_eq!(res, Done(&b""[..], ("", "456897")));
    }

    #[test]
    fn flt_base_no_digits_before_dot5() {
        let input = &b".456897more input here\n\nbye"[..];
        let res = flt_base_no_digits_bef_dot(input);
        assert_eq!(res, Done(&b"more input here\n\nbye"[..], ("", "456897")));
    }

    #[test]
    fn flt_base_no_digits_before_dot6() {
        let input = &b".456897E-5"[..];
        let res = flt_base_no_digits_bef_dot(input);
        assert_eq!(res, Done(&b"E-5"[..], ("", "456897")));
    }

    #[test]
    fn flt_base_value() {
        let input = &b".456897E-5"[..];
        let res = flt_base(input);
        assert_eq!(res, Done(&b"E-5"[..], ("", "456897")));
    }

    #[test]
    fn flt_base_value2() {
        let input = &b"1.E-5"[..];
        let res = flt_base(input);
        assert_eq!(res, Done(&b"E-5"[..], ("1", "")));
    }

    #[test]
    fn flt_base_value3() {
        let input = &b"134.456E-5"[..];
        let res = flt_base(input);
        assert_eq!(res, Done(&b"E-5"[..], ("134", "456")));
    }

    #[test]
    fn flt_base_value4() {
        let input = &b"134."[..];
        let res = flt_base(input);
        assert_eq!(res, Done(&b""[..], ("134", "")));
    }

    #[test]
    fn flt_base_value5() {
        let input = &b".0\n"[..];
        let res = flt_base(input);
        assert_eq!(res, Done(&b"\n"[..], ("", "0")));
    }

    #[test]
    fn flt_exponent_value() {
        let input = &b"eee"[..];
        let res = flt_exponent(input);
        assert_eq!(res, Error(Position(ErrorCode::Digit as u32, b"ee" /* one e consumed */)));
    }

    #[test]
    fn flt_exponent_value2() {
        let input = &b"e-49"[..];
        let res = flt_exponent(input);
        assert_eq!(res, Done(&b""[..], ("-", "49")));
    }

    #[test]
    fn flt_exponent_value3() {
        let input = &b"e49"[..];
        let res = flt_exponent(input);
        assert_eq!(res, Done(&b""[..], ("+", "49")));
    }

    #[test]
    fn flt_exponent_value4() {
        let input = &b"E+49"[..];
        let res = flt_exponent(input);
        assert_eq!(res, Done(&b""[..], ("+", "49")));
    }

    #[test]
    fn flt_exponent_value5() {
        let input = &b"E-127\nmore\ninput\there"[..];
        let res = flt_exponent(input);
        assert_eq!(res, Done(&b"\nmore\ninput\there"[..], ("-", "127")));
    }

    #[test]
    fn flt_exponent_value6() {
        let input = &b"e4.;xpto"[..];
        let res = flt_exponent(input);
        assert_eq!(res, Done(&b".;xpto"[..], ("+", "4")));
    }

    #[test]
    fn flt32_as_scalar() {
        let input = &b"1.E3\r\n\r\n"[..];
        let res = flt32_scalar_value(input);
        assert_eq!(res, Done(&b"\r\n\r\n"[..], ScalarValue::Floating32(1.0E3)));
    }

    #[test]
    fn flt32_as_scalar2() {
        let input = &b"1.5\r\n\r\n"[..];
        let res = flt32_scalar_value(input);
        assert_eq!(res, Done(&b"\r\n\r\n"[..], ScalarValue::Floating32(1.5)));
    }

    #[test]
    fn flt64_as_scalar() {
        let input = &b"1.E3L\r\n\r\n"[..];
        let res = flt64_scalar_value(input);
        assert_eq!(res, Done(&b"\r\n\r\n"[..], ScalarValue::Floating64(1.0E3)));
    }

    #[test]
    fn flt64_as_scalar2() {
        let input = &b"1.5L\r\n\r\n"[..];
        let res = flt64_scalar_value(input);
        assert_eq!(res, Done(&b"\r\n\r\n"[..], ScalarValue::Floating64(1.5)));
    }

    #[test]
    fn flt_as_scalar() {
        let input = &b"1.E3\r\n\r\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"\r\n\r\n"[..], ScalarValue::Floating32(1.0E3)));
    }

    #[test]
    fn flt_as_scalar2() {
        let input = &b"-1.E3\r\n\r\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"\r\n\r\n"[..], ScalarValue::Floating32(-1.0E3)));
    }

    #[test]
    fn flt_as_scalar3() {
        let input = &b"+1.E3\r\n\r\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"\r\n\r\n"[..], ScalarValue::Floating32(1.0E3)));
    }

    #[test]
    fn flt_as_scalar4() {
        let input = &b"1.E+3\r\n\r\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"\r\n\r\n"[..], ScalarValue::Floating32(1.0E3)));
    }

    #[test]
    fn flt_as_scalar5() {
        let input = &b"1.E-3\r\n\r\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"\r\n\r\n"[..], ScalarValue::Floating32(1.0E-3)));
    }

    #[test]
    fn flt_as_scalar6() {
        let input = &b"+1.E-3\r\n\r\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"\r\n\r\n"[..], ScalarValue::Floating32(1.0E-3)));
    }

    #[test]
    fn flt_as_scalar7() {
        let input = &b"-1.E+3\r\n\r\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"\r\n\r\n"[..], ScalarValue::Floating32(-1.0E3)));
    }

    #[test]
    fn flt_as_scalar8() {
        let input = &b"-.25e0\r\n\r\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"\r\n\r\n"[..], ScalarValue::Floating32(-0.25)));
    }

    #[test]
    fn flt_as_scalar9() {
        let input = &b"-.25e-2\r\n\r\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"\r\n\r\n"[..], ScalarValue::Floating32(-0.25e-2)));
    }

    #[test]
    fn flt_as_scalar10() {
        let input = &b"1.5e1with more input"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"with more input"[..], ScalarValue::Floating32(15.0)));
    }

    #[test]
    fn flt_as_scalar11() {
        let input = &b".5\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"\n"[..], ScalarValue::Floating32(0.5)));
    }

    #[test]
    fn flt_as_scalar12() {
        let input = &b"1.\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"\n"[..], ScalarValue::Floating32(1.0)));
    }

    #[test]
    fn flt_as_scalar13() {
        let input = &b"1.5;\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b";\n"[..], ScalarValue::Floating32(1.5)));
    }

    #[test]
    fn flt_as_scalar14() {
        let input = &b"-1.5;\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b";\n"[..], ScalarValue::Floating32(-1.5)));
    }

    #[test]
    fn flt_as_scalar15() {
        let input = &b"-.1e2;\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b";\n"[..], ScalarValue::Floating32(-10.0)));
    }

    #[test]
    fn flt_l_as_scalar() {
        let input = &b"1.E3L\r\n\r\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"\r\n\r\n"[..], ScalarValue::Floating64(1.0E3)));
    }

    #[test]
    fn flt_l_as_scalar2() {
        let input = &b"-1.E3L\r\n\r\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"\r\n\r\n"[..], ScalarValue::Floating64(-1.0E3)));
    }

    #[test]
    fn flt_l_as_scalar3() {
        let input = &b"+1.E3L\r\n\r\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"\r\n\r\n"[..], ScalarValue::Floating64(1.0E3)));
    }

    #[test]
    fn flt_l_as_scalar4() {
        let input = &b"1.E+3L\r\n\r\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"\r\n\r\n"[..], ScalarValue::Floating64(1.0E3)));
    }

    #[test]
    fn flt_l_as_scalar5() {
        let input = &b"1.E-3L\r\n\r\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"\r\n\r\n"[..], ScalarValue::Floating64(1.0E-3)));
    }

    #[test]
    fn flt_l_as_scalar6() {
        let input = &b"+1.E-3L\r\n\r\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"\r\n\r\n"[..], ScalarValue::Floating64(1.0E-3)));
    }

    #[test]
    fn flt_l_as_scalar7() {
        let input = &b"-1.E+3L\r\n\r\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"\r\n\r\n"[..], ScalarValue::Floating64(-1.0E3)));
    }

    #[test]
    fn flt_l_as_scalar8() {
        let input = &b"-.25e0L\r\n\r\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"\r\n\r\n"[..], ScalarValue::Floating64(-0.25)));
    }

    #[test]
    fn flt_l_as_scalar9() {
        let input = &b"-.25e-2L\r\n\r\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"\r\n\r\n"[..], ScalarValue::Floating64(-0.25e-2)));
    }

    #[test]
    fn flt_l_as_scalar10() {
        let input = &b"1.5e1Lwith more input"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"with more input"[..], ScalarValue::Floating64(15.0)));
    }

    #[test]
    fn flt_l_as_scalar11() {
        let input = &b".5L\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"\n"[..], ScalarValue::Floating64(0.5)));
    }

    #[test]
    fn flt_l_as_scalar12() {
        let input = &b"1.L\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b"\n"[..], ScalarValue::Floating64(1.0)));
    }

    #[test]
    fn flt_l_as_scalar13() {
        let input = &b"1.5L;\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b";\n"[..], ScalarValue::Floating64(1.5)));
    }

    #[test]
    fn flt_l_as_scalar14() {
        let input = &b"-1.5L;\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b";\n"[..], ScalarValue::Floating64(-1.5)));
    }

    #[test]
    fn flt_l_as_scalar15() {
        let input = &b"-.1e2L;\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b";\n"[..], ScalarValue::Floating64(-10.0)));
    }

    #[test]
    fn flt_l_as_scalar16() {
        let input = &b"4503599627370496.L;\n"[..];
        let res = flt_scalar_value(input);
        assert_eq!(res, Done(&b";\n"[..], ScalarValue::Floating64(4503599627370496.0)));
    }

    #[test]
    fn integer32_scalar_value() {
        let input = &b"14\n"[..];
        let res = int32_scalar_value(input);
        assert_eq!(res, Done(&b"\n"[..], ScalarValue::Integer32(14)));
    }

    #[test]
    fn integer64_scalar_value() {
        let input = &b"14L\n"[..];
        let res = int64_scalar_value(input);
        assert_eq!(res, Done(&b"\n"[..], ScalarValue::Integer64(14)));
    }

    #[test]
    fn integer_scalar_value() {
        let input = &b"-------"[..];
        let res = int_scalar_value(input);
        assert_eq!(res, Error(Position(ErrorCode::Alt as u32, b"-------")));
    }

    #[test]
    fn integer_scalar_value2() {
        let input = &b"0\n"[..];
        let res = int_scalar_value(input);
        assert_eq!(res, Done(&b"\n"[..], ScalarValue::Integer32(0)));
    }

    #[test]
    fn integer_scalar_value3() {
        let input = &b"14\n"[..];
        let res = int_scalar_value(input);
        assert_eq!(res, Done(&b"\n"[..], ScalarValue::Integer32(14)));
    }

    #[test]
    fn integer_scalar_value4() {
        let input = &b"-59\n"[..];
        let res = int_scalar_value(input);
        assert_eq!(res, Done(&b"\n"[..], ScalarValue::Integer32(-59)));
    }

    #[test]
    fn integer_scalar_value5() {
        let input = &b"+74\n"[..];
        let res = int_scalar_value(input);
        assert_eq!(res, Done(&b"\n"[..], ScalarValue::Integer32(74)));
    }

    #[test]
    fn integer_scalar_value6() {
        let input = &b"-0\n"[..];
        let res = int_scalar_value(input);
        assert_eq!(res, Done(&b"\n"[..], ScalarValue::Integer32(0)));
    }

    #[test]
    fn integer_scalar_value7() {
        let input = &b"+0\n"[..];
        let res = int_scalar_value(input);
        assert_eq!(res, Done(&b"\n"[..], ScalarValue::Integer32(0)));
    }

    #[test]
    fn integer_l_scalar_value() {
        let input = &b"+0L\n"[..];
        let res = int_scalar_value(input);
        assert_eq!(res, Done(&b"\n"[..], ScalarValue::Integer64(0)));
    }

    #[test]
    fn integer_l_scalar_value2() {
        let input = &b"-55586L\n"[..];
        let res = int_scalar_value(input);
        assert_eq!(res, Done(&b"\n"[..], ScalarValue::Integer64(-55586)));
    }

    #[test]
    fn integer_l_scalar_value3() {
        let input = &b"55586L\n"[..];
        let res = int_scalar_value(input);
        assert_eq!(res, Done(&b"\n"[..], ScalarValue::Integer64(55586)));
    }

    #[test]
    fn integer_l_scalar_value4() {
        let input = &b"+55586L\n"[..];
        let res = int_scalar_value(input);
        assert_eq!(res, Done(&b"\n"[..], ScalarValue::Integer64(55586)));
    }

    #[test]
    fn integer_l_scalar_value5() {
        let input = &b"-0L\n"[..];
        let res = int_scalar_value(input);
        assert_eq!(res, Done(&b"\n"[..], ScalarValue::Integer64(0)));
    }

    #[test]
    fn integer_l_scalar_value6() {
        let input = &b"9223372000000000000L\n"[..];
        let res = int_scalar_value(input);
        assert_eq!(res, Done(&b"\n"[..], ScalarValue::Integer64(9223372000000000000i64)));
    }

    #[test]
    fn boolean_true_value() {
        let input = &b"TRUE;"[..];
        let res = bool_true_value(input);
        assert_eq!(res, Done(&b";"[..], ScalarValue::Boolean(true)));
    }

    #[test]
    fn boolean_true_value2() {
        let input = &b"TrUe;"[..];
        let res = bool_true_value(input);
        assert_eq!(res, Done(&b";"[..], ScalarValue::Boolean(true)));
    }

    #[test]
    fn boolean_true_value3() {
        let input = &b"YES;"[..];
        let res = bool_true_value(input);
        assert_eq!(res, Done(&b";"[..], ScalarValue::Boolean(true)));
    }

    #[test]
    fn boolean_true_value4() {
        let input = &b"yEs;"[..];
        let res = bool_true_value(input);
        assert_eq!(res, Done(&b";"[..], ScalarValue::Boolean(true)));
    }

    #[test]
    fn boolean_false_value() {
        let input = &b"FALSE;"[..];
        let res = bool_false_value(input);
        assert_eq!(res, Done(&b";"[..], ScalarValue::Boolean(false)));
    }

    #[test]
    fn boolean_false_value2() {
        let input = &b"FaLsE;"[..];
        let res = bool_false_value(input);
        assert_eq!(res, Done(&b";"[..], ScalarValue::Boolean(false)));
    }

    #[test]
    fn boolean_false_value3() {
        let input = &b"NO;"[..];
        let res = bool_false_value(input);
        assert_eq!(res, Done(&b";"[..], ScalarValue::Boolean(false)));
    }

    #[test]
    fn boolean_false_value4() {
        let input = &b"nO;"[..];
        let res = bool_false_value(input);
        assert_eq!(res, Done(&b";"[..], ScalarValue::Boolean(false)));
    }

    #[test]
    fn bool_scalar_value() {
        let input = &b"False;"[..];
        let res = boolean_scalar_value(input);
        assert_eq!(res, Done(&b";"[..], ScalarValue::Boolean(false)));
    }

    #[test]
    fn bool_scalar_value2() {
        let input = &b"true;"[..];
        let res = boolean_scalar_value(input);
        assert_eq!(res, Done(&b";"[..], ScalarValue::Boolean(true)));
    }

    #[test]
    fn bool_scalar_value3() {
        let input = &b"yeS;"[..];
        let res = boolean_scalar_value(input);
        assert_eq!(res, Done(&b";"[..], ScalarValue::Boolean(true)));
    }

    #[test]
    fn bool_scalar_value4() {
        let input = &b"no;"[..];
        let res = boolean_scalar_value(input);
        assert_eq!(res, Done(&b";"[..], ScalarValue::Boolean(false)));
    }

    #[test]
    fn bool_array_elems() {
        let input = &b"true, true, no, yes, false\n\n];"[..];
        let res = boolean_array_elements(input);
        assert_eq!(res, Done(&b"];"[..], vec![
            Value::Svalue(ScalarValue::Boolean(true)),
            Value::Svalue(ScalarValue::Boolean(true)),
            Value::Svalue(ScalarValue::Boolean(false)),
            Value::Svalue(ScalarValue::Boolean(true)),
            Value::Svalue(ScalarValue::Boolean(false))]));
    }

    #[test]
    fn bool_array_elems2() {
        let input = &b"true]"[..];
        let res = boolean_array_elements(input);
        assert_eq!(res, Done(&b"]"[..], vec![Value::Svalue(ScalarValue::Boolean(true))]));
    }

    #[test]
    fn bool_array_elems3() {
        let input = &b"true, false/*test*/\n//lala\n];"[..];
        let res = boolean_array_elements(input);
        assert_eq!(res, Done(&b"];"[..], vec![
            Value::Svalue(ScalarValue::Boolean(true)),
            Value::Svalue(ScalarValue::Boolean(false))]));
    }

    #[test]
    fn string_array_elems() {
        let input = &b"\"a\", \"1\", \"b\"\n];"[..];
        let res = str_array_elements(input);
        assert_eq!(res, Done(&b"];"[..], vec![Value::Svalue(ScalarValue::Str("a".to_string())),
                                              Value::Svalue(ScalarValue::Str("1".to_string())),
                                              Value::Svalue(ScalarValue::Str("b".to_string()))]));
    }

    #[test]
    fn string_array_elems2() {
        let input = &b"\"a\"\"1\"/**/\"b\"\n];"[..];
        let res = str_array_elements(input);
        assert_eq!(res, Done(&b"];"[..], vec![Value::Svalue(ScalarValue::Str("a1b".to_string()))]));
    }

    #[test]
    fn flt64_array_elems() {
        let input = &b"1.E4L, 5.0L  , 0.5L];"[..];
        let res = flt64_array_elements(input);
        assert_eq!(res, Done(&b"];"[..], vec![Value::Svalue(ScalarValue::Floating64(1.0e4)),
                                              Value::Svalue(ScalarValue::Floating64(5.0)),
                                              Value::Svalue(ScalarValue::Floating64(0.5))]));
    }

    #[test]
    fn flt64_array_elems2() {
        let input = &b"-5.0e-1L];"[..];
        let res = flt64_array_elements(input);
        assert_eq!(res, Done(&b"];"[..], vec![Value::Svalue(ScalarValue::Floating64(-5.0e-1))]));
    }

    #[test]
    fn flt32_array_elems() {
        let input = &b"1.E4, 5.0  , 0.5];"[..];
        let res = flt32_array_elements(input);
        assert_eq!(res, Done(&b"];"[..], vec![Value::Svalue(ScalarValue::Floating32(1.0e4)),
                                              Value::Svalue(ScalarValue::Floating32(5.0)),
                                              Value::Svalue(ScalarValue::Floating32(0.5))]));
    }

    #[test]
    fn flt32_array_elems2() {
        let input = &b"-5.0e-1];"[..];
        let res = flt32_array_elements(input);
        assert_eq!(res, Done(&b"];"[..], vec![Value::Svalue(ScalarValue::Floating32(-5.0e-1))]));
    }

    #[test]
    fn int64_array_elems() {
        let input = &b"1L, 5L  , 6L];"[..];
        let res = int64_array_elements(input);
        assert_eq!(res, Done(&b"];"[..], vec![Value::Svalue(ScalarValue::Integer64(1)),
                                              Value::Svalue(ScalarValue::Integer64(5)),
                                              Value::Svalue(ScalarValue::Integer64(6))]));
    }

    #[test]
    fn int64_array_elems2() {
        let input = &b"1L];"[..];
        let res = int64_array_elements(input);
        assert_eq!(res, Done(&b"];"[..], vec![Value::Svalue(ScalarValue::Integer64(1))]));
    }

    #[test]
    fn int32_array_elems() {
        let input = &b"1, 5  , 6];"[..];
        let res = int32_array_elements(input);
        assert_eq!(res, Done(&b"];"[..], vec![Value::Svalue(ScalarValue::Integer32(1)),
                                              Value::Svalue(ScalarValue::Integer32(5)),
                                              Value::Svalue(ScalarValue::Integer32(6))]));
    }

    #[test]
    fn int32_array_elems2() {
        let input = &b"1];"[..];
        let res = int32_array_elements(input);
        assert_eq!(res, Done(&b"];"[..], vec![Value::Svalue(ScalarValue::Integer32(1))]));
    }

    #[test]
    fn empty_array() {
        let input = &b"[\n\n\n\n]\n;"[..];
        let res = array(input);
        assert_eq!(res, Done(&b"\n;"[..], Value::Array(Vec::new())));
    }

    #[test]
    fn empty_array2() {
        let input = &b"[]\n;"[..];
        let res = array(input);
        assert_eq!(res, Done(&b"\n;"[..], Value::Array(Vec::new())));
    }

    #[test]
    fn bad_array() {
        let input = &b"[true, \"a\", 14, 19, 5.0e1];\n"[..];
        let res = array(input);
        assert_eq!(res, Error(Position(ErrorCode::Alt as u32, b"[true, \"a\", 14, 19, 5.0e1];\n")));
    }

    #[test]
    fn bad_array2() {
        let input = &b"[\"a bad array\", 12, 3.0e-1, true];\n"[..];
        let res = array(input);
        assert_eq!(res, Error(Position(ErrorCode::Alt as u32, b"[\"a bad array\", 12, 3.0e-1, true];\n")));
    }

    #[test]
    fn bool_array() {
        let input = &b"[  true\t, yes//a test\n, \n\nfalse\n]\n;"[..];
        let res = array(input);
        assert_eq!(res, Done(&b"\n;"[..], Value::Array(vec![
            Value::Svalue(ScalarValue::Boolean(true)),
            Value::Svalue(ScalarValue::Boolean(true)),
            Value::Svalue(ScalarValue::Boolean(false))])));
    }

    #[test]
    fn str_array() {
        let input = &b"[  \"a\", \"b\"//a test\n,\n\"c\\\\d\"\n]\n;"[..];
        let res = array(input);
        assert_eq!(res, Done(&b"\n;"[..], Value::Array(vec![
            Value::Svalue(ScalarValue::Str("a".to_string())),
            Value::Svalue(ScalarValue::Str("b".to_string())),
            Value::Svalue(ScalarValue::Str("c\\d".to_string()))])));
    }

    #[test]
    fn flt64_array() {
        let input = &b"[1.E4L, 5.0L  , 0.5L];"[..];
        let res = array(input);
        assert_eq!(res, Done(&b";"[..], Value::Array(vec![
            Value::Svalue(ScalarValue::Floating64(1.0e4)),
            Value::Svalue(ScalarValue::Floating64(5.0)),
            Value::Svalue(ScalarValue::Floating64(0.5))])));
    }

    #[test]
    fn flt32_array() {
        let input = &b"[1.E4, 5.0  , 0.5];"[..];
        let res = array(input);
        assert_eq!(res, Done(&b";"[..], Value::Array(vec![
            Value::Svalue(ScalarValue::Floating32(1.0e4)),
            Value::Svalue(ScalarValue::Floating32(5.0)),
            Value::Svalue(ScalarValue::Floating32(0.5))])));
    }

    #[test]
    fn int64_array() {
        let input = &b"[1L, 5L  , 55L];"[..];
        let res = array(input);
        assert_eq!(res, Done(&b";"[..], Value::Array(vec![
            Value::Svalue(ScalarValue::Integer64(1)),
            Value::Svalue(ScalarValue::Integer64(5)),
            Value::Svalue(ScalarValue::Integer64(55))])));
    }

    #[test]
    fn int32_array() {
        let input = &b"[1, 5  , 55];"[..];
        let res = array(input);
        assert_eq!(res, Done(&b";"[..], Value::Array(vec![
            Value::Svalue(ScalarValue::Integer32(1)),
            Value::Svalue(ScalarValue::Integer32(5)),
            Value::Svalue(ScalarValue::Integer32(55))])));
    }

    #[test]
    fn scalar_val() {
        let input = &b"true;"[..];
        let res = scalar_value(input);
        assert_eq!(res, Done(&b";"[..], ScalarValue::Boolean(true)));
    }

    #[test]
    fn scalar_val2() {
        let input = &b"443.5e+2L;"[..];
        let res = scalar_value(input);
        assert_eq!(res, Done(&b";"[..], ScalarValue::Floating64(443.5E2)));
    }

    #[test]
    fn scalar_val3() {
        let input = &b"443.5;"[..];
        let res = scalar_value(input);
        assert_eq!(res, Done(&b";"[..], ScalarValue::Floating32(443.5)));
    }

    #[test]
    fn scalar_val4() {
        let input = &b"-32;"[..];
        let res = scalar_value(input);
        assert_eq!(res, Done(&b";"[..], ScalarValue::Integer32(-32)));
    }

    #[test]
    fn scalar_val5() {
        let input = &b"64L;"[..];
        let res = scalar_value(input);
        assert_eq!(res, Done(&b";"[..], ScalarValue::Integer64(64)));
    }

    #[test]
    fn scalar_val6() {
        let input = &b"\"a string literal\";"[..];
        let res = scalar_value(input);
        assert_eq!(res, Done(&b";"[..], ScalarValue::Str("a string literal".to_string())));
    }

    #[test]
    fn scalar_val7() {
        let input = &b"\"he said: \\\"hello\\\"\";"[..];
        let res = scalar_value(input);
        assert_eq!(res, Done(&b";"[..], ScalarValue::Str("he said: \"hello\"".to_string())));
    }

    #[test]
    fn scalar_val8() {
        let input = &b"\"he\"\" said:\" \r\n//lala\r\n\" \\\"hello\\\"\";"[..];
        let res = scalar_value(input);
        assert_eq!(res, Done(&b";"[..], ScalarValue::Str("he said: \"hello\"".to_string())));
    }

    #[test]
    fn single_value() {
        let a_value = &b"\"USA\"\n\n;"[..];
        let res = value(a_value);
        assert_eq!(res, Done(&b";"[..], Value::Svalue(ScalarValue::Str("USA".to_string()))));
    }

    #[test]
    fn single_value_list() {
        let a_value = &b"(1, 2, 3);"[..];
        let res = value(a_value);
        assert_eq!(res, Done(&b";"[..],
                             Value::List(vec![
                                 Value::Svalue(ScalarValue::Integer32(1)),
                                 Value::Svalue(ScalarValue::Integer32(2)),
                                 Value::Svalue(ScalarValue::Integer32(3))])));
    }

    #[test]
    fn single_value_array() {
        let a_value = &b"[1, 2, 3];"[..];
        let res = value(a_value);
        assert_eq!(res, Done(&b";"[..],
                             Value::Array(vec![
                                 Value::Svalue(ScalarValue::Integer32(1)),
                                 Value::Svalue(ScalarValue::Integer32(2)),
                                 Value::Svalue(ScalarValue::Integer32(3))])));
    }

    #[test]
    fn single_value_group() {
        let a_value = &b"{ x = 1; y = 2; };"[..];
        let res = value(a_value);

        let mut grp = SettingsList::new();
        grp.insert("x".to_string(),
                   Setting::new("x".to_string(), Value::Svalue(ScalarValue::Integer32(1))));
        grp.insert("y".to_string(),
                   Setting::new("y".to_string(), Value::Svalue(ScalarValue::Integer32(2))));

        assert_eq!(res, Done(&b";"[..], Value::Group(grp)));
    }

    #[test]
    fn single_group() {
        let a_group = &b"{ x = 1; y = 2; }"[..];
        let res = group(a_group);

        let mut grp = SettingsList::new();
        grp.insert("x".to_string(),
                   Setting::new("x".to_string(), Value::Svalue(ScalarValue::Integer32(1))));
        grp.insert("y".to_string(),
                   Setting::new("y".to_string(), Value::Svalue(ScalarValue::Integer32(2))));

        assert_eq!(res, Done(&b""[..], grp));
    }

    #[test]
    fn single_list() {
        let a_list = &b"(1, 2, 3)"[..];
        let res = list(a_list);
        assert_eq!(res, Done(&b""[..],
                             Value::List(vec![
                                 Value::Svalue(ScalarValue::Integer32(1)),
                                 Value::Svalue(ScalarValue::Integer32(2)),
                                 Value::Svalue(ScalarValue::Integer32(3))])));
    }

    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~ Top level parser tests ~~~
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    #[test]
    fn blank_conf() {
        let confs = vec![
            conf(&b"     \n"[..]),
            conf(&b"\t\t"[..]),
            conf(&b"\r"[..]),
            conf(&b"\r\n   \t  \t\r\n\n\n\n\r\r\r\r  \n"[..])];

        for pconf in confs.into_iter() {
            assert_eq!(pconf, Done(&b""[..], Config::new(SettingsList::new())));
        }
    }

    #[test]
    fn empty_conf() {
        let parsed = conf(&b""[..]);
        assert_eq!(parsed, Done(&b""[..], Config::new(SettingsList::new())));
    }

    #[test]
    fn only_comments() {
        let parsed = conf(&concat!(
            "// This conf consists of comments and nothing else.\n",
            "// Well, I mean, comments and newlines, that is.\n",
            "\n\n\n\n\n",
            "/* This is a block comment.\n",
            " * It spans multiple lines.\n",
            " * It can be closed with `*` followed by `/`\n",
            " * Block comments do not nest. That is, /* does not\n",
            " * open another comment block.\n",
            " */ \n",
            "// That was the end of our example.\n",
            "// Attempting to start a block comment inside a line comment has no effect.\n",
            "// For example, this won't start a block comment: /* no, it doesn't work! /**/\n",
            " # Single line comments can also start with `#` rather than `//`.\n",
            "\r\n\r\n\r\n\r\n                                 ",
            "/// That's it for now. Bye!\n\n\n").as_bytes()[..]);

        assert_eq!(parsed, Done(&b""[..], Config::new(SettingsList::new())));
    }

    #[test]
    fn conf_boolean_scalar() {
        let parsed = conf(&b"windows=NO;\nlinux = true;\nUNIX\t=\nFaLsE;\n"[..]);

        let mut expected = SettingsList::new();
        expected.insert("windows".to_string(),
                        Setting::new("windows".to_string(),
                                     Value::Svalue(ScalarValue::Boolean(false))));
        expected.insert("linux".to_string(),
                        Setting::new("linux".to_string(),
                                     Value::Svalue(ScalarValue::Boolean(true))));
        expected.insert("UNIX".to_string(),
                        Setting::new("UNIX".to_string(),
                                     Value::Svalue(ScalarValue::Boolean(false))));

        assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }

    #[test]
    fn conf_integer32_scalar() {
        let parsed = conf(&concat!("\n\nmiles :  3;mpg=27;\nweight_lbs = \t44;\t\n\n",
                                   "something_big = 2000000000;").as_bytes()[..]);

        let mut expected = SettingsList::new();
        expected.insert("miles".to_string(),
                        Setting::new("miles".to_string(),
                                     Value::Svalue(ScalarValue::Integer32(3))));
        expected.insert("mpg".to_string(),
                        Setting::new("mpg".to_string(),
                                     Value::Svalue(ScalarValue::Integer32(27))));
        expected.insert("weight_lbs".to_string(),
                        Setting::new("weight_lbs".to_string(),
                                     Value::Svalue(ScalarValue::Integer32(44))));
        expected.insert("something_big".to_string(),
                        Setting::new("something_big".to_string(),
                                     Value::Svalue(ScalarValue::Integer32(2000000000))));

        assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }

    #[test]
    fn conf_integer64_scalar() {
        let parsed = conf(&concat!("miles: 300000000000000L\r\n;",
                                   "\r\n\n\nmpg=2L;",
                                   "weight_lbs=922000000000000000L;\n",
                                   "loan_amount : \r\n8000000000000000001L;\t\t").as_bytes()[..]);

        let mut expected = SettingsList::new();
        expected.insert("miles".to_string(),
                        Setting::new("miles".to_string(),
                                     Value::Svalue(ScalarValue::Integer64(300000000000000))));
        expected.insert("mpg".to_string(),
                        Setting::new("mpg".to_string(),
                                     Value::Svalue(ScalarValue::Integer64(2))));
        expected.insert("weight_lbs".to_string(),
                        Setting::new("weight_lbs".to_string(),
                                     Value::Svalue(ScalarValue::Integer64(922000000000000000i64))));
        expected.insert("loan_amount".to_string(),
                        Setting::new("loan_amount".to_string(),
                                     Value::Svalue(ScalarValue::Integer64(
                                         8000000000000000001i64))));

        assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }

    #[test]
    fn conf_flt32_scalar() {
        let parsed = conf(&concat!("width = 5.0e0;\r\n",
                                   "height=1040.0e-1;\r\n",
                                   "misc=\t2.5e+4;\r\n",
                                   "height_x=4.0e3;\r\n",
                                   "xpto=.1;\r\n",
                                   "xpto2 = 2.;\r\n",
                                   "out_of_names = .5e1;\r\n",
                                   "keep_going =   .5e+1;\r\n",
                                   "lalala = .5e-5;\r\n",
                                   "num = 2.e1;\r\n",
                                   "num_ = 2.e-2;\r\n",
                                   "num__ = 2.e+2;\r\n").as_bytes()[..]);

        let mut expected = SettingsList::new();
        expected.insert("width".to_string(),
                        Setting::new("width".to_string(),
                                     Value::Svalue(ScalarValue::Floating32(5.0))));

        expected.insert("height".to_string(),
                        Setting::new("height".to_string(),
                                     Value::Svalue(ScalarValue::Floating32(1040e-1))));

        expected.insert("misc".to_string(),
                        Setting::new("misc".to_string(),
                                     Value::Svalue(ScalarValue::Floating32(2.5e+4))));

        expected.insert("height_x".to_string(),
                        Setting::new("height_x".to_string(),
                                     Value::Svalue(ScalarValue::Floating32(4.0e3))));

        expected.insert("xpto".to_string(),
                        Setting::new("xpto".to_string(),
                                     Value::Svalue(ScalarValue::Floating32(0.1))));

        expected.insert("xpto2".to_string(),
                        Setting::new("xpto2".to_string(),
                                     Value::Svalue(ScalarValue::Floating32(2.0))));

        expected.insert("out_of_names".to_string(),
                        Setting::new("out_of_names".to_string(),
                                     Value::Svalue(ScalarValue::Floating32(0.5e1))));

        expected.insert("keep_going".to_string(),
                        Setting::new("keep_going".to_string(),
                                     Value::Svalue(ScalarValue::Floating32(0.5e+1))));

        expected.insert("lalala".to_string(),
                        Setting::new("lalala".to_string(),
                                     Value::Svalue(ScalarValue::Floating32(0.5e-5))));

        expected.insert("num".to_string(),
                        Setting::new("num".to_string(),
                                     Value::Svalue(ScalarValue::Floating32(2.0e1))));

        expected.insert("num_".to_string(),
                        Setting::new("num_".to_string(),
                                     Value::Svalue(ScalarValue::Floating32(2.0e-2))));

        expected.insert("num__".to_string(),
                        Setting::new("num__".to_string(),
                                     Value::Svalue(ScalarValue::Floating32(2.0e+2))));
          
      assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }

    #[test]
    fn conf_flt64_scalar() {
        let parsed = conf(&b"miles: 55937598585.5L;\tdistance:10000000000.25L;"[..]);

        let mut expected = SettingsList::new();
        expected.insert("miles".to_string(),
                        Setting::new("miles".to_string(),
                                     Value::Svalue(ScalarValue::Floating64(55937598585.5))));
        expected.insert("distance".to_string(),
                        Setting::new("distance".to_string(),
                                     Value::Svalue(ScalarValue::Floating64(10000000000.25))));

        assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }

    #[test]
    fn conf_simple_str() {
        let parsed = conf(&b"\n\nserver_name\t= \"testing.org\"\r\n\r\n;"[..]);

        let mut expected = SettingsList::new();
        expected.insert("server_name".to_string(),
                        Setting::new("server_name".to_string(),
                                     Value::Svalue(ScalarValue::Str("testing.org".to_string()))));

        assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }

    #[test]
    fn conf_str() {
        let parsed = conf(
            &concat!("\n\n\nserver_name\t= \"testing.org\"\r\n\r\n;\r\n\r\n",
                     "escaped_str=\"Just a \\\"test\\\" with escapes.\";",
                     "str_w_prime = \"He said: 'Hello!'\";\n",
                     "quotes_everywhere = \"\\\"\\\"\";\n",
                     "backslashes = \"A backslash in quotes: \\\"\\\\\\\"\";\n",
                     "i=\"escaped_str=\\\"Just a \\\\\\\"test\\\\\\\" ",
                     "with escapes.\\\";\";").as_bytes()[..]);

        let mut expected = SettingsList::new();
        expected.insert("server_name".to_string(),
                        Setting::new("server_name".to_string(),
                                     Value::Svalue(ScalarValue::Str("testing.org".to_string()))));
        expected.insert("escaped_str".to_string(),
                        Setting::new("escaped_str".to_string(),
                                     Value::Svalue(ScalarValue::Str("Just a \"test\" with escapes."
                                                                    .to_string()))));
        expected.insert("str_w_prime".to_string(),
                        Setting::new("str_w_prime".to_string(),
                                     Value::Svalue(ScalarValue::Str("He said: 'Hello!'"
                                                                    .to_string()))));
        expected.insert("quotes_everywhere".to_string(),
                        Setting::new("quotes_everywhere".to_string(),
                                     Value::Svalue(ScalarValue::Str("\"\"".to_string()))));
        expected.insert("backslashes".to_string(),
                        Setting::new("backslashes".to_string(),
                                     Value::Svalue(ScalarValue::Str("A backslash in quotes: \"\\\""
                                                                    .to_string()))));
        /* Yes, this one is tricky. Here's how to break it down:
         * The string literal representing the RHS of this setting is:
         * escaped_str=\\\"Just a \\\\\\\"test\\\\\\\" with escapes.\\\";
         * At compile-time, Rust sees it as:
         * escaped_str=\"Just a \\\"test\\\" with escapes.\"
         * This is also what the parser will see. So, the expected result is:
         * escaped_str="Just a \"test\" with escapes."
         * This is the raw string associated to the setting `i` in our test.
         * Finally, we escape special chars to get again a string literal for the expected result:
         * escaped_str=\"Just a \\\"test\\\" with escapes.\"
         */
        expected.insert("i".to_string(),
                        Setting::new("i".to_string(),
                                     Value::Svalue(
                                         ScalarValue::Str(
                                             "escaped_str=\"Just a \\\"test\\\" with escapes.\";"
                                                 .to_string()))));

        assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }

    #[test]
    fn conf_multiline_str() {
        let parsed = conf(
            &concat!("\n\n\nserver_name\t= \"testing.org\"\r\n\r\n;\r\n\r\n",
                     "big_str = \"This is a very big string. It will span multiple lines.\"\n",
                     "          \" This line is still part of our very big string.\"\n",
                     "          \" We could do this all day\"\". Notice we can also use a single\"",
                     "          \" space to separate string literals components,\"\t\"",
                     " or even tabs!\"; another_str = \"bye\";").as_bytes()[..]);

        let big_str = concat!("This is a very big string. It will span multiple lines.",
                              " This line is still part of our very big string.",
                              " We could do this all day. Notice we can also use a single",
                              " space to separate string literals components, or even tabs!");

        let mut expected = SettingsList::new();
        expected.insert("server_name".to_string(),
                        Setting::new("server_name".to_string(),
                                     Value::Svalue(ScalarValue::Str("testing.org".to_string()))));
        expected.insert("big_str".to_string(),
                        Setting::new("big_str".to_string(),
                                     Value::Svalue(ScalarValue::Str(big_str.to_string()))));
        expected.insert("another_str".to_string(),
                        Setting::new("another_str".to_string(),
                                     Value::Svalue(ScalarValue::Str("bye".to_string()))));

        assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }

    #[test]
    fn conf_empty_array() {
        let parsed = conf(&b"array_one = [\n\n\n\n\n];\r\narray_two=[];"[..]);

        let mut expected = SettingsList::new();
        expected.insert("array_one".to_string(),
                        Setting::new("array_one".to_string(), Value::Array(Vec::new())));
        expected.insert("array_two".to_string(),
                        Setting::new("array_two".to_string(), Value::Array(Vec::new())));

        assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }

    #[test]
    fn conf_boolean_array() {
        let parsed = conf(&b"my_array = [true, true, YEs, No, FaLSE, false, true];"[..]);

        let mut expected = SettingsList::new();
        expected.insert("my_array".to_string(),
                        Setting::new("my_array".to_string(),
                                     Value::Array(vec![
                                         Value::Svalue(ScalarValue::Boolean(true)),
                                         Value::Svalue(ScalarValue::Boolean(true)),
                                         Value::Svalue(ScalarValue::Boolean(true)),
                                         Value::Svalue(ScalarValue::Boolean(false)),
                                         Value::Svalue(ScalarValue::Boolean(false)),
                                         Value::Svalue(ScalarValue::Boolean(false)),
                                         Value::Svalue(ScalarValue::Boolean(true))])));

        assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }

    #[test]
    fn conf_integer32_array() {
        let parsed = conf(&b"my_array: [10, 11, 12];\narray = [1];\n"[..]);

        let mut expected = SettingsList::new();
        expected.insert("my_array".to_string(),
                        Setting::new("my_array".to_string(),
                                     Value::Array(vec![
                                         Value::Svalue(ScalarValue::Integer32(10)),
                                         Value::Svalue(ScalarValue::Integer32(11)),
                                         Value::Svalue(ScalarValue::Integer32(12))])));
        expected.insert("array".to_string(),
                        Setting::new("array".to_string(),
                                     Value::Array(vec![Value::Svalue(ScalarValue::Integer32(1))])));

        assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }

    #[test]
    fn conf_integer64_array() {
        let parsed = conf(&b"a=[9000000000000000000L,8000000000000000002L,5L];\nb=[5L,6L,7L];"[..]);

        let mut expected = SettingsList::new();
        expected.insert("a".to_string(),
                        Setting::new("a".to_string(),
                                     Value::Array(vec![
                                         Value::Svalue(ScalarValue::Integer64(
                                             9000000000000000000i64)),
                                         Value::Svalue(ScalarValue::Integer64(
                                             8000000000000000002i64)),
                                         Value::Svalue(ScalarValue::Integer64(5))])));
        expected.insert("b".to_string(),
                        Setting::new("b".to_string(),
                                     Value::Array(vec![
                                         Value::Svalue(ScalarValue::Integer64(5)),
                                         Value::Svalue(ScalarValue::Integer64(6)),
                                         Value::Svalue(ScalarValue::Integer64(7))])));

        assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }

    #[test]
    fn conf_flt32_array() {
        let parsed = conf(&b"a=[4.5, 0.5, 0.25]\n;\nb = [5.0e-1, 1.0e0];\n\n"[..]);

        let mut expected = SettingsList::new();
        expected.insert("a".to_string(),
                        Setting::new("a".to_string(),
                                     Value::Array(vec![
                                         Value::Svalue(ScalarValue::Floating32(4.5)),
                                         Value::Svalue(ScalarValue::Floating32(0.5)),
                                         Value::Svalue(ScalarValue::Floating32(0.25))])));

        expected.insert("b".to_string(),
                        Setting::new("b".to_string(),
                                     Value::Array(vec![
                                         Value::Svalue(ScalarValue::Floating32(5.0e-1)),
                                         Value::Svalue(ScalarValue::Floating32(1.0))])));

        assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }

    #[test]
    fn conf_flt64_array() {
        let parsed = conf(&b"a=[55937598585.5L,10000000000.25L];"[..]);

        let mut expected = SettingsList::new();
        expected.insert("a".to_string(),
                        Setting::new("a".to_string(),
                                     Value::Array(vec![
                                         Value::Svalue(ScalarValue::Floating64(55937598585.5)),
                                         Value::Svalue(ScalarValue::Floating64(10000000000.25))])));

        assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }

    #[test]
    fn conf_str_arrays() {
        let parsed = conf(
            &concat!("my_strs = [                          ",
                     "\"testing.org\"                , ",
                     "\"Just a \\\"test\\\" with escapes.\",",
                     "\"He said: 'Hello!'\", ",
                     "\"\\\"\\\"\"\t\t, ",
                     "\"A backslash in quotes: \\\"\\\\\\\"\",",
                     "\"escaped_str=\\\"Just a \\\\\\\"test\\\\\\\" with escapes.\\\";\", ",
                     "\"\\n\\r\\t\\\"\"\n\n]\n;\n",
                     "my_simple_strs = [\"hello\", \"world\"];\n").as_bytes()[..]);

        let mut expected = SettingsList::new();
        expected.insert("my_strs".to_string(),
                        Setting::new("my_strs".to_string(),
                                     Value::Array(vec![
                                         Value::Svalue(ScalarValue::Str("testing.org".to_string())),
                                         Value::Svalue(ScalarValue::Str(
                                             "Just a \"test\" with escapes.".to_string())),
                                         Value::Svalue(ScalarValue::Str(
                                             "He said: 'Hello!'".to_string())),
                                         Value::Svalue(ScalarValue::Str("\"\"".to_string())),
                                         Value::Svalue(ScalarValue::Str(
                                             "A backslash in quotes: \"\\\"".to_string())),
                                             Value::Svalue(ScalarValue::Str(
                                                 concat!("escaped_str=\"Just a",
                                                         " \\\"test\\\" with escapes.\";")
                                                     .to_string())),
                                         Value::Svalue(ScalarValue::Str(
                                             "\n\r\t\"".to_string()))])));
        expected.insert("my_simple_strs".to_string(),
                        Setting::new("my_simple_strs".to_string(),
                                     Value::Array(vec![
                                         Value::Svalue(ScalarValue::Str("hello".to_string())),
                                         Value::Svalue(ScalarValue::Str("world".to_string()))])));

        assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }

    #[test]
    fn conf_simple_bad_array() {
        let parsed = conf(&b"bad_array = [\"a bad array\", 12, 3.0e-1, true];\n"[..]);
        assert_eq!(parsed, Error(Code(0)));
    }

    #[test]
    fn conf_bad_array() {
        let parsed = conf(&b"bad_array = [\"a bad array\", (\"array\", 5, 4, 2)];\n"[..]);
        assert_eq!(parsed, Error(Code(0)));
    }

    #[test]
    fn conf_bad_array_not_scalar() {
        let parsed = conf(&b"bad_array = [(1, 2, 3), (4, 5, 6)];\n"[..]);
        assert_eq!(parsed, Error(Code(0)));
    }

    #[test]
    fn conf_empty_list() {
        let parsed = conf(&b"list=();final=\n(\t  \n) \n;"[..]);

        let mut expected = SettingsList::new();
        expected.insert("list".to_string(),
                        Setting::new("list".to_string(), Value::List(Vec::new())));
        expected.insert("final".to_string(),
                        Setting::new("final".to_string(), Value::List(Vec::new())));

        assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }

    #[test]
    fn conf_nested_empty_list() {
        let parsed = conf(&b"list=((()));\n"[..]);

        let mut expected = SettingsList::new();
        expected.insert("list".to_string(),
                        Setting::new("list".to_string(),
                                     Value::List(vec![Value::List(vec![Value::List(Vec::new())])])));

        assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }

    #[test]
    fn conf_scalar_lists() {
        let parsed = conf(&concat!("my_list = (\n\"a \\\"string\\\" with \\nquo\\ttes\",\n",
                                   "15, 0.25e+2, 9000000000000000000L, 54, 55937598585.5L,\n",
                                   "yes\n,\ntrue\t,false,NO\n\n\n);\nanother_list=(10, \"0\");\n",
                                   "another_list\n=\n(\n   yes, 19, \"bye\"\n)\n;\n",
                                   "last_one:(true);\n").as_bytes()[..]);

        let mut expected = SettingsList::new();
        expected.insert("my_list".to_string(),
                        Setting::new("my_list".to_string(),
                                     Value::List(vec![
                                         Value::Svalue(
                                             ScalarValue::Str("a \"string\" with \nquo\ttes"
                                                              .to_string())),
                                         Value::Svalue(ScalarValue::Integer32(15)),
                                         Value::Svalue(ScalarValue::Floating32(0.25e+2)),
                                         Value::Svalue(
                                             ScalarValue::Integer64(9000000000000000000i64)),
                                         Value::Svalue(ScalarValue::Integer32(54)),
                                         Value::Svalue(ScalarValue::Floating64(55937598585.5f64)),
                                         Value::Svalue(ScalarValue::Boolean(true)),
                                         Value::Svalue(ScalarValue::Boolean(true)),
                                         Value::Svalue(ScalarValue::Boolean(false)),
                                         Value::Svalue(ScalarValue::Boolean(false))])));
        expected.insert("another_list".to_string(),
                        Setting::new("another_list".to_string(),
                                     Value::List(vec![
                                         Value::Svalue(ScalarValue::Boolean(true)),
                                         Value::Svalue(ScalarValue::Integer32(19)),
                                         Value::Svalue(ScalarValue::Str("bye".to_string()))])));
        expected.insert("last_one".to_string(),
                        Setting::new("last_one".to_string(),
                                     Value::List(vec![Value::Svalue(ScalarValue::Boolean(true))])));

        assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }

    #[test]
    fn conf_values_list() {
        let parsed = conf(&concat!("my_superb_list = (",
                                   "[yes, no], 21, [0.25, .5, .125],",
                                   "(()), ((\"a\")), (\"a\"), [\"\\\"x\\\"\"],",
                                   "(14, [\"x\"], (true, (false, (4), [5, 6]), \"y\")),",
                                   "\"goodbye!\\r\\n\", ",
                                   "{ s = [1, 2]; x = \"str\"; y = (); });\n").as_bytes()[..]);

        let mut group_in_list = SettingsList::new();
        group_in_list.insert("s".to_string(),
                             Setting::new("s".to_string(),
                                          Value::Array(vec![
                                              Value::Svalue(ScalarValue::Integer32(1)),
                                              Value::Svalue(ScalarValue::Integer32(2))])));
        group_in_list.insert("x".to_string(),
                             Setting::new("x".to_string(),
                                          Value::Svalue(ScalarValue::Str("str".to_string()))));

        group_in_list.insert("y".to_string(),
                             Setting::new("y".to_string(), Value::List(Vec::new())));


        let list_elements = vec![
            Value::Array(vec![
                Value::Svalue(ScalarValue::Boolean(true)),
                Value::Svalue(ScalarValue::Boolean(false))]),
            Value::Svalue(ScalarValue::Integer32(21)),
            Value::Array(vec![
                Value::Svalue(ScalarValue::Floating32(0.25)),
                Value::Svalue(ScalarValue::Floating32(0.5)),
                Value::Svalue(ScalarValue::Floating32(0.125))]),
            Value::List(vec![Value::List(Vec::new())]),
            Value::List(vec![Value::List(vec![Value::Svalue(ScalarValue::Str("a".to_string()))])]),
            Value::List(vec![Value::Svalue(ScalarValue::Str("a".to_string()))]),
            Value::Array(vec![Value::Svalue(ScalarValue::Str("\"x\"".to_string()))]),
            Value::List(vec![Value::Svalue(ScalarValue::Integer32(14)),
                             Value::Array(vec![Value::Svalue(ScalarValue::Str("x".to_string()))]),
                             Value::List(vec![Value::Svalue(ScalarValue::Boolean(true)),
                                              Value::List(vec![
                                                  Value::Svalue(ScalarValue::Boolean(false)),
                                                  Value::List(vec![
                                                      Value::Svalue(ScalarValue::Integer32(4))]),
                                                  Value::Array(vec![
                                                      Value::Svalue(ScalarValue::Integer32(5)),
                                                      Value::Svalue(ScalarValue::Integer32(6))])]),
                                              Value::Svalue(ScalarValue::Str("y".to_string()))])]),
            Value::Svalue(ScalarValue::Str("goodbye!\r\n".to_string())),
            Value::Group(group_in_list)];

        let mut expected = SettingsList::new();
        expected.insert("my_superb_list".to_string(),
                        Setting::new("my_superb_list".to_string(), Value::List(list_elements)));

        assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }

    #[test]
    fn sample_conf_small() {
        let parsed = conf(&concat!(
            "\n\napplication:\n",
            "{\n",
            "  window:\n",
            "  {\n",
            "    title = \"My Application\";\n",
            "    size = { w = 640; h = 480; };\n",
            "  };\n",
            "  a = 5;\n",
            "  ff = 1.E6;\n",
            "  group1:\n",
            "  {\n",
            "    x = 5;  y = 10;\n",
            "    my_array = [ 10, 11, 12 ];\n",
            "    flag = TRUE;\n",
            "    states = [\"CT\", \"CA\", \"TX\", \"NV\", \"FL\"];",
            "  };\n",
            "};\n").as_bytes()[..]);

        let mut size_group = SettingsList::new();
        size_group.insert("w".to_string(),
                          Setting::new("w".to_string(),
                                       Value::Svalue(ScalarValue::Integer32(640))));
        size_group.insert("h".to_string(),
                          Setting::new("h".to_string(),
                                       Value::Svalue(ScalarValue::Integer32(480))));

        let mut window_group = SettingsList::new();
        window_group.insert("title".to_string(),
                            Setting::new("title".to_string(),
                                         Value::Svalue(ScalarValue::Str("My Application"
                                                                        .to_string()))));
        window_group.insert("size".to_string(),
                            Setting::new("size".to_string(), Value::Group(size_group)));

        let mut group1 = SettingsList::new();
        group1.insert("x".to_string(),
                      Setting::new("x".to_string(), Value::Svalue(ScalarValue::Integer32(5))));
        group1.insert("y".to_string(),
                      Setting::new("y".to_string(), Value::Svalue(ScalarValue::Integer32(10))));
        group1.insert("my_array".to_string(),
                      Setting::new("my_array".to_string(),
                                   Value::Array(vec![
                                       Value::Svalue(ScalarValue::Integer32(10)),
                                       Value::Svalue(ScalarValue::Integer32(11)),
                                       Value::Svalue(ScalarValue::Integer32(12))])));
        group1.insert("flag".to_string(),
                      Setting::new("flag".to_string(), Value::Svalue(ScalarValue::Boolean(true))));
        group1.insert("states".to_string(),
                      Setting::new("states".to_string(),
                                   Value::Array(vec![
                                       Value::Svalue(ScalarValue::Str("CT".to_string())),
                                       Value::Svalue(ScalarValue::Str("CA".to_string())),
                                       Value::Svalue(ScalarValue::Str("TX".to_string())),
                                       Value::Svalue(ScalarValue::Str("NV".to_string())),
                                       Value::Svalue(ScalarValue::Str("FL".to_string()))])));

        let mut app_group = SettingsList::new();
        app_group.insert("window".to_string(),
                         Setting::new("window".to_string(), Value::Group(window_group)));
        app_group.insert("a".to_string(),
                         Setting::new("a".to_string(), Value::Svalue(ScalarValue::Integer32(5))));
        app_group.insert("ff".to_string(),
                         Setting::new("ff".to_string(),
                                      Value::Svalue(ScalarValue::Floating32(1e6))));
        app_group.insert("group1".to_string(),
                         Setting::new("group1".to_string(), Value::Group(group1)));

        let mut expected = SettingsList::new();
        expected.insert("application".to_string(),
                        Setting::new("application".to_string(), Value::Group(app_group)));

        assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }

    #[test]
    fn sample_conf_comments() {
        let parsed = conf(&concat!(
            "\n\napplication:\n",
            "{//This is a comment. It spans until the end of the line\n",
            "  window:\n",
            "  {\n",
            "    title =/*the app title \r\n\r\n\r\n*/\"My Application\"//Another comment;\n;\n",
            "    size = { w = 640; h// = 480; };\n = 480; };\n",
            "  }; //This was for the window. Now the rest.\n",
            "  a = 5;\n",
            "  ff = 1.E6;\n",
            "  # This is going to be called `group1`\n",
            "  group1:\n",
            "  {\n",
            "    x = 5;  y = 10;\n",
            "    my_array = [ /*comments*/10, /*everywhere*/11, 12 ];\n",
            "    flag = TRUE;\n",
            "    //DING DONG!\nstates = [\"CT\", \"CA\", \"TX\", \"NV\", \"FL\"];",
            "  };\n",
            "};\n").as_bytes()[..]);

        let mut size_group = SettingsList::new();
        size_group.insert("w".to_string(),
                          Setting::new("w".to_string(),
                                       Value::Svalue(ScalarValue::Integer32(640))));
        size_group.insert("h".to_string(),
                          Setting::new("h".to_string(),
                                       Value::Svalue(ScalarValue::Integer32(480))));

        let mut window_group = SettingsList::new();
        window_group.insert("title".to_string(),
                            Setting::new("title".to_string(),
                                         Value::Svalue(ScalarValue::Str("My Application"
                                                                        .to_string()))));
        window_group.insert("size".to_string(),
                            Setting::new("size".to_string(), Value::Group(size_group)));

        let mut group1 = SettingsList::new();
        group1.insert("x".to_string(),
                      Setting::new("x".to_string(), Value::Svalue(ScalarValue::Integer32(5))));
        group1.insert("y".to_string(),
                      Setting::new("y".to_string(), Value::Svalue(ScalarValue::Integer32(10))));
        group1.insert("my_array".to_string(),
                      Setting::new("my_array".to_string(),
                                   Value::Array(vec![
                                       Value::Svalue(ScalarValue::Integer32(10)),
                                       Value::Svalue(ScalarValue::Integer32(11)),
                                       Value::Svalue(ScalarValue::Integer32(12))])));
        group1.insert("flag".to_string(),
                      Setting::new("flag".to_string(), Value::Svalue(ScalarValue::Boolean(true))));
        group1.insert("states".to_string(),
                      Setting::new("states".to_string(),
                                   Value::Array(vec![
                                       Value::Svalue(ScalarValue::Str("CT".to_string())),
                                       Value::Svalue(ScalarValue::Str("CA".to_string())),
                                       Value::Svalue(ScalarValue::Str("TX".to_string())),
                                       Value::Svalue(ScalarValue::Str("NV".to_string())),
                                       Value::Svalue(ScalarValue::Str("FL".to_string()))])));

        let mut app_group = SettingsList::new();
        app_group.insert("window".to_string(),
                         Setting::new("window".to_string(), Value::Group(window_group)));
        app_group.insert("a".to_string(),
                         Setting::new("a".to_string(), Value::Svalue(ScalarValue::Integer32(5))));
        app_group.insert("ff".to_string(),
                         Setting::new("ff".to_string(),
                                      Value::Svalue(ScalarValue::Floating32(1e6))));
        app_group.insert("group1".to_string(),
                         Setting::new("group1".to_string(), Value::Group(group1)));

        let mut expected = SettingsList::new();
        expected.insert("application".to_string(),
                        Setting::new("application".to_string(), Value::Group(app_group)));

        assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }

    #[test]
    fn sample_conf_all_features() {
        let my_conf = &concat!(
            "#----------------------------\n",
            "# Example Configuration File\n",
            "#---------------------------\n",
            "#\n",
            "\n",
            "application:\n",
            "{\n",
            "\n",
            " /* This section defines some settings for our\n",
            "  * main application window, such as size and\n",
            "  * position.\n",
            "  */\n",
            "\n",
            "  window:\n",
            "  {\n",
            "    title = \"My Application\";\n",
            "    size = { /* width */ w = 640; /* height */ h = 480; };\n",
            "    pos = { x = 350; y = 250; };\n",
            "  };\n",
            "\n",
            "  a = 5;\n",
            "  b = 6;\n",
            "  ff = 1.0E6;\n",
            "  test-comment = \"/* hello\\n \\\"there\\\"*/\";\n",
            "\n",
            "  test-long-string = \"A very long string that spans multiple lines. \"\n",
            "  /* but wait, there's more... */ \"Adjacent strings are automatically\"\n",
            "  \" concatenated.\";\n",
            "\n",
            "  test-escaped-string = \"\\\"This is\\n a test.\\\"\";\n",
            "\n",
            "  group1:\n",
            "  {\n",
            "    x = 5;  y = 10;\n",
            "    my_array = [ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22 ];\n",
            "    flag = TRUE;\n",
            "\n",
            "    group2: { zzz = \"this is a test\"; };\n",
            "\n",
            "    states = [	\"CT\", // Connecticut\n",
            "		\"CA\", // California\n",
            "		\"TX\", // Texas\n",
            "		\"NV\", // Nevada\n",
            "		\"FL\"  // Florida\n",
            "    ];\n",
            "  };\n",
            "\n",
            "};\n",
            "\n",
            "// Commented for now\n",
            "//binary = [ 0xAA, 0xBB, 0xCC ];\n",
            "\n",
            "list = ( ( \"abc\", 123, true ), 1.234, ( /* an empty list */ ) ,[ 1, 2, 3 ],\n",
            "	   { a = (1, 2, true); } );\n",
            "\n",
            "books = ( \"inventory\",\n",
            "          { title  = \"Treasure Island\";\n",
            "            author = \"Robert Louis Stevenson\";\n",
            "            price  = 29.99;\n",
            "            qty    = 5; },\n",
            "          { title  = \"Snow Crash\";\n",
            "            author = \"Neal Stephenson\";\n",
            "            price  = 9.99;\n",
            "            qty    = 8; });\n",
            "\n",
            "# miscellaneous stuff\n",
            "\n",
            "misc:\n",
            "{\n",
            "  port = 5000;\n",
            "  minutes = 3.0e0;\n",
            "  enabled = FALSE;\n",
            "  // Commented for now",
            "  // mask = 0xAABBCCDD;\n",
            "  unicode = \"STARGÎ›ÌŠTE SG-1\"; // UTF-8 string\n",
            "  bigint = 9223372036854775807L;\n",
            "  // Commented for now\n",
            "  // bighex = 0x1122334455667788L;\n",
            "};\n",
            "\n",
            "\n",
            "### eof\n").as_bytes()[..];


        let parsed = conf(my_conf);

        let mut size_group = SettingsList::new();
        size_group.insert("w".to_string(),
                          Setting::new("w".to_string(),
                                       Value::Svalue(ScalarValue::Integer32(640))));
        size_group.insert("h".to_string(),
                          Setting::new("h".to_string(),
                                       Value::Svalue(ScalarValue::Integer32(480))));

        let mut pos_group = SettingsList::new();
        pos_group.insert("x".to_string(),
                         Setting::new("x".to_string(),
                                      Value::Svalue(ScalarValue::Integer32(350))));
        pos_group.insert("y".to_string(),
                         Setting::new("y".to_string(),
                                      Value::Svalue(ScalarValue::Integer32(250))));

        let mut window_group = SettingsList::new();
        window_group.insert("title".to_string(),
                            Setting::new("title".to_string(),
                                         Value::Svalue(ScalarValue::Str("My Application"
                                                                        .to_string()))));
        window_group.insert("size".to_string(),
                            Setting::new("size".to_string(), Value::Group(size_group)));
        window_group.insert("pos".to_string(),
                            Setting::new("pos".to_string(), Value::Group(pos_group)));

        let mut group2 = SettingsList::new();
        group2.insert("zzz".to_string(),
                      Setting::new("zzz".to_string(),
                                   Value::Svalue(ScalarValue::Str("this is a test".to_string()))));

        let mut group1 = SettingsList::new();
        group1.insert("x".to_string(),
                      Setting::new("x".to_string(), Value::Svalue(ScalarValue::Integer32(5))));
        group1.insert("y".to_string(),
                      Setting::new("y".to_string(), Value::Svalue(ScalarValue::Integer32(10))));
        group1.insert("my_array".to_string(),
                      Setting::new("my_array".to_string(),
                                   Value::Array(vec![
                                       Value::Svalue(ScalarValue::Integer32(10)),
                                       Value::Svalue(ScalarValue::Integer32(11)),
                                       Value::Svalue(ScalarValue::Integer32(12)),
                                       Value::Svalue(ScalarValue::Integer32(13)),
                                       Value::Svalue(ScalarValue::Integer32(14)),
                                       Value::Svalue(ScalarValue::Integer32(15)),
                                       Value::Svalue(ScalarValue::Integer32(16)),
                                       Value::Svalue(ScalarValue::Integer32(17)),
                                       Value::Svalue(ScalarValue::Integer32(18)),
                                       Value::Svalue(ScalarValue::Integer32(19)),
                                       Value::Svalue(ScalarValue::Integer32(20)),
                                       Value::Svalue(ScalarValue::Integer32(21)),
                                       Value::Svalue(ScalarValue::Integer32(22))])));
        group1.insert("flag".to_string(),
                      Setting::new("flag".to_string(), Value::Svalue(ScalarValue::Boolean(true))));
        group1.insert("group2".to_string(),
                      Setting::new("group2".to_string(), Value::Group(group2)));
        group1.insert("states".to_string(),
                      Setting::new("states".to_string(),
                                   Value::Array(vec![
                                       Value::Svalue(ScalarValue::Str("CT".to_string())),
                                       Value::Svalue(ScalarValue::Str("CA".to_string())),
                                       Value::Svalue(ScalarValue::Str("TX".to_string())),
                                       Value::Svalue(ScalarValue::Str("NV".to_string())),
                                       Value::Svalue(ScalarValue::Str("FL".to_string()))])));

        let mut app_group = SettingsList::new();
        app_group.insert("window".to_string(),
                         Setting::new("window".to_string(), Value::Group(window_group)));
        app_group.insert("a".to_string(),
                         Setting::new("a".to_string(), Value::Svalue(ScalarValue::Integer32(5))));
        app_group.insert("b".to_string(),
                         Setting::new("b".to_string(), Value::Svalue(ScalarValue::Integer32(6))));
        app_group.insert("ff".to_string(),
                         Setting::new("ff".to_string(),
                                      Value::Svalue(ScalarValue::Floating32(1e6))));
        app_group.insert("test-comment".to_string(),
                         Setting::new("test-comment".to_string(),
                                      Value::Svalue(ScalarValue::Str("/* hello\n \"there\"*/"
                                                                     .to_string()))));
        app_group.insert("test-long-string".to_string(),
                         Setting::new("test-long-string".to_string(),
                                      Value::Svalue(ScalarValue::Str(
                                          concat!("A very long string that spans multiple lines. ",
                                                  "Adjacent strings are automatically",
                                                  " concatenated.").to_string()))));
        app_group.insert("test-escaped-string".to_string(),
                         Setting::new("test-escaped-string".to_string(),
                                      Value::Svalue(ScalarValue::Str("\"This is\n a test.\""
                                                                     .to_string()))));
        app_group.insert("group1".to_string(),
                         Setting::new("group1".to_string(), Value::Group(group1)));

        let mut expected = SettingsList::new();
        expected.insert("application".to_string(),
                        Setting::new("application".to_string(), Value::Group(app_group)));

        let mut group_a = SettingsList::new();
        group_a.insert("a".to_string(),
                       Setting::new("a".to_string(),
                                    Value::List(vec![
                                        Value::Svalue(ScalarValue::Integer32(1)),
                                        Value::Svalue(ScalarValue::Integer32(2)),
                                        Value::Svalue(ScalarValue::Boolean(true))])));

        let list_elements = vec![
            Value::List(vec![
                Value::Svalue(ScalarValue::Str("abc".to_string())),
                Value::Svalue(ScalarValue::Integer32(123)),
                Value::Svalue(ScalarValue::Boolean(true))]),
            Value::Svalue(ScalarValue::Floating32(1.234)),
            Value::List(Vec::new()),
            Value::Array(vec![
                Value::Svalue(ScalarValue::Integer32(1)),
                Value::Svalue(ScalarValue::Integer32(2)),
                Value::Svalue(ScalarValue::Integer32(3))]),
            Value::Group(group_a)];

        expected.insert("list".to_string(),
                        Setting::new("list".to_string(),
                                     Value::List(list_elements)));

        let mut inventory_group_1 = SettingsList::new();
        inventory_group_1.insert("title".to_string(),
                                 Setting::new("title".to_string(),
                                              Value::Svalue(ScalarValue::Str(
                                                  "Treasure Island".to_string()))));
        inventory_group_1.insert("author".to_string(),
                                 Setting::new("author".to_string(),
                                              Value::Svalue(ScalarValue::Str(
                                                  "Robert Louis Stevenson".to_string()))));
        inventory_group_1.insert("price".to_string(),
                                 Setting::new("price".to_string(),
                                              Value::Svalue(ScalarValue::Floating32(29.99))));
        inventory_group_1.insert("qty".to_string(),
                                 Setting::new("qty".to_string(),
                                              Value::Svalue(ScalarValue::Integer32(5))));

        let mut inventory_group_2 = SettingsList::new();
        inventory_group_2.insert("title".to_string(),
                                 Setting::new("title".to_string(),
                                              Value::Svalue(ScalarValue::Str(
                                                  "Snow Crash".to_string()))));
        inventory_group_2.insert("author".to_string(),
                                 Setting::new("author".to_string(),
                                              Value::Svalue(ScalarValue::Str(
                                                  "Neal Stephenson".to_string()))));
        inventory_group_2.insert("price".to_string(),
                                 Setting::new("price".to_string(),
                                              Value::Svalue(ScalarValue::Floating32(9.99))));
        inventory_group_2.insert("qty".to_string(),
                                 Setting::new("qty".to_string(),
                                              Value::Svalue(ScalarValue::Integer32(8))));


        let books_list_elements = vec![
            Value::Svalue(ScalarValue::Str("inventory".to_string())),
            Value::Group(inventory_group_1),
            Value::Group(inventory_group_2)];

        expected.insert("books".to_string(),
                        Setting::new("books".to_string(),
                                     Value::List(books_list_elements)));

        let mut misc_group = SettingsList::new();
        misc_group.insert("port".to_string(),
                          Setting::new("port".to_string(),
                                       Value::Svalue(ScalarValue::Integer32(5000))));
        misc_group.insert("minutes".to_string(),
                          Setting::new("minutes".to_string(),
                                       Value::Svalue(ScalarValue::Floating32(3.0))));
        misc_group.insert("enabled".to_string(),
                          Setting::new("enabled".to_string(),
                                       Value::Svalue(ScalarValue::Boolean(false))));
        misc_group.insert("unicode".to_string(),
                          Setting::new("unicode".to_string(),
                                       Value::Svalue(ScalarValue::Str(
                                           "STARGÎ›ÌŠTE SG-1".to_string()))));
        misc_group.insert("bigint".to_string(),
                          Setting::new("bigint".to_string(),
                                       Value::Svalue(ScalarValue::Integer64(
                                           9223372036854775807i64))));

        expected.insert("misc".to_string(),
                        Setting::new("misc".to_string(),
                                     Value::Group(misc_group)));

        assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }

    #[test]
    fn sample_conf_all_features_no_newline_on_end() {
        let my_conf = &concat!(
            "#----------------------------\n",
            "# Example Configuration File\n",
            "#---------------------------\n",
            "#\n",
            "\n",
            "application:\n",
            "{\n",
            "\n",
            " /* This section defines some settings for our\n",
            "  * main application window, such as size and\n",
            "  * position.\n",
            "  */\n",
            "\n",
            "  window:\n",
            "  {\n",
            "    title = \"My Application\";\n",
            "    size = { /* width */ w = 640; /* height */ h = 480; };\n",
            "    pos = { x = 350; y = 250; };\n",
            "  };\n",
            "\n",
            "  a = 5;\n",
            "  b = 6;\n",
            "  ff = 1.0E6;\n",
            "  test-comment = \"/* hello\\n \\\"there\\\"*/\";\n",
            "\n",
            "  test-long-string = \"A very long string that spans multiple lines. \"\n",
            "  /* but wait, there's more... */ \"Adjacent strings are automatically\"\n",
            "  \" concatenated.\";\n",
            "\n",
            "  test-escaped-string = \"\\\"This is\\n a test.\\\"\";\n",
            "\n",
            "  group1:\n",
            "  {\n",
            "    x = 5;  y = 10;\n",
            "    my_array = [ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22 ];\n",
            "    flag = TRUE;\n",
            "\n",
            "    group2: { zzz = \"this is a test\"; };\n",
            "\n",
            "    states = [	\"CT\", // Connecticut\n",
            "		\"CA\", // California\n",
            "		\"TX\", // Texas\n",
            "		\"NV\", // Nevada\n",
            "		\"FL\"  // Florida\n",
            "    ];\n",
            "  };\n",
            "\n",
            "};\n",
            "\n",
            "// Commented for now\n",
            "//binary = [ 0xAA, 0xBB, 0xCC ];\n",
            "\n",
            "list = ( ( \"abc\", 123, true ), 1.234, ( /* an empty list */ ) ,[ 1, 2, 3 ],\n",
            "	   { a = (1, 2, true); } );\n",
            "\n",
            "books = ( \"inventory\",\n",
            "          { title  = \"Treasure Island\";\n",
            "            author = \"Robert Louis Stevenson\";\n",
            "            price  = 29.99;\n",
            "            qty    = 5; },\n",
            "          { title  = \"Snow Crash\";\n",
            "            author = \"Neal Stephenson\";\n",
            "            price  = 9.99;\n",
            "            qty    = 8; });\n",
            "\n",
            "# miscellaneous stuff\n",
            "\n",
            "misc:\n",
            "{\n",
            "  port = 5000;\n",
            "  minutes = 3.0e0;\n",
            "  enabled = FALSE;\n",
            "  // Commented for now",
            "  // mask = 0xAABBCCDD;\n",
            "  unicode = \"STARGÎ›ÌŠTE SG-1\"; // UTF-8 string\n",
            "  bigint = 9223372036854775807L;\n",
            "  // Commented for now\n",
            "  // bighex = 0x1122334455667788L;\n",
            "};\n",
            "\n",
            "\n",
            "### eof").as_bytes()[..];


        let parsed = conf(my_conf);

        let mut size_group = SettingsList::new();
        size_group.insert("w".to_string(),
                          Setting::new("w".to_string(),
                                       Value::Svalue(ScalarValue::Integer32(640))));
        size_group.insert("h".to_string(),
                          Setting::new("h".to_string(),
                                       Value::Svalue(ScalarValue::Integer32(480))));

        let mut pos_group = SettingsList::new();
        pos_group.insert("x".to_string(),
                         Setting::new("x".to_string(),
                                      Value::Svalue(ScalarValue::Integer32(350))));
        pos_group.insert("y".to_string(),
                         Setting::new("y".to_string(),
                                      Value::Svalue(ScalarValue::Integer32(250))));

        let mut window_group = SettingsList::new();
        window_group.insert("title".to_string(),
                            Setting::new("title".to_string(),
                                         Value::Svalue(ScalarValue::Str("My Application"
                                                                        .to_string()))));
        window_group.insert("size".to_string(),
                            Setting::new("size".to_string(), Value::Group(size_group)));
        window_group.insert("pos".to_string(),
                            Setting::new("pos".to_string(), Value::Group(pos_group)));

        let mut group2 = SettingsList::new();
        group2.insert("zzz".to_string(),
                      Setting::new("zzz".to_string(),
                                   Value::Svalue(ScalarValue::Str("this is a test".to_string()))));

        let mut group1 = SettingsList::new();
        group1.insert("x".to_string(),
                      Setting::new("x".to_string(), Value::Svalue(ScalarValue::Integer32(5))));
        group1.insert("y".to_string(),
                      Setting::new("y".to_string(), Value::Svalue(ScalarValue::Integer32(10))));
        group1.insert("my_array".to_string(),
                      Setting::new("my_array".to_string(),
                                   Value::Array(vec![
                                       Value::Svalue(ScalarValue::Integer32(10)),
                                       Value::Svalue(ScalarValue::Integer32(11)),
                                       Value::Svalue(ScalarValue::Integer32(12)),
                                       Value::Svalue(ScalarValue::Integer32(13)),
                                       Value::Svalue(ScalarValue::Integer32(14)),
                                       Value::Svalue(ScalarValue::Integer32(15)),
                                       Value::Svalue(ScalarValue::Integer32(16)),
                                       Value::Svalue(ScalarValue::Integer32(17)),
                                       Value::Svalue(ScalarValue::Integer32(18)),
                                       Value::Svalue(ScalarValue::Integer32(19)),
                                       Value::Svalue(ScalarValue::Integer32(20)),
                                       Value::Svalue(ScalarValue::Integer32(21)),
                                       Value::Svalue(ScalarValue::Integer32(22))])));
        group1.insert("flag".to_string(),
                      Setting::new("flag".to_string(), Value::Svalue(ScalarValue::Boolean(true))));
        group1.insert("group2".to_string(),
                      Setting::new("group2".to_string(), Value::Group(group2)));
        group1.insert("states".to_string(),
                      Setting::new("states".to_string(),
                                   Value::Array(vec![
                                       Value::Svalue(ScalarValue::Str("CT".to_string())),
                                       Value::Svalue(ScalarValue::Str("CA".to_string())),
                                       Value::Svalue(ScalarValue::Str("TX".to_string())),
                                       Value::Svalue(ScalarValue::Str("NV".to_string())),
                                       Value::Svalue(ScalarValue::Str("FL".to_string()))])));

        let mut app_group = SettingsList::new();
        app_group.insert("window".to_string(),
                         Setting::new("window".to_string(), Value::Group(window_group)));
        app_group.insert("a".to_string(),
                         Setting::new("a".to_string(), Value::Svalue(ScalarValue::Integer32(5))));
        app_group.insert("b".to_string(),
                         Setting::new("b".to_string(), Value::Svalue(ScalarValue::Integer32(6))));
        app_group.insert("ff".to_string(),
                         Setting::new("ff".to_string(),
                                      Value::Svalue(ScalarValue::Floating32(1e6))));
        app_group.insert("test-comment".to_string(),
                         Setting::new("test-comment".to_string(),
                                      Value::Svalue(ScalarValue::Str("/* hello\n \"there\"*/"
                                                                     .to_string()))));
        app_group.insert("test-long-string".to_string(),
                         Setting::new("test-long-string".to_string(),
                                      Value::Svalue(ScalarValue::Str(
                                          concat!("A very long string that spans multiple lines. ",
                                                  "Adjacent strings are automatically",
                                                  " concatenated.").to_string()))));
        app_group.insert("test-escaped-string".to_string(),
                         Setting::new("test-escaped-string".to_string(),
                                      Value::Svalue(ScalarValue::Str("\"This is\n a test.\""
                                                                     .to_string()))));
        app_group.insert("group1".to_string(),
                         Setting::new("group1".to_string(), Value::Group(group1)));

        let mut expected = SettingsList::new();
        expected.insert("application".to_string(),
                        Setting::new("application".to_string(), Value::Group(app_group)));

        let mut group_a = SettingsList::new();
        group_a.insert("a".to_string(),
                       Setting::new("a".to_string(),
                                    Value::List(vec![
                                        Value::Svalue(ScalarValue::Integer32(1)),
                                        Value::Svalue(ScalarValue::Integer32(2)),
                                        Value::Svalue(ScalarValue::Boolean(true))])));

        let list_elements = vec![
            Value::List(vec![
                Value::Svalue(ScalarValue::Str("abc".to_string())),
                Value::Svalue(ScalarValue::Integer32(123)),
                Value::Svalue(ScalarValue::Boolean(true))]),
            Value::Svalue(ScalarValue::Floating32(1.234)),
            Value::List(Vec::new()),
            Value::Array(vec![
                Value::Svalue(ScalarValue::Integer32(1)),
                Value::Svalue(ScalarValue::Integer32(2)),
                Value::Svalue(ScalarValue::Integer32(3))]),
            Value::Group(group_a)];

        expected.insert("list".to_string(),
                        Setting::new("list".to_string(),
                                     Value::List(list_elements)));

        let mut inventory_group_1 = SettingsList::new();
        inventory_group_1.insert("title".to_string(),
                                 Setting::new("title".to_string(),
                                              Value::Svalue(ScalarValue::Str(
                                                  "Treasure Island".to_string()))));
        inventory_group_1.insert("author".to_string(),
                                 Setting::new("author".to_string(),
                                              Value::Svalue(ScalarValue::Str(
                                                  "Robert Louis Stevenson".to_string()))));
        inventory_group_1.insert("price".to_string(),
                                 Setting::new("price".to_string(),
                                              Value::Svalue(ScalarValue::Floating32(29.99))));
        inventory_group_1.insert("qty".to_string(),
                                 Setting::new("qty".to_string(),
                                              Value::Svalue(ScalarValue::Integer32(5))));

        let mut inventory_group_2 = SettingsList::new();
        inventory_group_2.insert("title".to_string(),
                                 Setting::new("title".to_string(),
                                              Value::Svalue(ScalarValue::Str(
                                                  "Snow Crash".to_string()))));
        inventory_group_2.insert("author".to_string(),
                                 Setting::new("author".to_string(),
                                              Value::Svalue(ScalarValue::Str(
                                                  "Neal Stephenson".to_string()))));
        inventory_group_2.insert("price".to_string(),
                                 Setting::new("price".to_string(),
                                              Value::Svalue(ScalarValue::Floating32(9.99))));
        inventory_group_2.insert("qty".to_string(),
                                 Setting::new("qty".to_string(),
                                              Value::Svalue(ScalarValue::Integer32(8))));


        let books_list_elements = vec![
            Value::Svalue(ScalarValue::Str("inventory".to_string())),
            Value::Group(inventory_group_1),
            Value::Group(inventory_group_2)];

        expected.insert("books".to_string(),
                        Setting::new("books".to_string(),
                                     Value::List(books_list_elements)));

        let mut misc_group = SettingsList::new();
        misc_group.insert("port".to_string(),
                          Setting::new("port".to_string(),
                                       Value::Svalue(ScalarValue::Integer32(5000))));
        misc_group.insert("minutes".to_string(),
                          Setting::new("minutes".to_string(),
                                       Value::Svalue(ScalarValue::Floating32(3.0))));
        misc_group.insert("enabled".to_string(),
                          Setting::new("enabled".to_string(),
                                       Value::Svalue(ScalarValue::Boolean(false))));
        misc_group.insert("unicode".to_string(),
                          Setting::new("unicode".to_string(),
                                       Value::Svalue(ScalarValue::Str(
                                           "STARGÎ›ÌŠTE SG-1".to_string()))));
        misc_group.insert("bigint".to_string(),
                          Setting::new("bigint".to_string(),
                                       Value::Svalue(ScalarValue::Integer64(
                                           9223372036854775807i64))));

        expected.insert("misc".to_string(),
                        Setting::new("misc".to_string(),
                                     Value::Group(misc_group)));

        assert_eq!(parsed, Done(&b""[..], Config::new(expected)));
    }
}
