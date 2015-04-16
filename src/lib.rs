//! A library to parse and load configuration files.
//!
//! This library parses and loads configuration files in much the same way as C/C++'s
//! [libconfig](http://www.hyperrealm.com/libconfig/). At this time, It can only read and load
//! configurations, but future plans include a much richer user API to allow other forms of
//! interaction, such as manipulating values, writing the current configuration to a file, etc.
//!
//!# Getting started
//!
//! A configuration file consists of a group of settings. A setting is a key/value pair, where the
//! key is a string of the form `[a-zA-Z][-a-zA-Z0-9_]*`, and the value is anything represented by
//! `types::Value`. In a configuration file, setting/value pairs are written as
//! `setting_name = value;`. Possible values include primitive integer and floating point types,
//! strings, arrays, lists, etc.
//!
//! The library provides a bunch of methods to read the configuration data in the `reader` module.
//! The possibilities include reading from a string, from a file, or from anything that implements
//! the `Read` trait.
//!
//! As of now, every reader will first attempt to read the whole configuration into memory,
//! and only begins parsing once everything was read.
//!
//! In case of error, the parser returns a `config::error`.
//! On success, the parser returns a `Config`, which can then be used to browse the loaded data.
//!
//! Browsing is typically done with the `lookup_*` methods implemented by `Config`. The `lookup`
//! methods work with *paths*. Essentially, a path unambiguously describes the location of
//! a setting.
//!
//! For example, consider this input configuration file:
//!
//!```ignore
//! title = "My HTTP server";
//! listen_ports = [ 80, 443 ];
//! misc = {
//!     owner = "Chuck Norris";
//!     location = "CA";
//!     contact = {
//!         phone = "415-256-9999";
//!         // This is an array. Arrays start with `[` and end with `]`
//!         // Arrays are homogeneous and can only hold scalar data types.
//!         // See types::ScalarValue for further information
//!         emails = ["chuck@norris.com", "chuck.norris@gmail.com"];
//!     };
//! };
//!```
//!
//! At the top level, we have 3 settings: `title`, `listen_ports` and `misc`. `misc` is itself a
//! group of settings. Each value in this configuration is reachable by a unique path of settings
//! to follow. Each setting in a path is separated by `.`
//!
//! **Example**: The path `misc.contact.phone` identifies the setting with value `415-256-9999`.
//!
//! Array and list types are indexed using `[i]` in a path.
//!
//! **Example**: The path `listen_ports.[1]` identifies the setting with value `443`.
//!
//! **Example**: The path `misc.contact.emails.[0]` identifies the setting with value
//! `chuck@norris.com`
//!
//! Lists, as opposed to arrays, are heterogeneous and can store any data type, including other
//! lists. Here's a setting consisting of a heterogeneous list:
//!
//!```ignore
//! // This is a list. Lists start with `(` and end with `)`
//! // Lists are heterogeneous and can store any data type, including other lists
//! a_setting = ("a string", // The first element is a string
//!              ((1, 2, 3)), // The 2nd element is a list storing a list of 3 integers
//!              misc = { x = 4; y = 3; } // 3rd element: a group
//!             );
//!```
//!
//! Here are some valid paths for this example:
//!
//! `a_setting.[0]` - returns the string `a string`
//!
//! `a_setting.[1].[0].[2]` - returns the integer `3`
//!
//! `a_setting.[2].misc.x` - returns the integer `4`
//!
//! Adjacent string literals separated by whitespace, newlines and / or comments are automatically
//! concatenated.
//!
//! **Example**:
//!
//! ```ignore
//! "a"/* a comment */" string"    " liter"
//!
//! // This is a commment
//!
//!    "al"
//! ```
//!
//! is equivalent to:
//!
//! ```ignore
//! "a string literal"
//! ```
//!
//! See the integration tests for sample use cases and more complex examples.
//!
//!# Grammar
//!
//! Here's the grammar that describes the configuration input format:
//!
//! ```ignore
//! TODO Write this
//! ```
//!

#![feature(rustc_private)]
#![feature(collections, str_char)]
#![feature(plugin)]
#![plugin(peg_syntax_ext)]

extern crate syntax;

mod parser;

pub mod types;
pub mod error;
pub mod reader;
