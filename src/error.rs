//! This module defines the errors that can occur while parsing a configuration
//! Errors come in 3 flavors: TODO Write this

use std::io::Error as IoError;
use parser::ParseError;

/// A generic configuration error type
#[derive(Debug)]
#[unstable = "Library still under heavy development; design may change."]
pub struct ConfigError {
    /// Indicates what kind of error this is
    pub kind: ConfigErrorKind,
    /// A descriptive message about the error
    pub desc: &'static str,
    /// Error details, if available
    pub detail: Option<String>
}

/// Possible error kinds
#[derive(Debug)]
#[derive(PartialEq)]
#[unstable = "Library still under heavy development; design may change."]
pub enum ConfigErrorKind {
    /// An I/O error. Can only occur if reading from a stream (file, socket, etc.)
    IoError,
    /// A parse error - something is wrong with the input configuration
    ParseError
}

/// Converts an I/O Error into a `ConfigError`
#[unstable = "Library still under heavy development; design may change."]
pub fn from_io_err(err: IoError) -> ConfigError {
    ConfigError {
        kind: ConfigErrorKind::IoError,
        desc: "An I/O error has occurred",
        detail: Some(format!("{}", err))
    }
}

/// Converts a `ParseError` into a `ConfigError`
#[unstable = "Library still under heavy development; design may change."]
pub fn from_parse_err(err: ParseError) -> ConfigError {
    ConfigError {
        kind: ConfigErrorKind::ParseError,
        desc: "Syntax error",
        detail: Some(format!("{}", err))
    }
}
