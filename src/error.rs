//! Errors that can occur while parsing a configuration

use std::io::Error as IoError;
use parser::ParseError;

/// A generic configuration error type
#[derive(Debug)]
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
pub enum ConfigErrorKind {
    /// An I/O error. Can only occur if reading from a stream (file, socket, etc.)
    IoError,
    /// A syntax error
    ParseError
}

/// Converts an I/O Error into a `ConfigError`
pub fn from_io_err(err: IoError) -> ConfigError {
    ConfigError {
        kind: ConfigErrorKind::IoError,
        desc: "An I/O error has occurred",
        detail: Some(format!("{}", err))
    }
}

/// Converts a `ParseError` into a `ConfigError`
pub fn from_parse_err(err: ParseError) -> ConfigError {
    ConfigError {
        kind: ConfigErrorKind::ParseError,
        desc: "Syntax error",
        detail: Some(format!("{}", err))
    }
}
