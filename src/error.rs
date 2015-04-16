//! This module defines the errors that can occur while parsing a configuration
//! As of now, errors come in 2 flavors: I/O errors, and parse errors.
//! An I/O error occurs when the underlying stream yields an error. This can happen for example
//! when attempting to open a file that does not exist, or has the wrong permissions. Another
//! possibility is network problems on a `TcpStream`, if the configuration is being transferred
//! and read over the network.
//! Parse errors indicate a syntax error on the configuration format.

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
    /// A parse error - something is wrong with the input configuration
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
