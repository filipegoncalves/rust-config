//! Errors that can occur while parsing a configuration

use std::error::Error;
use std::fmt;
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
    pub detail: Option<String>,

    err_desc: String
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

fn mk_error<E: fmt::Display>(kind: ConfigErrorKind, desc: &'static str, err: E) -> ConfigError {
    let err_desc = format!("{:?} {}: {}", kind, desc, err);
    ConfigError {
        kind: kind,
        desc: desc,
        detail: Some(format!("{}", err)),

        err_desc: err_desc
    }
}

/// Converts an I/O Error into a `ConfigError`
pub fn from_io_err(err: IoError) -> ConfigError {
    mk_error(ConfigErrorKind::IoError, "An I/O error has occurred", err)
}

/// Converts a `ParseError` into a `ConfigError`
pub fn from_parse_err(err: ParseError) -> ConfigError {
    mk_error(ConfigErrorKind::ParseError, "Syntax error", err)
}

impl fmt::Display for ConfigError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "{}", self.description()));
        Ok(())
    }
}

impl Error for ConfigError {
    fn description(&self) -> &str {
        &self.err_desc[..]
    }
}
