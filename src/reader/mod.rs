
use std::io::Read;

use parser::Config;
use parser::parse;
use error::ConfigError;
use error::{from_io_err, from_parse_err};

pub fn from_stream<T: Read>(stream: &mut T) -> Result<Config, ConfigError> {
    let mut buf = String::new();

    match stream.read_to_string(&mut buf) {
        Ok(_) => from_str(&buf[..]),
        Err(e) => Err(from_io_err(e))
    }
}

//pub fn from_file(path: String) -> Result<Config, ConfigError> {

//}

pub fn from_str(input: &str) -> Result<Config, ConfigError> {
    parse(input).map_err(|e| from_parse_err(e))
}
