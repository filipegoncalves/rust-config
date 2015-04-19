//! Reader types to parse a configuration.

use std::io::Read;
use std::path::Path;
use std::fs::File;

use types::Config;
use parser::parse;
use error::ConfigError;
use error::{from_io_err, from_parse_err};

/// Reads a configuration from a generic stream.
/// Errors can be caused by:
///
/// * An I/O error on `stream`, in which case no parsing was done
/// * A syntax error
///
/// If a syntax error is reported, it means that the stream successfully delivered every piece of
/// data, since parsing doesn't start until the whole input is read to memory.
/// # Examples
/// For educational / demonstration purposes, we can wrap a string inside a `Cursor` to simulate
/// a stream of data:
///
/// ```
/// use std::io::Cursor;
/// use config::reader::from_stream;
///
/// let sample_conf = "windows=NO;\nlinux = YES;\n";
/// let mut cursor = Cursor::new(sample_conf.as_bytes());
/// let parsed = from_stream(&mut cursor);
/// assert!(parsed.is_ok());
/// ```
///
/// In this example, we do the same, but with a broken conf:
///
/// ```
/// use std::io::Cursor;
/// use config::reader::from_stream;
/// use config::error::ConfigErrorKind;
///
/// let sample_conf = "windows=\n";
/// let mut cursor = Cursor::new(sample_conf.as_bytes());
/// let parsed = from_stream(&mut cursor);
/// assert!(parsed.is_err());
/// assert_eq!(parsed.unwrap_err().kind, ConfigErrorKind::ParseError);
/// ```
///
/// The other situation where an error is returned is when the underlying stream
/// yields an I/O error. We can simulate this behavior by implementing a reader that
/// always returns an error:
///
/// ```
/// use std::io::{Read, Cursor};
/// use std::io::Error as IoError;
/// use std::io::Result as IoResult;
/// use std::io::ErrorKind;
///
/// use config::reader::from_stream;
/// use config::error::ConfigErrorKind;
///
/// struct BadCursor;
///
/// impl Read for BadCursor {
///     fn read(&mut self, buf: &mut [u8]) -> IoResult<usize> {
///         Err(IoError::new(ErrorKind::Other, "An I/O error has occurred."))
///     }
/// }
///
/// let parsed = from_stream(&mut BadCursor);
/// assert!(parsed.is_err());
/// assert_eq!(parsed.unwrap_err().kind, ConfigErrorKind::IoError);
///
/// ```
///
pub fn from_stream<T: Read>(stream: &mut T) -> Result<Config, ConfigError> {
    let mut buf = String::new();

    match stream.read_to_string(&mut buf) {
        Ok(_) => from_str(&buf[..]),
        Err(e) => Err(from_io_err(e))
    }
}

/// Reads a configuration from a UTF-8 file.
/// Errors can be caused by:
///
/// * An error when trying to locate / open the file. This is treated as an I/O error.
/// * A syntax error
///
/// Errors upon opening the file can happen due to the file not existing, or bad permissions, etc.
///
/// # Examples
/// This reads and parses a configuration stored in `examples/sample.conf`:
///
/// ```
/// use std::path::Path;
///
/// use config::reader::from_file;
///
/// let parsed = from_file(Path::new("tests/sample.conf"));
/// assert!(parsed.is_ok());
/// ```
///
pub fn from_file(path: &Path) -> Result<Config, ConfigError> {
    let mut file = match File::open(path) {
        Ok(f) => f,
        Err(e) => return Err(from_io_err(e))
    };
    from_stream(&mut file)
}

/// Reads a configuration from a string slice.
/// The only possible error that can occur is a syntax error.
/// # Examples
///
/// ```
/// use config::reader::from_str;
/// use config::error::ConfigErrorKind;
///
/// let parsed = from_str("windows=NO;\nlinux=true;\n");
/// assert!(parsed.is_ok());
/// ```
///
/// This will return a syntax error (missing a semi-colon)
///
/// ```
/// use config::reader::from_str;
/// use config::error::ConfigErrorKind;
///
/// let parsed = from_str("windows=NO\n");
/// assert!(parsed.is_err());
/// assert_eq!(parsed.unwrap_err().kind, ConfigErrorKind::ParseError);
/// ```
///
pub fn from_str(input: &str) -> Result<Config, ConfigError> {
    parse(input).map_err(|e| from_parse_err(e))
}

#[cfg(test)]
mod test {

    use super::{from_str, from_stream, from_file};

    use types::Config;
    use types::{SettingsList, Setting, Value, ScalarValue};
    use error::{ConfigErrorKind};

    use std::io::{Read, Cursor};
    use std::io::Error as IoError;
    use std::io::ErrorKind;
    use std::io::Result as IoResult;
    use std::path::Path;

    struct BadStrCursor<'a> {
        cursor: Cursor<&'a [u8]>,
        calls: u16,
        max_calls_before_err: u16
    }

    impl<'a> BadStrCursor<'a> {
        fn new(data: &'a [u8], max_calls: u16) -> BadStrCursor<'a> {
            BadStrCursor { cursor: Cursor::new(data), calls: 0, max_calls_before_err: max_calls }
        }
    }

    impl<'a> Read for BadStrCursor<'a> {
        fn read(&mut self, buf: &mut [u8]) -> IoResult<usize> {
            self.calls += 1;
            if self.calls >= self.max_calls_before_err {
                Err(IoError::new(ErrorKind::Other, "An I/O error has occurred."))
            } else {
                self.cursor.read(buf)
            }
        }
    }

    #[test]
    fn conf_from_str() {
        let parsed = from_str("windows=NO;\nlinux = true;\nUNIX\t=\nFaLsE;\n");
        assert!(parsed.is_ok());

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

        assert_eq!(parsed.unwrap(), Config::new(expected));
    }

    #[test]
    fn conf_from_str_parse_err() {
        let parsed = from_str("windows=NO\nlinux=true;\n");
        assert!(parsed.is_err());
        assert_eq!(parsed.unwrap_err().kind, ConfigErrorKind::ParseError);
    }

    #[test]
    fn conf_from_stream() {
        let sample_conf = "windows=NO;\nlinux = true;\nUNIX\t=\nFaLsE;\n";
        let mut cursor = Cursor::new(sample_conf.as_bytes());
        let parsed = from_stream(&mut cursor);

        assert!(parsed.is_ok());
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

        assert_eq!(parsed.unwrap(), Config::new(expected));
    }

    #[test]
    fn conf_from_stream_parse_err() {
        let sample_conf = "windows=NO\nlinux = true;\n";
        let mut cursor = Cursor::new(sample_conf.as_bytes());
        let parsed = from_stream(&mut cursor);

        assert!(parsed.is_err());
        assert_eq!(parsed.unwrap_err().kind, ConfigErrorKind::ParseError);
    }

    #[test]
    fn conf_from_stream_io_err() {
        let sample_conf = "windows=NO;\nlinux = true;\n";
        let mut bad_cursor = BadStrCursor::new(sample_conf.as_bytes(), 1);
        let parsed = from_stream(&mut bad_cursor);

        assert!(parsed.is_err());
        assert_eq!(parsed.unwrap_err().kind, ConfigErrorKind::IoError);
    }

    #[test]
    fn conf_from_file() {
        let parsed = from_file(Path::new("tests/sample.conf"));
        assert!(parsed.is_ok());
    }
}
