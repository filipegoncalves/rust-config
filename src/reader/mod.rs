use std::io::Read;
use std::path::Path;
use std::fs::File;

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

pub fn from_file(path: &Path) -> Result<Config, ConfigError> {
    let mut file = match File::open(path) {
        Ok(f) => f,
        Err(e) => return Err(from_io_err(e))
    };
    from_stream(&mut file)
}

pub fn from_str(input: &str) -> Result<Config, ConfigError> {
    parse(input).map_err(|e| from_parse_err(e))
}

#[cfg(test)]
mod test {

    use super::{from_str, from_stream, from_file};

    use parser::Config;
    use parser::{SettingsList, Setting, Value, ScalarValue};
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
                Err(IoError::new(ErrorKind::Other, "An I/O error has occurred.", None))
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
        let parsed = from_file(Path::new("examples/sample.conf"));
        assert!(parsed.is_ok());
    }
}
