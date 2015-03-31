
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


#[cfg(test)]
mod test {

    use super::from_str;

    use parser::Config;
    use parser::{SettingsList, Setting, Value, ScalarValue};
    use error::{ConfigErrorKind};

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
    fn conf_from_str_with_err() {
        let parsed = from_str("windows=NO\nlinux=true;\n");
        assert!(parsed.is_err());
        assert_eq!(parsed.unwrap_err().kind, ConfigErrorKind::ParseError);
    }
}
