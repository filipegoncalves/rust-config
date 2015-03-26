use std::collections::HashMap;

pub type SettingsList = HashMap<String, Setting>;

#[derive(PartialEq)]
#[derive(Debug)]
pub struct Setting {
    pub name: String,
    pub value: Value
}

#[derive(PartialEq)]
#[derive(Debug)]
pub enum Value {
    Svalue(ScalarValue),
    Array(ArrayValue),
    List(ListValue),
    Group(SettingsList)
}

#[derive(PartialEq)]
#[derive(Debug)]
pub enum ScalarValue {
    Boolean(bool),
    Integer32(i32),
    Integer64(i64),
    Floating32(f32),
    Floating64(f64),
    Str(String)
}

pub type ArrayValue = Vec<ScalarValue>;
pub type ListValue = Vec<Value>;

impl Setting {
    pub fn new(sname: String, val: Value) -> Setting {
        Setting { name: sname, value: val }
    }
}

peg_file! grammar("grammar.rustpeg");

pub mod prelude {
    pub use super::grammar::ParseError;
}

pub fn parse(config: &str) -> Result<SettingsList, grammar::ParseError> {
    grammar::conf(config)
}

#[cfg(test)]
mod test {
    use super::parse;
    use super::{Value, ScalarValue};
    use super::{SettingsList, Setting};
    #[test]
    fn empty_conf() {
        let parsed_conf = parse("");
        assert!(parsed_conf.is_ok());
        let my_conf = parsed_conf.unwrap();
        assert_eq!(my_conf.len(), 0);
    }

    // TODO Fix this test
/*
    #[test]
    fn blank_conf() {
        let confs = vec![
            parse("     \n"),
            parse("\t\t"),
            parse("\r"),
            parse("\r\n   \t  \t\r\n\n\n\n\r\r\r\r  \n")];

        for pconf in confs.into_iter() {
            assert!(pconf.is_ok());
            assert!(pconf.unwrap().len() == 0);
        }
    }
*/
    #[test]
    fn boolean_scalar_value() {
        let parsed = parse("windows=NO;\nlinux = true;\nUNIX\t=\nFaLsE;\n");
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

        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn integer32_scalar_value() {
        let parsed = parse(concat!("\n\nmiles :  3;mpg=27;\nweight_lbs = \t44;\t\n\n",
                                   "something_big = 2000000000;"));
        assert!(parsed.is_ok());

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

        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn integer64_scalar_value() {
        let parsed = parse(concat!("miles: 300000000000000L\r\n;",
                                   "\r\n\n\nmpg=2L;",
                                   "weight_lbs=922000000000000000L;\n",
                                   "loan_amount : \r\n8000000000000000001L;\t\t"));
        assert!(parsed.is_ok());

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
                                     Value::Svalue(ScalarValue::Integer64(8000000000000000001i64))));

        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn flt32_scalar_value() {
        let parsed = parse(concat!("width = 5.0e0;\r\n",
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
                                   "num__ = 2.e+2;\r\n"));

        assert!(parsed.is_ok());

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
          
      assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn flt64_scalar_value() {
        let parsed = parse("miles: 55937598585.5L;\tdistance:10000000000.25L;");
        assert!(parsed.is_ok());

        let mut expected = SettingsList::new();
        expected.insert("miles".to_string(),
                        Setting::new("miles".to_string(),
                                     Value::Svalue(ScalarValue::Floating64(55937598585.5))));
        expected.insert("distance".to_string(),
                        Setting::new("distance".to_string(),
                                     Value::Svalue(ScalarValue::Floating64(10000000000.25))));

        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn simple_str_scalar_value() {
        let parsed = parse("\n\nserver_name\t= \"testing.org\"\r\n\r\n;");
        assert!(parsed.is_ok());

        let mut expected = SettingsList::new();
        expected.insert("server_name".to_string(),
                        Setting::new("server_name".to_string(),
                                     Value::Svalue(ScalarValue::Str("testing.org".to_string()))));

        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn str_scalar_value() {
        let parsed = parse(
            concat!("\n\n\nserver_name\t= \"testing.org\"\r\n\r\n;\r\n\r\n",
                    "escaped_str=\"Just a \\\"test\\\" with escapes.\";",
                    "str_w_prime = \"He said: 'Hello!'\";\n",
                    "quotes_everywhere = \"\\\"\\\"\";\n",
                    "backslashes = \"A backslash in quotes: \\\"\\\\\\\"\";\n",
                    "i=\"escaped_str=\\\"Just a \\\\\\\"test\\\\\\\" with escapes.\\\";\";"));

        assert!(parsed.is_ok());

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

        assert_eq!(parsed.unwrap(), expected);
    }
}
