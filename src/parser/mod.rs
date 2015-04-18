//! The core parser module.
//! Upon successfully parsing a configuration, a `Config` is created. Conceptually, a
//! `Config` consits of a `SettingsList`, which is a map that binds a `Setting` name to a `Value`.
//!
//! This map is the basis for the rest of the library. The public library API is nothing more
//! than a simple set of wrappers to make it easier to manage a the `SettingsList` inside
//! a `Config`.
//!
//! When a parse call is invoked, a settings list is built as input is read. It is not expected that
//! library users manipulate or otherwise deal directly with these internal data structures.
//!
//! Most of the setting types allowed in a configuration will pretty much map to either a Rust
//! primitive type or a container.
//!
//! # Examples
//! This example shows how to create a settings list and store a `Boolean` scalar value named
//! `my_setting` with the boolean value `true`. This is how the parser works internally.
//!
//! The first step is to create a new, empty settings list:
//!
//! ```
//! use config::types::SettingsList;
//! # use config::types::ScalarValue;
//! # use config::types::Value;
//! # use config::types::Setting;
//!
//! let mut my_settings_list = SettingsList::new();
//! # let setting_name = "my_setting".to_string();
//! # let a_scalar = ScalarValue::Boolean(true);
//! # let setting_value = Value::Svalue(a_scalar);
//! # let my_setting = Setting::new(setting_name.clone(), setting_value);
//! # my_settings_list.insert(setting_name, my_setting);
//! ```
//!
//! Next, we define the setting name as *my_setting*:
//!
//! ```
//! # use config::types::SettingsList;
//! # use config::types::ScalarValue;
//! # use config::types::Value;
//! # use config::types::Setting;
//!
//! # let mut my_settings_list = SettingsList::new();
//! let setting_name = "my_setting".to_string();
//! # let a_scalar = ScalarValue::Boolean(true);
//! # let setting_value = Value::Svalue(a_scalar);
//! # let my_setting = Setting::new(setting_name.clone(), setting_value);
//! # my_settings_list.insert(setting_name, my_setting);
//! ```
//!
//! Then, we create a boolean scalar value holding `true`:
//!
//! ```
//! # use config::types::SettingsList;
//! use config::types::ScalarValue;
//! # use config::types::Value;
//! # use config::types::Setting;
//!
//! # let mut my_settings_list = SettingsList::new();
//! # let setting_name = "my_setting".to_string();
//! let a_scalar = ScalarValue::Boolean(true);
//! # let setting_value = Value::Svalue(a_scalar);
//! # let my_setting = Setting::new(setting_name.clone(), setting_value);
//! # my_settings_list.insert(setting_name, my_setting);
//! ```
//!
//! ... and we wrap it in a `Value`, because settings store generic values:
//!
//! ```
//! # use config::types::SettingsList;
//! # use config::types::ScalarValue;
//! use config::types::Value;
//! # use config::types::Setting;
//!
//! # let mut my_settings_list = SettingsList::new();
//! # let setting_name = "my_setting".to_string();
//! # let a_scalar = ScalarValue::Boolean(true);
//! let setting_value = Value::Svalue(a_scalar);
//! # let my_setting = Setting::new(setting_name.clone(), setting_value);
//! # my_settings_list.insert(setting_name, my_setting);
//! ```
//!
//! And finally, we create the new setting:
//!
//! ```
//! # use config::types::SettingsList;
//! # use config::types::ScalarValue;
//! # use config::types::Value;
//! use config::types::Setting;
//!
//! # let mut my_settings_list = SettingsList::new();
//! # let setting_name = "my_setting".to_string();
//! # let a_scalar = ScalarValue::Boolean(true);
//! # let setting_value = Value::Svalue(a_scalar);
//! let my_setting = Setting::new(setting_name.clone(), setting_value);
//! # my_settings_list.insert(setting_name, my_setting);
//! ```
//! ... and insert it into the settings list:
//!
//! ```
//! # use config::types::SettingsList;
//! # use config::types::ScalarValue;
//! # use config::types::Value;
//! # use config::types::Setting;
//!
//! # let mut my_settings_list = SettingsList::new();
//! # let setting_name = "my_setting".to_string();
//! # let a_scalar = ScalarValue::Boolean(true);
//! # let setting_value = Value::Svalue(a_scalar);
//! # let my_setting = Setting::new(setting_name.clone(), setting_value);
//! my_settings_list.insert(setting_name, my_setting);
//! ```
//!
//! Here's the complete example:
//! ```
//! use config::types::SettingsList;
//! use config::types::ScalarValue;
//! use config::types::Value;
//! use config::types::Setting;
//!
//! let mut my_settings_list = SettingsList::new();
//! let setting_name = "my_setting".to_string();
//! let a_scalar = ScalarValue::Boolean(true);
//! let setting_value = Value::Svalue(a_scalar);
//! let my_setting = Setting::new(setting_name.clone(), setting_value);
//! my_settings_list.insert(setting_name, my_setting);
//! ```
//!
//! As a final step, the parser creates a `Config` out of the `SettingsList`.
//! This is what the user sees and interacts with. It is as simple as:
//!
//! ```
//! # use config::types::SettingsList;
//! # use config::types::ScalarValue;
//! # use config::types::Value;
//! # use config::types::Setting;
//! use config::types::Config;
//!
//! # let mut my_settings_list = SettingsList::new();
//! # let setting_name = "my_setting".to_string();
//! # let a_scalar = ScalarValue::Boolean(true);
//! # let setting_value = Value::Svalue(a_scalar);
//! # let my_setting = Setting::new(setting_name.clone(), setting_value);
//! # my_settings_list.insert(setting_name, my_setting);
//! let my_config = Config::new(my_settings_list);
//! ```
//!


pub use parser::grammar::ParseError;
use parser::grammar::conf;
use types::Config;

peg_file! grammar("grammar.rustpeg");

/// Parses a configuration file from a `&str`.
/// A `ParseError` is returned in case of syntax error.
pub fn parse(config: &str) -> Result<Config, ParseError> {
    conf(config).and_then(|sl| Ok(Config::new(sl)))
}

#[cfg(test)]
mod test {
    use super::grammar::conf as parse_conf;
    use types::{Value, ScalarValue, SettingsList, Setting};

    #[test]
    fn empty_conf() {
        let parsed = parse_conf("");
        assert!(parsed.is_ok());
        assert_eq!(parsed.unwrap().len(), 0);
    }

    #[test]
    fn blank_conf() {
        let confs = vec![
            parse_conf("     \n"),
            parse_conf("\t\t"),
            parse_conf("\r"),
            parse_conf("\r\n   \t  \t\r\n\n\n\n\r\r\r\r  \n")];

        for pconf in confs.into_iter() {
            assert!(pconf.is_ok());
            assert!(pconf.unwrap().len() == 0);
        }
    }

    #[test]
    fn only_comments() {
        let conf = parse_conf(concat!(
            "// This conf consists of comments and nothing else.\n",
            "// Well, I mean, comments and newlines, that is.\n",
            "\n\n\n\n\n",
            "/* This is a block comment.\n",
            " * It spans multiple lines.\n",
            " * It can be closed with `*` followed by `/`\n",
            " * Block comments do not nest. That is, /* does not\n",
            " * open another comment block.\n",
            " */ \n",
            "// That was the end of our example.\n",
            "// Attempting to start a block comment inside a line comment has no effect.\n",
            "// For example, this won't start a block comment: /* no, it doesn't work! /**/\n",
            " # Single line comments can also start with `#` rather than `//`.\n",
            "\r\n\r\n\r\n\r\n                                 /// That's it for now. Bye!\n\n\n"));

        assert!(conf.is_ok());
        assert!(conf.unwrap().len() == 0);
    }

    #[test]
    fn boolean_scalar_value() {
        let parsed = parse_conf("windows=NO;\nlinux = true;\nUNIX\t=\nFaLsE;\n");
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
        let parsed = parse_conf(concat!("\n\nmiles :  3;mpg=27;\nweight_lbs = \t44;\t\n\n",
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
        let parsed = parse_conf(concat!("miles: 300000000000000L\r\n;",
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
                                     Value::Svalue(ScalarValue::Integer64(
                                         8000000000000000001i64))));

        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn flt32_scalar_value() {
        let parsed = parse_conf(concat!("width = 5.0e0;\r\n",
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
        let parsed = parse_conf("miles: 55937598585.5L;\tdistance:10000000000.25L;");
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
        let parsed = parse_conf("\n\nserver_name\t= \"testing.org\"\r\n\r\n;");
        assert!(parsed.is_ok());

        let mut expected = SettingsList::new();
        expected.insert("server_name".to_string(),
                        Setting::new("server_name".to_string(),
                                     Value::Svalue(ScalarValue::Str("testing.org".to_string()))));

        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn str_scalar_value() {
        let parsed = parse_conf(
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

    #[test]
    fn multiline_str_scalar_value() {
        let parsed = parse_conf(
            concat!("\n\n\nserver_name\t= \"testing.org\"\r\n\r\n;\r\n\r\n",
                    "big_str = \"This is a very big string. It will span multiple lines.\"\n",
                    "          \" This line is still part of our very big string.\"\n",
                    "          \" We could do this all day\" \". Notice we can also use a single\"",
                    "          \" space to separate string literals components,\"\t\"",
                    " or even tabs!\"; another_str = \"bye\";"));

        assert!(parsed.is_ok());

        let big_str = concat!("This is a very big string. It will span multiple lines.",
                              " This line is still part of our very big string.",
                              " We could do this all day. Notice we can also use a single",
                              " space to separate string literals components, or even tabs!");

        let mut expected = SettingsList::new();
        expected.insert("server_name".to_string(),
                        Setting::new("server_name".to_string(),
                                     Value::Svalue(ScalarValue::Str("testing.org".to_string()))));
        expected.insert("big_str".to_string(),
                        Setting::new("big_str".to_string(),
                                     Value::Svalue(ScalarValue::Str(big_str.to_string()))));
        expected.insert("another_str".to_string(),
                        Setting::new("another_str".to_string(),
                                     Value::Svalue(ScalarValue::Str("bye".to_string()))));

        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn empty_array() {
        let parsed = parse_conf("array_one = [\n\n\n\n\n];\r\narray_two=[];");

        assert!(parsed.is_ok());
        let mut expected = SettingsList::new();
        expected.insert("array_one".to_string(),
                        Setting::new("array_one".to_string(), Value::Array(Vec::new())));
        expected.insert("array_two".to_string(),
                        Setting::new("array_two".to_string(), Value::Array(Vec::new())));

        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn simple_boolean_array() {
        let parsed = parse_conf("my_array = [true, true, YEs, No, FaLSE, false, true];");

        assert!(parsed.is_ok());

        let mut expected = SettingsList::new();
        expected.insert("my_array".to_string(),
                        Setting::new("my_array".to_string(),
                                     Value::Array(vec![
                                         Value::Svalue(ScalarValue::Boolean(true)),
                                         Value::Svalue(ScalarValue::Boolean(true)),
                                         Value::Svalue(ScalarValue::Boolean(true)),
                                         Value::Svalue(ScalarValue::Boolean(false)),
                                         Value::Svalue(ScalarValue::Boolean(false)),
                                         Value::Svalue(ScalarValue::Boolean(false)),
                                         Value::Svalue(ScalarValue::Boolean(true))])));

        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn simple_integer32_array() {
        let parsed = parse_conf("my_array: [10, 11, 12];\narray = [1];\n");
        assert!(parsed.is_ok());

        let mut expected = SettingsList::new();
        expected.insert("my_array".to_string(),
                        Setting::new("my_array".to_string(),
                                     Value::Array(vec![
                                         Value::Svalue(ScalarValue::Integer32(10)),
                                         Value::Svalue(ScalarValue::Integer32(11)),
                                         Value::Svalue(ScalarValue::Integer32(12))])));
        expected.insert("array".to_string(),
                        Setting::new("array".to_string(),
                                     Value::Array(vec![Value::Svalue(ScalarValue::Integer32(1))])));

        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn simple_integer64_array() {
        let parsed = parse_conf("a=[9000000000000000000L,8000000000000000002L,5L];\nb=[5L,6L,7L];");

        assert!(parsed.is_ok());
        let mut expected = SettingsList::new();
        expected.insert("a".to_string(),
                        Setting::new("a".to_string(),
                                     Value::Array(vec![
                                         Value::Svalue(ScalarValue::Integer64(
                                             9000000000000000000i64)),
                                         Value::Svalue(ScalarValue::Integer64(
                                             8000000000000000002i64)),
                                         Value::Svalue(ScalarValue::Integer64(5))])));
        expected.insert("b".to_string(),
                        Setting::new("b".to_string(),
                                     Value::Array(vec![
                                         Value::Svalue(ScalarValue::Integer64(5)),
                                         Value::Svalue(ScalarValue::Integer64(6)),
                                         Value::Svalue(ScalarValue::Integer64(7))])));

        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn simple_flt32_array() {
        let parsed = parse_conf("a=[4.5, 0.5, 0.25]\n;\nb = [5.0e-1, 1.0e0];\n\n");
        assert!(parsed.is_ok());

        let mut expected = SettingsList::new();
        expected.insert("a".to_string(),
                        Setting::new("a".to_string(),
                                     Value::Array(vec![
                                         Value::Svalue(ScalarValue::Floating32(4.5)),
                                         Value::Svalue(ScalarValue::Floating32(0.5)),
                                         Value::Svalue(ScalarValue::Floating32(0.25))])));

        expected.insert("b".to_string(),
                        Setting::new("b".to_string(),
                                     Value::Array(vec![
                                         Value::Svalue(ScalarValue::Floating32(5.0e-1)),
                                         Value::Svalue(ScalarValue::Floating32(1.0))])));

        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn simple_flt64_array() {
        let parsed = parse_conf("a=[55937598585.5L,10000000000.25L];");
        assert!(parsed.is_ok());

        let mut expected = SettingsList::new();
        expected.insert("a".to_string(),
                        Setting::new("a".to_string(),
                                     Value::Array(vec![
                                         Value::Svalue(ScalarValue::Floating64(55937598585.5)),
                                         Value::Svalue(ScalarValue::Floating64(10000000000.25))])));

        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn str_arrays() {
        let parsed = parse_conf(
            concat!("my_strs = [                          ",
                    "\"testing.org\"                , ",
                    "\"Just a \\\"test\\\" with escapes.\",",
                    "\"He said: 'Hello!'\", ",
                    "\"\\\"\\\"\"\t\t, ",
                    "\"A backslash in quotes: \\\"\\\\\\\"\",",
                    "\"escaped_str=\\\"Just a \\\\\\\"test\\\\\\\" with escapes.\\\";\", ",
                    "\"\\n\\r\\t\\\"\"\n\n]\n;\n",
                    "my_simple_strs = [\"hello\", \"world\"];\n"));

        assert!(parsed.is_ok());

        let mut expected = SettingsList::new();
        expected.insert("my_strs".to_string(),
                        Setting::new("my_strs".to_string(),
                                     Value::Array(vec![
                                         Value::Svalue(ScalarValue::Str("testing.org".to_string())),
                                         Value::Svalue(ScalarValue::Str(
                                             "Just a \"test\" with escapes.".to_string())),
                                         Value::Svalue(ScalarValue::Str(
                                             "He said: 'Hello!'".to_string())),
                                         Value::Svalue(ScalarValue::Str("\"\"".to_string())),
                                         Value::Svalue(ScalarValue::Str(
                                             "A backslash in quotes: \"\\\"".to_string())),
                                             Value::Svalue(ScalarValue::Str(
                                                 concat!("escaped_str=\"Just a",
                                                         " \\\"test\\\" with escapes.\";")
                                                     .to_string())),
                                         Value::Svalue(ScalarValue::Str("\n\r\t\"".to_string()))])));
        expected.insert("my_simple_strs".to_string(),
                        Setting::new("my_simple_strs".to_string(),
                                     Value::Array(vec![
                                         Value::Svalue(ScalarValue::Str("hello".to_string())),
                                         Value::Svalue(ScalarValue::Str("world".to_string()))])));

        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn simple_bad_array() {
        let parsed = parse_conf("bad_array = [\"a bad array\", 12, 3.0e-1, true];\n");
        assert!(parsed.is_err());
    }

    #[test]
    fn bad_array() {
        let parsed = parse_conf("bad_array = [\"a bad array\", (\"array\", 5, 4, 2)];\n");
        assert!(parsed.is_err());
    }

    #[test]
    fn bad_array_not_scalar() {
        let parsed = parse_conf("bad_array = [(1, 2, 3), (4, 5, 6)];\n");
        assert!(parsed.is_err());
    }

    #[test]
    fn empty_list() {
        let parsed = parse_conf("list=();final=\n(\t  \n) \n;");
        assert!(parsed.is_ok());
        let mut expected = SettingsList::new();
        expected.insert("list".to_string(),
                        Setting::new("list".to_string(), Value::List(Vec::new())));
        expected.insert("final".to_string(),
                        Setting::new("final".to_string(), Value::List(Vec::new())));
        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn nested_empty_list() {
        let parsed = parse_conf("list=((()));\n");
        assert!(parsed.is_ok());

        let mut expected = SettingsList::new();
        expected.insert("list".to_string(),
                        Setting::new("list".to_string(),
                                     Value::List(vec![Value::List(vec![Value::List(Vec::new())])])));

        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn scalar_lists() {
        let parsed = parse_conf(concat!("my_list = (\n\"a \\\"string\\\" with \\nquo\\ttes\",\n",
                                   "15, 0.25e+2, 9000000000000000000L, 54, 55937598585.5L,\n",
                                   "yes\n,\ntrue\t,false,NO\n\n\n);\nanother_list=(10, \"0\");\n",
                                   "another_list\n=\n(\n   yes, 19, \"bye\"\n)\n;\n",
                                   "last_one:(true);\n"));

        assert!(parsed.is_ok());

        let mut expected = SettingsList::new();
        expected.insert("my_list".to_string(),
                        Setting::new("my_list".to_string(),
                                     Value::List(vec![
                                         Value::Svalue(
                                             ScalarValue::Str("a \"string\" with \nquo\ttes"
                                                              .to_string())),
                                         Value::Svalue(ScalarValue::Integer32(15)),
                                         Value::Svalue(ScalarValue::Floating32(0.25e+2)),
                                         Value::Svalue(
                                             ScalarValue::Integer64(9000000000000000000i64)),
                                         Value::Svalue(ScalarValue::Integer32(54)),
                                         Value::Svalue(ScalarValue::Floating64(55937598585.5f64)),
                                         Value::Svalue(ScalarValue::Boolean(true)),
                                         Value::Svalue(ScalarValue::Boolean(true)),
                                         Value::Svalue(ScalarValue::Boolean(false)),
                                         Value::Svalue(ScalarValue::Boolean(false))])));
        expected.insert("another_list".to_string(),
                        Setting::new("another_list".to_string(),
                                     Value::List(vec![
                                         Value::Svalue(ScalarValue::Boolean(true)),
                                         Value::Svalue(ScalarValue::Integer32(19)),
                                         Value::Svalue(ScalarValue::Str("bye".to_string()))])));
        expected.insert("last_one".to_string(),
                        Setting::new("last_one".to_string(),
                                     Value::List(vec![Value::Svalue(ScalarValue::Boolean(true))])));

        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn values_list() {
        let parsed = parse_conf(concat!("my_superb_list = (",
                                   "[yes, no], 21, [0.25, .5, .125],",
                                   "(()), ((\"a\")), (\"a\"), [\"\\\"x\\\"\"],",
                                   "(14, [\"x\"], (true, (false, (4), [5, 6]), \"y\")),",
                                   "\"goodbye!\\r\\n\", { s = [1, 2]; x = \"str\"; y = (); });\n"));
                             
        assert!(parsed.is_ok());


        let mut group_in_list = SettingsList::new();
        group_in_list.insert("s".to_string(),
                             Setting::new("s".to_string(),
                                          Value::Array(vec![
                                              Value::Svalue(ScalarValue::Integer32(1)),
                                              Value::Svalue(ScalarValue::Integer32(2))])));
        group_in_list.insert("x".to_string(),
                             Setting::new("x".to_string(),
                                          Value::Svalue(ScalarValue::Str("str".to_string()))));

        group_in_list.insert("y".to_string(),
                             Setting::new("y".to_string(), Value::List(Vec::new())));


        let list_elements = vec![
            Value::Array(vec![
                Value::Svalue(ScalarValue::Boolean(true)),
                Value::Svalue(ScalarValue::Boolean(false))]),
            Value::Svalue(ScalarValue::Integer32(21)),
            Value::Array(vec![
                Value::Svalue(ScalarValue::Floating32(0.25)),
                Value::Svalue(ScalarValue::Floating32(0.5)),
                Value::Svalue(ScalarValue::Floating32(0.125))]),
            Value::List(vec![Value::List(Vec::new())]),
            Value::List(vec![Value::List(vec![Value::Svalue(ScalarValue::Str("a".to_string()))])]),
            Value::List(vec![Value::Svalue(ScalarValue::Str("a".to_string()))]),
            Value::Array(vec![Value::Svalue(ScalarValue::Str("\"x\"".to_string()))]),
            Value::List(vec![Value::Svalue(ScalarValue::Integer32(14)),
                             Value::Array(vec![Value::Svalue(ScalarValue::Str("x".to_string()))]),
                             Value::List(vec![Value::Svalue(ScalarValue::Boolean(true)),
                                              Value::List(vec![
                                                  Value::Svalue(ScalarValue::Boolean(false)),
                                                  Value::List(vec![
                                                      Value::Svalue(ScalarValue::Integer32(4))]),
                                                  Value::Array(vec![
                                                      Value::Svalue(ScalarValue::Integer32(5)),
                                                      Value::Svalue(ScalarValue::Integer32(6))])]),
                                              Value::Svalue(ScalarValue::Str("y".to_string()))])]),
            Value::Svalue(ScalarValue::Str("goodbye!\r\n".to_string())),
            Value::Group(group_in_list)];

        let mut expected = SettingsList::new();
        expected.insert("my_superb_list".to_string(),
                        Setting::new("my_superb_list".to_string(), Value::List(list_elements)));

        assert_eq!(parsed.unwrap(), expected);                                     
                                                                   
    }

    #[test]
    fn sample_conf_small() {
        let parsed = parse_conf(concat!(
            "\n\napplication:\n",
            "{\n",
            "  window:\n",
            "  {\n",
            "    title = \"My Application\";\n",
            "    size = { w = 640; h = 480; };\n",
            "  };\n",
            "  a = 5;\n",
            "  ff = 1.E6;\n",
            "  group1:\n",
            "  {\n",
            "    x = 5;  y = 10;\n",
            "    my_array = [ 10, 11, 12 ];\n",
            "    flag = TRUE;\n",
            "    states = [\"CT\", \"CA\", \"TX\", \"NV\", \"FL\"];",
            "  };\n",
            "};\n"));

        assert!(parsed.is_ok());

        let mut size_group = SettingsList::new();
        size_group.insert("w".to_string(),
                          Setting::new("w".to_string(),
                                       Value::Svalue(ScalarValue::Integer32(640))));
        size_group.insert("h".to_string(),
                          Setting::new("h".to_string(),
                                       Value::Svalue(ScalarValue::Integer32(480))));

        let mut window_group = SettingsList::new();
        window_group.insert("title".to_string(),
                            Setting::new("title".to_string(),
                                         Value::Svalue(ScalarValue::Str("My Application"
                                                                        .to_string()))));
        window_group.insert("size".to_string(),
                            Setting::new("size".to_string(), Value::Group(size_group)));

        let mut group1 = SettingsList::new();
        group1.insert("x".to_string(),
                      Setting::new("x".to_string(), Value::Svalue(ScalarValue::Integer32(5))));
        group1.insert("y".to_string(),
                      Setting::new("y".to_string(), Value::Svalue(ScalarValue::Integer32(10))));
        group1.insert("my_array".to_string(),
                      Setting::new("my_array".to_string(),
                                   Value::Array(vec![
                                       Value::Svalue(ScalarValue::Integer32(10)),
                                       Value::Svalue(ScalarValue::Integer32(11)),
                                       Value::Svalue(ScalarValue::Integer32(12))])));
        group1.insert("flag".to_string(),
                      Setting::new("flag".to_string(), Value::Svalue(ScalarValue::Boolean(true))));
        group1.insert("states".to_string(),
                      Setting::new("states".to_string(),
                                   Value::Array(vec![
                                       Value::Svalue(ScalarValue::Str("CT".to_string())),
                                       Value::Svalue(ScalarValue::Str("CA".to_string())),
                                       Value::Svalue(ScalarValue::Str("TX".to_string())),
                                       Value::Svalue(ScalarValue::Str("NV".to_string())),
                                       Value::Svalue(ScalarValue::Str("FL".to_string()))])));

        let mut app_group = SettingsList::new();
        app_group.insert("window".to_string(),
                         Setting::new("window".to_string(), Value::Group(window_group)));
        app_group.insert("a".to_string(),
                         Setting::new("a".to_string(), Value::Svalue(ScalarValue::Integer32(5))));
        app_group.insert("ff".to_string(),
                         Setting::new("ff".to_string(),
                                      Value::Svalue(ScalarValue::Floating32(1e6))));
        app_group.insert("group1".to_string(),
                         Setting::new("group1".to_string(), Value::Group(group1)));

        let mut expected = SettingsList::new();
        expected.insert("application".to_string(),
                        Setting::new("application".to_string(), Value::Group(app_group)));

        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn sample_conf_comments() {
        let parsed = parse_conf(concat!(
            "\n\napplication:\n",
            "{//This is a comment. It spans until the end of the line\n",
            "  window:\n",
            "  {\n",
            "    title =/*the app title \r\n\r\n\r\n*/\"My Application\"//Another comment;\n;\n",
            "    size = { w = 640; h// = 480; };\n = 480; };\n",
            "  }; //This was for the window. Now the rest.\n",
            "  a = 5;\n",
            "  ff = 1.E6;\n",
            "  # This is going to be called `group1`\n",
            "  group1:\n",
            "  {\n",
            "    x = 5;  y = 10;\n",
            "    my_array = [ /*comments*/10, /*everywhere*/11, 12 ];\n",
            "    flag = TRUE;\n",
            "    //DING DONG!\nstates = [\"CT\", \"CA\", \"TX\", \"NV\", \"FL\"];",
            "  };\n",
            "};\n"));

        assert!(parsed.is_ok());

        let mut size_group = SettingsList::new();
        size_group.insert("w".to_string(),
                          Setting::new("w".to_string(),
                                       Value::Svalue(ScalarValue::Integer32(640))));
        size_group.insert("h".to_string(),
                          Setting::new("h".to_string(),
                                       Value::Svalue(ScalarValue::Integer32(480))));

        let mut window_group = SettingsList::new();
        window_group.insert("title".to_string(),
                            Setting::new("title".to_string(),
                                         Value::Svalue(ScalarValue::Str("My Application"
                                                                        .to_string()))));
        window_group.insert("size".to_string(),
                            Setting::new("size".to_string(), Value::Group(size_group)));

        let mut group1 = SettingsList::new();
        group1.insert("x".to_string(),
                      Setting::new("x".to_string(), Value::Svalue(ScalarValue::Integer32(5))));
        group1.insert("y".to_string(),
                      Setting::new("y".to_string(), Value::Svalue(ScalarValue::Integer32(10))));
        group1.insert("my_array".to_string(),
                      Setting::new("my_array".to_string(),
                                   Value::Array(vec![
                                       Value::Svalue(ScalarValue::Integer32(10)),
                                       Value::Svalue(ScalarValue::Integer32(11)),
                                       Value::Svalue(ScalarValue::Integer32(12))])));
        group1.insert("flag".to_string(),
                      Setting::new("flag".to_string(), Value::Svalue(ScalarValue::Boolean(true))));
        group1.insert("states".to_string(),
                      Setting::new("states".to_string(),
                                   Value::Array(vec![
                                       Value::Svalue(ScalarValue::Str("CT".to_string())),
                                       Value::Svalue(ScalarValue::Str("CA".to_string())),
                                       Value::Svalue(ScalarValue::Str("TX".to_string())),
                                       Value::Svalue(ScalarValue::Str("NV".to_string())),
                                       Value::Svalue(ScalarValue::Str("FL".to_string()))])));

        let mut app_group = SettingsList::new();
        app_group.insert("window".to_string(),
                         Setting::new("window".to_string(), Value::Group(window_group)));
        app_group.insert("a".to_string(),
                         Setting::new("a".to_string(), Value::Svalue(ScalarValue::Integer32(5))));
        app_group.insert("ff".to_string(),
                         Setting::new("ff".to_string(),
                                      Value::Svalue(ScalarValue::Floating32(1e6))));
        app_group.insert("group1".to_string(),
                         Setting::new("group1".to_string(), Value::Group(group1)));

        let mut expected = SettingsList::new();
        expected.insert("application".to_string(),
                        Setting::new("application".to_string(), Value::Group(app_group)));

        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn sample_conf_all_features() {
        let my_conf = concat!(
            "#----------------------------\n",
            "# Example Configuration File\n",
            "#---------------------------\n",
            "#\n",
            "\n",
            "application:\n",
            "{\n",
            "\n",
            " /* This section defines some settings for our\n",
            "  * main application window, such as size and\n",
            "  * position.\n",
            "  */\n",
            "\n",
            "  window:\n",
            "  {\n",
            "    title = \"My Application\";\n",
            "    size = { /* width */ w = 640; /* height */ h = 480; };\n",
            "    pos = { x = 350; y = 250; };\n",
            "  };\n",
            "\n",
            "  a = 5;\n",
            "  b = 6;\n",
            "  ff = 1.0E6;\n",
            "  test-comment = \"/* hello\\n \\\"there\\\"*/\";\n",
            "\n",
            "  test-long-string = \"A very long string that spans multiple lines. \"\n",
            "  /* but wait, there's more... */ \"Adjacent strings are automatically\"\n",
            "  \" concatenated.\";\n",
            "\n",
            "  test-escaped-string = \"\\\"This is\\n a test.\\\"\";\n",
            "\n",
            "  group1:\n",
            "  {\n",
            "    x = 5;  y = 10;\n",
            "    my_array = [ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22 ];\n",
            "    flag = TRUE;\n",
            "\n",
            "    group2: { zzz = \"this is a test\"; };\n",
            "\n",
            "    states = [	\"CT\", // Connecticut\n",
            "		\"CA\", // California\n",
            "		\"TX\", // Texas\n",
            "		\"NV\", // Nevada\n",
            "		\"FL\"  // Florida\n",
            "    ];\n",
            "  };\n",
            "\n",
            "};\n",
            "\n",
            "// Commented for now\n",
            "//binary = [ 0xAA, 0xBB, 0xCC ];\n",
            "\n",
            "list = ( ( \"abc\", 123, true ), 1.234, ( /* an empty list */ ) ,[ 1, 2, 3 ],\n",
            "	   { a = (1, 2, true); } );\n",
            "\n",
            "books = ( \"inventory\",\n",
            "          { title  = \"Treasure Island\";\n",
            "            author = \"Robert Louis Stevenson\";\n",
            "            price  = 29.99;\n",
            "            qty    = 5; },\n",
            "          { title  = \"Snow Crash\";\n",
            "            author = \"Neal Stephenson\";\n",
            "            price  = 9.99;\n",
            "            qty    = 8; });\n",
            "\n",
            "# miscellaneous stuff\n",
            "\n",
            "misc:\n",
            "{\n",
            "  port = 5000;\n",
            "  minutes = 3.0e0;\n",
            "  enabled = FALSE;\n",
            "  // Commented for now",
            "  // mask = 0xAABBCCDD;\n",
            "  unicode = \"STARGÎ›ÌŠTE SG-1\"; // UTF-8 string\n",
            "  bigint = 9223372036854775807L;\n",
            "  // Commented for now\n",
            "  // bighex = 0x1122334455667788L;\n",
            "};\n",
            "\n",
            "\n",
            "### eof");


        let parsed = parse_conf(my_conf);
        assert!(parsed.is_ok());

        let mut size_group = SettingsList::new();
        size_group.insert("w".to_string(),
                          Setting::new("w".to_string(),
                                       Value::Svalue(ScalarValue::Integer32(640))));
        size_group.insert("h".to_string(),
                          Setting::new("h".to_string(),
                                       Value::Svalue(ScalarValue::Integer32(480))));

        let mut pos_group = SettingsList::new();
        pos_group.insert("x".to_string(),
                         Setting::new("x".to_string(),
                                      Value::Svalue(ScalarValue::Integer32(350))));
        pos_group.insert("y".to_string(),
                         Setting::new("y".to_string(),
                                      Value::Svalue(ScalarValue::Integer32(250))));

        let mut window_group = SettingsList::new();
        window_group.insert("title".to_string(),
                            Setting::new("title".to_string(),
                                         Value::Svalue(ScalarValue::Str("My Application"
                                                                        .to_string()))));
        window_group.insert("size".to_string(),
                            Setting::new("size".to_string(), Value::Group(size_group)));
        window_group.insert("pos".to_string(),
                            Setting::new("pos".to_string(), Value::Group(pos_group)));

        let mut group2 = SettingsList::new();
        group2.insert("zzz".to_string(),
                      Setting::new("zzz".to_string(),
                                   Value::Svalue(ScalarValue::Str("this is a test".to_string()))));

        let mut group1 = SettingsList::new();
        group1.insert("x".to_string(),
                      Setting::new("x".to_string(), Value::Svalue(ScalarValue::Integer32(5))));
        group1.insert("y".to_string(),
                      Setting::new("y".to_string(), Value::Svalue(ScalarValue::Integer32(10))));
        group1.insert("my_array".to_string(),
                      Setting::new("my_array".to_string(),
                                   Value::Array(vec![
                                       Value::Svalue(ScalarValue::Integer32(10)),
                                       Value::Svalue(ScalarValue::Integer32(11)),
                                       Value::Svalue(ScalarValue::Integer32(12)),
                                       Value::Svalue(ScalarValue::Integer32(13)),
                                       Value::Svalue(ScalarValue::Integer32(14)),
                                       Value::Svalue(ScalarValue::Integer32(15)),
                                       Value::Svalue(ScalarValue::Integer32(16)),
                                       Value::Svalue(ScalarValue::Integer32(17)),
                                       Value::Svalue(ScalarValue::Integer32(18)),
                                       Value::Svalue(ScalarValue::Integer32(19)),
                                       Value::Svalue(ScalarValue::Integer32(20)),
                                       Value::Svalue(ScalarValue::Integer32(21)),
                                       Value::Svalue(ScalarValue::Integer32(22))])));
        group1.insert("flag".to_string(),
                      Setting::new("flag".to_string(), Value::Svalue(ScalarValue::Boolean(true))));
        group1.insert("group2".to_string(),
                      Setting::new("group2".to_string(), Value::Group(group2)));
        group1.insert("states".to_string(),
                      Setting::new("states".to_string(),
                                   Value::Array(vec![
                                       Value::Svalue(ScalarValue::Str("CT".to_string())),
                                       Value::Svalue(ScalarValue::Str("CA".to_string())),
                                       Value::Svalue(ScalarValue::Str("TX".to_string())),
                                       Value::Svalue(ScalarValue::Str("NV".to_string())),
                                       Value::Svalue(ScalarValue::Str("FL".to_string()))])));

        let mut app_group = SettingsList::new();
        app_group.insert("window".to_string(),
                         Setting::new("window".to_string(), Value::Group(window_group)));
        app_group.insert("a".to_string(),
                         Setting::new("a".to_string(), Value::Svalue(ScalarValue::Integer32(5))));
        app_group.insert("b".to_string(),
                         Setting::new("b".to_string(), Value::Svalue(ScalarValue::Integer32(6))));
        app_group.insert("ff".to_string(),
                         Setting::new("ff".to_string(),
                                      Value::Svalue(ScalarValue::Floating32(1e6))));
        app_group.insert("test-comment".to_string(),
                         Setting::new("test-comment".to_string(),
                                      Value::Svalue(ScalarValue::Str("/* hello\n \"there\"*/"
                                                                     .to_string()))));
        app_group.insert("test-long-string".to_string(),
                         Setting::new("test-long-string".to_string(),
                                      Value::Svalue(ScalarValue::Str(
                                          concat!("A very long string that spans multiple lines. ",
                                                  "Adjacent strings are automatically",
                                                  " concatenated.").to_string()))));
        app_group.insert("test-escaped-string".to_string(),
                         Setting::new("test-escaped-string".to_string(),
                                      Value::Svalue(ScalarValue::Str("\"This is\n a test.\""
                                                                     .to_string()))));
        app_group.insert("group1".to_string(),
                         Setting::new("group1".to_string(), Value::Group(group1)));

        let mut expected = SettingsList::new();
        expected.insert("application".to_string(),
                        Setting::new("application".to_string(), Value::Group(app_group)));

        let mut group_a = SettingsList::new();
        group_a.insert("a".to_string(),
                       Setting::new("a".to_string(),
                                    Value::List(vec![
                                        Value::Svalue(ScalarValue::Integer32(1)),
                                        Value::Svalue(ScalarValue::Integer32(2)),
                                        Value::Svalue(ScalarValue::Boolean(true))])));

        let list_elements = vec![
            Value::List(vec![
                Value::Svalue(ScalarValue::Str("abc".to_string())),
                Value::Svalue(ScalarValue::Integer32(123)),
                Value::Svalue(ScalarValue::Boolean(true))]),
            Value::Svalue(ScalarValue::Floating32(1.234)),
            Value::List(Vec::new()),
            Value::Array(vec![
                Value::Svalue(ScalarValue::Integer32(1)),
                Value::Svalue(ScalarValue::Integer32(2)),
                Value::Svalue(ScalarValue::Integer32(3))]),
            Value::Group(group_a)];

        expected.insert("list".to_string(),
                        Setting::new("list".to_string(),
                                     Value::List(list_elements)));

        let mut inventory_group_1 = SettingsList::new();
        inventory_group_1.insert("title".to_string(),
                                 Setting::new("title".to_string(),
                                              Value::Svalue(ScalarValue::Str(
                                                  "Treasure Island".to_string()))));
        inventory_group_1.insert("author".to_string(),
                                 Setting::new("author".to_string(),
                                              Value::Svalue(ScalarValue::Str(
                                                  "Robert Louis Stevenson".to_string()))));
        inventory_group_1.insert("price".to_string(),
                                 Setting::new("price".to_string(),
                                              Value::Svalue(ScalarValue::Floating32(29.99))));
        inventory_group_1.insert("qty".to_string(),
                                 Setting::new("qty".to_string(),
                                              Value::Svalue(ScalarValue::Integer32(5))));

        let mut inventory_group_2 = SettingsList::new();
        inventory_group_2.insert("title".to_string(),
                                 Setting::new("title".to_string(),
                                              Value::Svalue(ScalarValue::Str(
                                                  "Snow Crash".to_string()))));
        inventory_group_2.insert("author".to_string(),
                                 Setting::new("author".to_string(),
                                              Value::Svalue(ScalarValue::Str(
                                                  "Neal Stephenson".to_string()))));
        inventory_group_2.insert("price".to_string(),
                                 Setting::new("price".to_string(),
                                              Value::Svalue(ScalarValue::Floating32(9.99))));
        inventory_group_2.insert("qty".to_string(),
                                 Setting::new("qty".to_string(),
                                              Value::Svalue(ScalarValue::Integer32(8))));


        let books_list_elements = vec![
            Value::Svalue(ScalarValue::Str("inventory".to_string())),
            Value::Group(inventory_group_1),
            Value::Group(inventory_group_2)];

        expected.insert("books".to_string(),
                        Setting::new("books".to_string(),
                                     Value::List(books_list_elements)));

        let mut misc_group = SettingsList::new();
        misc_group.insert("port".to_string(),
                          Setting::new("port".to_string(),
                                       Value::Svalue(ScalarValue::Integer32(5000))));
        misc_group.insert("minutes".to_string(),
                          Setting::new("minutes".to_string(),
                                       Value::Svalue(ScalarValue::Floating32(3.0))));
        misc_group.insert("enabled".to_string(),
                          Setting::new("enabled".to_string(),
                                       Value::Svalue(ScalarValue::Boolean(false))));
        misc_group.insert("unicode".to_string(),
                          Setting::new("unicode".to_string(),
                                       Value::Svalue(ScalarValue::Str(
                                           "STARGÎ›ÌŠTE SG-1".to_string()))));
        misc_group.insert("bigint".to_string(),
                          Setting::new("bigint".to_string(),
                                       Value::Svalue(ScalarValue::Integer64(
                                           9223372036854775807i64))));

        expected.insert("misc".to_string(),
                        Setting::new("misc".to_string(),
                                     Value::Group(misc_group)));

        assert_eq!(parsed.unwrap(), expected);
    }
}
