//! The core parser module.
//! Upon successfully parsing a configuration, a `SettingsList` is created. Conceptually, a
//! `SettingsList` is a map associating settings names' to a `Value`.
//!
//! This map is the basis for the rest of the library. The public library API is nothing more
//! than a simple set of wrappers to make it easier to manage a `SettingsList`.
//!
//! When a parse call is invoked, a settings list is built as input is read. It is not expected that
//! library users manipulate or otherwise deal directly with these internal data structures,
//! although, of course, they are free to do so.
//!
//! Most of the setting types allowed in a configuration will pretty much map to either a Rust
//! primitive type or a container.
//!
//! # Examples
//! This example shows how to create a settings list and store a `Boolean` scalar value named
//! *my_setting* with the boolean value `true`.
//!
//! The first step is to create a new, empty settings list:
//!
//! ```
//! use config::parser::SettingsList;
//! # use config::parser::ScalarValue;
//! # use config::parser::Value;
//! # use config::parser::Setting;
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
//! # use config::parser::SettingsList;
//! # use config::parser::ScalarValue;
//! # use config::parser::Value;
//! # use config::parser::Setting;
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
//! # use config::parser::SettingsList;
//! use config::parser::ScalarValue;
//! # use config::parser::Value;
//! # use config::parser::Setting;
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
//! # use config::parser::SettingsList;
//! # use config::parser::ScalarValue;
//! use config::parser::Value;
//! # use config::parser::Setting;
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
//! # use config::parser::SettingsList;
//! # use config::parser::ScalarValue;
//! # use config::parser::Value;
//! use config::parser::Setting;
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
//! # use config::parser::SettingsList;
//! # use config::parser::ScalarValue;
//! # use config::parser::Value;
//! # use config::parser::Setting;
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
//! use config::parser::SettingsList;
//! use config::parser::ScalarValue;
//! use config::parser::Value;
//! use config::parser::Setting;
//!
//! let mut my_settings_list = SettingsList::new();
//! let setting_name = "my_setting".to_string();
//! let a_scalar = ScalarValue::Boolean(true);
//! let setting_value = Value::Svalue(a_scalar);
//! let my_setting = Setting::new(setting_name.clone(), setting_value);
//! my_settings_list.insert(setting_name, my_setting);
//! ```
//!

use std::collections::HashMap;
pub use parser::grammar::ParseError;
use parser::grammar::conf;

/// The top-level `Config` type that represents a configuration
#[derive(PartialEq)]
#[derive(Debug)]
#[unstable = "Library still under heavy development; design may change."]
pub struct Config {
    settings: SettingsList
}

/// Settings list representation. Associates settings to their names.
#[unstable = "Library still under heavy development; design may change."]
pub type SettingsList = HashMap<String, Setting>;

/// A `Setting` representation. Settings have a name and a value.
#[derive(PartialEq)]
#[derive(Debug)]
#[unstable = "Library still under heavy development; design may change."]
pub struct Setting {
    /// Setting name, as read from the configuration file
    pub name: String,
    /// This setting's value. A value can be a scalar, an array, a list, or a group.
    pub value: Value
}

/// A type representing a generic value. `Setting`s store `Value`s.
#[derive(PartialEq)]
#[derive(Debug)]
#[unstable = "Library still under heavy development; design may change."]
pub enum Value {
    /// A scalar
    Svalue(ScalarValue),
    /// An array
    Array(ArrayValue),
    /// A list. Arrays can only store scalars of the same type, whereas lists store `Value`s of
    /// possibly different types, including other lists.
    List(ListValue),
    /// A group. Basically, a group acts as another configuration file - it stores a `SettingsList`.
    Group(SettingsList)
}

/// The scalar values representation. Scalar values bind directly to Rust primitive types.
#[derive(PartialEq)]
#[derive(Debug)]
#[unstable = "Library still under heavy development; design may change."]
pub enum ScalarValue {
    /// A boolean scalar
    Boolean(bool),
    /// An i32 scalar
    Integer32(i32),
    /// An i64 scalar
    Integer64(i64),
    /// An f32 scalar
    Floating32(f32),
    /// An f64 scalar
    Floating64(f64),
    /// A string scalar
    Str(String)
}

/// The type used to represent the scalars inside an array.
/// An array can only store scalar values of the same type.
#[unstable = "Library still under heavy development; design may change."]
pub type ArrayValue = Vec<ScalarValue>;

/// The type used to represent the generic values inside a list.
/// Lists are heterogeneous and can store any type of value, including other lists.
#[unstable = "Library still under heavy development; design may change."]
pub type ListValue = Vec<Value>;

impl Config {
    /// Creates a new wrapper `Config` to hold a settings list
    #[unstable = "Library still under heavy development; design may change."]
    pub fn new(sl: SettingsList) -> Config {
        Config { settings: sl }
    }
}

impl Setting {
    /// Creates a new setting with a given name and value
    /// # Examples 
    /// Let's say we want to create a setting to store an `i32`.
    /// We start off by creating a `ScalarValue`:
    ///
    /// ```
    /// use config::parser::ScalarValue;
    /// # use config::parser::Value;
    /// # use config::parser::Setting;
    ///
    /// let setting_scalarvalue = ScalarValue::Integer32(1);
    /// # let setting_value = Value::Svalue(setting_scalarvalue);
    /// # let setting_name = "my_setting".to_string();
    /// # let my_setting = Setting::new(setting_name, setting_value);
    /// ```
    ///
    /// Then, we wrap it into a `Value`, because settings store generic values:
    ///
    /// ```
    /// # use config::parser::ScalarValue;
    /// use config::parser::Value;
    /// # use config::parser::Setting;
    ///
    /// # let setting_scalarvalue = ScalarValue::Integer32(1);
    /// let setting_value = Value::Svalue(setting_scalarvalue);
    /// # let setting_name = "my_setting".to_string();
    /// # let my_setting = Setting::new(setting_name, setting_value);
    /// ```
    ///
    /// And then we choose a name for our setting and create it:
    ///
    /// ```
    /// # use config::parser::ScalarValue;
    /// # use config::parser::Value;
    /// use config::parser::Setting;
    ///
    /// # let setting_scalarvalue = ScalarValue::Integer32(1);
    /// # let setting_value = Value::Svalue(setting_scalarvalue);
    /// let setting_name = "my_setting".to_string();
    /// let my_setting = Setting::new(setting_name, setting_value);
    /// ```
    ///
    /// Here's the complete example:
    ///
    /// ```
    /// use config::parser::ScalarValue;
    /// use config::parser::Value;
    /// use config::parser::Setting;
    ///
    /// let setting_scalarvalue = ScalarValue::Integer32(1);
    /// let setting_value = Value::Svalue(setting_scalarvalue);
    /// let setting_name = "my_setting".to_string();
    /// let my_setting = Setting::new(setting_name, setting_value);
    /// ```
    ///
    #[unstable = "Library still under heavy development; design may change."]
    pub fn new(sname: String, val: Value) -> Setting {
        Setting { name: sname, value: val }
    }
}

peg_file! grammar("grammar.rustpeg");

/// Parses a configuration file from a `&str`.
pub fn parse(config: &str) -> Result<Config, ParseError> {
    conf(config).and_then(|sl| Ok(Config::new(sl)))
}

#[cfg(test)]
mod test {
    use super::parse;
    use super::{Value, ScalarValue};
    use super::{SettingsList, Setting};
    use super::Config;

    #[test]
    fn empty_conf() {
        let parsed_conf = parse("");
        assert!(parsed_conf.is_ok());
        let my_conf = parsed_conf.unwrap();
        assert_eq!(my_conf.settings.len(), 0);
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

        assert_eq!(parsed.unwrap(), Config::new(expected));
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

        assert_eq!(parsed.unwrap(), Config::new(expected));
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
                                     Value::Svalue(ScalarValue::Integer64(
                                         8000000000000000001i64))));

        assert_eq!(parsed.unwrap(), Config::new(expected));
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
          
      assert_eq!(parsed.unwrap(), Config::new(expected));
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

        assert_eq!(parsed.unwrap(), Config::new(expected));
    }

    #[test]
    fn simple_str_scalar_value() {
        let parsed = parse("\n\nserver_name\t= \"testing.org\"\r\n\r\n;");
        assert!(parsed.is_ok());

        let mut expected = SettingsList::new();
        expected.insert("server_name".to_string(),
                        Setting::new("server_name".to_string(),
                                     Value::Svalue(ScalarValue::Str("testing.org".to_string()))));

        assert_eq!(parsed.unwrap(), Config::new(expected));
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

        assert_eq!(parsed.unwrap(), Config::new(expected));
    }

    #[test]
    fn empty_array() {
        let parsed = parse("array_one = [\n\n\n\n\n];\r\narray_two=[];");

        assert!(parsed.is_ok());
        let mut expected = SettingsList::new();
        expected.insert("array_one".to_string(),
                        Setting::new("array_one".to_string(), Value::Array(Vec::new())));
        expected.insert("array_two".to_string(),
                        Setting::new("array_two".to_string(), Value::Array(Vec::new())));

        assert_eq!(parsed.unwrap(), Config::new(expected));
    }

    #[test]
    fn simple_boolean_array() {
        let parsed = parse("my_array = [true, true, YEs, No, FaLSE, false, true];");

        assert!(parsed.is_ok());

        let mut expected = SettingsList::new();
        expected.insert("my_array".to_string(),
                        Setting::new("my_array".to_string(),
                                     Value::Array(vec![
                                         ScalarValue::Boolean(true),
                                         ScalarValue::Boolean(true),
                                         ScalarValue::Boolean(true),
                                         ScalarValue::Boolean(false),
                                         ScalarValue::Boolean(false),
                                         ScalarValue::Boolean(false),
                                         ScalarValue::Boolean(true)])));

        assert_eq!(parsed.unwrap(), Config::new(expected));
    }

    #[test]
    fn simple_integer32_array() {
        let parsed = parse("my_array: [10, 11, 12];\narray = [1];\n");
        assert!(parsed.is_ok());

        let mut expected = SettingsList::new();
        expected.insert("my_array".to_string(),
                        Setting::new("my_array".to_string(),
                                     Value::Array(vec![
                                         ScalarValue::Integer32(10),
                                         ScalarValue::Integer32(11),
                                         ScalarValue::Integer32(12)])));
        expected.insert("array".to_string(),
                        Setting::new("array".to_string(),
                                     Value::Array(vec![ScalarValue::Integer32(1)])));

        assert_eq!(parsed.unwrap(), Config::new(expected));
    }

    #[test]
    fn simple_integer64_array() {
        let parsed = parse("a=[9000000000000000000L,8000000000000000002L,5L];\nb=[5L,6L,7L];");

        assert!(parsed.is_ok());
        let mut expected = SettingsList::new();
        expected.insert("a".to_string(),
                        Setting::new("a".to_string(),
                                     Value::Array(vec![
                                         ScalarValue::Integer64(9000000000000000000i64),
                                         ScalarValue::Integer64(8000000000000000002i64),
                                         ScalarValue::Integer64(5)])));
        expected.insert("b".to_string(),
                        Setting::new("b".to_string(),
                                     Value::Array(vec![
                                         ScalarValue::Integer64(5),
                                         ScalarValue::Integer64(6),
                                         ScalarValue::Integer64(7)])));

        assert_eq!(parsed.unwrap(), Config::new(expected));
    }

    #[test]
    fn simple_flt32_array() {
        let parsed = parse("a=[4.5, 0.5, 0.25]\n;\nb = [5.0e-1, 1.0e0];\n\n");
        assert!(parsed.is_ok());

        let mut expected = SettingsList::new();
        expected.insert("a".to_string(),
                        Setting::new("a".to_string(),
                                     Value::Array(vec![
                                         ScalarValue::Floating32(4.5),
                                         ScalarValue::Floating32(0.5),
                                         ScalarValue::Floating32(0.25)])));

        expected.insert("b".to_string(),
                        Setting::new("b".to_string(),
                                     Value::Array(vec![
                                         ScalarValue::Floating32(5.0e-1),
                                         ScalarValue::Floating32(1.0)])));

        assert_eq!(parsed.unwrap(), Config::new(expected));
    }

    #[test]
    fn simple_flt64_array() {
        let parsed = parse("a=[55937598585.5L,10000000000.25L];");
        assert!(parsed.is_ok());

        let mut expected = SettingsList::new();
        expected.insert("a".to_string(),
                        Setting::new("a".to_string(),
                                     Value::Array(vec![
                                         ScalarValue::Floating64(55937598585.5),
                                         ScalarValue::Floating64(10000000000.25)])));

        assert_eq!(parsed.unwrap(), Config::new(expected));
    }

    #[test]
    fn str_arrays() {
        let parsed = parse(
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
                                         ScalarValue::Str("testing.org".to_string()),
                                         ScalarValue::Str("Just a \"test\" with escapes."
                                                          .to_string()),
                                         ScalarValue::Str("He said: 'Hello!'".to_string()),
                                         ScalarValue::Str("\"\"".to_string()),
                                         ScalarValue::Str("A backslash in quotes: \"\\\""
                                                          .to_string()),
                                         ScalarValue::Str(concat!("escaped_str=\"Just a",
                                                                  " \\\"test\\\" with escapes.\";")
                                                          .to_string()),
                                         ScalarValue::Str("\n\r\t\"".to_string())])));
        expected.insert("my_simple_strs".to_string(),
                        Setting::new("my_simple_strs".to_string(),
                                     Value::Array(vec![
                                         ScalarValue::Str("hello".to_string()),
                                         ScalarValue::Str("world".to_string())])));

        assert_eq!(parsed.unwrap(), Config::new(expected));
    }

    #[test]
    fn empty_list() {
        let parsed = parse("list=();final=\n(\t  \n) \n;");
        assert!(parsed.is_ok());
        let mut expected = SettingsList::new();
        expected.insert("list".to_string(),
                        Setting::new("list".to_string(), Value::List(Vec::new())));
        expected.insert("final".to_string(),
                        Setting::new("final".to_string(), Value::List(Vec::new())));
        assert_eq!(parsed.unwrap(), Config::new(expected));
    }

    #[test]
    fn nested_empty_list() {
        let parsed = parse("list=((()));\n");
        assert!(parsed.is_ok());

        let mut expected = SettingsList::new();
        expected.insert("list".to_string(),
                        Setting::new("list".to_string(),
                                     Value::List(vec![Value::List(vec![Value::List(Vec::new())])])));

        assert_eq!(parsed.unwrap(), Config::new(expected));
    }

    #[test]
    fn scalar_lists() {
        let parsed = parse(concat!("my_list = (\n\"a \\\"string\\\" with \\nquo\\ttes\",\n",
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

        assert_eq!(parsed.unwrap(), Config::new(expected));
    }

    #[test]
    fn values_list() {
        let parsed = parse(concat!("my_superb_list = (",
                                   "[yes, no], 21, [0.25, .5, .125],",
                                   "(()), ((\"a\")), (\"a\"), [\"\\\"x\\\"\"],",
                                   "(14, [\"x\"], (true, (false, (4), [5, 6]), \"y\")),",
                                   "\"goodbye!\\r\\n\", { s = [1, 2]; x = \"str\"; y = (); });\n"));
                             
        assert!(parsed.is_ok());


        let mut group_in_list = SettingsList::new();
        group_in_list.insert("s".to_string(),
                             Setting::new("s".to_string(),
                                          Value::Array(vec![
                                              ScalarValue::Integer32(1),
                                              ScalarValue::Integer32(2)])));
        group_in_list.insert("x".to_string(),
                             Setting::new("x".to_string(),
                                          Value::Svalue(ScalarValue::Str("str".to_string()))));

        group_in_list.insert("y".to_string(),
                             Setting::new("y".to_string(), Value::List(Vec::new())));


        let list_elements = vec![
            Value::Array(vec![ScalarValue::Boolean(true), ScalarValue::Boolean(false)]),
            Value::Svalue(ScalarValue::Integer32(21)),
            Value::Array(vec![ScalarValue::Floating32(0.25), ScalarValue::Floating32(0.5),
                              ScalarValue::Floating32(0.125)]),
            Value::List(vec![Value::List(Vec::new())]),
            Value::List(vec![Value::List(vec![Value::Svalue(ScalarValue::Str("a".to_string()))])]),
            Value::List(vec![Value::Svalue(ScalarValue::Str("a".to_string()))]),
            Value::Array(vec![ScalarValue::Str("\"x\"".to_string())]),
            Value::List(vec![Value::Svalue(ScalarValue::Integer32(14)),
                             Value::Array(vec![ScalarValue::Str("x".to_string())]),
                             Value::List(vec![Value::Svalue(ScalarValue::Boolean(true)),
                                              Value::List(vec![
                                                  Value::Svalue(ScalarValue::Boolean(false)),
                                                  Value::List(vec![
                                                      Value::Svalue(ScalarValue::Integer32(4))]),
                                                  Value::Array(vec![
                                                      ScalarValue::Integer32(5),
                                                      ScalarValue::Integer32(6)])]),
                                              Value::Svalue(ScalarValue::Str("y".to_string()))])]),
            Value::Svalue(ScalarValue::Str("goodbye!\r\n".to_string())),
            Value::Group(group_in_list)];

        let mut expected = SettingsList::new();
        expected.insert("my_superb_list".to_string(),
                        Setting::new("my_superb_list".to_string(), Value::List(list_elements)));

        assert_eq!(parsed.unwrap(), Config::new(expected));                                     
                                                                   
    }

    #[test]
    fn sample_conf_small() {
        let parsed = parse(concat!(
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
                                       ScalarValue::Integer32(10),
                                       ScalarValue::Integer32(11),
                                       ScalarValue::Integer32(12)])));
        group1.insert("flag".to_string(),
                      Setting::new("flag".to_string(), Value::Svalue(ScalarValue::Boolean(true))));
        group1.insert("states".to_string(),
                      Setting::new("states".to_string(),
                                   Value::Array(vec![
                                       ScalarValue::Str("CT".to_string()),
                                       ScalarValue::Str("CA".to_string()),
                                       ScalarValue::Str("TX".to_string()),
                                       ScalarValue::Str("NV".to_string()),
                                       ScalarValue::Str("FL".to_string())])));

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

        assert_eq!(parsed.unwrap(), Config::new(expected));
    }
}
