//! This module defines the internal types used to represent a configuration.

use std::collections::HashMap;

/// The top-level `Config` type that represents a configuration
#[derive(PartialEq)]
#[derive(Debug)]
#[unstable = "Library still under heavy development; design may change."]
pub struct Config {
    root: Value
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
pub type ArrayValue = Vec<Value>;

/// The type used to represent the generic values inside a list.
/// Lists are heterogeneous and can store any type of value, including other lists.
#[unstable = "Library still under heavy development; design may change."]
pub type ListValue = Vec<Value>;

impl Config {
    /// Creates a new wrapper `Config` to hold a `SettingsList`
    #[unstable = "Library still under heavy development; design may change."]
    pub fn new(sl: SettingsList) -> Config {
        Config { root: Value::Group(sl) }
    }

    #[unstable = "Library still under heavy development; design may change."]
    pub fn lookup(&self, path: &str) -> Option<&Value> {
        let mut last_value = &self.root;
        for segment in path.split(".") {
            if segment.starts_with("[") {
                if !segment.ends_with("]") || segment.len() < 3 {
                    return None;
                }
                if let Ok(index) = (&segment[1..segment.len()-1]).parse::<usize>() {
                    if let &Value::Array(ref arr) = last_value {
                        if index >= arr.len() {
                            return None;
                        }
                        last_value = &arr[index];
                    }
                    else if let &Value::List(ref list) = last_value {
                        if index >= list.len() {
                            return None;
                        }
                        last_value = &list[index];
                    } else {
                        return None;
                    }
                } else {
                    return None;
                }
            } else {
                if let &Value::Group(ref settings_list) = last_value {
                    let next_setting = match settings_list.get(&segment[..]) {
                        Some(v) => v,
                        None => return None
                    };
                    last_value = &next_setting.value;
                } else {
                    return None;
                }
            }
        }
        Some(last_value)
    }
}

impl Setting {
    /// Creates a new setting with a given name and value
    /// # Examples 
    /// Let's say we want to create a setting to store an `i32`.
    /// We start by creating a `ScalarValue`:
    ///
    /// ```
    /// use config::types::ScalarValue;
    /// # use config::types::Value;
    /// # use config::types::Setting;
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
    /// # use config::types::ScalarValue;
    /// use config::types::Value;
    /// # use config::types::Setting;
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
    /// # use config::types::ScalarValue;
    /// # use config::types::Value;
    /// use config::types::Setting;
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
    /// use config::types::ScalarValue;
    /// use config::types::Value;
    /// use config::types::Setting;
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

#[cfg(test)]
mod test {
    use super::Config;
    use types::{Value, ScalarValue, SettingsList, Setting};

    #[test]
    fn simple_bool_lookup() {

        let mut my_settings = SettingsList::new();
        my_settings.insert("windows".to_string(),
                           Setting::new("windows".to_string(),
                                        Value::Svalue(ScalarValue::Boolean(false))));
        my_settings.insert("linux".to_string(),
                           Setting::new("linux".to_string(),
                                        Value::Svalue(ScalarValue::Boolean(true))));
        my_settings.insert("UNIX".to_string(),
                           Setting::new("UNIX".to_string(),
                                        Value::Svalue(ScalarValue::Boolean(false))));

        let my_conf = Config::new(my_settings);

        let windows_lookup = my_conf.lookup("windows");
        assert!(windows_lookup.is_some());
        assert_eq!(windows_lookup.unwrap(), &Value::Svalue(ScalarValue::Boolean(false)));

        let linux_lookup = my_conf.lookup("linux");
        assert!(linux_lookup.is_some());
        assert_eq!(linux_lookup.unwrap(), &Value::Svalue(ScalarValue::Boolean(true)));

        let unix_lookup = my_conf.lookup("UNIX");
        assert!(unix_lookup.is_some());
        assert_eq!(unix_lookup.unwrap(), &Value::Svalue(ScalarValue::Boolean(false)));
    }

    #[test]
    fn simple_integer_lookup() {

        let mut my_settings = SettingsList::new();
        my_settings.insert("miles".to_string(),
                           Setting::new("miles".to_string(),
                                        Value::Svalue(ScalarValue::Integer32(3))));
        my_settings.insert("mpg".to_string(),
                           Setting::new("mpg".to_string(),
                                        Value::Svalue(ScalarValue::Integer32(27))));

        let my_conf = Config::new(my_settings);

        let miles_lookup = my_conf.lookup("miles");
        assert!(miles_lookup.is_some());
        assert_eq!(miles_lookup.unwrap(), &Value::Svalue(ScalarValue::Integer32(3)));

        let mpg_lookup = my_conf.lookup("mpg");
        assert!(mpg_lookup.is_some());
        assert_eq!(mpg_lookup.unwrap(), &Value::Svalue(ScalarValue::Integer32(27)));
    }

    #[test]
    fn lookup_nested_empty_list() {
        // list = ((()));
        let mut my_settings = SettingsList::new();
        my_settings.insert("list".to_string(),
                           Setting::new("list".to_string(),
                                        Value::List(vec![
                                            Value::List(vec![
                                                Value::List(Vec::new())])])));

        let my_conf = Config::new(my_settings);

        let first = my_conf.lookup("list.[0]");
        assert!(first.is_some());
        assert_eq!(first.unwrap(), &Value::List(vec![Value::List(Vec::new())]));

        let second = my_conf.lookup("list.[0].[0]");
        assert!(second.is_some());
        assert_eq!(second.unwrap(), &Value::List(Vec::new()));
    }

    #[test]
    fn lookup_scalar_list() {

        let mut my_settings = SettingsList::new();
        my_settings.insert("my_list".to_string(),
                        Setting::new("my_list".to_string(),
                                     Value::List(vec![
                                         Value::Svalue(
                                             ScalarValue::Str("a \"string\" with \nquo\ttes"
                                                              .to_string())),
                                         Value::Svalue(
                                             ScalarValue::Integer64(9000000000000000000i64))])));

        let my_conf = Config::new(my_settings);

        let the_string = my_conf.lookup("my_list.[0]");
        assert!(the_string.is_some());
        assert_eq!(the_string.unwrap(), &Value::Svalue(ScalarValue::Str(
            "a \"string\" with \nquo\ttes".to_string())));

        let big_int = my_conf.lookup("my_list.[1]");
        assert!(big_int.is_some());
        assert_eq!(big_int.unwrap(), &Value::Svalue(ScalarValue::Integer64(9000000000000000000i64)));

    }

/*
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
                                         ScalarValue::Boolean(true),
                                         ScalarValue::Boolean(true),
                                         ScalarValue::Boolean(true),
                                         ScalarValue::Boolean(false),
                                         ScalarValue::Boolean(false),
                                         ScalarValue::Boolean(false),
                                         ScalarValue::Boolean(true)])));

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
                                         ScalarValue::Integer32(10),
                                         ScalarValue::Integer32(11),
                                         ScalarValue::Integer32(12)])));
        expected.insert("array".to_string(),
                        Setting::new("array".to_string(),
                                     Value::Array(vec![ScalarValue::Integer32(1)])));

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
                                         ScalarValue::Integer64(9000000000000000000i64),
                                         ScalarValue::Integer64(8000000000000000002i64),
                                         ScalarValue::Integer64(5)])));
        expected.insert("b".to_string(),
                        Setting::new("b".to_string(),
                                     Value::Array(vec![
                                         ScalarValue::Integer64(5),
                                         ScalarValue::Integer64(6),
                                         ScalarValue::Integer64(7)])));

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
                                         ScalarValue::Floating32(4.5),
                                         ScalarValue::Floating32(0.5),
                                         ScalarValue::Floating32(0.25)])));

        expected.insert("b".to_string(),
                        Setting::new("b".to_string(),
                                     Value::Array(vec![
                                         ScalarValue::Floating32(5.0e-1),
                                         ScalarValue::Floating32(1.0)])));

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
                                         ScalarValue::Floating64(55937598585.5),
                                         ScalarValue::Floating64(10000000000.25)])));

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

        assert_eq!(parsed.unwrap(), expected);
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

        assert_eq!(parsed.unwrap(), expected);
    }
*/
}
