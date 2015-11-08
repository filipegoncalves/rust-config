//! Internal types used to represent a configuration and corresponding primitives to browse it

use std::collections::HashMap;
use std::string::ToString;

/// The top-level `Config` type that represents a configuration
#[derive(PartialEq)]
#[derive(Debug)]
pub struct Config {
    root: Value
}

/// Settings list representation. Associates settings to their names.
pub type SettingsList = HashMap<String, Setting>;

/// A `Setting` representation. Settings have a name and a value.
#[derive(PartialEq)]
#[derive(Debug)]
pub struct Setting {
    /// Setting name, as read from the configuration file
    pub name: String,
    /// This setting's value. A value can be a scalar, an array, a list, or a group.
    pub value: Value
}

/// A type representing a generic value. `Setting`s store `Value`s.
#[derive(PartialEq)]
#[derive(Debug)]
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
pub type ArrayValue = Vec<Value>;

/// The type used to represent the generic values inside a list.
/// Lists are heterogeneous and can store any type of value, including other lists.
pub type ListValue = Vec<Value>;

impl Config {
    /// Creates a new wrapper `Config` to hold a `SettingsList`
    pub fn new(sl: SettingsList) -> Config {
        Config { root: Value::Group(sl) }
    }

    /// Looks up a value in a configuration. A path is a dot-separated list of settings
    /// describing the path of the desired value in the configuration.
    /// Returns `None` if the path is invalid. A path is invalid if it is not syntactically
    /// well-formed, if it attempts to index an array or list beyond the limit, or if it
    /// includes an unknown setting.
    /// # Examples
    /// Suppose we have loaded a configuration that consists of:
    ///
    /// ```text
    /// my_string = "hello";
    /// a_list = ([1, 2, 3], true, { x = 4; }, "good bye");
    /// ```
    ///
    /// Then, the path to retrieve `"hello"` is `my_string`.
    /// The path to retrieve `true` inside `a_list` would be `a_list.[1]`.
    /// The path to retrieve the setting `x` inside `a_list` would be `alist.[2].x`.
    ///
    /// Here's a small demonstration:
    ///
    /// ```
    /// use config::reader::from_str;
    ///
    /// let my_conf = from_str("my_string = \"hello\"; a_list = ([1, 2, 3], true, { x = 4; }, \"good_bye\");").unwrap();
    ///
    /// let my_str_value = my_conf.lookup("my_string");
    /// assert!(my_str_value.is_some());
    ///
    /// let my_boolean_value = my_conf.lookup("a_list.[1]");
    /// assert!(my_boolean_value.is_some());
    ///
    /// let my_x_setting = my_conf.lookup("a_list.[2].x");
    /// assert!(my_x_setting.is_some());
    ///
    /// ```
    ///
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

    /// A convenient wrapper around `lookup()` that unwraps the underlying primitive
    /// type of a generic `Value`.
    ///
    /// Returns `None` in the same way `lookup()` does; or if the underlying `Value`
    /// type does not match with the requested type - in this case, `bool`.
    pub fn lookup_boolean(&self, path: &str) -> Option<bool> {
        self.lookup(path).and_then(|v|
                                   match v {
                                       &Value::Svalue(ScalarValue::Boolean(b)) => Some(b),
                                       _ => None
                                   })
    }

    /// A convenient wrapper around `lookup()` that unwraps the underlying primitive
    /// type of a generic `Value`.
    ///
    /// Returns `None` in the same way `lookup()` does; or if the underlying `Value`
    /// type does not match with the requested type - in this case, `i32`.
    pub fn lookup_integer32(&self, path: &str) -> Option<i32> {
        self.lookup(path).and_then(|v|
                                   match v {
                                       &Value::Svalue(ScalarValue::Integer32(x)) => Some(x),
                                       _ => None
                                   })
    }

    /// A convenient wrapper around `lookup()` that unwraps the underlying primitive
    /// type of a generic `Value`.
    ///
    /// Returns `None` in the same way `lookup()` does; or if the underlying `Value`
    /// type does not match with the requested type - in this case, `i64`.
    pub fn lookup_integer64(&self, path: &str) -> Option<i64> {
        self.lookup(path).and_then(|v|
                                   match v {
                                       &Value::Svalue(ScalarValue::Integer64(x)) => Some(x),
                                       _ => None
                                   })
    }

    /// A convenient wrapper around `lookup()` that unwraps the underlying primitive
    /// type of a generic `Value`.
    ///
    /// Returns `None` in the same way `lookup()` does; or if the underlying `Value`
    /// type does not match with the requested type - in this case, `f32`.
    pub fn lookup_floating32(&self, path: &str) -> Option<f32> {
        self.lookup(path).and_then(|v|
                                   match v {
                                       &Value::Svalue(ScalarValue::Floating32(x)) => Some(x),
                                       _ => None
                                   })
    }

    /// A convenient wrapper around `lookup()` that unwraps the underlying primitive
    /// type of a generic `Value`.
    ///
    /// Returns `None` in the same way `lookup()` does; or if the underlying `Value`
    /// type does not match with the requested type - in this case, `f64`.
    pub fn lookup_floating64(&self, path: &str) -> Option<f64> {
        self.lookup(path).and_then(|v|
                                   match v {
                                       &Value::Svalue(ScalarValue::Floating64(x)) => Some(x),
                                       _ => None
                                   })
    }

    /// A convenient wrapper around `lookup()` that unwraps the underlying primitive
    /// type of a generic `Value`.
    ///
    /// Returns `None` in the same way `lookup()` does; or if the underlying `Value`
    /// type does not match with the requested type - in this case, `String`.
    pub fn lookup_str(&self, path: &str) -> Option<&str> {
        self.lookup(path).and_then(|v|
                                   match v {
                                       &Value::Svalue(ScalarValue::Str(ref s)) => Some(&s[..]),
                                       _ => None
                                   })
    }

    /// A convenient wrapper around `lookup_boolean()` that unwraps the underlying primitive
    /// type of a boolean `Value`.
    ///
    /// If either of `lookup_boolean()` or `lookup` return `None`,
    /// then the user-provided default value is returned.
    pub fn lookup_boolean_or(&self, path: &str, default: bool) -> bool {
        self.lookup_boolean(path).unwrap_or(default)
    }

    /// A convenient wrapper around `lookup_integer32()` that unwraps the underlying primitive
    /// type of an integer32 `Value`.
    ///
    /// If either of `lookup_integer32()` or `lookup` return `None`,
    /// then the user-provided default value is returned.
    pub fn lookup_integer32_or(&self, path: &str, default: i32) -> i32 {
        self.lookup_integer32(path).unwrap_or(default)
    }

    /// A convenient wrapper around `lookup_integer64()` that unwraps the underlying primitive
    /// type of an integer64 `Value`.
    ///
    /// If either of `lookup_integer64()` or `lookup` return `None`,
    /// then the user-provided default value is returned.
    pub fn lookup_integer64_or(&self, path: &str, default: i64) -> i64 {
        self.lookup_integer64(path).unwrap_or(default)
    }

    /// A convenient wrapper around `lookup_floating32()` that unwraps the underlying primitive
    /// type of an floating32 `Value`.
    ///
    /// If either of `lookup_floating32()` or `lookup` return `None`,
    /// then the user-provided default value is returned.
    pub fn lookup_floating32_or(&self, path: &str, default: f32) -> f32 {
        self.lookup_floating32(path).unwrap_or(default)
    }

    /// A convenient wrapper around `lookup_floating64()` that unwraps the underlying primitive
    /// type of an floating64 `Value`. If either of `lookup_floating64()` or `lookup` return `None`,
    /// then the user-provided default value is returned.
    pub fn lookup_floating64_or(&self, path: &str, default: f64) -> f64 {
        self.lookup_floating64(path).unwrap_or(default)
    }

    /// A convenient wrapper around `lookup_str()` that unwraps the underlying primitive
    /// type of a string `Value`.
    ///
    /// If either of `lookup_str()` or `lookup` return `None`,
    /// then the user-provided default value is returned.
    pub fn lookup_str_or<'a>(&'a self, path: &str, default: &'a str) -> &'a str {
        self.lookup_str(path).unwrap_or(default)
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
    pub fn new(sname: String, val: Value) -> Setting {
        Setting { name: sname, value: val }
    }
}

// Implement to_string() method for scalar value
impl ToString for ScalarValue {
    fn to_string(&self) -> String {
        match self {
            &ScalarValue::Boolean(ref value) => value.to_string(),
            &ScalarValue::Integer32(ref value) => value.to_string(),
            &ScalarValue::Integer64(ref value) => value.to_string(),
            &ScalarValue::Floating32(ref value) => value.to_string(),
            &ScalarValue::Floating64(ref value) => value.to_string(),
            &ScalarValue::Str(ref value) => value.clone(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::Config;
    use types::{Value, ScalarValue, SettingsList, Setting};

    #[test]
    fn simple_lookup_generic_bool() {

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
    fn simple_lookup_bool() {
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

        let windows_lookup = my_conf.lookup_boolean("windows");
        assert!(windows_lookup.is_some());
        assert_eq!(windows_lookup.unwrap(), false);

        let linux_lookup = my_conf.lookup_boolean("linux");
        assert!(linux_lookup.is_some());
        assert_eq!(linux_lookup.unwrap(), true);

        let unix_lookup = my_conf.lookup_boolean("UNIX");
        assert!(unix_lookup.is_some());
        assert_eq!(unix_lookup.unwrap(), false);
    }

    #[test]
    fn simple_lookup_generic_integer32() {

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
    fn simple_lookup_integer32() {

        let mut my_settings = SettingsList::new();
        my_settings.insert("miles".to_string(),
                           Setting::new("miles".to_string(),
                                        Value::Svalue(ScalarValue::Integer32(3))));
        my_settings.insert("mpg".to_string(),
                           Setting::new("mpg".to_string(),
                                        Value::Svalue(ScalarValue::Integer32(27))));

        let my_conf = Config::new(my_settings);

        let miles_lookup = my_conf.lookup_integer32("miles");
        assert!(miles_lookup.is_some());
        assert_eq!(miles_lookup.unwrap(), 3);

        let mpg_lookup = my_conf.lookup_integer32("mpg");
        assert!(mpg_lookup.is_some());
        assert_eq!(mpg_lookup.unwrap(), 27);
    }

    #[test]
    fn simple_lookup_default() {

        let mut my_settings = SettingsList::new();
        my_settings.insert("miles".to_string(),
                           Setting::new("miles".to_string(),
                                        Value::Svalue(ScalarValue::Integer32(3))));
        my_settings.insert("mpg".to_string(),
                           Setting::new("mpg".to_string(),
                                        Value::Svalue(ScalarValue::Integer32(27))));

        let my_conf = Config::new(my_settings);

        let miles = my_conf.lookup_integer32_or("miles", 4);
        assert_eq!(miles, 3);

        let invalid_lookup = my_conf.lookup_integer32_or("blablabla", 22);
        assert_eq!(invalid_lookup, 22);
    }

    #[test]
    fn lookup_nested_empty_list() {
        // ((()));
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

    #[test]
    fn lookup_array() {
        let mut my_settings = SettingsList::new();
        my_settings.insert("my_array".to_string(),
                           Setting::new("my_array".to_string(),
                                        Value::Array(vec![
                                            Value::Svalue(ScalarValue::Boolean(true)),
                                            Value::Svalue(ScalarValue::Boolean(false)),
                                            Value::Svalue(ScalarValue::Boolean(true))])));

        let my_conf = Config::new(my_settings);

        let value0 = my_conf.lookup("my_array.[0]");
        assert!(value0.is_some());
        assert_eq!(value0.unwrap(), &Value::Svalue(ScalarValue::Boolean(true)));

        let value1 = my_conf.lookup("my_array.[1]");
        assert!(value1.is_some());
        assert_eq!(value1.unwrap(), &Value::Svalue(ScalarValue::Boolean(false)));

        let value2 = my_conf.lookup("my_array.[2]");
        assert!(value2.is_some());
        assert_eq!(value2.unwrap(), &Value::Svalue(ScalarValue::Boolean(true)));
    }

    #[test]
    fn lookup_values_list() {

        /* my_superb_list = (
         *     [yes, no],
         *     21,
         *     [0.25, .5, .125],
         *     (()),
         *     (("a")),
         *     ("a"),
         *     ["\"x\""],
         *     (
         *         14,
         *         ["x"],
         *         (
         *             true,
         *             (
         *                 false,
         *                 (
         *                     4
         *                 ),
         *                 [5, 6]
         *             ),
         *             "y"
         *         )
         *    ),
         *    "goodbye!\r\n",
         *    {
         *        s = [1, 2];
         *        x = "str";
         *        y = ();
         *    }
         * )
         */

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

        let mut my_settings = SettingsList::new();
        my_settings.insert("my_superb_list".to_string(),
                           Setting::new("my_superb_list".to_string(), Value::List(list_elements)));

        let my_conf = Config::new(my_settings);

        let lookup_bool = my_conf.lookup("my_superb_list.[0].[1]");
        assert!(lookup_bool.is_some());
        assert_eq!(lookup_bool.unwrap(), &Value::Svalue(ScalarValue::Boolean(false)));

        let lookup_empty_lst = my_conf.lookup("my_superb_list.[3].[0]");
        assert!(lookup_empty_lst.is_some());
        assert_eq!(lookup_empty_lst.unwrap(), &Value::List(Vec::new()));

        let lookup_deep = my_conf.lookup("my_superb_list.[7].[2].[1].[2].[1]");
        assert!(lookup_deep.is_some());
        assert_eq!(lookup_deep.unwrap(), &Value::Svalue(ScalarValue::Integer32(6)));

        let lookup_str = my_conf.lookup("my_superb_list.[9].x");
        assert!(lookup_str.is_some());
        assert_eq!(lookup_str.unwrap(), &Value::Svalue(ScalarValue::Str("str".to_string())));

        let lookup_deep_int = my_conf.lookup("my_superb_list.[9].s.[1]");
        assert!(lookup_deep_int.is_some());
        assert_eq!(lookup_deep_int.unwrap(), &Value::Svalue(ScalarValue::Integer32(2)));

        let lookup_empty_lst = my_conf.lookup("my_superb_list.[9].y");
        assert!(lookup_empty_lst.is_some());
        assert_eq!(lookup_empty_lst.unwrap(), &Value::List(Vec::new()));
    }

    #[test]
    fn lookup_invalid_path() {
        let mut my_settings = SettingsList::new();
        my_settings.insert("my_array".to_string(),
                           Setting::new("my_array".to_string(),
                                        Value::Array(vec![
                                            Value::Svalue(ScalarValue::Boolean(true)),
                                            Value::Svalue(ScalarValue::Boolean(false)),
                                            Value::Svalue(ScalarValue::Boolean(true))])));

        let my_conf = Config::new(my_settings);

        let value0 = my_conf.lookup("my_array.[1456]");
        assert!(value0.is_none());

        let value1 = my_conf.lookup("my_array.[-30]");
        assert!(value1.is_none());

        let value2 = my_conf.lookup("something_that_does_not_exist.[14].lala.lele.[24]");
        assert!(value2.is_none());
    }

    #[test]
    fn lookup_invalid_type() {
        let mut my_settings = SettingsList::new();
        my_settings.insert("my_array".to_string(),
                           Setting::new("my_array".to_string(),
                                        Value::Array(vec![
                                            Value::Svalue(ScalarValue::Boolean(true)),
                                            Value::Svalue(ScalarValue::Boolean(false)),
                                            Value::Svalue(ScalarValue::Boolean(true))])));

        let my_conf = Config::new(my_settings);

        let value0 = my_conf.lookup_integer32("my_array.[0]");
        assert!(value0.is_none());

        let value1 = my_conf.lookup_str("my_array.[1]");
        assert!(value1.is_none());
    }

    #[test]
    fn scalar_value_to_string() {
        let mut value = ScalarValue::Boolean(true);
        assert_eq!(value.to_string(), "true");

        value = ScalarValue::Integer32(32i32);
        assert_eq!(value.to_string(), "32");

        value = ScalarValue::Integer64(-64i64);
        assert_eq!(value.to_string(), "-64");

        value = ScalarValue::Floating32(3f32);
        assert!(value.to_string().starts_with("3"));

        value = ScalarValue::Floating64(99f64);
        assert!(value.to_string().starts_with("99"));

        value = ScalarValue::Str("this is a string".to_string());
        assert_eq!(value.to_string(), "this is a string");
    }
}
