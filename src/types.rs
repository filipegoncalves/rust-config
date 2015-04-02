use std::collections::HashMap;

/// The top-level `Config` type that represents a configuration
#[derive(PartialEq)]
#[derive(Debug)]
#[unstable = "Library still under heavy development; design may change."]
pub struct Config {
    /// The settings inside the configuration
    pub settings: SettingsList
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
