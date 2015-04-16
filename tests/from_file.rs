extern crate config;

use std::path::Path;

use config::reader;
use config::types::Value;
use config::types::ScalarValue;

#[test]
fn conf_from_file() {
    let my_conf = reader::from_file(Path::new("tests/sample.conf"));
    assert!(my_conf.is_ok());

    // Configuration was successfully parsed and is now loaded
    let configuration = my_conf.unwrap();

    // Lookup in this case will return an Option<&String>
    let my_app_title = configuration.lookup_str("application.window.title");
    assert!(my_app_title.is_some());
    assert_eq!(&my_app_title.unwrap()[..], "My Application");

    // Accessing out-of-bound positions in arrays or lists will result in a None being returned
    let out_of_bounds = configuration.lookup_str("list.[2].[0].[0]");
    assert!(out_of_bounds.is_none());

    // Lists are heterogeneous. Let's browse a list
    let list_example1 = configuration.lookup_boolean("list.[4].a.[2]");
    assert!(list_example1.is_some());
    assert_eq!(list_example1.unwrap(), true);

    // What if we don't know what type is a specific list element?
    // We can use a generic lookup and then match it
    let list_element_unknown_type = configuration.lookup("books.[0]");
    assert!(list_element_unknown_type.is_some());
    let unknown_type = list_element_unknown_type.unwrap();
    let matches_expected =
        match unknown_type {
            &Value::Svalue(ScalarValue::Integer32(_)) => { /* It's an integer */ false },
            &Value::Svalue(ScalarValue::Floating32(_)) => { /* It's a floating32 */ false },
            /* ... */
            &Value::Svalue(ScalarValue::Str(ref s)) => {
                /* It's a string */
                &s[..] == "inventory" },
            _ => false
        };
    assert!(matches_expected);

    // Find and inspect the list of known states
    if let Some(&Value::Array(ref array)) = configuration.lookup("application.group1.states") {
        // How many states do we know?
        let states_count = array.len();
        assert_eq!(states_count, 5);

        // California here we go!

        // With another lookup...
        let california = configuration.lookup_str("application.group1.states.[1]");
        assert!(california.is_some());
        assert_eq!(&california.unwrap()[..], "CA");

        // Or accessing it directly
        let ca = &array[1];
        assert_eq!(ca, &Value::Svalue(ScalarValue::Str("CA".to_string())));
    } else {
        assert!(false);
    }
}
