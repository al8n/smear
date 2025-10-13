const ALL: &str = include_str!("../../../fixtures/parser/graphql/ok/0030_values.graphql");

#[test]
fn values() {
  use smear::parser::graphql::ast::{Document, ParseStr};

  let document =
    Document::<&str>::parse_str(ALL)
      .unwrap();

  let query = document
    .definitions()
    .first()
    .unwrap()
    .unwrap_definition_ref()
    .unwrap_executable_ref()
    .unwrap_operation_ref()
    .unwrap_shorthand_ref();
  let mut iter = query.selections().iter();

  {
    let user = iter.next().unwrap().unwrap_field_ref();
    assert_eq!(user.name().source(), "user");
    let arguments = user.arguments().cloned().unwrap();
    assert_eq!(arguments.arguments().len(), 7);
    let mut arg_iter = arguments.arguments().iter();

    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name().source(), "id");
      let value = arg.value();
      assert_eq!(value.unwrap_int_ref().source(), "4");
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name().source(), "size");
      let value = arg.value();
      assert_eq!(value.unwrap_variable_ref().name().source(), "size");
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name().source(), "value");
      let value = arg.value();
      assert_eq!(
        value.unwrap_string_ref().source().trim_matches('"'),
        "string"
      );
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name().source(), "input");
      let value = arg.value();
      let list = value.unwrap_list_ref();
      let items = list.values();
      assert_eq!(items.len(), 2);
      {
        let first = &items[0];
        assert_eq!(first.unwrap_string_ref().source().trim_matches('"'), "one");
      }
      {
        let second = &items[1];
        assert_eq!(second.unwrap_float_ref().source(), "1.34");
      }
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name().source(), "otherInput");
      let value = arg.value();
      let object = value.unwrap_object_ref();
      let fields = object.fields();
      assert_eq!(fields.len(), 2);
      {
        let first = &fields[0];
        assert_eq!(first.name().source(), "key");
        assert!(!first.value().unwrap_boolean_ref().value());
      }
      {
        let second = &fields[1];
        assert_eq!(second.name().source(), "output");
        assert_eq!(second.value().unwrap_null_ref().source(), "null");
      }
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name().source(), "emptyList");
      let value = arg.value();
      let list = value.unwrap_list_ref();
      let items = list.values();
      assert_eq!(items.len(), 0);
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name().source(), "emptyObject");
      let value = arg.value();
      let object = value.unwrap_object_ref();
      let fields = object.fields();
      assert_eq!(fields.len(), 0);
    }
  }
}

#[test]
fn graphqlx_values() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let document =
    Document::<&str>::parse_str(ALL)
      .unwrap();

  let query = document
    .definitions()
    .first()
    .unwrap()
    .unwrap_definition_ref()
    .unwrap_executable_ref()
    .unwrap_operation_ref()
    .unwrap_shorthand_ref();
  let mut iter = query.selections().iter();

  {
    let user = iter.next().unwrap().unwrap_field_ref();
    assert_eq!(user.name(), "user");
    let arguments = user.arguments().cloned().unwrap();
    assert_eq!(arguments.arguments().len(), 7);
    let mut arg_iter = arguments.arguments().iter();

    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name(), "id");
      let value = arg.value();
      assert_eq!(value.unwrap_int_ref().value().unwrap_decimal(), "4");
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name(), "size");
      let value = arg.value();
      assert_eq!(value.unwrap_variable_ref().name(), "size");
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name(), "value");
      let value = arg.value();
      assert_eq!(
        value.unwrap_string_ref().source().trim_matches('"'),
        "string"
      );
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name().source(), "input");
      let value = arg.value();
      let list = value.unwrap_list_ref();
      let items = list.values();
      assert_eq!(items.len(), 2);
      {
        let first = &items[0];
        assert_eq!(first.unwrap_string_ref().source().trim_matches('"'), "one");
      }
      {
        let second = &items[1];
        assert_eq!(second.unwrap_float_ref().value_ref().unwrap_decimal(), "1.34");
      }
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name(), "otherInput");
      let value = arg.value();
      let object = value.unwrap_object_ref();
      let fields = object.fields();
      assert_eq!(fields.len(), 2);
      {
        let first = &fields[0];
        assert_eq!(first.name(), "key");
        assert!(!first.value().unwrap_boolean_ref().value());
      }
      {
        let second = &fields[1];
        assert_eq!(second.name(), "output");
        assert_eq!(second.value().unwrap_null_ref().source(), "null");
      }
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name(), "emptyList");
      let value = arg.value();
      let list = value.unwrap_list_ref();
      let items = list.values();
      assert_eq!(items.len(), 0);
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name(), "emptyObject");
      let value = arg.value();
      let object = value.unwrap_object_ref();
      let fields = object.fields();
      assert_eq!(fields.len(), 0);
    }
  }
}
