use smear_graphql::parser::ast::{Document, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0030_values.graphql");

#[test]
fn values() {
  let document =
    Document::<&str>::parse_str(ALL)
      .unwrap();

  let query = document
    .definitions()
    .first()
    .unwrap()
    .unwrap_executable_ref()
    .unwrap_operation_ref()
    .unwrap_shorthand_ref();
  let mut iter = query.selections().iter();

  {
    let user = iter.next().unwrap().unwrap_field_ref();
    assert_eq!(user.name().slice(), "user");
    let arguments = user.arguments().cloned().unwrap();
    assert_eq!(arguments.arguments().len(), 7);
    let mut arg_iter = arguments.arguments().iter();

    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name().slice(), "id");
      let value = arg.value();
      assert_eq!(value.unwrap_int_ref().slice(), "4");
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name().slice(), "size");
      let value = arg.value();
      assert_eq!(value.unwrap_variable_ref().name().slice(), "size");
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name().slice(), "value");
      let value = arg.value();
      assert_eq!(
        value.unwrap_string_ref().content(),
        "string"
      );
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name().slice(), "input");
      let value = arg.value();
      let list = value.unwrap_list_ref();
      let items = list.values();
      assert_eq!(items.len(), 2);
      {
        let first = &items[0];
        assert_eq!(first.unwrap_string_ref().content(), "one");
      }
      {
        let second = &items[1];
        assert_eq!(second.unwrap_float_ref().slice(), "1.34");
      }
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name().slice(), "otherInput");
      let value = arg.value();
      let object = value.unwrap_object_ref();
      let fields = object.fields();
      assert_eq!(fields.len(), 2);
      {
        let first = &fields[0];
        assert_eq!(first.name().slice(), "key");
        assert_eq!(first.value().unwrap_boolean_ref().slice(), "false");
      }
      {
        let second = &fields[1];
        assert_eq!(second.name().slice(), "output");
        assert_eq!(second.value().unwrap_null_ref().slice(), "null");
      }
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name().slice(), "emptyList");
      let value = arg.value();
      let list = value.unwrap_list_ref();
      let items = list.values();
      assert_eq!(items.len(), 0);
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name().slice(), "emptyObject");
      let value = arg.value();
      let object = value.unwrap_object_ref();
      let fields = object.fields();
      assert_eq!(fields.len(), 0);
    }
  }
}
