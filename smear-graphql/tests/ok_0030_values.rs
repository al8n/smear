use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = r###"
{
    user(
        id: 4,
        size: $size
        value: "string",
        input: [ "one", 1.34 ],
        otherInput: { key: false, output: null }
        emptyList: []
        emptyObject: {}
    )
}

"###;

#[test]
fn values() {
  let document =
    Document::<WithSource<&str, SimpleSpan>>::parse_str_padded::<extra::Err<Rich<char>>>(ALL)
      .unwrap();

  let query = document
    .content()
    .first()
    .unwrap()
    .unwrap_definition_ref()
    .unwrap_operation_ref()
    .unwrap_shorthand_ref();
  let mut iter = query.selections().iter();

  {
    let user = iter.next().unwrap().unwrap_field_ref();
    assert_eq!(user.name().span().source(), &"user");
    let arguments = user.arguments().cloned().unwrap();
    assert_eq!(arguments.arguments().len(), 7);
    let mut arg_iter = arguments.arguments().iter();

    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name().span().source(), &"id");
      let value = arg.value();
      assert_eq!(value.unwrap_int_ref().span().source(), &"4");
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name().span().source(), &"size");
      let value = arg.value();
      assert_eq!(value.unwrap_variable_ref().name().span().source(), &"size");
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name().span().source(), &"value");
      let value = arg.value();
      assert_eq!(
        value.unwrap_string_ref().content().span().source(),
        &"string"
      );
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name().span().source(), &"input");
      let value = arg.value();
      let list = value.unwrap_list_ref();
      let items = list.values();
      assert_eq!(items.len(), 2);
      {
        let first = &items[0];
        assert_eq!(first.unwrap_string_ref().content().span().source(), &"one");
      }
      {
        let second = &items[1];
        assert_eq!(second.unwrap_float_ref().span().source(), &"1.34");
      }
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name().span().source(), &"otherInput");
      let value = arg.value();
      let object = value.unwrap_object_ref();
      let fields = object.fields();
      assert_eq!(fields.len(), 2);
      {
        let first = &fields[0];
        assert_eq!(first.name().span().source(), &"key");
        assert_eq!(first.value().unwrap_boolean_ref().span().source(), &"false");
      }
      {
        let second = &fields[1];
        assert_eq!(second.name().span().source(), &"output");
        assert_eq!(second.value().unwrap_null_ref().span().source(), &"null");
      }
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name().span().source(), &"emptyList");
      let value = arg.value();
      let list = value.unwrap_list_ref();
      let items = list.values();
      assert_eq!(items.len(), 0);
    }
    {
      let arg = arg_iter.next().unwrap();
      assert_eq!(arg.name().span().source(), &"emptyObject");
      let value = arg.value();
      let object = value.unwrap_object_ref();
      let fields = object.fields();
      assert_eq!(fields.len(), 0);
    }
  }
}
