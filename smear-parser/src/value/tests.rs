use std::{string::String, vec};

use smear_lexer::LitStr;
use tokora::{
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::{
  BlockStringValue, BooleanValue, DefaultInputValue, EnumValue, FloatValue, InlineStringValue,
  IntValue, List, NullValue, Object, ObjectField, StringValue, VariableValue,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct CustomSpan(u8);

struct ArrayBacked<T, const N: usize>([T; N]);

impl<T, const N: usize> AsRef<[T]> for ArrayBacked<T, N> {
  fn as_ref(&self) -> &[T] {
    &self.0
  }
}

trait CustomLang {}

#[test]
fn carriers_support_custom_spans_and_language_markers() {
  let boolean = BooleanValue::<str, CustomSpan, dyn CustomLang>::new(CustomSpan(1), true);
  assert_eq!(boolean.as_span(), &CustomSpan(1));
  assert_eq!(
    BooleanValue::<str, CustomSpan, dyn CustomLang>::new(CustomSpan(1), true).into_span(),
    CustomSpan(1)
  );
  assert_eq!(boolean.into_components(), (CustomSpan(1), true));

  let enum_value = EnumValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(2), "ACTIVE");
  assert_eq!(enum_value.as_span(), &CustomSpan(2));
  assert_eq!(
    EnumValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(2), "ACTIVE").into_span(),
    CustomSpan(2)
  );
  assert_eq!(enum_value.into_components(), (CustomSpan(2), "ACTIVE"));

  let float = FloatValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(3), "1.5");
  assert_eq!(float.as_span(), &CustomSpan(3));
  assert_eq!(
    FloatValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(3), "1.5").into_span(),
    CustomSpan(3)
  );
  assert_eq!(float.into_components(), (CustomSpan(3), "1.5"));

  let int = IntValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(4), "1");
  assert_eq!(int.as_span(), &CustomSpan(4));
  assert_eq!(
    IntValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(4), "1").into_span(),
    CustomSpan(4)
  );
  assert_eq!(int.into_components(), (CustomSpan(4), "1"));

  let null = NullValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(5), "null");
  assert_eq!(null.as_span(), &CustomSpan(5));
  assert_eq!(
    NullValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(5), "null").into_span(),
    CustomSpan(5)
  );
  assert_eq!(null.into_components(), (CustomSpan(5), "null"));

  let inline_lit = match LitStr::try_from("\"inline\"").unwrap() {
    LitStr::Inline(lit) => lit,
    LitStr::Block(_) => panic!("inline spelling produced a block literal"),
  };
  let string =
    StringValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(6), LitStr::Inline(inline_lit));
  assert_eq!(string.as_span(), &CustomSpan(6));
  assert_eq!(
    StringValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(6), LitStr::Inline(inline_lit),)
      .into_span(),
    CustomSpan(6)
  );
  let (span, lit) = string.into_components();
  assert_eq!(span, CustomSpan(6));
  assert_eq!(lit.source_ref(), &"\"inline\"");

  let inline = InlineStringValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(7), inline_lit);
  assert_eq!(inline.as_span(), &CustomSpan(7));
  assert_eq!(
    InlineStringValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(7), inline_lit).into_span(),
    CustomSpan(7)
  );
  let (span, lit) = inline.into_components();
  assert_eq!(span, CustomSpan(7));
  assert_eq!(lit.source_ref(), &"\"inline\"");

  let block_lit = match LitStr::try_from("\"\"\"block\"\"\"").unwrap() {
    LitStr::Block(lit) => lit,
    LitStr::Inline(_) => panic!("block spelling produced an inline literal"),
  };
  let block = BlockStringValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(8), block_lit);
  assert_eq!(block.as_span(), &CustomSpan(8));
  assert_eq!(
    BlockStringValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(8), block_lit).into_span(),
    CustomSpan(8)
  );
  let (span, lit) = block.into_components();
  assert_eq!(span, CustomSpan(8));
  assert_eq!(lit.source_ref(), &"\"\"\"block\"\"\"");

  let variable = VariableValue::<_, CustomSpan>::new(CustomSpan(9), "value");
  assert_eq!(variable.as_span(), &CustomSpan(9));
  assert_eq!(
    VariableValue::<_, CustomSpan>::new(CustomSpan(9), "value").into_span(),
    CustomSpan(9)
  );
  assert_eq!(variable.into_components(), (CustomSpan(9), "value"));

  let list = List::<i32, CustomSpan>::new(CustomSpan(10), vec![1, 2]);
  assert_eq!(list.as_span(), &CustomSpan(10));
  assert_eq!(list.values(), &[1, 2]);
  assert_eq!(list.into_components(), (CustomSpan(10), vec![1, 2]));

  let field = ObjectField::<_, _, CustomSpan>::new(CustomSpan(15), "answer", 42);
  assert_eq!(field.as_span(), &CustomSpan(15));
  assert_eq!(field.name(), &"answer");
  assert_eq!(field.value(), &42);
  assert_eq!(field.into_components(), (CustomSpan(15), "answer", 42));

  let object_field = ObjectField::<_, _, CustomSpan>::new(CustomSpan(12), "answer", 42);
  let object = Object::<&str, i32, CustomSpan>::new(CustomSpan(13), vec![object_field]);
  assert_eq!(object.as_span(), &CustomSpan(13));
  assert_eq!(object.fields().len(), 1);
  let (span, fields) = object.into_components();
  assert_eq!(span, CustomSpan(13));
  assert_eq!(fields[0].name(), &"answer");

  let default = DefaultInputValue::<_, CustomSpan>::new(CustomSpan(14), 42);
  assert_eq!(default.as_span(), &CustomSpan(14));
  assert_eq!(default.value(), &42);
  assert_eq!(default.into_components(), (CustomSpan(14), 42));

  #[cfg(feature = "graphql")]
  {
    let branded: crate::graphql::ast::BooleanValue<str, CustomSpan> =
      BooleanValue::<str, CustomSpan, crate::graphql::GraphQL>::new(CustomSpan(15), false);
    let _: crate::value::BooleanValue<str, CustomSpan, crate::graphql::GraphQL> = branded;
  }
}

#[cfg(feature = "graphqlx")]
#[test]
fn graphqlx_collections_support_custom_spans() {
  use super::{Map, MapEntry, Set};

  let set = Set::<i32, CustomSpan>::new(CustomSpan(11), vec![1, 2]);
  assert_eq!(set.as_span(), &CustomSpan(11));
  assert_eq!(set.values(), &[1, 2]);
  assert_eq!(set.into_components(), (CustomSpan(11), vec![1, 2]));

  let entry = MapEntry::<_, _, CustomSpan>::new(CustomSpan(12), "answer", 42);
  assert_eq!(entry.as_span(), &CustomSpan(12));
  assert_eq!(entry.key(), &"answer");
  assert_eq!(entry.value(), &42);
  assert_eq!(entry.into_components(), (CustomSpan(12), "answer", 42));

  let map = Map::<_, _, CustomSpan>::new(
    CustomSpan(13),
    vec![MapEntry::<_, _, CustomSpan>::new(
      CustomSpan(14),
      "answer",
      42,
    )],
  );
  assert_eq!(map.as_span(), &CustomSpan(13));
  assert_eq!(map.entries().len(), 1);
  assert_eq!(map.entries()[0].key(), &"answer");
  assert_eq!(map.into_entries()[0].value(), &42);
}

#[test]
fn collection_accessors_project_array_backed_containers_to_slices() {
  let list = List::<u8, CustomSpan, _>::new(CustomSpan(1), ArrayBacked([1_u8, 2]));
  let projected: &[u8] = list.values();
  assert_eq!(projected, &[1, 2]);
  assert_eq!(list.into_values().0, [1, 2]);

  let field = ObjectField::<_, _, CustomSpan>::new(CustomSpan(2), "answer", 42_u8);
  let object = Object::<_, _, CustomSpan, _>::new(CustomSpan(3), ArrayBacked([field]));
  let projected: &[ObjectField<&str, u8, CustomSpan>] = object.fields();
  assert_eq!(projected.len(), 1);
  assert_eq!(projected[0].name(), &"answer");
  assert_eq!(object.into_fields().0[0].value(), &42);
}

#[test]
fn literal_sources_borrow_non_copy_carriers() {
  let int = IntValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(1), String::from("1"));
  let source: &String = int.source();
  assert_eq!(source, "1");

  let float = FloatValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(2), String::from("1.5"));
  let source: &String = float.source();
  assert_eq!(source, "1.5");

  let enumeration =
    EnumValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(3), String::from("ACTIVE"));
  let source: &String = enumeration.source();
  assert_eq!(source, "ACTIVE");

  let null = NullValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(4), String::from("null"));
  let source: &String = null.source();
  assert_eq!(source, "null");

  // The literal carriers ride `into_equivalent`, which is the only conversion a literal has that
  // may change its representation. It is bounded by `tokora`'s sealed `IntoEquivalent`, so the
  // bytes survive the change — and that is what a literal needs, because a `Plain` one asserts
  // that its cooked value IS its borrowed source and a conversion free to rewrite the bytes would
  // forge that. `String` is not one of the representations tokora seals in, so `Bytes` is the
  // non-`Copy` carrier these three ride.
  #[cfg(feature = "bytes")]
  {
    use ::bytes::Bytes;

    let inline_lit = match LitStr::try_from(b"\"inline\"".as_slice()).unwrap() {
      LitStr::Inline(lit) => lit.into_equivalent::<Bytes>(),
      LitStr::Block(_) => panic!("inline spelling produced a block literal"),
    };
    let string = StringValue::<_, CustomSpan, dyn CustomLang>::new(
      CustomSpan(5),
      LitStr::Inline(inline_lit.clone()),
    );
    let source: &Bytes = string.source();
    assert_eq!(source, "\"inline\"");

    let inline = InlineStringValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(6), inline_lit);
    let source: &Bytes = inline.source();
    assert_eq!(source, "\"inline\"");

    let block_lit = match LitStr::try_from(b"\"\"\"block\"\"\"".as_slice()).unwrap() {
      LitStr::Block(lit) => lit.into_equivalent::<Bytes>(),
      LitStr::Inline(_) => panic!("block spelling produced an inline literal"),
    };
    let block = BlockStringValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(7), block_lit);
    let source: &Bytes = block.source();
    assert_eq!(source, "\"\"\"block\"\"\"");
  }
}
