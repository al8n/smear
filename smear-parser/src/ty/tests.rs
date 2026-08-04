use std::vec;

use tokora::{
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::ListType;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CustomSpan(u8);

#[test]
fn carriers_support_custom_spans() {
  let list = ListType::<_, CustomSpan>::new(CustomSpan(2), "Element", false);
  assert_eq!(list.as_span(), &CustomSpan(2));
  assert_eq!(
    ListType::<_, CustomSpan>::new(CustomSpan(2), "Element", false).into_span(),
    CustomSpan(2)
  );
  assert_eq!(list.into_components(), (CustomSpan(2), "Element", false));
}

#[cfg(feature = "graphql")]
#[test]
fn named_type_carrier_supports_custom_spans() {
  use super::NamedType;

  let named = NamedType::<_, CustomSpan>::new(CustomSpan(1), "Name", true);
  assert_eq!(named.as_span(), &CustomSpan(1));
  assert_eq!(
    NamedType::<_, CustomSpan>::new(CustomSpan(1), "Name", true).into_span(),
    CustomSpan(1)
  );
  assert_eq!(named.into_components(), (CustomSpan(1), "Name", true));
}

#[cfg(feature = "graphqlx")]
#[test]
fn graphqlx_carriers_support_custom_spans() {
  use crate::path::Path;

  use super::{DefinitionTypePath, MapType, SetType, TypeGenerics};

  let set = SetType::<_, CustomSpan>::new(CustomSpan(3), "Element", true);
  assert_eq!(set.as_span(), &CustomSpan(3));
  assert_eq!(set.ty(), &"Element");
  assert!(set.required());
  assert_eq!(set.into_components(), (CustomSpan(3), "Element", true));

  let map = MapType::<_, _, CustomSpan>::new(CustomSpan(4), "Key", "Value", false);
  assert_eq!(map.as_span(), &CustomSpan(4));
  assert_eq!(map.key(), &"Key");
  assert_eq!(map.value(), &"Value");
  assert!(!map.required());
  assert_eq!(
    map.into_components(),
    (CustomSpan(4), "Key", "Value", false)
  );

  let generics = TypeGenerics::<_, CustomSpan>::new(CustomSpan(5), vec!["Argument"]);
  assert_eq!(generics.as_span(), &CustomSpan(5));
  assert_eq!(generics.params(), &["Argument"]);
  assert_eq!(
    generics.into_components(),
    (CustomSpan(5), vec!["Argument"])
  );

  let definition = DefinitionTypePath::<_, _, CustomSpan>::new(
    CustomSpan(6),
    Path::new(CustomSpan(7), vec!["Namespace", "Item"], true),
    Some(TypeGenerics::<_, CustomSpan>::new(
      CustomSpan(8),
      vec!["Argument"],
    )),
    true,
  );
  assert_eq!(definition.as_span(), &CustomSpan(6));
  assert!(definition.path().is_fully_qualified());
  assert_eq!(definition.path().segments(), &["Namespace", "Item"]);
  assert_eq!(definition.type_generics().unwrap().params(), &["Argument"]);
  assert!(definition.required());
}
