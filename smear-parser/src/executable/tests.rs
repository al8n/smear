use tokora::{
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::{
  Described, Document, FragmentDefinition, NamedOperationDefinition, VariableDefinition,
  VariablesDefinition,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CustomSpan(u8);

struct ArrayBacked<T, const N: usize>([T; N]);

impl<T, const N: usize> AsRef<[T]> for ArrayBacked<T, N> {
  fn as_ref(&self) -> &[T] {
    &self.0
  }
}

#[test]
fn carriers_support_custom_spans() {
  let variable = VariableDefinition::<_, _, _, _, CustomSpan>::new(
    CustomSpan(1),
    "$id",
    "ID!",
    Some("= 1"),
    Some("@skip"),
  );
  assert_eq!(variable.as_span(), &CustomSpan(1));
  assert_eq!(
    variable.into_components(),
    (CustomSpan(1), "$id", "ID!", Some("= 1"), Some("@skip"))
  );

  let described = Described::<_, _, CustomSpan>::new(CustomSpan(2), Some("desc"), "node");
  assert_eq!(described.description(), Some(&"desc"));
  assert_eq!(described.into_span(), CustomSpan(2));

  let variables = VariablesDefinition::<&str, CustomSpan, _>::new(CustomSpan(3), ["$id"]);
  assert_eq!(variables.variable_definitions(), &["$id"]);

  let fragment = FragmentDefinition::<_, _, _, _, CustomSpan>::new(
    CustomSpan(4),
    "Part",
    "on User",
    None::<&str>,
    "{ id }",
  );
  assert_eq!(fragment.selection_set(), &"{ id }");

  let named = NamedOperationDefinition::<_, _, _, _, _, CustomSpan>::new(
    CustomSpan(5),
    "query",
    Some("Q"),
    None::<&str>,
    Some("@cache"),
    "{ id }",
  );
  assert_eq!(named.name(), Some(&"Q"));

  let document = Document::<&str, CustomSpan, _>::new(CustomSpan(6), ["query { id }"]);
  assert_eq!(document.as_span(), &CustomSpan(6));
  assert_eq!(
    document.into_components(),
    (CustomSpan(6), ["query { id }"])
  );
}

#[test]
fn collection_accessors_project_array_backed_containers_to_slices() {
  let variables =
    VariablesDefinition::<u8, CustomSpan, _>::new(CustomSpan(1), ArrayBacked([1_u8, 2]));
  let projected: &[u8] = variables.variable_definitions();
  assert_eq!(projected, &[1, 2]);
  assert_eq!(variables.into_variable_definitions().0, [1, 2]);

  let document = Document::<u8, CustomSpan, _>::new(CustomSpan(2), ArrayBacked([3_u8, 4]));
  let projected: &[u8] = document.definitions();
  assert_eq!(projected, &[3, 4]);
  assert_eq!(document.into_definitions().0, [3, 4]);
}
