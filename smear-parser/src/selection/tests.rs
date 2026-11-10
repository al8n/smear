use tokora::{
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::{Alias, Field, FragmentSpread, InlineFragment, SelectionSet, TypeCondition};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CustomSpan(u8);

/// A span of the test's own, taking the obligation in the one line a consumer would write. Every
/// selection carrier owns its span by value and releases it where it finds it, so the span
/// answers the same question the value and type carriers' does (`al8n/smear#176`).
impl crate::value::Leaf for CustomSpan {}

struct ArrayBacked<T, const N: usize>([T; N]);

impl<T, const N: usize> AsRef<[T]> for ArrayBacked<T, N> {
  fn as_ref(&self) -> &[T] {
    &self.0
  }
}

#[test]
fn carriers_support_custom_spans() {
  let alias = Alias::<_, CustomSpan>::new(CustomSpan(1), "alias");
  assert_eq!(alias.as_span(), &CustomSpan(1));
  assert_eq!(alias.into_components(), (CustomSpan(1), "alias"));

  let condition = TypeCondition::<_, CustomSpan>::new(CustomSpan(2), "User");
  assert_eq!(condition.into_span(), CustomSpan(2));

  let set = SelectionSet::<&str, CustomSpan, _>::new(CustomSpan(3), ["field"]);
  assert_eq!(set.selections(), &["field"]);

  let spread = FragmentSpread::<_, _, CustomSpan>::new(CustomSpan(4), "part", Some("@skip"));
  assert_eq!(spread.directives(), Some(&"@skip"));

  let inline = InlineFragment::<_, _, _, CustomSpan>::new(
    CustomSpan(5),
    Some("on User"),
    None::<&str>,
    "{ field }",
  );
  assert_eq!(inline.type_condition(), Some(&"on User"));

  let field = Field::<_, _, _, _, _, CustomSpan>::new(
    CustomSpan(6),
    Some("alias"),
    "field",
    Some("(arg: 1)"),
    None::<&str>,
    Some("{ child }"),
  );
  assert_eq!(field.name(), &"field");
  assert_eq!(field.into_span(), CustomSpan(6));
}

#[test]
fn selections_project_array_backed_containers_to_slices() {
  let selections = SelectionSet::<u8, CustomSpan, _>::new(CustomSpan(1), ArrayBacked([1_u8, 2]));
  let projected: &[u8] = selections.selections();
  assert_eq!(projected, &[1, 2]);
  assert_eq!(selections.into_selections().0, [1, 2]);
}
