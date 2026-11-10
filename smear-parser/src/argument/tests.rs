use tokora::{
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::{Argument, ArgumentList};
use crate::directive::{Directive, Directives};

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
  let argument = Argument::<_, _, CustomSpan>::new(CustomSpan(1), "name", 7_u8);
  assert_eq!(argument.as_span(), &CustomSpan(1));
  assert_eq!(argument.into_span(), CustomSpan(1));
  assert_eq!(argument.into_components(), (CustomSpan(1), "name", 7));

  let arguments =
    ArgumentList::<Argument<&str, u8, CustomSpan>, _, CustomSpan>::new(CustomSpan(2), [argument]);
  assert_eq!(arguments.as_span(), &CustomSpan(2));
  assert_eq!(arguments.into_span(), CustomSpan(2));
  let (span, [argument]) = arguments.into_components();
  assert_eq!(span, CustomSpan(2));
  assert_eq!(argument.span(), &CustomSpan(1));
  assert_eq!(argument.name(), &"name");
  assert_eq!(argument.value(), &7);

  let directive = Directive::<_, _, CustomSpan>::new(CustomSpan(3), "name", Some(7_u8));
  assert_eq!(directive.as_span(), &CustomSpan(3));
  assert_eq!(directive.into_span(), CustomSpan(3));
  assert_eq!(
    directive.into_components(),
    (CustomSpan(3), "name", Some(7))
  );

  let directives =
    Directives::<Directive<&str, u8, CustomSpan>, _, CustomSpan>::new(CustomSpan(4), [directive]);
  assert_eq!(directives.as_span(), &CustomSpan(4));
  assert_eq!(directives.into_span(), CustomSpan(4));
  let (span, [directive]) = directives.into_components();
  assert_eq!(span, CustomSpan(4));
  assert_eq!(directive.span(), &CustomSpan(3));
  assert_eq!(directive.name(), &"name");
  assert_eq!(directive.arguments(), Some(&7));
}

#[test]
fn collection_accessors_project_array_backed_containers_to_slices() {
  let arguments = ArgumentList::<u8, _, CustomSpan>::new(CustomSpan(1), ArrayBacked([1_u8, 2]));
  let projected: &[u8] = arguments.arguments();
  assert_eq!(projected, &[1, 2]);
  assert_eq!(arguments.into_arguments().0, [1, 2]);

  let directives = Directives::<u8, _, CustomSpan>::new(CustomSpan(2), ArrayBacked([3_u8, 4]));
  let projected: &[u8] = directives.directives();
  assert_eq!(projected, &[3, 4]);
  assert_eq!(directives.into_directives().0, [3, 4]);
}
