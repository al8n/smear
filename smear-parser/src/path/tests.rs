use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  types::Ident,
  utils::IntoComponents,
};

use super::Path;

#[test]
fn path_projects_and_consumes_segments() {
  let path = Path::new(SimpleSpan::new(0, 7), vec!["one", "two"], true);
  assert_eq!(path.segments(), &["one", "two"]);
  assert!(path.is_fully_qualified());
  assert_eq!(path.to_string(), "::one::two");
  assert_eq!(path.as_span(), &SimpleSpan::new(0, 7));
  assert_eq!(path.clone().into_span(), SimpleSpan::new(0, 7));
  assert_eq!(
    path.clone().into_components(),
    (SimpleSpan::new(0, 7), true, vec!["one", "two"])
  );
  assert_eq!(path.into_segments(), vec!["one", "two"]);
}

#[test]
fn one_name_path_inherits_the_name_span() {
  let name = Ident::<_, _, ()>::new(SimpleSpan::new(4, 8), "Item");
  let path: Path<_> = name.into();
  assert_eq!(path.segments().len(), 1);
  assert_eq!(path.span(), &SimpleSpan::new(4, 8));
  assert!(!path.is_fully_qualified());
}
