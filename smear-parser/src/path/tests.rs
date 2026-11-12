use core::num::NonZeroUsize;
use std::{string::ToString, vec};

use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  types::Ident,
  utils::IntoComponents,
};

use super::{Path, PathRoot};

#[test]
fn path_projects_and_consumes_segments() {
  let root = PathRoot::Dependency(SimpleSpan::new(0, 3));
  let path = Path::new(SimpleSpan::new(0, 11), root, vec!["one", "two"]);
  assert_eq!(path.segments(), &["one", "two"]);
  assert_eq!(path.root(), &root);
  assert_eq!(path.to_string(), "^::one::two");
  assert_eq!(path.as_span(), &SimpleSpan::new(0, 11));
  assert_eq!(path.clone().into_span(), SimpleSpan::new(0, 11));
  assert_eq!(
    path.clone().into_components(),
    (SimpleSpan::new(0, 11), root, vec!["one", "two"])
  );
  assert_eq!(path.into_segments(), vec!["one", "two"]);
}

#[test]
fn one_name_path_inherits_the_name_span() {
  let name = Ident::<_, _, ()>::new(SimpleSpan::new(4, 8), "Item");
  let path: Path<_> = name.into();
  assert_eq!(path.segments().len(), 1);
  assert_eq!(path.span(), &SimpleSpan::new(4, 8));
  assert!(path.root().is_relative());
  assert_eq!(path.root().span(), None);
}

#[test]
fn every_root_displays_as_it_is_spelled() {
  let span = SimpleSpan::new(0, 3);
  let twice = NonZeroUsize::new(2).expect("two is not zero");
  for (root, spelled) in [
    (PathRoot::Relative, "a::b"),
    (PathRoot::Package(span), "~::a::b"),
    (PathRoot::Dependency(span), "^::a::b"),
    (PathRoot::Parent { span, count: twice }, "..::..::a::b"),
  ] {
    let path = Path::new(span, root, vec!["a", "b"]);
    assert_eq!(path.to_string(), spelled, "{root:?}");
    assert_eq!(path.root().span(), root.span());
    assert_eq!(root.is_relative(), root == PathRoot::Relative);
  }
}
