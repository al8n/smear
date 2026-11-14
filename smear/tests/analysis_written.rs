//! The two accessors smear's own rules read and a consumer's rule may: who wrote what is written at
//! a location, `SourceMap::written_by`, and the arguments written on a type occurrence of any
//! shape, `TypeExpr::written_args`.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, Layout, Limits, MemberId, Namespace, Package, PackageId, SourceFile, SourceId, Tier,
    TypeExprId, TypeExprKind, analyze,
  },
  diagnostic::Location,
  lexer::tokora::SimpleSpan,
};

/// A package named `name`, `Flat`, at tier `Std`, holding `files`, `(path, text)`.
fn package(name: &str, files: &[(&'static str, &'static str)]) -> Package<&'static str> {
  Package::new(name, Layout::Flat, Tier::Std).with_files(
    files
      .iter()
      .map(|&(path, text): &(&'static str, &'static str)| SourceFile::new(path, text)),
  )
}

/// The type occurrence the field `field` of `owner`, a canonical name, writes as its type.
fn field_type(analysis: &Analysis, owner: &str, field: &str) -> TypeExprId {
  let model = analysis.model();
  let owner = model
    .lookup(Namespace::Type, owner)
    .unwrap_or_else(|| panic!("no type `{owner}`"));
  let member = model
    .def(owner)
    .fields()
    .into_iter()
    .find(|id: &MemberId| model.member(*id).name() == field)
    .unwrap_or_else(|| panic!("no field `{field}`"));
  model.member(member).ty().expect("a field has a type")
}

/// The package of a source answers for every location in it, a dependency's and the root's, and a
/// source id at or past the map's sources, which is a consumer's own, answers none. An answer read
/// through `SourceMap::source` would panic on that id.
#[test]
fn written_by_answers_the_package_of_the_source() {
  let root = package(
    "app",
    &[("schema.graphqlx", "type Query { place: ^::geo::Point }")],
  )
  .with_dependencies(["geo"]);
  let geo = package("geo", &[("geo.graphqlx", "type Point { x: Int }")]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let model = analysis.model();
  let sources = analysis.sources();

  let point = model
    .lookup(Namespace::Type, "^::geo::Point")
    .expect("geo's `Point`");
  let query = model
    .lookup(Namespace::Type, "^::app::Query")
    .expect("the root's `Query`");
  assert_eq!(
    sources.written_by(model.def(point).location()),
    Some(PackageId::new(4))
  );
  assert_eq!(
    sources.written_by(model.def(query).location()),
    Some(PackageId::new(3))
  );

  // Every source of the map answers its own package, the built-in packages' included.
  for id in sources.sources() {
    assert_eq!(
      sources.written_by(Location::entire(id.get())),
      Some(sources.source(id).package()),
      "source {}",
      id.get()
    );
  }

  // A consumer's own source ids: none of the map's, so no package, and no panic.
  let past: SourceId = SourceId::new(sources.len());
  assert_eq!(
    sources.written_by(Location::new(past.get(), SimpleSpan::new(0, 1))),
    None
  );
  assert_eq!(sources.written_by(Location::entire(u32::MAX)), None);
}

/// The arguments written on an occurrence, whatever it resolved to: a named occurrence's are its
/// kind's; a type parameter written with one, and a path that did not resolve, keep theirs, which
/// their kinds do not hold; and a list has none, its element being its kind's.
#[test]
fn written_args_answers_the_arguments_of_every_shape() {
  let root = package(
    "app",
    &[(
      "schema.graphqlx",
      "type Page<X> { x: X }\ntype G<T> { a: Page<Int>  b: T<Int>  c: Missing<Int>  d: [Int] }\n",
    )],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  let model = analysis.model();

  let a = model.type_expr(field_type(&analysis, "^::app::G", "a"));
  let TypeExprKind::Named { args, .. } = a.kind() else {
    panic!("`a` names `Page`: {:?}", a.kind());
  };
  assert_eq!(a.written_args().len(), 1);
  assert_eq!(a.written_args(), args);

  let b = model.type_expr(field_type(&analysis, "^::app::G", "b"));
  assert!(
    matches!(b.kind(), TypeExprKind::Param { .. }),
    "`b` names `T`: {:?}",
    b.kind()
  );
  assert_eq!(b.written_args().len(), 1);

  let c = model.type_expr(field_type(&analysis, "^::app::G", "c"));
  assert_eq!(c.kind(), TypeExprKind::Unresolved);
  assert_eq!(c.written_args().len(), 1);

  let d = model.type_expr(field_type(&analysis, "^::app::G", "d"));
  assert!(d.written_args().is_empty(), "{:?}", d.written_args());
  let TypeExprKind::List { element, .. } = d.kind() else {
    panic!("`d` is a list: {:?}", d.kind());
  };
  assert!(
    matches!(model.type_expr(element).kind(), TypeExprKind::Named { .. }),
    "{:?}",
    model.type_expr(element).kind()
  );
}
