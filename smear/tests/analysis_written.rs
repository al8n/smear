//! The accessors smear's own rules read and a consumer's rule may: who wrote what is written at a
//! location, `SourceMap::written_by`; the arguments written on a type occurrence of any shape,
//! `TypeExpr::written_args`; an application's type and its complete type arguments,
//! `Application::ty` and `Application::ty_args`; and the application an extern is bound from,
//! `Extern::application`.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, ApplicationId, Extern, Layout, Limits, MemberId, Namespace, Package, PackageId,
    SourceFile, SourceId, Tier, TyKind, TypeExprId, TypeExprKind, analyze,
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

/// The one application written on the type `owner`, a canonical name.
fn only_application(analysis: &Analysis, owner: &str) -> ApplicationId {
  let model = analysis.model();
  let owner = model
    .lookup(Namespace::Type, owner)
    .unwrap_or_else(|| panic!("no type `{owner}`"));
  match model.def(owner).applications() {
    [only] => *only,
    other => panic!("{} applications: {other:?}", other.len()),
  }
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

/// An application's type is its directive's instantiation, and its complete type arguments are
/// that type's: the written `Int`, then the default `[A]` filled with it. A directive of no type
/// parameter, a count the directive does not take and a directive that does not resolve give no
/// type and no type argument. Refuses the written type arguments alone, and a type built for a
/// refused count.
#[test]
fn an_applications_type_and_type_arguments_are_its_instantiation() {
  let text = "directive @tag<A, B = [A]> on OBJECT\ndirective @plain on OBJECT\n\
              type Query @tag<Int> { x: Int }\ntype Other @plain { y: Int }\n\
              type Third @tag<Int, Int, Int> { z: Int }\ntype Fourth @nope { w: Int }\n";
  let root = package("app", &[("schema.graphqlx", text)]);
  let analysis = analyze(&root, &[], &Limits::new());
  let model = analysis.model();
  let tag = model
    .lookup(Namespace::Directive, "@^::app::tag")
    .expect("the directive `@tag`");

  let tagged = model.application(only_application(&analysis, "^::app::Query"));
  let args = tagged.ty_args();
  assert_eq!(args.len(), 2, "{args:?}");
  let written = model.type_expr(tagged.type_args()[0]).ty();
  assert_eq!(args[0], written);
  let TyKind::List { element, .. } = model.ty(args[1]).kind() else {
    panic!(
      "the default `[A]` fills a list: {:?}",
      model.ty(args[1]).kind()
    );
  };
  assert_eq!(element, written);
  let ty = tagged.ty().expect("`@tag<Int>` has a type");
  let TyKind::Named { def, args: of, .. } = model.ty(ty).kind() else {
    panic!("its type is a named type: {:?}", model.ty(ty).kind());
  };
  assert_eq!(def, tag);
  assert_eq!(of, args);

  for owner in ["^::app::Other", "^::app::Third", "^::app::Fourth"] {
    let applied = model.application(only_application(&analysis, owner));
    assert_eq!(applied.ty(), None, "{owner}");
    assert!(
      applied.ty_args().is_empty(),
      "{owner}: {:?}",
      applied.ty_args()
    );
  }
}

/// Each extern answers the `@extern` it is bound from, the definition's and then the extension's:
/// one application answered for every extern of a scalar fails here.
#[test]
fn an_extern_answers_the_application_it_is_bound_from() {
  let text = "scalar Money @extern(lang: \"rust\", type: \"rust_decimal::Decimal\")\n\
              extend scalar Money @extern(lang: \"typescript\", type: \"string\")\n\
              type Query { m: Money }\n";
  let root = package("app", &[("schema.graphqlx", text)]);
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  let money = model.def(
    model
      .lookup(Namespace::Type, "^::app::Money")
      .expect("the scalar `Money`"),
  );
  let applications: Vec<Option<ApplicationId>> = money
    .applications()
    .iter()
    .map(|id: &ApplicationId| Some(*id))
    .collect();
  assert_eq!(applications.len(), 2, "{applications:?}");
  let bound: Vec<Option<ApplicationId>> = money
    .externs()
    .map(|binding: Extern<'_>| binding.application())
    .collect();
  assert_eq!(bound, applications);
}
