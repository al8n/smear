//! The GraphQLx analyzer's tiers: what each definition and each type requires, the least of
//! `core`, `alloc` and `std` that holds everything it uses.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::analysis::graphqlx::{
  Analysis, DefId, IntegerProvider, Layout, Limits, MemberId, Model, Namespace, Package,
  PackageKind, SourceFile, Tier, TypeExprId, analyze, test_support,
};

/// A package named `name`, `Flat`, at `tier`, holding `files`, `(path, text)`.
fn package<'a>(name: &'a str, tier: Tier, files: &[(&'a str, &'a str)]) -> Package<&'a str> {
  Package::new(name, Layout::Flat, tier).with_files(
    files
      .iter()
      .map(|&(path, text): &(&'a str, &'a str)| SourceFile::new(path, text)),
  )
}

/// An analysis of the root package `app`, `Flat`, at tier `Std`, holding the one file
/// `schema.graphqlx`.
fn analyze_text(text: &str) -> Analysis {
  analyze(
    &package("app", Tier::Std, &[("schema.graphqlx", text)]),
    &[],
    &Limits::new(),
  )
}

/// Asserts that `analysis` says nothing.
fn quiet(analysis: &Analysis) {
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

/// The definition the canonical name `name` names: a directive when it begins with `@`.
fn named(model: &Model, name: &str) -> DefId {
  let namespace = if name.starts_with('@') {
    Namespace::Directive
  } else {
    Namespace::Type
  };
  model
    .lookup(namespace, name)
    .unwrap_or_else(|| panic!("no definition `{name}`"))
}

/// The tier of the definition the canonical name `name` names.
fn tier_of(model: &Model, name: &str) -> Tier {
  model.def(named(model, name)).tier()
}

/// The field `field` of the definition the canonical name `owner` names.
fn field(model: &Model, owner: &str, field: &str) -> MemberId {
  model
    .def(named(model, owner))
    .fields()
    .into_iter()
    .find(|id: &MemberId| model.member(*id).name() == field)
    .unwrap_or_else(|| panic!("no field `{owner}.{field}`"))
}

/// The tier of the type of the field `name` of the definition the canonical name `owner` names.
fn field_tier(model: &Model, owner: &str, name: &str) -> Tier {
  let written: TypeExprId = model
    .member(field(model, owner, name))
    .ty()
    .unwrap_or_else(|| panic!("`{owner}.{name}` has no type"));
  model.ty(model.type_expr(written).ty()).tier()
}

/// Every definition of the built-in library requires its package's tier, whatever it holds: the
/// meta-types `core`'s, and a GraphQL built-in the root package replaces its built-in's. Refuses a
/// built-in's tier computed.
#[test]
fn the_library_keeps_its_tags() {
  let analysis = analyze_text("type Query {\n  x: Int\n  s: String\n}\nscalar String\n");
  quiet(&analysis);
  let model = analysis.model();
  for id in model.defs() {
    let definition = model.def(id);
    let package = model.package(model.module(definition.module()).package());
    if package.kind() == PackageKind::BuiltIn {
      assert_eq!(definition.tier(), package.tier(), "{}", definition.name());
    }
  }
  assert_eq!(model.def(model.builtins().string()).tier(), Tier::Alloc);
  assert_eq!(tier_of(model, "^::core::__Schema"), Tier::Core);
  assert_eq!(tier_of(model, "^::std::HashMap"), Tier::Std);
}

/// A type requires its definition's tier and its arguments': `string` `alloc`, a list `alloc` at
/// least, `HashMap` `std`, `NonZeroU32` and a provided `i256` `core`, and `Box<string>` `alloc`
/// though `Box` is `core`; a type and its non-null form alike. Refuses a list counted `core`, and a
/// type read off its definition alone.
#[test]
fn a_types_tier_counts_its_definition_and_its_arguments() {
  let text = "type Query {\n  a: string\n  b: [i32]\n  c: [[i32]]!\n  d: HashMap<string, i32>\n  \
              e: NonZeroU32\n  f: i256\n  g: Box<string>\n  h: string!\n}\nscalar Box<T>\n";
  let root =
    package("app", Tier::Std, &[("schema.graphqlx", text)]).with_integers(IntegerProvider::numo());
  let analysis = analyze(&root, &[], &Limits::new());
  quiet(&analysis);
  let model = analysis.model();
  let tiers: Vec<Tier> = ["a", "b", "c", "d", "e", "f", "g", "h"]
    .into_iter()
    .map(|name: &str| field_tier(model, "^::app::Query", name))
    .collect();
  assert_eq!(
    tiers,
    [
      Tier::Alloc,
      Tier::Alloc,
      Tier::Alloc,
      Tier::Std,
      Tier::Core,
      Tier::Core,
      Tier::Alloc,
      Tier::Alloc,
    ]
  );
  assert_eq!(tier_of(model, "^::app::Box"), Tier::Core);
}

/// A definition requires the greatest tier of what it holds: an object of its fields' types and
/// their arguments', an input object of its input fields', a union of its members', an interface
/// of its fields'; an enum and a directive `core`, a directive's arguments counting nowhere.
/// Refuses a field's argument left out, and a directive counted.
#[test]
fn a_definitions_tier_is_the_greatest_of_what_it_holds() {
  let analysis = analyze_text(
    "type Query {\n  x: Int\n}\ntype A {\n  s: string\n}\ntype B {\n  f(x: [Int]): Int\n}\n\
     input C {\n  h: HashSet<Int>\n}\ntype D {\n  x: Int\n}\nunion U = A | D\n\
     interface Named {\n  name: string\n}\nenum E {\n  V\n}\n\
     directive @dir(a: string!, b: [Int]) on OBJECT\n",
  );
  quiet(&analysis);
  let model = analysis.model();
  let tiers: Vec<Tier> = [
    "^::app::A",
    "^::app::B",
    "^::app::C",
    "^::app::U",
    "^::app::Named",
    "^::app::E",
    "@^::app::dir",
    "^::app::D",
  ]
  .into_iter()
  .map(|name: &str| tier_of(model, name))
  .collect();
  assert_eq!(
    tiers,
    [
      Tier::Alloc,
      Tier::Alloc,
      Tier::Std,
      Tier::Alloc,
      Tier::Alloc,
      Tier::Core,
      Tier::Core,
      Tier::Core,
    ]
  );
}

/// A type parameter counts as `core`: `Page<T>` is `core`, and `Page<string>` `alloc`. Refuses a
/// definition read off one of its instantiations.
#[test]
fn a_type_parameter_counts_as_core() {
  let analysis =
    analyze_text("type Page<T> {\n  items: T\n}\ntype Query {\n  p: Page<string>\n}\n");
  quiet(&analysis);
  let model = analysis.model();
  assert_eq!(tier_of(model, "^::app::Page"), Tier::Core);
  assert_eq!(field_tier(model, "^::app::Query", "p"), Tier::Alloc);
}

/// A field, a field's argument or an input field that applies `@heap` requires `alloc`, and so
/// does its definition; a directive's argument counts nowhere. Refuses `@heap` read off the type,
/// and a directive's argument counted.
#[test]
fn heap_raises_its_position_to_alloc() {
  let analysis = analyze_text(
    "type Query {\n  x: Int\n}\ntype Q {\n  n: i64 @heap\n}\ninput I {\n  n: i64 @heap\n}\n\
     type R {\n  f(n: i64 @heap): Int\n}\ndirective @d(n: i64 @heap) on OBJECT\n",
  );
  quiet(&analysis);
  let model = analysis.model();
  assert_eq!(tier_of(model, "^::app::Q"), Tier::Alloc);
  assert_eq!(tier_of(model, "^::app::I"), Tier::Alloc);
  assert_eq!(tier_of(model, "^::app::R"), Tier::Alloc);
  assert_eq!(tier_of(model, "@^::app::d"), Tier::Core);
  assert_eq!(field_tier(model, "^::app::Q", "n"), Tier::Core);
}

/// A scalar a supplied package defines requires the greatest of its externs' tiers, an extern
/// with no `tier` its package's: `ALLOC` and none in a `std` package, `std`; two of `CORE`,
/// `core`; none in a `core` package, `core`. A custom scalar and a collection with no extern are
/// `core`. Refuses the first extern's tier taken alone.
#[test]
fn an_extern_scalar_takes_its_externs_greatest_tier() {
  let ours = "type Query {\n  x: Int\n}\n\
              scalar A @extern(lang: \"rust\", type: \"a\", tier: ALLOC) \
              @extern(lang: \"typescript\", type: \"a\")\n\
              scalar B @extern(lang: \"rust\", type: \"b\", tier: CORE) \
              @extern(lang: \"typescript\", type: \"b\", tier: CORE)\n\
              scalar C\nscalar Bag<T> @set\n";
  let theirs = "scalar D @extern(lang: \"rust\", type: \"d\")\n";
  let root = package("app", Tier::Std, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let geo = package("geo", Tier::Core, &[("geo.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  quiet(&analysis);
  let model = analysis.model();
  assert_eq!(tier_of(model, "^::app::A"), Tier::Std);
  assert_eq!(tier_of(model, "^::app::B"), Tier::Core);
  assert_eq!(tier_of(model, "^::app::C"), Tier::Core);
  assert_eq!(tier_of(model, "^::app::Bag"), Tier::Core);
  assert_eq!(tier_of(model, "^::geo::D"), Tier::Core);
}

/// Definitions that hold one another share the greatest tier any of them reaches: `A` and `B`,
/// through `B`'s `string`; a union and the object that holds it; and two objects through a type
/// argument. One that holds only itself stays `core`. Refuses a cycle cut, which leaves `A` at
/// `core`.
#[test]
fn a_cycle_shares_its_greatest_tier() {
  let analysis = analyze_text(
    "type Query {\n  x: Int\n}\ntype A {\n  b: B\n}\ntype B {\n  a: A\n  s: string\n}\n\
     type N {\n  next: N\n}\nunion U = V | W\ntype V {\n  u: U\n}\ntype W {\n  s: string\n}\n\
     type P {\n  q: Box<Q>\n}\ntype Q {\n  p: P\n  s: string\n}\nscalar Box<T>\n",
  );
  quiet(&analysis);
  let model = analysis.model();
  for name in ["A", "B", "U", "V", "W", "P", "Q"] {
    assert_eq!(
      tier_of(model, &format!("^::app::{name}")),
      Tier::Alloc,
      "{name}"
    );
  }
  assert_eq!(tier_of(model, "^::app::N"), Tier::Core);
  assert_eq!(tier_of(model, "^::app::Box"), Tier::Core);
}

/// One hundred objects, each the type of the field of the one before it, the last holding a
/// `HashMap`: every one requires `std`. Written in id order, each names a later one, so one pass
/// over the definitions in id order would leave the first ninety-nine at `core`. Refuses one pass
/// in id order.
#[test]
fn a_long_chain_rises_to_its_end() {
  let mut text = String::from("type Query {\n  x: Int\n}\n");
  for index in 0..99 {
    text.push_str(&format!("type T{index} {{\n  next: T{}\n}}\n", index + 1));
  }
  text.push_str("type T99 {\n  m: HashMap<i32, i32>\n}\n");
  let analysis = analyze_text(&text);
  quiet(&analysis);
  let model = analysis.model();
  for index in 0..100 {
    assert_eq!(
      tier_of(model, &format!("^::app::T{index}")),
      Tier::Std,
      "T{index}"
    );
  }
}

/// Every definition's tier and every occurrence's type's tier, one a line, in id order.
fn rendered(analysis: &Analysis) -> String {
  let model = analysis.model();
  let mut text = String::new();
  for id in model.defs() {
    let definition = model.def(id);
    text.push_str(&format!("{} {:?}\n", definition.name(), definition.tier()));
  }
  let (_, occurrences, _, _) = test_support::counts(model);
  for index in 0..occurrences {
    let ty = model.type_expr(TypeExprId::new(index)).ty();
    text.push_str(&format!("{index} {:?}\n", model.ty(ty).tier()));
  }
  text
}

/// The tiers are the same whichever order the files and the dependencies are passed in.
#[test]
fn reversing_files_and_dependencies_changes_no_tier() {
  let a = "type Query {\n  x: ^::geo::Point\n  y: Pair\n}\ntype Pair {\n  b: Other\n}\n";
  let b = "type Other {\n  p: Pair\n  s: [string]\n}\n";
  let geo = package(
    "geo",
    Tier::Std,
    &[("geo.graphqlx", "type Point {\n  m: HashSet<i32>\n}\n")],
  );
  let far = package(
    "far",
    Tier::Std,
    &[("far.graphqlx", "type Far {\n  x: Int\n}\n")],
  );
  let forward = package("app", Tier::Std, &[("a.graphqlx", a), ("b.graphqlx", b)])
    .with_dependencies(["geo", "far"]);
  let backward = package("app", Tier::Std, &[("b.graphqlx", b), ("a.graphqlx", a)])
    .with_dependencies(["far", "geo"]);
  let one = analyze(&forward, &[geo.clone(), far.clone()], &Limits::new());
  let two = analyze(&backward, &[far, geo], &Limits::new());
  quiet(&one);
  // Not vacuous: `Pair` and `Other` hold one another, and `Point` holds a `HashSet`.
  let model = one.model();
  assert_eq!(tier_of(model, "^::app::Pair"), Tier::Alloc);
  assert_eq!(tier_of(model, "^::geo::Point"), Tier::Std);
  assert_eq!(tier_of(model, "^::app::Query"), Tier::Std);
  assert_eq!(rendered(&one), rendered(&two));
}
