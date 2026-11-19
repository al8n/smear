//! The GraphQLx analyzer's tiers: what each definition and each type requires, the least of
//! `core`, `alloc` and `std` that holds everything it uses, and which definitions only
//! compile-time metadata reaches, which the tier rule does not judge.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, DefId, Diagnostic, IntegerProvider, Layout, Limits, MemberId, Model, Namespace,
    Origin, Package, PackageId, PackageKind, SourceFile, SourceId, Tier, TypeExprId, analyze,
    test_support,
  },
  diagnostic::{Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

/// The code for a use above the tier its package declares.
const TIER: &str = "smear::graphqlx::tier";

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

/// An analysis of the root package `app`, `Flat`, at `tier`, holding the one file
/// `schema.graphqlx`.
fn analyze_at(text: &str, tier: Tier) -> Analysis {
  analyze(
    &package("app", tier, &[("schema.graphqlx", text)]),
    &[],
    &Limits::new(),
  )
}

/// The id of the file at `path` in the package `package`.
fn file(analysis: &Analysis, package: u32, path: &str) -> SourceId {
  let sources = analysis.sources();
  analysis
    .model()
    .package(PackageId::new(package))
    .sources()
    .into_iter()
    .find(|id: &SourceId| sources.source(*id).path() == path)
    .unwrap_or_else(|| panic!("no file `{path}` in package {package}"))
}

/// Where the `nth` occurrence, counting from 0, of `needle` is in `text`, the file `source`.
fn at(source: SourceId, text: &str, needle: &str, nth: usize) -> Location {
  let start = text
    .match_indices(needle)
    .nth(nth)
    .map(|(start, _): (usize, &str)| start)
    .unwrap_or_else(|| panic!("`{needle}` does not occur {} times", nth + 1));
  Location::new(source.get(), SimpleSpan::new(start, start + needle.len()))
}

/// Each diagnostic whose code is `code`: its primary location and its first label's location, in
/// the list's order.
fn found(analysis: &Analysis, code: &str) -> Vec<(Location, Option<Location>)> {
  analysis
    .diagnostics()
    .iter()
    .filter(|diagnostic: &&Diagnostic| diagnostic.code().as_str() == code)
    .map(|diagnostic: &Diagnostic| {
      (
        diagnostic.primary(),
        diagnostic.label(0).map(|label| label.location()),
      )
    })
    .collect()
}

/// The message of each diagnostic whose code is `code`, in the list's order.
fn messages(analysis: &Analysis, code: &str) -> Vec<String> {
  analysis
    .diagnostics()
    .iter()
    .filter(|diagnostic: &&Diagnostic| diagnostic.code().as_str() == code)
    .map(|diagnostic: &Diagnostic| diagnostic.to_string())
    .collect()
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
  let test_support::Counts {
    type_exprs: occurrences,
    ..
  } = test_support::counts(model);
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

/// What a list says, for a package declared at `core`.
const LIST: &str = "a list needs `alloc`, and package `app` is declared at tier `core`";

/// A name above the tier its package declares is refused where it is written: at `core`,
/// `String`, `bytes` and `ID` need `alloc` and `HashMap` `std`; at `alloc`, `HashMap` alone; at
/// `std`, nothing. Refuses resolution answering at every tier with nothing said, as it still
/// answers.
#[test]
fn a_name_above_the_packages_tier_is_refused_where_it_is_written() {
  let text =
    "type Query {\n  x: Int\n  s: String\n  t: bytes\n  u: ID\n  h: HashMap<i32, i32>\n}\n";
  let analysis = analyze_at(text, Tier::Core);
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    found(&analysis, TIER),
    [
      (at(schema, text, "String", 0), None),
      (at(schema, text, "bytes", 0), None),
      (at(schema, text, "ID", 0), None),
      (at(schema, text, "HashMap<i32, i32>", 0), None),
    ]
  );
  assert_eq!(
    messages(&analysis, TIER),
    [
      "`string` needs `alloc`, and package `app` is declared at tier `core`",
      "`bytes` needs `alloc`, and package `app` is declared at tier `core`",
      "`id` needs `alloc`, and package `app` is declared at tier `core`",
      "`HashMap` needs `std`, and package `app` is declared at tier `core`",
    ]
  );
  let analysis = analyze_at(text, Tier::Alloc);
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    found(&analysis, TIER),
    [(at(schema, text, "HashMap<i32, i32>", 0), None)]
  );
  assert_eq!(
    messages(&analysis, TIER),
    ["`HashMap` needs `std`, and package `app` is declared at tier `alloc`"]
  );
  quiet(&analyze_at(text, Tier::Std));
}

/// Every spelling of one name is refused once, where it is written: `HashMap`, `std::HashMap`,
/// `^::std::HashMap`, and a second name a `use` gives it. A `use` nothing uses says nothing.
/// Refuses a check by spelling.
#[test]
fn every_spelling_of_a_name_is_one_refusal() {
  let text = "use std::HashMap as M\nuse std::HashSet\ntype Query {\n  x: Int\n  \
              a: HashMap<i32, i32>\n  b: std::HashMap<i32, i32>\n  c: ^::std::HashMap<i32, i32>\n  \
              d: M<i32, i32>\n}\n";
  let analysis = analyze_at(text, Tier::Core);
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    found(&analysis, TIER),
    [
      (at(schema, text, "HashMap<i32, i32>", 0), None),
      (at(schema, text, "std::HashMap<i32, i32>", 0), None),
      (at(schema, text, "^::std::HashMap<i32, i32>", 0), None),
      (at(schema, text, "M<i32, i32>", 0), None),
    ]
  );
}

/// A list needs `alloc`, each level of it, and what it holds is judged as well: `[Int]` once,
/// `[[Int]]` twice, `[String]` twice. Refuses a list judged by its element alone.
#[test]
fn a_list_needs_alloc() {
  let text = "type Query {\n  x: Int\n  a: [Int]\n  b: [[Int]]\n  c: [String]\n}\n";
  let analysis = analyze_at(text, Tier::Core);
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    found(&analysis, TIER),
    [
      (at(schema, text, "[Int]", 0), None),
      (at(schema, text, "[[Int]]", 0), None),
      (at(schema, text, "[Int]", 1), None),
      (at(schema, text, "[String]", 0), None),
      (at(schema, text, "String", 0), None),
    ]
  );
  assert_eq!(
    messages(&analysis, TIER),
    [
      LIST,
      LIST,
      LIST,
      LIST,
      "`string` needs `alloc`, and package `app` is declared at tier `core`",
    ]
  );
}

/// A definition of the package's own is reported at what raises it, and not at its uses: one
/// report, at `string`. Refuses a cause reported at every use.
#[test]
fn a_packages_own_definition_is_reported_at_its_cause_alone() {
  let text = "type Query {\n  x: Int\n  t: T\n  u: T\n}\ntype T {\n  s: string\n}\n";
  let analysis = analyze_at(text, Tier::Core);
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    found(&analysis, TIER),
    [(at(schema, text, "string", 0), None)]
  );
}

/// Another package's definition that needs more than the root package declares is refused at each
/// use; the dependency, declared at `alloc`, says nothing of its own. Refuses a dependency's
/// definition taken for the root package's own.
#[test]
fn another_packages_definition_is_judged_at_each_use() {
  let theirs = "type X {\n  s: string\n}\n";
  let ours = "type Query {\n  x: Int\n  a: ^::dep::X\n  b: ^::dep::X\n}\n";
  let root = package("app", Tier::Core, &[("schema.graphqlx", ours)]).with_dependencies(["dep"]);
  let dep = package("dep", Tier::Alloc, &[("dep.graphqlx", theirs)]);
  let analysis = analyze(&root, &[dep], &Limits::new());
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    found(&analysis, TIER),
    [
      (at(schema, ours, "^::dep::X", 0), None),
      (at(schema, ours, "^::dep::X", 1), None),
    ]
  );
  assert_eq!(
    messages(&analysis, TIER),
    ["`X` needs `alloc`, and package `app` is declared at tier `core`"; 2]
  );
}

/// A dependency's own mistake is reported once, in the dependency: what a definition needs past
/// its package's tier is not asked of its uses. Refuses the need taken uncapped.
#[test]
fn a_dependencys_own_mistake_is_reported_once() {
  let theirs = "type X {\n  s: string\n}\n";
  let ours = "type Query {\n  x: Int\n  a: ^::dep::X\n  b: ^::dep::X\n}\n";
  let root = package("app", Tier::Core, &[("schema.graphqlx", ours)]).with_dependencies(["dep"]);
  let dep = package("dep", Tier::Core, &[("dep.graphqlx", theirs)]);
  let analysis = analyze(&root, &[dep], &Limits::new());
  let dep_file = file(&analysis, 4, "dep.graphqlx");
  assert_eq!(
    found(&analysis, TIER),
    [(at(dep_file, theirs, "string", 0), None)]
  );
  assert_eq!(
    messages(&analysis, TIER),
    ["`string` needs `alloc`, and package `dep` is declared at tier `core`"]
  );
}

/// What a use of another package's definition needs is capped at that package's tier, and the
/// message says the capped need: `X` holds a `HashMap`, which needs `std`, in a dependency declared
/// at `alloc`, which reports its `HashMap`; the root package, at `core`, reports its use of `X` as
/// needing `alloc`. Refuses the uncapped `std` in the root package's message.
#[test]
fn a_use_names_the_need_capped_at_the_definitions_package() {
  let theirs = "type X {\n  h: HashMap<i32, i32>\n}\n";
  let ours = "type Query {\n  x: Int\n  a: ^::dep::X\n}\n";
  let root = package("app", Tier::Core, &[("schema.graphqlx", ours)]).with_dependencies(["dep"]);
  let dep = package("dep", Tier::Alloc, &[("dep.graphqlx", theirs)]);
  let analysis = analyze(&root, &[dep], &Limits::new());
  let schema = file(&analysis, 3, "schema.graphqlx");
  let dep_file = file(&analysis, 4, "dep.graphqlx");
  assert_eq!(
    found(&analysis, TIER),
    [
      (at(schema, ours, "^::dep::X", 0), None),
      (at(dep_file, theirs, "HashMap<i32, i32>", 0), None),
    ]
  );
  assert_eq!(
    messages(&analysis, TIER),
    [
      "`X` needs `alloc`, and package `app` is declared at tier `core`",
      "`HashMap` needs `std`, and package `dep` is declared at tier `alloc`",
    ]
  );
}

/// An `@extern` that writes `tier` twice is judged by the first, as binding reads it: at `core`,
/// `tier: ALLOC, tier: CORE` is refused at `ALLOC`. Refuses the later `tier` read.
#[test]
fn an_externs_first_tier_is_the_one_judged() {
  let text = "type Query {\n  x: Int\n}\n\
              scalar S @extern(lang: \"rust\", type: \"a\", tier: ALLOC, tier: CORE)\n";
  let analysis = analyze_at(text, Tier::Core);
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    found(&analysis, TIER),
    [(at(schema, text, "ALLOC", 0), None)]
  );
  assert_eq!(
    messages(&analysis, TIER),
    [
      "this `@extern` says its host type needs `alloc`, and package `app`, which declares the \
       scalar, is declared at tier `core`"
    ]
  );
}

/// A use that leaves a type argument to a default needs what the default needs: `^::dep::P` is
/// refused, with a label at its default `string`, and `^::dep::P<Int>` is not. Refuses the filled
/// slots ignored, and a default reported as the root package's own writing.
#[test]
fn a_slot_filled_from_a_default_is_counted_at_the_use() {
  let theirs = "type P<T = string> {\n  v: T\n}\n";
  let ours = "type Query {\n  x: Int\n  p: ^::dep::P\n  q: ^::dep::P<Int>\n}\n";
  let root = package("app", Tier::Core, &[("schema.graphqlx", ours)]).with_dependencies(["dep"]);
  let dep = package("dep", Tier::Std, &[("dep.graphqlx", theirs)]);
  let analysis = analyze(&root, &[dep], &Limits::new());
  let schema = file(&analysis, 3, "schema.graphqlx");
  let dep_file = file(&analysis, 4, "dep.graphqlx");
  assert_eq!(
    found(&analysis, TIER),
    [(
      at(schema, ours, "^::dep::P", 0),
      Some(at(dep_file, theirs, "string", 0))
    )]
  );
  assert_eq!(
    messages(&analysis, TIER),
    ["`P` needs `alloc`, and package `app` is declared at tier `core`"]
  );
}

/// An application's type arguments count nowhere, the written ones and those its directive's
/// default fills: another package's `@^::dep::d<string>` and its bare `@^::dep::d`, whose `T` the
/// default `string` fills, say nothing in a root at `core`, as they say nothing in one at `alloc`.
/// Refuses an application's type arguments judged, whether written or left to a default.
#[test]
fn an_applications_type_arguments_are_not_judged_written_or_filled() {
  let theirs = "directive @d<T = string>(value: T = \"x\") on SCALAR\n";
  let omitted = "type Query {\n  x: Int\n}\nscalar S @^::dep::d\n";
  let written = "type Query {\n  x: Int\n}\nscalar S @^::dep::d<string>\n";
  let dep = || package("dep", Tier::Alloc, &[("dep.graphqlx", theirs)]);

  for tier in [Tier::Core, Tier::Alloc] {
    for text in [omitted, written] {
      let root = package("app", tier, &[("schema.graphqlx", text)]).with_dependencies(["dep"]);
      quiet(&analyze(&root, &[dep()], &Limits::new()));
    }
  }
}

/// A slot filled from a default needs what the default needs of its own, a parameter it names
/// counting as `core`, and never the argument the slot ends up holding: `^::dep::P2<string>` is
/// refused once, at its written `string`, and `^::dep::P3` once, labelled at `T`'s default.
/// Refuses the substituted argument read as the default's need, which reports `P2` too.
#[test]
fn a_default_that_names_a_parameter_adds_nothing_of_its_own() {
  let theirs = "type P2<T, U = T> {\n  v: U\n}\ntype P3<T = string, U = T> {\n  v: U\n}\n";
  let ours = "type Query {\n  x: Int\n  p: ^::dep::P2<string>\n  q: ^::dep::P2<Int>\n  \
              r: ^::dep::P3\n}\n";
  let root = package("app", Tier::Core, &[("schema.graphqlx", ours)]).with_dependencies(["dep"]);
  let dep = package("dep", Tier::Std, &[("dep.graphqlx", theirs)]);
  let analysis = analyze(&root, &[dep], &Limits::new());
  let schema = file(&analysis, 3, "schema.graphqlx");
  let dep_file = file(&analysis, 4, "dep.graphqlx");
  assert_eq!(
    found(&analysis, TIER),
    [
      (at(schema, ours, "string", 0), None),
      (
        at(schema, ours, "^::dep::P3", 0),
        Some(at(dep_file, theirs, "string", 0))
      ),
    ]
  );
}

/// `@heap` in a `core` package is refused at its path, on a field, a field's argument and an input
/// field; on a directive definition's argument it raises nothing, and says nothing; and in an
/// `alloc` package nothing is said.
#[test]
fn heap_in_a_core_package_is_refused() {
  let text = "type Query {\n  x: Int\n  a: i64 @heap\n  b(n: i64 @heap): Int\n}\n\
              input I {\n  c: i64 @heap\n}\ndirective @d(n: i64 @heap) on OBJECT\n";
  let analysis = analyze_at(text, Tier::Core);
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    found(&analysis, TIER),
    [
      (at(schema, text, "heap", 0), None),
      (at(schema, text, "heap", 1), None),
      (at(schema, text, "heap", 2), None),
    ]
  );
  assert_eq!(
    messages(&analysis, TIER),
    ["`@heap` keeps this value on the heap, which needs `alloc`, and package `app` is declared \
       at tier `core`"; 3]
  );
  quiet(&analyze_at(text, Tier::Alloc));
}

/// An extern whose `tier` is above the tier of the package that declares its scalar is refused at
/// that `tier`; one within it, a `core` scalar used by its own package, and a `std` package's
/// `CORE` say nothing.
#[test]
fn an_extern_tier_above_its_package_is_refused_at_its_tier() {
  let theirs = "scalar S @extern(lang: \"rust\", type: \"s\", tier: ALLOC)\n\
                scalar C @extern(lang: \"rust\", type: \"c\", tier: CORE)\n\
                type Uses {\n  s: S\n  c: C\n}\n";
  let ours =
    "type Query {\n  x: Int\n}\nscalar R @extern(lang: \"rust\", type: \"r\", tier: CORE)\n";
  let root = package("app", Tier::Std, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let geo = package("geo", Tier::Core, &[("geo.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  assert_eq!(
    found(&analysis, TIER),
    [(at(geo_file, theirs, "ALLOC", 0), None)]
  );
  assert_eq!(
    messages(&analysis, TIER),
    [
      "this `@extern` says its host type needs `alloc`, and package `geo`, which declares the \
       scalar, is declared at tier `core`"
    ]
  );
}

/// What counts nowhere is not judged. At `core`, a directive definition writes `string!`, a list
/// and the list's `string!`, a type parameter's default, and a bound that names a dependency's
/// interface needing `alloc`; two applications write a type argument, one of them a list; and
/// `@heap` stands on a directive's argument: nothing is said. Refuses compile-time metadata judged.
#[test]
fn what_counts_nowhere_is_not_judged() {
  let theirs = "interface Named {\n  s: string\n}\n";
  let ours = "type Query {\n  x: Int\n}\n\
              directive @doc(text: string!, tags: [string!]) on FIELD_DEFINITION\n\
              directive @g<T = string> on OBJECT\n\
              type Q @g<String> {\n  y: Int @doc(text: \"x\")\n}\n\
              type R @g<[string]> {\n  z: Int\n}\n\
              directive @h(n: i64 @heap) on OBJECT\n\
              directive @b<T> on OBJECT where T: ^::dep::Named\n";
  let root = package("app", Tier::Core, &[("schema.graphqlx", ours)]).with_dependencies(["dep"]);
  let dep = package("dep", Tier::Alloc, &[("dep.graphqlx", theirs)]);
  quiet(&analyze(&root, &[dep], &Limits::new()));
}

/// What a generic type writes is judged, as what a generic directive writes is not: at `core`, a
/// type parameter's default `string`, and a bound that names a dependency's interface needing
/// `alloc`, are each refused where they are written. Refuses the exemption read off any generic
/// definition.
#[test]
fn a_generic_types_defaults_and_bounds_are_judged() {
  let theirs = "interface Named {\n  s: string\n}\n";
  let ours = "type Query {\n  x: Int\n}\ntype P<T = string> {\n  v: T\n}\n\
              type B<T> where T: ^::dep::Named {\n  v: T\n}\n";
  let root = package("app", Tier::Core, &[("schema.graphqlx", ours)]).with_dependencies(["dep"]);
  let dep = package("dep", Tier::Alloc, &[("dep.graphqlx", theirs)]);
  let analysis = analyze(&root, &[dep], &Limits::new());
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    found(&analysis, TIER),
    [
      (at(schema, ours, "string", 0), None),
      (at(schema, ours, "^::dep::Named", 0), None),
    ]
  );
}

/// The rule says nothing within what a phase refused: the resolver's diagnostic alone.
#[test]
fn the_tier_rule_says_nothing_where_a_phase_spoke() {
  let analysis = analyze_at("type Query {\n  x: Strin\n  s: String\n}\n", Tier::Core);
  let codes: Vec<&str> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.code().as_str())
    .collect();
  assert_eq!(codes, ["smear::schema::undefined-type"]);
}

/// In an analysis that is ok, every definition's tier is at most its package's, but a metadata
/// definition's: at `core`, primitives, `NonZero`, an enum and an extern of `CORE`; at `alloc`,
/// strings and lists; at `std`, a `HashMap`; the meta-types and the library's definitions their
/// tags; and at `core`, `Binding`, which only a directive's argument takes, is metadata and needs
/// `alloc`.
#[test]
fn every_definition_is_within_its_packages_tier_in_an_ok_analysis() {
  for (text, tier) in [
    (
      "type Query {\n  x: Int\n  n: NonZeroU32\n  e: E\n  m: M\n}\nenum E {\n  A\n}\n\
       scalar M @extern(lang: \"rust\", type: \"m\", tier: CORE)\n",
      Tier::Core,
    ),
    (
      "type Query {\n  x: Int\n  s: String\n  l: [Int]\n}\n",
      Tier::Alloc,
    ),
    ("type Query {\n  h: HashMap<i32, i32>\n}\n", Tier::Std),
    (BINDING, Tier::Core),
  ] {
    let analysis = analyze_at(text, tier);
    quiet(&analysis);
    let model = analysis.model();
    for id in model.defs() {
      let definition = model.def(id);
      let package = model.package(model.module(definition.module()).package());
      assert!(
        definition.is_metadata() || definition.tier() <= package.tier(),
        "{text}: {}",
        definition.name()
      );
    }
  }
  let analysis = analyze_at(BINDING, Tier::Core);
  let model = analysis.model();
  assert!(metadata(model, "^::app::Binding"));
  assert_eq!(tier_of(model, "^::app::Binding"), Tier::Alloc);
}

/// A message quotes a definition's name and a `lang` only as excerpts.
#[test]
fn long_names_are_quoted_as_excerpts() {
  let long = "E".repeat(4096);
  let theirs = format!("type {long} {{\n  s: string\n}}\n");
  let ours = format!("type Query {{\n  x: Int\n  y: ^::geo::{long}\n}}\n");
  let root =
    package("app", Tier::Core, &[("schema.graphqlx", ours.as_str())]).with_dependencies(["geo"]);
  let geo = package("geo", Tier::Alloc, &[("geo.graphqlx", theirs.as_str())]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let written = messages(&analysis, TIER);
  assert_eq!(written.len(), 1, "{written:#?}");
  for message in written {
    assert!(message.len() < 1024 && message.contains('…'), "{message}");
  }

  let lang = "l".repeat(4096);
  let text = format!(
    "type Query {{\n  x: Int\n}}\n\
     scalar M @extern(lang: \"{lang}\", type: \"a\") @extern(lang: \"{lang}\", type: \"b\")\n"
  );
  let analysis = analyze_at(&text, Tier::Std);
  let written = messages(&analysis, "smear::graphqlx::extern-misuse");
  assert_eq!(written.len(), 1, "{written:#?}");
  for message in written {
    assert!(message.len() < 1024 && message.contains('…'), "{message}");
  }
}

// What only compile-time metadata reaches: a scalar, an enum or an input object that only the
// types of a directive no operation can apply and applications' type arguments name, which the
// rule does not judge, and what it judges once a type outside metadata names it too.

/// What a `string` says, for a package declared at `core`.
const STRING: &str = "`string` needs `alloc`, and package `app` is declared at tier `core`";

/// What `@heap` says, for a package declared at `core`.
const HEAP: &str = "`@heap` keeps this value on the heap, which needs `alloc`, and package `app` \
                    is declared at tier `core`";

/// An input object only a directive's argument takes, and a scalar the directive is applied to.
const BINDING: &str = "type Query {\n  x: Int\n}\n\
                       input Binding {\n  lang: string!\n  path: string!\n}\n\
                       directive @bind(with: [Binding!]!) on SCALAR\n\
                       scalar Point @bind(with: [{ lang: \"rust\", path: \"geo::Point\" }])\n";

/// What a directive's argument reaches, transitively: an input object, an enum, a scalar with an
/// extern, a map, and the input object the map holds.
const TRANSITIVE: &str = "type Query {\n  x: Int\n}\n\
                          input Outer {\n  inner: Inner\n  kind: Kind\n  url: Url\n  \
                          tags: Dict<i32, Tag>\n}\n\
                          input Inner {\n  notes: [string!]\n  big: i64 @heap\n}\n\
                          enum Kind {\n  A\n  B\n}\n\
                          scalar Url @extern(lang: \"rust\", type: \"url::Url\", tier: ALLOC)\n\
                          scalar Dict<K, V> @map\ninput Tag {\n  name: string!\n}\n\
                          directive @meta(o: Outer) on OBJECT\n";

/// Two input objects that hold each other and one that holds itself, each only a directive's
/// argument takes.
const CYCLE: &str = "type Query {\n  x: Int\n}\ninput A {\n  b: B\n  note: string\n}\n\
                     input B {\n  a: A\n}\ndirective @d(a: A) on OBJECT\n\
                     input C {\n  c: C\n  s: string\n}\ndirective @e(c: C) on OBJECT\n";

/// Whether the definition the canonical name `name` names is compile-time metadata.
fn metadata(model: &Model, name: &str) -> bool {
  model.def(named(model, name)).is_metadata()
}

/// Each definition's name and whether it is compile-time metadata, in id order.
fn flags(analysis: &Analysis) -> Vec<(String, bool)> {
  let model = analysis.model();
  model
    .defs()
    .into_iter()
    .map(|id: DefId| {
      let definition = model.def(id);
      (definition.name().to_owned(), definition.is_metadata())
    })
    .collect()
}

/// Where `needle` is in the first occurrence of `around` in `text`, the file `source`.
fn within(source: SourceId, text: &str, around: &str, needle: &str) -> Location {
  let start = text
    .find(around)
    .unwrap_or_else(|| panic!("`{around}` does not occur"));
  let offset = around
    .find(needle)
    .unwrap_or_else(|| panic!("`{needle}` is not in `{around}`"));
  let start = start + offset;
  Location::new(source.get(), SimpleSpan::new(start, start + needle.len()))
}

/// Each label of each diagnostic whose code is `code`, where it is and what it says, in the list's
/// order.
fn labels(analysis: &Analysis, code: &str) -> Vec<Vec<(Location, String)>> {
  analysis
    .diagnostics()
    .iter()
    .filter(|diagnostic: &&Diagnostic| diagnostic.code().as_str() == code)
    .map(|diagnostic: &Diagnostic| {
      (0..diagnostic.labels())
        .filter_map(|index: usize| diagnostic.label(index))
        .map(|label| (label.location(), String::from(label.text())))
        .collect()
    })
    .collect()
}

/// The label of a report within `name`, a definition compile-time metadata reaches, at where a
/// type outside metadata first reaches it.
fn entry(name: &str) -> String {
  format!("`{name}` is reached from here, outside compile-time metadata")
}

/// The code of each diagnostic, in the list's order.
fn codes(analysis: &Analysis) -> Vec<&str> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.code().as_str())
    .collect()
}

/// What only a directive's argument reaches is compile-time metadata, and nothing within it is
/// judged: at `core`, `Binding` holds `string!`s and needs `alloc`, and nothing is said, whether
/// the directive takes it in a list or bare. A `TypeRef` value that names it, in an `@repr` on the
/// schema, leaves it metadata. `Point`, the scalar the directive is applied to, `Query` and the
/// directive are not metadata. Refuses the shallow exemption, which judges `Binding`.
#[test]
fn a_definition_only_directive_arguments_reach_is_metadata() {
  let repr = format!("{BINDING}extend schema @repr(for: Binding, lang: \"rust\", type: \"x\")\n");
  let bare = "type Query {\n  x: Int\n}\ninput Binding {\n  lang: string!\n}\n\
              directive @bind(b: Binding) on SCALAR\n\
              scalar Point @bind(b: { lang: \"rust\" })\n";
  for text in [BINDING, repr.as_str(), bare] {
    let analysis = analyze_at(text, Tier::Core);
    quiet(&analysis);
    let model = analysis.model();
    assert!(metadata(model, "^::app::Binding"), "{text}");
    assert_eq!(tier_of(model, "^::app::Binding"), Tier::Alloc, "{text}");
    for name in ["^::app::Point", "^::app::Query", "@^::app::bind"] {
      assert!(!metadata(model, name), "{text}: {name}");
    }
  }
}

/// What a directive's argument reaches is metadata transitively, each input kind on the way: an
/// input object an input field holds, an enum, a scalar with an extern, a map, and the input object
/// the map holds. At `core` nothing is said: not the list and the `string!`s they hold, nor
/// `@heap`, nor the extern's `ALLOC`. Refuses a pass one level deep, and an extern's or `@heap`'s
/// report kept within metadata.
#[test]
fn what_directive_arguments_reach_transitively_is_metadata() {
  let analysis = analyze_at(TRANSITIVE, Tier::Core);
  quiet(&analysis);
  let model = analysis.model();
  for name in ["Outer", "Inner", "Kind", "Url", "Dict", "Tag"] {
    assert!(metadata(model, &format!("^::app::{name}")), "{name}");
  }
  // Not vacuous: what they hold needs `alloc`.
  for name in ["Outer", "Inner", "Url", "Tag"] {
    assert_eq!(
      tier_of(model, &format!("^::app::{name}")),
      Tier::Alloc,
      "{name}"
    );
  }
  assert!(!metadata(model, "^::app::Query"));
}

/// A definition both reach is judged: `Binding`, which a field's argument takes too, is refused at
/// each `string!`, and is no metadata. Each report is labelled at where the reach from outside
/// metadata entered, `Binding` in that argument. A report that a default raises has that label
/// last, after its label at the default: `Binding` holds `^::dep::P`, whose default `string` makes
/// it need `alloc`. Refuses an exemption read off what metadata reaches alone, and the label
/// placed before the default's.
#[test]
fn a_definition_reached_both_ways_is_judged() {
  let text = BINDING.replacen("  x: Int\n", "  x: Int\n  f(b: Binding): Int\n", 1);
  let analysis = analyze_at(&text, Tier::Core);
  let schema = file(&analysis, 3, "schema.graphqlx");
  let entered = within(schema, &text, "f(b: Binding)", "Binding");
  assert_eq!(
    found(&analysis, TIER),
    [
      (at(schema, &text, "string!", 0), Some(entered)),
      (at(schema, &text, "string!", 1), Some(entered)),
    ]
  );
  assert_eq!(
    labels(&analysis, TIER),
    vec![vec![(entered, entry("Binding"))]; 2]
  );
  assert_eq!(messages(&analysis, TIER), [STRING; 2]);
  assert_eq!(
    analysis.diagnostics().len(),
    2,
    "{:#?}",
    analysis.diagnostics()
  );
  assert!(!metadata(analysis.model(), "^::app::Binding"));

  let theirs = "input P<T = string> {\n  v: T\n}\n";
  let ours = "type Query {\n  x: Int\n  f(b: Binding): Int\n}\ninput Binding {\n  p: ^::dep::P\n}\n\
              directive @bind(b: Binding) on SCALAR\n";
  let root = package("app", Tier::Core, &[("schema.graphqlx", ours)]).with_dependencies(["dep"]);
  let dep = package("dep", Tier::Std, &[("dep.graphqlx", theirs)]);
  let analysis = analyze(&root, &[dep], &Limits::new());
  let schema = file(&analysis, 3, "schema.graphqlx");
  let dep_file = file(&analysis, 4, "dep.graphqlx");
  assert_eq!(
    found(&analysis, TIER),
    [(
      at(schema, ours, "^::dep::P", 0),
      Some(at(dep_file, theirs, "string", 0))
    )]
  );
  assert_eq!(
    labels(&analysis, TIER),
    [vec![
      (
        at(dep_file, theirs, "string", 0),
        String::from("filled from this default")
      ),
      (
        within(schema, ours, "f(b: Binding)", "Binding"),
        entry("Binding")
      ),
    ]]
  );
  assert_eq!(
    messages(&analysis, TIER),
    ["`P` needs `alloc`, and package `app` is declared at tier `core`"]
  );
}

/// A use through any type argument is a use, whatever a value of the type holds: `Binding` is
/// judged when a field's type `Ref<Binding>` names it, though a value of `Ref` holds none; when an
/// object's type parameter has the default `Binding`; when an object implements `I<Binding>`; when
/// an object's bound is `I<Binding>`; and when a field's argument takes `[Binding]`, in a package
/// at `alloc`, where its `HashMap` is refused. Each report is labelled at the use: the field's
/// type, the default, the `implements` entry, the bound's interface, and the argument's type.
/// Refuses the walk outside metadata following only what a value holds.
#[test]
fn a_use_through_any_type_argument_is_a_use() {
  let binding = "input Binding {\n  lang: string!\n  path: string!\n}\n\
                 directive @bind(with: [Binding!]!) on SCALAR\n";
  for (uses, around, used) in [
    (
      "type Query {\n  x: Int\n  r: Ref<Binding>\n}\nscalar Ref<T>\n",
      "r: Ref<Binding>",
      "Ref<Binding>",
    ),
    (
      "type Query {\n  x: Int\n}\ntype P<T = Binding> {\n  f(b: T): Int\n}\n",
      "T = Binding",
      "Binding",
    ),
    (
      "type Query {\n  x: Int\n}\ninterface I<X> {\n  x: Int\n}\n\
       type Impl implements I<Binding> {\n  x: Int\n}\n",
      "implements I<Binding>",
      "I<Binding>",
    ),
    (
      "type Query {\n  x: Int\n}\ninterface I<X> {\n  x: Int\n}\n\
       type B<T> where T: I<Binding> {\n  v: T\n}\n",
      "T: I<Binding>",
      "I<Binding>",
    ),
  ] {
    let text = format!("{uses}{binding}");
    let analysis = analyze_at(&text, Tier::Core);
    let schema = file(&analysis, 3, "schema.graphqlx");
    let entered = within(schema, &text, around, used);
    assert_eq!(
      found(&analysis, TIER),
      [
        (at(schema, &text, "string!", 0), Some(entered)),
        (at(schema, &text, "string!", 1), Some(entered)),
      ],
      "{text}"
    );
    assert_eq!(
      labels(&analysis, TIER),
      vec![vec![(entered, entry("Binding"))]; 2],
      "{text}"
    );
    assert_eq!(
      analysis.diagnostics().len(),
      2,
      "{text}: {:#?}",
      analysis.diagnostics()
    );
    assert!(!metadata(analysis.model(), "^::app::Binding"), "{text}");
  }

  let text = "type Query {\n  x: Int\n  f(b: [Binding]): Int\n}\n\
              input Binding {\n  m: HashMap<i32, i32>\n}\ndirective @bind(b: Binding) on SCALAR\n";
  let analysis = analyze_at(text, Tier::Alloc);
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    found(&analysis, TIER),
    [(
      at(schema, text, "HashMap<i32, i32>", 0),
      Some(at(schema, text, "[Binding]", 0))
    )]
  );
  assert_eq!(
    messages(&analysis, TIER),
    ["`HashMap` needs `std`, and package `app` is declared at tier `alloc`"]
  );
  assert!(!metadata(analysis.model(), "^::app::Binding"));
}

/// What metadata reaches is found over every package together. `geo`, at `core`, takes its
/// `Binding` in a directive's argument alone: under a root package at `alloc` that uses `geo`'s
/// `Point`, and alone, nothing is said. When the root package's field argument takes `Binding` too,
/// it is refused once, in `geo`, labelled at that use in the root package's file, and the root
/// package's use, capped at `geo`'s tier, says nothing; and so it is when a sibling dependency's
/// field argument takes it, labelled in the sibling's file. Refuses metadata found one package at
/// a time, which a dependant's use would escape under the cap.
#[test]
fn metadata_reaches_over_every_package() {
  const GEO: &str = "type Query {\n  ok: Int\n}\ninput Binding {\n  lang: string!\n}\n\
                     directive @bind(b: Binding) on SCALAR\n\
                     scalar Point @bind(b: { lang: \"rust\" })\n";
  const POINT: &str = "type Query {\n  p: ^::geo::Point\n}\n";
  const USES: &str = "type Query {\n  p: ^::geo::Point\n  f(b: ^::geo::Binding): Int\n}\n";
  const SIB: &str = "type Uses {\n  f(b: ^::geo::Binding): Int\n}\n";
  let geo = || package("geo", Tier::Core, &[("geo.graphqlx", GEO)]);
  let refused = "`string` needs `alloc`, and package `geo` is declared at tier `core`";

  let root = package("app", Tier::Alloc, &[("schema.graphqlx", POINT)]).with_dependencies(["geo"]);
  let analysis = analyze(&root, &[geo()], &Limits::new());
  quiet(&analysis);
  assert!(metadata(analysis.model(), "^::geo::Binding"));

  let root = package("app", Tier::Alloc, &[("schema.graphqlx", USES)]).with_dependencies(["geo"]);
  let analysis = analyze(&root, &[geo()], &Limits::new());
  let schema = file(&analysis, 3, "schema.graphqlx");
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  let entered = at(schema, USES, "^::geo::Binding", 0);
  assert_eq!(
    found(&analysis, TIER),
    [(at(geo_file, GEO, "string!", 0), Some(entered))]
  );
  assert_eq!(labels(&analysis, TIER), [vec![(entered, entry("Binding"))]]);
  assert_eq!(messages(&analysis, TIER), [refused]);
  assert_eq!(
    analysis.diagnostics().len(),
    1,
    "{:#?}",
    analysis.diagnostics()
  );
  assert!(!metadata(analysis.model(), "^::geo::Binding"));

  let analysis = analyze(&geo(), &[], &Limits::new());
  quiet(&analysis);
  assert!(metadata(analysis.model(), "^::geo::Binding"));

  let sibling = package("sib", Tier::Alloc, &[("sib.graphqlx", SIB)]).with_dependencies(["geo"]);
  let root =
    package("app", Tier::Alloc, &[("schema.graphqlx", POINT)]).with_dependencies(["geo", "sib"]);
  let analysis = analyze(&root, &[geo(), sibling], &Limits::new());
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  let sib_file = file(&analysis, 5, "sib.graphqlx");
  assert_eq!(
    found(&analysis, TIER),
    [(
      at(geo_file, GEO, "string!", 0),
      Some(at(sib_file, SIB, "^::geo::Binding", 0))
    )]
  );
  assert_eq!(messages(&analysis, TIER), [refused]);
  assert_eq!(
    analysis.diagnostics().len(),
    1,
    "{:#?}",
    analysis.diagnostics()
  );
  assert!(!metadata(analysis.model(), "^::geo::Binding"));
}

/// A cycle only metadata reaches is metadata, two input objects that hold each other and one that
/// holds itself, and nothing in them is said. When a field's argument takes `B`, `B` and then `A`,
/// which `B` holds, are judged, and `A`'s `string` is refused, labelled at `B` in that argument,
/// where the chain starts, and not at `B`'s field; `C` stays metadata. Refuses a cycle cut, a
/// cycle's runtime half missed, and a label at the occurrence that reached `A` last.
#[test]
fn a_cycle_only_metadata_reaches_is_metadata() {
  let analysis = analyze_at(CYCLE, Tier::Core);
  quiet(&analysis);
  let model = analysis.model();
  for name in ["A", "B", "C"] {
    assert!(metadata(model, &format!("^::app::{name}")), "{name}");
  }

  let text = CYCLE.replacen("  x: Int\n", "  x: Int\n  f(b: B): Int\n", 1);
  let analysis = analyze_at(&text, Tier::Core);
  let schema = file(&analysis, 3, "schema.graphqlx");
  let entered = within(schema, &text, "f(b: B)", "B");
  assert_eq!(
    found(&analysis, TIER),
    [(at(schema, &text, "string", 0), Some(entered))]
  );
  assert_eq!(labels(&analysis, TIER), [vec![(entered, entry("A"))]]);
  assert_eq!(messages(&analysis, TIER), [STRING]);
  let model = analysis.model();
  assert!(!metadata(model, "^::app::A"));
  assert!(!metadata(model, "^::app::B"));
  assert!(metadata(model, "^::app::C"));
}

/// Generic definitions and their instantiations, at `core`. (a) A directive's argument gives
/// `Note` to `Pair`, which a field's argument instantiates with `Int`: `Pair` is used outside
/// metadata and `Note` is metadata; with `Pair<Note>` as the argument's type, `Note` is judged.
/// (b) A generic directive's written type argument `Sample` is metadata. (c) A directive's default
/// type argument `Tags` is metadata, with no application. (d) An object's type parameter's default
/// `Filter` is a use, and judged. (e) `Outer<In>` names `In`, which only substitution gives `Box`:
/// judged, and metadata with no such field. (f) `Empty<In>` names `In`, though no value of `Empty`
/// holds one: metadata. Each report is labelled at the use outside metadata. Refuses an argument
/// missed by either walk, and a default read only where an application fills it.
#[test]
fn generics_and_instantiations() {
  let pair = "type Query {\n  x(p: Pair<Int>): Int \
              @note(p: { first: { text: \"a\" }, second: { text: \"b\" } })\n}\n\
              input Pair<T> {\n  first: T\n  second: T\n}\ninput Note {\n  text: string!\n}\n\
              directive @note(p: Pair<Note>) on FIELD_DEFINITION\n";
  let analysis = analyze_at(pair, Tier::Core);
  quiet(&analysis);
  assert!(metadata(analysis.model(), "^::app::Note"));
  assert!(!metadata(analysis.model(), "^::app::Pair"));
  let text = pair.replacen("x(p: Pair<Int>)", "x(p: Pair<Note>)", 1);
  let analysis = analyze_at(&text, Tier::Core);
  let schema = file(&analysis, 3, "schema.graphqlx");
  let entered = at(schema, &text, "Pair<Note>", 0);
  assert_eq!(
    found(&analysis, TIER),
    [(at(schema, &text, "string!", 0), Some(entered))]
  );
  assert_eq!(labels(&analysis, TIER), [vec![(entered, entry("Note"))]]);
  assert!(!metadata(analysis.model(), "^::app::Note"));

  let written = "type Query {\n  x: Int @doc<Sample>(example: { text: \"hi\" })\n}\n\
                 directive @doc<T>(example: T) on FIELD_DEFINITION\n\
                 input Sample {\n  text: string!\n}\n";
  let analysis = analyze_at(written, Tier::Core);
  quiet(&analysis);
  assert!(metadata(analysis.model(), "^::app::Sample"));

  let defaulted = "type Query {\n  x: Int\n}\ndirective @tagged<T = Tags>(t: T) on OBJECT\n\
                   input Tags {\n  names: [string!]!\n}\n";
  let analysis = analyze_at(defaulted, Tier::Core);
  quiet(&analysis);
  assert!(metadata(analysis.model(), "^::app::Tags"));

  let page = "type Query {\n  x: Int\n}\ntype Page<T = Filter> {\n  items(filter: T): Int\n}\n\
              input Filter {\n  q: string\n}\ndirective @f(x: Filter) on OBJECT\n";
  let analysis = analyze_at(page, Tier::Core);
  let schema = file(&analysis, 3, "schema.graphqlx");
  let entered = within(schema, page, "T = Filter", "Filter");
  assert_eq!(
    found(&analysis, TIER),
    [(at(schema, page, "string", 0), Some(entered))]
  );
  assert_eq!(labels(&analysis, TIER), [vec![(entered, entry("Filter"))]]);
  assert!(!metadata(analysis.model(), "^::app::Filter"));

  let outer = "type Query {\n  x: Int\n  o: Outer<In>\n}\ntype Outer<U> {\n  b(a: Box<U>): Int\n}\n\
               input Box<T> {\n  v: T\n}\ninput In {\n  s: string\n}\n\
               directive @d(i: In) on OBJECT\n";
  let analysis = analyze_at(outer, Tier::Core);
  let schema = file(&analysis, 3, "schema.graphqlx");
  let entered = at(schema, outer, "Outer<In>", 0);
  assert_eq!(
    found(&analysis, TIER),
    [(at(schema, outer, "string", 0), Some(entered))]
  );
  assert_eq!(labels(&analysis, TIER), [vec![(entered, entry("In"))]]);
  assert!(!metadata(analysis.model(), "^::app::In"));
  let unused = outer.replacen("  o: Outer<In>\n", "", 1);
  let analysis = analyze_at(&unused, Tier::Core);
  quiet(&analysis);
  assert!(metadata(analysis.model(), "^::app::In"));

  let empty = "type Query {\n  x: Int\n}\ninput Empty<T> {\n  n: Int\n}\n\
               input In {\n  s: string\n}\ndirective @e(x: Empty<In>) on OBJECT\n";
  let analysis = analyze_at(empty, Tier::Core);
  quiet(&analysis);
  assert!(metadata(analysis.model(), "^::app::In"));
  assert!(metadata(analysis.model(), "^::app::Empty"));
}

/// An object, an interface and a union are never metadata, whatever names them: `Shape` and
/// `Named`, which only a directive's argument names, through `Ref`, are judged, and the `string`
/// each holds is refused; `Either` is no metadata either, and `Ref` is. Refuses an output kind
/// marked.
#[test]
fn output_kinds_are_never_metadata() {
  let text = "type Query {\n  x: Int\n}\nscalar Ref<T>\ntype Shape {\n  label: string\n}\n\
              interface Named {\n  name: string\n}\nunion Either = Shape\n\
              directive @shape(r: Ref<Shape>, n: Ref<Named>, e: Ref<Either>) on OBJECT\n";
  let analysis = analyze_at(text, Tier::Core);
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    found(&analysis, TIER),
    [
      (at(schema, text, "string", 0), None),
      (at(schema, text, "string", 1), None),
    ]
  );
  assert_eq!(
    analysis.diagnostics().len(),
    2,
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  for name in ["Shape", "Named", "Either"] {
    assert!(!metadata(model, &format!("^::app::{name}")), "{name}");
  }
  assert!(metadata(model, "^::app::Ref"));
}

/// What nothing reaches is judged, as it was: `Unused`'s `string` is refused at `core`. A `TypeRef`
/// value that names it, in an `@repr` on the schema, reaches nothing, and changes nothing. Refuses
/// an unused definition taken for metadata, and a `TypeRef` value read as a reach.
#[test]
fn a_definition_nothing_reaches_is_judged() {
  let unused = "type Query {\n  x: Int\n}\ninput Unused {\n  s: string\n}\n";
  let repr = format!("{unused}extend schema @repr(for: Unused, lang: \"rust\", type: \"x\")\n");
  for text in [unused, repr.as_str()] {
    let analysis = analyze_at(text, Tier::Core);
    let schema = file(&analysis, 3, "schema.graphqlx");
    assert_eq!(
      found(&analysis, TIER),
      [(at(schema, text, "string", 0), None)],
      "{text}"
    );
    assert_eq!(
      analysis.diagnostics().len(),
      1,
      "{text}: {:#?}",
      analysis.diagnostics()
    );
    assert!(!metadata(analysis.model(), "^::app::Unused"), "{text}");
  }
}

/// The library is never metadata: what a directive's argument names of it, `HashMap<string, i32>`,
/// and the GraphQL built-in `String` the root package replaces, which only a directive's argument
/// names, are not, nor is anything of `core`, `alloc` or `std`; and nothing is said at `core`.
/// Refuses marking by kind alone.
#[test]
fn the_library_is_never_metadata() {
  let analysis = analyze_at(
    "type Query {\n  x: Int\n}\ndirective @bind(m: HashMap<string, i32>) on SCALAR\n\
     scalar String\ndirective @s(v: String) on OBJECT\n",
    Tier::Core,
  );
  quiet(&analysis);
  let model = analysis.model();
  assert_eq!(
    model.def(model.builtins().string()).origin(),
    Origin::BuiltIn
  );
  for id in model.defs() {
    let definition = model.def(id);
    if definition.origin() != Origin::User {
      assert!(!definition.is_metadata(), "{}", definition.name());
    }
  }
}

/// An occurrence a phase refused names nothing: `Binding<Int>`, a count `Binding` does not take, is
/// the phase's `generic-arity` alone, the rule silent in `Query`, and `Binding` stays metadata.
/// Refuses an untyped occurrence that panics the pass, or that reaches.
#[test]
fn an_occurrence_a_phase_refused_reaches_nothing() {
  let text = BINDING.replacen("  x: Int\n", "  x: Int\n  f(b: Binding<Int>): Int\n", 1);
  let analysis = analyze_at(&text, Tier::Core);
  assert_eq!(codes(&analysis), ["smear::graphqlx::generic-arity"]);
  assert!(metadata(analysis.model(), "^::app::Binding"));
}

/// Which definitions are metadata, and what the rule says, labels included, is the same whichever
/// order the files and the dependencies are passed in: a dependency's directive and the uses of
/// the root package and of a sibling; a cycle written over two files; and a dependant's directive
/// that takes a dependency's input object.
#[test]
fn the_metadata_flags_do_not_depend_on_order() {
  const GEO: &str = "input Binding {\n  lang: string!\n}\ndirective @bind(b: Binding) on SCALAR\n\
                     scalar Point @bind(b: { lang: \"rust\" })\ninput Cfg {\n  s: string\n}\n\
                     directive @cfg(c: Cfg) on OBJECT\n";
  const SIB: &str = "type Uses {\n  f(b: ^::geo::Binding): Int\n}\n";
  const QUERY: &str = "type Query {\n  p: ^::geo::Point\n}\n";
  const OTHER: &str = "type Other {\n  f(b: ^::geo::Binding): Int\n}\n";
  const ONE: &str = "type Query {\n  x: Int\n  f(b: B): Int\n}\n\
                     input A {\n  b: B\n  note: string\n}\n";
  const TWO: &str = "input B {\n  a: A\n}\ndirective @d(a: A) on OBJECT\n\
                     input C {\n  c: C\n  s: string\n}\ndirective @e(c: C) on OBJECT\n";
  const FAR: &str = "input Cfg {\n  s: string\n}\n";
  const TAKES: &str = "directive @rd(c: ^::far::Cfg) on OBJECT\n";
  let geo = || package("geo", Tier::Core, &[("geo.graphqlx", GEO)]);
  let sib = || package("sib", Tier::Alloc, &[("sib.graphqlx", SIB)]).with_dependencies(["geo"]);
  let far = || package("far", Tier::Core, &[("far.graphqlx", FAR)]);
  let pairs = [
    (
      analyze(
        &package(
          "app",
          Tier::Alloc,
          &[("a.graphqlx", QUERY), ("b.graphqlx", OTHER)],
        )
        .with_dependencies(["geo", "sib"]),
        &[geo(), sib()],
        &Limits::new(),
      ),
      analyze(
        &package(
          "app",
          Tier::Alloc,
          &[("b.graphqlx", OTHER), ("a.graphqlx", QUERY)],
        )
        .with_dependencies(["sib", "geo"]),
        &[sib(), geo()],
        &Limits::new(),
      ),
      1,
    ),
    (
      analyze_files(&[("a.graphqlx", ONE), ("b.graphqlx", TWO)]),
      analyze_files(&[("b.graphqlx", TWO), ("a.graphqlx", ONE)]),
      1,
    ),
    (
      analyze(
        &package(
          "app",
          Tier::Std,
          &[
            ("a.graphqlx", "type Query {\n  x: Int\n}\n"),
            ("b.graphqlx", TAKES),
          ],
        )
        .with_dependencies(["far", "geo"]),
        &[far(), geo()],
        &Limits::new(),
      ),
      analyze(
        &package(
          "app",
          Tier::Std,
          &[
            ("b.graphqlx", TAKES),
            ("a.graphqlx", "type Query {\n  x: Int\n}\n"),
          ],
        )
        .with_dependencies(["geo", "far"]),
        &[geo(), far()],
        &Limits::new(),
      ),
      0,
    ),
  ];
  for (forward, backward, reports) in pairs {
    // Not vacuous: each program marks some definition, and refuses what it is built to refuse.
    assert!(
      flags(&forward)
        .iter()
        .any(|&(_, flag): &(String, bool)| flag),
      "{:#?}",
      flags(&forward)
    );
    assert_eq!(
      found(&forward, TIER).len(),
      reports,
      "{:#?}",
      forward.diagnostics()
    );
    assert_eq!(flags(&forward), flags(&backward));
    assert_eq!(forward.diagnostics(), backward.diagnostics());
  }
}

/// An analysis of the root package `app`, `Flat`, at `core`, holding `files`, `(path, text)`.
fn analyze_files(files: &[(&str, &str)]) -> Analysis {
  analyze(&package("app", Tier::Core, files), &[], &Limits::new())
}

/// Two thousand input objects, each holding the next and the last a `string`, reached from one
/// directive's argument: every one is metadata, and nothing is said. With a field's argument taking
/// the thousandth, the rest of the chain is judged, its one `string` refused and labelled at that
/// argument, a thousand definitions away, and the first thousand stay metadata. Written forwards
/// and backwards, so that each names a later definition by id, or an earlier one. Refuses a walk
/// that stops short, and one pass in id order.
#[test]
fn a_long_chain_only_metadata_reaches() {
  const LENGTH: usize = 2_000;
  const USED: usize = LENGTH / 2;
  for forwards in [true, false] {
    for used in [false, true] {
      let mut text = String::from("type Query {\n  x: Int\n");
      if used {
        text.push_str(&format!("  f(a: A{USED}): Int\n"));
      }
      text.push_str("}\ndirective @d(a: A0) on OBJECT\n");
      let order: Vec<usize> = if forwards {
        (0..LENGTH).collect()
      } else {
        (0..LENGTH).rev().collect()
      };
      for index in order {
        if index + 1 < LENGTH {
          text.push_str(&format!("input A{index} {{\n  next: A{}\n}}\n", index + 1));
        } else {
          text.push_str(&format!("input A{index} {{\n  s: string\n}}\n"));
        }
      }
      let analysis = analyze_at(&text, Tier::Core);
      if used {
        let schema = file(&analysis, 3, "schema.graphqlx");
        let entered = within(
          schema,
          &text,
          &format!("f(a: A{USED})"),
          &format!("A{USED}"),
        );
        assert_eq!(
          found(&analysis, TIER),
          [(at(schema, &text, "string", 0), Some(entered))],
          "forwards {forwards}"
        );
        assert_eq!(
          labels(&analysis, TIER),
          [vec![(entered, entry(&format!("A{}", LENGTH - 1)))]],
          "forwards {forwards}"
        );
        assert_eq!(analysis.diagnostics().len(), 1, "forwards {forwards}");
      } else {
        quiet(&analysis);
      }
      let model = analysis.model();
      for index in 0..LENGTH {
        assert_eq!(
          metadata(model, &format!("^::app::A{index}")),
          !used || index < USED,
          "A{index}, forwards {forwards}, used {used}"
        );
      }
    }
  }
}

/// Two thousand directives, each taking one input object in its argument: it is metadata, and
/// nothing is said; with a field's argument taking it too, its `string` is refused once, labelled
/// at that argument. Each directive's type is the one type, pushed once however many directives
/// write it.
#[test]
fn many_directives_reaching_one_definition() {
  const DIRECTIVES: usize = 2_000;
  for used in [false, true] {
    let mut text = String::from("type Query {\n  x: Int\n");
    if used {
      text.push_str("  f(b: Binding): Int\n");
    }
    text.push_str("}\ninput Binding {\n  s: string\n}\n");
    for index in 0..DIRECTIVES {
      text.push_str(&format!("directive @d{index}(b: Binding) on OBJECT\n"));
    }
    let analysis = analyze_at(&text, Tier::Core);
    if used {
      let schema = file(&analysis, 3, "schema.graphqlx");
      assert_eq!(
        found(&analysis, TIER),
        [(
          at(schema, &text, "string", 0),
          Some(within(schema, &text, "f(b: Binding)", "Binding"))
        )]
      );
      assert_eq!(analysis.diagnostics().len(), 1);
    } else {
      quiet(&analysis);
    }
    assert_eq!(metadata(analysis.model(), "^::app::Binding"), !used);
  }
}

/// A scalar of a thousand type parameters, each with a default, named bare by three thousand fields
/// and by one directive's argument: every use fills the same thousand slots, and their one type is
/// walked once by each walk, the first push of each occurrence's type guarded as every other push
/// is. With the directive alone it is metadata, and with the fields it is not; nothing is said
/// either way. A test sees the verdicts, not the work, which holds by construction.
#[test]
fn a_wide_type_is_walked_once() {
  const WIDTH: usize = 1_000;
  const USES: usize = 3_000;
  for uses in [0, USES] {
    let mut text = String::from("type Query {\n  x: Int\n}\nscalar Big<T0 = Int");
    for index in 1..WIDTH {
      text.push_str(&format!(", T{index} = Int"));
    }
    text.push_str(">\ndirective @wide(b: Big) on OBJECT\n");
    if uses > 0 {
      text.push_str("type Uses {\n");
      for index in 0..uses {
        text.push_str(&format!("  f{index}: Big\n"));
      }
      text.push_str("}\n");
    }
    let analysis = analyze_at(&text, Tier::Core);
    quiet(&analysis);
    assert_eq!(
      metadata(analysis.model(), "^::app::Big"),
      uses == 0,
      "{uses} uses"
    );
  }
}

/// What the argument types of a directive an operation can apply name is used outside metadata, its
/// values arriving at run time: `Hint`, which `@cached` on `FIELD` takes, is judged, its list, its
/// `string!` and its `@heap` refused; and so it is when the directive is on `FIELD | OBJECT`, and
/// when a directive no operation can apply takes `Hint` too, each report then labelled at `Hint`
/// in `@cached`'s argument, where the reach from outside metadata entered. With no such directive
/// nothing else reaches `Hint`, and no report is labelled. The types such a directive writes are
/// exempt all the same: `@c(key: string!)` on `FIELD` says nothing. Refuses every directive's
/// arguments taken for metadata, and the exemption of what a directive writes lost.
#[test]
fn an_executable_directives_arguments_are_a_runtime_use() {
  let hint = "type Query {\n  x: Int\n}\ninput Hint {\n  tags: [string!]!\n  ttl: i64 @heap\n}\n";
  for (directives, labelled) in [
    ("directive @cached(hint: Hint) on FIELD\n", false),
    ("directive @cached(hint: Hint) on FIELD | OBJECT\n", false),
    (
      "directive @cached(hint: Hint) on FIELD\ndirective @meta(hint: Hint) on OBJECT\n",
      true,
    ),
  ] {
    let text = format!("{hint}{directives}");
    let analysis = analyze_at(&text, Tier::Core);
    let schema = file(&analysis, 3, "schema.graphqlx");
    let entered = labelled.then(|| within(schema, &text, "@cached(hint: Hint)", "Hint"));
    assert_eq!(
      found(&analysis, TIER),
      [
        (at(schema, &text, "[string!]!", 0), entered),
        (at(schema, &text, "string!", 0), entered),
        (at(schema, &text, "heap", 0), entered),
      ],
      "{text}"
    );
    let label: Vec<(Location, String)> = entered
      .map(|at: Location| (at, entry("Hint")))
      .into_iter()
      .collect();
    assert_eq!(labels(&analysis, TIER), vec![label; 3], "{text}");
    assert_eq!(messages(&analysis, TIER), [LIST, STRING, HEAP], "{text}");
    assert_eq!(
      analysis.diagnostics().len(),
      3,
      "{text}: {:#?}",
      analysis.diagnostics()
    );
    assert!(!metadata(analysis.model(), "^::app::Hint"), "{text}");
  }
  quiet(&analyze_at(
    "type Query {\n  x: Int\n}\ndirective @c(key: string!) on FIELD\n",
    Tier::Core,
  ));
}

/// What an executable directive's own type parameter can receive, where one of its arguments'
/// types holds it, is the operations' to choose at run time: `@run<T>(v: T)` on `FIELD` can be
/// applied as `@run<Payload>(v: $p)`, so `Payload`, which only `@meta`'s argument names, is judged,
/// its `[Int]` refused and labelled at `T` in `@run`'s argument. So it is when a bound names `T`,
/// which narrows nothing, and when the argument holds `T` in a list or as an argument of a named
/// type, labelled at that type. A generic executable directive that holds its parameter in no
/// argument's type, `T` unused, named only by a bound or only by another parameter's default,
/// enters nothing, and neither does a generic directive no operation can apply: `Payload` stays
/// metadata, and nothing is said. Refuses an executable directive's own parameter passed over, a
/// bound taken to narrow what it receives, and a parameter no argument holds taken for one an
/// operation fills.
#[test]
fn an_executable_directives_own_parameter_can_receive_any_metadata() {
  let payload = "type Query {\n  ok: Int\n}\ninput Payload {\n  items: [Int]\n}\n\
                 directive @meta(v: Payload) on OBJECT\n";
  for (run, around, holds) in [
    ("directive @run<T>(v: T) on FIELD\n", "(v: T)", "T"),
    (
      "directive @run<T>(v: T) on FIELD where T: MapKey\n",
      "(v: T)",
      "T",
    ),
    ("directive @run<T>(v: [T]) on FIELD\n", "(v: [T])", "[T]"),
    (
      "directive @run<T>(v: Box<T>) on FIELD\ninput Box<X> {\n  x: X\n}\n",
      "(v: Box<T>)",
      "Box<T>",
    ),
  ] {
    let text = format!("{payload}{run}");
    let analysis = analyze_at(&text, Tier::Core);
    let schema = file(&analysis, 3, "schema.graphqlx");
    let entered = within(schema, &text, around, holds);
    assert_eq!(
      found(&analysis, TIER),
      [(at(schema, &text, "[Int]", 0), Some(entered))],
      "{text}"
    );
    assert_eq!(
      labels(&analysis, TIER),
      [vec![(entered, entry("Payload"))]],
      "{text}"
    );
    assert_eq!(messages(&analysis, TIER), [LIST], "{text}");
    assert_eq!(
      analysis.diagnostics().len(),
      1,
      "{text}: {:#?}",
      analysis.diagnostics()
    );
    assert!(!metadata(analysis.model(), "^::app::Payload"), "{text}");
  }
  for run in [
    "directive @run<T>(n: Int) on FIELD\n",
    "interface Cmp<X> {\n  id: Int\n}\ndirective @run<T>(n: Int) on FIELD where T: Cmp<T>\n",
    "directive @run<T, U = [T]>(n: Int) on FIELD\n",
    "directive @tag<T>(v: T) on OBJECT\n",
  ] {
    let text = format!("{payload}{run}");
    let analysis = analyze_at(&text, Tier::Core);
    quiet(&analysis);
    assert!(metadata(analysis.model(), "^::app::Payload"), "{text}");
  }
}

/// A bound in a directive no operation can apply roots what its interface's arguments name: `Cfg`,
/// named in `Sized<Cfg>`, is metadata, and nothing is said; `Sized`, an interface, is not. Refuses
/// an output type's arguments not looked through.
#[test]
fn a_bound_in_a_directive_roots_its_interfaces_arguments() {
  let analysis = analyze_at(
    "type Query {\n  x: Int\n}\ninterface Sized<X> {\n  size: Int\n}\ninput Cfg {\n  s: string\n}\n\
     directive @d<T> on OBJECT where T: Sized<Cfg>\n",
    Tier::Core,
  );
  quiet(&analysis);
  let model = analysis.model();
  assert!(metadata(model, "^::app::Cfg"));
  assert!(!metadata(model, "^::app::Sized"));
}

/// The schema's written roots are used outside metadata: `Root<Binding>`, the query root, names
/// `Binding`, which a directive's argument takes too, so it is judged, and each report is labelled
/// at that root. Refuses the walk outside metadata without the schema's roots.
#[test]
fn a_schema_root_is_a_runtime_use() {
  let text = "schema {\n  query: Root<Binding>\n}\ntype Root<T> {\n  f(b: T): Int\n}\n\
              input Binding {\n  lang: string!\n  path: string!\n}\n\
              directive @bind(with: [Binding!]!) on SCALAR\n\
              scalar Point @bind(with: [{ lang: \"rust\", path: \"geo::Point\" }])\n";
  let analysis = analyze_at(text, Tier::Core);
  let schema = file(&analysis, 3, "schema.graphqlx");
  let entered = at(schema, text, "Root<Binding>", 0);
  assert_eq!(
    found(&analysis, TIER),
    [
      (at(schema, text, "string!", 0), Some(entered)),
      (at(schema, text, "string!", 1), Some(entered)),
    ]
  );
  assert_eq!(
    labels(&analysis, TIER),
    vec![vec![(entered, entry("Binding"))]; 2]
  );
  assert_eq!(
    analysis.diagnostics().len(),
    2,
    "{:#?}",
    analysis.diagnostics()
  );
  assert!(!metadata(analysis.model(), "^::app::Binding"));
}

/// An extension's occurrences are its definition's: an extension of `Binding` adds a list of
/// `string!`, and with only the directive taking `Binding` nothing is said; when an extension of
/// `Query` takes `Binding` in a field's argument, each `string!` and the extension's list are
/// refused, each labelled at that argument. Refuses an extension's occurrences read apart from its
/// definition's.
#[test]
fn an_extension_is_its_definitions() {
  let text = format!("{BINDING}extend input Binding {{\n  more: [string!]\n}}\n");
  let analysis = analyze_at(&text, Tier::Core);
  quiet(&analysis);
  assert!(metadata(analysis.model(), "^::app::Binding"));

  let used = format!("{text}extend type Query {{\n  g(b: Binding): Int\n}}\n");
  let analysis = analyze_at(&used, Tier::Core);
  let schema = file(&analysis, 3, "schema.graphqlx");
  let entered = Some(within(schema, &used, "g(b: Binding)", "Binding"));
  assert_eq!(
    found(&analysis, TIER),
    [
      (at(schema, &used, "string!", 0), entered),
      (at(schema, &used, "string!", 1), entered),
      (at(schema, &used, "[string!]", 0), entered),
      (at(schema, &used, "string!", 2), entered),
    ]
  );
  assert_eq!(messages(&analysis, TIER), [STRING, STRING, LIST, STRING]);
  assert!(!metadata(analysis.model(), "^::app::Binding"));
}

/// A dependant can exempt a dependency's definition: `geo`, at `core`, holds an input object that
/// nothing in it names, which alone it refuses at its `string`; under a root package whose
/// directive takes it, it is metadata, and nothing is said. Refuses metadata found one package at
/// a time.
#[test]
fn a_dependant_can_exempt_a_dependencys_definition() {
  const GEO: &str = "type Query {\n  ok: Int\n}\ninput Cfg {\n  s: string\n}\n";
  let geo = || package("geo", Tier::Core, &[("geo.graphqlx", GEO)]);
  let analysis = analyze(&geo(), &[], &Limits::new());
  let geo_file = file(&analysis, 3, "geo.graphqlx");
  assert_eq!(
    found(&analysis, TIER),
    [(at(geo_file, GEO, "string", 0), None)]
  );
  assert_eq!(
    analysis.diagnostics().len(),
    1,
    "{:#?}",
    analysis.diagnostics()
  );
  assert!(!metadata(analysis.model(), "^::geo::Cfg"));

  let ours = "type Query {\n  x: Int\n}\ndirective @rd(c: ^::geo::Cfg) on OBJECT\n";
  let root = package("app", Tier::Std, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let analysis = analyze(&root, &[geo()], &Limits::new());
  quiet(&analysis);
  assert!(metadata(analysis.model(), "^::geo::Cfg"));
}

/// An application's written type arguments are roots, each by its own type, even where a phase
/// refused the application: `@doc<Sample, Int>`, a count `@doc` does not take, is the phase's
/// `generic-arity` alone, and `Sample` is metadata. An application whose directive names nothing
/// keeps no type argument: `@docc<Sample>` is the resolver's `undefined-directive`, and `Sample`'s
/// `string!` is refused, as it was. Refuses roots read off an application's complete type alone.
#[test]
fn an_applications_written_type_arguments_are_roots() {
  let sample = "directive @doc<T>(example: T) on FIELD_DEFINITION\n\
                input Sample {\n  text: string!\n}\n";
  let refused =
    format!("type Query {{\n  x: Int @doc<Sample, Int>(example: {{ text: \"hi\" }})\n}}\n{sample}");
  let analysis = analyze_at(&refused, Tier::Core);
  assert_eq!(codes(&analysis), ["smear::graphqlx::generic-arity"]);
  assert!(metadata(analysis.model(), "^::app::Sample"));

  let misspelt =
    format!("type Query {{\n  x: Int @docc<Sample>(example: {{ text: \"hi\" }})\n}}\n{sample}");
  let analysis = analyze_at(&misspelt, Tier::Core);
  assert_eq!(
    codes(&analysis),
    ["smear::schema::undefined-directive", TIER]
  );
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    found(&analysis, TIER),
    [(at(schema, &misspelt, "string!", 0), None)]
  );
  assert!(!metadata(analysis.model(), "^::app::Sample"));
}
