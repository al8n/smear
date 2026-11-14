//! The GraphQLx analyzer's tiers: what each definition and each type requires, the least of
//! `core`, `alloc` and `std` that holds everything it uses.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, DefId, Diagnostic, IntegerProvider, Layout, Limits, MemberId, Model, Namespace,
    Package, PackageId, PackageKind, SourceFile, SourceId, Tier, TypeExprId, analyze, test_support,
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

/// The exemption is shallow: an input object a directive's argument names is a definition like any
/// other, and its fields are judged where they are written. At `core`, `Binding`'s `string!` is
/// refused once, and the directive's argument that names `Binding` is not. Refuses an input object
/// exempt because a directive's argument names it.
#[test]
fn an_input_object_a_directive_names_is_judged_where_it_is_written() {
  let text = "type Query {\n  x: Int\n}\ninput Binding {\n  lang: string!\n}\n\
              directive @bind(b: Binding) on SCALAR\n\
              scalar S @bind(b: { lang: \"rust\" })\n";
  let analysis = analyze_at(text, Tier::Core);
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    analysis.diagnostics().len(),
    1,
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    found(&analysis, TIER),
    [(at(schema, text, "string!", 0), None)]
  );
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

/// In an analysis that is ok, every definition's tier is at most its package's: at `core`,
/// primitives, `NonZero`, an enum and an extern of `CORE`; at `alloc`, strings and lists; at
/// `std`, a `HashMap`; the meta-types and the library's definitions their tags.
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
  ] {
    let analysis = analyze_at(text, tier);
    quiet(&analysis);
    let model = analysis.model();
    for id in model.defs() {
      let definition = model.def(id);
      let package = model.package(model.module(definition.module()).package());
      assert!(
        definition.tier() <= package.tier(),
        "{text}: {}",
        definition.name()
      );
    }
  }
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
