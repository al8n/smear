//! Derived instantiations, design Z's D11: which generic definitions grow, one fact that the
//! analysis and the opt-in rule `UnboundedInstantiation` read off one graph; and what a written
//! instantiation derives through the definitions that do not grow, judged for its definition's
//! values, the arguments it makes required and its entries and members, and reported at the
//! written occurrence that derives it.
#![cfg(all(feature = "std", feature = "analysis"))]

use std::path::PathBuf;

use smear::{
  analysis::graphqlx::{
    Analysis, DefId, Diagnostic, Layout, Limits, Model, Namespace, Package, PackageId, SourceFile,
    SourceId, Tier, analyze, test_support,
  },
  diagnostic::{Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

/// A bound of the limits passed.
const LIMIT: &str = "smear::graphqlx::resource-limit";
/// GraphQL's code for a default that does not fit.
const DEFAULT: &str = "smear::schema::invalid-default-value";
/// GraphQL's code for two members of a union of one type.
const MEMBER: &str = "smear::schema::duplicate-union-member";
/// GraphQL's code for an argument a field requires that its interface's field does not declare.
const UNEXPECTED: &str = "smear::schema::unexpected-required-argument";
/// GraphQL's code for a directive argument's value that does not fit.
const ARGUMENT: &str = "smear::schema::invalid-directive-argument-value";
/// GraphQL's code for a required directive argument left out, or given `null`.
const MISSING: &str = "smear::schema::missing-required-directive-argument";
/// GraphQL's code for a default that needs a copy of itself.
const CYCLE: &str = "smear::schema::input-object-default-value-cycle";
/// The code of a marker that declares a field.
const MARKER_MISUSE: &str = "smear::graphqlx::marker-misuse";
/// GraphQL's code for two arguments of one name.
const DUPLICATE_ARGUMENT: &str = "smear::schema::duplicate-argument-name";
/// The code of an extension a package may not write.
const ORPHAN: &str = "smear::graphqlx::orphan-extension";
/// The label at the type that derives the instantiation a report names.
const DERIVES: &str = "this derives that instantiation";

/// An analysis of the root package `app`, `Flat`, at tier `Std`, holding the one file
/// `schema.graphqlx`, within `limits`.
fn analyze_text(text: &str, limits: &Limits) -> Analysis {
  let root = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", text)]);
  analyze(&root, &[], limits)
}

/// The root package's one file, `schema.graphqlx`.
fn schema_file(analysis: &Analysis) -> SourceId {
  let sources = analysis.sources();
  analysis
    .model()
    .package(PackageId::new(3))
    .sources()
    .into_iter()
    .find(|id: &SourceId| sources.source(*id).path() == "schema.graphqlx")
    .expect("the root package's file")
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

/// Where `literal` is written right after `before`, the first time, in `text`, the file `source`.
fn after(source: SourceId, text: &str, before: &str, literal: &str) -> Location {
  let written = format!("{before}{literal}");
  let start = text
    .find(written.as_str())
    .unwrap_or_else(|| panic!("`{written}` is not written"))
    + before.len();
  Location::new(source.get(), SimpleSpan::new(start, start + literal.len()))
}

/// Where the `nth` occurrence, counting from 0, of the name `name` written right after `before` is
/// in `text`, the file `source`: a name that a longer name begins with is not one.
fn named(source: SourceId, text: &str, before: &str, name: &str, nth: usize) -> Location {
  let written = format!("{before}{name}");
  let start = text
    .match_indices(written.as_str())
    .map(|(start, _): (usize, &str)| start + before.len())
    .filter(|start: &usize| {
      text[start + name.len()..]
        .chars()
        .next()
        .is_none_or(|next: char| !(next.is_ascii_alphanumeric() || next == '_'))
    })
    .nth(nth)
    .unwrap_or_else(|| panic!("`{written}` does not occur {} times", nth + 1));
  Location::new(source.get(), SimpleSpan::new(start, start + name.len()))
}

/// Each diagnostic's code and primary location, in the list's order.
fn found(analysis: &Analysis) -> Vec<(&'static str, Location)> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| (diagnostic.code().as_str(), diagnostic.primary()))
    .collect()
}

/// The primary location of each diagnostic with the code `code`, in the list's order.
fn codes_at(analysis: &Analysis, code: &str) -> Vec<Location> {
  analysis
    .diagnostics()
    .iter()
    .filter(|diagnostic: &&Diagnostic| diagnostic.code().as_str() == code)
    .map(|diagnostic: &Diagnostic| diagnostic.primary())
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

/// The diagnostic with the code `code` at `at`.
fn the<'a>(analysis: &'a Analysis, code: &str, at: Location) -> &'a Diagnostic {
  analysis
    .diagnostics()
    .iter()
    .find(|diagnostic: &&Diagnostic| {
      diagnostic.code().as_str() == code && diagnostic.primary() == at
    })
    .unwrap_or_else(|| panic!("no `{code}` at {at:?}: {:#?}", analysis.diagnostics()))
}

/// Each label of `diagnostic`, where it is and what it says, in order.
fn labels(diagnostic: &Diagnostic) -> Vec<(Location, &str)> {
  (0..diagnostic.labels())
    .filter_map(|index: usize| diagnostic.label(index))
    .map(|label| (label.location(), label.text()))
    .collect()
}

/// The definition the root package's `name` names in the type namespace.
fn type_def(model: &Model, name: &str) -> DefId {
  let canonical = format!("^::app::{name}");
  model
    .lookup(Namespace::Type, &canonical)
    .unwrap_or_else(|| panic!("no type `{canonical}`"))
}

/// Every file of the GraphQLx corpus and of the GraphQL schema fixtures, by path, each with its
/// bytes.
fn corpus() -> Vec<(PathBuf, Vec<u8>)> {
  let tests = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
  let mut paths: Vec<PathBuf> = Vec::new();
  for directory in [
    tests.join("corpusx"),
    tests.join("fixtures").join("schemas"),
  ] {
    let before = paths.len();
    let entries = std::fs::read_dir(&directory).unwrap_or_else(|error: std::io::Error| {
      panic!(
        "the corpus at {} is unreadable: {error}",
        directory.display()
      )
    });
    for entry in entries {
      let path = entry.expect("a corpus directory entry").path();
      if path.is_file() {
        paths.push(path);
      }
    }
    assert!(
      paths.len() > before,
      "the corpus at {} holds no file",
      directory.display()
    );
  }
  paths.sort();
  let mut files: Vec<(PathBuf, Vec<u8>)> = Vec::with_capacity(paths.len());
  for path in paths {
    let text = std::fs::read(&path)
      .unwrap_or_else(|error: std::io::Error| panic!("{} is unreadable: {error}", path.display()));
    files.push((path, text));
  }
  files
}

/// The definitions of `model` that grow, as `Def::grows` answers, in id order.
fn grown(model: &Model) -> Vec<DefId> {
  model
    .defs()
    .into_iter()
    .filter(|id: &DefId| model.def(*id).grows())
    .collect()
}

/// The names of `defs`, definitions of `model`, in order.
fn names(model: &Model, defs: &[DefId]) -> Vec<String> {
  defs
    .iter()
    .map(|id: &DefId| model.def(*id).name().to_owned())
    .collect()
}

/// Programs whose definitions grow, and near misses, each with the names of the definitions that
/// grow, in id order: a cycle through one definition, through two, through two parameters, through
/// an `implements` entry, a union member, a directive application and a filled default; a growth
/// that reaches no cycle, a definition that only uses one that grows, parameters passed along, the
/// same instantiation again, and a collection's key, which is non-null in its holder's type.
const GROWTH: &[(&str, &[&str])] = &[
  (
    "type Nest<T> { value: T  deeper: Nest<[T]> }\ntype Query { nest: Nest<i32> }\n",
    &["Nest"],
  ),
  (
    "type A<T> { b: B<[T]> }\ntype B<U> { a: A<U> }\ntype Query { x: Int }\n",
    &["A", "B"],
  ),
  (
    "type A<T, U> { f: A<U, [T]> }\ntype Query { x: Int }\n",
    &["A"],
  ),
  (
    "interface I<T> implements J<[T]> { a: Int }\ninterface J<U> implements I<U> { a: Int }\n\
     type Query { x: Int }\n",
    &["I", "J"],
  ),
  (
    "union U<T> = B<[T]>\ntype B<S> { u: U<S> }\ntype Query { x: Int }\n",
    &["U", "B"],
  ),
  (
    "directive @d<U>(x: G<[U]>) on INPUT_OBJECT\ninput G<T> @d<T> { v: T }\n\
     type Query { x: Int }\n",
    &["d", "G"],
  ),
  (
    "type E<A, B = [A]> { next: E<B> }\ntype Query { x: Int }\n",
    &["E"],
  ),
  (
    "type A<T> { b: B<[T]> }\ntype B<U> { v: U }\ntype Query { x: Int }\n",
    &[],
  ),
  (
    "type Outer<T> { n: Nest<T> }\ntype Nest<T> { value: T  deeper: Nest<[T]> }\n\
     type Query { o: Outer<i32> }\n",
    &["Nest"],
  ),
  (
    "type Swap<T, U> { s: Swap<U, T> }\ntype Query { x: Int }\n",
    &[],
  ),
  (
    "type Nest<T> { value: T  deeper: Nest<T> }\ntype Query { nest: Nest<i32> }\n",
    &[],
  ),
  (
    "directive @d<U>(x: A<U>) on SCALAR\nscalar Bag<T> @set @d<T>\ninput A<T> { m: Bag<T> }\n\
     type Query { x: Int }\n",
    &[],
  ),
];

/// `Def::grows`, which the analysis reads off the growth graph once instantiation has typed every
/// occurrence, agrees with the graph the opt-in rule builds over the finished model, on every
/// definition of the corpus and of the growth programs; and on those, it names the definitions
/// expected. A definition the rule finds to grow and the analysis does not, or the reverse, fails
/// here, and so does a sweep that met no definition that grows.
#[test]
fn def_grows_agrees_with_the_opt_in_rule_on_every_definition_of_the_corpus() {
  let mut definitions: usize = 0;
  for (path, text) in corpus() {
    println!("{}", path.display());
    let root: Package<&[u8]> = Package::new("app", Layout::Flat, Tier::Std)
      .with_files([SourceFile::new("schema.graphqlx", text.as_slice())]);
    let analysis = analyze(&root, &[], &Limits::new());
    let model = analysis.model();
    assert_eq!(
      grown(model),
      test_support::grown_by_the_rule(model, analysis.sources()),
      "{}",
      path.display()
    );
    definitions += model.defs().into_iter().count();
  }
  assert!(definitions > 0, "the corpus held no definition");

  let mut growing: usize = 0;
  for &(text, expected) in GROWTH {
    let analysis = analyze_text(text, &Limits::new());
    let model = analysis.model();
    let ours = grown(model);
    assert_eq!(
      ours,
      test_support::grown_by_the_rule(model, analysis.sources()),
      "{text}"
    );
    assert_eq!(names(model, &ours), expected, "{text}");
    growing += ours.len();
  }
  assert!(growing > 0, "no program grows");
}

/// A value that only a substitution derives an instantiation for, and that does not fit there, is
/// reported at each written occurrence that derives it, once, both nullnesses sharing the verdict:
/// `Outer<String>` derives `Box<String>` through `b`, under which `Box`'s `1` does not fit, with
/// the code the same failure has where `Box<String>` is written, labelled at the type that derives
/// it and at the value. `Outer<Int>` derives a `Box<Int>` that fits. Nothing derived is kept:
/// `Box` has no instantiation the program writes. Refuses the gap D11 closes, which said nothing.
#[test]
fn a_failure_only_a_substitution_derives_is_reported_at_the_written_occurrence() {
  let text = "type Query {\n  f(o: Outer<String>): Int\n  g(o: Outer<String>!): Int\n}\n\
              input Box<T> {\n  v: T = 1\n}\ninput Outer<U> {\n  b: Box<U>\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let nullable = at(schema, text, "Outer<String>", 0);
  let non_null = at(schema, text, "Outer<String>!", 0);
  assert_eq!(found(&analysis), [(DEFAULT, nullable), (DEFAULT, non_null)]);
  for occurrence in [nullable, non_null] {
    let report = the(&analysis, DEFAULT, occurrence);
    assert_eq!(
      report.to_string(),
      "`Outer` is instantiated here with type arguments that derive an instantiation of `Box` \
       under which `1`, a value it holds, does not fit the type it takes"
    );
    assert_eq!(report.primary_label(), Some("invalid default value"));
    assert_eq!(
      labels(report),
      [
        (after(schema, text, "b: ", "Box<U>"), DERIVES),
        (after(schema, text, "v: T = ", "1"), "this value"),
      ]
    );
  }
  let model = analysis.model();
  assert!(model.instantiations(type_def(model, "Box")).is_empty());

  let fits = "type Query {\n  f(o: Outer<Int>): Int\n}\ninput Box<T> {\n  v: T = 1\n}\n\
              input Outer<U> {\n  b: Box<U>\n}\n";
  quiet(&analyze_text(fits, &Limits::new()));
}

/// What a type parameter's default fills is derived by that default: `Conn<String>` leaves `E` to
/// `Edge<N>`, which gives it `Edge<String>`, whose `1` does not fit, though no member of `Conn`
/// names `E`. Reported at `Conn<String>`, labelled at the default. Refuses a root whose filled
/// arguments are not read.
#[test]
fn a_failure_only_a_fill_derives_is_reported_at_the_written_occurrence() {
  let text = "type Query {\n  f(c: Conn<String>): Int\n}\ninput Edge<N> {\n  v: N = 1\n}\n\
              input Conn<N, E = Edge<N>> {\n  n: N\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let occurrence = at(schema, text, "Conn<String>", 0);
  assert_eq!(found(&analysis), [(DEFAULT, occurrence)]);
  assert_eq!(
    labels(the(&analysis, DEFAULT, occurrence)),
    [
      (after(schema, text, "E = ", "Edge<N>"), DERIVES),
      (after(schema, text, "v: N = ", "1"), "this value"),
    ]
  );
}

/// A derived instantiation of a definition that grows is not judged. `Nest<Int>` derives
/// `Nest<[Int]!>`, under which `v`'s `null` would not fit, and says nothing: `Nest` grows. The
/// `Box<Int>` that the written `Nest<Int>` derives is judged all the same, `Box` not growing, and
/// its `"s"` reported at `Nest<Int>`; the `Box<[Int]>` that only `Nest<[Int]>` would derive is
/// never derived. Refuses a closure that expands a growing definition's derived instantiation, and
/// one that judges nothing a growing definition's written instantiation derives.
#[test]
fn a_growing_definitions_derived_instantiation_is_not_judged() {
  let text = "type Query {\n  f(n: Nest<Int>): Int\n}\n\
              input Nest<T> {\n  v: T = null\n  deeper: Nest<[T]!>\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  quiet(&analysis);
  let model = analysis.model();
  assert!(model.def(type_def(model, "Nest")).grows());

  let text = "type Query {\n  f(n: Nest<Int>): Int\n}\ninput Box<T> {\n  v: T = \"s\"\n}\n\
              input Nest<T> {\n  b: Box<T>\n  deeper: Nest<[T]>\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let occurrence = at(schema, text, "Nest<Int>", 0);
  assert_eq!(found(&analysis), [(DEFAULT, occurrence)]);
  assert_eq!(
    labels(the(&analysis, DEFAULT, occurrence)),
    [
      (after(schema, text, "b: ", "Box<T>"), DERIVES),
      (after(schema, text, "v: T = ", "\"s\""), "this value"),
    ]
  );
  let model = analysis.model();
  assert!(model.def(type_def(model, "Nest")).grows());
  assert!(!model.def(type_def(model, "Box")).grows());
}

/// The closure counts each instantiation it derives toward `Limits::max_instantiations`, and stops
/// past it with one `resource-limit`, at where the written instantiation whose expansion was
/// running is first written, however many occurrences it has. `A1<Int>` derives `A2<[Int]>`,
/// `A3<[[Int]]>` and `A4<[[[Int]]]>`, each new: under the least bound the program is ok at, less
/// one, the last cannot be built. `A1`, `A2` and `A3` judge nothing: each is expanded because its
/// expansion reaches `A4`, which does. Refuses a closure uncounted, a report at each occurrence,
/// and a pruning that is not transitive.
#[test]
fn the_closure_stops_at_max_instantiations_with_one_resource_limit() {
  let text = "type Query {\n  f(a: A1<Int>): Int\n  g(a: A1<Int>): Int\n}\n\
              input A1<T> {\n  n: A2<[T]>\n}\ninput A2<T> {\n  n: A3<[T]>\n}\n\
              input A3<T> {\n  n: A4<[T]>\n}\ninput A4<T> {\n  v: T = 1\n}\n";
  quiet(&analyze_text(text, &Limits::new()));
  let least = (0..64_u32)
    .find(|&bound: &u32| analyze_text(text, &Limits::new().with_max_instantiations(bound)).is_ok())
    .expect("a bound the program is ok under");
  assert!(least > 0);
  let short = analyze_text(text, &Limits::new().with_max_instantiations(least - 1));
  let schema = schema_file(&short);
  let occurrence = at(schema, text, "A1<Int>", 0);
  assert_eq!(found(&short), [(LIMIT, occurrence)]);
  assert_eq!(short.diagnostics().error_count(), 1);
  assert_eq!(
    the(&short, LIMIT, occurrence).to_string(),
    format!(
      "what `A1` derives under the type arguments it is instantiated with here could not be \
       read: it derives more instantiations of generic definitions than \
       `Limits::max_instantiations`, {}, admits",
      least - 1
    )
  );
}

/// Two members of a union that a derived instantiation makes one type are refused at the written
/// occurrence that derives it: `Outer<User>` derives `Either<User, User>` through `e`, labelled at
/// `e`'s type and at the two members; `Outer<Post>` derives `Either<Post, User>`, whose members
/// stay distinct. Refuses the implementation check's gap at derived instantiations.
#[test]
fn derived_members_that_meet_are_refused_at_the_written_occurrence() {
  let text = "type Query {\n  o: Outer<User>\n  p: Outer<Post>\n}\ntype User {\n  id: ID\n}\n\
              type Post {\n  id: ID\n}\nunion Either<A, B> = A | B\n\
              type Outer<T> {\n  e: Either<T, User>\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let occurrence = at(schema, text, "Outer<User>", 0);
  assert_eq!(found(&analysis), [(MEMBER, occurrence)]);
  let report = the(&analysis, MEMBER, occurrence);
  assert_eq!(
    report.to_string(),
    "`Outer` is instantiated here with type arguments that derive an instantiation of `Either` \
     under which two of its members are one type"
  );
  assert_eq!(
    labels(report),
    [
      (after(schema, text, "e: ", "Either<T, User>"), DERIVES),
      (named(schema, text, "= ", "A", 0), "this member"),
      (named(schema, text, "| ", "B", 0), "the same type here"),
    ]
  );
}

/// An argument a field adds beside its interface's field's, typed by its own bare nullable type
/// parameter, is required in a derived instantiation that gives the parameter a non-null type:
/// `Outer<Int!>` derives `Page<Int!>`, which no occurrence writes, and is refused, labelled at
/// `p`'s type, at the argument and at the interface's field; `Outer<Int>` derives a `Page<Int>`
/// that requires nothing. Refuses the gap the module recorded for `Outer<Int!>`.
#[test]
fn an_argument_a_derived_instantiation_makes_required_is_refused_at_the_written_occurrence() {
  let text = "type Query {\n  o: Outer<Int!>\n  q: Outer<Int>\n}\n\
              interface Paged {\n  items: [Int]\n}\n\
              type Page<T> implements Paged {\n  items(after: T): [Int]\n}\n\
              type Outer<U> {\n  p: Page<U>\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let occurrence = at(schema, text, "Outer<Int!>", 0);
  assert_eq!(found(&analysis), [(UNEXPECTED, occurrence)]);
  let report = the(&analysis, UNEXPECTED, occurrence);
  assert_eq!(
    report.to_string(),
    "`Outer` is instantiated here with type arguments that derive an instantiation of `Page` \
     under which its field `items` requires the argument `after`, which `Paged`'s field `items` \
     does not declare"
  );
  assert_eq!(
    labels(report),
    [
      (after(schema, text, "p: ", "Page<U>"), DERIVES),
      (
        named(schema, text, "items(", "after", 0),
        "required under these type arguments"
      ),
      (
        named(schema, text, "  ", "items", 0),
        "the interface's field"
      ),
    ]
  );
}

/// An application whose directive's instantiation derives one that fails is reported at its path:
/// `@w<String>` derives `Box<String>` through `w`'s argument `a`, labelled there and at the value.
/// Refuses a closure that starts from type occurrences alone.
#[test]
fn an_application_reports_what_its_directives_instantiation_derives() {
  let text = "type Query @w<String> {\n  x: Int\n}\ninput Box<T> {\n  v: T = 1\n}\n\
              directive @w<T>(a: Box<T>) on OBJECT\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let path = named(schema, text, "@", "w", 0);
  assert_eq!(found(&analysis), [(DEFAULT, path)]);
  let report = the(&analysis, DEFAULT, path);
  assert_eq!(
    report.to_string(),
    "`@w` is applied here with type arguments that derive an instantiation of `Box` under which \
     `1`, a value it holds, does not fit the type it takes"
  );
  assert_eq!(
    labels(report),
    [
      (after(schema, text, "a: ", "Box<T>"), DERIVES),
      (after(schema, text, "v: T = ", "1"), "this value"),
    ]
  );
}

/// An instantiation a supplied package writes is judged where it is written, and what derives it
/// says nothing of it: `Box<String>` is written beside `Outer<String>`, which derives it, and is
/// reported at its own occurrence alone. Refuses a second report of one mistake.
#[test]
fn a_derived_instantiation_a_supplied_package_writes_is_judged_where_it_is_written() {
  let text = "type Query {\n  f(o: Outer<String>, b: Box<String>): Int\n}\n\
              input Box<T> {\n  v: T = 1\n}\ninput Outer<U> {\n  b: Box<U>\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let occurrence = at(schema, text, "Box<String>", 0);
  assert_eq!(found(&analysis), [(DEFAULT, occurrence)]);
  assert_eq!(
    labels(the(&analysis, DEFAULT, occurrence)),
    [(after(schema, text, "v: T = ", "1"), "this value")]
  );
}

/// A written instantiation whose own values do not fit reports them, and nothing it derives: the
/// first failing instantiation of `Outer<String>`'s expansion is itself, its `2`, and the
/// `Box<String>` it derives is not reported too. Refuses a report for each failing instantiation.
#[test]
fn a_written_instantiation_whose_own_values_fail_reports_them_alone() {
  let text = "type Query {\n  f(o: Outer<String>): Int\n}\ninput Box<T> {\n  v: T = 1\n}\n\
              input Outer<U> {\n  u: U = 2\n  b: Box<U>\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let occurrence = at(schema, text, "Outer<String>", 0);
  assert_eq!(found(&analysis), [(DEFAULT, occurrence)]);
  assert_eq!(
    labels(the(&analysis, DEFAULT, occurrence)),
    [(after(schema, text, "u: U = ", "2"), "this value")]
  );
}

/// A value a written object literal of a derived instantiation reported already, as its own copy
/// that does not fit, is the literal's (W:979): the check of that instantiation passes over it to
/// its next row, which the written occurrence that derives it reports. `{ w: "s" }`, a
/// `Box<String>` under `Wrap<String>`, leaves out `v`, whose `1` it reports; `Wrap<String>`
/// derives that `Box<String>` through `b`, and reports its `true`, which no literal reported.
/// Refuses a second report of one mistake, and a check that ends at a row it passes over.
#[test]
fn a_value_a_literal_reported_is_passed_over_where_it_is_derived() {
  let text = "type Query {\n  x: Int\n}\ninput Box<U> {\n  v: U = 1\n  w: U = true\n}\n\
              input Wrap<W> {\n  b: Box<W>\n}\n\
              input H {\n  h: Wrap<String> = { b: { w: \"s\" } }\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let wrap = at(schema, text, "Wrap<String>", 0);
  let literal = after(schema, text, "{ b: ", "{ w: \"s\" }");
  assert_eq!(found(&analysis), [(DEFAULT, wrap), (DEFAULT, literal)]);
  let report = the(&analysis, DEFAULT, wrap);
  assert_eq!(
    report.to_string(),
    "`Wrap` is instantiated here with type arguments that derive an instantiation of `Box` under \
     which `true`, a value it holds, does not fit the type it takes"
  );
  assert_eq!(
    labels(report),
    [
      (after(schema, text, "b: ", "Box<W>"), DERIVES),
      (after(schema, text, "w: U = ", "true"), "this value"),
    ]
  );
  assert_eq!(
    labels(the(&analysis, DEFAULT, literal)),
    [(after(schema, text, "v: U = ", "1"), "this default")]
  );
}

/// `null` written for an argument a collection's element makes required is the application's own
/// mistake, refused where it is written and recorded for no instantiation, so no derived
/// instantiation reports it again: `Holder<Int>` and `Holder<String>` derive `Tags<Int>` and
/// `Tags<String>`, each judged for `Tags`'s other value, the `1` of `@fine`, which `String` does
/// not take. Refuses the `null` reported at a derivation.
#[test]
fn a_null_for_an_argument_a_collections_element_makes_required_derives_nothing() {
  let text = "type Query {\n  a: Holder<Int>\n  b: Holder<String>\n}\n\
              directive @need<T>(v: T) on SCALAR\ndirective @fine<T>(w: T) on SCALAR\n\
              scalar Tags<K> @set @need<K>(v: null) @fine<K>(w: 1)\n\
              type Holder<T> {\n  t: Tags<T>\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let derives = at(schema, text, "Holder<String>", 0);
  let argument = after(schema, text, "@need<K>(", "v");
  assert_eq!(found(&analysis), [(ARGUMENT, derives), (MISSING, argument)]);
  assert_eq!(
    the(&analysis, MISSING, argument).to_string(),
    "the argument `v` of `@need` is required, and `null` is given"
  );
  let report = the(&analysis, ARGUMENT, derives);
  assert_eq!(
    report.to_string(),
    "`Holder` is instantiated here with type arguments that derive an instantiation of `Tags` \
     under which `1`, a value it holds, does not fit the type it takes"
  );
  assert_eq!(
    labels(report),
    [
      (after(schema, text, "t: ", "Tags<T>"), DERIVES),
      (after(schema, text, "@fine<K>(w: ", "1"), "this value"),
    ]
  );
}

/// What a default fills is derived wherever it stands, closed or not, and a definition that holds
/// one is expanded though it judges nothing itself: `D`'s `Conn<Int, T>` is completed with
/// `Edge<Int>`, which `E`'s default fills and no occurrence writes, so `D<String>` derives it,
/// through `Conn`, which judges nothing, and reports its `"s"`, labelled at `c`'s type. Refuses a
/// pruning that follows open types alone.
#[test]
fn a_closed_instantiation_a_default_fills_is_derived() {
  let text = "type Query {\n  f(d: D<String>): Int\n}\ninput Edge<N> {\n  v: N = \"s\"\n}\n\
              input Conn<A, B, E = Edge<A>> {\n  b: B\n  e: E\n}\n\
              input D<T> {\n  c: Conn<Int, T>\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let occurrence = at(schema, text, "D<String>", 0);
  assert_eq!(found(&analysis), [(DEFAULT, occurrence)]);
  let report = the(&analysis, DEFAULT, occurrence);
  assert_eq!(
    report.to_string(),
    "`D` is instantiated here with type arguments that derive an instantiation of `Edge` under \
     which `\"s\"`, a value it holds, does not fit the type it takes"
  );
  assert_eq!(
    labels(report),
    [
      (after(schema, text, "c: ", "Conn<Int, T>"), DERIVES),
      (after(schema, text, "v: N = ", "\"s\""), "this value"),
    ]
  );
}

/// The steps of `Limits::max_value_steps` end the closure with one `resource-limit`, at where the
/// written instantiation whose expansion was running is first written, and what it derived before
/// is judged all the same, each instantiation as it was derived: under the least budget that
/// judges `Bad<Int>`, which `A1<Int>` derives first, the rebuild of `A2<[T]>` cannot be paid for,
/// and `Bad<Int>`'s `"s"` is still reported at both occurrences of `A1<Int>`. Refuses a stop that
/// leaves what was derived unjudged, and a stop reported at each occurrence.
#[test]
fn the_closure_stops_at_max_value_steps_and_judges_what_it_derived() {
  let text = "type Query {\n  f(a: A1<Int>): Int\n  g(a: A1<Int>): Int\n}\n\
              input Bad<T> {\n  v: T = \"s\"\n}\ninput A1<T> {\n  b: Bad<T>\n  n: A2<[T]>\n}\n\
              input A2<T> {\n  b: Bad<T>\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let first = at(schema, text, "A1<Int>", 0);
  let second = at(schema, text, "A1<Int>", 1);
  assert_eq!(found(&analysis), [(DEFAULT, first), (DEFAULT, second)]);
  let least = (0..256_u32)
    .find(|&steps: &u32| {
      let analysis = analyze_text(text, &Limits::new().with_max_value_steps(steps));
      codes_at(&analysis, DEFAULT).contains(&first)
    })
    .expect("a budget that judges `Bad<Int>`");
  let short = analyze_text(text, &Limits::new().with_max_value_steps(least));
  assert_eq!(codes_at(&short, LIMIT), [first]);
  assert_eq!(codes_at(&short, DEFAULT), [first, second]);
  assert_eq!(short.diagnostics().len(), 3);
  assert_eq!(
    the(&short, LIMIT, first).to_string(),
    format!(
      "what `A1` derives under the type arguments it is instantiated with here could not be read \
       within `Limits::max_value_steps`, {least}, the steps the analysis may take to complete \
       values"
    )
  );
  assert_eq!(
    labels(the(&short, DEFAULT, first)),
    [
      (after(schema, text, "b: ", "Bad<T>"), DERIVES),
      (after(schema, text, "v: T = ", "\"s\""), "this value"),
    ]
  );
}

/// A type the closure would derive past `Limits::max_generic_depth` ends it with one
/// `resource-limit`, at where the root whose expansion was running is first written, and what it
/// derived before is judged all the same: under the least depth the program says no limit at, less
/// one, `A3<[[Int]]>` cannot be built, though every written type can, and `Bad<Int>`'s `"s"` is
/// still reported at both occurrences of `A1<Int>`. Refuses a stop that leaves what was derived
/// unjudged.
#[test]
fn the_closure_stops_at_max_generic_depth_and_judges_what_it_derived() {
  let text = "type Query {\n  f(a: A1<Int>): Int\n  g(a: A1<Int>): Int\n}\n\
              input Bad<T> {\n  v: T = \"s\"\n}\ninput A1<T> {\n  b: Bad<T>\n  n: A2<[T]>\n}\n\
              input A2<T> {\n  n: A3<[T]>\n}\ninput A3<T> {\n  b: Bad<T>\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let first = at(schema, text, "A1<Int>", 0);
  let second = at(schema, text, "A1<Int>", 1);
  assert_eq!(found(&analysis), [(DEFAULT, first), (DEFAULT, second)]);
  let least = (0..64_u32)
    .find(|&depth: &u32| {
      let analysis = analyze_text(text, &Limits::new().with_max_generic_depth(depth));
      codes_at(&analysis, LIMIT).is_empty()
    })
    .expect("a depth the program says no limit at");
  assert!(least > 0);
  let short = analyze_text(text, &Limits::new().with_max_generic_depth(least - 1));
  assert_eq!(codes_at(&short, LIMIT), [first]);
  assert_eq!(codes_at(&short, DEFAULT), [first, second]);
  assert_eq!(short.diagnostics().len(), 3);
  assert_eq!(
    the(&short, LIMIT, first).to_string(),
    format!(
      "what `A1` derives under the type arguments it is instantiated with here could not be read: \
       a type it derives nests deeper than `Limits::max_generic_depth`, {}, admits",
      least - 1
    )
  );
}

/// The closure fills nothing, and so never stops at `Limits::max_substitutions`: a stop there is
/// the fill's, one `resource-limit` at the written occurrence it could not complete, which then
/// derives nothing, and what the other written instantiations derive is judged all the same.
/// Under the least bound the program says no limit at, less one, `Fill<Int>` cannot be filled,
/// and `A1<Int>` still reports `Bad<Int>`'s `"s"`. Refuses a closure that a fill's stop silences.
#[test]
fn a_stop_at_max_substitutions_is_the_fills_and_leaves_the_closure_judging() {
  let text = "type Query {\n  f(a: A1<Int>): Int\n  h(x: Fill<Int>): Int\n}\n\
              input Bad<T> {\n  v: T = \"s\"\n}\ninput A1<T> {\n  b: Bad<T>\n}\n\
              input Fill<T, U = [T]> {\n  u: U\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let derives = at(schema, text, "A1<Int>", 0);
  let fill = at(schema, text, "Fill<Int>", 0);
  assert_eq!(found(&analysis), [(DEFAULT, derives)]);
  let least = (0..64_u32)
    .find(|&bound: &u32| {
      let analysis = analyze_text(text, &Limits::new().with_max_substitutions(bound));
      codes_at(&analysis, LIMIT).is_empty()
    })
    .expect("a bound the program says no limit at");
  assert!(least > 0);
  let short = analyze_text(text, &Limits::new().with_max_substitutions(least - 1));
  assert_eq!(found(&short), [(DEFAULT, derives), (LIMIT, fill)]);
}

/// An application's report of a default it leaves out that does not fit owns what fails inside
/// that default (W:979): `@w<String>` leaves out `a`, whose default `{}`, copied as a
/// `Box<String>`, leaves out `v`, whose `1` does not fit. The application reports the default at
/// its path, and the `Box<String>` its instantiation derives through `a` passes over that `1`. One
/// report, at the path. Refuses a second report of one mistake.
#[test]
fn a_default_an_application_reported_owns_what_fails_inside_it() {
  let text = "type Query @w<String> {\n  x: Int\n}\ninput Box<U> {\n  v: U = 1\n}\n\
              directive @w<T>(a: Box<T> = {}) on OBJECT\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let path = named(schema, text, "@", "w", 0);
  assert_eq!(found(&analysis), [(DEFAULT, path)]);
  let report = the(&analysis, DEFAULT, path);
  assert_eq!(
    report.to_string(),
    "under this application's type arguments, the default `{}` of `a` does not fit the type `a` \
     takes"
  );
  assert_eq!(
    labels(report),
    [(after(schema, text, "Box<T> = ", "{}"), "this default")]
  );
}

/// A stop inside a root's own expansion leaves what that expansion derived before it blamed on the
/// root: under the least depth the program says no limit at, less one, `A1<[Int]>`'s first
/// position derives `Bad<[Int]>`, whose `"s"` does not fit, and its second, `Deep<[[T]]>`, cannot
/// be rebuilt. One `resource-limit` at the first `A1<[Int]>`, and the failure at both. Refuses an
/// interrupted expansion that loses the children it judged.
#[test]
fn a_stopped_expansion_keeps_what_it_derived_before_the_stop() {
  let text = "type Query {\n  f(a: A1<[Int]>): Int\n  g(a: A1<[Int]>): Int\n}\n\
              input Bad<T> {\n  v: T = \"s\"\n}\ninput A1<T> {\n  b: Bad<T>\n  n: Deep<[[T]]>\n}\n\
              input Deep<T> {\n  b: Bad<T>\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let first = at(schema, text, "A1<[Int]>", 0);
  let second = at(schema, text, "A1<[Int]>", 1);
  assert_eq!(found(&analysis), [(DEFAULT, first), (DEFAULT, second)]);
  let least = (0..64_u32)
    .find(|&depth: &u32| {
      let analysis = analyze_text(text, &Limits::new().with_max_generic_depth(depth));
      codes_at(&analysis, LIMIT).is_empty()
    })
    .expect("a depth the program says no limit at");
  assert!(least > 0);
  let short = analyze_text(text, &Limits::new().with_max_generic_depth(least - 1));
  assert_eq!(codes_at(&short, LIMIT), [first]);
  assert_eq!(codes_at(&short, DEFAULT), [first, second]);
  assert_eq!(short.diagnostics().len(), 3);
  assert!(
    the(&short, LIMIT, first)
      .to_string()
      .contains("a type it derives nests deeper than `Limits::max_generic_depth`"),
    "{:#?}",
    short.diagnostics()
  );
  assert_eq!(
    labels(the(&short, DEFAULT, first)),
    [
      (after(schema, text, "b: ", "Bad<T>"), DERIVES),
      (after(schema, text, "v: T = ", "\"s\""), "this value"),
    ]
  );
}

/// A check a limit stops while one root's expansion is running is that root's, reported at its
/// occurrence, and the root whose expansion the limit stops next says so once: under one step less
/// than `R1<Int>` needs to judge the `Bad<Int>` it derives, that check stops, `R1<Int>` needs
/// nothing more, and `R2<Int>`'s first rebuild cannot be paid for. One `resource-limit` at each.
/// Refuses a later root that takes the blame for an earlier root's spent budget.
#[test]
fn a_spent_budget_is_reported_at_the_root_whose_check_it_stopped() {
  let text = "type Query {\n  f(a: R1<Int>): Int\n  g(b: R2<Int>): Int\n}\n\
              input Bad<T> {\n  v: T = \"s\"\n}\ninput R1<T> {\n  b: Bad<T>\n}\n\
              input R2<T> {\n  b: Bad<[T]>\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let one = at(schema, text, "R1<Int>", 0);
  let two = at(schema, text, "R2<Int>", 0);
  assert_eq!(found(&analysis), [(DEFAULT, one), (DEFAULT, two)]);
  let least = (0..256_u32)
    .find(|&steps: &u32| {
      let analysis = analyze_text(text, &Limits::new().with_max_value_steps(steps));
      codes_at(&analysis, DEFAULT).contains(&one)
    })
    .expect("a budget that judges `Bad<Int>`");
  assert!(least > 0);
  let short = analyze_text(text, &Limits::new().with_max_value_steps(least - 1));
  assert_eq!(found(&short), [(LIMIT, one), (LIMIT, two)]);
  assert!(
    the(&short, LIMIT, one)
      .to_string()
      .starts_with("the values an instantiation of `Bad` that `R1` derives here holds could not"),
    "{:#?}",
    short.diagnostics()
  );
  assert!(
    the(&short, LIMIT, two)
      .to_string()
      .starts_with("what `R2` derives under the type arguments it is instantiated with here"),
    "{:#?}",
    short.diagnostics()
  );
}

/// An argument an occurrence writes is no fill, though its parameter has a default: `Wrap<[i32]>`
/// writes `T`, so its `[i32]` is not walked, and costs nothing, even with no step to spend while
/// another definition records a value. Refuses a written argument charged as a fill.
#[test]
fn an_argument_written_where_a_default_could_fill_it_is_not_walked() {
  let text = "type Query {\n  f(w: Wrap<[i32]>): Int\n}\ninput Wrap<T = i32> {\n  t: T\n}\n\
              input Unused<U> {\n  u: U = 1\n}\n";
  quiet(&analyze_text(text, &Limits::new().with_max_value_steps(0)));
}

/// A report of a copy owns each copy beneath it that does not fit (W:979): `@w<String>(a: {})`
/// leaves out `Mid`'s `leaf`, whose default `{}` leaves out `Leaf`'s `bad`, whose `1` does not fit
/// `String`. The literal reports `leaf`'s default, once, and the `Mid<String>` and `Leaf<String>`
/// that `@w<String>` derives pass over what it owns. Refuses a nested mistake reported twice.
#[test]
fn a_reported_copy_owns_the_copies_beneath_it() {
  let text = "type Query @w<String>(a: {}) {\n  x: Int\n}\ninput Leaf<T> {\n  bad: T = 1\n}\n\
              input Mid<T> {\n  leaf: Leaf<T> = {}\n}\ndirective @w<T>(a: Mid<T>) on OBJECT\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let literal = after(schema, text, "(a: ", "{}");
  assert_eq!(found(&analysis), [(DEFAULT, literal)]);
  let report = the(&analysis, DEFAULT, literal);
  assert_eq!(
    report.to_string(),
    "this literal leaves out `leaf`, whose default `{}` does not fit the type `leaf` takes under \
     these type arguments"
  );
  assert_eq!(
    labels(report),
    [(after(schema, text, "Leaf<T> = ", "{}"), "this default")]
  );
}

/// An application's report of a default it leaves out owns what its count found beneath it, though
/// no copy is made: `@d<Int>` leaves out `a`, whose default holds a copy of itself through `next`,
/// and whose `"bad"`, and `next`'s `"x"`, do not fit `Int`. The cycle is reported at `next`, the
/// default at the path, and the `Rec<Int>` that `@d<Int>` derives passes over `next`'s default.
/// Refuses a default whose copy, never made, owns nothing.
#[test]
fn a_reported_default_owns_what_its_count_found_though_no_copy_is_made() {
  let text = "type Query @d<Int> {\n  x: Int\n}\n\
              directive @d<T>(a: Rec<T> = { value: \"bad\" }) on OBJECT\n\
              input Rec<T> {\n  value: T\n  next: Rec<T> = { value: \"x\" }\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let path = named(schema, text, "@", "d", 0);
  let next = after(schema, text, "value: T\n  ", "next");
  assert_eq!(found(&analysis), [(DEFAULT, path), (CYCLE, next)]);
  assert_eq!(
    the(&analysis, DEFAULT, path).to_string(),
    "under this application's type arguments, the default `{ value: \"bad\" }` of `a` does not \
     fit the type `a` takes"
  );
}

/// The closure's work follows what it can use: `W` has many fields typed by its bare parameter and
/// one that derives `Bad<T>`, and each of many written instantiations of it derives its `Bad`,
/// judged once each, within a budget linear in the program. Refuses an expansion whose steps grow
/// with the positions it cannot use.
#[test]
fn many_instantiations_of_a_wide_definition_are_read_within_a_linear_budget() {
  const N: usize = 200;
  const M: usize = 200;
  let fields: String = (0..M).map(|k: usize| format!("  f{k}: T\n")).collect();
  let uses: String = (0..N)
    .map(|j: usize| format!("  u{j}(w: W<X{j}>): Int\n"))
    .collect();
  let inputs: String = (0..N)
    .map(|j: usize| format!("input X{j} {{\n  v: Int\n}}\n"))
    .collect();
  let text = format!(
    "type Query {{\n{uses}}}\ninput Bad<T> {{\n  v: [T] = []\n}}\n\
     input W<T> {{\n{fields}  b: Bad<T>\n}}\n{inputs}"
  );
  let budget = u32::try_from(16 * (N + M)).expect("a small budget");
  quiet(&analyze_text(
    &text,
    &Limits::new().with_max_value_steps(budget),
  ));
}

/// A type many roots share through a default is read once, however wide: each `Pair<Int, Xj>`
/// fills `Extra` with the one `Wide<Int, …>`, whose many arguments, all of no type parameter, cost
/// nothing past the first reading, within a budget linear in the program. Refuses a walk that
/// rereads a shared type's arguments for each root.
#[test]
fn a_wide_type_many_roots_fill_is_read_within_a_linear_budget() {
  const N: usize = 200;
  const M: usize = 200;
  let params: Vec<String> = (0..M).map(|i: usize| format!("A{i} = Int")).collect();
  let params = params.join(", ");
  let uses: String = (0..N)
    .map(|j: usize| format!("  u{j}(p: Pair<Int, X{j}>): Int\n"))
    .collect();
  let inputs: String = (0..N)
    .map(|j: usize| format!("input X{j} {{\n  v: Int\n}}\n"))
    .collect();
  let text = format!(
    "type Query {{\n{uses}}}\ninput Wide<{params}> {{\n  a: A0 = 1\n}}\n\
     input Pair<A, B, Extra = Wide<A>> {{\n  b: B\n  e: Extra\n}}\n{inputs}"
  );
  let budget = u32::try_from(16 * (N + M)).expect("a small budget");
  quiet(&analyze_text(
    &text,
    &Limits::new().with_max_value_steps(budget),
  ));
}

/// The implementation check's report pass reads each occurrence once, a lookup each: one derived
/// failure, `Outer<User>`'s `Either<User, User>`, beside many occurrences of a wide `Wide!`, is
/// reported once. Refuses a pass that rebuilds a wide type's key for each occurrence.
#[test]
fn many_occurrences_of_a_wide_type_beside_a_failure_are_read_once_each() {
  const N: usize = 200;
  const M: usize = 200;
  let params: Vec<String> = (0..M).map(|i: usize| format!("A{i} = Int")).collect();
  let params = params.join(", ");
  let wides: String = (0..N).map(|j: usize| format!("  w{j}: Wide!\n")).collect();
  let text = format!(
    "type Query {{\n  o: Outer<User>\n{wides}}}\ntype User {{\n  id: ID\n}}\n\
     union Either<A, B> = A | B\ntype Outer<T> {{\n  e: Either<T, User>\n}}\n\
     type Wide<{params}> {{\n  a: A0\n}}\n"
  );
  let analysis = analyze_text(&text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(MEMBER, at(schema, &text, "Outer<User>", 0))]
  );
}

/// A derived instantiation of a definition that grows is neither judged nor expanded, so a default
/// that fills one is no fill to walk, and costs nothing: `Holder<Int>` leaves `U` to `Nest<T>`,
/// whose `null` only an instantiation the closure never judges would read, and with no step to
/// spend the program says nothing. Refuses a fill walked for a check that cannot happen.
#[test]
fn a_fill_of_a_definition_that_grows_is_not_walked() {
  let text = "type Query {\n  h: Holder<Int>\n}\n\
              type Nest<T> {\n  next: Nest<[T]>\n  f(x: T = null): Int\n}\n\
              type Holder<T, U = Nest<T>> {\n  v: T\n}\n";
  quiet(&analyze_text(text, &Limits::new().with_max_value_steps(0)));
  let analysis = analyze_text(text, &Limits::new());
  quiet(&analysis);
  let model = analysis.model();
  assert!(model.def(type_def(model, "Nest")).grows());
}

/// The root whose expansion a limit stops absorbs the stopped checks it reaches, and still reports
/// its first confirmed failure: at depth 3, `Pre<[[Int]]>` derives `A<[[Int]]>`, whose check stops
/// on depth, and is told so at its occurrence; `S<[[Int]]>` reads `A<T>`, then `Bad<T>`, whose
/// `"bad"` does not fit, then `A<[[T]]>`, which stops its expansion. `S<[[Int]]>` reports the stop
/// and the confirmed failure. Refuses a stopped check that hides a confirmed one.
#[test]
fn a_stopped_root_reports_its_confirmed_failure_beside_the_stop() {
  let text = "type Query {\n  p(a: Pre<[[Int]]>): Int\n  s(a: S<[[Int]]>): Int\n}\n\
              input A<T> {\n  x: [[T]] = []\n}\ninput Bad<T> {\n  x: T = \"bad\"\n}\n\
              input Pre<T> {\n  a: A<T>\n}\n\
              input S<T> {\n  a: A<T>\n  b: Bad<T>\n  c: A<[[T]]>\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_generic_depth(3));
  let schema = schema_file(&analysis);
  let pre = at(schema, text, "Pre<[[Int]]>", 0);
  let s = at(schema, text, "S<[[Int]]>", 0);
  assert_eq!(codes_at(&analysis, LIMIT), [pre, s]);
  assert_eq!(codes_at(&analysis, DEFAULT), [s]);
  assert_eq!(analysis.diagnostics().len(), 3);
  assert_eq!(
    labels(the(&analysis, DEFAULT, s)),
    [
      (after(schema, text, "b: ", "Bad<T>"), DERIVES),
      (after(schema, text, "x: T = ", "\"bad\""), "this value"),
    ]
  );
}

/// An application's report of the default it leaves out owns what fails beneath that default, but
/// not another application's omission of it: `@d<Int>` leaves out `x`, and so does the `@d<T>` on
/// `Inner`, whose `Inner<Int>`, which `Outer<Int>` derives, is refused at `Outer<Int>` whether the
/// first application leaves `x` out or writes it. Refuses one application owning another's
/// omission.
#[test]
fn an_application_does_not_own_another_applications_omission() {
  for (first, reported) in [("@d<Int>", true), ("@d<Int>(x: 1)", false)] {
    let text = format!(
      "type Query {first} {{\n  o: Outer<Int>\n}}\ndirective @d<T>(x: T = \"bad\") on OBJECT\n\
       type Inner<T> @d<T> {{\n  v: T\n}}\ntype Outer<T> {{\n  i: Inner<T>\n}}\n"
    );
    let analysis = analyze_text(&text, &Limits::new());
    let schema = schema_file(&analysis);
    let outer = at(schema, &text, "Outer<Int>", 0);
    let path = named(schema, &text, "@", "d", 0);
    let expected: Vec<Location> = if reported {
      vec![path, outer]
    } else {
      vec![outer]
    };
    assert_eq!(codes_at(&analysis, DEFAULT), expected, "{text}");
    assert_eq!(analysis.diagnostics().len(), expected.len(), "{text}");
    assert_eq!(
      the(&analysis, DEFAULT, outer).to_string(),
      "`Outer` is instantiated here with type arguments that derive an instantiation of `Inner` \
       under which `\"bad\"`, a value it holds, does not fit the type it takes"
    );
  }
}

/// The rebuild that stops the closure is reported once: at depth 3, `Outer<[[Int]]>` derives
/// `U<[[Int]]>`, whose member `Box<[[T]]>` cannot be rebuilt there. Coercion reports it, and the
/// implementation check, which compares `U<[[Int]]>`'s members and meets the same rebuild, says
/// nothing more. Refuses one failed rebuild reported by two phases.
#[test]
fn a_rebuild_that_stops_the_closure_is_reported_once() {
  let text = "type Query {\n  o: Outer<[[Int]]>\n}\ntype Box<T> {\n  f(x: T = null): Int\n}\n\
              union U<T> = Box<[[T]]> | Box<T>\ntype Outer<T> {\n  u: U<T>\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_generic_depth(3));
  let schema = schema_file(&analysis);
  let outer = at(schema, text, "Outer<[[Int]]>", 0);
  assert_eq!(found(&analysis), [(LIMIT, outer)]);
  assert!(
    the(&analysis, LIMIT, outer)
      .to_string()
      .starts_with("what `Outer` derives under the type arguments it is instantiated with here"),
    "{:#?}",
    analysis.diagnostics()
  );
}

/// A failure at an open occurrence is reported where it is written, and again at each closed
/// written root that derives a failing instantiation of it, as design Z's section 4.9 says: at each
/// written occurrence whose expansion holds the failing instantiation, once for each. `Outer<T>`'s
/// `Either<T, T>` has two members of one type however it is closed: one report there, and one at
/// each of `Outer<User>` and `Outer<Order>`, labelled at the type that derives it. Refuses a
/// failure lost at a root that derives it.
#[test]
fn a_failure_at_an_open_occurrence_is_reported_there_and_at_each_root_that_derives_it() {
  let text = "type Query {\n  a: Outer<User>\n  b: Outer<Order>\n}\ntype User {\n  id: ID\n}\n\
              type Order {\n  id: ID\n}\nunion Either<A, B> = A | B\n\
              type Outer<T> {\n  e: Either<T, T>\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let user = at(schema, text, "Outer<User>", 0);
  let order = at(schema, text, "Outer<Order>", 0);
  let open = at(schema, text, "Either<T, T>", 0);
  assert_eq!(
    found(&analysis),
    [(MEMBER, user), (MEMBER, order), (MEMBER, open)]
  );
  for root in [user, order] {
    assert_eq!(
      labels(the(&analysis, MEMBER, root)),
      [
        (after(schema, text, "e: ", "Either<T, T>"), DERIVES),
        (named(schema, text, "= ", "A", 0), "this member"),
        (named(schema, text, "| ", "B", 0), "the same type here"),
      ]
    );
  }
}

/// An application's report of a default it leaves out claims the counts beneath it once, however
/// many applications leave it out: each of many `@d<String>` leaves out `a`, whose `{}` holds a
/// copy of itself through `again` and leaves out many fields whose `1` does not fit `String`. Each
/// application reports its default, the cycle is reported once, and the `Rec<String>` the
/// applications derive passes over what the reports own, within a budget linear in the program.
/// Refuses claims that walk the same counts again for each application.
#[test]
fn many_applications_of_one_reported_default_claim_its_counts_once() {
  const M: usize = 200;
  const N: usize = 200;
  let fields: String = (0..N).map(|k: usize| format!("  f{k}: T = 1\n")).collect();
  let uses: String = (0..M)
    .map(|j: usize| format!("  x{j}: Int @d<String>\n"))
    .collect();
  let text = format!(
    "type Query {{\n{uses}}}\ninput Rec<T> {{\n  again: Rec<T> = {{}}\n{fields}}}\n\
     directive @d<T>(a: Rec<T> = {{}}) on FIELD_DEFINITION\n"
  );
  let budget = u32::try_from(16 * (M + N)).expect("a small budget");
  let analysis = analyze_text(&text, &Limits::new().with_max_value_steps(budget));
  assert_eq!(
    codes_at(&analysis, DEFAULT).len(),
    M,
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(codes_at(&analysis, CYCLE).len(), 1);
  assert_eq!(analysis.diagnostics().len(), M + 1);
}

/// Two phases that each run out of their own budget at one rebuild each say so: with no value
/// step, the closure of the written `U<Int>` stops rebuilding its member `Box<[T]>`, and with one
/// implementation step fewer than its comparison needs, the comparison of `U<Int>`'s members stops
/// on the same rebuild. Raising either budget clears only its own report. Refuses two budgets taken
/// for one stop.
#[test]
fn two_phases_out_of_their_own_budgets_at_one_rebuild_each_report_it() {
  let text = "type Query {\n  u: U<Int>\n}\ntype Box<T> {\n  f(x: T = null): Int\n}\n\
              union U<T> = Box<[T]>\n";
  let least = (0..256_u32)
    .find(|&steps: &u32| {
      let analysis = analyze_text(text, &Limits::new().with_max_implementation_steps(steps));
      codes_at(&analysis, LIMIT).is_empty()
    })
    .expect("an implementation budget the program says no limit at");
  assert!(least > 0);
  let limits = Limits::new()
    .with_max_value_steps(0)
    .with_max_implementation_steps(least - 1);
  let analysis = analyze_text(text, &limits);
  let schema = schema_file(&analysis);
  let occurrence = at(schema, text, "U<Int>", 0);
  assert_eq!(codes_at(&analysis, LIMIT), [occurrence, occurrence]);
  assert_eq!(analysis.diagnostics().len(), 2);
  let messages: Vec<String> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.to_string())
    .collect();
  assert!(
    messages
      .iter()
      .any(|message: &String| message.contains("`Limits::max_value_steps`")),
    "{messages:#?}"
  );
  assert!(
    messages
      .iter()
      .any(|message: &String| message.contains("`Limits::max_implementation_steps`")),
    "{messages:#?}"
  );
}

/// A rebuild the written check of an instantiation stopped at, which the closure of that root then
/// meets again, is reported once: with one instantiation fewer than the program needs, the check
/// of `Outer<i32>`'s default of its field's argument cannot rebuild `Box<T>` there, and says so,
/// and the closure, reading the same position, stops on the same rebuild and says nothing more.
/// Refuses one stopped rebuild reported twice at one occurrence.
#[test]
fn a_rebuild_the_written_check_reported_is_not_reported_again_by_the_closure() {
  let text = "type Query {\n  o: Outer<i32>\n}\ntype Outer<T> {\n  f(arg: Box<T> = {}): Int\n}\n\
              input Box<T> {\n  v: T = null\n}\n";
  let least = (0..64_u32)
    .find(|&bound: &u32| {
      let analysis = analyze_text(text, &Limits::new().with_max_instantiations(bound));
      codes_at(&analysis, LIMIT).is_empty()
    })
    .expect("a bound the program says no limit at");
  assert!(least > 0);
  let short = analyze_text(text, &Limits::new().with_max_instantiations(least - 1));
  let schema = schema_file(&short);
  assert_eq!(found(&short), [(LIMIT, at(schema, text, "Outer<i32>", 0))]);
}

/// A type the walk paid for is reached before the next is charged, so a budget that stops the walk
/// loses nothing it paid for: `Outer<Int!>` reads `Pair<Page<U>, Box<U>>`, whose `Page<Int!>` makes
/// `items`'s `after` required. Under the least budget the program says no limit at, less one, the
/// walk pays for `Page<Int!>` and not for `Box<Int!>`, which ends the closure at `Outer<Int!>`; the
/// `Page<Int!>` it paid for is derived all the same, and refused there by the implementation
/// check, which pays from its own budget. Refuses a paid child lost when a later sibling cannot
/// pay.
#[test]
fn a_type_the_walk_paid_for_is_derived_though_the_next_cannot_be_paid() {
  let text = "type Query {\n  o: Outer<Int!>\n}\ninterface Paged {\n  items: [Int]\n}\n\
              type Page<T> implements Paged {\n  items(after: T): [Int]\n}\n\
              type Box<T> {\n  v: T\n}\ntype Pair<A, B> {\n  a: A\n  b: B\n}\n\
              type Outer<U> {\n  p: Pair<Page<U>, Box<U>>\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let outer = at(schema, text, "Outer<Int!>", 0);
  assert_eq!(found(&analysis), [(UNEXPECTED, outer)]);
  let least = (0..256_u32)
    .find(|&steps: &u32| {
      let analysis = analyze_text(text, &Limits::new().with_max_value_steps(steps));
      codes_at(&analysis, LIMIT).is_empty()
    })
    .expect("a budget the program says no limit at");
  assert!(least > 0);
  let short = analyze_text(text, &Limits::new().with_max_value_steps(least - 1));
  assert_eq!(found(&short), [(LIMIT, outer), (UNEXPECTED, outer)]);
  assert!(
    the(&short, LIMIT, outer)
      .to_string()
      .starts_with("what `Outer` derives under the type arguments it is instantiated with here"),
    "{:#?}",
    short.diagnostics()
  );
}

/// An argument of a field that implements no interface's field makes nothing relevant: `Page<T>`
/// implements `Node`, whose one field is `id`, and the `x` its `extra` takes, typed by its bare
/// nullable parameter, is no argument the implementation check ever judges. With no step to spend,
/// `Outer<Int>`, which holds `Page<T>`, says nothing. Refuses a definition made relevant by an
/// unrelated field.
#[test]
fn an_argument_of_a_field_that_implements_nothing_makes_nothing_relevant() {
  let text = "type Query {\n  o: Outer<Int>\n}\ninterface Node {\n  id: ID\n}\n\
              type Page<T> implements Node {\n  id: ID\n  extra(x: T): Int\n}\n\
              type Outer<T> {\n  p: Page<T>\n}\n";
  quiet(&analyze_text(text, &Limits::new().with_max_value_steps(0)));
  quiet(&analyze_text(text, &Limits::new()));
}

/// A limit met at one place by two paths is said once: at depth 3, `@d<[[Int]]>` leaves out `x`,
/// whose default `{}` would be a `Box<[[[Int]]]>`, one level too deep. Coercing the application
/// says so at its path, and the closure, whose root it is, stops on the same rebuild there and says
/// nothing more. Refuses one limit reported twice at one place.
#[test]
fn an_omitted_argument_and_the_closure_say_one_depth_stop_once() {
  let text = "type Query @d<[[Int]]> {\n  x: Int\n}\n\
              directive @d<T>(x: Box<[T]> = {}) on OBJECT\ninput Box<V> {\n  v: V = null\n}\n";
  quiet(&analyze_text(text, &Limits::new()));
  let analysis = analyze_text(text, &Limits::new().with_max_generic_depth(3));
  let schema = schema_file(&analysis);
  let path = named(schema, text, "@", "d", 0);
  assert_eq!(found(&analysis), [(LIMIT, path)]);
  assert!(
    the(&analysis, LIMIT, path)
      .to_string()
      .contains("`Limits::max_generic_depth`, 3"),
    "{:#?}",
    analysis.diagnostics()
  );
}

/// A limit on types met at one place by two phases is said once: at depth 3, `O` implements
/// `I<[[Int]]>`, and `I<T>` implements `J<[T]>`, whose `J<[[[Int]]]>` nests one level too deep.
/// The closure of `I<[[Int]]>`, which reads `J`'s default through that entry, stops there and says
/// so; the implementation check, which needs the same type to check what `O` implements through
/// `I`, says nothing more. Refuses one limit reported twice at one place.
#[test]
fn the_closure_and_the_implementation_check_say_one_depth_stop_once() {
  let text = "type Query {\n  x: Int\n}\ninterface J<V> {\n  f(a: V = null): Int\n}\n\
              interface I<T> implements J<[T]> {\n  f(a: [T] = null): Int\n}\n\
              type O implements I<[[Int]]> {\n  f(a: [[[Int]]] = null): Int\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_generic_depth(3));
  let schema = schema_file(&analysis);
  let entry = at(schema, text, "I<[[Int]]>", 0);
  assert_eq!(found(&analysis), [(LIMIT, entry)]);
  assert!(
    the(&analysis, LIMIT, entry)
      .to_string()
      .contains("`Limits::max_generic_depth`, 3"),
    "{:#?}",
    analysis.diagnostics()
  );
}

/// The relevance scan reads the first of a definition's own fields of each name, as the
/// implementation check does, and its lookups across the entries are bounded linearly in the
/// program: `I` has many fields named `f`, each declaring `a`, and `D<T>`, which implements it, as
/// many, each taking `a: T`. `D` adds no argument, so it is not relevant, nor is `Outer`, which
/// holds a `D<T>`, and with no value step to spend `Outer<Int>` says nothing of what it derives.
/// Refuses a scan that compares each of `D`'s fields of a name with each of `I`'s, whose lookups
/// outrun the bound and leave `D` relevant.
#[test]
fn a_definitions_fields_of_one_name_are_read_once_by_the_relevance_scan() {
  const N: usize = 200;
  let declares: String = (0..N).map(|_: usize| "  f(a: Int): Int\n").collect();
  let takes: String = (0..N).map(|_: usize| "  f(a: T): Int\n").collect();
  let text = format!(
    "type Query {{\n  o: Outer<Int>\n}}\ninterface I {{\n{declares}}}\n\
     type D<T> implements I {{\n{takes}}}\ntype Outer<T> {{\n  d: D<T>\n}}\n"
  );
  for limits in [Limits::new().with_max_value_steps(0), Limits::new()] {
    let analysis = analyze_text(&text, &limits);
    assert!(
      codes_at(&analysis, LIMIT).is_empty(),
      "{:#?}",
      analysis.diagnostics()
    );
  }
}

/// The `resource-limit`s a diagnostic list holds at `at`, each as it reads.
fn limits_at(analysis: &Analysis, at: Location) -> Vec<String> {
  analysis
    .diagnostics()
    .iter()
    .filter(|diagnostic: &&Diagnostic| {
      diagnostic.code().as_str() == LIMIT && diagnostic.primary() == at
    })
    .map(|diagnostic: &Diagnostic| diagnostic.to_string())
    .collect()
}

/// Whether `analysis` says a `resource-limit` that names `limit`.
fn says_limit(analysis: &Analysis, limit: &str) -> bool {
  analysis
    .diagnostics()
    .iter()
    .any(|diagnostic: &Diagnostic| {
      diagnostic.code().as_str() == LIMIT && diagnostic.to_string().contains(limit)
    })
}

/// Each occurrence of a root decides by its own place, and by the kind of each stop, whether that
/// stop was said: at depth 2, `Outer<[i32]>`, written twice, derives `Bad<[i32]>`, whose `[]` takes
/// a `[[[i32]]]`, one level too deep, so its check stops on depth. Under one value step less than
/// the closure needs, the walk below `Bad<[i32]>` then stops on steps. The first `Outer<[i32]>`
/// says both, the closure's steps and the check's depth, and the second says the depth it reaches
/// too. Refuses a closure's stop taken for a stop of another kind, and a stop dropped at a repeated
/// occurrence of its root.
#[test]
fn each_occurrence_of_a_root_says_each_kind_of_stop_it_reaches() {
  let text = "type Query {\n  f(a: Outer<[i32]>): Int\n  g(a: Outer<[i32]>): Int\n}\n\
              input Bad<U> {\n  v: [[U]] = []\n}\ninput Outer<T> {\n  b: Bad<T>\n}\n";
  quiet(&analyze_text(text, &Limits::new()));
  let depth = Limits::new().with_max_generic_depth(2);
  let least = (0..256_u32)
    .find(|&steps: &u32| {
      let analysis = analyze_text(text, &depth.with_max_value_steps(steps));
      !says_limit(&analysis, "`Limits::max_value_steps`")
    })
    .expect("a budget the closure is not stopped at");
  assert!(least > 0);
  let short = analyze_text(text, &depth.with_max_value_steps(least - 1));
  let schema = schema_file(&short);
  let first = at(schema, text, "Outer<[i32]>", 0);
  let second = at(schema, text, "Outer<[i32]>", 1);
  assert_eq!(
    found(&short),
    [(LIMIT, first), (LIMIT, first), (LIMIT, second)]
  );
  let said = limits_at(&short, first);
  for limit in [
    "`Limits::max_value_steps`",
    "`Limits::max_generic_depth`, 2",
  ] {
    assert!(
      said.iter().any(|message: &String| message.contains(limit)),
      "{said:#?}"
    );
  }
  let said = limits_at(&short, second);
  assert!(
    said[0].contains("`Limits::max_generic_depth`, 2"),
    "{said:#?}"
  );
}

/// An entry says each kind of stop its checks meet there, once each: at depth 2, `O`'s check of
/// `I<[i32]>` cannot build `[[[i32]]]`, `I`'s `g` under it, and says so at the entry. Under one
/// implementation step less than the program needs, the comparison of `I<[i32]>`'s entries, made
/// after it, stops on steps at the same entry, and says so too. Refuses an entry whose stop hides a
/// stop of another kind there.
#[test]
fn an_entry_says_each_kind_of_stop_its_checks_meet() {
  let text = "type Query {\n  x: Int\n}\ninterface J<V> {\n  f: Int\n}\n\
              interface I<T> implements J<T> {\n  f: Int\n  g: [[T]]\n}\n\
              type O implements I<[i32]> & J<[i32]> {\n  f: Int\n  g: Int\n}\n";
  let depth = Limits::new().with_max_generic_depth(2);
  let least = (0..256_u32)
    .find(|&steps: &u32| {
      let analysis = analyze_text(text, &depth.with_max_implementation_steps(steps));
      !says_limit(&analysis, "`Limits::max_implementation_steps`")
    })
    .expect("a budget the implementation check is not stopped at");
  assert!(least > 0);
  let short = analyze_text(text, &depth.with_max_implementation_steps(least - 1));
  let schema = schema_file(&short);
  let entry = at(schema, text, "I<[i32]>", 0);
  assert_eq!(found(&short), [(LIMIT, entry), (LIMIT, entry)]);
  let said = limits_at(&short, entry);
  for limit in [
    "`Limits::max_generic_depth`, 2",
    "`Limits::max_implementation_steps`",
  ] {
    assert!(
      said.iter().any(|message: &String| message.contains(limit)),
      "{said:#?}"
    );
  }
}

/// A limit on types that stops a rebuild ends the expansion there, a boundary of this version: what
/// fails inside the types that rebuild had built in part is not guaranteed a report, and raising
/// the limit shows it. At depth 2, `Outer<[i32]>` reads `Envelope<Bad<T>>`, whose `Bad<[i32]>` is
/// built and whose `Envelope<Bad<[i32]>>` is one level too deep: `Outer<[i32]>` is
/// `resource-limit`, and `Bad<[i32]>`'s `"text"`, which does not fit, is not reported. Under the
/// default limits it is, at `Outer<[i32]>`. Refuses a report that walks a stopped rebuild's parts.
#[test]
fn what_a_stopped_rebuild_built_in_part_is_not_judged() {
  let text = "type Query {\n  f(a: Outer<[i32]>): Int\n}\ninput Bad<U> {\n  v: U = \"text\"\n}\n\
              input Envelope<E> {\n  e: E\n}\ninput Outer<T> {\n  x: Envelope<Bad<T>>\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let outer = at(schema, text, "Outer<[i32]>", 0);
  assert_eq!(found(&analysis), [(DEFAULT, outer)]);
  assert_eq!(
    labels(the(&analysis, DEFAULT, outer)),
    [
      (after(schema, text, "x: ", "Envelope<Bad<T>>"), DERIVES),
      (after(schema, text, "v: U = ", "\"text\""), "this value"),
    ]
  );
  let short = analyze_text(text, &Limits::new().with_max_generic_depth(2));
  assert_eq!(found(&short), [(LIMIT, outer)]);
  assert!(
    the(&short, LIMIT, outer)
      .to_string()
      .contains("`Limits::max_generic_depth`, 2"),
    "{:#?}",
    short.diagnostics()
  );
}

/// A default its declaration refuses, a whole `null` no instantiation takes, has that one owner
/// through a derived instantiation too: `In<T>`'s `a`, written `T!`, defaults to `null`, refused
/// where it is declared and recorded for none. `Outer<T>`'s `i` defaults to `{}`, which leaves `a`
/// out, and `Wrap<Int>` derives `Outer<Int>`, whose check copies that `null` against `Int!`: the
/// copy is the declaration's, makes the `{}` no less fit, and is said nothing of at `Wrap<Int>`.
/// Refuses a refused `null` reported again through what a written instantiation derives.
#[test]
fn a_default_its_declaration_refuses_is_not_reported_through_a_derived_instantiation() {
  let text = "type Query {\n  f(w: Wrap<Int>): Int\n}\ninput In<T> {\n  a: T! = null\n}\n\
              input Outer<T> {\n  i: In<T> = {}\n}\ninput Wrap<T> {\n  o: Outer<T>\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(DEFAULT, after(schema, text, "a: T! = ", "null"))]
  );
}

/// What another package writes into a definition is not the implementation check's, nor the
/// relevance scan's: the dependency `dep`'s `P<T>`, whose `f` takes `extra: T`, implements `I` only
/// through the root package's extension, an orphan the check does not read, so `extra` is no
/// argument the check ever judges, and `P` is not relevant. With no value step to spend,
/// `Outer<Int>`, which holds a `P<T>`, says nothing of what it derives. Refuses a foreign entry
/// read as the definition's own.
#[test]
fn a_foreign_implements_entry_makes_nothing_relevant() {
  let theirs = "interface I {\n  f: Int\n}\ntype P<T> {\n  f(extra: T): Int\n}\n";
  let ours = "type Query {\n  o: Outer<Int>\n}\ntype Outer<T> {\n  p: ^::dep::P<T>\n}\n\
              extend type ^::dep::P<T> implements ^::dep::I\n";
  let root = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", ours)])
    .with_dependencies(["dep"]);
  let dependencies = [Package::new("dep", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("dep.graphqlx", theirs)])];
  for limits in [Limits::new().with_max_value_steps(0), Limits::new()] {
    let analysis = analyze(&root, &dependencies, &limits);
    assert!(
      codes_at(&analysis, LIMIT).is_empty(),
      "{:#?}",
      analysis.diagnostics()
    );
  }
}

/// A marker's fields are none of the implementation check's: `Page<T>` implements the marker `M`,
/// which declares `f` and is `marker-misuse` there, and `Page`'s `f` takes `extra: T`, which the
/// check never compares with anything, so `Page` is not relevant. With no value step to spend,
/// `Outer<Int>`, which holds a `Page<T>`, says nothing of what it derives. Refuses a marker's field
/// read as an obligation.
#[test]
fn a_markers_field_makes_nothing_relevant() {
  let text = "type Query {\n  o: Outer<Int>\n}\ninterface M @marker {\n  f: Int\n}\n\
              type Page<T> implements M {\n  f(extra: T): Int\n}\n\
              type Outer<T> {\n  p: Page<T>\n}\n";
  for limits in [Limits::new().with_max_value_steps(0), Limits::new()] {
    let analysis = analyze_text(text, &limits);
    let schema = schema_file(&analysis);
    assert!(
      codes_at(&analysis, LIMIT).is_empty(),
      "{:#?}",
      analysis.diagnostics()
    );
    assert_eq!(
      codes_at(&analysis, MARKER_MISUSE),
      [named(schema, text, "  ", "f", 0)]
    );
  }
}

/// An argument name written more than once is one name: `Page<T>` implements `I`, whose `f`
/// declares `a`, and `Page`'s `f` writes `a` three times, `duplicate-argument-name`, twice typed
/// `T`. The implementation check passes over every `a`, which `I`'s `f` declares, so `Page` is not
/// relevant, and with no value step to spend `Outer<Int>` says nothing of what it derives. Refuses
/// a repeated name counted as more arguments added than the interface's field declares.
#[test]
fn an_argument_name_written_twice_is_one_name_to_the_relevance_scan() {
  let text = "type Query {\n  o: Outer<Int>\n}\ninterface I {\n  f(a: Int): Int\n}\n\
              type Page<T> implements I {\n  f(a: Int, a: T, a: T): Int\n}\n\
              type Outer<T> {\n  p: Page<T>\n}\n";
  for limits in [Limits::new().with_max_value_steps(0), Limits::new()] {
    let analysis = analyze_text(text, &limits);
    assert!(
      codes_at(&analysis, LIMIT).is_empty(),
      "{:#?}",
      analysis.diagnostics()
    );
    assert!(
      !codes_at(&analysis, DUPLICATE_ARGUMENT).is_empty(),
      "{:#?}",
      analysis.diagnostics()
    );
  }
}

/// An analysis of the root package `app`, its one file `schema.graphqlx` holding `ours`, beside the
/// dependency `dep`, its one file `dep.graphqlx` holding `theirs`, within `limits`.
fn analyze_beside(ours: &str, theirs: &str, limits: &Limits) -> Analysis {
  let root = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", ours)])
    .with_dependencies(["dep"]);
  let dependencies = [Package::new("dep", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("dep.graphqlx", theirs)])];
  analyze(&root, &dependencies, limits)
}

/// Each diagnostic of `analysis` in the dependency `dep`'s file, its code and where it is, in the
/// list's order.
fn in_dependency(analysis: &Analysis) -> Vec<(&'static str, Location)> {
  let sources = analysis.sources();
  let file = analysis
    .model()
    .package(PackageId::new(4))
    .sources()
    .into_iter()
    .find(|id: &SourceId| sources.source(*id).path() == "dep.graphqlx")
    .expect("the dependency's file");
  found(analysis)
    .into_iter()
    .filter(|&(_, at): &(&'static str, Location)| at.source() == file.get())
    .collect()
}

/// Asserts that `orphan`, an extension the root package writes of a definition of the dependency
/// holding `theirs`, beside `ours`, changes nothing said in the dependency's file, under the
/// default limits and with no value step to spend, and that the root package's file says
/// something of it. Under the default limits that is the orphan rule's refusal. With no step to
/// spend, a phase may stop inside the extension, at a value whose type it cannot read under the
/// extension's type arguments, and the orphan rule says nothing of an extension a phase found an
/// error in: so the root package's file holds a report inside the extension, the orphan rule's or
/// the phase's.
fn an_orphan_changes_nothing_in_the_dependency(theirs: &str, ours: &str, orphan: &str) {
  let extended = format!("{ours}{orphan}");
  for (limits, ruled) in [
    (Limits::new(), true),
    (Limits::new().with_max_value_steps(0), false),
  ] {
    let without = analyze_beside(ours, theirs, &limits);
    let with = analyze_beside(&extended, theirs, &limits);
    if ruled {
      assert!(
        !codes_at(&with, ORPHAN).is_empty(),
        "{:#?}",
        with.diagnostics()
      );
    } else {
      // The extension is written after `ours`, in the root package's file.
      let file = schema_file(&with).get();
      assert!(
        found(&with)
          .iter()
          .any(|&(_, at): &(&'static str, Location)| at.source() == file
            && at
              .span()
              .is_some_and(|span: SimpleSpan| span.start() >= ours.len())),
        "{:#?}",
        with.diagnostics()
      );
    }
    assert_eq!(
      in_dependency(&with),
      in_dependency(&without),
      "{orphan}{:#?}",
      with.diagnostics()
    );
  }
}

/// A field, with an argument, that another package's extension adds to a dependency's generic type
/// derives nothing: `arg: Bad<T>` of the root package's `extra` on `^::dep::Box<T>` would derive
/// `Bad<Int>` from the dependency's `Box<Int>`, whose `"text"` does not fit. Refuses a refused
/// member, or its argument, read as a position.
#[test]
fn an_orphan_field_with_an_argument_derives_nothing_in_the_dependency() {
  an_orphan_changes_nothing_in_the_dependency(
    "type Box<T> {\n  v: T\n}\ntype Holder {\n  b: Box<Int>\n}\n",
    "type Query {\n  x: Int\n}\ninput Bad<U> {\n  v: U = \"text\"\n}\n",
    "extend type ^::dep::Box<T> {\n  extra(arg: Bad<T>): Int\n}\n",
  );
}

/// An `implements` entry another package's extension adds to a dependency's generic type derives
/// nothing: `implements I<T>` on `^::dep::Box<T>` would derive `I<Boolean>` from the dependency's
/// `Box<Boolean>`, whose `arg` defaults to `0`. Refuses a refused entry read as a position.
#[test]
fn an_orphan_implements_entry_derives_nothing_in_the_dependency() {
  an_orphan_changes_nothing_in_the_dependency(
    "type Box<T> {\n  v: T\n}\ntype Holder {\n  b: Box<Boolean>\n}\n",
    "type Query {\n  x: Int\n}\ninterface I<X> {\n  f(arg: X = 0): Int\n}\n",
    "extend type ^::dep::Box<T> implements I<T>\n",
  );
}

/// A directive application another package's extension adds to a dependency's generic type derives
/// nothing: `@d<T>` on `^::dep::Box<T>` would derive `d<Int>`, and through `v`'s type `Bad<Int>`,
/// from the dependency's `Box<Int>`. Refuses a refused application read as a position.
#[test]
fn an_orphan_application_on_a_type_derives_nothing_in_the_dependency() {
  an_orphan_changes_nothing_in_the_dependency(
    "type Box<T> {\n  v: T\n}\ntype Holder {\n  b: Box<Int>\n}\n",
    "type Query {\n  x: Int\n}\ninput Bad<U> {\n  v: U = \"text\"\n}\n\
     directive @d<X>(v: Bad<X>) on OBJECT\n",
    "extend type ^::dep::Box<T> @d<T>\n",
  );
}

/// An application on a field another package's extension adds to a dependency's generic type
/// derives nothing: `@d<T>` on the root package's `extra` would derive `d<Int>`, and `Bad<Int>`,
/// from the dependency's `Box<Int>`. Refuses a refused member's application read as a position.
#[test]
fn an_orphan_application_on_a_field_derives_nothing_in_the_dependency() {
  an_orphan_changes_nothing_in_the_dependency(
    "type Box<T> {\n  v: T\n}\ntype Holder {\n  b: Box<Int>\n}\n",
    "type Query {\n  x: Int\n}\ninput Bad<U> {\n  v: U = \"text\"\n}\n\
     directive @d<X>(v: Bad<X>) on FIELD_DEFINITION\n",
    "extend type ^::dep::Box<T> {\n  extra: Int @d<T>\n}\n",
  );
}

/// A default another package's extension writes in a dependency's generic type, of the type `T`,
/// is recorded for no instantiation: `"x"`, the default of `arg` on the root package's `extra` on
/// `^::dep::Box<T>`, would be judged at the dependency's own written `Box<Int>`, and refused there.
/// Written by the dependency itself, the same default is. Refuses a refused member's argument's
/// value recorded under the definition the contribution extends.
#[test]
fn an_orphan_fields_argument_default_is_judged_at_no_instantiation() {
  let theirs = "type Box<T> {\n  v: T\n}\ntype Holder {\n  b: Box<Int>\n}\n";
  let ours = "type Query {\n  x: Int\n}\n";
  an_orphan_changes_nothing_in_the_dependency(
    theirs,
    ours,
    "extend type ^::dep::Box<T> {\n  extra(arg: T = \"x\"): Int\n}\n",
  );
  let own = format!("{theirs}extend type Box<T> {{\n  extra(arg: T = \"x\"): Int\n}}\n");
  let analysis = analyze_beside(ours, &own, &Limits::new());
  assert!(
    in_dependency(&analysis)
      .iter()
      .any(|&(code, _): &(&'static str, Location)| code == DEFAULT),
    "{:#?}",
    analysis.diagnostics()
  );
}

/// An argument another package's extension writes in an application on a dependency's generic
/// type, of the type `T`, is recorded for no instantiation: `"x"`, the argument of the root
/// package's `@d<T>` on `^::dep::Box<T>`, would be judged at the dependency's own written
/// `Box<Int>`, as `d<Int>`'s, and refused there. Written by the dependency itself, the same
/// argument is. Refuses a refused application's argument recorded under the definition the
/// contribution extends.
#[test]
fn an_orphan_applications_argument_is_judged_at_no_instantiation() {
  let theirs = "type Box<T> {\n  v: T\n}\ntype Holder {\n  b: Box<Int>\n}\n";
  let ours = "type Query {\n  x: Int\n}\n";
  an_orphan_changes_nothing_in_the_dependency(
    theirs,
    &format!("{ours}directive @d<X>(v: X) on OBJECT\n"),
    "extend type ^::dep::Box<T> @d<T>(v: \"x\")\n",
  );
  let own =
    format!("{theirs}directive @d<X>(v: X) on OBJECT\nextend type Box<T> @d<T>(v: \"x\")\n");
  let analysis = analyze_beside(ours, &own, &Limits::new());
  assert!(
    in_dependency(&analysis)
      .iter()
      .any(|&(code, _): &(&'static str, Location)| code == ARGUMENT),
    "{:#?}",
    analysis.diagnostics()
  );
}

/// A copy of a default another package's extension writes in a dependency's generic input object
/// is reported at the object literal that leaves its field out, whether or not a supplied package
/// writes the literal's type: the root package's refused extension gives `^::dep::Box<U>` the field
/// `extra: U = 1`, and `@take<String>(x: {})` copies `1` against `String`. The check where
/// `Box<String>` is written records no default a refused contribution writes, so the literal leaves
/// it no copy. Refuses a literal that leaves its copy to a check that holds no row of it.
#[test]
fn a_refused_defaults_copy_is_reported_at_the_literal_whatever_is_written() {
  let theirs = "input Box<T> {\n  value: T\n}\n";
  let rest = "directive @take<T>(x: ^::dep::Box<T>) on OBJECT\n\
              extend input ^::dep::Box<U> {\n  extra: U = 1\n}\n";
  // No type occurrence writes `Box<String>`, and then an argument's type does.
  for query in [
    "type Query @take<String>(x: {}) {\n  ok: Int\n}\n",
    "type Query @take<String>(x: {}) {\n  ok(b: ^::dep::Box<String>): Int\n}\n",
  ] {
    let ours = format!("{query}{rest}");
    let analysis = analyze_beside(&ours, theirs, &Limits::new());
    let schema = schema_file(&analysis);
    assert_eq!(
      codes_at(&analysis, DEFAULT),
      [after(schema, &ours, "x: ", "{}")],
      "{:#?}",
      analysis.diagnostics()
    );
    assert!(
      !codes_at(&analysis, ORPHAN).is_empty(),
      "{:#?}",
      analysis.diagnostics()
    );
  }
}

/// What another package writes into a dependency's definition grows nothing: the root package's
/// `implements Loop<[T]>` on `^::dep::I<T>`, where `Loop<X>` holds an `I<X>`, would close a cycle
/// that wraps `T`, and make `I` grow, so that the `I<Boolean>` the dependency's `Wrap<Boolean>`
/// derives, whose `x` defaults to `0`, would no longer be judged. `I` grows in neither analysis,
/// and the dependency's `invalid-default-value` is said in both. Refuses a refused entry read by
/// the growth graph.
#[test]
fn an_orphan_implements_entry_grows_nothing_in_the_dependency() {
  let theirs = "interface I<T> {\n  f(x: T = 0): Int\n}\ntype Wrap<T> {\n  i: I<T>\n}\n\
                type Holder {\n  w: Wrap<Boolean>\n}\n";
  let ours = "type Query {\n  x: Int\n}\ninterface Loop<X> {\n  item: ^::dep::I<X>\n}\n";
  let orphan = "extend interface ^::dep::I<T> implements Loop<[T]>\n";
  an_orphan_changes_nothing_in_the_dependency(theirs, ours, orphan);
  for text in [ours.to_owned(), format!("{ours}{orphan}")] {
    let analysis = analyze_beside(&text, theirs, &Limits::new());
    assert!(
      in_dependency(&analysis)
        .iter()
        .any(|&(code, _): &(&'static str, Location)| code == DEFAULT),
      "{:#?}",
      analysis.diagnostics()
    );
    let model = analysis.model();
    let interface = model
      .lookup(Namespace::Type, "^::dep::I")
      .expect("the dependency's `I`");
    assert!(!model.def(interface).grows(), "{text}");
  }
}

/// The one extension of another package's definition the orphan rule accepts, the root package's
/// of a built-in that adds directives and nothing else, is read as the definition's own: the root
/// package's `@tag<U>` on `std`'s `HashSet<U>` has the open type `tag<T>`, so `HashSet<Int>`
/// derives `tag<Int>`, and through `extra`'s type `Wrap<Int>`, whose `"s"` does not fit, reported
/// at `HashSet<Int>`. Refuses an accepted application on a built-in left out of the closure.
#[test]
fn the_root_packages_application_on_a_generic_built_in_is_still_expanded() {
  let text = "type Query {\n  f(s: HashSet<Int>): Int\n}\ninput Wrap<W> {\n  w: W = \"s\"\n}\n\
              directive @tag<X>(value: X, extra: Wrap<X>) on SCALAR\n\
              extend scalar HashSet<U> @tag<U>(value: 1)\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let set = at(schema, text, "HashSet<Int>", 0);
  assert_eq!(found(&analysis), [(DEFAULT, set)]);
  assert_eq!(
    labels(the(&analysis, DEFAULT, set)),
    [
      (after(schema, text, "extra: ", "Wrap<X>"), DERIVES),
      (after(schema, text, "w: W = ", "\"s\""), "this value"),
    ]
  );
}

/// The orphan rule's predicate is read before the model is finished, by the growth graph and by
/// the closure of derived instantiations, when the model's built-ins answer nothing yet: the four
/// directives it refuses in the root package's extension of a built-in are read off `core`'s
/// declarations. The root package's extension of `std`'s generic `HashSet<U>` that applies an
/// ordinary directive is quiet; one that applies `@map` beside it is refused where `@map` is
/// written, as the orphan rule says, and nothing else. Refuses a phase that asks the unfinished
/// model for its built-ins, which panics.
#[test]
fn an_extension_of_a_generic_built_in_is_judged_before_the_model_is_finished() {
  let ordinary = "type Query {\n  f(s: HashSet<Int>): Int\n}\n\
                  directive @tag<X>(value: X) on SCALAR\n\
                  extend scalar HashSet<U> @tag<U>(value: 1)\n";
  quiet(&analyze_text(ordinary, &Limits::new()));

  let refused = "type Query {\n  f(s: HashSet<Int>): Int\n}\n\
                 directive @tag<X>(value: X) on SCALAR\n\
                 extend scalar HashSet<U> @tag<U>(value: 1) @map\n";
  let analysis = analyze_text(refused, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(ORPHAN, at(schema, refused, "map", 0))],
    "{:#?}",
    analysis.diagnostics()
  );
}
