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
      test_support::grown_by_the_rule(model),
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
    assert_eq!(ours, test_support::grown_by_the_rule(model), "{text}");
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
