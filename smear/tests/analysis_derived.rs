//! Derived instantiations, design Z's D11: which generic definitions grow, one fact that the
//! analysis and the opt-in rule `UnboundedInstantiation` read off one graph.
#![cfg(all(feature = "std", feature = "analysis"))]

use std::path::PathBuf;

use smear::analysis::graphqlx::{
  Analysis, DefId, Layout, Limits, Model, Package, SourceFile, Tier, analyze, test_support,
};

/// An analysis of the root package `app`, `Flat`, at tier `Std`, holding the one file
/// `schema.graphqlx`, within `limits`.
fn analyze_text(text: &str, limits: &Limits) -> Analysis {
  let root = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", text)]);
  analyze(&root, &[], limits)
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
