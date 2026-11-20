//! A number with a suffix and a `char` literal, before the analyzer gives each its meaning:
//! lowering reports each as an item of a kind it does not know, once for a file and at the form,
//! and lowers it as one refused node, of which coercion says nothing. A custom scalar keeps a list
//! with one inside it whole, and the list's refused node is never read.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, Diagnostic, Layout, Limits, Options, Package, PackageId, SourceFile, SourceId, Tier,
    analyze,
  },
  diagnostic::{Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

/// The code lowering reports an item of a kind it does not know with.
const UNKNOWN_ITEM: &str = "smear::graphqlx::unknown-item";

/// The code of a program with no query root, which none of these programs declares.
const NO_QUERY_ROOT: &str = "smear::schema::missing-query-root-operation-type";

/// The analysis of `text` as the one file `schema.graphqlx` of the `Flat` root package `app`.
fn analyze_text(text: &str) -> Analysis {
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", text)]);
  analyze(&root, &[], &Limits::new(), &Options::new())
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

/// Where `literal`, written right after the first `before`, is in `text`, the file `source`.
fn after(source: SourceId, text: &str, before: &str, literal: &str) -> Location {
  let written = format!("{before}{literal}");
  let start = text
    .find(written.as_str())
    .unwrap_or_else(|| panic!("`{written}` is not written"))
    + before.len();
  Location::new(source.get(), SimpleSpan::new(start, start + literal.len()))
}

/// Each diagnostic's code and primary location, in the list's order, but for the missing query
/// root.
fn found(analysis: &Analysis) -> Vec<(&'static str, Location)> {
  analysis
    .diagnostics()
    .iter()
    .filter(|diagnostic: &&Diagnostic| diagnostic.code().as_str() != NO_QUERY_ROOT)
    .map(|diagnostic: &Diagnostic| (diagnostic.code().as_str(), diagnostic.primary()))
    .collect()
}

/// One `unknown-item` at the number, and nothing else: an arm that reported and still lowered
/// the number would add `invalid-default-value` from coercion.
#[test]
fn a_suffixed_number_is_an_unknown_item_and_nothing_more() {
  for (text, before, literal) in [
    ("input I { n: u8 = 1u8 }\n", "n: u8 = ", "1u8"),
    ("input F { x: f32 = 1.5f32 }\n", "x: f32 = ", "1.5f32"),
  ] {
    let analysis = analyze_text(text);
    let schema = schema_file(&analysis);
    assert_eq!(
      found(&analysis),
      [(UNKNOWN_ITEM, after(schema, text, before, literal))],
      "{text:?}"
    );
    assert!(!analysis.is_ok(), "{text:?}: a reported number is an error");
  }
}

/// Two suffixed numbers in one file are one report, at the first, and the second reaches no
/// coercion that could report it.
#[test]
fn the_report_is_once_for_a_file() {
  let text = "input I {\n  a: u8 = 1u8\n  b: f32 = 2.5f32\n}\n";
  let analysis = analyze_text(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(UNKNOWN_ITEM, after(schema, text, "a: u8 = ", "1u8"))]
  );
}

/// A suffixed number inside a list is still one node, so the count lowering charged holds and
/// nothing panics. A custom scalar keeps the list whole, and its refused node is never read.
#[test]
fn a_nested_suffixed_number_keeps_the_count() {
  for (text, before, literal) in [
    ("input L { xs: [u8] = [1, 2u8, 3] }\n", "[1, ", "2u8"),
    (
      "scalar Json\ninput J { s: Json = [1, 2u8] }\n",
      "[1, ",
      "2u8",
    ),
  ] {
    let analysis = analyze_text(text);
    let schema = schema_file(&analysis);
    assert_eq!(
      found(&analysis),
      [(UNKNOWN_ITEM, after(schema, text, before, literal))],
      "{text:?}"
    );
  }
}

/// A suffixed number inside a `bytes` list is reported once, by lowering. The list's reading meets
/// the refused element without asking what it is, and says nothing of it, where the default is
/// written and where an instantiation judges it: the value is left unresolved, and no
/// `invalid-default-value` joins the report. An element that is no byte keeps its report.
#[test]
fn a_suffixed_byte_is_reported_once() {
  for (text, before) in [
    ("input I { data: bytes = [1u8] }\n", "data: bytes = ["),
    (
      "type Query {\n  f(b: Box<bytes>): Int\n}\ninput Box<T> {\n  v: T = [1u8]\n}\n",
      "v: T = [",
    ),
  ] {
    let analysis = analyze_text(text);
    let schema = schema_file(&analysis);
    assert_eq!(
      found(&analysis),
      [(UNKNOWN_ITEM, after(schema, text, before, "1u8"))],
      "{text:?}"
    );
  }

  let text = "input I { data: bytes = [1u8, \"a\"] }\n";
  let analysis = analyze_text(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (UNKNOWN_ITEM, after(schema, text, "[", "1u8")),
      (
        "smear::schema::invalid-default-value",
        after(schema, text, "1u8, ", "\"a\"")
      ),
    ]
  );
}

/// One `unknown-item` at the `char` literal, and nothing else: an arm that reported and still
/// lowered the literal would add `invalid-default-value` from coercion.
#[test]
fn a_char_literal_is_an_unknown_item_and_nothing_more() {
  let text = "input I { c: u8 = 'a' }\n";
  let analysis = analyze_text(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(UNKNOWN_ITEM, after(schema, text, "c: u8 = ", "'a'"))]
  );
  assert!(!analysis.is_ok(), "a reported literal is an error");
}

/// A list of `char` literals is one node for each, so the count lowering charged holds and nothing
/// panics. A custom scalar keeps the list whole, and its refused nodes are never read.
#[test]
fn a_list_of_char_literals_keeps_the_count() {
  for text in [
    "input L { cs: [u8] = ['a', 'b'] }\n",
    "scalar Json\ninput J { s: Json = ['a', 'b'] }\n",
  ] {
    let analysis = analyze_text(text);
    let schema = schema_file(&analysis);
    assert_eq!(
      found(&analysis),
      [(UNKNOWN_ITEM, after(schema, text, "[", "'a'"))],
      "{text:?}"
    );
  }
}

/// A `char` literal and a suffixed number in one file are one report, at the first.
#[test]
fn a_char_and_a_suffixed_number_are_one_report() {
  let text = "input I {\n  a: u8 = 'a'\n  b: u8 = 1u8\n}\n";
  let analysis = analyze_text(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(UNKNOWN_ITEM, after(schema, text, "a: u8 = ", "'a'"))]
  );
}

/// The two parser layers lower a suffixed number and a `char` literal alike: the same codes at the
/// same places.
#[cfg(all(feature = "rowan_0_17", feature = "std"))]
#[test]
fn both_layers_agree() {
  use smear::{analysis::graphqlx::analyze_with, parser::graphqlx::Lossless};

  for text in [
    "input I { n: u8 = 1u8 }\n",
    "input I {\n  a: u8 = 1u8\n  b: f32 = 2.5f32\n}\n",
    "input L { xs: [u8] = [1, 2u8, 3] }\n",
    "input I { data: bytes = [1u8] }\n",
    "input I { c: u8 = 'a' }\n",
    "input L { cs: [u8] = ['a', 'b'] }\n",
    "input I {\n  a: u8 = 'a'\n  b: u8 = 1u8\n}\n",
  ] {
    let syntactic = analyze_text(text);
    let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
      .with_files([SourceFile::new("schema.graphqlx", text)]);
    let (lossless, _trees) = analyze_with(
      &Lossless::new(),
      &root,
      &[],
      &Limits::new(),
      &Options::new(),
    );
    assert_eq!(found(&lossless), found(&syntactic), "{text:?}");
  }
}
