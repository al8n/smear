#![cfg(all(feature = "rowan_0_17", feature = "std", feature = "graphqlx"))]

//! The sources a syntactic layer reads are the sources its lexer reads, held as text.
//!
//! Each dialect's syntactic layer has one `Layer` impl for each source, written by one macro
//! invocation over a list, and that list is the fifth place the sources are listed: the lexer
//! closes the set with its crate-private `DelegateStringError`, which no other crate can name. A
//! source added to the lexer and to no list would be a source the layer silently does not read; a
//! source in a list that the lexer does not read fails to compile where it is written. This file
//! holds the first.
//!
//! It reads sources, as `lossless_isolation.rs` does and under that file's gate, because the
//! property is about which impls are written: a type-level test would need the private trait.

use std::path::PathBuf;

/// The lexer's file, which holds every `DelegateStringError` impl.
const LEXER: &str = "../smear-lexer/src/string_lexer/mod.rs";

/// Each dialect's layers.
const LAYERS: [&str; 2] = [
  "../smear-parser/src/graphqlx/layer.rs",
  "../smear-parser/src/graphql/layer.rs",
];

/// One source: the type as the lexer's impl names it, the type as a layer's entry names it, and
/// the feature the entry stands under.
type Row = (&'static str, &'static str, Option<&'static str>);

/// The sources the syntactic lexers read, in the order the lexer implements them.
const SOURCES: &[Row] = &[
  ("str", "str", None),
  ("[u8]", "[u8]", None),
  ("Bytes", "bytes_1::Bytes", Some("bytes_1")),
  ("HipByt<'_>", "hipstr_0_8::HipByt<'h>", Some("hipstr_0_8")),
  ("HipStr<'_>", "hipstr_0_8::HipStr<'h>", Some("hipstr_0_8")),
  (
    "smol_bytes_0_2::shared::Bytes",
    "smol_bytes_0_2::shared::Bytes",
    Some("smol-bytes_0_2"),
  ),
  (
    "smol_bytes_0_2::compact::Bytes",
    "smol_bytes_0_2::compact::Bytes",
    Some("smol-bytes_0_2"),
  ),
  (
    "smol_bytes_0_2::Utf8Bytes",
    "smol_bytes_0_2::Utf8Bytes",
    Some("smol-bytes_0_2"),
  ),
  (
    "smol_bytes_0_2::compact::Utf8Bytes",
    "smol_bytes_0_2::compact::Utf8Bytes",
    Some("smol-bytes_0_2"),
  ),
  ("BStr", "bstr_1::BStr", Some("bstr_1")),
];

/// The text of `file`, a path relative to this package.
fn text_of(file: &str) -> String {
  let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(file);
  std::fs::read_to_string(&path).unwrap_or_else(|error| {
    panic!(
      "{} is unreadable: {error}; a list read from a file that is not there holds nothing",
      path.display()
    )
  })
}

/// Every source the lexer implements `DelegateStringError` for, as its impl line names it.
fn lexer_sources() -> Vec<String> {
  const HEAD: &str = "impl DelegateStringError for ";
  let mut sources = Vec::new();
  for line in text_of(LEXER).lines() {
    let line = line.trim();
    if let Some(rest) = line.strip_prefix(HEAD) {
      let source = rest.strip_suffix(" {").unwrap_or_else(|| {
        panic!("the impl line {line:?} does not end in ` {{`, so its source cannot be read off it")
      });
      sources.push(String::from(source));
    }
  }
  sources
}

/// One entry of a `syntactic_layer!` invocation: the attribute lines above it, and its source.
struct Entry {
  attributes: Vec<String>,
  source: String,
}

/// Every entry of the one `syntactic_layer!` invocation in `file`.
fn layer_entries(file: &str) -> Vec<Entry> {
  let text = text_of(file);
  let invoked = text
    .lines()
    .filter(|line: &&str| line.trim() == "syntactic_layer! {")
    .count();
  assert_eq!(
    invoked, 1,
    "{file} invokes `syntactic_layer!` {invoked} times, and the list is one invocation"
  );

  let mut entries = Vec::new();
  let mut attributes: Vec<String> = Vec::new();
  let mut inside = false;
  let mut closed = false;
  for line in text.lines() {
    let line = line.trim();
    if !inside {
      inside = line == "syntactic_layer! {";
      continue;
    }
    if line == "}" {
      closed = true;
      break;
    }
    if line.starts_with("#[") {
      attributes.push(String::from(line));
      continue;
    }
    // An entry is `<generics> source;`. The source starts after the first `> `: an entry's
    // generics are lifetimes, which hold no `>`.
    let entry = line
      .strip_prefix('<')
      .and_then(|rest: &str| rest.split_once("> "))
      .and_then(|(_, source): (&str, &str)| source.strip_suffix(';'));
    let Some(source) = entry else {
      panic!("{file}: {line:?} is neither an attribute nor an entry of the list");
    };
    entries.push(Entry {
      attributes: core::mem::take(&mut attributes),
      source: String::from(source),
    });
  }
  assert!(closed, "{file}: the invocation is never closed");
  assert!(
    attributes.is_empty(),
    "{file}: {attributes:?} stand above no entry"
  );
  entries
}

/// The attribute lines an entry under `feature` carries: its gate, and the same gate for the
/// documentation.
fn attributes_of(feature: Option<&str>) -> Vec<String> {
  match feature {
    None => Vec::new(),
    Some(feature) => vec![
      format!("#[cfg(feature = \"{feature}\")]"),
      format!("#[cfg_attr(docsrs, doc(cfg(feature = \"{feature}\")))]"),
    ],
  }
}

#[test]
fn the_lexer_reads_exactly_the_sources_of_the_table() {
  let found = lexer_sources();
  let expected: Vec<String> = SOURCES
    .iter()
    .map(|row: &Row| String::from(row.0))
    .collect();
  for source in &found {
    assert!(
      expected.contains(source),
      "the lexer reads `{source}`, which the table does not hold: no syntactic layer has an impl \
       over it. Add it to the table, and an entry to each dialect's `syntactic_layer!`"
    );
  }
  assert_eq!(
    found, expected,
    "the lexer's `DelegateStringError` impls are not the table's sources, in the lexer's order"
  );
  assert_eq!(found.len(), 10, "ten sources");
}

#[test]
fn each_dialects_layer_has_exactly_the_lexers_sources_each_under_its_feature() {
  for file in LAYERS {
    let entries = layer_entries(file);
    for row in SOURCES {
      let (_, source, feature) = *row;
      let matching: Vec<&Entry> = entries
        .iter()
        .filter(|entry: &&Entry| entry.source == source)
        .collect();
      assert_eq!(
        matching.len(),
        1,
        "{file}: the lexer reads `{source}`, and the layer's list has {} entries for it",
        matching.len()
      );
      assert_eq!(
        matching[0].attributes,
        attributes_of(feature),
        "{file}: the entry for `{source}` does not stand under its feature alone"
      );
    }
    assert_eq!(
      entries.len(),
      SOURCES.len(),
      "{file}: the list has an entry for a source the lexer does not read"
    );
  }
}

#[test]
fn no_entry_is_a_reference_form() {
  for file in LAYERS {
    for entry in layer_entries(file) {
      assert!(
        !entry.source.starts_with('&'),
        "{file}: `{}` is a reference form, which the lexer does not read",
        entry.source
      );
    }
  }
}

/// The readers above read what is written: an entry line and an impl line each give their source,
/// and a line that is neither is refused rather than skipped.
#[test]
fn the_readers_read_the_lines_they_are_given() {
  let lexer = lexer_sources();
  assert!(lexer.contains(&String::from("str")));
  assert!(lexer.contains(&String::from("HipStr<'_>")));

  for file in LAYERS {
    let entries = layer_entries(file);
    let hipstr = entries
      .iter()
      .find(|entry: &&Entry| entry.source == "hipstr_0_8::HipStr<'h>")
      .expect("an entry whose generics hold a second lifetime is still read");
    assert_eq!(hipstr.attributes.len(), 2);
    let text = entries
      .iter()
      .find(|entry: &&Entry| entry.source == "str")
      .expect("the first entry is read");
    assert!(text.attributes.is_empty());
  }
}
