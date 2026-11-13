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

/// The lexer's sources. Any file under it may hold an impl of `DelegateStringError`.
const LEXER: &str = "../smear-lexer/src";

/// What an impl of `DelegateStringError` writes before its source.
const IMPLEMENTED_FOR: &str = "DelegateStringError for ";

/// Each dialect's layers.
const LAYERS: [&str; 2] = [
  "../smear-parser/src/graphqlx/layer.rs",
  "../smear-parser/src/graphql/layer.rs",
];

/// One source: the type as the lexer's impl names it, the type as a layer's entry names it, and
/// the feature the entry stands under.
type Row = (&'static str, &'static str, Option<&'static str>);

/// The sources the syntactic lexers read, in the order the lexer's files, by path, implement
/// them.
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

/// Every `.rs` file under `dir`, a path relative to this package, in path order.
fn rust_files(dir: &str) -> Vec<PathBuf> {
  let mut files = Vec::new();
  let mut pending = vec![PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(dir)];
  while let Some(next) = pending.pop() {
    let entries = std::fs::read_dir(&next).unwrap_or_else(|error| {
      panic!(
        "{} is unreadable: {error}; a list read from a directory that is not there holds nothing",
        next.display()
      )
    });
    for entry in entries {
      let path = entry.expect("a directory entry").path();
      if path.is_dir() {
        pending.push(path);
      } else if path.extension().is_some_and(|ext| ext == "rs") {
        files.push(path);
      }
    }
  }
  files.sort();
  files
}

/// `text` with every run of white space as one space, so that an impl reads the same however its
/// lines are broken.
fn flat(text: &str) -> String {
  text.split_whitespace().collect::<Vec<&str>>().join(" ")
}

/// What the lexer's files say of `DelegateStringError`'s impls.
struct LexerImpls {
  /// The source of each impl the reader could read, in path order and then in text order.
  sources: Vec<String>,
  /// How many times the files write `DelegateStringError for` at all.
  written: usize,
}

/// Reads every file of the lexer for the impls of `DelegateStringError`, whichever file holds one
/// and whatever generics it has. A writing of `DelegateStringError for` that does not read as an
/// impl is counted and gives no source, so the two numbers differ and the test that compares
/// them fails, where skipping it would let a source go unseen.
fn lexer_impls() -> LexerImpls {
  let mut sources = Vec::new();
  let mut written = 0usize;
  for path in rust_files(LEXER) {
    let text = std::fs::read_to_string(&path)
      .unwrap_or_else(|error| panic!("{} is unreadable: {error}", path.display()));
    let text = flat(&text);
    for (at, _) in text.match_indices(IMPLEMENTED_FOR) {
      written += 1;
      if let Some(source) = implemented_source(&text, at) {
        sources.push(source);
      }
    }
  }
  LexerImpls { sources, written }
}

/// The source of the impl whose `DelegateStringError for` stands at `at` in `text`, a text made
/// [`flat`]: what stands between those words and the impl's `where` or `{`, when what stands
/// before them is `impl` with its generics, if it has any. `None` for any other writing of the
/// words.
fn implemented_source(text: &str, at: usize) -> Option<String> {
  let head = text[..at].trim_end();
  let head = match head.strip_suffix('>') {
    None => head,
    Some(generics) => {
      // Back to the `<` that opens the generics this `>` closes.
      let mut depth = 1usize;
      let mut open = None;
      for (index, character) in generics.char_indices().rev() {
        match character {
          '>' => depth += 1,
          '<' => {
            depth -= 1;
            if depth == 0 {
              open = Some(index);
              break;
            }
          }
          _ => {}
        }
      }
      generics[..open?].trim_end()
    }
  };
  let before = head.strip_suffix("impl")?;
  if before.ends_with(|character: char| character.is_alphanumeric() || character == '_') {
    return None;
  }
  let rest = &text[at + IMPLEMENTED_FOR.len()..];
  let header = &rest[..rest.find('{')?];
  let source = match header.split_once(" where ") {
    Some((source, _)) => source,
    None => header,
  }
  .trim();
  if source.is_empty() {
    return None;
  }
  Some(String::from(source))
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
  let lexer = lexer_impls();
  assert_eq!(
    lexer.written,
    lexer.sources.len(),
    "the lexer's files write `DelegateStringError for` {} times, and {} of those read as the impl \
     of a source: one is written in a form this test cannot read, which may be a source no layer \
     has. Teach `implemented_source` the form, or reword what is no impl",
    lexer.written,
    lexer.sources.len()
  );
  let found = lexer.sources;
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

/// The readers above read what is written: an entry line gives its source, an impl gives its
/// source in each form an impl is written in, and the trait's words where no impl stands give
/// none, so that the lexer's count and its sources differ where a silence would agree.
#[test]
fn the_readers_read_the_lines_they_are_given() {
  let lexer = lexer_impls().sources;
  assert!(lexer.contains(&String::from("str")));
  assert!(lexer.contains(&String::from("HipStr<'_>")));

  // What the first `DelegateStringError for` of `written` gives, read as a file's text is.
  let read = |written: &str| -> Option<String> {
    let text = flat(written);
    let at = text.find(IMPLEMENTED_FOR)?;
    implemented_source(&text, at)
  };
  assert_eq!(
    read("impl DelegateStringError for str {").as_deref(),
    Some("str")
  );
  assert_eq!(
    read("  impl<'a> DelegateStringError for Other<'a> {").as_deref(),
    Some("Other<'a>"),
    "generics after `impl`"
  );
  assert_eq!(
    read("impl<'a, T: Bound<'a>> DelegateStringError\n  for Other<'a, T>\nwhere\n  T: Leaf,\n{")
      .as_deref(),
    Some("Other<'a, T>"),
    "an impl broken over lines, with a `where` clause"
  );
  assert_eq!(
    read("// a `T: DelegateStringError for ` that no impl writes {"),
    None,
    "the words with no `impl` before them"
  );
  assert_eq!(
    read("simpl DelegateStringError for str {"),
    None,
    "the words after a name that only ends in `impl`"
  );

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
