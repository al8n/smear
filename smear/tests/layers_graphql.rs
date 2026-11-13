#![cfg(all(
  any(feature = "std", feature = "alloc"),
  feature = "graphql",
  feature = "parser"
))]

//! GraphQL's parser layers at each call site: every source the syntactic layer reads, for both
//! document kinds, the lossless layer over text, the answer that owns its slices, and the
//! consumers written once over the layer and over the source, which feed the validator.
//!
//! Each `#[test]` is one call site. What it holds first is that the call compiles as written, in
//! every feature row that compiles this file; what it asserts is what the call site promises. The
//! two layers are held to one reading over the valid corpus here, and the hand-over's laws over
//! the whole corpus in `layers_handover_graphql.rs`.

use core::ops::ControlFlow;

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
use smear::parser::graphql::Lossless;
use smear::{
  lexer::tokora::SimpleSpan,
  parser::{
    graphql::{
      Layer, Recovery, Syntactic,
      ast::{ExecutableDocument, TypeSystemDocument},
    },
    layer::from_fn,
  },
};

/// A valid schema: two types.
const SDL: &str = "type Query { hero: Hero }\ntype Hero { name: String }";

/// A valid query against [`SDL`].
const QUERY: &str = "query Q { hero { name } }";

// ── The sources the syntactic layer reads ────────────────────────────────────────────────────

/// What a document looks like from outside its slices: its span, its definition count, its
/// recovery, its verdict and its error count. The slices differ from source to source, so this is
/// what one source's answer is held to another's by.
type Outline = (SimpleSpan, usize, Recovery, bool, usize);

/// The outlines the syntactic layer answers for a schema and a query over one source type,
/// through its inherent methods and through the trait, which must agree.
macro_rules! outlines {
  ($sdl:expr, $query:expr) => {{
    let syntactic = Syntactic::new();
    let schema = syntactic
      .type_system_document($sdl, &mut ())
      .expect("the syntactic layer reads the schema");
    let query = syntactic
      .executable_document($query, &mut ())
      .expect("the syntactic layer reads the query");
    let schema_through = Layer::type_system_document(&syntactic, $sdl, &mut ())
      .expect("the syntactic layer reads the schema");
    let query_through = Layer::executable_document(&syntactic, $query, &mut ())
      .expect("the syntactic layer reads the query");
    let schema: Outline = (
      *schema.document().span(),
      schema.document().definitions().len(),
      schema.recovery(),
      schema.is_valid(),
      schema.error_count(),
    );
    let query: Outline = (
      *query.document().span(),
      query.document().definitions().len(),
      query.recovery(),
      query.is_valid(),
      query.error_count(),
    );
    let schema_through: Outline = (
      *schema_through.document().span(),
      schema_through.document().definitions().len(),
      schema_through.recovery(),
      schema_through.is_valid(),
      schema_through.error_count(),
    );
    let query_through: Outline = (
      *query_through.document().span(),
      query_through.document().definitions().len(),
      query_through.recovery(),
      query_through.is_valid(),
      query_through.error_count(),
    );
    assert_eq!(
      (schema, query),
      (schema_through, query_through),
      "the inherent methods and the trait's methods answer one outline each"
    );
    (schema, query)
  }};
}

/// What [`SDL`] and [`QUERY`] answer as `&str`, which every other source is held to.
fn over_str() -> (Outline, Outline) {
  outlines!(SDL, QUERY)
}

/// The syntactic layer over each source its lexer reads, each under the feature that brings its
/// type: each answer has the outline the same bytes give as `str`.
mod every_source_gives_the_outline_str_gives {
  use super::{Layer, Outline, QUERY, Recovery, SDL, SimpleSpan, Syntactic, over_str};

  #[test]
  fn text() {
    let schema: Outline = (
      SimpleSpan::new(0, SDL.len()),
      2,
      Recovery::new(2, 0),
      true,
      0,
    );
    let query: Outline = (
      SimpleSpan::new(0, QUERY.len()),
      1,
      Recovery::new(1, 0),
      true,
      0,
    );
    assert_eq!(over_str(), (schema, query));
  }

  #[test]
  fn a_byte_slice() {
    assert_eq!(outlines!(SDL.as_bytes(), QUERY.as_bytes()), over_str());
  }

  #[cfg(feature = "bstr_1")]
  #[test]
  fn bstr() {
    assert_eq!(
      outlines!(bstr_1::BStr::new(SDL), bstr_1::BStr::new(QUERY)),
      over_str()
    );
  }

  #[cfg(feature = "bytes_1")]
  #[test]
  fn bytes() {
    let sdl = bytes_1::Bytes::from_static(SDL.as_bytes());
    let query = bytes_1::Bytes::from_static(QUERY.as_bytes());
    assert_eq!(outlines!(&sdl, &query), over_str());
  }

  #[cfg(feature = "hipstr_0_8")]
  #[test]
  fn hipstr() {
    let sdl = hipstr_0_8::HipStr::from(SDL);
    let query = hipstr_0_8::HipStr::from(QUERY);
    assert_eq!(outlines!(&sdl, &query), over_str());
  }

  #[cfg(feature = "hipstr_0_8")]
  #[test]
  fn hipbyt() {
    let sdl = hipstr_0_8::HipByt::from(SDL.as_bytes());
    let query = hipstr_0_8::HipByt::from(QUERY.as_bytes());
    assert_eq!(outlines!(&sdl, &query), over_str());
  }

  #[cfg(feature = "smol-bytes_0_2")]
  #[test]
  fn shared_bytes() {
    let sdl = smol_bytes_0_2::shared::Bytes::from(SDL);
    let query = smol_bytes_0_2::shared::Bytes::from(QUERY);
    assert_eq!(outlines!(&sdl, &query), over_str());
  }

  #[cfg(feature = "smol-bytes_0_2")]
  #[test]
  fn compact_bytes() {
    let sdl = smol_bytes_0_2::compact::Bytes::from(SDL);
    let query = smol_bytes_0_2::compact::Bytes::from(QUERY);
    assert_eq!(outlines!(&sdl, &query), over_str());
  }

  #[cfg(feature = "smol-bytes_0_2")]
  #[test]
  fn shared_utf8_bytes() {
    let sdl = smol_bytes_0_2::Utf8Bytes::from(SDL);
    let query = smol_bytes_0_2::Utf8Bytes::from(QUERY);
    assert_eq!(outlines!(&sdl, &query), over_str());
  }

  #[cfg(feature = "smol-bytes_0_2")]
  #[test]
  fn compact_utf8_bytes() {
    let sdl = smol_bytes_0_2::compact::Utf8Bytes::from(SDL);
    let query = smol_bytes_0_2::compact::Utf8Bytes::from(QUERY);
    assert_eq!(outlines!(&sdl, &query), over_str());
  }
}

#[test]
fn bytes_that_are_not_utf8_are_rejected_with_an_error_lent() {
  let bad: &[u8] = b"query Q { hero }\xFF";
  let mut lent = Vec::new();
  let Err(rejected) = Syntactic::new().executable_document(bad, &mut lent) else {
    panic!("bytes that are not UTF-8 are not a document");
  };
  assert!(rejected.error_count() >= 1);
  assert_eq!(lent.len(), rejected.error_count());
}

// ── Consumers written once over the layer ────────────────────────────────────────────────────

/// What one reading of a text answers: the document, the recovery, the verdict, the error count,
/// and how many errors the layer lent.
#[derive(Debug, PartialEq)]
struct Reading<D> {
  document: D,
  recovery: Recovery,
  valid: bool,
  errors: usize,
  lent: usize,
}

/// One reading of a schema, written once over the layer; `None` when the layer answers no
/// document.
fn read_schema<'s, P: Layer<'s>>(
  parser: &P,
  text: &'s str,
) -> Option<Reading<TypeSystemDocument<&'s str>>> {
  let mut lent = 0usize;
  let mut sink = from_fn(|_: &P::Error| {
    lent += 1;
    ControlFlow::Continue(())
  });
  let parsed = parser.type_system_document(text, &mut sink).ok()?;
  Some(Reading {
    document: parsed.document().clone(),
    recovery: parsed.recovery(),
    valid: parsed.is_valid(),
    errors: parsed.error_count(),
    lent,
  })
}

/// One reading of a query, written once over the layer; `None` when the layer answers no
/// document.
fn read_query<'s, P: Layer<'s>>(
  parser: &P,
  text: &'s str,
) -> Option<Reading<ExecutableDocument<&'s str>>> {
  let mut lent = 0usize;
  let mut sink = from_fn(|_: &P::Error| {
    lent += 1;
    ControlFlow::Continue(())
  });
  let parsed = parser.executable_document(text, &mut sink).ok()?;
  Some(Reading {
    document: parsed.document().clone(),
    recovery: parsed.recovery(),
    valid: parsed.is_valid(),
    errors: parsed.error_count(),
    lent,
  })
}

#[test]
fn one_reading_serves_both_layers() {
  let schema = read_schema(&Syntactic::new(), SDL).expect("the syntactic layer reads the schema");
  assert_eq!(schema.document.definitions().len(), 2);
  assert_eq!(schema.recovery, Recovery::new(2, 0));
  assert!(schema.valid);
  assert_eq!((schema.errors, schema.lent), (0, 0));

  let query = read_query(&Syntactic::new(), QUERY).expect("the syntactic layer reads the query");
  assert_eq!(query.document.definitions().len(), 1);
  assert_eq!(query.recovery, Recovery::new(1, 0));
  assert!(query.valid);
  assert_eq!((query.errors, query.lent), (0, 0));

  #[cfg(all(feature = "rowan_0_17", feature = "std"))]
  {
    let lossless = Lossless::new();
    assert_eq!(read_schema(&lossless, SDL), Some(schema));
    assert_eq!(read_query(&lossless, QUERY), Some(query));
  }
}

// ── The lossless layer ───────────────────────────────────────────────────────────────────────

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
mod lossless {
  use core::ops::ControlFlow;
  use std::path::PathBuf;

  use smear::parser::{
    graphql::{Layer, Lossless, Syntactic},
    layer::from_fn,
  };

  use super::{QUERY, SDL, read_query, read_schema};

  #[test]
  fn the_lossless_layer_reads_a_str() {
    let lossless = Lossless::new();
    let schema = lossless
      .type_system_document(SDL, &mut ())
      .expect("short enough for a tree");
    let query = lossless
      .executable_document(QUERY, &mut ())
      .expect("short enough for a tree");
    assert!(schema.is_valid() && query.is_valid());
    assert!(!schema.tree().has_errors() && !query.tree().has_errors());
  }

  #[test]
  fn the_lossless_layer_reads_an_owned_string_through_as_str() {
    let owned = String::from(QUERY);
    let from_owned = Lossless::new()
      .executable_document(owned.as_str(), &mut ())
      .expect("short enough for a tree");
    let from_str = Lossless::new()
      .executable_document(QUERY, &mut ())
      .expect("short enough for a tree");
    assert_eq!(from_owned.document(), from_str.document());
  }

  #[test]
  fn the_lossless_layer_through_the_trait() {
    let lossless = Lossless::new();
    let schema =
      Layer::type_system_document(&lossless, SDL, &mut ()).expect("short enough for a tree");
    let query =
      Layer::executable_document(&lossless, QUERY, &mut ()).expect("short enough for a tree");
    assert!(schema.is_valid() && query.is_valid());
  }

  #[test]
  fn decoded_bytes_reach_the_lossless_layer_as_the_same_text() {
    let bytes: &[u8] = QUERY.as_bytes();
    let text = core::str::from_utf8(bytes).expect("the bytes are UTF-8");
    let decoded = Lossless::new()
      .executable_document(text, &mut ())
      .expect("short enough for a tree");
    let direct = Lossless::new()
      .executable_document(QUERY, &mut ())
      .expect("short enough for a tree");
    assert_eq!(decoded.document(), direct.document());
    assert_eq!(decoded.recovery(), direct.recovery());
    assert_eq!(decoded.error_count(), direct.error_count());
    assert_eq!(decoded.tree().green(), direct.tree().green());
    assert_eq!(decoded.tree().syntax().text().to_string(), QUERY);
  }

  #[test]
  fn a_broken_schema_answers_its_well_formed_part_and_lends_its_errors() {
    let text = "type Query { hero: String }\ntype Half { f: }";
    let mut lent = 0usize;
    let mut sink = from_fn(|_| {
      lent += 1;
      ControlFlow::Continue(())
    });
    let parsed = Lossless::new()
      .type_system_document(text, &mut sink)
      .expect("short enough for a tree");
    assert!(!parsed.is_valid());
    assert!(parsed.error_count() >= 1);
    assert_eq!(lent, parsed.error_count());
    assert!(parsed.tree().has_errors());
    assert_eq!(parsed.document().definitions().len(), 1);
    assert!(!parsed.recovery().is_complete());
  }

  /// Every corpus entry named `valid_*`, as `(name, text)`, in name order.
  fn valid_corpus() -> Vec<(String, String)> {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests")
      .join("corpus");
    let mut files: Vec<PathBuf> = std::fs::read_dir(&dir)
      .unwrap_or_else(|error| panic!("the corpus at {} is unreadable: {error}", dir.display()))
      .map(|entry| entry.expect("a corpus directory entry").path())
      .filter(|path: &PathBuf| path.extension().is_some_and(|ext| ext == "graphql"))
      .collect();
    files.sort();
    let mut entries = Vec::new();
    for path in files {
      let name = path
        .file_name()
        .expect("a corpus entry has a file name")
        .to_string_lossy()
        .to_string();
      if name.starts_with("valid_") {
        let text = std::fs::read_to_string(&path)
          .unwrap_or_else(|error| panic!("{name} is unreadable: {error}"));
        entries.push((name, text));
      }
    }
    entries
  }

  /// Where nothing is broken the two layers answer one reading, for each document kind: equal
  /// documents, complete and equal recoveries, both valid, no error counted and none lent.
  ///
  /// The valid corpus is valid for the mixed root, and each layer method reads one kind, so an
  /// entry of the other kind is no document for it: the syntactic layer rejects it, and the
  /// lossless layer must then say so too.
  #[test]
  fn the_two_layers_answer_one_reading_over_the_valid_corpus() {
    let mut schemas = 0usize;
    let mut queries = 0usize;
    let mut others = 0usize;
    for (name, text) in valid_corpus() {
      let lossless = read_schema(&Lossless::new(), &text)
        .unwrap_or_else(|| panic!("{name}: a corpus text is short enough for a tree"));
      match read_schema(&Syntactic::new(), &text) {
        Some(syntactic) => {
          assert!(syntactic.valid && lossless.valid, "{name}: both are valid");
          assert_eq!(
            (
              syntactic.errors,
              syntactic.lent,
              lossless.errors,
              lossless.lent
            ),
            (0, 0, 0, 0),
            "{name}: no error is counted and none is lent"
          );
          assert!(
            syntactic.recovery.is_complete() && lossless.recovery.is_complete(),
            "{name}: both recoveries are complete"
          );
          assert_eq!(
            lossless, syntactic,
            "{name}: the two layers answer one schema and count one recovery"
          );
          schemas += 1;
        }
        None => {
          assert!(
            !lossless.valid,
            "{name}: the syntactic layer rejects it as a schema and the lossless layer calls it \
             valid"
          );
          assert_eq!(lossless.lent, lossless.errors, "{name}");
          others += 1;
        }
      }

      let lossless = read_query(&Lossless::new(), &text)
        .unwrap_or_else(|| panic!("{name}: a corpus text is short enough for a tree"));
      match read_query(&Syntactic::new(), &text) {
        Some(syntactic) => {
          assert!(syntactic.valid && lossless.valid, "{name}: both are valid");
          assert_eq!(
            (
              syntactic.errors,
              syntactic.lent,
              lossless.errors,
              lossless.lent
            ),
            (0, 0, 0, 0),
            "{name}: no error is counted and none is lent"
          );
          assert!(
            syntactic.recovery.is_complete() && lossless.recovery.is_complete(),
            "{name}: both recoveries are complete"
          );
          assert_eq!(
            lossless, syntactic,
            "{name}: the two layers answer one query and count one recovery"
          );
          queries += 1;
        }
        None => {
          assert!(
            !lossless.valid,
            "{name}: the syntactic layer rejects it as a query and the lossless layer calls it \
             valid"
          );
          assert_eq!(lossless.lent, lossless.errors, "{name}");
          others += 1;
        }
      }
    }
    assert!(
      schemas >= 10 && queries >= 10,
      "{schemas} valid entries are schemas and {queries} are queries; the reading is held over \
       too few of one kind"
    );
    assert!(
      others >= 10,
      "only {others} readings were of a text of the other kind; the two layers' verdicts are \
       never seen to agree on a rejection"
    );
  }
}

// ── The validator ────────────────────────────────────────────────────────────────────────────

#[cfg(feature = "validator")]
mod validator {
  use smear::{
    lexer::tokora::{SimpleSpan, Source},
    parser::graphql::{Layer, Syntactic},
    validator::{Budget, Collect, Diagnostic, Ignore, Rule, Schema, Scratch, validate_executable},
  };

  use super::{QUERY, SDL};

  /// The schema [`SDL`] builds.
  fn schema() -> Schema {
    let parsed = Syntactic::new()
      .type_system_document(SDL, &mut ())
      .expect("the syntactic layer reads the schema");
    Schema::build(parsed.document()).expect("the schema is valid")
  }

  /// U6's verdict, written once over the layer for text: the syntax verdict and the validator's.
  fn verdict<'s, P: Layer<'s>>(
    parser: &P,
    schema: &Schema,
    text: &'s str,
  ) -> Result<bool, P::Failure> {
    let parsed = parser.executable_document(text, &mut ())?;
    let found = validate_executable(
      schema,
      parsed.document(),
      &mut Scratch::new(),
      &Budget::default(),
      &mut Ignore,
    );
    Ok(parsed.is_valid() && found.is_ok())
  }

  /// U6's verdict over the source: the consumer states what it uses of the source itself.
  fn verdict_over<'src, Src, P>(
    parser: &P,
    schema: &Schema,
    source: &'src Src,
  ) -> Result<bool, P::Failure>
  where
    Src: Source<usize> + ?Sized,
    Src::Slice<'src>: AsRef<[u8]> + Clone,
    P: Layer<'src, Src>,
  {
    let parsed = parser.executable_document(source, &mut ())?;
    let found = validate_executable(
      schema,
      parsed.document(),
      &mut Scratch::new(),
      &Budget::default(),
      &mut Ignore,
    );
    Ok(parsed.is_valid() && found.is_ok())
  }

  #[test]
  fn one_verdict_serves_both_layers() {
    let schema = schema();
    assert_eq!(verdict(&Syntactic::new(), &schema, QUERY), Ok(true));
    #[cfg(all(feature = "rowan_0_17", feature = "std"))]
    assert_eq!(
      verdict(&smear::parser::graphql::Lossless::new(), &schema, QUERY),
      Ok(true)
    );
  }

  #[test]
  fn one_verdict_over_the_source_serves_each_source_a_layer_reads() {
    let schema = schema();
    assert_eq!(verdict_over(&Syntactic::new(), &schema, QUERY), Ok(true));
    assert_eq!(
      verdict_over(&Syntactic::new(), &schema, QUERY.as_bytes()),
      Ok(true)
    );
    #[cfg(feature = "bytes_1")]
    {
      let body = bytes_1::Bytes::from_static(QUERY.as_bytes());
      assert_eq!(verdict_over(&Syntactic::new(), &schema, &body), Ok(true));
    }
    #[cfg(all(feature = "rowan_0_17", feature = "std"))]
    assert_eq!(
      verdict_over(&smear::parser::graphql::Lossless::new(), &schema, QUERY),
      Ok(true)
    );
  }

  /// A query the validator refuses twice against [`SDL`]: `Hero` has no `nme`, and `Query` has no
  /// `villain`.
  const MISTAKEN: &str = "query Q { hero { nme } villain }";

  /// What the validator finds in [`MISTAKEN`] against the schema [`SDL`] builds, both read by the
  /// syntactic layer over one source type: each finding's rule and span, in the order found.
  macro_rules! findings {
    ($sdl:expr, $query:expr) => {{
      let syntactic = Syntactic::new();
      let schema = syntactic
        .type_system_document($sdl, &mut ())
        .expect("the syntactic layer reads the schema");
      let schema = Schema::build(schema.document()).expect("the schema is valid");
      let query = syntactic
        .executable_document($query, &mut ())
        .expect("the syntactic layer reads the query");
      let mut found = Vec::new();
      let verdict = validate_executable(
        &schema,
        query.document(),
        &mut Scratch::new(),
        &Budget::default(),
        &mut Collect::new(&mut found),
      );
      assert!(verdict.is_err(), "the query is not valid");
      found
        .iter()
        .map(|finding: &Diagnostic<_>| (finding.rule(), finding.span()))
        .collect::<Vec<(Rule, SimpleSpan)>>()
    }};
  }

  /// What [`MISTAKEN`] answers as `&str`, which every other source is held to.
  fn over_str() -> Vec<(Rule, SimpleSpan)> {
    findings!(SDL, MISTAKEN)
  }

  /// The builder and the validator read every slice as bytes, so this is where the slices
  /// themselves are compared: over each source the validator finds the same rules at the same
  /// spans.
  #[test]
  fn the_validator_agrees_across_the_sources() {
    let expected = over_str();
    assert!(
      expected.len() >= 2,
      "the validator found {} mistakes where the query makes two",
      expected.len()
    );

    assert_eq!(findings!(SDL.as_bytes(), MISTAKEN.as_bytes()), expected);
    #[cfg(feature = "bstr_1")]
    assert_eq!(
      findings!(bstr_1::BStr::new(SDL), bstr_1::BStr::new(MISTAKEN)),
      expected
    );
    #[cfg(feature = "bytes_1")]
    {
      let sdl = bytes_1::Bytes::from_static(SDL.as_bytes());
      let query = bytes_1::Bytes::from_static(MISTAKEN.as_bytes());
      assert_eq!(findings!(&sdl, &query), expected);
    }
    #[cfg(feature = "hipstr_0_8")]
    {
      let sdl = hipstr_0_8::HipStr::from(SDL);
      let query = hipstr_0_8::HipStr::from(MISTAKEN);
      assert_eq!(findings!(&sdl, &query), expected);
      let sdl = hipstr_0_8::HipByt::from(SDL.as_bytes());
      let query = hipstr_0_8::HipByt::from(MISTAKEN.as_bytes());
      assert_eq!(findings!(&sdl, &query), expected);
    }
    #[cfg(feature = "smol-bytes_0_2")]
    {
      let sdl = smol_bytes_0_2::shared::Bytes::from(SDL);
      let query = smol_bytes_0_2::shared::Bytes::from(MISTAKEN);
      assert_eq!(findings!(&sdl, &query), expected);
      let sdl = smol_bytes_0_2::compact::Bytes::from(SDL);
      let query = smol_bytes_0_2::compact::Bytes::from(MISTAKEN);
      assert_eq!(findings!(&sdl, &query), expected);
      let sdl = smol_bytes_0_2::Utf8Bytes::from(SDL);
      let query = smol_bytes_0_2::Utf8Bytes::from(MISTAKEN);
      assert_eq!(findings!(&sdl, &query), expected);
      let sdl = smol_bytes_0_2::compact::Utf8Bytes::from(SDL);
      let query = smol_bytes_0_2::compact::Utf8Bytes::from(MISTAKEN);
      assert_eq!(findings!(&sdl, &query), expected);
    }
  }

  /// The answer over an owned body owns its slices: the body is dropped while the document lives,
  /// and the document is validated and executed after it.
  #[cfg(feature = "bytes_1")]
  #[test]
  fn the_answer_over_an_owned_body_borrows_nothing() {
    use smear::lexer::tokora::utils::IntoComponents;

    let schema = schema();
    let body = bytes_1::Bytes::from(String::from(QUERY));
    let parsed = Syntactic::new()
      .executable_document(&body, &mut ())
      .expect("the syntactic layer reads the query");
    drop(body);
    let (document, recovery, errors, ()) = parsed.into_components();
    assert!(recovery.is_complete());
    assert_eq!(errors, 0);
    validate_executable(
      &schema,
      &document,
      &mut Scratch::new(),
      &Budget::default(),
      &mut Ignore,
    )
    .expect("the query is valid");
    #[cfg(feature = "proto")]
    {
      let executor: smear::proto::Executor<'_, bytes_1::Bytes, Nothing> =
        smear::proto::Executor::new(&schema, &document);
      drop(executor);
    }
  }

  /// A driver with nothing to say: the executor above is built and never started.
  #[cfg(all(feature = "proto", feature = "bytes_1"))]
  struct Nothing;

  #[cfg(all(feature = "proto", feature = "bytes_1"))]
  impl smear::proto::Values for Nothing {
    type Value = ();

    fn is_null(&self, _: &()) -> bool {
      false
    }

    fn as_bool(&self, _: &()) -> Option<bool> {
      None
    }

    fn as_str<'a>(&'a self, _: &'a ()) -> Option<&'a str> {
      None
    }

    fn list_len(&self, _: &()) -> Option<usize> {
      None
    }

    fn list_item(&mut self, _: &(), _: usize) {}

    fn type_name<'a>(&'a self, _: &'a ()) -> Option<&'a str> {
      None
    }

    fn coerce_leaf(&mut self, value: (), _: smear::proto::Leaf<'_>) -> Option<()> {
      Some(value)
    }

    fn variable(&mut self, _: &str) -> Option<()> {
      None
    }
  }
}
