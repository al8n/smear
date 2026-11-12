#![cfg(all(
  any(feature = "std", feature = "alloc"),
  feature = "graphql",
  feature = "parser"
))]

//! GraphQL's parser layers at each call site design Y shows: every source the syntactic layer
//! reads, for both document kinds, the lossless layer over text, the answer that owns its slices,
//! and the consumers written once over the layer and over the source, which feed the validator.
//!
//! Each `#[test]` is one call site. What it pins first is that the call compiles as written, in
//! every feature row that compiles this file; what it asserts is what the call site promises.

use core::ops::ControlFlow;

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
use smear::parser::graphql::Lossless;
use smear::parser::{
  graphql::{Layer, Recovery, Syntactic},
  layer::from_fn,
};

/// A valid schema: two types.
const SDL: &str = "type Query { hero: Hero }\ntype Hero { name: String }";

/// A valid query against [`SDL`].
const QUERY: &str = "query Q { hero { name } }";

// ── The sources the syntactic layer reads ────────────────────────────────────────────────────

/// What the syntactic layer answers for a schema and a query over one source type, through its
/// inherent methods and through the trait, which must agree: each document's definition count and
/// recovery.
macro_rules! syntactic_reads {
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
    assert!(schema.is_valid() && query.is_valid());
    assert!(schema_through.is_valid() && query_through.is_valid());
    assert_eq!(schema.recovery(), schema_through.recovery());
    assert_eq!(query.recovery(), query_through.recovery());
    (
      schema.document().definitions().len(),
      schema.recovery(),
      query.document().definitions().len(),
      query.recovery(),
    )
  }};
}

/// What [`SDL`] and [`QUERY`] answer as `&str`, which every other source is held to.
fn over_str() -> (usize, Recovery, usize, Recovery) {
  syntactic_reads!(SDL, QUERY)
}

#[test]
fn the_syntactic_layer_reads_str() {
  assert_eq!(over_str(), (2, Recovery::new(2, 0), 1, Recovery::new(1, 0)));
}

#[test]
fn the_syntactic_layer_reads_a_byte_slice() {
  assert_eq!(
    syntactic_reads!(SDL.as_bytes(), QUERY.as_bytes()),
    over_str()
  );
}

#[cfg(feature = "bstr_1")]
#[test]
fn the_syntactic_layer_reads_bstr() {
  assert_eq!(
    syntactic_reads!(bstr_1::BStr::new(SDL), bstr_1::BStr::new(QUERY)),
    over_str()
  );
}

#[cfg(feature = "bytes_1")]
#[test]
fn the_syntactic_layer_reads_bytes() {
  let sdl = bytes_1::Bytes::from_static(SDL.as_bytes());
  let query = bytes_1::Bytes::from_static(QUERY.as_bytes());
  assert_eq!(syntactic_reads!(&sdl, &query), over_str());
}

#[cfg(feature = "hipstr_0_8")]
#[test]
fn the_syntactic_layer_reads_hipstr() {
  let sdl = hipstr_0_8::HipStr::from(SDL);
  let query = hipstr_0_8::HipStr::from(QUERY);
  assert_eq!(syntactic_reads!(&sdl, &query), over_str());
}

#[cfg(feature = "hipstr_0_8")]
#[test]
fn the_syntactic_layer_reads_hipbyt() {
  let sdl = hipstr_0_8::HipByt::from(SDL.as_bytes());
  let query = hipstr_0_8::HipByt::from(QUERY.as_bytes());
  assert_eq!(syntactic_reads!(&sdl, &query), over_str());
}

#[cfg(feature = "smol-bytes_0_2")]
#[test]
fn the_syntactic_layer_reads_shared_bytes() {
  let sdl = smol_bytes_0_2::shared::Bytes::from(SDL);
  let query = smol_bytes_0_2::shared::Bytes::from(QUERY);
  assert_eq!(syntactic_reads!(&sdl, &query), over_str());
}

#[cfg(feature = "smol-bytes_0_2")]
#[test]
fn the_syntactic_layer_reads_compact_bytes() {
  let sdl = smol_bytes_0_2::compact::Bytes::from(SDL);
  let query = smol_bytes_0_2::compact::Bytes::from(QUERY);
  assert_eq!(syntactic_reads!(&sdl, &query), over_str());
}

#[cfg(feature = "smol-bytes_0_2")]
#[test]
fn the_syntactic_layer_reads_shared_utf8_bytes() {
  let sdl = smol_bytes_0_2::Utf8Bytes::from(SDL);
  let query = smol_bytes_0_2::Utf8Bytes::from(QUERY);
  assert_eq!(syntactic_reads!(&sdl, &query), over_str());
}

#[cfg(feature = "smol-bytes_0_2")]
#[test]
fn the_syntactic_layer_reads_compact_utf8_bytes() {
  let sdl = smol_bytes_0_2::compact::Utf8Bytes::from(SDL);
  let query = smol_bytes_0_2::compact::Utf8Bytes::from(QUERY);
  assert_eq!(syntactic_reads!(&sdl, &query), over_str());
}

#[test]
fn bytes_that_are_not_utf8_are_rejected_by_the_syntactic_layer() {
  let bad: &[u8] = b"query Q { hero }\xFF";
  let mut lent = Vec::new();
  let Err(rejected) = Syntactic::new().executable_document(bad, &mut lent) else {
    panic!("bytes that are not UTF-8 are not a document");
  };
  assert!(rejected.error_count() >= 1);
  assert_eq!(lent.len(), rejected.error_count());
}

// ── A consumer written once over the layer ───────────────────────────────────────────────────

/// One reading of a query, written once over the layer: the definition count, the recovery, the
/// verdict, the error count, and how many errors the layer lent.
fn read<'s, P: Layer<'s>>(
  parser: &P,
  text: &'s str,
) -> Option<(usize, Recovery, bool, usize, usize)> {
  let mut lent = 0usize;
  let mut sink = from_fn(|_: &P::Error| {
    lent += 1;
    ControlFlow::Continue(())
  });
  let parsed = parser.executable_document(text, &mut sink).ok()?;
  Some((
    parsed.document().definitions().len(),
    parsed.recovery(),
    parsed.is_valid(),
    parsed.error_count(),
    lent,
  ))
}

#[test]
fn one_reading_serves_both_layers() {
  let syntactic = read(&Syntactic::new(), QUERY).expect("the syntactic layer reads the query");
  assert_eq!(syntactic, (1, Recovery::new(1, 0), true, 0, 0));
  #[cfg(all(feature = "rowan_0_17", feature = "std"))]
  {
    let lossless = read(&Lossless::new(), QUERY).expect("the lossless layer reads the query");
    assert_eq!(lossless, syntactic);
  }
}

// ── The lossless layer ───────────────────────────────────────────────────────────────────────

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
mod lossless {
  use smear::parser::graphql::{Layer, Lossless};

  use super::{QUERY, SDL};

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
  fn bytes_reach_the_lossless_layer_decoded() {
    let text = core::str::from_utf8(QUERY.as_bytes()).expect("the bytes are UTF-8");
    let parsed = Lossless::new()
      .executable_document(text, &mut ())
      .expect("short enough for a tree");
    assert!(parsed.is_valid());
  }

  #[test]
  fn a_broken_schema_answers_its_well_formed_part() {
    let text = "type Query { hero: String }\ntype Half { f: }";
    let parsed = Lossless::new()
      .type_system_document(text, &mut ())
      .expect("short enough for a tree");
    assert!(!parsed.is_valid());
    assert!(parsed.tree().has_errors());
    assert_eq!(parsed.document().definitions().len(), 1);
    assert!(!parsed.recovery().is_complete());
  }
}

// ── The validator ────────────────────────────────────────────────────────────────────────────

#[cfg(feature = "validator")]
mod validator {
  use smear::{
    lexer::tokora::Source,
    parser::graphql::{Layer, Syntactic},
    validator::{Budget, Ignore, Schema, Scratch, validate_executable},
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
