//! Entry tests: the [`ParseSource`]/[`ParseStr`]/[`ParseBytes`] trait surface, the
//! prefix-semantics pin, and the in-crate integration corpus — the same fixture
//! documents the `smear` corpus and the lexer benches use, parsed at document
//! level over the whole enabled source matrix with the ASTs asserted equal
//! (counts + spans) modulo the slice type.

use crate::{
  entry::{ParseBytes, ParseSource, ParseStr},
  graphql::ast::{
    DescribedEnumTypeDefinition, Document, ExecutableDocument, ObjectTypeExtension,
    SchemaExtension, SelectionSet, TypeSystemDocument,
  },
};

// ─── corpus fixtures (the smear corpus + lexer bench set, path-included) ─────

const Q_TINY: &str =
  include_str!("../../../../../smear/tests/fixtures/executables/bench_01_tiny_simple.graphql");
const Q_LARGE: &str =
  include_str!("../../../../../smear/tests/fixtures/executables/bench_06_large_complex.graphql");
const Q_HUGE: &str = include_str!(
  "../../../../../smear/tests/fixtures/executables/bench_10_huge_comprehensive.graphql"
);
const Q_KITCHEN_SINK: &str =
  include_str!("../../../../../smear/tests/fixtures/executables/kitchen-sink_canonical.graphql");

const S_MINIMAL: &str = include_str!("../../../../../smear/tests/fixtures/schemas/minimal.graphql");
const S_KITCHEN_SINK: &str =
  include_str!("../../../../../smear/tests/fixtures/schemas/schema_kitchen_sink.graphql");
const S_GMX: &str = include_str!("../../../../../smear/tests/fixtures/schemas/gmx_schema.graphql");
const S_GITHUB: &str =
  include_str!("../../../../../smear/tests/fixtures/schemas/github_schema.graphql");

const S_EXTEND_SCALAR: &str =
  include_str!("../../../../../smear/tests/fixtures/schemas/extend_scalar.graphql");
const S_EXTEND_ENUM: &str =
  include_str!("../../../../../smear/tests/fixtures/schemas/extend_enum.graphql");
const S_EXTEND_OBJECT: &str =
  include_str!("../../../../../smear/tests/fixtures/schemas/extend_object.graphql");
const S_EXTEND_INTERFACE: &str =
  include_str!("../../../../../smear/tests/fixtures/schemas/extend_interface.graphql");
const S_EXTEND_INPUT: &str =
  include_str!("../../../../../smear/tests/fixtures/schemas/extend_input_canonical.graphql");
const S_UNION_EXTENSION: &str =
  include_str!("../../../../../smear/tests/fixtures/schemas/union_extension.graphql");

/// Executable corpus documents.
const EXECUTABLES: &[(&str, &str)] = &[
  ("bench_01_tiny_simple", Q_TINY),
  ("bench_06_large_complex", Q_LARGE),
  ("bench_10_huge_comprehensive", Q_HUGE),
  ("kitchen-sink_canonical", Q_KITCHEN_SINK),
];

/// Type-system corpus documents (schemas + the targeted extension fixtures).
const SCHEMAS: &[(&str, &str)] = &[
  ("minimal", S_MINIMAL),
  ("schema_kitchen_sink", S_KITCHEN_SINK),
  ("gmx_schema", S_GMX),
  ("github_schema", S_GITHUB),
  ("extend_scalar", S_EXTEND_SCALAR),
  ("extend_enum", S_EXTEND_ENUM),
  ("extend_object", S_EXTEND_OBJECT),
  ("extend_interface", S_EXTEND_INTERFACE),
  ("extend_input_canonical", S_EXTEND_INPUT),
  ("union_extension", S_UNION_EXTENSION),
];

/// Parses `src` as a full [`Document`] over `str` and `[u8]` (and `Bytes` behind
/// the feature, through the `[u8]` impl) and asserts the ASTs are equal modulo the
/// slice type: same definition count, same document span, same per-definition
/// spans.
fn document_matrix(name: &str, src: &str) {
  let by_str = Document::parse_str(src).unwrap_or_else(|e| panic!("{name}: str parse: {e:?}"));
  let by_slice =
    Document::parse_bytes(src.as_bytes()).unwrap_or_else(|e| panic!("{name}: slice parse: {e:?}"));
  assert!(
    !by_str.definitions().is_empty(),
    "{name}: parsed an empty document"
  );
  assert_eq!(
    by_str.definitions().len(),
    by_slice.definitions().len(),
    "{name}: definition count differs across sources"
  );
  assert_eq!(
    by_str.span(),
    by_slice.span(),
    "{name}: document span differs across sources"
  );
  for (i, (a, b)) in by_str
    .definitions()
    .iter()
    .zip(by_slice.definitions())
    .enumerate()
  {
    assert_eq!(
      tokora::span::AsSpan::as_span(a),
      tokora::span::AsSpan::as_span(b),
      "{name}: definition #{i} span differs across sources"
    );
  }
  #[cfg(feature = "bytes")]
  {
    let owned = ::bytes::Bytes::copy_from_slice(src.as_bytes());
    let by_bytes =
      Document::parse_bytes(&owned).unwrap_or_else(|e| panic!("{name}: bytes parse: {e:?}"));
    assert_eq!(
      by_str.definitions().len(),
      by_bytes.definitions().len(),
      "{name}: definition count differs on Bytes"
    );
  }
}

#[test]
fn corpus_executables_parse_as_documents() {
  for (name, src) in EXECUTABLES {
    document_matrix(name, src);
    // The executable root accepts the same documents.
    let exec = ExecutableDocument::parse_str(src)
      .unwrap_or_else(|e| panic!("{name}: executable parse: {e:?}"));
    assert_eq!(
      exec.definitions().len(),
      Document::parse_str(src).expect(name).definitions().len(),
      "{name}: executable vs full document count"
    );
  }
}

#[test]
fn corpus_schemas_parse_as_documents() {
  for (name, src) in SCHEMAS {
    document_matrix(name, src);
    // The type-system root accepts the same documents.
    let tsd = TypeSystemDocument::parse_str(src)
      .unwrap_or_else(|e| panic!("{name}: type-system parse: {e:?}"));
    assert_eq!(
      tsd.definitions().len(),
      Document::parse_str(src).expect(name).definitions().len(),
      "{name}: type-system vs full document count"
    );
  }
}

#[test]
fn corpus_root_separation_holds() {
  // A schema document is NOT an executable document, and vice versa.
  assert!(ExecutableDocument::parse_str(S_GMX).is_err());
  assert!(TypeSystemDocument::parse_str(Q_KITCHEN_SINK).is_err());
}

#[test]
fn extension_fixtures_materialize_extensions() {
  for (name, src) in &[
    ("extend_scalar", S_EXTEND_SCALAR),
    ("extend_enum", S_EXTEND_ENUM),
    ("extend_object", S_EXTEND_OBJECT),
    ("extend_interface", S_EXTEND_INTERFACE),
    ("extend_input_canonical", S_EXTEND_INPUT),
    ("union_extension", S_UNION_EXTENSION),
  ] {
    let doc = TypeSystemDocument::parse_str(src).unwrap_or_else(|e| panic!("{name}: {e:?}"));
    assert!(
      doc.definitions().iter().all(|d| d.is_extension()),
      "{name}: expected only extensions"
    );
  }
}

// ─── trait surface ───────────────────────────────────────────────────────────

#[test]
fn parse_roots_drive_through_the_trait_surface() {
  // A sample of non-document roots over both convenience traits.
  assert!(SelectionSet::parse_str("{ a b }").is_ok());
  assert!(SelectionSet::parse_bytes(b"{ a b }").is_ok());
  assert!(ObjectTypeExtension::parse_str("extend type User @dir").is_ok());
  assert!(ObjectTypeExtension::parse_bytes(b"extend type User @dir").is_ok());
  assert!(SchemaExtension::parse_str("extend schema { query: Q }").is_ok());
  assert!(DescribedEnumTypeDefinition::parse_str("\"states\" enum E { A }").is_ok());
  // The fully-qualified form works too (the traits are conveniences).
  assert!(<Document<&str> as ParseSource<'_, str>>::parse_source("{ f }").is_ok());
}

#[test]
fn prefix_semantics_pinned() {
  // A non-document root parses a LEADING Self, ignoring the tail (frozen-parity
  // entry behavior, documented on the trait family) …
  assert!(SelectionSet::parse_str("{ f } trailing garbage !!!").is_ok());
  // … while the document roots consume the whole source and error on a bad tail.
  assert!(Document::parse_str("{ f } bogus !!!").is_err());
  assert!(ExecutableDocument::parse_str("{ f } bogus !!!").is_err());
}
