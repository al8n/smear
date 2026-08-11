//! Full GraphQL document-production tests.

use crate::lexer::graphql::syntactic::SyntacticTokenKind;
use tokora::{FatalContext, Parse, Parser, SimpleSpan};

use super::{definition, definition_or_extension, described_definition, document};
use crate::parser::graphql::{
  GraphQL,
  ast::{Definition, DefinitionOrExtension, DescribedDefinition, Document},
  error::{ErrorData, GraphqlErrors, Unclosed},
  syntactic::{GraphqlInput, GraphqlLexer},
};

type StrCtx<'inp> = FatalContext<'inp, GraphqlLexer<'inp, str>, GraphqlErrors<&'inp str>, GraphQL>;
type SliceCtx<'inp> =
  FatalContext<'inp, GraphqlLexer<'inp, [u8]>, GraphqlErrors<&'inp [u8]>, GraphQL>;

fn drive_str<'inp, O>(
  parser: impl for<'c> FnMut(
    &mut GraphqlInput<'inp, 'c, str, StrCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp str>>,
  source: &'inp str,
) -> Result<O, GraphqlErrors<&'inp str>> {
  Parser::with_parser::<'inp, GraphqlLexer<'inp, str>, O, GraphqlErrors<&'inp str>, _, GraphQL>(
    parser,
  )
  .parse_str(source)
}

fn drive_slice<'inp, O>(
  parser: impl for<'c> FnMut(
    &mut GraphqlInput<'inp, 'c, [u8], SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  source: &'inp [u8],
) -> Result<O, GraphqlErrors<&'inp [u8]>> {
  Parser::with_parser::<'inp, GraphqlLexer<'inp, [u8]>, O, GraphqlErrors<&'inp [u8]>, _, GraphQL>(
    parser,
  )
  .parse_slice(source)
}

#[cfg(feature = "bytes")]
fn drive_bytes<'inp, O>(
  parser: impl for<'c> FnMut(
    &mut GraphqlInput<'inp, 'c, [u8], SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  source: &'inp ::bytes::Bytes,
) -> Result<O, GraphqlErrors<&'inp [u8]>> {
  Parser::with_parser::<'inp, GraphqlLexer<'inp, [u8]>, O, GraphqlErrors<&'inp [u8]>, _, GraphQL>(
    parser,
  )
  .parse_bytes(source)
}

macro_rules! accept_all {
  ($parser:expr, $source:expr, $check:path) => {{
    $check(drive_str($parser, $source).expect("str source accepts"));
    $check(drive_slice($parser, $source.as_bytes()).expect("[u8] source accepts"));
    #[cfg(feature = "bytes")]
    {
      let bytes = ::bytes::Bytes::from_static($source.as_bytes());
      $check(drive_bytes($parser, &bytes).expect("Bytes source accepts"));
    }
  }};
}

macro_rules! reject_all {
  ($parser:expr, $source:expr) => {{
    assert!(
      drive_str(|inp| $parser(inp).map(|_| ()), $source).is_err(),
      "str should reject {:?}",
      $source,
    );
    assert!(
      drive_slice(|inp| $parser(inp).map(|_| ()), $source.as_bytes()).is_err(),
      "[u8] should reject {:?}",
      $source,
    );
    #[cfg(feature = "bytes")]
    {
      let bytes = ::bytes::Bytes::from_static($source.as_bytes());
      assert!(
        drive_bytes(|inp| $parser(inp).map(|_| ()), &bytes).is_err(),
        "Bytes should reject {:?}",
        $source,
      );
    }
  }};
}

#[test]
fn document_accepts_mixed_executable_sdl_and_extension_entries() {
  const SOURCE: &str = r#""Query documentation" type Query { id: ID }
query GetViewer { id }
fragment ViewerFields on Query { id }
extend type Query { name: String }"#;

  fn check<S: AsRef<[u8]>>(value: Document<S>) {
    let definitions = value.definitions();
    assert_eq!(definitions.len(), 4);
    assert!(definitions[0].is_definition());
    assert!(
      definitions[0]
        .unwrap_definition_ref()
        .description()
        .is_some()
    );
    assert!(
      definitions[0]
        .unwrap_definition_ref()
        .node()
        .is_type_system()
    );
    assert!(
      definitions[1]
        .unwrap_definition_ref()
        .node()
        .is_executable()
    );
    assert!(
      definitions[2]
        .unwrap_definition_ref()
        .node()
        .is_executable()
    );
    assert!(definitions[3].is_extension());
    assert_eq!(value.span().start(), 0);
  }

  accept_all!(document, SOURCE, check);
}

#[test]
fn definition_fused_dispatches_each_root_family() {
  fn accepted<S>(_: Definition<S>) {}

  for source in [
    "{ id }",
    "query Q { id }",
    "mutation M { update }",
    "subscription S { notice }",
    "fragment F on Query { id }",
    "schema { query: Query }",
    "directive @tag on FIELD",
    "scalar Date",
    "type User { id: ID }",
    "interface Node { id: ID }",
    "union Search = User",
    "enum Role { USER }",
    "input Filter { limit: Int }",
  ] {
    accept_all!(definition, source, accepted);
  }
}

#[test]
fn described_definitions_cover_sdl_and_executable_forms() {
  fn described_sdl<S: AsRef<[u8]>>(value: DescribedDefinition<S>) {
    assert!(value.description().is_some());
    assert!(value.node().is_type_system());
  }
  fn described_executable<S: AsRef<[u8]>>(value: DescribedDefinition<S>) {
    assert!(value.description().is_some());
    assert!(value.node().is_executable());
  }
  fn undescribed_shorthand<S: AsRef<[u8]>>(value: DescribedDefinition<S>) {
    assert!(value.description().is_none());
    assert!(value.node().is_executable());
  }

  accept_all!(
    described_definition,
    "\"type documentation\" type User { id: ID }",
    described_sdl
  );
  accept_all!(
    described_definition,
    "\"query documentation\" query GetUser { id }",
    described_executable
  );
  accept_all!(
    described_definition,
    "\"fragment documentation\" fragment F on T { id }",
    described_executable
  );

  // `OperationDefinition : SelectionSet` carries no `Description?` slot, so the shorthand is
  // accepted bare and refused the moment a description precedes it — from both roots that
  // read one.
  accept_all!(described_definition, "{ id }", undescribed_shorthand);
  reject_all!(described_definition, "\"shorthand documentation\" { id }");
  reject_all!(
    definition_or_extension,
    "\"shorthand documentation\" { id }"
  );
  reject_all!(
    definition_or_extension,
    "\"\"\"shorthand documentation\"\"\" { id }"
  );
  reject_all!(document, "type User { id: ID } \"shorthand\" { id }");
}

#[test]
fn extensions_are_only_accepted_by_definition_or_extension() {
  const EXTENSION: &str = "extend type User { id: ID }";

  fn extension<S: AsRef<[u8]>>(value: DefinitionOrExtension<S>) {
    assert!(value.is_extension());
  }

  reject_all!(definition, EXTENSION);
  accept_all!(definition_or_extension, EXTENSION, extension);
  reject_all!(
    definition_or_extension,
    "\"extension documentation\" extend type User { id: ID }"
  );
}

#[test]
fn document_is_nonempty_and_preserves_root_spans() {
  const DESCRIBED: &str = "\"documentation\" query Q { id }";
  const SOURCE: &str =
    "\"documentation\" query Q { id }\nextend scalar Date @specifiedBy(url: \"x\")";

  let described =
    drive_str(described_definition, DESCRIBED).expect("described executable definition");
  let keyword_start = DESCRIBED.find("query").expect("query keyword");
  assert_eq!(described.span(), &SimpleSpan::new(0, DESCRIBED.len()));
  assert_eq!(
    described.node().span(),
    &SimpleSpan::new(keyword_start, DESCRIBED.len())
  );

  let parsed = drive_str(document, SOURCE).expect("full document");
  assert_eq!(parsed.span(), &SimpleSpan::new(0, SOURCE.len()));
  assert_eq!(parsed.definitions()[0].span().start(), 0);
  assert_eq!(parsed.definitions()[1].span().end(), SOURCE.len());

  reject_all!(document, "");
  reject_all!(document, "   ");
}

#[test]
fn root_boundaries_keep_native_selection_set_diagnostics() {
  reject_all!(document, "{ id } )");
  reject_all!(document, "{ id } type");
  reject_all!(definition, "\"documentation\" type User { id: ID }");

  let error = drive_str(|inp| document(inp).map(|_| ()), "{ id")
    .expect_err("unclosed shorthand selection set");
  assert!(matches!(
    error
      .into_iter()
      .next()
      .expect("fatal context emits one diagnostic")
      .data(),
    ErrorData::Unclosed(Unclosed::Object)
  ));

  let result = drive_str(
    |inp| {
      let result = definition(inp).map(|_| ());
      let next = inp
        .next()?
        .expect("an invalid non-identifier root head remains available");
      Ok::<_, GraphqlErrors<&str>>((result, next.data.kind()))
    },
    ")",
  )
  .expect("inspection parser runs");
  assert!(result.0.is_err());
  assert_eq!(result.1, SyntacticTokenKind::RParen);
}

#[test]
fn associated_root_apis_infer_str_and_byte_slice_sources() {
  let _: Definition<&str> =
    drive_str(Definition::<&str>::graphql, "query Q { id }").expect("str definition");
  let _: Definition<&[u8]> =
    drive_slice(Definition::<&[u8]>::graphql, b"query Q { id }").expect("slice definition");

  let _: DescribedDefinition<&str> = drive_str(
    DescribedDefinition::<&str>::graphql,
    "\"doc\" type Query { id: ID }",
  )
  .expect("str described definition");
  let _: DefinitionOrExtension<&[u8]> = drive_slice(
    DefinitionOrExtension::<&[u8]>::graphql,
    b"extend type Query { id: ID }",
  )
  .expect("slice definition or extension");
  let _: Document<&str> = drive_str(
    Document::<&str>::graphql,
    "type Query { id: ID } query Q { id }",
  )
  .expect("str document");
}
