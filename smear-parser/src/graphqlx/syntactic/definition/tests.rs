//! Focused GraphQLx SDL and import-document tests.

use smear_lexer::graphqlx::syntactic::SyntacticTokenKind;
use tokora::{FatalContext, Parse, Parser, SimpleSpan, utils::Equivalent};

use super::*;
use crate::graphqlx::{
  GraphQLx, ast,
  error::{ErrorData, Expectation, GraphqlxErrors, Unclosed},
};

type StrCtx<'inp> =
  FatalContext<'inp, GraphqlxLexer<'inp, str>, GraphqlxErrors<&'inp str>, GraphQLx>;
type SliceCtx<'inp> =
  FatalContext<'inp, GraphqlxLexer<'inp, [u8]>, GraphqlxErrors<&'inp [u8]>, GraphQLx>;

#[cfg(feature = "bytes")]
type BytesCtx<'inp> =
  FatalContext<'inp, GraphqlxLexer<'inp, ::bytes::Bytes>, GraphqlxErrors<::bytes::Bytes>, GraphQLx>;

fn drive_str<'inp, Output>(
  parser: impl for<'ctx> FnMut(
    &mut GraphqlxInput<'inp, 'ctx, str, StrCtx<'inp>>,
  ) -> Result<Output, GraphqlxErrors<&'inp str>>,
  source: &'inp str,
) -> Result<Output, GraphqlxErrors<&'inp str>> {
  Parser::with_parser::<
    'inp,
    GraphqlxLexer<'inp, str>,
    Output,
    GraphqlxErrors<&'inp str>,
    _,
    GraphQLx,
  >(parser)
  .parse_str(source)
}

fn drive_slice<'inp, Output>(
  parser: impl for<'ctx> FnMut(
    &mut GraphqlxInput<'inp, 'ctx, [u8], SliceCtx<'inp>>,
  ) -> Result<Output, GraphqlxErrors<&'inp [u8]>>,
  source: &'inp [u8],
) -> Result<Output, GraphqlxErrors<&'inp [u8]>> {
  Parser::with_parser::<
    'inp,
    GraphqlxLexer<'inp, [u8]>,
    Output,
    GraphqlxErrors<&'inp [u8]>,
    _,
    GraphQLx,
  >(parser)
  .parse_slice(source)
}

#[cfg(feature = "bytes")]
fn drive_owned_bytes<'inp, Output>(
  parser: impl for<'ctx> FnMut(
    &mut GraphqlxInput<'inp, 'ctx, ::bytes::Bytes, BytesCtx<'inp>>,
  ) -> Result<Output, GraphqlxErrors<::bytes::Bytes>>,
  source: &'inp ::bytes::Bytes,
) -> Result<Output, GraphqlxErrors<::bytes::Bytes>> {
  Parser::with_parser::<
    'inp,
    GraphqlxLexer<'inp, ::bytes::Bytes>,
    Output,
    GraphqlxErrors<::bytes::Bytes>,
    _,
    GraphQLx,
  >(parser)
  .parse(source)
}

macro_rules! accept_document {
  ($source:expr) => {{
    drive_str(ast::TypeSystemDocument::graphqlx, $source)
      .expect(concat!("str accepts fixture: ", stringify!($source)));
    drive_slice(ast::TypeSystemDocument::graphqlx, $source.as_bytes())
      .expect(concat!("slice accepts fixture: ", stringify!($source)));
  }};
}

#[test]
fn type_system_document_accepts_graphqlx_sdl_and_import_fixtures() {
  const FIXTURES: &[&str] = &[
    include_str!("../../../../tests/fixtures/parser/graphqlx/ok/0001_import_named.graphqlx"),
    include_str!("../../../../tests/fixtures/parser/graphqlx/ok/0002_import_wildcard.graphqlx"),
    include_str!("../../../../tests/fixtures/parser/graphqlx/ok/0003_import_with_alias.graphqlx"),
    include_str!("../../../../tests/fixtures/parser/graphqlx/ok/0004_generics_simple.graphqlx"),
    include_str!(
      "../../../../tests/fixtures/parser/graphqlx/ok/0005_generics_multiple_params.graphqlx"
    ),
    include_str!("../../../../tests/fixtures/parser/graphqlx/ok/0006_where_clause_simple.graphqlx"),
    include_str!(
      "../../../../tests/fixtures/parser/graphqlx/ok/0007_where_clause_multiple_bounds.graphqlx"
    ),
    include_str!("../../../../tests/fixtures/parser/graphqlx/ok/0008_map_value_simple.graphqlx"),
    include_str!("../../../../tests/fixtures/parser/graphqlx/ok/0009_map_value_nested.graphqlx"),
    include_str!(
      "../../../../tests/fixtures/parser/graphqlx/ok/0010_generics_with_default.graphqlx"
    ),
    include_str!(
      "../../../../tests/fixtures/parser/graphqlx/ok/0011_interface_with_generics.graphqlx"
    ),
    include_str!("../../../../tests/fixtures/parser/graphqlx/ok/0012_path_type.graphqlx"),
    include_str!("../../../../tests/fixtures/parser/graphqlx/ok/0013_complex_import.graphqlx"),
    include_str!("../../../../tests/fixtures/parser/graphqlx/ok/0014_generics_nested.graphqlx"),
    include_str!(
      "../../../../tests/fixtures/parser/graphqlx/ok/0015_extend_with_generics.graphqlx"
    ),
    include_str!(
      "../../../../tests/fixtures/parser/graphqlx/ok/0017_union_with_path_types.graphqlx"
    ),
    include_str!(
      "../../../../tests/fixtures/parser/graphqlx/ok/0018_union_with_path_types_and_generics.graphqlx"
    ),
    include_str!(
      "../../../../tests/fixtures/parser/graphqlx/ok/0019_input_with_map_default.graphqlx"
    ),
    include_str!(
      "../../../../tests/fixtures/parser/graphqlx/ok/0020_where_multiple_predicates.graphqlx"
    ),
    include_str!("../../../../tests/fixtures/parser/graphqlx/ok/0021_fat_arrow_in_map.graphqlx"),
  ];

  for source in FIXTURES {
    accept_document!(source);
  }
}

#[test]
fn type_system_document_preserves_imports_generic_paths_and_descriptions() {
  let document = drive_str(
    ast::TypeSystemDocument::graphqlx,
    "import { User as DomainUser } from \"domain.graphqlx\"\n\"doc\" type Box<T> where T: Node { value: T }",
  )
  .expect("type-system document");
  assert_eq!(document.definitions().len(), 2);
  assert!(document.definitions()[0].is_import());
  let ast::ImportOrTypeSystemDefinitionOrExtension::Definition(definition) =
    &document.definitions()[1]
  else {
    panic!("expected described type definition");
  };
  assert!(definition.description().is_some());
  let ast::TypeSystemDefinition::Type(ast::TypeDefinition::Object(object)) = definition.node()
  else {
    panic!("expected object type definition");
  };
  assert!("Box".equivalent(object.name().name().source()));
  assert!(object.name().generics().is_some());
  assert!(
    object
      .fields_definition()
      .expect("fields")
      .where_clause()
      .is_some()
  );
}

#[cfg(feature = "bytes")]
#[test]
fn type_system_direct_apis_preserve_owned_bytes() {
  let source = ::bytes::Bytes::from_static(b"type Box<T> { value: T }");
  let definition: ast::ObjectTypeDefinition<::bytes::Bytes> =
    drive_owned_bytes(ast::ObjectTypeDefinition::graphqlx, &source)
      .expect("owned Bytes object definition");
  assert_eq!(
    definition.name().name().source(),
    &::bytes::Bytes::from_static(b"Box")
  );
}

#[test]
fn type_system_definitions_commit_where_and_delimiter_diagnostics() {
  let error = drive_str(
    ast::ObjectTypeDefinition::graphqlx,
    "type Box<T> where T: Node",
  )
  .expect_err("where requires a field definition")
  .into_iter()
  .next()
  .expect("typed error");
  assert!(matches!(
    error.into_data(),
    ErrorData::UnexpectedToken(unexpected)
      if unexpected.expected() == &Expectation::LBrace && unexpected.found().is_none()
  ));

  let error = drive_str(ast::TypeSystemDocument::graphqlx, "type User { id: ID")
    .expect_err("unclosed fields definition")
    .into_iter()
    .next()
    .expect("unclosed error");
  assert!(matches!(
    error.into_data(),
    ErrorData::Unclosed(Unclosed::Object)
  ));

  let error = drive_str(ast::DirectiveDefinition::graphqlx, "directive @d on |")
    .expect_err("trailing directive location separator")
    .into_iter()
    .next()
    .expect("typed error");
  assert!(matches!(
    error.into_data(),
    ErrorData::UnexpectedToken(unexpected)
      if unexpected.expected() == &Expectation::Production("a directive location")
        && unexpected.found().is_none()
  ));
}

#[test]
fn separator_tails_preserve_leading_spans_and_committed_diagnostics() {
  let source = "| QUERY | FIELD";
  let locations = drive_str(ast::DirectiveLocations::graphqlx, source)
    .expect("leading directive-location separator");
  assert_eq!(locations.locations().len(), 2);
  assert_eq!(locations.span(), &SimpleSpan::new(0, source.len()));

  let locations = drive_slice(ast::DirectiveLocations::graphqlx, source.as_bytes())
    .expect("slice leading directive-location separator");
  assert_eq!(locations.locations().len(), 2);
  assert_eq!(locations.span(), &SimpleSpan::new(0, source.len()));

  let error = drive_str(ast::DirectiveLocations::graphqlx, "QUERY | type")
    .expect_err("the location after a separator remains committed")
    .into_iter()
    .next()
    .expect("typed error");
  assert!(matches!(
    error.into_data(),
    ErrorData::UnexpectedToken(unexpected)
      if unexpected.expected() == &Expectation::Production("a directive location")
        && unexpected.found().copied() == Some(SyntacticTokenKind::Identifier)
  ));

  let error = drive_str(ast::DirectiveLocations::graphqlx, "QUERY |")
    .expect_err("a trailing directive-location separator remains local")
    .into_iter()
    .next()
    .expect("typed error");
  assert!(matches!(
    error.into_data(),
    ErrorData::UnexpectedToken(unexpected)
      if unexpected.expected() == &Expectation::Production("a directive location")
        && unexpected.found().is_none()
  ));

  let object_source = "type User implements & ::pkg::Node & Resource<T> where T: Node { id: ID }";
  let object = drive_str(ast::ObjectTypeDefinition::graphqlx, object_source)
    .expect("leading implements separator and following where clause");
  let implements = object.implements().expect("implements clause");
  assert_eq!(implements.interfaces().len(), 2);
  assert_eq!(implements.span().start(), "type User ".len());
  assert_eq!(
    implements.span().end(),
    object_source.find(" where").expect("where boundary")
  );
  assert!(
    object
      .fields_definition()
      .expect("fields definition")
      .where_clause()
      .is_some()
  );

  let members_source = "= | ::pkg::User | Post<T>";
  let members = drive_str(ast::UnionMemberTypes::graphqlx, members_source)
    .expect("leading union-member separator");
  assert_eq!(members.members().len(), 2);
  assert_eq!(members.span(), &SimpleSpan::new(0, members_source.len()));
}

#[test]
fn described_type_system_definition_spans_optional_descriptions() {
  for source in ["\"doc\" type User", "type User"] {
    let definition = drive_str(ast::DescribedTypeSystemDefinition::graphqlx, source)
      .expect("described type-system definition");
    assert_eq!(definition.span(), &SimpleSpan::new(0, source.len()));
  }
}

#[test]
fn extension_dispatch_selects_all_named_shapes() {
  for source in [
    "extend scalar Date @specifiedBy(url: \"x\")",
    "extend type User { id: ID }",
    "extend interface Node { id: ID }",
    "extend union Search = User | Post",
    "extend enum Role { ADMIN }",
    "extend input Filter { limit: Int }",
  ] {
    drive_str(ast::TypeExtension::graphqlx, source).expect("type extension");
    drive_slice(ast::TypeExtension::graphqlx, source.as_bytes()).expect("slice type extension");
  }
}

/// Draft §3.13 and §3.13.6 in GraphQLx's own name binding: a directive definition's directives
/// sit after its arguments and before `repeatable`, its locations keep their trailing `where`, and
/// a directive extension names its target with an extension path.
#[test]
fn a_directive_definition_carries_its_own_directives_and_an_extension_adds_more() {
  let source = "directive @tag<T>(x: T) @meta(level: 1) @ns::other repeatable \
                on FIELD | DIRECTIVE_DEFINITION where T: Node";
  let definition =
    drive_str(ast::DirectiveDefinition::graphqlx, source).expect("directive definition");
  let directives = definition
    .directives()
    .expect("the definition's own directives");
  assert_eq!(directives.directives().len(), 2);
  assert!(definition.repeatable());
  assert_eq!(
    definition
      .locations()
      .target()
      .locations()
      .iter()
      .map(ast::Location::as_str)
      .collect::<Vec<_>>(),
    ["FIELD", "DIRECTIVE_DEFINITION"]
  );
  assert!(definition.locations().where_clause().is_some());
  assert!(
    drive_str(
      ast::DirectiveDefinition::graphqlx,
      "directive @tag repeatable @meta on FIELD"
    )
    .is_err(),
    "the directives come before `repeatable`"
  );

  let source = "extend directive @ns::tag @meta(level: 1) @other";
  let extension =
    drive_str(ast::DirectiveExtension::graphqlx, source).expect("directive extension");
  assert_eq!(extension.span(), &SimpleSpan::new(0, source.len()));
  assert_eq!(extension.name().path().segments().len(), 2);
  assert_eq!(extension.directives().directives().len(), 2);
  let extension =
    drive_str(ast::TypeSystemExtension::graphqlx, source).expect("type-system extension");
  assert!(extension.is_directive());
  assert!(
    drive_str(ast::TypeExtension::graphqlx, source).is_err(),
    "a directive extension is not a type extension"
  );
  for source in [
    "extend directive @tag",
    "extend directive tag @meta",
    "extend directive @tag @meta(level: $v)",
  ] {
    assert!(
      drive_str(ast::DirectiveExtension::graphqlx, source).is_err(),
      "{source:?} is refused"
    );
  }
}

/// A committed definition or extension phase names the production it expected: under
/// `smear::syntax::unexpected-token` where a token stood in its place, and under
/// `smear::syntax::unexpected-end-of-input` where the source ended there.
#[test]
fn a_rejected_phase_names_the_production_it_expected() {
  use std::string::{String, ToString};

  use smear_lexer::SyntaxErrorKind;
  use tokora::diagnostic::{Code, Diagnose};

  /// The code and the sentence of the one error a refused fixture reports.
  fn refusal<O>(result: Result<O, GraphqlxErrors<&str>>) -> (Code, String) {
    let Err(errors) = result else {
      panic!("the fixture should be refused");
    };
    let error = errors
      .into_iter()
      .next()
      .expect("the fatal context reports one error");
    (error.code(), error.to_string())
  }

  let token = SyntaxErrorKind::UnexpectedToken;
  let end = SyntaxErrorKind::UnexpectedEndOfInput;

  for (what, (code, rendered), kind, expected) in [
    (
      "an entry opened by no keyword",
      refusal(drive_str(
        ast::TypeSystemDefinitionOrExtension::graphqlx,
        "1",
      )),
      token,
      "unexpected integer, expected a type-system definition or an extension",
    ),
    (
      "an entry named by no keyword",
      refusal(drive_str(
        ast::TypeSystemDefinitionOrExtension::graphqlx,
        "foo",
      )),
      token,
      "unexpected name, expected a type-system definition or an extension",
    ),
    (
      "a type definition named by no keyword",
      refusal(drive_str(ast::TypeDefinition::graphqlx, "foo")),
      token,
      "unexpected name, expected a type definition",
    ),
    (
      "a type definition opened by no keyword",
      refusal(drive_str(ast::TypeDefinition::graphqlx, "1")),
      token,
      "unexpected integer, expected a type definition",
    ),
    (
      "an extension of a shape that is not a type",
      refusal(drive_str(ast::TypeExtension::graphqlx, "extend foo")),
      token,
      "unexpected name, expected a type extension",
    ),
    (
      "an extension that no shape follows",
      refusal(drive_str(ast::TypeExtension::graphqlx, "extend 1")),
      token,
      "unexpected integer, expected a type extension",
    ),
    (
      "a type-system extension of no shape",
      refusal(drive_str(ast::TypeSystemExtension::graphqlx, "extend foo")),
      token,
      "unexpected name, expected a type-system extension",
    ),
    (
      "a type-system extension that no shape follows",
      refusal(drive_str(ast::TypeSystemExtension::graphqlx, "extend 1")),
      token,
      "unexpected integer, expected a type-system extension",
    ),
    (
      "a union extension that adds nothing",
      refusal(drive_str(
        ast::UnionTypeExtension::graphqlx,
        "extend union U",
      )),
      end,
      "unexpected end of input, expected directives or union member types",
    ),
    (
      "an enum extension that adds nothing",
      refusal(drive_str(ast::EnumTypeExtension::graphqlx, "extend enum E")),
      end,
      "unexpected end of input, expected directives or an enum values definition",
    ),
    (
      "an input extension that adds nothing",
      refusal(drive_str(
        ast::InputObjectTypeExtension::graphqlx,
        "extend input I",
      )),
      end,
      "unexpected end of input, expected directives or an input fields definition",
    ),
    (
      "a schema extension that adds nothing",
      refusal(drive_str(ast::SchemaExtension::graphqlx, "extend schema")),
      end,
      "unexpected end of input, expected directives or a root operation types definition",
    ),
    (
      "a directive extension that adds nothing",
      refusal(drive_str(
        ast::DirectiveExtension::graphqlx,
        "extend directive @tag",
      )),
      end,
      "unexpected end of input, expected `@`",
    ),
    (
      "a root operation with no operation type",
      refusal(drive_str(ast::RootOperationTypeDefinition::graphqlx, "")),
      end,
      "unexpected end of input, expected `query`, `mutation` or `subscription`",
    ),
  ] {
    assert_eq!(code, kind.code(), "{what}");
    assert_eq!(rendered, expected, "{what}");
  }
}
