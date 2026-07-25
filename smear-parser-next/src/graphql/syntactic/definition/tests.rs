//! SDL definition-production tests.
//!
//! The suite follows the concrete GraphQL syntactic API: public AST-associated
//! parsers are driven over `str`, `[u8]`, and `Bytes`; committed phases retain a
//! rejected token for the caller; and optional delimiter productions decline
//! without consuming their absent opener.

use smear_lexer::graphql::syntactic::SyntacticTokenKind;
use tokora::{
  FatalContext, Parse, Parser, SimpleSpan, try_parse_input::ParseAttempt, utils::cmp::Equivalent,
};

use super::description;
use crate::graphql::{
  GraphQL,
  ast::{
    ArgumentsDefinition, Described, DirectiveDefinition, DirectiveLocations, EnumTypeDefinition,
    EnumValueDefinition, EnumValuesDefinition, FieldDefinition, FieldsDefinition,
    ImplementInterfaces, InputFieldsDefinition, InputObjectTypeDefinition, InputValueDefinition,
    InterfaceTypeDefinition, Location, Name, ObjectTypeDefinition, RootOperationTypeDefinition,
    RootOperationTypesDefinition, ScalarTypeDefinition, SchemaDefinition, StringValue,
    TypeDefinition, UnionMemberTypes, UnionTypeDefinition,
  },
  error::{ErrorData, Expectation, GraphqlErrors, Unclosed},
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
  Parser::with_parser_of::<'inp, GraphqlLexer<'inp, str>, O, GraphqlErrors<&'inp str>, _, GraphQL>(
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
  Parser::with_parser_of::<
    'inp,
    GraphqlLexer<'inp, [u8]>,
    O,
    GraphqlErrors<&'inp [u8]>,
    _,
    GraphQL,
  >(parser)
  .parse_slice(source)
}

#[cfg(feature = "bytes")]
fn drive_bytes<'inp, O>(
  parser: impl for<'c> FnMut(
    &mut GraphqlInput<'inp, 'c, [u8], SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  source: &'inp ::bytes::Bytes,
) -> Result<O, GraphqlErrors<&'inp [u8]>> {
  Parser::with_parser_of::<
    'inp,
    GraphqlLexer<'inp, [u8]>,
    O,
    GraphqlErrors<&'inp [u8]>,
    _,
    GraphQL,
  >(parser)
  .parse_bytes(source)
}

macro_rules! accept_all {
  ($parser:expr, $source:expr, $check:path) => {{
    $check(drive_str($parser, $source).expect(concat!("str accepts: ", $source)));
    $check(drive_slice($parser, $source.as_bytes()).expect(concat!("[u8] accepts: ", $source)));
    #[cfg(feature = "bytes")]
    {
      let bytes = ::bytes::Bytes::from_static($source.as_bytes());
      $check(drive_bytes($parser, &bytes).expect(concat!("Bytes accepts: ", $source)));
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
  }};
}

fn assert_unexpected(
  result: Result<(), GraphqlErrors<&str>>,
  expected: Expectation,
  found: Option<SyntacticTokenKind>,
) {
  let error = result
    .expect_err("fixture should fail")
    .into_iter()
    .next()
    .expect("fatal emitter reports an error");
  assert!(matches!(
    error.data(),
    ErrorData::UnexpectedToken(unexpected)
      if unexpected.expected() == &expected && unexpected.found() == found.as_ref()
  ));
}

#[test]
fn description_is_optional_and_non_consuming() {
  fn some<S: AsRef<[u8]>>(value: Option<StringValue<S>>) {
    assert!(value.is_some());
  }

  accept_all!(description, "\"doc\"", some);
  accept_all!(description, "\"\"\"block\"\"\"", some);

  let name = drive_str(
    |inp| {
      assert!(description(inp)?.is_none());
      Name::<_>::graphql(inp)
    },
    "Name",
  )
  .expect("description declines without consuming a name");
  assert_eq!(name.source_ref(), &"Name");
}

#[test]
fn input_value_and_field_definitions_use_committed_local_phases() {
  fn minimal_input<S: AsRef<[u8]>>(value: InputValueDefinition<S>) {
    assert!(value.description().is_none());
    assert_eq!(value.name().source_ref().as_ref(), b"limit");
    assert!(value.default_value().is_none());
    assert!(value.directives().is_none());
  }
  accept_all!(
    InputValueDefinition::<_>::graphql,
    "limit: Int",
    minimal_input
  );

  fn input<S: AsRef<[u8]>>(value: InputValueDefinition<S>) {
    assert!(value.description().is_some());
    assert_eq!(value.name().source_ref().as_ref(), b"limit");
    assert!(value.default_value().is_some());
    assert!(value.directives().is_some());
  }
  accept_all!(
    InputValueDefinition::<_>::graphql,
    "\"doc\" limit: Int = 10 @deprecated",
    input
  );

  fn minimal_field<S: AsRef<[u8]>>(value: FieldDefinition<S>) {
    assert!(value.description().is_none());
    assert_eq!(value.name().source_ref().as_ref(), b"search");
    assert!(value.arguments_definition().is_none());
    assert!(value.directives().is_none());
  }
  accept_all!(
    FieldDefinition::<_>::graphql,
    "search: String",
    minimal_field
  );

  fn field<S: AsRef<[u8]>>(value: FieldDefinition<S>) {
    assert!(value.description().is_some());
    assert_eq!(value.name().source_ref().as_ref(), b"search");
    assert!(value.arguments_definition().is_some());
    assert!(value.directives().is_some());
  }
  accept_all!(
    FieldDefinition::<_>::graphql,
    "\"doc\" search(limit: Int): String @deprecated",
    field
  );

  reject_all!(InputValueDefinition::<_>::graphql, "limit");
  reject_all!(InputValueDefinition::<_>::graphql, "limit:");
  reject_all!(FieldDefinition::<_>::graphql, "search");

  assert_unexpected(
    drive_str(
      |inp| InputValueDefinition::<_>::graphql(inp).map(|_| ()),
      ": Int",
    ),
    Expectation::Name,
    Some(SyntacticTokenKind::Colon),
  );
  assert_unexpected(
    drive_str(
      |inp| FieldDefinition::<_>::graphql(inp).map(|_| ()),
      "name:",
    ),
    Expectation::Type,
    None,
  );

  let (expected, next) = drive_str(
    |inp| {
      let error =
        InputValueDefinition::<_>::graphql(inp).expect_err("missing name should be rejected");
      let expected = matches!(
        error.into_iter().next().expect("one error").into_data(),
        ErrorData::UnexpectedToken(unexpected)
          if unexpected.expected() == &Expectation::Name
      );
      let next = inp
        .next()?
        .expect("the rejected token remains available")
        .data()
        .kind();
      Ok::<_, GraphqlErrors<&str>>((expected, next))
    },
    ": Int",
  )
  .expect("phase diagnostic must not consume the rejected token");
  assert!(expected);
  assert_eq!(next, SyntacticTokenKind::Colon);
}

#[test]
fn delimited_definition_groups_are_nonempty_typed_and_optional() {
  fn arguments<S: AsRef<[u8]>>(value: ArgumentsDefinition<S>) {
    assert_eq!(value.input_value_definitions().len(), 2);
  }
  accept_all!(
    ArgumentsDefinition::<_>::graphql,
    "(a: Int, b: String)",
    arguments
  );

  fn fields<S: AsRef<[u8]>>(value: FieldsDefinition<S>) {
    assert_eq!(value.field_definitions().len(), 2);
  }
  accept_all!(
    FieldsDefinition::<_>::graphql,
    "{ a: Int b: String }",
    fields
  );

  fn input_fields<S: AsRef<[u8]>>(value: InputFieldsDefinition<S>) {
    assert_eq!(value.input_value_definitions().len(), 2);
  }
  accept_all!(
    InputFieldsDefinition::<_>::graphql,
    "{ a: Int b: String }",
    input_fields
  );

  assert!(matches!(
    drive_str(ArgumentsDefinition::<_>::try_graphql, "name").expect("try parser runs"),
    ParseAttempt::Decline
  ));
  assert!(matches!(
    drive_str(FieldsDefinition::<_>::try_graphql, "name").expect("try parser runs"),
    ParseAttempt::Decline
  ));
  assert!(matches!(
    drive_str(InputFieldsDefinition::<_>::try_graphql, "name").expect("try parser runs"),
    ParseAttempt::Decline
  ));

  reject_all!(ArgumentsDefinition::<_>::graphql, "()");
  reject_all!(FieldsDefinition::<_>::graphql, "{}");
  reject_all!(InputFieldsDefinition::<_>::graphql, "{}");

  let parens = drive_str(
    |inp| ArgumentsDefinition::<_>::graphql(inp).map(|_| ()),
    "(a: Int",
  )
  .expect_err("unclosed arguments should fail")
  .into_iter()
  .next()
  .expect("one error");
  assert!(matches!(
    parens.data(),
    ErrorData::Unclosed(Unclosed::Parentheses)
  ));

  let braces = drive_str(
    |inp| FieldsDefinition::<_>::graphql(inp).map(|_| ()),
    "{ a: Int",
  )
  .expect_err("unclosed fields should fail")
  .into_iter()
  .next()
  .expect("one error");
  assert!(matches!(
    braces.data(),
    ErrorData::Unclosed(Unclosed::Object)
  ));
}

#[test]
fn implements_and_union_members_use_native_separated_lists() {
  fn one_interface<S: AsRef<[u8]>>(value: ImplementInterfaces<Name<S>>) {
    assert_eq!(value.interfaces().len(), 1);
    assert_eq!(value.interfaces()[0].source_ref().as_ref(), b"Node");
  }
  accept_all!(
    ImplementInterfaces::<Name<_>>::graphql,
    "implements Node",
    one_interface
  );

  fn interfaces<S: AsRef<[u8]>>(value: ImplementInterfaces<Name<S>>) {
    assert_eq!(value.interfaces().len(), 2);
    assert_eq!(value.interfaces()[0].source_ref().as_ref(), b"Node");
  }
  accept_all!(
    ImplementInterfaces::<Name<_>>::graphql,
    "implements & Node & Named",
    interfaces
  );

  fn one_member<S: AsRef<[u8]>>(value: UnionMemberTypes<Name<S>>) {
    assert_eq!(value.members().len(), 1);
    assert_eq!(value.members()[0].source_ref().as_ref(), b"Dog");
  }
  accept_all!(UnionMemberTypes::<Name<_>>::graphql, "= Dog", one_member);

  fn members<S: AsRef<[u8]>>(value: UnionMemberTypes<Name<S>>) {
    assert_eq!(value.members().len(), 2);
    assert_eq!(value.members()[1].source_ref().as_ref(), b"Cat");
  }
  accept_all!(
    UnionMemberTypes::<Name<_>>::graphql,
    "= | Dog | Cat",
    members
  );

  assert!(matches!(
    drive_str(ImplementInterfaces::<Name<_>>::try_graphql, "@dir").expect("try parser runs"),
    ParseAttempt::Decline
  ));
  assert!(matches!(
    drive_str(UnionMemberTypes::<Name<_>>::try_graphql, "@dir").expect("try parser runs"),
    ParseAttempt::Decline
  ));
  reject_all!(ImplementInterfaces::<Name<_>>::graphql, "implements Node &");
  reject_all!(UnionMemberTypes::<Name<_>>::graphql, "= Dog |");
}

#[test]
fn optional_sdl_groups_accept_after_their_opener() {
  assert!(matches!(
    drive_str(ArgumentsDefinition::<_>::try_graphql, "(a: Int)").expect("try parser runs"),
    ParseAttempt::Accept(_)
  ));
  assert!(matches!(
    drive_str(FieldsDefinition::<_>::try_graphql, "{ a: Int }").expect("try parser runs"),
    ParseAttempt::Accept(_)
  ));
  assert!(matches!(
    drive_str(InputFieldsDefinition::<_>::try_graphql, "{ a: Int }").expect("try parser runs"),
    ParseAttempt::Accept(_)
  ));
  assert!(matches!(
    drive_str(
      ImplementInterfaces::<Name<_>>::try_graphql,
      "implements Node"
    )
    .expect("try parser runs"),
    ParseAttempt::Accept(_)
  ));
  assert!(matches!(
    drive_str(UnionMemberTypes::<Name<_>>::try_graphql, "= Node").expect("try parser runs"),
    ParseAttempt::Accept(_)
  ));
  assert!(matches!(
    drive_str(EnumValuesDefinition::<_>::try_graphql, "{ A }").expect("try parser runs"),
    ParseAttempt::Accept(_)
  ));

  let name = drive_str(
    |inp| {
      assert!(matches!(
        ArgumentsDefinition::<_>::try_graphql(inp)?,
        ParseAttempt::Decline
      ));
      assert!(matches!(
        FieldsDefinition::<_>::try_graphql(inp)?,
        ParseAttempt::Decline
      ));
      assert!(matches!(
        InputFieldsDefinition::<_>::try_graphql(inp)?,
        ParseAttempt::Decline
      ));
      assert!(matches!(
        ImplementInterfaces::<Name<_>>::try_graphql(inp)?,
        ParseAttempt::Decline
      ));
      assert!(matches!(
        UnionMemberTypes::<Name<_>>::try_graphql(inp)?,
        ParseAttempt::Decline
      ));
      assert!(matches!(
        EnumValuesDefinition::<_>::try_graphql(inp)?,
        ParseAttempt::Decline
      ));
      Name::<_>::graphql(inp)
    },
    "Name",
  )
  .expect("each declining parser leaves its head available");
  assert_eq!(name.source_ref(), &"Name");
}

#[test]
fn directive_locations_are_exact_and_lossless_at_the_token_level() {
  const LOCATIONS: [&str; 19] = [
    "QUERY",
    "MUTATION",
    "SUBSCRIPTION",
    "FIELD",
    "FRAGMENT_DEFINITION",
    "FRAGMENT_SPREAD",
    "INLINE_FRAGMENT",
    "VARIABLE_DEFINITION",
    "SCHEMA",
    "SCALAR",
    "OBJECT",
    "FIELD_DEFINITION",
    "ARGUMENT_DEFINITION",
    "INTERFACE",
    "UNION",
    "ENUM",
    "ENUM_VALUE",
    "INPUT_OBJECT",
    "INPUT_FIELD_DEFINITION",
  ];

  fn assert_location(source: &str) {
    let location = drive_str(Location::graphql, source).expect("str accepts directive location");
    assert_eq!(location.as_str(), source);
    assert_eq!(*location.span(), SimpleSpan::new(0, source.len()));

    let location =
      drive_slice(Location::graphql, source.as_bytes()).expect("[u8] accepts directive location");
    assert_eq!(location.as_str(), source);
    assert_eq!(*location.span(), SimpleSpan::new(0, source.len()));

    #[cfg(feature = "bytes")]
    {
      let bytes = ::bytes::Bytes::copy_from_slice(source.as_bytes());
      let location =
        drive_bytes(Location::graphql, &bytes).expect("Bytes accepts directive location");
      assert_eq!(location.as_str(), source);
      assert_eq!(*location.span(), SimpleSpan::new(0, source.len()));
    }
  }

  for location in LOCATIONS {
    assert_location(location);
  }

  fn all(locations: DirectiveLocations<Location>) {
    assert_eq!(locations.locations().len(), 19);
    assert_eq!(
      locations
        .locations()
        .iter()
        .map(Location::as_str)
        .collect::<Vec<_>>(),
      LOCATIONS,
    );
  }
  accept_all!(
    DirectiveLocations::<Location>::graphql,
    "| QUERY | MUTATION | SUBSCRIPTION | FIELD | FRAGMENT_DEFINITION | FRAGMENT_SPREAD | INLINE_FRAGMENT | VARIABLE_DEFINITION | SCHEMA | SCALAR | OBJECT | FIELD_DEFINITION | ARGUMENT_DEFINITION | INTERFACE | UNION | ENUM | ENUM_VALUE | INPUT_OBJECT | INPUT_FIELD_DEFINITION",
    all
  );

  fn assert_not_location(source: &str) {
    assert!(
      drive_str(|inp| Location::graphql(inp).map(|_| ()), source).is_err(),
      "str rejects non-location {source:?}",
    );
    assert!(
      drive_slice(|inp| Location::graphql(inp).map(|_| ()), source.as_bytes()).is_err(),
      "[u8] rejects non-location {source:?}",
    );
    #[cfg(feature = "bytes")]
    {
      let bytes = ::bytes::Bytes::copy_from_slice(source.as_bytes());
      assert!(
        drive_bytes(|inp| Location::graphql(inp).map(|_| ()), &bytes).is_err(),
        "Bytes rejects non-location {source:?}",
      );
    }
  }

  for keyword in [
    "query",
    "mutation",
    "subscription",
    "schema",
    "scalar",
    "interface",
    "union",
    "enum",
  ] {
    assert_not_location(keyword);
  }

  assert_unexpected(
    drive_str(
      |inp| Location::graphql(inp).map(|_| ()),
      "FIELD_DEFINITIONS",
    ),
    Expectation::DirectiveLocation,
    Some(SyntacticTokenKind::Identifier),
  );
  let next = drive_str(
    |inp| {
      Location::graphql(inp).expect_err("unknown directive location is rejected");
      Ok::<_, GraphqlErrors<&str>>(
        inp
          .next()?
          .expect("unknown directive location is consumed")
          .data()
          .kind(),
      )
    },
    "FIELD_DEFINITIONS :",
  )
  .expect("the token after an unknown directive location remains available");
  assert_eq!(next, SyntacticTokenKind::Colon);

  reject_all!(DirectiveLocations::<Location>::graphql, "");
  reject_all!(DirectiveLocations::<Location>::graphql, "BOGUS");
  reject_all!(DirectiveLocations::<Location>::graphql, "FIELD |");
}

#[test]
fn enum_values_and_root_operations_are_checked_in_their_grammar_positions() {
  fn plain_enum_value<S: AsRef<[u8]>>(value: EnumValueDefinition<S>) {
    assert!(value.description().is_none());
    assert_eq!(value.value().source_ref().as_ref(), b"ACTIVE");
    assert!(value.directives().is_none());
  }
  accept_all!(
    EnumValueDefinition::<_>::graphql,
    "ACTIVE",
    plain_enum_value
  );

  fn enum_value<S: AsRef<[u8]>>(value: EnumValueDefinition<S>) {
    assert!(value.description().is_some());
    assert_eq!(value.value().source_ref().as_ref(), b"ACTIVE");
    assert!(value.directives().is_some());
  }
  accept_all!(
    EnumValueDefinition::<_>::graphql,
    "\"doc\" ACTIVE @deprecated",
    enum_value
  );
  for literal in ["true", "false", "null"] {
    reject_all!(EnumValueDefinition::<_>::graphql, literal);
  }

  fn values<S: AsRef<[u8]>>(value: EnumValuesDefinition<S>) {
    assert_eq!(value.enum_value_definitions().len(), 2);
  }
  accept_all!(EnumValuesDefinition::<_>::graphql, "{ A B }", values);
  fn soft_keywords<S: AsRef<[u8]>>(value: EnumTypeDefinition<S>) {
    assert_eq!(
      value
        .enum_values_definition()
        .expect("values")
        .enum_value_definitions()
        .len(),
      3
    );
  }
  accept_all!(
    EnumTypeDefinition::<_>::graphql,
    "enum Keywords { on type query }",
    soft_keywords
  );
  assert!(matches!(
    drive_str(EnumValuesDefinition::<_>::try_graphql, "A").expect("try parser runs"),
    ParseAttempt::Decline
  ));

  fn root<S: AsRef<[u8]>>(value: RootOperationTypeDefinition<S>) {
    assert!(value.operation_type().is_query());
    assert_eq!(value.name().source_ref().as_ref(), b"Query");
  }
  accept_all!(
    RootOperationTypeDefinition::<_>::graphql,
    "query: Query",
    root
  );
  reject_all!(RootOperationTypeDefinition::<_>::graphql, "nope: Query");

  fn roots<S: AsRef<[u8]>>(value: RootOperationTypesDefinition<S>) {
    assert_eq!(value.root_operation_type_definitions().len(), 2);
  }
  accept_all!(
    RootOperationTypesDefinition::<_>::graphql,
    "{ query: Query mutation: Mutation }",
    roots
  );
  reject_all!(RootOperationTypeDefinition::<_>::graphql, "query Query");
  reject_all!(RootOperationTypesDefinition::<_>::graphql, "{}");
}

#[test]
fn concrete_type_definition_apis_cover_each_sdl_head() {
  fn scalar<S: AsRef<[u8]>>(value: ScalarTypeDefinition<S>) {
    assert_eq!(value.name().source_ref().as_ref(), b"DateTime");
  }
  accept_all!(
    ScalarTypeDefinition::<_>::graphql,
    "scalar DateTime",
    scalar
  );
  fn scalar_with_directive<S: AsRef<[u8]>>(value: ScalarTypeDefinition<S>) {
    assert!(value.directives().is_some());
  }
  accept_all!(
    ScalarTypeDefinition::<_>::graphql,
    "scalar DateTime @specifiedBy",
    scalar_with_directive
  );

  fn object<S: AsRef<[u8]>>(value: ObjectTypeDefinition<S>) {
    assert_eq!(
      value.implements().expect("implements").interfaces().len(),
      2
    );
    assert_eq!(
      value
        .fields_definition()
        .expect("fields")
        .field_definitions()
        .len(),
      1
    );
    assert!(value.directives().is_some());
  }
  accept_all!(
    ObjectTypeDefinition::<_>::graphql,
    "type User implements Node & Named @dir { id: ID! }",
    object
  );

  fn interface<S: AsRef<[u8]>>(value: InterfaceTypeDefinition<S>) {
    assert!(value.implements().is_some());
    assert_eq!(
      value
        .fields_definition()
        .expect("fields")
        .field_definitions()
        .len(),
      1
    );
  }
  accept_all!(
    InterfaceTypeDefinition::<_>::graphql,
    "interface Named implements Node { name: String }",
    interface
  );

  fn union<S: AsRef<[u8]>>(value: UnionTypeDefinition<S>) {
    assert_eq!(value.member_types().expect("members").members().len(), 2);
  }
  accept_all!(
    UnionTypeDefinition::<_>::graphql,
    "union Pet = Dog | Cat",
    union
  );

  fn enumeration<S: AsRef<[u8]>>(value: EnumTypeDefinition<S>) {
    assert_eq!(
      value
        .enum_values_definition()
        .expect("values")
        .enum_value_definitions()
        .len(),
      2
    );
  }
  accept_all!(
    EnumTypeDefinition::<_>::graphql,
    "enum Direction { NORTH SOUTH }",
    enumeration
  );

  fn input_object<S: AsRef<[u8]>>(value: InputObjectTypeDefinition<S>) {
    assert_eq!(
      value
        .fields_definition()
        .expect("fields")
        .input_value_definitions()
        .len(),
      2
    );
  }
  accept_all!(
    InputObjectTypeDefinition::<_>::graphql,
    "input Point { x: Int y: Int }",
    input_object
  );

  reject_all!(ObjectTypeDefinition::<_>::graphql, "type Bad {}");
  reject_all!(EnumTypeDefinition::<_>::graphql, "enum Bad { true }");
}

#[test]
fn concrete_type_definitions_allow_their_bare_forms() {
  fn bare_object<S: AsRef<[u8]>>(value: ObjectTypeDefinition<S>) {
    assert!(value.implements().is_none());
    assert!(value.fields_definition().is_none());
  }
  accept_all!(
    ObjectTypeDefinition::<_>::graphql,
    "type Empty",
    bare_object
  );

  fn bare_interface<S: AsRef<[u8]>>(value: InterfaceTypeDefinition<S>) {
    assert!(value.implements().is_none());
    assert!(value.fields_definition().is_none());
  }
  accept_all!(
    InterfaceTypeDefinition::<_>::graphql,
    "interface Empty",
    bare_interface
  );

  fn bare_union<S: AsRef<[u8]>>(value: UnionTypeDefinition<S>) {
    assert!(value.member_types().is_none());
    assert!(value.directives().is_some());
  }
  accept_all!(
    UnionTypeDefinition::<_>::graphql,
    "union Empty @dir",
    bare_union
  );

  fn bare_enum<S: AsRef<[u8]>>(value: EnumTypeDefinition<S>) {
    assert!(value.enum_values_definition().is_none());
  }
  accept_all!(EnumTypeDefinition::<_>::graphql, "enum Empty", bare_enum);

  fn bare_input<S: AsRef<[u8]>>(value: InputObjectTypeDefinition<S>) {
    assert!(value.fields_definition().is_none());
  }
  accept_all!(
    InputObjectTypeDefinition::<_>::graphql,
    "input Empty",
    bare_input
  );
}

#[test]
fn directive_schema_and_type_dispatch_share_the_concrete_ast_api() {
  fn directive<S: AsRef<[u8]>>(value: DirectiveDefinition<S>) {
    assert_eq!(value.name().source_ref().as_ref(), b"tag");
    assert!(value.arguments_definition().is_some());
    assert!(value.repeatable());
    assert_eq!(value.locations().locations().len(), 2);
  }
  accept_all!(
    DirectiveDefinition::<_>::graphql,
    "directive @tag(reason: String) repeatable on FIELD | OBJECT",
    directive
  );
  fn simple_directive<S: AsRef<[u8]>>(value: DirectiveDefinition<S>) {
    assert_eq!(value.name().source_ref().as_ref(), b"deprecated");
    assert!(value.arguments_definition().is_none());
    assert!(!value.repeatable());
    assert_eq!(value.locations().locations().len(), 1);
  }
  accept_all!(
    DirectiveDefinition::<_>::graphql,
    "directive @deprecated on FIELD",
    simple_directive
  );
  reject_all!(DirectiveDefinition::<_>::graphql, "directive @tag on");

  fn schema<S: AsRef<[u8]>>(value: SchemaDefinition<S>) {
    assert!(value.directives().is_some());
    assert_eq!(
      value
        .root_operation_types_definition()
        .root_operation_type_definitions()
        .len(),
      2
    );
  }
  accept_all!(
    SchemaDefinition::<_>::graphql,
    "schema @link { query: Query mutation: Mutation }",
    schema
  );
  fn bare_schema<S: AsRef<[u8]>>(value: SchemaDefinition<S>) {
    assert!(value.directives().is_none());
    assert_eq!(
      value
        .root_operation_types_definition()
        .root_operation_type_definitions()
        .len(),
      1
    );
  }
  accept_all!(
    SchemaDefinition::<_>::graphql,
    "schema { query: Query }",
    bare_schema
  );
  reject_all!(SchemaDefinition::<_>::graphql, "schema {}");
  reject_all!(SchemaDefinition::<_>::graphql, "schema");

  fn object<S: AsRef<[u8]>>(value: TypeDefinition<S>) {
    assert!(value.is_object());
  }
  fn scalar<S: AsRef<[u8]>>(value: TypeDefinition<S>) {
    assert!(value.is_scalar());
  }
  accept_all!(
    TypeDefinition::<_>::graphql,
    "type T { value: Int }",
    object
  );
  accept_all!(TypeDefinition::<_>::graphql, "scalar T", scalar);
}

#[test]
fn type_definition_dispatches_all_six_heads_once() {
  fn scalar<S: AsRef<[u8]>>(value: TypeDefinition<S>) {
    assert!(value.is_scalar());
  }
  fn object<S: AsRef<[u8]>>(value: TypeDefinition<S>) {
    assert!(value.is_object());
  }
  fn interface<S: AsRef<[u8]>>(value: TypeDefinition<S>) {
    assert!(value.is_interface());
  }
  fn union<S: AsRef<[u8]>>(value: TypeDefinition<S>) {
    assert!(value.is_union());
  }
  fn enumeration<S: AsRef<[u8]>>(value: TypeDefinition<S>) {
    assert!(value.is_enum());
  }
  fn input_object<S: AsRef<[u8]>>(value: TypeDefinition<S>) {
    assert!(value.is_input_object());
  }

  accept_all!(TypeDefinition::<_>::graphql, "scalar DateTime", scalar);
  accept_all!(TypeDefinition::<_>::graphql, "type User { id: ID }", object);
  accept_all!(
    TypeDefinition::<_>::graphql,
    "interface Node { id: ID }",
    interface
  );
  accept_all!(TypeDefinition::<_>::graphql, "union Pet = Dog | Cat", union);
  accept_all!(
    TypeDefinition::<_>::graphql,
    "enum Direction { NORTH SOUTH }",
    enumeration
  );
  accept_all!(
    TypeDefinition::<_>::graphql,
    "input Point { x: Int }",
    input_object
  );
}

#[test]
fn type_and_directive_definition_parity_rows_remain_stable() {
  const TYPE_DEFINITION_ROWS: &[(&str, bool)] = &[
    ("scalar DateTime", true),
    ("scalar DateTime @specifiedBy", true),
    ("scalar DateTime @dir", true),
    ("type Foo", true),
    ("type Foo { a: Int }", true),
    ("type Foo implements Node { id: ID }", true),
    ("interface Node { id: ID }", true),
    ("union Result = Ok | Err", true),
    ("union Result", true),
    ("enum State { OPEN CLOSED }", true),
    ("enum State", true),
    ("input Filter { limit: Int }", true),
    ("scalar", false),
    ("type", false),
    ("schema { query: Query }", false),
    ("directive @tag on FIELD", false),
    ("bogus Type", false),
    ("", false),
  ];
  for (source, accepted) in TYPE_DEFINITION_ROWS {
    assert_eq!(
      drive_str(|inp| TypeDefinition::<_>::graphql(inp).map(|_| ()), source).is_ok(),
      *accepted,
      "str TypeDefinition({source:?})",
    );
    assert_eq!(
      drive_slice(
        |inp| TypeDefinition::<_>::graphql(inp).map(|_| ()),
        source.as_bytes(),
      )
      .is_ok(),
      *accepted,
      "[u8] TypeDefinition({source:?})",
    );
  }

  const DIRECTIVE_DEFINITION_ROWS: &[(&str, bool)] = &[
    ("directive @tag on FIELD", true),
    ("directive @tag on FIELD | OBJECT", true),
    ("directive @tag repeatable on FIELD", true),
    ("directive @tag(reason: String) on FIELD", true),
    ("directive @tag on | FIELD", true),
    ("directive tag on FIELD", false),
    ("directive @tag FIELD", false),
    ("directive @tag on", false),
    ("directive @tag on BOGUS", false),
  ];
  for (source, accepted) in DIRECTIVE_DEFINITION_ROWS {
    assert_eq!(
      drive_str(
        |inp| DirectiveDefinition::<_>::graphql(inp).map(|_| ()),
        source
      )
      .is_ok(),
      *accepted,
      "str DirectiveDefinition({source:?})",
    );
    assert_eq!(
      drive_slice(
        |inp| DirectiveDefinition::<_>::graphql(inp).map(|_| ()),
        source.as_bytes(),
      )
      .is_ok(),
      *accepted,
      "[u8] DirectiveDefinition({source:?})",
    );
  }
}

#[test]
fn described_type_definition_commits_after_a_description() {
  fn described<S: AsRef<[u8]>>(value: Described<TypeDefinition<S>, S>) {
    assert!(value.description().is_some());
    assert!(value.node().is_object());
  }
  accept_all!(
    Described::<TypeDefinition<_>, _>::graphql,
    "\"doc\" type T { value: Int }",
    described
  );
  fn undescribed<S: AsRef<[u8]>>(value: Described<TypeDefinition<S>, S>) {
    assert!(value.description().is_none());
    assert!(value.node().is_scalar());
  }
  accept_all!(
    Described::<TypeDefinition<_>, _>::graphql,
    "scalar DateTime",
    undescribed
  );

  let (expected, next) = drive_str(
    |inp| {
      let error = Described::<TypeDefinition<_>, _>::graphql(inp)
        .expect_err("a described non-type head is committed");
      let expected = matches!(
        error.into_iter().next().expect("one error").into_data(),
        ErrorData::UnexpectedToken(unexpected)
          if unexpected.expected() == &Expectation::Keyword("type definition")
      );
      let next = inp
        .next()?
        .expect("the invalid head remains available")
        .data()
        .kind();
      Ok::<_, GraphqlErrors<&str>>((expected, next))
    },
    "\"doc\" query",
  )
  .expect("the committed diagnostic retains its rejected token");
  assert!(expected);
  assert_eq!(next, SyntacticTokenKind::Identifier);
}

#[test]
fn source_typed_associated_api_remains_inferred_from_the_input() {
  let str_value =
    drive_str(InputValueDefinition::<_>::graphql, "x: Int").expect("str source is inferred");
  assert!("x".equivalent(str_value.name().source_ref()));

  let slice_value = drive_slice(InputValueDefinition::<_>::graphql, b"x: Int")
    .expect("byte-slice source is inferred");
  assert_eq!(slice_value.name().source_ref(), &&b"x"[..]);

  #[cfg(feature = "bytes")]
  {
    let bytes = ::bytes::Bytes::from_static(b"x: Int");
    let value =
      drive_bytes(InputValueDefinition::<_>::graphql, &bytes).expect("Bytes source is inferred");
    assert_eq!(value.name().source_ref(), &&b"x"[..]);
  }
}
