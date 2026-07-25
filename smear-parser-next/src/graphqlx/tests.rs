//! End-to-end GraphQLX production tests.
//!
//! The parser API is deliberately source-generic.  Each successful case below
//! is exercised through the concrete lexer over `str`, `[u8]`, and `Bytes`
//! (when enabled), so the AST aliases and parser entry points cannot
//! accidentally become tied to one source representation.

use smear_lexer::graphqlx::{LitFloat, LitInt, syntactic::SyntacticTokenKind};
use tokora::{
  FatalContext, Parse, Parser, SimpleSpan, span::AsSpan, try_parse_input::ParseAttempt,
  utils::cmp::Equivalent,
};

use super::{
  GraphQLX, ast,
  error::{ErrorData, Expectation, GraphqlxErrors, Unclosed},
  syntactic::{self, GraphqlxInput, GraphqlxLexer},
};

/// The fatal context a `str`-sourced parse runs under.
type StrCtx<'inp> =
  FatalContext<'inp, GraphqlxLexer<'inp, str>, GraphqlxErrors<&'inp str>, GraphQLX>;
/// The fatal context a `[u8]`-sourced parse runs under.
type SliceCtx<'inp> =
  FatalContext<'inp, GraphqlxLexer<'inp, [u8]>, GraphqlxErrors<&'inp [u8]>, GraphQLX>;
/// The fatal context for an owned, refcount-sliced [`bytes::Bytes`] source.
#[cfg(feature = "bytes")]
type BytesCtx<'inp> =
  FatalContext<'inp, GraphqlxLexer<'inp, ::bytes::Bytes>, GraphqlxErrors<::bytes::Bytes>, GraphQLX>;

/// Drives `f` over a `str` source under `Fatal<GraphqlxErrors<&str>>`.
fn drive_str<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlxInput<'inp, 'c, str, StrCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<&'inp str>>,
  input: &'inp str,
) -> Result<O, GraphqlxErrors<&'inp str>> {
  Parser::with_parser_of::<'inp, GraphqlxLexer<'inp, str>, O, GraphqlxErrors<&'inp str>, _, GraphQLX>(
    f,
  )
  .parse_str(input)
}

/// Drives `f` over a `[u8]` source under `Fatal<GraphqlxErrors<&[u8]>>`.
fn drive_slice<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlxInput<'inp, 'c, [u8], SliceCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<&'inp [u8]>>,
  input: &'inp [u8],
) -> Result<O, GraphqlxErrors<&'inp [u8]>> {
  Parser::with_parser_of::<
    'inp,
    GraphqlxLexer<'inp, [u8]>,
    O,
    GraphqlxErrors<&'inp [u8]>,
    _,
    GraphQLX,
  >(f)
  .parse_slice(input)
}

#[cfg(feature = "bytes")]
fn drive_bytes_as_slice<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlxInput<'inp, 'c, [u8], SliceCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<&'inp [u8]>>,
  input: &'inp ::bytes::Bytes,
) -> Result<O, GraphqlxErrors<&'inp [u8]>> {
  Parser::with_parser_of::<
    'inp,
    GraphqlxLexer<'inp, [u8]>,
    O,
    GraphqlxErrors<&'inp [u8]>,
    _,
    GraphQLX,
  >(f)
  .parse_bytes(input)
}

/// Drives `f` over an owned [`bytes::Bytes`] source rather than Tokora's
/// byte-slice convenience adapter.
#[cfg(feature = "bytes")]
fn drive_owned_bytes<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlxInput<'inp, 'c, ::bytes::Bytes, BytesCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<::bytes::Bytes>>,
  input: &'inp ::bytes::Bytes,
) -> Result<O, GraphqlxErrors<::bytes::Bytes>> {
  Parser::with_parser_of::<
    'inp,
    GraphqlxLexer<'inp, ::bytes::Bytes>,
    O,
    GraphqlxErrors<::bytes::Bytes>,
    _,
    GraphQLX,
  >(f)
  .parse(input)
}

/// Runs `parser` over `src` as `str`, `[u8]`, and (behind the feature) `Bytes`.
macro_rules! accept_all {
  ($parser:expr, $src:expr, $check:path) => {{
    $check(drive_str($parser, $src).expect(concat!("str accept: ", $src)));
    $check(drive_slice($parser, $src.as_bytes()).expect(concat!("slice accept: ", $src)));
    #[cfg(feature = "bytes")]
    {
      let owned = ::bytes::Bytes::from_static($src.as_bytes());
      $check(drive_bytes_as_slice($parser, &owned).expect(concat!("bytes accept: ", $src)));
    }
  }};
}

/// Asserts `parser` rejects `src` over both borrowed source forms.
macro_rules! reject_all {
  ($parser:expr, $src:expr) => {{
    assert!(
      drive_str(|inp| $parser(inp).map(|_| ()), $src).is_err(),
      "str should reject: {:?}",
      $src
    );
    assert!(
      drive_slice(|inp| $parser(inp).map(|_| ()), $src.as_bytes()).is_err(),
      "slice should reject: {:?}",
      $src
    );
  }};
}

fn assert_unexpected<'inp>(
  parser: impl for<'c> FnMut(
    &mut GraphqlxInput<'inp, 'c, str, StrCtx<'inp>>,
  ) -> Result<(), GraphqlxErrors<&'inp str>>,
  input: &'inp str,
  expected: Expectation,
  found: Option<SyntacticTokenKind>,
) -> SimpleSpan {
  let error = drive_str(parser, input)
    .expect_err("input should report a typed parser error")
    .into_iter()
    .next()
    .expect("parser error collection should contain one error");
  let span = error.span();
  assert!(matches!(
    error.into_data(),
    ErrorData::UnexpectedToken(unexpected)
      if unexpected.expected() == &expected && unexpected.found().copied() == found
  ));
  span
}

#[test]
fn names_and_paths_are_source_generic() {
  fn check_name<S: AsRef<[u8]>>(name: ast::Name<S>) {
    assert!("true".equivalent(name.source()));
    assert_eq!(name.as_span(), &SimpleSpan::new(0, 4));
  }
  accept_all!(ast::Name::graphqlx, "true", check_name);

  fn check_path<S: AsRef<[u8]>>(path: ast::Path<S>) {
    assert!(path.is_fully_qualified());
    assert_eq!(path.segments().len(), 2);
    assert!("Core".equivalent(path.segments()[0].source()));
    assert!("Item".equivalent(path.segments()[1].source()));
    assert_eq!(path.span(), &SimpleSpan::new(0, 12));
  }
  accept_all!(syntactic::path, "::Core::Item", check_path);
}

#[test]
fn path_try_api_declines_without_consuming_a_non_path_head() {
  let (declined, leftover) = drive_str(
    |inp| {
      let attempt = ast::Path::try_graphqlx(inp)?;
      let leftover = inp.next()?.map(|token| token.data().kind());
      Ok::<_, GraphqlxErrors<&str>>((attempt == ParseAttempt::Decline, leftover))
    },
    "$value",
  )
  .expect("try path should decline instead of reporting a failure");

  assert!(declined);
  assert_eq!(leftover, Some(SyntacticTokenKind::Dollar));
}

#[test]
fn extended_values_cover_set_map_object_and_variables() {
  fn check_set<S: AsRef<[u8]>>(value: ast::InputValue<S>) {
    let set = match value {
      ast::InputValue::Set(set) => set,
      _ => panic!("expected set"),
    };
    assert_eq!(set.values().len(), 3);
    assert!(matches!(set.values()[0], ast::InputValue::Int(_)));
    assert!(matches!(set.values()[1], ast::InputValue::Boolean(_)));
    assert!(matches!(set.values()[2], ast::InputValue::Enum(_)));
    assert_eq!(set.span(), &SimpleSpan::new(0, 28));
  }
  accept_all!(
    ast::InputValue::graphqlx,
    "set { 1 true ::Some::Value }",
    check_set
  );

  fn check_map<S: AsRef<[u8]>>(value: ast::InputValue<S>) {
    let map = match value {
      ast::InputValue::Map(map) => map,
      _ => panic!("expected map"),
    };
    assert_eq!(map.entries().len(), 2);
    assert!(matches!(map.entries()[0].key(), ast::InputValue::Enum(_)));
    assert!(matches!(map.entries()[0].value(), ast::InputValue::Int(_)));
    assert!(matches!(map.entries()[1].value(), ast::InputValue::Set(_)));
  }
  accept_all!(
    syntactic::value::value,
    "map { one => 1 two => set { false } }",
    check_map
  );

  fn check_object<S: AsRef<[u8]>>(value: ast::InputValue<S>) {
    let object = match value {
      ast::InputValue::Object(object) => object,
      _ => panic!("expected object"),
    };
    assert_eq!(object.fields().len(), 1);
    assert!("field".equivalent(object.fields()[0].name().source()));
    assert!(matches!(
      object.fields()[0].value(),
      ast::InputValue::List(_)
    ));
  }
  accept_all!(
    ast::InputValue::graphqlx,
    "{ field: [1 null] }",
    check_object
  );

  fn check_variable<S: AsRef<[u8]>>(value: ast::InputValue<S>) {
    let variable = match value {
      ast::InputValue::Variable(variable) => variable,
      _ => panic!("expected variable"),
    };
    assert!("value".equivalent(variable.name().source()));
  }
  accept_all!(ast::InputValue::graphqlx, "$value", check_variable);
  reject_all!(ast::ConstInputValue::graphqlx, "$value");
}

#[test]
fn graphqlx_radix_literals_and_enum_paths_preserve_their_heads() {
  fn hex_int<S: AsRef<[u8]>>(value: ast::IntValue<S>) {
    assert!(matches!(
      value.source(),
      LitInt::Hex(source) if "0x2A".equivalent(source)
    ));
  }
  accept_all!(ast::IntValue::graphqlx, "0x2A", hex_int);

  fn binary_int<S: AsRef<[u8]>>(value: ast::IntValue<S>) {
    assert!(matches!(
      value.source(),
      LitInt::Binary(source) if "0b1010".equivalent(source)
    ));
  }
  accept_all!(ast::IntValue::graphqlx, "0b1010", binary_int);

  fn octal_int<S: AsRef<[u8]>>(value: ast::IntValue<S>) {
    assert!(matches!(
      value.source(),
      LitInt::Octal(source) if "0o52".equivalent(source)
    ));
  }
  accept_all!(ast::IntValue::graphqlx, "0o52", octal_int);

  fn hex_float<S: AsRef<[u8]>>(value: ast::FloatValue<S>) {
    assert!(matches!(
      value.source(),
      LitFloat::Hex(source) if "0x1.8p3".equivalent(source)
    ));
  }
  accept_all!(ast::FloatValue::graphqlx, "0x1.8p3", hex_float);

  fn check_enum<S: AsRef<[u8]>>(value: ast::InputValue<S>) {
    let value = match value {
      ast::InputValue::Enum(value) => value,
      _ => panic!("expected an enum path"),
    };
    assert!(!value.segments().is_empty());
  }
  accept_all!(ast::InputValue::graphqlx, "set", check_enum);
  accept_all!(ast::InputValue::graphqlx, "map", check_enum);
  accept_all!(ast::InputValue::graphqlx, "set::Member", check_enum);
}

#[test]
fn public_value_composition_parsers_and_direct_apis_are_source_generic() {
  fn check_set<S: AsRef<[u8]>>(value: ast::Set<S>) {
    assert_eq!(value.values().len(), 1);
  }
  accept_all!(syntactic::value::set_value, "set { 1 }", check_set);

  fn check_map<S: AsRef<[u8]>>(value: ast::Map<S>) {
    assert_eq!(value.entries().len(), 1);
  }
  accept_all!(syntactic::value::map_value, "map { key => 1 }", check_map);

  fn check_entry<S: AsRef<[u8]>>(value: ast::MapEntry<S>) {
    assert!(matches!(value.key(), ast::InputValue::Enum(_)));
    assert!(matches!(value.value(), ast::InputValue::Int(_)));
  }
  accept_all!(syntactic::value::map_entry, "key => 1", check_entry);

  fn check_enum<S: AsRef<[u8]>>(value: ast::EnumValue<S>) {
    assert_eq!(value.segments().len(), 2);
  }
  accept_all!(syntactic::value::enum_value, "Thing::Member", check_enum);

  let int: ast::IntValue<&str> =
    drive_str(ast::IntValue::graphqlx, "0x2A").expect("int API witness");
  assert!(matches!(int.source(), LitInt::Hex(_)));
  let _: ast::Type<&str> = drive_str(ast::Type::graphqlx, "Item").expect("type API witness");
  let _: ast::ImportDefinition<&str> =
    drive_str(ast::ImportDefinition::graphqlx, "import * from \"module\"")
      .expect("import API witness");
}

#[cfg(feature = "bytes")]
#[test]
fn owned_bytes_source_keeps_refcount_sliced_ast_payloads() {
  let backing = ::bytes::Bytes::from_static(b"prefix 0x2A suffix");
  let source = backing.slice(7..11);
  let int: ast::IntValue<::bytes::Bytes> =
    drive_owned_bytes(ast::IntValue::graphqlx, &source).expect("owned Bytes int API witness");

  match int.source() {
    LitInt::Hex(payload) => {
      assert_eq!(payload, &source);
      assert_eq!(payload.as_ptr(), source.as_ptr());
    }
    _ => panic!("expected a hexadecimal Bytes payload"),
  }
}

#[test]
fn direct_value_and_collection_errors_report_local_expectations() {
  assert_unexpected(
    |inp| ast::IntValue::graphqlx(inp).map(|_| ()),
    "true",
    Expectation::IntValue,
    Some(SyntacticTokenKind::Identifier),
  );
  assert_unexpected(
    |inp| ast::IntValue::graphqlx(inp).map(|_| ()),
    "",
    Expectation::IntValue,
    None,
  );
  assert_unexpected(
    |inp| ast::FloatValue::graphqlx(inp).map(|_| ()),
    "1",
    Expectation::FloatValue,
    Some(SyntacticTokenKind::Int),
  );
  assert_unexpected(
    |inp| ast::StringValue::graphqlx(inp).map(|_| ()),
    "1",
    Expectation::StringValue,
    Some(SyntacticTokenKind::Int),
  );
  assert_unexpected(
    |inp| ast::BooleanValue::graphqlx(inp).map(|_| ()),
    "null",
    Expectation::BooleanValue,
    Some(SyntacticTokenKind::Identifier),
  );
  assert_unexpected(
    |inp| ast::NullValue::graphqlx(inp).map(|_| ()),
    "true",
    Expectation::NullValue,
    Some(SyntacticTokenKind::Identifier),
  );
  assert_unexpected(
    |inp| ast::EnumValue::graphqlx(inp).map(|_| ()),
    "true",
    Expectation::EnumValue,
    Some(SyntacticTokenKind::Identifier),
  );
  assert_eq!(
    assert_unexpected(
      |inp| ast::VariableValue::graphqlx(inp).map(|_| ()),
      "$",
      Expectation::VariableValue,
      None,
    ),
    SimpleSpan::new(1, 1)
  );

  assert_unexpected(
    |inp| ast::InputValue::graphqlx(inp).map(|_| ()),
    "{ key value }",
    Expectation::Colon,
    Some(SyntacticTokenKind::Identifier),
  );
  assert_unexpected(
    |inp| ast::InputValue::graphqlx(inp).map(|_| ()),
    "{ key: }",
    Expectation::InputValue,
    Some(SyntacticTokenKind::RBrace),
  );
  assert_unexpected(
    |inp| ast::InputValue::graphqlx(inp).map(|_| ()),
    "map { key value }",
    Expectation::FatArrow,
    Some(SyntacticTokenKind::Identifier),
  );
  assert_unexpected(
    |inp| ast::InputValue::graphqlx(inp).map(|_| ()),
    "map { key => }",
    Expectation::InputValue,
    Some(SyntacticTokenKind::RBrace),
  );
  assert_unexpected(
    |inp| ast::InputValue::graphqlx(inp).map(|_| ()),
    "set { => }",
    Expectation::InputValue,
    Some(SyntacticTokenKind::FatArrow),
  );

  assert_unexpected(
    |inp| ast::Set::graphqlx(inp).map(|_| ()),
    "set value",
    Expectation::LBrace,
    Some(SyntacticTokenKind::Identifier),
  );
  assert_unexpected(
    |inp| ast::Set::graphqlx(inp).map(|_| ()),
    "set",
    Expectation::LBrace,
    None,
  );
  assert_unexpected(
    |inp| ast::ConstSet::graphqlx(inp).map(|_| ()),
    "set value",
    Expectation::LBrace,
    Some(SyntacticTokenKind::Identifier),
  );
  assert_unexpected(
    |inp| ast::ConstSet::graphqlx(inp).map(|_| ()),
    "set",
    Expectation::LBrace,
    None,
  );
  assert_unexpected(
    |inp| ast::Map::graphqlx(inp).map(|_| ()),
    "map value",
    Expectation::LBrace,
    Some(SyntacticTokenKind::Identifier),
  );
  assert_unexpected(
    |inp| ast::Map::graphqlx(inp).map(|_| ()),
    "map",
    Expectation::LBrace,
    None,
  );
  assert_unexpected(
    |inp| ast::ConstMap::graphqlx(inp).map(|_| ()),
    "map value",
    Expectation::LBrace,
    Some(SyntacticTokenKind::Identifier),
  );
  assert_unexpected(
    |inp| ast::ConstMap::graphqlx(inp).map(|_| ()),
    "map",
    Expectation::LBrace,
    None,
  );
}

#[test]
fn graphqlx_types_cover_paths_generics_sets_and_maps() {
  fn check_path_type<S: AsRef<[u8]>>(ty: ast::Type<S>) {
    let path = match ty {
      ast::Type::Path(path) => path,
      _ => panic!("expected path type"),
    };
    assert!(path.required());
    assert!(path.path().is_fully_qualified());
    assert_eq!(path.path().segments().len(), 2);
    let generics = path.type_generics().expect("generic arguments");
    assert_eq!(generics.params().len(), 2);
    assert!(matches!(generics.params()[0], ast::Type::Path(_)));
    assert!(matches!(generics.params()[1], ast::Type::List(_)));
  }
  accept_all!(
    ast::Type::graphqlx,
    "::Core::Result<String [Value!]>!",
    check_path_type
  );

  fn check_set_type<S: AsRef<[u8]>>(ty: ast::Type<S>) {
    let set = match ty {
      ast::Type::Set(set) => set,
      _ => panic!("expected set type"),
    };
    assert!(set.required());
    assert!(matches!(set.ty(), ast::Type::Path(_)));
  }
  accept_all!(syntactic::ty::ty, "<Item>!", check_set_type);

  fn check_map_type<S: AsRef<[u8]>>(ty: ast::Type<S>) {
    let map = match ty {
      ast::Type::Map(map) => map,
      _ => panic!("expected map type"),
    };
    assert!(matches!(map.key(), ast::Type::Path(_)));
    assert!(matches!(map.value(), ast::Type::List(_)));
    assert!(!map.required());
  }
  accept_all!(ast::Type::graphqlx, "<Key => [Value]>", check_map_type);

  fn check_nested<S: AsRef<[u8]>>(ty: ast::Type<S>) {
    let outer = match ty {
      ast::Type::Path(path) => path,
      _ => panic!("expected an outer path type"),
    };
    assert_eq!(
      outer
        .type_generics()
        .expect("generic arguments")
        .params()
        .len(),
      2
    );
  }
  accept_all!(
    ast::Type::graphqlx,
    "Outer<Inner<[Item]> <Key => <Value>>>",
    check_nested
  );

  reject_all!(ast::Type::graphqlx, "Foo<>");
  reject_all!(ast::Type::graphqlx, "Foo<");
  reject_all!(ast::Type::graphqlx, "<Key => >");
}

#[test]
fn imports_cover_named_wildcard_aliases_and_module_strings() {
  fn check_list_import<S: AsRef<[u8]>>(definition: ast::ImportDefinition<S>) {
    let list = match definition.clause() {
      ast::ImportClause::List(list) => list,
      _ => panic!("expected import list"),
    };
    assert_eq!(list.members().len(), 2);
    match &list.members()[0] {
      ast::ImportMember::Named(named) => {
        assert!("Thing".equivalent(named.name().source()));
        assert_eq!(named.alias().expect("alias").segments().len(), 2);
      }
      _ => panic!("expected named import"),
    }
    match &list.members()[1] {
      ast::ImportMember::Wildcard(wildcard) => {
        assert_eq!(wildcard.alias().expect("alias").segments().len(), 1);
      }
      _ => panic!("expected wildcard import"),
    }
    assert!(
      b"\"module\""
        .as_slice()
        .equivalent(definition.file().source())
    );
  }
  accept_all!(
    ast::ImportDefinition::graphqlx,
    "import { Thing as Local::Thing * as Everything } from \"module\"",
    check_list_import
  );

  fn check_wildcard_import<S: AsRef<[u8]>>(definition: ast::ImportDefinition<S>) {
    let wildcard = match definition.clause() {
      ast::ImportClause::Wildcard(wildcard) => wildcard,
      _ => panic!("expected wildcard import"),
    };
    assert!(wildcard.alias().is_none());
  }
  accept_all!(
    syntactic::import::import_definition,
    "import * from \"module\"",
    check_wildcard_import
  );
}

#[test]
fn failures_keep_typed_expectations_and_unclosed_kinds() {
  let error = drive_str(|inp| ast::Type::graphqlx(inp).map(|_| ()), "42")
    .expect_err("an integer cannot begin a GraphQLX type")
    .into_iter()
    .next()
    .expect("invalid type should emit an error");
  assert!(matches!(
    error.into_data(),
    ErrorData::UnexpectedToken(unexpected)
      if unexpected.expected() == &Expectation::Type
        && unexpected.found() == Some(&SyntacticTokenKind::Int)
  ));

  let list_error = drive_str(|inp| ast::InputValue::graphqlx(inp).map(|_| ()), "[1")
    .expect_err("unterminated list should fail")
    .into_iter()
    .next()
    .expect("unterminated list should emit an error");
  assert!(matches!(
    list_error.into_data(),
    ErrorData::Unclosed(Unclosed::List)
  ));

  let angle_error = drive_str(|inp| ast::Type::graphqlx(inp).map(|_| ()), "<Item")
    .expect_err("unterminated angle type should fail")
    .into_iter()
    .next()
    .expect("unterminated angle type should emit an error");
  assert!(matches!(
    angle_error.into_data(),
    ErrorData::Unclosed(Unclosed::Angle)
  ));

  reject_all!(
    ast::ImportDefinition::graphqlx,
    "import Thing from \"module\""
  );
  reject_all!(ast::ImportDefinition::graphqlx, "import {} from \"module\"");
  reject_all!(
    ast::ImportDefinition::graphqlx,
    "import { Thing as } from \"module\""
  );
  reject_all!(ast::ImportDefinition::graphqlx, "import * \"module\"");
  reject_all!(ast::ImportDefinition::graphqlx, "import * from 1");
  reject_all!(
    ast::ImportDefinition::graphqlx,
    "import * from \"\"\"module\"\"\""
  );

  let import_brace_error = drive_str(
    |inp| ast::ImportDefinition::graphqlx(inp).map(|_| ()),
    "import { Thing",
  )
  .expect_err("unterminated import list should fail")
  .into_iter()
  .next()
  .expect("unterminated import list should emit an error");
  assert!(matches!(
    import_brace_error.into_data(),
    ErrorData::Unclosed(Unclosed::Object)
  ));
}
