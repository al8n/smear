//! End-to-end GraphQLx production tests.
//!
//! The parser API is deliberately source-generic.  Each successful case below
//! is exercised through the concrete lexer over `str`, `[u8]`, and `Bytes`
//! (when enabled), so the AST aliases and parser entry points cannot
//! accidentally become tied to one source representation.

use smear_lexer::graphqlx::{LitFloat, LitInt, syntactic::SyntacticTokenKind};
use tokora::{
  FatalContext, Parse, ParseAttempt, Parser, SimpleSpan, span::AsSpan, utils::Equivalent,
};

use super::{
  GraphQLx, ast,
  error::{ErrorData, Expectation, GraphqlxErrors, Unclosed},
  syntactic::{self, GraphqlxInput, GraphqlxLexer},
};

/// The fatal context a `str`-sourced parse runs under.
type StrCtx<'inp> =
  FatalContext<'inp, GraphqlxLexer<'inp, str>, GraphqlxErrors<&'inp str>, GraphQLx>;
/// The fatal context a `[u8]`-sourced parse runs under.
type SliceCtx<'inp> =
  FatalContext<'inp, GraphqlxLexer<'inp, [u8]>, GraphqlxErrors<&'inp [u8]>, GraphQLx>;
/// The fatal context for an owned, refcount-sliced [`bytes_1::Bytes`] source.
#[cfg(feature = "bytes_1")]
type BytesCtx<'inp> = FatalContext<
  'inp,
  GraphqlxLexer<'inp, ::bytes_1::Bytes>,
  GraphqlxErrors<::bytes_1::Bytes>,
  GraphQLx,
>;

/// Drives `f` over a `str` source under `Fatal<GraphqlxErrors<&str>>`.
fn drive_str<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlxInput<'inp, 'c, str, StrCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<&'inp str>>,
  input: &'inp str,
) -> Result<O, GraphqlxErrors<&'inp str>> {
  Parser::with_parser::<'inp, GraphqlxLexer<'inp, str>, O, GraphqlxErrors<&'inp str>, _, GraphQLx>(
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
  Parser::with_parser::<
    'inp,
    GraphqlxLexer<'inp, [u8]>,
    O,
    GraphqlxErrors<&'inp [u8]>,
    _,
    GraphQLx,
  >(f)
  .parse_slice(input)
}

#[cfg(feature = "bytes_1")]
fn drive_bytes_as_slice<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlxInput<'inp, 'c, [u8], SliceCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<&'inp [u8]>>,
  input: &'inp ::bytes_1::Bytes,
) -> Result<O, GraphqlxErrors<&'inp [u8]>> {
  Parser::with_parser::<
    'inp,
    GraphqlxLexer<'inp, [u8]>,
    O,
    GraphqlxErrors<&'inp [u8]>,
    _,
    GraphQLx,
  >(f)
  .parse_bytes(input)
}

/// Drives `f` over an owned [`bytes_1::Bytes`] source rather than Tokora's
/// byte-slice convenience adapter.
#[cfg(feature = "bytes_1")]
fn drive_owned_bytes<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlxInput<'inp, 'c, ::bytes_1::Bytes, BytesCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<::bytes_1::Bytes>>,
  input: &'inp ::bytes_1::Bytes,
) -> Result<O, GraphqlxErrors<::bytes_1::Bytes>> {
  Parser::with_parser::<
    'inp,
    GraphqlxLexer<'inp, ::bytes_1::Bytes>,
    O,
    GraphqlxErrors<::bytes_1::Bytes>,
    _,
    GraphQLx,
  >(f)
  .parse(input)
}

/// Runs `parser` over `src` as `str`, `[u8]`, and (behind the feature) `Bytes`.
macro_rules! accept_all {
  ($parser:expr, $src:expr, $check:path) => {{
    $check(drive_str($parser, $src).expect(concat!("str accept: ", $src)));
    $check(drive_slice($parser, $src.as_bytes()).expect(concat!("slice accept: ", $src)));
    #[cfg(feature = "bytes_1")]
    {
      let owned = ::bytes_1::Bytes::from_static($src.as_bytes());
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
    assert_eq!(
      path.root(),
      &ast::PathRoot::Dependency(SimpleSpan::new(0, 3))
    );
    assert_eq!(path.segments().len(), 2);
    assert!("Core".equivalent(path.segments()[0].source()));
    assert!("Item".equivalent(path.segments()[1].source()));
    assert_eq!(path.span(), &SimpleSpan::new(0, 13));
  }
  accept_all!(syntactic::path, "^::Core::Item", check_path);
}

#[test]
fn every_path_root_is_read_and_a_leading_separator_is_refused() {
  use core::num::NonZeroUsize;
  use std::string::ToString;
  use tokora::diagnostic::Diagnose;

  let twice = NonZeroUsize::new(2).expect("two is not zero");
  for (src, root, end) in [
    ("Core::Item", ast::PathRoot::Relative, 10),
    (
      "~::Core::Item",
      ast::PathRoot::Package(SimpleSpan::new(0, 3)),
      13,
    ),
    (
      "^ :: Core",
      ast::PathRoot::Dependency(SimpleSpan::new(0, 4)),
      9,
    ),
    (
      "..:: .. ::Core",
      ast::PathRoot::Parent {
        span: SimpleSpan::new(0, 10),
        count: twice,
      },
      14,
    ),
  ] {
    let path = drive_str(syntactic::path, src).expect(src);
    assert_eq!(path.root(), &root, "{src:?}");
    assert_eq!(
      path.span(),
      &SimpleSpan::new(0, end),
      "{src:?}: a path's span starts at its root"
    );
  }

  // `::` only joins: where a path begins it roots nothing, and the refusal says how a dependency
  // path is written.
  let error = drive_str(syntactic::path, "::Core::Item")
    .expect_err("a leading `::` must be refused")
    .into_iter()
    .next()
    .expect("one error");
  assert!(
    matches!(error.data(), ErrorData::LeadingPathSeparator),
    "{:?}",
    error.data()
  );
  assert_eq!(error.span(), SimpleSpan::new(0, 2));
  assert_eq!(error.to_string(), "a path cannot start with `::`");
  assert_eq!(error.help(), Some("a dependency path starts with `^::`"));

  // A root's sigil without its `::` is refused at the token in its place.
  for src in ["~Core", "^Core", "..Core", ".. .. ::Core"] {
    assert!(drive_str(syntactic::path, src).is_err(), "{src:?}");
  }
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
    assert_eq!(set.span(), &SimpleSpan::new(0, 29));
  }
  accept_all!(
    ast::InputValue::graphqlx,
    "set!{ 1 true ^::Some::Value }",
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
    "map!{ one => 1 two => set!{ false } }",
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
  accept_all!(syntactic::value::set_value, "set!{ 1 }", check_set);

  fn check_map<S: AsRef<[u8]>>(value: ast::Map<S>) {
    assert_eq!(value.entries().len(), 1);
  }
  accept_all!(syntactic::value::map_value, "map!{ key => 1 }", check_map);

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
  let _: ast::UseDefinition<&str> =
    drive_str(ast::UseDefinition::graphqlx, "use ~::module::*").expect("use API witness");
}

#[cfg(feature = "bytes_1")]
#[test]
fn owned_bytes_source_keeps_refcount_sliced_ast_payloads() {
  let backing = ::bytes_1::Bytes::from_static(b"prefix 0x2A suffix");
  let source = backing.slice(7..11);
  let int: ast::IntValue<::bytes_1::Bytes> =
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
    "map!{ key value }",
    Expectation::FatArrow,
    Some(SyntacticTokenKind::Identifier),
  );
  assert_unexpected(
    |inp| ast::InputValue::graphqlx(inp).map(|_| ()),
    "map!{ key => }",
    Expectation::InputValue,
    Some(SyntacticTokenKind::RBrace),
  );
  assert_unexpected(
    |inp| ast::InputValue::graphqlx(inp).map(|_| ()),
    "set!{ => }",
    Expectation::InputValue,
    Some(SyntacticTokenKind::FatArrow),
  );

  assert_unexpected(
    |inp| ast::Set::graphqlx(inp).map(|_| ()),
    "set value",
    Expectation::Bang,
    Some(SyntacticTokenKind::Identifier),
  );
  assert_unexpected(
    |inp| ast::Set::graphqlx(inp).map(|_| ()),
    "set",
    Expectation::Bang,
    None,
  );
  assert_unexpected(
    |inp| ast::Set::graphqlx(inp).map(|_| ()),
    "set! value",
    Expectation::LBrace,
    Some(SyntacticTokenKind::Identifier),
  );
  assert_unexpected(
    |inp| ast::Set::graphqlx(inp).map(|_| ()),
    "set!",
    Expectation::LBrace,
    None,
  );
  assert_unexpected(
    |inp| ast::ConstSet::graphqlx(inp).map(|_| ()),
    "set value",
    Expectation::Bang,
    Some(SyntacticTokenKind::Identifier),
  );
  assert_unexpected(
    |inp| ast::ConstSet::graphqlx(inp).map(|_| ()),
    "set",
    Expectation::Bang,
    None,
  );
  assert_unexpected(
    |inp| ast::ConstSet::graphqlx(inp).map(|_| ()),
    "set! value",
    Expectation::LBrace,
    Some(SyntacticTokenKind::Identifier),
  );
  assert_unexpected(
    |inp| ast::ConstSet::graphqlx(inp).map(|_| ()),
    "set!",
    Expectation::LBrace,
    None,
  );
  assert_unexpected(
    |inp| ast::Map::graphqlx(inp).map(|_| ()),
    "map value",
    Expectation::Bang,
    Some(SyntacticTokenKind::Identifier),
  );
  assert_unexpected(
    |inp| ast::Map::graphqlx(inp).map(|_| ()),
    "map",
    Expectation::Bang,
    None,
  );
  assert_unexpected(
    |inp| ast::Map::graphqlx(inp).map(|_| ()),
    "map! value",
    Expectation::LBrace,
    Some(SyntacticTokenKind::Identifier),
  );
  assert_unexpected(
    |inp| ast::Map::graphqlx(inp).map(|_| ()),
    "map!",
    Expectation::LBrace,
    None,
  );
  assert_unexpected(
    |inp| ast::ConstMap::graphqlx(inp).map(|_| ()),
    "map value",
    Expectation::Bang,
    Some(SyntacticTokenKind::Identifier),
  );
  assert_unexpected(
    |inp| ast::ConstMap::graphqlx(inp).map(|_| ()),
    "map",
    Expectation::Bang,
    None,
  );
  assert_unexpected(
    |inp| ast::ConstMap::graphqlx(inp).map(|_| ()),
    "map! value",
    Expectation::LBrace,
    Some(SyntacticTokenKind::Identifier),
  );
  assert_unexpected(
    |inp| ast::ConstMap::graphqlx(inp).map(|_| ()),
    "map!",
    Expectation::LBrace,
    None,
  );
}

#[test]
fn graphqlx_types_cover_paths_and_generics() {
  fn check_path_type<S: AsRef<[u8]>>(ty: ast::Type<S>) {
    let path = match ty {
      ast::Type::Path(path) => path,
      _ => panic!("expected path type"),
    };
    assert!(path.required());
    assert!(matches!(path.path().root(), ast::PathRoot::Dependency(_)));
    assert_eq!(path.path().segments().len(), 2);
    let generics = path.type_generics().expect("generic arguments");
    assert_eq!(generics.params().len(), 2);
    assert!(matches!(generics.params()[0], ast::Type::Path(_)));
    assert!(matches!(generics.params()[1], ast::Type::List(_)));
  }
  accept_all!(
    ast::Type::graphqlx,
    "^::Core::Result<String [Value!]>!",
    check_path_type
  );

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
    "Outer<Inner<[Item]> Map<Key, Set<Value>>>",
    check_nested
  );

  reject_all!(ast::Type::graphqlx, "Foo<>");
  reject_all!(ast::Type::graphqlx, "Foo<");
  // A `<` only ever follows a path, as its arguments: no type starts with one.
  reject_all!(ast::Type::graphqlx, "<Item>");
  reject_all!(ast::Type::graphqlx, "<Key => Value>");
}

#[test]
fn use_trees_cover_roots_paths_sources_globs_groups_and_aliases() {
  fn check_group<S: AsRef<[u8]>>(definition: ast::UseDefinition<S>) {
    let tree = definition.tree();
    let ast::UsePrefix::Path(prefix) = tree.prefix() else {
      panic!("expected a path prefix");
    };
    assert!(matches!(prefix.root(), ast::PathRoot::Dependency(_)));
    assert_eq!(prefix.segments().len(), 1);
    let ast::UseTreeKind::Group(trees) = tree.kind() else {
      panic!("expected a group");
    };
    assert_eq!(trees.len(), 3);
    let ast::UsePrefix::Path(first) = trees[0].prefix() else {
      panic!("a group member's prefix is a path");
    };
    assert!(first.root().is_relative());
    assert!("Thing".equivalent(first.segments()[0].source()));
    let ast::UseTreeKind::Simple { alias: Some(alias) } = trees[0].kind() else {
      panic!("expected an alias");
    };
    assert!("Local".equivalent(alias.source()));
    assert!(trees[1].kind().is_glob());
    // A member named `as` is read as a name where a tree starts.
    let ast::UsePrefix::Path(named_as) = trees[2].prefix() else {
      panic!("a group member's prefix is a path");
    };
    assert!("as".equivalent(named_as.segments()[0].source()));
    assert!(matches!(
      trees[2].kind(),
      ast::UseTreeKind::Simple { alias: None }
    ));
  }
  accept_all!(
    ast::UseDefinition::graphqlx,
    "use ^::module::{Thing as Local, nested::* as}",
    check_group
  );

  fn check_source<S: AsRef<[u8]>>(definition: ast::UseDefinition<S>) {
    let ast::UsePrefix::Source { source, segments } = definition.tree().prefix() else {
      panic!("expected a string source");
    };
    assert!(b"\"./media\"".as_slice().equivalent(source.source()));
    assert_eq!(segments.len(), 2);
    let ast::UseTreeKind::Simple { alias: Some(alias) } = definition.tree().kind() else {
      panic!("expected an alias");
    };
    assert!("Img".equivalent(alias.source()));
  }
  accept_all!(
    ast::UseDefinition::graphqlx,
    "use \"./media\"::vision::Image as Img",
    check_source
  );

  fn check_root_glob<S: AsRef<[u8]>>(definition: ast::UseDefinition<S>) {
    assert!(matches!(
      definition.tree().prefix(),
      ast::UsePrefix::Root(ast::PathRoot::Package(_))
    ));
    assert!(definition.tree().kind().is_glob());
    assert_eq!(definition.span(), &SimpleSpan::new(0, 8));
  }
  accept_all!(ast::UseDefinition::graphqlx, "use ~::*", check_root_glob);

  fn check_parent_group<S: AsRef<[u8]>>(tree: ast::UseTree<S>) {
    let ast::UsePrefix::Root(ast::PathRoot::Parent { count, .. }) = tree.prefix() else {
      panic!("expected a parent root");
    };
    assert_eq!(count.get(), 2);
    let ast::UseTreeKind::Group(trees) = tree.kind() else {
      panic!("expected a group");
    };
    assert_eq!(trees.len(), 1);
  }
  accept_all!(
    syntactic::use_tree::use_tree,
    "..::..::{A}",
    check_parent_group
  );
}

#[test]
fn inline_modules_hold_type_system_items_and_their_own_description() {
  fn check_module<S: AsRef<[u8]>>(document: ast::TypeSystemDocument<S>) {
    let [ast::TypeSystemItem::Mod(module)] = document.definitions() else {
      panic!("expected one module");
    };
    // The description is the module's own, and the span starts at it.
    assert!(module.description().is_some());
    assert_eq!(module.span(), &SimpleSpan::new(0, 76));
    assert!("media".equivalent(module.name().source()));
    let items = module.items();
    assert_eq!(items.len(), 4);
    assert!(items[0].is_use());
    let ast::TypeSystemItem::Mod(nested) = &items[1] else {
      panic!("expected a nested module");
    };
    // An empty module is a namespace.
    assert!(nested.items().is_empty());
    assert!(nested.description().is_none());
    assert_eq!(nested.span(), &SimpleSpan::new(27, 39));
    assert!(items[2].is_definition());
    assert!(items[3].is_extension());
  }
  accept_all!(
    ast::TypeSystemDocument::graphqlx,
    "\"doc\" mod media { use a::b mod empty {} type T { f: Int } extend type T @d }",
    check_module
  );

  // A mixed document holds a module beside a type and a shorthand query.
  fn check_mixed<S: AsRef<[u8]>>(document: ast::Document<S>) {
    let definitions = document.definitions();
    assert_eq!(definitions.len(), 3);
    let ast::DocumentItem::Mod(module) = &definitions[0] else {
      panic!("expected a module first");
    };
    assert_eq!(module.span(), &SimpleSpan::new(0, 33));
    assert!(definitions[1].is_definition());
    assert!(definitions[2].is_definition());
  }
  accept_all!(
    ast::Document::graphqlx,
    "mod geo { type Point { x: Int } }\ntype Query { origin: geo::Point }\n{ origin { x } }",
    check_mixed
  );

  fn check_standalone<S: AsRef<[u8]>>(module: ast::ModDefinition<S>) {
    assert!("x".equivalent(module.name().source()));
    assert!(module.items().is_empty());
  }
  accept_all!(ast::ModDefinition::graphqlx, "mod x {}", check_standalone);
}

#[test]
fn a_module_is_refused_where_no_module_stands_and_without_its_parts() {
  // The executable root does not recognise `mod`: it is an unexpected head there.
  assert_unexpected(
    |inp| ast::ExecutableDocument::graphqlx(inp).map(|_| ()),
    "mod m {}",
    Expectation::Production("a use item or an executable definition"),
    Some(SyntacticTokenKind::Identifier),
  );
  // A module's body holds type-system items only.
  for src in ["mod m { query Q { f } }", "mod m { fragment F on T { f } }"] {
    assert_unexpected(
      |inp| ast::Document::graphqlx(inp).map(|_| ()),
      src,
      Expectation::Production("a use item, a module, a type-system definition or an extension"),
      Some(SyntacticTokenKind::Identifier),
    );
  }
  assert_unexpected(
    |inp| ast::Document::graphqlx(inp).map(|_| ()),
    "mod {}",
    Expectation::Name,
    Some(SyntacticTokenKind::LBrace),
  );
  // The `{` is confirmed before the body is entered, so its absence is the brace delimiter's own
  // report, at the token standing where the `{` belongs.
  let at = assert_unexpected(
    |inp| ast::Document::graphqlx(inp).map(|_| ()),
    "mod m type T { f: Int }",
    Expectation::LBrace,
    Some(SyntacticTokenKind::Identifier),
  );
  assert_eq!(at, SimpleSpan::new(6, 10));
  assert_unexpected(
    |inp| ast::Document::graphqlx(inp).map(|_| ()),
    "mod m",
    Expectation::LBrace,
    None,
  );
  let unclosed = drive_str(
    |inp| ast::Document::graphqlx(inp).map(|_| ()),
    "mod m { type T { f: Int }",
  )
  .expect_err("an unclosed body should fail")
  .into_iter()
  .next()
  .expect("an unclosed body should emit an error");
  assert!(matches!(
    unclosed.into_data(),
    ErrorData::Unclosed(Unclosed::Object)
  ));
  // A description before `mod` is the module's own at the mixed root too.
  fn check_described<S: AsRef<[u8]>>(document: ast::Document<S>) {
    let [ast::DocumentItem::Mod(module)] = document.definitions() else {
      panic!("expected one module");
    };
    assert!(module.description().is_some());
    assert_eq!(module.span(), &SimpleSpan::new(0, 14));
  }
  accept_all!(ast::Document::graphqlx, "\"doc\" mod m {}", check_described);
}

#[test]
fn scalars_enums_unions_and_input_objects_implement_interfaces() {
  // `implements` follows the name and generics and precedes the directives, on all four kinds.
  fn check_scalar<S: AsRef<[u8]>>(scalar: ast::ScalarTypeDefinition<S>) {
    let implements = scalar.implements().expect("the scalar's clause");
    assert_eq!(implements.interfaces().len(), 2);
    assert_eq!(implements.span(), &SimpleSpan::new(9, 29));
    assert!(scalar.directives().is_some());
    assert_eq!(scalar.span(), &SimpleSpan::new(0, 32));
  }
  accept_all!(
    ast::ScalarTypeDefinition::graphqlx,
    "scalar S implements I & ns::J @d",
    check_scalar
  );

  fn check_enum<S: AsRef<[u8]>>(definition: ast::EnumTypeDefinition<S>) {
    assert!(definition.implements().is_some());
    assert!(definition.directives().is_none());
    assert!(definition.enum_values_definition().is_some());
  }
  accept_all!(
    ast::EnumTypeDefinition::graphqlx,
    "enum E<T> implements I { A }",
    check_enum
  );

  fn check_union<S: AsRef<[u8]>>(definition: ast::UnionTypeDefinition<S>) {
    assert!(definition.implements().is_some());
    assert!(definition.member_types().is_some());
  }
  accept_all!(
    ast::UnionTypeDefinition::graphqlx,
    "union U implements I = A | B",
    check_union
  );

  fn check_input<S: AsRef<[u8]>>(definition: ast::InputObjectTypeDefinition<S>) {
    assert!(definition.implements().is_some());
    let fields = definition.fields_definition().expect("the input's fields");
    assert!(fields.where_clause().is_some());
  }
  accept_all!(
    ast::InputObjectTypeDefinition::graphqlx,
    "input In<T> implements I where T: B { a: Int }",
    check_input
  );

  // An extension adds at least one thing, and an `implements` clause counts.
  fn check_bare_scalar_extension<S: AsRef<[u8]>>(extension: ast::ScalarTypeExtension<S>) {
    assert!(extension.data().is_implements());
    assert!(extension.directives().is_none());
    assert_eq!(extension.span(), &SimpleSpan::new(0, 28));
  }
  accept_all!(
    ast::ScalarTypeExtension::graphqlx,
    "extend scalar S implements I",
    check_bare_scalar_extension
  );

  fn check_scalar_extension<S: AsRef<[u8]>>(extension: ast::ScalarTypeExtension<S>) {
    assert!(extension.data().is_directives());
    assert!(extension.implements().is_some());
    assert_eq!(extension.span(), &SimpleSpan::new(0, 31));
  }
  accept_all!(
    ast::ScalarTypeExtension::graphqlx,
    "extend scalar S implements I @d",
    check_scalar_extension
  );

  fn check_union_extension<S: AsRef<[u8]>>(extension: ast::UnionTypeExtension<S>) {
    assert!(extension.data().is_implements());
    assert!(extension.member_types().is_none());
    assert_eq!(extension.span(), &SimpleSpan::new(0, 27));
  }
  accept_all!(
    ast::UnionTypeExtension::graphqlx,
    "extend union U implements I",
    check_union_extension
  );

  fn check_enum_extension<S: AsRef<[u8]>>(extension: ast::EnumTypeExtension<S>) {
    assert!(extension.data().is_values());
    assert!(extension.implements().is_some());
    assert!(extension.directives().is_some());
  }
  accept_all!(
    ast::EnumTypeExtension::graphqlx,
    "extend enum E implements I @d { A }",
    check_enum_extension
  );

  fn check_input_extension<S: AsRef<[u8]>>(extension: ast::InputObjectTypeExtension<S>) {
    assert!(extension.data().is_fields());
    assert!(extension.implements().is_some());
    assert!(extension.directives().is_none());
  }
  accept_all!(
    ast::InputObjectTypeExtension::graphqlx,
    "extend input In implements I { a: Int }",
    check_input_extension
  );

  // After the directives, `implements` is no longer the definition's: the scalar ends there and
  // the clause is an entry head of nothing.
  assert_unexpected(
    |inp| ast::Document::graphqlx(inp).map(|_| ()),
    "scalar S @d implements I",
    Expectation::Production("a use item, a module, a definition or an extension"),
    Some(SyntacticTokenKind::Identifier),
  );
}

#[test]
fn a_scalar_ends_on_its_where_clause_and_the_next_entry_is_its_own() {
  // The clause has no terminator and forces nothing after the scalar, so a `use` item rooted at a
  // dependency must start a new entry rather than continue the clause.
  fn check_regression<S: AsRef<[u8]>>(document: ast::TypeSystemDocument<S>) {
    let definitions = document.definitions();
    assert_eq!(definitions.len(), 2);
    let ast::TypeSystemItem::Definition(described) = &definitions[0] else {
      panic!("expected the scalar first");
    };
    let ast::TypeSystemDefinition::Type(ast::TypeDefinition::Scalar(scalar)) = described.node()
    else {
      panic!("expected a scalar type definition");
    };
    let clause = scalar.where_clause().expect("the scalar's clause");
    assert_eq!(clause.predicates().len(), 1);
    assert_eq!(scalar.span(), &SimpleSpan::new(0, 25));
    assert!(definitions[1].is_use());
  }
  accept_all!(
    ast::TypeSystemDocument::graphqlx,
    "scalar S<A> where A: Node\nuse ^::dep::X",
    check_regression
  );

  // The control: a second predicate whose bounded parameter is named `use` continues the clause,
  // because a name followed by `:` opens a predicate whatever it spells.
  fn check_control<S: AsRef<[u8]>>(scalar: ast::ScalarTypeDefinition<S>) {
    let clause = scalar.where_clause().expect("the scalar's clause");
    assert_eq!(clause.predicates().len(), 2);
    assert!(scalar.implements().is_none());
  }
  accept_all!(
    ast::ScalarTypeDefinition::graphqlx,
    "scalar S<A, use> where A: Node use: Node",
    check_control
  );

  // All three clauses together, in their one order.
  fn check_all<S: AsRef<[u8]>>(scalar: ast::ScalarTypeDefinition<S>) {
    assert!(scalar.implements().is_some());
    assert!(scalar.directives().is_some());
    assert!(scalar.where_clause().is_some());
  }
  accept_all!(
    ast::ScalarTypeDefinition::graphqlx,
    "scalar Id<T> implements Node @d where T: Key",
    check_all
  );

  // A scalar extension takes no `where`: it ends at its directives, and the clause is an entry
  // head of nothing.
  assert_unexpected(
    |inp| ast::Document::graphqlx(inp).map(|_| ()),
    "extend scalar S @d where T: I",
    Expectation::Production("a use item, a module, a definition or an extension"),
    Some(SyntacticTokenKind::Identifier),
  );
}

#[test]
fn failures_keep_typed_expectations_and_unclosed_kinds() {
  let error = drive_str(|inp| ast::Type::graphqlx(inp).map(|_| ()), "42")
    .expect_err("an integer cannot begin a GraphQLx type")
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

  let angle_error = drive_str(|inp| ast::Type::graphqlx(inp).map(|_| ()), "Box<Item")
    .expect_err("an unterminated type-argument list should fail")
    .into_iter()
    .next()
    .expect("an unterminated type-argument list should emit an error");
  assert!(matches!(
    angle_error.into_data(),
    ErrorData::Unclosed(Unclosed::Angle)
  ));

  reject_all!(ast::UseDefinition::graphqlx, "use");
  reject_all!(ast::UseDefinition::graphqlx, "use a::{}");
  reject_all!(ast::UseDefinition::graphqlx, "use a::{*}");
  reject_all!(ast::UseDefinition::graphqlx, "use a as");
  reject_all!(ast::UseDefinition::graphqlx, "use a::");
  reject_all!(ast::UseDefinition::graphqlx, "use ::a");
  reject_all!(ast::UseDefinition::graphqlx, "use ~::");
  reject_all!(ast::UseDefinition::graphqlx, "use \"\"\"module\"\"\"");
  reject_all!(ast::UseDefinition::graphqlx, "use a::{\"m\"}");

  // Each refusal names what the grammar takes at its position.
  assert_unexpected(
    |inp| ast::UseDefinition::graphqlx(inp).map(|_| ()),
    "use 5",
    Expectation::UseTree,
    Some(SyntacticTokenKind::Int),
  );
  assert_unexpected(
    |inp| ast::UseDefinition::graphqlx(inp).map(|_| ()),
    "use a::5",
    Expectation::NameGlobOrGroup,
    Some(SyntacticTokenKind::Int),
  );
  assert_unexpected(
    |inp| ast::UseDefinition::graphqlx(inp).map(|_| ()),
    "use a::{~::b}",
    Expectation::Name,
    Some(SyntacticTokenKind::Tilde),
  );
  assert_unexpected(
    |inp| ast::UseDefinition::graphqlx(inp).map(|_| ()),
    "use a as 5",
    Expectation::Name,
    Some(SyntacticTokenKind::Int),
  );
  assert_unexpected(
    |inp| ast::UseDefinition::graphqlx(inp).map(|_| ()),
    "import a",
    Expectation::Use,
    Some(SyntacticTokenKind::Identifier),
  );

  let group_brace_error = drive_str(
    |inp| ast::UseDefinition::graphqlx(inp).map(|_| ()),
    "use a::{Thing",
  )
  .expect_err("an unterminated group should fail")
  .into_iter()
  .next()
  .expect("an unterminated group should emit an error");
  assert!(matches!(
    group_brace_error.into_data(),
    ErrorData::Unclosed(Unclosed::Object)
  ));
}

/// A radix prefix with no digit before a delimiter is refused as the lexer refuses it: an
/// incomplete number, at the prefix's span. The lexer's batch for it used to be empty, which the
/// conversion can only report as its untyped note at `0..0`.
#[test]
fn a_bare_radix_prefix_in_an_argument_is_an_incomplete_number() {
  use smear_lexer::SyntaxErrorKind;
  use tokora::diagnostic::Diagnose as _;

  let error = drive_str(
    |inp| ast::Document::graphqlx(inp).map(|_| ()),
    "{ a(x: 0x) }",
  )
  .expect_err("a radix prefix with no digit is not a value")
  .into_iter()
  .next()
  .expect("the refused parse reports an error");
  assert_eq!(error.code().as_str(), "smear::syntax::incomplete-number");
  assert_eq!(error.kind(), SyntaxErrorKind::IncompleteNumber);
  assert_eq!(error.span(), SimpleSpan::new(7, 9));
  assert!(
    matches!(
      error.data(),
      ErrorData::Lexical(SyntaxErrorKind::IncompleteNumber)
    ),
    "{:?}",
    error.data()
  );
}

#[test]
fn complete_document_accepts_every_graphqlx_fixture_over_all_borrowed_sources() {
  const FIXTURES: &[&str] = &[
    include_str!("../../tests/fixtures/parser/graphqlx/ok/0001_use_group.graphqlx"),
    include_str!("../../tests/fixtures/parser/graphqlx/ok/0002_use_glob.graphqlx"),
    include_str!("../../tests/fixtures/parser/graphqlx/ok/0003_use_with_alias.graphqlx"),
    include_str!("../../tests/fixtures/parser/graphqlx/ok/0004_generics_simple.graphqlx"),
    include_str!("../../tests/fixtures/parser/graphqlx/ok/0005_generics_multiple_params.graphqlx"),
    include_str!("../../tests/fixtures/parser/graphqlx/ok/0006_where_clause_simple.graphqlx"),
    include_str!(
      "../../tests/fixtures/parser/graphqlx/ok/0007_where_clause_multiple_bounds.graphqlx"
    ),
    include_str!("../../tests/fixtures/parser/graphqlx/ok/0008_map_value_simple.graphqlx"),
    include_str!("../../tests/fixtures/parser/graphqlx/ok/0009_map_value_nested.graphqlx"),
    include_str!("../../tests/fixtures/parser/graphqlx/ok/0010_generics_with_default.graphqlx"),
    include_str!("../../tests/fixtures/parser/graphqlx/ok/0011_interface_with_generics.graphqlx"),
    include_str!("../../tests/fixtures/parser/graphqlx/ok/0012_path_type.graphqlx"),
    include_str!("../../tests/fixtures/parser/graphqlx/ok/0013_complex_use.graphqlx"),
    include_str!("../../tests/fixtures/parser/graphqlx/ok/0014_generics_nested.graphqlx"),
    include_str!("../../tests/fixtures/parser/graphqlx/ok/0015_extend_with_generics.graphqlx"),
    include_str!("../../tests/fixtures/parser/graphqlx/ok/0016_operation_with_generics.graphqlx"),
    include_str!("../../tests/fixtures/parser/graphqlx/ok/0017_union_with_path_types.graphqlx"),
    include_str!(
      "../../tests/fixtures/parser/graphqlx/ok/0018_union_with_path_types_and_generics.graphqlx"
    ),
    include_str!("../../tests/fixtures/parser/graphqlx/ok/0019_input_with_map_default.graphqlx"),
    include_str!("../../tests/fixtures/parser/graphqlx/ok/0020_where_multiple_predicates.graphqlx"),
    include_str!("../../tests/fixtures/parser/graphqlx/ok/0021_fat_arrow_in_map.graphqlx"),
    include_str!("../../tests/fixtures/parser/graphqlx/ok/0022_complex_fragments.graphqlx"),
  ];

  fn check_document<S: AsRef<[u8]>>(document: ast::Document<S>) {
    assert!(!document.definitions().is_empty());
  }

  for source in FIXTURES {
    check_document(
      drive_str(ast::Document::graphqlx, source).expect("str accepts complete GraphQLx fixture"),
    );
    check_document(
      drive_slice(ast::Document::graphqlx, source.as_bytes())
        .expect("slice accepts complete GraphQLx fixture"),
    );
    #[cfg(feature = "bytes_1")]
    {
      let owned = ::bytes_1::Bytes::from_static(source.as_bytes());
      check_document(
        drive_bytes_as_slice(ast::Document::graphqlx, &owned)
          .expect("Bytes slice accepts complete GraphQLx fixture"),
      );
    }
  }
}

#[test]
fn complete_document_preserves_mixed_entries_and_definition_descriptions() {
  let document = drive_str(
    ast::Document::graphqlx,
    "use \"./types\"::{User}\n\"a documented type\" type User<T> where T: Node { id: ID }\nquery GetUser { user { id } }\nextend type User<String> { displayName: String }",
  )
  .expect("mixed GraphQLx document");

  assert_eq!(document.definitions().len(), 4);
  assert!(document.definitions()[0].is_use());
  assert!(document.definitions()[3].is_extension());

  let ast::DocumentItem::Definition(type_definition) = &document.definitions()[1] else {
    panic!("expected a described type-system definition");
  };
  assert!(type_definition.description().is_some());
  assert!(matches!(
    type_definition.node(),
    ast::Definition::TypeSystem(ast::TypeSystemDefinition::Type(_))
  ));

  let ast::DocumentItem::Definition(executable_definition) = &document.definitions()[2] else {
    panic!("expected an executable definition");
  };
  assert!(executable_definition.description().is_none());
  assert!(matches!(
    executable_definition.node(),
    ast::Definition::Executable(ast::ExecutableDefinition::Operation(_))
  ));
}

#[cfg(feature = "bytes_1")]
#[test]
fn complete_document_direct_api_preserves_owned_bytes() {
  let source = ::bytes_1::Bytes::from_static(
    b"use \"./types\"::{User}\ntype User { id: ID }\nquery Users { users { id } }",
  );
  let document: ast::Document<::bytes_1::Bytes> =
    drive_owned_bytes(ast::Document::graphqlx, &source).expect("owned Bytes document");
  assert_eq!(document.definitions().len(), 3);
}

#[test]
fn complete_document_rejects_descriptions_before_use_items_and_extensions() {
  assert_unexpected(
    |inp| ast::DocumentItem::graphqlx(inp).map(|_| ()),
    "\"description\" use \"./types\"::{User}",
    Expectation::Production("a definition or a module after a description"),
    Some(SyntacticTokenKind::Identifier),
  );
  assert_unexpected(
    |inp| ast::DocumentItem::graphqlx(inp).map(|_| ()),
    "\"description\" extend type User { id: ID }",
    Expectation::Production("a definition or a module after a description"),
    Some(SyntacticTokenKind::Identifier),
  );
}
