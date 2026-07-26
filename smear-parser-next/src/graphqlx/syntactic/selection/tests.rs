//! Focused GraphQLx selection-production tests.

use smear_lexer::graphqlx::syntactic::SyntacticTokenKind;
use tokora::{FatalContext, Parse, Parser, try_parse_input::ParseAttempt, utils::cmp::Equivalent};

use super::{field, fragment_spread, inline_fragment, selection, selection_set, type_condition};
use crate::graphqlx::{
  GraphQLx, ast,
  error::{ErrorData, Expectation, GraphqlxErrors, Unclosed},
  syntactic::{GraphqlxInput, GraphqlxLexer},
};

type StrCtx<'inp> =
  FatalContext<'inp, GraphqlxLexer<'inp, str>, GraphqlxErrors<&'inp str>, GraphQLx>;
type SliceCtx<'inp> =
  FatalContext<'inp, GraphqlxLexer<'inp, [u8]>, GraphqlxErrors<&'inp [u8]>, GraphQLx>;

#[cfg(feature = "bytes")]
type BytesCtx<'inp> =
  FatalContext<'inp, GraphqlxLexer<'inp, ::bytes::Bytes>, GraphqlxErrors<::bytes::Bytes>, GraphQLx>;

fn drive_str<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlxInput<'inp, 'c, str, StrCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<&'inp str>>,
  input: &'inp str,
) -> Result<O, GraphqlxErrors<&'inp str>> {
  Parser::with_parser_of::<'inp, GraphqlxLexer<'inp, str>, O, GraphqlxErrors<&'inp str>, _, GraphQLx>(
    f,
  )
  .parse_str(input)
}

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
    GraphQLx,
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
    GraphQLx,
  >(f)
  .parse_bytes(input)
}

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
    GraphQLx,
  >(f)
  .parse(input)
}

macro_rules! accept_all {
  ($parser:expr, $src:expr, $check:path) => {{
    $check(drive_str($parser, $src).expect(concat!("str accepts: ", $src)));
    $check(drive_slice($parser, $src.as_bytes()).expect(concat!("slice accepts: ", $src)));
    #[cfg(feature = "bytes")]
    {
      let owned = ::bytes::Bytes::from_static($src.as_bytes());
      $check(drive_bytes_as_slice($parser, &owned).expect(concat!("bytes accepts: ", $src)));
    }
  }};
}

fn assert_unexpected(
  result: Result<(), GraphqlxErrors<&str>>,
  expected: Expectation,
  found: Option<SyntacticTokenKind>,
) {
  let error = result
    .expect_err("fixture should report a typed error")
    .into_iter()
    .next()
    .expect("fatal context reports one error");
  assert!(matches!(
    error.into_data(),
    ErrorData::UnexpectedToken(unexpected)
      if unexpected.expected() == &expected && unexpected.found().copied() == found
  ));
}

#[test]
fn direct_field_api_accepts_alias_arguments_directives_and_nested_selections() {
  fn check<S: AsRef<[u8]>>(field: ast::Field<S>) {
    assert!("alias".equivalent(field.alias().expect("alias").name().source()));
    assert!("user".equivalent(field.name().source()));
    assert_eq!(field.arguments().expect("arguments").arguments().len(), 1);
    assert_eq!(
      field.directives().expect("directives").directives().len(),
      1
    );
    assert_eq!(
      field
        .selection_set()
        .expect("nested set")
        .selections()
        .len(),
      1
    );
  }

  accept_all!(
    ast::Field::graphqlx,
    "alias: user(id: 1) @::tool::cache<Key>(ttl: 1) { id }",
    check
  );
}

#[test]
fn type_conditions_and_spread_dispatch_preserve_graphqlx_paths_and_generics() {
  fn check_condition<S: AsRef<[u8]>>(condition: ast::TypeCondition<S>) {
    let path = condition.name();
    assert!(path.path().is_fully_qualified());
    assert_eq!(path.path().segments().len(), 2);
    assert_eq!(
      path.type_generics().expect("type arguments").params().len(),
      1
    );
  }
  accept_all!(
    ast::TypeCondition::graphqlx,
    "on ::pkg::User<Node>",
    check_condition
  );

  fn check_named<S: AsRef<[u8]>>(selection: ast::Selection<S>) {
    let spread = selection.unwrap_fragment_spread_ref();
    assert!(spread.name().path().is_fully_qualified());
    assert_eq!(spread.name().path().segments().len(), 2);
    assert_eq!(
      spread
        .name()
        .type_generics()
        .expect("type arguments")
        .params()
        .len(),
      1
    );
    assert_eq!(
      spread.directives().expect("directives").directives().len(),
      1
    );
  }
  accept_all!(
    ast::Selection::graphqlx,
    "... ::shared::Part<Node> @skip(if: false)",
    check_named
  );

  fn check_typed_inline<S: AsRef<[u8]>>(selection: ast::Selection<S>) {
    let fragment = selection.unwrap_inline_fragment_ref();
    let condition = fragment.type_condition().expect("type condition");
    assert!(condition.name().path().is_fully_qualified());
    assert!(condition.name().type_generics().is_some());
    assert_eq!(fragment.selection_set().selections().len(), 1);
  }
  accept_all!(
    ast::Selection::graphqlx,
    "... on ::pkg::User<Node> { id }",
    check_typed_inline
  );

  fn check_untyped_inline<S: AsRef<[u8]>>(selection: ast::Selection<S>) {
    let fragment = selection.unwrap_inline_fragment_ref();
    assert!(fragment.type_condition().is_none());
    assert_eq!(
      fragment
        .directives()
        .expect("directives")
        .directives()
        .len(),
      1
    );
    assert_eq!(fragment.selection_set().selections().len(), 1);
  }
  accept_all!(
    ast::Selection::graphqlx,
    "... @skip(if: true) { id }",
    check_untyped_inline
  );
}

#[test]
fn type_condition_try_api_declines_without_consuming_a_non_on_name() {
  let (attempt, following) = drive_str(
    |inp| {
      let attempt = ast::TypeCondition::try_graphqlx(inp)?;
      let following = super::super::name(inp)?;
      Ok((attempt, following))
    },
    "User",
  )
  .expect("a non-on name remains available");

  assert!(matches!(attempt, ParseAttempt::Decline));
  assert!("User".equivalent(following.source()));
}

#[test]
fn selection_set_is_nonempty_recursive_and_reports_local_failures() {
  fn check<S: AsRef<[u8]>>(selection_set: ast::SelectionSet<S>) {
    assert_eq!(selection_set.selections().len(), 3);
    assert!(selection_set.selections()[0].is_field());
    assert!(selection_set.selections()[1].is_fragment_spread());
    assert!(selection_set.selections()[2].is_inline_fragment());
  }
  accept_all!(
    ast::SelectionSet::graphqlx,
    "{ user { id } ... ::Part<Node> ... on Item { name } }",
    check
  );

  let empty = drive_str(|inp| selection_set(inp).map(|_| ()), "{}")
    .expect_err("empty selection set violates the grammar")
    .into_iter()
    .next()
    .expect("fatal context reports one error");
  assert!(matches!(
    empty.into_data(),
    ErrorData::Other(message) if message == "too few elements"
  ));

  let unclosed = drive_str(|inp| selection_set(inp).map(|_| ()), "{ id")
    .expect_err("unterminated selection set fails")
    .into_iter()
    .next()
    .expect("fatal context reports one error");
  assert!(matches!(
    unclosed.into_data(),
    ErrorData::Unclosed(Unclosed::Object)
  ));

  assert_unexpected(
    drive_str(|inp| selection(inp).map(|_| ()), "123"),
    Expectation::Name,
    Some(SyntacticTokenKind::Int),
  );
  assert_unexpected(
    drive_str(|inp| type_condition(inp).map(|_| ()), "User"),
    Expectation::Keyword("on"),
    Some(SyntacticTokenKind::Identifier),
  );
  assert_unexpected(
    drive_str(|inp| fragment_spread(inp).map(|_| ()), "... on"),
    Expectation::Path,
    Some(SyntacticTokenKind::Identifier),
  );
  assert!(
    drive_str(|inp| inline_fragment(inp).map(|_| ()), "... on").is_err(),
    "a typed inline fragment commits after `on` and requires its type path"
  );
  assert_unexpected(
    drive_str(|inp| field(inp).map(|_| ()), "alias: }"),
    Expectation::Name,
    Some(SyntacticTokenKind::RBrace),
  );
}

#[cfg(feature = "bytes")]
#[test]
fn direct_selection_api_preserves_owned_bytes_payloads() {
  let source = ::bytes::Bytes::from_static(b"user { id }");
  let field: ast::Field<::bytes::Bytes> = drive_owned_bytes(ast::Field::graphqlx, &source)
    .expect("owned Bytes field parses without a borrowed fallback");
  assert_eq!(field.name().source(), &::bytes::Bytes::from_static(b"user"));
}
