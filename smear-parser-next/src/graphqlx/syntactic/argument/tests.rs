//! Focused GraphQLx argument-production tests.

use smear_lexer::graphqlx::syntactic::SyntacticTokenKind;
use tokora::{FatalContext, Parse, Parser, SimpleSpan, utils::cmp::Equivalent};

use super::super::{GraphqlxInput, GraphqlxLexer};
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
    $check(drive_str($parser, $src).expect(concat!("str accept: ", $src)));
    $check(drive_slice($parser, $src.as_bytes()).expect(concat!("slice accept: ", $src)));
    #[cfg(feature = "bytes")]
    {
      let owned = ::bytes::Bytes::from_static($src.as_bytes());
      $check(drive_bytes_as_slice($parser, &owned).expect(concat!("bytes accept: ", $src)));
    }
  }};
}

fn assert_unexpected<'inp>(
  parser: impl for<'c> FnMut(
    &mut GraphqlxInput<'inp, 'c, str, StrCtx<'inp>>,
  ) -> Result<(), GraphqlxErrors<&'inp str>>,
  input: &'inp str,
  expected: Expectation,
  found: Option<SyntacticTokenKind>,
) {
  let error = drive_str(parser, input)
    .expect_err("input should report a typed parser error")
    .into_iter()
    .next()
    .expect("parser error collection should contain one error");
  assert!(matches!(
    error.into_data(),
    ErrorData::UnexpectedToken(unexpected)
      if unexpected.expected() == &expected && unexpected.found().copied() == found
  ));
}

#[test]
fn direct_argument_apis_accept_standard_graphql_forms_over_every_source() {
  fn check_argument<S: AsRef<[u8]>>(argument: ast::Argument<S>) {
    assert!("count".equivalent(argument.name().source()));
    assert!(matches!(argument.value(), ast::InputValue::Int(_)));
    assert_eq!(argument.span(), &SimpleSpan::new(0, 8));
  }
  accept_all!(ast::Argument::graphqlx, "count: 1", check_argument);

  fn check_const_argument<S: AsRef<[u8]>>(argument: ast::ConstArgument<S>) {
    assert!("enabled".equivalent(argument.name().source()));
    assert!(matches!(argument.value(), ast::ConstInputValue::Boolean(_)));
    assert_eq!(argument.span(), &SimpleSpan::new(0, 13));
  }
  accept_all!(
    ast::ConstArgument::graphqlx,
    "enabled: true",
    check_const_argument
  );

  fn check_arguments<S: AsRef<[u8]>>(arguments: ast::Arguments<S>) {
    assert_eq!(arguments.arguments().len(), 2);
    assert!("first".equivalent(arguments.arguments()[0].name().source()));
    assert!("second".equivalent(arguments.arguments()[1].name().source()));
  }
  accept_all!(
    ast::Arguments::graphqlx,
    "(first: 1, second: true)",
    check_arguments
  );

  fn check_const_arguments<S: AsRef<[u8]>>(arguments: ast::ConstArguments<S>) {
    assert_eq!(arguments.arguments().len(), 1);
  }
  accept_all!(
    ast::ConstArguments::graphqlx,
    "(item: { id: 1 })",
    check_const_arguments
  );
}

#[test]
fn direct_arguments_preserve_phase_local_expectations() {
  assert_unexpected(
    |inp| ast::Argument::graphqlx(inp).map(|_| ()),
    "",
    Expectation::Name,
    None,
  );
  assert_unexpected(
    |inp| ast::ConstArgument::graphqlx(inp).map(|_| ()),
    "item",
    Expectation::Colon,
    None,
  );
  assert_unexpected(
    |inp| ast::Argument::graphqlx(inp).map(|_| ()),
    "item:",
    Expectation::InputValue,
    None,
  );
  assert_unexpected(
    |inp| ast::ConstArgument::graphqlx(inp).map(|_| ()),
    "item:",
    Expectation::ConstInputValue,
    None,
  );
}

#[test]
fn argument_collections_are_absent_or_empty_without_losing_the_following_token() {
  let (empty, following) = drive_str(
    |inp| {
      let arguments = ast::Arguments::graphqlx(inp)?;
      let following = super::super::name(inp)?;
      Ok((arguments, following))
    },
    "field",
  )
  .expect("absent arguments should not consume the following name");
  assert!(empty.arguments().is_empty());
  assert_eq!(empty.span(), &SimpleSpan::new(0, 0));
  assert!("field".equivalent(following.source()));

  let (empty_const, following) = drive_str(
    |inp| {
      let arguments = ast::ConstArguments::graphqlx(inp)?;
      let following = super::super::name(inp)?;
      Ok((arguments, following))
    },
    "field",
  )
  .expect("absent constant arguments should not consume the following name");
  assert!(empty_const.arguments().is_empty());
  assert_eq!(empty_const.span(), &SimpleSpan::new(0, 0));
  assert!("field".equivalent(following.source()));

  fn check_empty<S: AsRef<[u8]>>(arguments: ast::Arguments<S>) {
    assert!(arguments.arguments().is_empty());
    assert_eq!(arguments.span(), &SimpleSpan::new(0, 2));
  }
  accept_all!(ast::Arguments::graphqlx, "()", check_empty);

  fn check_empty_const<S: AsRef<[u8]>>(arguments: ast::ConstArguments<S>) {
    assert!(arguments.arguments().is_empty());
    assert_eq!(arguments.span(), &SimpleSpan::new(0, 2));
  }
  accept_all!(ast::ConstArguments::graphqlx, "()", check_empty_const);
}

#[cfg(feature = "bytes")]
#[test]
fn direct_argument_api_preserves_owned_bytes_payloads() {
  let source = ::bytes::Bytes::from_static(b"count: 1");
  let argument: ast::Argument<::bytes::Bytes> =
    drive_owned_bytes(ast::Argument::graphqlx, &source).expect("owned Bytes argument should parse");
  assert_eq!(
    argument.name().source(),
    &::bytes::Bytes::from_static(b"count")
  );
}

#[test]
fn argument_collections_commit_malformed_members_and_report_unclosed_parens() {
  assert_unexpected(
    |inp| ast::Arguments::graphqlx(inp).map(|_| ()),
    "(arg: )",
    Expectation::InputValue,
    Some(SyntacticTokenKind::RParen),
  );
  assert_unexpected(
    |inp| ast::Arguments::graphqlx(inp).map(|_| ()),
    "(: value)",
    Expectation::Name,
    Some(SyntacticTokenKind::Colon),
  );
  assert_unexpected(
    |inp| ast::ConstArgument::graphqlx(inp).map(|_| ()),
    "item: $value",
    Expectation::ConstInputValue,
    Some(SyntacticTokenKind::Dollar),
  );

  let error = drive_str(ast::Arguments::graphqlx, "(arg: 1")
    .expect_err("unterminated arguments should fail")
    .into_iter()
    .next()
    .expect("unterminated arguments should emit an error");
  assert!(matches!(
    error.into_data(),
    ErrorData::Unclosed(Unclosed::Parentheses)
  ));

  let error = drive_str(ast::ConstArguments::graphqlx, "(arg: 1")
    .expect_err("unterminated constant arguments should fail")
    .into_iter()
    .next()
    .expect("unterminated constant arguments should emit an error");
  assert!(matches!(
    error.into_data(),
    ErrorData::Unclosed(Unclosed::Parentheses)
  ));
}
