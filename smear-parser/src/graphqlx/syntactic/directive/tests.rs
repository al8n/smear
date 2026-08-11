//! Focused GraphQLx directive-production tests.

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
  Parser::with_parser::<'inp, GraphqlxLexer<'inp, str>, O, GraphqlxErrors<&'inp str>, _, GraphQLx>(
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

#[cfg(feature = "bytes")]
fn drive_bytes_as_slice<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlxInput<'inp, 'c, [u8], SliceCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<&'inp [u8]>>,
  input: &'inp ::bytes::Bytes,
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

#[cfg(feature = "bytes")]
fn drive_owned_bytes<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlxInput<'inp, 'c, ::bytes::Bytes, BytesCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<::bytes::Bytes>>,
  input: &'inp ::bytes::Bytes,
) -> Result<O, GraphqlxErrors<::bytes::Bytes>> {
  Parser::with_parser::<
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
fn direct_directive_apis_accept_standard_graphql_forms_over_every_source() {
  fn check_directive<S: AsRef<[u8]>>(directive: ast::Directive<S>) {
    assert!("skip".equivalent(directive.name().path().segments()[0].source()));
    assert_eq!(
      directive
        .arguments()
        .expect("standard arguments")
        .arguments()
        .len(),
      1
    );
  }
  accept_all!(ast::Directive::graphqlx, "@skip(if: true)", check_directive);

  fn check_const_directive<S: AsRef<[u8]>>(directive: ast::ConstDirective<S>) {
    assert!("include".equivalent(directive.name().path().segments()[0].source()));
    assert_eq!(
      directive
        .arguments()
        .expect("standard arguments")
        .arguments()
        .len(),
      1
    );
  }
  accept_all!(
    ast::ConstDirective::graphqlx,
    "@include(if: false)",
    check_const_directive
  );

  fn check_directives<S: AsRef<[u8]>>(directives: ast::Directives<S>) {
    assert_eq!(directives.directives().len(), 2);
  }
  accept_all!(
    ast::Directives::graphqlx,
    "@skip(if: true) @include(if: false)",
    check_directives
  );

  fn check_const_directives<S: AsRef<[u8]>>(directives: ast::ConstDirectives<S>) {
    assert_eq!(directives.directives().len(), 2);
  }
  accept_all!(
    ast::ConstDirectives::graphqlx,
    "@skip(if: true) @include(if: false)",
    check_const_directives
  );
}

#[test]
fn directives_use_generic_capable_type_paths_and_store_empty_or_absent_arguments_as_none() {
  fn check_generic_path<S: AsRef<[u8]>>(directive: ast::Directive<S>) {
    let name = directive.name();
    assert!(name.path().is_fully_qualified());
    assert_eq!(name.path().segments().len(), 2);
    assert_eq!(
      name.type_generics().expect("type arguments").params().len(),
      1
    );
    assert_eq!(
      directive.arguments().expect("arguments").arguments().len(),
      1
    );
  }
  accept_all!(
    ast::Directive::graphqlx,
    "@::tool::cache<Key>(ttl: 1)",
    check_generic_path
  );

  fn check_empty_arguments<S: AsRef<[u8]>>(directive: ast::Directive<S>) {
    assert!(directive.arguments().is_none());
    assert_eq!(directive.span(), &SimpleSpan::new(0, 8));
  }
  accept_all!(ast::Directive::graphqlx, "@cache()", check_empty_arguments);

  fn check_absent_arguments<S: AsRef<[u8]>>(directive: ast::Directive<S>) {
    assert!(directive.arguments().is_none());
    assert_eq!(directive.span(), &SimpleSpan::new(0, 6));
  }
  accept_all!(ast::Directive::graphqlx, "@cache", check_absent_arguments);

  let directive = drive_str(ast::ConstDirective::graphqlx, "@cache()")
    .expect("constant directive with empty arguments should parse");
  assert!(directive.arguments().is_none());
  assert_eq!(directive.span(), &SimpleSpan::new(0, 8));
}

#[test]
fn directive_collections_are_absent_without_losing_the_following_token() {
  let (directives, following) = drive_str(
    |inp| {
      let directives = ast::Directives::graphqlx(inp)?;
      let following = super::super::name(inp)?;
      Ok((directives, following))
    },
    "field",
  )
  .expect("absent directives should not consume the following name");
  assert!(directives.directives().is_empty());
  assert_eq!(directives.span(), &SimpleSpan::new(0, 0));
  assert!("field".equivalent(following.source()));

  let directives = drive_str(ast::ConstDirectives::graphqlx, "field")
    .expect("absent constant directives should parse as an empty collection");
  assert!(directives.directives().is_empty());
  assert_eq!(directives.span(), &SimpleSpan::new(0, 0));
}

#[cfg(feature = "bytes")]
#[test]
fn direct_directive_api_preserves_owned_bytes_payloads() {
  let source = ::bytes::Bytes::from_static(b"@skip(if: true)");
  let directive: ast::Directive<::bytes::Bytes> =
    drive_owned_bytes(ast::Directive::graphqlx, &source)
      .expect("owned Bytes directive should parse");
  assert_eq!(
    directive.name().path().segments()[0].source(),
    &::bytes::Bytes::from_static(b"skip")
  );
}

#[test]
fn directives_commit_and_report_typed_malformed_or_unclosed_inputs() {
  assert_unexpected(
    |inp| ast::Directive::graphqlx(inp).map(|_| ()),
    "@",
    Expectation::Path,
    None,
  );
  assert_unexpected(
    |inp| ast::Directive::graphqlx(inp).map(|_| ()),
    "@::",
    Expectation::Path,
    None,
  );
  assert_unexpected(
    |inp| ast::Directive::graphqlx(inp).map(|_| ()),
    "@cache(arg: )",
    Expectation::InputValue,
    Some(SyntacticTokenKind::RParen),
  );
  assert_unexpected(
    |inp| ast::ConstDirective::graphqlx(inp).map(|_| ()),
    "@cache(arg: $value)",
    Expectation::ConstInputValue,
    Some(SyntacticTokenKind::Dollar),
  );

  let error = drive_str(ast::Directive::graphqlx, "@cache(")
    .expect_err("unterminated directive arguments should fail")
    .into_iter()
    .next()
    .expect("unterminated arguments should emit an error");
  assert!(matches!(
    error.into_data(),
    ErrorData::Unclosed(Unclosed::Parentheses)
  ));

  let error = drive_str(ast::Directive::graphqlx, "@cache<Thing")
    .expect_err("unterminated directive type arguments should fail")
    .into_iter()
    .next()
    .expect("unterminated type arguments should emit an error");
  assert!(matches!(
    error.into_data(),
    ErrorData::Unclosed(Unclosed::Angle)
  ));
}
