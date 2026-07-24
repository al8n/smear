//! `Argument`/`Arguments` production tests.
//!
//! Mirrors `value`'s and `ty`'s test harness: every case drives the real GraphQL
//! syntactic lexer under a `Fatal<GraphqlErrors>` context, accept cases run the
//! full source matrix, reject cases assert the error family, and a table-driven
//! oracle pins the frozen `smear-parser` `parse_argument`/`parse_arguments`
//! verdicts for the same inputs.

use smear_lexer::graphql::syntactic::SyntacticTokenKind;
use tokora::{FatalContext, Parse, Parser, SimpleSpan, utils::cmp::Equivalent};

use crate::graphql::{
  GraphQL,
  ast::{
    Argument, Argument as AstArgument, Arguments, Arguments as AstArguments, ConstArgument,
    ConstArgument as AstConstArgument, ConstArguments, ConstArguments as AstConstArguments,
  },
  error::{ErrorData, Expectation, GraphqlErrors, Unclosed},
  syntactic::{GraphqlInput, GraphqlLexer},
};

/// The fatal context a `str`-sourced parse runs under.
type StrCtx<'inp> = FatalContext<'inp, GraphqlLexer<'inp, str>, GraphqlErrors<&'inp str>, GraphQL>;
/// The fatal context a `[u8]`-sourced parse runs under.
type SliceCtx<'inp> =
  FatalContext<'inp, GraphqlLexer<'inp, [u8]>, GraphqlErrors<&'inp [u8]>, GraphQL>;

/// Drives `f` over a `str` source under `Fatal<GraphqlErrors<&str>>`.
fn drive_str<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlInput<'inp, 'c, str, StrCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp str>>,
  input: &'inp str,
) -> Result<O, GraphqlErrors<&'inp str>> {
  Parser::with_parser_of::<'inp, GraphqlLexer<'inp, str>, O, GraphqlErrors<&'inp str>, _, GraphQL>(
    f,
  )
  .parse_str(input)
}

/// Drives `f` over a `[u8]` source under `Fatal<GraphqlErrors<&[u8]>>`.
fn drive_slice<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlInput<'inp, 'c, [u8], SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  input: &'inp [u8],
) -> Result<O, GraphqlErrors<&'inp [u8]>> {
  Parser::with_parser_of::<
    'inp,
    GraphqlLexer<'inp, [u8]>,
    O,
    GraphqlErrors<&'inp [u8]>,
    _,
    GraphQL,
  >(f)
  .parse_slice(input)
}

#[cfg(feature = "bytes")]
fn drive_bytes<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlInput<'inp, 'c, [u8], SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  input: &'inp ::bytes::Bytes,
) -> Result<O, GraphqlErrors<&'inp [u8]>> {
  Parser::with_parser_of::<
    'inp,
    GraphqlLexer<'inp, [u8]>,
    O,
    GraphqlErrors<&'inp [u8]>,
    _,
    GraphQL,
  >(f)
  .parse_bytes(input)
}

/// Runs `parser` over `src` as `str`, `[u8]`, and (behind the feature) `Bytes`,
/// applying the generic `check` fn to each accepted AST — the source matrix.
macro_rules! accept_all {
  ($parser:expr, $src:expr, $check:path) => {{
    $check(drive_str($parser, $src).expect(concat!("str accept: ", $src)));
    $check(drive_slice($parser, $src.as_bytes()).expect(concat!("slice accept: ", $src)));
    #[cfg(feature = "bytes")]
    {
      let owned = ::bytes::Bytes::from_static($src.as_bytes());
      $check(drive_bytes($parser, &owned).expect(concat!("bytes accept: ", $src)));
    }
  }};
}

/// Asserts `parser` rejects `src` over both `str` and `[u8]`.
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

// ─── argument ────────────────────────────────────────────────────────────────

#[test]
fn argument_accepts() {
  fn check<S: AsRef<[u8]>>(a: AstArgument<S>) {
    assert!("x".equivalent(a.name().source()));
    assert!(a.value().is_int());
    assert_eq!(a.span().start(), 0);
  }
  accept_all!(Argument::<_>::graphql, "x: 1", check);
}

#[test]
fn argument_accepts_variable_value() {
  fn check<S: AsRef<[u8]>>(a: AstArgument<S>) {
    assert!(a.value().is_variable());
  }
  accept_all!(Argument::<_>::graphql, "x: $v", check);
}

#[test]
fn argument_accepts_composite_value() {
  fn check<S: AsRef<[u8]>>(a: AstArgument<S>) {
    let list = a.value().unwrap_list_ref();
    assert_eq!(list.values().len(), 2);
  }
  accept_all!(Argument::<_>::graphql, "xs: [1, 2]", check);
}

#[test]
fn argument_rejects_missing_colon() {
  reject_all!(Argument::<_>::graphql, "x 1");
}

#[test]
fn argument_rejects_missing_value() {
  reject_all!(Argument::<_>::graphql, "x:");
  reject_all!(Argument::<_>::graphql, "x: }");
}

#[test]
fn argument_rejects_missing_name() {
  reject_all!(Argument::<_>::graphql, ": 1");
  reject_all!(Argument::<_>::graphql, "");
}

#[test]
fn argument_phase_diagnostics_are_typed() {
  for (src, expected, found) in [
    (": 1", Expectation::Name, Some(SyntacticTokenKind::Colon)),
    ("x 1", Expectation::Colon, Some(SyntacticTokenKind::Int)),
    ("x:", Expectation::InputValue, None),
    (
      "x: }",
      Expectation::InputValue,
      Some(SyntacticTokenKind::RBrace),
    ),
  ] {
    let error = drive_str(|inp| Argument::<_>::graphql(inp).map(|_| ()), src)
      .expect_err("malformed argument should fail")
      .into_iter()
      .next()
      .expect("malformed argument should emit an error");
    assert!(matches!(
      error.into_data(),
      ErrorData::UnexpectedToken(unexpected)
        if unexpected.expected() == &expected && unexpected.found() == found.as_ref()
    ));
  }
}

// ─── const_argument ──────────────────────────────────────────────────────────

#[test]
fn const_argument_accepts_and_rejects_variable() {
  fn check<S: AsRef<[u8]>>(a: AstConstArgument<S>) {
    assert!("k".equivalent(a.name().source()));
    assert!(a.value().is_int());
  }
  accept_all!(ConstArgument::<_>::graphql, "k: 1", check);
  reject_all!(ConstArgument::<_>::graphql, "k: $v");
}

// ─── arguments ───────────────────────────────────────────────────────────────

#[test]
fn arguments_accepts_single() {
  fn check<S: AsRef<[u8]>>(args: AstArguments<S>) {
    assert_eq!(args.arguments().len(), 1);
    assert!("x".equivalent(args.arguments()[0].name().source()));
  }
  accept_all!(Arguments::<_>::graphql, "(x: 1)", check);
}

#[test]
fn arguments_accepts_multiple() {
  fn check<S: AsRef<[u8]>>(args: AstArguments<S>) {
    assert_eq!(args.arguments().len(), 2);
    assert!("a".equivalent(args.arguments()[0].name().source()));
    assert!("b".equivalent(args.arguments()[1].name().source()));
  }
  accept_all!(Arguments::<_>::graphql, "(a: 1, b: 2)", check);
}

#[test]
fn arguments_accepts_empty_parens() {
  // Leniency deviation from the spec (plan Amendment 5, REVERSES the Amendment-2
  // entry for this site): `Arguments : ( Argument+ )` demands one-or-more, but
  // parser-next stays lenient and accepts an empty `()` here, matching the frozen
  // parser's unenforced `+`.
  fn check<S: AsRef<[u8]>>(args: AstArguments<S>) {
    assert!(args.arguments().is_empty());
    assert_eq!(*args.span(), SimpleSpan::new(0, 2));
  }
  accept_all!(Arguments::<_>::graphql, "()", check);
}

#[test]
fn arguments_absent_is_empty_zero_width_and_non_consuming() {
  // No tokens consumed: a following production sees the identifier untouched.
  let ok = drive_str(
    |inp| {
      let args = Arguments::<_>::graphql(inp)?;
      let leftover = crate::combinator::ident(inp)?;
      Ok::<_, GraphqlErrors<&str>>(
        args.arguments().is_empty()
          && *args.span() == SimpleSpan::new(0, 0)
          && *leftover.source_ref() == "x",
      )
    },
    "x",
  )
  .unwrap();
  assert!(ok);
}

#[test]
fn arguments_rejects_unterminated() {
  reject_all!(Arguments::<_>::graphql, "(x: 1");
  let error = drive_str(|inp| Arguments::<_>::graphql(inp).map(|_| ()), "(x: 1")
    .expect_err("unterminated arguments should fail")
    .into_iter()
    .next()
    .expect("unterminated arguments should emit an error");
  assert!(matches!(
    error.into_data(),
    ErrorData::Unclosed(Unclosed::Parentheses)
  ));
}

#[test]
fn arguments_rejects_malformed_argument() {
  reject_all!(Arguments::<_>::graphql, "(x 1)");
}

#[test]
fn arguments_commit_non_closing_heads_to_argument_phases() {
  for (src, expected, found) in [
    ("(foo)", Expectation::Colon, SyntacticTokenKind::RParen),
    ("(x:)", Expectation::InputValue, SyntacticTokenKind::RParen),
  ] {
    let error = drive_str(|inp| Arguments::<_>::graphql(inp).map(|_| ()), src)
      .expect_err("malformed argument list should fail")
      .into_iter()
      .next()
      .expect("malformed argument list should emit an error");
    assert!(matches!(
      error.into_data(),
      ErrorData::UnexpectedToken(unexpected)
        if unexpected.expected() == &expected && unexpected.found() == Some(&found)
    ));
  }
}

// ─── const_arguments ─────────────────────────────────────────────────────────

#[test]
fn const_arguments_accepts_and_rejects_variable() {
  fn check<S: AsRef<[u8]>>(args: AstConstArguments<S>) {
    assert_eq!(args.arguments().len(), 1);
  }
  accept_all!(ConstArguments::<_>::graphql, "(k: 1)", check);
  reject_all!(ConstArguments::<_>::graphql, "(k: $v)");
}

#[test]
fn const_arguments_absent_is_empty_and_zero_width() {
  let args = drive_str(ConstArguments::<_>::graphql, "").unwrap();
  assert!(args.arguments().is_empty());
  assert_eq!(*args.span(), SimpleSpan::new(0, 0));
}

#[test]
fn const_arguments_accepts_empty_parens() {
  // Const twin of `arguments_accepts_empty_parens` (plan Amendment 5): empty `()`
  // is accepted, matching frozen parity.
  fn check<S: AsRef<[u8]>>(args: AstConstArguments<S>) {
    assert!(args.arguments().is_empty());
    assert_eq!(*args.span(), SimpleSpan::new(0, 2));
  }
  accept_all!(ConstArguments::<_>::graphql, "()", check);
}

// ─── frozen-parity oracle (table-driven) ─────────────────────────────────────

/// Accept/reject verdicts the frozen `smear-parser` `parse_argument` produces for
/// the same inputs.
const ARGUMENT_ORACLE: &[(&str, bool)] = &[
  ("x: 1", true),
  ("x: \"s\"", true),
  ("x: $v", true),
  ("x: [1, 2]", true),
  ("x: { a: 1 }", true),
  ("x 1", false),
  ("x:", false),
  (":", false),
  ("", false),
];

#[test]
fn argument_matches_frozen_verdicts() {
  for (src, accept) in ARGUMENT_ORACLE {
    assert_eq!(
      drive_str(|inp| Argument::<_>::graphql(inp).map(|_| ()), src).is_ok(),
      *accept,
      "str argument({src:?})"
    );
    assert_eq!(
      drive_slice(
        |inp| Argument::<_>::graphql(inp).map(|_| ()),
        src.as_bytes()
      )
      .is_ok(),
      *accept,
      "slice argument({src:?})"
    );
  }
}

/// Accept/reject verdicts for `arguments`. Includes the empty-`()` row: frozen
/// accepts it, and parser-next now matches (plan Amendment 5, frozen parity) —
/// pinned by `arguments_accepts_empty_parens`.
const ARGUMENTS_ORACLE: &[(&str, bool)] = &[
  ("(x: 1)", true),
  ("(a: 1, b: 2)", true),
  ("()", true),
  ("(x: 1", false),
  ("(x 1)", false),
];

#[test]
fn arguments_matches_frozen_verdicts() {
  for (src, accept) in ARGUMENTS_ORACLE {
    assert_eq!(
      drive_str(|inp| Arguments::<_>::graphql(inp).map(|_| ()), src).is_ok(),
      *accept,
      "str arguments({src:?})"
    );
    assert_eq!(
      drive_slice(
        |inp| Arguments::<_>::graphql(inp).map(|_| ()),
        src.as_bytes()
      )
      .is_ok(),
      *accept,
      "slice arguments({src:?})"
    );
  }
  // A missing `(` yields an empty collection rather than erroring.
  assert!(drive_str(|inp| Arguments::<_>::graphql(inp).map(|_| ()), "x").is_ok());
}
