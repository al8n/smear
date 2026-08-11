//! `Directive`/`Directives` production tests.
//!
//! Mirrors `value`'s, `ty`'s, and `argument`'s test harness: every case drives the
//! real GraphQL syntactic lexer under a `Fatal<GraphqlErrors>` context, accept cases
//! run the full source matrix, reject cases assert the error family, and a
//! table-driven oracle pins the frozen `smear-parser`
//! `parse_directive`/`parse_directives` verdicts for the same inputs.

use crate::lexer::graphql::syntactic::SyntacticTokenKind;
use tokora::{FatalContext, Parse, Parser, SimpleSpan, utils::cmp::Equivalent};

use crate::parser::graphql::{
  GraphQL,
  ast::{
    ConstDirective, ConstDirective as AstConstDirective, ConstDirectives,
    ConstDirectives as AstConstDirectives, Directive, Directive as AstDirective, Directives,
    Directives as AstDirectives,
  },
  error::{ErrorData, Expectation, GraphqlErrors},
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
  Parser::with_parser::<'inp, GraphqlLexer<'inp, str>, O, GraphqlErrors<&'inp str>, _, GraphQL>(f)
    .parse_str(input)
}

/// Drives `f` over a `[u8]` source under `Fatal<GraphqlErrors<&[u8]>>`.
fn drive_slice<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlInput<'inp, 'c, [u8], SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  input: &'inp [u8],
) -> Result<O, GraphqlErrors<&'inp [u8]>> {
  Parser::with_parser::<'inp, GraphqlLexer<'inp, [u8]>, O, GraphqlErrors<&'inp [u8]>, _, GraphQL>(f)
    .parse_slice(input)
}

#[cfg(feature = "bytes")]
fn drive_bytes<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlInput<'inp, 'c, [u8], SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  input: &'inp ::bytes::Bytes,
) -> Result<O, GraphqlErrors<&'inp [u8]>> {
  Parser::with_parser::<'inp, GraphqlLexer<'inp, [u8]>, O, GraphqlErrors<&'inp [u8]>, _, GraphQL>(f)
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

// ─── directive ───────────────────────────────────────────────────────────────

#[test]
fn directive_accepts_without_arguments() {
  fn check<S: AsRef<[u8]>>(d: AstDirective<S>) {
    assert!("deprecated".equivalent(d.name().source()));
    assert!(d.arguments().is_none());
    assert_eq!(d.span().start(), 0);
  }
  accept_all!(Directive::<_>::graphql, "@deprecated", check);
}

#[test]
fn directive_accepts_with_arguments() {
  fn check<S: AsRef<[u8]>>(d: AstDirective<S>) {
    assert!("include".equivalent(d.name().source()));
    let args = d.arguments().expect("present");
    assert_eq!(args.arguments().len(), 1);
    assert!("if".equivalent(args.arguments()[0].name().source()));
  }
  accept_all!(Directive::<_>::graphql, "@include(if: true)", check);
}

#[test]
fn directive_accepts_empty_arguments_as_none() {
  fn check<S: AsRef<[u8]>>(d: AstDirective<S>) {
    assert!(d.arguments().is_none());
  }
  accept_all!(Directive::<_>::graphql, "@d()", check);
}

#[test]
fn directive_rejects_missing_at() {
  reject_all!(Directive::<_>::graphql, "deprecated");
}

#[test]
fn directive_rejects_missing_name() {
  reject_all!(Directive::<_>::graphql, "@");
  reject_all!(Directive::<_>::graphql, "@(x: 1)");
}

#[test]
fn directive_phase_diagnostics_are_typed_and_leave_wrong_tokens() {
  for (src, expected, found) in [
    (
      "deprecated",
      Expectation::At,
      SyntacticTokenKind::Identifier,
    ),
    ("@(", Expectation::Name, SyntacticTokenKind::LParen),
  ] {
    let (diagnostic_matches, leftover_kind) = drive_str(
      |inp| {
        let error = Directive::<_>::graphql(inp)
          .expect_err("malformed directive should fail")
          .into_iter()
          .next()
          .expect("malformed directive should emit an error");
        let diagnostic_matches = matches!(
          error.into_data(),
          ErrorData::UnexpectedToken(unexpected)
            if unexpected.expected() == &expected && unexpected.found() == Some(&found)
        );
        let leftover_kind = inp.next()?.map(|token| token.data().kind());
        Ok::<_, GraphqlErrors<&str>>((diagnostic_matches, leftover_kind))
      },
      src,
    )
    .expect("the rejected token should remain readable");
    assert!(diagnostic_matches);
    assert_eq!(leftover_kind, Some(found));
  }

  let error = drive_str(|inp| Directive::<_>::graphql(inp).map(|_| ()), "@")
    .expect_err("a directive name is required")
    .into_iter()
    .next()
    .expect("missing directive name should emit an error");
  assert!(matches!(
    error.into_data(),
    ErrorData::UnexpectedToken(unexpected)
      if unexpected.expected() == &Expectation::Name && unexpected.found().is_none()
  ));
}

#[test]
fn directive_rejects_malformed_arguments() {
  reject_all!(Directive::<_>::graphql, "@d(x 1)");
  reject_all!(Directive::<_>::graphql, "@d(x: 1");
}

// ─── const_directive ─────────────────────────────────────────────────────────

#[test]
fn const_directive_accepts_and_rejects_variable() {
  fn check<S: AsRef<[u8]>>(d: AstConstDirective<S>) {
    assert!("d".equivalent(d.name().source()));
  }
  accept_all!(ConstDirective::<_>::graphql, "@d(x: 1)", check);
  reject_all!(ConstDirective::<_>::graphql, "@d(x: $v)");
}

// ─── directives ──────────────────────────────────────────────────────────────

#[test]
fn directives_accepts_single() {
  fn check<S: AsRef<[u8]>>(ds: AstDirectives<S>) {
    assert_eq!(ds.directives().len(), 1);
    assert!("deprecated".equivalent(ds.directives()[0].name().source()));
  }
  accept_all!(Directives::<_>::graphql, "@deprecated", check);
}

#[test]
fn directives_accepts_multiple() {
  fn check<S: AsRef<[u8]>>(ds: AstDirectives<S>) {
    assert_eq!(ds.directives().len(), 2);
    assert!("a".equivalent(ds.directives()[0].name().source()));
    assert!("b".equivalent(ds.directives()[1].name().source()));
  }
  accept_all!(Directives::<_>::graphql, "@a @b", check);
}

#[test]
fn directives_absent_is_empty_zero_width_and_non_consuming() {
  // No tokens consumed: a following production sees the identifier untouched.
  let ok = drive_str(
    |inp| {
      let ds = Directives::<_>::graphql(inp)?;
      let leftover = crate::parser::combinator::ident(inp)?;
      Ok::<_, GraphqlErrors<&str>>(
        ds.directives().is_empty()
          && *ds.span() == SimpleSpan::new(0, 0)
          && *leftover.source_ref() == "x",
      )
    },
    "x",
  )
  .unwrap();
  assert!(ok);
}

#[test]
fn directives_absent_on_empty_input() {
  let str_directives = drive_str(Directives::<_>::graphql, "").unwrap();
  assert!(str_directives.directives().is_empty());
  assert_eq!(*str_directives.span(), SimpleSpan::new(0, 0));

  let slice_directives = drive_slice(Directives::<_>::graphql, b"").unwrap();
  assert!(slice_directives.directives().is_empty());
  assert_eq!(*slice_directives.span(), SimpleSpan::new(0, 0));
}

#[test]
fn directives_rejects_malformed_directive_mid_run() {
  // The first directive commits the `@`; a malformed follow-on directive is an
  // error, not a decline back to a shorter accepted run.
  reject_all!(Directives::<_>::graphql, "@a @");
  let error = drive_str(|inp| Directives::<_>::graphql(inp).map(|_| ()), "@a @")
    .expect_err("a later directive head commits")
    .into_iter()
    .next()
    .expect("the malformed directive should emit an error");
  assert!(matches!(
    error.into_data(),
    ErrorData::UnexpectedToken(unexpected)
      if unexpected.expected() == &Expectation::Name && unexpected.found().is_none()
  ));
}

// ─── const_directives ────────────────────────────────────────────────────────

#[test]
fn const_directives_accepts_and_rejects_variable() {
  fn check<S: AsRef<[u8]>>(ds: AstConstDirectives<S>) {
    assert_eq!(ds.directives().len(), 1);
  }
  accept_all!(ConstDirectives::<_>::graphql, "@d(x: 1)", check);
  reject_all!(ConstDirectives::<_>::graphql, "@d(x: $v)");
}

#[test]
fn const_directives_absent_is_empty_and_zero_width() {
  let directives = drive_str(ConstDirectives::<_>::graphql, "").unwrap();
  assert!(directives.directives().is_empty());
  assert_eq!(*directives.span(), SimpleSpan::new(0, 0));
}

// ─── frozen-parity oracle (table-driven) ─────────────────────────────────────

/// Accept/reject verdicts the frozen `smear-parser` `parse_directive` produces for
/// the same inputs.
const DIRECTIVE_ORACLE: &[(&str, bool)] = &[
  ("@deprecated", true),
  ("@include(if: true)", true),
  ("@d(a: 1, b: 2)", true),
  ("deprecated", false),
  ("@", false),
  ("@d(x 1)", false),
  ("", false),
];

#[test]
fn directive_matches_frozen_verdicts() {
  for (src, accept) in DIRECTIVE_ORACLE {
    assert_eq!(
      drive_str(|inp| Directive::<_>::graphql(inp).map(|_| ()), src).is_ok(),
      *accept,
      "str directive({src:?})"
    );
    assert_eq!(
      drive_slice(
        |inp| Directive::<_>::graphql(inp).map(|_| ()),
        src.as_bytes()
      )
      .is_ok(),
      *accept,
      "slice directive({src:?})"
    );
  }
}

/// Accept/reject verdicts for `directives` (a missing `@` yields an empty
/// collection rather than erroring, so it is not itself a "reject" row here — see
/// `directives_absent_is_empty_zero_width_and_non_consuming`).
const DIRECTIVES_ORACLE: &[(&str, bool)] = &[
  ("@a", true),
  ("@a @b", true),
  ("@a(x: 1) @b", true),
  ("@a @", false),
];

#[test]
fn directives_matches_frozen_verdicts() {
  for (src, accept) in DIRECTIVES_ORACLE {
    assert_eq!(
      drive_str(|inp| Directives::<_>::graphql(inp).map(|_| ()), src).is_ok(),
      *accept,
      "str directives({src:?})"
    );
    assert_eq!(
      drive_slice(
        |inp| Directives::<_>::graphql(inp).map(|_| ()),
        src.as_bytes()
      )
      .is_ok(),
      *accept,
      "slice directives({src:?})"
    );
  }
}
