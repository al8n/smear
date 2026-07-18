//! GraphQLx `Directive`/`Directives` production tests.
//!
//! Every case is driven end to end over the real GraphQLx syntactic lexer under a
//! `Fatal<GraphqlxErrors>` context, matching `ty`'s harness. Accept cases run the
//! full source matrix (`str`, `[u8]`, and `Bytes` behind the feature); reject
//! cases assert the error family.

use smear_lexer::graphqlx::syntactic::SyntacticLexer;
use tokora::{FatalContext, InputRef, Parse, Parser};

use super::{const_directives, directive, directives};
use crate::graphqlx::error::GraphqlxErrors;

/// The fatal context a `str`-sourced parse runs under.
type StrCtx<'inp> = FatalContext<'inp, SyntacticLexer<'inp, str>, GraphqlxErrors<&'inp str>>;
/// The fatal context a `[u8]`-sourced parse runs under.
type SliceCtx<'inp> = FatalContext<'inp, SyntacticLexer<'inp, [u8]>, GraphqlxErrors<&'inp [u8]>>;

/// Drives `f` over a `str` source under `Fatal<GraphqlxErrors<&str>>`.
fn drive_str<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, str>, StrCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<&'inp str>>,
  input: &'inp str,
) -> Result<O, GraphqlxErrors<&'inp str>> {
  Parser::with_parser(f).parse_str(input)
}

/// Drives `f` over a `[u8]` source under `Fatal<GraphqlxErrors<&[u8]>>`.
fn drive_slice<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, [u8]>, SliceCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<&'inp [u8]>>,
  input: &'inp [u8],
) -> Result<O, GraphqlxErrors<&'inp [u8]>> {
  Parser::with_parser(f).parse_slice(input)
}

#[cfg(feature = "bytes")]
fn drive_bytes<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, [u8]>, SliceCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<&'inp [u8]>>,
  input: &'inp ::bytes::Bytes,
) -> Result<O, GraphqlxErrors<&'inp [u8]>> {
  Parser::with_parser(f).parse_bytes(input)
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

/// Views a slice (`&str` or `&[u8]`) as bytes, so one assertion body reads across
/// every source representation.
fn bytes<S: AsRef<[u8]>>(slice: &S) -> &[u8] {
  slice.as_ref()
}

// ─── directive ───────────────────────────────────────────────────────────────

#[test]
fn directive_bare_and_with_arguments() {
  fn bare<S: AsRef<[u8]>>(d: crate::graphqlx::ast::Directive<S>) {
    assert_eq!(bytes(d.name().source_ref()), b"deprecated");
    assert!(d.arguments().is_none());
  }
  accept_all!(directive, "@deprecated", bare);

  fn with_args<S: AsRef<[u8]>>(d: crate::graphqlx::ast::Directive<S>) {
    assert_eq!(bytes(d.name().source_ref()), b"skip");
    let args = d.arguments().expect("arguments present");
    assert_eq!(args.arguments().len(), 1);
  }
  accept_all!(directive, "@skip(if: $flag)", with_args);
}

#[test]
fn directive_rejects_missing_at_or_name() {
  reject_all!(directive, "skip");
  reject_all!(directive, "@");
  // The empty argument list breaches `( Argument+ )` (Amendment 2).
  reject_all!(directive, "@skip()");
}

// ─── directives / const_directives ───────────────────────────────────────────

#[test]
fn directives_greedy_run_and_decline() {
  fn two<S: AsRef<[u8]>>(d: Option<crate::graphqlx::ast::Directives<S>>) {
    let d = d.expect("directives present");
    assert_eq!(d.directives().len(), 2);
  }
  accept_all!(directives, "@a @b(x: 0b101)", two);

  // No `@` ahead: decline to `None` without consuming.
  let out = drive_str(
    |inp| {
      let ds = directives(inp)?;
      assert!(ds.is_none());
      crate::combinator::ident(inp).map(|id| *id.source_ref())
    },
    "plain",
  );
  assert_eq!(out.ok(), Some("plain"));
}

#[test]
fn const_directives_reject_variable_arguments() {
  fn one<S: AsRef<[u8]>>(d: Option<crate::graphqlx::ast::ConstDirectives<S>>) {
    assert_eq!(d.expect("directives present").directives().len(), 1);
  }
  accept_all!(const_directives, "@limit(max: 10)", one);
  reject_all!(const_directives, "@limit(max: $n)");
}
