//! The GraphQL dialect's matrix test harness and an end-to-end smoke test.
//!
//! Mirrors `combinator/tests.rs` — drive a polymorphic parser closure over every
//! source representation — but binds the real dialect error
//! ([`GraphqlErrors`](super::error::GraphqlErrors)), so it is the harness the
//! Wave 1+ production tests reuse. The smoke test drives two atoms
//! ([`ident`](crate::parser::combinator::ident) and
//! [`keyword_exact`](crate::parser::combinator::keyword_exact)) end to end through a
//! `Fatal<GraphqlErrors>` context over both `str` and `[u8]`, proving the
//! substrate — atoms, the dialect error, and its `From` glue — hangs together.
//!
//! The `Lang` marker is `GraphQL` at the type level ([`error`](super::error)'s
//! `ParseCtx` compile test pins that); the runners here take the default marker
//! because tokora's `Parser::with_parser*` constructors bind the driving closure's
//! `ParseInput` at `Lang = ()`. The marker is inert at runtime, so the atoms behave
//! identically — the entry runner (a later wave) owns marker-pinned driving.

use crate::lexer::graphql::syntactic::SyntacticLexer;
use tokora::{FatalContext, InputRef, Parse, Parser};

use super::error::GraphqlErrors;
use crate::parser::combinator::{ident, keyword_exact};

/// The fatal context a `str`-sourced GraphQL parse runs under.
type StrCtx<'inp> = FatalContext<'inp, SyntacticLexer<'inp, str>, GraphqlErrors<&'inp str>>;
/// The fatal context a `[u8]`-sourced GraphQL parse runs under.
type SliceCtx<'inp> = FatalContext<'inp, SyntacticLexer<'inp, [u8]>, GraphqlErrors<&'inp [u8]>>;

/// Drives `f` over a `str` source under `Fatal<GraphqlErrors<&str>>`.
fn drive_str<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, str>, StrCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp str>>,
  input: &'inp str,
) -> Result<O, GraphqlErrors<&'inp str>> {
  Parser::with_parser(f).parse_str(input)
}

/// Drives `f` over a `[u8]` source under `Fatal<GraphqlErrors<&[u8]>>`.
fn drive_slice<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, [u8]>, SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  input: &'inp [u8],
) -> Result<O, GraphqlErrors<&'inp [u8]>> {
  Parser::with_parser(f).parse_slice(input)
}

#[test]
fn ident_commits_over_str_and_slice() {
  // The `ident` atom drives end to end through the dialect context and yields the
  // matched source, equal across `str` and `[u8]` modulo the slice type.
  let out = drive_str(|inp| ident(inp).map(|id| *id.source_ref()), "hello");
  assert_eq!(out.ok(), Some("hello"));

  let out = drive_slice(|inp| ident(inp).map(|id| *id.source_ref()), b"hello");
  assert_eq!(out.ok(), Some(&b"hello"[..]));
}

#[test]
fn ident_errors_on_a_non_identifier() {
  // A committed atom on the wrong token produces the real dialect error, not a
  // panic — the `From<UnexpectedToken>` glue in action.
  assert!(drive_str(|inp| ident(inp).map(|_| ()), "{").is_err());
  assert!(drive_slice(|inp| ident(inp).map(|_| ()), b"{").is_err());
}

#[test]
fn keyword_exact_commits_and_errors_over_str_and_slice() {
  // The committed keyword atom matches its spelling …
  assert!(drive_str(|inp| keyword_exact(inp, "query").map(|_| ()), "query").is_ok());
  assert!(drive_slice(|inp| keyword_exact(inp, "query").map(|_| ()), b"query").is_ok());

  // … and errors on any other spelling, through the dialect error.
  assert!(drive_str(|inp| keyword_exact(inp, "query").map(|_| ()), "mutation").is_err());
  assert!(drive_slice(|inp| keyword_exact(inp, "query").map(|_| ()), b"mutation").is_err());
}
