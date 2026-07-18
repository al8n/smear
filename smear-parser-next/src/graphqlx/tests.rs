//! The GraphQLx dialect's matrix test harness and an end-to-end smoke test.
//!
//! Mirrors `combinator/tests.rs` — drive a polymorphic parser closure over every
//! source representation — but binds the real dialect error
//! ([`GraphqlxErrors`](super::error::GraphqlxErrors)), so it is the harness the
//! Wave 7+ production tests reuse. The smoke tests drive the shared atoms
//! ([`ident`](crate::combinator::ident),
//! [`keyword_exact`](crate::combinator::keyword_exact)) and the GraphQLx-only
//! punctuator atoms ([`path_sep`](crate::combinator::path_sep),
//! [`asterisk`](crate::combinator::asterisk),
//! [`fat_arrow`](crate::combinator::fat_arrow)) end to end through a
//! `Fatal<GraphqlxErrors>` context over both `str` and `[u8]`, proving the
//! substrate — atoms, the GraphQLx keyword/punctuator lexer capabilities, the
//! dialect error, and its `From` glue — hangs together.
//!
//! The `Lang` marker is `GraphQLx` at the type level ([`error`](super::error)'s
//! `ParseCtx` compile test pins that); the runners here take the default marker
//! because tokora's `Parser::with_parser*` constructors bind the driving closure's
//! `ParseInput` at `Lang = ()`. The marker is inert at runtime, so the atoms behave
//! identically — the entry runner (a later wave) owns marker-pinned driving.

use smear_lexer::graphqlx::syntactic::SyntacticLexer;
use tokora::{FatalContext, InputRef, Parse, Parser};

use super::error::GraphqlxErrors;
use crate::combinator::{asterisk, fat_arrow, ident, keyword_exact, path_sep};

/// The fatal context a `str`-sourced GraphQLx parse runs under.
type StrCtx<'inp> = FatalContext<'inp, SyntacticLexer<'inp, str>, GraphqlxErrors<&'inp str>>;
/// The fatal context a `[u8]`-sourced GraphQLx parse runs under.
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
  // A shared GraphQL keyword matches its spelling …
  assert!(drive_str(|inp| keyword_exact(inp, "query").map(|_| ()), "query").is_ok());
  assert!(drive_slice(|inp| keyword_exact(inp, "query").map(|_| ()), b"query").is_ok());

  // … and errors on any other spelling, through the dialect error.
  assert!(drive_str(|inp| keyword_exact(inp, "query").map(|_| ()), "mutation").is_err());
  assert!(drive_slice(|inp| keyword_exact(inp, "query").map(|_| ()), b"mutation").is_err());
}

#[test]
fn graphqlx_only_keywords_are_recognized() {
  // The GraphQLx-only keywords the lexer's `KeywordToken` surface adds
  // (`import`/`from`/`as`/`where`/`set`/`map`) match through `keyword_exact` …
  for kw in ["import", "from", "as", "where", "set", "map"] {
    assert!(
      drive_str(move |inp| keyword_exact(inp, kw).map(|_| ()), kw).is_ok(),
      "GraphQLx keyword `{kw}` should match itself"
    );
  }

  // … and a GraphQLx-only keyword atom declines a plain identifier that is not
  // that keyword.
  assert!(drive_str(|inp| keyword_exact(inp, "import").map(|_| ()), "imported").is_err());
}

#[test]
fn graphqlx_punctuators_commit_over_str_and_slice() {
  // The GraphQLx-only punctuator atoms drive through the dialect context — the
  // lexer's `PunctuatorToken` capability plus the new `punct_atoms!` entries.
  assert!(drive_str(|inp| path_sep(inp).map(|_| ()), "::").is_ok());
  assert!(drive_slice(|inp| path_sep(inp).map(|_| ()), b"::").is_ok());

  assert!(drive_str(|inp| asterisk(inp).map(|_| ()), "*").is_ok());
  assert!(drive_str(|inp| fat_arrow(inp).map(|_| ()), "=>").is_ok());

  // … and error on the wrong punctuator, through the dialect error.
  assert!(drive_str(|inp| path_sep(inp).map(|_| ()), "@").is_err());
  assert!(drive_slice(|inp| asterisk(inp).map(|_| ()), b"@").is_err());
}
