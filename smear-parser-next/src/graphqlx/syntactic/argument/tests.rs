//! GraphQLx `Argument`/`Arguments` production tests.
//!
//! Every case is driven end to end over the real GraphQLx syntactic lexer under a
//! `Fatal<GraphqlxErrors>` context, matching `ty`'s harness. Accept cases run the
//! full source matrix (`str`, `[u8]`, and `Bytes` behind the feature); reject
//! cases assert the error family. The GraphQLx twist under test is the value
//! family: radix-preserving literals, `set`/`map` composites, and `::`-path enums
//! flow through the shared argument shape.

use smear_lexer::graphqlx::syntactic::SyntacticLexer;
use tokora::{FatalContext, InputRef, Parse, Parser, utils::cmp::Equivalent};

use super::{argument, arguments, const_argument, const_arguments};
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

// ─── argument / const_argument ───────────────────────────────────────────────

#[test]
fn argument_int_and_variable_values() {
  fn int<S: AsRef<[u8]>>(a: crate::graphqlx::ast::Argument<S>) {
    assert!("answer".equivalent(a.name().source_ref()));
    assert!(a.value().is_int());
  }
  accept_all!(argument, "answer: 0x2A", int);

  fn var<S: AsRef<[u8]>>(a: crate::graphqlx::ast::Argument<S>) {
    assert!(a.value().is_variable());
  }
  accept_all!(argument, "id: $userId", var);
}

#[test]
fn argument_graphqlx_composite_values() {
  // The GraphQLx value family flows through the shared argument shape.
  fn set<S: AsRef<[u8]>>(a: crate::graphqlx::ast::Argument<S>) {
    assert!(a.value().is_set());
  }
  accept_all!(argument, "tags: set { A B }", set);

  fn path<S: AsRef<[u8]>>(a: crate::graphqlx::ast::Argument<S>) {
    assert!(a.value().is_enum());
  }
  accept_all!(argument, "status: ::state::Active", path);
}

#[test]
fn argument_rejects_missing_colon_or_value() {
  reject_all!(argument, "answer 42");
  reject_all!(argument, "answer:");
}

#[test]
fn const_argument_rejects_variables() {
  // The const twin threads `const_value`: a `$` head is not a const value.
  fn int<S: AsRef<[u8]>>(a: crate::graphqlx::ast::ConstArgument<S>) {
    assert!(a.value().is_int());
  }
  accept_all!(const_argument, "answer: 42", int);
  reject_all!(const_argument, "id: $userId");
}

// ─── arguments / const_arguments ─────────────────────────────────────────────

#[test]
fn arguments_list_and_decline() {
  fn two<S: AsRef<[u8]>>(a: Option<crate::graphqlx::ast::Arguments<S>>) {
    let a = a.expect("arguments present");
    assert_eq!(a.arguments().len(), 2);
  }
  accept_all!(arguments, "(a: 1, b: \"two\")", two);

  // No `(` ahead: decline without consuming — the next token stays readable.
  let out = drive_str(
    |inp| {
      let args = arguments(inp)?;
      assert!(args.is_none());
      crate::combinator::ident(inp).map(|id| *id.source_ref())
    },
    "plain",
  );
  assert_eq!(out.ok(), Some("plain"));
}

#[test]
fn arguments_empty_parens_reject_per_cardinality() {
  // Amendment 2 (the W2-retrofit site, carried to GraphQLx): `( Argument+ )`.
  reject_all!(arguments, "()");
  reject_all!(const_arguments, "()");
}

#[test]
fn const_arguments_reject_variable_values() {
  fn one<S: AsRef<[u8]>>(a: Option<crate::graphqlx::ast::ConstArguments<S>>) {
    assert_eq!(a.expect("arguments present").arguments().len(), 1);
  }
  accept_all!(const_arguments, "(limit: 10)", one);
  reject_all!(const_arguments, "(limit: $n)");
}
