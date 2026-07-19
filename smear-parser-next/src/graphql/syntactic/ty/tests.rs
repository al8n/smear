//! `Type` production tests.
//!
//! Every case is driven end to end over the real GraphQL syntactic lexer under a
//! `Fatal<GraphqlErrors>` context, matching `value`'s test harness. Accept cases run
//! the full source matrix (`str`, `[u8]`, and `Bytes` behind the feature); reject
//! cases assert the error family; and a table-driven oracle pins the accept/reject
//! verdicts the frozen `smear-parser` crate's `parse_type` produces for the same
//! inputs.

use smear_lexer::graphql::syntactic::SyntacticLexer;
use tokora::{FatalContext, InputRef, Parse, Parser, SimpleSpan, utils::cmp::Equivalent};

use super::ty;
use crate::graphql::{
  ast::{Name, Type},
  error::GraphqlErrors,
};

/// The fatal context a `str`-sourced parse runs under.
type StrCtx<'inp> = FatalContext<'inp, SyntacticLexer<'inp, str>, GraphqlErrors<&'inp str>>;
/// The fatal context a `[u8]`-sourced parse runs under.
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

#[cfg(feature = "bytes")]
fn drive_bytes<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, [u8]>, SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  input: &'inp ::bytes::Bytes,
) -> Result<O, GraphqlErrors<&'inp [u8]>> {
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

// ─── NamedType ────────────────────────────────────────────────────────────────

#[test]
fn named_type_accepts() {
  fn check<S: AsRef<[u8]>>(t: Type<Name<S>>) {
    let nt = t.unwrap_name();
    assert!("Foo".equivalent(nt.name().source_ref()));
    assert!(!nt.required());
    assert_eq!(*nt.span(), SimpleSpan::new(0, 3));
  }
  accept_all!(ty, "Foo", check);
}

#[test]
fn named_type_non_null_accepts() {
  fn check<S: AsRef<[u8]>>(t: Type<Name<S>>) {
    let nt = t.unwrap_name();
    assert!("Foo".equivalent(nt.name().source_ref()));
    assert!(nt.required());
    assert_eq!(*nt.span(), SimpleSpan::new(0, 4));
  }
  accept_all!(ty, "Foo!", check);
}

#[test]
fn named_type_accepts_soft_keyword_spellings() {
  // Type names are pure-lexical `Name`s; no keyword exclusion applies (Ruling 1).
  fn check<S: AsRef<[u8]>>(t: Type<Name<S>>) {
    assert!(t.is_name());
  }
  accept_all!(ty, "type", check);
  accept_all!(ty, "enum", check);
  accept_all!(ty, "true", check);
}

// ─── ListType ────────────────────────────────────────────────────────────────

#[test]
fn list_type_accepts() {
  fn check<S: AsRef<[u8]>>(t: Type<Name<S>>) {
    let lt = t.unwrap_list();
    assert!(!lt.required());
    assert_eq!(*lt.span(), SimpleSpan::new(0, 5));
    let inner = lt.ty().unwrap_name_ref();
    assert!("Foo".equivalent(inner.name().source_ref()));
    assert!(!inner.required());
  }
  accept_all!(ty, "[Foo]", check);
}

#[test]
fn list_type_non_null_accepts() {
  fn check<S: AsRef<[u8]>>(t: Type<Name<S>>) {
    let lt = t.unwrap_list();
    assert!(lt.required());
    assert_eq!(*lt.span(), SimpleSpan::new(0, 6));
  }
  accept_all!(ty, "[Foo]!", check);
}

#[test]
fn list_type_of_non_null_named_type() {
  fn check<S: AsRef<[u8]>>(t: Type<Name<S>>) {
    let lt = t.unwrap_list();
    assert!(!lt.required());
    let inner = lt.ty().unwrap_name_ref();
    assert!(inner.required());
  }
  accept_all!(ty, "[Foo!]", check);
}

#[test]
fn list_type_of_non_null_named_type_non_null_list() {
  // `[Foo!]!` — outer list is non-null, inner element is also non-null.
  fn check<S: AsRef<[u8]>>(t: Type<Name<S>>) {
    let lt = t.unwrap_list();
    assert!(lt.required());
    let inner = lt.ty().unwrap_name_ref();
    assert!(inner.required());
  }
  accept_all!(ty, "[Foo!]!", check);
}

#[test]
fn doubly_nested_list_type() {
  fn check<S: AsRef<[u8]>>(t: Type<Name<S>>) {
    let outer = t.unwrap_list();
    let middle = outer.ty().unwrap_list_ref();
    let inner = middle.ty().unwrap_name_ref();
    assert!("Foo".equivalent(inner.name().source_ref()));
  }
  accept_all!(ty, "[[Foo]]", check);
}

#[test]
fn doubly_nested_list_type_with_inner_non_nulls() {
  fn check<S: AsRef<[u8]>>(t: Type<Name<S>>) {
    // `[[Foo!]!]!`
    let outer = t.unwrap_list();
    assert!(outer.required());
    let middle = outer.ty().unwrap_list_ref();
    assert!(middle.required());
    let inner = middle.ty().unwrap_name_ref();
    assert!(inner.required());
  }
  accept_all!(ty, "[[Foo!]!]!", check);
}

// ─── reject rows + error families ────────────────────────────────────────────

#[test]
fn ty_rejects_non_type_heads() {
  reject_all!(ty, "42");
  reject_all!(ty, "\"s\"");
  reject_all!(ty, "!");
  reject_all!(ty, "");
  reject_all!(ty, "$x");
  reject_all!(ty, "@");
}

#[test]
fn ty_rejects_unterminated_list() {
  reject_all!(ty, "[Foo");
  reject_all!(ty, "[Foo!");
}

#[test]
fn ty_rejects_empty_brackets() {
  // `ListType : [ Type ]` — the element is required, not `Type*`.
  reject_all!(ty, "[]");
}

#[test]
fn ty_unexpected_token_error_family() {
  let is_unexpected = |src: &str| match drive_str(|inp| ty(inp).map(|_| ()), src) {
    Err(errs) => errs
      .into_iter()
      .next()
      .is_some_and(|e| e.data().is_unexpected_token()),
    Ok(()) => false,
  };
  assert!(is_unexpected("42"));
  assert!(is_unexpected("[]"));
}

#[test]
fn ty_end_of_input_error_family() {
  let is_eot = match drive_str(|inp| ty(inp).map(|_| ()), "") {
    Err(errs) => errs
      .into_iter()
      .next()
      .is_some_and(|e| e.data().is_end_of_input()),
    Ok(()) => false,
  };
  assert!(is_eot);
}

// ─── frozen-parity oracle (table-driven) ─────────────────────────────────────

/// Accept/reject verdicts the frozen `smear-parser` `parse_type` produces for the
/// same inputs. `smear-parser` is not a dependency here, so the verdicts are pinned
/// as a table (the spec/frozen behaviour is the arbiter). Deviations from frozen
/// would appear only via the Deviations Register — there are none for type refs.
const TYPE_ORACLE: &[(&str, bool)] = &[
  ("Foo", true),
  ("Foo!", true),
  ("[Foo]", true),
  ("[Foo]!", true),
  ("[Foo!]", true),
  ("[Foo!]!", true),
  ("[[Foo]]", true),
  ("[[Foo!]!]!", true),
  ("42", false),
  ("\"s\"", false),
  ("true", true),
  ("!", false),
  ("", false),
  ("[Foo", false),
  ("[]", false),
  ("$x", false),
];

#[test]
fn ty_matches_frozen_verdicts() {
  for (src, accept) in TYPE_ORACLE {
    assert_eq!(
      drive_str(|inp| ty(inp).map(|_| ()), src).is_ok(),
      *accept,
      "str ty({src:?})"
    );
    assert_eq!(
      drive_slice(|inp| ty(inp).map(|_| ()), src.as_bytes()).is_ok(),
      *accept,
      "slice ty({src:?})"
    );
  }
}
