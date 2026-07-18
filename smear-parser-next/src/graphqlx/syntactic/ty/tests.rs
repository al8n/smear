//! GraphQLx `Type` production tests.
//!
//! Every case is driven end to end over the real GraphQLx syntactic lexer under a
//! `Fatal<GraphqlxErrors>` context, matching `value`'s harness. Accept cases run the
//! full source matrix (`str`, `[u8]`, and `Bytes` behind the feature); reject cases
//! assert the error family; and a table-driven oracle pins the accept/reject verdicts
//! the GraphQLx grammar demands. The frozen `smear-parser` type parser no longer
//! exists, so the `ok_*` corpus fixtures and the scaffold node shapes are the arbiter
//! (plan Wave 7).

use smear_lexer::graphqlx::syntactic::SyntacticLexer;
use tokora::{FatalContext, InputRef, Parse, Parser, SimpleSpan};

use super::{path, ty};
use crate::graphqlx::{ast::Type, error::GraphqlxErrors};

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

// ─── path (standalone) ───────────────────────────────────────────────────────

#[test]
fn path_single_and_multi_segment() {
  fn single<S: AsRef<[u8]>>(p: crate::graphqlx::ast::Path<S>) {
    assert!(!p.is_fully_qualified());
    assert_eq!(p.segments_slice().len(), 1);
    assert_eq!(bytes(p.segments_slice()[0].source_ref()), b"Foo");
  }
  accept_all!(path, "Foo", single);
  fn multi<S: AsRef<[u8]>>(p: crate::graphqlx::ast::Path<S>) {
    let segs = p.segments_slice();
    assert_eq!(segs.len(), 2);
    assert_eq!(bytes(segs[0].source_ref()), b"user");
    assert_eq!(bytes(segs[1].source_ref()), b"Profile");
  }
  accept_all!(path, "user::Profile", multi);
}

// ─── TypePath ────────────────────────────────────────────────────────────────

#[test]
fn type_path_plain() {
  fn check<S: AsRef<[u8]>>(t: Type<S>) {
    let p = t.unwrap_path();
    assert!(!p.required());
    assert!(p.type_generics().is_none());
    assert_eq!(p.path().segments_slice().len(), 1);
    assert_eq!(bytes(p.path().segments_slice()[0].source_ref()), b"Foo");
  }
  accept_all!(ty, "Foo", check);
}

#[test]
fn type_path_non_null() {
  fn check<S: AsRef<[u8]>>(t: Type<S>) {
    assert!(t.unwrap_path().required());
  }
  accept_all!(ty, "Foo!", check);
}

#[test]
fn type_path_multi_segment() {
  fn check<S: AsRef<[u8]>>(t: Type<S>) {
    let p = t.unwrap_path();
    assert_eq!(p.path().segments_slice().len(), 2);
  }
  accept_all!(ty, "user::Profile", check);
}

#[test]
fn type_path_soft_keyword_names() {
  // Type names are pure-lexical; the soft keywords are valid path segments (Ruling 1).
  fn check<S: AsRef<[u8]>>(t: Type<S>) {
    assert!(t.is_path());
  }
  accept_all!(ty, "set", check);
  accept_all!(ty, "map", check);
  accept_all!(ty, "type", check);
}

#[test]
fn type_path_with_generics() {
  fn check<S: AsRef<[u8]>>(t: Type<S>) {
    let p = t.unwrap_path();
    assert!(p.required());
    let g = p.type_generics().unwrap();
    assert_eq!(g.params_slice().len(), 1);
    let arg = g.params_slice()[0].unwrap_path_ref();
    assert!(arg.required());
    assert_eq!(
      bytes(arg.path().segments_slice()[0].source_ref()),
      b"String"
    );
  }
  accept_all!(ty, "Container<String!>!", check);
}

#[test]
fn type_path_with_multiple_generics() {
  fn check<S: AsRef<[u8]>>(t: Type<S>) {
    let p = t.unwrap_path();
    let g = p.type_generics().unwrap();
    assert_eq!(g.params_slice().len(), 2);
  }
  accept_all!(ty, "Pair<K, V>", check);
  accept_all!(ty, "Pair<K V>", check); // commas insignificant
}

#[test]
fn type_path_nested_generics() {
  fn check<S: AsRef<[u8]>>(t: Type<S>) {
    let outer = t.unwrap_path();
    let inner_arg = outer.type_generics().unwrap().params_slice()[0].unwrap_path_ref();
    assert_eq!(
      bytes(inner_arg.path().segments_slice()[0].source_ref()),
      b"Box"
    );
    assert_eq!(inner_arg.type_generics().unwrap().params_slice().len(), 1);
  }
  accept_all!(ty, "Container<Box<T>>", check);
}

#[test]
fn type_path_with_path_segment_generics() {
  fn check<S: AsRef<[u8]>>(t: Type<S>) {
    let p = t.unwrap_path();
    assert_eq!(p.path().segments_slice().len(), 2);
    assert_eq!(p.type_generics().unwrap().params_slice().len(), 1);
  }
  accept_all!(ty, "user::Profile<I>", check);
}

// ─── ListType ────────────────────────────────────────────────────────────────

#[test]
fn list_type() {
  fn check<S: AsRef<[u8]>>(t: Type<S>) {
    let l = t.unwrap_list();
    assert!(!l.required());
    assert!(l.ty().is_path());
  }
  accept_all!(ty, "[Foo]", check);
}

#[test]
fn list_type_nullability_combinations() {
  fn outer_required<S: AsRef<[u8]>>(t: Type<S>) {
    let l = t.unwrap_list();
    assert!(l.required());
    assert!(!l.ty().unwrap_path_ref().required());
  }
  accept_all!(ty, "[Foo]!", outer_required);
  fn inner_required<S: AsRef<[u8]>>(t: Type<S>) {
    let l = t.unwrap_list();
    assert!(!l.required());
    assert!(l.ty().unwrap_path_ref().required());
  }
  accept_all!(ty, "[Foo!]", inner_required);
  fn both<S: AsRef<[u8]>>(t: Type<S>) {
    let l = t.unwrap_list();
    assert!(l.required());
    assert!(l.ty().unwrap_path_ref().required());
  }
  accept_all!(ty, "[Foo!]!", both);
}

#[test]
fn nested_list_type() {
  fn check<S: AsRef<[u8]>>(t: Type<S>) {
    let outer = t.unwrap_list();
    let inner = outer.ty().unwrap_list_ref();
    assert!(inner.ty().is_path());
  }
  accept_all!(ty, "[[Foo]]", check);
}

// ─── SetType ─────────────────────────────────────────────────────────────────

#[test]
fn set_type() {
  fn check<S: AsRef<[u8]>>(t: Type<S>) {
    let s = t.unwrap_set();
    assert!(!s.required());
    assert!(s.ty().is_path());
  }
  accept_all!(ty, "<Foo>", check);
}

#[test]
fn set_type_nullability() {
  fn check<S: AsRef<[u8]>>(t: Type<S>) {
    let s = t.unwrap_set();
    assert!(s.required());
    assert!(s.ty().unwrap_path_ref().required());
  }
  accept_all!(ty, "<Foo!>!", check);
}

#[test]
fn nested_set_type() {
  fn check<S: AsRef<[u8]>>(t: Type<S>) {
    let outer = t.unwrap_set();
    assert!(outer.ty().unwrap_set_ref().ty().is_path());
  }
  accept_all!(ty, "<<Foo>>", check);
}

// ─── MapType ─────────────────────────────────────────────────────────────────

#[test]
fn map_type() {
  fn check<S: AsRef<[u8]>>(t: Type<S>) {
    let m = t.unwrap_map();
    assert!(!m.required());
    assert_eq!(
      bytes(m.key().unwrap_path_ref().path().segments_slice()[0].source_ref()),
      b"String"
    );
    assert!(m.value().is_path());
  }
  accept_all!(ty, "<String! => String!>", check);
}

#[test]
fn map_type_non_null() {
  fn check<S: AsRef<[u8]>>(t: Type<S>) {
    let m = t.unwrap_map();
    assert!(m.required());
    assert!(m.key().unwrap_path_ref().required());
    assert!(m.value().unwrap_path_ref().required());
  }
  accept_all!(ty, "<String! => String!>!", check);
}

#[test]
fn nested_map_type() {
  // Fixture 0009: `<String! => <String! => String!>!>!`.
  fn check<S: AsRef<[u8]>>(t: Type<S>) {
    let m = t.unwrap_map();
    assert!(m.required());
    let inner = m.value().unwrap_map_ref();
    assert!(inner.required());
    assert!(inner.value().unwrap_path_ref().required());
  }
  accept_all!(ty, "<String! => <String! => String!>!>!", check);
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
fn ty_rejects_malformed_delimited() {
  reject_all!(ty, "[Foo"); // unterminated list
  reject_all!(ty, "[]"); // empty list element
  reject_all!(ty, "<>"); // empty angle
  reject_all!(ty, "<K =>"); // map missing value
  reject_all!(ty, "Container<>"); // empty generics
  reject_all!(ty, "Container<String"); // unterminated generics
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

// ─── grammar oracle (table-driven; fixtures are the arbiter) ──────────────────

/// Accept/reject verdicts the GraphQLx type grammar demands (the `ok_*` corpus
/// fixtures + scaffold node shapes are the arbiter — the frozen type parser no longer
/// exists). No Deviations Register entry touches type refs.
const TYPE_ORACLE: &[(&str, bool)] = &[
  ("Foo", true),
  ("Foo!", true),
  ("user::Profile", true),
  ("Container<String!>!", true),
  ("Pair<K, V>", true),
  ("Container<Box<T>>", true),
  ("[Foo]", true),
  ("[Foo]!", true),
  ("[[Foo!]!]!", true),
  ("<Foo>", true),
  ("<Foo!>!", true),
  ("<String! => String!>", true),
  ("<String! => <String! => String!>!>!", true),
  ("42", false),
  ("\"s\"", false),
  ("!", false),
  ("", false),
  ("[Foo", false),
  ("[]", false),
  ("<>", false),
  ("<K =>", false),
  ("Container<>", false),
  ("$x", false),
];

#[test]
fn ty_matches_grammar_verdicts() {
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

// The unused `SimpleSpan` import guard: span assertions live in `value`'s suite; this
// module asserts structure, so a single span check keeps the import honest.
#[test]
fn ty_span_covers_bang() {
  fn check<S: AsRef<[u8]>>(t: Type<S>) {
    let p = t.unwrap_path();
    assert_eq!(*p.span(), SimpleSpan::new(0, 4));
  }
  accept_all!(ty, "Foo!", check);
}
