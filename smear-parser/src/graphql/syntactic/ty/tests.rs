//! `Type` production tests.
//!
//! Every case is driven end to end over the real GraphQL syntactic lexer under a
//! `Fatal<GraphqlErrors>` context, matching `value`'s test harness. Accept cases run
//! the full source matrix (`str`, `[u8]`, and `Bytes` behind the feature); reject
//! cases assert the error family; and a table-driven oracle pins the accept/reject
//! verdicts the frozen `smear-parser` crate's `parse_type` produces for the same
//! inputs.

use smear_lexer::graphql::syntactic::SyntacticTokenKind;
use tokora::{
  FatalContext, Lexer, Parse, Parser, SimpleSpan, Source, punct::Bracket, utils::cmp::Equivalent,
};

use crate::graphql::{
  GraphQL,
  ast::{Name, Type, Type as AstType},
  error::{ErrorData, Expectation, GraphqlError as DialectGraphqlError, GraphqlErrors, Unclosed},
  syntactic::{GraphqlError, GraphqlInput, GraphqlLexer, GraphqlToken},
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

// ─── NamedType ────────────────────────────────────────────────────────────────

#[test]
fn named_type_accepts() {
  fn check<S: AsRef<[u8]>>(t: AstType<Name<S>>) {
    let nt = t.unwrap_name();
    assert!("Foo".equivalent(nt.name().source()));
    assert!(!nt.required());
    assert_eq!(*nt.span(), SimpleSpan::new(0, 3));
  }
  accept_all!(Type::graphql, "Foo", check);
}

#[test]
fn named_type_non_null_accepts() {
  fn check<S: AsRef<[u8]>>(t: AstType<Name<S>>) {
    let nt = t.unwrap_name();
    assert!("Foo".equivalent(nt.name().source()));
    assert!(nt.required());
    assert_eq!(*nt.span(), SimpleSpan::new(0, 4));
  }
  accept_all!(Type::graphql, "Foo!", check);
}

#[test]
fn named_type_accepts_soft_keyword_spellings() {
  // Type names are pure-lexical `Name`s; no keyword exclusion applies (Ruling 1).
  fn check<S: AsRef<[u8]>>(t: AstType<Name<S>>) {
    assert!(t.is_name());
  }
  accept_all!(Type::graphql, "type", check);
  accept_all!(Type::graphql, "enum", check);
  accept_all!(Type::graphql, "true", check);
}

#[test]
fn type_graphql_does_not_require_equivalent() {
  #[allow(dead_code)]
  fn parse_type_slice<'inp, Src, TypeSlice, Ctx>(
    inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  ) -> Result<AstType<Name<TypeSlice>>, GraphqlError<'inp, Src, Ctx>>
  where
    Src: Source<usize, Slice<'inp> = TypeSlice> + ?Sized,
    TypeSlice: tokora::Slice<'inp> + Clone + 'inp + crate::value::Leaf,
    GraphqlLexer<'inp, Src>:
      Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: crate::combinator::ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
    GraphqlError<'inp, Src, Ctx>: From<tokora::error::Unclosed<Bracket, SimpleSpan, GraphQL>>
      + From<DialectGraphqlError<TypeSlice>>,
  {
    Type::graphql(inp)
  }
}

// ─── ListType ────────────────────────────────────────────────────────────────

#[test]
fn list_type_accepts() {
  fn check<S: AsRef<[u8]>>(t: AstType<Name<S>>) {
    let lt = t.unwrap_list();
    assert!(!lt.required());
    assert_eq!(*lt.span(), SimpleSpan::new(0, 5));
    let inner = lt.ty().unwrap_name_ref();
    assert!("Foo".equivalent(inner.name().source()));
    assert!(!inner.required());
  }
  accept_all!(Type::graphql, "[Foo]", check);
}

#[test]
fn list_type_non_null_accepts() {
  fn check<S: AsRef<[u8]>>(t: AstType<Name<S>>) {
    let lt = t.unwrap_list();
    assert!(lt.required());
    assert_eq!(*lt.span(), SimpleSpan::new(0, 6));
  }
  accept_all!(Type::graphql, "[Foo]!", check);
}

#[test]
fn list_type_of_non_null_named_type() {
  fn check<S: AsRef<[u8]>>(t: AstType<Name<S>>) {
    let lt = t.unwrap_list();
    assert!(!lt.required());
    let inner = lt.ty().unwrap_name_ref();
    assert!(inner.required());
  }
  accept_all!(Type::graphql, "[Foo!]", check);
}

#[test]
fn list_type_of_non_null_named_type_non_null_list() {
  // `[Foo!]!` — outer list is non-null, inner element is also non-null.
  fn check<S: AsRef<[u8]>>(t: AstType<Name<S>>) {
    let lt = t.unwrap_list();
    assert!(lt.required());
    let inner = lt.ty().unwrap_name_ref();
    assert!(inner.required());
  }
  accept_all!(Type::graphql, "[Foo!]!", check);
}

#[test]
fn doubly_nested_list_type() {
  fn check<S: AsRef<[u8]>>(t: AstType<Name<S>>) {
    let outer = t.unwrap_list();
    let middle = outer.ty().unwrap_list_ref();
    let inner = middle.ty().unwrap_name_ref();
    assert!("Foo".equivalent(inner.name().source()));
  }
  accept_all!(Type::graphql, "[[Foo]]", check);
}

#[test]
fn doubly_nested_list_type_with_inner_non_nulls() {
  fn check<S: AsRef<[u8]>>(t: AstType<Name<S>>) {
    // `[[Foo!]!]!`
    let outer = t.unwrap_list();
    assert!(outer.required());
    let middle = outer.ty().unwrap_list_ref();
    assert!(middle.required());
    let inner = middle.ty().unwrap_name_ref();
    assert!(inner.required());
  }
  accept_all!(Type::graphql, "[[Foo!]!]!", check);
}

// ─── reject rows + error families ────────────────────────────────────────────

#[test]
fn ty_rejects_non_type_heads() {
  reject_all!(Type::graphql, "42");
  reject_all!(Type::graphql, "\"s\"");
  reject_all!(Type::graphql, "!");
  reject_all!(Type::graphql, "");
  reject_all!(Type::graphql, "$x");
  reject_all!(Type::graphql, "@");
}

#[test]
fn ty_rejects_unterminated_list() {
  reject_all!(Type::graphql, "[Foo");
  reject_all!(Type::graphql, "[Foo!");
}

#[test]
fn ty_rejects_empty_brackets() {
  // `ListType : [ Type ]` — the element is required, not `Type*`.
  reject_all!(Type::graphql, "[]");
}

#[test]
fn ty_invalid_head_expects_type_and_remains_unconsumed() {
  let (diagnostic_matches, leftover_kind) = drive_str(
    |inp| {
      let error = Type::graphql(inp)
        .expect_err("an integer cannot begin a type")
        .into_iter()
        .next()
        .expect("invalid type head should emit an error");
      let diagnostic_matches = matches!(
        error.into_data(),
        ErrorData::UnexpectedToken(unexpected)
          if unexpected.expected() == &Expectation::Type
            && unexpected.found() == Some(&SyntacticTokenKind::Int)
      );
      let leftover_kind = inp.next()?.map(|token| token.data().kind());
      Ok::<_, GraphqlErrors<&str>>((diagnostic_matches, leftover_kind))
    },
    "42",
  )
  .expect("the invalid head should remain readable");
  assert!(diagnostic_matches);
  assert_eq!(leftover_kind, Some(SyntacticTokenKind::Int));

  let nested = drive_str(|inp| Type::graphql(inp).map(|_| ()), "[]")
    .expect_err("an empty list type should fail")
    .into_iter()
    .next()
    .expect("an empty list type should emit an error");
  assert!(matches!(
    nested.into_data(),
    ErrorData::UnexpectedToken(unexpected)
      if unexpected.expected() == &Expectation::Type
        && unexpected.found() == Some(&SyntacticTokenKind::RBracket)
  ));
}

#[test]
fn ty_end_of_input_expects_type() {
  let error = drive_str(|inp| Type::graphql(inp).map(|_| ()), "")
    .expect_err("end of input cannot begin a type")
    .into_iter()
    .next()
    .expect("end of input should emit an error");
  assert!(matches!(
    error.into_data(),
    ErrorData::UnexpectedToken(unexpected)
      if unexpected.expected() == &Expectation::Type && unexpected.found().is_none()
  ));
}

#[test]
fn ty_unterminated_list_is_unclosed_list() {
  let error = drive_str(|inp| Type::graphql(inp).map(|_| ()), "[Foo")
    .expect_err("unterminated list type should fail")
    .into_iter()
    .next()
    .expect("unterminated list type should emit an error");
  assert!(matches!(
    error.into_data(),
    ErrorData::Unclosed(Unclosed::List)
  ));
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
      drive_str(|inp| Type::graphql(inp).map(|_| ()), src).is_ok(),
      *accept,
      "str ty({src:?})"
    );
    assert_eq!(
      drive_slice(|inp| Type::graphql(inp).map(|_| ()), src.as_bytes()).is_ok(),
      *accept,
      "slice ty({src:?})"
    );
  }
}
