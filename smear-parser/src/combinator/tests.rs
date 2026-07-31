#![cfg(any(feature = "graphql", feature = "graphqlx"))]

use smear_lexer::tokora::{
  FatalContext, InputRef, Lexer, Parse, Parser,
  emitter::{FromUnclosed, FullContainerEmitter, SeparatedEmitter, TooFewEmitter},
  error::{
    Unclosed, UnexpectedEot,
    syntax::{FullContainer, MissingSyntax, TooFew},
    token::{MissingToken, SeparatedError, UnexpectedToken},
  },
};

use super::ParseCtx;

#[cfg(feature = "graphql")]
use smear_lexer::graphql::syntactic::SyntacticLexer;

#[cfg(feature = "graphqlx")]
use smear_lexer::graphqlx::syntactic::SyntacticLexer as GxLexer;

/// A minimal error sink for the bundle gate: it absorbs every tokora error family
/// (and either dialect's lexer errors) into a unit, so `Fatal<TestError>` is a
/// [`ComposableEmitter`](super::ComposableEmitter) for any lexer. That lets the
/// bundle be instantiated here without pulling in a real dialect error type,
/// which the parser defines in a later layer.
#[derive(Debug)]
struct TestError;

impl<'a, T, Kind: Clone, S, Lang: ?Sized> From<UnexpectedToken<'a, T, Kind, S, Lang>>
  for TestError
{
  fn from(_: UnexpectedToken<'a, T, Kind, S, Lang>) -> Self {
    Self
  }
}

impl<'a, T, Kind: Clone, S, Lang: ?Sized> From<SeparatedError<'a, T, Kind, S, Lang>> for TestError {
  fn from(_: SeparatedError<'a, T, Kind, S, Lang>) -> Self {
    Self
  }
}

impl<'a, Kind: Clone, O, Lang: ?Sized> From<MissingToken<'a, Kind, O, Lang>> for TestError {
  fn from(_: MissingToken<'a, Kind, O, Lang>) -> Self {
    Self
  }
}

impl<O, Lang: ?Sized> From<MissingSyntax<O, Lang>> for TestError {
  fn from(_: MissingSyntax<O, Lang>) -> Self {
    Self
  }
}

// One `Set`-generic impl covers both end-of-input members of `FromTokenErrors`: the
// default `&'static str` set and a dispatch driver's `&'static [Kind]` table.
impl<O, Lang: ?Sized, Set: Clone + 'static> From<UnexpectedEot<O, Lang, Set>> for TestError {
  fn from(_: UnexpectedEot<O, Lang, Set>) -> Self {
    Self
  }
}

impl<'a, L: Lexer<'a>, Lang: ?Sized> FromUnclosed<'a, L, Lang> for TestError {
  fn from_unclosed<D>(_: Unclosed<D, L::Span, Lang>) -> Self {
    Self
  }
}

impl<S, Lang: ?Sized> From<FullContainer<S, Lang>> for TestError {
  fn from(_: FullContainer<S, Lang>) -> Self {
    Self
  }
}

impl<S, Lang: ?Sized> From<TooFew<S, Lang>> for TestError {
  fn from(_: TooFew<S, Lang>) -> Self {
    Self
  }
}

#[cfg(feature = "graphql")]
impl<Char, StateError> From<smear_lexer::graphql::error::LexerErrors<Char, StateError>>
  for TestError
{
  fn from(_: smear_lexer::graphql::error::LexerErrors<Char, StateError>) -> Self {
    Self
  }
}

#[cfg(feature = "graphqlx")]
impl<Char, StateError> From<smear_lexer::graphqlx::error::LexerErrors<Char, StateError>>
  for TestError
{
  fn from(_: smear_lexer::graphqlx::error::LexerErrors<Char, StateError>) -> Self {
    Self
  }
}

fn assert_ctx<'inp, L, Ctx>()
where
  L: Lexer<'inp>,
  Ctx: ParseCtx<'inp, L>,
{
}

fn requires_separated<'inp, L, Em>()
where
  L: Lexer<'inp>,
  Em: SeparatedEmitter<'inp, L>,
{
}

fn requires_too_few<'inp, L, Em>()
where
  L: Lexer<'inp>,
  Em: TooFewEmitter<'inp, L>,
{
}

fn requires_full_container<'inp, L, Em>()
where
  L: Lexer<'inp>,
  Em: FullContainerEmitter<'inp, L>,
{
}

fn elaborates<'inp, L, Ctx>()
where
  L: Lexer<'inp>,
  Ctx: ParseCtx<'inp, L>,
{
  requires_separated::<L, Ctx::Emitter>();
  requires_too_few::<L, Ctx::Emitter>();
  requires_full_container::<L, Ctx::Emitter>();
}

#[test]
fn ctx_bundle_holds_for_both_dialects_and_sources() {
  #[cfg(feature = "graphql")]
  {
    assert_ctx::<SyntacticLexer<'_, str>, FatalContext<'_, SyntacticLexer<'_, str>, TestError>>();
    assert_ctx::<SyntacticLexer<'_, [u8]>, FatalContext<'_, SyntacticLexer<'_, [u8]>, TestError>>();
    elaborates::<SyntacticLexer<'_, str>, FatalContext<'_, SyntacticLexer<'_, str>, TestError>>();
    elaborates::<SyntacticLexer<'_, [u8]>, FatalContext<'_, SyntacticLexer<'_, [u8]>, TestError>>();
  }
  #[cfg(feature = "graphqlx")]
  {
    assert_ctx::<GxLexer<'_, str>, FatalContext<'_, GxLexer<'_, str>, TestError>>();
    assert_ctx::<GxLexer<'_, [u8]>, FatalContext<'_, GxLexer<'_, [u8]>, TestError>>();
    elaborates::<GxLexer<'_, str>, FatalContext<'_, GxLexer<'_, str>, TestError>>();
    elaborates::<GxLexer<'_, [u8]>, FatalContext<'_, GxLexer<'_, [u8]>, TestError>>();
  }
}

#[cfg(feature = "graphql")]
fn drive_str<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<
      'inp,
      'c,
      SyntacticLexer<'inp, str>,
      FatalContext<'inp, SyntacticLexer<'inp, str>, TestError>,
    >,
  ) -> Result<O, TestError>,
  input: &'inp str,
) -> Result<O, TestError> {
  Parser::with_parser(f).parse_str(input)
}

#[cfg(feature = "graphql")]
fn drive_slice<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<
      'inp,
      'c,
      SyntacticLexer<'inp, [u8]>,
      FatalContext<'inp, SyntacticLexer<'inp, [u8]>, TestError>,
    >,
  ) -> Result<O, TestError>,
  input: &'inp [u8],
) -> Result<O, TestError> {
  Parser::with_parser(f).parse_slice(input)
}

#[cfg(feature = "graphql")]
#[test]
fn trivial_parse_drives_over_str_and_slice() {
  let out = drive_str(|inp| inp.next().map(|t| t.is_some()), "query");
  assert!(matches!(out, Ok(true)));

  let out = drive_slice(|inp| inp.next().map(|t| t.is_some()), b"query");
  assert!(matches!(out, Ok(true)));
}

#[cfg(feature = "graphqlx")]
fn drive_str_gx<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, GxLexer<'inp, str>, FatalContext<'inp, GxLexer<'inp, str>, TestError>>,
  ) -> Result<O, TestError>,
  input: &'inp str,
) -> Result<O, TestError> {
  Parser::with_parser(f).parse_str(input)
}

#[cfg(feature = "graphqlx")]
fn drive_slice_gx<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, GxLexer<'inp, [u8]>, FatalContext<'inp, GxLexer<'inp, [u8]>, TestError>>,
  ) -> Result<O, TestError>,
  input: &'inp [u8],
) -> Result<O, TestError> {
  Parser::with_parser(f).parse_slice(input)
}

#[cfg(feature = "graphqlx")]
#[test]
fn trivial_parse_drives_over_str_and_slice_graphqlx() {
  let out = drive_str_gx(|inp| inp.next().map(|t| t.is_some()), "query");
  assert!(matches!(out, Ok(true)));

  let out = drive_slice_gx(|inp| inp.next().map(|t| t.is_some()), b"query");
  assert!(matches!(out, Ok(true)));
}
