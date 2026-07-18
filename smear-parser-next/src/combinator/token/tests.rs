// Punctuator atoms are generic over any lexer whose token implements
// `PunctuatorToken`. Among the dialect lexers only GraphQL's `SyntacticToken`
// carries that capability today, so these tests drive GraphQL over the full
// source matrix (`str`, `[u8]`, and `Bytes`) rather than both dialects.
#![cfg(feature = "graphql")]

use smear_lexer::{
  graphql::syntactic::SyntacticLexer,
  tokora::{
    Emitter, InputRef, Parse, Parser, ParserContext,
    emitter::{Fatal, Verbose},
    error::{
      UnexpectedEot,
      syntax::{FullContainer, MissingSyntax, TooFew},
      token::{MissingToken, SeparatedError, UnexpectedToken},
    },
  },
};

use super::{at, colon, spread, try_at};

/// A test error sink that absorbs every tokora error family (and GraphQL's lexer
/// errors) into a unit. Implementing the full `From` set makes it a
/// [`FromEmitterError`](smear_lexer::tokora::emitter::FromEmitterError), so both
/// [`Fatal`] and [`Verbose`] instantiate as complete emitters over it, and the
/// [`UnexpectedEot`] conversion satisfies the committed atoms' error bound.
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

impl<O, Lang: ?Sized> From<UnexpectedEot<O, Lang>> for TestError {
  fn from(_: UnexpectedEot<O, Lang>) -> Self {
    Self
  }
}

impl<O, Lang: ?Sized> From<MissingSyntax<O, Lang>> for TestError {
  fn from(_: MissingSyntax<O, Lang>) -> Self {
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

impl<Char, StateError> From<smear_lexer::graphql::error::LexerErrors<Char, StateError>>
  for TestError
{
  fn from(_: smear_lexer::graphql::error::LexerErrors<Char, StateError>) -> Self {
    Self
  }
}

// The drive helpers pin one concrete lexer per source yet stay generic over the
// emitter, so the same closure runs under a fail-fast [`Fatal`] and a collecting
// [`Verbose`] context. The closure the callers pass is polymorphic in its
// `InputRef`, so each helper monomorphises it for its own lexer.

fn drive_str<'inp, O, Em>(
  emitter: Em,
  f: impl for<'c> FnMut(
    &mut InputRef<
      'inp,
      'c,
      SyntacticLexer<'inp, str>,
      ParserContext<'inp, SyntacticLexer<'inp, str>, Em>,
    >,
  ) -> Result<O, TestError>,
  input: &'inp str,
) -> Result<O, TestError>
where
  Em: Emitter<'inp, SyntacticLexer<'inp, str>, Error = TestError>,
{
  let ctx: ParserContext<'inp, SyntacticLexer<'inp, str>, Em> = ParserContext::new(emitter);
  Parser::with_parser_and_context(f, ctx).parse_str(input)
}

fn drive_slice<'inp, O, Em>(
  emitter: Em,
  f: impl for<'c> FnMut(
    &mut InputRef<
      'inp,
      'c,
      SyntacticLexer<'inp, [u8]>,
      ParserContext<'inp, SyntacticLexer<'inp, [u8]>, Em>,
    >,
  ) -> Result<O, TestError>,
  input: &'inp [u8],
) -> Result<O, TestError>
where
  Em: Emitter<'inp, SyntacticLexer<'inp, [u8]>, Error = TestError>,
{
  let ctx: ParserContext<'inp, SyntacticLexer<'inp, [u8]>, Em> = ParserContext::new(emitter);
  Parser::with_parser_and_context(f, ctx).parse_slice(input)
}

#[cfg(feature = "bytes")]
fn drive_bytes<'inp, O, Em>(
  emitter: Em,
  f: impl for<'c> FnMut(
    &mut InputRef<
      'inp,
      'c,
      SyntacticLexer<'inp, [u8]>,
      ParserContext<'inp, SyntacticLexer<'inp, [u8]>, Em>,
    >,
  ) -> Result<O, TestError>,
  input: &'inp ::bytes::Bytes,
) -> Result<O, TestError>
where
  Em: Emitter<'inp, SyntacticLexer<'inp, [u8]>, Error = TestError>,
{
  let ctx: ParserContext<'inp, SyntacticLexer<'inp, [u8]>, Em> = ParserContext::new(emitter);
  Parser::with_parser_and_context(f, ctx).parse_bytes(input)
}

/// Runs `body` against `src` under `emitter` across every source representation
/// (`str`, `[u8]`, and `Bytes`), applying `assert` to each `Result`.
macro_rules! drive_all {
  ($emitter:expr, |$inp:ident| $body:expr, $src:expr, $assert:expr) => {{
    $assert(drive_str($emitter, |$inp: &mut _| $body, $src));
    $assert(drive_slice($emitter, |$inp: &mut _| $body, $src.as_bytes()));
    #[cfg(feature = "bytes")]
    {
      let owned = ::bytes::Bytes::from_static($src.as_bytes());
      $assert(drive_bytes($emitter, |$inp: &mut _| $body, &owned));
    }
  }};
}

#[test]
fn at_commits_on_at() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| at(inp).map(|_| ()),
    "@",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

#[test]
fn at_errors_on_colon() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| at(inp).map(|_| ()),
    ":",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
  drive_all!(
    Verbose::<TestError>::new(),
    |inp| at(inp).map(|_| ()),
    ":",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
}

#[test]
fn try_at_declines_on_colon_and_leaves_colon() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let declined = try_at(inp)?.is_decline();
      let colon_parsed = colon(inp).map(|_| ()).is_ok();
      Ok::<_, TestError>((declined, colon_parsed))
    },
    ":",
    |out: Result<(bool, bool), TestError>| assert!(matches!(out, Ok((true, true))))
  );
}

#[test]
fn spread_commits_on_spread() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| spread(inp).map(|_| ()),
    "...",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

#[test]
fn spread_errors_on_partial_spread() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| spread(inp).map(|_| ()),
    "..",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
  drive_all!(
    Verbose::<TestError>::new(),
    |inp| spread(inp).map(|_| ()),
    "..",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
}
