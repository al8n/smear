// Punctuator atoms need `PunctuatorToken`, which among the dialect lexers only
// GraphQL's `SyntacticToken` supplies today, so the punctuator tests drive
// GraphQL alone. Identifier atoms need `IdentifierToken`, which both dialects
// supply, so the identifier tests drive GraphQL and GraphQL-like alike. Every
// atom runs over the full source matrix (`str`, `[u8]`, and `Bytes`).

use smear_lexer::tokora::{
  Emitter, InputRef, Parse, Parser, ParserContext, SimpleSpan,
  emitter::{Fatal, Verbose},
  error::{
    UnexpectedEot,
    syntax::{FullContainer, MissingSyntax, TooFew},
    token::{MissingToken, SeparatedError, UnexpectedToken},
  },
};

use super::{ident, try_ident};

#[cfg(feature = "graphql")]
use super::{at, colon, lbrace, spread, try_at};
#[cfg(feature = "graphql")]
use smear_lexer::graphql::syntactic::SyntacticLexer;

#[cfg(feature = "graphqlx")]
use smear_lexer::graphqlx::syntactic::SyntacticLexer as GxLexer;

/// A test error sink that absorbs every tokora error family (and either dialect's
/// lexer errors) into a unit. Implementing the full `From` set makes it a
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

// The drive helpers pin one concrete lexer per source yet stay generic over the
// emitter, so the same closure runs under a fail-fast [`Fatal`] and a collecting
// [`Verbose`] context. The closure the callers pass is polymorphic in its
// `InputRef`, so each helper monomorphises it for its own lexer.

#[cfg(feature = "graphql")]
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

#[cfg(feature = "graphql")]
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

#[cfg(all(feature = "graphql", feature = "bytes"))]
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
#[cfg(feature = "graphql")]
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

#[cfg(feature = "graphqlx")]
fn drive_str_gx<'inp, O, Em>(
  emitter: Em,
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, GxLexer<'inp, str>, ParserContext<'inp, GxLexer<'inp, str>, Em>>,
  ) -> Result<O, TestError>,
  input: &'inp str,
) -> Result<O, TestError>
where
  Em: Emitter<'inp, GxLexer<'inp, str>, Error = TestError>,
{
  let ctx: ParserContext<'inp, GxLexer<'inp, str>, Em> = ParserContext::new(emitter);
  Parser::with_parser_and_context(f, ctx).parse_str(input)
}

#[cfg(feature = "graphqlx")]
fn drive_slice_gx<'inp, O, Em>(
  emitter: Em,
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, GxLexer<'inp, [u8]>, ParserContext<'inp, GxLexer<'inp, [u8]>, Em>>,
  ) -> Result<O, TestError>,
  input: &'inp [u8],
) -> Result<O, TestError>
where
  Em: Emitter<'inp, GxLexer<'inp, [u8]>, Error = TestError>,
{
  let ctx: ParserContext<'inp, GxLexer<'inp, [u8]>, Em> = ParserContext::new(emitter);
  Parser::with_parser_and_context(f, ctx).parse_slice(input)
}

#[cfg(all(feature = "graphqlx", feature = "bytes"))]
fn drive_bytes_gx<'inp, O, Em>(
  emitter: Em,
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, GxLexer<'inp, [u8]>, ParserContext<'inp, GxLexer<'inp, [u8]>, Em>>,
  ) -> Result<O, TestError>,
  input: &'inp ::bytes::Bytes,
) -> Result<O, TestError>
where
  Em: Emitter<'inp, GxLexer<'inp, [u8]>, Error = TestError>,
{
  let ctx: ParserContext<'inp, GxLexer<'inp, [u8]>, Em> = ParserContext::new(emitter);
  Parser::with_parser_and_context(f, ctx).parse_bytes(input)
}

/// The GraphQL-like twin of [`drive_all`], routing each source through the
/// GraphQL-like lexer.
#[cfg(feature = "graphqlx")]
macro_rules! drive_all_gx {
  ($emitter:expr, |$inp:ident| $body:expr, $src:expr, $assert:expr) => {{
    $assert(drive_str_gx($emitter, |$inp: &mut _| $body, $src));
    $assert(drive_slice_gx(
      $emitter,
      |$inp: &mut _| $body,
      $src.as_bytes(),
    ));
    #[cfg(feature = "bytes")]
    {
      let owned = ::bytes::Bytes::from_static($src.as_bytes());
      $assert(drive_bytes_gx($emitter, |$inp: &mut _| $body, &owned));
    }
  }};
}

#[cfg(feature = "graphql")]
#[test]
fn at_commits_on_at() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| at(inp).map(|_| ()),
    "@",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

#[cfg(feature = "graphql")]
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

#[cfg(feature = "graphql")]
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

#[cfg(feature = "graphql")]
#[test]
fn spread_commits_on_spread() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| spread(inp).map(|_| ()),
    "...",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

#[cfg(feature = "graphql")]
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

/// Views a lexer slice (`&str` or `&[u8]`) as bytes via `AsRef<[u8]>`, so one
/// assertion body reads the identifier's text across every source representation.
fn as_bytes<S: AsRef<[u8]>>(slice: &S) -> &[u8] {
  slice.as_ref()
}

// Identifier atoms: the accept paths assert the returned identifier's span (and,
// via `AsRef<[u8]>`, its slice) so the same body reads across `str` and byte
// sources; the error paths run under both emitter modes.

#[cfg(feature = "graphql")]
#[test]
fn ident_commits_on_hello() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let id = ident(inp)?;
      assert_eq!(as_bytes(id.source_ref()), b"hello");
      assert_eq!(id.span(), SimpleSpan::new(0, 5));
      Ok::<_, TestError>(())
    },
    "hello",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

#[cfg(feature = "graphql")]
#[test]
fn try_ident_accepts_hello() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let attempt = try_ident(inp)?;
      assert!(attempt.is_accept());
      let id = attempt.unwrap_accept();
      assert_eq!(as_bytes(id.source_ref()), b"hello");
      assert_eq!(id.span(), SimpleSpan::new(0, 5));
      Ok::<_, TestError>(())
    },
    "hello",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

#[cfg(feature = "graphql")]
#[test]
fn try_ident_declines_on_lbrace_and_leaves_it() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let declined = try_ident(inp)?.is_decline();
      let lbrace_parsed = lbrace(inp).map(|_| ()).is_ok();
      Ok::<_, TestError>((declined, lbrace_parsed))
    },
    "{",
    |out: Result<(bool, bool), TestError>| assert!(matches!(out, Ok((true, true))))
  );
}

#[cfg(feature = "graphql")]
#[test]
fn ident_errors_on_lbrace() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| ident(inp).map(|_| ()),
    "{",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
  drive_all!(
    Verbose::<TestError>::new(),
    |inp| ident(inp).map(|_| ()),
    "{",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
}

#[cfg(feature = "graphql")]
#[test]
fn ident_errors_on_empty_input() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| ident(inp).map(|_| ()),
    "",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
  drive_all!(
    Verbose::<TestError>::new(),
    |inp| ident(inp).map(|_| ()),
    "",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
}

#[cfg(feature = "graphql")]
#[test]
fn try_ident_declines_on_empty_input() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| Ok::<_, TestError>(try_ident(inp)?.is_decline()),
    "",
    |out: Result<bool, TestError>| assert!(matches!(out, Ok(true)))
  );
}

#[cfg(feature = "graphqlx")]
#[test]
fn ident_commits_on_hello_graphqlx() {
  drive_all_gx!(
    Fatal::<TestError>::new(),
    |inp| {
      let id = ident(inp)?;
      assert_eq!(as_bytes(id.source_ref()), b"hello");
      assert_eq!(id.span(), SimpleSpan::new(0, 5));
      Ok::<_, TestError>(())
    },
    "hello",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

#[cfg(feature = "graphqlx")]
#[test]
fn try_ident_accepts_hello_graphqlx() {
  drive_all_gx!(
    Fatal::<TestError>::new(),
    |inp| {
      let attempt = try_ident(inp)?;
      assert!(attempt.is_accept());
      let id = attempt.unwrap_accept();
      assert_eq!(as_bytes(id.source_ref()), b"hello");
      assert_eq!(id.span(), SimpleSpan::new(0, 5));
      Ok::<_, TestError>(())
    },
    "hello",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

#[cfg(feature = "graphqlx")]
#[test]
fn try_ident_declines_on_lbrace_and_leaves_it_graphqlx() {
  // GraphQL-like tokens do not carry `PunctuatorToken`, so the leftover check
  // pulls the untouched brace straight off the input instead of a punct atom.
  drive_all_gx!(
    Fatal::<TestError>::new(),
    |inp| {
      let declined = try_ident(inp)?.is_decline();
      let brace_present = inp.next()?.is_some();
      Ok::<_, TestError>((declined, brace_present))
    },
    "{",
    |out: Result<(bool, bool), TestError>| assert!(matches!(out, Ok((true, true))))
  );
}

#[cfg(feature = "graphqlx")]
#[test]
fn ident_errors_on_lbrace_graphqlx() {
  drive_all_gx!(
    Fatal::<TestError>::new(),
    |inp| ident(inp).map(|_| ()),
    "{",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
  drive_all_gx!(
    Verbose::<TestError>::new(),
    |inp| ident(inp).map(|_| ()),
    "{",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
}

#[cfg(feature = "graphqlx")]
#[test]
fn ident_errors_on_empty_input_graphqlx() {
  drive_all_gx!(
    Fatal::<TestError>::new(),
    |inp| ident(inp).map(|_| ()),
    "",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
  drive_all_gx!(
    Verbose::<TestError>::new(),
    |inp| ident(inp).map(|_| ()),
    "",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
}

#[cfg(feature = "graphqlx")]
#[test]
fn try_ident_declines_on_empty_input_graphqlx() {
  drive_all_gx!(
    Fatal::<TestError>::new(),
    |inp| Ok::<_, TestError>(try_ident(inp)?.is_decline()),
    "",
    |out: Result<bool, TestError>| assert!(matches!(out, Ok(true)))
  );
}
