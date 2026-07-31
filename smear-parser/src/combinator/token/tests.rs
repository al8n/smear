// Punctuator atoms need `PunctuatorToken`, which among the dialect lexers only
// GraphQL's `SyntacticToken` supplies today, so the punctuator tests drive
// GraphQL alone. Identifier atoms need `IdentifierToken`, which both dialects
// supply, so the identifier tests drive GraphQL and GraphQL-like alike. Every
// atom runs over the full source matrix (`str`, `[u8]`, and `Bytes`).

use smear_lexer::tokora::{
  Emitter, InputRef, Lexer, Parse, Parser, ParserContext, SimpleSpan,
  emitter::{Fatal, FromUnclosed, Verbose},
  error::{
    Unclosed, UnexpectedEot,
    syntax::{FullContainer, MissingSyntax, TooFew},
    token::{MissingToken, SeparatedError, UnexpectedToken},
  },
};

use super::{enum_value, ident, try_enum_value, try_ident};

#[cfg(feature = "graphql")]
use super::{at, colon, keyword_exact, lbrace, spread, try_at, try_keyword_exact};
#[cfg(feature = "graphql")]
use smear_lexer::graphql::syntactic::{SyntacticLexer, SyntacticToken};

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
    |inp| {
      let at = at(inp)?;
      assert_eq!(at.span(), &SimpleSpan::new(0, 1));
      Ok::<_, TestError>(())
    },
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
      // The `:` is untouched, so the committed `colon` atom pulls it straight off;
      // its span proves the leftover is the colon and that the decline consumed
      // nothing.
      let col = colon(inp)?;
      assert_eq!(col.span(), &SimpleSpan::new(0, 1));
      Ok::<_, TestError>(declined)
    },
    ":",
    |out: Result<bool, TestError>| assert!(matches!(out, Ok(true)))
  );
}

#[cfg(feature = "graphql")]
#[test]
fn spread_commits_on_spread() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let spread = spread(inp)?;
      assert_eq!(spread.span(), &SimpleSpan::new(0, 3));
      Ok::<_, TestError>(())
    },
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

// `EnumValue` atoms: the accept paths mirror `ident`'s (span and, via
// `AsRef<[u8]>`, the source slice); the reject paths additionally cover the
// content-based exclusion `enum_value`/`try_enum_value` enforce on top of
// `ident`'s lexical check. Soft keywords (`enum`, `type`) are pinned as legal
// Names, matching the spec: only `true`, `false`, and `null` are excluded.

#[cfg(feature = "graphql")]
#[test]
fn enum_value_commits_on_hello() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let id = enum_value(inp)?;
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
fn enum_value_commits_on_soft_keywords() {
  // `enum` and `type` are soft keywords: the spec reserves no words for
  // `Name` besides the three `enum_value` excludes, so both parse here.
  for word in ["enum", "type"] {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let id = enum_value(inp)?;
        assert_eq!(as_bytes(id.source_ref()), word.as_bytes());
        assert_eq!(id.span(), SimpleSpan::new(0, word.len()));
        Ok::<_, TestError>(())
      },
      word,
      |out: Result<(), TestError>| assert!(out.is_ok(), "{word} should parse as a Name")
    );
  }
}

#[cfg(feature = "graphql")]
#[test]
fn enum_value_errors_on_reserved_words() {
  for word in ["true", "false", "null"] {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| enum_value(inp).map(|_| ()),
      word,
      |out: Result<(), TestError>| assert!(out.is_err(), "{word} should be rejected")
    );
    drive_all!(
      Verbose::<TestError>::new(),
      |inp| enum_value(inp).map(|_| ()),
      word,
      |out: Result<(), TestError>| assert!(out.is_err(), "{word} should be rejected")
    );
  }
}

#[cfg(feature = "graphql")]
#[test]
fn enum_value_errors_on_lbrace() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| enum_value(inp).map(|_| ()),
    "{",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
  drive_all!(
    Verbose::<TestError>::new(),
    |inp| enum_value(inp).map(|_| ()),
    "{",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
}

#[cfg(feature = "graphql")]
#[test]
fn enum_value_errors_on_empty_input() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| enum_value(inp).map(|_| ()),
    "",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
  drive_all!(
    Verbose::<TestError>::new(),
    |inp| enum_value(inp).map(|_| ()),
    "",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
}

/// A test error that keeps an [`UnexpectedToken`]'s span, unlike [`TestError`]
/// (which absorbs every error family into a unit, so it cannot answer
/// "where"). Scoped to [`enum_value_errors_carry_the_reserved_words_span`],
/// the one test that needs to inspect a rejected span directly; the other
/// conversions this sink carries exist only to complete the emitter's `From`
/// set (as [`TestError`]'s do) and are unreachable for the well-formed,
/// single-token inputs that test drives.
#[cfg(feature = "graphql")]
#[derive(Debug)]
struct SpanOnly(SimpleSpan);

#[cfg(feature = "graphql")]
impl<'a, T, Kind: Clone, Lang: ?Sized> From<UnexpectedToken<'a, T, Kind, SimpleSpan, Lang>>
  for SpanOnly
{
  fn from(err: UnexpectedToken<'a, T, Kind, SimpleSpan, Lang>) -> Self {
    Self(err.span())
  }
}

#[cfg(feature = "graphql")]
impl<'a, T, Kind: Clone, S, Lang: ?Sized> From<SeparatedError<'a, T, Kind, S, Lang>> for SpanOnly {
  fn from(_: SeparatedError<'a, T, Kind, S, Lang>) -> Self {
    unreachable!("enum_value span tests never drive a separated combinator")
  }
}

#[cfg(feature = "graphql")]
impl<'a, Kind: Clone, O, Lang: ?Sized> From<MissingToken<'a, Kind, O, Lang>> for SpanOnly {
  fn from(_: MissingToken<'a, Kind, O, Lang>) -> Self {
    unreachable!("enum_value span tests never report a missing token")
  }
}

#[cfg(feature = "graphql")]
impl<O, Lang: ?Sized, Set: Clone + 'static> From<UnexpectedEot<O, Lang, Set>> for SpanOnly {
  fn from(_: UnexpectedEot<O, Lang, Set>) -> Self {
    unreachable!("enum_value span tests never run past end of input")
  }
}

#[cfg(feature = "graphql")]
impl<'a, L: Lexer<'a>, Lang: ?Sized> FromUnclosed<'a, L, Lang> for SpanOnly {
  fn from_unclosed<D>(_: Unclosed<D, L::Span, Lang>) -> Self {
    unreachable!("enum_value span tests never open a delimiter")
  }
}

#[cfg(feature = "graphql")]
impl<O, Lang: ?Sized> From<MissingSyntax<O, Lang>> for SpanOnly {
  fn from(_: MissingSyntax<O, Lang>) -> Self {
    unreachable!("enum_value span tests never report missing syntax")
  }
}

#[cfg(feature = "graphql")]
impl<S, Lang: ?Sized> From<FullContainer<S, Lang>> for SpanOnly {
  fn from(_: FullContainer<S, Lang>) -> Self {
    unreachable!("enum_value span tests never fill a container")
  }
}

#[cfg(feature = "graphql")]
impl<S, Lang: ?Sized> From<TooFew<S, Lang>> for SpanOnly {
  fn from(_: TooFew<S, Lang>) -> Self {
    unreachable!("enum_value span tests never underfill a repetition")
  }
}

#[cfg(feature = "graphql")]
impl<Char, StateError> From<smear_lexer::graphql::error::LexerErrors<Char, StateError>>
  for SpanOnly
{
  fn from(_: smear_lexer::graphql::error::LexerErrors<Char, StateError>) -> Self {
    unreachable!("enum_value span tests never trip a lexer error")
  }
}

/// The [`drive_str`] twin over [`SpanOnly`], pinning the closure's `InputRef`
/// shape so [`enum_value`] monomorphises against the span-keeping sink.
#[cfg(feature = "graphql")]
fn drive_str_span_only<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<
      'inp,
      'c,
      SyntacticLexer<'inp, str>,
      ParserContext<'inp, SyntacticLexer<'inp, str>, Fatal<SpanOnly>>,
    >,
  ) -> Result<O, SpanOnly>,
  input: &'inp str,
) -> Result<O, SpanOnly> {
  let ctx: ParserContext<'inp, SyntacticLexer<'inp, str>, Fatal<SpanOnly>> =
    ParserContext::new(Fatal::new());
  Parser::with_parser_and_context(f, ctx).parse_str(input)
}

/// Runs [`enum_value`] over `src` under the GraphQL lexer with [`SpanOnly`] as
/// the error sink, returning the rejected span. Panics if `enum_value` accepts
/// `src`; every caller passes one of the three excluded spellings.
#[cfg(feature = "graphql")]
fn enum_value_rejection_span(src: &str) -> SimpleSpan {
  match drive_str_span_only(|inp: &mut _| enum_value(inp).map(|_| ()), src) {
    Err(SpanOnly(span)) => span,
    Ok(()) => panic!("enum_value accepted the reserved word `{src}`"),
  }
}

#[cfg(feature = "graphql")]
#[test]
fn enum_value_errors_carry_the_reserved_words_span() {
  assert_eq!(enum_value_rejection_span("true"), SimpleSpan::new(0, 4));
  assert_eq!(enum_value_rejection_span("false"), SimpleSpan::new(0, 5));
  assert_eq!(enum_value_rejection_span("null"), SimpleSpan::new(0, 4));
}

#[cfg(feature = "graphql")]
#[test]
fn try_enum_value_accepts_hello() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let attempt = try_enum_value(inp)?;
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
fn try_enum_value_accepts_soft_keywords() {
  for word in ["enum", "type"] {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let attempt = try_enum_value(inp)?;
        assert!(attempt.is_accept());
        let id = attempt.unwrap_accept();
        assert_eq!(as_bytes(id.source_ref()), word.as_bytes());
        Ok::<_, TestError>(())
      },
      word,
      |out: Result<(), TestError>| assert!(out.is_ok(), "{word} should parse as a Name")
    );
  }
}

#[cfg(feature = "graphql")]
#[test]
fn try_enum_value_declines_on_reserved_words_and_leaves_them() {
  for word in ["true", "false", "null"] {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let declined = try_enum_value(inp)?.is_decline();
        // The reserved word is untouched, so a plain `ident` parse (which has
        // no exclusion of its own) picks it straight back up.
        let leftover = ident(inp)?;
        assert_eq!(as_bytes(leftover.source_ref()), word.as_bytes());
        Ok::<_, TestError>(declined)
      },
      word,
      |out: Result<bool, TestError>| assert!(matches!(out, Ok(true)), "{word} should decline")
    );
  }
}

#[cfg(feature = "graphql")]
#[test]
fn try_enum_value_declines_on_lbrace_and_leaves_it() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let declined = try_enum_value(inp)?.is_decline();
      let brace_present = inp.next()?.is_some();
      Ok::<_, TestError>((declined, brace_present))
    },
    "{",
    |out: Result<(bool, bool), TestError>| assert!(matches!(out, Ok((true, true))))
  );
}

#[cfg(feature = "graphql")]
#[test]
fn try_enum_value_declines_on_empty_input() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| Ok::<_, TestError>(try_enum_value(inp)?.is_decline()),
    "",
    |out: Result<bool, TestError>| assert!(matches!(out, Ok(true)))
  );
}

// GraphQL-like mirrors: `enum_value`/`try_enum_value` only need
// `IdentifierToken`, which the GraphQL-like lexer supplies same as GraphQL's,
// so the same exclusion rule and matrix apply.

#[cfg(feature = "graphqlx")]
#[test]
fn enum_value_commits_on_hello_graphqlx() {
  drive_all_gx!(
    Fatal::<TestError>::new(),
    |inp| {
      let id = enum_value(inp)?;
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
fn enum_value_errors_on_reserved_words_graphqlx() {
  for word in ["true", "false", "null"] {
    drive_all_gx!(
      Fatal::<TestError>::new(),
      |inp| enum_value(inp).map(|_| ()),
      word,
      |out: Result<(), TestError>| assert!(out.is_err(), "{word} should be rejected")
    );
    drive_all_gx!(
      Verbose::<TestError>::new(),
      |inp| enum_value(inp).map(|_| ()),
      word,
      |out: Result<(), TestError>| assert!(out.is_err(), "{word} should be rejected")
    );
  }
}

#[cfg(feature = "graphqlx")]
#[test]
fn enum_value_errors_on_lbrace_graphqlx() {
  drive_all_gx!(
    Fatal::<TestError>::new(),
    |inp| enum_value(inp).map(|_| ()),
    "{",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
  drive_all_gx!(
    Verbose::<TestError>::new(),
    |inp| enum_value(inp).map(|_| ()),
    "{",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
}

#[cfg(feature = "graphqlx")]
#[test]
fn enum_value_errors_on_empty_input_graphqlx() {
  drive_all_gx!(
    Fatal::<TestError>::new(),
    |inp| enum_value(inp).map(|_| ()),
    "",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
  drive_all_gx!(
    Verbose::<TestError>::new(),
    |inp| enum_value(inp).map(|_| ()),
    "",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
}

#[cfg(feature = "graphqlx")]
#[test]
fn try_enum_value_accepts_hello_graphqlx() {
  drive_all_gx!(
    Fatal::<TestError>::new(),
    |inp| {
      let attempt = try_enum_value(inp)?;
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
fn try_enum_value_declines_on_reserved_words_and_leaves_them_graphqlx() {
  for word in ["true", "false", "null"] {
    drive_all_gx!(
      Fatal::<TestError>::new(),
      |inp| {
        let declined = try_enum_value(inp)?.is_decline();
        let leftover = ident(inp)?;
        assert_eq!(as_bytes(leftover.source_ref()), word.as_bytes());
        Ok::<_, TestError>(declined)
      },
      word,
      |out: Result<bool, TestError>| assert!(matches!(out, Ok(true)), "{word} should decline")
    );
  }
}

#[cfg(feature = "graphqlx")]
#[test]
fn try_enum_value_declines_on_lbrace_and_leaves_it_graphqlx() {
  drive_all_gx!(
    Fatal::<TestError>::new(),
    |inp| {
      let declined = try_enum_value(inp)?.is_decline();
      let brace_present = inp.next()?.is_some();
      Ok::<_, TestError>((declined, brace_present))
    },
    "{",
    |out: Result<(bool, bool), TestError>| assert!(matches!(out, Ok((true, true))))
  );
}

#[cfg(feature = "graphqlx")]
#[test]
fn try_enum_value_declines_on_empty_input_graphqlx() {
  drive_all_gx!(
    Fatal::<TestError>::new(),
    |inp| Ok::<_, TestError>(try_enum_value(inp)?.is_decline()),
    "",
    |out: Result<bool, TestError>| assert!(matches!(out, Ok(true)))
  );
}

// Keyword atoms need `KeywordToken`, which among the dialect lexers only
// GraphQL's `SyntacticToken` supplies today, so the keyword tests drive GraphQL
// alone. Every test runs over the full source matrix (`str`, `[u8]`, and
// `Bytes`) via `drive_all!`, which exercises keyword equivalence over byte
// sources as well as `str`. Accept paths assert the matched keyword's span and,
// for the untyped atoms, the identifier text carried by the token; error paths
// run under both emitter modes.

/// Reads the identifier text out of a keyword atom's token payload, so accept
/// paths can assert the matched slice across every source representation.
#[cfg(feature = "graphql")]
fn keyword_text<S: AsRef<[u8]>>(kw: &SyntacticToken<S>) -> &[u8] {
  match kw {
    SyntacticToken::Identifier(s) => as_bytes(s),
    _ => panic!("keyword atom yielded a non-identifier token"),
  }
}

#[cfg(feature = "graphql")]
#[test]
fn keyword_exact_commits_on_on() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let kw = keyword_exact(inp, "on")?;
      assert_eq!(kw.span(), SimpleSpan::new(0, 2));
      assert_eq!(keyword_text(kw.source_ref()), b"on");
      Ok::<_, TestError>(())
    },
    "on",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

#[cfg(feature = "graphql")]
#[test]
fn keyword_exact_errors_on_wrong_keyword() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| keyword_exact(inp, "on").map(|_| ()),
    "type",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
  drive_all!(
    Verbose::<TestError>::new(),
    |inp| keyword_exact(inp, "on").map(|_| ()),
    "type",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
}

#[cfg(feature = "graphql")]
#[test]
fn keyword_exact_errors_on_non_keyword() {
  // A brace is not a keyword, so `keyword()` yields `None` and the committed
  // atom reports an unexpected token rather than a keyword mismatch.
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| keyword_exact(inp, "on").map(|_| ()),
    "{",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
  drive_all!(
    Verbose::<TestError>::new(),
    |inp| keyword_exact(inp, "on").map(|_| ()),
    "{",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
}

#[cfg(feature = "graphql")]
#[test]
fn keyword_exact_errors_on_empty_input() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| keyword_exact(inp, "on").map(|_| ()),
    "",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
  drive_all!(
    Verbose::<TestError>::new(),
    |inp| keyword_exact(inp, "on").map(|_| ()),
    "",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
}

#[cfg(feature = "graphql")]
#[test]
fn try_keyword_exact_accepts_on() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let attempt = try_keyword_exact(inp, "on")?;
      assert!(attempt.is_accept());
      let kw = attempt.unwrap_accept();
      assert_eq!(kw.span(), SimpleSpan::new(0, 2));
      assert_eq!(keyword_text(kw.source_ref()), b"on");
      Ok::<_, TestError>(())
    },
    "on",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

#[cfg(feature = "graphql")]
#[test]
fn try_keyword_exact_declines_on_type_and_leaves_it() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let declined = try_keyword_exact(inp, "on")?.is_decline();
      // The declined `type` keyword is untouched, so the committed atom pulls it
      // straight off the input; its span proves the leftover is the `type`
      // keyword and that the decline consumed nothing.
      let leftover = keyword_exact(inp, "type")?;
      assert_eq!(leftover.span(), SimpleSpan::new(0, 4));
      assert_eq!(keyword_text(leftover.source_ref()), b"type");
      Ok::<_, TestError>(declined)
    },
    "type",
    |out: Result<bool, TestError>| assert!(matches!(out, Ok(true)))
  );
}

#[cfg(feature = "graphql")]
#[test]
fn try_keyword_exact_declines_on_empty_input() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| Ok::<_, TestError>(try_keyword_exact(inp, "on")?.is_decline()),
    "",
    |out: Result<bool, TestError>| assert!(matches!(out, Ok(true)))
  );
}

// The typed-keyword mapping macro is a consumer surface: the test module invokes
// it (naming dialect keyword nodes is allowed for tests) to prove the generated
// atoms commit, decline, and map onto the concrete `On`/`Type` nodes.
#[cfg(feature = "graphql")]
typed_keyword_atom!(
  kw_on / try_kw_on => "on" => smear_lexer::keywords::On,
  kw_type / try_kw_type => "type" => smear_lexer::keywords::Type,
);

#[cfg(feature = "graphql")]
#[test]
fn kw_on_commits_on_on() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let on = kw_on(inp)?;
      assert_eq!(on.span(), &SimpleSpan::new(0, 2));
      Ok::<_, TestError>(())
    },
    "on",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

#[cfg(feature = "graphql")]
#[test]
fn kw_on_errors_on_wrong_keyword() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| kw_on(inp).map(|_| ()),
    "type",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
  drive_all!(
    Verbose::<TestError>::new(),
    |inp| kw_on(inp).map(|_| ()),
    "type",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
}

#[cfg(feature = "graphql")]
#[test]
fn kw_on_errors_on_empty_input() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| kw_on(inp).map(|_| ()),
    "",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
  drive_all!(
    Verbose::<TestError>::new(),
    |inp| kw_on(inp).map(|_| ()),
    "",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
}

#[cfg(feature = "graphql")]
#[test]
fn try_kw_on_accepts_on() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let attempt = try_kw_on(inp)?;
      assert!(attempt.is_accept());
      assert_eq!(attempt.unwrap_accept().span(), &SimpleSpan::new(0, 2));
      Ok::<_, TestError>(())
    },
    "on",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

#[cfg(feature = "graphql")]
#[test]
fn try_kw_on_declines_on_type_and_leaves_it() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let declined = try_kw_on(inp)?.is_decline();
      // The untouched `type` keyword is accepted by its own typed declining atom,
      // asserting the leftover's type rather than mere presence.
      let leftover = try_kw_type(inp)?;
      let accepted_type = leftover.is_accept();
      Ok::<_, TestError>((declined, accepted_type))
    },
    "type",
    |out: Result<(bool, bool), TestError>| assert!(matches!(out, Ok((true, true))))
  );
}

#[cfg(feature = "graphql")]
#[test]
fn kw_type_commits_on_type() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let ty = kw_type(inp)?;
      assert_eq!(ty.span(), &SimpleSpan::new(0, 4));
      Ok::<_, TestError>(())
    },
    "type",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}
