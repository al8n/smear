// The shape atoms exercise here need a concrete lexer supplying `PunctuatorToken`
// (for `at`/`colon`/`spread`) and `LiteralValueToken` (for `try_description`),
// which among the dialect lexers only GraphQL's `SyntacticToken` supplies today,
// so these tests drive GraphQL alone. Every atom runs over the full source matrix
// (`str`, `[u8]`, and `Bytes`). The declining and peeking atoms never emit, so
// their paths need a single emitter mode; `spanned` propagates a committed
// sub-parser's error, so that one path runs under both a fail-fast and a
// collecting emitter. The harness stays emitter-parametrized to match the token
// and literal atoms' harnesses.
#![cfg(feature = "graphql")]

use smear_lexer::tokora::{
  Emitter, InputRef, Lexer, Parse, Parser, ParserContext, SimpleSpan,
  emitter::{Fatal, Verbose},
  error::{
    UnexpectedEot,
    syntax::{FullContainer, MissingSyntax, TooFew},
    token::{MissingToken, SeparatedError, UnexpectedToken},
  },
  punct::Pipe,
  span::Spanned,
};

use super::{
  list_of, opt, padded, padded_skipping, peek_kind, separated1, spanned, try_description,
};

use crate::combinator::{StringLiteral, at, colon, ident, rbrace, spread, try_at};

use smear_lexer::graphql::{
  lossless::{LosslessLexer, LosslessToken},
  syntactic::{SyntacticLexer, SyntacticToken, SyntacticTokenKind},
};

/// A test error sink that absorbs every tokora error family (and GraphQL's lexer
/// errors) into a unit. Implementing the full `From` set makes it a
/// [`FromEmitterError`](smear_lexer::tokora::emitter::FromEmitterError), so both
/// [`Fatal`] and [`Verbose`] instantiate as complete emitters over it, and the
/// [`UnexpectedEot`] conversion satisfies the atoms' error bound.
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
// [`Verbose`] context.

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

// The lossless twin of the drive helpers, pinning the lossless lexer per source.
// Its stream surfaces trivia as real tokens, so it is what the capturing
// `padded` and the discarding `padded_skipping` are exercised over.

fn drive_str_lossless<'inp, O, Em>(
  emitter: Em,
  f: impl for<'c> FnMut(
    &mut InputRef<
      'inp,
      'c,
      LosslessLexer<'inp, &'inp str>,
      ParserContext<'inp, LosslessLexer<'inp, &'inp str>, Em>,
    >,
  ) -> Result<O, TestError>,
  input: &'inp str,
) -> Result<O, TestError>
where
  Em: Emitter<'inp, LosslessLexer<'inp, &'inp str>, Error = TestError>,
{
  let ctx: ParserContext<'inp, LosslessLexer<'inp, &'inp str>, Em> = ParserContext::new(emitter);
  Parser::with_parser_and_context(f, ctx).parse_str(input)
}

fn drive_slice_lossless<'inp, O, Em>(
  emitter: Em,
  f: impl for<'c> FnMut(
    &mut InputRef<
      'inp,
      'c,
      LosslessLexer<'inp, &'inp [u8]>,
      ParserContext<'inp, LosslessLexer<'inp, &'inp [u8]>, Em>,
    >,
  ) -> Result<O, TestError>,
  input: &'inp [u8],
) -> Result<O, TestError>
where
  Em: Emitter<'inp, LosslessLexer<'inp, &'inp [u8]>, Error = TestError>,
{
  let ctx: ParserContext<'inp, LosslessLexer<'inp, &'inp [u8]>, Em> = ParserContext::new(emitter);
  Parser::with_parser_and_context(f, ctx).parse_slice(input)
}

#[cfg(feature = "bytes")]
fn drive_bytes_lossless<'inp, O, Em>(
  emitter: Em,
  f: impl for<'c> FnMut(
    &mut InputRef<
      'inp,
      'c,
      LosslessLexer<'inp, &'inp [u8]>,
      ParserContext<'inp, LosslessLexer<'inp, &'inp [u8]>, Em>,
    >,
  ) -> Result<O, TestError>,
  input: &'inp ::bytes::Bytes,
) -> Result<O, TestError>
where
  Em: Emitter<'inp, LosslessLexer<'inp, &'inp [u8]>, Error = TestError>,
{
  let ctx: ParserContext<'inp, LosslessLexer<'inp, &'inp [u8]>, Em> = ParserContext::new(emitter);
  Parser::with_parser_and_context(f, ctx).parse_bytes(input)
}

/// The lossless twin of [`drive_all`], routing each source through the lossless
/// lexer whose stream carries trivia.
macro_rules! drive_all_lossless {
  ($emitter:expr, |$inp:ident| $body:expr, $src:expr, $assert:expr) => {{
    $assert(drive_str_lossless($emitter, |$inp: &mut _| $body, $src));
    $assert(drive_slice_lossless(
      $emitter,
      |$inp: &mut _| $body,
      $src.as_bytes(),
    ));
    #[cfg(feature = "bytes")]
    {
      let owned = ::bytes::Bytes::from_static($src.as_bytes());
      $assert(drive_bytes_lossless($emitter, |$inp: &mut _| $body, &owned));
    }
  }};
}

/// Views a lexer slice (`&str` or `&[u8]`) as bytes via `AsRef<[u8]>`, so one
/// assertion body reads a payload's text across every source representation.
fn as_bytes<S: AsRef<[u8]>>(slice: &S) -> &[u8] {
  slice.as_ref()
}

// `peek_kind`: reports the next kind without consuming, so the same peek repeats
// and a committed atom still parses the leftover; end of input peeks as `None`.

#[test]
fn peek_kind_reports_at_twice_then_at_parses() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let first = peek_kind(inp)?;
      let second = peek_kind(inp)?;
      assert_eq!(first, Some(SyntacticTokenKind::At));
      assert_eq!(first, second);
      // Peeking consumed nothing, so the committed `at` atom pulls the `@` straight
      // off the input; its span proves the leftover and that peeking left it in place.
      let at = at(inp)?;
      assert_eq!(at.span(), &SimpleSpan::new(0, 1));
      Ok::<_, TestError>(())
    },
    "@x",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

#[test]
fn peek_kind_none_on_empty_input() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| peek_kind(inp),
    "",
    |out: Result<Option<SyntacticTokenKind>, TestError>| assert!(matches!(out, Ok(None)))
  );
}

// `opt`: an accepted `try_`-attempt becomes `Some`, a decline becomes `None` with
// the leftover left in place for the next atom.

#[test]
fn opt_try_at_accepts_at() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let opt_at = opt(try_at)(inp)?;
      assert!(opt_at.is_some());
      let at = opt_at.unwrap();
      assert_eq!(at.span(), &SimpleSpan::new(0, 1));
      Ok::<_, TestError>(())
    },
    "@",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

#[test]
fn opt_try_at_declines_on_colon_and_leaves_it() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let declined = opt(try_at)(inp)?.is_none();
      // The `:` is untouched, so the committed `colon` atom parses it; its span
      // proves the leftover's type and that the decline consumed nothing.
      let col = colon(inp)?;
      assert_eq!(col.span(), &SimpleSpan::new(0, 1));
      Ok::<_, TestError>(declined)
    },
    ":",
    |out: Result<bool, TestError>| assert!(matches!(out, Ok(true)))
  );
}

// `spanned`: pairs the sub-parser's output with the span it covered, and
// propagates a committed sub-parser's error untouched.

#[test]
fn spanned_spread_covers_three() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let (_spread, span) = spanned(spread)(inp)?;
      assert_eq!(span, SimpleSpan::new(0, 3));
      Ok::<_, TestError>(())
    },
    "...",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

#[test]
fn spanned_propagates_sub_parser_error() {
  // A partial spread makes the committed `spread` sub-parser emit and error, so
  // the wrapper returns `Err` under both a fail-fast and a collecting emitter.
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| spanned(spread)(inp).map(|_| ()),
    "..",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
  drive_all!(
    Verbose::<TestError>::new(),
    |inp| spanned(spread)(inp).map(|_| ()),
    "..",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
}

// `try_description`: the optional leading string literal — `Some((payload, span))`
// on a string, `None` (nothing consumed) otherwise.

#[test]
fn try_description_accepts_inline() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let desc = try_description(inp)?;
      assert!(desc.is_some());
      let (value, span) = desc.unwrap();
      match value {
        StringLiteral::Inline(inline) => assert_eq!(as_bytes(inline.source_ref()), b"\"hi\""),
        StringLiteral::Block(_) => panic!("expected an inline description"),
      }
      assert_eq!(span, SimpleSpan::new(0, 4));
      Ok::<_, TestError>(())
    },
    "\"hi\"",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

#[test]
fn try_description_accepts_block() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let desc = try_description(inp)?;
      assert!(desc.is_some());
      let (value, span) = desc.unwrap();
      match value {
        StringLiteral::Block(block) => {
          assert_eq!(as_bytes(block.source_ref()), b"\"\"\"desc\"\"\"")
        }
        StringLiteral::Inline(_) => panic!("expected a block description"),
      }
      assert_eq!(span, SimpleSpan::new(0, 10));
      Ok::<_, TestError>(())
    },
    "\"\"\"desc\"\"\"",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

#[test]
fn try_description_declines_on_ident_and_leaves_it() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let declined = try_description(inp)?.is_none();
      // The `x` identifier is untouched, so the committed `ident` atom pulls it off;
      // its slice and span prove the leftover's type and that the decline consumed
      // nothing.
      let id = ident(inp)?;
      assert_eq!(as_bytes(id.source_ref()), b"x");
      assert_eq!(id.span(), SimpleSpan::new(0, 1));
      Ok::<_, TestError>(declined)
    },
    "x",
    |out: Result<bool, TestError>| assert!(matches!(out, Ok(true)))
  );
}

/// Continue-predicate for `separated1`: the next token is an identifier, so it
/// starts another item in a `|`-separated list of names.
fn starts_ident<S>(tok: &SyntacticToken<S>) -> bool {
  SyntacticTokenKind::from(tok) == SyntacticTokenKind::Identifier
}

/// Stop-predicate for `list_of`: the next token is the closing brace that ends the
/// list, so the loop stops and leaves the `}` in place.
fn is_close_brace<S>(tok: &SyntacticToken<S>) -> bool {
  SyntacticTokenKind::from(tok) == SyntacticTokenKind::RBrace
}

/// An inner sub-parser that records one non-fatal diagnostic and then yields
/// `()`. Under a fail-fast emitter the emit is fatal and this errors; under a
/// collecting emitter the diagnostic is recorded, the emit recovers, and this
/// returns `Ok` — so an enclosing `padded` threads on to collect its trailing
/// trivia. It is a bare `fn` (not a closure) so the padded atom's higher-order
/// bound pins the lexer for the emitter call, the way the atom fns compose.
fn emit_then_recover<'inp, L, Em>(
  inp: &mut InputRef<'inp, '_, L, ParserContext<'inp, L, Em>>,
) -> Result<(), TestError>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  Em: Emitter<'inp, L, Error = TestError>,
{
  inp
    .emitter()
    .emit_error(Spanned::new(SimpleSpan::new(0, 0), TestError))?;
  Ok(())
}

// `separated1`: one-or-more `|`-separated idents, leading separator allowed,
// trailing rejected, at least one required. The accept paths assert each item's
// slice and span; the leading-separator and too-few/trailing-separator paths run
// under both emitter modes, and the collecting mode's returned items are asserted.

#[test]
fn separated1_three_idents() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let items = separated1::<Pipe, _, _, _, _, _, _>(ident, starts_ident)(inp)?;
      assert_eq!(items.len(), 3);
      assert_eq!(as_bytes(items[0].source_ref()), b"A");
      assert_eq!(items[0].span(), SimpleSpan::new(0, 1));
      assert_eq!(as_bytes(items[1].source_ref()), b"B");
      assert_eq!(items[1].span(), SimpleSpan::new(4, 5));
      assert_eq!(as_bytes(items[2].source_ref()), b"C");
      assert_eq!(items[2].span(), SimpleSpan::new(8, 9));
      Ok::<_, TestError>(())
    },
    "A | B | C",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

#[test]
fn separated1_allows_leading_separator() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      // The leading `|` is consumed and does not count as an item, so the two
      // names remain and their spans skip the leading separator.
      let items = separated1::<Pipe, _, _, _, _, _, _>(ident, starts_ident)(inp)?;
      assert_eq!(items.len(), 2);
      assert_eq!(as_bytes(items[0].source_ref()), b"A");
      assert_eq!(items[0].span(), SimpleSpan::new(2, 3));
      assert_eq!(as_bytes(items[1].source_ref()), b"B");
      assert_eq!(items[1].span(), SimpleSpan::new(6, 7));
      Ok::<_, TestError>(())
    },
    "| A | B",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

// A trailing separator is unexpected (no `allow_trailing`): the closing `|` with
// no item after it is an unexpected-trailing-separator emit. Under a fail-fast
// emitter it aborts to `Err`; under a collecting emitter it is recorded and the
// parse threads on to return the one item gathered before the trailing separator.
#[test]
fn separated1_trailing_separator_errors() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| separated1::<Pipe, _, _, _, _, _, _>(ident, starts_ident)(inp).map(|_| ()),
    "A |",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
  drive_all!(
    Verbose::<TestError>::new(),
    |inp| {
      let items = separated1::<Pipe, _, _, _, _, _, _>(ident, starts_ident)(inp)?;
      assert_eq!(items.len(), 1);
      assert_eq!(as_bytes(items[0].source_ref()), b"A");
      assert_eq!(items[0].span(), SimpleSpan::new(0, 1));
      Ok::<_, TestError>(())
    },
    "A |",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

// At least one item is required: empty input is a too-few emit. Under a fail-fast
// emitter it aborts to `Err`; under a collecting emitter it is recorded and the
// parse returns the empty collection.
#[test]
fn separated1_empty_errors() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| separated1::<Pipe, _, _, _, _, _, _>(ident, starts_ident)(inp).map(|_| ()),
    "",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
  drive_all!(
    Verbose::<TestError>::new(),
    |inp| {
      let items = separated1::<Pipe, _, _, _, _, _, _>(ident, starts_ident)(inp)?;
      assert!(items.is_empty());
      Ok::<_, TestError>(())
    },
    "",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

// `list_of`: zero-or-more idents, no separator, stopping at the `}` the stop
// predicate accepts. The accept path asserts each item's slice and span, then
// commits `rbrace` to prove `list_of` stopped before the `}` and left it in place.

#[test]
fn list_of_three_idents_until_brace() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let items = list_of(ident, is_close_brace)(inp)?;
      assert_eq!(items.len(), 3);
      assert_eq!(as_bytes(items[0].source_ref()), b"a");
      assert_eq!(items[0].span(), SimpleSpan::new(0, 1));
      assert_eq!(as_bytes(items[1].source_ref()), b"b");
      assert_eq!(items[1].span(), SimpleSpan::new(2, 3));
      assert_eq!(as_bytes(items[2].source_ref()), b"c");
      assert_eq!(items[2].span(), SimpleSpan::new(4, 5));
      // The `}` was the stop token, left in place, so the committed closer parses it.
      let close = rbrace(inp)?;
      assert_eq!(close.span(), &SimpleSpan::new(6, 7));
      Ok::<_, TestError>(())
    },
    "a b c }",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

// An immediate stop yields an empty list with the stop token left in place — the
// zero-or-more lower bound, with no diagnostic.
#[test]
fn list_of_empty_leaves_stop_token() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let items = list_of(ident, is_close_brace)(inp)?;
      assert!(items.is_empty());
      let close = rbrace(inp)?;
      assert_eq!(close.span(), &SimpleSpan::new(0, 1));
      Ok::<_, TestError>(())
    },
    "}",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

// `padded`: over the lossless stream the lexer surfaces trivia as real tokens,
// and the atom captures rather than discards it. `"  x  # c\n"` pads the ident
// `x` with two leading spaces and, after it, two spaces, a comment, and a
// newline; the atom keeps every one. The success assertions cover the value's
// slice and span, both capture collections (non-empty, as the brief requires),
// the run span that reaches over the trivia, and one trivia token's own slice
// and span — proving the comment was retained verbatim, not dropped.
#[test]
fn padded_captures_surrounding_trivia_over_lossless() {
  drive_all_lossless!(
    Fatal::<TestError>::new(),
    |inp| {
      let wrapped = padded(ident)(inp)?;
      // The value is the ident, stripped of its padding.
      assert_eq!(as_bytes(wrapped.value().source_ref()), b"x");
      assert_eq!(wrapped.value().span(), SimpleSpan::new(2, 3));
      // Both edges captured their trivia — two leading spaces; two spaces, a
      // comment, and a newline trailing.
      assert_eq!(wrapped.leading().len(), 2);
      assert_eq!(wrapped.trailing().len(), 4);
      // The run span reaches over the captured trivia, not just the value.
      assert_eq!(wrapped.span(), &SimpleSpan::new(0, 9));
      // A leading space, kept with its span.
      assert_eq!(wrapped.leading()[0].span, SimpleSpan::new(0, 1));
      // The comment trivia is retained with its slice and span — the whole point
      // of the atom: nothing the value was wrapped in is discarded.
      let comment = wrapped
        .trailing()
        .iter()
        .find_map(|t| match &t.data {
          LosslessToken::Comment(slice) => Some((slice, t.span)),
          _ => None,
        })
        .expect("the comment is retained among the trailing trivia");
      assert_eq!(as_bytes(comment.0), b"# c");
      assert_eq!(comment.1, SimpleSpan::new(5, 8));
      Ok::<_, TestError>(())
    },
    "  x  # c\n",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

// The same atom over the plain syntactic stream, whose lexer skips trivia so it
// never reaches the parser: both capture collections come back empty and the run
// span is exactly the value's.
#[test]
fn padded_captures_nothing_when_stream_omits_trivia() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| {
      let wrapped = padded(ident)(inp)?;
      assert_eq!(as_bytes(wrapped.value().source_ref()), b"x");
      assert_eq!(wrapped.value().span(), SimpleSpan::new(0, 1));
      assert!(wrapped.leading().is_empty());
      assert!(wrapped.trailing().is_empty());
      assert_eq!(wrapped.span(), &SimpleSpan::new(0, 1));
      Ok::<_, TestError>(())
    },
    "x",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

// `padded_skipping` delegates to tokora's padding combinator: over the lossless
// stream it skips the surrounding trivia and hands back the bare ident, keeping
// none of it. Its span is the value's alone, and after it the stream is
// exhausted — proving the trailing trivia was consumed, not left behind.
#[test]
fn padded_skipping_discards_surrounding_trivia_over_lossless() {
  drive_all_lossless!(
    Fatal::<TestError>::new(),
    |inp| {
      let id = padded_skipping(ident)(inp)?;
      assert_eq!(as_bytes(id.source_ref()), b"x");
      assert_eq!(id.span(), SimpleSpan::new(2, 3));
      assert!(peek_kind(inp)?.is_none());
      Ok::<_, TestError>(())
    },
    "  x  ",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}

// `padded` runs its sub-parser between the two trivia loops, so an emit from that
// sub-parser threads through the atom exactly as the delimited atoms' do.
// `emit_then_recover` records one non-fatal diagnostic: under a fail-fast emitter
// it is fatal, so `padded` aborts before it can wrap and returns `Err`; under a
// collecting emitter the diagnostic recovers, the trailing loop finds no trivia,
// and `padded` returns the value wrapped with empty capture collections.
#[test]
fn padded_threads_inner_emit_across_emitter_modes() {
  drive_all!(
    Fatal::<TestError>::new(),
    |inp| padded(emit_then_recover)(inp).map(|_| ()),
    "",
    |out: Result<(), TestError>| assert!(out.is_err())
  );
  drive_all!(
    Verbose::<TestError>::new(),
    |inp| {
      let wrapped = padded(emit_then_recover)(inp)?;
      assert!(wrapped.leading().is_empty());
      assert!(wrapped.trailing().is_empty());
      Ok::<_, TestError>(())
    },
    "",
    |out: Result<(), TestError>| assert!(out.is_ok())
  );
}
