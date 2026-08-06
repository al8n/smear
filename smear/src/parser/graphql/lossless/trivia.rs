//! The trivia-aware atom set — the **only** peek/expect door the lossless productions use.
//!
//! Over a trivia-surfacing stream every decision point must commit leading trivia before it
//! looks at the next token. Committing trivia during a peek is safe: trivia belongs to the
//! parse, and to the tree, no matter which branch wins. Every atom here opens with that skip
//! so a production cannot forget it without bypassing this module.
//!
//! # What counts as trivia
//!
//! The atoms cross whatever [`Token::is_trivia`] admits, which for the lossless GraphQL token
//! is `smear-lexer`'s own `LosslessToken::is_trivia` — eight token forms:
//!
//! | Form | Bytes |
//! |---|---|
//! | `Bom` | `\u{FEFF}` |
//! | `Space` | `` ` ` `` |
//! | `Tab` | `\t` |
//! | `Newline` | `\n` |
//! | `CarriageReturn` | `\r` |
//! | `CarriageReturnAndNewline` | `\r\n` |
//! | `Comma` | `,` |
//! | `Comment` | `#` to the line terminator, terminator excluded |
//!
//! That is exactly GraphQL's `Ignored` production, and it is the same set `apollo-parser`
//! ignores — `is_whitespace_assimilated` (`lexer/mod.rs:602`) folds the BOM, the two space
//! forms and the two line terminators into one `Whitespace`, and `skip_ignored`
//! (`parser/mod.rs:236`) adds `Comment` and `Comma`. The sets agree; only the granularity
//! differs, and the coarser one is apollo's. `tests/lossless_trivia_atoms.rs` pins the set by
//! behaviour, form by form, so a lexer change that silently reclassifies one is a test failure
//! rather than a formatter bug found months later.
//!
//! **The comma is trivia, not punctuation.** GraphQL says so, and a reader coming from a
//! comma-separated language is the one most likely to misread it. It is not a separator any
//! production may require or count.
//!
//! # The kind vocabulary here is the lexer's, not the tree's
//!
//! `peek_kind` answers in `LosslessTokenKind` — `smear-lexer`'s vocabulary — and **not** in
//! [`SyntaxKind`](crate::graphql::kinds::SyntaxKind), the tree's. The two are different
//! spaces with overlapping variant names; a production compares an atom's answer against
//! `LosslessTokenKind`, and only [`super::kind_map`] ever speaks both.
//!
//! One consequence is worth stating because the obvious reading is wrong. The lexer's space
//! keeps `\r`, `\n` and `\r\n` apart where the tree's folds all three onto `SyntaxKind::Newline`
//! — but **no atom here can expose that difference**, because every atom skips trivia before
//! it answers, so a trivia kind is never a possible answer (pinned by
//! `peek_kind_never_answers_with_a_trivia_kind`). A consumer that needs to tell CRLF from LF
//! reads the token's text off the tree, which keeps it verbatim. The finer lexer kinds buy this
//! module nothing; they are simply what the projection happens to be.
//!
//! # Trivia commits eagerly, and that is a deliberate divergence
//!
//! `apollo-parser` queues ignored tokens in a `pending` buffer and flushes them into the tree
//! at the *next real token's* `eat` (`push_ignored`, `parser/mod.rs:243`), so trivia can land
//! inside a node that was opened after the trivia was read. These atoms commit trivia the
//! moment they cross it, which attaches it to whatever node is open **at the decision point** —
//! the outer one. That is the placement a formatter wants (the blank line before a field
//! belongs to the selection set, not to the field), and it is the placement tokora's sink gives
//! for free: a committed token lands in the innermost node open at its commit. Reproducing
//! apollo's deferral would mean a second buffering layer beside the sink's own mark/rollback
//! discipline, for a placement this suite does not want.

use tokora::{
  Lexer, SimpleSpan, Source, Token,
  error::{UnexpectedEot, token::UnexpectedToken},
  lexer::FromLogos,
  try_parse_input::ParseAttempt,
  utils::DowncastRef,
};

use crate::graphql::GraphQL;

use super::{
  GraphqlLosslessError, GraphqlLosslessInput, GraphqlLosslessLexer, GraphqlLosslessToken,
};

// ---------------------------------------------------------------------------------------------
// The six atoms are `crate::lossless::trivia`'s, wrapped here with this dialect's four aliases
// pinned.
//
// **Wrappers, not `pub use` re-exports.** A re-export would force every call site's turbofish to
// become `expect::<GraphqlLosslessLexer<'inp, Src>, Ctx, GraphQL>(…)`, changing ~200 lines across
// seven production files for no gain — and destroying the property that makes this lift
// reviewable, namely that no production file changed at all. The wrappers add no behaviour; each
// is a single delegating call, and `#[inline]` makes the indirection free.
//
// The where-bundles below are the *dialect-bound* spellings, and they are longer than the
// substrate's on purpose. `GraphqlLosslessToken<'inp, Src>: Token<'inp> + …` and the
// `Lexer<Token = …>` equality are not implied over a generic `Src`, because `LosslessToken<S>`
// receives its `Token` impl from `smear-lexer`'s `token!` macro **once per concrete slice type**
// — unlike `SyntacticToken<S>`, whose impl is generic. Generically, `L::Token` simply *is* the
// lexer's associated type, so the substrate needs neither clause; here they are what makes the
// projection normalize.
// ---------------------------------------------------------------------------------------------

pub(crate) use crate::lossless::trivia::kind_of;

/// [`crate::lossless::trivia::peek_kind`] over this dialect's input.
#[inline]
pub(crate) fn peek_kind<'inp, Src, Ctx>(
  inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>,
) -> Result<
  Option<<GraphqlLosslessToken<'inp, Src> as Token<'inp>>::Kind>,
  GraphqlLosslessError<'inp, Src, Ctx>,
>
where
  Src: Source<usize> + ?Sized,
  GraphqlLosslessToken<'inp, Src>: Token<'inp> + FromLogos<'inp>,
  GraphqlLosslessLexer<'inp, Src>:
    Lexer<'inp, Token = GraphqlLosslessToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: tokora::ParseContext<'inp, GraphqlLosslessLexer<'inp, Src>, GraphQL>,
  GraphqlLosslessError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQL>>,
{
  crate::lossless::trivia::peek_kind(inp)
}

/// [`crate::lossless::trivia::peek_as`] over this dialect's input.
#[inline]
pub(crate) fn peek_as<'inp, Src, Ctx, Projection>(
  inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>,
) -> Result<Option<Projection>, GraphqlLosslessError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlLosslessToken<'inp, Src>: Token<'inp> + FromLogos<'inp> + DowncastRef<Projection>,
  GraphqlLosslessLexer<'inp, Src>:
    Lexer<'inp, Token = GraphqlLosslessToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: tokora::ParseContext<'inp, GraphqlLosslessLexer<'inp, Src>, GraphQL>,
  GraphqlLosslessError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQL>>,
{
  crate::lossless::trivia::peek_as(inp)
}

/// [`crate::lossless::trivia::expect`] over this dialect's input.
#[inline]
pub(crate) fn expect<'inp, Src, Ctx>(
  inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>,
  kind: <GraphqlLosslessToken<'inp, Src> as Token<'inp>>::Kind,
) -> Result<(), GraphqlLosslessError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlLosslessToken<'inp, Src>: Token<'inp> + FromLogos<'inp> + Clone,
  GraphqlLosslessLexer<'inp, Src>:
    Lexer<'inp, Token = GraphqlLosslessToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: tokora::ParseContext<'inp, GraphqlLosslessLexer<'inp, Src>, GraphQL>,
  GraphqlLosslessError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQL>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlLosslessToken<'inp, Src>,
        <GraphqlLosslessToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQL,
      >,
    >,
{
  crate::lossless::trivia::expect(inp, kind)
}

/// [`crate::lossless::trivia::eat_if`] over this dialect's input.
#[inline]
pub(crate) fn eat_if<'inp, Src, Ctx>(
  inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>,
  kind: <GraphqlLosslessToken<'inp, Src> as Token<'inp>>::Kind,
) -> Result<bool, GraphqlLosslessError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlLosslessToken<'inp, Src>: Token<'inp> + FromLogos<'inp>,
  GraphqlLosslessLexer<'inp, Src>:
    Lexer<'inp, Token = GraphqlLosslessToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: tokora::ParseContext<'inp, GraphqlLosslessLexer<'inp, Src>, GraphQL>,
{
  crate::lossless::trivia::eat_if(inp, kind)
}

/// [`crate::lossless::trivia::try_eat`] over this dialect's input.
#[inline]
pub(crate) fn try_eat<'inp, Src, Ctx>(
  inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>,
  kind: <GraphqlLosslessToken<'inp, Src> as Token<'inp>>::Kind,
) -> Result<ParseAttempt<()>, GraphqlLosslessError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlLosslessToken<'inp, Src>: Token<'inp> + FromLogos<'inp>,
  GraphqlLosslessLexer<'inp, Src>:
    Lexer<'inp, Token = GraphqlLosslessToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: tokora::ParseContext<'inp, GraphqlLosslessLexer<'inp, Src>, GraphQL>,
{
  crate::lossless::trivia::try_eat(inp, kind)
}

/// Drivers that run one atom over a `&str` and report what it committed.
///
/// Test-only scaffolding, exported so the integration test at
/// `tests/lossless_trivia_atoms.rs` can reach it; nothing in the crate calls it.
///
/// **This module is where the atoms stop being generic.** The atoms above name only the
/// projections; these drivers must choose a concrete source, emitter and context to build a
/// `Sink` at all, exactly as [`super::runner::parse_document`] does.
///
/// Behind `feature = "test-support"`, and hidden even then. `pub` is forced only because
/// `tests/lossless_trivia_atoms.rs` is a separate crate; the drivers themselves name this
/// dialect's concrete lexer and are of no use to anyone outside it.
#[cfg(feature = "test-support")]
#[doc(hidden)]
pub mod test_support {
  use std::{
    cell::Cell,
    string::{String, ToString},
  };

  use tokora::{
    InputRef, SimpleSpan,
    cache::DefaultCache,
    cst::{Sink, parse_lossless},
    emitter::Verbose,
  };

  use crate::graphql::{GraphQL, kinds::SyntaxKind as K};

  use super::super::{GraphqlLosslessErrors, GraphqlLosslessLexer, SyntaxNode, runner::profile};

  /// The kind vocabulary the atoms answer in — the lexer's, not the tree's.
  pub type Kind = smear_lexer::graphql::lossless::LosslessTokenKind;

  /// The recording emitter, the sink over it, the context pair, and the input the driver's
  /// closure receives.
  ///
  /// Spelled out because a closure's parameter type is **not** inferred through a
  /// `ParseInput` bound — only through an `Fn` bound — so `|inp: &mut _|` leaves `L` and `Ctx`
  /// unresolved and the body's first method call is the error site. `runner::parse_document` never
  /// hits this: it applies a named function whose own signature pins both.
  type Emitter<'inp> = Verbose<GraphqlLosslessErrors<&'inp str>, SimpleSpan, GraphQL>;
  type TestSink<'inp> = Sink<'inp, GraphqlLosslessLexer<'inp, str>, Emitter<'inp>>;
  type TestCtx<'inp> = (
    TestSink<'inp>,
    DefaultCache<'inp, GraphqlLosslessLexer<'inp, str>>,
  );
  type TestInput<'inp, 'input> =
    InputRef<'inp, 'input, GraphqlLosslessLexer<'inp, str>, TestCtx<'inp>, GraphQL>;

  /// Runs one atom over `src` inside a `Document` node, drains whatever the atom left, and
  /// materializes.
  ///
  /// Two details carry the whole file's evidence:
  ///
  /// - **The atom runs inside a node**, so "what the atom committed" is a thing the *tree*
  ///   can be asked — it is that node's text — rather than a cursor offset the sink never saw.
  ///   A skip that fails to commit produces a shorter node, which is how the mutation proof in
  ///   Task 4 Step 5 goes red.
  /// - **The remainder is drained afterwards.** `Sink::finish` refuses any source byte that no
  ///   committed token covers and no lexer-error diagnostic explains
  ///   (`FinishError::UncoveredGap`), and an atom deliberately stops mid-source. Without the
  ///   drain every helper here would fail materialization instead of reporting its atom.
  macro_rules! drive {
    ($lt:lifetime, $src:expr, $init:expr, |$inp:ident| $atom:expr) => {{
      let src: &$lt str = $src;
      let out = Cell::new($init);

      let (cst, _) = parse_lossless::<GraphqlLosslessLexer<$lt, str>, GraphQL, _, _, _, _>(
        src,
        Default::default(),
        Emitter::default(),
        profile::<str>(),
        DefaultCache::<GraphqlLosslessLexer<'_, str>>::default(),
        |$inp: &mut TestInput<$lt, '_>| {
          // The `node` combinator's own `mark` / `start_at` / `finish`, spelled out. The
          // combinator wants an inner parser whose error type is pinned, and a closure whose
          // body is `Ok(())` pins nothing — so the wrap is driven directly instead of teaching
          // the test scaffold's inference about a type it never names.
          let mark = $inp.cst_mark();
          out.set($atom);
          $inp.cst_start_at(mark, K::Document.raw());
          $inp.cst_finish(K::Document.raw());
          // Whatever the atom left behind, so `finish` has full coverage.
          $inp.skip_while(|_| true)
        },
      );

      let (green, _emitter) = cst.finish(K::Root.raw());
      let root = SyntaxNode::new_root(
        green.expect("the trivia-atom driver emitted a malformed event stream"),
      );
      // The `Document` node is the only node child; its text is what the atom committed.
      let text: String = root
        .first_child()
        .map(|n| n.text().to_string())
        .unwrap_or_default();

      (out.into_inner(), text)
    }};
  }

  /// `super::peek_kind` over `src`.
  ///
  /// `::<str, _>` for the reason `runner::parse_document` spells out: `str` and `&str` both project
  /// `Slice<'inp> = &'inp str`, so the lexer type alone leaves an atom's `Src` genuinely
  /// ambiguous, and `str` is the one that matches `L::Source`.
  ///
  /// The error is a **panic, not a `None`**. `peek_kind`'s `None` means end of input and a test
  /// asserts on it; folding an error into the same value would let a broken atom pass the
  /// end-of-input test for the wrong reason.
  pub fn peek_kind_of<'inp>(src: &'inp str) -> Option<Kind> {
    let (kind, _text) = drive!('inp, src, None, |inp| super::peek_kind::<str, _>(inp)
      .expect("peek_kind must not error over these fixtures"));
    kind
  }

  /// `super::expect` for `{` over `src`: did it match, and what did it commit?
  pub fn expect_brace_of<'inp>(src: &'inp str) -> (bool, String) {
    drive!('inp, src, false, |inp| super::expect::<str, _>(inp, Kind::LBrace).is_ok())
  }

  /// `super::eat_if` for `{` over `src`: did it eat, and what did it commit?
  ///
  /// Panics on an error for the same reason `peek_kind_of` does: `eat_if`'s `false` means
  /// "declined", which a test asserts on, so an error must not be able to spell it.
  /// `expect_brace_of` is the exception — its `bool` **is** `is_ok()`, since a failed `expect`
  /// erroring is the behaviour under test.
  pub fn eat_if_brace_of<'inp>(src: &'inp str) -> (bool, String) {
    drive!('inp, src, false, |inp| super::eat_if::<str, _>(inp, Kind::LBrace)
      .expect("eat_if must not error over these fixtures"))
  }
}
