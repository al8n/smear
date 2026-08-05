//! The trivia-aware atom set — the **only** peek/expect door the lossless productions use.
//!
//! Over a trivia-surfacing stream every decision point must commit leading trivia before it looks
//! at the next token. Committing trivia during a peek is safe: trivia belongs to the parse, and to
//! the tree, no matter which branch wins. Every atom here opens with that skip so a production
//! cannot forget it without bypassing this module.
//!
//! # What counts as trivia
//!
//! The atoms cross whatever [`Token::is_trivia`] admits, which for the lossless GraphQLx token is
//! `smear-lexer`'s own `LosslessToken::is_trivia` — the same eight token forms GraphQL's admits:
//! `Bom`, `Space`, `Tab`, `Newline`, `CarriageReturn`, `CarriageReturnAndNewline`, `Comma` and
//! `Comment`. GraphQLx widens the *significant* vocabulary and leaves the ignored one alone;
//! `tests/lossless_x_trivia_atoms.rs` pins that by behaviour, form by form, rather than by this
//! sentence.
//!
//! **The comma is trivia, not punctuation**, here as in GraphQL. It is not a separator any
//! production may require or count — and GraphQLx's generic lists, `where` clauses and import
//! lists are all comma-*tolerant* rather than comma-separated for exactly that reason.
//!
//! # The kind vocabulary here is the lexer's, not the tree's
//!
//! `peek_kind` answers in `LosslessTokenKind` — `smear-lexer`'s vocabulary — and **not** in
//! [`SyntaxKind`](crate::graphqlx::kinds::SyntaxKind), the tree's. The two are different spaces
//! with overlapping variant names; a production compares an atom's answer against
//! `LosslessTokenKind`, and only [`super::kind_map`] ever speaks both. The lexer's space keeps
//! `\r`, `\n` and `\r\n` apart where the tree's folds all three, but **no atom here can expose
//! that difference**, because every atom skips trivia before it answers.

use tokora::{
  Lexer, SimpleSpan, Source, Token,
  error::{UnexpectedEot, token::UnexpectedToken},
  lexer::FromLogos,
  try_parse_input::ParseAttempt,
  utils::DowncastRef,
};

use crate::graphqlx::GraphQLx;

use super::{
  GraphqlxLosslessError, GraphqlxLosslessInput, GraphqlxLosslessLexer, GraphqlxLosslessToken,
};

// ---------------------------------------------------------------------------------------------
// The six atoms are `crate::lossless::trivia`'s, wrapped here with this dialect's four aliases
// pinned.
//
// **Wrappers, not `pub use` re-exports**, for the reason GraphQL's twin records: a re-export would
// force every call site's turbofish to become
// `expect::<GraphqlxLosslessLexer<'inp, Src>, Ctx, GraphQLx>(…)`. The wrappers add no behaviour;
// each is a single delegating call, and `#[inline]` makes the indirection free.
//
// The where-bundles below are the *dialect-bound* spellings, and they are longer than the
// substrate's on purpose. `GraphqlxLosslessToken<'inp, Src>: Token<'inp> + …` and the
// `Lexer<Token = …>` equality are not implied over a generic `Src`, because `LosslessToken<S>`
// receives its `Token` impl from `smear-lexer`'s `token!` macro **once per concrete slice type** —
// unlike `SyntacticToken<S>`, whose impl is generic. Generically, `L::Token` simply *is* the
// lexer's associated type, so the substrate needs neither clause; here they are what makes the
// projection normalize.
// ---------------------------------------------------------------------------------------------

/// The kind projection the atoms compare against, re-exported so `recover.rs`'s restart predicate
/// can adapt a token to a kind without naming the projection twice.
pub(crate) use crate::lossless::trivia::kind_of;

/// [`crate::lossless::trivia::peek_kind`] over this dialect's input.
#[inline]
pub(crate) fn peek_kind<'inp, Src, Ctx>(
  inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>,
) -> Result<
  Option<<GraphqlxLosslessToken<'inp, Src> as Token<'inp>>::Kind>,
  GraphqlxLosslessError<'inp, Src, Ctx>,
>
where
  Src: Source<usize> + ?Sized,
  GraphqlxLosslessToken<'inp, Src>: Token<'inp> + FromLogos<'inp>,
  GraphqlxLosslessLexer<'inp, Src>:
    Lexer<'inp, Token = GraphqlxLosslessToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: tokora::ParseContext<'inp, GraphqlxLosslessLexer<'inp, Src>, GraphQLx>,
  GraphqlxLosslessError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLx>>,
{
  crate::lossless::trivia::peek_kind(inp)
}

/// [`crate::lossless::trivia::peek_as`] over this dialect's input.
#[inline]
pub(crate) fn peek_as<'inp, Src, Ctx, Projection>(
  inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>,
) -> Result<Option<Projection>, GraphqlxLosslessError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxLosslessToken<'inp, Src>: Token<'inp> + FromLogos<'inp> + DowncastRef<Projection>,
  GraphqlxLosslessLexer<'inp, Src>:
    Lexer<'inp, Token = GraphqlxLosslessToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: tokora::ParseContext<'inp, GraphqlxLosslessLexer<'inp, Src>, GraphQLx>,
  GraphqlxLosslessError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLx>>,
{
  crate::lossless::trivia::peek_as(inp)
}

/// [`crate::lossless::trivia::expect`] over this dialect's input.
#[inline]
pub(crate) fn expect<'inp, Src, Ctx>(
  inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>,
  kind: <GraphqlxLosslessToken<'inp, Src> as Token<'inp>>::Kind,
) -> Result<(), GraphqlxLosslessError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxLosslessToken<'inp, Src>: Token<'inp> + FromLogos<'inp> + Clone,
  GraphqlxLosslessLexer<'inp, Src>:
    Lexer<'inp, Token = GraphqlxLosslessToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: tokora::ParseContext<'inp, GraphqlxLosslessLexer<'inp, Src>, GraphQLx>,
  GraphqlxLosslessError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLx>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlxLosslessToken<'inp, Src>,
        <GraphqlxLosslessToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQLx,
      >,
    >,
{
  crate::lossless::trivia::expect(inp, kind)
}

/// [`crate::lossless::trivia::eat_if`] over this dialect's input.
#[inline]
pub(crate) fn eat_if<'inp, Src, Ctx>(
  inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>,
  kind: <GraphqlxLosslessToken<'inp, Src> as Token<'inp>>::Kind,
) -> Result<bool, GraphqlxLosslessError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxLosslessToken<'inp, Src>: Token<'inp> + FromLogos<'inp>,
  GraphqlxLosslessLexer<'inp, Src>:
    Lexer<'inp, Token = GraphqlxLosslessToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: tokora::ParseContext<'inp, GraphqlxLosslessLexer<'inp, Src>, GraphQLx>,
{
  crate::lossless::trivia::eat_if(inp, kind)
}

/// [`crate::lossless::trivia::try_eat`] over this dialect's input.
#[inline]
pub(crate) fn try_eat<'inp, Src, Ctx>(
  inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>,
  kind: <GraphqlxLosslessToken<'inp, Src> as Token<'inp>>::Kind,
) -> Result<ParseAttempt<()>, GraphqlxLosslessError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxLosslessToken<'inp, Src>: Token<'inp> + FromLogos<'inp>,
  GraphqlxLosslessLexer<'inp, Src>:
    Lexer<'inp, Token = GraphqlxLosslessToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: tokora::ParseContext<'inp, GraphqlxLosslessLexer<'inp, Src>, GraphQLx>,
{
  crate::lossless::trivia::try_eat(inp, kind)
}

/// Drivers that run one atom over a `&str` and report what it committed.
///
/// Test-only scaffolding, exported so the integration test at
/// `tests/lossless_x_trivia_atoms.rs` can reach it; nothing in the crate calls it.
///
/// **This module is where the atoms stop being generic.** The atoms above name only the
/// projections; these drivers must choose a concrete source, emitter and context to build a
/// `Sink` at all, exactly as [`super::runner::parse_str`] does.
///
/// There are five, one per atom, where GraphQL's twin has three. The two extra —
/// `peek_keyword_of` and `try_eat_bang_of` — cover `peek_as` and `try_eat`, which GraphQL leaves
/// to be exercised by its productions. GraphQLx has no productions yet, and an atom whose only
/// witness is a production that does not exist is an atom with no witness.
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

  use crate::graphqlx::{GraphQLx, kinds::SyntaxKind as K};

  use super::super::{GraphqlxLosslessErrors, GraphqlxLosslessLexer, SyntaxNode, runner::profile};

  /// The kind vocabulary the atoms answer in — the lexer's, not the tree's.
  pub type Kind = smear_lexer::graphqlx::lossless::LosslessTokenKind;

  /// The contextual-keyword projection `peek_as` downcasts to.
  pub type Keyword = smear_lexer::graphqlx::ContextualKeyword;

  /// The recording emitter, the sink over it, the context pair, and the input the driver's
  /// closure receives.
  ///
  /// Spelled out because a closure's parameter type is **not** inferred through a `ParseInput`
  /// bound — only through an `Fn` bound — so `|inp: &mut _|` leaves `L` and `Ctx` unresolved and
  /// the body's first method call is the error site. `runner::parse_str` never hits this: it
  /// applies a named function whose own signature pins both.
  type Emitter<'inp> = Verbose<GraphqlxLosslessErrors<&'inp str>, SimpleSpan, GraphQLx>;
  type TestSink<'inp> = Sink<'inp, GraphqlxLosslessLexer<'inp, str>, Emitter<'inp>>;
  type TestCtx<'inp> = (
    TestSink<'inp>,
    DefaultCache<'inp, GraphqlxLosslessLexer<'inp, str>>,
  );
  type TestInput<'inp, 'input> =
    InputRef<'inp, 'input, GraphqlxLosslessLexer<'inp, str>, TestCtx<'inp>, GraphQLx>;

  /// Runs one atom over `src` inside a `Document` node, drains whatever the atom left, and
  /// materializes.
  ///
  /// Two details carry the whole file's evidence:
  ///
  /// - **The atom runs inside a node**, so "what the atom committed" is a thing the *tree* can be
  ///   asked — it is that node's text — rather than a cursor offset the sink never saw. A skip
  ///   that fails to commit produces a shorter node.
  /// - **The remainder is drained afterwards.** `Sink::finish` refuses any source byte that no
  ///   committed token covers and no lexer-error diagnostic explains (`FinishError::UncoveredGap`),
  ///   and an atom deliberately stops mid-source. Without the drain every helper here would fail
  ///   materialization instead of reporting its atom.
  macro_rules! drive {
    ($lt:lifetime, $src:expr, $init:expr, |$inp:ident| $atom:expr) => {{
      let src: &$lt str = $src;
      let out = Cell::new($init);

      let (cst, _) = parse_lossless::<GraphqlxLosslessLexer<$lt, str>, GraphQLx, _, _, _, _>(
        src,
        Default::default(),
        Emitter::default(),
        profile::<str>(),
        DefaultCache::<GraphqlxLosslessLexer<'_, str>>::default(),
        |$inp: &mut TestInput<$lt, '_>| {
          // The `node` combinator's own `mark` / `start_at` / `finish`, spelled out. The
          // combinator wants an inner parser whose error type is pinned, and a closure whose body
          // is `Ok(())` pins nothing — so the wrap is driven directly instead of teaching the test
          // scaffold's inference about a type it never names.
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
  /// `::<str, _>` for the reason `runner::parse_str` spells out: `str` and `&str` both project
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

  /// `super::peek_as` over `src`, projecting to the contextual keyword.
  ///
  /// The atom GraphQLx leans on hardest: `import`, `from`, `as`, `where`, `set` and `map` are all
  /// ordinary identifier tokens, so every one of those dispatches is a downcast and not a kind
  /// comparison. A `None` means either "no token" or "this identifier spells nothing contextual",
  /// which is the flattening the substrate's `peek_as` documents.
  pub fn peek_keyword_of<'inp>(src: &'inp str) -> Option<Keyword> {
    let (keyword, _text) = drive!('inp, src, None, |inp| super::peek_as::<str, _, Keyword>(inp)
      .expect("peek_as must not error over these fixtures"));
    keyword
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

  /// `super::try_eat` for `!` over `src`: did it accept, and what did it commit?
  ///
  /// `!` because that is the retro-wrap probe GraphQLx's type references will spend it on — the
  /// non-null marker, which this dialect folds into the type node it follows rather than wrapping.
  /// The `bool` is `ParseAttempt::Accept`, so a decline and an accept are told apart while the
  /// committed text answers the question a decline still has to answer: did the trivia it crossed
  /// reach the tree?
  pub fn try_eat_bang_of<'inp>(src: &'inp str) -> (bool, String) {
    drive!('inp, src, false, |inp| matches!(
      super::try_eat::<str, _>(inp, Kind::Bang)
        .expect("try_eat must not error over these fixtures"),
      ::tokora::try_parse_input::ParseAttempt::Accept(())
    ))
  }
}
