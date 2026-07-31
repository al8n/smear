//! Recovery: a grammar error becomes a diagnostic and, where there is anything to attribute,
//! an `Error` node; the parse then continues. `finish` gap-tiles anything left uncovered, so
//! text fidelity holds regardless.
//!
//! Built on tokora's sync family, not on hand-rolled skip loops. The members behave
//! differently and the differences decide which one each helper uses:
//!
//! - `sync_balanced(classifier, pred)` — **two** args: `classifier` names which kinds open and
//!   close pairs (`DelimClass`), `pred` is the depth-0 sync predicate. On a **successful** sync
//!   that skipped ≥1 token it reports the region once through `emit_skipped_region`, and the
//!   sink wraps those tokens in the profile's `error_kind` **automatically** — no explicit
//!   `node` call.
//! - `sync_balanced` **makes no progress in two cases**, and both are reachable here. A
//!   no-match run to end of input "commits nothing and returns `Ok(None)`, leaving no trace";
//!   and a *successful* sync whose predicate matches the very first token returns
//!   `Some(Hole { skipped: 0 })` — the sync point was already at hand — consuming nothing and
//!   emitting nothing. See `unexpected` for why that pair is a termination hazard rather than
//!   a curiosity.
//! - `sync_to` / `sync_through` — also two args, and they do **not** auto-wrap, so a caller
//!   that wants the skipped tokens inside a node must open one itself. Task 8's top-level
//!   resync is their caller; nothing in Task 5 reaches them, and the plan's
//!   `resync_to_definition` is therefore deferred to the task that can test it at its own call
//!   site.
//!
//! # Every helper called from inside a loop must consume at least one token
//!
//! A helper that returns `Ok` without consuming turns its caller's `while` into an infinite
//! loop. `unexpected` is in that class and guarantees progress explicitly; `unclosed_list` and
//! `unclosed_object` are not, because their call sites `return` out of the loop rather than
//! falling through it. The distinction is not "does it consume" but "does the call site
//! continue".
//!
//! **Do not write `node(K::Error.raw(), |_| Ok(()))`.** It is a no-op: an empty, zero-width
//! `Error` node that consumes nothing, which is the rule above violated in the one place it
//! looks like recovery.

use smear_lexer::graphql::lossless::LosslessTokenKind as Kind;
use tokora::{
  Emitter as _, SimpleSpan,
  emitter::{CstEmitter as _, FromUnclosed},
  error::{UnclosedBrace, UnclosedBracket, UnexpectedEot, token::UnexpectedToken},
  input::Balance,
  span::Spanned,
};

use crate::graphql::{GraphQL, kinds::SyntaxKind as K};

use super::{GraphqlLosslessError, GraphqlLosslessLexer, trivia::kind_of};

/// The token kinds a `Value` may begin with — what an "expected a value" diagnostic names.
///
/// `true`, `false` and `null` are `Identifier`s to the lexer, so they need no entry of their
/// own; the value dispatcher separates them on their spelling, not on their kind.
pub(crate) const VALUE_HEADS: &[Kind] = &[
  Kind::Dollar,
  Kind::Int,
  Kind::Float,
  Kind::InlineString,
  Kind::BlockString,
  Kind::LBracket,
  Kind::LBrace,
  Kind::Identifier,
];

/// The token kinds an `ObjectField` may begin with.
pub(crate) const OBJECT_FIELD_HEADS: &[Kind] = &[Kind::Identifier];

/// Where a value-position recovery is willing to stop: a token that could start a value, or a
/// closer the enclosing shape knows how to consume.
///
/// The closers are in the set even though no value starts with one — stopping *before* `]` is
/// what lets an enclosing `list_value` close on its own delimiter instead of running to end of
/// input. `sync_balanced` consults this only at depth zero, so a `]` inside skipped nesting is
/// crossed rather than mistaken for the enclosing one.
#[inline]
fn is_sync_point(kind: Kind) -> bool {
  matches!(
    kind,
    Kind::Dollar
      | Kind::Int
      | Kind::Float
      | Kind::InlineString
      | Kind::BlockString
      | Kind::LBracket
      | Kind::LBrace
      | Kind::Identifier
      | Kind::RBracket
      | Kind::RBrace
      | Kind::RParen
  )
}

/// GraphQL's three delimiter pairs, for `sync_balanced`'s depth counting.
///
/// The pair identity is a `u8` rather than a bespoke enum because the balanced scan is
/// deliberately pair-blind — it counts depth and never checks that a closer matches its
/// opener — so the identity is only ever compared for equality, and never by this suite.
#[inline]
fn delimiters(kind: &Kind) -> Balance<u8> {
  match kind {
    Kind::LParen => Balance::Open(b'('),
    Kind::RParen => Balance::Close(b'('),
    Kind::LBracket => Balance::Open(b'['),
    Kind::RBracket => Balance::Close(b'['),
    Kind::LBrace => Balance::Open(b'{'),
    Kind::RBrace => Balance::Close(b'{'),
    _ => Balance::Neutral,
  }
}

lossless_production! {
  /// A list ran to end of input before its `]` arrived.
  ///
  /// `open` is the opening `[`'s span, which is where the diagnostic points — the closer that
  /// never came has no position of its own.
  ///
  /// **This helper opens no node and consumes nothing, and that is correct rather than a
  /// shortcoming.** Its only caller reaches it when the atom set reported end of input, so
  /// there is no token left to skip, nothing to attribute to an `Error` node, and nothing for
  /// `sync_balanced` to settle: at end of input it would commit nothing, wrap nothing and emit
  /// no hole diagnostic. Calling it here would be dead code that *reads* as the mechanism.
  ///
  /// **Loop safety does not depend on consuming here.** The caller's `while` is guarded by an
  /// end-of-input test and this helper's result is `return`ed out of that loop rather than
  /// continuing it, so there is no iteration to starve. The missing closer's absence is
  /// recorded in the diagnostic; the source bytes are already accounted for by the tokens
  /// committed before end of input.
  fn unclosed_list<'inp, Src, Ctx>(inp, open: SimpleSpan) {
    let err = <GraphqlLosslessError<'inp, Src, Ctx> as FromUnclosed<
      'inp,
      GraphqlLosslessLexer<'inp, Src>,
      GraphQL,
    >>::from_unclosed(UnclosedBracket::<SimpleSpan, GraphQL>::bracket_of(open));
    inp.emitter().emit_error(Spanned::new(open, err))?;
    Ok(())
  }

  /// An object ran to end of input before its `}` arrived — `unclosed_list`'s twin, and the
  /// same reasoning applies clause for clause.
  fn unclosed_object<'inp, Src, Ctx>(inp, open: SimpleSpan) {
    let err = <GraphqlLosslessError<'inp, Src, Ctx> as FromUnclosed<
      'inp,
      GraphqlLosslessLexer<'inp, Src>,
      GraphQL,
    >>::from_unclosed(UnclosedBrace::<SimpleSpan, GraphQL>::brace_of(open));
    inp.emitter().emit_error(Spanned::new(open, err))?;
    Ok(())
  }

  /// Nothing that could start one of `expected` is here, and there is still input.
  ///
  /// Reports once, then makes progress — in that order, because the diagnostic names the
  /// token that is about to be skipped.
  ///
  /// # Why this cannot be `sync_balanced` alone
  ///
  /// The plan's draft was a bare `inp.sync_balanced(…)`, on the reasoning that a balanced skip
  /// both makes progress and reports itself. It does neither reliably, and the two gaps are
  /// exactly the inputs a recovery path meets:
  ///
  /// - **A stray closer is itself a sync point.** At depth zero `pred` is consulted *first*,
  ///   so over `[1 ) 2]` the scan matches the `)` the caller is standing on, returns
  ///   `Some(Hole { skipped: 0 })` and consumes nothing. The caller's `while` then re-reads
  ///   that same `)` — forever.
  /// - **Garbage running to end of input never matches.** Over `[1 ! ! !` there is no sync
  ///   point, so the scan rewinds wholesale and returns `Ok(None)`. Same spin.
  ///
  /// So the skip is *attempted first* — it is the good outcome: one hole diagnostic for a
  /// whole garbage run, nesting-aware, wrapped by the sink itself — and a fallback consumes
  /// exactly one token into an `Error` node when the skip made no progress. Progress is then
  /// unconditional whenever input remains, which is the enclosing loop's whole safety
  /// argument.
  ///
  /// The fallback node is opened **only once the token is in hand**, by hand rather than
  /// through the `node` combinator, so a caller that reaches this at genuine end of input gets
  /// no node at all rather than an empty zero-width `Error` one.
  fn unexpected<'inp, Src, Ctx>(inp, expected: &'static [Kind]) {
    // `Clone::clone(t.data)`, not `t.data.clone()`: `Spanned<&Token, &Span>`'s `data` field is
    // already a reference, so the method form resolves to `<&Token as Clone>::clone` and hands
    // back another borrow — which then infers `UnexpectedToken`'s `T` as `&Token` and fails the
    // `From` bound a long way from here.
    match inp.peek_head_map(|t| Spanned::new(*t.span, Clone::clone(t.data)))? {
      Some(found) => {
        let span = found.span;
        let err = UnexpectedToken::<_, _, _, GraphQL>::expected_one_of(span, expected)
          .with_found(found.data);
        inp.emitter().emit_error(Spanned::new(span, err.into()))?;
      }
      None => {
        let end = inp.span().end();
        let span = SimpleSpan::new(end, end);
        let err = UnexpectedEot::<usize, GraphQL>::eot_of(end);
        inp.emitter().emit_error(Spanned::new(span, err.into()))?;
      }
    }

    let hole = inp.sync_balanced(delimiters, |t| is_sync_point(kind_of(t.data)))?;
    if hole.is_some_and(|h| h.skipped() > 0) {
      // The sink wrapped the skipped region in the profile's `error_kind` on its own.
      return Ok(());
    }

    let mark = inp.emitter().cst_mark();
    if inp.try_expect(|_| true)?.is_some() {
      inp.emitter().cst_start_at(mark, K::Error.raw());
      inp.emitter().cst_finish(K::Error.raw());
    }
    Ok(())
  }
}
