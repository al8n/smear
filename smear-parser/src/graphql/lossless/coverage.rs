//! The per-production hit counter behind `feature = "lossless-coverage"`.
//!
//! # What it is for
//!
//! `tests/lossless_trivia.rs` (gate 2) injects trivia at every token boundary of every valid
//! corpus entry and asserts the verdict and the node-kind pre-order are unchanged. That gate
//! passes just as readily over a corpus that exercises three shapes many times as over one that
//! reaches every production — **success and silence read identically**, which is the failure mode
//! a coverage gate exists to close. This module is the measurement: every node bracket the
//! lossless suite opens records its kind, and the gate asserts that every kind a production can
//! open was recorded at least once.
//!
//! # Why the instrumentation is a `node` shim rather than a call in each production
//!
//! The counter has to sit at the ~65 sites where a production opens a node, and a hand-placed
//! `hit(K::X)` at each of them is exactly the kind of bookkeeping that a *new* production forgets
//! — after which the gate reports full coverage of the kinds it knows about and stays silent
//! about the one that was added. So this module's own `node` and `node_at` shadow
//! [`tokora::parser::node`] and [`tokora::parser::node_at`], and the production modules import
//! them from here. A production cannot open a node without being counted, because opening a node
//! *is* calling one of these.
//!
//! # What a hit means: the node was opened, not merely attempted
//!
//! The count is bumped on precisely the condition tokora's own wrap fires on — the inner parser
//! returned `Ok` (or `Ok(Accept)` for the declining shape) — by wrapping the *inner* parser
//! rather than the node combinator. Counting entries instead would make the two retro-wrap
//! probes worthless: `node_at` over `try_eat` runs on **every** field and **every** type
//! reference, so `Alias` and `NonNullType` would report thousands of hits over a corpus with no
//! alias and no `!` in it. Counting the wrap makes every entry in the report a statement that
//! the corpus produced that node.
//!
//! Two consequences, both deliberate:
//!
//! - a production that runs and then unwinds is **not** counted, which is right: its node never
//!   existed (`lossless_parity.rs`'s `a_verdict_gate_is_blind_to_a_lost_definition_node` is the
//!   recorded case);
//! - a node opened inside a branch that is later rolled back **is** counted, because a
//!   thread-local is not transactional. That over-counts reach and never under-counts it, so it
//!   cannot turn an unreached production into a green.
//!
//! # Off by default, and then not compiled at all
//!
//! Without the feature, `node` and `node_at` are plain re-exports of tokora's, so the shipped
//! build runs the code it always ran — no wrapper type, no branch, nothing to inline away. The
//! cost is that the instrumented path is only linted and run in the two feature-enabled gate rows
//! (Task 14's `test_coverage` and `clippy_cov`); the alternative — a wrapper compiled always and
//! neutered by a `cfg` inside — puts a type the shipped parser does not need into the shipped
//! parser's monomorphization for no gain.

#[cfg(not(feature = "lossless-coverage"))]
pub(crate) use tokora::parser::{node, node_at};

#[cfg(feature = "lossless-coverage")]
pub use counter::{hits, hits_of, reset};

#[cfg(feature = "lossless-coverage")]
pub(crate) use instrumented::{node, node_at};

/// The thread-local tally, indexed by [`SyntaxKind::raw`](crate::graphql::kinds::SyntaxKind::raw).
///
/// Thread-local rather than global because the test harness runs each `#[test]` on its own
/// thread: a shared counter would make one gate's coverage depend on whichever other tests
/// happened to run first, which is the reverse of a measurement.
#[cfg(feature = "lossless-coverage")]
mod counter {
  use std::{cell::RefCell, vec, vec::Vec};

  use crate::graphql::kinds::SyntaxKind;

  thread_local! {
    static HITS: RefCell<Vec<u32>> = RefCell::new(vec![0; SyntaxKind::ALL.len()]);
  }

  /// Records one opened node of `raw`.
  ///
  /// Out-of-space raw values are dropped rather than panicking: the sink's own kind validator
  /// already refuses them at the emit door, and a counter is not the place to add a second,
  /// weaker copy of that check.
  #[inline]
  pub(super) fn hit(raw: u16) {
    HITS.with(|hits| {
      if let Some(slot) = hits.borrow_mut().get_mut(raw as usize) {
        *slot += 1;
      }
    });
  }

  /// Clears the calling thread's tally. Call it at the start of a measurement.
  pub fn reset() {
    HITS.with(|hits| hits.borrow_mut().iter_mut().for_each(|slot| *slot = 0));
  }

  /// The calling thread's tally, indexed by raw kind — `hits()[k.raw() as usize]` is how often a
  /// node of kind `k` was opened since the last [`reset`].
  pub fn hits() -> Vec<u32> {
    HITS.with(|hits| hits.borrow().clone())
  }

  /// How often a node of `kind` was opened since the last [`reset`].
  pub fn hits_of(kind: SyntaxKind) -> u32 {
    HITS.with(|hits| {
      hits
        .borrow()
        .get(kind.raw() as usize)
        .copied()
        .unwrap_or_default()
    })
  }
}

/// The counting shims: [`tokora::parser::node`] and [`tokora::parser::node_at`] with the inner
/// parser wrapped so a successful sub-parse — the exact condition tokora wraps on — is recorded.
#[cfg(feature = "lossless-coverage")]
mod instrumented {
  use tokora::{
    Emitter, InputRef, Lexer, ParseContext, ParseInput, TryParseInput,
    cst::event::EventMark,
    parser::{Node, NodeAt},
    try_parse_input::ParseAttempt,
  };

  /// [`tokora::parser::node`], counting the wrap.
  #[inline]
  pub(crate) fn node<P>(kind: u16, parser: P) -> Node<Counted<P>> {
    tokora::parser::node(kind, Counted { kind, parser })
  }

  /// [`tokora::parser::node_at`], counting the wrap.
  #[inline]
  pub(crate) fn node_at<P>(mark: EventMark, kind: u16, parser: P) -> NodeAt<Counted<P>> {
    tokora::parser::node_at(mark, kind, Counted { kind, parser })
  }

  /// A node's inner parser, plus the kind the wrap around it will carry.
  ///
  /// Wrapping *inside* the node combinator rather than around it is what keeps the count and the
  /// wrap on one condition: tokora wraps iff the inner parser returned `Ok` / `Ok(Accept)`, and
  /// that is this type's own success test, one frame lower. There is no second predicate to drift
  /// out of step with the first.
  pub(crate) struct Counted<P> {
    kind: u16,
    parser: P,
  }

  impl<'inp, L, O, Ctx, Lang, P> ParseInput<'inp, L, O, Ctx, Lang> for Counted<P>
  where
    Lang: ?Sized,
    P: ParseInput<'inp, L, O, Ctx, Lang>,
    L: Lexer<'inp>,
    Ctx: ParseContext<'inp, L, Lang>,
  {
    #[inline]
    fn parse_input(
      &mut self,
      input: &mut InputRef<'inp, '_, L, Ctx, Lang>,
    ) -> Result<O, <Ctx::Emitter as Emitter<'inp, L, Lang>>::Error> {
      let res = self.parser.parse_input(input);
      if res.is_ok() {
        super::counter::hit(self.kind);
      }
      res
    }
  }

  impl<'inp, L, O, Ctx, Lang, P> TryParseInput<'inp, L, O, Ctx, Lang> for Counted<P>
  where
    Lang: ?Sized,
    P: TryParseInput<'inp, L, O, Ctx, Lang>,
    L: Lexer<'inp>,
    Ctx: ParseContext<'inp, L, Lang>,
  {
    #[inline]
    fn try_parse_input(
      &mut self,
      input: &mut InputRef<'inp, '_, L, Ctx, Lang>,
    ) -> Result<ParseAttempt<O>, <Ctx::Emitter as Emitter<'inp, L, Lang>>::Error> {
      let res = self.parser.try_parse_input(input);
      if matches!(res, Ok(ParseAttempt::Accept(_))) {
        super::counter::hit(self.kind);
      }
      res
    }
  }
}
