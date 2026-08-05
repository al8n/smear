//! The per-production hit counter behind `feature = "lossless-coverage"`.
//!
//! # What it is for
//!
//! Each dialect's trivia-injection gate injects trivia at every token boundary of every valid
//! corpus entry and asserts the verdict and the node-kind pre-order are unchanged. That gate
//! passes just as readily over a corpus that exercises three shapes many times as over one that
//! reaches every production — **success and silence read identically**, which is the failure mode
//! a coverage gate exists to close. This module is the measurement: every node bracket a lossless
//! suite opens records its kind, and the gate asserts that every kind a production can open was
//! recorded at least once.
//!
//! # Why the instrumentation is a `node` shim rather than a call in each production
//!
//! The counter has to sit at the ~65 sites where a production opens a node, and a hand-placed
//! `hit(K::X)` at each of them is exactly the kind of bookkeeping that a *new* production forgets
//! — after which the gate reports full coverage of the kinds it knows about and stays silent
//! about the one that was added. So this module's `node` and `node_at` — which exist only under
//! the feature, hence no link — shadow [`tokora::parser::node`] and [`tokora::parser::node_at`],
//! and the production modules import
//! them through their dialect's binding. A production cannot open a node without being counted,
//! because opening a node *is* calling one of these.
//!
//! # What a hit means: the node was opened, not merely attempted
//!
//! The count is bumped on precisely the condition tokora's own wrap fires on — the inner parser
//! returned `Ok` (or `Ok(Accept)` for the declining shape) — by wrapping the *inner* parser
//! rather than the node combinator. Counting entries instead would make the two retro-wrap
//! probes worthless: `node_at` over `try_eat` runs on **every** field and **every** type
//! reference, so a field alias and a non-null marker would report thousands of hits over a corpus
//! with neither. Counting the wrap makes every entry in the report a statement that the corpus
//! produced that node.
//!
//! Two consequences, both deliberate:
//!
//! - a production that runs and then unwinds is **not** counted, which is right: its node never
//!   existed;
//! - a node opened inside a branch that is later rolled back **is** counted, because a
//!   thread-local is not transactional. That over-counts reach and never under-counts it, so it
//!   cannot turn an unreached production into a green.
//!
//! # One tally per kind space, not one tally
//!
//! Rust has no generic statics, so two dialects cannot each own "the" tally. The counter keys a
//! thread-local map by [`KindSpace::NAME`](super::KindSpace::NAME) — which is precisely why
//! `NAME` sits on the kind space rather than on a separate marker. A lane is created on first
//! use, sized by that space's `ALL.len()`.
//!
//! # Off by default, and then not compiled at all
//!
//! Without the feature, a dialect's binding is a plain re-export of tokora's `node`/`node_at`, so
//! the shipped build runs the code it always ran — no wrapper type, no branch, nothing to inline
//! away. The cost is that the instrumented path is only linted and run in the two feature-enabled
//! gate rows; the alternative — a wrapper compiled always and neutered by a `cfg` inside — puts a
//! type the shipped parser does not need into the shipped parser's monomorphization for no gain.

#[cfg(feature = "lossless-coverage")]
pub use counter::{hits, hits_of, reset};

#[cfg(feature = "lossless-coverage")]
pub use instrumented::{Counted, node, node_at};

/// The thread-local tally, one lane per kind space.
#[cfg(feature = "lossless-coverage")]
mod counter {
  use std::{cell::RefCell, collections::BTreeMap, vec, vec::Vec};

  use crate::lossless::KindSpace;

  thread_local! {
    /// One tally per kind space, keyed by [`KindSpace::NAME`].
    ///
    /// A map rather than a `Vec`, because Rust has no generic statics and two dialects therefore
    /// cannot each own "the" tally. Thread-local rather than global because the harness runs each
    /// `#[test]` on its own thread: a shared counter would make one gate's coverage depend on
    /// whichever other tests happened to run first, which is the reverse of a measurement.
    static HITS: RefCell<BTreeMap<&'static str, Vec<u32>>> = const { RefCell::new(BTreeMap::new()) };
  }

  /// Records one opened node of `raw` in `K`'s lane.
  ///
  /// Out-of-space raw values are dropped rather than panicking: the sink's own kind validator
  /// already refuses them at the emit door, and a counter is not the place to add a second,
  /// weaker copy of that check.
  #[inline]
  pub(super) fn hit<K: KindSpace>(raw: u16) {
    HITS.with(|hits| {
      let mut hits = hits.borrow_mut();
      let lane = hits.entry(K::NAME).or_insert_with(|| vec![0; K::ALL.len()]);
      if let Some(slot) = lane.get_mut(raw as usize) {
        *slot += 1;
      }
    });
  }

  /// Clears `K`'s lane on the calling thread. Call it at the start of a measurement.
  ///
  /// Leaves every other lane alone, which is the property that makes the tally per-dialect.
  pub fn reset<K: KindSpace>() {
    HITS.with(|hits| {
      if let Some(lane) = hits.borrow_mut().get_mut(K::NAME) {
        lane.iter_mut().for_each(|slot| *slot = 0);
      }
    });
  }

  /// `K`'s lane on the calling thread, indexed by raw kind — `hits::<K>()[k.raw() as usize]` is
  /// how often a node of kind `k` was opened since the last [`reset`].
  ///
  /// A lane that has never been touched reads as all-zero rather than empty, so a caller does
  /// not have to tell "no hits" from "no lane".
  pub fn hits<K: KindSpace>() -> Vec<u32> {
    HITS.with(|hits| {
      hits
        .borrow()
        .get(K::NAME)
        .cloned()
        .unwrap_or_else(|| vec![0; K::ALL.len()])
    })
  }

  /// How often a node of `kind` was opened in `K`'s lane since the last [`reset`].
  pub fn hits_of<K: KindSpace>(kind: K) -> u32 {
    HITS.with(|hits| {
      hits
        .borrow()
        .get(K::NAME)
        .and_then(|lane| lane.get(kind.raw() as usize))
        .copied()
        .unwrap_or_default()
    })
  }
}

/// The counting shims: [`tokora::parser::node`] and [`tokora::parser::node_at`] with the inner
/// parser wrapped so a successful sub-parse — the exact condition tokora wraps on — is recorded.
#[cfg(feature = "lossless-coverage")]
mod instrumented {
  use core::marker::PhantomData;

  use tokora::{
    Emitter, InputRef, Lexer, ParseContext, ParseInput, TryParseInput,
    cst::event::EventMark,
    parser::{Node, NodeAt},
    try_parse_input::ParseAttempt,
  };

  use crate::lossless::KindSpace;

  /// [`tokora::parser::node`], counting the wrap in `K`'s lane.
  #[inline]
  pub fn node<K: KindSpace, P>(kind: u16, parser: P) -> Node<Counted<K, P>> {
    tokora::parser::node(kind, Counted::new(kind, parser))
  }

  /// [`tokora::parser::node_at`], counting the wrap in `K`'s lane.
  #[inline]
  pub fn node_at<K: KindSpace, P>(mark: EventMark, kind: u16, parser: P) -> NodeAt<Counted<K, P>> {
    tokora::parser::node_at(mark, kind, Counted::new(kind, parser))
  }

  /// A node's inner parser, plus the kind the wrap around it will carry and the lane it belongs
  /// to.
  ///
  /// Wrapping *inside* the node combinator rather than around it is what keeps the count and the
  /// wrap on one condition: tokora wraps iff the inner parser returned `Ok` / `Ok(Accept)`, and
  /// that is this type's own success test, one frame lower. There is no second predicate to drift
  /// out of step with the first.
  pub struct Counted<K, P> {
    kind: u16,
    parser: P,
    space: PhantomData<fn() -> K>,
  }

  impl<K, P> Counted<K, P> {
    #[inline]
    fn new(kind: u16, parser: P) -> Self {
      Self {
        kind,
        parser,
        space: PhantomData,
      }
    }
  }

  impl<'inp, L, O, Ctx, Lang, K, P> ParseInput<'inp, L, O, Ctx, Lang> for Counted<K, P>
  where
    Lang: ?Sized,
    K: KindSpace,
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
        super::counter::hit::<K>(self.kind);
      }
      res
    }
  }

  impl<'inp, L, O, Ctx, Lang, K, P> TryParseInput<'inp, L, O, Ctx, Lang> for Counted<K, P>
  where
    Lang: ?Sized,
    K: KindSpace,
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
        super::counter::hit::<K>(self.kind);
      }
      res
    }
  }
}
