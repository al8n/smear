//! This dialect's binding of [`crate::parser::lossless::coverage`].
//!
//! The seven production modules import `node` and `node_at` from here, so a production cannot
//! open a node without being counted — a hand-placed `hit(K::X)` at ~65 sites is the bookkeeping
//! a *new* production forgets, after which the gate reports full coverage of the kinds it knows
//! about. Everything about *why* the counter has the shape it does is in the substrate's module
//! docs; what is here is the choice of lane.
//!
//! The re-export path is unchanged from Phase A, deliberately: no production's `use` statement
//! moved, so the whole of this lift is invisible to `value.rs` and its six siblings.

#[cfg(not(feature = "lossless-coverage"))]
pub(crate) use tokora::parser::{node, node_at};

#[cfg(feature = "lossless-coverage")]
pub use bound::{hits, hits_of, reset};

#[cfg(feature = "lossless-coverage")]
pub(crate) use bound::{node, node_at};

#[cfg(feature = "lossless-coverage")]
mod bound {
  use tokora::cst::event::EventMark;

  use crate::parser::{graphql::kinds::SyntaxKind as K, lossless::coverage};

  /// [`tokora::parser::node`], counting the wrap in this dialect's lane.
  #[inline]
  pub(crate) fn node<P>(kind: u16, parser: P) -> tokora::parser::Node<coverage::Counted<K, P>> {
    coverage::node::<K, P>(kind, parser)
  }

  /// [`tokora::parser::node_at`], counting the wrap in this dialect's lane.
  #[inline]
  pub(crate) fn node_at<P>(
    mark: EventMark,
    kind: u16,
    parser: P,
  ) -> tokora::parser::NodeAt<coverage::Counted<K, P>> {
    coverage::node_at::<K, P>(mark, kind, parser)
  }

  /// Clears this dialect's lane on the calling thread. Call it at the start of a measurement.
  pub fn reset() {
    coverage::reset::<K>();
  }

  /// This dialect's lane on the calling thread, indexed by raw kind.
  pub fn hits() -> std::vec::Vec<u32> {
    coverage::hits::<K>()
  }

  /// How often a node of `kind` was opened in this dialect's lane since the last [`reset`].
  pub fn hits_of(kind: K) -> u32 {
    coverage::hits_of::<K>(kind)
  }
}
