//! The dialect-generic lossless substrate.
//!
//! # Why this module exists at the crate root
//!
//! The crate's own precedent (`combinator`, `name`, `ty`, `value`, `selection`, …) is
//! **dialect-generic building blocks at the crate root, dialect-specific assembly under
//! `graphql/` and `graphqlx/`, and no cross-dialect imports in either direction.** Phase A's
//! lossless layer did not follow it — it was a self-contained, dialect-bound tower — and Phase B's
//! design measured what that costs a second dialect: roughly 2,100 lines that would be duplicated
//! for no reason, on top of the ~2,100 that genuinely differ.
//!
//! # What belongs here, and what does not
//!
//! **Here:** anything whose behaviour is independent of the grammar — the kind-space contract, the
//! trivia atoms, the `Parse`/`Diagnostic` surface, the coverage shims, the typed-wrapper substrate.
//!
//! **Not here:** anything that names a concrete dialect token type, kind enum, keyword enum or
//! error type. That is the Lego rule, and `tests/lossless_isolation.rs` (gate 6) mechanises it.
//!
//! Phase B is **option B3**: this substrate is lifted, the productions are forked. The measurement
//! that decided it is in the design document; the short version is that a shared production needs
//! ~26 trait members against a budget of 8, and no vocabulary change reaches that.

// Each of the four carries its own `//!` header, and none of them carries an outer `///` here.
// That is deliberate and it is a bug fix, not a style choice: when a module has doc fragments from
// both places, rustdoc resolves the intra-doc links in *all* of them against the scope of the
// first fragment — this file's — so `[`node`]` inside `coverage.rs` looked for `lossless::node`
// and every such link silently broke. One fragment, one scope, links that resolve where they are
// written.

pub mod ast;

pub mod coverage;

// Gated on there being a dialect assembly to consume them. `rowan` alone — which is what
// `cargo hack --each-feature` builds, and what `lossless-coverage` implies — compiles this
// substrate with no dialect in the crate, and three macros nobody invokes are three
// `unused_macros` denials. The cfg is `any(…)` rather than `graphql` because GraphQLx's assembly
// is the second consumer.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod macros;

pub mod recover;

pub mod runner;

pub mod trivia;

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) use macros::{
  description_head_predicate, directive_location_predicate, lossless_drivers, lossless_error_impls,
  lossless_production,
};

/// What a dialect's syntax-kind space must provide for the shared runner, coverage counter and
/// typed layer to work over it.
///
/// # Why a trait rather than a set of arguments
///
/// The three bookkeeping kinds and the raw round-trip are needed in four unrelated places — the
/// sink profile, the kind validator, the coverage tally's width, and `rowan::Language` — and
/// threading four values through four call chains is how they drift apart. Naming them once is
/// also what lets `test_support::assert_kind_space_is_well_formed` — behind
/// `feature = "test-support"` — be written once instead of three times per dialect.
///
/// The reference is deliberately **not** an intra-doc link: the item it names is compiled only
/// under that feature, and a link to a `cfg`-gated item is a `broken_intra_doc_links` warning in
/// every build that does not enable it. `tests/lossless_isolation.rs` records the same failure
/// mode from the other direction — a doc link is a compile-time reference, and gating the item
/// without gating the link moves the breakage into `cargo doc`.
///
/// # The invariants, and which one is load-bearing for what
///
/// - [`raw`](Self::raw) is the declaration index, so [`from_raw`](Self::from_raw) can index
///   [`ALL`](Self::ALL). This is what makes the sink's kind validator a bounds check rather than a
///   match.
/// - The bookkeeping triple is the **last three** kinds, in the order [`ERROR`](Self::ERROR),
///   [`GAP`](Self::GAP), [`ROOT`](Self::ROOT). The shared runner is parameterised over them; a
///   dialect that appends a content kind after `Root` breaks the coverage tally's width and the
///   validator's upper bound at once.
/// - Nothing may occupy `u16::MAX`, which tokora reserves crate-wide as a tombstone.
pub trait KindSpace: Copy + Eq + core::fmt::Debug + 'static {
  /// The dialect's short name — `"graphql"`, `"graphqlx"`.
  ///
  /// Keys the coverage tally's lane and names the space in the validator's panic. It is on the
  /// *kind space* rather than on a separate marker because the kind space is the thing that is
  /// per-dialect in every one of those places.
  const NAME: &'static str;

  /// The recovery-hole kind the sink materializes a skipped region as.
  const ERROR: Self;

  /// The gap-tile kind the sink materializes uncovered bytes as.
  const GAP: Self;

  /// The synthetic document root `finish` is named at.
  const ROOT: Self;

  /// Every kind, in declaration order. [`from_raw`](Self::from_raw) indexes this by raw value.
  const ALL: &'static [Self];

  /// The raw `u16` the event channel and the sink speak.
  fn raw(self) -> u16;

  /// The fallible inverse of [`raw`](Self::raw).
  ///
  /// `rowan::Language::kind_from_raw` has no fallible form, which is why tokora carries the kind
  /// validator as profile data. This is the door that validator uses.
  fn from_raw(raw: u16) -> Option<Self>;
}

/// The conformance assertion for a [`KindSpace`] impl, behind `feature = "test-support"`.
///
/// # Why this one is documented where the dialects' scaffolding is hidden
///
/// [`KindSpace`] is a public, unsealed trait, and it is the substrate's extension point: the
/// module header's whole claim is that these pieces are usable without naming a dialect, and a
/// third dialect — in this crate or outside it — becomes one by implementing it. Its invariants
/// are stated in prose four paragraphs up and enforced nowhere else, which is precisely the shape
/// that a downstream implementor gets wrong and finds out about as a panic inside the sink's kind
/// validator. So the check ships, as something a consumer may opt into, rather than being hidden
/// from the docs of the trait it belongs to.
///
/// Everything under a *dialect's* `lossless` module named `test_support` is the opposite case —
/// drivers over one concrete grammar, several of which build deliberately corrupt trees — and
/// stays `#[doc(hidden)]` under the same feature.
///
/// The whole module is compiled only under the feature; nothing in a default build, and nothing
/// in a plain `rowan` build, contains it.
#[cfg(feature = "test-support")]
#[cfg_attr(docsrs, doc(cfg(feature = "test-support")))]
pub mod test_support {
  use super::KindSpace;

  /// Assert `K` satisfies every invariant [`KindSpace`] states.
  ///
  /// Written once so a second dialect inherits the checks by declaring the impl, rather than by
  /// someone remembering to copy three `#[test]`s into a new `kinds.rs`. That copying is exactly
  /// the failure Phase B's "derived, not diffed" requirement is about.
  pub fn assert_kind_space_is_well_formed<K: KindSpace>() {
    assert!(
      !K::ALL.is_empty(),
      "{}: the kind space is empty, so every loop below is vacuous",
      K::NAME
    );

    for (index, kind) in K::ALL.iter().enumerate() {
      assert_eq!(
        kind.raw(),
        index as u16,
        "{}: declaration index drifted at {kind:?}",
        K::NAME
      );
      assert_ne!(
        kind.raw(),
        u16::MAX,
        "{}: {kind:?} collides with tokora's tombstone",
        K::NAME
      );
      assert_eq!(
        K::from_raw(kind.raw()),
        Some(*kind),
        "{}: {kind:?} did not round-trip through raw {}",
        K::NAME,
        kind.raw()
      );
    }

    let content = K::ALL.len() as u16 - 3;
    assert_eq!(
      K::ERROR.raw(),
      content,
      "{}: ERROR is not third-from-last",
      K::NAME
    );
    assert_eq!(
      K::GAP.raw(),
      content + 1,
      "{}: GAP is not second-from-last",
      K::NAME
    );
    assert_eq!(K::ROOT.raw(), content + 2, "{}: ROOT is not last", K::NAME);

    assert_eq!(
      K::from_raw(K::ALL.len() as u16),
      None,
      "{}: from_raw admitted a kind one past the end",
      K::NAME
    );
  }
}
