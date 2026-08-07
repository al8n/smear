//! The mirror image of the whitelist: places the **oracle** has a check and **smear** does not.
//!
//! # Why this cannot be part of [`Class`](super::Class)
//!
//! A whitelist class excuses smear being *stricter* than apollo. That direction is safe to excuse
//! because it can only ever mean apollo is missing a rule. This module excuses the opposite
//! direction — smear being *laxer* — which is the direction that hides bugs, so it is a separate
//! type with separate rules and no shared vocabulary. Nothing here can be reached by declaring a
//! class on a case, and nothing in the whitelist can be reached by declaring a gap.
//!
//! # What makes a gap admissible
//!
//! Exactly one thing: the rule is **openly not implemented yet**, its absence is scoped to a named
//! draft section, and the divergence is attributable to that section *by apollo's own typed error
//! classification* rather than by a substring of rendered text. A gap is not a tolerance. It is a
//! statement that this build of smear does not claim the rule, and it is only usable while that
//! statement is true.
//!
//! # How a gap closes itself
//!
//! Two mechanisms, both mechanical:
//!
//! * **Liveness.** An executable-stage gap is live only while no [`Rule`] carries its section. The
//!   moment draft 5.3.2 lands as a `Rule`, [`Gap::is_live`] goes false and every merge divergence
//!   it was covering becomes a hard failure again — no edit required for the gate to start
//!   watching.
//! * **Exercise.** Every declared gap must be hit by at least one case on every run. A gap nothing
//!   hits has closed — the corpus still contains the documents, and they now agree — so the gate
//!   fails asking for the declaration to be deleted. That is the half that works for schema-stage
//!   gaps too, where there is no `Rule` to watch.
//!
//! Between them, phase 3 landing draft 5.3.2 needs one deletion from [`GAPS`] and no other change
//! anywhere in the harness.
//!
//! # [`GAPS`] is currently empty, and that is the mechanism working
//!
//! Both declarations this module shipped with have since been deleted by the rules they described
//! landing: draft 5.3.2 by liveness (a [`Rule`] now carries the section) and `3.13-usages` by
//! exercise (`Schema::build` refuses all six probes, so nothing reached the gap any more). Neither
//! deletion was noticed by a reviewer — the gate demanded each of them by name.
//!
//! An empty list is not an invitation to leave the machinery unread. The admission rules above are
//! what the *next* declaration has to satisfy, and the two closure mechanisms are what will delete
//! it again.

use smear::validator::Rule;

/// Which of the two comparisons a gap shows up in.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Stage {
  /// The executable-document comparison: smear accepts a query apollo rejects.
  Executable,
  /// The schema comparison: smear builds an SDL apollo refuses to validate.
  Schema,
}

/// A check `apollo-compiler` performs and this build of smear does not.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Gap {
  /// The draft section, used as the gap's identity in reports and in [`Gap::is_live`].
  pub section: &'static str,
  /// The specification's title for the rule.
  pub title: &'static str,
  /// Where the divergence surfaces.
  pub stage: Stage,
  /// `apollo-compiler`'s internal names for the diagnostics this rule produces.
  ///
  /// Taken from
  /// [`DiagnosticData::unstable_error_name`][apollo_compiler::validation::DiagnosticData::unstable_error_name].
  /// That accessor is `#[doc(hidden)]` and named "unstable", and depending on it is a deliberate
  /// trade: the alternative is matching substrings of an `ariadne`-rendered report, which is both
  /// less precise and *more* fragile — a rendering change moves it, and a message that merely
  /// contains the right words matches by accident. A typed variant name cannot match by accident.
  /// The exposure is bounded by the `=1.32.0` pin in `Cargo.toml`: the names cannot move without a
  /// deliberate bump, and a bump that moves them fails here rather than silently widening what is
  /// excused.
  pub apollo_error_names: &'static [&'static str],
  /// Where the work that closes the gap lives.
  pub tracking: &'static str,
}

impl Gap {
  /// Whether the gap is still open.
  ///
  /// An executable-stage gap closes the moment a [`Rule`] claims its section, which is what makes
  /// the declaration self-limiting. A schema-stage gap has no `Rule` to watch, so it is always
  /// nominally live and relies on the exercise requirement instead.
  pub fn is_live(&self) -> bool {
    match self.stage {
      Stage::Executable => !Rule::ALL.iter().any(|rule| rule.section() == self.section),
      Stage::Schema => true,
    }
  }

  /// Whether every one of a set of apollo error names belongs to this gap.
  ///
  /// Deliberately "every", not "any". A document that trips a merge conflict *and* an undefined
  /// field is a document smear should have rejected for the undefined field, and attributing the
  /// whole verdict to the open gap would lose exactly the finding the harness exists for.
  pub fn covers(&self, names: &[Option<&'static str>]) -> bool {
    !names.is_empty()
      && names
        .iter()
        .all(|name| name.is_some_and(|name| self.apollo_error_names.contains(&name)))
  }
}

/// Every rule apollo checks that this build of smear does not. **Currently none.**
///
/// Kept short on purpose. An oracle whose exceptions outnumber its checks is worse than no oracle,
/// and the arithmetic that keeps that honest is in the report: four whitelist classes and no gaps
/// at all against thirty-one rules and several hundred compared cases.
///
/// The two entries this list shipped with were both deleted by the gate, not by review — see the
/// module header. Deleting the second one is also what makes the schema comparison unconditional:
/// with no schema-stage gap declared, [`attribute`] can no longer return `Some` for an SDL, so an
/// SDL only apollo refuses is a `SchemaOutcome::ApolloRejected` failure with no door at all.
pub const GAPS: &[Gap] = &[];

/// The gap that explains a set of apollo error names at a given stage, if one does.
///
/// Stage-scoped because several of the names above are shared between apollo's schema and
/// executable validation. Without the scope, a *query* that smear wrongly accepted while apollo
/// reported only `RequiredArgument` would be excused by a declaration about SDL — an exception
/// leaking into the comparison it was never measured against.
pub fn attribute(stage: Stage, names: &[Option<&'static str>]) -> Option<&'static Gap> {
  GAPS
    .iter()
    .find(|gap| gap.stage == stage && gap.is_live() && gap.covers(names))
}

/// Gaps whose section a [`Rule`] now claims: declarations that have outlived their rule.
pub fn stale() -> Vec<&'static Gap> {
  GAPS.iter().filter(|gap| !gap.is_live()).collect()
}
