//! The four places where `apollo-compiler` 1.32.0 is *less* strict than the draft specification,
//! and the rule that makes the exception safe.
//!
//! # Why a whitelist at all
//!
//! A differential test is only as good as its oracle, and this oracle is measurably partial. The
//! design spec (issue #85, §6 G2) mapped `apollo-compiler`'s §5 coverage file by file and found
//! it complete except for four classes. Without them the harness would red on every document that
//! exercises a rule apollo does not have — dozens of true positives reported as failures, which is
//! how a differential gate gets switched off.
//!
//! # Why the whitelist is **directional**, and what that buys
//!
//! Every class below excuses exactly one direction: **smear rejects, apollo accepts**. The
//! opposite direction — smear accepts what apollo rejects — is never excusable by any class, and
//! [`Class`] has no way to spell it. That asymmetry is the whole point.
//!
//! Take [`Class::W1`]. apollo does not implement draft 5.6.3 at all: `{ a: 1, a: 2 }` passes. Used
//! naively as an oracle — "smear must agree with apollo" — it would not merely fail to catch a bug
//! in smear's own 5.6.3, it would **demand** one: the moment smear's duplicate-key check started
//! firing, a symmetric harness would call it a divergence. A directional whitelist inverts that.
//! It says apollo may be laxer here and smear may not follow, and it says nothing at all about the
//! other direction, which stays a hard failure.
//!
//! # Why a class going quiet is also a failure
//!
//! [`Class::expectation`] is asserted, not merely permitted. If a W1 document starts coming back
//! *invalid* from apollo, the exception is stale — apollo has implemented the rule — and the
//! harness says so instead of carrying a dead excuse. The same test that proves the whitelist is
//! honest therefore also re-verifies it against whatever version of `apollo-compiler` resolved,
//! which is what makes the `=1.32.0` pin in `Cargo.toml` a checkable claim rather than a comment.

use core::fmt;

/// A behaviour of `apollo-compiler` 1.32.0 that the specification does not sanction, and that the
/// harness therefore has to excuse in one direction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Class {
  /// **Draft 5.6.3, Input Object Field Uniqueness — not implemented at all.**
  ///
  /// `{ f(arg: { a: 1, a: 2 }) }` validates clean. Reproduced by
  /// [`super::Oracle`] on every run through the `w1/*` cases.
  W1,
  /// **Draft 5.8.5, All Variable Usages Are Allowed — named types only.**
  ///
  /// apollo compares the *named* type of a variable against the named type of its location and
  /// ignores the list and non-null wrappers once the usage is nested inside a list or object
  /// literal, so `query ($v: Int) { listNn(arg: [$v]) }` passes even though the item location is
  /// `Int!`. It is a coverage gap rather than a reading of the specification: the control case —
  /// the same mismatch at the top level — is rejected, and apollo's source carries its own TODO.
  W2,
  /// **Draft 5.5.2.3 on an interface with no implementors — both sides deviate, identically.**
  ///
  /// `{ i { ... on I { __typename } } }` where `I` is an interface nothing implements has an empty
  /// possible-type intersection, so the specification's text makes it invalid. graphql-js, apollo
  /// and graphql-spec#1109 all short-circuit `condition == scope` to valid, and smear adopts that
  /// deviation deliberately (design §5). So this class expects **agreement**, and exists only so
  /// that apollo changing its mind is a red rather than a silent realignment.
  W3,
  /// **Draft §3.10 / 5.6.1 OneOf Input Objects — absent.**
  ///
  /// apollo 1.32 has no `@oneOf` built-in and no OneOf literal rules, so a input object declared
  /// `@oneOf` is to it an ordinary input object carrying a user-defined directive: every OneOf
  /// constraint — exactly one field, that field not null, a variable in a OneOf position being
  /// non-null — goes unchecked.
  W4,
}

/// What the harness requires of a case that declares a [`Class`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Expectation {
  /// smear must reject and apollo must accept.
  ///
  /// Both halves are asserted. apollo accepting is the gap the class records; smear rejecting is
  /// the check the gap would otherwise hide, so a class whose documents smear also accepts is a
  /// missing rule in smear, reported under the class that was supposed to be covering for it.
  SmearStricter,
  /// Both must accept.
  ///
  /// The deviation is shared, so the verdicts agree; the class exists to notice if that stops
  /// being true.
  AgreeValid,
}

impl Class {
  /// Every class, so the harness can assert each one is exercised rather than trusting that it is.
  pub const ALL: &'static [Self] = &[Self::W1, Self::W2, Self::W3, Self::W4];

  /// The verdict pair a case in this class must produce.
  #[inline]
  pub const fn expectation(self) -> Expectation {
    match self {
      Self::W1 | Self::W2 | Self::W4 => Expectation::SmearStricter,
      Self::W3 => Expectation::AgreeValid,
    }
  }

  /// The draft section the class is about.
  #[inline]
  pub const fn section(self) -> &'static str {
    match self {
      Self::W1 => "5.6.3",
      Self::W2 => "5.8.5",
      Self::W3 => "5.5.2.3",
      Self::W4 => "3.10 / 5.6.1",
    }
  }

  /// One line naming the gap, used in the harness's report and in failure messages.
  #[inline]
  pub const fn summary(self) -> &'static str {
    match self {
      Self::W1 => "apollo does not implement Input Object Field Uniqueness",
      Self::W2 => "apollo compares named types only for a nested variable usage",
      Self::W3 => {
        "both sides short-circuit an empty possible-type intersection (graphql-spec#1109)"
      }
      Self::W4 => "apollo has no OneOf Input Objects",
    }
  }
}

impl fmt::Display for Class {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{self:?} (draft {}): {}", self.section(), self.summary())
  }
}
