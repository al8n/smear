//! Directive applications: each `@directive(…)` as written, and the arguments it has.

use tokora::diagnostic::Location;

use crate::graphqlx::id::{MemberId, ValueId};

/// What a written name resolved to, or that it did not resolve.
///
/// It is never `Unresolved` in an analysis that [`is_ok`](crate::graphqlx::Analysis::is_ok): it
/// appears only within the extent of an error a phase reported.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Resolution<T> {
  /// It resolved to this.
  Resolved(T),
  /// It did not resolve, and a diagnostic says why.
  Unresolved,
}

/// One argument of a directive application: one the author wrote, or one its definition's default
/// filled.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ApplicationArgument {
  pub(super) argument: MemberId,
  pub(super) name: Option<Location>,
  pub(super) value: ValueId,
}

impl ApplicationArgument {
  /// The argument of the directive definition it sets.
  #[inline]
  pub const fn argument(&self) -> MemberId {
    self.argument
  }

  /// Where its name is written, or `None` when its definition's default filled it.
  #[inline]
  pub const fn name_location(&self) -> Option<Location> {
    self.name
  }

  /// Its value.
  #[inline]
  pub const fn value(&self) -> ValueId {
    self.value
  }

  /// Whether the author wrote it, rather than its definition's default filling it.
  #[inline]
  pub const fn written(&self) -> bool {
    self.name.is_some()
  }
}
