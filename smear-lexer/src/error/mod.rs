pub use kind::SyntaxErrorKind;
pub use string::*;
pub use traits::*;

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) mod describe;
mod kind;
mod string;
mod traits;

/// Length error when converting from a list of errors to a single error.
///
/// # Exhaustive, deliberately
///
/// No `#[non_exhaustive]`. The conversion succeeds exactly when the list holds one error, so a
/// failure is a list with none or with more than one: these two variants are every failing
/// length, not a vocabulary that grows.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, thiserror::Error)]
pub enum LengthError {
  /// The error list contains more than one error.
  #[error("too many errors")]
  TooManyErrors,
  /// The error list is empty.
  #[error("no errors")]
  Empty,
}
