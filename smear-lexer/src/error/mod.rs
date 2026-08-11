pub use string::*;
pub use traits::*;

mod string;
mod traits;

/// Length error when converting from a list of errors to a single error.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, thiserror::Error)]
pub enum LengthError {
  /// The error list contains more than one error.
  #[error("too many errors")]
  TooManyErrors,
  /// The error list is empty.
  #[error("no errors")]
  Empty,
}
