pub use kind::SyntaxErrorKind;
pub use string::*;
pub use traits::*;

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) mod describe;
mod kind;
mod string;
mod traits;

/// How many errors the record of one lexeme keeps: of a string literal, inline or block.
///
/// Past these, each further fault in that lexeme is counted and not stored, and the count closes
/// the record as one more entry, so a record holds at most seventeen however long its lexeme is.
/// [`StringError::Omitted`] is that entry for a string literal.
pub(crate) const MAX_LEXEME_ERRORS: usize = 16;

/// The errors a lexer finds in one lexeme, as its record keeps them: the first
/// [`MAX_LEXEME_ERRORS`] in the order found, and how many more there were.
///
/// Every push of an error into one lexeme's record goes through this, so that no record grows with
/// the lexeme's length.
pub(crate) struct LexemeErrors<T> {
  kept: std::vec::Vec<T>,
  omitted: usize,
}

impl<T> LexemeErrors<T> {
  /// No error found yet.
  #[inline]
  pub(crate) const fn new() -> Self {
    Self {
      kept: std::vec::Vec::new(),
      omitted: 0,
    }
  }

  /// Keeps `error` while fewer than [`MAX_LEXEME_ERRORS`] are kept, and otherwise counts it.
  /// Answers whether it was kept.
  #[inline]
  pub(crate) fn push(&mut self, error: T) -> bool {
    if self.kept.len() < MAX_LEXEME_ERRORS {
      self.kept.push(error);
      true
    } else {
      self.omitted += 1;
      false
    }
  }

  /// Whether no error was found.
  #[inline]
  pub(crate) fn is_empty(&self) -> bool {
    self.kept.is_empty()
  }

  /// The kept errors in the order found, and after them `omitted(n)` when `n` more were found.
  #[inline]
  pub(crate) fn finish(mut self, omitted: impl FnOnce(usize) -> T) -> std::vec::Vec<T> {
    if self.omitted > 0 {
      self.kept.push(omitted(self.omitted));
    }
    self.kept
  }
}

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
