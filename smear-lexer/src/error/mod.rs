pub use kind::SyntaxErrorKind;
pub use string::*;
pub use traits::*;

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) mod describe;
mod kind;
mod string;
mod traits;

/// How many errors the record of one lexeme keeps: of a string literal, inline or block, and of a
/// comment in a byte source.
///
/// Past these, each further fault in that lexeme is counted and not stored, and the count follows
/// them as one more entry: [`StringError::Omitted`] for a string literal, and each dialect's
/// `LexerErrorData::Omitted` for a comment. A literal its line or the input ends closes its record
/// with [`StringError::Unterminated`], which no count takes, because it is why the token ends where
/// it does; a comment has no such error. So a literal's record holds at most eighteen entries and
/// a comment's at most seventeen, however long the lexeme is.
pub(crate) const MAX_LEXEME_ERRORS: usize = 16;

/// The errors a lexer finds in one lexeme, as its record keeps them: the first
/// [`MAX_LEXEME_ERRORS`] in the order found, and how many more there were.
///
/// Every push of an error into one lexeme's record goes through this, so that no record grows with
/// the lexeme's length. A literal's terminal `Unterminated` is not pushed: it is appended after
/// the count, where no cap reaches it.
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
