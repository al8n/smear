mod argument;
pub use argument::{Error as ArgumentError, ErrorKind as ArgumentErrorKind};

mod directive;
pub use directive::{Error as DirectiveError, ErrorKind as DirectiveErrorKind};

#[allow(clippy::len_without_is_empty)]
mod value;
pub use value::{Error as ValueError, ErrorKind as ValueErrorKind, ErrorUnknownObjectField};




pub enum Style {
  /// A bug.
  Bug,
  /// An error.
  Error,
  /// A help message.
  Help,
  /// A note.
  Note,
  /// A warning.
  Warning,
}

impl Style {
  pub(crate) fn diagnostic<'a, FileId>(&self) -> codespan_reporting::diagnostic::Diagnostic<FileId>
  where
    FileId: 'a + Copy + PartialEq,
  {
    use codespan_reporting::diagnostic::Diagnostic as CodespanDiagnostic;

    match self {
      Self::Bug => CodespanDiagnostic::bug(),
      Self::Error => CodespanDiagnostic::error(),
      Self::Help => CodespanDiagnostic::help(),
      Self::Note => CodespanDiagnostic::note(),
      Self::Warning => CodespanDiagnostic::warning(),
    }
  }
}