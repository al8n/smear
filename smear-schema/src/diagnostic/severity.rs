/// How much a diagnostic asks of its reader.
///
/// # Every diagnostic this crate emits today is an [`Error`](Severity::Error)
///
/// Said plainly so that nobody invents a warning to justify the axis. The three levels exist
/// because a diagnostic contract that only admits errors cannot later carry the deprecation-lint
/// class — `@deprecated` field selected, deprecated enum value passed — which is advice about a
/// document that is perfectly valid and must not change any verdict. Adding the level afterwards
/// would mean revisiting every implementor; declaring it now costs one method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Severity {
  /// The document, schema or response is refused. A verdict depends on it.
  Error,
  /// The input is accepted and something about it is worth saying.
  Warning,
  /// A suggestion, carrying no judgement about the input.
  Advice,
}

impl Severity {
  /// Returns the level's lowercase name.
  #[inline]
  pub const fn as_str(&self) -> &'static str {
    match self {
      Self::Error => "error",
      Self::Warning => "warning",
      Self::Advice => "advice",
    }
  }
}

impl core::fmt::Display for Severity {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.write_str(self.as_str())
  }
}
