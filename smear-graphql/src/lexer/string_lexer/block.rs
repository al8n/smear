use derive_more::IsVariant;
use logosky::utils::human_display::DisplayHuman;

mod str;

/// A block string representation in GraphQL.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, IsVariant)]
pub enum BlockString<S> {
  /// A clean block string, no escaped triple quotes, no CR/CRLF,
  /// no leading/trailing blank lines, and no common indent.
  Clean(S),

  /// A block string required some processing to unescape or normalize.
  /// This includes handling escaped triple quotes, line endings, and indentation.
  Mixed {
    /// The raw source slice including the `"""` delimiters.
    source: S,
    /// At least one `\"\"\"` escape was seen (becomes `"""`).
    has_escaped_triple_quote: bool,
    /// There were `\r` or `\r\n` line endings to normalize to `\n`.
    has_cr_terminators: bool,
    /// Number of **leading** blank lines (spaces/tabs only) to trim.
    leading_blank_lines: usize,
    /// Number of **trailing** blank lines (spaces/tabs only) to trim.
    trailing_blank_lines: usize,
    /// The common indentation (spaces/tabs) across all non-blank lines
    /// *after the first* that should be stripped.
    common_indent: usize,
    /// The total number of lines in the block string (including blank lines).
    total_lines: usize,
  },
}

impl<S: DisplayHuman> core::fmt::Display for BlockString<S> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Clean(s) => DisplayHuman::fmt(s, f),
      Self::Mixed { source, .. } => DisplayHuman::fmt(source, f),
    }
  }
}

impl<'a> BlockString<&'a str> {
  /// Returns the str representation of the block string.
  #[inline(always)]
  pub const fn as_str(&self) -> &'a str {
    match self {
      Self::Clean(s) => s,
      Self::Mixed { source, .. } => source,
    }
  }
}

impl<'a> BlockString<&'a [u8]> {
  /// Returns the byte slice representation of the block string.
  #[inline(always)]
  pub const fn as_bytes(&self) -> &'a [u8] {
    match self {
      Self::Clean(s) => s,
      Self::Mixed { source, .. } => source,
    }
  }
}

impl<S> BlockString<S> {
  /// Returns the underlying source.
  #[inline(always)]
  pub const fn source(self) -> S
  where
    S: Copy,
  {
    match self {
      Self::Clean(s) | Self::Mixed { source: s, .. } => s,
    }
  }

  /// Returns the reference to the underlying source.
  #[inline(always)]
  pub const fn source_ref(&self) -> &S {
    match self {
      Self::Clean(s) | Self::Mixed { source: s, .. } => s,
    }
  }
}

impl_common_traits!(BlockString::<&'a str>::as_str);
impl_common_traits!(BlockString::<&'a [u8]>::as_bytes);
