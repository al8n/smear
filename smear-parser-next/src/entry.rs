//! The parse-entry trait family: [`ParseSource`] and its [`ParseStr`] /
//! [`ParseBytes`] conveniences.
//!
//! A dialect's entry module (e.g. [`graphql::syntactic::entry`](crate::graphql))
//! implements [`ParseSource`] for each of its parse roots — the AST node types a
//! caller can parse a source into directly — over every source representation the
//! dialect's syntactic lexer scans (`str`, `[u8]`, and the feature-gated owned
//! sources). The traits are dialect-agnostic: they name only the source and the
//! implementing node type.
//!
//! # Prefix semantics
//!
//! A root parses a LEADING `Self` from the source; only the document roots consume
//! the whole source (their entry list runs to end of input and errors on a
//! malformed tail). A non-document root with trailing tokens succeeds on the
//! prefix — the frozen crate's entry behavior, kept as parity.

/// Parses `Self` from a borrowed source `S`.
///
/// The lifetime `'inp` ties the parsed AST to the source it borrows from; owned
/// sources (e.g. `bytes::Bytes`) yield owned slices and only borrow transiently.
pub trait ParseSource<'inp, S: ?Sized>: Sized {
  /// The error the parse reports.
  type Error;

  /// Parses a leading `Self` from `source`.
  fn parse_source(source: &'inp S) -> Result<Self, Self::Error>;
}

/// [`ParseSource`] over `str`, with the conventional method name.
pub trait ParseStr<'inp>: ParseSource<'inp, str> {
  /// Parses a leading `Self` from the string slice.
  #[inline]
  fn parse_str(source: &'inp str) -> Result<Self, Self::Error> {
    Self::parse_source(source)
  }
}

impl<'inp, T: ParseSource<'inp, str>> ParseStr<'inp> for T {}

/// [`ParseSource`] over `[u8]`, with the conventional method name.
pub trait ParseBytes<'inp>: ParseSource<'inp, [u8]> {
  /// Parses a leading `Self` from the byte slice.
  #[inline]
  fn parse_bytes(source: &'inp [u8]) -> Result<Self, Self::Error> {
    Self::parse_source(source)
  }
}

impl<'inp, T: ParseSource<'inp, [u8]>> ParseBytes<'inp> for T {}
