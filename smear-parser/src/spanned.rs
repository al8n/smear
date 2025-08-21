use chumsky::{extra::ParserExtra, input::SliceInput};

/// A value paired with its source span information.
///
/// `Spanned` is a fundamental container that associates parsed data with its
/// exact location in the original source text. This pairing is essential for
/// GraphQL parsing because it enables precise error reporting, source mapping,
/// and tooling support.
///
/// ## Core Concept
///
/// Every parsed element in GraphQL retains knowledge of where it came from in
/// the source text. This allows the parser to provide meaningful error messages
/// that point to exact locations, enables IDE features like hover information
/// and go-to-definition, and supports advanced tooling like source maps.
///
/// ## Generic Parameters
///
/// - `Src`: The source slice type (typically `&str` for string input)
/// - `Span`: The span type representing position information (e.g., byte ranges, line/column pairs)
///
/// ## Memory Layout
///
/// `Spanned` is designed to be efficient:
/// - Zero-cost when `Src` is a reference type like `&str`
/// - Can be copied when both `Src` and `Span` are `Copy`
/// - Minimal memory overhead (just the source and span data)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Spanned<Src, Span> {
  src: Src,
  span: Span,
}

impl<Src, Span> Spanned<Src, Span> {
  /// Creates a new spanned value from source data and span information.
  ///
  /// This constructor pairs source data with its location information,
  /// creating the fundamental building block for location-aware parsing.
  #[inline]
  pub const fn new(src: Src, span: Span) -> Self {
    Self { src, span }
  }

  /// Returns a reference to the source data.
  ///
  /// This provides access to the actual parsed content without the span
  /// information. The source data represents what was parsed from the
  /// original input at the location specified by the span.
  pub const fn source(&self) -> &Src {
    &self.src
  }

  /// Returns a reference to the span information.
  ///
  /// This provides access to the location data indicating where in the
  /// original source this element was found. The span format depends on
  /// the span type being used (byte positions, line/column, etc.).
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Transforms the source data while preserving the span.
  ///
  /// This method applies a transformation function to the source data while
  /// keeping the same span information. This is useful when converting between
  /// different representations of the same logical data.
  pub fn map_source<U>(self, f: impl FnOnce(Src) -> U) -> Spanned<U, Span> {
    Spanned::new(f(self.src), self.span)
  }

  /// Transforms the span information while preserving the source data.
  ///
  /// This method applies a transformation function to the span information while
  /// keeping the same source data. This is useful when adjusting the location
  /// information without modifying the underlying data.
  pub fn map_span<U>(self, f: impl FnOnce(Span) -> U) -> Spanned<Src, U> {
    Spanned::new(self.src, f(self.span))
  }

  /// Transforms both the source data and span information.
  ///
  /// This method allows transforming both components of the spanned value
  /// simultaneously. This is useful when converting to different coordinate
  /// systems or when both the data and its location need adjustment.
  pub fn map<U, NewSpan>(
    self,
    src: impl FnOnce(Src) -> U,
    span: impl FnOnce(Span) -> NewSpan,
  ) -> Spanned<U, NewSpan> {
    Spanned::new(src(self.src), span(self.span))
  }
}

impl<'src, 'b, I: SliceInput<'src>, E: ParserExtra<'src, I>>
  From<&mut chumsky::input::MapExtra<'src, 'b, I, E>> for Spanned<I::Slice, I::Span>
{
  fn from(value: &mut chumsky::input::MapExtra<'src, 'b, I, E>) -> Self {
    Self::new(value.slice(), value.span())
  }
}
