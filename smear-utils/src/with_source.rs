use crate::convert::FromMapExtra;

use chumsky::{
  extra::ParserExtra,
  input::{Input, MapExtra, SliceInput},
};

/// A value paired with its source span information.
///
/// `WithSource` is a fundamental container that associates parsed data with its
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
/// - `Source`: The source slice type (typically `&str` for string input)
/// - `Span`: The span type representing position information (e.g., byte ranges, line/column pairs)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WithSource<Source, Span> {
  src: Source,
  span: Span,
}

impl<Source, Span> WithSource<Source, Span> {
  /// Creates a new spanned value from source data and span information.
  ///
  /// This constructor pairs source data with its location information,
  /// creating the fundamental building block for location-aware parsing.
  #[inline]
  pub const fn new(src: Source, span: Span) -> Self {
    Self { src, span }
  }

  /// Returns a reference to the source data.
  ///
  /// This provides access to the actual parsed content without the span
  /// information. The source data represents what was parsed from the
  /// original input at the location specified by the span.
  pub const fn source(&self) -> &Source {
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
  /// different representations of the same logical data without losing location
  /// information.
  pub fn map_source<U>(self, f: impl FnOnce(Source) -> U) -> WithSource<U, Span> {
    WithSource::new(f(self.src), self.span)
  }

  /// Transforms the span information while preserving the source data.
  ///
  /// This method applies a transformation function to the span information while
  /// keeping the same source data. This is useful when adjusting coordinate
  /// systems or normalizing span representations without affecting the underlying
  /// parsed content.
  pub fn map_span<U>(self, f: impl FnOnce(Span) -> U) -> WithSource<Source, U> {
    WithSource::new(self.src, f(self.span))
  }

  /// Transforms both the source data and span information.
  ///
  /// This method allows transforming both components of the spanned value
  /// simultaneously. This is useful when converting to different coordinate
  /// systems or when both the data and its location need adjustment.
  pub fn map<U, NewSpan>(
    self,
    src: impl FnOnce(Source) -> U,
    span: impl FnOnce(Span) -> NewSpan,
  ) -> WithSource<U, NewSpan> {
    WithSource::new(src(self.src), span(self.span))
  }

  /// Consumes the spanned value and returns its components as a tuple.
  ///
  /// This method destructures the spanned value into its constituent parts,
  /// providing owned access to both the source data and span information.
  /// Useful when you need to work with the components separately.
  pub fn into_components(self) -> (Source, Span) {
    (self.src, self.span)
  }
}

impl<'src, 'b, I: SliceInput<'src>, E: ParserExtra<'src, I>> From<&mut MapExtra<'src, 'b, I, E>>
  for WithSource<I::Slice, I::Span>
{
  #[inline]
  fn from(value: &mut MapExtra<'src, 'b, I, E>) -> Self {
    Self::new(value.slice(), value.span())
  }
}

impl<'src, I, E> FromMapExtra<'src, I, E> for WithSource<I::Slice, I::Span>
where
  I: SliceInput<'src>,
  E: ParserExtra<'src, I>,
{
  #[inline]
  fn from_map_extra<'b>(value: &mut MapExtra<'src, 'b, I, E>) -> Self
  where
    I: Input<'src>,
    E: ParserExtra<'src, I>,
  {
    Self::from(value)
  }
}
