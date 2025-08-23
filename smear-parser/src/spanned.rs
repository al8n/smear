use chumsky::{
  extra::ParserExtra,
  input::{Input, MapExtra, SliceInput},
};

use crate::source::Span;

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
/// ## Design Philosophy
///
/// The `WithSource` type is designed around several key principles:
/// - **Zero-cost abstractions**: Minimal overhead when using reference types
/// - **Type flexibility**: Generic over both source and span types
/// - **Transformation support**: Easy conversion between representations
/// - **Memory efficiency**: Optimized layout for common use cases
/// - **Parser integration**: Seamless integration with Chumsky combinators
///
/// ## Generic Parameters
///
/// - `Source`: The source slice type (typically `&str` for string input)
/// - `Span`: The span type representing position information (e.g., byte ranges, line/column pairs)
///
/// ## Examples
///
/// ```rust
/// use chumsky::span::SimpleSpan;
///
/// // Create a spanned value manually
/// let spanned_name = WithSource::new("userName", SimpleSpan::new(10, 18));
/// assert_eq!(spanned_name.source(), &"userName");
/// assert_eq!(spanned_name.span().start(), 10);
/// assert_eq!(spanned_name.span().end(), 18);
///
/// // Transform the source while preserving location
/// let uppercase = spanned_name.map_source(|s| s.to_uppercase());
/// assert_eq!(uppercase.source(), &"USERNAME");
/// assert_eq!(uppercase.span(), spanned_name.span()); // Same span
///
/// // Transform both source and span
/// let adjusted = spanned_name.map(
///     |s| s.to_owned(),
///     |span| span.offset_by(100)
/// );
/// ```
///
/// ## Usage Patterns
///
/// 1. **Error Reporting**: Provide precise error locations
/// ```rust
/// fn report_parse_error(spanned: &WithSource<&str, SimpleSpan>, message: &str) {
///     eprintln!("Error at {}-{}: {} in '{}'",
///               spanned.span().start(),
///               spanned.span().end(),
///               message,
///               spanned.source());
/// }
/// ```
///
/// 2. **Source Mapping**: Map parsed elements back to source
/// ```rust
/// fn extract_source_ranges(elements: &[WithSource<&str, SimpleSpan>]) -> Vec<(usize, usize)> {
///     elements.iter()
///         .map(|e| (e.span().start(), e.span().end()))
///         .collect()
/// }
/// ```
///
/// 3. **Transformation**: Preserve location through transformations
/// ```rust
/// fn to_owned_preserving_location(
///     spanned: WithSource<&str, SimpleSpan>
/// ) -> WithSource<String, SimpleSpan> {
///     spanned.map_source(|s| s.to_owned())
/// }
/// ```
///
/// ## Memory Layout
///
/// `WithSource` is designed to be efficient:
/// - **Zero-cost when using references**: No overhead for `&str` source data
/// - **Copyable when appropriate**: Implements `Copy` when both components are `Copy`
/// - **Minimal memory overhead**: Just the source and span data, no extra indirection
/// - **Cache-friendly**: Compact layout improves memory locality
///
/// ## Integration with Parsing
///
/// The type integrates seamlessly with Chumsky's parsing infrastructure:
/// - **Automatic creation**: Via `From` implementation for `MapExtra`
/// - **Parser combinators**: Works with `map_with` for span capture
/// - **Generic parsing**: Compatible with different input and span types
/// - **Error propagation**: Span information preserved through error handling
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
  ///
  /// # Parameters
  ///
  /// * `src` - The source data (typically a string slice or parsed value)
  /// * `span` - The position information (start/end positions, line/column, etc.)
  ///
  /// # Examples
  ///
  /// ```rust
  /// use chumsky::span::SimpleSpan;
  ///
  /// // Create a spanned string slice
  /// let spanned = WithSource::new("hello", SimpleSpan::new(0, 5));
  /// assert_eq!(spanned.source(), &"hello");
  ///
  /// // Create a spanned parsed value
  /// let spanned_number = WithSource::new(42, SimpleSpan::new(10, 12));
  /// assert_eq!(*spanned_number.source(), 42);
  /// ```
  #[inline]
  pub const fn new(src: Source, span: Span) -> Self {
    Self { src, span }
  }

  /// Returns a reference to the source data.
  ///
  /// This provides access to the actual parsed content without the span
  /// information. The source data represents what was parsed from the
  /// original input at the location specified by the span.
  ///
  /// # Examples
  ///
  /// ```rust
  /// let spanned = WithSource::new("fieldName", SimpleSpan::new(5, 14));
  /// assert_eq!(spanned.source(), &"fieldName");
  ///
  /// // Extract the source for further processing
  /// let field_name: &str = spanned.source();
  /// if field_name.starts_with("__") {
  ///     println!("Introspection field detected");
  /// }
  /// ```
  pub const fn source(&self) -> &Source {
    &self.src
  }

  /// Returns a reference to the span information.
  ///
  /// This provides access to the location data indicating where in the
  /// original source this element was found. The span format depends on
  /// the span type being used (byte positions, line/column, etc.).
  ///
  /// # Examples
  ///
  /// ```rust
  /// let spanned = WithSource::new("value", SimpleSpan::new(20, 25));
  /// let span = spanned.span();
  ///
  /// println!("Found 'value' at bytes {}-{}", span.start(), span.end());
  ///
  /// // Use span for error reporting
  /// if span.len() > MAX_IDENTIFIER_LENGTH {
  ///     report_error(span, "Identifier too long");
  /// }
  /// ```
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Transforms the source data while preserving the span.
  ///
  /// This method applies a transformation function to the source data while
  /// keeping the same span information. This is useful when converting between
  /// different representations of the same logical data without losing location
  /// information.
  ///
  /// # Parameters
  ///
  /// * `f` - A function that transforms the source data
  ///
  /// # Examples
  ///
  /// ```rust
  /// // Convert string to uppercase while preserving location
  /// let spanned_name = WithSource::new("username", SimpleSpan::new(0, 8));
  /// let uppercase = spanned_name.map_source(|s| s.to_uppercase());
  /// assert_eq!(uppercase.source(), &"USERNAME");
  /// assert_eq!(uppercase.span(), spanned_name.span()); // Same span
  ///
  /// // Parse string to number while preserving location
  /// let spanned_str = WithSource::new("42", SimpleSpan::new(10, 12));
  /// let spanned_num = spanned_str.map_source(|s| s.parse::<i32>().unwrap());
  /// assert_eq!(*spanned_num.source(), 42);
  /// ```
  pub fn map_source<U>(self, f: impl FnOnce(Source) -> U) -> WithSource<U, Span> {
    WithSource::new(f(self.src), self.span)
  }

  /// Transforms the span information while preserving the source data.
  ///
  /// This method applies a transformation function to the span information while
  /// keeping the same source data. This is useful when adjusting coordinate
  /// systems or normalizing span representations without affecting the underlying
  /// parsed content.
  ///
  /// # Parameters
  ///
  /// * `f` - A function that transforms the span information
  ///
  /// # Examples
  ///
  /// ```rust
  /// // Adjust span coordinates for different coordinate system
  /// let spanned = WithSource::new("temp", SimpleSpan::new(5, 9));
  /// let adjusted = spanned.map_span(|span| span.offset_by(100));
  /// assert_eq!(adjusted.source(), &"temp"); // Same source
  /// assert_eq!(adjusted.span().start(), 105); // Adjusted span
  ///
  /// // Convert between span types
  /// let normalized = spanned.map_span(|span| LineColumn::from(span));
  /// ```
  pub fn map_span<U>(self, f: impl FnOnce(Span) -> U) -> WithSource<Source, U> {
    WithSource::new(self.src, f(self.span))
  }

  /// Transforms both the source data and span information.
  ///
  /// This method allows transforming both components of the spanned value
  /// simultaneously. This is useful when converting to different coordinate
  /// systems or when both the data and its location need adjustment.
  ///
  /// # Parameters
  ///
  /// * `src` - A function that transforms the source data
  /// * `span` - A function that transforms the span information
  ///
  /// # Examples
  ///
  /// ```rust
  /// // Convert to owned string and adjust span coordinates
  /// let spanned = WithSource::new("temp", SimpleSpan::new(5, 9));
  /// let owned = spanned.map(
  ///     |s| s.to_owned(),
  ///     |span| span.offset_by(100)
  /// );
  /// assert_eq!(owned.source(), &"temp".to_owned());
  /// assert_eq!(owned.span().start(), 105);
  ///
  /// // Transform both to different types
  /// let converted = spanned.map(
  ///     |s| s.len(),                    // Source: &str -> usize
  ///     |span| (span.start(), span.end()) // Span: SimpleSpan -> (usize, usize)
  /// );
  /// ```
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
  ///
  /// # Examples
  ///
  /// ```rust
  /// let spanned = WithSource::new("hello", SimpleSpan::new(0, 5));
  /// let (source, span) = spanned.into_components();
  /// assert_eq!(source, "hello");
  /// assert_eq!(span.start(), 0);
  /// assert_eq!(span.end(), 5);
  /// ```
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

impl<'src, I, E> Span<'src, I, E> for WithSource<I::Slice, I::Span>
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
