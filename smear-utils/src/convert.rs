use chumsky::{
  extra::ParserExtra,
  input::{Input, MapExtra},
};

/// Enables consuming a parsed element to extract its source span.
///
/// This trait provides a way to take ownership of the span information from
/// a parsed element, which is useful when the element itself is no longer
/// needed but the span data should be preserved or transferred to another
/// data structure.
///
/// ## Usage Patterns
///
/// Common scenarios for using this trait:
/// - **AST construction**: Building higher-level AST nodes that need owned spans
/// - **Error collection**: Gathering span information for batch error reporting
/// - **Transformation**: Converting between different representations while preserving location
/// - **Optimization**: Avoiding clones when transferring ownership is acceptable
///
/// ## Implementation Notes
///
/// Implementing types should ensure that:
/// - The returned span is equivalent to what `AsSpan::spanned()` would return
/// - All span information is preserved during the conversion
/// - The conversion is efficient (ideally zero-cost)
pub trait IntoSpan<Span>: AsRef<Span> {
  /// Consumes this element and returns the owned source span.
  ///
  /// This method takes ownership of the element and extracts its span information
  /// as an owned value. This is useful when you need to transfer ownership of
  /// the span data to another data structure or when the element itself is no
  /// longer needed but the location information should be preserved.
  fn into_span(self) -> Span;
}

/// Enables destructuring a parsed element into its constituent components.
///
/// This trait provides a way to break down complex parsed elements into their
/// individual parts, taking ownership of each component. This is particularly
/// useful for transformation, analysis, or when building different representations
/// of the parsed data.
///
/// ## Design Philosophy
///
/// The trait uses an associated type rather than generic parameters to ensure
/// that each implementing type has exactly one way to be decomposed. This provides
/// type safety and makes the interface predictable for consumers.
///
/// ## Usage Patterns
///
/// Common scenarios for using this trait:
/// - **AST transformation**: Converting parsed elements into different AST representations
/// - **Analysis**: Extracting specific components for validation or processing
/// - **Serialization**: Breaking down elements for custom serialization formats
/// - **Testing**: Accessing individual components for detailed assertions
///
/// ## Examples
///
/// ```rust
/// // Extracting components for transformation
/// let float_value: FloatValue<&str, SimpleSpan> = parse_float("3.14e-2")?;
/// let (span, int_part, frac_part, exp_part) = float_value.into_components();
///
/// // Building a custom representation
/// let custom_float = CustomFloat {
///     location: span,
///     integer: int_part,
///     fractional: frac_part,
///     exponent: exp_part,
/// };
///
/// // Component analysis
/// let int_literal: IntValue<&str, SimpleSpan> = parse_int("-42")?;
/// let (span, sign, digits) = int_literal.into_components();
///
/// if sign.is_some() {
///     println!("Found negative integer at {:?}", span);
/// }
/// ```
///
/// ## Implementation Guidelines
///
/// When implementing this trait:
/// - Include all meaningful components of the parsed element
/// - Order components logically (typically: span first, then sub-components in source order)
/// - Use tuples for simple decomposition, custom structs for complex cases
/// - Ensure the decomposition is complete (no information loss)
/// - Document the component structure clearly
///
/// ## Component Ordering Convention
///
/// To maintain consistency across implementations, follow this ordering:
/// 1. **Overall span**: The span covering the entire element
/// 2. **Required components**: Core parts that are always present
/// 3. **Optional components**: Parts that may or may not be present
/// 4. **Sub-elements**: Nested parsed elements in source order
pub trait IntoComponents {
  /// The tuple or struct type containing the decomposed components.
  ///
  /// This associated type defines the structure returned by `into_components()`.
  /// It should include all meaningful parts of the parsed element in a logical
  /// order that makes sense for the specific element type.
  type Components;

  /// Consumes this element and returns its constituent components.
  ///
  /// This method breaks down the parsed element into its individual parts,
  /// providing owned access to each component. The exact structure of the
  /// returned components is defined by the `Components` associated type.
  fn into_components(self) -> Self::Components;
}

/// Trait for types that can be created from Chumsky's parser extra information.
///
/// This trait provides a unified interface for creating spanned values and span
/// types from the extra information available during parsing. It enables generic
/// parsing code that can work with different span representations while maintaining
/// consistent creation patterns.
///
/// ## Design Purpose
///
/// The trait serves several key purposes:
/// - **Abstraction**: Unified interface for creating different spanned types
/// - **Flexibility**: Support for both complex spanned values and simple spans
/// - **Integration**: Seamless integration with Chumsky's parsing infrastructure
/// - **Polymorphism**: Enable generic parsing functions over different span types
///
/// ## Usage Patterns
///
/// ```ignore
/// // Generic parser that works with any spanned type
/// fn parse_with_span<'src, I, E, S>() -> impl Parser<'src, I, S, E>
/// where
///     I: Input<'src>,
///     E: ParserExtra<'src, I>,
///     S: Span<'src, I, E>,
/// {
///     just("example").map_with(|_, extra| S::from_map_extra(extra))
/// }
///
/// // Can be used with both WithSource and raw spans
/// let with_source_parser = parse_with_span::<_, _, _, WithSource<_, _>>();
/// let span_only_parser = parse_with_span::<_, _, _, SimpleSpan>();
/// ```
///
/// ## Implementation Types
///
/// The trait is implemented for:
/// - **`WithSource<Source, Span>`**: Creates source+span pairs
/// - **Any span type implementing `chumsky::span::Span`**: Creates raw spans
///
/// This allows the same parsing interface to work with different levels of
/// detail in span tracking.
pub trait FromMapExtra<'src, I: Input<'src>, E: ParserExtra<'src, I>>: 'src {
  /// Creates a spanned value from Chumsky's parser extra information.
  ///
  /// This method extracts the necessary information from the parser's extra
  /// state to create the appropriate spanned representation. The exact behavior
  /// depends on the implementing type:
  /// - `WithSource` creates a source+span pair
  /// - Raw span types extract just the span information
  fn from_map_extra<'b>(extra: &mut MapExtra<'src, 'b, I, E>) -> Self;
}

impl<'src, I, E, T> FromMapExtra<'src, I, E> for T
where
  I: Input<'src, Span = T>,
  E: ParserExtra<'src, I>,
  T: chumsky::span::Span + 'src,
{
  #[inline]
  fn from_map_extra<'b>(value: &mut MapExtra<'src, 'b, I, E>) -> Self
  where
    I: Input<'src>,
    E: ParserExtra<'src, I>,
  {
    value.span()
  }
}
