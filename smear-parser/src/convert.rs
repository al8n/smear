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
/// - The returned span is equivalent to what `AsSpanned::spanned()` would return
/// - All span information is preserved during the conversion
/// - The conversion is efficient (ideally zero-cost)
pub trait IntoSpanned<Spanned>: AsRef<Spanned> {
  /// Consumes this element and returns the owned source span.
  ///
  /// This method takes ownership of the element and extracts its span information
  /// as an owned value. This is useful when you need to transfer ownership of
  /// the span data to another data structure or when the element itself is no
  /// longer needed but the location information should be preserved.
  fn into_spanned(self) -> Spanned;
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
