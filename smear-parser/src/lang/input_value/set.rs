use chumsky::{container::Container, extra::ParserExtra, prelude::*};

use crate::{
  convert::*,
  lang::{
    ignored,
    punct::{LAngle, RAngle},
    Const,
  },
  source::{Char, Slice, Source},
};

/// A set literal with angle bracket delimiters.
///
/// Represents a collection of unique values using angle bracket syntax (`< >`).
/// Set literals provide an alternative to list notation for collections where
/// element uniqueness is semantically important, though duplicate detection
/// and enforcement is typically handled at a higher semantic level.
///
/// ## Specification Rules
///
/// Set literals follow these formatting rules:
/// - **Angle bracket delimiters**: Must be enclosed in `<` and `>`
/// - **Element separation**: Elements separated by whitespace (commas optional)
/// - **Empty set syntax**: Empty sets use `<>` (no special sentinel needed)
/// - **Flexible whitespace**: Whitespace and comments allowed throughout
/// - **No duplicate checking**: Parser allows duplicates (semantic validation handles uniqueness)
///
/// ## Grammar
///
/// ```text
/// Set ::= '<' Values? '>'
/// Values ::= Value+
/// ```
///
/// ## Examples
///
/// **Valid set literals:**
/// ```text
/// <>                           // Empty set
/// < "apple" >                  // Single element
/// < "apple", "banana" >        // Multiple elements with commas
/// < "apple" "banana" >         // Multiple elements with spaces
/// < "apple", "banana", >       // Trailing comma allowed
/// <
///   "red",
///   "green",
///   "blue"
/// >                            // Multi-line format
///
/// // Different value types
/// < 1, 2, 3 >                  // Numbers
/// < true, false >              // Booleans  
/// < USER, ADMIN, GUEST >       // Enum values
/// < "tag1", "tag2", "tag3" >   // Strings
/// ```
#[derive(Debug, Clone, Copy)]
pub struct Set<T, Span, C = Vec<T>> {
  span: Span,
  l_angle: LAngle<Span>,
  values: C,
  r_angle: RAngle<Span>,
  _marker: core::marker::PhantomData<T>,
}

impl<T, Span, C> AsRef<Span> for Set<T, Span, C> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<T, Span, C> IntoSpan<Span> for Set<T, Span, C> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<T, Span, C> IntoComponents for Set<T, Span, C> {
  type Components = (Span, LAngle<Span>, C, RAngle<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_angle, self.values, self.r_angle)
  }
}

impl<T, Span, C> Set<T, Span, C> {
  /// Returns the source span of the entire set literal.
  ///
  /// This span covers from the opening angle bracket through the closing
  /// angle bracket, including all elements and whitespace within. Useful for
  /// error reporting, source mapping, and extracting the complete set text.
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the opening angle bracket token.
  ///
  /// This provides access to the `<` character that begins the set,
  /// including its exact source position. Useful for syntax highlighting,
  /// bracket matching, and precise error reporting at set boundaries.
  pub const fn l_angle(&self) -> &LAngle<Span> {
    &self.l_angle
  }

  /// Returns the container holding the set elements.
  ///
  /// This provides access to all values that were successfully parsed
  /// from the set literal.
  pub const fn values(&self) -> &C {
    &self.values
  }

  /// Returns the closing angle bracket token.
  ///
  /// This provides access to the `>` character that ends the set,
  /// including its exact source position. Useful for syntax highlighting,
  /// bracket matching, and detecting incomplete sets.
  pub const fn r_angle(&self) -> &RAngle<Span> {
    &self.r_angle
  }

  /// Creates a parser for set literals with a custom value parser.
  ///
  /// This is the core parsing function that accepts any value parser and
  /// creates a complete set parser. It handles all set syntax including angle
  /// brackets, element parsing, whitespace management, and empty sets.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the set.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, P>(value_parser: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    P: Parser<'src, I, T, E> + Clone,
    C: Container<T>,
  {
    LAngle::parser()
      .then_ignore(ignored())
      .then(choice((
        // Empty: "<>"
        RAngle::<Span>::parser().rewind().map(|_| C::default()),
        // Non-empty: one-or-more elems; commas are in `ws`
        value_parser
          .padded_by(ignored())
          .repeated()
          .at_least(1)
          .collect::<C>(),
      )))
      .then(RAngle::parser())
      .map_with(|((l_angle, values), r_angle), sp| Self {
        span: Span::from_map_extra(sp),
        l_angle,
        r_angle,
        values,
        _marker: core::marker::PhantomData,
      })
  }
}
