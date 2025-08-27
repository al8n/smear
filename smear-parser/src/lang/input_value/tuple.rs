use chumsky::{container::Container, extra::ParserExtra, prelude::*};

use crate::{
  lang::{
    ignored,
    punct::{LParen, RParen},
  },
  source::*,
};

/// A tuple literal with parentheses delimiters.
///
/// Represents a collection of unique values using parentheses (`( )`).
/// Tuple literals provide an alternative to list notation for collections where
/// element uniqueness is semantically important, though duplicate detection
/// and enforcement is typically handled at a higher semantic level.
///
/// ## Grammar
///
/// ```text
/// Tuple ::= '(' Values? ')'
/// Values ::= Value+
/// ```
///
/// ## Examples
///
/// **Valid tuple literals:**
/// ```text
/// ()                           // Empty tuple
/// ( "apple" )                  // Single element
/// ( "apple", "banana" )        // Multiple elements with commas
/// ( "apple" "banana" )         // Multiple elements with spaces
/// ( "apple", "banana", )       // Trailing comma allowed
/// (
///   "red",
///   "green",
///   "blue"
/// )                            // Multi-line format
///
/// // Different value types
/// ( 1, 2, 3 )                  // Numbers
/// ( true, false )              // Booleans
/// ( USER, ADMIN, GUEST )       // Enum values
/// ( "tag1", "tag2", "tag3" )   // Strings
/// ```
#[derive(Debug, Clone, Copy)]
pub struct Tuple<T, Span, C = Vec<T>> {
  span: Span,
  l_paren: LParen<Span>,
  values: C,
  r_paren: RParen<Span>,
  _marker: core::marker::PhantomData<T>,
}

impl<T, Span, C> AsRef<Span> for Tuple<T, Span, C> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<T, Span, C> IntoSpan<Span> for Tuple<T, Span, C> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<T, Span, C> IntoComponents for Tuple<T, Span, C> {
  type Components = (Span, LParen<Span>, C, RParen<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_paren, self.values, self.r_paren)
  }
}

impl<T, Span, C> Tuple<T, Span, C> {
  /// Returns the source span of the entire tuple literal.
  ///
  /// This span covers from the opening angle bracket through the closing
  /// angle bracket, including all elements and whitespace within. Useful for
  /// error reporting, source mapping, and extracting the complete tuple text.
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the opening parenthesis token.
  ///
  /// This provides access to the `(` character that begins the tuple,
  /// including its exact source position. Useful for syntax highlighting,
  /// bracket matching, and precise error reporting at tuple boundaries.
  pub const fn l_paren(&self) -> &LParen<Span> {
    &self.l_paren
  }

  /// Returns the container holding the tuple elements.
  ///
  /// This provides access to all values that were successfully parsed
  /// from the tuple literal.
  pub const fn values(&self) -> &C {
    &self.values
  }

  /// Returns the closing parenthesis token.
  ///
  /// This provides access to the `)` character that ends the tuple,
  /// including its exact source position. Useful for syntax highlighting,
  /// bracket matching, and detecting incomplete tuples.
  pub const fn r_paren(&self) -> &RParen<Span> {
    &self.r_paren
  }

  /// Creates a parser for tuple literals with a custom value parser.
  ///
  /// This is the core parsing function that accepts any value parser and
  /// creates a complete tuple parser. It handles all tuple syntax including parentheses,
  /// element parsing, whitespace management, and empty tuples.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the tuple.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, P>(value_parser: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
    P: Parser<'src, I, T, E> + Clone,
    C: Container<T>,
  {
    LParen::parser()
      .then_ignore(ignored())
      .then(choice((
        // Empty: "()"
        RParen::<Span>::parser().rewind().map(|_| C::default()),
        // Non-empty: one-or-more elems; commas are in `ws`
        value_parser
          .padded_by(ignored())
          .repeated()
          .at_least(1)
          .collect::<C>(),
      )))
      .then(RParen::parser())
      .map_with(|((l_paren, values), r_paren), sp| Self {
        span: Span::from_map_extra(sp),
        l_paren,
        r_paren,
        values,
        _marker: core::marker::PhantomData,
      })
  }
}
