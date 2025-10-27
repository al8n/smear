use logosky::{
  Logos, Source, Token, Tokenizer,
  chumsky::{self, IterParser as _, Parseable, Parser, extra::ParserExtra},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::error::UnclosedBraceError;

use smear_lexer::{
  keywords,
  punctuator::{LBrace, RBrace},
};

use core::marker::PhantomData;
use std::vec::Vec;

/// A GraphQLx set literal value.
///
/// Represents a complete set literal as defined by the GraphQLx specification.
/// Set literals are ordered collections of values enclosed in square brackets,
/// supporting any valid GraphQLx values including nested sets and objects.
///
/// ## Specification Rules
///
/// GraphQLx set literals follow these formatting rules:
/// - **Bracket delimiters**: Must be enclosed in `[` and `]`
/// - **Value separation**: Elements separated by whitespace (commas optional but conventional)
/// - **Trailing commas**: Allowed after the last element
/// - **Nested values**: Can contain any valid GraphQLx input values
/// - **Empty sets**: `[]` is a valid empty set
/// - **Whitespace handling**: Flexible whitespace and comments between elements
///
/// ## Grammar
///
/// ```text
/// Set ::= 'set' '{' Values? '}'
/// Values    ::= Value+
/// ```
///
/// ## Generic Parameters
///
/// - `Value`: The type of elements contained in the set
/// - `Container`: The collection type (defaults to `Vec<Value>`, can be customized)
///
/// ## Container Flexibility
///
/// The `Container` parameter allows using different collection types:
/// - `Vec<Value>` (default): Standard dynamic array
/// - Any type implementing `chumsky::container::Container<Value>`
///
/// ## Component Structure
///
/// Each set literal contains:
/// - **Overall span**: Covers the entire set including brackets
/// - **Left bracket**: The opening `[` token with its position
/// - **Right bracket**: The closing `]` token with its position  
/// - **Values**: The parsed elements in their container
#[derive(Debug, Clone)]
pub struct Set<Value, Container = Vec<Value>> {
  span: Span,
  values: Container,
  _m: PhantomData<Value>,
}

impl<Value, Container> Set<Value, Container> {
  /// Creates a new set literal with the given span and values.
  #[inline]
  pub const fn new(span: Span, values: Container) -> Self {
    Self {
      span,
      values,
      _m: PhantomData,
    }
  }

  /// Returns the span covering the entire set literal.
  ///
  /// This span includes the opening and closing brackets as well as all contained values.
  /// It is useful for error reporting, syntax highlighting, and source mapping.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the container holding the parsed set elements.
  ///
  /// This provides access to all elements that were successfully parsed
  /// from the set literal.
  #[inline]
  pub const fn values(&self) -> &Container {
    &self.values
  }

  /// Creates a parser for GraphQLx set literals with customizable value parsing.
  ///
  /// This parser handles the complete set syntax including brackets and
  /// enforces proper structure. It uses the provided `value_parser` to parse
  /// each individual element within the set.
  ///
  /// ## Error Handling
  ///
  /// If the closing bracket is missing, the parser invokes the provided
  /// `on_missing_rbracket` function to generate a custom error message.
  /// This allows for context-specific error reporting.
  pub fn parser_with<'src, I, T, Error, VP, E>(
    value_parser: VP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: UnclosedBraceError + 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    VP: Parser<'src, I, Value, E> + Clone + 'src,
    Container: chumsky::container::Container<Value>,
    LBrace: Parseable<'src, I, T, Error>,
    RBrace: Parseable<'src, I, T, Error>,
    keywords::Set: Parseable<'src, I, T, Error>,
  {
    keywords::Set::parser()
      .then(LBrace::parser())
      .ignore_then(value_parser.repeated().collect())
      .then(RBrace::parser().or_not())
      .try_map(move |(values, r), span| match r {
        Some(_) => Ok(Set::new(span, values)),
        None => Err(Error::unclosed_brace(span)),
      })
  }
}

impl<Value, Container> AsSpan<Span> for Set<Value, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Value, Container> IntoSpan<Span> for Set<Value, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Value, Container> IntoComponents for Set<Value, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.values)
  }
}

impl<'a, Value, Container, I, T, Error> Parseable<'a, I, T, Error> for Set<Value, Container>
where
  Error: UnclosedBraceError + 'a,
  Value: Parseable<'a, I, T, Error>,
  Container: chumsky::container::Container<Value>,
  LBrace: Parseable<'a, I, T, Error>,
  RBrace: Parseable<'a, I, T, Error>,
  smear_lexer::keywords::Set: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Self::parser_with(Value::parser())
  }
}
