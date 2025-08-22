use chumsky::{extra::ParserExtra, prelude::*};

use crate::{
  convert::*,
  source::{Char, Slice, Source},
  spanned::Spanned,
};

/// A GraphQL null literal value.
///
/// Represents the null literal as defined by the GraphQL specification. The null
/// value represents the intentional absence of a value and is distinct from
/// undefined or missing values in GraphQL's type system.
///
/// ## Specification Rules
///
/// GraphQL null literals must follow these strict formatting rules:
/// - **Exact spelling**: Must be exactly `null` in lowercase
/// - **Case-sensitive**: `NULL`, `Null`, `nULL`, etc. are not valid null literals
/// - **Complete word**: Must be the complete sequence `n-u-l-l`
/// - **No variations**: No alternative spellings or abbreviations allowed
///
/// ## Format
///
/// ```text
/// NullValue ::= 'null'
/// ```
///
/// Spec: [Null Value](https://spec.graphql.org/draft/#sec-Null-Value)
#[derive(Debug, Clone, Copy)]
pub struct NullValue<Span>(Span);

impl<Span> NullValue<Span> {
  /// Returns the source span of the null literal.
  ///
  /// This provides access to the exact location in the source where the `null`
  /// keyword was found, useful for error reporting, source mapping, syntax
  /// highlighting, and other tooling that needs to relate the null value back
  /// to its original source position.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.0
  }

  /// Creates a parser for GraphQL null literals.
  ///
  /// This parser implements the GraphQL null literal specification by matching
  /// the exact character sequence `n-u-l-l`. It is strictly case-sensitive and
  /// will reject any variations in spelling or capitalization.
  ///
  /// Spec: [Null Value](https://spec.graphql.org/draft/#sec-Null-Value)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,
  {
    just([I::Token::n, I::Token::u, I::Token::l, I::Token::l])
      .map_with(|_, span| Self(Spanned::from_map_extra(span)))
  }
}

impl<Span> AsRef<Span> for NullValue<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpanned<Span> for NullValue<Span> {
  #[inline]
  fn into_spanned(self) -> Span {
    self.0
  }
}

impl<Span> IntoComponents for NullValue<Span> {
  type Components = Span;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.into_spanned()
  }
}
