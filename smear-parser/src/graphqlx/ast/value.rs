//! GraphQLx value nodes of the dialect's own: the forms whose payload is a literal this dialect's
//! lexer makes, which the value carriers GraphQL shares cannot name.

use smear_lexer::graphqlx::LitChar;
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::Leaf;

/// A `char` value: `'a'`, one character or one escape between single quotes.
///
/// The literal is the lexer's own, so [`value`](Self::value) answers both its slice, the quotes
/// included, and the one character it spells.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CharValue<S, Span = SimpleSpan> {
  span: Span,
  value: LitChar<S>,
}

impl<S, Span> CharValue<S, Span> {
  /// Creates a `char` value from its span and the literal the lexer read there.
  #[inline]
  pub(crate) const fn new(span: Span, value: LitChar<S>) -> Self
  where
    S: Leaf,
    Span: Leaf,
  {
    Self { span, value }
  }

  /// Returns the span covering the literal, its quotes included.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the literal: its slice and the character it spells.
  #[inline]
  pub const fn value(&self) -> &LitChar<S> {
    &self.value
  }
}

impl<S, Span> AsSpan<Span> for CharValue<S, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span> IntoSpan<Span> for CharValue<S, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

/// `CharValue` taken apart: every field, in the order it is written.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CharValueComponents<S, Span> {
  /// The span covering the literal, its quotes included.
  pub span: Span,
  /// The literal, as the lexer read it.
  pub value: LitChar<S>,
}

impl<S, Span> IntoComponents for CharValue<S, Span> {
  type Components = CharValueComponents<S, Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    CharValueComponents {
      span: self.span,
      value: self.value,
    }
  }
}
