use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{Name, SmearChar, Spanned};

/// Represents an enum value parsed from input
///
/// Spec: [Enum Value](https://spec.graphql.org/draft/#sec-Enum-Value)
#[derive(Debug, Clone, Copy)]
pub struct Enum<Src, Span> {
  /// The name of the enum value
  name: Name<Src, Span>,
}

impl<Src, Span> Enum<Src, Span> {
  /// Returns the span of the enum value.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    self.name.span()
  }

  /// Returns a parser for the enum value.
  ///
  /// Spec: [Enum Value](https://spec.graphql.org/draft/#sec-Enum-Value)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    // Match exactly "true"/"false"/"null" when followed by a non-name char (or EOF)
    // so "trueValue"/"nullish" are NOT matched.
    choice((
      just([I::Token::t, I::Token::r, I::Token::u, I::Token::e]).not(),
      just([
        I::Token::f,
        I::Token::a,
        I::Token::l,
        I::Token::s,
        I::Token::e,
      ])
      .not(),
      just([I::Token::n, I::Token::u, I::Token::l, I::Token::l]).not(),
    ))
    .then(Name::<Src, Span>::parser())
    .map_with(|(_, name), _| Enum { name })
    .padded_by(super::ignored::padded())
  }
}
