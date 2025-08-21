use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use super::{
  super::{
    super::{char::Char, name::Name, spanned::Spanned},
    punct::Dollar,
  },
  ignored::ignored,
};

/// Represents a variable value parsed from input
///
/// Spec: [Variable Value](https://spec.graphql.org/draft/#sec-Variable-Value)
#[derive(Debug, Clone, Copy)]
pub struct Variable<Src, Span> {
  span: Spanned<Src, Span>,
  /// The name of the variable value
  name: Name<Src, Span>,
  /// The span of the dollar character
  dollar: Dollar<Src, Span>,
}

impl<Src, Span> Variable<Src, Span> {
  /// Returns the span of the name
  #[inline]
  pub const fn name(&self) -> &Name<Src, Span> {
    &self.name
  }

  /// Returns the span of the dollar character
  #[inline]
  pub const fn dollar(&self) -> &Dollar<Src, Span> {
    &self.dollar
  }

  /// Returns the span of the variable
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns a parser for the variable value.
  ///
  /// Spec: [Variable Value](https://spec.graphql.org/draft/#sec-Variable-Value)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    Dollar::parser()
      .then_ignore(ignored())
      .then(Name::parser())
      .map_with(|(dollar, name), sp| Variable {
        name,
        span: Spanned::from(sp),
        dollar,
      })
  }
}
