use chumsky::{container::Container, extra::ParserExtra, prelude::*};

use super::super::{
  super::{char::Char, source::Source, spanned::Spanned, convert::*, language::ignored::ignored},
  punct::{LAngle, RAngle},
};

#[derive(Debug, Clone)]
pub struct SetValue<T, Span, C = Vec<T>> {
  span: Span,
  l_angle: LAngle<Span>,
  r_angle: RAngle<Span>,
  values: C,
  _marker: core::marker::PhantomData<T>,
}

impl<T, Span, C> AsRef<Span> for SetValue<T, Span, C> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<T, Span, C> IntoSpanned<Span> for SetValue<T, Span, C> {
  #[inline]
  fn into_spanned(self) -> Span {
    self.span
  }
}

impl<T, Span, C> IntoComponents for SetValue<T, Span, C> {
  type Components = (Span, LAngle<Span>, C, RAngle<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_angle, self.values, self.r_angle)
  }
}

impl<T, Span, C> SetValue<T, Span, C> {
  pub const fn span(&self) -> &Span {
    &self.span
  }
  pub const fn l_angle(&self) -> &LAngle<Span> {
    &self.l_angle
  }
  pub const fn values(&self) -> &C {
    &self.values
  }
  pub const fn r_angle(&self) -> &RAngle<Span> {
    &self.r_angle
  }
  pub fn parser_with<'src, I, E, P>(value_parser: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,
    P: Parser<'src, I, T, E> + Clone,
    C: Container<T>,
  {
    LAngle::parser()
      .then_ignore(ignored())
      .then(choice((
        // Empty: "<>"
        just(I::Token::GREATER_THAN).rewind().map(|_| C::default()),
        // Non-empty: one-or-more elems; commas are in `ws`
        value_parser.padded_by(ignored()).repeated().at_least(1).collect::<C>(),
      )))
      .then_ignore(ignored())
      .then(RAngle::parser())
      .map_with(|((l_angle, values), r_angle), sp| Self {
        span: Spanned::from_map_extra(sp),
        l_angle,
        r_angle,
        values,
        _marker: core::marker::PhantomData,
      })
  }
}

