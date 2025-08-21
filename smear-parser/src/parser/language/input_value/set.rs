use chumsky::{
  container::Container, extra::ParserExtra, input::StrInput, label::LabelError, prelude::*,
  text::TextExpected, util::MaybeRef,
};

use crate::parser::{
  language::punct::{LAngle, RAngle},
  Char, Spanned,
};

#[derive(Debug, Clone)]
pub struct SetValue<T, Src, Span, C = Vec<T>> {
  span: Spanned<Src, Span>,
  l_angle: LAngle<Src, Span>,
  r_angle: RAngle<Src, Span>,
  values: C,
  _marker: core::marker::PhantomData<T>,
}

impl<T, Src, Span, C> SetValue<T, Src, Span, C> {
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }
  pub const fn l_angle(&self) -> &LAngle<Src, Span> {
    &self.l_angle
  }
  pub const fn r_angle(&self) -> &RAngle<Src, Span> {
    &self.r_angle
  }
  pub const fn values(&self) -> &C {
    &self.values
  }
}

impl<T, Src, Span, C> SetValue<T, Src, Span, C>
where
  C: Container<T>,
{
  pub fn parser_with<'src, I, E, P>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    P: Parser<'src, I, T, E> + Clone,
  {
    let ws = super::ignored::ignored();
    let open = LAngle::parser();
    let close = RAngle::parser();

    let elem = value.then_ignore(ws.clone()); // trailing ignored only

    open
      .then_ignore(ws.clone())
      .then(choice((
        // Empty: "<>"
        just(I::Token::GREATER_THAN).rewind().map(|_| C::default()),
        // Non-empty: one-or-more elems; commas are in `ws`
        elem.repeated().at_least(1).collect::<C>(),
      )))
      .then_ignore(ws)
      .then(close)
      .map_with(|((l_angle, values), r_angle), sp| Self {
        span: Spanned::from(sp),
        l_angle,
        r_angle,
        values,
        _marker: core::marker::PhantomData,
      })
  }
}
