use chumsky::{
  container::Container, extra::ParserExtra, input::StrInput, label::LabelError, prelude::*,
  text::TextExpected, util::MaybeRef,
};

use crate::parser::{
  language::punct::{LAngle, RAngle},
  SmearChar, Spanned,
};

#[derive(Debug, Clone)]
pub struct MapEntry<T, Src, Span> {
  span: Spanned<Src, Span>,
  colon: Spanned<Src, Span>,
  key: T,
  value: T,
}

impl<T, Src, Span> MapEntry<T, Src, Span> {
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }
  pub const fn colon(&self) -> &Spanned<Src, Span> {
    &self.colon
  }
  pub const fn key(&self) -> &T {
    &self.key
  }
  pub const fn value(&self) -> &T {
    &self.value
  }

  /// key ':' value  (only **trailing** ignored so repeats stop cleanly at '>')
  pub fn parser_with<'src, I, E, P>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    P: Parser<'src, I, T, E> + Clone,
  {
    let ws = super::ignored::ignored();

    value
      .clone()
      .then(
        just(I::Token::COLON)
          .map_with(|_, sp| Spanned::from(sp))
          .padded_by(ws.clone()), // padding around ':'
      )
      .then(value)
      .map_with(|((key, colon), value), sp| Self {
        key,
        colon,
        value,
        span: Spanned::from(sp),
      })
      .then_ignore(ws) // trailing ignored (incl. commas)
  }
}

#[derive(Debug, Clone)]
pub struct Map<T, Src, Span, C = Vec<MapEntry<T, Src, Span>>> {
  span: Spanned<Src, Span>,
  l_angle: LAngle<Spanned<Src, Span>>,
  r_angle: RAngle<Spanned<Src, Span>>,
  fields: C,
  _value: core::marker::PhantomData<T>,
}

impl<T, Src, Span, C> Map<T, Src, Span, C> {
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }
  pub const fn l_angle(&self) -> &LAngle<Spanned<Src, Span>> {
    &self.l_angle
  }
  pub const fn r_angle(&self) -> &RAngle<Spanned<Src, Span>> {
    &self.r_angle
  }
  pub const fn fields(&self) -> &C {
    &self.fields
  }
}

impl<T, Src, Span, C> Map<T, Src, Span, C>
where
  C: Container<MapEntry<T, Src, Span>>,
{
  pub fn parser_with<'src, I, E, P>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    P: Parser<'src, I, T, E> + Clone,
  {
    let ws = super::ignored::ignored();
    let colon = just(I::Token::COLON);

    let open = just(I::Token::LESS_THAN).map_with(|_, sp| LAngle::new(Spanned::from(sp)));
    let close = just(I::Token::GREATER_THAN).map_with(|_, sp| RAngle::new(Spanned::from(sp)));

    let entry = MapEntry::<T, Src, Span>::parser_with(value);

    open
      .then_ignore(ws.clone())
      .then(choice((
        // Empty sentinel: "<:>"
        colon
          .clone()
          .then_ignore(ws.clone())
          .then_ignore(just(I::Token::GREATER_THAN).rewind())
          .map(|_| C::default()),
        // Non-empty: one-or-more entries; commas live in `ws`
        entry.repeated().at_least(1).collect::<C>(),
      )))
      .then_ignore(ws)
      .then(close)
      .map_with(|((l_angle, fields), r_angle), sp| Self {
        span: Spanned::from(sp),
        l_angle,
        r_angle,
        fields,
        _value: core::marker::PhantomData,
      })
  }
}
