use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{
  language::punct::{LParen, RParen},
  Name, SmearChar, Spanned,
};

use std::vec::Vec;

#[derive(Debug, Clone)]
pub struct Argument<Value, Src, Span> {
  span: Spanned<Src, Span>,
  name: Name<Src, Span>,
  colon: Spanned<Src, Span>,
  value: Value,
}

impl<Value, Src, Span> Argument<Value, Src, Span> {
  /// Returns the span of the argument.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the span of the colon
  #[inline]
  pub const fn colon(&self) -> &Spanned<Src, Span> {
    &self.colon
  }

  /// Returns the span of the argument name
  #[inline]
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.name.span()
  }

  /// Returns the value of the argument.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }

  /// Returns a parser for the argument.
  pub fn parser_with<'src, I, E, P>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    P: Parser<'src, I, Value, E> + Clone,
  {
    let ws = super::ignored::ignored();

    Name::<Src, Span>::parser()
      .then(
        just(I::Token::COLON)
          .map_with(|_, sp| Spanned::from(sp))
          .padded_by(ws.clone()), // padding around the colon
      )
      .then(value)
      .map_with(|((name, colon), value), sp| Self {
        span: Spanned::from(sp),
        name,
        colon,
        value,
      })
      .then_ignore(ws)
  }
}

#[derive(Debug, Clone)]
pub struct Arguments<Arg, Src, Span, Container = Vec<Arg>> {
  span: Spanned<Src, Span>,
  l_paren: LParen<Src, Span>,
  arguments: Container,
  r_paren: RParen<Src, Span>,
  _arg: core::marker::PhantomData<Arg>,
}

impl<Arg, Src, Span, Container> Arguments<Arg, Src, Span, Container> {
  /// Returns the span of the arguments.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the left parenthesis of the arguments.
  #[inline]
  pub const fn l_paren(&self) -> &LParen<Src, Span> {
    &self.l_paren
  }

  /// Returns the right parenthesis of the arguments.
  #[inline]
  pub const fn r_paren(&self) -> &RParen<Src, Span> {
    &self.r_paren
  }

  /// Returns the arguments.
  #[inline]
  pub const fn arguments(&self) -> &Container {
    &self.arguments
  }

  /// Consumes the arguments.
  #[inline]
  pub fn into_arguments(self) -> Container {
    self.arguments
  }
}

impl<Value, Src, Span, Container> Arguments<Value, Src, Span, Container>
where
  Container: AsRef<[Argument<Value, Src, Span>]>,
{
  /// Returns the arguments as a slice.
  #[inline]
  pub fn arguments_slice(&self) -> &[Argument<Value, Src, Span>] {
    self.arguments().as_ref()
  }
}

impl<Arg, Src, Span, Container> Arguments<Arg, Src, Span, Container>
where
  Container: chumsky::container::Container<Arg>,
{
  /// Returns a parser to parse arguments.
  pub fn parser_with<'src, I, E, P>(arg: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    P: Parser<'src, I, Arg, E> + Clone,
  {
    let ws = super::ignored::ignored();
    let open = LParen::parser();
    let close = RParen::parser();

    // '(' ws? arg+ ')'
    open
      .then_ignore(ws.clone())
      .then(arg.repeated().at_least(1).collect())
      .then(close)
      .map_with(|((l_paren, arguments), r_paren), sp| Self {
        span: Spanned::from(sp),
        l_paren,
        arguments,
        r_paren,
        _arg: core::marker::PhantomData,
      })
  }
}
