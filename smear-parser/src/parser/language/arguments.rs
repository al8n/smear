use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{
  language::punct::{LParen, RParen},
  Name, SmearChar, Spanned,
};

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

  /// Returns the name of the argument.
  #[inline]
  pub const fn name(&self) -> &Name<Src, Span> {
    &self.name
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
pub struct Arguments<Value, Src, Span, Container = std::vec::Vec<Argument<Value, Src, Span>>> {
  span: Spanned<Src, Span>,
  l_paren: LParen<Spanned<Src, Span>>,
  arguments: Container,
  r_paren: RParen<Spanned<Src, Span>>,
  _value: core::marker::PhantomData<Value>,
}

impl<Value, Src, Span, Container> Arguments<Value, Src, Span, Container> {
  /// Returns the span of the arguments.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the left parenthesis of the arguments.
  #[inline]
  pub const fn l_paren(&self) -> &LParen<Spanned<Src, Span>> {
    &self.l_paren
  }

  /// Returns the arguments.
  #[inline]
  pub const fn arguments(&self) -> &Container {
    &self.arguments
  }

  /// Returns the right parenthesis of the arguments.
  #[inline]
  pub const fn r_paren(&self) -> &RParen<Spanned<Src, Span>> {
    &self.r_paren
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

impl<Value, Src, Span, Container> Arguments<Value, Src, Span, Container>
where
  Container: chumsky::container::Container<Argument<Value, Src, Span>>,
{
  /// Returns a parser to parse arguments.
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
    let open = just(I::Token::PAREN_OPEN).map_with(|_, sp| LParen::new(Spanned::from(sp)));
    let close = just(I::Token::PAREN_CLOSE).map_with(|_, sp| RParen::new(Spanned::from(sp)));

    let arg = Argument::<Value, Src, Span>::parser_with(value);

    // '(' ws? arg+ ')'
    open
      .then_ignore(ws.clone())                           // allow ignored right after '('
      .then(arg.repeated().at_least(1).collect())
      .then(close)                                       // no extra ws needed; arg ate trailing ws
      .map_with(|((l_paren, arguments), r_paren), sp| Self {
        span: Spanned::from(sp),
        l_paren,
        arguments,
        r_paren,
        _value: core::marker::PhantomData,
      })
  }
}
