use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use super::arguments::{Argument, Arguments};
use crate::parser::{language::punct::At, Name, SmearChar, Spanned};

#[derive(Debug, Clone)]
pub struct Directive<Value, Src, Span, Container = std::vec::Vec<Argument<Value, Src, Span>>> {
  span: Spanned<Src, Span>,
  at: At<Spanned<Src, Span>>,
  name: Name<Src, Span>,
  arguments: Option<Arguments<Value, Src, Span, Container>>,
}

impl<Value, Src, Span, Container> Directive<Value, Src, Span, Container> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }
  #[inline]
  pub const fn at(&self) -> &At<Spanned<Src, Span>> {
    &self.at
  }
  #[inline]
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.name.span()
  }
  #[inline]
  pub const fn arguments(&self) -> Option<&Arguments<Value, Src, Span, Container>> {
    self.arguments.as_ref()
  }

  #[inline]
  pub fn into_arguments(self) -> Option<Arguments<Value, Src, Span, Container>> {
    self.arguments
  }
}

impl<Value, Src, Span, Container> Directive<Value, Src, Span, Container>
where
  Container: chumsky::container::Container<Argument<Value, Src, Span>>,
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
    P: Parser<'src, I, Value, E> + Clone,
  {
    let ws = super::ignored::ignored();

    let at_tok = just(I::Token::AT).map_with(|_, sp| At::new(Spanned::from(sp)));
    let name = Name::<Src, Span>::parser();
    let args = Arguments::<Value, Src, Span, Container>::parser_with(value).or_not();

    at_tok
      .then_ignore(ws.clone())     // allow ignored between '@' and the name
      .then(name)
      .then_ignore(ws.clone())     // allow ignored before optional '('
      .then(args)
      .map_with(|((at, name), arguments), sp| Self {
        span: Spanned::from(sp),
        at,
        name,
        arguments,
      })
      .then_ignore(ws) // trailing ignored so lists of directives repeat cleanly
  }
}

#[derive(Debug, Clone)]
pub struct Directives<
  Value,
  Src,
  Span,
  Args = std::vec::Vec<Argument<Value, Src, Span>>,
  Container = std::vec::Vec<Directive<Value, Src, Span, Args>>,
> {
  span: Spanned<Src, Span>,
  directives: Container,
  _value: core::marker::PhantomData<Value>,
  _args: core::marker::PhantomData<Args>,
}

impl<Value, Src, Span, Args, Container> Directives<Value, Src, Span, Args, Container> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }
  #[inline]
  pub const fn directives(&self) -> &Container {
    &self.directives
  }
  #[inline]
  pub fn into_directives(self) -> Container {
    self.directives
  }
}

impl<Value, Src, Span, Args, Container> Directives<Value, Src, Span, Args, Container>
where
  Container: chumsky::container::Container<Directive<Value, Src, Span, Args>>,
  Args: chumsky::container::Container<Argument<Value, Src, Span>>,
{
  /// `Directive+` (the nonterminal `Directives` in the spec is one-or-more).
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
    let dir = Directive::<Value, Src, Span, Args>::parser_with(value);

    dir
      .repeated()
      .at_least(1)
      .collect()
      .map_with(|directives, sp| Self {
        span: Spanned::from(sp),
        directives,
        _value: core::marker::PhantomData,
        _args: core::marker::PhantomData,
      })
  }
}
