use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{language::punct::At, Name, SmearChar, Spanned};

use std::vec::Vec;

#[derive(Debug, Clone)]
pub struct Directive<Args, Src, Span> {
  span: Spanned<Src, Span>,
  at: At<Src, Span>,
  name: Name<Src, Span>,
  arguments: Option<Args>,
}

impl<Args, Src, Span> Directive<Args, Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }
  #[inline]
  pub const fn at(&self) -> &At<Src, Span> {
    &self.at
  }
  #[inline]
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.name.span()
  }

  #[inline]
  pub const fn arguments(&self) -> Option<&Args> {
    self.arguments.as_ref()
  }

  #[inline]
  pub fn into_arguments(self) -> Option<Args> {
    self.arguments
  }
}

impl<Args, Src, Span> Directive<Args, Src, Span> {
  pub fn parser_with<'src, I, E, P>(args: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    P: Parser<'src, I, Args, E> + Clone,
  {
    let ws = super::ignored::ignored();
    let name = Name::<Src, Span>::parser();
    let args = args.or_not();

    At::parser()
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
pub struct Directives<Directive, Src, Span, Container = Vec<Directive>> {
  span: Spanned<Src, Span>,
  directives: Container,
  _directive: core::marker::PhantomData<Directive>,
}

impl<Directive, Src, Span, Container> Directives<Directive, Src, Span, Container> {
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

impl<Directive, Src, Span, Container> Directives<Directive, Src, Span, Container>
where
  Container: chumsky::container::Container<Directive>,
{
  /// `Directive+` (the nonterminal `Directives` in the spec is one-or-more).
  pub fn parser_with<'src, I, E, P>(directive: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    P: Parser<'src, I, Directive, E> + Clone,
  {
    directive
      .repeated()
      .at_least(1)
      .collect()
      .map_with(|directives, sp| Self {
        span: Spanned::from(sp),
        directives,
        _directive: core::marker::PhantomData,
      })
  }
}
