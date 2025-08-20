use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{
  keywords, language::{ignored::ignored, punct::Ampersand}, Name, SmearChar, Spanned
};



#[derive(Debug, Clone)]
pub struct ImplementInterface<Src, Span> {
  span: Spanned<Src, Span>,
  amp: Option<Ampersand<Src, Span>>,
  name: Name<Src, Span>,
}

impl<Src, Span> ImplementInterface<Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  #[inline]
  pub const fn ampersand(&self) -> Option<&Ampersand<Src, Span>> {
    self.amp.as_ref()
  }

  #[inline]
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.name.span()
  }

  /// First member: `|? Name`  (pipe is optional)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    Ampersand::parser()
      .or_not()
      .then_ignore(ignored())
      .then(Name::parser())
      .map_with(|(amp, name), sp| Self {
        span: Spanned::from(sp),
        amp,
        name,
      })
  }

  /// Subsequent members: `| Name`  (pipe is **required**)
  pub fn parser_with_pipe<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    Ampersand::parser()
      .then_ignore(ignored())
      .then(Name::parser())
      .map_with(|(amp, name), sp| Self {
        span: Spanned::from(sp),
        amp: Some(amp),
        name,
      })
  }
}

#[derive(Debug, Clone)]
pub struct ImplementInterfaces<Src, Span, Container = Vec<ImplementInterface<Src, Span>>> {
  span: Spanned<Src, Span>,
  implements: keywords::Implements<Src, Span>,
  interfaces: Container,
}

impl<Src, Span, Container> ImplementInterfaces<Src, Span, Container> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  #[inline]
  pub const fn implements(&self) -> &keywords::Implements<Src, Span> {
    &self.implements
  }

  #[inline]
  pub const fn interfaces(&self) -> &Container {
    &self.interfaces
  }

  #[inline]
  pub fn into_interfaces(self) -> Container {
    self.interfaces
  }

  #[inline]
  pub fn into_components(
    self,
  ) -> (
    Spanned<Src, Span>,
    keywords::Implements<Src, Span>,
    Container,
  ) {
    (
      self.span,
      self.implements,
      self.interfaces,
    )
  }

  /// Parses a list of implemented interfaces.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    Container: chumsky::container::Container<ImplementInterface<Src, Span>>,
  {
    let ws = ignored();

    keywords::Implements::<Src, Span>::parser()
      .then_ignore(ws.clone())
      .then(
        ImplementInterface::parser()
          .then_ignore(ws.clone())
          .repeated()
          .at_least(1)
          .collect(),
      )
      .map_with(|(implements, interfaces), sp| Self {
        span: Spanned::from(sp),
        implements,
        interfaces,
      })
  }
}

#[derive(Debug, Clone)]
pub struct InterfaceDefinition<FieldsDefinition, Directives, ImplementInterfaces, Src, Span> {
  span: Spanned<Src, Span>,
  description: Option<String<Src, Span>>,
  interface: keywords::Interface<Src, Span>,
  name: Name<Src, Span>,
  implements: Option<ImplementInterfaces>,
  directives: Option<Directives>,
  fields_definition: Option<FieldsDefinition>,
}

