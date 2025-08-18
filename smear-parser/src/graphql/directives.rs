use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};
use derive_more::{AsMut, AsRef, From, Into};

use crate::parser::{
  language::{directives, punct::At},
  SmearChar, Spanned,
};

use super::arguments::{Arguments, ConstArguments};

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct Directive<Src, Span>(directives::Directive<Arguments<Src, Span>, Src, Span>);

impl<Src, Span> Directive<Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    self.0.span()
  }
  #[inline]
  pub const fn at(&self) -> &At<Spanned<Src, Span>> {
    self.0.at()
  }
  #[inline]
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.0.name()
  }

  #[inline]
  pub const fn arguments(&self) -> Option<&Arguments<Src, Span>> {
    self.0.arguments()
  }

  #[inline]
  pub fn into_arguments(self) -> Option<Arguments<Src, Span>> {
    self.0.into_arguments()
  }

  /// Returns a parser for the directive.
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
    directives::Directive::parser_with(Arguments::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct ConstDirective<Src, Span>(directives::Directive<ConstArguments<Src, Span>, Src, Span>);

impl<Src, Span> ConstDirective<Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    self.0.span()
  }
  #[inline]
  pub const fn at(&self) -> &At<Spanned<Src, Span>> {
    self.0.at()
  }
  #[inline]
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.0.name()
  }

  #[inline]
  pub const fn arguments(&self) -> Option<&ConstArguments<Src, Span>> {
    self.0.arguments()
  }

  #[inline]
  pub fn into_arguments(self) -> Option<ConstArguments<Src, Span>> {
    self.0.into_arguments()
  }

  /// Returns a parser for the directive.
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
    directives::Directive::parser_with(ConstArguments::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct Directives<Src, Span>(directives::Directives<Directive<Src, Span>, Src, Span>);

impl<Src, Span> Directives<Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    self.0.span()
  }
  #[inline]
  pub const fn directives(&self) -> &[Directive<Src, Span>] {
    self.0.directives().as_slice()
  }
  #[inline]
  pub fn into_directives(self) -> Vec<Directive<Src, Span>> {
    self.0.into_directives()
  }

  /// Returns a parser for the directive.
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
    directives::Directives::parser_with(Directive::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct ConstDirectives<Src, Span>(directives::Directives<ConstDirective<Src, Span>, Src, Span>);

impl<Src, Span> ConstDirectives<Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    self.0.span()
  }
  #[inline]
  pub const fn directives(&self) -> &[ConstDirective<Src, Span>] {
    self.0.directives().as_slice()
  }
  #[inline]
  pub fn into_directives(self) -> Vec<ConstDirective<Src, Span>> {
    self.0.into_directives()
  }

  /// Returns a parser for the directive.
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
    directives::Directives::parser_with(ConstDirective::parser()).map(Self)
  }
}
