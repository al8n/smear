use chumsky::{extra::ParserExtra, prelude::*, text::TextExpected, util::MaybeRef};
use derive_more::{AsMut, AsRef, From, Into};

use super::{
  lang::{directives, punct::At},
  Char, Span,
};

use super::arguments::{Arguments, ConstArguments};

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct Directive<Span>(directives::Directive<Arguments<Span>, Span>);

impl<Span> Directive<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }
  #[inline]
  pub const fn at(&self) -> &At<Span> {
    self.0.at()
  }
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn arguments(&self) -> Option<&Arguments<Span>> {
    self.0.arguments()
  }

  #[inline]
  pub fn into_arguments(self) -> Option<Arguments<Span>> {
    self.0.into_arguments()
  }

  /// Returns a parser for the directive.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
  {
    directives::Directive::parser_with(Arguments::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct ConstDirective<Span>(directives::Directive<ConstArguments<Span>, Span>);

impl<Span> ConstDirective<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }
  #[inline]
  pub const fn at(&self) -> &At<Span> {
    self.0.at()
  }
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn arguments(&self) -> Option<&ConstArguments<Span>> {
    self.0.arguments()
  }

  #[inline]
  pub fn into_arguments(self) -> Option<ConstArguments<Span>> {
    self.0.into_arguments()
  }

  /// Returns a parser for the directive.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
  {
    directives::Directive::parser_with(ConstArguments::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct Directives<Span>(directives::Directives<Directive<Span>, Span>);

impl<Span> Directives<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }
  #[inline]
  pub const fn directives(&self) -> &[Directive<Span>] {
    self.0.directives().as_slice()
  }
  #[inline]
  pub fn into_directives(self) -> Vec<Directive<Span>> {
    self.0.into_directives()
  }

  /// Returns a parser for the directive.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
  {
    directives::Directives::parser_with(Directive::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct ConstDirectives<Span>(directives::Directives<ConstDirective<Span>, Span>);

impl<Span> ConstDirectives<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }
  #[inline]
  pub const fn directives(&self) -> &[ConstDirective<Span>] {
    self.0.directives().as_slice()
  }
  #[inline]
  pub fn into_directives(self) -> Vec<ConstDirective<Span>> {
    self.0.into_directives()
  }

  /// Returns a parser for the directive.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
  {
    directives::Directives::parser_with(ConstDirective::parser()).map(Self)
  }
}
