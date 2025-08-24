use chumsky::{extra::ParserExtra, prelude::*};
use derive_more::{AsMut, AsRef, From, Into};

use smear_parser::{
  lang::{self, punct::At, Const, Name},
  source::{self, Char, Slice, Source},
};

use super::arguments::{Arguments, ConstArguments};

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct Directive<Span>(lang::Directive<Arguments<Span>, Span>);

impl<Span> Const<false> for Directive<Span> {}

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
    Span: source::Span<'src, I, E>,
  {
    lang::Directive::parser_with(Arguments::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct ConstDirective<Span>(lang::Directive<ConstArguments<Span>, Span>);

impl<Span> Const<true> for ConstDirective<Span> {}

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
    Span: source::Span<'src, I, E>,
  {
    lang::Directive::parser_with(ConstArguments::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct Directives<Span>(lang::Directives<Directive<Span>, Span>);

impl<Span> Const<false> for Directives<Span> {}

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
    Span: source::Span<'src, I, E>,
  {
    lang::Directives::parser_with(Directive::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct ConstDirectives<Span>(lang::Directives<ConstDirective<Span>, Span>);

impl<Span> Const<true> for ConstDirectives<Span> {}

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
    Span: source::Span<'src, I, E>,
  {
    lang::Directives::parser_with(ConstDirective::parser()).map(Self)
  }
}
