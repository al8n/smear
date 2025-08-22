use chumsky::{extra::ParserExtra, prelude::*, text::TextExpected, util::MaybeRef};
use derive_more::{AsMut, AsRef, From, Into};

use super::{
  language::{
    arguments,
    punct::{LParen, RParen},
  },
  Char, Spanned,
};

use super::value::{ConstInputValue, InputValue};

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct Argument<Span>(arguments::Argument<InputValue<Span>, Span>);

impl<Span> Argument<Span> {
  /// Returns the span of the argument.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the span of the colon
  #[inline]
  pub const fn colon(&self) -> &Span {
    self.0.colon()
  }

  /// Returns the name of the argument.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  /// Returns the value of the argument.
  #[inline]
  pub const fn value(&self) -> &InputValue<Span> {
    self.0.value()
  }

  /// Returns a parser for the argument.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,
  {
    arguments::Argument::parser_with(InputValue::parser()).map(|arg| Self(arg))
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct ConstArgument<Span>(arguments::Argument<ConstInputValue<Span>, Span>);

impl<Span> ConstArgument<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the span of the colon
  #[inline]
  pub const fn colon(&self) -> &Span {
    self.0.colon()
  }

  /// Returns the name of the argument.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  /// Returns the value of the argument.
  #[inline]
  pub const fn value(&self) -> &ConstInputValue<Span> {
    self.0.value()
  }

  /// Returns a parser for the argument.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,
  {
    arguments::Argument::parser_with(ConstInputValue::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct Arguments<Span>(arguments::Arguments<Argument<Span>, Span>);

impl<Span> Arguments<Span> {
  /// Returns the span of the arguments.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the left parenthesis of the arguments.
  #[inline]
  pub const fn l_paren(&self) -> &LParen<Span> {
    self.0.l_paren()
  }

  /// Returns the right parenthesis of the arguments.
  #[inline]
  pub const fn r_paren(&self) -> &RParen<Span> {
    self.0.r_paren()
  }

  /// Returns the list of arguments
  #[inline]
  pub const fn arguments(&self) -> &[Argument<Span>] {
    self.0.arguments().as_slice()
  }

  /// Returns the arguments
  #[inline]
  pub fn into_arguments(self) -> Vec<Argument<Span>> {
    self.0.into_arguments()
  }

  /// Returns a parser for the arguments.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,
  {
    arguments::Arguments::parser_with(Argument::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct ConstArguments<Span>(arguments::Arguments<ConstArgument<Span>, Span>);

impl<Span> ConstArguments<Span> {
  /// Returns the span of the arguments.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the left parenthesis of the arguments.
  #[inline]
  pub const fn l_paren(&self) -> &LParen<Span> {
    self.0.l_paren()
  }

  /// Returns the right parenthesis of the arguments.
  #[inline]
  pub const fn r_paren(&self) -> &RParen<Span> {
    self.0.r_paren()
  }

  /// Returns the list of arguments
  #[inline]
  pub const fn arguments(&self) -> &[ConstArgument<Span>] {
    self.0.arguments().as_slice()
  }

  /// Returns the arguments
  #[inline]
  pub fn into_arguments(self) -> Vec<ConstArgument<Span>> {
    self.0.into_arguments()
  }

  /// Returns a parser for the arguments.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,
  {
    arguments::Arguments::parser_with(ConstArgument::parser()).map(Self)
  }
}
