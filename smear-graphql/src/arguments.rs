use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};
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
pub struct Argument<Src, Span>(arguments::Argument<InputValue<Src, Span>, Src, Span>);

impl<Src, Span> Argument<Src, Span> {
  /// Returns the span of the argument.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    self.0.span()
  }

  /// Returns the span of the colon
  #[inline]
  pub const fn colon(&self) -> &Spanned<Src, Span> {
    self.0.colon()
  }

  /// Returns the name of the argument.
  #[inline]
  pub const fn name(&self) -> &Name<Src, Span> {
    self.0.name()
  }

  /// Returns the value of the argument.
  #[inline]
  pub const fn value(&self) -> &InputValue<Src, Span> {
    self.0.value()
  }

  /// Returns a parser for the argument.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    arguments::Argument::parser_with(InputValue::parser()).map(|arg| Self(arg))
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct ConstArgument<Src, Span>(arguments::Argument<ConstInputValue<Src, Span>, Src, Span>);

impl<Src, Span> ConstArgument<Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    self.0.span()
  }

  /// Returns the span of the colon
  #[inline]
  pub const fn colon(&self) -> &Spanned<Src, Span> {
    self.0.colon()
  }

  /// Returns the name of the argument.
  #[inline]
  pub const fn name(&self) -> &Name<Src, Span> {
    self.0.name()
  }

  /// Returns the value of the argument.
  #[inline]
  pub const fn value(&self) -> &ConstInputValue<Src, Span> {
    self.0.value()
  }

  /// Returns a parser for the argument.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    arguments::Argument::parser_with(ConstInputValue::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct Arguments<Src, Span>(arguments::Arguments<Argument<Src, Span>, Src, Span>);

impl<Src, Span> Arguments<Src, Span> {
  /// Returns the span of the arguments.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    self.0.span()
  }

  /// Returns the left parenthesis of the arguments.
  #[inline]
  pub const fn l_paren(&self) -> &LParen<Src, Span> {
    self.0.l_paren()
  }

  /// Returns the right parenthesis of the arguments.
  #[inline]
  pub const fn r_paren(&self) -> &RParen<Src, Span> {
    self.0.r_paren()
  }

  /// Returns the list of arguments
  #[inline]
  pub const fn arguments(&self) -> &[Argument<Src, Span>] {
    self.0.arguments().as_slice()
  }

  /// Returns the arguments
  #[inline]
  pub fn into_arguments(self) -> Vec<Argument<Src, Span>> {
    self.0.into_arguments()
  }

  /// Returns a parser for the arguments.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    arguments::Arguments::parser_with(Argument::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct ConstArguments<Src, Span>(arguments::Arguments<ConstArgument<Src, Span>, Src, Span>);

impl<Src, Span> ConstArguments<Src, Span> {
  /// Returns the span of the arguments.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    self.0.span()
  }

  /// Returns the left parenthesis of the arguments.
  #[inline]
  pub const fn l_paren(&self) -> &LParen<Src, Span> {
    self.0.l_paren()
  }

  /// Returns the right parenthesis of the arguments.
  #[inline]
  pub const fn r_paren(&self) -> &RParen<Src, Span> {
    self.0.r_paren()
  }

  /// Returns the list of arguments
  #[inline]
  pub const fn arguments(&self) -> &[ConstArgument<Src, Span>] {
    self.0.arguments().as_slice()
  }

  /// Returns the arguments
  #[inline]
  pub fn into_arguments(self) -> Vec<ConstArgument<Src, Span>> {
    self.0.into_arguments()
  }

  /// Returns a parser for the arguments.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    arguments::Arguments::parser_with(ConstArgument::parser()).map(Self)
  }
}
