use chumsky::{extra::ParserExtra, prelude::*};

use super::{
  super::{char::Char, name::Name, source::Source, spanned::Spanned, convert::*, language::ignored::ignored},
  punct::{LParen, RParen, Colon},
};

use core::marker::PhantomData;
use std::vec::Vec;

#[derive(Debug, Clone)]
pub struct Argument<Value, Span> {
  span: Span,
  name: Name<Span>,
  colon: Colon<Span>,
  value: Value,
}

impl<Value, Span> AsRef<Span> for Argument<Value, Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Value, Span> IntoSpanned<Span> for Argument<Value, Span> {
  #[inline]
  fn into_spanned(self) -> Span {
    self.span
  }
}

impl<Value, Span> IntoComponents for Argument<Value, Span> {
  type Components = (Span, Name<Span>, Colon<Span>, Value);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.colon, self.value)
  }
}

impl<Value, Span> Argument<Value, Span> {
  /// Returns the span of the argument.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the span of the colon
  #[inline]
  pub const fn colon(&self) -> &Colon<Span> {
    &self.colon
  }

  /// Returns the span of the argument name
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
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
    I: Source<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,
    P: Parser<'src, I, Value, E> + Clone,
  {
    Name::parser()
      .then(
        Colon::parser()
          .padded_by(ignored()),
      )
      .then(value)
      .map_with(|((name, colon), value), sp| Self {
        span: Spanned::from_map_extra(sp),
        name,
        colon,
        value,
      })
  }
}

#[derive(Debug, Clone)]
pub struct Arguments<Arg, Span, Container = Vec<Arg>> {
  span: Span,
  l_paren: LParen<Span>,
  arguments: Container,
  r_paren: RParen<Span>,
  _arg: PhantomData<Arg>,
}

impl<Arg, Span, Container> AsRef<Span> for Arguments<Arg, Span, Container> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Arg, Span, Container> IntoSpanned<Span> for Arguments<Arg, Span, Container> {
  #[inline]
  fn into_spanned(self) -> Span {
    self.span
  }
}

impl<Arg, Span, Container> IntoComponents for Arguments<Arg, Span, Container> {
  type Components = (Span, LParen<Span>, Container, RParen<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_paren, self.arguments, self.r_paren)
  }
}

impl<Arg, Span, Container> Arguments<Arg, Span, Container> {
  /// Returns the span of the arguments.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the left parenthesis of the arguments.
  #[inline]
  pub const fn l_paren(&self) -> &LParen<Span> {
    &self.l_paren
  }

  /// Returns the right parenthesis of the arguments.
  #[inline]
  pub const fn r_paren(&self) -> &RParen<Span> {
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

  /// Returns a parser to parse arguments.
  pub fn parser_with<'src, I, E, P>(arg_parser: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,
    P: Parser<'src, I, Arg, E> + Clone,
    Container: chumsky::container::Container<Arg>,
  {
    // '(' ws? arg+ ')'
    LParen::parser()
      .then_ignore(ignored())
      .then(arg_parser.padded_by(ignored()).repeated().at_least(1).collect())
      .then(RParen::parser())
      .map_with(|((l_paren, arguments), r_paren), sp| Self {
        span: Spanned::from_map_extra(sp),
        l_paren,
        arguments,
        r_paren,
        _arg: PhantomData,
      })
  }
}

impl<Value, Span, Container> Arguments<Value, Span, Container>
where
  Container: AsRef<[Argument<Value, Span>]>,
{
  /// Returns the arguments as a slice.
  #[inline]
  pub fn arguments_slice(&self) -> &[Argument<Value, Span>] {
    self.arguments().as_ref()
  }
}
