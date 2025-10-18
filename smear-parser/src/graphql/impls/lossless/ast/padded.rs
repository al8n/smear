use chumsky::{IterParser, Parser, extra::ParserExtra, prelude::any};
use logosky::{Lexed, Parseable, utils::Span};

use crate::{
  error::{Error, LineTerminatorHint, WhiteSpaceHint},
  parser::ast::Comma,
};

use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineTerminator {
  hint: LineTerminatorHint,
  span: Span,
}

impl LineTerminator {
  #[inline]
  const fn new(hint: LineTerminatorHint, span: Span) -> Self {
    Self { hint, span }
  }

  #[inline]
  pub const fn hint(&self) -> LineTerminatorHint {
    self.hint
  }

  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }
}

impl<'a> Parseable<'a, LosslessTokenStream<'a>, Token<'a>, LosslessTokenErrors<'a, &'a str>>
  for LineTerminator
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, LosslessTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, LosslessTokenStream<'a>, Error = LosslessTokenErrors<'a, &'a str>> + 'a,
  {
    any().try_map(|res, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        match tok {
          Token::NewLine => Ok(LineTerminator::new(LineTerminatorHint::NewLine, span)),
          Token::CarriageReturn => Ok(LineTerminator::new(
            LineTerminatorHint::CarriageReturn,
            span,
          )),
          Token::CarriageReturnNewLine => Ok(LineTerminator::new(
            LineTerminatorHint::CarriageReturnNewLine,
            span,
          )),
          tok => Err(Error::unexpected_token(tok, TokenKind::LineTerminator, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Bom(Span);

impl Bom {
  #[inline]
  const fn new(span: Span) -> Self {
    Self(span)
  }

  #[inline]
  pub const fn span(&self) -> &Span {
    &self.0
  }
}

impl<'a> Parseable<'a, LosslessTokenStream<'a>, Token<'a>, LosslessTokenErrors<'a, &'a str>>
  for Bom
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, LosslessTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, LosslessTokenStream<'a>, Error = LosslessTokenErrors<'a, &'a str>> + 'a,
  {
    any().try_map(|res, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        match tok {
          Token::Bom => Ok(Bom::new(span)),
          tok => Err(Error::unexpected_token(tok, TokenKind::Bom, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Whitespace {
  hint: WhiteSpaceHint,
  span: Span,
}

impl Whitespace {
  #[inline]
  const fn new(hint: WhiteSpaceHint, span: Span) -> Self {
    Self { hint, span }
  }

  #[inline]
  pub const fn hint(&self) -> WhiteSpaceHint {
    self.hint
  }

  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }
}

impl<'a> Parseable<'a, LosslessTokenStream<'a>, Token<'a>, LosslessTokenErrors<'a, &'a str>>
  for Whitespace
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, LosslessTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, LosslessTokenStream<'a>, Error = LosslessTokenErrors<'a, &'a str>> + 'a,
  {
    any().try_map(|res, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        Ok(match tok {
          Token::Space => Whitespace::new(WhiteSpaceHint::Space, span),
          Token::Tab => Whitespace::new(WhiteSpaceHint::Tab, span),
          tok => return Err(Error::unexpected_token(tok, TokenKind::Whitespace, span).into()),
        })
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Comment<S> {
  span: Span,
  content: S,
}

impl<S> Comment<S> {
  #[inline]
  const fn new(span: Span, content: S) -> Self {
    Self { span, content }
  }

  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  #[inline]
  pub const fn content(&self) -> &S {
    &self.content
  }
}

impl<'a> Parseable<'a, LosslessTokenStream<'a>, Token<'a>, LosslessTokenErrors<'a, &'a str>>
  for Comment<&'a str>
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, LosslessTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, LosslessTokenStream<'a>, Error = LosslessTokenErrors<'a, &'a str>> + 'a,
  {
    any().try_map(|res, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        match tok {
          Token::Comment(content) => Ok(Comment::new(span, content)),
          tok => Err(Error::unexpected_token(tok, TokenKind::Comment, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Ignored<S> {
  Whitespace(Whitespace),
  LineTerminator(LineTerminator),
  ByteOrderMark(Bom),
  Comment(Comment<S>),
  Comma(Comma),
}

impl<S> Ignored<S> {
  pub const fn span(&self) -> &Span {
    match self {
      Self::Whitespace(w) => w.span(),
      Self::LineTerminator(lt) => lt.span(),
      Self::ByteOrderMark(bom) => bom.span(),
      Self::Comment(c) => c.span(),
      Self::Comma(c) => c.span(),
    }
  }
}

impl<'a> Parseable<'a, LosslessTokenStream<'a>, Token<'a>, LosslessTokenErrors<'a, &'a str>>
  for Ignored<&'a str>
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, LosslessTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, LosslessTokenStream<'a>, Error = LosslessTokenErrors<'a, &'a str>> + 'a,
  {
    any().try_map(|res, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        Ok(match tok {
          Token::Space => Self::Whitespace(Whitespace::new(WhiteSpaceHint::Space, span)),
          Token::Tab => Self::Whitespace(Whitespace::new(WhiteSpaceHint::Tab, span)),
          Token::NewLine => {
            Self::LineTerminator(LineTerminator::new(LineTerminatorHint::NewLine, span))
          }
          Token::CarriageReturn => Self::LineTerminator(LineTerminator::new(
            LineTerminatorHint::CarriageReturn,
            span,
          )),
          Token::CarriageReturnNewLine => Self::LineTerminator(LineTerminator::new(
            LineTerminatorHint::CarriageReturnNewLine,
            span,
          )),
          Token::Bom => Self::ByteOrderMark(Bom::new(span)),
          Token::Comment(content) => Self::Comment(Comment::new(span, content)),
          Token::Comma => Self::Comma(Comma::new(span)),
          tok => return Err(Error::unexpected_token(tok, TokenKind::Whitespace, span).into()),
        })
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}

/// A value padded on the left by ignored tokens.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PaddedLeft<T, S> {
  span: Span,
  ignored: Vec<Ignored<S>>,
  value: T,
}

impl<T, S> core::ops::Deref for PaddedLeft<T, S> {
  type Target = T;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.value()
  }
}

impl<T, S> PaddedLeft<T, S> {
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  #[inline]
  pub const fn ignored(&self) -> &[Ignored<S>] {
    self.ignored.as_slice()
  }

  #[inline]
  pub const fn value(&self) -> &T {
    &self.value
  }
}

/// A value padded on the left by ignored tokens.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PaddedRight<T, S> {
  span: Span,
  value: T,
  ignored: Vec<Ignored<S>>,
}

impl<T, S> core::ops::Deref for PaddedRight<T, S> {
  type Target = T;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.value()
  }
}

impl<T, S> PaddedRight<T, S> {
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  #[inline]
  pub const fn ignored(&self) -> &[Ignored<S>] {
    self.ignored.as_slice()
  }

  #[inline]
  pub const fn value(&self) -> &T {
    &self.value
  }
}

impl<'a, T> Parseable<'a, LosslessTokenStream<'a>, Token<'a>, LosslessTokenErrors<'a, &'a str>>
  for PaddedRight<T, &'a str>
where
  T: Parseable<'a, LosslessTokenStream<'a>, Token<'a>, LosslessTokenErrors<'a, &'a str>> + 'a,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, LosslessTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, LosslessTokenStream<'a>, Error = LosslessTokenErrors<'a, &'a str>> + 'a,
  {
    padded_right_parser(T::parser())
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Padded<T, S> {
  span: Span,
  left_ignored: Vec<Ignored<S>>,
  value: T,
  right_ignored: Vec<Ignored<S>>,
}

impl<T, S> core::ops::Deref for Padded<T, S> {
  type Target = T;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.value()
  }
}

impl<T, S> Padded<T, S> {
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  #[inline]
  pub const fn left_ignored(&self) -> &[Ignored<S>] {
    self.left_ignored.as_slice()
  }

  #[inline]
  pub const fn value(&self) -> &T {
    &self.value
  }

  #[inline]
  pub const fn right_ignored(&self) -> &[Ignored<S>] {
    self.right_ignored.as_slice()
  }
}

impl<'a, T> Parseable<'a, LosslessTokenStream<'a>, Token<'a>, LosslessTokenErrors<'a, &'a str>>
  for Padded<T, &'a str>
where
  T: Parseable<'a, LosslessTokenStream<'a>, Token<'a>, LosslessTokenErrors<'a, &'a str>> + 'a,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, LosslessTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, LosslessTokenStream<'a>, Error = LosslessTokenErrors<'a, &'a str>> + 'a,
  {
    padded_parser(T::parser())
  }
}

pub fn padded_parser<'a, V, VP, E>(
  value_parser: VP,
) -> impl Parser<'a, LosslessTokenStream<'a>, Padded<V, &'a str>, E> + Clone
where
  E: ParserExtra<'a, LosslessTokenStream<'a>, Error = LosslessTokenErrors<'a, &'a str>> + 'a,
  VP: Parser<'a, LosslessTokenStream<'a>, V, E> + Clone + 'a,
  V: 'a,
{
  <Ignored<&'a str> as Parseable<
    'a,
    LosslessTokenStream<'a>,
    Token<'a>,
    LosslessTokenErrors<'a, &'a str>,
  >>::parser()
  .repeated()
  .collect()
  .then(value_parser)
  .then(
    <Ignored<&'a str> as Parseable<
      'a,
      LosslessTokenStream<'a>,
      Token<'a>,
      LosslessTokenErrors<'a, &'a str>,
    >>::parser()
    .repeated()
    .collect(),
  )
  .map_with(|((left_ignored, value), right_ignored), exa| Padded {
    span: exa.span(),
    left_ignored,
    value,
    right_ignored,
  })
}

pub fn padded_left_parser<'a, V, VP, E>(
  value_parser: VP,
) -> impl Parser<'a, LosslessTokenStream<'a>, PaddedLeft<V, &'a str>, E> + Clone
where
  E: ParserExtra<'a, LosslessTokenStream<'a>, Error = LosslessTokenErrors<'a, &'a str>> + 'a,
  VP: Parser<'a, LosslessTokenStream<'a>, V, E> + Clone + 'a,
  V: 'a,
{
  <Ignored<&'a str> as Parseable<
    'a,
    LosslessTokenStream<'a>,
    Token<'a>,
    LosslessTokenErrors<'a, &'a str>,
  >>::parser()
  .repeated()
  .collect()
  .then(value_parser)
  .map_with(|(ignored, value), exa| PaddedLeft {
    span: exa.span(),
    ignored,
    value,
  })
}

pub fn padded_right_parser<'a, V, VP, E>(
  value_parser: VP,
) -> impl Parser<'a, LosslessTokenStream<'a>, PaddedRight<V, &'a str>, E> + Clone
where
  E: ParserExtra<'a, LosslessTokenStream<'a>, Error = LosslessTokenErrors<'a, &'a str>> + 'a,
  VP: Parser<'a, LosslessTokenStream<'a>, V, E> + Clone + 'a,
  V: 'a,
{
  value_parser
    .then(
      <Ignored<&'a str> as Parseable<
        'a,
        LosslessTokenStream<'a>,
        Token<'a>,
        LosslessTokenErrors<'a, &'a str>,
      >>::parser()
      .repeated()
      .collect(),
    )
    .map_with(|(value, ignored), exa| PaddedRight {
      span: exa.span(),
      ignored,
      value,
    })
}
