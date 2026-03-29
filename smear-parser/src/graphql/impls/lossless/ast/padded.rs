use smear_lexer::tokit::{
  lexer::FromLogos,
  Emitter, InputRef, Lexer, ParseContext, SimpleSpan as Span,
  span::Spanned,
  utils::cmp::Equivalent,
};
use smear_lexer::punctuator::Comma;

use crate::lexer::graphql::lossless::{LosslessLexer, LosslessToken};
use crate::graphql::Expectation;
use crate::hints::LineTerminatorHint;

/// Hint for the type of whitespace encountered.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WhiteSpaceHint {
  /// A space character.
  Space,
  /// A tab character.
  Tab,
}

use super::{LosslessTokenError, LosslessTokenErrors, next_token};

/// A line terminator token preserved from the source.
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

  /// Returns the hint for what type of line terminator this is.
  #[inline]
  pub const fn hint(&self) -> LineTerminatorHint {
    self.hint
  }

  /// Returns the span of this line terminator.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }
}

/// A byte-order mark token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Bom(Span);

impl Bom {
  #[inline]
  const fn new(span: Span) -> Self {
    Self(span)
  }

  /// Returns the span of this byte-order mark.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.0
  }
}

/// A whitespace token preserved from the source.
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

  /// Returns the hint for what type of whitespace this is.
  #[inline]
  pub const fn hint(&self) -> WhiteSpaceHint {
    self.hint
  }

  /// Returns the span of this whitespace token.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }
}

/// A comment token preserved from the source.
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

  /// Returns the span of this comment token.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the content of this comment.
  #[inline]
  pub const fn content(&self) -> &S {
    &self.content
  }
}

/// An ignored token (trivia) preserved in the CST.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Ignored<S> {
  /// Whitespace (space or tab).
  Whitespace(Whitespace),
  /// Line terminator (newline, carriage return, or both).
  LineTerminator(LineTerminator),
  /// Byte order mark.
  ByteOrderMark(Bom),
  /// Comment.
  Comment(Comment<S>),
  /// Comma.
  Comma(Comma),
}

impl<S> Ignored<S> {
  /// Returns the span of this ignored token.
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

/// Tries to parse a single ignored (trivia) token from the lossless input.
///
/// Returns `Ok(Some(ignored))` if a trivia token was consumed, `Ok(None)` if the next
/// token is not trivia, or an error on end-of-input.
fn try_parse_ignored<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
) -> Result<Option<Ignored<S>>, LosslessTokenErrors<S>>
where
  S: Clone,
  LosslessToken<S>: FromLogos<'inp>,
  LosslessLexer<'inp, S>: Lexer<'inp, Token = LosslessToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, LosslessLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, LosslessLexer<'inp, S>, Lang, Error = LosslessTokenErrors<S>>,
  Lang: ?Sized,
{
  let saved = input.save();
  match next_token(input) {
    Ok(Spanned { span, data: token }) => match token {
      LosslessToken::Space => Ok(Some(Ignored::Whitespace(Whitespace::new(WhiteSpaceHint::Space, span)))),
      LosslessToken::Tab => Ok(Some(Ignored::Whitespace(Whitespace::new(WhiteSpaceHint::Tab, span)))),
      LosslessToken::Newline => Ok(Some(Ignored::LineTerminator(LineTerminator::new(LineTerminatorHint::NewLine, span)))),
      LosslessToken::CarriageReturn => Ok(Some(Ignored::LineTerminator(LineTerminator::new(LineTerminatorHint::CarriageReturn, span)))),
      LosslessToken::CarriageReturnAndNewline => Ok(Some(Ignored::LineTerminator(LineTerminator::new(LineTerminatorHint::CarriageReturnNewLine, span)))),
      LosslessToken::Bom(_) => Ok(Some(Ignored::ByteOrderMark(Bom::new(span)))),
      LosslessToken::Comment(content) => Ok(Some(Ignored::Comment(Comment::new(span, content)))),
      LosslessToken::Comma => Ok(Some(Ignored::Comma(Comma::new(span)))),
      _ => {
        input.restore(saved);
        Ok(None)
      }
    },
    Err(_) => {
      input.restore(saved);
      Ok(None)
    }
  }
}

/// Parses zero or more ignored (trivia) tokens from the input.
fn parse_ignored_list<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
) -> Result<std::vec::Vec<Ignored<S>>, LosslessTokenErrors<S>>
where
  S: Clone,
  LosslessToken<S>: FromLogos<'inp>,
  LosslessLexer<'inp, S>: Lexer<'inp, Token = LosslessToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, LosslessLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, LosslessLexer<'inp, S>, Lang, Error = LosslessTokenErrors<S>>,
  Lang: ?Sized,
{
  let mut ignored = std::vec::Vec::new();
  while let Some(ig) = try_parse_ignored(input)? {
    ignored.push(ig);
  }
  Ok(ignored)
}

/// A value padded on the left by ignored tokens.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PaddedLeft<T, S> {
  span: Span,
  ignored: std::vec::Vec<Ignored<S>>,
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
  /// Returns the span covering the padding and value.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the ignored (trivia) tokens before the value.
  #[inline]
  pub const fn ignored(&self) -> &[Ignored<S>] {
    self.ignored.as_slice()
  }

  /// Returns a reference to the inner value.
  #[inline]
  pub const fn value(&self) -> &T {
    &self.value
  }
}

/// A value padded on the right by ignored tokens.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PaddedRight<T, S> {
  span: Span,
  value: T,
  ignored: std::vec::Vec<Ignored<S>>,
}

impl<T, S> core::ops::Deref for PaddedRight<T, S> {
  type Target = T;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.value()
  }
}

impl<T, S> PaddedRight<T, S> {
  /// Returns the span covering the value and trailing padding.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the ignored (trivia) tokens after the value.
  #[inline]
  pub const fn ignored(&self) -> &[Ignored<S>] {
    self.ignored.as_slice()
  }

  /// Returns a reference to the inner value.
  #[inline]
  pub const fn value(&self) -> &T {
    &self.value
  }
}

/// A value padded on both sides by ignored tokens.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Padded<T, S> {
  span: Span,
  left_ignored: std::vec::Vec<Ignored<S>>,
  value: T,
  right_ignored: std::vec::Vec<Ignored<S>>,
}

impl<T, S> core::ops::Deref for Padded<T, S> {
  type Target = T;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.value()
  }
}

impl<T, S> Padded<T, S> {
  /// Returns the span covering all padding and the value.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the ignored (trivia) tokens before the value.
  #[inline]
  pub const fn left_ignored(&self) -> &[Ignored<S>] {
    self.left_ignored.as_slice()
  }

  /// Returns a reference to the inner value.
  #[inline]
  pub const fn value(&self) -> &T {
    &self.value
  }

  /// Returns the ignored (trivia) tokens after the value.
  #[inline]
  pub const fn right_ignored(&self) -> &[Ignored<S>] {
    self.right_ignored.as_slice()
  }
}

/// Parses a value padded on the left by ignored tokens.
pub fn parse_padded_left<'inp, S, Ctx, Lang, T>(
  input: &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
  parse_value: impl FnOnce(&mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>) -> Result<T, LosslessTokenErrors<S>>,
) -> Result<PaddedLeft<T, S>, LosslessTokenErrors<S>>
where
  S: Clone,
  LosslessToken<S>: FromLogos<'inp>,
  LosslessLexer<'inp, S>: Lexer<'inp, Token = LosslessToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, LosslessLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, LosslessLexer<'inp, S>, Lang, Error = LosslessTokenErrors<S>>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  let ignored = parse_ignored_list(input)?;
  let value = parse_value(input)?;
  let span = input.span_since(&cursor);
  Ok(PaddedLeft { span, ignored, value })
}

/// Parses a value padded on the right by ignored tokens.
pub fn parse_padded_right<'inp, S, Ctx, Lang, T>(
  input: &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
  parse_value: impl FnOnce(&mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>) -> Result<T, LosslessTokenErrors<S>>,
) -> Result<PaddedRight<T, S>, LosslessTokenErrors<S>>
where
  S: Clone,
  LosslessToken<S>: FromLogos<'inp>,
  LosslessLexer<'inp, S>: Lexer<'inp, Token = LosslessToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, LosslessLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, LosslessLexer<'inp, S>, Lang, Error = LosslessTokenErrors<S>>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  let value = parse_value(input)?;
  let ignored = parse_ignored_list(input)?;
  let span = input.span_since(&cursor);
  Ok(PaddedRight { span, value, ignored })
}

/// Parses a value padded on both sides by ignored tokens.
pub fn parse_padded<'inp, S, Ctx, Lang, T>(
  input: &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
  parse_value: impl FnOnce(&mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>) -> Result<T, LosslessTokenErrors<S>>,
) -> Result<Padded<T, S>, LosslessTokenErrors<S>>
where
  S: Clone,
  LosslessToken<S>: FromLogos<'inp>,
  LosslessLexer<'inp, S>: Lexer<'inp, Token = LosslessToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, LosslessLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, LosslessLexer<'inp, S>, Lang, Error = LosslessTokenErrors<S>>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  let left_ignored = parse_ignored_list(input)?;
  let value = parse_value(input)?;
  let right_ignored = parse_ignored_list(input)?;
  let span = input.span_since(&cursor);
  Ok(Padded { span, left_ignored, value, right_ignored })
}
