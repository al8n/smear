use logosky::utils::{
  AsSpan, IntoComponents, IntoSpan, Span,
  human_display::DisplayHuman,
  sdl_display::{DisplayCompact, DisplayPretty},
};

use core::fmt::Display;

use crate::lexer::{LitBlockStr, LitInlineStr, LitStr};

/// A string value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringValue<S> {
  span: Span,
  lit: LitStr<S>,
}

impl<S: AsRef<str>> AsRef<str> for StringValue<S> {
  #[inline]
  fn as_ref(&self) -> &str {
    self.lit.source_ref().as_ref()
  }
}

impl<S: AsRef<[u8]>> AsRef<[u8]> for StringValue<S> {
  #[inline]
  fn as_ref(&self) -> &[u8] {
    self.lit.source_ref().as_ref()
  }
}

impl<S> AsSpan<Span> for StringValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S> IntoSpan<Span> for StringValue<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S> IntoComponents for StringValue<S> {
  type Components = (Span, LitStr<S>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.lit)
  }
}

impl<S> StringValue<S> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(crate) const fn new(span: Span, lit: LitStr<S>) -> Self {
    Self { span, lit }
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(crate) const fn block(span: Span, lit: LitBlockStr<S>) -> Self {
    Self::new(span, LitStr::Block(lit))
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(crate) const fn inline(span: Span, lit: LitInlineStr<S>) -> Self {
    Self::new(span, LitStr::Inline(lit))
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the underlying source.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    self.lit.source()
  }

  /// Returns the reference to the underlying source.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source_ref(&self) -> &S {
    self.lit.source_ref()
  }

  /// Returns the content of the string.
  #[inline]
  pub fn content(&self) -> &str
  where
    S: AsRef<str>,
  {
    self.lit.source_ref().as_ref().trim_matches('"')
  }
}

impl<S> Display for StringValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source_ref(), f)
  }
}

/// A inline string value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InlineStringValue<S> {
  span: Span,
  lit: LitInlineStr<S>,
}

impl<S: AsRef<str>> AsRef<str> for InlineStringValue<S> {
  #[inline]
  fn as_ref(&self) -> &str {
    self.lit.source_ref().as_ref()
  }
}

impl<S: AsRef<[u8]>> AsRef<[u8]> for InlineStringValue<S> {
  #[inline]
  fn as_ref(&self) -> &[u8] {
    self.lit.source_ref().as_ref()
  }
}

impl<S> AsSpan<Span> for InlineStringValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S> IntoSpan<Span> for InlineStringValue<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S> IntoComponents for InlineStringValue<S> {
  type Components = (Span, LitInlineStr<S>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.lit)
  }
}

impl<S> Display for InlineStringValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source_ref(), f)
  }
}

impl<S> InlineStringValue<S> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(crate) const fn new(span: Span, lit: LitInlineStr<S>) -> Self {
    Self { span, lit }
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the underlying source.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    self.lit.source()
  }

  /// Returns the reference to the underlying source.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source_ref(&self) -> &S {
    self.lit.source_ref()
  }

  /// Returns the content of the inline string.
  #[inline]
  pub fn content(&self) -> &str
  where
    S: AsRef<str>,
  {
    self.lit.source_ref().as_ref().trim_matches('"')
  }
}

impl<S> DisplayCompact for InlineStringValue<S>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    core::fmt::Display::fmt(&self, f)
  }
}

/// A block string value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BlockStringValue<S> {
  span: Span,
  lit: LitBlockStr<S>,
}

impl<S: AsRef<str>> AsRef<str> for BlockStringValue<S> {
  #[inline]
  fn as_ref(&self) -> &str {
    self.lit.source_ref().as_ref()
  }
}

impl<S: AsRef<[u8]>> AsRef<[u8]> for BlockStringValue<S> {
  #[inline]
  fn as_ref(&self) -> &[u8] {
    self.lit.source_ref().as_ref()
  }
}

impl<S> AsSpan<Span> for BlockStringValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S> IntoSpan<Span> for BlockStringValue<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S> IntoComponents for BlockStringValue<S> {
  type Components = (Span, LitBlockStr<S>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.lit)
  }
}

impl<S> Display for BlockStringValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source_ref(), f)
  }
}

impl<S> BlockStringValue<S> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(crate) const fn new(span: Span, lit: LitBlockStr<S>) -> Self {
    Self { span, lit }
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the underlying source.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    self.lit.source()
  }

  /// Returns the reference to the underlying source.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source_ref(&self) -> &S {
    self.lit.source_ref()
  }

  /// Returns the content of the block string.
  #[inline]
  pub fn content(&self) -> &str
  where
    S: AsRef<str>,
  {
    self.lit.source_ref().as_ref().trim_matches('"')
  }
}

impl<S> DisplayPretty for BlockStringValue<S>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    DisplayHuman::fmt(self.source_ref(), f)
  }
}
