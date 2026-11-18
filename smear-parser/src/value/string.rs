use core::{fmt::Display, marker::PhantomData};

use smear_lexer::{LitBlockStr, LitInlineStr, LitStr};
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::{DisplayCompact, DisplayHuman, DisplayPretty, IntoComponents},
};

/// A string value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringValue<S, Span = SimpleSpan, Lang: ?Sized = ()> {
  span: Span,
  lit: LitStr<S>,
  _lang: PhantomData<Lang>,
}

impl<S: AsRef<str>, Span, Lang: ?Sized> AsRef<str> for StringValue<S, Span, Lang> {
  #[inline]
  fn as_ref(&self) -> &str {
    self.source().as_ref()
  }
}

impl<S: AsRef<[u8]>, Span, Lang: ?Sized> AsRef<[u8]> for StringValue<S, Span, Lang> {
  #[inline]
  fn as_ref(&self) -> &[u8] {
    self.source().as_ref()
  }
}

impl<S, Span, Lang: ?Sized> AsSpan<Span> for StringValue<S, Span, Lang> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, Lang: ?Sized> IntoSpan<Span> for StringValue<S, Span, Lang> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

/// [`StringValue`] taken apart: every field, in the order it is written.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringValueComponents<S, Span> {
  /// The span covering the string literal.
  pub span: Span,
  /// The string literal, as its lexer token read it.
  pub lit: LitStr<S>,
}

impl<S, Span, Lang: ?Sized> IntoComponents for StringValue<S, Span, Lang> {
  type Components = StringValueComponents<S, Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    StringValueComponents {
      span: self.span,
      lit: self.lit,
    }
  }
}

impl<S, Span, Lang: ?Sized> StringValue<S, Span, Lang> {
  /// Creates a new string value.
  #[inline]
  pub(crate) const fn new(span: Span, lit: LitStr<S>) -> Self
  where
    S: crate::value::Leaf,
    Span: crate::value::Leaf,
  {
    Self {
      span,
      lit,
      _lang: PhantomData,
    }
  }

  /// Returns the span covering the string literal.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the string literal's source spelling.
  #[inline]
  pub const fn source(&self) -> &S {
    self.lit.source_ref()
  }

  /// Returns the content of the string.
  #[inline]
  pub fn content(&self) -> &str
  where
    S: AsRef<str>,
  {
    self.source().as_ref().trim_matches('"')
  }
}

impl<S, Span, Lang: ?Sized> Display for StringValue<S, Span, Lang>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source(), f)
  }
}

/// An inline string value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InlineStringValue<S, Span = SimpleSpan, Lang: ?Sized = ()> {
  span: Span,
  lit: LitInlineStr<S>,
  _lang: PhantomData<Lang>,
}

impl<S: AsRef<str>, Span, Lang: ?Sized> AsRef<str> for InlineStringValue<S, Span, Lang> {
  #[inline]
  fn as_ref(&self) -> &str {
    self.source().as_ref()
  }
}

impl<S: AsRef<[u8]>, Span, Lang: ?Sized> AsRef<[u8]> for InlineStringValue<S, Span, Lang> {
  #[inline]
  fn as_ref(&self) -> &[u8] {
    self.source().as_ref()
  }
}

impl<S, Span, Lang: ?Sized> AsSpan<Span> for InlineStringValue<S, Span, Lang> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, Lang: ?Sized> IntoSpan<Span> for InlineStringValue<S, Span, Lang> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

/// [`InlineStringValue`] taken apart: every field, in the order it is written.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InlineStringValueComponents<S, Span> {
  /// The span covering the inline string literal.
  pub span: Span,
  /// The inline string literal, as its lexer token read it.
  pub lit: LitInlineStr<S>,
}

impl<S, Span, Lang: ?Sized> IntoComponents for InlineStringValue<S, Span, Lang> {
  type Components = InlineStringValueComponents<S, Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    InlineStringValueComponents {
      span: self.span,
      lit: self.lit,
    }
  }
}

impl<S, Span, Lang: ?Sized> Display for InlineStringValue<S, Span, Lang>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source(), f)
  }
}

impl<S, Span, Lang: ?Sized> InlineStringValue<S, Span, Lang> {
  /// Creates a new inline string value.
  #[inline]
  pub(crate) const fn new(span: Span, lit: LitInlineStr<S>) -> Self
  where
    S: crate::value::Leaf,
    Span: crate::value::Leaf,
  {
    Self {
      span,
      lit,
      _lang: PhantomData,
    }
  }

  /// Returns the span covering the inline string literal.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the inline string literal's source spelling.
  #[inline]
  pub const fn source(&self) -> &S {
    self.lit.source_ref()
  }

  /// Returns the content of the inline string.
  #[inline]
  pub fn content(&self) -> &str
  where
    S: AsRef<str>,
  {
    self.source().as_ref().trim_matches('"')
  }
}

impl<S, Span, Lang: ?Sized> DisplayCompact for InlineStringValue<S, Span, Lang>
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
pub struct BlockStringValue<S, Span = SimpleSpan, Lang: ?Sized = ()> {
  span: Span,
  lit: LitBlockStr<S>,
  _lang: PhantomData<Lang>,
}

impl<S: AsRef<str>, Span, Lang: ?Sized> AsRef<str> for BlockStringValue<S, Span, Lang> {
  #[inline]
  fn as_ref(&self) -> &str {
    self.source().as_ref()
  }
}

impl<S: AsRef<[u8]>, Span, Lang: ?Sized> AsRef<[u8]> for BlockStringValue<S, Span, Lang> {
  #[inline]
  fn as_ref(&self) -> &[u8] {
    self.source().as_ref()
  }
}

impl<S, Span, Lang: ?Sized> AsSpan<Span> for BlockStringValue<S, Span, Lang> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, Lang: ?Sized> IntoSpan<Span> for BlockStringValue<S, Span, Lang> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

/// [`BlockStringValue`] taken apart: every field, in the order it is written.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BlockStringValueComponents<S, Span> {
  /// The span covering the block string literal.
  pub span: Span,
  /// The block string literal, as its lexer token read it.
  pub lit: LitBlockStr<S>,
}

impl<S, Span, Lang: ?Sized> IntoComponents for BlockStringValue<S, Span, Lang> {
  type Components = BlockStringValueComponents<S, Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    BlockStringValueComponents {
      span: self.span,
      lit: self.lit,
    }
  }
}

impl<S, Span, Lang: ?Sized> Display for BlockStringValue<S, Span, Lang>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source(), f)
  }
}

impl<S, Span, Lang: ?Sized> BlockStringValue<S, Span, Lang> {
  /// Creates a new block string value.
  #[inline]
  pub(crate) const fn new(span: Span, lit: LitBlockStr<S>) -> Self
  where
    S: crate::value::Leaf,
    Span: crate::value::Leaf,
  {
    Self {
      span,
      lit,
      _lang: PhantomData,
    }
  }

  /// Returns the span covering the block string literal.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the block string literal's source spelling.
  #[inline]
  pub const fn source(&self) -> &S {
    self.lit.source_ref()
  }

  /// Returns the content of the block string.
  #[inline]
  pub fn content(&self) -> &str
  where
    S: AsRef<str>,
  {
    self.source().as_ref().trim_matches('"')
  }
}

impl<S, Span, Lang: ?Sized> DisplayPretty for BlockStringValue<S, Span, Lang>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    DisplayHuman::fmt(self.source(), f)
  }
}
