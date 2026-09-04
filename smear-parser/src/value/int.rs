use core::{fmt::Display, marker::PhantomData};

use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan, Span as SpanTrait},
  utils::{
    IntoComponents,
    human_display::DisplayHuman,
    sdl_display::{DisplayCompact, DisplayPretty},
    syntax_tree_display::DisplaySyntaxTree,
  },
};

/// An integer value literal.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IntValue<S, Span = SimpleSpan, Lang: ?Sized = ()> {
  span: Span,
  value: S,
  _lang: PhantomData<Lang>,
}

impl<S, Span, Lang: ?Sized> Display for IntValue<S, Span, Lang>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source(), f)
  }
}

impl<S, Span, Lang: ?Sized> AsSpan<Span> for IntValue<S, Span, Lang> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, Lang: ?Sized> IntoSpan<Span> for IntValue<S, Span, Lang> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S, Span, Lang: ?Sized> IntoComponents for IntValue<S, Span, Lang> {
  type Components = (Span, S);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.value)
  }
}

impl<S, Span, Lang: ?Sized> core::ops::Deref for IntValue<S, Span, Lang> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.value
  }
}

impl<S, Span, Lang: ?Sized> IntValue<S, Span, Lang> {
  /// Creates a new int value.
  ///
  /// `S` here is the materialised tree's integer width `I` — the one payload parameter whose hole
  /// is held shut by this constructor's visibility rather than by a bound. The
  /// [`Leaf`](crate::value::Leaf) obligation is written anyway, so publishing this constructor is
  /// a change to one keyword and not a re-run of `al8n/smear#176`'s analysis.
  #[inline]
  pub(crate) const fn new(span: Span, value: S) -> Self
  where
    S: crate::value::Leaf,
    Span: crate::value::Leaf,
  {
    Self {
      span,
      value,
      _lang: PhantomData,
    }
  }

  /// Returns the span covering the integer literal.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the integer literal's source spelling.
  #[inline]
  pub const fn source(&self) -> &S {
    &self.value
  }
}

impl<S, Span, Lang: ?Sized> DisplayCompact for IntValue<S, Span, Lang>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.value.fmt(f)
  }
}

impl<S, Span, Lang: ?Sized> DisplayPretty for IntValue<S, Span, Lang>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.value.fmt(f)
  }
}

impl<S, Span, Lang: ?Sized> DisplaySyntaxTree for IntValue<S, Span, Lang>
where
  S: DisplayHuman,
  Span: SpanTrait,
  <Span as SpanTrait>::Offset: Display,
{
  #[inline]
  fn fmt(
    &self,
    level: usize,
    indent: usize,
    f: &mut core::fmt::Formatter<'_>,
  ) -> core::fmt::Result {
    let padding = level * indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    writeln!(
      f,
      "- INT@{}..{} \"{}\"",
      self.span.start(),
      self.span.end(),
      self.source().display()
    )
  }
}
