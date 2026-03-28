use core::fmt::Display;

use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::{
    IntoComponents,
    human_display::DisplayHuman,
    sdl_display::{DisplayCompact, DisplayPretty},
  },
};

use crate::{error::ParseVariableValueError, hints::VariableValueHint};

/// A variable value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariableValue<Name> {
  span: Span,
  name: Name,
}

impl<Name> AsSpan<Span> for VariableValue<Name> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name> IntoSpan<Span> for VariableValue<Name> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name> IntoComponents for VariableValue<Name> {
  type Components = (Span, Name);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name)
  }
}

impl<Name> core::ops::Deref for VariableValue<Name> {
  type Target = Name;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.name()
  }
}

impl<Name> Display for VariableValue<Name>
where
  Name: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "${}", self.name.display())
  }
}

impl<Name> VariableValue<Name> {
  /// Creates a new variable from the given span and name.
  #[inline(always)]
  pub(crate) const fn new(span: Span, name: Name) -> Self {
    Self { span, name }
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the name as a string slice.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }
}

impl<Name> DisplayCompact for VariableValue<Name>
where
  Name: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    core::fmt::Display::fmt(self, f)
  }
}

impl<Name> DisplayPretty for VariableValue<Name>
where
  Name: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    core::fmt::Display::fmt(self, f)
  }
}
