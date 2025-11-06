use derive_more::Display;
use logosky::utils::human_display::DisplayHuman;

/// A displayable fragment type path syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("fragment type path")]
pub struct FragmentTypePathSyntax(pub(crate) ());

impl DisplayHuman for FragmentTypePathSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable directive location syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("directive location")]
pub struct DirectiveLocationSyntax(pub(crate) ());

impl DisplayHuman for DirectiveLocationSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable operation type syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("operation type")]
pub struct OperationTypeSyntax(pub(crate) ());

impl DisplayHuman for OperationTypeSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}
