use logosky::utils::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RBracket(Span);

impl RBracket {
  #[inline(always)]
  pub const fn new(span: Span) -> Self {
    Self(span)
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> Span {
    self.0
  }
}

impl core::fmt::Display for RBracket {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "]")
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LBracket(Span);

impl LBracket {
  #[inline(always)]
  pub const fn new(span: Span) -> Self {
    Self(span)
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> Span {
    self.0
  }
}

impl core::fmt::Display for LBracket {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "[")
  }
}
