use logosky::utils::{Span, human_display::DisplayHuman, syntax_tree_display::DisplaySyntaxTree};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RBrace(Span);

impl RBrace {
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

impl core::fmt::Display for RBrace {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "}}")
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LBrace(Span);

impl LBrace {
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

impl core::fmt::Display for LBrace {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "{{")
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Dollar(Span);

impl Dollar {
  #[inline(always)]
  pub const fn new(span: Span) -> Self {
    Self(span)
  }

  /// Returns the span of the name.
  #[inline(always)]
  pub const fn span(&self) -> Span {
    self.0
  }
}

impl core::fmt::Display for Dollar {
  #[inline(always)]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "$")
  }
}

impl DisplayHuman for Dollar {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Display::fmt(self, f)
  }
}

impl DisplaySyntaxTree for Dollar {
  #[inline]
  fn fmt(
    &self,
    level: usize,
    indent: usize,
    f: &mut core::fmt::Formatter<'_>,
  ) -> core::fmt::Result {
    let padding = level * indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    writeln!(f, "- DOLLAR@{}..{}", self.span().start(), self.span().end())
  }
}
