use core::fmt::Debug;

use logosky::cst::error::SyntaxNodeMismatch;

use crate::cst::{Directive, Directives, Node};

use super::{GraphQLLanguage, SyntaxKind};

impl<Name, Arguments> Node for Directive<Name, Arguments, GraphQLLanguage>
where
  Name: Debug,
  Arguments: Debug,
{
  type Language = GraphQLLanguage;

  const KIND: SyntaxKind = SyntaxKind::Directive;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn can_cast(kind: <Self::Language as rowan::Language>::Kind) -> bool
  where
    Self: Sized,
  {
    matches!(kind, SyntaxKind::Directive)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn try_cast(syntax: rowan::SyntaxNode<Self::Language>) -> Result<Self, SyntaxNodeMismatch<Self>>
  where
    Self: Sized,
  {
    if Self::can_cast(syntax.kind()) {
      Ok(Self::new(syntax))
    } else {
      Err(SyntaxNodeMismatch::new(Self::KIND, syntax))
    }
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn syntax(&self) -> &rowan::SyntaxNode<Self::Language> {
    self.syntax()
  }
}

impl<Dir> Node for Directives<Dir, GraphQLLanguage>
where
  Dir: Debug,
{
  type Language = GraphQLLanguage;

  const KIND: SyntaxKind = SyntaxKind::Directives;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn can_cast(kind: <Self::Language as rowan::Language>::Kind) -> bool
  where
    Self: Sized,
  {
    matches!(kind, SyntaxKind::Directives)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn try_cast(syntax: rowan::SyntaxNode<Self::Language>) -> Result<Self, SyntaxNodeMismatch<Self>>
  where
    Self: Sized,
  {
    if Self::can_cast(syntax.kind()) {
      Ok(Self::new(syntax))
    } else {
      Err(SyntaxNodeMismatch::new(Self::KIND, syntax))
    }
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn syntax(&self) -> &rowan::SyntaxNode<Self::Language> {
    self.syntax()
  }
}
