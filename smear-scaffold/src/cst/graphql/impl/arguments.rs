use core::fmt::Debug;

use logosky::cst::error::SyntaxNodeMismatch;

use crate::cst::{Argument, Arguments, Node};

use super::{GraphQLLanguage, SyntaxKind};

impl<Name, Value> Node for Argument<Name, Value, GraphQLLanguage>
where
  Name: Debug,
  Value: Debug,
{
  type Language = GraphQLLanguage;

  const KIND: SyntaxKind = SyntaxKind::Argument;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn can_cast(kind: <Self::Language as rowan::Language>::Kind) -> bool
  where
    Self: Sized,
  {
    matches!(kind, SyntaxKind::Argument)
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

impl<Arg> Node for Arguments<Arg, GraphQLLanguage>
where
  Arg: Debug,
{
  type Language = GraphQLLanguage;

  const KIND: SyntaxKind = SyntaxKind::Arguments;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn can_cast(kind: <Self::Language as rowan::Language>::Kind) -> bool
  where
    Self: Sized,
  {
    matches!(kind, SyntaxKind::Arguments)
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
