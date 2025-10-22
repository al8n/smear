use super::*;

macro_rules! impl_graphql_node {
  (for<$($generics:tt),*> $ty:ty {
    type Component = $component:ty;
    type COMPONENTS = $components:ty;
  } => $kind:ident($expr:expr) $(where $($where:tt)+)? ) => {
    impl<$($generics),*> $crate::cst::CstElement for $ty
    $(where
      $($where)+
    )?
    {
      type Language = $crate::cst::graphql::GraphQLLanguage;

      const KIND: $crate::cst::graphql::SyntaxKind = $crate::cst::graphql::SyntaxKind::$kind;

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn can_cast(kind: <Self::Language as rowan::Language>::Kind) -> bool
      where
        Self: Sized,
      {
        matches!(kind, $crate::cst::graphql::SyntaxKind::$kind)
      }
    }

    impl<$($generics),*> $crate::cst::CstNode for $ty
    $(where
      $($where)+
    )?
    {
      type Component = $component;
      type COMPONENTS = $components;

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn try_cast_node(
        syntax: rowan::SyntaxNode<Self::Language>,
      ) -> Result<Self, logosky::cst::error::SyntaxError<Self>>
      where
        Self: Sized,
      {
        if <Self as $crate::cst::CstElement>::can_cast(syntax.kind()) {
          $expr(syntax)
        } else {
          Err(logosky::cst::error::SyntaxError::NodeMismatch(logosky::cst::error::CstNodeMismatch::new(syntax)))
        }
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn syntax(&self) -> &rowan::SyntaxNode<Self::Language> {
        self.syntax()
      }
    }
  };
}

mod arguments;
mod definitions;
mod directives;
mod field;
mod fragment;
mod input_value;
mod selection_set;
