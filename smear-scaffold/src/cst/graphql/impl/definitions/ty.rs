use core::fmt::Debug;

use logosky::cst::{
  CstNode,
  cast::{child, token},
  error::IncompleteSyntax,
  typenum::{U1, U3},
};
use rowan::SyntaxNode;

use crate::cst::{
  ListType, NamedType,
  graphql::{GraphQLLanguage, SyntaxKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum NamedTypeSyntax {
  #[display("type name")]
  Name,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum ListTypeSyntax {
  #[display("'['")]
  LBracket,
  #[display("type")]
  Type,
  #[display("']'")]
  RBracket,
}

impl_graphql_node! {
  for<Name> NamedType<Name, GraphQLLanguage> {
    type Component = NamedTypeSyntax;
    type COMPONENTS = U1;
  } => NamedType(|syntax: SyntaxNode<GraphQLLanguage>| {
    if child::<Name>(&syntax).is_some() {
      Ok(NamedType::new(syntax))
    } else {
      let missing = IncompleteSyntax::new(NamedTypeSyntax::Name);
      Err(missing.into())
    }
  })
  where
    Name: Debug + CstNode<Language = GraphQLLanguage>,
}

impl_graphql_node! {
  for<Type> ListType<Type, GraphQLLanguage> {
    type Component = ListTypeSyntax;
    type COMPONENTS = U3;
  } => ListType(|syntax: SyntaxNode<GraphQLLanguage>| {
    let l_bracket_missing = token(&syntax, &SyntaxKind::LBracket).is_none();
    let ty_missing = child::<Type>(&syntax).is_none();
    let r_bracket_missing = token(&syntax, &SyntaxKind::RBracket).is_none();

    if l_bracket_missing || ty_missing || r_bracket_missing {
      let missing = [
        l_bracket_missing.then_some(ListTypeSyntax::LBracket),
        ty_missing.then_some(ListTypeSyntax::Type),
        r_bracket_missing.then_some(ListTypeSyntax::RBracket),
      ];
      let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
      Err(missing.into())
    } else {
      Ok(ListType::new(syntax))
    }
  })
  where
    Type: Debug + CstNode<Language = GraphQLLanguage>,
}
