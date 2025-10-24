use core::fmt::Debug;

use logosky::cst::{
  CstNode,
  cast::{child, token},
  error::IncompleteSyntax,
  typenum::{U1, U3},
};
use rowan::SyntaxNode;

use smear_lexer::punctuator::{Bang, LBracket, RBracket};

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
    let name = child::<Name>(&syntax);
    let bang = token(&syntax, &SyntaxKind::Bang)
      .map(|t| Bang::with_content(t.text_range(), t));

    match name {
      Some(name) => Ok(NamedType::new(syntax, name, bang)),
      None => {
        let missing = IncompleteSyntax::new(NamedTypeSyntax::Name);
        Err(missing.into())
      }
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
    let l_bracket = token(&syntax, &SyntaxKind::LBracket)
      .map(|t| LBracket::with_content(t.text_range(), t));
    let ty = child::<Type>(&syntax);
    let r_bracket = token(&syntax, &SyntaxKind::RBracket)
      .map(|t| RBracket::with_content(t.text_range(), t));
    let bang = token(&syntax, &SyntaxKind::Bang)
      .map(|t| Bang::with_content(t.text_range(), t));

    match (l_bracket, ty, r_bracket) {
      (Some(l_bracket), Some(ty), Some(r_bracket)) => Ok(ListType::new(
        syntax,
        l_bracket,
        ty,
        r_bracket,
        bang,
      )),
      (l_bracket, ty, r_bracket) => {
        let missing = [
          l_bracket.is_none().then_some(ListTypeSyntax::LBracket),
          ty.is_none().then_some(ListTypeSyntax::Type),
          r_bracket.is_none().then_some(ListTypeSyntax::RBracket),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    Type: Debug + CstNode<Language = GraphQLLanguage>,
}
