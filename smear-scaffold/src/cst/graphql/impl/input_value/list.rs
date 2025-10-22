use core::fmt::Debug;

use logosky::cst::{
  CstNode,
  cast::{children, token},
  error::IncompleteSyntax,
  typenum::U3,
};
use rowan::SyntaxNode;
use smear_lexer::punctuator::{LBracket, RBracket};

use crate::cst::{
  List,
  graphql::{GraphQLLanguage, SyntaxKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum ListSyntax {
  #[display("'['")]
  LBracket,
  #[display("elements")]
  Elements,
  #[display("']'")]
  RBracket,
}

impl_graphql_node! {
  for<Value> List<Value, GraphQLLanguage> {
    type Component = ListSyntax;
    type COMPONENTS = U3;
  } => ListValue(|syntax: SyntaxNode<GraphQLLanguage>| {
    let l_bracket = token(&syntax, &SyntaxKind::LBracket).map(|t| LBracket::with_content(t.text_range(), t));
    let r_bracket = token(&syntax, &SyntaxKind::RBracket).map(|t| RBracket::with_content(t.text_range(), t));
    let values = children(&syntax);

    match (l_bracket, r_bracket) {
      (Some(l_bracket), Some(r_bracket)) => {
        Ok(List::new(
          syntax,
          l_bracket,
          values,
          r_bracket,
        ))
      }
      (l_bracket, r_bracket) => {
        Err(IncompleteSyntax::from_iter([
          l_bracket.is_none().then_some(ListSyntax::LBracket),
          r_bracket.is_none().then_some(ListSyntax::RBracket),
        ].into_iter().flatten()).unwrap().into())
      }
    }
  })
  where
    Value: CstNode<Language = GraphQLLanguage>,
}
