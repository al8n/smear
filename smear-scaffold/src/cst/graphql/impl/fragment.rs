use core::fmt::Debug;

use logosky::cst::{
  CstNode,
  cast::{child, token},
  error::IncompleteSyntax,
  typenum::{U2, U3, U4},
};
use rowan::SyntaxNode;
use smear_lexer::{keywords::On, punctuator::Spread};

use crate::cst::{
  FragmentSpread, InlineFragment, TypeCondition,
  graphql::{GraphQLLanguage, SyntaxKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum TypeConditionSyntax {
  #[display("'on'")]
  On,
  #[display("type name")]
  Name,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum FragmentSpreadSyntax {
  #[display("'...'")]
  Spread,
  #[display("fragment name")]
  FragmentName,
  #[display("directives")]
  Directives,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum InlineFragmentSyntax {
  #[display("'...'")]
  Spread,
  #[display("type condition")]
  TypeCondition,
  #[display("directives")]
  Directives,
  #[display("selection set")]
  SelectionSet,
}

impl_graphql_node! {
  for<Name> TypeCondition<Name, GraphQLLanguage> {
    type Component = TypeConditionSyntax;
    type COMPONENTS = U2;
  } => TypeCondition(|syntax: SyntaxNode<GraphQLLanguage>| {
    let on = token(&syntax, &SyntaxKind::on_KW)
      .map(|t| On::with_content(t.text_range(), t));
    let name = child::<Name>(&syntax);

    match (on, name) {
      (Some(on), Some(name)) => Ok(TypeCondition::new(syntax, on, name)),
      (on, name) => {
        Err(IncompleteSyntax::from_iter([
          on.is_none().then_some(TypeConditionSyntax::On),
          name.is_none().then_some(TypeConditionSyntax::Name),
        ].into_iter().flatten()).unwrap().into())
      }
    }
  })
  where
    Name: CstNode<Language = GraphQLLanguage>,
}

impl_graphql_node! {
  for<FragmentName, Directives> FragmentSpread<FragmentName, Directives, GraphQLLanguage> {
    type Component = FragmentSpreadSyntax;
    type COMPONENTS = U3;
  } => FragmentSpread(|syntax: SyntaxNode<GraphQLLanguage>| {
    let spread = token(&syntax, &SyntaxKind::Spread)
      .map(|t| Spread::with_content(t.text_range(), t));
    let name = child::<FragmentName>(&syntax);
    let directives = child::<Directives>(&syntax);

    match (spread, name) {
      (Some(spread), Some(name)) => Ok(FragmentSpread::new(syntax, spread, name, directives)),
      (spread, name) => {
        Err(IncompleteSyntax::from_iter([
          spread.is_none().then_some(FragmentSpreadSyntax::Spread),
          name.is_none().then_some(FragmentSpreadSyntax::FragmentName),
        ].into_iter().flatten()).unwrap().into())
      }
    }
  })
  where
    FragmentName: CstNode<Language = GraphQLLanguage>,
    Directives: CstNode<Language = GraphQLLanguage>,
}

impl_graphql_node! {
  for<TypeCond, Directives, SelectionSet>
    InlineFragment<TypeCond, Directives, SelectionSet, GraphQLLanguage> {
      type Component = InlineFragmentSyntax;
      type COMPONENTS = U4;
    } => InlineFragment(|syntax: SyntaxNode<GraphQLLanguage>| {
      let spread = token(&syntax, &SyntaxKind::Spread)
        .map(|t| Spread::with_content(t.text_range(), t));
      let type_condition = child::<TypeCond>(&syntax);
      let directives = child::<Directives>(&syntax);
      let selection_set = child::<SelectionSet>(&syntax);

      match (spread, selection_set) {
        (Some(spread), Some(selection_set)) => Ok(InlineFragment::new(
          syntax,
          spread,
          type_condition,
          directives,
          selection_set,
        )),
        (spread, selection_set) => {
          Err(IncompleteSyntax::from_iter([
            spread.is_none().then_some(InlineFragmentSyntax::Spread),
            selection_set.is_none().then_some(InlineFragmentSyntax::SelectionSet),
          ].into_iter().flatten()).unwrap().into())
        }
      }
    })
  where
    TypeCond: CstNode<Language = GraphQLLanguage>,
    Directives: CstNode<Language = GraphQLLanguage>,
    SelectionSet: CstNode<Language = GraphQLLanguage>,
}
