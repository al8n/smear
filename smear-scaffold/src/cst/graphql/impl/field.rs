use core::fmt::Debug;

use logosky::cst::{
  CstNode,
  cast::{child, token},
  error::IncompleteSyntax,
  typenum::{U2, U5},
};
use rowan::SyntaxNode;
use smear_lexer::punctuator::Colon;

use crate::cst::{
  Alias, Field,
  graphql::{GraphQLLanguage, SyntaxKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum AliasSyntax {
  #[display("alias name")]
  Name,
  #[display("':'")]
  Colon,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum FieldSyntax {
  #[display("alias")]
  Alias,
  #[display("name")]
  Name,
  #[display("arguments")]
  Arguments,
  #[display("directives")]
  Directives,
  #[display("selection set")]
  SelectionSet,
}

impl_graphql_node! {
  for<Name> Alias<Name, GraphQLLanguage> {
    type Component = AliasSyntax;
    type COMPONENTS = U2;
  } => Alias(|syntax: SyntaxNode<GraphQLLanguage>| {
    let name = child(&syntax);
    let colon = token(&syntax, &SyntaxKind::Colon)
      .map(|t| Colon::with_content(t.text_range(), t));

    match (name, colon) {
      (Some(name), Some(colon)) => Ok(Alias::new(syntax, name, colon)),
      (name, colon) => {
        Err(IncompleteSyntax::from_iter([
          name.is_none().then_some(AliasSyntax::Name),
          colon.is_none().then_some(AliasSyntax::Colon),
        ].into_iter().flatten()).unwrap().into())
      }
    }
  })
  where
    Name: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<AliasT, Name, Arguments, Directives, SelectionSet>
    Field<AliasT, Name, Arguments, Directives, SelectionSet, GraphQLLanguage> {
      type Component = FieldSyntax;
      type COMPONENTS = U5;
    } => Field(|syntax: SyntaxNode<GraphQLLanguage>| {
      let alias = child(&syntax);
      let name = child(&syntax);
      let arguments = child(&syntax);
      let directives = child(&syntax);
      let selection_set = child(&syntax);

      match name {
        Some(name) => Ok(Field::new(
          syntax,
          alias,
          name,
          arguments,
          directives,
          selection_set,
        )),
        None => Err(IncompleteSyntax::new(FieldSyntax::Name).into()),
      }
    })
  where
    AliasT: CstNode<GraphQLLanguage>,
    Name: CstNode<GraphQLLanguage>,
    Arguments: CstNode<GraphQLLanguage>,
    Directives: CstNode<GraphQLLanguage>,
    SelectionSet: CstNode<GraphQLLanguage>,
}
