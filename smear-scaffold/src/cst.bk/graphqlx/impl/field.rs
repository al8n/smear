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
  graphqlx::{GraphQLxLanguage, SyntaxKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum AliasSyntax {
  #[display("name")]
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

impl_graphqlx_node! {
  for<Name> Alias<Name, GraphQLxLanguage> {
    type Component = AliasSyntax;
    type COMPONENTS = U2;
  } => Alias(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let name = child::<Name>(&syntax);
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
    Name: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<AliasT, Name, Arguments, Directives, SelectionSet>
    Field<AliasT, Name, Arguments, Directives, SelectionSet, GraphQLxLanguage> {
      type Component = FieldSyntax;
      type COMPONENTS = U5;
    } => Field(|syntax: SyntaxNode<GraphQLxLanguage>| {
      let alias = child::<AliasT>(&syntax);
      let name = child::<Name>(&syntax);
      let arguments = child::<Arguments>(&syntax);
      let directives = child::<Directives>(&syntax);
      let selection_set = child::<SelectionSet>(&syntax);

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
    AliasT: Debug + CstNode<Language = GraphQLxLanguage>,
    Name: Debug + CstNode<Language = GraphQLxLanguage>,
    Arguments: Debug + CstNode<Language = GraphQLxLanguage>,
    Directives: Debug + CstNode<Language = GraphQLxLanguage>,
    SelectionSet: Debug + CstNode<Language = GraphQLxLanguage>,
}
