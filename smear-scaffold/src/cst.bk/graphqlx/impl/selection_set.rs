use core::fmt::Debug;

use logosky::cst::{
  CstNode,
  cast::{children, token},
  error::IncompleteSyntax,
  typenum::U3,
};
use rowan::SyntaxNode;
use smear_lexer::punctuator::{LBrace, RBrace};

use crate::cst::{
  SelectionSet, SelectionSetSyntax,
  graphqlx::{GraphQLxLanguage, SyntaxKind},
};

impl_graphqlx_node! {
  for<Selection> SelectionSet<Selection, GraphQLxLanguage> {
    type Component = SelectionSetSyntax;
    type COMPONENTS = U3;
  } => SelectionSet(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let lbrace = token(&syntax, &SyntaxKind::LBrace)
      .map(|t| LBrace::with_content(t.text_range(), t));
    let selections = children::<Selection>(&syntax);
    let rbrace = token(&syntax, &SyntaxKind::RBrace)
      .map(|t| RBrace::with_content(t.text_range(), t));

    let empty = selections.clone().next().is_none();

    match (lbrace, !empty, rbrace) {
      (Some(lbrace), true, Some(rbrace)) => Ok(SelectionSet::new(
        syntax,
        selections,
        lbrace,
        rbrace,
      )),
      (lbrace, has_elements, rbrace) => {
        let missing = [
          lbrace.is_none().then_some(SelectionSetSyntax::LBrace),
          (!has_elements).then_some(SelectionSetSyntax::Selections),
          rbrace.is_none().then_some(SelectionSetSyntax::RBrace),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    Selection: Debug + CstNode<Language = GraphQLxLanguage>,
}
