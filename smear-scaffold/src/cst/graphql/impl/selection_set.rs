use core::fmt::Debug;

use logosky::cst::{CstNode, error::IncompleteSyntax, typenum::U3};
use rowan::SyntaxNode;
use smear_lexer::punctuator::{LBrace, RBrace};

use crate::cst::{
  SelectionSet, SelectionSetSyntax,
  cast::{children, token},
  graphql::{GraphQLLanguage, SyntaxKind},
};

impl_graphql_node! {
  for<Selection> SelectionSet<Selection, GraphQLLanguage> {
    type Component = SelectionSetSyntax;
    type COMPONENTS = U3;
  } => SelectionSet(|syntax: SyntaxNode<GraphQLLanguage>| {
    let lbrace = token(&syntax, &SyntaxKind::LBrace).map(|t| LBrace::with_content(t.text_range(), t));
    let selections = children(&syntax);
    let rbrace = token(&syntax, &SyntaxKind::RBrace).map(|t| RBrace::with_content(t.text_range(), t));

    let empty_selections = selections.clone().next().is_none();
    match (lbrace, !empty_selections, rbrace) {
      (Some(lbrace), true, Some(rbrace)) => Ok(SelectionSet::new(
        syntax,
        selections,
        lbrace,
        rbrace,
      )),
      (lbrace, empty_selections, rbrace) => {
        let buf = [lbrace.is_none().then_some(SelectionSetSyntax::LBrace), empty_selections.then_some(SelectionSetSyntax::Selections), rbrace.is_none().then_some(SelectionSetSyntax::RBrace)];

        Err(IncompleteSyntax::from_iter(buf.into_iter().flatten()).unwrap().into())
      }
    }
  })
  where Selection: CstNode<GraphQLLanguage>,
}
