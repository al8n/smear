use core::fmt::Debug;

use logosky::cst::{
  CstNode,
  cast::{child, children, token},
  error::IncompleteSyntax,
  typenum::{U1, U3},
};
use rowan::SyntaxNode;
use smear_lexer::punctuator::At;

use crate::cst::{
  Directive, Directives,
  graphql::{GraphQLLanguage, SyntaxKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum DirectiveSyntax {
  #[display("'@'")]
  At,
  #[display("name")]
  Name,
  #[display("arguments")]
  Arguments,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum DirectivesSyntax {
  #[display("directive")]
  Directive,
}

impl_graphql_node! {
  for<Name, Arguments> Directive<Name, Arguments, GraphQLLanguage> {
    type Component = DirectiveSyntax;
    type COMPONENTS = U3;
  } => Directive(|syntax: SyntaxNode<GraphQLLanguage>| {
    let at = token(&syntax, &SyntaxKind::At)
      .map(|t| At::with_content(t.text_range(), t));
    let name = child(&syntax);
    let arguments = child(&syntax);

    match (at, name) {
      (Some(at), Some(name)) => Ok(Directive::new(syntax, at, name, arguments)),
      (at, name) => {
        Err(IncompleteSyntax::from_iter([
          at.is_none().then_some(DirectiveSyntax::At),
          name.is_none().then_some(DirectiveSyntax::Name),
        ].into_iter().flatten()).unwrap().into())
      }
    }
  })
  where
    Name: CstNode<GraphQLLanguage>,
    Arguments: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<Dir> Directives<Dir, GraphQLLanguage> {
    type Component = DirectivesSyntax;
    type COMPONENTS = U1;
  } => Directives(|syntax: SyntaxNode<GraphQLLanguage>| {
    let directives = children(&syntax);
    let has_directive = directives.clone().next().is_some();
    if has_directive {
      Ok(Directives::new(syntax, directives))
    } else {
      Err(IncompleteSyntax::new(DirectivesSyntax::Directive).into())
    }
  })
  where
    Dir: CstNode<GraphQLLanguage>,
}
