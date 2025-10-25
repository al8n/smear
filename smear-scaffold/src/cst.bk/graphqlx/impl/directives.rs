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
  graphqlx::{GraphQLxLanguage, SyntaxKind},
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

impl_graphqlx_node! {
  for<Name, Arguments> Directive<Name, Arguments, GraphQLxLanguage> {
    type Component = DirectiveSyntax;
    type COMPONENTS = U3;
  } => Directive(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let at = token(&syntax, &SyntaxKind::At)
      .map(|t| At::with_content(t.text_range(), t));
    let name = child::<Name>(&syntax);
    let arguments = child::<Arguments>(&syntax);

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
    Name: Debug + CstNode<Language = GraphQLxLanguage>,
    Arguments: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Dir> Directives<Dir, GraphQLxLanguage> {
    type Component = DirectivesSyntax;
    type COMPONENTS = U1;
  } => Directives(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let directives_iter = children::<Dir>(&syntax);
    if directives_iter.clone().next().is_some() {
      Ok(Directives::new(syntax, directives_iter))
    } else {
      let missing = IncompleteSyntax::new(DirectivesSyntax::Directive);
      Err(missing.into())
    }
  })
  where
    Dir: Debug + CstNode<Language = GraphQLxLanguage>,
}
