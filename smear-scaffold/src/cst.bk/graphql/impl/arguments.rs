use core::fmt::Debug;

use logosky::cst::{
  CstNode,
  cast::{child, children, token},
  error::IncompleteSyntax,
  typenum::U3,
};
use rowan::SyntaxNode;
use smear_lexer::punctuator::{Colon, LParen, RParen};

use crate::cst::{
  Argument, Arguments,
  graphql::{GraphQLLanguage, SyntaxKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum ArgumentSyntax {
  #[display("argument name")]
  Name,
  #[display("':'")]
  Colon,
  #[display("argument value")]
  Value,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum ArgumentsSyntax {
  #[display("'('")]
  LParen,
  #[display("arguments")]
  Arguments,
  #[display("')'")]
  RParen,
}

impl_graphql_node! {
  for<Name, Value> Argument<Name, Value, GraphQLLanguage> {
    type Component = ArgumentSyntax;
    type COMPONENTS = U3;
  } => Argument(|syntax: SyntaxNode<GraphQLLanguage>| {
    let name = child(&syntax);
    let colon = token(&syntax, &SyntaxKind::Colon)
      .map(|t| Colon::with_content(t.text_range(), t));
    let value = child(&syntax);

    match (name, colon, value) {
      (Some(name), Some(colon), Some(value)) => Ok(Argument::new(syntax, name, colon, value)),
      (name, colon, value) => {
        Err(IncompleteSyntax::from_iter([
          name.is_none().then_some(ArgumentSyntax::Name),
          colon.is_none().then_some(ArgumentSyntax::Colon),
          value.is_none().then_some(ArgumentSyntax::Value),
        ].into_iter().flatten()).unwrap().into())
      }
    }
  })
  where
    Name: CstNode<GraphQLLanguage>,
    Value: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<Arg> Arguments<Arg, GraphQLLanguage> {
    type Component = ArgumentsSyntax;
    type COMPONENTS = U3;
  } => Arguments(|syntax: SyntaxNode<GraphQLLanguage>| {
    let l_paren = token(&syntax, &SyntaxKind::LParen)
      .map(|t| LParen::with_content(t.text_range(), t));
    let arguments_iter = children(&syntax);
    let has_arguments = arguments_iter.clone().next().is_some();
    let r_paren = token(&syntax, &SyntaxKind::RParen)
      .map(|t| RParen::with_content(t.text_range(), t));

    match (l_paren, has_arguments, r_paren) {
      (Some(l_paren), true, Some(r_paren)) => Ok(Arguments::new(
        syntax,
        l_paren,
        arguments_iter,
        r_paren,
      )),
      (l_paren, has_arguments, r_paren) => {
        Err(IncompleteSyntax::from_iter([
          l_paren.is_none().then_some(ArgumentsSyntax::LParen),
          (!has_arguments).then_some(ArgumentsSyntax::Arguments),
          r_paren.is_none().then_some(ArgumentsSyntax::RParen),
        ].into_iter().flatten()).unwrap().into())
      }
    }
  })
  where
    Arg: CstNode<GraphQLLanguage>,
}
