use core::fmt::Debug;

use logosky::cst::{
  CstNode,
  cast::{child, children, token},
  error::IncompleteSyntax,
  typenum::U3,
};
use rowan::SyntaxNode;

use smear_lexer::punctuator::{LParen, RParen};

use crate::cst::{
  Argument, Arguments,
  graphqlx::{GraphQLxLanguage, SyntaxKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum ArgumentSyntax {
  #[display("name")]
  Name,
  #[display("':'")]
  Colon,
  #[display("value")]
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

impl_graphqlx_node! {
  for<Name, Value> Argument<Name, Value, GraphQLxLanguage> {
    type Component = ArgumentSyntax;
    type COMPONENTS = U3;
  } => Argument(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let name = child::<Name>(&syntax);
    let colon = token(&syntax, &SyntaxKind::Colon)
      .map(|t| smear_lexer::punctuator::Colon::with_content(t.text_range(), t));
    let value = child::<Value>(&syntax);

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
    Name: CstNode<Language = GraphQLxLanguage>,
    Value: CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Arg> Arguments<Arg, GraphQLxLanguage> {
    type Component = ArgumentsSyntax;
    type COMPONENTS = U3;
  } => Arguments(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let l_paren = token(&syntax, &SyntaxKind::LParen)
      .map(|t| LParen::with_content(t.text_range(), t));
    let arguments_iter = children::<Arg>(&syntax);
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
    Arg: CstNode<Language = GraphQLxLanguage>,
}
