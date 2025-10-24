use core::fmt::Debug;

use logosky::cst::{
  CstNode,
  cast::{child, children, token},
  error::IncompleteSyntax,
  typenum::{U2, U3},
};
use rowan::SyntaxNode;
use smear_lexer::punctuator::{Colon, Equal, LBrace, RBrace};

use crate::cst::{
  DefaultInputValue, Object, ObjectField,
  graphql::{GraphQLLanguage, SyntaxKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum DefaultInputValueSyntax {
  #[display("'='")]
  Equal,
  #[display("value")]
  Value,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum ObjectFieldSyntax {
  #[display("field name")]
  Name,
  #[display("':'")]
  Colon,
  #[display("field value")]
  Value,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum ObjectSyntax {
  #[display("'{{'")]
  LBrace,
  #[display("fields")]
  Fields,
  #[display("'}}'")]
  RBrace,
}

impl_graphql_node! {
  for<Value> DefaultInputValue<Value, GraphQLLanguage> {
    type Component = DefaultInputValueSyntax;
    type COMPONENTS = U2;
  } => DefaultValue(|syntax: SyntaxNode<GraphQLLanguage>| {
    let equal = token(&syntax, &SyntaxKind::Equal).map(|t| Equal::with_content(t.text_range(), t));
    let value = child::<Value, _>(&syntax);

    match (equal, value) {
      (Some(equal), Some(value)) => Ok(DefaultInputValue::new(syntax, equal, value)),
      (equal, value) => {
        Err(IncompleteSyntax::from_iter([
          equal.is_none().then_some(DefaultInputValueSyntax::Equal),
          value.is_none().then_some(DefaultInputValueSyntax::Value),
        ].into_iter().flatten()).unwrap().into())
      }
    }
  })
  where
    Value: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<Name, Value> ObjectField<Name, Value, GraphQLLanguage> {
    type Component = ObjectFieldSyntax;
    type COMPONENTS = U3;
  } => ObjectField(|syntax: SyntaxNode<GraphQLLanguage>| {
    let name = child(&syntax);
    let colon = token(&syntax, &SyntaxKind::Colon).map(|t| Colon::with_content(t.text_range(), t));
    let value = child(&syntax);

    match (name, colon, value) {
      (Some(name), Some(colon), Some(value)) => Ok(ObjectField::new(syntax, name, colon, value)),
      (name, colon, value) => {
        Err(IncompleteSyntax::from_iter([
          name.is_none().then_some(ObjectFieldSyntax::Name),
          colon.is_none().then_some(ObjectFieldSyntax::Colon),
          value.is_none().then_some(ObjectFieldSyntax::Value),
        ].into_iter().flatten()).unwrap().into())
      }
    }
  })
  where
    Name: CstNode<GraphQLLanguage>,
    Value: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<Name, Value> Object<Name, Value, GraphQLLanguage> {
    type Component = ObjectSyntax;
    type COMPONENTS = U3;
  } => ObjectValue(|syntax: SyntaxNode<GraphQLLanguage>| {
    let l_brace = token(&syntax, &SyntaxKind::LBrace).map(|t| LBrace::with_content(t.text_range(), t));
    let r_brace = token(&syntax, &SyntaxKind::RBrace).map(|t| RBrace::with_content(t.text_range(), t));

    let fields = children(&syntax);

    match (l_brace, r_brace) {
      (Some(l_brace), Some(r_brace)) => Ok(Object::new(syntax, l_brace, fields, r_brace)),
      (l_brace, r_brace) => {
        Err(IncompleteSyntax::from_iter([
          l_brace.is_none().then_some(ObjectSyntax::LBrace),
          r_brace.is_none().then_some(ObjectSyntax::RBrace),
        ].into_iter().flatten()).unwrap().into())
      }
    }
  })
  where
    Name: CstNode<GraphQLLanguage>,
    Value: CstNode<GraphQLLanguage>,
}

mod list;
