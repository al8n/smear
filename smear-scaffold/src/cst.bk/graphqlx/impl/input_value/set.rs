use core::fmt::Debug;

use logosky::cst::{
  CstNode,
  cast::{children, token},
  error::IncompleteSyntax,
  typenum::U4,
};
use rowan::SyntaxNode;
use smear_lexer::{
  keywords,
  punctuator::{LBrace, RBrace},
};

use crate::cst::{
  Set,
  graphqlx::{GraphQLxLanguage, SyntaxKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum SetSyntax {
  #[display("'set'")]
  SetKeyword,
  #[display("'{{'")]
  LBrace,
  #[display("values")]
  Values,
  #[display("'}}'")]
  RBrace,
}

impl_graphqlx_node! {
  for<Value> Set<Value, GraphQLxLanguage> {
    type Component = SetSyntax;
    type COMPONENTS = U4;
  } => SetValue(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let set_kw = token(&syntax, &SyntaxKind::set_KW);
    let l_brace = token(&syntax, &SyntaxKind::LBrace);
    let values = children::<Value>(&syntax);
    let r_brace = token(&syntax, &SyntaxKind::RBrace);

    match (set_kw, l_brace, r_brace) {
      (Some(set_keyword), Some(l_brace), Some(r_brace)) => {
        Ok(Set::new(
          syntax,
          keywords::Set::with_content(set_keyword.text_range(), set_keyword),
          LBrace::with_content(l_brace.text_range(), l_brace),
          values,
          RBrace::with_content(r_brace.text_range(), r_brace),
        ))
      }
      (set_kw, l_brace, r_brace) => {
        Err(IncompleteSyntax::from_iter([
          set_kw.is_none().then_some(SetSyntax::SetKeyword),
          l_brace.is_none().then_some(SetSyntax::LBrace),
          r_brace.is_none().then_some(SetSyntax::RBrace),
        ].into_iter().flatten()).unwrap().into())
      }
    }
  })
  where
    Value: Debug + CstNode<Language = GraphQLxLanguage>,
}
