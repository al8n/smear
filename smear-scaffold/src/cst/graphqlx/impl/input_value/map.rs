use core::fmt::Debug;

use logosky::cst::{
  CstNode,
  cast::{child, children, token},
  error::IncompleteSyntax,
  typenum::{U3, U4},
};
use rowan::SyntaxNode;
use smear_lexer::{
  keywords,
  punctuator::{FatArrow, LBrace, RBrace},
};

use crate::cst::{
  Map, MapEntry,
  graphqlx::{GraphQLxLanguage, SyntaxKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum MapEntrySyntax {
  #[display("key")]
  Key,
  #[display("'=>'")]
  FatArrow,
  #[display("value")]
  Value,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum MapSyntax {
  #[display("'map'")]
  MapKeyword,
  #[display("'{{'")]
  LBrace,
  #[display("entries")]
  Entries,
  #[display("'}}'")]
  RBrace,
}

impl_graphqlx_node! {
  for<Key, Value> MapEntry<Key, Value, GraphQLxLanguage> {
    type Component = MapEntrySyntax;
    type COMPONENTS = U3;
  } => MapEntry(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let key = child::<Key>(&syntax);
    let arrow = token(&syntax, &SyntaxKind::FatArrow).map(|t| FatArrow::with_content(t.text_range(), t));
    let value = child::<Value>(&syntax);

    match (key, arrow, value) {
      (Some(key), Some(arrow), Some(value)) => {
        Ok(MapEntry::new(syntax, key, arrow, value))
      }
      (key, arrow, value) => {
        Err(IncompleteSyntax::from_iter([
          key.is_none().then_some(MapEntrySyntax::Key),
          arrow.is_none().then_some(MapEntrySyntax::FatArrow),
          value.is_none().then_some(MapEntrySyntax::Value),
        ].into_iter().flatten()).unwrap().into())
      }
    }
  })
  where
    Key: CstNode<Language = GraphQLxLanguage>,
    Value: CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Key, Value> Map<Key, Value, GraphQLxLanguage> {
    type Component = MapSyntax;
    type COMPONENTS = U4;
  } => MapValue(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let map_kw = token(&syntax, &SyntaxKind::map_KW).map(|t| keywords::Map::with_content(t.text_range(), t));
    let l_brace = token(&syntax, &SyntaxKind::LBrace).map(|t| LBrace::with_content(t.text_range(), t));
    let r_brace = token(&syntax, &SyntaxKind::RBrace).map(|t| RBrace::with_content(t.text_range(), t));
    let entries = children::<MapEntry<Key, Value, GraphQLxLanguage>>(&syntax);

    match (map_kw, l_brace, r_brace) {
      (Some(map_keyword), Some(l_brace), Some(r_brace)) => {
        Ok(Map::new(
          syntax,
          map_keyword,
          l_brace,
          entries,
          r_brace,
        ))
      }
      (map_kw, l_brace, r_brace) => {
        Err(IncompleteSyntax::from_iter([
          map_kw.is_none().then_some(MapSyntax::MapKeyword),
          l_brace.is_none().then_some(MapSyntax::LBrace),
          r_brace.is_none().then_some(MapSyntax::RBrace),
        ].into_iter().flatten()).unwrap().into())
      }
    }
  })
  where
    Key: CstNode<Language = GraphQLxLanguage>,
    Value: CstNode<Language = GraphQLxLanguage>,
}
