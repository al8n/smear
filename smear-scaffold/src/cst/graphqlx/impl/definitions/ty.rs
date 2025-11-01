use core::fmt::Debug;

use logosky::cst::{
  CstNode,
  cast::{child, token},
  error::IncompleteSyntax,
  typenum::{U1, U3, U4, U6},
};
use rowan::SyntaxNode;

use smear_lexer::{
  keywords::{Map, Set},
  punctuator::{Bang, FatArrow, LBrace, LBracket, RBrace, RBracket},
};

use crate::cst::{
  AngleType, ListType, MapType, NamedType, SetType,
  graphqlx::{GraphQLxLanguage, SyntaxKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum NamedTypeSyntax {
  #[display("type name")]
  Name,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum ListTypeSyntax {
  #[display("'['")]
  LBracket,
  #[display("type")]
  Type,
  #[display("']'")]
  RBracket,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum SetTypeSyntax {
  #[display("'set'")]
  SetKeyword,
  #[display("'{'")]
  LBrace,
  #[display("type")]
  Type,
  #[display("'}'")]
  RBrace,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum MapTypeSyntax {
  #[display("'map'")]
  MapKeyword,
  #[display("'{'")]
  LBrace,
  #[display("key type")]
  KeyType,
  #[display("'=>'")]
  FatArrow,
  #[display("value type")]
  ValueType,
  #[display("'}'")]
  RBrace,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum AngleTypeSyntax {
  #[display("'<' ")]
  LAngle,
  #[display("type")]
  Type,
  #[display("'>' ")]
  RAngle,
}

impl_graphqlx_node! {
  for<Name> NamedType<Name, GraphQLxLanguage> {
    type Component = NamedTypeSyntax;
    type COMPONENTS = U1;
  } => NamedType(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let name = child::<Name>(&syntax);
    let bang = token(&syntax, &SyntaxKind::Bang)
      .map(|t| Bang::with_content(t.text_range(), t));

    match name {
      Some(name) => Ok(NamedType::new(syntax, name, bang)),
      None => {
        let missing = IncompleteSyntax::new(NamedTypeSyntax::Name);
        Err(missing.into())
      }
    }
  })
  where
    Name: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Type> ListType<Type, GraphQLxLanguage> {
    type Component = ListTypeSyntax;
    type COMPONENTS = U3;
  } => ListType(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let l_bracket = token(&syntax, &SyntaxKind::LBracket)
      .map(|t| LBracket::with_content(t.text_range(), t));
    let ty = child::<Type>(&syntax);
    let r_bracket = token(&syntax, &SyntaxKind::RBracket)
      .map(|t| RBracket::with_content(t.text_range(), t));
    let bang = token(&syntax, &SyntaxKind::Bang)
      .map(|t| Bang::with_content(t.text_range(), t));

    match (l_bracket, ty, r_bracket) {
      (Some(l_bracket), Some(ty), Some(r_bracket)) => Ok(ListType::new(
        syntax,
        l_bracket,
        ty,
        r_bracket,
        bang,
      )),
      (l_bracket, ty, r_bracket) => {
        let missing = [
          l_bracket.is_none().then_some(ListTypeSyntax::LBracket),
          ty.is_none().then_some(ListTypeSyntax::Type),
          r_bracket.is_none().then_some(ListTypeSyntax::RBracket),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    Type: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Type> SetType<Type, GraphQLxLanguage> {
    type Component = SetTypeSyntax;
    type COMPONENTS = U4;
  } => SetType(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let set_kw = token(&syntax, &SyntaxKind::set_KW)
      .map(|t| Set::with_content(t.text_range(), t));
    let l_brace = token(&syntax, &SyntaxKind::LBrace)
      .map(|t| LBrace::with_content(t.text_range(), t));
    let ty = child::<Type>(&syntax);
    let r_brace = token(&syntax, &SyntaxKind::RBrace)
      .map(|t| RBrace::with_content(t.text_range(), t));
    let bang = token(&syntax, &SyntaxKind::Bang)
      .map(|t| Bang::with_content(t.text_range(), t));

    match (set_kw, l_brace, ty, r_brace) {
      (Some(set_kw), Some(l_brace), Some(ty), Some(r_brace)) => Ok(SetType::new(
        syntax,
        set_kw,
        l_brace,
        ty,
        r_brace,
        bang,
      )),
      (set_kw, l_brace, ty, r_brace) => {
        let missing = [
          set_kw.is_none().then_some(SetTypeSyntax::SetKeyword),
          l_brace.is_none().then_some(SetTypeSyntax::LBrace),
          ty.is_none().then_some(SetTypeSyntax::Type),
          r_brace.is_none().then_some(SetTypeSyntax::RBrace),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    Type: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<KeyType, ValueType> MapType<KeyType, ValueType, GraphQLxLanguage> {
    type Component = MapTypeSyntax;
    type COMPONENTS = U6;
  } => MapType(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let map_kw = token(&syntax, &SyntaxKind::map_KW)
      .map(|t| Map::with_content(t.text_range(), t));
    let l_brace = token(&syntax, &SyntaxKind::LBrace)
      .map(|t| LBrace::with_content(t.text_range(), t));
    let key = child::<KeyType>(&syntax);
    let fat_arrow = token(&syntax, &SyntaxKind::FatArrow)
      .map(|t| FatArrow::with_content(t.text_range(), t));
    let value = child::<ValueType>(&syntax);
    let r_brace = token(&syntax, &SyntaxKind::RBrace)
      .map(|t| RBrace::with_content(t.text_range(), t));
    let bang = token(&syntax, &SyntaxKind::Bang)
      .map(|t| Bang::with_content(t.text_range(), t));

    match (map_kw, l_brace, key, fat_arrow, value, r_brace) {
      (Some(map_kw), Some(l_brace), Some(key), Some(fat_arrow), Some(value), Some(r_brace)) => {
        Ok(MapType::new(
          syntax,
          map_kw,
          l_brace,
          key,
          fat_arrow,
          value,
          r_brace,
          bang,
        ))
      }
      (map_kw, l_brace, key, fat_arrow, value, r_brace) => {
        let missing = [
          map_kw.is_none().then_some(MapTypeSyntax::MapKeyword),
          l_brace.is_none().then_some(MapTypeSyntax::LBrace),
          key.is_none().then_some(MapTypeSyntax::KeyType),
          fat_arrow.is_none().then_some(MapTypeSyntax::FatArrow),
          value.is_none().then_some(MapTypeSyntax::ValueType),
          r_brace.is_none().then_some(MapTypeSyntax::RBrace),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    KeyType: Debug + CstNode<Language = GraphQLxLanguage>,
    ValueType: Debug + CstNode<Language = GraphQLxLanguage>,
}

// impl_graphqlx_node! {
//   for<Type> AngleType<Type, GraphQLxLanguage> {
//     type Component = AngleTypeSyntax;
//     type COMPONENTS = U3;
//   } => AngleType(|syntax: SyntaxNode<GraphQLxLanguage>| {
//     let langle_missing = token(&syntax, &SyntaxKind::LAngle).is_none();
//     let type_missing = child::<Type>(&syntax).is_none();
//     let rangle_missing = token(&syntax, &SyntaxKind::RAngle).is_none();

//     if langle_missing || type_missing || rangle_missing {
//       let missing = [
//         langle_missing.then_some(AngleTypeSyntax::LAngle),
//         type_missing.then_some(AngleTypeSyntax::Type),
//         rangle_missing.then_some(AngleTypeSyntax::RAngle),
//       ];
//       let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
//       Err(missing.into())
//     } else {
//       Ok(AngleType::new(syntax))
//     }
//   })
//   where
//     Type: Debug + CstNode<Language = GraphQLxLanguage>,
// }
