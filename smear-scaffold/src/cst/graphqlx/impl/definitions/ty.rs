use core::fmt::Debug;

use logosky::cst::{
  CstNode,
  cast::{child, token},
  error::IncompleteSyntax,
  typenum::{U1, U3, U4},
};
use rowan::SyntaxNode;

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
  #[display("type")]
  Type,
  #[display("'>' ")]
  Terminator,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum MapTypeSyntax {
  #[display("'map'")]
  MapKeyword,
  #[display("key type")]
  KeyType,
  #[display("'=>'")]
  FatArrow,
  #[display("value type")]
  ValueType,
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
    if child::<Name>(&syntax).is_some() {
      Ok(NamedType::new(syntax))
    } else {
      let missing = IncompleteSyntax::new(NamedTypeSyntax::Name);
      Err(missing.into())
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
    let l_bracket_missing = token(&syntax, &SyntaxKind::LBracket).is_none();
    let type_missing = child::<Type>(&syntax).is_none();
    let r_bracket_missing = token(&syntax, &SyntaxKind::RBracket).is_none();

    if l_bracket_missing || type_missing || r_bracket_missing {
      let missing = [
        l_bracket_missing.then_some(ListTypeSyntax::LBracket),
        type_missing.then_some(ListTypeSyntax::Type),
        r_bracket_missing.then_some(ListTypeSyntax::RBracket),
      ];
      let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
      Err(missing.into())
    } else {
      Ok(ListType::new(syntax))
    }
  })
  where
    Type: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Type> SetType<Type, GraphQLxLanguage> {
    type Component = SetTypeSyntax;
    type COMPONENTS = U3;
  } => SetType(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let set_kw_missing = token(&syntax, &SyntaxKind::set_KW).is_none();
    let type_missing = child::<Type>(&syntax).is_none();
    let terminator_missing = token(&syntax, &SyntaxKind::RAngle).is_none();

    if set_kw_missing || type_missing || terminator_missing {
      let missing = [
        set_kw_missing.then_some(SetTypeSyntax::SetKeyword),
        type_missing.then_some(SetTypeSyntax::Type),
        terminator_missing.then_some(SetTypeSyntax::Terminator),
      ];
      let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
      Err(missing.into())
    } else {
      Ok(SetType::new(syntax))
    }
  })
  where
    Type: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<KeyType, ValueType> MapType<KeyType, ValueType, GraphQLxLanguage> {
    type Component = MapTypeSyntax;
    type COMPONENTS = U4;
  } => MapType(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let map_kw_missing = token(&syntax, &SyntaxKind::map_KW).is_none();
    let key_missing = child::<KeyType>(&syntax).is_none();
    let fat_arrow_missing = token(&syntax, &SyntaxKind::FatArrow).is_none();
    let value_missing = child::<ValueType>(&syntax).is_none();

    if map_kw_missing || key_missing || fat_arrow_missing || value_missing {
      let missing = [
        map_kw_missing.then_some(MapTypeSyntax::MapKeyword),
        key_missing.then_some(MapTypeSyntax::KeyType),
        fat_arrow_missing.then_some(MapTypeSyntax::FatArrow),
        value_missing.then_some(MapTypeSyntax::ValueType),
      ];
      let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
      Err(missing.into())
    } else {
      Ok(MapType::new(syntax))
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
