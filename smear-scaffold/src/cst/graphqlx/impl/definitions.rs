use core::fmt::Debug;

use logosky::cst::{
  CstNode,
  cast::{child, children, token},
  error::IncompleteSyntax,
  typenum::{U1, U2, U3, U5},
};
use rowan::SyntaxNode;

use smear_lexer::punctuator::{Colon, LBrace, LParen, RBrace, RParen};

use crate::cst::{
  ArgumentsDefinition, Described, Document, FieldsDefinition, FragmentDefinition,
  InputFieldsDefinition, InputValueDefinition, NamedOperationDefinition,
  RootOperationTypeDefinition, VariableDefinition, VariablesDefinition,
  graphqlx::{GraphQLxLanguage, SyntaxKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum DocumentSyntax {
  #[display("definitions")]
  Definitions,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum NamedOperationDefinitionSyntax {
  #[display("operation type")]
  OperationType,
  #[display("operation name")]
  Name,
  #[display("variable definitions")]
  Variables,
  #[display("directives")]
  Directives,
  #[display("selection set")]
  SelectionSet,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum InputValueDefinitionSyntax {
  #[display("name")]
  Name,
  #[display("':'")]
  Colon,
  #[display("type")]
  Type,
  #[display("default value")]
  DefaultValue,
  #[display("directives")]
  Directives,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum ArgumentsDefinitionSyntax {
  #[display("'('")]
  LParen,
  #[display("arguments")]
  Arguments,
  #[display("')'")]
  RParen,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum InputFieldsDefinitionSyntax {
  #[display("'{{'")]
  LBrace,
  #[display("fields")]
  Fields,
  #[display("'}}'")]
  RBrace,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum FieldsDefinitionSyntax {
  #[display("'{{'")]
  LBrace,
  #[display("fields")]
  Fields,
  #[display("'}}'")]
  RBrace,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum FragmentDefinitionSyntax {
  #[display("'fragment'")]
  FragmentKeyword,
  #[display("fragment name")]
  FragmentName,
  #[display("type condition")]
  TypeCondition,
  #[display("directives")]
  Directives,
  #[display("selection set")]
  SelectionSet,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum RootOperationTypeDefinitionSyntax {
  #[display("operation type")]
  OperationType,
  #[display("':'")]
  Colon,
  #[display("name")]
  Name,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum VariableDefinitionSyntax {
  #[display("variable")]
  Variable,
  #[display("':'")]
  Colon,
  #[display("type")]
  Type,
  #[display("default value")]
  DefaultValue,
  #[display("directives")]
  Directives,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum VariablesDefinitionSyntax {
  #[display("'('")]
  LParen,
  #[display("variables")]
  Variables,
  #[display("')'")]
  RParen,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum DescribedSyntax {
  #[display("description")]
  Description,
  #[display("node")]
  Node,
}

impl_graphqlx_node! {
  for<Definition> Document<Definition, GraphQLxLanguage> {
    type Component = DocumentSyntax;
    type COMPONENTS = U1;
  } => Document(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let definitions = children::<Definition>(&syntax);

    if definitions.clone().next().is_some() {
      Ok(Document::new(syntax, definitions))
    } else {
      let missing = IncompleteSyntax::new(DocumentSyntax::Definitions);
      Err(missing.into())
    }
  })
  where
    Definition: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Name, OperationType, VariablesDef, Directives, SelectionSet>
    NamedOperationDefinition<Name, OperationType, VariablesDef, Directives, SelectionSet, GraphQLxLanguage> {
      type Component = NamedOperationDefinitionSyntax;
      type COMPONENTS = U5;
    } => NamedOperationDefinition(|syntax: SyntaxNode<GraphQLxLanguage>| {
      let operation_missing = child::<OperationType>(&syntax).is_none();
      let selection_missing = child::<SelectionSet>(&syntax).is_none();

      if operation_missing || selection_missing {
        let missing = [
          operation_missing.then_some(NamedOperationDefinitionSyntax::OperationType),
          selection_missing.then_some(NamedOperationDefinitionSyntax::SelectionSet),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      } else {
        Ok(NamedOperationDefinition::new(syntax))
      }
    })
  where
    Name: Debug + CstNode<Language = GraphQLxLanguage>,
    OperationType: Debug + CstNode<Language = GraphQLxLanguage>,
    VariablesDef: Debug + CstNode<Language = GraphQLxLanguage>,
    Directives: Debug + CstNode<Language = GraphQLxLanguage>,
    SelectionSet: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Name, Type, DefaultValue, Directives>
    InputValueDefinition<Name, Type, DefaultValue, Directives, GraphQLxLanguage> {
      type Component = InputValueDefinitionSyntax;
      type COMPONENTS = U5;
    } => InputValueDefinition(|syntax: SyntaxNode<GraphQLxLanguage>| {
      let name = child::<Name>(&syntax);
      let colon = token(&syntax, &SyntaxKind::Colon)
        .map(|t| Colon::with_content(t.text_range(), t));
      let ty = child::<Type>(&syntax);
      let default_value = child::<DefaultValue>(&syntax);
      let directives = child::<Directives>(&syntax);

      match (name, colon, ty) {
        (Some(name), Some(colon), Some(ty)) => Ok(InputValueDefinition::new(
          syntax,
          name,
          colon,
          ty,
          default_value,
          directives,
        )),
        (name, colon, ty) => {
          Err(IncompleteSyntax::from_iter([
            name.is_none().then_some(InputValueDefinitionSyntax::Name),
            colon.is_none().then_some(InputValueDefinitionSyntax::Colon),
            ty.is_none().then_some(InputValueDefinitionSyntax::Type),
          ].into_iter().flatten()).unwrap().into())
        }
      }
    })
  where
    Name: Debug + CstNode<Language = GraphQLxLanguage>,
    Type: Debug + CstNode<Language = GraphQLxLanguage>,
    DefaultValue: Debug + CstNode<Language = GraphQLxLanguage>,
    Directives: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<InputValueDef> ArgumentsDefinition<InputValueDef, GraphQLxLanguage> {
    type Component = ArgumentsDefinitionSyntax;
    type COMPONENTS = U3;
  } => ArgumentsDefinition(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let l_paren = token(&syntax, &SyntaxKind::LParen)
      .map(|t| LParen::with_content(t.text_range(), t));
    let arguments = children::<InputValueDef>(&syntax);
    let has_arguments = arguments.clone().next().is_some();
    let r_paren = token(&syntax, &SyntaxKind::RParen)
      .map(|t| RParen::with_content(t.text_range(), t));

    match (l_paren, has_arguments, r_paren) {
      (Some(l_paren), true, Some(r_paren)) => Ok(ArgumentsDefinition::new(
        syntax,
        l_paren,
        arguments,
        r_paren,
      )),
      (l_paren, has_arguments, r_paren) => {
        Err(IncompleteSyntax::from_iter([
          l_paren.is_none().then_some(ArgumentsDefinitionSyntax::LParen),
          (!has_arguments).then_some(ArgumentsDefinitionSyntax::Arguments),
          r_paren.is_none().then_some(ArgumentsDefinitionSyntax::RParen),
        ].into_iter().flatten()).unwrap().into())
      }
    }
  })
  where
    InputValueDef: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<InputValueDef> InputFieldsDefinition<InputValueDef, GraphQLxLanguage> {
    type Component = InputFieldsDefinitionSyntax;
    type COMPONENTS = U3;
  } => InputFieldsDefinition(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let l_brace = token(&syntax, &SyntaxKind::LBrace)
      .map(|t| LBrace::with_content(t.text_range(), t));
    let fields = children::<InputValueDef>(&syntax);
    let has_fields = fields.clone().next().is_some();
    let r_brace = token(&syntax, &SyntaxKind::RBrace)
      .map(|t| RBrace::with_content(t.text_range(), t));

    match (l_brace, has_fields, r_brace) {
      (Some(l_brace), true, Some(r_brace)) => Ok(InputFieldsDefinition::new(
        syntax,
        l_brace,
        fields,
        r_brace,
      )),
      (l_brace, has_fields, r_brace) => {
        Err(IncompleteSyntax::from_iter([
          l_brace.is_none().then_some(InputFieldsDefinitionSyntax::LBrace),
          (!has_fields).then_some(InputFieldsDefinitionSyntax::Fields),
          r_brace.is_none().then_some(InputFieldsDefinitionSyntax::RBrace),
        ].into_iter().flatten()).unwrap().into())
      }
    }
  })
  where
    InputValueDef: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<FieldDef> FieldsDefinition<FieldDef, GraphQLxLanguage> {
    type Component = FieldsDefinitionSyntax;
    type COMPONENTS = U3;
  } => FieldsDefinition(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let l_brace = token(&syntax, &SyntaxKind::LBrace)
      .map(|t| LBrace::with_content(t.text_range(), t));
    let fields = children::<FieldDef>(&syntax);
    let has_fields = fields.clone().next().is_some();
    let r_brace = token(&syntax, &SyntaxKind::RBrace)
      .map(|t| RBrace::with_content(t.text_range(), t));

    match (l_brace, has_fields, r_brace) {
      (Some(l_brace), true, Some(r_brace)) => Ok(FieldsDefinition::new(
        syntax,
        l_brace,
        fields,
        r_brace,
      )),
      (l_brace, has_fields, r_brace) => {
        Err(IncompleteSyntax::from_iter([
          l_brace.is_none().then_some(FieldsDefinitionSyntax::LBrace),
          (!has_fields).then_some(FieldsDefinitionSyntax::Fields),
          r_brace.is_none().then_some(FieldsDefinitionSyntax::RBrace),
        ].into_iter().flatten()).unwrap().into())
      }
    }
  })
  where
    FieldDef: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<FragmentName, TypeCond, Directives, SelectionSet>
    FragmentDefinition<FragmentName, TypeCond, Directives, SelectionSet, GraphQLxLanguage> {
      type Component = FragmentDefinitionSyntax;
      type COMPONENTS = U5;
    } => FragmentDefinition(|syntax: SyntaxNode<GraphQLxLanguage>| {
      let fragment_kw_missing = token(&syntax, &SyntaxKind::fragment_KW).is_none();
      let name_missing = child::<FragmentName>(&syntax).is_none();
      let type_cond_missing = child::<TypeCond>(&syntax).is_none();
      let selection_missing = child::<SelectionSet>(&syntax).is_none();

      if fragment_kw_missing || name_missing || type_cond_missing || selection_missing {
        let missing = [
          fragment_kw_missing.then_some(FragmentDefinitionSyntax::FragmentKeyword),
          name_missing.then_some(FragmentDefinitionSyntax::FragmentName),
          type_cond_missing.then_some(FragmentDefinitionSyntax::TypeCondition),
          selection_missing.then_some(FragmentDefinitionSyntax::SelectionSet),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      } else {
        Ok(FragmentDefinition::new(syntax))
      }
    })
  where
    FragmentName: Debug + CstNode<Language = GraphQLxLanguage>,
    TypeCond: Debug + CstNode<Language = GraphQLxLanguage>,
    Directives: Debug + CstNode<Language = GraphQLxLanguage>,
    SelectionSet: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<OperationType, Name>
    RootOperationTypeDefinition<OperationType, Name, GraphQLxLanguage> {
      type Component = RootOperationTypeDefinitionSyntax;
      type COMPONENTS = U3;
    } => RootOperationTypeDefinition(|syntax: SyntaxNode<GraphQLxLanguage>| {
      let operation_missing = child::<OperationType>(&syntax).is_none();
      let colon_missing = token(&syntax, &SyntaxKind::Colon).is_none();
      let name_missing = child::<Name>(&syntax).is_none();

      if operation_missing || colon_missing || name_missing {
        let missing = [
          operation_missing.then_some(RootOperationTypeDefinitionSyntax::OperationType),
          colon_missing.then_some(RootOperationTypeDefinitionSyntax::Colon),
          name_missing.then_some(RootOperationTypeDefinitionSyntax::Name),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      } else {
        Ok(RootOperationTypeDefinition::new(syntax))
      }
    })
  where
    OperationType: Debug + CstNode<Language = GraphQLxLanguage>,
    Name: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Variable, Type, DefaultValue, Directives>
    VariableDefinition<Variable, Type, DefaultValue, Directives, GraphQLxLanguage> {
      type Component = VariableDefinitionSyntax;
      type COMPONENTS = U5;
    } => VariableDefinition(|syntax: SyntaxNode<GraphQLxLanguage>| {
      let variable_missing = child::<Variable>(&syntax).is_none();
      let colon_missing = token(&syntax, &SyntaxKind::Colon).is_none();
      let type_missing = child::<Type>(&syntax).is_none();

      if variable_missing || colon_missing || type_missing {
        let missing = [
          variable_missing.then_some(VariableDefinitionSyntax::Variable),
          colon_missing.then_some(VariableDefinitionSyntax::Colon),
          type_missing.then_some(VariableDefinitionSyntax::Type),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      } else {
        Ok(VariableDefinition::new(syntax))
      }
    })
  where
    Variable: Debug + CstNode<Language = GraphQLxLanguage>,
    Type: Debug + CstNode<Language = GraphQLxLanguage>,
    DefaultValue: Debug + CstNode<Language = GraphQLxLanguage>,
    Directives: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<VariableDef> VariablesDefinition<VariableDef, GraphQLxLanguage> {
    type Component = VariablesDefinitionSyntax;
    type COMPONENTS = U3;
  } => VariablesDefinition(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let l_paren_missing = token(&syntax, &SyntaxKind::LParen).is_none();
    let has_variables = children::<VariableDef>(&syntax).next().is_some();
    let r_paren_missing = token(&syntax, &SyntaxKind::RParen).is_none();

    if l_paren_missing || !has_variables || r_paren_missing {
      let missing = [
        l_paren_missing.then_some(VariablesDefinitionSyntax::LParen),
        (!has_variables).then_some(VariablesDefinitionSyntax::Variables),
        r_paren_missing.then_some(VariablesDefinitionSyntax::RParen),
      ];
      let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
      Err(missing.into())
    } else {
      Ok(VariablesDefinition::new(syntax))
    }
  })
  where
    VariableDef: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<T, Description> Described<T, Description, GraphQLxLanguage> {
    type Component = DescribedSyntax;
    type COMPONENTS = U2;
  } => Description(|syntax: SyntaxNode<GraphQLxLanguage>| {
    if child::<T>(&syntax).is_some() {
      Ok(Described::new(syntax))
    } else {
      let missing = IncompleteSyntax::new(DescribedSyntax::Node);
      Err(missing.into())
    }
  })
  where
    T: Debug + CstNode<Language = GraphQLxLanguage>,
    Description: Debug + CstNode<Language = GraphQLxLanguage>,
}

mod generic;
mod ty;
