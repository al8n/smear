use core::fmt::Debug;

use logosky::cst::{
  CstNode,
  cast::{child, children, token},
  error::IncompleteSyntax,
  typenum::{U1, U2, U3, U4, U5, U7},
};
use rowan::SyntaxNode;

use smear_lexer::{keywords, punctuator::{Ampersand, At, Colon, Equal, LBrace, LParen, Pipe, RBrace, RParen}};

use smear_lexer::keywords::{
  Enum as EnumKeyword,
  Fragment as FragmentKeyword,
  Interface as InterfaceKeyword,
  Input as InputKeyword,
  Scalar as ScalarKeyword,
  Schema as SchemaKeyword,
  Type as TypeKeyword,
  Union as UnionKeyword,
};

use crate::cst::{
  ArgumentsDefinition, Described, Document, EnumTypeDefinition, EnumValueDefinition,
  EnumValuesDefinition, FieldsDefinition, FragmentDefinition, InputFieldsDefinition, DirectiveDefinition,
  InputObjectTypeDefinition, InputValueDefinition, InterfaceTypeDefinition, NamedOperationDefinition,
  ObjectTypeDefinition, RootOperationTypeDefinition, RootOperationTypesDefinition, ScalarTypeDefinition,
  SchemaDefinition, UnionMember, UnionMembers, UnionTypeDefinition, VariableDefinition, VariablesDefinition,
  ImplementInterfaceMember, ImplementsInterfaces,
  graphql::{GraphQLLanguage, SyntaxKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum DocumentSyntax {
  #[display("definitions")]
  Definitions,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum SchemaDefinitionSyntax {
  #[display("'schema'")]
  SchemaKeyword,
  #[display("directives")]
  Directives,
  #[display("root operations")]
  RootOperations,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum TypeSystemDocumentSyntax {
  #[display("definitions")]
  Definitions,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum ExecutableDocumentSyntax {
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
pub enum DirectiveDefinitionSyntax {
  #[display("'directive'")]
  DirectiveKeyword,
  #[display("'@'")]
  AtToken,
  #[display("name")]
  Name,
  #[display("arguments definition")]
  ArgumentsDefinition,
  #[display("'repeatable'")]
  RepeatableKeyword,
  #[display("'on'")]
  OnKeyword,
  #[display("directive locations")]
  DirectiveLocations,
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
pub enum EnumValueDefinitionSyntax {
  #[display("value")]
  Value,
  #[display("directives")]
  Directives,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum EnumValuesDefinitionSyntax {
  #[display("'{{'")]
  LBrace,
  #[display("values")]
  Values,
  #[display("'}}'")]
  RBrace,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum EnumTypeDefinitionSyntax {
  #[display("'enum'")]
  EnumKeyword,
  #[display("name")]
  Name,
  #[display("directives")]
  Directives,
  #[display("values")]
  ValuesDefinition,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum ObjectTypeDefinitionSyntax {
  #[display("'type'")]
  TypeKeyword,
  #[display("name")]
  Name,
  #[display("implements")]
  Implements,
  #[display("directives")]
  Directives,
  #[display("fields")]
  FieldsDefinition,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum ImplementInterfaceMemberSyntax {
  #[display("interface")]
  Interface,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum ImplementsInterfacesSyntax {
  #[display("interfaces")]
  Interfaces,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum InterfaceTypeDefinitionSyntax {
  #[display("'interface'")]
  InterfaceKeyword,
  #[display("name")]
  Name,
  #[display("implements")]
  Implements,
  #[display("directives")]
  Directives,
  #[display("fields")]
  FieldsDefinition,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum InputObjectTypeDefinitionSyntax {
  #[display("'input'")]
  InputKeyword,
  #[display("name")]
  Name,
  #[display("directives")]
  Directives,
  #[display("fields")]
  FieldsDefinition,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum UnionMemberSyntax {
  #[display("type")]
  Type,
  #[display("'|'")]
  Pipe,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum UnionMembersSyntax {
  #[display("members")]
  Members,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum UnionTypeDefinitionSyntax {
  #[display("'union'")]
  UnionKeyword,
  #[display("name")]
  Name,
  #[display("directives")]
  Directives,
  #[display("'='")]
  Equal,
  #[display("members")]
  Members,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum ScalarTypeDefinitionSyntax {
  #[display("'scalar'")]
  ScalarKeyword,
  #[display("name")]
  Name,
  #[display("directives")]
  Directives,
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
  #[display("type name")]
  Name,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum RootOperationTypesDefinitionSyntax {
  #[display("'{{'")]
  LBrace,
  #[display("root operation type definitions")]
  Definitions,
  #[display("'}}'")]
  RBrace,
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

impl_graphql_node! {
  for<Definition> Document<Definition, GraphQLLanguage> {
    type Component = DocumentSyntax;
    type COMPONENTS = U1;
  } => Document(|syntax: SyntaxNode<GraphQLLanguage>| {
    let definitions = children(&syntax);
    Ok(Document::new(syntax, definitions))
  })
  where
    Definition: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<Directives, RootOperations>
    SchemaDefinition<Directives, RootOperations, GraphQLLanguage> {
      type Component = SchemaDefinitionSyntax;
      type COMPONENTS = U3;
    } => SchemaDefinition(|syntax: SyntaxNode<GraphQLLanguage>| {
    let schema_kw = token(&syntax, &SyntaxKind::schema_KW)
      .map(|t| SchemaKeyword::with_content(t.text_range(), t));
    let directives = child(&syntax);
    let root_operations = child(&syntax);

    match (schema_kw, root_operations) {
      (Some(schema_kw), Some(root_operations)) => Ok(SchemaDefinition::new(
        syntax,
        schema_kw,
        directives,
        root_operations,
      )),
      (schema_kw, root_operations) => {
        let missing = [
          schema_kw.is_none().then_some(SchemaDefinitionSyntax::SchemaKeyword),
          root_operations
            .is_none()
            .then_some(SchemaDefinitionSyntax::RootOperations),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    Directives: CstNode<GraphQLLanguage>,
    RootOperations: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<Name, OperationType, VariablesDef, Directives, SelectionSet>
    NamedOperationDefinition<Name, OperationType, VariablesDef, Directives, SelectionSet, GraphQLLanguage> {
      type Component = NamedOperationDefinitionSyntax;
      type COMPONENTS = U5;
    } => NamedOperationDefinition(|syntax: SyntaxNode<GraphQLLanguage>| {
    let operation_type = child(&syntax);
    let name = child(&syntax);
    let variables = child(&syntax);
    let directives = child(&syntax);
    let selection_set = child(&syntax);

    match (operation_type, selection_set) {
      (Some(operation_type), Some(selection_set)) => Ok(NamedOperationDefinition::new(
        syntax,
        operation_type,
        name,
        variables,
        directives,
        selection_set,
      )),
      (operation_type, selection_set) => {
        let missing = [
          operation_type
            .is_none()
            .then_some(NamedOperationDefinitionSyntax::OperationType),
          selection_set
            .is_none()
            .then_some(NamedOperationDefinitionSyntax::SelectionSet),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    Name: CstNode<GraphQLLanguage>,
    OperationType: CstNode<GraphQLLanguage>,
    VariablesDef: CstNode<GraphQLLanguage>,
    Directives: CstNode<GraphQLLanguage>,
    SelectionSet: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<Name, Type, DefaultValue, Directives>
    InputValueDefinition<Name, Type, DefaultValue, Directives, GraphQLLanguage> {
      type Component = InputValueDefinitionSyntax;
      type COMPONENTS = U5;
    } => InputValueDefinition(|syntax: SyntaxNode<GraphQLLanguage>| {
      let name = child(&syntax);
      let colon = token(&syntax, &SyntaxKind::Colon)
        .map(|t| Colon::with_content(t.text_range(), t));
      let ty = child(&syntax);
      let default_value = child(&syntax);
      let directives = child(&syntax);

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
    Name: CstNode<GraphQLLanguage>,
    Type: CstNode<GraphQLLanguage>,
    DefaultValue: CstNode<GraphQLLanguage>,
    Directives: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<InputValueDef> ArgumentsDefinition<InputValueDef, GraphQLLanguage> {
    type Component = ArgumentsDefinitionSyntax;
    type COMPONENTS = U3;
  } => ArgumentsDefinition(|syntax: SyntaxNode<GraphQLLanguage>| {
    let l_paren = token(&syntax, &SyntaxKind::LParen)
      .map(|t| LParen::with_content(t.text_range(), t));
    let arguments = children(&syntax);
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
    InputValueDef: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<Name, Args, Locations> DirectiveDefinition<Name, Args, Locations, GraphQLLanguage> {
    type Component = DirectiveDefinitionSyntax;
    type COMPONENTS = U7;
  } => DirectiveDefinition(|syntax: SyntaxNode<GraphQLLanguage>| {
    let directive_keyword = token(&syntax, &SyntaxKind::directive_KW)
      .map(|t| keywords::Directive::with_content(t.text_range(), t));
    let at_token = token(&syntax, &SyntaxKind::At)
      .map(|t| At::with_content(t.text_range(), t));
    let name = child(&syntax);
    let arguments_definition = child(&syntax);
    let repeateable = token(&syntax, &SyntaxKind::repeatable_KW)
      .map(|t| keywords::Repeatable::with_content(t.text_range(), t));
    let on_keyword = token(&syntax, &SyntaxKind::on_KW)
      .map(|t| keywords::On::with_content(t.text_range(), t));
    let directive_locations = child(&syntax);

    match (directive_keyword, at_token, name, directive_locations) {
      (Some(directive_keyword), Some(at_token), Some(name), Some(directive_locations)) => Ok(DirectiveDefinition::new(
        syntax,
        directive_keyword,
        at_token,
        name,
        arguments_definition,
        repeateable,
        on_keyword,
        directive_locations,
      )),
      (directive_keyword, at_token, name, directive_locations) => {
        Err(IncompleteSyntax::from_iter([
          directive_keyword.is_none().then_some(DirectiveDefinitionSyntax::DirectiveKeyword),
          at_token.is_none().then_some(DirectiveDefinitionSyntax::AtToken),
          name.is_none().then_some(DirectiveDefinitionSyntax::Name),
          directive_locations.is_none().then_some(DirectiveDefinitionSyntax::DirectiveLocations),
        ].into_iter().flatten()).unwrap().into())
      }
    }
  })
  where
    Name: CstNode<GraphQLLanguage>,
    Args: CstNode<GraphQLLanguage>,
    Locations: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<InputValueDef> InputFieldsDefinition<InputValueDef, GraphQLLanguage> {
    type Component = InputFieldsDefinitionSyntax;
    type COMPONENTS = U3;
  } => InputFieldsDefinition(|syntax: SyntaxNode<GraphQLLanguage>| {
    let l_brace = token(&syntax, &SyntaxKind::LBrace)
      .map(|t| LBrace::with_content(t.text_range(), t));
    let fields = children(&syntax);
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
    InputValueDef: Debug + CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<FieldDef> FieldsDefinition<FieldDef, GraphQLLanguage> {
    type Component = FieldsDefinitionSyntax;
    type COMPONENTS = U3;
  } => FieldsDefinition(|syntax: SyntaxNode<GraphQLLanguage>| {
    let l_brace = token(&syntax, &SyntaxKind::LBrace)
      .map(|t| LBrace::with_content(t.text_range(), t));
    let fields = children(&syntax);
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
    FieldDef: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<Value, Directives>
    EnumValueDefinition<Value, Directives, GraphQLLanguage> {
      type Component = EnumValueDefinitionSyntax;
      type COMPONENTS = U2;
    } => EnumValueDefinition(|syntax: SyntaxNode<GraphQLLanguage>| {
    let value = child(&syntax);
    let directives = child(&syntax);

    match value {
      Some(value) => Ok(EnumValueDefinition::new(syntax, value, directives)),
      None => {
        let missing = IncompleteSyntax::new(EnumValueDefinitionSyntax::Value);
        Err(missing.into())
      }
    }
  })
  where
    Value: CstNode<GraphQLLanguage>,
    Directives: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<ValueDef> EnumValuesDefinition<ValueDef, GraphQLLanguage> {
    type Component = EnumValuesDefinitionSyntax;
    type COMPONENTS = U3;
  } => EnumValuesDefinition(|syntax: SyntaxNode<GraphQLLanguage>| {
    let l_brace = token(&syntax, &SyntaxKind::LBrace)
      .map(|t| LBrace::with_content(t.text_range(), t));
    let values = children(&syntax);
    let has_values = values.clone().next().is_some();
    let r_brace = token(&syntax, &SyntaxKind::RBrace)
      .map(|t| RBrace::with_content(t.text_range(), t));

    match (l_brace, has_values, r_brace) {
      (Some(l_brace), true, Some(r_brace)) => Ok(EnumValuesDefinition::new(
        syntax,
        l_brace,
        values,
        r_brace,
      )),
      (l_brace, has_values, r_brace) => {
        let missing = [
          l_brace.is_none().then_some(EnumValuesDefinitionSyntax::LBrace),
          (!has_values).then_some(EnumValuesDefinitionSyntax::Values),
          r_brace.is_none().then_some(EnumValuesDefinitionSyntax::RBrace),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    ValueDef: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<Name, Directives, ValuesDefinition>
    EnumTypeDefinition<Name, Directives, ValuesDefinition, GraphQLLanguage> {
      type Component = EnumTypeDefinitionSyntax;
      type COMPONENTS = U4;
    } => EnumTypeDefinition(|syntax: SyntaxNode<GraphQLLanguage>| {
    let enum_kw = token(&syntax, &SyntaxKind::enum_KW)
      .map(|t| EnumKeyword::with_content(t.text_range(), t));
    let name = child(&syntax);
    let directives = child(&syntax);
    let values_definition = child(&syntax);

    match (enum_kw, name) {
      (Some(enum_kw), Some(name)) => Ok(EnumTypeDefinition::new(
        syntax,
        enum_kw,
        name,
        directives,
        values_definition,
      )),
      (enum_kw, name) => {
        let missing = [
          enum_kw.is_none().then_some(EnumTypeDefinitionSyntax::EnumKeyword),
          name.is_none().then_some(EnumTypeDefinitionSyntax::Name),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    Name: CstNode<GraphQLLanguage>,
    Directives: CstNode<GraphQLLanguage>,
    ValuesDefinition: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<Name, ImplementsInterfaces, Directives, FieldsDefinition>
    ObjectTypeDefinition<Name, ImplementsInterfaces, Directives, FieldsDefinition, GraphQLLanguage> {
      type Component = ObjectTypeDefinitionSyntax;
      type COMPONENTS = U5;
    } => ObjectTypeDefinition(|syntax: SyntaxNode<GraphQLLanguage>| {
    let type_kw = token(&syntax, &SyntaxKind::type_KW)
      .map(|t| TypeKeyword::with_content(t.text_range(), t));
    let name = child(&syntax);
    let implements = child(&syntax);
    let directives = child(&syntax);
    let fields_definition = child(&syntax);

    match (type_kw, name) {
      (Some(type_kw), Some(name)) => Ok(ObjectTypeDefinition::new(
        syntax,
        type_kw,
        name,
        implements,
        directives,
        fields_definition,
      )),
      (type_kw, name) => {
        let missing = [
          type_kw.is_none().then_some(ObjectTypeDefinitionSyntax::TypeKeyword),
          name.is_none().then_some(ObjectTypeDefinitionSyntax::Name),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    Name: CstNode<GraphQLLanguage>,
    ImplementsInterfaces: CstNode<GraphQLLanguage>,
    Directives: CstNode<GraphQLLanguage>,
    FieldsDefinition: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<Interface>
    ImplementInterfaceMember<Interface, GraphQLLanguage> {
      type Component = ImplementInterfaceMemberSyntax;
      type COMPONENTS = U1;
    } => ImplementInterfaceMember(|syntax: SyntaxNode<GraphQLLanguage>| {
    let interface = child(&syntax);
    let ampersand = token(&syntax, &SyntaxKind::Ampersand)
      .map(|t| Ampersand::with_content(t.text_range(), t));

    match interface {
      Some(interface) => Ok(ImplementInterfaceMember::new(syntax, ampersand, interface)),
      None => {
        Err(IncompleteSyntax::new(ImplementInterfaceMemberSyntax::Interface).into())
      }
    }
  })
  where
    Interface: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<InterfaceMember>
    ImplementsInterfaces<InterfaceMember, GraphQLLanguage> {
      type Component = ImplementsInterfacesSyntax;
      type COMPONENTS = U1;
    } => ImplementsInterfaces(|syntax: SyntaxNode<GraphQLLanguage>| {
    let interfaces = children(&syntax);
    let has_interfaces = interfaces.clone().next().is_some();

    if has_interfaces {
      Ok(ImplementsInterfaces::new(syntax, interfaces))
    } else {
      Err(IncompleteSyntax::new(ImplementsInterfacesSyntax::Interfaces).into())
    }
  })
  where
    InterfaceMember: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<Name, ImplementsInterfaces, Directives, FieldsDefinition>
    InterfaceTypeDefinition<Name, ImplementsInterfaces, Directives, FieldsDefinition, GraphQLLanguage> {
      type Component = InterfaceTypeDefinitionSyntax;
      type COMPONENTS = U5;
    } => InterfaceTypeDefinition(|syntax: SyntaxNode<GraphQLLanguage>| {
    let interface_kw = token(&syntax, &SyntaxKind::interface_KW)
      .map(|t| InterfaceKeyword::with_content(t.text_range(), t));
    let name = child(&syntax);
    let implements = child(&syntax);
    let directives = child(&syntax);
    let fields_definition = child(&syntax);

    match (interface_kw, name) {
      (Some(interface_kw), Some(name)) => Ok(InterfaceTypeDefinition::new(
        syntax,
        interface_kw,
        name,
        implements,
        directives,
        fields_definition,
      )),
      (interface_kw, name) => {
        let missing = [
          interface_kw
            .is_none()
            .then_some(InterfaceTypeDefinitionSyntax::InterfaceKeyword),
          name.is_none().then_some(InterfaceTypeDefinitionSyntax::Name),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    Name: CstNode<GraphQLLanguage>,
    ImplementsInterfaces: CstNode<GraphQLLanguage>,
    Directives: CstNode<GraphQLLanguage>,
    FieldsDefinition: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<Name, Directives, FieldsDefinition>
    InputObjectTypeDefinition<Name, Directives, FieldsDefinition, GraphQLLanguage> {
      type Component = InputObjectTypeDefinitionSyntax;
      type COMPONENTS = U4;
    } => InputObjectTypeDefinition(|syntax: SyntaxNode<GraphQLLanguage>| {
    let input_kw = token(&syntax, &SyntaxKind::input_KW)
      .map(|t| InputKeyword::with_content(t.text_range(), t));
    let name = child(&syntax);
    let directives = child(&syntax);
    let fields_definition = child(&syntax);

    match (input_kw, name) {
      (Some(input_kw), Some(name)) => Ok(InputObjectTypeDefinition::new(
        syntax,
        input_kw,
        name,
        directives,
        fields_definition,
      )),
      (input_kw, name) => {
        let missing = [
          input_kw
            .is_none()
            .then_some(InputObjectTypeDefinitionSyntax::InputKeyword),
          name.is_none().then_some(InputObjectTypeDefinitionSyntax::Name),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    Name: CstNode<GraphQLLanguage>,
    Directives: CstNode<GraphQLLanguage>,
    FieldsDefinition: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<Name> UnionMember<Name, GraphQLLanguage> {
    type Component = UnionMemberSyntax;
    type COMPONENTS = U2;
  } => UnionMember(|syntax: SyntaxNode<GraphQLLanguage>| {
    let ty = child(&syntax);
    let pipe = token(&syntax, &SyntaxKind::Pipe)
      .map(|t| Pipe::with_content(t.text_range(), t));

    match ty {
      Some(ty) => Ok(UnionMember::new(syntax, pipe, ty)),
      None => {
        Err(IncompleteSyntax::new(UnionMemberSyntax::Type).into())
      }
    }
  })
  where
    Name: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<Name> UnionMembers<Name, GraphQLLanguage> {
    type Component = UnionMembersSyntax;
    type COMPONENTS = U1;
  } => UnionMembers(|syntax: SyntaxNode<GraphQLLanguage>| {
    let members = children(&syntax);
    let has_members = members.clone().next().is_some();

    if has_members {
      Ok(UnionMembers::new(syntax, members))
    } else {
      Err(IncompleteSyntax::new(UnionMembersSyntax::Members).into())
    }
  })
  where
    Name: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<Name, Directives, Members>
    UnionTypeDefinition<Name, Directives, Members, GraphQLLanguage> {
      type Component = UnionTypeDefinitionSyntax;
      type COMPONENTS = U5;
    } => UnionTypeDefinition(|syntax: SyntaxNode<GraphQLLanguage>| {
    let union_kw = token(&syntax, &SyntaxKind::union_KW)
      .map(|t| UnionKeyword::with_content(t.text_range(), t));
    let name = child(&syntax);
    let directives = child(&syntax);
    let equal = token(&syntax, &SyntaxKind::Equal)
      .map(|t| Equal::with_content(t.text_range(), t));
    let members = child(&syntax);

    match (union_kw, name, equal, members) {
      (Some(union_kw), Some(name), Some(equal), Some(members)) => Ok(UnionTypeDefinition::new(
        syntax,
        union_kw,
        name,
        directives,
        equal,
        members,
      )),
      (union_kw, name, equal, members) => {
        let missing = [
          union_kw.is_none().then_some(UnionTypeDefinitionSyntax::UnionKeyword),
          name.is_none().then_some(UnionTypeDefinitionSyntax::Name),
          equal.is_none().then_some(UnionTypeDefinitionSyntax::Equal),
          members.is_none().then_some(UnionTypeDefinitionSyntax::Members),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    Name: CstNode<GraphQLLanguage>,
    Directives: CstNode<GraphQLLanguage>,
    Members: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<Name, Directives>
    ScalarTypeDefinition<Name, Directives, GraphQLLanguage> {
      type Component = ScalarTypeDefinitionSyntax;
      type COMPONENTS = U3;
    } => ScalarTypeDefinition(|syntax: SyntaxNode<GraphQLLanguage>| {
    let scalar_kw = token(&syntax, &SyntaxKind::scalar_KW)
      .map(|t| ScalarKeyword::with_content(t.text_range(), t));
    let name = child(&syntax);
    let directives = child(&syntax);

    match (scalar_kw, name) {
      (Some(scalar_kw), Some(name)) => Ok(ScalarTypeDefinition::new(
        syntax,
        scalar_kw,
        name,
        directives,
      )),
      (scalar_kw, name) => {
        let missing = [
          scalar_kw.is_none().then_some(ScalarTypeDefinitionSyntax::ScalarKeyword),
          name.is_none().then_some(ScalarTypeDefinitionSyntax::Name),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    Name: CstNode<GraphQLLanguage>,
    Directives: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<FragmentName, TypeCond, Directives, SelectionSet>
    FragmentDefinition<FragmentName, TypeCond, Directives, SelectionSet, GraphQLLanguage> {
      type Component = FragmentDefinitionSyntax;
      type COMPONENTS = U5;
    } => FragmentDefinition(|syntax: SyntaxNode<GraphQLLanguage>| {
    let fragment_kw = token(&syntax, &SyntaxKind::fragment_KW)
      .map(|t| FragmentKeyword::with_content(t.text_range(), t));
    let name = child(&syntax);
    let type_cond = child(&syntax);
    let directives = child(&syntax);
    let selection_set = child(&syntax);

    match (fragment_kw, name, type_cond, selection_set) {
      (Some(fragment_kw), Some(name), Some(type_cond), Some(selection_set)) => Ok(
        FragmentDefinition::new(syntax, fragment_kw, name, type_cond, directives, selection_set),
      ),
      (fragment_kw, name, type_cond, selection_set) => {
        let missing = [
          fragment_kw.is_none().then_some(FragmentDefinitionSyntax::FragmentKeyword),
          name.is_none().then_some(FragmentDefinitionSyntax::FragmentName),
          type_cond.is_none().then_some(FragmentDefinitionSyntax::TypeCondition),
          selection_set
            .is_none()
            .then_some(FragmentDefinitionSyntax::SelectionSet),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    FragmentName: CstNode<GraphQLLanguage>,
    TypeCond: CstNode<GraphQLLanguage>,
    Directives: CstNode<GraphQLLanguage>,
    SelectionSet: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<OperationType, Name>
    RootOperationTypeDefinition<OperationType, Name, GraphQLLanguage> {
      type Component = RootOperationTypeDefinitionSyntax;
      type COMPONENTS = U3;
    } => RootOperationTypeDefinition(|syntax: SyntaxNode<GraphQLLanguage>| {
    let operation_type = child(&syntax);
    let colon = token(&syntax, &SyntaxKind::Colon)
      .map(|t| Colon::with_content(t.text_range(), t));
    let name = child(&syntax);

    match (operation_type, colon, name) {
      (Some(operation_type), Some(colon), Some(name)) => Ok(RootOperationTypeDefinition::new(
        syntax,
        operation_type,
        colon,
        name,
      )),
      (operation_type, colon, name) => {
        let missing = [
          operation_type
            .is_none()
            .then_some(RootOperationTypeDefinitionSyntax::OperationType),
          colon.is_none().then_some(RootOperationTypeDefinitionSyntax::Colon),
          name.is_none().then_some(RootOperationTypeDefinitionSyntax::Name),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    OperationType: CstNode<GraphQLLanguage>,
    Name: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<RootOpDef> RootOperationTypesDefinition<RootOpDef, GraphQLLanguage> {
    type Component = RootOperationTypesDefinitionSyntax;
    type COMPONENTS = U1;
  } => RootOperationTypesDefinition(|syntax: SyntaxNode<GraphQLLanguage>| {
    let l_brace = token(&syntax, &SyntaxKind::LBrace)
      .map(|t| LBrace::with_content(t.text_range(), t));
    let r_brace = token(&syntax, &SyntaxKind::RBrace)
      .map(|t| RBrace::with_content(t.text_range(), t));
    let definitions = children(&syntax);
    let has_definitions = definitions.clone().next().is_some();

    match (l_brace, has_definitions, r_brace) {
      (Some(l_brace), true, Some(r_brace)) => Ok(RootOperationTypesDefinition::new(
        syntax,
        l_brace,
        definitions,
        r_brace,
      )),
      (l_brace, has_definitions, r_brace) => {
        let missing = [
          l_brace.is_none().then_some(RootOperationTypesDefinitionSyntax::LBrace),
          (!has_definitions).then_some(RootOperationTypesDefinitionSyntax::Definitions),
          r_brace.is_none().then_some(RootOperationTypesDefinitionSyntax::RBrace),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    RootOpDef: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<Variable, Type, DefaultValue, Directives>
    VariableDefinition<Variable, Type, DefaultValue, Directives, GraphQLLanguage> {
      type Component = VariableDefinitionSyntax;
      type COMPONENTS = U5;
    } => VariableDefinition(|syntax: SyntaxNode<GraphQLLanguage>| {
      let variable = child(&syntax);
      let colon = token(&syntax, &SyntaxKind::Colon).map(|t| Colon::with_content(t.text_range(), t));
      let ty = child(&syntax);
      let directives = child(&syntax);
      let default_value = child(&syntax);
      match (variable, colon, ty) {
        (Some(variable), Some(colon), Some(ty)) => Ok(VariableDefinition::new(
          syntax,
          variable,
          colon,
          ty,
          default_value,
          directives,
        )),
        (variable, colon, ty) => {
          Err(IncompleteSyntax::from_iter([
            variable.is_none().then_some(VariableDefinitionSyntax::Variable),
            colon.is_none().then_some(VariableDefinitionSyntax::Colon),
            ty.is_none().then_some(VariableDefinitionSyntax::Type),
          ].into_iter().flatten()).unwrap().into())
        }
      }
    })
  where
    Variable: Debug + CstNode<GraphQLLanguage>,
    Type: Debug + CstNode<GraphQLLanguage>,
    DefaultValue: Debug + CstNode<GraphQLLanguage>,
    Directives: Debug + CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<VariableDef> VariablesDefinition<VariableDef, GraphQLLanguage> {
    type Component = VariablesDefinitionSyntax;
    type COMPONENTS = U3;
  } => VariablesDefinition(|syntax: SyntaxNode<GraphQLLanguage>| {
    let l_paren = token(&syntax, &SyntaxKind::LParen)
      .map(|t| LParen::with_content(t.text_range(), t));
    let variables = children(&syntax);
    let has_variables = variables.clone().next().is_some();
    let r_paren = token(&syntax, &SyntaxKind::RParen)
      .map(|t| RParen::with_content(t.text_range(), t));

    match (l_paren, has_variables, r_paren) {
      (Some(l_paren), true, Some(r_paren)) => Ok(VariablesDefinition::new(
        syntax,
        l_paren,
        variables,
        r_paren,
      )),
      (l_paren, has_variables, r_paren) => {
        let missing = [
          l_paren.is_none().then_some(VariablesDefinitionSyntax::LParen),
          (!has_variables).then_some(VariablesDefinitionSyntax::Variables),
          r_paren.is_none().then_some(VariablesDefinitionSyntax::RParen),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    VariableDef: CstNode<GraphQLLanguage>,
}

impl_graphql_node! {
  for<T, Description> Described<T, Description, GraphQLLanguage> {
    type Component = DescribedSyntax;
    type COMPONENTS = U2;
  } => Description(|syntax: SyntaxNode<GraphQLLanguage>| {
    let description = child(&syntax);
    let node = child(&syntax);

    match node {
      Some(node) => Ok(Described::new(syntax, description, node)),
      None => {
        let missing = IncompleteSyntax::new(DescribedSyntax::Node);
        Err(missing.into())
      }
    }
  })
  where
    T: CstNode<GraphQLLanguage>,
    Description: CstNode<GraphQLLanguage>,
}

mod ty;
