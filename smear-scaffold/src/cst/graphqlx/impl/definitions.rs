use core::fmt::Debug;

use crate::cst::{
  ArgumentsDefinition, Described, Document, FieldsDefinition, FragmentDefinition,
  InputFieldsDefinition, InputValueDefinition, NamedOperationDefinition,
  RootOperationTypeDefinition, RootOperationTypesDefinition, VariableDefinition, VariablesDefinition, graphqlx::GraphQLxLanguage,
};

impl_graphqlx_node! {
  for<Definition> Document<Definition, GraphQLxLanguage> => Document(Document::new)
  where Definition: Debug
}

impl_graphqlx_node! {
  for<Name, OperationType, VariablesDef, Directives, SelectionSet>
    NamedOperationDefinition<
      Name,
      OperationType,
      VariablesDef,
      Directives,
      SelectionSet,
      GraphQLxLanguage,
    > => NamedOperationDefinition(NamedOperationDefinition::new)
  where
    Name: Debug,
    OperationType: Debug,
    VariablesDef: Debug,
    Directives: Debug,
    SelectionSet: Debug
}

impl_graphqlx_node! {
  for<Name, Type, DefaultValue, Directives>
    InputValueDefinition<Name, Type, DefaultValue, Directives, GraphQLxLanguage> =>
      InputValueDefinition(InputValueDefinition::new)
  where
    Name: Debug,
    Type: Debug,
    DefaultValue: Debug,
    Directives: Debug
}

impl_graphqlx_node! {
  for<InputValueDef> ArgumentsDefinition<InputValueDef, GraphQLxLanguage> =>
    ArgumentsDefinition(ArgumentsDefinition::new)
  where InputValueDef: Debug
}

impl_graphqlx_node! {
  for<InputValueDef> InputFieldsDefinition<InputValueDef, GraphQLxLanguage> =>
    InputFieldsDefinition(InputFieldsDefinition::new)
  where InputValueDef: Debug
}

impl_graphqlx_node! {
  for<FieldDef> FieldsDefinition<FieldDef, GraphQLxLanguage> => FieldsDefinition(FieldsDefinition::new)
  where FieldDef: Debug
}

impl_graphqlx_node! {
  for<FragmentName, TypeCond, Directives, SelectionSet>
    FragmentDefinition<FragmentName, TypeCond, Directives, SelectionSet, GraphQLxLanguage> =>
      FragmentDefinition(FragmentDefinition::new)
  where
    FragmentName: Debug,
    TypeCond: Debug,
    Directives: Debug,
    SelectionSet: Debug
}

impl_graphqlx_node! {
  for<OperationType, Name>
    RootOperationTypeDefinition<OperationType, Name, GraphQLxLanguage> =>
      RootOperationTypeDefinition(RootOperationTypeDefinition::new)
  where
    OperationType: Debug,
    Name: Debug
}

impl_graphqlx_node! {
  for<RootOpTypeDef> RootOperationTypesDefinition<RootOpTypeDef, GraphQLxLanguage> =>
    RootOperationTypesDefinition(RootOperationTypesDefinition::new)
  where RootOpTypeDef: Debug
}

impl_graphqlx_node! {
  for<Variable, Type, DefaultValue, Directives>
    VariableDefinition<Variable, Type, DefaultValue, Directives, GraphQLxLanguage> =>
      VariableDefinition(VariableDefinition::new)
  where
    Variable: Debug,
    Type: Debug,
    DefaultValue: Debug,
    Directives: Debug
}

impl_graphqlx_node! {
  for<VariableDef> VariablesDefinition<VariableDef, GraphQLxLanguage> =>
    VariablesDefinition(VariablesDefinition::new)
  where VariableDef: Debug
}

impl_graphqlx_node! {
  for<T, Description> Described<T, Description, GraphQLxLanguage> => Description(Described::new)
  where
    T: Debug,
    Description: Debug
}

mod generic;
mod ty;
