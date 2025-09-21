use logosky::utils::Span;
use smear_parser::{
  definitions::{self, minized::Location},
  lang,
};

pub use definitions::{Described, minized::RcType};
pub use lang::minized::{FragmentName, Name};

use crate::lexer::token::fast::{Token, TokenKind};

use super::{FastToken, FastTokenErrors, FastTokenStream};

pub use boolean_value::*;
pub use directive_locations::*;
pub use enum_value::*;
pub use float::*;
pub use fragment::*;
pub use implement_interfaces::*;
pub use int::*;
pub use null_value::*;
pub use string::*;
pub use union_member_types::*;
pub use value::*;

mod boolean_value;
mod directive_locations;
mod enum_value;
mod field;
mod float;
mod fragment;
mod implement_interfaces;
mod int;
mod keyword;
mod list;
mod name;
mod null_value;
mod object;
mod operation_type;
mod punctuator;
mod selection_set;
mod string;
mod union_member_types;
mod value;

pub type Variable<S> = smear_parser::lang::minized::Variable<Name<S>>;

pub type Selection<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<Directive<S, ArgumentContainer>>,
> = selection_set::Selection<
  Alias<S>,
  Name<S>,
  FragmentName<S>,
  TypeCondition<S>,
  Arguments<S, ArgumentContainer>,
  Directives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type SelectionSet<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<Directive<S, ArgumentContainer>>,
> = selection_set::SelectionSet<
  Alias<S>,
  Name<S>,
  FragmentName<S>,
  TypeCondition<S>,
  Arguments<S, ArgumentContainer>,
  Directives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type Argument<S> = lang::minized::Argument<Name<S>, InputValue<S>>;
pub type Arguments<S, Container = Vec<Argument<S>>> =
  lang::minized::Arguments<Argument<S>, Container>;

pub type ConstArgument<S> = lang::minized::Argument<Name<S>, ConstInputValue<S>>;
pub type ConstArguments<S, Container = Vec<ConstArgument<S>>> =
  lang::minized::Arguments<ConstArgument<S>, Container>;

pub type Directive<S, ArgumentsContainer = Vec<Argument<S>>> =
  lang::minized::Directive<Name<S>, Arguments<S, ArgumentsContainer>>;
pub type Directives<
  S,
  ArgumentsContainer = Vec<Argument<S>>,
  Container = Vec<Directive<S, ArgumentsContainer>>,
> = lang::minized::Directives<Directive<S, ArgumentsContainer>, Container>;

pub type ConstDirective<S, ArgumentsContainer = Vec<ConstArgument<S>>> =
  lang::minized::Directive<Name<S>, ConstArguments<S, ArgumentsContainer>>;
pub type ConstDirectives<
  S,
  ArgumentsContainer = Vec<ConstArgument<S>>,
  Container = Vec<ConstDirective<S, ArgumentsContainer>>,
> = lang::minized::Directives<ConstDirective<S, ArgumentsContainer>, Container>;

pub type Alias<S> = lang::minized::Alias<Name<S>>;

pub type Field<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<Directive<S, ArgumentContainer>>,
> = field::Field<
  Alias<S>,
  Name<S>,
  FragmentName<S>,
  TypeCondition<S>,
  Arguments<S, ArgumentContainer>,
  Directives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type ArgumentsDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<Directive<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::ArgumentsDefinition<
  InputValueDefinition<S, ArgumentContainer, DirectiveContainer>,
  InputValueContainer,
>;

pub type DirectiveDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  LocationsContainer = Vec<Location>,
> = definitions::minized::DirectiveDefinition<
  Name<S>,
  ArgumentsDefinition<S, ArgumentContainer, InputValueDefinition<S, ArgumentContainer>>,
  DirectiveLocations<LocationsContainer>,
>;

pub type VariableDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<Directive<S, ArgumentContainer>>,
> = definitions::minized::VariableDefinition<
  Name<S>,
  RcType<Name<S>>,
  DefaultInputValue<S>,
  Directives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type VariablesDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<Directive<S, ArgumentContainer>>,
  VariableContainer = Vec<VariableDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::VariablesDefinition<
  VariableDefinition<S, ArgumentContainer, DirectiveContainer>,
  VariableContainer,
>;

pub type InputValueDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
> = Described<
  definitions::minized::InputValueDefinition<
    Name<S>,
    RcType<Name<S>>,
    DefaultInputValue<S>,
    ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  >,
  StringValue<S>,
>;

pub type InputFieldsDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::InputFieldsDefinition<
  InputValueDefinition<S, ArgumentContainer, DirectiveContainer>,
  InputValueContainer,
>;

pub type FieldDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = Described<
  definitions::minized::FieldDefinition<
    Name<S>,
    ArgumentsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
    RcType<Name<S>>,
    ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  >,
  StringValue<S>,
>;

pub type FieldsDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
  FieldContainer = Vec<
    FieldDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
  >,
> = definitions::minized::FieldsDefinition<
  FieldDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
  FieldContainer,
>;

pub type InputObjectTypeDefinitionContent<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::InputObjectTypeDefinitionContent<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  InputFieldsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
>;

pub type InputObjectTypeDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::InputObjectTypeDefinition<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  InputFieldsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
>;

pub type InputObjectTypeExtensionContent<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::InputObjectTypeExtensionContent<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  InputFieldsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
>;

pub type InputObjectTypeExtension<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::InputObjectTypeExtension<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  InputFieldsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
>;

pub type FragmentDefinitionContent<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<Directive<S, ArgumentContainer>>,
> = definitions::minized::FragmentDefinitionContent<
  FragmentName<S>,
  TypeCondition<S>,
  ConstDirectives<S, ArgumentContainer>,
  SelectionSet<S, ArgumentContainer, DirectiveContainer>,
>;

pub type FragmentDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<Directive<S, ArgumentContainer>>,
> = definitions::minized::FragmentDefinition<
  FragmentName<S>,
  TypeCondition<S>,
  ConstDirectives<S, ArgumentContainer>,
  SelectionSet<S, ArgumentContainer, DirectiveContainer>,
>;

pub type ScalarTypeDefinitionContent<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
> = definitions::minized::ScalarTypeDefinitionContent<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type ScalarTypeDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
> = definitions::minized::ScalarTypeDefinition<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type ScalarTypeExtensionContent<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
> = definitions::minized::ScalarTypeExtensionContent<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type ScalarTypeExtension<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
> = definitions::minized::ScalarTypeExtension<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type ObjectTypeDefinitionContent<
  S,
  ImplementInterfacesContainer = Vec<Name<S>>,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::ObjectTypeDefinitionContent<
  Name<S>,
  ImplementInterfaces<S, ImplementInterfacesContainer>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  FieldsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
>;

pub type ObjectTypeDefinition<
  S,
  ImplementInterfacesContainer = Vec<Name<S>>,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::ObjectTypeDefinition<
  Name<S>,
  ImplementInterfaces<S, ImplementInterfacesContainer>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  FieldsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
>;

pub type ObjectTypeExtensionContent<
  S,
  ImplementInterfacesContainer = Vec<Name<S>>,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::ObjectTypeExtensionContent<
  Name<S>,
  ImplementInterfaces<S, ImplementInterfacesContainer>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  FieldsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
>;

pub type ObjectTypeExtension<
  S,
  ImplementInterfacesContainer = Vec<Name<S>>,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<Directive<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::ObjectTypeExtension<
  Name<S>,
  ImplementInterfaces<S, ImplementInterfacesContainer>,
  Directives<S, ArgumentContainer, DirectiveContainer>,
  FieldsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
>;

pub type InterfaceTypeDefinitionContent<
  S,
  ImplementInterfacesContainer = Vec<Name<S>>,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::InterfaceTypeDefinitionContent<
  Name<S>,
  ImplementInterfaces<S, ImplementInterfacesContainer>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  FieldsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
>;

pub type InterfaceTypeDefinition<
  S,
  ImplementInterfacesContainer = Vec<Name<S>>,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::InterfaceTypeDefinition<
  Name<S>,
  ImplementInterfaces<S, ImplementInterfacesContainer>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  FieldsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
>;

pub type InterfaceTypeExtensionContent<
  S,
  ImplementInterfacesContainer = Vec<Name<S>>,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::InterfaceTypeExtensionContent<
  Name<S>,
  ImplementInterfaces<S, ImplementInterfacesContainer>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  FieldsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
>;

pub type InterfaceTypeExtension<
  S,
  ImplementInterfacesContainer = Vec<Name<S>>,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::InterfaceTypeExtension<
  Name<S>,
  ImplementInterfaces<S, ImplementInterfacesContainer>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  FieldsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
>;

pub type UnionTypeDefinitionContent<
  S,
  UnionMemberTypesContainer = Vec<Name<S>>,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
> = definitions::minized::UnionTypeDefinitionContent<
  Name<S>,
  UnionMemberTypes<S, UnionMemberTypesContainer>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type UnionTypeDefinition<
  S,
  UnionMemberTypesContainer = Vec<Name<S>>,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
> = definitions::minized::UnionTypeDefinition<
  Name<S>,
  UnionMemberTypes<S, UnionMemberTypesContainer>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type UnionTypeExtensionContent<
  S,
  UnionMemberTypesContainer = Vec<Name<S>>,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
> = definitions::minized::UnionTypeExtensionContent<
  Name<S>,
  UnionMemberTypes<S, UnionMemberTypesContainer>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type UnionTypeExtension<
  S,
  UnionMemberTypesContainer = Vec<Name<S>>,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
> = definitions::minized::UnionTypeExtension<
  Name<S>,
  UnionMemberTypes<S, UnionMemberTypesContainer>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type EnumValueDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
> = Described<
  definitions::minized::EnumValueDefinition<
    Name<S>,
    ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  >,
  StringValue<S>,
>;

pub type EnumValuesDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  EnumValueContainer = Vec<EnumValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::EnumValuesDefinition<
  EnumValueDefinition<S, ArgumentContainer, DirectiveContainer>,
  EnumValueContainer,
>;

pub type EnumTypeDefinitionContent<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  EnumValueContainer = Vec<EnumValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::EnumTypeDefinitionContent<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  EnumValuesDefinition<S, ArgumentContainer, DirectiveContainer, EnumValueContainer>,
>;

pub type EnumTypeDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  EnumValueContainer = Vec<EnumValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::EnumTypeDefinition<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  EnumValuesDefinition<S, ArgumentContainer, DirectiveContainer, EnumValueContainer>,
>;

pub type EnumTypeExtensionContent<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  EnumValueContainer = Vec<EnumValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::EnumTypeExtensionContent<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  EnumValuesDefinition<S, ArgumentContainer, DirectiveContainer, EnumValueContainer>,
>;

pub type EnumTypeExtension<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  EnumValueContainer = Vec<EnumValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::EnumTypeExtension<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  EnumValuesDefinition<S, ArgumentContainer, DirectiveContainer, EnumValueContainer>,
>;
