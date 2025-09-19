use logosky::utils::Span;
use smear_parser::{definitions, lang};

pub use definitions::v2::{Described, RcType};
pub use lang::v2::{FragmentName, Name};

use crate::{
  lexer::token::fast::{Token, TokenKind},
  parser::ast::{self, StringValue},
};

use super::{FastTokenErrors, FastTokenStream};

pub use directive_locations::*;
pub use fragment::*;
pub use implement_interfaces::*;
pub use list::*;
pub use object::*;
pub use union_member_types::*;
pub use value::*;

mod boolean_value;
mod directive_locations;
mod enum_value;
mod float;
mod fragment;
mod implement_interfaces;
mod int;
mod keyword;
mod list;
mod name;
mod null_value;
mod object;
mod punctuator;
mod string;
mod union_member_types;
mod value;
mod variable;

pub type Selection<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<Directive<S, ArgumentContainer>>,
> = ast::Selection<
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
> = ast::SelectionSet<
  Alias<S>,
  Name<S>,
  FragmentName<S>,
  TypeCondition<S>,
  Arguments<S, ArgumentContainer>,
  Directives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type Argument<S> = lang::v2::Argument<Name<S>, InputValue<S>>;
pub type Arguments<S, Container = Vec<Argument<S>>> = lang::v2::Arguments<Argument<S>, Container>;

pub type ConstArgument<S> = lang::v2::Argument<Name<S>, ConstInputValue<S>>;
pub type ConstArguments<S, Container = Vec<ConstArgument<S>>> =
  lang::v2::Arguments<ConstArgument<S>, Container>;

pub type Directive<S, ArgumentsContainer = Vec<Argument<S>>> =
  lang::v2::Directive<Name<S>, Arguments<S, ArgumentsContainer>>;
pub type Directives<
  S,
  ArgumentsContainer = Vec<Argument<S>>,
  Container = Vec<Directive<S, ArgumentsContainer>>,
> = lang::v2::Directives<Directive<S, ArgumentsContainer>, Container>;

pub type ConstDirective<S, ArgumentsContainer = Vec<ConstArgument<S>>> =
  lang::v2::Directive<Name<S>, ConstArguments<S, ArgumentsContainer>>;
pub type ConstDirectives<
  S,
  ArgumentsContainer = Vec<ConstArgument<S>>,
  Container = Vec<ConstDirective<S, ArgumentsContainer>>,
> = lang::v2::Directives<ConstDirective<S, ArgumentsContainer>, Container>;

pub type Alias<S> = lang::v2::Alias<Name<S>>;

pub type Field<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<Directive<S, ArgumentContainer>>,
> = ast::Field<
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
> = definitions::v2::ArgumentsDefinition<
  InputValueDefinition<S, ArgumentContainer, DirectiveContainer>,
  InputValueContainer,
>;

pub type VariableDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<Directive<S, ArgumentContainer>>,  
> = definitions::v2::VariableDefinition<
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
> = definitions::v2::VariablesDefinition<
  VariableDefinition<S, ArgumentContainer, DirectiveContainer>,
  VariableContainer,
>;

pub type InputValueDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
> = Described<definitions::v2::InputValueDefinition<
  Name<S>,
  RcType<Name<S>>,
  DefaultInputValue<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
>, StringValue<S>>;

pub type InputFieldsDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::v2::InputFieldsDefinition<
  InputValueDefinition<S, ArgumentContainer, DirectiveContainer>,
  InputValueContainer,
>;

pub type FieldDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = Described<
  definitions::v2::FieldDefinition<
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
> = definitions::v2::FieldsDefinition<
  FieldDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
  FieldContainer,
>;

pub type InputObjectTypeDefinitionContent<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::v2::InputObjectTypeDefinitionContent<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  InputFieldsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
>;

pub type InputObjectTypeDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::v2::InputObjectTypeDefinition<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  InputFieldsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
>;

pub type InputObjectTypeExtensionContent<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::v2::InputObjectTypeExtensionContent<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  InputFieldsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
>;

pub type InputObjectTypeExtension<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::v2::InputObjectTypeExtension<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  InputFieldsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
>;

pub type FragmentDefinitionContent<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<Directive<S, ArgumentContainer>>,
> = definitions::v2::FragmentDefinitionContent<
  FragmentName<S>,
  TypeCondition<S>,
  ConstDirectives<S, ArgumentContainer>,
  SelectionSet<S, ArgumentContainer, DirectiveContainer>,
>;

pub type FragmentDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<Directive<S, ArgumentContainer>>,
> = definitions::v2::FragmentDefinition<
  FragmentName<S>,
  TypeCondition<S>,
  ConstDirectives<S, ArgumentContainer>,
  SelectionSet<S, ArgumentContainer, DirectiveContainer>,
>;

pub type ScalarTypeDefinitionContent<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
> = definitions::v2::ScalarTypeDefinitionContent<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type ScalarTypeDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
> = definitions::v2::ScalarTypeDefinition<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type ScalarTypeExtensionContent<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
> = definitions::v2::ScalarTypeExtensionContent<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type ScalarTypeExtension<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
> = definitions::v2::ScalarTypeExtension<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type ObjectTypeDefinitionContent<
  S,
  ImplementInterfacesContainer = Vec<Name<S>>,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::v2::ObjectTypeDefinitionContent<
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
> = definitions::v2::ObjectTypeDefinition<
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
> = definitions::v2::ObjectTypeExtensionContent<
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
> = definitions::v2::ObjectTypeExtension<
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
> = definitions::v2::InterfaceTypeDefinitionContent<
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
> = definitions::v2::InterfaceTypeDefinition<
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
> = definitions::v2::InterfaceTypeExtensionContent<
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
> = definitions::v2::InterfaceTypeExtension<
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
> = definitions::v2::UnionTypeDefinitionContent<
  Name<S>,
  UnionMemberTypes<S, UnionMemberTypesContainer>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type UnionTypeDefinition<
  S,
  UnionMemberTypesContainer = Vec<Name<S>>,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
> = definitions::v2::UnionTypeDefinition<
  Name<S>,
  UnionMemberTypes<S, UnionMemberTypesContainer>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type UnionTypeExtensionContent<
  S,
  UnionMemberTypesContainer = Vec<Name<S>>,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
> = definitions::v2::UnionTypeExtensionContent<
  Name<S>,
  UnionMemberTypes<S, UnionMemberTypesContainer>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type UnionTypeExtension<
  S,
  UnionMemberTypesContainer = Vec<Name<S>>,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
> = definitions::v2::UnionTypeExtension<
  Name<S>,
  UnionMemberTypes<S, UnionMemberTypesContainer>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type EnumValueDefinition<S, ArgumentContainer = Vec<Argument<S>>, DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>> =
  Described<definitions::v2::EnumValueDefinition<Name<S>, ConstDirectives<S, ArgumentContainer, DirectiveContainer>>, StringValue<S>>;

pub type EnumValuesDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  EnumValueContainer = Vec<EnumValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::v2::EnumValuesDefinition<
  EnumValueDefinition<S, ArgumentContainer, DirectiveContainer>,
  EnumValueContainer,
>;

pub type EnumTypeDefinitionContent<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  EnumValueContainer = Vec<EnumValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::v2::EnumTypeDefinitionContent<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  EnumValuesDefinition<S, ArgumentContainer, DirectiveContainer, EnumValueContainer>,
>;

pub type EnumTypeDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  EnumValueContainer = Vec<EnumValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::v2::EnumTypeDefinition<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  EnumValuesDefinition<S, ArgumentContainer, DirectiveContainer, EnumValueContainer>,
>;

pub type EnumTypeExtensionContent<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  EnumValueContainer = Vec<EnumValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::v2::EnumTypeExtensionContent<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  EnumValuesDefinition<S, ArgumentContainer, DirectiveContainer, EnumValueContainer>,
>;

pub type EnumTypeExtension<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  EnumValueContainer = Vec<EnumValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::v2::EnumTypeExtension<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  EnumValuesDefinition<S, ArgumentContainer, DirectiveContainer, EnumValueContainer>,
>;
