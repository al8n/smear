use chumsky::{extra::ParserExtra, prelude::*};
use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{Parseable, Source, Token, Tokenizer, utils::Span};
use smear_parser::{
  definitions::{self, minized::Location},
  lang,
  source::IntoSpan,
};

pub use definitions::{
  Described,
  minized::{OperationType, RcType},
};
pub use lang::minized::{FragmentName, Name};

use super::{FastToken, FastTokenErrors, FastTokenKind, FastTokenStream};

pub use directive_locations::*;
pub use fragment::*;
pub use implement_interfaces::*;
pub use union_member_types::*;
pub use value::*;

mod directive_locations;
mod field;
mod fragment;
mod implement_interfaces;
mod keyword;
mod name;
mod operation_type;
mod punctuator;
mod selection_set;
mod union_member_types;
mod value;

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
  ArgumentContainer = Vec<ConstArgument<S>>,
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
  ArgumentContainer = Vec<ConstArgument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::InputFieldsDefinition<
  InputValueDefinition<S, ArgumentContainer, DirectiveContainer>,
  InputValueContainer,
>;

pub type FieldDefinition<
  S,
  ArgumentContainer = Vec<ConstArgument<S>>,
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
  ArgumentContainer = Vec<ConstArgument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
  FieldContainer = Vec<
    FieldDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
  >,
> = definitions::minized::FieldsDefinition<
  FieldDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
  FieldContainer,
>;

pub type InputObjectTypeDefinition<
  S,
  ArgumentContainer = Vec<ConstArgument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::InputObjectTypeDefinition<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  InputFieldsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
>;

pub type InputObjectTypeExtension<
  S,
  ArgumentContainer = Vec<ConstArgument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::InputObjectTypeExtension<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  InputFieldsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
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

pub type ScalarTypeDefinition<
  S,
  ArgumentContainer = Vec<ConstArgument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
> = definitions::minized::ScalarTypeDefinition<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type ScalarTypeExtension<
  S,
  ArgumentContainer = Vec<ConstArgument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
> = definitions::minized::ScalarTypeExtension<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type ObjectTypeDefinition<
  S,
  ImplementInterfacesContainer = Vec<Name<S>>,
  ArgumentContainer = Vec<ConstArgument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::ObjectTypeDefinition<
  Name<S>,
  ImplementInterfaces<S, ImplementInterfacesContainer>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  FieldsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
>;

pub type ObjectTypeExtension<
  S,
  ImplementInterfacesContainer = Vec<Name<S>>,
  ArgumentContainer = Vec<ConstArgument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::ObjectTypeExtension<
  Name<S>,
  ImplementInterfaces<S, ImplementInterfacesContainer>,
  Directives<S, ArgumentContainer, DirectiveContainer>,
  FieldsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
>;

pub type InterfaceTypeDefinition<
  S,
  ImplementInterfacesContainer = Vec<Name<S>>,
  ArgumentContainer = Vec<ConstArgument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::InterfaceTypeDefinition<
  Name<S>,
  ImplementInterfaces<S, ImplementInterfacesContainer>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  FieldsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
>;

pub type InterfaceTypeExtension<
  S,
  ImplementInterfacesContainer = Vec<Name<S>>,
  ArgumentContainer = Vec<ConstArgument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::InterfaceTypeExtension<
  Name<S>,
  ImplementInterfaces<S, ImplementInterfacesContainer>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  FieldsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
>;

pub type UnionTypeDefinition<
  S,
  UnionMemberTypesContainer = Vec<Name<S>>,
  ArgumentContainer = Vec<ConstArgument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
> = definitions::minized::UnionTypeDefinition<
  Name<S>,
  UnionMemberTypes<S, UnionMemberTypesContainer>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type UnionTypeExtension<
  S,
  UnionMemberTypesContainer = Vec<Name<S>>,
  ArgumentContainer = Vec<ConstArgument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
> = definitions::minized::UnionTypeExtension<
  Name<S>,
  UnionMemberTypes<S, UnionMemberTypesContainer>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type EnumValueDefinition<
  S,
  ArgumentContainer = Vec<ConstArgument<S>>,
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
  ArgumentContainer = Vec<ConstArgument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  EnumValueContainer = Vec<EnumValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::EnumValuesDefinition<
  EnumValueDefinition<S, ArgumentContainer, DirectiveContainer>,
  EnumValueContainer,
>;

pub type EnumTypeDefinition<
  S,
  ArgumentContainer = Vec<ConstArgument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  EnumValueContainer = Vec<EnumValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::EnumTypeDefinition<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  EnumValuesDefinition<S, ArgumentContainer, DirectiveContainer, EnumValueContainer>,
>;

pub type EnumTypeExtension<
  S,
  ArgumentContainer = Vec<ConstArgument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  EnumValueContainer = Vec<EnumValueDefinition<S, ArgumentContainer, DirectiveContainer>>,
> = definitions::minized::EnumTypeExtension<
  Name<S>,
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  EnumValuesDefinition<S, ArgumentContainer, DirectiveContainer, EnumValueContainer>,
>;

pub type NamedOperationDefinition<S> = definitions::minized::NamedOperationDefinition<
  Name<S>,
  OperationType,
  VariablesDefinition<S>,
  Directives<S>,
  SelectionSet<S>,
>;

pub type OperationDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<Directive<S, ArgumentContainer>>,
> = definitions::minized::OperationDefinition<
  Name<S>,
  OperationType,
  VariablesDefinition<S, ArgumentContainer, DirectiveContainer>,
  Directives<S, ArgumentContainer, DirectiveContainer>,
  SelectionSet<S, ArgumentContainer, DirectiveContainer>,
>;

pub type RootOperationTypeDefinition<S> =
  definitions::minized::RootOperationTypeDefinition<Name<S>, OperationType>;

pub type RootOperationTypesDefinition<S, Container = Vec<RootOperationTypeDefinition<S>>> =
  definitions::minized::RootOperationTypesDefinition<RootOperationTypeDefinition<S>, Container>;

pub type SchemaDefinition<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  Container = Vec<RootOperationTypeDefinition<S>>,
> = definitions::minized::SchemaDefinition<
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  RootOperationTypesDefinition<S, Container>,
>;

pub type SchemaExtension<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<ConstDirective<S, ArgumentContainer>>,
  Container = Vec<RootOperationTypeDefinition<S>>,
> = definitions::minized::SchemaExtension<
  ConstDirectives<S, ArgumentContainer, DirectiveContainer>,
  RootOperationTypesDefinition<S, Container>,
>;

#[derive(Debug, Clone, IsVariant, From, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum TypeDefinition<S> {
  Scalar(ScalarTypeDefinition<S>),
  Object(ObjectTypeDefinition<S>),
  Interface(InterfaceTypeDefinition<S>),
  Union(UnionTypeDefinition<S>),
  Enum(EnumTypeDefinition<S>),
  InputObject(InputObjectTypeDefinition<S>),
}

impl<S> AsRef<Span> for TypeDefinition<S> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<S> IntoSpan<Span> for TypeDefinition<S> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Scalar(def) => def.into_span(),
      Self::Object(def) => def.into_span(),
      Self::Interface(def) => def.into_span(),
      Self::Union(def) => def.into_span(),
      Self::Enum(def) => def.into_span(),
      Self::InputObject(def) => def.into_span(),
    }
  }
}

impl<S> TypeDefinition<S> {
  /// Returns a reference to the span covering the entire type definition.
  ///
  /// The span includes the description (if any), the type keyword, the name, any interfaces or
  /// directives, and the fields or values.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Scalar(def) => def.span(),
      Self::Object(def) => def.span(),
      Self::Interface(def) => def.span(),
      Self::Union(def) => def.span(),
      Self::Enum(def) => def.span(),
      Self::InputObject(def) => def.span(),
    }
  }
}

impl<'a> Parseable<'a, FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>>
  for TypeDefinition<&'a str>
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, FastTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, FastTokenStream<'a>, Error = FastTokenErrors<'a, &'a str>> + 'a,
  {
    choice((
      ScalarTypeDefinition::parser::<E>().map(Self::Scalar),
      ObjectTypeDefinition::parser::<E>().map(Self::Object),
      InterfaceTypeDefinition::parser::<E>().map(Self::Interface),
      UnionTypeDefinition::parser::<E>().map(Self::Union),
      EnumTypeDefinition::parser::<E>().map(Self::Enum),
      InputObjectTypeDefinition::parser::<E>().map(Self::InputObject),
    ))
  }
}
