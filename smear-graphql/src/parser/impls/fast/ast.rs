#![allow(clippy::type_complexity)]

use chumsky::{container::Container as ChumskyContainer, extra::ParserExtra, prelude::*};
use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{Parseable, Source, Token, Tokenizer, utils::Span};
use smear_parser::{
  definitions::{self, minized::Location},
  lang,
  source::IntoSpan,
};

use super::{FastToken, FastTokenErrors, FastTokenKind, FastTokenStream};

use core::marker::PhantomData;

pub use definitions::{
  Described,
  minized::{OperationType, RcType},
};
pub use lang::minized::{FragmentName, Name};

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

/// The default container type used for collections in the AST.
pub type DefaultVec<T> = Vec<T>;
/// The default container type used for arguments in the AST.
pub type DefaultArgumentsContainer<S> = DefaultVec<Argument<S>>;
/// The default container type used for directives in the AST.
pub type DefaultDirectivesContainer<S, ArgumentsContainer> =
  DefaultVec<Directive<S, ArgumentsContainer>>;
/// The default container type used for const arguments in the AST.
pub type DefaultConstArgumentsContainer<S> = DefaultVec<ConstArgument<S>>;
/// The default container type used for const directives in the AST.
pub type DefaultConstDirectivesContainer<S, ConstArgumentsContainer> =
  Vec<ConstDirective<S, ConstArgumentsContainer>>;
/// The default container type used for input values in the AST.
pub type DefaultInputValuesContainer<S, ConstArgumentsContainer, ConstDirectivesContainer> =
  Vec<InputValueDefinition<S, ConstArgumentsContainer, ConstDirectivesContainer>>;
/// The default container type used for enum values in the AST.
pub type DefaultEnumValuesContainer<S, ConstArgumentsContainer, ConstDirectivesContainer> =
  Vec<EnumValueDefinition<S, ConstArgumentsContainer, ConstDirectivesContainer>>;
/// The default container type used for names in the AST.
pub type DefaultNamesContainer<S> = DefaultVec<Name<S>>;
/// The default container type used for locations in the AST.
pub type DefaultLocationsContainer = DefaultVec<Location>;
/// The default container type used for root operation types in the AST.
pub type DefaultRootOperationTypesContainer<S> = DefaultVec<RootOperationTypeDefinition<S>>;
/// The default container type used for variable definitions in the AST.
pub type DefaultVariablesContainer<S, ArgumentsContainer, DirectivesContainer> =
  Vec<VariableDefinition<S, ArgumentsContainer, DirectivesContainer>>;
/// The default container type used for fields in the AST.
pub type DefaultFieldsContainer<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer> =
  Vec<FieldDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>>;

pub type Selection<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = selection_set::Selection<
  Alias<S>,
  Name<S>,
  FragmentName<S>,
  TypeCondition<S>,
  Arguments<S, ArgumentsContainer>,
  Directives<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type SelectionSet<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = selection_set::SelectionSet<
  Alias<S>,
  Name<S>,
  FragmentName<S>,
  TypeCondition<S>,
  Arguments<S, ArgumentsContainer>,
  Directives<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type Argument<S> = lang::minized::Argument<Name<S>, InputValue<S>>;
pub type Arguments<S, Container = DefaultArgumentsContainer<S>> =
  lang::minized::Arguments<Argument<S>, Container>;

pub type ConstArgument<S> = lang::minized::Argument<Name<S>, ConstInputValue<S>>;
pub type ConstArguments<S, Container = DefaultConstArgumentsContainer<S>> =
  lang::minized::Arguments<ConstArgument<S>, Container>;

pub type Directive<S, ArgumentsContainer = DefaultArgumentsContainer<S>> =
  lang::minized::Directive<Name<S>, Arguments<S, ArgumentsContainer>>;
pub type Directives<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  Container = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = lang::minized::Directives<Directive<S, ArgumentsContainer>, Container>;

pub type ConstDirective<S, ArgumentsContainer = DefaultConstArgumentsContainer<S>> =
  lang::minized::Directive<Name<S>, ConstArguments<S, ArgumentsContainer>>;
pub type ConstDirectives<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  Container = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = lang::minized::Directives<ConstDirective<S, ArgumentsContainer>, Container>;

pub type Alias<S> = lang::minized::Alias<Name<S>>;

pub type Field<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = field::Field<
  Alias<S>,
  Name<S>,
  FragmentName<S>,
  TypeCondition<S>,
  Arguments<S, ArgumentsContainer>,
  Directives<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type ArgumentsDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::minized::ArgumentsDefinition<
  InputValueDefinition<S, ArgumentsContainer, DirectivesContainer>,
  InputValuesContainer,
>;

pub type DirectiveDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  LocationsContainer = DefaultLocationsContainer,
> = definitions::minized::DirectiveDefinition<
  Name<S>,
  ArgumentsDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
  DirectiveLocations<LocationsContainer>,
>;

pub type VariableDefinition<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = definitions::minized::VariableDefinition<
  Variable<S>,
  RcType<Name<S>>,
  DefaultInputValue<S>,
  Directives<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type VariablesDefinition<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
  VariablesContainer = DefaultVariablesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::minized::VariablesDefinition<
  VariableDefinition<S, ArgumentsContainer, DirectivesContainer>,
  VariablesContainer,
>;

pub type InputValueDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = Described<
  definitions::minized::InputValueDefinition<
    Name<S>,
    RcType<Name<S>>,
    DefaultInputValue<S>,
    ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  >,
  StringValue<S>,
>;

pub type InputFieldsDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::minized::InputFieldsDefinition<
  InputValueDefinition<S, ArgumentsContainer, DirectivesContainer>,
  InputValuesContainer,
>;

pub type FieldDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = Described<
  definitions::minized::FieldDefinition<
    Name<S>,
    ArgumentsDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
    RcType<Name<S>>,
    ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  >,
  StringValue<S>,
>;

pub type FieldsDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  FieldsContainer = DefaultFieldsContainer<
    S,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
> = definitions::minized::FieldsDefinition<
  FieldDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
  FieldsContainer,
>;

pub type InputObjectTypeDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::minized::InputObjectTypeDefinition<
  Name<S>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  InputFieldsDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
>;

pub type DescribedInputObjectTypeDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = Described<
  InputObjectTypeDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
  StringValue<S>,
>;

pub type InputObjectTypeExtension<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::minized::InputObjectTypeExtension<
  Name<S>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  InputFieldsDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
>;

pub type FragmentDefinition<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = definitions::minized::FragmentDefinition<
  FragmentName<S>,
  TypeCondition<S>,
  Directives<S, ArgumentsContainer>,
  SelectionSet<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type ScalarTypeDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = definitions::minized::ScalarTypeDefinition<
  Name<S>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type DescribedScalarTypeDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = Described<ScalarTypeDefinition<S, ArgumentsContainer, DirectivesContainer>, StringValue<S>>;

pub type ScalarTypeExtension<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = definitions::minized::ScalarTypeExtension<
  Name<S>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type ObjectTypeDefinition<
  S,
  ImplementInterfacesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::minized::ObjectTypeDefinition<
  Name<S>,
  ImplementInterfaces<S, ImplementInterfacesContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  FieldsDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
>;

pub type DescribedObjectTypeDefinition<
  S,
  ImplementInterfacesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = Described<
  ObjectTypeDefinition<
    S,
    ImplementInterfacesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
  StringValue<S>,
>;

pub type ObjectTypeExtension<
  S,
  ImplementInterfacesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::minized::ObjectTypeExtension<
  Name<S>,
  ImplementInterfaces<S, ImplementInterfacesContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  FieldsDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
>;

pub type InterfaceTypeDefinition<
  S,
  ImplementInterfacesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::minized::InterfaceTypeDefinition<
  Name<S>,
  ImplementInterfaces<S, ImplementInterfacesContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  FieldsDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
>;

pub type DescribedInterfaceTypeDefinition<
  S,
  ImplementInterfacesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = Described<
  InterfaceTypeDefinition<
    S,
    ImplementInterfacesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
  StringValue<S>,
>;

pub type InterfaceTypeExtension<
  S,
  ImplementInterfacesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::minized::InterfaceTypeExtension<
  Name<S>,
  ImplementInterfaces<S, ImplementInterfacesContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  FieldsDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
>;

pub type UnionTypeDefinition<
  S,
  UnionMemberTypesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = definitions::minized::UnionTypeDefinition<
  Name<S>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  UnionMemberTypes<S, UnionMemberTypesContainer>,
>;

pub type DescribedUnionTypeDefinition<
  S,
  UnionMemberTypesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = Described<
  UnionTypeDefinition<S, UnionMemberTypesContainer, ArgumentsContainer, DirectivesContainer>,
  StringValue<S>,
>;

pub type UnionTypeExtension<
  S,
  UnionMemberTypesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = definitions::minized::UnionTypeExtension<
  Name<S>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  UnionMemberTypes<S, UnionMemberTypesContainer>,
>;

pub type EnumValueDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = Described<
  definitions::minized::EnumValueDefinition<
    Name<S>,
    ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  >,
  StringValue<S>,
>;

pub type EnumValuesDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::minized::EnumValuesDefinition<
  EnumValueDefinition<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesContainer,
>;

pub type EnumTypeDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::minized::EnumTypeDefinition<
  Name<S>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesDefinition<S, ArgumentsContainer, DirectivesContainer, EnumValuesContainer>,
>;

pub type DescribedEnumTypeDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = Described<
  EnumTypeDefinition<S, ArgumentsContainer, DirectivesContainer, EnumValuesContainer>,
  StringValue<S>,
>;

pub type EnumTypeExtension<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::minized::EnumTypeExtension<
  Name<S>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesDefinition<S, ArgumentsContainer, DirectivesContainer, EnumValuesContainer>,
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
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = definitions::minized::OperationDefinition<
  Name<S>,
  OperationType,
  VariablesDefinition<S, ArgumentsContainer, DirectivesContainer>,
  Directives<S, ArgumentsContainer, DirectivesContainer>,
  SelectionSet<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type RootOperationTypeDefinition<S> =
  definitions::minized::RootOperationTypeDefinition<Name<S>, OperationType>;

pub type RootOperationTypesDefinition<S, Container = DefaultRootOperationTypesContainer<S>> =
  definitions::minized::RootOperationTypesDefinition<RootOperationTypeDefinition<S>, Container>;

pub type SchemaDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  Container = DefaultRootOperationTypesContainer<S>,
> = definitions::minized::SchemaDefinition<
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  RootOperationTypesDefinition<S, Container>,
>;

pub type SchemaExtension<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  Container = DefaultRootOperationTypesContainer<S>,
> = definitions::minized::SchemaExtension<
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  RootOperationTypesDefinition<S, Container>,
>;

#[derive(Debug, Clone, IsVariant, From, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum TypeDefinition<
  S,
  NamesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> {
  Scalar(ScalarTypeDefinition<S, ArgumentsContainer, DirectivesContainer>),
  Object(
    ObjectTypeDefinition<
      S,
      NamesContainer,
      ArgumentsContainer,
      DirectivesContainer,
      InputValuesContainer,
    >,
  ),
  Interface(
    InterfaceTypeDefinition<
      S,
      NamesContainer,
      ArgumentsContainer,
      DirectivesContainer,
      InputValuesContainer,
    >,
  ),
  Union(UnionTypeDefinition<S, NamesContainer, ArgumentsContainer, DirectivesContainer>),
  Enum(EnumTypeDefinition<S, ArgumentsContainer, DirectivesContainer, EnumValuesContainer>),
  InputObject(
    InputObjectTypeDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
  ),
}

impl<
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
> AsRef<Span>
  for TypeDefinition<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
  >
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
> IntoSpan<Span>
  for TypeDefinition<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
  >
{
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

impl<
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
>
  TypeDefinition<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
  >
{
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

impl<
  'a,
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  I,
  T,
  Error,
> Parseable<'a, I, T, Error>
  for TypeDefinition<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
  >
where
  ScalarTypeDefinition<S, ArgumentsContainer, DirectivesContainer>: Parseable<'a, I, T, Error>,
  ObjectTypeDefinition<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >: Parseable<'a, I, T, Error>,
  InterfaceTypeDefinition<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >: Parseable<'a, I, T, Error>,
  UnionTypeDefinition<S, NamesContainer, ArgumentsContainer, DirectivesContainer>:
    Parseable<'a, I, T, Error>,
  EnumTypeDefinition<S, ArgumentsContainer, DirectivesContainer, EnumValuesContainer>:
    Parseable<'a, I, T, Error>,
  InputObjectTypeDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>:
    Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
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

#[derive(Debug, Clone, IsVariant, From, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum TypeSystemDefinition<
  S,
  NamesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  LocationsContainer = DefaultLocationsContainer,
  RootOperationTypesContainer = DefaultRootOperationTypesContainer<S>,
> {
  Type(
    TypeDefinition<
      S,
      NamesContainer,
      ArgumentsContainer,
      DirectivesContainer,
      InputValuesContainer,
      EnumValuesContainer,
    >,
  ),
  Directive(
    DirectiveDefinition<
      S,
      ArgumentsContainer,
      DirectivesContainer,
      InputValuesContainer,
      LocationsContainer,
    >,
  ),
  Schema(SchemaDefinition<S, ArgumentsContainer, DirectivesContainer, RootOperationTypesContainer>),
}

impl<
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  LocationsContainer,
  RootOperationTypesContainer,
> AsRef<Span>
  for TypeSystemDefinition<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  LocationsContainer,
  RootOperationTypesContainer,
> IntoSpan<Span>
  for TypeSystemDefinition<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Type(t) => t.into_span(),
      Self::Directive(d) => d.into_span(),
      Self::Schema(s) => s.into_span(),
    }
  }
}

impl<
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  LocationsContainer,
  RootOperationTypesContainer,
>
  TypeSystemDefinition<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >
{
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Type(t) => t.span(),
      Self::Directive(d) => d.span(),
      Self::Schema(s) => s.span(),
    }
  }
}

impl<
  'a,
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  LocationsContainer,
  RootOperationTypesContainer,
  I,
  T,
  Error,
> Parseable<'a, I, T, Error>
  for TypeSystemDefinition<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >
where
  TypeDefinition<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
  >: Parseable<'a, I, T, Error>,
  DirectiveDefinition<
    S,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    LocationsContainer,
  >: Parseable<'a, I, T, Error>,
  SchemaDefinition<S, ArgumentsContainer, DirectivesContainer, RootOperationTypesContainer>:
    Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    choice((
      TypeDefinition::parser::<E>().map(Self::Type),
      DirectiveDefinition::parser::<E>().map(Self::Directive),
      SchemaDefinition::parser::<E>().map(Self::Schema),
    ))
  }
}

#[derive(Debug, Clone, IsVariant, From, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum TypeExtension<
  S,
  NamesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> {
  Scalar(ScalarTypeExtension<S, ArgumentsContainer, DirectivesContainer>),
  Enum(EnumTypeExtension<S, ArgumentsContainer, DirectivesContainer, EnumValuesContainer>),
  Union(UnionTypeExtension<S, NamesContainer, ArgumentsContainer, DirectivesContainer>),
  InputObject(
    InputObjectTypeExtension<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
  ),
  Object(
    ObjectTypeExtension<
      S,
      NamesContainer,
      ArgumentsContainer,
      DirectivesContainer,
      InputValuesContainer,
    >,
  ),
  Interface(
    InterfaceTypeExtension<
      S,
      NamesContainer,
      ArgumentsContainer,
      DirectivesContainer,
      InputValuesContainer,
    >,
  ),
}

impl<
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
> AsRef<Span>
  for TypeExtension<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
  >
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
> IntoSpan<Span>
  for TypeExtension<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
  >
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Scalar(s) => s.into_span(),
      Self::Enum(e) => e.into_span(),
      Self::Union(u) => u.into_span(),
      Self::InputObject(i) => i.into_span(),
      Self::Object(o) => o.into_span(),
      Self::Interface(i) => i.into_span(),
    }
  }
}

impl<
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
>
  TypeExtension<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
  >
{
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Scalar(s) => s.span(),
      Self::Enum(e) => e.span(),
      Self::Union(u) => u.span(),
      Self::InputObject(i) => i.span(),
      Self::Object(o) => o.span(),
      Self::Interface(i) => i.span(),
    }
  }
}

impl<
  'a,
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  I,
  T,
  Error,
> Parseable<'a, I, T, Error>
  for TypeExtension<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
  >
where
  ScalarTypeExtension<S, ArgumentsContainer, DirectivesContainer>: Parseable<'a, I, T, Error>,
  ObjectTypeExtension<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >: Parseable<'a, I, T, Error>,
  InterfaceTypeExtension<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >: Parseable<'a, I, T, Error>,
  UnionTypeExtension<S, NamesContainer, ArgumentsContainer, DirectivesContainer>:
    Parseable<'a, I, T, Error>,
  EnumTypeExtension<S, ArgumentsContainer, DirectivesContainer, EnumValuesContainer>:
    Parseable<'a, I, T, Error>,
  InputObjectTypeExtension<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>:
    Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    choice((
      ScalarTypeExtension::parser::<E>().map(Self::Scalar),
      ObjectTypeExtension::parser::<E>().map(Self::Object),
      InterfaceTypeExtension::parser::<E>().map(Self::Interface),
      UnionTypeExtension::parser::<E>().map(Self::Union),
      EnumTypeExtension::parser::<E>().map(Self::Enum),
      InputObjectTypeExtension::parser::<E>().map(Self::InputObject),
    ))
  }
}

#[derive(Debug, Clone, IsVariant, From, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum TypeSystemExtension<
  S,
  NamesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  RootOperationTypesContainer = DefaultRootOperationTypesContainer<S>,
> {
  Type(
    TypeExtension<
      S,
      NamesContainer,
      ArgumentsContainer,
      DirectivesContainer,
      InputValuesContainer,
      EnumValuesContainer,
    >,
  ),
  Schema(SchemaExtension<S, ArgumentsContainer, DirectivesContainer, RootOperationTypesContainer>),
}

impl<
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  RootOperationTypesContainer,
> AsRef<Span>
  for TypeSystemExtension<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    RootOperationTypesContainer,
  >
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  RootOperationTypesContainer,
> IntoSpan<Span>
  for TypeSystemExtension<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    RootOperationTypesContainer,
  >
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Type(t) => t.into_span(),
      Self::Schema(s) => s.into_span(),
    }
  }
}

impl<
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  RootOperationTypesContainer,
>
  TypeSystemExtension<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    RootOperationTypesContainer,
  >
{
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Type(t) => t.span(),
      Self::Schema(s) => s.span(),
    }
  }
}

impl<
  'a,
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  RootOperationTypesContainer,
  I,
  T,
  Error,
> Parseable<'a, I, T, Error>
  for TypeSystemExtension<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    RootOperationTypesContainer,
  >
where
  TypeExtension<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
  >: Parseable<'a, I, T, Error>,
  SchemaExtension<S, ArgumentsContainer, DirectivesContainer, RootOperationTypesContainer>:
    Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    choice((
      TypeExtension::parser::<E>().map(Self::Type),
      SchemaExtension::parser::<E>().map(Self::Schema),
    ))
  }
}

#[derive(Debug, Clone, IsVariant, From, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum TypeSystemDefinitionOrExtension<
  S,
  NamesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  LocationsContainer = DefaultLocationsContainer,
  RootOperationTypesContainer = DefaultRootOperationTypesContainer<S>,
> {
  Definition(
    Described<
      TypeSystemDefinition<
        S,
        NamesContainer,
        ArgumentsContainer,
        DirectivesContainer,
        InputValuesContainer,
        EnumValuesContainer,
        LocationsContainer,
        RootOperationTypesContainer,
      >,
      StringValue<S>,
    >,
  ),
  Extension(
    TypeSystemExtension<
      S,
      NamesContainer,
      ArgumentsContainer,
      DirectivesContainer,
      InputValuesContainer,
      EnumValuesContainer,
      RootOperationTypesContainer,
    >,
  ),
}

impl<
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  LocationsContainer,
  RootOperationTypesContainer,
> AsRef<Span>
  for TypeSystemDefinitionOrExtension<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  LocationsContainer,
  RootOperationTypesContainer,
> IntoSpan<Span>
  for TypeSystemDefinitionOrExtension<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Definition(d) => d.into_span(),
      Self::Extension(e) => e.into_span(),
    }
  }
}

impl<
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  LocationsContainer,
  RootOperationTypesContainer,
>
  TypeSystemDefinitionOrExtension<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >
{
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Definition(d) => d.span(),
      Self::Extension(e) => e.span(),
    }
  }
}

impl<
  'a,
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  LocationsContainer,
  RootOperationTypesContainer,
  I,
  T,
  Error,
> Parseable<'a, I, T, Error>
  for TypeSystemDefinitionOrExtension<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >
where
  Described<
    TypeSystemDefinition<
      S,
      NamesContainer,
      ArgumentsContainer,
      DirectivesContainer,
      InputValuesContainer,
      EnumValuesContainer,
      LocationsContainer,
      RootOperationTypesContainer,
    >,
    StringValue<S>,
  >: Parseable<'a, I, T, Error>,
  TypeSystemExtension<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    RootOperationTypesContainer,
  >: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    choice((
      Described::parser::<E>().map(Self::Definition),
      TypeSystemExtension::parser::<E>().map(Self::Extension),
    ))
  }
}

#[derive(Debug, Clone, IsVariant, From, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum ExecutableDefinition<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> {
  Fragment(FragmentDefinition<S, ArgumentsContainer, DirectivesContainer>),
  Operation(OperationDefinition<S, ArgumentsContainer, DirectivesContainer>),
}

impl<S, ArgumentsContainer, DirectivesContainer> AsRef<Span>
  for ExecutableDefinition<S, ArgumentsContainer, DirectivesContainer>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<S, ArgumentsContainer, DirectivesContainer> IntoSpan<Span>
  for ExecutableDefinition<S, ArgumentsContainer, DirectivesContainer>
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Fragment(f) => f.into_span(),
      Self::Operation(o) => match o {
        OperationDefinition::Named(named) => named.into_span(),
        OperationDefinition::Shorthand(short) => short.into_span(),
      },
    }
  }
}

impl<S, ArgumentsContainer, DirectivesContainer>
  ExecutableDefinition<S, ArgumentsContainer, DirectivesContainer>
{
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Fragment(f) => f.span(),
      Self::Operation(o) => match o {
        OperationDefinition::Named(named) => named.span(),
        OperationDefinition::Shorthand(short) => short.span(),
      },
    }
  }
}

impl<'a, S, ArgumentsContainer, DirectivesContainer, I, T, Error> Parseable<'a, I, T, Error>
  for ExecutableDefinition<S, ArgumentsContainer, DirectivesContainer>
where
  FragmentDefinition<S, ArgumentsContainer, DirectivesContainer>: Parseable<'a, I, T, Error>,
  OperationDefinition<S, ArgumentsContainer, DirectivesContainer>: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    choice((
      FragmentDefinition::parser::<E>().map(Self::Fragment),
      OperationDefinition::parser::<E>().map(Self::Operation),
    ))
  }
}

#[derive(Debug, Clone, IsVariant, From, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum Definition<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
  ConstArgumentsContainer = DefaultConstArgumentsContainer<S>,
  ConstDirectivesContainer = DefaultConstDirectivesContainer<S, ConstArgumentsContainer>,
  NamesContainer = DefaultNamesContainer<S>,
  InputValuesContainer = DefaultInputValuesContainer<
    S,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
  >,
  EnumValuesContainer = DefaultEnumValuesContainer<
    S,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
  >,
  LocationsContainer = DefaultLocationsContainer,
  RootOperationTypesContainer = DefaultRootOperationTypesContainer<S>,
> {
  TypeSystem(
    TypeSystemDefinitionOrExtension<
      S,
      NamesContainer,
      ConstArgumentsContainer,
      ConstDirectivesContainer,
      InputValuesContainer,
      EnumValuesContainer,
      LocationsContainer,
      RootOperationTypesContainer,
    >,
  ),
  Executable(ExecutableDefinition<S, ArgumentsContainer, DirectivesContainer>),
}

impl<
  S,
  ArgumentsContainer,
  DirectivesContainer,
  ConstArgumentsContainer,
  ConstDirectivesContainer,
  NamesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  LocationsContainer,
  RootOperationTypesContainer,
> AsRef<Span>
  for Definition<
    S,
    ArgumentsContainer,
    DirectivesContainer,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
    NamesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<
  S,
  ArgumentsContainer,
  DirectivesContainer,
  ConstArgumentsContainer,
  ConstDirectivesContainer,
  NamesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  LocationsContainer,
  RootOperationTypesContainer,
> IntoSpan<Span>
  for Definition<
    S,
    ArgumentsContainer,
    DirectivesContainer,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
    NamesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::TypeSystem(t) => t.into_span(),
      Self::Executable(e) => e.into_span(),
    }
  }
}

impl<
  S,
  ArgumentsContainer,
  DirectivesContainer,
  ConstArgumentsContainer,
  ConstDirectivesContainer,
  NamesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  LocationsContainer,
  RootOperationTypesContainer,
>
  Definition<
    S,
    ArgumentsContainer,
    DirectivesContainer,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
    NamesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >
{
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::TypeSystem(t) => t.span(),
      Self::Executable(e) => e.span(),
    }
  }
}

impl<
  'a,
  S,
  ArgumentsContainer,
  DirectivesContainer,
  ConstArgumentsContainer,
  ConstDirectivesContainer,
  NamesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  LocationsContainer,
  RootOperationTypesContainer,
  I,
  T,
  Error,
> Parseable<'a, I, T, Error>
  for Definition<
    S,
    ArgumentsContainer,
    DirectivesContainer,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
    NamesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >
where
  TypeSystemDefinitionOrExtension<
    S,
    NamesContainer,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >: Parseable<'a, I, T, Error>,
  ExecutableDefinition<S, ArgumentsContainer, DirectivesContainer>: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    choice((
      TypeSystemDefinitionOrExtension::parser::<E>().map(Self::TypeSystem),
      ExecutableDefinition::parser::<E>().map(Self::Executable),
    ))
  }
}

#[derive(Debug, Clone)]
pub struct TypeSystemDocument<
  S,
  NamesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  LocationsContainer = DefaultLocationsContainer,
  RootOperationTypesContainer = DefaultRootOperationTypesContainer<S>,
  DefinitionContainer = DefaultVec<
    TypeSystemDefinitionOrExtension<
      S,
      NamesContainer,
      ArgumentsContainer,
      DirectivesContainer,
      InputValuesContainer,
      EnumValuesContainer,
      LocationsContainer,
      RootOperationTypesContainer,
    >,
  >,
> {
  span: Span,
  definitions: DefinitionContainer,
  _m: PhantomData<
    TypeSystemDefinitionOrExtension<
      S,
      NamesContainer,
      ArgumentsContainer,
      DirectivesContainer,
      InputValuesContainer,
      EnumValuesContainer,
      LocationsContainer,
      RootOperationTypesContainer,
    >,
  >,
}

impl<
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  LocationsContainer,
  RootOperationTypesContainer,
  DefinitionContainer,
>
  TypeSystemDocument<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
    DefinitionContainer,
  >
{
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  #[inline]
  pub const fn definitions(&self) -> &DefinitionContainer {
    &self.definitions
  }
}

impl<
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  LocationsContainer,
  RootOperationTypesContainer,
  DefinitionContainer,
> AsRef<Span>
  for TypeSystemDocument<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
    DefinitionContainer,
  >
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<
  'a,
  S,
  NamesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  LocationsContainer,
  RootOperationTypesContainer,
  DefinitionContainer,
  I,
  T,
  Error,
> Parseable<'a, I, T, Error>
  for TypeSystemDocument<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
    DefinitionContainer,
  >
where
  DefinitionContainer: ChumskyContainer<
    TypeSystemDefinitionOrExtension<
      S,
      NamesContainer,
      ArgumentsContainer,
      DirectivesContainer,
      InputValuesContainer,
      EnumValuesContainer,
      LocationsContainer,
      RootOperationTypesContainer,
    >,
  >,
  TypeSystemDefinitionOrExtension<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    TypeSystemDefinitionOrExtension::parser::<E>()
      .repeated()
      .at_least(1)
      .collect()
      .map_with(|definitions, exa| Self {
        span: exa.span(),
        definitions,
        _m: PhantomData,
      })
  }
}

#[derive(Debug, Clone)]
pub struct ExecutableDocument<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
  DefinitionContainer = DefaultVec<
    ExecutableDefinition<S, ArgumentsContainer, DirectivesContainer>,
  >,
> {
  span: Span,
  definitions: DefinitionContainer,
  _m: PhantomData<ExecutableDefinition<S, ArgumentsContainer, DirectivesContainer>>,
}

impl<S, ArgumentsContainer, DirectivesContainer, DefinitionContainer>
  ExecutableDocument<S, ArgumentsContainer, DirectivesContainer, DefinitionContainer>
{
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  #[inline]
  pub const fn definitions(&self) -> &DefinitionContainer {
    &self.definitions
  }
}

impl<S, ArgumentsContainer, DirectivesContainer, DefinitionContainer> AsRef<Span>
  for ExecutableDocument<S, ArgumentsContainer, DirectivesContainer, DefinitionContainer>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<'a, S, ArgumentsContainer, DirectivesContainer, DefinitionContainer, I, T, Error>
  Parseable<'a, I, T, Error>
  for ExecutableDocument<S, ArgumentsContainer, DirectivesContainer, DefinitionContainer>
where
  DefinitionContainer:
    ChumskyContainer<ExecutableDefinition<S, ArgumentsContainer, DirectivesContainer>>,
  ExecutableDefinition<S, ArgumentsContainer, DirectivesContainer>: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    ExecutableDefinition::parser::<E>()
      .repeated()
      .at_least(1)
      .collect()
      .map_with(|definitions, exa| Self {
        span: exa.span(),
        definitions,
        _m: PhantomData,
      })
  }
}

#[derive(Debug, Clone)]
pub struct Document<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
  ConstArgumentsContainer = DefaultConstArgumentsContainer<S>,
  ConstDirectivesContainer = DefaultConstDirectivesContainer<S, ConstArgumentsContainer>,
  NamesContainer = DefaultNamesContainer<S>,
  InputValuesContainer = DefaultInputValuesContainer<
    S,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
  >,
  EnumValuesContainer = DefaultEnumValuesContainer<
    S,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
  >,
  LocationsContainer = DefaultLocationsContainer,
  RootOperationTypesContainer = DefaultRootOperationTypesContainer<S>,
  DefinitionContainer = DefaultVec<
    Definition<
      S,
      ArgumentsContainer,
      DirectivesContainer,
      ConstArgumentsContainer,
      ConstDirectivesContainer,
      NamesContainer,
      InputValuesContainer,
      EnumValuesContainer,
      LocationsContainer,
      RootOperationTypesContainer,
    >,
  >,
> {
  span: Span,
  definitions: DefinitionContainer,
  _m: PhantomData<
    Definition<
      S,
      ArgumentsContainer,
      DirectivesContainer,
      ConstArgumentsContainer,
      ConstDirectivesContainer,
      NamesContainer,
      InputValuesContainer,
      EnumValuesContainer,
      LocationsContainer,
      RootOperationTypesContainer,
    >,
  >,
}

impl<
  S,
  ArgumentsContainer,
  DirectivesContainer,
  ConstArgumentsContainer,
  ConstDirectivesContainer,
  NamesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  LocationsContainer,
  RootOperationTypesContainer,
  DefinitionContainer,
>
  Document<
    S,
    ArgumentsContainer,
    DirectivesContainer,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
    NamesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
    DefinitionContainer,
  >
{
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }
  #[inline]
  pub const fn definitions(&self) -> &DefinitionContainer {
    &self.definitions
  }
}

impl<
  S,
  ArgumentsContainer,
  DirectivesContainer,
  ConstArgumentsContainer,
  ConstDirectivesContainer,
  NamesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  LocationsContainer,
  RootOperationTypesContainer,
  DefinitionContainer,
> AsRef<Span>
  for Document<
    S,
    ArgumentsContainer,
    DirectivesContainer,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
    NamesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
    DefinitionContainer,
  >
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<
  'a,
  S,
  ArgumentsContainer,
  DirectivesContainer,
  ConstArgumentsContainer,
  ConstDirectivesContainer,
  NamesContainer,
  InputValuesContainer,
  EnumValuesContainer,
  LocationsContainer,
  RootOperationTypesContainer,
  DefinitionContainer,
  I,
  T,
  Error,
> Parseable<'a, I, T, Error>
  for Document<
    S,
    ArgumentsContainer,
    DirectivesContainer,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
    NamesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
    DefinitionContainer,
  >
where
  DefinitionContainer: ChumskyContainer<
    Definition<
      S,
      ArgumentsContainer,
      DirectivesContainer,
      ConstArgumentsContainer,
      ConstDirectivesContainer,
      NamesContainer,
      InputValuesContainer,
      EnumValuesContainer,
      LocationsContainer,
      RootOperationTypesContainer,
    >,
  >,
  Definition<
    S,
    ArgumentsContainer,
    DirectivesContainer,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
    NamesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Definition::parser::<E>()
      .repeated()
      .at_least(1)
      .collect()
      .map_with(|definitions, exa| Self {
        span: exa.span(),
        definitions,
        _m: PhantomData,
      })
  }
}
