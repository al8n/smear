//! Shared GraphQL-family type-system AST carriers.
//!
//! These data-only nodes model the SDL grammar independently of a concrete
//! dialect. GraphQL and GraphQLx bind their own name, type, value, and directive
//! nodes through aliases in their AST modules.
//!
//! The scalar, enum, union and input object carriers are GraphQL's alone and compile only with
//! `graphql`: GraphQLx's four kinds take an `implements` clause and bind carriers of their own from
//! `crate::generic`.

use core::marker::PhantomData;

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use std::vec::Vec;

use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

#[cfg(test)]
mod tests;

macro_rules! impl_node_traits {
  (
    $node:ident<$($generic:ident),+>,
    ($($component:ty),+),
    ($($field:ident),+),
    [$($used:ident),+]
  ) => {
    impl<$($generic),+> AsSpan<Span> for $node<$($generic),+> {
      #[inline]
      fn as_span(&self) -> &Span {
        self.span()
      }
    }

    impl<$($generic),+> IntoSpan<Span> for $node<$($generic),+> {
      #[inline]
      fn into_span(self) -> Span {
        self.span
      }
    }

    paste::paste! {
      #[doc = concat!("`", stringify!($node), "` taken apart: every field, in the order it is written.")]
      #[derive(Debug, Clone, Copy, PartialEq, Eq)]
      pub struct [<$node Components>]<$($used),+> {
        $(
          #[doc = concat!("The node's `", stringify!($field), "`.")]
          pub $field: $component,
        )+
      }

      impl<$($generic),+> IntoComponents for $node<$($generic),+> {
        type Components = [<$node Components>]<$($used),+>;

        #[inline]
        fn into_components(self) -> Self::Components {
          [<$node Components>] { $($field: self.$field,)+ }
        }
      }
    }
  };
}

// GraphQL's own carriers — see the module docs. Declared after `impl_node_traits!`, which they
// expand.
#[cfg(feature = "graphql")]
mod graphql;
#[cfg(feature = "graphql")]
pub use graphql::*;

/// A definition of the arguments accepted by a field or directive.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct ArgumentsDefinition<InputValue, Container = Vec<InputValue>, Span = SimpleSpan> {
  span: Span,
  input_value_definitions: Container,
  _input_value: PhantomData<InputValue>,
}

impl_node_traits!(
  ArgumentsDefinition<InputValue, Container, Span>,
  (Span, Container),
  (span, input_value_definitions),
  [Container, Span]
);

impl<InputValue, Container, Span> ArgumentsDefinition<InputValue, Container, Span> {
  /// Creates an arguments definition from its complete delimiter span and values.
  #[inline]
  pub const fn new(span: Span, input_value_definitions: Container) -> Self {
    Self {
      span,
      input_value_definitions,
      _input_value: PhantomData,
    }
  }

  /// Returns the span including the surrounding parentheses.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the contained input-value definitions.
  #[inline]
  pub fn input_value_definitions(&self) -> &[InputValue]
  where
    Container: AsRef<[InputValue]>,
  {
    self.input_value_definitions.as_ref()
  }

  /// Consumes this definition and returns its input-value definitions.
  #[inline]
  pub fn into_input_value_definitions(self) -> Container {
    self.input_value_definitions
  }
}

/// An input value definition used by a field, directive, or input object.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct InputValueDefinition<Name, Type, DefaultValue, Directives, Span = SimpleSpan> {
  span: Span,
  name: Name,
  ty: Type,
  default_value: Option<DefaultValue>,
  directives: Option<Directives>,
}

impl_node_traits!(
  InputValueDefinition<Name, Type, DefaultValue, Directives, Span>,
  (Span, Name, Type, Option<DefaultValue>, Option<Directives>),
  (span, name, ty, default_value, directives),
  [Name, Type, DefaultValue, Directives, Span]
);

impl<Name, Type, DefaultValue, Directives, Span>
  InputValueDefinition<Name, Type, DefaultValue, Directives, Span>
{
  /// Creates an input value definition in source order.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    ty: Type,
    default_value: Option<DefaultValue>,
    directives: Option<Directives>,
  ) -> Self {
    Self {
      span,
      name,
      ty,
      default_value,
      directives,
    }
  }

  /// Returns the complete definition span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the input value name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the declared type.
  #[inline]
  pub const fn ty(&self) -> &Type {
    &self.ty
  }

  /// Returns the optional default value.
  #[inline]
  pub const fn default_value(&self) -> Option<&DefaultValue> {
    self.default_value.as_ref()
  }

  /// Returns the optional directive collection.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }
}

/// A braced collection of input value definitions.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct InputFieldsDefinition<InputValue, Container = Vec<InputValue>, Span = SimpleSpan> {
  span: Span,
  input_value_definitions: Container,
  _input_value: PhantomData<InputValue>,
}

impl_node_traits!(
  InputFieldsDefinition<InputValue, Container, Span>,
  (Span, Container),
  (span, input_value_definitions),
  [Container, Span]
);

impl<InputValue, Container, Span> InputFieldsDefinition<InputValue, Container, Span> {
  /// Creates an input-fields definition from its complete delimiter span and values.
  #[inline]
  pub const fn new(span: Span, input_value_definitions: Container) -> Self {
    Self {
      span,
      input_value_definitions,
      _input_value: PhantomData,
    }
  }

  /// Returns the span including the surrounding braces.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the contained input-value definitions.
  #[inline]
  pub fn input_value_definitions(&self) -> &[InputValue]
  where
    Container: AsRef<[InputValue]>,
  {
    self.input_value_definitions.as_ref()
  }

  /// Consumes this definition and returns its input-value definitions.
  #[inline]
  pub fn into_input_value_definitions(self) -> Container {
    self.input_value_definitions
  }
}

/// A field definition in an object or interface type.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct FieldDefinition<Name, Arguments, Type, Directives, Span = SimpleSpan> {
  span: Span,
  name: Name,
  arguments_definition: Option<Arguments>,
  ty: Type,
  directives: Option<Directives>,
}

impl_node_traits!(
  FieldDefinition<Name, Arguments, Type, Directives, Span>,
  (Span, Name, Option<Arguments>, Type, Option<Directives>),
  (span, name, arguments_definition, ty, directives),
  [Name, Arguments, Type, Directives, Span]
);

impl<Name, Arguments, Type, Directives, Span>
  FieldDefinition<Name, Arguments, Type, Directives, Span>
{
  /// Creates a field definition in source order.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    arguments_definition: Option<Arguments>,
    ty: Type,
    directives: Option<Directives>,
  ) -> Self {
    Self {
      span,
      name,
      arguments_definition,
      ty,
      directives,
    }
  }

  /// Returns the complete definition span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the field name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the optional arguments definition.
  #[inline]
  pub const fn arguments_definition(&self) -> Option<&Arguments> {
    self.arguments_definition.as_ref()
  }

  /// Returns the field type.
  #[inline]
  pub const fn ty(&self) -> &Type {
    &self.ty
  }

  /// Returns the optional directive collection.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }
}

/// A braced collection of field definitions.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct FieldsDefinition<Field, Container = Vec<Field>, Span = SimpleSpan> {
  span: Span,
  field_definitions: Container,
  _field: PhantomData<Field>,
}

impl_node_traits!(
  FieldsDefinition<Field, Container, Span>,
  (Span, Container),
  (span, field_definitions),
  [Container, Span]
);

impl<Field, Container, Span> FieldsDefinition<Field, Container, Span> {
  /// Creates a fields definition from its complete delimiter span and fields.
  #[inline]
  pub const fn new(span: Span, field_definitions: Container) -> Self {
    Self {
      span,
      field_definitions,
      _field: PhantomData,
    }
  }

  /// Returns the span including the surrounding braces.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the contained field definitions.
  #[inline]
  pub fn field_definitions(&self) -> &[Field]
  where
    Container: AsRef<[Field]>,
  {
    self.field_definitions.as_ref()
  }

  /// Consumes this definition and returns its fields.
  #[inline]
  pub fn into_field_definitions(self) -> Container {
    self.field_definitions
  }
}

/// A list of interfaces implemented by an object or interface type.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct ImplementInterfaces<Name, Container = Vec<Name>, Span = SimpleSpan> {
  span: Span,
  interfaces: Container,
  _name: PhantomData<Name>,
}

impl_node_traits!(
  ImplementInterfaces<Name, Container, Span>,
  (Span, Container),
  (span, interfaces),
  [Container, Span]
);

impl<Name, Container, Span> ImplementInterfaces<Name, Container, Span> {
  /// Creates an implements clause from its complete span and interfaces.
  #[inline]
  pub const fn new(span: Span, interfaces: Container) -> Self {
    Self {
      span,
      interfaces,
      _name: PhantomData,
    }
  }

  /// Returns the complete clause span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the implemented interfaces.
  #[inline]
  pub fn interfaces(&self) -> &[Name]
  where
    Container: AsRef<[Name]>,
  {
    self.interfaces.as_ref()
  }

  /// Consumes this clause and returns its interfaces.
  #[inline]
  pub fn into_interfaces(self) -> Container {
    self.interfaces
  }
}

/// An interface type definition.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct InterfaceTypeDefinition<Name, Implements, Directives, Fields, Span = SimpleSpan> {
  span: Span,
  name: Name,
  implements: Option<Implements>,
  directives: Option<Directives>,
  fields_definition: Option<Fields>,
}

impl_node_traits!(
  InterfaceTypeDefinition<Name, Implements, Directives, Fields, Span>,
  (Span, Name, Option<Implements>, Option<Directives>, Option<Fields>),
  (span, name, implements, directives, fields_definition),
  [Name, Implements, Directives, Fields, Span]
);

impl<Name, Implements, Directives, Fields, Span>
  InterfaceTypeDefinition<Name, Implements, Directives, Fields, Span>
{
  /// Creates an interface type definition in source order.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    implements: Option<Implements>,
    directives: Option<Directives>,
    fields_definition: Option<Fields>,
  ) -> Self {
    Self {
      span,
      name,
      implements,
      directives,
      fields_definition,
    }
  }

  /// Returns the complete definition span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the interface name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the optional implements clause.
  #[inline]
  pub const fn implements(&self) -> Option<&Implements> {
    self.implements.as_ref()
  }

  /// Returns the optional directive collection.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns the optional fields definition.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&Fields> {
    self.fields_definition.as_ref()
  }
}

/// A root operation type definition in a schema definition.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct RootOperationTypeDefinition<Name, OperationType, Span = SimpleSpan> {
  span: Span,
  operation_type: OperationType,
  name: Name,
}

impl_node_traits!(
  RootOperationTypeDefinition<Name, OperationType, Span>,
  (Span, OperationType, Name),
  (span, operation_type, name),
  [Name, OperationType, Span]
);

impl<Name, OperationType, Span> RootOperationTypeDefinition<Name, OperationType, Span> {
  /// Creates a root operation type definition in source order.
  #[inline]
  pub const fn new(span: Span, operation_type: OperationType, name: Name) -> Self {
    Self {
      span,
      operation_type,
      name,
    }
  }

  /// Returns the complete definition span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the operation type.
  #[inline]
  pub const fn operation_type(&self) -> &OperationType {
    &self.operation_type
  }

  /// Returns the named root type.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }
}

/// A braced collection of root operation type definitions.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct RootOperationTypesDefinition<
  RootOperation,
  Container = Vec<RootOperation>,
  Span = SimpleSpan,
> {
  span: Span,
  root_operation_type_definitions: Container,
  _root_operation: PhantomData<RootOperation>,
}

impl_node_traits!(
  RootOperationTypesDefinition<RootOperation, Container, Span>,
  (Span, Container),
  (span, root_operation_type_definitions),
  [Container, Span]
);

impl<RootOperation, Container, Span> RootOperationTypesDefinition<RootOperation, Container, Span> {
  /// Creates a root-operation-types definition from its complete span and operations.
  #[inline]
  pub const fn new(span: Span, root_operation_type_definitions: Container) -> Self {
    Self {
      span,
      root_operation_type_definitions,
      _root_operation: PhantomData,
    }
  }

  /// Returns the span including the surrounding braces.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the contained root operation type definitions.
  #[inline]
  pub fn root_operation_type_definitions(&self) -> &[RootOperation]
  where
    Container: AsRef<[RootOperation]>,
  {
    self.root_operation_type_definitions.as_ref()
  }

  /// Consumes this definition and returns its root operation type definitions.
  #[inline]
  pub fn into_root_operation_type_definitions(self) -> Container {
    self.root_operation_type_definitions
  }
}

/// An enum value definition.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct EnumValueDefinition<EnumValue, Directives, Span = SimpleSpan> {
  span: Span,
  value: EnumValue,
  directives: Option<Directives>,
}

impl_node_traits!(
  EnumValueDefinition<EnumValue, Directives, Span>,
  (Span, EnumValue, Option<Directives>),
  (span, value, directives),
  [EnumValue, Directives, Span]
);

impl<EnumValue, Directives, Span> EnumValueDefinition<EnumValue, Directives, Span> {
  /// Creates an enum value definition in source order.
  #[inline]
  pub const fn new(span: Span, value: EnumValue, directives: Option<Directives>) -> Self {
    Self {
      span,
      value,
      directives,
    }
  }

  /// Returns the complete definition span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the enum value.
  #[inline]
  pub const fn value(&self) -> &EnumValue {
    &self.value
  }

  /// Returns the optional directive collection.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }
}

/// A braced collection of enum value definitions.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct EnumValuesDefinition<EnumValue, Container = Vec<EnumValue>, Span = SimpleSpan> {
  span: Span,
  enum_value_definitions: Container,
  _enum_value: PhantomData<EnumValue>,
}

impl_node_traits!(
  EnumValuesDefinition<EnumValue, Container, Span>,
  (Span, Container),
  (span, enum_value_definitions),
  [Container, Span]
);

impl<EnumValue, Container, Span> EnumValuesDefinition<EnumValue, Container, Span> {
  /// Creates an enum-values definition from its complete delimiter span and values.
  #[inline]
  pub const fn new(span: Span, enum_value_definitions: Container) -> Self {
    Self {
      span,
      enum_value_definitions,
      _enum_value: PhantomData,
    }
  }

  /// Returns the span including the surrounding braces.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the contained enum value definitions.
  #[inline]
  pub fn enum_value_definitions(&self) -> &[EnumValue]
  where
    Container: AsRef<[EnumValue]>,
  {
    self.enum_value_definitions.as_ref()
  }

  /// Consumes this definition and returns its enum value definitions.
  #[inline]
  pub fn into_enum_value_definitions(self) -> Container {
    self.enum_value_definitions
  }
}

/// An object type definition.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct ObjectTypeDefinition<Name, Implements, Directives, Fields, Span = SimpleSpan> {
  span: Span,
  name: Name,
  implements: Option<Implements>,
  directives: Option<Directives>,
  fields_definition: Option<Fields>,
}

impl_node_traits!(
  ObjectTypeDefinition<Name, Implements, Directives, Fields, Span>,
  (Span, Name, Option<Implements>, Option<Directives>, Option<Fields>),
  (span, name, implements, directives, fields_definition),
  [Name, Implements, Directives, Fields, Span]
);

impl<Name, Implements, Directives, Fields, Span>
  ObjectTypeDefinition<Name, Implements, Directives, Fields, Span>
{
  /// Creates an object type definition in source order.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    implements: Option<Implements>,
    directives: Option<Directives>,
    fields_definition: Option<Fields>,
  ) -> Self {
    Self {
      span,
      name,
      implements,
      directives,
      fields_definition,
    }
  }

  /// Returns the complete definition span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the object type name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the optional implements clause.
  #[inline]
  pub const fn implements(&self) -> Option<&Implements> {
    self.implements.as_ref()
  }

  /// Returns the optional directive collection.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns the optional fields definition.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&Fields> {
    self.fields_definition.as_ref()
  }
}

/// A list of named union member types.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct UnionMemberTypes<Name, Container = Vec<Name>, Span = SimpleSpan> {
  span: Span,
  members: Container,
  _name: PhantomData<Name>,
}

impl_node_traits!(
  UnionMemberTypes<Name, Container, Span>,
  (Span, Container),
  (span, members),
  [Container, Span]
);

impl<Name, Container, Span> UnionMemberTypes<Name, Container, Span> {
  /// Creates a union-members clause from its complete span and member types.
  #[inline]
  pub const fn new(span: Span, members: Container) -> Self {
    Self {
      span,
      members,
      _name: PhantomData,
    }
  }

  /// Returns the complete clause span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the union member types.
  #[inline]
  pub fn members(&self) -> &[Name]
  where
    Container: AsRef<[Name]>,
  {
    self.members.as_ref()
  }

  /// Consumes this clause and returns its member types.
  #[inline]
  pub fn into_members(self) -> Container {
    self.members
  }
}

/// A location where an executable directive can be applied.
///
/// # Not a map key reachable through `&str`
///
/// Every variant carries a span and this type derives `Eq` and `Hash` over it, while
/// [`as_str`](Self::as_str) answers a per-variant **constant**. A `Borrow<str>` impl would
/// promise those two agree, and they do not in either direction: two `QUERY` locations read at
/// different offsets borrow *equal* while comparing *unequal*, and they hash differently besides.
/// There was such an impl; it is gone, and the map below does not compile:
///
/// ```compile_fail,E0308
/// use std::collections::HashMap;
/// use smear_parser::graphql::ast::ExecutableDirectiveLocation;
///
/// let map: HashMap<ExecutableDirectiveLocation, ()> = HashMap::new();
/// let _ = map.get("QUERY");
/// ```
///
/// Narrowing `Eq` and `Hash` to the discriminant would make it true and is a decision about what
/// a *location* is, not about `Borrow`: it silently drops the span from equality, so every rule
/// that compares two locations — draft §5's duplicate-location checks among them — changes answer
/// with nothing to announce it. `Ord` is not derived here, so unlike the string literal carriers
/// there is no sort order to lose; the violation is `Eq` and `Hash` alone. Use
/// [`as_str`](Self::as_str) or `AsRef<str>` and key a `HashMap<&str, _>` on that.
///
/// Per this repository's convention the error code is checked only under a nightly
/// `cargo test --doc`; on stable the assertion is that the snippet does not compile at all.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ExecutableDirectiveLocation<Span = SimpleSpan> {
  /// `QUERY`.
  Query(Span),
  /// `MUTATION`.
  Mutation(Span),
  /// `SUBSCRIPTION`.
  Subscription(Span),
  /// `FIELD`.
  Field(Span),
  /// `FRAGMENT_DEFINITION`.
  FragmentDefinition(Span),
  /// `FRAGMENT_SPREAD`.
  FragmentSpread(Span),
  /// `INLINE_FRAGMENT`.
  InlineFragment(Span),
  /// `VARIABLE_DEFINITION`.
  VariableDefinition(Span),
}

impl<Span> ExecutableDirectiveLocation<Span> {
  /// Creates the `QUERY` location.
  #[inline]
  pub const fn query(span: Span) -> Self {
    Self::Query(span)
  }
  /// Creates the `MUTATION` location.
  #[inline]
  pub const fn mutation(span: Span) -> Self {
    Self::Mutation(span)
  }
  /// Creates the `SUBSCRIPTION` location.
  #[inline]
  pub const fn subscription(span: Span) -> Self {
    Self::Subscription(span)
  }
  /// Creates the `FIELD` location.
  #[inline]
  pub const fn field(span: Span) -> Self {
    Self::Field(span)
  }
  /// Creates the `FRAGMENT_DEFINITION` location.
  #[inline]
  pub const fn fragment_definition(span: Span) -> Self {
    Self::FragmentDefinition(span)
  }
  /// Creates the `FRAGMENT_SPREAD` location.
  #[inline]
  pub const fn fragment_spread(span: Span) -> Self {
    Self::FragmentSpread(span)
  }
  /// Creates the `INLINE_FRAGMENT` location.
  #[inline]
  pub const fn inline_fragment(span: Span) -> Self {
    Self::InlineFragment(span)
  }
  /// Creates the `VARIABLE_DEFINITION` location.
  #[inline]
  pub const fn variable_definition(span: Span) -> Self {
    Self::VariableDefinition(span)
  }

  /// Returns the location span.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Query(span)
      | Self::Mutation(span)
      | Self::Subscription(span)
      | Self::Field(span)
      | Self::FragmentDefinition(span)
      | Self::FragmentSpread(span)
      | Self::InlineFragment(span)
      | Self::VariableDefinition(span) => span,
    }
  }

  /// Returns the canonical GraphQL spelling.
  #[inline]
  pub const fn as_str(&self) -> &'static str {
    match self {
      Self::Query(_) => "QUERY",
      Self::Mutation(_) => "MUTATION",
      Self::Subscription(_) => "SUBSCRIPTION",
      Self::Field(_) => "FIELD",
      Self::FragmentDefinition(_) => "FRAGMENT_DEFINITION",
      Self::FragmentSpread(_) => "FRAGMENT_SPREAD",
      Self::InlineFragment(_) => "INLINE_FRAGMENT",
      Self::VariableDefinition(_) => "VARIABLE_DEFINITION",
    }
  }
}

impl<Span> AsSpan<Span> for ExecutableDirectiveLocation<Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for ExecutableDirectiveLocation<Span> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Query(span)
      | Self::Mutation(span)
      | Self::Subscription(span)
      | Self::Field(span)
      | Self::FragmentDefinition(span)
      | Self::FragmentSpread(span)
      | Self::InlineFragment(span)
      | Self::VariableDefinition(span) => span,
    }
  }
}

/// `ExecutableDirectiveLocation` taken apart: the span it was written over and its spelling.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExecutableDirectiveLocationComponents<Span> {
  /// The span the location was written over.
  pub span: Span,
  /// The location's spelling in a directive definition, such as `QUERY`.
  pub spelling: &'static str,
}

impl<Span> IntoComponents for ExecutableDirectiveLocation<Span> {
  type Components = ExecutableDirectiveLocationComponents<Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    let spelling = self.as_str();
    ExecutableDirectiveLocationComponents {
      span: self.into_span(),
      spelling,
    }
  }
}

/// A location where a type-system directive can be applied.
///
/// # Not a map key reachable through `&str`
///
/// The same span-against-constant mismatch as
/// [`ExecutableDirectiveLocation`](ExecutableDirectiveLocation#not-a-map-key-reachable-through-str),
/// and the same removed impl:
///
/// ```compile_fail,E0308
/// use std::collections::HashMap;
/// use smear_parser::graphql::ast::TypeSystemDirectiveLocation;
///
/// let map: HashMap<TypeSystemDirectiveLocation, ()> = HashMap::new();
/// let _ = map.get("SCHEMA");
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeSystemDirectiveLocation<Span = SimpleSpan> {
  /// `SCHEMA`.
  Schema(Span),
  /// `SCALAR`.
  Scalar(Span),
  /// `OBJECT`.
  Object(Span),
  /// `FIELD_DEFINITION`.
  FieldDefinition(Span),
  /// `ARGUMENT_DEFINITION`.
  ArgumentDefinition(Span),
  /// `INTERFACE`.
  Interface(Span),
  /// `UNION`.
  Union(Span),
  /// `ENUM`.
  Enum(Span),
  /// `ENUM_VALUE`.
  EnumValue(Span),
  /// `INPUT_OBJECT`.
  InputObject(Span),
  /// `INPUT_FIELD_DEFINITION`.
  InputFieldDefinition(Span),
  /// `DIRECTIVE_DEFINITION`.
  DirectiveDefinition(Span),
}

impl<Span> TypeSystemDirectiveLocation<Span> {
  /// Creates the `SCHEMA` location.
  #[inline]
  pub const fn schema(span: Span) -> Self {
    Self::Schema(span)
  }
  /// Creates the `SCALAR` location.
  #[inline]
  pub const fn scalar(span: Span) -> Self {
    Self::Scalar(span)
  }
  /// Creates the `OBJECT` location.
  #[inline]
  pub const fn object(span: Span) -> Self {
    Self::Object(span)
  }
  /// Creates the `FIELD_DEFINITION` location.
  #[inline]
  pub const fn field_definition(span: Span) -> Self {
    Self::FieldDefinition(span)
  }
  /// Creates the `ARGUMENT_DEFINITION` location.
  #[inline]
  pub const fn argument_definition(span: Span) -> Self {
    Self::ArgumentDefinition(span)
  }
  /// Creates the `INTERFACE` location.
  #[inline]
  pub const fn interface(span: Span) -> Self {
    Self::Interface(span)
  }
  /// Creates the `UNION` location.
  #[inline]
  pub const fn union(span: Span) -> Self {
    Self::Union(span)
  }
  /// Creates the `ENUM` location.
  #[inline]
  pub const fn r#enum(span: Span) -> Self {
    Self::Enum(span)
  }
  /// Creates the `ENUM_VALUE` location.
  #[inline]
  pub const fn enum_value(span: Span) -> Self {
    Self::EnumValue(span)
  }
  /// Creates the `INPUT_OBJECT` location.
  #[inline]
  pub const fn input_object(span: Span) -> Self {
    Self::InputObject(span)
  }
  /// Creates the `INPUT_FIELD_DEFINITION` location.
  #[inline]
  pub const fn input_field_definition(span: Span) -> Self {
    Self::InputFieldDefinition(span)
  }
  /// Creates the `DIRECTIVE_DEFINITION` location.
  #[inline]
  pub const fn directive_definition(span: Span) -> Self {
    Self::DirectiveDefinition(span)
  }

  /// Returns the location span.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Schema(span)
      | Self::Scalar(span)
      | Self::Object(span)
      | Self::FieldDefinition(span)
      | Self::ArgumentDefinition(span)
      | Self::Interface(span)
      | Self::Union(span)
      | Self::Enum(span)
      | Self::EnumValue(span)
      | Self::InputObject(span)
      | Self::InputFieldDefinition(span)
      | Self::DirectiveDefinition(span) => span,
    }
  }

  /// Returns the canonical GraphQL spelling.
  #[inline]
  pub const fn as_str(&self) -> &'static str {
    match self {
      Self::Schema(_) => "SCHEMA",
      Self::Scalar(_) => "SCALAR",
      Self::Object(_) => "OBJECT",
      Self::FieldDefinition(_) => "FIELD_DEFINITION",
      Self::ArgumentDefinition(_) => "ARGUMENT_DEFINITION",
      Self::Interface(_) => "INTERFACE",
      Self::Union(_) => "UNION",
      Self::Enum(_) => "ENUM",
      Self::EnumValue(_) => "ENUM_VALUE",
      Self::InputObject(_) => "INPUT_OBJECT",
      Self::InputFieldDefinition(_) => "INPUT_FIELD_DEFINITION",
      Self::DirectiveDefinition(_) => "DIRECTIVE_DEFINITION",
    }
  }
}

impl<Span> AsSpan<Span> for TypeSystemDirectiveLocation<Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for TypeSystemDirectiveLocation<Span> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Schema(span)
      | Self::Scalar(span)
      | Self::Object(span)
      | Self::FieldDefinition(span)
      | Self::ArgumentDefinition(span)
      | Self::Interface(span)
      | Self::Union(span)
      | Self::Enum(span)
      | Self::EnumValue(span)
      | Self::InputObject(span)
      | Self::InputFieldDefinition(span)
      | Self::DirectiveDefinition(span) => span,
    }
  }
}

/// `TypeSystemDirectiveLocation` taken apart: the span it was written over and its spelling.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeSystemDirectiveLocationComponents<Span> {
  /// The span the location was written over.
  pub span: Span,
  /// The location's spelling in a directive definition, such as `SCHEMA`.
  pub spelling: &'static str,
}

impl<Span> IntoComponents for TypeSystemDirectiveLocation<Span> {
  type Components = TypeSystemDirectiveLocationComponents<Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    let spelling = self.as_str();
    TypeSystemDirectiveLocationComponents {
      span: self.into_span(),
      spelling,
    }
  }
}

/// A directive location in either executable or type-system syntax.
///
/// # Not a map key reachable through `&str`
///
/// It wraps the two enums above and inherits their mismatch — a discriminant *and* a span against
/// a per-variant constant — so it carried the same removed impl:
///
/// ```compile_fail,E0308
/// use std::collections::HashMap;
/// use smear_parser::graphql::ast::Location;
///
/// let map: HashMap<Location, ()> = HashMap::new();
/// let _ = map.get("QUERY");
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Location<Span = SimpleSpan> {
  /// An executable directive location.
  Executable(ExecutableDirectiveLocation<Span>),
  /// A type-system directive location.
  TypeSystem(TypeSystemDirectiveLocation<Span>),
}

impl<Span> Location<Span> {
  /// Returns the location span.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Executable(location) => location.span(),
      Self::TypeSystem(location) => location.span(),
    }
  }

  /// Returns the canonical GraphQL spelling.
  #[inline]
  pub const fn as_str(&self) -> &'static str {
    match self {
      Self::Executable(location) => location.as_str(),
      Self::TypeSystem(location) => location.as_str(),
    }
  }
}

impl<Span> AsSpan<Span> for Location<Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for Location<Span> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Executable(location) => location.into_span(),
      Self::TypeSystem(location) => location.into_span(),
    }
  }
}

/// `Location` taken apart: the span it was written over and its spelling.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocationComponents<Span> {
  /// The span the location was written over.
  pub span: Span,
  /// The location's spelling in a directive definition, such as `QUERY` or `SCHEMA`.
  pub spelling: &'static str,
}

impl<Span> IntoComponents for Location<Span> {
  type Components = LocationComponents<Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    match self {
      Self::Executable(location) => {
        let ExecutableDirectiveLocationComponents { span, spelling } = location.into_components();
        LocationComponents { span, spelling }
      }
      Self::TypeSystem(location) => {
        let TypeSystemDirectiveLocationComponents { span, spelling } = location.into_components();
        LocationComponents { span, spelling }
      }
    }
  }
}

impl<Span> AsRef<str> for ExecutableDirectiveLocation<Span> {
  #[inline]
  fn as_ref(&self) -> &str {
    self.as_str()
  }
}

impl<Span> AsRef<str> for TypeSystemDirectiveLocation<Span> {
  #[inline]
  fn as_ref(&self) -> &str {
    self.as_str()
  }
}

impl<Span> AsRef<str> for Location<Span> {
  #[inline]
  fn as_ref(&self) -> &str {
    self.as_str()
  }
}

impl<Span> core::fmt::Display for ExecutableDirectiveLocation<Span> {
  #[inline]
  fn fmt(&self, formatter: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    formatter.write_str(self.as_str())
  }
}

impl<Span> core::fmt::Display for TypeSystemDirectiveLocation<Span> {
  #[inline]
  fn fmt(&self, formatter: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    formatter.write_str(self.as_str())
  }
}

impl<Span> core::fmt::Display for Location<Span> {
  #[inline]
  fn fmt(&self, formatter: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    formatter.write_str(self.as_str())
  }
}

/// A collection of directive locations.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct DirectiveLocations<
  DirectiveLocation,
  Container = Vec<DirectiveLocation>,
  Span = SimpleSpan,
> {
  span: Span,
  locations: Container,
  _location: PhantomData<DirectiveLocation>,
}

impl_node_traits!(
  DirectiveLocations<DirectiveLocation, Container, Span>,
  (Span, Container),
  (span, locations),
  [Container, Span]
);

impl<DirectiveLocation, Container, Span> DirectiveLocations<DirectiveLocation, Container, Span> {
  /// Creates a directive-locations clause from its complete span and locations.
  #[inline]
  pub const fn new(span: Span, locations: Container) -> Self {
    Self {
      span,
      locations,
      _location: PhantomData,
    }
  }

  /// Returns the complete clause span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the contained directive locations.
  #[inline]
  pub fn locations(&self) -> &[DirectiveLocation]
  where
    Container: AsRef<[DirectiveLocation]>,
  {
    self.locations.as_ref()
  }

  /// Consumes this clause and returns its directive locations.
  #[inline]
  pub fn into_locations(self) -> Container {
    self.locations
  }
}

/// A directive definition.
///
/// See the [GraphQL Directive Definition specification](https://spec.graphql.org/draft/#DirectiveDefinition):
/// `Description? directive @ Name ArgumentsDefinition? Directives[Const]? repeatable? on
/// DirectiveLocations`. The directives written on the definition sit after its arguments.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct DirectiveDefinition<Name, Arguments, Directives, Locations, Span = SimpleSpan> {
  span: Span,
  name: Name,
  arguments_definition: Option<Arguments>,
  directives: Option<Directives>,
  repeatable: bool,
  locations: Locations,
}

impl_node_traits!(
  DirectiveDefinition<Name, Arguments, Directives, Locations, Span>,
  (Span, Name, Option<Arguments>, Option<Directives>, bool, Locations),
  (span, name, arguments_definition, directives, repeatable, locations),
  [Name, Arguments, Directives, Locations, Span]
);

impl<Name, Arguments, Directives, Locations, Span>
  DirectiveDefinition<Name, Arguments, Directives, Locations, Span>
{
  /// Creates a directive definition in source order.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    arguments_definition: Option<Arguments>,
    directives: Option<Directives>,
    repeatable: bool,
    locations: Locations,
  ) -> Self {
    Self {
      span,
      name,
      arguments_definition,
      directives,
      repeatable,
      locations,
    }
  }

  /// Returns the complete definition span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the directive name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the optional arguments definition.
  #[inline]
  pub const fn arguments_definition(&self) -> Option<&Arguments> {
    self.arguments_definition.as_ref()
  }

  /// Returns the optional directives applied to the definition itself.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns whether the directive is repeatable.
  #[inline]
  pub const fn repeatable(&self) -> bool {
    self.repeatable
  }

  /// Returns the required directive-locations clause.
  #[inline]
  pub const fn locations(&self) -> &Locations {
    &self.locations
  }
}

/// A schema definition.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct SchemaDefinition<Directives, RootOperations, Span = SimpleSpan> {
  span: Span,
  directives: Option<Directives>,
  root_operation_types_definition: RootOperations,
}

impl_node_traits!(
  SchemaDefinition<Directives, RootOperations, Span>,
  (Span, Option<Directives>, RootOperations),
  (span, directives, root_operation_types_definition),
  [Directives, RootOperations, Span]
);

impl<Directives, RootOperations, Span> SchemaDefinition<Directives, RootOperations, Span> {
  /// Creates a schema definition in source order.
  #[inline]
  pub const fn new(
    span: Span,
    directives: Option<Directives>,
    root_operation_types_definition: RootOperations,
  ) -> Self {
    Self {
      span,
      directives,
      root_operation_types_definition,
    }
  }

  /// Returns the complete definition span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the optional directive collection.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns the required root-operation-types definition.
  #[inline]
  pub const fn root_operation_types_definition(&self) -> &RootOperations {
    &self.root_operation_types_definition
  }
}

/// Directives added by an extension, with the optional interfaces written before them.
///
/// The `Directives` payload of the object and interface type extension data enums, and of
/// GraphQLx's scalar, enum, union and input object ones. Built with [`new`](Self::new); edited in
/// place with the `set_*` setters or by value with the `with_*` ones.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExtensionDirectives<Implements, Directives> {
  pub(crate) implements: Option<Implements>,
  pub(crate) directives: Directives,
}

impl<Implements, Directives> ExtensionDirectives<Implements, Directives> {
  /// Creates the payload from the optional implemented interfaces and the directives being added.
  #[inline]
  pub const fn new(implements: Option<Implements>, directives: Directives) -> Self {
    Self {
      implements,
      directives,
    }
  }

  /// Returns the optional implemented interfaces.
  #[inline]
  pub const fn implements(&self) -> Option<&Implements> {
    self.implements.as_ref()
  }

  /// Returns the directives being added.
  #[inline]
  pub const fn directives(&self) -> &Directives {
    &self.directives
  }

  /// Replaces the optional implemented interfaces.
  #[inline]
  pub fn set_implements(&mut self, implements: Option<Implements>) -> &mut Self {
    self.implements = implements;
    self
  }

  /// Replaces the directives.
  #[inline]
  pub fn set_directives(&mut self, directives: Directives) -> &mut Self {
    self.directives = directives;
    self
  }

  /// Returns the payload with the optional implemented interfaces replaced.
  #[inline]
  #[must_use]
  pub fn with_implements(mut self, implements: Option<Implements>) -> Self {
    self.implements = implements;
    self
  }

  /// Returns the payload with the directives replaced.
  #[inline]
  #[must_use]
  pub fn with_directives(mut self, directives: Directives) -> Self {
    self.directives = directives;
    self
  }
}

/// A fields definition added by an extension, with the optional interfaces and directives written
/// before it.
///
/// The `Fields` payload of the object and interface type extension data enums, and of GraphQLx's
/// input object one. Built with [`new`](Self::new); edited in place with the `set_*` setters or by
/// value with the `with_*` ones.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExtensionFields<Implements, Directives, Fields> {
  pub(crate) implements: Option<Implements>,
  pub(crate) directives: Option<Directives>,
  pub(crate) fields_definition: Fields,
}

impl<Implements, Directives, Fields> ExtensionFields<Implements, Directives, Fields> {
  /// Creates the payload from the optional implemented interfaces, the optional directives and the
  /// fields definition being added.
  #[inline]
  pub const fn new(
    implements: Option<Implements>,
    directives: Option<Directives>,
    fields_definition: Fields,
  ) -> Self {
    Self {
      implements,
      directives,
      fields_definition,
    }
  }

  /// Returns the optional implemented interfaces.
  #[inline]
  pub const fn implements(&self) -> Option<&Implements> {
    self.implements.as_ref()
  }

  /// Returns the optional directives being added.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns the nonempty fields definition being added.
  #[inline]
  pub const fn fields_definition(&self) -> &Fields {
    &self.fields_definition
  }

  /// Replaces the optional implemented interfaces.
  #[inline]
  pub fn set_implements(&mut self, implements: Option<Implements>) -> &mut Self {
    self.implements = implements;
    self
  }

  /// Replaces the optional directives.
  #[inline]
  pub fn set_directives(&mut self, directives: Option<Directives>) -> &mut Self {
    self.directives = directives;
    self
  }

  /// Replaces the fields definition.
  #[inline]
  pub fn set_fields_definition(&mut self, fields_definition: Fields) -> &mut Self {
    self.fields_definition = fields_definition;
    self
  }

  /// Returns the payload with the optional implemented interfaces replaced.
  #[inline]
  #[must_use]
  pub fn with_implements(mut self, implements: Option<Implements>) -> Self {
    self.implements = implements;
    self
  }

  /// Returns the payload with the optional directives replaced.
  #[inline]
  #[must_use]
  pub fn with_directives(mut self, directives: Option<Directives>) -> Self {
    self.directives = directives;
    self
  }

  /// Returns the payload with the fields definition replaced.
  #[inline]
  #[must_use]
  pub fn with_fields_definition(mut self, fields_definition: Fields) -> Self {
    self.fields_definition = fields_definition;
    self
  }
}

/// The payload contributed by an object or interface type extension.
///
/// Each variant represents one of GraphQL's legal nonempty extension forms.
#[derive(Debug, Clone, Copy, PartialEq, Eq, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ObjectTypeExtensionData<Implements, Directives, Fields> {
  /// Adds one or more implemented interfaces only.
  Implements(Implements),
  /// Adds directives, with optional implemented interfaces.
  Directives(ExtensionDirectives<Implements, Directives>),
  /// Adds fields, with optional interfaces and directives.
  Fields(ExtensionFields<Implements, Directives, Fields>),
}

impl<Implements, Directives, Fields> ObjectTypeExtensionData<Implements, Directives, Fields> {
  /// Returns the added implemented interfaces, if any.
  #[inline]
  pub const fn implements(&self) -> Option<&Implements> {
    match self {
      Self::Implements(implements) => Some(implements),
      Self::Directives(extension) => extension.implements(),
      Self::Fields(extension) => extension.implements(),
    }
  }

  /// Returns the added directives, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Directives(extension) => Some(extension.directives()),
      Self::Fields(extension) => extension.directives(),
      Self::Implements(_) => None,
    }
  }

  /// Returns the added fields definition, if any.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&Fields> {
    match self {
      Self::Fields(extension) => Some(extension.fields_definition()),
      Self::Implements(_) | Self::Directives(_) => None,
    }
  }
}

/// An object type extension (`extend type Name …`).
///
/// See the [GraphQL Object Type Extension specification](https://spec.graphql.org/draft/#ObjectTypeExtension).
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct ObjectTypeExtension<Name, Implements, Directives, Fields, Span = SimpleSpan> {
  span: Span,
  name: Name,
  data: ObjectTypeExtensionData<Implements, Directives, Fields>,
}

impl_node_traits!(
  ObjectTypeExtension<Name, Implements, Directives, Fields, Span>,
  (Span, Name, ObjectTypeExtensionData<Implements, Directives, Fields>),
  (span, name, data),
  [Name, Implements, Directives, Fields, Span]
);

impl<Name, Implements, Directives, Fields, Span>
  ObjectTypeExtension<Name, Implements, Directives, Fields, Span>
{
  /// Creates an object type extension from its complete span, name, and payload.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    data: ObjectTypeExtensionData<Implements, Directives, Fields>,
  ) -> Self {
    Self { span, name, data }
  }

  /// Returns the span including the leading `extend` keyword.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the extended object type name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the nonempty extension payload.
  #[inline]
  pub const fn data(&self) -> &ObjectTypeExtensionData<Implements, Directives, Fields> {
    &self.data
  }

  /// Returns the added implemented interfaces, if any.
  #[inline]
  pub const fn implements(&self) -> Option<&Implements> {
    self.data.implements()
  }

  /// Returns the added directives, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.data.directives()
  }

  /// Returns the added fields definition, if any.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&Fields> {
    self.data.fields_definition()
  }
}

/// The payload contributed by an interface type extension.
///
/// Each variant represents one of GraphQL's legal nonempty extension forms.
#[derive(Debug, Clone, Copy, PartialEq, Eq, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum InterfaceTypeExtensionData<Implements, Directives, Fields> {
  /// Adds one or more implemented interfaces only.
  Implements(Implements),
  /// Adds directives, with optional implemented interfaces.
  Directives(ExtensionDirectives<Implements, Directives>),
  /// Adds fields, with optional interfaces and directives.
  Fields(ExtensionFields<Implements, Directives, Fields>),
}

impl<Implements, Directives, Fields> InterfaceTypeExtensionData<Implements, Directives, Fields> {
  /// Returns the added implemented interfaces, if any.
  #[inline]
  pub const fn implements(&self) -> Option<&Implements> {
    match self {
      Self::Implements(implements) => Some(implements),
      Self::Directives(extension) => extension.implements(),
      Self::Fields(extension) => extension.implements(),
    }
  }

  /// Returns the added directives, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Directives(extension) => Some(extension.directives()),
      Self::Fields(extension) => extension.directives(),
      Self::Implements(_) => None,
    }
  }

  /// Returns the added fields definition, if any.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&Fields> {
    match self {
      Self::Fields(extension) => Some(extension.fields_definition()),
      Self::Implements(_) | Self::Directives(_) => None,
    }
  }
}

/// An interface type extension (`extend interface Name …`).
///
/// See the [GraphQL Interface Type Extension specification](https://spec.graphql.org/draft/#InterfaceTypeExtension).
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct InterfaceTypeExtension<Name, Implements, Directives, Fields, Span = SimpleSpan> {
  span: Span,
  name: Name,
  data: InterfaceTypeExtensionData<Implements, Directives, Fields>,
}

impl_node_traits!(
  InterfaceTypeExtension<Name, Implements, Directives, Fields, Span>,
  (Span, Name, InterfaceTypeExtensionData<Implements, Directives, Fields>),
  (span, name, data),
  [Name, Implements, Directives, Fields, Span]
);

impl<Name, Implements, Directives, Fields, Span>
  InterfaceTypeExtension<Name, Implements, Directives, Fields, Span>
{
  /// Creates an interface type extension from its complete span, name, and payload.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    data: InterfaceTypeExtensionData<Implements, Directives, Fields>,
  ) -> Self {
    Self { span, name, data }
  }

  /// Returns the span including the leading `extend` keyword.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the extended interface type name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the nonempty extension payload.
  #[inline]
  pub const fn data(&self) -> &InterfaceTypeExtensionData<Implements, Directives, Fields> {
    &self.data
  }

  /// Returns the added implemented interfaces, if any.
  #[inline]
  pub const fn implements(&self) -> Option<&Implements> {
    self.data.implements()
  }

  /// Returns the added directives, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.data.directives()
  }

  /// Returns the added fields definition, if any.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&Fields> {
    self.data.fields_definition()
  }
}

/// Root operation types added by a schema extension, with the optional directives written before
/// them.
///
/// The `Operations` payload of the schema extension data enum. Built with [`new`](Self::new);
/// edited in place with the `set_*` setters or by value with the `with_*` ones.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExtensionOperations<Directives, RootOperations> {
  pub(crate) directives: Option<Directives>,
  pub(crate) root_operation_types_definition: RootOperations,
}

impl<Directives, RootOperations> ExtensionOperations<Directives, RootOperations> {
  /// Creates the payload from the optional directives and the root-operation-types definition being
  /// added.
  #[inline]
  pub const fn new(
    directives: Option<Directives>,
    root_operation_types_definition: RootOperations,
  ) -> Self {
    Self {
      directives,
      root_operation_types_definition,
    }
  }

  /// Returns the optional directives being added.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns the nonempty root-operation-types definition being added.
  #[inline]
  pub const fn root_operation_types_definition(&self) -> &RootOperations {
    &self.root_operation_types_definition
  }

  /// Replaces the optional directives.
  #[inline]
  pub fn set_directives(&mut self, directives: Option<Directives>) -> &mut Self {
    self.directives = directives;
    self
  }

  /// Replaces the root-operation-types definition.
  #[inline]
  pub fn set_root_operation_types_definition(
    &mut self,
    root_operation_types_definition: RootOperations,
  ) -> &mut Self {
    self.root_operation_types_definition = root_operation_types_definition;
    self
  }

  /// Returns the payload with the optional directives replaced.
  #[inline]
  #[must_use]
  pub fn with_directives(mut self, directives: Option<Directives>) -> Self {
    self.directives = directives;
    self
  }

  /// Returns the payload with the root-operation-types definition replaced.
  #[inline]
  #[must_use]
  pub fn with_root_operation_types_definition(
    mut self,
    root_operation_types_definition: RootOperations,
  ) -> Self {
    self.root_operation_types_definition = root_operation_types_definition;
    self
  }
}

/// The payload contributed by a schema extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum SchemaExtensionData<Directives, RootOperations> {
  /// Adds directives only.
  Directives(Directives),
  /// Adds root operation types, with optional directives.
  Operations(ExtensionOperations<Directives, RootOperations>),
}

impl<Directives, RootOperations> SchemaExtensionData<Directives, RootOperations> {
  /// Returns the added directives, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Directives(directives) => Some(directives),
      Self::Operations(extension) => extension.directives(),
    }
  }

  /// Returns the added root-operation-types definition, if any.
  #[inline]
  pub const fn root_operation_types_definition(&self) -> Option<&RootOperations> {
    match self {
      Self::Operations(extension) => Some(extension.root_operation_types_definition()),
      Self::Directives(_) => None,
    }
  }
}

/// A schema extension (`extend schema …`).
///
/// See the [GraphQL Schema Extension specification](https://spec.graphql.org/draft/#SchemaExtension).
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct SchemaExtension<Directives, RootOperations, Span = SimpleSpan> {
  span: Span,
  data: SchemaExtensionData<Directives, RootOperations>,
}

impl_node_traits!(
  SchemaExtension<Directives, RootOperations, Span>,
  (Span, SchemaExtensionData<Directives, RootOperations>),
  (span, data),
  [Directives, RootOperations, Span]
);

impl<Directives, RootOperations, Span> SchemaExtension<Directives, RootOperations, Span> {
  /// Creates a schema extension from its complete span and nonempty payload.
  #[inline]
  pub const fn new(span: Span, data: SchemaExtensionData<Directives, RootOperations>) -> Self {
    Self { span, data }
  }

  /// Returns the span including the leading `extend` keyword.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the nonempty extension payload.
  #[inline]
  pub const fn data(&self) -> &SchemaExtensionData<Directives, RootOperations> {
    &self.data
  }

  /// Returns the added directives, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.data.directives()
  }

  /// Returns the added root-operation-types definition, if any.
  #[inline]
  pub const fn root_operation_types_definition(&self) -> Option<&RootOperations> {
    self.data.root_operation_types_definition()
  }
}

/// One of GraphQL's six named type extension shapes.
#[derive(Debug, Clone, PartialEq, Eq, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeExtension<
  ScalarTypeExtension,
  ObjectTypeExtension,
  InterfaceTypeExtension,
  UnionTypeExtension,
  EnumTypeExtension,
  InputObjectTypeExtension,
> {
  /// A scalar type extension.
  Scalar(ScalarTypeExtension),
  /// An object type extension.
  Object(ObjectTypeExtension),
  /// An interface type extension.
  Interface(InterfaceTypeExtension),
  /// A union type extension.
  Union(UnionTypeExtension),
  /// An enum type extension.
  Enum(EnumTypeExtension),
  /// An input object type extension.
  InputObject(InputObjectTypeExtension),
}

impl<
  ScalarTypeExtension,
  ObjectTypeExtension,
  InterfaceTypeExtension,
  UnionTypeExtension,
  EnumTypeExtension,
  InputObjectTypeExtension,
  Span,
> AsSpan<Span>
  for TypeExtension<
    ScalarTypeExtension,
    ObjectTypeExtension,
    InterfaceTypeExtension,
    UnionTypeExtension,
    EnumTypeExtension,
    InputObjectTypeExtension,
  >
where
  ScalarTypeExtension: AsSpan<Span>,
  ObjectTypeExtension: AsSpan<Span>,
  InterfaceTypeExtension: AsSpan<Span>,
  UnionTypeExtension: AsSpan<Span>,
  EnumTypeExtension: AsSpan<Span>,
  InputObjectTypeExtension: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Scalar(value) => value.as_span(),
      Self::Object(value) => value.as_span(),
      Self::Interface(value) => value.as_span(),
      Self::Union(value) => value.as_span(),
      Self::Enum(value) => value.as_span(),
      Self::InputObject(value) => value.as_span(),
    }
  }
}

impl<
  ScalarTypeExtension,
  ObjectTypeExtension,
  InterfaceTypeExtension,
  UnionTypeExtension,
  EnumTypeExtension,
  InputObjectTypeExtension,
>
  TypeExtension<
    ScalarTypeExtension,
    ObjectTypeExtension,
    InterfaceTypeExtension,
    UnionTypeExtension,
    EnumTypeExtension,
    InputObjectTypeExtension,
  >
{
  /// Returns the span of the selected extension arm.
  #[inline]
  pub fn span<Span>(&self) -> &Span
  where
    ScalarTypeExtension: AsSpan<Span>,
    ObjectTypeExtension: AsSpan<Span>,
    InterfaceTypeExtension: AsSpan<Span>,
    UnionTypeExtension: AsSpan<Span>,
    EnumTypeExtension: AsSpan<Span>,
    InputObjectTypeExtension: AsSpan<Span>,
  {
    self.as_span()
  }
}

impl<
  ScalarTypeExtension,
  ObjectTypeExtension,
  InterfaceTypeExtension,
  UnionTypeExtension,
  EnumTypeExtension,
  InputObjectTypeExtension,
  Span,
> IntoSpan<Span>
  for TypeExtension<
    ScalarTypeExtension,
    ObjectTypeExtension,
    InterfaceTypeExtension,
    UnionTypeExtension,
    EnumTypeExtension,
    InputObjectTypeExtension,
  >
where
  ScalarTypeExtension: IntoSpan<Span>,
  ObjectTypeExtension: IntoSpan<Span>,
  InterfaceTypeExtension: IntoSpan<Span>,
  UnionTypeExtension: IntoSpan<Span>,
  EnumTypeExtension: IntoSpan<Span>,
  InputObjectTypeExtension: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Scalar(value) => value.into_span(),
      Self::Object(value) => value.into_span(),
      Self::Interface(value) => value.into_span(),
      Self::Union(value) => value.into_span(),
      Self::Enum(value) => value.into_span(),
      Self::InputObject(value) => value.into_span(),
    }
  }
}

/// A directive extension (`extend directive @Name Directives`).
///
/// See the [GraphQL Directive Extension specification](https://spec.graphql.org/draft/#DirectiveExtension).
/// The grammar gives it exactly one shape, so its directives are mandatory.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct DirectiveExtension<Name, Directives, Span = SimpleSpan> {
  span: Span,
  name: Name,
  directives: Directives,
}

impl_node_traits!(
  DirectiveExtension<Name, Directives, Span>,
  (Span, Name, Directives),
  (span, name, directives),
  [Name, Directives, Span]
);

impl<Name, Directives, Span> DirectiveExtension<Name, Directives, Span> {
  /// Creates a directive extension from its complete span, name, and directives.
  #[inline]
  pub const fn new(span: Span, name: Name, directives: Directives) -> Self {
    Self {
      span,
      name,
      directives,
    }
  }

  /// Returns the span including the leading `extend` keyword.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the extended directive's name, without its `@`.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the required directives being added.
  #[inline]
  pub const fn directives(&self) -> &Directives {
    &self.directives
  }
}

/// A type-system definition: named type, directive, or schema.
#[derive(Debug, Clone, PartialEq, Eq, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeSystemDefinition<TypeDefinition, DirectiveDefinition, SchemaDefinition> {
  /// A named type definition.
  Type(TypeDefinition),
  /// A directive definition.
  Directive(DirectiveDefinition),
  /// A schema definition.
  Schema(SchemaDefinition),
}

impl<TypeDefinition, DirectiveDefinition, SchemaDefinition, Span> AsSpan<Span>
  for TypeSystemDefinition<TypeDefinition, DirectiveDefinition, SchemaDefinition>
where
  TypeDefinition: AsSpan<Span>,
  DirectiveDefinition: AsSpan<Span>,
  SchemaDefinition: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Type(value) => value.as_span(),
      Self::Directive(value) => value.as_span(),
      Self::Schema(value) => value.as_span(),
    }
  }
}

impl<TypeDefinition, DirectiveDefinition, SchemaDefinition, Span> IntoSpan<Span>
  for TypeSystemDefinition<TypeDefinition, DirectiveDefinition, SchemaDefinition>
where
  TypeDefinition: IntoSpan<Span>,
  DirectiveDefinition: IntoSpan<Span>,
  SchemaDefinition: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Type(value) => value.into_span(),
      Self::Directive(value) => value.into_span(),
      Self::Schema(value) => value.into_span(),
    }
  }
}

impl<TypeDefinition, DirectiveDefinition, SchemaDefinition>
  TypeSystemDefinition<TypeDefinition, DirectiveDefinition, SchemaDefinition>
{
  /// Returns the span of the selected definition arm.
  #[inline]
  pub fn span<Span>(&self) -> &Span
  where
    TypeDefinition: AsSpan<Span>,
    DirectiveDefinition: AsSpan<Span>,
    SchemaDefinition: AsSpan<Span>,
  {
    self.as_span()
  }
}

/// A type-system extension: named type, directive, or schema.
#[derive(Debug, Clone, PartialEq, Eq, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeSystemExtension<TypeExtension, DirectiveExtension, SchemaExtension> {
  /// A named type extension.
  Type(TypeExtension),
  /// A directive extension.
  Directive(DirectiveExtension),
  /// A schema extension.
  Schema(SchemaExtension),
}

impl<TypeExtension, DirectiveExtension, SchemaExtension, Span> AsSpan<Span>
  for TypeSystemExtension<TypeExtension, DirectiveExtension, SchemaExtension>
where
  TypeExtension: AsSpan<Span>,
  DirectiveExtension: AsSpan<Span>,
  SchemaExtension: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Type(value) => value.as_span(),
      Self::Directive(value) => value.as_span(),
      Self::Schema(value) => value.as_span(),
    }
  }
}

impl<TypeExtension, DirectiveExtension, SchemaExtension, Span> IntoSpan<Span>
  for TypeSystemExtension<TypeExtension, DirectiveExtension, SchemaExtension>
where
  TypeExtension: IntoSpan<Span>,
  DirectiveExtension: IntoSpan<Span>,
  SchemaExtension: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Type(value) => value.into_span(),
      Self::Directive(value) => value.into_span(),
      Self::Schema(value) => value.into_span(),
    }
  }
}

impl<TypeExtension, DirectiveExtension, SchemaExtension>
  TypeSystemExtension<TypeExtension, DirectiveExtension, SchemaExtension>
{
  /// Returns the span of the selected extension arm.
  #[inline]
  pub fn span<Span>(&self) -> &Span
  where
    TypeExtension: AsSpan<Span>,
    DirectiveExtension: AsSpan<Span>,
    SchemaExtension: AsSpan<Span>,
  {
    self.as_span()
  }
}

/// Either a described type-system definition or a type-system extension.
#[derive(Debug, Clone, PartialEq, Eq, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeSystemDefinitionOrExtension<Definition, Extension> {
  /// A type-system definition, including its optional description.
  Definition(Definition),
  /// A type-system extension, which cannot carry a description.
  Extension(Extension),
}

impl<Definition, Extension, Span> AsSpan<Span>
  for TypeSystemDefinitionOrExtension<Definition, Extension>
where
  Definition: AsSpan<Span>,
  Extension: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Definition(value) => value.as_span(),
      Self::Extension(value) => value.as_span(),
    }
  }
}

impl<Definition, Extension, Span> IntoSpan<Span>
  for TypeSystemDefinitionOrExtension<Definition, Extension>
where
  Definition: IntoSpan<Span>,
  Extension: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Definition(value) => value.into_span(),
      Self::Extension(value) => value.into_span(),
    }
  }
}

impl<Definition, Extension> TypeSystemDefinitionOrExtension<Definition, Extension> {
  /// Returns the span of the selected document-entry arm.
  #[inline]
  pub fn span<Span>(&self) -> &Span
  where
    Definition: AsSpan<Span>,
    Extension: AsSpan<Span>,
  {
    self.as_span()
  }
}
