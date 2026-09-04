//! Shared GraphQL-family type-system AST carriers.
//!
//! These data-only nodes model the SDL grammar independently of a concrete
//! dialect. GraphQL and GraphQLx bind their own name, type, value, and directive
//! nodes through aliases in their AST modules.

use core::marker::PhantomData;

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use std::vec::Vec;

use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

macro_rules! impl_node_traits {
  ($node:ident<$($generic:ident),+>, ($($component:ty),+), ($($field:ident),+)) => {
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

    impl<$($generic),+> IntoComponents for $node<$($generic),+> {
      type Components = ($($component,)+);

      #[inline]
      fn into_components(self) -> Self::Components {
        ($(self.$field,)+)
      }
    }
  };
}

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
  (span, input_value_definitions)
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
  (span, name, ty, default_value, directives)
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
  (span, input_value_definitions)
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
  (span, name, arguments_definition, ty, directives)
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
  (span, field_definitions)
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
  (span, interfaces)
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
  (span, name, implements, directives, fields_definition)
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

/// A scalar type definition.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct ScalarTypeDefinition<Name, Directives, Span = SimpleSpan> {
  span: Span,
  name: Name,
  directives: Option<Directives>,
}

impl_node_traits!(
  ScalarTypeDefinition<Name, Directives, Span>,
  (Span, Name, Option<Directives>),
  (span, name, directives)
);

impl<Name, Directives, Span> ScalarTypeDefinition<Name, Directives, Span> {
  /// Creates a scalar type definition in source order.
  #[inline]
  pub const fn new(span: Span, name: Name, directives: Option<Directives>) -> Self {
    Self {
      span,
      name,
      directives,
    }
  }

  /// Returns the complete definition span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the scalar name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the optional directive collection.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
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
  (span, operation_type, name)
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
  (span, root_operation_type_definitions)
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
  (span, value, directives)
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
  (span, enum_value_definitions)
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

/// An enum type definition.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct EnumTypeDefinition<Name, Directives, EnumValues, Span = SimpleSpan> {
  span: Span,
  name: Name,
  directives: Option<Directives>,
  enum_values_definition: Option<EnumValues>,
}

impl_node_traits!(
  EnumTypeDefinition<Name, Directives, EnumValues, Span>,
  (Span, Name, Option<Directives>, Option<EnumValues>),
  (span, name, directives, enum_values_definition)
);

impl<Name, Directives, EnumValues, Span> EnumTypeDefinition<Name, Directives, EnumValues, Span> {
  /// Creates an enum type definition in source order.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    directives: Option<Directives>,
    enum_values_definition: Option<EnumValues>,
  ) -> Self {
    Self {
      span,
      name,
      directives,
      enum_values_definition,
    }
  }

  /// Returns the complete definition span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the enum type name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the optional directive collection.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns the optional enum-values definition.
  #[inline]
  pub const fn enum_values_definition(&self) -> Option<&EnumValues> {
    self.enum_values_definition.as_ref()
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
  (span, name, implements, directives, fields_definition)
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
  (span, members)
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

/// A union type definition.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct UnionTypeDefinition<Name, Directives, MemberTypes, Span = SimpleSpan> {
  span: Span,
  name: Name,
  directives: Option<Directives>,
  member_types: Option<MemberTypes>,
}

impl_node_traits!(
  UnionTypeDefinition<Name, Directives, MemberTypes, Span>,
  (Span, Name, Option<Directives>, Option<MemberTypes>),
  (span, name, directives, member_types)
);

impl<Name, Directives, MemberTypes, Span> UnionTypeDefinition<Name, Directives, MemberTypes, Span> {
  /// Creates a union type definition in source order.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    directives: Option<Directives>,
    member_types: Option<MemberTypes>,
  ) -> Self {
    Self {
      span,
      name,
      directives,
      member_types,
    }
  }

  /// Returns the complete definition span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the union type name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the optional directive collection.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns the optional union-member-types clause.
  #[inline]
  pub const fn member_types(&self) -> Option<&MemberTypes> {
    self.member_types.as_ref()
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

impl<Span> IntoComponents for ExecutableDirectiveLocation<Span> {
  type Components = (Span, &'static str);

  #[inline]
  fn into_components(self) -> Self::Components {
    match self {
      Self::Query(span) => (span, "QUERY"),
      Self::Mutation(span) => (span, "MUTATION"),
      Self::Subscription(span) => (span, "SUBSCRIPTION"),
      Self::Field(span) => (span, "FIELD"),
      Self::FragmentDefinition(span) => (span, "FRAGMENT_DEFINITION"),
      Self::FragmentSpread(span) => (span, "FRAGMENT_SPREAD"),
      Self::InlineFragment(span) => (span, "INLINE_FRAGMENT"),
      Self::VariableDefinition(span) => (span, "VARIABLE_DEFINITION"),
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
      | Self::InputFieldDefinition(span) => span,
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
      | Self::InputFieldDefinition(span) => span,
    }
  }
}

impl<Span> IntoComponents for TypeSystemDirectiveLocation<Span> {
  type Components = (Span, &'static str);

  #[inline]
  fn into_components(self) -> Self::Components {
    match self {
      Self::Schema(span) => (span, "SCHEMA"),
      Self::Scalar(span) => (span, "SCALAR"),
      Self::Object(span) => (span, "OBJECT"),
      Self::FieldDefinition(span) => (span, "FIELD_DEFINITION"),
      Self::ArgumentDefinition(span) => (span, "ARGUMENT_DEFINITION"),
      Self::Interface(span) => (span, "INTERFACE"),
      Self::Union(span) => (span, "UNION"),
      Self::Enum(span) => (span, "ENUM"),
      Self::EnumValue(span) => (span, "ENUM_VALUE"),
      Self::InputObject(span) => (span, "INPUT_OBJECT"),
      Self::InputFieldDefinition(span) => (span, "INPUT_FIELD_DEFINITION"),
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

impl<Span> IntoComponents for Location<Span> {
  type Components = (Span, &'static str);

  #[inline]
  fn into_components(self) -> Self::Components {
    match self {
      Self::Executable(location) => location.into_components(),
      Self::TypeSystem(location) => location.into_components(),
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
  (span, locations)
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
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct DirectiveDefinition<Name, Arguments, Locations, Span = SimpleSpan> {
  span: Span,
  name: Name,
  arguments_definition: Option<Arguments>,
  repeatable: bool,
  locations: Locations,
}

impl_node_traits!(
  DirectiveDefinition<Name, Arguments, Locations, Span>,
  (Span, Name, Option<Arguments>, bool, Locations),
  (span, name, arguments_definition, repeatable, locations)
);

impl<Name, Arguments, Locations, Span> DirectiveDefinition<Name, Arguments, Locations, Span> {
  /// Creates a directive definition in source order.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    arguments_definition: Option<Arguments>,
    repeatable: bool,
    locations: Locations,
  ) -> Self {
    Self {
      span,
      name,
      arguments_definition,
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
  (span, directives, root_operation_types_definition)
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

/// An input object type definition.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct InputObjectTypeDefinition<Name, Directives, Fields, Span = SimpleSpan> {
  span: Span,
  name: Name,
  directives: Option<Directives>,
  fields_definition: Option<Fields>,
}

impl_node_traits!(
  InputObjectTypeDefinition<Name, Directives, Fields, Span>,
  (Span, Name, Option<Directives>, Option<Fields>),
  (span, name, directives, fields_definition)
);

impl<Name, Directives, Fields, Span> InputObjectTypeDefinition<Name, Directives, Fields, Span> {
  /// Creates an input object type definition in source order.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    directives: Option<Directives>,
    fields_definition: Option<Fields>,
  ) -> Self {
    Self {
      span,
      name,
      directives,
      fields_definition,
    }
  }

  /// Returns the complete definition span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the input object type name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the optional directive collection.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns the optional input-fields definition.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&Fields> {
    self.fields_definition.as_ref()
  }
}

/// The payload contributed by an object or interface type extension.
///
/// Each variant represents one of GraphQL's legal nonempty extension forms.
#[derive(Debug, Clone, Copy, PartialEq, Eq, IsVariant)]
pub enum ObjectTypeExtensionData<Implements, Directives, Fields> {
  /// Adds one or more implemented interfaces only.
  Implements(Implements),
  /// Adds directives, with optional implemented interfaces.
  Directives {
    /// The optional implemented interfaces.
    implements: Option<Implements>,
    /// The directives being added.
    directives: Directives,
  },
  /// Adds fields, with optional interfaces and directives.
  Fields {
    /// The optional implemented interfaces.
    implements: Option<Implements>,
    /// The optional directives being added.
    directives: Option<Directives>,
    /// The nonempty fields definition being added.
    fields_definition: Fields,
  },
}

impl<Implements, Directives, Fields> ObjectTypeExtensionData<Implements, Directives, Fields> {
  /// Returns the added implemented interfaces, if any.
  #[inline]
  pub const fn implements(&self) -> Option<&Implements> {
    match self {
      Self::Implements(implements) => Some(implements),
      Self::Directives { implements, .. } | Self::Fields { implements, .. } => implements.as_ref(),
    }
  }

  /// Returns the added directives, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Directives { directives, .. } => Some(directives),
      Self::Fields { directives, .. } => directives.as_ref(),
      Self::Implements(_) => None,
    }
  }

  /// Returns the added fields definition, if any.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&Fields> {
    match self {
      Self::Fields {
        fields_definition, ..
      } => Some(fields_definition),
      Self::Implements(_) | Self::Directives { .. } => None,
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
  (span, name, data)
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, IsVariant)]
pub enum InterfaceTypeExtensionData<Implements, Directives, Fields> {
  /// Adds one or more implemented interfaces only.
  Implements(Implements),
  /// Adds directives, with optional implemented interfaces.
  Directives {
    /// The optional implemented interfaces.
    implements: Option<Implements>,
    /// The directives being added.
    directives: Directives,
  },
  /// Adds fields, with optional interfaces and directives.
  Fields {
    /// The optional implemented interfaces.
    implements: Option<Implements>,
    /// The optional directives being added.
    directives: Option<Directives>,
    /// The nonempty fields definition being added.
    fields_definition: Fields,
  },
}

impl<Implements, Directives, Fields> InterfaceTypeExtensionData<Implements, Directives, Fields> {
  /// Returns the added implemented interfaces, if any.
  #[inline]
  pub const fn implements(&self) -> Option<&Implements> {
    match self {
      Self::Implements(implements) => Some(implements),
      Self::Directives { implements, .. } | Self::Fields { implements, .. } => implements.as_ref(),
    }
  }

  /// Returns the added directives, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Directives { directives, .. } => Some(directives),
      Self::Fields { directives, .. } => directives.as_ref(),
      Self::Implements(_) => None,
    }
  }

  /// Returns the added fields definition, if any.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&Fields> {
    match self {
      Self::Fields {
        fields_definition, ..
      } => Some(fields_definition),
      Self::Implements(_) | Self::Directives { .. } => None,
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
  (span, name, data)
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

/// The payload contributed by a union type extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, IsVariant)]
pub enum UnionTypeExtensionData<Directives, MemberTypes> {
  /// Adds directives only.
  Directives(Directives),
  /// Adds union member types, with optional directives.
  Members {
    /// The optional directives being added.
    directives: Option<Directives>,
    /// The nonempty member-types clause being added.
    member_types: MemberTypes,
  },
}

impl<Directives, MemberTypes> UnionTypeExtensionData<Directives, MemberTypes> {
  /// Returns the added directives, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Directives(directives) => Some(directives),
      Self::Members { directives, .. } => directives.as_ref(),
    }
  }

  /// Returns the added member-types clause, if any.
  #[inline]
  pub const fn member_types(&self) -> Option<&MemberTypes> {
    match self {
      Self::Members { member_types, .. } => Some(member_types),
      Self::Directives(_) => None,
    }
  }
}

/// A union type extension (`extend union Name …`).
///
/// See the [GraphQL Union Type Extension specification](https://spec.graphql.org/draft/#UnionTypeExtension).
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct UnionTypeExtension<Name, Directives, MemberTypes, Span = SimpleSpan> {
  span: Span,
  name: Name,
  data: UnionTypeExtensionData<Directives, MemberTypes>,
}

impl_node_traits!(
  UnionTypeExtension<Name, Directives, MemberTypes, Span>,
  (Span, Name, UnionTypeExtensionData<Directives, MemberTypes>),
  (span, name, data)
);

impl<Name, Directives, MemberTypes, Span> UnionTypeExtension<Name, Directives, MemberTypes, Span> {
  /// Creates a union type extension from its complete span, name, and payload.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    data: UnionTypeExtensionData<Directives, MemberTypes>,
  ) -> Self {
    Self { span, name, data }
  }

  /// Returns the span including the leading `extend` keyword.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the extended union type name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the nonempty extension payload.
  #[inline]
  pub const fn data(&self) -> &UnionTypeExtensionData<Directives, MemberTypes> {
    &self.data
  }

  /// Returns the added directives, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.data.directives()
  }

  /// Returns the added union-member-types clause, if any.
  #[inline]
  pub const fn member_types(&self) -> Option<&MemberTypes> {
    self.data.member_types()
  }
}

/// The payload contributed by an enum type extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, IsVariant)]
pub enum EnumTypeExtensionData<Directives, Values> {
  /// Adds directives only.
  Directives(Directives),
  /// Adds enum values, with optional directives.
  Values {
    /// The optional directives being added.
    directives: Option<Directives>,
    /// The nonempty enum-values definition being added.
    enum_values_definition: Values,
  },
}

impl<Directives, Values> EnumTypeExtensionData<Directives, Values> {
  /// Returns the added directives, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Directives(directives) => Some(directives),
      Self::Values { directives, .. } => directives.as_ref(),
    }
  }

  /// Returns the added enum-values definition, if any.
  #[inline]
  pub const fn enum_values_definition(&self) -> Option<&Values> {
    match self {
      Self::Values {
        enum_values_definition,
        ..
      } => Some(enum_values_definition),
      Self::Directives(_) => None,
    }
  }
}

/// An enum type extension (`extend enum Name …`).
///
/// See the [GraphQL Enum Type Extension specification](https://spec.graphql.org/draft/#EnumTypeExtension).
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct EnumTypeExtension<Name, Directives, Values, Span = SimpleSpan> {
  span: Span,
  name: Name,
  data: EnumTypeExtensionData<Directives, Values>,
}

impl_node_traits!(
  EnumTypeExtension<Name, Directives, Values, Span>,
  (Span, Name, EnumTypeExtensionData<Directives, Values>),
  (span, name, data)
);

impl<Name, Directives, Values, Span> EnumTypeExtension<Name, Directives, Values, Span> {
  /// Creates an enum type extension from its complete span, name, and payload.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    data: EnumTypeExtensionData<Directives, Values>,
  ) -> Self {
    Self { span, name, data }
  }

  /// Returns the span including the leading `extend` keyword.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the extended enum type name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the nonempty extension payload.
  #[inline]
  pub const fn data(&self) -> &EnumTypeExtensionData<Directives, Values> {
    &self.data
  }

  /// Returns the added directives, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.data.directives()
  }

  /// Returns the added enum-values definition, if any.
  #[inline]
  pub const fn enum_values_definition(&self) -> Option<&Values> {
    self.data.enum_values_definition()
  }
}

/// The payload contributed by an input object type extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, IsVariant)]
pub enum InputObjectTypeExtensionData<Directives, Fields> {
  /// Adds directives only.
  Directives(Directives),
  /// Adds input fields, with optional directives.
  Fields {
    /// The optional directives being added.
    directives: Option<Directives>,
    /// The nonempty input-fields definition being added.
    fields_definition: Fields,
  },
}

impl<Directives, Fields> InputObjectTypeExtensionData<Directives, Fields> {
  /// Returns the added directives, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Directives(directives) => Some(directives),
      Self::Fields { directives, .. } => directives.as_ref(),
    }
  }

  /// Returns the added input-fields definition, if any.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&Fields> {
    match self {
      Self::Fields {
        fields_definition, ..
      } => Some(fields_definition),
      Self::Directives(_) => None,
    }
  }
}

/// An input object type extension (`extend input Name …`).
///
/// See the [GraphQL Input Object Type Extension specification](https://spec.graphql.org/draft/#InputObjectTypeExtension).
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct InputObjectTypeExtension<Name, Directives, Fields, Span = SimpleSpan> {
  span: Span,
  name: Name,
  data: InputObjectTypeExtensionData<Directives, Fields>,
}

impl_node_traits!(
  InputObjectTypeExtension<Name, Directives, Fields, Span>,
  (Span, Name, InputObjectTypeExtensionData<Directives, Fields>),
  (span, name, data)
);

impl<Name, Directives, Fields, Span> InputObjectTypeExtension<Name, Directives, Fields, Span> {
  /// Creates an input object type extension from its complete span, name, and payload.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    data: InputObjectTypeExtensionData<Directives, Fields>,
  ) -> Self {
    Self { span, name, data }
  }

  /// Returns the span including the leading `extend` keyword.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the extended input object type name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the nonempty extension payload.
  #[inline]
  pub const fn data(&self) -> &InputObjectTypeExtensionData<Directives, Fields> {
    &self.data
  }

  /// Returns the added directives, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.data.directives()
  }

  /// Returns the added input-fields definition, if any.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&Fields> {
    self.data.fields_definition()
  }
}

/// The payload contributed by a schema extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, IsVariant)]
pub enum SchemaExtensionData<Directives, RootOperations> {
  /// Adds directives only.
  Directives(Directives),
  /// Adds root operation types, with optional directives.
  Operations {
    /// The optional directives being added.
    directives: Option<Directives>,
    /// The nonempty root-operation-types definition being added.
    root_operation_types_definition: RootOperations,
  },
}

impl<Directives, RootOperations> SchemaExtensionData<Directives, RootOperations> {
  /// Returns the added directives, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Directives(directives) => Some(directives),
      Self::Operations { directives, .. } => directives.as_ref(),
    }
  }

  /// Returns the added root-operation-types definition, if any.
  #[inline]
  pub const fn root_operation_types_definition(&self) -> Option<&RootOperations> {
    match self {
      Self::Operations {
        root_operation_types_definition,
        ..
      } => Some(root_operation_types_definition),
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
  (span, data)
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

/// A scalar type extension (`extend scalar Name Directives`).
///
/// See the [GraphQL Scalar Type Extension specification](https://spec.graphql.org/draft/#ScalarTypeExtension).
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct ScalarTypeExtension<Name, Directives, Span = SimpleSpan> {
  span: Span,
  name: Name,
  directives: Directives,
}

impl_node_traits!(
  ScalarTypeExtension<Name, Directives, Span>,
  (Span, Name, Directives),
  (span, name, directives)
);

impl<Name, Directives, Span> ScalarTypeExtension<Name, Directives, Span> {
  /// Creates a scalar type extension from its complete span, name, and directives.
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

  /// Returns the extended scalar type name.
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

/// A type-system extension: named type or schema.
#[derive(Debug, Clone, PartialEq, Eq, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeSystemExtension<TypeExtension, SchemaExtension> {
  /// A named type extension.
  Type(TypeExtension),
  /// A schema extension.
  Schema(SchemaExtension),
}

impl<TypeExtension, SchemaExtension, Span> AsSpan<Span>
  for TypeSystemExtension<TypeExtension, SchemaExtension>
where
  TypeExtension: AsSpan<Span>,
  SchemaExtension: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Type(value) => value.as_span(),
      Self::Schema(value) => value.as_span(),
    }
  }
}

impl<TypeExtension, SchemaExtension, Span> IntoSpan<Span>
  for TypeSystemExtension<TypeExtension, SchemaExtension>
where
  TypeExtension: IntoSpan<Span>,
  SchemaExtension: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Type(value) => value.into_span(),
      Self::Schema(value) => value.into_span(),
    }
  }
}

impl<TypeExtension, SchemaExtension> TypeSystemExtension<TypeExtension, SchemaExtension> {
  /// Returns the span of the selected extension arm.
  #[inline]
  pub fn span<Span>(&self) -> &Span
  where
    TypeExtension: AsSpan<Span>,
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

#[cfg(test)]
mod tests {

  use tokora::{
    span::{AsSpan, IntoSpan},
    utils::IntoComponents,
  };

  use super::{
    ArgumentsDefinition, DirectiveDefinition, DirectiveLocations, ExecutableDirectiveLocation,
    FieldDefinition, InputValueDefinition, Location, ObjectTypeDefinition,
    TypeSystemDirectiveLocation,
  };

  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  struct CustomSpan(u8);

  struct ArrayBacked<T, const N: usize>([T; N]);

  impl<T, const N: usize> AsRef<[T]> for ArrayBacked<T, N> {
    fn as_ref(&self) -> &[T] {
      &self.0
    }
  }

  #[test]
  fn carriers_support_custom_spans_and_components() {
    let input = InputValueDefinition::<_, _, _, _, CustomSpan>::new(
      CustomSpan(1),
      "limit",
      "Int",
      Some("10"),
      Some("@range"),
    );
    assert_eq!(input.as_span(), &CustomSpan(1));
    assert_eq!(input.name(), &"limit");
    assert_eq!(input.ty(), &"Int");
    assert_eq!(input.default_value(), Some(&"10"));
    assert_eq!(input.directives(), Some(&"@range"));
    assert_eq!(
      input.into_components(),
      (CustomSpan(1), "limit", "Int", Some("10"), Some("@range"))
    );

    let field = FieldDefinition::<_, _, _, _, CustomSpan>::new(
      CustomSpan(2),
      "users",
      Some("(limit: Int)"),
      "[User!]!",
      None::<&str>,
    );
    assert_eq!(field.arguments_definition(), Some(&"(limit: Int)"));
    assert_eq!(field.directives(), None);
    assert_eq!(field.into_span(), CustomSpan(2));

    let object = ObjectTypeDefinition::<_, _, _, _, CustomSpan>::new(
      CustomSpan(3),
      "Query",
      None::<&str>,
      Some("@root"),
      Some("{ users: [User!]! }"),
    );
    assert_eq!(object.implements(), None);
    assert_eq!(object.directives(), Some(&"@root"));
    assert_eq!(object.fields_definition(), Some(&"{ users: [User!]! }"));

    let directive = DirectiveDefinition::<_, _, _, CustomSpan>::new(
      CustomSpan(4),
      "deprecated",
      None::<&str>,
      true,
      "FIELD_DEFINITION",
    );
    assert_eq!(directive.arguments_definition(), None);
    assert!(directive.repeatable());
    assert_eq!(directive.locations(), &"FIELD_DEFINITION");
  }

  #[test]
  fn collections_project_array_backed_containers_to_slices() {
    let arguments =
      ArgumentsDefinition::<u8, _, CustomSpan>::new(CustomSpan(1), ArrayBacked([1_u8, 2]));
    let projected: &[u8] = arguments.input_value_definitions();
    assert_eq!(projected, &[1, 2]);
    assert_eq!(arguments.into_input_value_definitions().0, [1, 2]);

    let locations =
      DirectiveLocations::<u8, _, CustomSpan>::new(CustomSpan(2), ArrayBacked([3_u8, 4]));
    let projected: &[u8] = locations.locations();
    assert_eq!(projected, &[3, 4]);
    assert_eq!(locations.into_locations().0, [3, 4]);
  }

  #[test]
  fn directive_locations_preserve_their_kind_and_span() {
    let executable = ExecutableDirectiveLocation::field(CustomSpan(1));
    assert_eq!(executable.as_str(), "FIELD");
    assert_eq!(executable.into_components(), (CustomSpan(1), "FIELD"));

    let location: Location<CustomSpan> =
      TypeSystemDirectiveLocation::input_field_definition(CustomSpan(2)).into();
    assert_eq!(location.as_span(), &CustomSpan(2));
    assert_eq!(location.as_str(), "INPUT_FIELD_DEFINITION");
  }

  #[test]
  fn directive_location_enums_keep_variant_and_textual_ergonomics() {
    let query = ExecutableDirectiveLocation::query(CustomSpan(1));
    assert!(query.is_query());
    assert_eq!(query.unwrap_query_ref(), &CustomSpan(1));
    assert_eq!(query.try_unwrap_query_ref(), Ok(&CustomSpan(1)));
    assert_eq!(AsRef::<str>::as_ref(&query), "QUERY");

    let input_field = TypeSystemDirectiveLocation::input_field_definition(CustomSpan(2));
    assert!(input_field.is_input_field_definition());
    assert_eq!(
      input_field.unwrap_input_field_definition_ref(),
      &CustomSpan(2)
    );
    assert_eq!(AsRef::<str>::as_ref(&input_field), "INPUT_FIELD_DEFINITION");

    let location: Location<CustomSpan> = query.into();
    assert!(location.is_executable());
    assert_eq!(location.unwrap_executable_ref().as_str(), "QUERY");
    assert!(location.try_unwrap_type_system_ref().is_err());
    assert_eq!(AsRef::<str>::as_ref(&location), "QUERY");
  }
}
