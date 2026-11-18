//! The scalar, enum, union and input object type-system carriers, which GraphQL binds and GraphQLx
//! does not.
//!
//! GraphQLx's four kinds take an `implements` clause and bind carriers of their own from
//! `crate::generic`, so these compile only with `graphql`.

use derive_more::{IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

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
  (span, name, directives),
  [Name, Directives, Span]
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
  (span, name, directives, enum_values_definition),
  [Name, Directives, EnumValues, Span]
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
  (span, name, directives, member_types),
  [Name, Directives, MemberTypes, Span]
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
  (span, name, directives, fields_definition),
  [Name, Directives, Fields, Span]
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

/// Union member types added by an extension, with the optional directives written before them.
///
/// The `Members` payload of the union type extension data enum. Built with [`new`](Self::new);
/// edited in place with the `set_*` setters or by value with the `with_*` ones.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExtensionMembers<Directives, MemberTypes> {
  pub(crate) directives: Option<Directives>,
  pub(crate) member_types: MemberTypes,
}

impl<Directives, MemberTypes> ExtensionMembers<Directives, MemberTypes> {
  /// Creates the payload from the optional directives and the member-types clause being added.
  #[inline]
  pub const fn new(directives: Option<Directives>, member_types: MemberTypes) -> Self {
    Self {
      directives,
      member_types,
    }
  }

  /// Returns the optional directives being added.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns the nonempty member-types clause being added.
  #[inline]
  pub const fn member_types(&self) -> &MemberTypes {
    &self.member_types
  }

  /// Replaces the optional directives.
  #[inline]
  pub fn set_directives(&mut self, directives: Option<Directives>) -> &mut Self {
    self.directives = directives;
    self
  }

  /// Replaces the member-types clause.
  #[inline]
  pub fn set_member_types(&mut self, member_types: MemberTypes) -> &mut Self {
    self.member_types = member_types;
    self
  }

  /// Returns the payload with the optional directives replaced.
  #[inline]
  #[must_use]
  pub fn with_directives(mut self, directives: Option<Directives>) -> Self {
    self.directives = directives;
    self
  }

  /// Returns the payload with the member-types clause replaced.
  #[inline]
  #[must_use]
  pub fn with_member_types(mut self, member_types: MemberTypes) -> Self {
    self.member_types = member_types;
    self
  }
}

/// The payload contributed by a union type extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum UnionTypeExtensionData<Directives, MemberTypes> {
  /// Adds directives only.
  Directives(Directives),
  /// Adds union member types, with optional directives.
  Members(ExtensionMembers<Directives, MemberTypes>),
}

impl<Directives, MemberTypes> UnionTypeExtensionData<Directives, MemberTypes> {
  /// Returns the added directives, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Directives(directives) => Some(directives),
      Self::Members(extension) => extension.directives(),
    }
  }

  /// Returns the added member-types clause, if any.
  #[inline]
  pub const fn member_types(&self) -> Option<&MemberTypes> {
    match self {
      Self::Members(extension) => Some(extension.member_types()),
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
  (span, name, data),
  [Name, Directives, MemberTypes, Span]
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

/// Enum values added by an extension, with the optional directives written before them.
///
/// The `Values` payload of the enum type extension data enum. Built with [`new`](Self::new); edited
/// in place with the `set_*` setters or by value with the `with_*` ones.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExtensionValues<Directives, Values> {
  pub(crate) directives: Option<Directives>,
  pub(crate) enum_values_definition: Values,
}

impl<Directives, Values> ExtensionValues<Directives, Values> {
  /// Creates the payload from the optional directives and the enum-values definition being added.
  #[inline]
  pub const fn new(directives: Option<Directives>, enum_values_definition: Values) -> Self {
    Self {
      directives,
      enum_values_definition,
    }
  }

  /// Returns the optional directives being added.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns the nonempty enum-values definition being added.
  #[inline]
  pub const fn enum_values_definition(&self) -> &Values {
    &self.enum_values_definition
  }

  /// Replaces the optional directives.
  #[inline]
  pub fn set_directives(&mut self, directives: Option<Directives>) -> &mut Self {
    self.directives = directives;
    self
  }

  /// Replaces the enum-values definition.
  #[inline]
  pub fn set_enum_values_definition(&mut self, enum_values_definition: Values) -> &mut Self {
    self.enum_values_definition = enum_values_definition;
    self
  }

  /// Returns the payload with the optional directives replaced.
  #[inline]
  #[must_use]
  pub fn with_directives(mut self, directives: Option<Directives>) -> Self {
    self.directives = directives;
    self
  }

  /// Returns the payload with the enum-values definition replaced.
  #[inline]
  #[must_use]
  pub fn with_enum_values_definition(mut self, enum_values_definition: Values) -> Self {
    self.enum_values_definition = enum_values_definition;
    self
  }
}

/// The payload contributed by an enum type extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum EnumTypeExtensionData<Directives, Values> {
  /// Adds directives only.
  Directives(Directives),
  /// Adds enum values, with optional directives.
  Values(ExtensionValues<Directives, Values>),
}

impl<Directives, Values> EnumTypeExtensionData<Directives, Values> {
  /// Returns the added directives, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Directives(directives) => Some(directives),
      Self::Values(extension) => extension.directives(),
    }
  }

  /// Returns the added enum-values definition, if any.
  #[inline]
  pub const fn enum_values_definition(&self) -> Option<&Values> {
    match self {
      Self::Values(extension) => Some(extension.enum_values_definition()),
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
  (span, name, data),
  [Name, Directives, Values, Span]
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

/// Input fields added by an extension, with the optional directives written before them.
///
/// The `Fields` payload of the input object type extension data enum. Built with
/// [`new`](Self::new); edited in place with the `set_*` setters or by value with the `with_*` ones.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExtensionInputFields<Directives, Fields> {
  pub(crate) directives: Option<Directives>,
  pub(crate) fields_definition: Fields,
}

impl<Directives, Fields> ExtensionInputFields<Directives, Fields> {
  /// Creates the payload from the optional directives and the input-fields definition being added.
  #[inline]
  pub const fn new(directives: Option<Directives>, fields_definition: Fields) -> Self {
    Self {
      directives,
      fields_definition,
    }
  }

  /// Returns the optional directives being added.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns the nonempty input-fields definition being added.
  #[inline]
  pub const fn fields_definition(&self) -> &Fields {
    &self.fields_definition
  }

  /// Replaces the optional directives.
  #[inline]
  pub fn set_directives(&mut self, directives: Option<Directives>) -> &mut Self {
    self.directives = directives;
    self
  }

  /// Replaces the input-fields definition.
  #[inline]
  pub fn set_fields_definition(&mut self, fields_definition: Fields) -> &mut Self {
    self.fields_definition = fields_definition;
    self
  }

  /// Returns the payload with the optional directives replaced.
  #[inline]
  #[must_use]
  pub fn with_directives(mut self, directives: Option<Directives>) -> Self {
    self.directives = directives;
    self
  }

  /// Returns the payload with the input-fields definition replaced.
  #[inline]
  #[must_use]
  pub fn with_fields_definition(mut self, fields_definition: Fields) -> Self {
    self.fields_definition = fields_definition;
    self
  }
}

/// The payload contributed by an input object type extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum InputObjectTypeExtensionData<Directives, Fields> {
  /// Adds directives only.
  Directives(Directives),
  /// Adds input fields, with optional directives.
  Fields(ExtensionInputFields<Directives, Fields>),
}

impl<Directives, Fields> InputObjectTypeExtensionData<Directives, Fields> {
  /// Returns the added directives, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Directives(directives) => Some(directives),
      Self::Fields(extension) => extension.directives(),
    }
  }

  /// Returns the added input-fields definition, if any.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&Fields> {
    match self {
      Self::Fields(extension) => Some(extension.fields_definition()),
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
  (span, name, data),
  [Name, Directives, Fields, Span]
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
  (span, name, directives),
  [Name, Directives, Span]
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
