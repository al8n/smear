//! The scalar, enum, union and input object type-system carriers, which GraphQL binds and GraphQLx
//! does not.
//!
//! GraphQLx's four kinds take an `implements` clause and bind carriers of their own from
//! `crate::generic`, so these compile only with `graphql`.

use derive_more::IsVariant;
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
