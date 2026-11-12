//! The four type kinds GraphQLx lets implement interfaces where GraphQL does not — scalars, enums,
//! unions and input objects — as definitions and as extensions.
//!
//! Each carrier is the shared type-system carrier of the same name with an `implements` slot after
//! its name. GraphQL gives none of these kinds an `implements` clause, so the shared carriers keep
//! their shape and the GraphQLx aliases bind these instead. An extension's payload names each
//! nonempty form the grammar allows, as the object extension's does: interfaces alone, or
//! directives or the kind's own body with optional interfaces before them.

use derive_more::IsVariant;
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

/// A scalar type definition, with the interfaces it implements.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct ScalarTypeDefinition<Name, Implements, Directives, Span = SimpleSpan> {
  span: Span,
  name: Name,
  implements: Option<Implements>,
  directives: Option<Directives>,
}

impl_node_traits!(
  ScalarTypeDefinition<Name, Implements, Directives, Span>,
  (Span, Name, Option<Implements>, Option<Directives>),
  (span, name, implements, directives)
);

impl<Name, Implements, Directives, Span> ScalarTypeDefinition<Name, Implements, Directives, Span> {
  /// Creates a scalar type definition in source order.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    implements: Option<Implements>,
    directives: Option<Directives>,
  ) -> Self {
    Self {
      span,
      name,
      implements,
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

  /// Returns the optional implemented interfaces.
  #[inline]
  pub const fn implements(&self) -> Option<&Implements> {
    self.implements.as_ref()
  }

  /// Returns the optional directive collection.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }
}

/// An enum type definition, with the interfaces it implements.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct EnumTypeDefinition<Name, Implements, Directives, EnumValues, Span = SimpleSpan> {
  span: Span,
  name: Name,
  implements: Option<Implements>,
  directives: Option<Directives>,
  enum_values_definition: Option<EnumValues>,
}

impl_node_traits!(
  EnumTypeDefinition<Name, Implements, Directives, EnumValues, Span>,
  (
    Span,
    Name,
    Option<Implements>,
    Option<Directives>,
    Option<EnumValues>
  ),
  (span, name, implements, directives, enum_values_definition)
);

impl<Name, Implements, Directives, EnumValues, Span>
  EnumTypeDefinition<Name, Implements, Directives, EnumValues, Span>
{
  /// Creates an enum type definition in source order.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    implements: Option<Implements>,
    directives: Option<Directives>,
    enum_values_definition: Option<EnumValues>,
  ) -> Self {
    Self {
      span,
      name,
      implements,
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

  /// Returns the optional implemented interfaces.
  #[inline]
  pub const fn implements(&self) -> Option<&Implements> {
    self.implements.as_ref()
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

/// A union type definition, with the interfaces it implements.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct UnionTypeDefinition<Name, Implements, Directives, MemberTypes, Span = SimpleSpan> {
  span: Span,
  name: Name,
  implements: Option<Implements>,
  directives: Option<Directives>,
  member_types: Option<MemberTypes>,
}

impl_node_traits!(
  UnionTypeDefinition<Name, Implements, Directives, MemberTypes, Span>,
  (
    Span,
    Name,
    Option<Implements>,
    Option<Directives>,
    Option<MemberTypes>
  ),
  (span, name, implements, directives, member_types)
);

impl<Name, Implements, Directives, MemberTypes, Span>
  UnionTypeDefinition<Name, Implements, Directives, MemberTypes, Span>
{
  /// Creates a union type definition in source order.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    implements: Option<Implements>,
    directives: Option<Directives>,
    member_types: Option<MemberTypes>,
  ) -> Self {
    Self {
      span,
      name,
      implements,
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

  /// Returns the optional implemented interfaces.
  #[inline]
  pub const fn implements(&self) -> Option<&Implements> {
    self.implements.as_ref()
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

/// An input object type definition, with the interfaces it implements.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct InputObjectTypeDefinition<Name, Implements, Directives, Fields, Span = SimpleSpan> {
  span: Span,
  name: Name,
  implements: Option<Implements>,
  directives: Option<Directives>,
  fields_definition: Option<Fields>,
}

impl_node_traits!(
  InputObjectTypeDefinition<Name, Implements, Directives, Fields, Span>,
  (
    Span,
    Name,
    Option<Implements>,
    Option<Directives>,
    Option<Fields>
  ),
  (span, name, implements, directives, fields_definition)
);

impl<Name, Implements, Directives, Fields, Span>
  InputObjectTypeDefinition<Name, Implements, Directives, Fields, Span>
{
  /// Creates an input object type definition in source order.
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

  /// Returns the input object type name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the optional implemented interfaces.
  #[inline]
  pub const fn implements(&self) -> Option<&Implements> {
    self.implements.as_ref()
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

/// The payload contributed by a scalar type extension.
///
/// Each variant is one of the two nonempty forms: interfaces alone, or directives with optional
/// interfaces before them.
#[derive(Debug, Clone, Copy, PartialEq, Eq, IsVariant)]
pub enum ScalarTypeExtensionData<Implements, Directives> {
  /// Adds one or more implemented interfaces only.
  Implements(Implements),
  /// Adds directives, with optional implemented interfaces.
  Directives {
    /// The optional implemented interfaces.
    implements: Option<Implements>,
    /// The directives being added.
    directives: Directives,
  },
}

impl<Implements, Directives> ScalarTypeExtensionData<Implements, Directives> {
  /// Returns the added implemented interfaces, if any.
  #[inline]
  pub const fn implements(&self) -> Option<&Implements> {
    match self {
      Self::Implements(implements) => Some(implements),
      Self::Directives { implements, .. } => implements.as_ref(),
    }
  }

  /// Returns the added directives, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Directives { directives, .. } => Some(directives),
      Self::Implements(_) => None,
    }
  }
}

/// A scalar type extension (`extend scalar Name …`), with the interfaces it adds.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct ScalarTypeExtension<Name, Implements, Directives, Span = SimpleSpan> {
  span: Span,
  name: Name,
  data: ScalarTypeExtensionData<Implements, Directives>,
}

impl_node_traits!(
  ScalarTypeExtension<Name, Implements, Directives, Span>,
  (Span, Name, ScalarTypeExtensionData<Implements, Directives>),
  (span, name, data)
);

impl<Name, Implements, Directives, Span> ScalarTypeExtension<Name, Implements, Directives, Span> {
  /// Creates a scalar type extension from its complete span, name, and payload.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    data: ScalarTypeExtensionData<Implements, Directives>,
  ) -> Self {
    Self { span, name, data }
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

  /// Returns the nonempty extension payload.
  #[inline]
  pub const fn data(&self) -> &ScalarTypeExtensionData<Implements, Directives> {
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
}

/// The payload contributed by an enum type extension.
///
/// Each variant is one of the nonempty forms: interfaces alone, directives with optional interfaces
/// before them, or enum values with optional interfaces and directives before them.
#[derive(Debug, Clone, Copy, PartialEq, Eq, IsVariant)]
pub enum EnumTypeExtensionData<Implements, Directives, Values> {
  /// Adds one or more implemented interfaces only.
  Implements(Implements),
  /// Adds directives, with optional implemented interfaces.
  Directives {
    /// The optional implemented interfaces.
    implements: Option<Implements>,
    /// The directives being added.
    directives: Directives,
  },
  /// Adds enum values, with optional interfaces and directives.
  Values {
    /// The optional implemented interfaces.
    implements: Option<Implements>,
    /// The optional directives being added.
    directives: Option<Directives>,
    /// The nonempty enum-values definition being added.
    enum_values_definition: Values,
  },
}

impl<Implements, Directives, Values> EnumTypeExtensionData<Implements, Directives, Values> {
  /// Returns the added implemented interfaces, if any.
  #[inline]
  pub const fn implements(&self) -> Option<&Implements> {
    match self {
      Self::Implements(implements) => Some(implements),
      Self::Directives { implements, .. } | Self::Values { implements, .. } => implements.as_ref(),
    }
  }

  /// Returns the added directives, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Directives { directives, .. } => Some(directives),
      Self::Values { directives, .. } => directives.as_ref(),
      Self::Implements(_) => None,
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
      Self::Implements(_) | Self::Directives { .. } => None,
    }
  }
}

/// An enum type extension (`extend enum Name …`), with the interfaces it adds.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct EnumTypeExtension<Name, Implements, Directives, Values, Span = SimpleSpan> {
  span: Span,
  name: Name,
  data: EnumTypeExtensionData<Implements, Directives, Values>,
}

impl_node_traits!(
  EnumTypeExtension<Name, Implements, Directives, Values, Span>,
  (Span, Name, EnumTypeExtensionData<Implements, Directives, Values>),
  (span, name, data)
);

impl<Name, Implements, Directives, Values, Span>
  EnumTypeExtension<Name, Implements, Directives, Values, Span>
{
  /// Creates an enum type extension from its complete span, name, and payload.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    data: EnumTypeExtensionData<Implements, Directives, Values>,
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
  pub const fn data(&self) -> &EnumTypeExtensionData<Implements, Directives, Values> {
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

  /// Returns the added enum-values definition, if any.
  #[inline]
  pub const fn enum_values_definition(&self) -> Option<&Values> {
    self.data.enum_values_definition()
  }
}

/// The payload contributed by a union type extension.
///
/// Each variant is one of the nonempty forms: interfaces alone, directives with optional interfaces
/// before them, or members with optional interfaces and directives before them.
#[derive(Debug, Clone, Copy, PartialEq, Eq, IsVariant)]
pub enum UnionTypeExtensionData<Implements, Directives, MemberTypes> {
  /// Adds one or more implemented interfaces only.
  Implements(Implements),
  /// Adds directives, with optional implemented interfaces.
  Directives {
    /// The optional implemented interfaces.
    implements: Option<Implements>,
    /// The directives being added.
    directives: Directives,
  },
  /// Adds union member types, with optional interfaces and directives.
  Members {
    /// The optional implemented interfaces.
    implements: Option<Implements>,
    /// The optional directives being added.
    directives: Option<Directives>,
    /// The nonempty member-types clause being added.
    member_types: MemberTypes,
  },
}

impl<Implements, Directives, MemberTypes>
  UnionTypeExtensionData<Implements, Directives, MemberTypes>
{
  /// Returns the added implemented interfaces, if any.
  #[inline]
  pub const fn implements(&self) -> Option<&Implements> {
    match self {
      Self::Implements(implements) => Some(implements),
      Self::Directives { implements, .. } | Self::Members { implements, .. } => implements.as_ref(),
    }
  }

  /// Returns the added directives, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Directives { directives, .. } => Some(directives),
      Self::Members { directives, .. } => directives.as_ref(),
      Self::Implements(_) => None,
    }
  }

  /// Returns the added member-types clause, if any.
  #[inline]
  pub const fn member_types(&self) -> Option<&MemberTypes> {
    match self {
      Self::Members { member_types, .. } => Some(member_types),
      Self::Implements(_) | Self::Directives { .. } => None,
    }
  }
}

/// A union type extension (`extend union Name …`), with the interfaces it adds.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct UnionTypeExtension<Name, Implements, Directives, MemberTypes, Span = SimpleSpan> {
  span: Span,
  name: Name,
  data: UnionTypeExtensionData<Implements, Directives, MemberTypes>,
}

impl_node_traits!(
  UnionTypeExtension<Name, Implements, Directives, MemberTypes, Span>,
  (Span, Name, UnionTypeExtensionData<Implements, Directives, MemberTypes>),
  (span, name, data)
);

impl<Name, Implements, Directives, MemberTypes, Span>
  UnionTypeExtension<Name, Implements, Directives, MemberTypes, Span>
{
  /// Creates a union type extension from its complete span, name, and payload.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    data: UnionTypeExtensionData<Implements, Directives, MemberTypes>,
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
  pub const fn data(&self) -> &UnionTypeExtensionData<Implements, Directives, MemberTypes> {
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

  /// Returns the added union-member-types clause, if any.
  #[inline]
  pub const fn member_types(&self) -> Option<&MemberTypes> {
    self.data.member_types()
  }
}

/// The payload contributed by an input object type extension.
///
/// Each variant is one of the nonempty forms: interfaces alone, directives with optional interfaces
/// before them, or input fields with optional interfaces and directives before them.
#[derive(Debug, Clone, Copy, PartialEq, Eq, IsVariant)]
pub enum InputObjectTypeExtensionData<Implements, Directives, Fields> {
  /// Adds one or more implemented interfaces only.
  Implements(Implements),
  /// Adds directives, with optional implemented interfaces.
  Directives {
    /// The optional implemented interfaces.
    implements: Option<Implements>,
    /// The directives being added.
    directives: Directives,
  },
  /// Adds input fields, with optional interfaces and directives.
  Fields {
    /// The optional implemented interfaces.
    implements: Option<Implements>,
    /// The optional directives being added.
    directives: Option<Directives>,
    /// The nonempty input-fields definition being added.
    fields_definition: Fields,
  },
}

impl<Implements, Directives, Fields> InputObjectTypeExtensionData<Implements, Directives, Fields> {
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

  /// Returns the added input-fields definition, if any.
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

/// An input object type extension (`extend input Name …`), with the interfaces it adds.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct InputObjectTypeExtension<Name, Implements, Directives, Fields, Span = SimpleSpan> {
  span: Span,
  name: Name,
  data: InputObjectTypeExtensionData<Implements, Directives, Fields>,
}

impl_node_traits!(
  InputObjectTypeExtension<Name, Implements, Directives, Fields, Span>,
  (Span, Name, InputObjectTypeExtensionData<Implements, Directives, Fields>),
  (span, name, data)
);

impl<Name, Implements, Directives, Fields, Span>
  InputObjectTypeExtension<Name, Implements, Directives, Fields, Span>
{
  /// Creates an input object type extension from its complete span, name, and payload.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    data: InputObjectTypeExtensionData<Implements, Directives, Fields>,
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
  pub const fn data(&self) -> &InputObjectTypeExtensionData<Implements, Directives, Fields> {
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

  /// Returns the added input-fields definition, if any.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&Fields> {
    self.data.fields_definition()
  }
}
