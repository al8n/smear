//! Definitions: the types and directives the packages define, and what each one answers.

use tokora::diagnostic::Location;

use crate::graphqlx::id::TypeExprId;

use super::member::MemberKind;

/// What kind of definition a definition is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum DefKind {
  /// A scalar: a primitive, a custom scalar, or a scalar a host type stands for.
  Scalar,
  /// An object type.
  Object,
  /// An interface, a marker included.
  Interface,
  /// A union.
  Union,
  /// An enum.
  Enum,
  /// An input object.
  InputObject,
  /// A directive definition.
  Directive,
}

impl DefKind {
  /// The kind of the direct members a definition of this kind has, or `None` for one that has
  /// none.
  pub(super) const fn member_kind(self) -> Option<MemberKind> {
    match self {
      Self::Object | Self::Interface => Some(MemberKind::Field),
      Self::InputObject => Some(MemberKind::InputField),
      Self::Enum => Some(MemberKind::EnumValue),
      Self::Directive => Some(MemberKind::Argument),
      Self::Scalar | Self::Union => None,
    }
  }
}

/// Where a definition comes from.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Origin {
  /// A supplied package's own definition.
  User,
  /// A definition of `core`, `alloc` or `std`: an embedded one, a materialised primitive, or a
  /// GraphQL built-in the root package replaces, which keeps its built-in's identity.
  BuiltIn,
  /// One of GraphQL's introspection meta-types, which a consumer may skip.
  Compat,
}

/// The meaning the language gives an intrinsic primitive of `core`.
///
/// A primitive is a definition like any other, and this is how a consumer knows which one it is
/// without matching on its spelling. `i32` and `Int` are two names of the definition whose
/// primitive is `Primitive::signed(32)`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Primitive {
  /// `bool`.
  Bool,
  /// `f32`.
  F32,
  /// `f64`.
  F64,
  /// `iN` or `uN`. Built with [`Primitive::signed`] and [`Primitive::unsigned`].
  #[non_exhaustive]
  Int {
    /// Whether it is `iN`, signed, rather than `uN`.
    signed: bool,
    /// Its width in bits: `N`.
    width: u32,
  },
}

impl Primitive {
  /// `iN`: the signed integer `width` bits wide.
  #[inline]
  pub const fn signed(width: u32) -> Self {
    Self::Int {
      signed: true,
      width,
    }
  }

  /// `uN`: the unsigned integer `width` bits wide.
  #[inline]
  pub const fn unsigned(width: u32) -> Self {
    Self::Int {
      signed: false,
      width,
    }
  }

  /// Its place among `core`'s materialised definitions: `bool`, `f32`, `f64`, then the signed
  /// widths ascending, then the unsigned widths ascending.
  pub(super) const fn rank(self) -> (u8, u32) {
    match self {
      Self::Bool => (0, 0),
      Self::F32 => (1, 0),
      Self::F64 => (2, 0),
      Self::Int {
        signed: true,
        width,
      } => (3, width),
      Self::Int {
        signed: false,
        width,
      } => (4, width),
    }
  }
}

/// The collection a definition stands for: what `@set` or `@map` makes of it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Collection {
  /// A set, through `@set`.
  Set,
  /// A map, through `@map`.
  Map,
}

/// Where a directive may be applied: one of the locations a directive definition lists.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum DirectiveLocation {
  /// `QUERY`.
  Query,
  /// `MUTATION`.
  Mutation,
  /// `SUBSCRIPTION`.
  Subscription,
  /// `FIELD`.
  Field,
  /// `FRAGMENT_DEFINITION`.
  FragmentDefinition,
  /// `FRAGMENT_SPREAD`.
  FragmentSpread,
  /// `INLINE_FRAGMENT`.
  InlineFragment,
  /// `VARIABLE_DEFINITION`.
  VariableDefinition,
  /// `SCHEMA`.
  Schema,
  /// `SCALAR`.
  Scalar,
  /// `OBJECT`.
  Object,
  /// `FIELD_DEFINITION`.
  FieldDefinition,
  /// `ARGUMENT_DEFINITION`.
  ArgumentDefinition,
  /// `INTERFACE`.
  Interface,
  /// `UNION`.
  Union,
  /// `ENUM`.
  Enum,
  /// `ENUM_VALUE`.
  EnumValue,
  /// `INPUT_OBJECT`.
  InputObject,
  /// `INPUT_FIELD_DEFINITION`.
  InputFieldDefinition,
  /// `DIRECTIVE_DEFINITION`.
  DirectiveDefinition,
}

/// A bound on one of a definition's type parameters: the `T: Node` of `where T: Node`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeBound {
  pub(super) parameter: u32,
  pub(super) interface: TypeExprId,
  pub(super) location: Location,
}

impl TypeBound {
  /// The bounded parameter: its index among its definition's type parameters.
  #[inline]
  pub const fn parameter(&self) -> u32 {
    self.parameter
  }

  /// The interface or marker the parameter must satisfy, as written.
  #[inline]
  pub const fn interface(&self) -> TypeExprId {
    self.interface
  }

  /// Where the bound is written.
  #[inline]
  pub const fn location(&self) -> Location {
    self.location
  }
}
