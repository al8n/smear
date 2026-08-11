//! The nineteen draft directive locations, and the bitmask a directive definition carries.

/// One of the draft grammar's nineteen `DirectiveLocation`s.
///
/// The discriminants are the bit positions used by [`DirectiveLocations`], so the whole of draft
/// 5.7.2 ("Directives Are In Valid Locations") is `locations.contains(loc)` — one shift and one
/// `AND` against a word computed at build time.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum DirectiveLocation {
  /// `QUERY`
  Query = 0,
  /// `MUTATION`
  Mutation = 1,
  /// `SUBSCRIPTION`
  Subscription = 2,
  /// `FIELD`
  Field = 3,
  /// `FRAGMENT_DEFINITION`
  FragmentDefinition = 4,
  /// `FRAGMENT_SPREAD`
  FragmentSpread = 5,
  /// `INLINE_FRAGMENT`
  InlineFragment = 6,
  /// `VARIABLE_DEFINITION`
  VariableDefinition = 7,
  /// `SCHEMA`
  Schema = 8,
  /// `SCALAR`
  Scalar = 9,
  /// `OBJECT`
  Object = 10,
  /// `FIELD_DEFINITION`
  FieldDefinition = 11,
  /// `ARGUMENT_DEFINITION`
  ArgumentDefinition = 12,
  /// `INTERFACE`
  Interface = 13,
  /// `UNION`
  Union = 14,
  /// `ENUM`
  Enum = 15,
  /// `ENUM_VALUE`
  EnumValue = 16,
  /// `INPUT_OBJECT`
  InputObject = 17,
  /// `INPUT_FIELD_DEFINITION`
  InputFieldDefinition = 18,
}

impl DirectiveLocation {
  /// Every location, in grammar order.
  ///
  /// Iterating this is how a test enumerates the space rather than restating it.
  pub const ALL: [Self; 19] = [
    Self::Query,
    Self::Mutation,
    Self::Subscription,
    Self::Field,
    Self::FragmentDefinition,
    Self::FragmentSpread,
    Self::InlineFragment,
    Self::VariableDefinition,
    Self::Schema,
    Self::Scalar,
    Self::Object,
    Self::FieldDefinition,
    Self::ArgumentDefinition,
    Self::Interface,
    Self::Union,
    Self::Enum,
    Self::EnumValue,
    Self::InputObject,
    Self::InputFieldDefinition,
  ];

  /// Returns the location's bit position in a [`DirectiveLocations`] mask.
  #[inline]
  pub const fn bit(&self) -> u32 {
    *self as u8 as u32
  }

  /// Returns the location's canonical GraphQL spelling.
  #[inline]
  pub const fn as_str(&self) -> &'static str {
    match self {
      Self::Query => "QUERY",
      Self::Mutation => "MUTATION",
      Self::Subscription => "SUBSCRIPTION",
      Self::Field => "FIELD",
      Self::FragmentDefinition => "FRAGMENT_DEFINITION",
      Self::FragmentSpread => "FRAGMENT_SPREAD",
      Self::InlineFragment => "INLINE_FRAGMENT",
      Self::VariableDefinition => "VARIABLE_DEFINITION",
      Self::Schema => "SCHEMA",
      Self::Scalar => "SCALAR",
      Self::Object => "OBJECT",
      Self::FieldDefinition => "FIELD_DEFINITION",
      Self::ArgumentDefinition => "ARGUMENT_DEFINITION",
      Self::Interface => "INTERFACE",
      Self::Union => "UNION",
      Self::Enum => "ENUM",
      Self::EnumValue => "ENUM_VALUE",
      Self::InputObject => "INPUT_OBJECT",
      Self::InputFieldDefinition => "INPUT_FIELD_DEFINITION",
    }
  }

  /// Resolves a location from its canonical spelling.
  ///
  /// This is the seam the dialect side crosses: the parser's own location enums render through
  /// `as_str`, so the mapping needs no dependency in either direction.
  pub fn from_name(name: &str) -> Option<Self> {
    Some(match name {
      "QUERY" => Self::Query,
      "MUTATION" => Self::Mutation,
      "SUBSCRIPTION" => Self::Subscription,
      "FIELD" => Self::Field,
      "FRAGMENT_DEFINITION" => Self::FragmentDefinition,
      "FRAGMENT_SPREAD" => Self::FragmentSpread,
      "INLINE_FRAGMENT" => Self::InlineFragment,
      "VARIABLE_DEFINITION" => Self::VariableDefinition,
      "SCHEMA" => Self::Schema,
      "SCALAR" => Self::Scalar,
      "OBJECT" => Self::Object,
      "FIELD_DEFINITION" => Self::FieldDefinition,
      "ARGUMENT_DEFINITION" => Self::ArgumentDefinition,
      "INTERFACE" => Self::Interface,
      "UNION" => Self::Union,
      "ENUM" => Self::Enum,
      "ENUM_VALUE" => Self::EnumValue,
      "INPUT_OBJECT" => Self::InputObject,
      "INPUT_FIELD_DEFINITION" => Self::InputFieldDefinition,
      _ => return None,
    })
  }
}

impl core::fmt::Display for DirectiveLocation {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.write_str(self.as_str())
  }
}

/// A set of directive locations, as a bitmask over [`DirectiveLocation`] discriminants.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub struct DirectiveLocations(u32);

impl DirectiveLocations {
  /// The empty set.
  pub const EMPTY: Self = Self(0);

  /// Creates a set from a raw mask.
  #[inline]
  pub const fn from_bits(bits: u32) -> Self {
    Self(bits)
  }

  /// Returns the raw mask.
  #[inline]
  pub const fn bits(&self) -> u32 {
    self.0
  }

  /// Returns the set with `location` added.
  #[inline]
  pub const fn with(self, location: DirectiveLocation) -> Self {
    Self(self.0 | (1u32 << location.bit()))
  }

  /// Adds `location` in place.
  #[inline]
  pub const fn insert(&mut self, location: DirectiveLocation) {
    self.0 |= 1u32 << location.bit();
  }

  /// Returns whether the set contains `location` — the whole of draft 5.7.2.
  #[inline]
  pub const fn contains(&self, location: DirectiveLocation) -> bool {
    self.0 & (1u32 << location.bit()) != 0
  }

  /// Returns whether the set is empty.
  #[inline]
  pub const fn is_empty(&self) -> bool {
    self.0 == 0
  }

  /// Iterates the set's locations in grammar order.
  pub fn iter(&self) -> impl Iterator<Item = DirectiveLocation> + '_ {
    DirectiveLocation::ALL
      .into_iter()
      .filter(|location| self.contains(*location))
  }
}

impl FromIterator<DirectiveLocation> for DirectiveLocations {
  fn from_iter<T: IntoIterator<Item = DirectiveLocation>>(iter: T) -> Self {
    iter.into_iter().fold(Self::EMPTY, Self::with)
  }
}
