//! What draft §4 introspection returns beyond a schema's shape: descriptions, deprecation,
//! `specifiedByURL` and the text of a default value.
//!
//! # Side tables, so the rows validation reads do not grow
//!
//! Every row here is aligned one to one with a table [`Schema`](super::Schema) already has — a
//! [`TypeMeta`] per type, a [`FieldMeta`] per field row, and so on — rather than widening the row
//! beside it. A validation rule reads names, kinds and packed types on its hot path and never a
//! description, so the bytes it walks stay the bytes it walked; an executor answering `__Type`
//! reads the row it wants by the same index.
//!
//! # One text arena, addressed by [`Text`]
//!
//! Every string these rows name lives in one arena the schema owns, separate from the name arena
//! on purpose: a name's symbol is a dense index that tables like `type_of_sym` are sized by, and a
//! description is not a name anything looks up.

/// A string in a schema's text arena, or the absence of one.
///
/// Read through [`Schema::text`](super::Schema::text). Like a [`Sym`](super::Sym), a handle belongs
/// to the schema that issued it and means nothing against another.
///
/// # Not a [`Range32`](super::Range32)
///
/// A range names rows, and an empty one is an empty list. A `Text` names a value draft §4 types
/// as a nullable `String`, and there *absent* is not *empty*: `""` as a description is a description
/// of zero bytes, while a definition with no description at all is [`Text::NONE`] and is returned as
/// `null`. The two have to stay apart, so the absent handle is a sentinel no arena issues rather
/// than the empty range at offset zero.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Text {
  start: u32,
  len: u32,
}

impl Text {
  /// No string: what draft §4 returns as `null`.
  ///
  /// The arena holds fewer than `u32::MAX` bytes, so no string it issues starts at this offset.
  pub const NONE: Self = Self {
    start: u32::MAX,
    len: 0,
  };

  /// Creates a handle from a byte offset and a length into a schema's text arena.
  #[inline]
  pub const fn new(start: u32, len: u32) -> Self {
    Self { start, len }
  }

  /// Returns whether this is [`Text::NONE`].
  #[inline]
  pub const fn is_none(&self) -> bool {
    self.start == u32::MAX
  }

  /// Returns whether this names a string — the empty one included.
  #[inline]
  pub const fn is_some(&self) -> bool {
    !self.is_none()
  }

  /// Borrows the string out of an arena: `None` for [`Text::NONE`], and for a handle that does not
  /// name a whole string in it — which only a handle from another arena can fail to.
  ///
  /// Crate-visible only: the arena is the schema's own, and [`Schema::text`](super::Schema::text)
  /// is the public reader of it.
  #[inline]
  pub(crate) fn slice<'a>(&self, arena: &'a str) -> Option<&'a str> {
    if self.is_none() {
      return None;
    }
    let start = self.start as usize;
    arena.get(start..start.checked_add(self.len as usize)?)
  }
}

impl Default for Text {
  /// [`Text::NONE`] — the absence of a string, not the empty one.
  #[inline]
  fn default() -> Self {
    Self::NONE
  }
}

/// Whether a field, an input value or an enum value is deprecated, and why.
///
/// Three answers, because draft §4 has three: not deprecated (`isDeprecated: false`,
/// `deprecationReason: null`); deprecated with a reason; and deprecated with `null` for a reason,
/// which `@deprecated(reason: null)` writes and which is `Deprecated(Text::NONE)`. A bare
/// `@deprecated` is the second of those, with the specification's default, `No longer supported`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum Deprecation {
  /// No `@deprecated` is applied.
  #[default]
  NotDeprecated,
  /// `@deprecated` is applied, and this is its reason — [`Text::NONE`] when the reason is `null`.
  Deprecated(Text),
}

impl Deprecation {
  /// Returns whether `@deprecated` is applied: draft §4's `isDeprecated`.
  #[inline]
  pub const fn is_deprecated(&self) -> bool {
    matches!(self, Self::Deprecated(_))
  }

  /// Returns the reason: draft §4's `deprecationReason`, [`Text::NONE`] when there is none.
  #[inline]
  pub const fn reason(&self) -> Text {
    match self {
      Self::Deprecated(reason) => *reason,
      Self::NotDeprecated => Text::NONE,
    }
  }
}

/// What `__Type` returns about a named type beyond its shape.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeMeta {
  description: Text,
  specified_by: Text,
}

impl TypeMeta {
  /// No description and no `@specifiedBy`.
  pub const EMPTY: Self = Self::new(Text::NONE, Text::NONE);

  /// Assembles a row.
  #[inline]
  pub const fn new(description: Text, specified_by: Text) -> Self {
    Self {
      description,
      specified_by,
    }
  }

  /// Returns the type's description.
  #[inline]
  pub const fn description(&self) -> Text {
    self.description
  }

  /// Returns the `url` of the `@specifiedBy` on a scalar — draft §4's `specifiedByURL` — and
  /// [`Text::NONE`] for every other type.
  #[inline]
  pub const fn specified_by(&self) -> Text {
    self.specified_by
  }
}

/// What `__Field` returns about a field beyond its shape.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldMeta {
  description: Text,
  deprecation: Deprecation,
}

impl FieldMeta {
  /// No description, not deprecated: what the three meta-fields carry.
  pub const EMPTY: Self = Self::new(Text::NONE, Deprecation::NotDeprecated);

  /// Assembles a row.
  #[inline]
  pub const fn new(description: Text, deprecation: Deprecation) -> Self {
    Self {
      description,
      deprecation,
    }
  }

  /// Returns the field's description.
  #[inline]
  pub const fn description(&self) -> Text {
    self.description
  }

  /// Returns whether the field is deprecated, and why.
  #[inline]
  pub const fn deprecation(&self) -> Deprecation {
    self.deprecation
  }
}

/// What `__InputValue` returns about an argument or an input-object field beyond its shape.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InputValueMeta {
  description: Text,
  deprecation: Deprecation,
  default_value: Text,
}

impl InputValueMeta {
  /// No description, not deprecated, no default.
  pub const EMPTY: Self = Self::new(Text::NONE, Deprecation::NotDeprecated, Text::NONE);

  /// Assembles a row.
  #[inline]
  pub const fn new(description: Text, deprecation: Deprecation, default_value: Text) -> Self {
    Self {
      description,
      deprecation,
      default_value,
    }
  }

  /// Returns the input value's description.
  #[inline]
  pub const fn description(&self) -> Text {
    self.description
  }

  /// Returns whether the input value is deprecated, and why.
  #[inline]
  pub const fn deprecation(&self) -> Deprecation {
    self.deprecation
  }

  /// Returns the default as GraphQL text — draft §4's `defaultValue` — or [`Text::NONE`] when
  /// none is declared.
  ///
  /// The text is canonical, printed once when the schema was built: lists as `[a, b]`, objects as
  /// `{a: 1, b: 2}`, every string re-encoded as a quoted string with its escapes normalised — a
  /// block string included — and numbers, enum values, booleans and `null` as the source spelled
  /// them. [`DefaultKind`](super::DefaultKind), on the input value's own row, is the two-bit
  /// reduction of the same declaration that validation reads.
  #[inline]
  pub const fn default_value(&self) -> Text {
    self.default_value
  }
}

/// What `__EnumValue` returns about an enum value beyond its name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EnumValueMeta {
  description: Text,
  deprecation: Deprecation,
}

impl EnumValueMeta {
  /// No description, not deprecated.
  pub const EMPTY: Self = Self::new(Text::NONE, Deprecation::NotDeprecated);

  /// Assembles a row.
  #[inline]
  pub const fn new(description: Text, deprecation: Deprecation) -> Self {
    Self {
      description,
      deprecation,
    }
  }

  /// Returns the enum value's description.
  #[inline]
  pub const fn description(&self) -> Text {
    self.description
  }

  /// Returns whether the enum value is deprecated, and why.
  #[inline]
  pub const fn deprecation(&self) -> Deprecation {
    self.deprecation
  }
}

/// What `__Directive` returns about a directive definition beyond its shape.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DirectiveMeta {
  description: Text,
}

impl DirectiveMeta {
  /// No description.
  pub const EMPTY: Self = Self::new(Text::NONE);

  /// Assembles a row.
  #[inline]
  pub const fn new(description: Text) -> Self {
    Self { description }
  }

  /// Returns the directive's description.
  #[inline]
  pub const fn description(&self) -> Text {
    self.description
  }
}
