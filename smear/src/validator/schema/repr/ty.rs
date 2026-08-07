//! Type identity, type kinds, packed type references, and the reduced schema-side default.

use super::name::Sym;

/// An index into the schema's type table.
///
/// Like [`Sym`], a `TypeId` belongs to the schema that issued it and to no other.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(u32);

impl TypeId {
  /// Creates a type id from its raw index.
  #[inline]
  pub const fn new(index: u32) -> Self {
    Self(index)
  }

  /// Returns the raw index.
  #[inline]
  pub const fn get(&self) -> u32 {
    self.0
  }
}

/// One of GraphQL's six named type kinds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeKind {
  /// A scalar type, built-in or custom.
  Scalar,
  /// An object type.
  Object,
  /// An interface type.
  Interface,
  /// A union type.
  Union,
  /// An enum type.
  Enum,
  /// An input object type.
  InputObject,
}

impl TypeKind {
  /// Returns whether the kind is composite — object, interface or union.
  ///
  /// Composite types are the ones a selection set may be written against, and the ones that carry
  /// a possible-object bitset.
  #[inline]
  pub const fn is_composite(&self) -> bool {
    matches!(self, Self::Object | Self::Interface | Self::Union)
  }

  /// Returns whether the kind is abstract — interface or union.
  #[inline]
  pub const fn is_abstract(&self) -> bool {
    matches!(self, Self::Interface | Self::Union)
  }

  /// Returns whether the kind is a leaf — scalar or enum.
  #[inline]
  pub const fn is_leaf(&self) -> bool {
    matches!(self, Self::Scalar | Self::Enum)
  }

  /// Returns whether a value of this kind may appear as a field's result type
  /// (`IsOutputType`, draft §3.4).
  #[inline]
  pub const fn is_output(&self) -> bool {
    !matches!(self, Self::InputObject)
  }

  /// Returns whether a value of this kind may appear as an argument or input-field type
  /// (`IsInputType`, draft §3.4).
  #[inline]
  pub const fn is_input(&self) -> bool {
    matches!(self, Self::Scalar | Self::Enum | Self::InputObject)
  }

  /// Returns the introspection spelling of the kind (`__TypeKind`).
  #[inline]
  pub const fn as_str(&self) -> &'static str {
    match self {
      Self::Scalar => "SCALAR",
      Self::Object => "OBJECT",
      Self::Interface => "INTERFACE",
      Self::Union => "UNION",
      Self::Enum => "ENUM",
      Self::InputObject => "INPUT_OBJECT",
    }
  }
}

impl core::fmt::Display for TypeKind {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.write_str(self.as_str())
  }
}

/// The presence and null-ness of a schema-side default value.
///
/// Executable-document validation never needs a default's *content* — only whether one exists and
/// whether it is `null` (draft 5.4.3, 5.6.4, 5.8.5). Content is checked once, at build, against
/// the declared type; coercing an actual runtime value is execution's job. Reducing the default to
/// two bits is what keeps the schema source-independent: there is no borrowed literal left to
/// keep the SDL alive.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u8)]
pub enum DefaultKind {
  /// No `= value` clause at all.
  #[default]
  Absent = 0,
  /// `= null`.
  Null = 1,
  /// `= <anything other than null>`.
  NonNull = 2,
}

impl DefaultKind {
  /// Returns whether a default value was written.
  #[inline]
  pub const fn is_present(&self) -> bool {
    !matches!(self, Self::Absent)
  }

  /// Returns the reduction's spelling, for diagnostics.
  #[inline]
  pub const fn as_str(&self) -> &'static str {
    match self {
      Self::Absent => "absent",
      Self::Null => "null",
      Self::NonNull => "non-null",
    }
  }
}

impl core::fmt::Display for DefaultKind {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.write_str(self.as_str())
  }
}

/// The 2-bit wrapper code for `!`.
const NON_NULL: u32 = 0b01;
/// The 2-bit wrapper code for `[…]`.
const LIST: u32 = 0b10;

/// How many list/non-null wrappers a [`PackedType`] can carry.
///
/// Fifteen 2-bit codes fill 30 bits of the wrapper word. No real schema comes close; the builder
/// rejects a deeper reference rather than truncating it.
pub const MAX_WRAPPERS: u32 = 15;

/// A resolved type reference, flattened into three words.
///
/// The base type is stored twice over — once as the interned name and once as the resolved id — so
/// a diagnostic can render the reference without a table walk while a rule compares it with an
/// integer. `wrappers` packs the list and non-null modifiers as 2-bit codes **innermost first**,
/// terminated by the first zero pair, so `[[Int!]]!` is `NonNull, List, NonNull, List` reading
/// outward from `Int`.
///
/// Draft 5.8.5's `AreTypesCompatible` and 5.3.2's shape comparison become integer walks over this
/// word: no pointer chasing, no allocation, and equality of two references is `==`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PackedType {
  base: Sym,
  base_id: TypeId,
  wrappers: u32,
}

impl PackedType {
  /// Creates a bare reference to a named type, with no wrappers.
  #[inline]
  pub const fn named(base: Sym, base_id: TypeId) -> Self {
    Self {
      base,
      base_id,
      wrappers: 0,
    }
  }

  /// Creates a reference from a base type and a prebuilt wrapper word.
  ///
  /// The word must be well-formed: 2-bit codes `0b01`/`0b10` packed from the low end with no gaps.
  /// [`PackedType::push_list`] and [`PackedType::push_non_null`] are the checked way to build one.
  #[inline]
  pub const fn from_parts(base: Sym, base_id: TypeId, wrappers: u32) -> Self {
    Self {
      base,
      base_id,
      wrappers,
    }
  }

  /// Returns the interned name of the innermost named type.
  #[inline]
  pub const fn base(&self) -> Sym {
    self.base
  }

  /// Returns the resolved id of the innermost named type.
  #[inline]
  pub const fn base_id(&self) -> TypeId {
    self.base_id
  }

  /// Returns the raw wrapper word.
  #[inline]
  pub const fn wrappers(&self) -> u32 {
    self.wrappers
  }

  /// Returns how many wrappers the reference carries.
  #[inline]
  pub const fn depth(&self) -> u32 {
    if self.wrappers == 0 {
      0
    } else {
      // Every occupied slot holds `0b01` or `0b10`, never `0b00`, so the highest set bit locates
      // the outermost wrapper exactly.
      (31 - self.wrappers.leading_zeros()) / 2 + 1
    }
  }

  /// Wraps the reference in a list, returning `None` past [`MAX_WRAPPERS`].
  #[inline]
  pub const fn push_list(self) -> Option<Self> {
    self.push(LIST)
  }

  /// Marks the reference non-null, returning `None` past [`MAX_WRAPPERS`].
  #[inline]
  pub const fn push_non_null(self) -> Option<Self> {
    self.push(NON_NULL)
  }

  #[inline]
  const fn push(self, code: u32) -> Option<Self> {
    let depth = self.depth();
    if depth >= MAX_WRAPPERS {
      return None;
    }
    Some(Self {
      base: self.base,
      base_id: self.base_id,
      wrappers: self.wrappers | (code << (2 * depth)),
    })
  }

  /// Returns whether the reference is non-null at its outermost level.
  #[inline]
  pub const fn is_non_null(&self) -> bool {
    let depth = self.depth();
    depth != 0 && (self.wrappers >> (2 * (depth - 1))) & 0b11 == NON_NULL
  }

  /// Returns whether the reference is a list at its outermost level.
  #[inline]
  pub const fn is_list(&self) -> bool {
    let depth = self.depth();
    depth != 0 && (self.wrappers >> (2 * (depth - 1))) & 0b11 == LIST
  }

  /// Returns the reference with its outermost wrapper removed, or `None` when there is none.
  #[inline]
  pub const fn strip_outer(&self) -> Option<Self> {
    let depth = self.depth();
    if depth == 0 {
      return None;
    }
    Some(Self {
      base: self.base,
      base_id: self.base_id,
      wrappers: self.wrappers & !(0b11 << (2 * (depth - 1))),
    })
  }

  /// Returns the nullable form of the reference — itself, unless it is non-null.
  #[inline]
  pub const fn nullable(&self) -> Self {
    if self.is_non_null() {
      match self.strip_outer() {
        Some(inner) => inner,
        // Unreachable: `is_non_null` already established a wrapper exists.
        None => *self,
      }
    } else {
      *self
    }
  }

  /// Returns the item type of a list reference, or `None` when the reference is not a list.
  ///
  /// The reference must already be nullable; `[Int]!` is not a list at its outermost level, it is
  /// a non-null.
  #[inline]
  pub const fn list_item(&self) -> Option<Self> {
    if self.is_list() {
      self.strip_outer()
    } else {
      None
    }
  }

  /// Writes the reference's GraphQL spelling, resolving the base name through `name`.
  pub fn write(&self, f: &mut core::fmt::Formatter<'_>, name: &str) -> core::fmt::Result {
    let depth = self.depth();
    // Opening brackets, outermost first.
    let mut level = depth;
    while level > 0 {
      if (self.wrappers >> (2 * (level - 1))) & 0b11 == LIST {
        f.write_str("[")?;
      }
      level -= 1;
    }
    f.write_str(name)?;
    // Closing brackets and bangs, innermost first.
    let mut level = 0;
    while level < depth {
      match (self.wrappers >> (2 * level)) & 0b11 {
        LIST => f.write_str("]")?,
        _ => f.write_str("!")?,
      }
      level += 1;
    }
    Ok(())
  }
}
