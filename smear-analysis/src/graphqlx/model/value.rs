//! Values: defaults, the arguments of applications, and every value nested in them.

use tokora::diagnostic::Location;

use crate::graphqlx::id::{DefId, MemberId, TyId, ValueId};

use super::{
  Model,
  table::{
    VALUE_BOOL, VALUE_BYTES, VALUE_ENUM, VALUE_FLOAT, VALUE_INT, VALUE_LIST, VALUE_LITERAL,
    VALUE_MAP, VALUE_NULL, VALUE_OBJECT, VALUE_SET, VALUE_STRING, VALUE_TYPE, VALUE_UNRESOLVED,
    ValueRow,
  },
  ty::Path,
};

/// The syntactic kind of a literal kept as written.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum LiteralKind {
  /// `true` or `false`.
  Boolean,
  /// A string or a block string.
  String,
  /// A float literal.
  Float,
  /// An integer literal.
  Int,
  /// A name or a path in value position: an enum value as written.
  Enum,
  /// `null`.
  Null,
  /// A list literal, `[…]`.
  List,
  /// A set literal.
  Set,
  /// A map literal.
  Map,
  /// An input object literal, `{…}`.
  Object,
}

/// One field of an input object value: the input field it sets, where the field's name is
/// written, and the field's value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ObjectEntry {
  pub(super) field: MemberId,
  pub(super) name: Option<Location>,
  pub(super) value: ValueId,
}

impl ObjectEntry {
  /// The input field it sets.
  #[inline]
  pub const fn field(&self) -> MemberId {
    self.field
  }

  /// Where the field's name is written, or `None` for a field a default filled.
  #[inline]
  pub const fn name(&self) -> Option<Location> {
    self.name
  }

  /// The field's value.
  #[inline]
  pub const fn value(&self) -> ValueId {
    self.value
  }
}

/// What one value is: the shape it was coerced to, by the type expected where it stands.
///
/// Three kinds of value are kept as written, as a [`Literal`] with nothing coerced beyond syntax: a
/// value of a custom scalar that is no collection, an extern one included, but `null`, which is
/// `null` where the scalar is nullable; a value whose expected type names a type parameter, which
/// only an instantiation can check; and a value whose expected type can take none, an output type
/// or a path that names nothing among them, which is another rule's or phase's to refuse.
#[derive(Debug, Clone, Copy, PartialEq)]
#[non_exhaustive]
pub enum ValueKind<'a> {
  /// `null`.
  Null,
  /// `true` or `false`.
  Bool(bool),
  /// An integer, of any width.
  Int(IntValue<'a>),
  /// A float.
  Float(FloatValue<'a>),
  /// A string, cooked: escapes decoded, and a block string's indentation removed.
  String(&'a str),
  /// A `bytes` value: its bytes, whichever literal spelled them, the UTF-8 encoding of a string or
  /// a list of integers from 0 to 255, so that two spellings of one sequence are one value.
  Bytes(&'a [u8]),
  /// A value of an enum.
  #[non_exhaustive]
  Enum {
    /// The enum.
    def: DefId,
    /// The value.
    value: MemberId,
  },
  /// A `TypeRef` value: a definition, never an instantiation, of any kind, named by a path resolved
  /// as a type where the value is written; a generic definition is named bare.
  #[non_exhaustive]
  Type {
    /// The definition it names.
    def: DefId,
  },
  /// A list, of its elements.
  List(&'a [ValueId]),
  /// A set, of its elements, in written order: a set literal where a set is expected, an extern
  /// set among them, each element coerced against the set's element type. An element written
  /// twice is held twice, and reported where it is written.
  Set(&'a [ValueId]),
  /// A map, of its entries, each a key and a value, in written order: a map literal where a map is
  /// expected, each key coerced against the map's key type and each value against its value type.
  /// A key written twice is held twice, and reported where it is written.
  Map(&'a [(ValueId, ValueId)]),
  /// An input object, of its fields, in the input object's order: each one its literal writes and
  /// the input object declares, one written twice twice, in written order, and each one it leaves
  /// out that has a default, a copy of the default; no absent one. A OneOf input object's fields
  /// left out are never filled.
  Object(&'a [ObjectEntry]),
  /// A value kept as written: of a custom scalar that is no collection, but `null`; of a type that
  /// names a type parameter; or of a type that can take no value.
  Literal(Literal<'a>),
  /// A value that did not coerce or resolve. Only an analysis that is not ok has one, within the
  /// extent of the error that says why.
  Unresolved,
}

/// An integer value, of any width: its sign, its magnitude and the literal as written.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IntValue<'a> {
  negative: bool,
  magnitude: &'a [u8],
  text: &'a str,
}

impl<'a> IntValue<'a> {
  /// Whether it is below zero.
  #[inline]
  pub const fn is_negative(&self) -> bool {
    self.negative
  }

  /// Its value, or `None` when it is outside `i128`'s range.
  pub fn to_i128(self) -> Option<i128> {
    let magnitude = small(self.magnitude)?;
    if self.negative {
      0i128.checked_sub_unsigned(magnitude)
    } else {
      i128::try_from(magnitude).ok()
    }
  }

  /// Its value, or `None` when it is negative or outside `u128`'s range.
  pub fn to_u128(self) -> Option<u128> {
    if self.negative {
      return None;
    }
    small(self.magnitude)
  }

  /// Its absolute value, little-endian, with no trailing zero byte: zero is no byte at all. This
  /// is how a width past 128 bits is read.
  #[inline]
  pub const fn magnitude(&self) -> &'a [u8] {
    self.magnitude
  }

  /// The literal as written.
  #[inline]
  pub const fn text(&self) -> &'a str {
    self.text
  }
}

/// A little-endian magnitude as a `u128`, or `None` when it is wider.
fn small(magnitude: &[u8]) -> Option<u128> {
  if magnitude.len() > 16 {
    return None;
  }
  let mut value: u128 = 0;
  for (index, byte) in magnitude.iter().enumerate() {
    value |= u128::from(*byte) << (8 * index);
  }
  Some(value)
}

/// A float value: the nearest `f64`, and the literal as written.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FloatValue<'a> {
  value: f64,
  text: &'a str,
}

impl<'a> FloatValue<'a> {
  /// The nearest `f64` to the literal.
  #[inline]
  pub const fn to_f64(self) -> f64 {
    self.value
  }

  /// The literal as written.
  #[inline]
  pub const fn text(&self) -> &'a str {
    self.text
  }
}

/// A value kept as written: its syntactic kind, its text and where it is written.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Literal<'a> {
  kind: LiteralKind,
  text: &'a str,
  location: Location,
}

impl<'a> Literal<'a> {
  /// Its syntactic kind.
  #[inline]
  pub const fn kind(&self) -> LiteralKind {
    self.kind
  }

  /// The literal as written.
  #[inline]
  pub const fn text(&self) -> &'a str {
    self.text
  }

  /// Where it is written.
  #[inline]
  pub const fn location(&self) -> Location {
    self.location
  }
}

impl Model {
  /// The value `id` names.
  ///
  /// # Panics
  ///
  /// If `id` is not one of this model's values: an id belongs to the analysis that issued it.
  #[inline]
  pub fn value(&self, id: ValueId) -> Value<'_> {
    assert!(
      (id.get() as usize) < self.values.len(),
      "value id {} is past the {} values of this model",
      id.get(),
      self.values.len()
    );
    Value { model: self, id }
  }
}

/// One value occurrence of the model: a default, an argument's value, or a value inside one.
///
/// `Copy`: it holds the model and an id, and everything it answers is borrowed from the model.
#[derive(Clone, Copy)]
pub struct Value<'a> {
  model: &'a Model,
  id: ValueId,
}

impl<'a> Value<'a> {
  /// Its id.
  #[inline]
  pub const fn id(&self) -> ValueId {
    self.id
  }

  /// What it is.
  //
  // Coercion gives a value its shape, each of the kind's.
  pub fn kind(&self) -> ValueKind<'a> {
    let model: &'a Model = self.model;
    let row = self.row();
    match row.shape {
      VALUE_NULL => ValueKind::Null,
      VALUE_BOOL => ValueKind::Bool(row.payload != 0),
      VALUE_INT => {
        let int = &model.ints[row.payload as usize];
        ValueKind::Int(IntValue {
          negative: int.negative,
          magnitude: int.magnitude.of(&model.magnitudes),
          text: int.text.text(&model.text),
        })
      }
      VALUE_FLOAT => {
        let float = &model.floats[row.payload as usize];
        ValueKind::Float(FloatValue {
          value: float.value,
          text: float.text.text(&model.text),
        })
      }
      VALUE_STRING => ValueKind::String(row.run.text(&model.text)),
      VALUE_BYTES => ValueKind::Bytes(row.run.of(&model.magnitudes)),
      VALUE_ENUM => ValueKind::Enum {
        def: row.def,
        value: row.member,
      },
      VALUE_TYPE => ValueKind::Type { def: row.def },
      VALUE_LIST => ValueKind::List(row.run.of(&model.value_items)),
      VALUE_SET => ValueKind::Set(row.run.of(&model.value_items)),
      VALUE_MAP => ValueKind::Map(row.run.of(&model.map_entries)),
      VALUE_OBJECT => ValueKind::Object(row.run.of(&model.object_entries)),
      VALUE_LITERAL => {
        let literal = &model.literals[row.payload as usize];
        ValueKind::Literal(Literal {
          kind: literal.kind,
          text: literal.text.text(&model.text),
          location: row.location,
        })
      }
      VALUE_UNRESOLVED => ValueKind::Unresolved,
      _ => ValueKind::Unresolved,
    }
  }

  /// Where it is written; for a value a default filled, where the default is written.
  #[inline]
  pub fn location(&self) -> Location {
    self.row().location
  }

  /// The type it was coerced against.
  //
  // Coercion gives it the type expected where it stands, with its `!`, the list a lone value
  // stands for included. A value whose expected type is unknown, which only a type that could not
  // be written has, answers the one unresolved type.
  #[inline]
  pub fn ty(&self) -> TyId {
    self.row().ty
  }

  /// Whether the author wrote it, rather than a default filling it. Every value inside a value a
  /// default filled was filled too.
  #[inline]
  pub fn written(&self) -> bool {
    self.row().written
  }

  /// The path a value written as a path writes, whatever it was coerced to: an enum value's, a type
  /// value's, and a literal's that is one. `None` for every other value, among them the lists a
  /// value written alone where a list is expected stands for.
  #[inline]
  pub fn path(&self) -> Option<Path<'a>> {
    let row = self.row().path?;
    Some(Path::of(self.model, row))
  }

  /// The model's record of this value.
  fn row(&self) -> &'a ValueRow {
    let model: &'a Model = self.model;
    &model.values[self.id.get() as usize]
  }
}

impl core::fmt::Debug for Value<'_> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("Value")
      .field("id", &self.id)
      .field("kind", &self.kind())
      .field("written", &self.written())
      .finish()
  }
}
