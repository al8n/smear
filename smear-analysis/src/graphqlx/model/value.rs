//! Values: defaults, the arguments of applications, and every value nested in them.

use tokora::diagnostic::Location;

use crate::graphqlx::id::{MemberId, ValueId};

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
