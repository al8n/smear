//! Members: the fields, arguments, input fields and enum values of definitions.

/// What kind of member a member is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum MemberKind {
  /// A field of an object or an interface.
  Field,
  /// An argument of a field, or of a directive definition.
  Argument,
  /// A field of an input object.
  InputField,
  /// A value of an enum.
  EnumValue,
}
