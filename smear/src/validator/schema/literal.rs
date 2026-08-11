//! What a literal *is*, and which built-in scalars accept it — **on the build door**.
//!
//! # One question, two implementations, and a test that keeps them honest
//!
//! Draft 5.6.1 ("Values of Correct Type") and the use-site value rule §3.13 asks about a directive
//! argument are the same question — does this literal fit this type? — asked once per request and
//! once per build. The parts that differ are the traversal and the diagnostic; the part that must
//! *not* differ is the answer, because a schema whose directive arguments were accepted at build
//! and a query whose arguments are accepted per request have to agree about what an `Int` is.
//!
//! It would be tidier if that meant one table. It does not. This module is the **builder's**
//! decision, reduced to what it depends on: the shape of the literal ([`LiteralShape`]), which
//! built-in scalar it is being offered to ([`BuiltInScalar`]), and — for the two numeric ranges —
//! the literal's retained spelling. The executable rules answer the same question in
//! [`validator::executable::values`](crate::validator)'s `scalar_accepts`, over the syntactic AST
//! and against interned `Sym`s rather than names, with its own copies of [`fits_i32`], [`fits_id`]
//! and [`is_finite`]. Its only caller is
//! [`SchemaBuilder`](super::builder)'s constant-value check.
//!
//! Two implementations of one paragraph can drift, and the drift is invisible from either side —
//! a completeness audit forced this module's `ID` range arm to `true` and the whole gate set,
//! differential oracle included, stayed green, because the SDL door's fixtures never offered an
//! out-of-range `ID`. `the_two_coercion_tables_agree` in `tests/validator_rules.rs` is what makes
//! that a red now: it runs every literal shape past every built-in scalar through *both* doors and
//! requires the same verdict. Anything added here has to be added there, and the test is what says
//! so.
//!
//! # Built-in-ness is decided by name
//!
//! A document may spell `scalar String` out — a printed schema does — and it is still the
//! specification's `String`. [`BuiltInScalar::from_name`] therefore reads the name and not a
//! provenance flag. Everything it does not recognise is a custom scalar, and a custom scalar
//! accepts any literal: only the service knows how to read one, so a validator that guessed would
//! reject valid documents.

/// The eight shapes a GraphQL input literal takes, with the variable arm left out.
///
/// A variable is not a shape a coercion table can rule on — whether it fits is draft 5.8.5's
/// question about its *declaration* — so callers resolve that arm before they get here.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum LiteralShape {
  /// The `null` literal.
  Null,
  /// `true` or `false`.
  Boolean,
  /// An integer literal.
  Int,
  /// A floating-point literal.
  Float,
  /// A string literal, inline or block.
  String,
  /// An unquoted enum member name.
  Enum,
  /// A list literal.
  List,
  /// An object literal.
  Object,
}

/// One of the five scalars the specification provides.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum BuiltInScalar {
  /// `Int`.
  Int,
  /// `Float`.
  Float,
  /// `String`.
  String,
  /// `Boolean`.
  Boolean,
  /// `ID`.
  ID,
}

impl BuiltInScalar {
  /// Resolves a scalar name, or `None` for a custom scalar.
  #[inline]
  pub(crate) fn from_name(name: &[u8]) -> Option<Self> {
    Some(match name {
      b"Int" => Self::Int,
      b"Float" => Self::Float,
      b"String" => Self::String,
      b"Boolean" => Self::Boolean,
      b"ID" => Self::ID,
      _ => return None,
    })
  }

  /// Returns whether this scalar accepts a literal of `shape` spelled `spelling`.
  ///
  /// `spelling` is the literal's retained source bytes, and is read only for `Int` and `Float`,
  /// whose acceptance depends on range rather than on shape. Anything else may pass an empty
  /// slice.
  pub(crate) fn accepts(self, shape: LiteralShape, spelling: &[u8]) -> bool {
    match self {
      // The specification's `Int` input coercion takes integer literals only, in 32-bit range.
      Self::Int => shape == LiteralShape::Int && fits_i32(spelling),
      // The coercion rules let an `Int` literal stand for a `Float`.
      Self::Float => match shape {
        LiteralShape::Float => is_finite(spelling),
        LiteralShape::Int => true,
        _ => false,
      },
      Self::String => shape == LiteralShape::String,
      Self::Boolean => shape == LiteralShape::Boolean,
      // `ID` accepts both spellings an identifier is written with.
      Self::ID => match shape {
        LiteralShape::String => true,
        LiteralShape::Int => fits_id(spelling),
        _ => false,
      },
    }
  }
}

/// Whether an `Int` literal's spelling fits GraphQL's 32-bit signed range.
///
/// Ten characters of digits is the first length that can overflow, so the common case is a length
/// check and nothing else.
pub(crate) fn fits_i32(spelling: &[u8]) -> bool {
  let digits = spelling.strip_prefix(b"-").unwrap_or(spelling);
  if digits.len() <= 9 {
    return !digits.is_empty();
  }
  core::str::from_utf8(spelling).is_ok_and(|text| text.parse::<i32>().is_ok())
}

/// Whether an `Int` literal may stand for an `ID`.
///
/// The specification's `ID` input coercion accepts integer values, and the range that matters is
/// the one an `Int` literal is allowed to carry in the first place.
#[inline]
pub(crate) fn fits_id(spelling: &[u8]) -> bool {
  fits_i32(spelling)
}

/// Whether a `Float` literal's spelling names a finite double.
pub(crate) fn is_finite(spelling: &[u8]) -> bool {
  core::str::from_utf8(spelling).is_ok_and(|text| text.parse::<f64>().is_ok_and(f64::is_finite))
}

#[cfg(test)]
mod tests {
  use super::{BuiltInScalar, LiteralShape, fits_i32, is_finite};

  #[test]
  fn names_resolve_only_the_five() {
    for (name, expected) in [
      (&b"Int"[..], Some(BuiltInScalar::Int)),
      (b"Float", Some(BuiltInScalar::Float)),
      (b"String", Some(BuiltInScalar::String)),
      (b"Boolean", Some(BuiltInScalar::Boolean)),
      (b"ID", Some(BuiltInScalar::ID)),
      (b"URL", None),
      (b"int", None),
      (b"", None),
    ] {
      assert_eq!(BuiltInScalar::from_name(name), expected, "{name:?}");
    }
  }

  #[test]
  fn coercion_follows_the_specification() {
    // The two widenings the specification grants, and nothing else.
    assert!(BuiltInScalar::Float.accepts(LiteralShape::Int, b"1"));
    assert!(BuiltInScalar::ID.accepts(LiteralShape::Int, b"4"));
    assert!(BuiltInScalar::ID.accepts(LiteralShape::String, b""));
    assert!(!BuiltInScalar::Int.accepts(LiteralShape::Float, b"1.0"));
    assert!(!BuiltInScalar::String.accepts(LiteralShape::Int, b"4"));
    assert!(!BuiltInScalar::Boolean.accepts(LiteralShape::Int, b"1"));
    assert!(!BuiltInScalar::Int.accepts(LiteralShape::String, b""));

    // No built-in scalar takes a container or a bare enum member.
    for scalar in [
      BuiltInScalar::Int,
      BuiltInScalar::Float,
      BuiltInScalar::String,
      BuiltInScalar::Boolean,
      BuiltInScalar::ID,
    ] {
      for shape in [LiteralShape::List, LiteralShape::Object, LiteralShape::Enum] {
        assert!(!scalar.accepts(shape, b""), "{scalar:?} took {shape:?}");
      }
    }
  }

  #[test]
  fn ranges_are_read_from_the_retained_spelling() {
    assert!(fits_i32(b"0"));
    assert!(fits_i32(b"-1"));
    assert!(fits_i32(b"999999999"));
    assert!(fits_i32(b"2147483647"));
    assert!(fits_i32(b"-2147483648"));
    assert!(!fits_i32(b"2147483648"));
    assert!(!fits_i32(b"-2147483649"));
    assert!(!fits_i32(b"99999999999999999999"));

    assert!(!BuiltInScalar::Int.accepts(LiteralShape::Int, b"2147483648"));
    assert!(BuiltInScalar::Float.accepts(LiteralShape::Int, b"2147483648"));

    // `ID`'s integer arm is the *same* range, and it is the branch `accepts` reaches only from
    // inside the range in the case above. A completeness audit forced that arm to `true` and every
    // gate in the repository stayed green, including a differential oracle over six hundred
    // documents — apollo-compiler 1.32.0 does not range-check `ID` at all, so nothing differential
    // can ever see it. These assertions and
    // `values_of_correct_type_reads_the_whole_coercion_table` in `tests/validator_rules.rs` are
    // what make the branch reachable by a gate.
    assert!(BuiltInScalar::ID.accepts(LiteralShape::Int, b"2147483647"));
    assert!(BuiltInScalar::ID.accepts(LiteralShape::Int, b"-2147483648"));
    assert!(!BuiltInScalar::ID.accepts(LiteralShape::Int, b"2147483648"));
    assert!(!BuiltInScalar::ID.accepts(LiteralShape::Int, b"-2147483649"));
    assert!(!BuiltInScalar::ID.accepts(LiteralShape::Int, b"99999999999999"));
    // A *string* `ID` is unbounded, which is what the range arm must not accidentally constrain.
    assert!(BuiltInScalar::ID.accepts(LiteralShape::String, b"99999999999999"));

    assert!(is_finite(b"1.0"));
    assert!(is_finite(b"-1.5e3"));
    assert!(!is_finite(b"1e400"));
    assert!(!BuiltInScalar::Float.accepts(LiteralShape::Float, b"1e400"));
  }
}
