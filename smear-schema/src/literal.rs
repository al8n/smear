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
//! the verdict [`fits_i32`] or [`is_finite`] returned where that literal was read. The executable
//! rules answer the same question in `smear-compiler`'s `executable::values::scalar_accepts`, over
//! the syntactic AST and against interned `Sym`s rather than names, with its own copies of
//! [`fits_i32`], `fits_id` and [`is_finite`]. Plain prose and not a link: that module is in a
//! crate ABOVE this one, so rustdoc cannot resolve it from here and a dev-dependency added to make
//! it resolve would put a cycle in the dev graph to satisfy a comment. Its only caller is
//! [`SchemaBuilder`](super::builder)'s constant-value check.
//!
//! The two doors read the range at different *times* and that is not a divergence: the request
//! door holds the document while it validates it, so it can read a spelling whenever it likes,
//! while this one outlives the document it was built from and would have to **store** anything it
//! wanted to read later. [`LiteralShape`]'s header carries what storing it cost.
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
///
/// # The two numeric arms carry a verdict, not a spelling
///
/// [`BuiltInScalar::accepts`] took the literal's retained source bytes beside the shape, and read
/// them for exactly two questions — [`fits_i32`] and [`is_finite`] — asked of exactly two arms.
/// Keeping the bytes to ask those questions later is what made a constant literal's spelling
/// something the builder had to *store*, one copy per distinct spelling, and a parse is not
/// injective into the bytes it reads: `B` parses of the `B` suffixes of one buffer through the
/// public `IntValue::graphql` mint `B` distinct spellings over the same `B` bytes, so the arena was
/// `B(B+1)/2`. See `SchemaBuilder`'s `RawShape` for the grid.
///
/// Both questions are answerable where the literal is read. Asking them there and carrying the
/// answer retains **zero** bytes and asks each question once per literal rather than once per
/// check. What it costs is that this type is no longer one byte and no longer a bare tag — the
/// `==` comparisons in `accepts` became patterns.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum LiteralShape {
  /// The `null` literal.
  Null,
  /// `true` or `false`.
  Boolean,
  /// An integer literal, with whether its spelling fit GraphQL's 32-bit signed range.
  Int {
    /// [`fits_i32`], decided where the literal was read.
    fits_i32: bool,
  },
  /// A floating-point literal, with whether its spelling named a finite double.
  Float {
    /// [`is_finite`], decided where the literal was read.
    is_finite: bool,
  },
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

  /// Returns whether this scalar accepts a literal of `shape`.
  ///
  /// The two range questions are read off [`LiteralShape`] rather than re-decided from a spelling
  /// the builder had to keep for the purpose; that type's header carries what keeping it cost.
  pub(crate) fn accepts(self, shape: LiteralShape) -> bool {
    match self {
      // The specification's `Int` input coercion takes integer literals only, in 32-bit range.
      Self::Int => matches!(shape, LiteralShape::Int { fits_i32: true }),
      // The coercion rules let an `Int` literal stand for a `Float`.
      Self::Float => match shape {
        LiteralShape::Float { is_finite } => is_finite,
        LiteralShape::Int { .. } => true,
        _ => false,
      },
      Self::String => shape == LiteralShape::String,
      Self::Boolean => shape == LiteralShape::Boolean,
      // `ID` accepts both spellings an identifier is written with, and its integer arm is the
      // *same* range an `Int` literal is allowed to carry in the first place — which is why it
      // reads the same verdict rather than a `fits_id` of its own.
      Self::ID => match shape {
        LiteralShape::String => true,
        LiteralShape::Int { fits_i32 } => fits_i32,
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

/// Whether a `Float` literal's spelling names a finite double.
pub(crate) fn is_finite(spelling: &[u8]) -> bool {
  core::str::from_utf8(spelling).is_ok_and(|text| text.parse::<f64>().is_ok_and(f64::is_finite))
}

#[cfg(test)]
mod tests {
  use super::{BuiltInScalar, LiteralShape, fits_i32, is_finite};

  /// An `Int` literal's shape, with the range verdict the reduction decides.
  fn int(spelling: &[u8]) -> LiteralShape {
    LiteralShape::Int {
      fits_i32: fits_i32(spelling),
    }
  }

  /// A `Float` literal's shape, likewise.
  fn float(spelling: &[u8]) -> LiteralShape {
    LiteralShape::Float {
      is_finite: is_finite(spelling),
    }
  }

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
    assert!(BuiltInScalar::Float.accepts(int(b"1")));
    assert!(BuiltInScalar::ID.accepts(int(b"4")));
    assert!(BuiltInScalar::ID.accepts(LiteralShape::String));
    assert!(!BuiltInScalar::Int.accepts(float(b"1.0")));
    assert!(!BuiltInScalar::String.accepts(int(b"4")));
    assert!(!BuiltInScalar::Boolean.accepts(int(b"1")));
    assert!(!BuiltInScalar::Int.accepts(LiteralShape::String));

    // No built-in scalar takes a container or a bare enum member.
    for scalar in [
      BuiltInScalar::Int,
      BuiltInScalar::Float,
      BuiltInScalar::String,
      BuiltInScalar::Boolean,
      BuiltInScalar::ID,
    ] {
      for shape in [LiteralShape::List, LiteralShape::Object, LiteralShape::Enum] {
        assert!(!scalar.accepts(shape), "{scalar:?} took {shape:?}");
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

    assert!(!BuiltInScalar::Int.accepts(int(b"2147483648")));
    assert!(BuiltInScalar::Float.accepts(int(b"2147483648")));

    // `ID`'s integer arm is the *same* range, and it is the branch `accepts` reaches only from
    // inside the range in the case above. A completeness audit forced that arm to `true` and every
    // gate in the repository stayed green, including a differential oracle over six hundred
    // documents — apollo-compiler 1.32.0 does not range-check `ID` at all, so nothing differential
    // can ever see it. These assertions and
    // `values_of_correct_type_reads_the_whole_coercion_table` in `tests/validator_rules.rs` are
    // what make the branch reachable by a gate.
    assert!(BuiltInScalar::ID.accepts(int(b"2147483647")));
    assert!(BuiltInScalar::ID.accepts(int(b"-2147483648")));
    assert!(!BuiltInScalar::ID.accepts(int(b"2147483648")));
    assert!(!BuiltInScalar::ID.accepts(int(b"-2147483649")));
    assert!(!BuiltInScalar::ID.accepts(int(b"99999999999999")));
    // A *string* `ID` is unbounded, which is what the range arm must not accidentally constrain.
    assert!(BuiltInScalar::ID.accepts(LiteralShape::String));

    assert!(is_finite(b"1.0"));
    assert!(is_finite(b"-1.5e3"));
    assert!(!is_finite(b"1e400"));
    assert!(!BuiltInScalar::Float.accepts(float(b"1e400")));
  }
}
