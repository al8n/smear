//! The one axis the two value parsers differ on: what an `Int` and a `Float` leaf carry.
//!
//! The composite value productions in this module tree are generic over [`Numbers`] and
//! monomorphic in nothing else. [`SliceNumbers`] keeps the source slice and is what
//! [`value`](super::value) and its siblings resolve to; [`MaterializedNumbers`] converts to
//! [`i64`] and [`f64`] and is what [`materialized::value`](super::materialized::value) resolves
//! to; [`MaterializedNumbers32`] converts to [`i32`] and [`f64`] and is what
//! [`materialized32::value`](crate::graphql::syntactic::value::materialized32::value) resolves
//! to. The three parsers are
//! then the *same* parser — same commit points, same recovery, the same error at every position
//! but the two leaves — which is a property held by construction rather than by three files
//! being kept in step. Value modules drifting apart is the failure this shape is chosen against.
//!
//! # Two materialised widths, because GraphQL has two honest answers
//!
//! Draft §3.5.1 says `Int` is a signed 32-bit integer. Draft §2.9.1's grammar for an `IntValue`
//! says nothing about how many digits it may have. So `2147483648` is a well-formed `IntValue`
//! that no conformant `Int` can hold, and the two markers are the two readings of that: one
//! answers *what does this document mean under the specification*, the other *what did the author
//! write*. A marker per reading, and never a width parameter on one marker — a parameterised
//! marker would make "which reading" an inference variable at every call site that names it.
//!
//! # `Error` is how the slice parser stays unable to overflow
//!
//! [`SliceNumbers::Error`](Numbers::Error) is [`Infallible`]. The failure arm of every
//! conversion is therefore an **uninhabited** match in the slice parser — `match error {}`, no
//! runtime branch, nothing to report — and the compiler is what certifies it, rather than a
//! comment. It is also why the two out-of-range error *constructors* can be gated on
//! `materialized-numbers` while their variants stay unconditional: the generic code never names
//! either variant — it reaches them only through [`Numbers::report`] — so a build without the
//! feature compiles no path to them and needs none, without those two names leaving the default
//! error surface.
//!
//! # Why the failure carries the slice back
//!
//! [`Numbers::int`] takes the slice **by value** and the failure carries it. The caller needs the
//! literal's spelling for the error it raises and needs to *not* pay for a clone on the path that
//! succeeds; handing the slice back on failure is the only shape with both. A `&S` argument would
//! force [`SliceNumbers`] — whose `Int` *is* `S` — to clone on every successful number in the
//! document.

use core::convert::Infallible;

use tokora::SimpleSpan;

#[cfg(feature = "materialized-numbers")]
use crate::graphql::ast::{
  materialized::{
    ConstInputValue as MaterializedConstInputValue, InputValue as MaterializedInputValue,
  },
  materialized32::{
    ConstInputValue as Materialized32ConstInputValue, InputValue as Materialized32InputValue,
  },
};
#[cfg(feature = "materialized-numbers")]
use crate::graphql::error::IntWidth;
use crate::graphql::{
  ast::{ConstInputValue, DefaultVec, InputValue, Name},
  error::GraphqlError as DialectGraphqlError,
};

/// What an `Int` and a `Float` leaf carry, how a literal's slice becomes one, and which value
/// tree the composite productions assemble.
///
/// `S` is the source slice the lexer produced for the literal.
///
/// # Why the tree is an associated type and not a third parameter
///
/// The trees are separate `enum`s rather than one enum at several instantiations, because a type
/// alias cannot be used as a module and `use ast::InputValue::{Int, String}` has to keep
/// compiling — [`ast::materialized`](crate::graphql::ast::materialized) records the whole
/// argument and [`ast::materialized32`](crate::graphql::ast::materialized32) records what a
/// second width adds to it. "Which tree" is therefore a property of the marker, exactly as "which
/// payload" already was, and belongs in the same place. The shared bodies name [`Numbers::Value`]
/// and convert a leaf into it, so no tree is spelled in a body that serves several.
pub(crate) trait Numbers<S> {
  /// The payload of an `IntValue` node.
  type Int;
  /// The payload of a `FloatValue` node.
  type Float;
  /// How a conversion can fail, carrying whatever the report needs.
  type Error;
  /// The input-value tree this marker assembles.
  type Value;
  /// The constant input-value tree this marker assembles.
  type ConstValue;

  /// Builds the `Int` payload.
  fn int(slice: S) -> Result<Self::Int, Self::Error>;

  /// Builds the `Float` payload.
  fn float(slice: S) -> Result<Self::Float, Self::Error>;

  /// Turns a conversion failure into the dialect error that reports it.
  fn report(error: Self::Error, span: SimpleSpan) -> DialectGraphqlError<S>;
}

/// The list node a marker's value tree holds.
pub(crate) type ValueList<S, N> =
  crate::value::List<<N as Numbers<S>>::Value, SimpleSpan, DefaultVec<<N as Numbers<S>>::Value>>;

/// One object field of a marker's value tree.
pub(crate) type ValueObjectField<S, N> =
  crate::value::ObjectField<Name<S>, <N as Numbers<S>>::Value>;

/// The object node a marker's value tree holds.
pub(crate) type ValueObject<S, N> = crate::value::Object<
  Name<S>,
  <N as Numbers<S>>::Value,
  SimpleSpan,
  DefaultVec<ValueObjectField<S, N>>,
>;

/// The list node a marker's constant value tree holds.
pub(crate) type ConstValueList<S, N> = crate::value::List<
  <N as Numbers<S>>::ConstValue,
  SimpleSpan,
  DefaultVec<<N as Numbers<S>>::ConstValue>,
>;

/// One object field of a marker's constant value tree.
pub(crate) type ConstValueObjectField<S, N> =
  crate::value::ObjectField<Name<S>, <N as Numbers<S>>::ConstValue>;

/// The object node a marker's constant value tree holds.
pub(crate) type ConstValueObject<S, N> = crate::value::Object<
  Name<S>,
  <N as Numbers<S>>::ConstValue,
  SimpleSpan,
  DefaultVec<ConstValueObjectField<S, N>>,
>;

/// The default-value node a marker's constant value tree holds.
pub(crate) type ValueDefault<S, N> = crate::value::DefaultInputValue<<N as Numbers<S>>::ConstValue>;

/// The payload the parser has always produced: the literal's source slice, unconverted.
///
/// Both conversions are the identity and neither can fail, so a production monomorphised against
/// this marker compiles to what it compiled to before the parameter existed.
pub(crate) enum SliceNumbers {}

impl<S> Numbers<S> for SliceNumbers {
  type Int = S;
  type Float = S;
  type Error = Infallible;
  type Value = InputValue<S>;
  type ConstValue = ConstInputValue<S>;

  #[inline(always)]
  fn int(slice: S) -> Result<S, Infallible> {
    Ok(slice)
  }

  #[inline(always)]
  fn float(slice: S) -> Result<S, Infallible> {
    Ok(slice)
  }

  #[inline(always)]
  fn report(error: Infallible, _: SimpleSpan) -> DialectGraphqlError<S> {
    match error {}
  }
}

/// A numeric literal that is valid GraphQL and does not fit the materialised payload.
///
/// It carries the literal's slice back out of the conversion so the error can name the spelling
/// the document used, and — for an integer — the [`IntWidth`] the conversion was attempted at.
///
/// # Why the width travels with the failure rather than being read off the marker
///
/// A `const WIDTH` on [`Numbers`] would have to be answered by [`SliceNumbers`] too, which
/// converts nothing and has no width; whatever it answered would be a value no code can reach
/// and every reader has to interpret. The width is a property of *this failure*, produced at the
/// one place that knows it, and [`SliceNumbers`] never constructs one because its `Error` is
/// [`Infallible`].
#[cfg(feature = "materialized-numbers")]
pub(crate) enum OutOfRange<S> {
  /// An `IntValue` outside the named width.
  Int {
    /// The literal's source spelling.
    value: S,
    /// The width the conversion was attempted at.
    width: IntWidth,
  },
  /// A `FloatValue` that is not a finite [`f64`].
  Float(S),
}

/// The one report body both materialising markers use.
///
/// Shared rather than written twice on purpose: the two markers differ in what an `Int` becomes
/// and in nothing else, so a divergence here would be a divergence in how the *same* failure is
/// reported at two widths — which is the defect the width-naming exists to rule out.
#[cfg(feature = "materialized-numbers")]
#[inline]
fn report_out_of_range<S>(error: OutOfRange<S>, span: SimpleSpan) -> DialectGraphqlError<S> {
  match error {
    OutOfRange::Int { value, width } => DialectGraphqlError::int_overflow(value, width, span),
    OutOfRange::Float(slice) => DialectGraphqlError::float_overflow(slice, span),
  }
}

/// [`i64`] and [`f64`], read out of the literal's bytes with no allocation anywhere.
///
/// **The grammar-permissive reading.** Draft §2.9.1's `IntValue` puts no bound on its digits, and
/// this marker accepts every literal a 64-bit signed integer can hold — including the ones
/// draft §3.5.1's 32-bit `Int` cannot. [`MaterializedNumbers32`] is the spec-exact reading beside
/// it; neither is a subset of "correct", they answer two different questions about a document.
///
/// `AsRef<[u8]>` is this crate's established spelling for "a slice whose text can be read", and
/// every source backing the crate ships satisfies it. It appears here and not on the slice
/// parser because only this one looks *inside* a literal.
#[cfg(feature = "materialized-numbers")]
pub(crate) enum MaterializedNumbers {}

#[cfg(feature = "materialized-numbers")]
impl<S> Numbers<S> for MaterializedNumbers
where
  S: AsRef<[u8]>,
{
  type Int = i64;
  type Float = f64;
  type Error = OutOfRange<S>;
  type Value = MaterializedInputValue<S>;
  type ConstValue = MaterializedConstInputValue<S>;

  #[inline]
  fn int(slice: S) -> Result<i64, OutOfRange<S>> {
    match parse_i64(slice.as_ref()) {
      Some(value) => Ok(value),
      None => Err(OutOfRange::Int {
        value: slice,
        width: IntWidth::I64,
      }),
    }
  }

  #[inline]
  fn float(slice: S) -> Result<f64, OutOfRange<S>> {
    match parse_f64(slice.as_ref()) {
      Some(value) => Ok(value),
      None => Err(OutOfRange::Float(slice)),
    }
  }

  #[inline]
  fn report(error: OutOfRange<S>, span: SimpleSpan) -> DialectGraphqlError<S> {
    report_out_of_range(error, span)
  }
}

/// [`i32`] and [`f64`] — **the specification's own reading of `Int`**.
///
/// Draft §3.5.1 defines GraphQL's `Int` as a signed 32-bit integer, so this marker is not the
/// narrow option beside [`MaterializedNumbers`]: it is the exact one, and the 64-bit marker is
/// the one that admits literals the specification does not. A document whose `Int` literals all
/// convert here is a document whose integers a spec-conformant server can accept; one that
/// converts only at [`MaterializedNumbers`] has told you something else, and the
/// [`IntWidth`] on the error is how a consumer learns which.
///
/// `Float` stays [`f64`] at both widths and `f32` never appears: GraphQL's `Float` **is** IEEE
/// 754 double precision (draft §3.5.2), so a 32-bit float would be non-conformant rather than
/// narrower. The `32` in the name is the integer width and nothing else.
#[cfg(feature = "materialized-numbers")]
pub(crate) enum MaterializedNumbers32 {}

#[cfg(feature = "materialized-numbers")]
impl<S> Numbers<S> for MaterializedNumbers32
where
  S: AsRef<[u8]>,
{
  type Int = i32;
  type Float = f64;
  type Error = OutOfRange<S>;
  type Value = Materialized32InputValue<S>;
  type ConstValue = Materialized32ConstInputValue<S>;

  #[inline]
  fn int(slice: S) -> Result<i32, OutOfRange<S>> {
    match parse_i32(slice.as_ref()) {
      Some(value) => Ok(value),
      None => Err(OutOfRange::Int {
        value: slice,
        width: IntWidth::I32,
      }),
    }
  }

  #[inline]
  fn float(slice: S) -> Result<f64, OutOfRange<S>> {
    match parse_f64(slice.as_ref()) {
      Some(value) => Ok(value),
      None => Err(OutOfRange::Float(slice)),
    }
  }

  #[inline]
  fn report(error: OutOfRange<S>, span: SimpleSpan) -> DialectGraphqlError<S> {
    report_out_of_range(error, span)
  }
}

/// Reads a GraphQL `IntValue` (draft §2.9.1) out of its bytes.
///
/// Digits are accumulated in the sign the literal already has rather than negated afterwards, so
/// `-9223372036854775808` — [`i64::MIN`], whose magnitude is not representable — converts, and
/// the range check is one `checked_*` per digit with no wider intermediate type.
///
/// `None` means the value does not fit, which is the documented bound of the materialised view:
/// a 26-digit literal is valid GraphQL and becomes a parse error here. It also covers a byte that
/// is not a digit, which the lexer's grammar makes unreachable — this reads the bytes rather than
/// trusting them, because "the lexer guarantees it" is a claim about another file.
#[cfg(feature = "materialized-numbers")]
fn parse_i64(bytes: &[u8]) -> Option<i64> {
  let (negative, digits) = match bytes.split_first() {
    Some((b'-', rest)) => (true, rest),
    _ => (false, bytes),
  };

  if digits.is_empty() {
    return None;
  }

  let mut accumulator: i64 = 0;
  for byte in digits {
    let digit = i64::from(byte.checked_sub(b'0').filter(|d| *d < 10)?);
    accumulator = accumulator.checked_mul(10)?;
    accumulator = if negative {
      accumulator.checked_sub(digit)?
    } else {
      accumulator.checked_add(digit)?
    };
  }

  Some(accumulator)
}

/// Reads a GraphQL `IntValue` at the specification's own width (draft §3.5.1) out of its bytes.
///
/// **One digit loop, not two.** Every value an [`i32`] can hold an [`i64`] can hold, so reading
/// at the wider width and narrowing is *exactly* the narrower read: a literal that fits `i32`
/// converts through both steps, and a literal that does not fails at whichever step first sees
/// it. The alternative — a second accumulator loop with `i32`'s `checked_*` — is the same
/// function written twice, and the case that separates them does not exist. The two are held to
/// that in `i32_is_i64_narrowed_on_every_boundary`.
///
/// `-2147483648` is the input this could get wrong and does not: [`i32::MIN`]'s magnitude is one
/// past [`i32::MAX`], and it survives because [`parse_i64`] accumulates in the literal's own sign
/// and `i32::try_from` is a range check rather than a negation.
#[cfg(feature = "materialized-numbers")]
fn parse_i32(bytes: &[u8]) -> Option<i32> {
  i32::try_from(parse_i64(bytes)?).ok()
}

/// Reads a GraphQL `FloatValue` (draft §2.9.2) out of its bytes.
///
/// The conversion is `core`'s own, reached through a borrowed `&str` over the literal's bytes —
/// so it is correctly rounded for a literal of any length, and it allocates nothing:
/// `core::num::dec2flt`'s slow path is a fixed-size big integer on the stack. Rolling a
/// mantissa-and-exponent conversion here instead would be double-rounded and therefore wrong on
/// halfway cases, and truncating into a fixed buffer would put a second, invented length bound on
/// the grammar.
///
/// `None` means the literal does not name a finite double. `f64::from_str` reports an overflow as
/// an infinity rather than as an error, and draft §3.5.2 admits only finite values, so the guard
/// is [`f64::is_finite`] and not the parse's own `Result`.
#[cfg(feature = "materialized-numbers")]
fn parse_f64(bytes: &[u8]) -> Option<f64> {
  let text = core::str::from_utf8(bytes).ok()?;
  let value: f64 = text.parse().ok()?;
  value.is_finite().then_some(value)
}

#[cfg(all(test, feature = "materialized-numbers"))]
mod tests;
