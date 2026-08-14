//! The one axis the two value parsers differ on: what an `Int` and a `Float` leaf carry.
//!
//! The composite value productions in this module tree are generic over [`Numbers`] and
//! monomorphic in nothing else. [`SliceNumbers`] keeps the source slice and is what
//! [`value`](super::value) and its siblings resolve to; [`Materialized<I>`](Materialized)
//! converts to `I` and [`f64`] and is what [`materialized::value`](super::materialized::value)
//! resolves to at each width [`MaterializedInt`] admits. The parsers are then the *same* parser —
//! same commit points, same recovery, the same error at every position but the two leaves — which
//! is a property held by construction rather than by files being kept in step. Value modules
//! drifting apart is the failure this shape is chosen against.
//!
//! # Two materialised widths, because GraphQL has two honest answers
//!
//! Draft §3.5.1 says `Int` is a signed 32-bit integer. Draft §2.9.1's grammar for an `IntValue`
//! says nothing about how many digits it may have. So `2147483648` is a well-formed `IntValue`
//! that no conformant `Int` can hold, and the two widths are the two readings of that: one
//! answers *what does this document mean under the specification*, the other *what did the author
//! write*.
//!
//! # Why the width is a parameter on one marker and not a second marker
//!
//! A second marker beside this one — `MaterializedNumbers32` beside `MaterializedNumbers` — is
//! what shipped first, and it was wrong. The reading it generalised from was about
//! [`ast::InputValue`](crate::graphql::ast::InputValue): an established, public,
//! consumer-constructed enum where a new parameter breaks unannotated construction and silently
//! reinterprets a positional `Container` argument. None of that is true of a marker nobody outside
//! this crate can name, or of a tree this feature introduced with no consumers. What the second
//! marker bought was a second value tree, a second production module and a second test suite, all
//! of them the first ones with one type substituted.
//!
//! What it *cost* is the reason this shape replaced it. With a marker per width, the width at a
//! conversion site is a **supplied** value: something has to say `IntWidth::I32` beside
//! `MaterializedNumbers32`, and nothing but a reader's attention keeps the two in step. Under
//! [`MaterializedInt::WIDTH`] the width is a **function of the payload type**, so no internal site
//! supplies it and none can get it wrong. The one place a width is still a value a caller chooses
//! is `IntOverflow::checked`, and [`overflows`] is the check that keeps that choice honest.
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
use core::marker::PhantomData;

#[cfg(feature = "materialized-numbers")]
use smear_lexer::graphql::syntactic::{SyntacticLexer, SyntacticToken};
#[cfg(feature = "materialized-numbers")]
use tokora::Lexer;

#[cfg(feature = "materialized-numbers")]
use crate::graphql::ast::materialized::{
  ConstInputValue as MaterializedConstInputValue, InputValue as MaterializedInputValue,
};
#[cfg(feature = "materialized-numbers")]
use crate::graphql::error::{IntOverflow, IntWidth};
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
/// The slice tree and the materialised tree are separate `enum`s rather than one enum at two
/// instantiations, because a type alias cannot be used as a module and
/// `use ast::InputValue::{Int, String}` has to keep compiling —
/// [`ast::materialized`](crate::graphql::ast::materialized) records the whole argument. That is a
/// statement about the *slice* boundary and not about the width: the materialised tree spans both
/// widths as one enum, because `I` there is a parameter on an enum and not an alias over one.
/// "Which tree" is therefore a property of the marker, exactly as "which payload" already was, and
/// belongs in the same place. The shared bodies name [`Numbers::Value`] and convert a leaf into it,
/// so no tree is spelled in a body that serves several.
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
/// # Why the width travels with the failure rather than sitting on [`Numbers`]
///
/// A `const WIDTH` on [`Numbers`] would have to be answered by [`SliceNumbers`] too, which
/// converts nothing and has no width; whatever it answered would be a value no code can reach
/// and every reader has to interpret. It sits on [`MaterializedInt`] instead — the trait only a
/// materialised *payload* implements — so the question is asked of the one type that has an
/// answer, [`SliceNumbers`] is never asked it, and the failure below carries what
/// [`MaterializedInt::WIDTH`] said at the one place that knows it. [`SliceNumbers`] never
/// constructs one because its `Error` is [`Infallible`].
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

/// The one report body every materialised width uses.
///
/// A free function rather than a method body so that a failure reported by the slice-free path
/// reads the same at every width by construction: there is one of it, and the width it prints is
/// the one [`Numbers::int`] read off the payload type.
#[cfg(feature = "materialized-numbers")]
#[inline]
pub(crate) fn report_out_of_range<S>(
  error: OutOfRange<S>,
  span: SimpleSpan,
) -> DialectGraphqlError<S> {
  match error {
    OutOfRange::Int { value, width } => {
      DialectGraphqlError::int_overflow(IntOverflow::new(value, width), span)
    }
    OutOfRange::Float(slice) => DialectGraphqlError::float_overflow(slice, span),
  }
}

/// A width a GraphQL `Int` literal can be materialised at.
///
/// Two impls ship — [`i32`], which is the width draft §3.5.1 specifies, and [`i64`], the
/// grammar-permissive reading that also accepts the literals draft §2.9.1 admits and §3.5.1 does
/// not. Neither is a subset of "correct": they answer two different questions about a document,
/// and [`WIDTH`](Self::WIDTH) is how a refusal says which one was asked.
///
/// # The width is read off the type, which is the whole reason this trait exists
///
/// Every internal site that reports an out-of-range literal reads [`WIDTH`](Self::WIDTH) off the
/// payload type it just failed to build. There is no argument to pass and therefore none to get
/// wrong — the failure and the width it names are produced by one expression, from one type.
/// A shape where each width was its own marker had to *state* the pairing at every conversion
/// site instead, which is a thing that can be true today and false after an edit.
///
/// # The width is all of it, because reading a literal is not a caller's question
///
/// [`WIDTH`](Self::WIDTH) is the only item, and the reader that turns bytes into an `I` sits on
/// the private supertrait beside the seal. It is a **range** reader: it grades digits and a
/// magnitude, and it has no opinion at all about whether the spelling in front of it is an
/// `IntValue`, because on the one path that reaches it the lexer has already said so.
///
/// Reachable out of crate, it answers `Some(7)` to `007` — a spelling this crate's lexer refuses
/// as `LeadingZeros`, that no production converts, and that [`IntOverflow::checked`] declines. The
/// crate would then carry three answers to "is this a valid integer literal", one of which is a
/// range check, and the whole of the round that produced `is_int_literal` was about there being
/// **one**. Adding the lexer's grammar to a public `parse` is the other way to close that and it
/// is worse: it is a second statement of draft §2.9.1, free to disagree with the first, which is
/// the defect that round removed.
///
/// So the reader is not reachable, and [`IntOverflow::checked`] is the public door to the question
/// it half-answers, with the lexer's conjunct in front of it.
///
/// # Sealed, and not exported
///
/// [`IntWidth`] enumerates exactly the two widths this crate reads, and its own documentation
/// argues that being exhaustive is a feature rather than an oversight. An out-of-crate impl of
/// this trait would have to answer [`WIDTH`](Self::WIDTH) with one of those two while being
/// neither, so every error it produced would name a width that is not its own — the forgery the
/// crate-private `IntOverflow::new` and the checked public door exist to rule out, arriving
/// through a third door. So the trait is sealed, and a third width is a change to this crate.
///
/// **The seal was not enough, and this is the second half.** `syntactic::value::materialized`
/// carried a `pub use` of this trait for two commits, on the reasoning that a public
/// `where I: MaterializedInt` has to name something a caller can reach. Sealing `parse` onto the
/// private supertrait was supposed to keep the reader off the public surface, and it does not:
/// **Rust resolves a supertrait's items through a generic subtrait bound**, so a downstream
/// generic function reaches `parse` without ever naming the seal.
///
/// The import is private now, so the trait is not nameable out of crate and neither route exists:
///
/// ```compile_fail,E0603
/// use smear_parser::graphql::syntactic::value::materialized::MaterializedInt;
///
/// // THIS COMPILED, and `read::<i32>(b"007")` answered `Some(7)`. `<i32 as MaterializedInt>::parse`
/// // did NOT compile at the same commit — UFCS on a concrete type resolves differently from
/// // `I::parse` under a generic bound — which is exactly why asserting only the UFCS form left
/// // the hole open while looking closed.
/// fn read<I: MaterializedInt>(bytes: &[u8]) -> Option<I> {
///   I::parse(bytes)
/// }
/// ```
///
/// An out-of-crate `impl MaterializedInt for Narrow` now fails at that same `use`, so the impl
/// seal is **subsumed rather than separately asserted**: a name that cannot be written cannot be
/// implemented. `sealed::MaterializedInt` stays a supertrait regardless — it is what keeps `parse`
/// off this trait's own item list, and it is what would still hold if a real consumer ever asked
/// for the bound back.
///
/// One caveat on the annotation above, established by planting a wrong code: **`rustdoc` on
/// stable does not check it.** A `compile_fail,E0603` on a snippet that fails with `E0425` passes;
/// only nightly reports *"Some expected error codes were not found"*. What this doctest asserts
/// under a stable `cargo test` is that the snippet does not compile at all, which is the property
/// — the code records which failure was observed and is verified by running the doc tests under
/// nightly.
///
/// # Not exported is not unusable
///
/// A bound does not have to be nameable for the function it constrains to be **called**. `I`
/// appears in every materialised return type, so a consumer writes `i32` or `i64` at the call and
/// inference supplies the rest. Out of crate, at both widths, with the trait unnameable:
///
/// ```
/// use smear_parser::graphql::{
///   GraphQL,
///   ast::IntValue,
///   error::{ErrorData, GraphqlErrors, IntWidth},
///   syntactic::{GraphqlLexer, value::materialized},
/// };
/// use tokora::{Parse as _, Parser};
///
/// // `2147483648` is the literal the two readings disagree about. Nothing here names the bound.
/// let at_i64 = Parser::with_parser::<
///   GraphqlLexer<'_, str>, IntValue<i64>, GraphqlErrors<&str>, _, GraphQL,
/// >(materialized::int_value::<_, _, i64>)
/// .parse_str("2147483648");
/// assert_eq!(at_i64.map(|node| *node.source()).ok(), Some(2_147_483_648_i64));
///
/// let at_i32 = Parser::with_parser::<
///   GraphqlLexer<'_, str>, IntValue<i32>, GraphqlErrors<&str>, _, GraphQL,
/// >(materialized::int_value::<_, _, i32>)
/// .parse_str("2147483648");
/// let width = at_i32
///   .expect_err("past `i32`")
///   .into_iter()
///   .find_map(|error| match error.data() {
///     ErrorData::IntOverflow(overflow) => Some(overflow.width()),
///     _ => None,
///   });
/// // The refusal names the width the call asked for, which is the whole of what `WIDTH` was
/// // public for — and it arrives on the error rather than out of the trait.
/// assert_eq!(width, Some(IntWidth::I32));
/// ```
#[cfg(feature = "materialized-numbers")]
#[cfg_attr(docsrs, doc(cfg(feature = "materialized-numbers")))]
pub trait MaterializedInt: Sized + sealed::MaterializedInt {
  /// Which width this payload is, for the error a refusal raises.
  const WIDTH: IntWidth;
}

/// The seal on [`MaterializedInt`], and **where the width's reader lives**.
///
/// Named in a public supertrait position and reachable from nowhere, which is what makes the
/// trait's impl list this file's — and, since `parse` is declared here rather than above, what
/// keeps the reader off the public surface. The section on [`MaterializedInt`] has the argument
/// for the second half.
#[cfg(feature = "materialized-numbers")]
mod sealed {
  /// The supertrait no out-of-crate type can name and therefore cannot implement.
  pub trait MaterializedInt: Sized {
    /// Reads a GraphQL `IntValue` (draft §2.9.1) out of its bytes at this width.
    ///
    /// `None` means the literal does not fit — which is the documented bound of the materialised
    /// view — or that the bytes are not an `IntValue` at all, which the lexer's grammar makes
    /// unreachable from a production: the only caller is `Numbers::int`, whose slice came out of
    /// a `LitInt` token.
    fn parse(bytes: &[u8]) -> Option<Self>;
  }

  impl MaterializedInt for i32 {
    #[inline]
    fn parse(bytes: &[u8]) -> Option<Self> {
      super::parse_i32(bytes)
    }
  }

  impl MaterializedInt for i64 {
    #[inline]
    fn parse(bytes: &[u8]) -> Option<Self> {
      super::parse_i64(bytes)
    }
  }
}

#[cfg(feature = "materialized-numbers")]
impl MaterializedInt for i64 {
  const WIDTH: IntWidth = IntWidth::I64;
}

#[cfg(feature = "materialized-numbers")]
impl MaterializedInt for i32 {
  const WIDTH: IntWidth = IntWidth::I32;
}

/// `I` and [`f64`], read out of the literal's bytes with no allocation anywhere.
///
/// One marker at two widths rather than a marker each, and the module header has the argument.
/// The short version is that a marker per width makes the width a value somebody has to supply
/// beside the marker, and this makes it [`MaterializedInt::WIDTH`] — a fact about the payload type
/// that no site can restate incorrectly because no site restates it at all.
///
/// `Float` is [`f64`] at every width and `f32` never appears: GraphQL's `Float` **is** IEEE 754
/// double precision (draft §3.5.2), so a 32-bit float would be non-conformant rather than
/// narrower. `I` is the integer payload and nothing else — which is why the parameter is on the
/// `Int` leaf alone, and why the float conversion below mentions it nowhere.
///
/// `AsRef<[u8]>` is this crate's established spelling for "a slice whose text can be read", and
/// every source backing the crate ships satisfies it. It appears here and not on the slice
/// parser because only this one looks *inside* a literal.
#[cfg(feature = "materialized-numbers")]
pub(crate) struct Materialized<I> {
  /// Uninhabited-in-practice: the marker is a type and never a value. `PhantomData` is what lets
  /// an empty carrier name the payload the [`Numbers`] impl below is written against.
  _payload: PhantomData<I>,
}

#[cfg(feature = "materialized-numbers")]
impl<S, I> Numbers<S> for Materialized<I>
where
  S: AsRef<[u8]>,
  I: MaterializedInt,
{
  type Int = I;
  type Float = f64;
  type Error = OutOfRange<S>;
  type Value = MaterializedInputValue<S, I>;
  type ConstValue = MaterializedConstInputValue<S, I>;

  #[inline]
  fn int(slice: S) -> Result<I, OutOfRange<S>> {
    match <I as sealed::MaterializedInt>::parse(slice.as_ref()) {
      Some(value) => Ok(value),
      // The width is `I`'s own, so this arm cannot name a width the conversion above was not
      // attempted at: there is one type in the expression and it decides both halves.
      None => Err(OutOfRange::Int {
        value: slice,
        width: I::WIDTH,
      }),
    }
  }

  #[inline]
  fn float(slice: S) -> Result<f64, OutOfRange<S>> {
    float(slice)
  }

  #[inline]
  fn report(error: OutOfRange<S>, span: SimpleSpan) -> DialectGraphqlError<S> {
    report_out_of_range(error, span)
  }
}

/// The `Float` conversion, **outside the marker because it is outside the width**.
///
/// It takes no `I` and there is no width at which it does something else: GraphQL's `Float` *is*
/// IEEE 754 double precision (draft §3.5.2). Every float path — the [`Numbers`] impl above and
/// [`materialized::float_value`](super::materialized::float_value) — is this one function, so
/// "the two widths agree about floats" is a fact about the call graph rather than a claim two
/// instantiations happen to satisfy.
#[cfg(feature = "materialized-numbers")]
#[inline]
pub(crate) fn float<S>(slice: S) -> Result<f64, OutOfRange<S>>
where
  S: AsRef<[u8]>,
{
  match parse_f64(slice.as_ref()) {
    Some(value) => Ok(value),
    None => Err(OutOfRange::Float(slice)),
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

/// Would the materialising production at `width` have refused this literal?
///
/// **The decider behind [`IntOverflow::checked`], and it lives here so there is one reader.** That
/// constructor is the only public way to name an [`IntWidth`], and its promise is a claim about
/// *these* functions: that the width a caller supplied is one the production at that width would
/// have failed on. A predicate written in `error.rs` beside the constructor would be a second
/// reading of the grammar, free to disagree with the two above and to keep the promise about
/// nothing.
///
/// # The one place a width is still a value, and why it stays one
///
/// Nothing else in this crate takes a width as an argument any more:
/// [`MaterializedInt::WIDTH`] answers it from the payload type at every production site. This
/// door is different in kind, because its caller is outside the type system that decided the
/// question — a gateway told at run time which reading it enforces has an [`IntWidth`] as a
/// *value*, and taking it as a type parameter would only move the choice, not remove it. What
/// makes it safe is not the shape of the argument but the check: a `(literal, width)` pair the
/// production at that width would have converted comes back as `Err`.
///
/// The `match` below is therefore the second statement of a correspondence
/// [`MaterializedInt::WIDTH`] states first — that `I32` is the width `i32` reads at and `I64` is
/// `i64`'s. `the_widths_the_door_dispatches_on_are_the_widths_the_readers_name` is the assertion
/// that the two statements are the same one; without it, a wrong `WIDTH` would leave this
/// function promising something about a production that does not exist.
///
/// [`is_int_literal`] is the first conjunct because [`parse_i64`] answers `None` to two different
/// questions — "outside the width" and "not an integer at all" — and only the first one is an
/// overflow. Without it `checked(b"hello", I64)` would build an error saying `hello` overflowed a
/// 64-bit integer, which is a checked constructor admitting a claim that is not merely unproven
/// but false.
#[cfg(feature = "materialized-numbers")]
pub(crate) fn overflows(bytes: &[u8], width: IntWidth) -> bool {
  is_int_literal(bytes)
    && match width {
      IntWidth::I32 => parse_i32(bytes).is_none(),
      IntWidth::I64 => parse_i64(bytes).is_none(),
    }
}

/// Is this slice the spelling of a GraphQL `IntValue`? — **asked of the lexer, which is the thing
/// that decides it.**
///
/// Not a grammar written here, and that is the whole content of this function. `smear-lexer`
/// already grades draft §2.9.1's `IntegerPart`, finely enough to report a leading zero as its own
/// `LeadingZeros` diagnostic; a `-?[0-9]+` predicate beside it was a *second* answer to that one
/// question, and it answered differently. `02147483648` is not an `IntValue` — the lexer refuses
/// it and no production ever converts it — yet the digit shape admitted it, so
/// `IntOverflow::checked("02147483648", I32)` minted a payload quoting a spelling the parser
/// cannot emit, and a renderer reported an overflow where the document has a leading zero.
///
/// The whole slice has to be the one token. The lexer skips leading trivia and stops at any legal
/// delimiter, so `" 7"` and `"7 "` each contain an `Int` without being one, and a token as long as
/// the input is necessarily the input.
///
/// **The bytes, not the characters.** `IntOverflow::checked` bounds its source by `AsRef<[u8]>`,
/// and an `IntValue` is ASCII throughout, so grading the byte source reaches the decision the
/// `str` source would — `the_decision_is_the_same_on_a_byte_slice_source` pins that from the door
/// itself.
///
/// This runs a token scan where a byte loop would do, on a path reached only by a caller of the
/// checked constructor — never by a production, which has the lexer's verdict already. That is
/// the price of there being one answer.
///
/// # The first-byte gate is a *necessary* condition, and that is why it is not the grammar that
/// was deleted
///
/// The deleted `-?[0-9]+` was a **sufficient** condition — "these bytes *are* a literal" — which
/// is a grammar, and a second grammar can disagree with the first about what a literal is. It
/// did: it admitted `007`. What is below is a **necessary** condition — "these bytes cannot
/// *possibly* be one" — and a necessary condition has no opinion about what a literal is. The
/// worst a wrong one can do is refuse something the lexer would have admitted; it can never admit
/// something the lexer refuses, because it decides nothing about admission at all. That single
/// failure mode is held by `the_gate_refuses_nothing_the_lexer_admits`, which runs this function
/// against itself-without-the-gate over every first byte there is. **Do not delete this as the
/// duplicate that R3 removed. It is not the same kind of thing.**
///
/// The condition itself is read straight off draft §2.9.1: an `IntValue`'s `IntegerPart` is
/// `NegativeSign? 0` or `NegativeSign? NonZeroDigit Digit*`, so its first character is `-` or a
/// digit, with no third case. A whole-slice token starts at byte 0 — that is what the length
/// check in the body establishes — so the input's first byte *is* the literal's first byte, and
/// any other value of it rules out the only verdict this function answers `true` to.
///
/// **It does not decide leading zeros, and must not.** `0` passes the gate, so `007` and
/// `02147483648` still reach the lexer and are still refused by it as `LeadingZeros` — the
/// refusal R3 bought. A gate that also read the second byte would be taking that decision back.
///
/// # What it buys, which is the reason it is here at all
///
/// `IntOverflow::checked` accepts arbitrary `AsRef<[u8]>`, so a caller validating an untrusted
/// payload can hand this function anything. Asking the *general* lexer means the first byte
/// chooses a sub-lexer, and the inline-string sub-lexer appends one diagnostic per malformed
/// escape to a `SmallVec` before this function drops every one of them: refusing `"\q\q\q…"`
/// cost heap proportional to the input, ~184 MB for a 1 MiB slice that was never going to be an
/// integer. The gate makes a refusal O(1) in heap for every byte that cannot start an `IntValue`,
/// and `a_refusal_costs_the_same_heap_at_a_kilobyte_and_at_a_megabyte` measures the constant
/// rather than asserting it. The bytes that *do* pass reach the number arm, whose diagnostics are
/// per-token and not per-byte; `no_first_byte_reaches_a_refusal_whose_cost_grows` is that claim
/// derived over all 256 first bytes instead of the few worth guessing.
#[cfg(feature = "materialized-numbers")]
fn is_int_literal(bytes: &[u8]) -> bool {
  if !matches!(bytes.first(), Some(b'-' | b'0'..=b'9')) {
    return false;
  }

  let mut lexer = SyntacticLexer::<'_, [u8]>::new(bytes);
  match lexer.lex() {
    Some(Ok(SyntacticToken::LitInt(literal))) => literal.len() == bytes.len(),
    _ => false,
  }
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
