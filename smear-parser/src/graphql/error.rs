//! The GraphQL dialect error and the `From` glue that makes it a
//! [`ParseCtx`](crate::combinator::ParseCtx) error.
//!
//! The error family — [`Expectation`], [`Unclosed`], [`ErrorData`], [`Error`], and
//! the [`Errors`] container — is copied from the frozen `smear-parser` crate
//! (`graphql/error.rs`), re-keyed to the source slice `S`. [`GraphqlError`] and
//! [`GraphqlErrors`] pin `S` and the found-token payload to the concrete GraphQL
//! syntactic token kind, the one dialect-specific instantiation the entry runners
//! and tests name.
//!
//! The atoms speak tokora's generic error families ([`UnexpectedToken`],
//! [`SeparatedError`], the container/`TooFew` families, [`UnexpectedEot`], and the
//! lexer errors). tokora's `ComposableEmitter` bundle and `From*` blankets
//! shrink the old twelve-impl set to the handful of `From` conversions the atoms'
//! bounds actually demand; each lands here so [`Fatal`](tokora::emitter::Fatal)
//! over [`GraphqlErrors`] is a complete [`ParseCtx`](crate::combinator::ParseCtx)
//! over both `str` and `[u8]` syntactic lexers — proven by the module's compile
//! test.

use derive_more::{AsMut, AsRef, Deref, DerefMut, From, Into, IsVariant, TryUnwrap, Unwrap};
use smear_lexer::{
  graphql::{
    error::LexerErrors,
    syntactic::{SyntacticToken, SyntacticTokenKind},
  },
  tokora::error::UnexpectedEnd,
};
use tokora::{
  Lexer, SimpleSpan as Span,
  emitter::FromUnclosed,
  error::{
    Unclosed as TokoraUnclosed, UnexpectedEot,
    syntax::{FullContainer, MissingSyntax, TooFew},
    token::{MissingToken, SeparatedError, UnexpectedToken as TokUnexpectedToken},
  },
  utils::{CowStr, Expected},
};

/// Hints for parsing a variable value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]
#[non_exhaustive]
pub enum VariableValueHint {
  /// A name was expected.
  #[display("name")]
  Name,
  /// A dollar `$` was expected.
  #[display("dollar")]
  Dollar,
}

/// A hint for what was expected in a object field value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]
#[non_exhaustive]
pub enum ObjectFieldValueHint {
  /// A colon was expected.
  #[display("colon")]
  Colon,
  /// A value was expected.
  #[display("value")]
  Value,
  /// A name was expected.
  #[display("name")]
  Name,
}

/// Hints for the next component was expected while parsing a schema extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]
#[non_exhaustive]
pub enum SchemaExtensionHint {
  /// Directives.
  #[display("directives")]
  Directives,
  /// Root operation types definition.
  #[display("root operation types definition")]
  RootOperationTypesDefinition,
  /// Extend keyword.
  #[display("extend keyword")]
  Extend,
  /// Schema keyword.
  #[display("schema keyword")]
  Schema,
  /// Directives or root operation types definition.
  #[display("directives or root operation types definition")]
  DirectivesOrRootOperationTypesDefinition,
}

/// Hints for the next component was expected while parsing a union type extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]
#[non_exhaustive]
pub enum UnionTypeExtensionHint {
  /// Directives.
  #[display("directives")]
  Directives,
  /// Union member types.
  #[display("union member types")]
  UnionMemberTypes,
  /// Name.
  #[display("name")]
  Name,
  /// Extend keyword.
  #[display("extend keyword")]
  Extend,
  /// Union keyword.
  #[display("union keyword")]
  Union,
  /// Directives or union member types.
  #[display("directives or union member types")]
  DirectivesOrUnionMemberTypes,
}

/// Hints for the next component was expected while parsing an input object type extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]
#[non_exhaustive]
pub enum InputObjectTypeExtensionHint {
  /// Directives.
  #[display("directives")]
  Directives,
  /// Input fields definition.
  #[display("input fields definition")]
  InputFieldsDefinition,
  /// Name.
  #[display("name")]
  Name,
  /// Extend keyword.
  #[display("extend keyword")]
  Extend,
  /// Input keyword.
  #[display("input keyword")]
  Input,
  /// Directives or input fields definition.
  #[display("directives or input fields definition")]
  DirectivesOrInputFieldsDefinition,
}

/// Hints for the next component was expected while parsing an object type extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]
#[non_exhaustive]
pub enum ObjectTypeExtensionHint {
  /// Implements interfaces.
  #[display("implements")]
  Implements,
  /// Directives.
  #[display("directives")]
  Directives,
  /// Fields definition.
  #[display("fields definition")]
  FieldsDefinition,
  /// Name.
  #[display("name")]
  Name,
  /// Extend keyword.
  #[display("extend keyword")]
  Extend,
  /// Type keyword.
  #[display("type keyword")]
  Type,
  /// Implements, directives, or fields definition.
  #[display("implements, directives, or fields definition")]
  ImplementsOrDirectivesOrFieldsDefinition,
}

/// Hints for the next component was expected while parsing an interface type extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]
#[non_exhaustive]
pub enum InterfaceTypeExtensionHint {
  /// Implements interfaces.
  #[display("implements")]
  Implements,
  /// Directives.
  #[display("directives")]
  Directives,
  /// Fields definition.
  #[display("fields definition")]
  FieldsDefinition,
  /// Name.
  #[display("name")]
  Name,
  /// Extend keyword.
  #[display("extend keyword")]
  Extend,
  /// Interface keyword.
  #[display("interface keyword")]
  Interface,
  /// Implements, directives, or fields definition.
  #[display("implements, directives, or fields definition")]
  ImplementsOrDirectivesOrFieldsDefinition,
}

/// Hints for the next component was expected while parsing an enum type extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]
#[non_exhaustive]
pub enum EnumTypeExtensionHint {
  /// Directives.
  #[display("directives")]
  Directives,
  /// Enum values definition.
  #[display("enum values definition")]
  EnumValuesDefinition,
  /// Name.
  #[display("name")]
  Name,
  /// Extend keyword.
  #[display("extend keyword")]
  Extend,
  /// Enum keyword.
  #[display("enum keyword")]
  Enum,
  /// Directives or enum values definition.
  #[display("directives or enum values definition")]
  DirectivesOrEnumValuesDefinition,
}

/// Expectations for the GraphQL parser.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Expectation {
  /// An inline string was expected.
  InlineString,
  /// A block string was expected.
  BlockString,
  /// A `$` was expected.
  Dollar,
  /// A `(` was expected.
  LParen,
  /// A `)` was expected.
  RParen,
  /// A `...` was expected.
  Spread,
  /// A `:` was expected.
  Colon,
  /// A `=` was expected.
  Equal,
  /// A `@` was expected.
  At,
  /// A `[` was expected.
  LBracket,
  /// A `]` was expected.
  RBracket,
  /// A `{` was expected.
  LBrace,
  /// A `}` was expected.
  RBrace,
  /// A `|` was expected.
  Pipe,
  /// A `!` was expected.
  Bang,
  /// A `&` was expected.
  Ampersand,

  /// Const input value was expected.
  ConstInputValue,
  /// Input value was expected.
  InputValue,
  /// A type reference was expected.
  Type,
  /// A selection was expected.
  Selection,
  /// A variable definition was expected.
  VariableDefinition,
  /// An operation type was expected.
  OperationType,
  /// An executable definition was expected.
  ExecutableDefinition,
  /// An operation type or the `fragment` keyword was expected.
  ///
  /// The position after a leading description on an executable definition. Only
  /// the keyworded alternatives carry a `Description?` — `OperationDefinition :
  /// Description? OperationType …` and `FragmentDefinition : Description?
  /// fragment …` — so the shorthand `OperationDefinition : SelectionSet` cannot
  /// follow one.
  OperationTypeOrFragment,
  /// Fragment name was expected.
  FragmentName,
  /// A name was expected.
  Name,
  /// An operation name was expected.
  OperationName,
  /// An directive location was expected.
  DirectiveLocation,
  /// Either a fragment spread or an inline fragment was expected.
  FragmentSpreadOrInlineFragment,
  /// A number was expected.
  IntValue,
  /// A boolean was expected.
  BooleanValue,
  /// A float was expected.
  FloatValue,
  /// A null value was expected.
  NullValue,
  /// An enum value was expected.
  EnumValue,
  /// A string value was expected.
  StringValue,
  /// A keyword was expected.
  Keyword(&'static str),
}

/// An unexpected token error carrying the found token and the expectation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnexpectedToken<T, TK> {
  found: Option<T>,
  expected: TK,
}

impl<T, TK> UnexpectedToken<T, TK> {
  /// Creates an unexpected token error without a found token.
  #[inline]
  pub const fn new(expected: TK) -> Self {
    Self::maybe_found(None, expected)
  }

  /// Creates a new unexpected token error.
  #[inline]
  pub const fn maybe_found(found: Option<T>, expected: TK) -> Self {
    Self { found, expected }
  }

  /// Creates a new unexpected token error with a found token.
  #[inline]
  pub const fn with_found(found: T, expected: TK) -> Self {
    Self::maybe_found(Some(found), expected)
  }

  /// Returns the found token, if any.
  #[inline]
  pub const fn found(&self) -> Option<&T> {
    self.found.as_ref()
  }

  /// Returns the expectation.
  #[inline]
  pub const fn expected(&self) -> &TK {
    &self.expected
  }
}

/// An unexpected keyword error.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnexpectedKeyword<S> {
  found: S,
  expected: &'static str,
}

impl<S> UnexpectedKeyword<S> {
  /// Creates a new unexpected keyword error.
  #[inline]
  pub const fn new(found: S, expected_kw: &'static str) -> Self {
    Self {
      found,
      expected: expected_kw,
    }
  }

  /// Returns the found keyword.
  #[inline]
  pub const fn found(&self) -> &S {
    &self.found
  }

  /// Returns the name of the expected keyword.
  #[inline]
  pub const fn expected(&self) -> &'static str {
    self.expected
  }
}

/// Represents an unclosed delimiter in GraphQL source.
#[derive(Debug, Copy, Clone, IsVariant)]
pub enum Unclosed {
  /// Unclosed parentheses (missing `)`).
  Parentheses,
  /// An unclosed list (missing `]`).
  List,
  /// An unclosed object (missing `}`).
  Object,
}

/// Which signed integer width a materialised `Int` leaf was read at.
///
/// **Both readings are legitimate and they disagree, which is why the error carries this.**
/// GraphQL specifies `Int` as a signed 32-bit integer (draft §3.5.1), so [`I32`](Self::I32) is
/// the spec-exact reading; the grammar in draft §2.9.1 puts no bound on an `IntValue`'s digits at
/// all, so [`I64`](Self::I64) is the grammar-permissive one that accepts literals the
/// specification does not. `2147483648` is out of range at one width and a value at the other,
/// and a consumer handed only "an integer overflowed" cannot tell which fact about the document
/// it was told.
///
/// # Exhaustive, deliberately
///
/// No `#[non_exhaustive]`, unlike the hint enums above it and like [`Unclosed`] beside it. The
/// whole reason to read this is to branch on the two, and a wildcard arm forced onto every
/// consumer would be a wildcard over a two-element closed set. A third width is not a variant
/// added here in isolation: it is a third marker and a third value tree in
/// [`ast`](crate::graphql::ast), so it is a change to the feature's surface either way.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]
pub enum IntWidth {
  /// 32 bits — GraphQL's specified `Int`.
  #[display("i32")]
  I32,
  /// 64 bits — the grammar-permissive reading, which accepts literals draft §3.5.1 does not.
  #[display("i64")]
  I64,
}

impl IntWidth {
  /// The width in bits, for a report that renders the number rather than the type name.
  #[inline]
  pub const fn bits(self) -> u32 {
    match self {
      Self::I32 => 32,
      Self::I64 => 64,
    }
  }
}

/// An integer literal that is valid GraphQL and does not fit the width it was read at.
///
/// It carries both halves of the fact, because neither is the fact on its own: the literal's
/// source spelling, so a report can name what the document said, and the [`IntWidth`] the
/// conversion was attempted at, so a report can name which reading refused it. See [`IntWidth`]
/// for why two readings exist.
///
/// # The width is not a caller's to choose, and that is the whole of this type's job
///
/// A payload with **no** width is unrepresentable because every constructor demands one. That is
/// the easy half, and on its own it buys less than it looks: a payload with the **wrong** width
/// was representable too, because the constructor took the discriminant as an argument and
/// accepted whichever one it was handed. `IntOverflow::new("2147483648", IntWidth::I64)` compiled
/// — in every configuration, including one with `materialized-numbers` off, and through direct
/// [`ErrorData::IntOverflow`] construction — and `2147483648` is a value at that width. A renderer
/// trusting the payload then reports a refusal that never happened, which is worse than reporting
/// none: an unobserved width is a gap a consumer can see, and a false one is not.
///
/// So the free-width constructor is crate-private, and the public door is `IntOverflow::checked`,
/// which refuses a pair the productions would not have produced. The two facts the payload carries
/// are no longer independent: the width has to be one the literal beside it genuinely overflows.
///
/// (`checked` is named in code font rather than linked, here and everywhere else in this file that
/// an unconditional doc comment reaches a `materialized-numbers` item. An intra-doc link to a
/// gated item is `rustdoc::broken_intra_doc_links` in every configuration that does not compile
/// it, and `cargo doc -p smear-parser` — this crate at its own default features, which is what a
/// direct dependent gets — is exactly such a configuration.)
///
/// Without `materialized-numbers` there is no public constructor at all — the same shape as the
/// variant this payload sits in, which is declared unconditionally and produced only under that
/// feature. See [`ErrorData`] for why the gate belongs on the producer rather than on the
/// declaration.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntOverflow<S> {
  value: S,
  width: IntWidth,
}

impl<S> IntOverflow<S> {
  /// Creates an integer-out-of-range payload at a width the caller is trusted to have attempted.
  ///
  /// **Crate-private, and the check on [`checked`](Self::checked) is what it buys.** The
  /// conversion sites in
  /// [`syntactic::value`](crate::graphql::syntactic::value) reach this one because each of them
  /// has *just* failed a conversion at the width it names, so a range check here would re-read a
  /// literal the caller already read and answer a question already answered. Every path from
  /// outside the crate goes through the checked door instead.
  ///
  /// Gated with the productions that call it: no configuration compiles a caller for it otherwise.
  #[cfg(feature = "materialized-numbers")]
  #[inline]
  pub(crate) const fn new(value: S, width: IntWidth) -> Self {
    Self { value, width }
  }

  /// Creates an integer-out-of-range payload, refusing a width the literal does not overflow.
  ///
  /// The public constructor, and the only way a caller outside this crate can name an
  /// [`IntWidth`]. It answers `Ok` exactly when `value` is an integer literal — an optional `-`
  /// and at least one decimal digit — that the materialising production at `width` would have
  /// refused, decided by the very readers those productions use. Everything else comes back as
  /// `Err(value)`: a literal that fits, a float, a name, an empty slice.
  ///
  /// **The literal comes back unconsumed on refusal**, for the reason the conversion trait behind
  /// [`syntactic::value`](crate::graphql::syntactic::value) hands its slice back — a caller who is
  /// told this is not an overflow still has a document to report something else about, and taking
  /// `&S` instead would force a clone on the path that succeeds.
  ///
  /// ```
  /// use smear_parser::graphql::error::{IntOverflow, IntWidth};
  ///
  /// // `2147483648` is `i32::MAX + 1`: out of range at the specified width…
  /// let overflow = IntOverflow::checked("2147483648", IntWidth::I32).expect("outside i32");
  /// assert_eq!(overflow.width(), IntWidth::I32);
  ///
  /// // …and a perfectly good `i64`, so the other width is a refusal that never happened.
  /// assert_eq!(IntOverflow::checked("2147483648", IntWidth::I64), Err("2147483648"));
  ///
  /// // A literal past both widths may name either, because both readers refuse it.
  /// assert!(IntOverflow::checked("9223372036854775808", IntWidth::I32).is_ok());
  /// assert!(IntOverflow::checked("9223372036854775808", IntWidth::I64).is_ok());
  ///
  /// // Neither an in-range literal nor a non-literal is an overflow at any width.
  /// assert_eq!(IntOverflow::checked("7", IntWidth::I32), Err("7"));
  /// assert_eq!(IntOverflow::checked("1.0", IntWidth::I64), Err("1.0"));
  /// ```
  ///
  /// The free-width constructor beside it is crate-private, so the forgery above has no second
  /// door:
  ///
  /// ```compile_fail,E0624
  /// use smear_parser::graphql::error::{IntOverflow, IntWidth};
  ///
  /// // `2147483648` fits `i64`. Naming that width for it is a fact about no attempt anyone made.
  /// let forged = IntOverflow::new("2147483648", IntWidth::I64);
  /// ```
  #[cfg(feature = "materialized-numbers")]
  #[cfg_attr(docsrs, doc(cfg(feature = "materialized-numbers")))]
  #[inline]
  pub fn checked(value: S, width: IntWidth) -> Result<Self, S>
  where
    S: AsRef<[u8]>,
  {
    if crate::graphql::syntactic::value::numbers::overflows(value.as_ref(), width) {
      Ok(Self::new(value, width))
    } else {
      Err(value)
    }
  }

  /// Returns the literal's source spelling.
  #[inline]
  pub const fn value(&self) -> &S {
    &self.value
  }

  /// Returns the width the conversion was attempted at.
  #[inline]
  pub const fn width(&self) -> IntWidth {
    self.width
  }

  /// Consumes the payload and returns the literal's source spelling.
  #[inline]
  pub fn into_value(self) -> S {
    self.value
  }
}

/// The data of a parser error.
///
/// # Two variants have a feature-gated producer, and are themselves unconditional
///
/// [`IntOverflow`](ErrorData::IntOverflow) and [`FloatOverflow`](ErrorData::FloatOverflow) are
/// raised by the materialising value productions and by nothing else, so
/// `Error::int_overflow` and `Error::float_overflow` — the paths that *produce* them — carry
/// `#[cfg(feature = "materialized-numbers")]`. **The variants do not.**
///
/// The defect being repaired was real and is worth naming precisely, because the gate that
/// repairs it is easy to put one level too high. Both variants existed here with **no
/// constructor and no construction site in any configuration**: declared and unproducible, the
/// same shape filed twice already against this project — tokora's
/// `FinishError::InvalidDialectKind`, and `smear-lexer`'s `LosslessTokenKind::Boolean` with 28
/// declared and 27 producible. What fixes that is a producer, not a `#[cfg]` on the declaration;
/// gating the variant as well would additionally *remove* two names from the default surface,
/// which is a second change wearing the first one's justification.
///
/// So the two claims the census makes are different claims, and it makes both:
/// `error_data_variant_census` matches this enum exhaustively and wildcard-free in **every**
/// configuration — 22 variants, always — and builds a sample through a public constructor for
/// each one whose producer that configuration compiled: 22 samples with the feature, 20 without.
/// A variant producible in no configuration is caught by the all-features run.
///
/// # Source-breaking change: `IntOverflow`'s payload
///
/// `IntOverflow(S)` is now [`IntOverflow(IntOverflow<S>)`](IntOverflow). This is a **breaking
/// change to a name that predates the branch**, stated here rather than in a changelog because
/// the workspace has never published a version to break — every crate in it is `0.0.0`, so there
/// is no released `smear-parser` for a semver bump to describe. The obligation the bump would
/// discharge is discharged here and in the PR instead, and the moment a version exists this
/// paragraph is what a changelog entry is written from.
///
/// **What stops compiling**, in every configuration including one with `materialized-numbers`
/// off, because the variant is unconditional:
///
/// - a match arm that binds the payload and uses it as `S` — `ErrorData::IntOverflow(v) => v`;
/// - the derive-generated `unwrap_int_overflow` / `try_unwrap_int_overflow`, whose return type
///   was `S`.
///
/// **The migration is one accessor per site**: `v` becomes `v.value()`, and
/// `unwrap_int_overflow()` gains `.into_value()`. `ErrorData` stays at 22 variants and no other
/// variant changes shape.
///
/// **What stops compiling under `materialized-numbers`**, on the producer rather than on the
/// enum: `Error::int_overflow` took `(value: S, span)` before the branch and now takes
/// `(overflow: IntOverflow<S>, span)`. The intermediate `(value, width, span)` shape existed only
/// within this branch and was the wrong-width hole itself, so no released or reviewed surface
/// carried it. **The migration is `IntOverflow::checked`**: `int_overflow(v, span)` becomes
/// `int_overflow(IntOverflow::checked(v, width)?, span)`, and there is no infallible spelling on
/// purpose — a caller who cannot say which width refused the literal does not have an
/// `IntOverflow` to report.
///
/// **Why this rather than the two alternatives**, both of which were considered and are worse:
///
/// - *A twenty-third variant, `IntOverflow32`.* This enum is deliberately not
///   `#[non_exhaustive]`, and `smear-smoke`'s `error_data_is_exhaustively_matchable` pins that
///   from outside the crate. A new variant is therefore `E0004` in every downstream exhaustive
///   match — a break too, and a wider one, since it reaches consumers who never touch integer
///   overflow at all.
/// - *A private `Option<IntWidth>` beside `data` on [`Error`], read through additive accessors.*
///   It breaks nothing in the type system, and it is the wrong shape for two reasons that are
///   about what a consumer does rather than about taste. First, `Error::into_data` and
///   `Error::data` hand out an [`ErrorData`] with no width in it, so the natural renderer — `fn
///   render(data: &ErrorData<…>) -> String` — cannot reach the field that exists for it, and
///   forwarding the data silently drops the width. Second, and worse: `None` would have to mean
///   *not recorded*, never *probably 64-bit*, because an overflow whose width was never observed
///   is not a 64-bit overflow and a default that supplies an unmeasured fact is a silent wrong
///   answer where a compile error is a loud one. Keeping `None` honest means `Error::new(span,
///   ErrorData::IntOverflow(v))` and the existing `int_overflow` constructor both build an
///   overflow with **no** width, so every consumer must branch on a state this crate would never
///   produce, forever. The payload makes that state unrepresentable instead: there is no way to
///   construct an `IntOverflow` without naming the width.
///
/// **Unrepresentable on both axes, and the second one had to be added.** The paragraph above used
/// to end by concluding that no path could report a width it did not attempt. That followed only
/// for a width-*less* overflow; a **wrong**-width one was still constructible, because the
/// constructor took the discriminant as an argument. [`IntOverflow`] records what closed it — the
/// free-width constructor is crate-private and `IntOverflow::checked` is the public door — and
/// it is worth naming the shape of the miss: the enumeration behind the original claim was over
/// every path *this crate* takes, all of which name a width they attempted, and said nothing
/// about the path a caller drives.
#[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ErrorData<S, T, Char = char, Exp = Expectation, StateError = ()> {
  /// One or more errors from the lexer.
  Lexer(LexerErrors<Char, StateError>),
  /// An integer literal is syntactically valid GraphQL but does not fit the width it was read
  /// at.
  ///
  /// The payload names the width as well as the spelling, because the
  /// `materialized-numbers` feature ships two readings of `Int` — see [`IntWidth`] — and
  /// "`2147483648` overflowed" is a different fact about the document under each. Raised only by
  /// the `materialized-numbers` productions; see `Error::int_overflow`.
  #[from(skip)]
  IntOverflow(IntOverflow<S>),
  /// A float literal is syntactically valid GraphQL but does not convert to a finite [`f64`].
  ///
  /// **No width here, and the asymmetry with [`IntOverflow`](Self::IntOverflow) is the point.**
  /// GraphQL's `Float` *is* IEEE 754 double precision (draft §3.5.2), so every materialising
  /// reading this crate ships converts it to [`f64`] and there is no second reading for a
  /// consumer to distinguish. A width here would name a distinction that does not exist.
  ///
  /// Raised only by the `materialized-numbers` productions; see `Error::float_overflow`.
  #[from(skip)]
  FloatOverflow(S),
  /// An enum value is invalid.
  #[from(skip)]
  InvalidEnumValue(S),
  /// A boolean value is invalid.
  #[from(skip)]
  InvalidBooleanValue(S),
  /// A null value is invalid.
  #[from(skip)]
  InvalidNullValue(S),
  /// A fragment name is invalid.
  #[from(skip)]
  InvalidFragmentName(S),
  /// A delimiter was not closed.
  Unclosed(Unclosed),
  /// An unexpected token was found.
  UnexpectedToken(UnexpectedToken<T, Exp>),
  /// An unexpected keyword was found.
  UnexpectedKeyword(UnexpectedKeyword<S>),
  /// An unexpected end was found in a variable value.
  UnexpectedEndOfVariableValue(UnexpectedEnd<VariableValueHint>),
  /// An unexpected end was found in an object field value.
  UnexpectedEndOfObjectFieldValue(UnexpectedEnd<ObjectFieldValueHint>),
  /// An unknown directive location was found.
  #[from(skip)]
  UnknownDirectiveLocation(S),
  /// An unknown operation type was found.
  #[from(skip)]
  UnknownOperationType(S),
  /// An unexpected end was found in an object type extension.
  UnexpectedEndOfObjectExtension(UnexpectedEnd<ObjectTypeExtensionHint>),
  /// An unexpected end was found in an interface type extension.
  UnexpectedEndOfInterfaceExtension(UnexpectedEnd<InterfaceTypeExtensionHint>),
  /// An unexpected end was found in an enum type extension.
  UnexpectedEndOfEnumExtension(UnexpectedEnd<EnumTypeExtensionHint>),
  /// An unexpected end was found in an input object type extension.
  UnexpectedEndOfInputObjectExtension(UnexpectedEnd<InputObjectTypeExtensionHint>),
  /// An unexpected end was found in a union type extension.
  UnexpectedEndOfUnionExtension(UnexpectedEnd<UnionTypeExtensionHint>),
  /// An unexpected end was found in a schema extension.
  UnexpectedEndOfSchemaExtension(UnexpectedEnd<SchemaExtensionHint>),
  /// An end of input was found.
  EndOfInput,
  /// Some other error.
  Other(std::borrow::Cow<'static, str>),
}

/// A parser error.
#[derive(Debug, Clone)]
pub struct Error<S, T, Char = char, Exp = Expectation, StateError = ()> {
  span: Span,
  data: ErrorData<S, T, Char, Exp, StateError>,
}

impl<S, T, Char, Exp, StateError> Error<S, T, Char, Exp, StateError> {
  /// Creates a new error.
  #[inline]
  pub const fn new(span: Span, data: ErrorData<S, T, Char, Exp, StateError>) -> Self {
    Self { span, data }
  }

  /// Creates an unexpected token error with an optional found token.
  #[inline]
  pub const fn maybe_unexpected_token(found: Option<T>, expected: Exp, span: Span) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedToken(UnexpectedToken::maybe_found(found, expected)),
    )
  }

  /// Creates an unexpected token error.
  #[inline]
  pub const fn unexpected_token(found: T, expected: Exp, span: Span) -> Self {
    Self::maybe_unexpected_token(Some(found), expected, span)
  }

  /// Creates an unexpected end in variable value error.
  #[inline]
  pub const fn unexpected_end_of_variable_value(hint: VariableValueHint, span: Span) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfVariableValue(UnexpectedEnd::with_name(
        0,
        CowStr::from_static("variable value"),
        hint,
      )),
    )
  }

  /// Creates an unexpected keyword error.
  #[inline]
  pub const fn unexpected_keyword(found: S, expected_kw: &'static str, span: Span) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedKeyword(UnexpectedKeyword::new(found, expected_kw)),
    )
  }

  /// Creates an unexpected end in object field value error.
  #[inline]
  pub const fn unexpected_end_of_object_field_value(
    hint: ObjectFieldValueHint,
    span: Span,
  ) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfObjectFieldValue(UnexpectedEnd::with_name(
        0,
        CowStr::from_static("object field value"),
        hint,
      )),
    )
  }

  /// Creates an unexpected end in object type extension error.
  #[inline]
  pub const fn unexpected_end_of_object_extension(
    span: Span,
    hint: ObjectTypeExtensionHint,
  ) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfObjectExtension(UnexpectedEnd::with_name(
        0,
        CowStr::from_static("object type extension"),
        hint,
      )),
    )
  }

  /// Creates an unexpected end in interface type extension error.
  #[inline]
  pub const fn unexpected_end_of_interface_extension(
    span: Span,
    hint: InterfaceTypeExtensionHint,
  ) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfInterfaceExtension(UnexpectedEnd::with_name(
        0,
        CowStr::from_static("interface type extension"),
        hint,
      )),
    )
  }

  /// Creates an unexpected end in enum type extension error.
  #[inline]
  pub const fn unexpected_end_of_enum_extension(span: Span, hint: EnumTypeExtensionHint) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfEnumExtension(UnexpectedEnd::with_name(
        0,
        CowStr::from_static("enum type extension"),
        hint,
      )),
    )
  }

  /// Creates an unexpected end in input object type extension error.
  #[inline]
  pub const fn unexpected_end_of_input_object_extension(
    span: Span,
    hint: InputObjectTypeExtensionHint,
  ) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfInputObjectExtension(UnexpectedEnd::with_name(
        0,
        CowStr::from_static("input object type extension"),
        hint,
      )),
    )
  }

  /// Creates an unexpected end in union type extension error.
  #[inline]
  pub const fn unexpected_end_of_union_extension(span: Span, hint: UnionTypeExtensionHint) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfUnionExtension(UnexpectedEnd::with_name(
        0,
        CowStr::from_static("union type extension"),
        hint,
      )),
    )
  }

  /// Creates an unexpected end in schema extension error.
  #[inline]
  pub const fn unexpected_end_of_schema_extension(span: Span, hint: SchemaExtensionHint) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfSchemaExtension(UnexpectedEnd::with_name(
        0,
        CowStr::from_static("schema extension"),
        hint,
      )),
    )
  }

  /// Creates an unclosed list error.
  #[inline]
  pub const fn unclosed_list(span: Span) -> Self {
    Self::new(span, ErrorData::Unclosed(Unclosed::List))
  }

  /// Creates an unclosed-parentheses error.
  #[inline]
  pub const fn unclosed_parentheses(span: Span) -> Self {
    Self::new(span, ErrorData::Unclosed(Unclosed::Parentheses))
  }

  /// Creates an unclosed object error.
  #[inline]
  pub const fn unclosed_object(span: Span) -> Self {
    Self::new(span, ErrorData::Unclosed(Unclosed::Object))
  }

  /// Creates an error from a lexer error.
  #[inline]
  pub const fn from_lexer_errors(err: LexerErrors<Char, StateError>, span: Span) -> Self {
    Self::new(span, ErrorData::Lexer(err))
  }

  /// Creates an invalid fragment name error.
  #[inline]
  pub const fn invalid_fragment_name(value: S, span: Span) -> Self {
    Self::new(span, ErrorData::InvalidFragmentName(value))
  }

  /// Creates an invalid enum value error.
  #[inline]
  pub const fn invalid_enum_value(value: S, span: Span) -> Self {
    Self::new(span, ErrorData::InvalidEnumValue(value))
  }

  /// Creates an invalid boolean value error.
  #[inline]
  pub const fn invalid_boolean_value(value: S, span: Span) -> Self {
    Self::new(span, ErrorData::InvalidBooleanValue(value))
  }

  /// Creates an invalid null value error.
  #[inline]
  pub const fn invalid_null_value(value: S, span: Span) -> Self {
    Self::new(span, ErrorData::InvalidNullValue(value))
  }

  /// Creates an unknown directive location error.
  #[inline]
  pub const fn unknown_directive_location(value: S, span: Span) -> Self {
    Self::new(span, ErrorData::UnknownDirectiveLocation(value))
  }

  /// Creates an unknown operation type error.
  #[inline]
  pub const fn unknown_operation_type(value: S, span: Span) -> Self {
    Self::new(span, ErrorData::UnknownOperationType(value))
  }

  /// Creates an unexpected end of input error.
  #[inline]
  pub const fn unexpected_end_of_input(span: Span) -> Self {
    Self::new(span, ErrorData::EndOfInput)
  }

  /// Creates an integer-out-of-range error from a payload that has already justified its width.
  ///
  /// **This is the producer, and it is what the feature gates** — the variant it builds is
  /// unconditional. See [`ErrorData`] for why the gate belongs here and not one level up, and
  /// [`graphql::syntactic::materialized`](crate::graphql::syntactic::value::materialized) for the
  /// documented bound that makes a specification-valid literal a *parse* error in that view.
  ///
  /// **It takes the payload rather than `(value, width)`, and that is the point.** A `width`
  /// parameter here would be a second free-width door beside the crate-private one
  /// [`IntOverflow`] closed: a caller could hand it any [`IntWidth`] and get an error naming a
  /// refusal that never happened. Taking an [`IntOverflow`] means the only widths that reach a
  /// report are the ones this crate's conversions attempted and the ones
  /// `IntOverflow::checked` agreed to.
  #[cfg(feature = "materialized-numbers")]
  #[cfg_attr(docsrs, doc(cfg(feature = "materialized-numbers")))]
  #[inline]
  pub const fn int_overflow(overflow: IntOverflow<S>, span: Span) -> Self {
    Self::new(span, ErrorData::IntOverflow(overflow))
  }

  /// Creates a float-out-of-range error, carrying the literal's source spelling.
  ///
  /// The producer, gated where [`Error::int_overflow`] is and for the same reason.
  #[cfg(feature = "materialized-numbers")]
  #[cfg_attr(docsrs, doc(cfg(feature = "materialized-numbers")))]
  #[inline]
  pub const fn float_overflow(value: S, span: Span) -> Self {
    Self::new(span, ErrorData::FloatOverflow(value))
  }

  /// Returns the span of the error.
  #[inline]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Returns the data of the error.
  #[inline]
  pub const fn data(&self) -> &ErrorData<S, T, Char, Exp, StateError> {
    &self.data
  }

  /// Returns a mutable reference to the data of the error.
  #[inline]
  pub const fn data_mut(&mut self) -> &mut ErrorData<S, T, Char, Exp, StateError> {
    &mut self.data
  }

  /// Consumes the error and returns its data.
  #[inline]
  pub fn into_data(self) -> ErrorData<S, T, Char, Exp, StateError> {
    self.data
  }
}

type DefaultErrorsContainer<S, T, Char = char, Exp = Expectation, StateError = ()> =
  std::vec::Vec<Error<S, T, Char, Exp, StateError>>;

/// A container for storing multiple parser errors.
#[derive(Debug, Clone, From, Into, Deref, DerefMut, AsMut, AsRef)]
pub struct Errors<S, T, Char = char, Exp = Expectation, StateError = ()>(
  DefaultErrorsContainer<S, T, Char, Exp, StateError>,
);

impl<S, T, Char, Exp, StateError> Default for Errors<S, T, Char, Exp, StateError> {
  #[inline]
  fn default() -> Self {
    Self(DefaultErrorsContainer::default())
  }
}

impl<S, T, Exp, Char, StateError> From<Error<S, T, Char, Exp, StateError>>
  for Errors<S, T, Char, Exp, StateError>
{
  #[inline]
  fn from(error: Error<S, T, Char, Exp, StateError>) -> Self {
    Self(core::iter::once(error).collect())
  }
}

impl<S, T, Char, Exp, StateError> Errors<S, T, Char, Exp, StateError> {
  /// Creates a new empty errors container with the given capacity.
  #[inline]
  pub fn with_capacity(capacity: usize) -> Self {
    Self(DefaultErrorsContainer::with_capacity(capacity))
  }
}

impl<S, T, Char, Exp, StateError> IntoIterator for Errors<S, T, Char, Exp, StateError> {
  type Item = Error<S, T, Char, Exp, StateError>;
  type IntoIter = <DefaultErrorsContainer<S, T, Char, Exp, StateError> as IntoIterator>::IntoIter;

  #[inline]
  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}

impl<S, T, Char, Exp, StateError> Extend<Error<S, T, Char, Exp, StateError>>
  for Errors<S, T, Char, Exp, StateError>
{
  #[inline]
  fn extend<I: IntoIterator<Item = Error<S, T, Char, Exp, StateError>>>(&mut self, iter: I) {
    self.0.extend(iter);
  }
}

/// The GraphQL dialect error, keyed to the source slice `S` and the concrete
/// GraphQL syntactic token kind.
pub type GraphqlError<S> = Error<S, SyntacticTokenKind, char, Expectation>;

/// The GraphQL dialect error container — the error type a
/// [`ParseCtx`](crate::combinator::ParseCtx) over a GraphQL lexer emits.
pub type GraphqlErrors<S> = Errors<S, SyntacticTokenKind, char, Expectation>;

// ---- `From` glue -----------------------------------------------------------
//
// The atoms bound `ErrorOf: From<…>` over tokora's generic error families; each
// conversion below lands the dialect side. The `Lang` marker stays generic so the
// bound is satisfied for `Lang = GraphQL` (and any other marker a production
// pins); Tokora token errors use the concrete `SyntacticTokenKind`, and found
// tokens are reduced to that dialect token kind.
// A single concrete Tokora expected kind is mapped into the corresponding
// GraphQL expectation, while absent or multi-kind expectations retain the historic
// `Name` fallback because [`ErrorData::UnexpectedToken`] has one expectation slot.
// Diagnostics-carrying families (missing separator/element, full container, too
// few, lexer errors) map to `Other` exactly as the frozen glue did — these are
// emitter paths a `Vec` container and a fail-fast context rarely reach.

#[inline]
fn expectation_from_token_kind(kind: SyntacticTokenKind) -> Expectation {
  match kind {
    SyntacticTokenKind::Identifier => Expectation::Name,
    SyntacticTokenKind::Int => Expectation::IntValue,
    SyntacticTokenKind::Float => Expectation::FloatValue,
    SyntacticTokenKind::InlineString => Expectation::InlineString,
    SyntacticTokenKind::BlockString => Expectation::BlockString,
    SyntacticTokenKind::Dollar => Expectation::Dollar,
    SyntacticTokenKind::LParen => Expectation::LParen,
    SyntacticTokenKind::RParen => Expectation::RParen,
    SyntacticTokenKind::Spread => Expectation::Spread,
    SyntacticTokenKind::Colon => Expectation::Colon,
    SyntacticTokenKind::Equal => Expectation::Equal,
    SyntacticTokenKind::At => Expectation::At,
    SyntacticTokenKind::LBracket => Expectation::LBracket,
    SyntacticTokenKind::RBracket => Expectation::RBracket,
    SyntacticTokenKind::LBrace => Expectation::LBrace,
    SyntacticTokenKind::RBrace => Expectation::RBrace,
    SyntacticTokenKind::Pipe => Expectation::Pipe,
    SyntacticTokenKind::Bang => Expectation::Bang,
    SyntacticTokenKind::Ampersand => Expectation::Ampersand,
    // `#[allow]` and not a deletion. `SyntacticTokenKind` is `#[non_exhaustive]`, which forced this
    // arm while the lexer was a separate crate; with the crates merged (#83) the compiler can see
    // every variant from here and calls it unreachable. The arm stays because deleting it would
    // turn "a new token kind falls back to `Name`" into "a new token kind fails to compile" — a
    // real change to this function's contract, and this merge is a relocation. Revisiting that
    // choice is follow-up work, not part of the move.
    #[allow(unreachable_patterns)]
    _ => Expectation::Name,
  }
}

#[inline]
fn expectation_from_tokora(expected: Option<Expected<'_, SyntacticTokenKind>>) -> Expectation {
  match expected {
    Some(Expected::One(kind)) => expectation_from_token_kind(kind),
    _ => Expectation::Name,
  }
}

impl<'a, S, Lang: ?Sized>
  From<TokUnexpectedToken<'a, SyntacticToken<S>, SyntacticTokenKind, Span, Lang>>
  for GraphqlErrors<S>
{
  #[inline]
  fn from(err: TokUnexpectedToken<'a, SyntacticToken<S>, SyntacticTokenKind, Span, Lang>) -> Self {
    let (span, found, expected) = err.into_components();
    match found {
      Some(token) => {
        GraphqlError::unexpected_token(token.kind(), expectation_from_tokora(expected), span).into()
      }
      None => GraphqlError::unexpected_end_of_input(span).into(),
    }
  }
}

impl<'a, S, Lang: ?Sized>
  From<SeparatedError<'a, SyntacticToken<S>, SyntacticTokenKind, Span, Lang>> for GraphqlErrors<S>
{
  #[inline]
  fn from(err: SeparatedError<'a, SyntacticToken<S>, SyntacticTokenKind, Span, Lang>) -> Self {
    Self::from(err.into_inner())
  }
}

impl<S, Kind: Clone, Lang: ?Sized> From<MissingToken<'_, Kind, usize, Lang>> for GraphqlErrors<S> {
  #[inline]
  fn from(err: MissingToken<'_, Kind, usize, Lang>) -> Self {
    let off = err.offset();
    GraphqlError::new(
      Span::new(off, off),
      ErrorData::Other(std::borrow::Cow::Borrowed("missing token")),
    )
    .into()
  }
}

impl<S, Lang: ?Sized> From<MissingSyntax<usize, Lang>> for GraphqlErrors<S> {
  #[inline]
  fn from(err: MissingSyntax<usize, Lang>) -> Self {
    let off = err.offset();
    GraphqlError::new(
      Span::new(off, off),
      ErrorData::Other(std::borrow::Cow::Borrowed("missing element")),
    )
    .into()
  }
}

impl<S, Lang: ?Sized> From<FullContainer<Span, Lang>> for GraphqlErrors<S> {
  #[inline]
  fn from(err: FullContainer<Span, Lang>) -> Self {
    GraphqlError::new(
      *err.span(),
      ErrorData::Other(std::borrow::Cow::Borrowed("container full")),
    )
    .into()
  }
}

impl<S, Lang: ?Sized> From<TooFew<Span, Lang>> for GraphqlErrors<S> {
  #[inline]
  fn from(err: TooFew<Span, Lang>) -> Self {
    GraphqlError::new(
      err.span(),
      ErrorData::Other(std::borrow::Cow::Borrowed("too few elements")),
    )
    .into()
  }
}

// The end-of-input conversion is written `Set`-generic so the one impl covers both
// members tokora's `FromTokenErrors` bundle names: the default `&'static str` set the
// `_or_stop` family raises, and the `&'static [Kind]` classification table a committed
// dispatch driver feeds straight into the diagnostic.
impl<S, Lang: ?Sized, Set: Clone + 'static> From<UnexpectedEot<usize, Lang, Set>>
  for GraphqlErrors<S>
{
  #[inline]
  fn from(err: UnexpectedEot<usize, Lang, Set>) -> Self {
    let off = err.offset();
    GraphqlError::unexpected_end_of_input(Span::new(off, off)).into()
  }
}

// One impl absorbs every delimiter pair. `Unclosed` carries the pair's name as data, so
// the pair is discriminated at run time on `name_ref` rather than by a `From` impl per
// pair; `Delimiter` is generic, so the catch-all arm is mandatory. GraphQL's grammar only
// ever opens `()`, `[]` and `{}`, so the catch-all is unreachable in practice — it still
// has to produce an error rather than panic.
impl<'inp, S, L, Lang: ?Sized> FromUnclosed<'inp, L, Lang> for GraphqlErrors<S>
where
  L: Lexer<'inp, Span = Span>,
{
  #[inline]
  fn from_unclosed<Delimiter>(err: TokoraUnclosed<Delimiter, Span, Lang>) -> Self {
    let span = err.span();
    match err.name_ref() {
      "[]" => GraphqlError::unclosed_list(span).into(),
      "()" => GraphqlError::unclosed_parentheses(span).into(),
      "{}" => GraphqlError::unclosed_object(span).into(),
      _ => GraphqlError::new(
        span,
        ErrorData::Other(std::borrow::Cow::Borrowed("unclosed delimiter")),
      )
      .into(),
    }
  }
}

impl<S, Char, StateError> From<LexerErrors<Char, StateError>> for GraphqlErrors<S> {
  #[inline]
  fn from(_err: LexerErrors<Char, StateError>) -> Self {
    GraphqlError::new(
      Span::new(0, 0),
      ErrorData::Other(std::borrow::Cow::Borrowed("lexer error")),
    )
    .into()
  }
}

#[cfg(test)]
mod tests {
  use smear_lexer::graphql::syntactic::{SyntacticLexer, SyntacticToken, SyntacticTokenKind};
  use tokora::{FatalContext, Lexer, SimpleSpan};

  use super::{ErrorData, Expectation, GraphqlErrors, SeparatedError, TokUnexpectedToken};
  use crate::{combinator::ParseCtx, graphql::GraphQL};

  fn assert_parse_ctx<'inp, L, Ctx>()
  where
    L: Lexer<'inp>,
    Ctx: ParseCtx<'inp, L, GraphQL>,
  {
  }

  #[test]
  fn graphql_error_is_parse_ctx_over_str_and_slice() {
    // `Fatal<GraphqlErrors<slice>>` is a complete `ParseCtx` over both source
    // representations — the whole point of the `From` glue above.
    assert_parse_ctx::<
      SyntacticLexer<'_, str>,
      FatalContext<'_, SyntacticLexer<'_, str>, GraphqlErrors<&str>, GraphQL>,
    >();
    assert_parse_ctx::<
      SyntacticLexer<'_, [u8]>,
      FatalContext<'_, SyntacticLexer<'_, [u8]>, GraphqlErrors<&[u8]>, GraphQL>,
    >();
  }

  type TokoraTokenError = TokUnexpectedToken<
    'static,
    SyntacticToken<&'static str>,
    SyntacticTokenKind,
    SimpleSpan,
    GraphQL,
  >;

  fn unexpected_with(expected: SyntacticTokenKind) -> TokoraTokenError {
    TokUnexpectedToken::expected_one_with_found(
      SimpleSpan::new(4, 9),
      SyntacticToken::Identifier("found"),
      expected,
    )
  }

  fn assert_expected(errors: GraphqlErrors<&'static str>, expected: Expectation) {
    let error = errors
      .into_iter()
      .next()
      .expect("Tokora conversion emits one error");
    assert_eq!(error.span(), SimpleSpan::new(4, 9));
    let ErrorData::UnexpectedToken(unexpected) = error.data() else {
      panic!(
        "expected unexpected-token diagnostic, got {:?}",
        error.data()
      );
    };
    assert_eq!(unexpected.found(), Some(&SyntacticTokenKind::Identifier));
    assert_eq!(unexpected.expected(), &expected);
  }

  #[test]
  fn tokora_unexpected_token_maps_single_expected_kinds() {
    for (kind, expected) in [
      (SyntacticTokenKind::Identifier, Expectation::Name),
      (SyntacticTokenKind::Int, Expectation::IntValue),
      (SyntacticTokenKind::Float, Expectation::FloatValue),
      (SyntacticTokenKind::InlineString, Expectation::InlineString),
      (SyntacticTokenKind::BlockString, Expectation::BlockString),
      (SyntacticTokenKind::Dollar, Expectation::Dollar),
      (SyntacticTokenKind::LParen, Expectation::LParen),
      (SyntacticTokenKind::RParen, Expectation::RParen),
      (SyntacticTokenKind::Spread, Expectation::Spread),
      (SyntacticTokenKind::Colon, Expectation::Colon),
      (SyntacticTokenKind::Equal, Expectation::Equal),
      (SyntacticTokenKind::At, Expectation::At),
      (SyntacticTokenKind::LBracket, Expectation::LBracket),
      (SyntacticTokenKind::RBracket, Expectation::RBracket),
      (SyntacticTokenKind::LBrace, Expectation::LBrace),
      (SyntacticTokenKind::RBrace, Expectation::RBrace),
      (SyntacticTokenKind::Pipe, Expectation::Pipe),
      (SyntacticTokenKind::Bang, Expectation::Bang),
      (SyntacticTokenKind::Ampersand, Expectation::Ampersand),
    ] {
      assert_expected(unexpected_with(kind).into(), expected);
    }
  }

  #[test]
  fn tokora_unrepresentable_expected_sets_keep_the_name_fallback() {
    static ONE_OF: [SyntacticTokenKind; 2] =
      [SyntacticTokenKind::LBrace, SyntacticTokenKind::LBracket];

    let without_expected =
      TokoraTokenError::of(SimpleSpan::new(4, 9)).with_found(SyntacticToken::Identifier("found"));
    assert_expected(without_expected.into(), Expectation::Name);

    let one_of = TokoraTokenError::expected_one_of_with_found(
      SimpleSpan::new(4, 9),
      SyntacticToken::Identifier("found"),
      &ONE_OF,
    );
    assert_expected(one_of.into(), Expectation::Name);

    assert_expected(
      SeparatedError::leading(unexpected_with(SyntacticTokenKind::LBrace)).into(),
      Expectation::LBrace,
    );
  }

  mod census;
  #[cfg(feature = "materialized-numbers")]
  mod int_overflow;
}
