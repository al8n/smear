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
  SyntaxErrorKind,
  graphql::{
    error::{LexerErrorData, LexerErrors},
    syntactic::{SyntacticToken, SyntacticTokenKind},
  },
  tokora::error::UnexpectedEnd,
};
use tokora::{
  Lexer, SimpleSpan as Span,
  diagnostic::{Code, Diagnose, Label, Location, PathSegment, Severity},
  emitter::FromUnclosed,
  error::{
    FullContainer, MaybeTerminal, MissingSyntax, MissingToken, SeparatedError, TooFew,
    Unclosed as TokoraUnclosed, UnexpectedEot, UnexpectedToken as TokUnexpectedToken,
  },
  utils::{CowStr, Expected},
};

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
  /// A keyword was expected: the spelling itself, such as `on` or `fragment`.
  Keyword(&'static str),
  /// A production was expected, named by a noun phrase that reads after "expected" — "a type
  /// definition", "a definition or an extension".
  ///
  /// [`Keyword`](Self::Keyword) used to carry these too, which made "expected `type definition`"
  /// read as though `type definition` were something to type.
  Production(&'static str),
  /// Nothing more specific is known than that the token found is not one the grammar takes here.
  ///
  /// What a conversion lands on when tokora names no expected kind, or several: the one
  /// expectation slot cannot hold a set, and naming one member of it — `Name` was the old answer —
  /// would claim a precision the parse does not have. It renders as nothing, so a message drops
  /// its "expected …" clause rather than naming a guess.
  Unspecified,
}

/// What a message says was expected, as the object of "expected …": punctuation and keywords in
/// backticks, and every other class as a noun phrase — "a name", "a value", "a selection".
///
/// [`Unspecified`](Expectation::Unspecified) renders as nothing at all, which is what tells a
/// message to drop the clause.
impl core::fmt::Display for Expectation {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::InlineString => f.write_str("a string"),
      Self::BlockString => f.write_str("a block string"),
      Self::Dollar => f.write_str("`$`"),
      Self::LParen => f.write_str("`(`"),
      Self::RParen => f.write_str("`)`"),
      Self::Spread => f.write_str("`...`"),
      Self::Colon => f.write_str("`:`"),
      Self::Equal => f.write_str("`=`"),
      Self::At => f.write_str("`@`"),
      Self::LBracket => f.write_str("`[`"),
      Self::RBracket => f.write_str("`]`"),
      Self::LBrace => f.write_str("`{`"),
      Self::RBrace => f.write_str("`}`"),
      Self::Pipe => f.write_str("`|`"),
      Self::Bang => f.write_str("`!`"),
      Self::Ampersand => f.write_str("`&`"),
      Self::ConstInputValue => f.write_str("a constant value"),
      Self::InputValue => f.write_str("a value"),
      Self::Type => f.write_str("a type"),
      Self::Selection => f.write_str("a selection"),
      Self::VariableDefinition => f.write_str("a variable definition"),
      Self::OperationType => f.write_str("an operation type"),
      Self::ExecutableDefinition => f.write_str("an executable definition"),
      Self::OperationTypeOrFragment => f.write_str("an operation type or `fragment`"),
      Self::FragmentName => f.write_str("a fragment name"),
      Self::Name => f.write_str("a name"),
      Self::OperationName => f.write_str("an operation name"),
      Self::DirectiveLocation => f.write_str("a directive location"),
      Self::FragmentSpreadOrInlineFragment => {
        f.write_str("a fragment spread or an inline fragment")
      }
      Self::IntValue => f.write_str("an integer"),
      Self::BooleanValue => f.write_str("a boolean"),
      Self::FloatValue => f.write_str("a float"),
      Self::NullValue => f.write_str("`null`"),
      Self::EnumValue => f.write_str("an enum value"),
      Self::StringValue => f.write_str("a string"),
      Self::Keyword(keyword) => write!(f, "`{keyword}`"),
      Self::Production(production) => f.write_str(production),
      Self::Unspecified => Ok(()),
    }
  }
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
/// No `#[non_exhaustive]`, like [`Unclosed`] beside it and unlike every other public enum in
/// this file. The whole reason to read this is to branch on the two, and a wildcard arm forced
/// onto every consumer would be a wildcard over a two-element closed set.
///
/// A third width is not a variant added here in isolation. It used to mean a third marker and a
/// third value tree in [`ast`](crate::graphql::ast); it now means a third impl of the **sealed**
/// `MaterializedInt` — `syntactic::value::materialized`'s trait, whose associated `WIDTH` is a
/// value of this enum. That is less to write and the same to review, and the seal is what keeps
/// this list and that impl list the same length: an out-of-crate impl would have to answer
/// `WIDTH` with one of the two variants below while being neither, which is the forged width
/// [`IntOverflow`]'s crate-private constructor exists to rule out, arriving through a door in
/// another module.
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
/// # Why the checked door survives a refactor that removed every other supplied width
///
/// `refactor/materialised-generic` made the width a function of the payload type — a production
/// reads `MaterializedInt::WIDTH` off `I` and nothing supplies one — which raises the question of
/// whether a door taking an [`IntWidth`] as an *argument* still has a caller. It does, and the two
/// cases are different in kind:
///
/// * inside the crate, the width is decided by a type the compiler already chose, so an argument
///   would be a second chance to be wrong about something already known. There is none left.
/// * outside it, the width may be a **value** — a gateway told at run time which reading it
///   enforces, a report replayed from a log, a validator coercing a variable against `Int`.
///   Turning the argument into a type parameter would only move that choice into a turbofish, not
///   remove it, and what makes the door safe was never the argument's shape: it is `overflows`,
///   the crate-private decider below, refusing a pair the production at that width would have
///   converted. (Code font and not a link, for the reason the parenthesis above gives.)
///
/// Deleting it was the other option and it costs more than it saves.
/// [`ErrorData::IntOverflow`] is a public variant whose payload would then have no public
/// constructor at all, so a consumer could build every other variant of that enum and not this
/// one — a hole in the error surface bought in exchange for a check that already holds.
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
  /// [`IntWidth`]. It answers `Ok` exactly when `value` is a GraphQL `IntValue` that the
  /// materialising production at `width` would have refused, decided by the very readers those
  /// productions use. Everything else comes back as `Err(value)`: a literal that fits, a float,
  /// a name, an empty slice.
  ///
  /// **`IntValue` means draft §2.9.1's, asked of the lexer rather than restated here.** So a
  /// leading zero is not one — `02147483648` is `LeadingZeros` to this crate's lexer, no
  /// production ever converts it, and a payload quoting it would name a refusal that could not
  /// have happened. Nor is a leading `+`, which the grammar has no unary form of.
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
  ///
  /// // A leading zero is not an `IntValue`, whatever the digits after it add up to.
  /// assert_eq!(IntOverflow::checked("007", IntWidth::I32), Err("007"));
  /// assert_eq!(
  ///   IntOverflow::checked("02147483648", IntWidth::I32),
  ///   Err("02147483648"),
  /// );
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
/// # `#[non_exhaustive]`, and the rule that decides it
///
/// `7b9b293` took the attribute off four enums in this crate and wrote the rule down:
/// **`#[non_exhaustive]` belongs on vocabulary smear owns, not on vocabulary the GraphQL
/// specification enumerates.** [`ast::Selection`](crate::graphql::ast::Selection) carries it in
/// prose — draft §2.4 is `Selection : Field | FragmentSpread | InlineFragment`, so a variant added
/// there would mean parsing something that is not GraphQL, and the attribute was advertising an
/// evolution freedom the type structurally does not have. For those four the `E0004` a downstream
/// exhaustive match gets is *information*: the language moved.
///
/// This enum is the other side of that rule, and it transcribes nothing. Its list is what this
/// parser chose to distinguish about a document it refused, and two parts of the list say so
/// without being asked:
///
/// - [`Other`](Self::Other) exists **because** the list is open. The six `From` conversions at the
///   foot of this file funnel tokora diagnostics into it exactly where `ErrorData` has no variant
///   for them; a spec-transcribed enum has no `Other`.
/// - [`IntOverflow`](Self::IntOverflow) and [`FloatOverflow`](Self::FloatOverflow) are not grammar
///   refusals at all. Draft §2.9.1 bounds an `IntValue`'s digits at nothing, and these two exist
///   because this crate ships a *materialising* reading — they arrived with a feature.
///
/// So the list is ours, it has grown for our reasons, and it is committed to growing further:
/// `graphqlx` is a second dialect in this crate today and more dialects are the plan.
///
/// **It has also shrunk, and for the same reason.** Fourteen variants that no parse produced —
/// `InvalidEnumValue`, `InvalidBooleanValue`, `InvalidNullValue`, `InvalidFragmentName`,
/// `UnexpectedKeyword`, `UnexpectedEndOfVariableValue`, `UnknownDirectiveLocation`,
/// `UnknownOperationType` and six per-extension `UnexpectedEndOf…Extension` variants — were
/// removed before 0.1. Every variant left answers a diagnostic code meant to outlive a rewording,
/// and a code for an error that cannot happen is a promise about nothing.
///
/// ## What the attribute costs, measured rather than argued
///
/// One site in the workspace. `cargo check --workspace --all-features --all-targets --keep-going`
/// with the attribute applied reports exactly one `error[E0004]`, in `smear-smoke` — the one crate
/// the attribute binds that held a wildcard-free match. Every other consumer in the tree already
/// reaches this type through `matches!`, a `let`-else, or a match with a wildcard, because looking
/// one diagnostic up is what a caller does with a diagnostic. That probe is **replaced and not
/// deleted**; the section below says what replaced it.
///
/// ## Why now, and what the delay already cost
///
/// Adding the attribute is itself a source-breaking change, so its price is paid once whenever it
/// is paid, and every release before then adds consumers to charge it to. This workspace is
/// `0.0.0` with nothing published, so today's price is the one site above and it only rises. pql
/// reached the identical conclusion for `PqlError` in its stage 0, on this reasoning, and recorded
/// it there.
///
/// The delay has already been charged for once. #160 wanted a twenty-third variant and could not
/// have one, because under an exhaustive enum a new variant is `E0004` in every downstream
/// exhaustive match — including consumers who never touch integer overflow. It reshaped
/// `IntOverflow`'s payload instead, which is a break of its own. The alternatives section below is
/// where that choice is recorded, and it is re-priced there rather than rewritten: the attribute
/// removes the argument that decided it and does not remove the other two.
///
/// ## What the attribute takes, and where the notice moved
///
/// No consumer can be told at build time that this vocabulary grew: making a downstream match
/// untotal is the whole of what `#[non_exhaustive]` does. The notice does not disappear, it moves
/// in-crate, where the attribute is inert. `error_data_variant_census` matches all 12 variants
/// wildcard-free in **every** configuration, so a new one is `E0004` there before it can reach
/// anybody, and it must carry a public producer before that test is green again.
///
/// What survives outside the crate is the half a wildcard cannot swallow, and `smear-smoke`'s
/// `error_data_variant_tag` holds it: every variant is named there and reachable from a dependent
/// through a public constructor, with the wildcard arm the attribute forces returning `None` so an
/// unlisted variant is a **reported failure** rather than a quiet fall-through.
///
/// ## The two enums beside it that stay exhaustive
///
/// Neither is an oversight, and both are the rule applied rather than an exception to it.
/// [`Unclosed`] is the three delimiter pairs GraphQL's grammar opens; a fourth belongs to another
/// dialect, and `graphqlx`'s own `Unclosed` is where the angle pair lives. [`IntWidth`]'s own doc
/// argues its case: it is read precisely in order to branch on two, so a forced wildcard there
/// would be a wildcard over a closed two-element set.
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
/// configuration — 12 variants, always — and builds a sample through a public constructor or
/// conversion for each one whose producer that configuration compiled: 12 samples with the
/// feature, 10 without.
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
/// - *A twenty-third variant, `IntOverflow32`.* **Re-priced by the attribute above, and kept at
///   the price it was decided at rather than rewritten to the price it has now.** When #160 chose,
///   this enum was exhaustive and a new variant was `E0004` in every downstream exhaustive match —
///   a break too, and a wider one, since it reaches consumers who never touch integer overflow at
///   all. That argument is gone: an additive variant now costs a downstream consumer nothing, and
///   it was the argument that decided the round.
///
///   Two survive it, and neither is about breakage. A variant *pair* puts the width in the
///   discriminant, which makes it unforgettable and leaves it unchecked — `IntOverflow32(v)` for a
///   `v` that fits `i32` is still constructible and still names a refusal that never happened, so
///   the hole `IntOverflow::checked` closed would have to be closed once per width instead of
///   once. And a consumer who wants *"an integer overflowed"* would match one name per width
///   forever, where the payload leaves one variant and moves the width into a field. What the
///   undecided attribute cost was the rounds spent reaching that, not the shape reached.
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
#[non_exhaustive]
pub enum ErrorData<S, T, Char = char, Exp = Expectation, StateError = ()> {
  /// One or more errors from the lexer.
  Lexer(LexerErrors<Char, StateError>),
  /// A lexer refusal on the syntactic path, by its kind alone.
  ///
  /// The syntactic container pins `char` and `()` where a lexer batch's are generic, so the batch
  /// itself cannot ride in [`Lexer`](Self::Lexer) there. What its `From<LexerErrors>` keeps is the
  /// first error's span, as this error's, and that error's [`SyntaxErrorKind`] — so an
  /// unterminated string, a bad escape or a leading zero reports its own code rather than one
  /// shared "lexer error". Never terminal: a `State` refusal routes to
  /// [`TerminalEndOfInput`](Self::TerminalEndOfInput) instead.
  #[from(skip)]
  Lexical(SyntaxErrorKind),
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
  /// A delimiter was not closed.
  Unclosed(Unclosed),
  /// An unexpected token was found.
  UnexpectedToken(UnexpectedToken<T, Exp>),
  /// An unexpected end was found in an object field value.
  UnexpectedEndOfObjectFieldValue(UnexpectedEnd<ObjectFieldValueHint>),
  /// A **genuine** end of input was found: the production asked for more and the document had
  /// ended.
  ///
  /// The recoverable half of a pair — [`TerminalEndOfInput`](Self::TerminalEndOfInput) is the
  /// other — and the split is smear issue #177. Only the offset separates the two in tokora's
  /// value; only this enum can carry the separation into a [`MaybeTerminal`] arm.
  EndOfInput,
  /// An end of input that stands in for a **terminal scanner stop** — smear issue #177.
  ///
  /// A resource-limit trip, or the poison boundary it latches, surfaced at a committed leaf or a
  /// delimited close as the end-of-input error that leaf would have raised anyway. tokora marks
  /// it — [`UnexpectedEnd::is_terminal`](tokora::error::UnexpectedEnd::is_terminal) — and the mark
  /// is the *only* thing that separates it from
  /// [`EndOfInput`](Self::EndOfInput): same carrier, same offset, opposite verdict. No input
  /// clears a tripped limit, so a root that resynchronises past this one re-reads a document the
  /// scanner has already given up on.
  ///
  /// **A variant and not a `bool` field on [`EndOfInput`](Self::EndOfInput)**, and the two reasons
  /// point in opposite directions.
  ///
  /// *A field is the source-breaking option.* `#[non_exhaustive]` protects this enum against
  /// **additions**; it does nothing for a variant that changes shape. `EndOfInput(bool)` makes
  /// every downstream pattern spelling `ErrorData::EndOfInput` an `E0532`, in every configuration.
  /// This variant breaks nothing, for the reason
  /// [`NestingLimitExceeded`](Self::NestingLimitExceeded) broke nothing: every out-of-crate
  /// exhaustive match already carries the wildcard the attribute forces, and a unit variant binds
  /// no payload.
  ///
  /// *And a field is the one the gates read least well.* Three things key on this enum, and a
  /// payload flip is visible to exactly one of them. `error_data_variant_census`'s generated `tag`
  /// matches `ErrorData::$variant { .. }`, which fits a tuple variant and cannot see inside it —
  /// that census says as much about itself. This impl's arm would become `Self::EndOfInput(t) =>
  /// *t`, which compiles once and then forces no decision ever again. Only
  /// `smear-parser/tests/enum_attribute_parity.rs`'s payload axis would notice. A variant is
  /// `E0004` in both wildcard-free matches **and** a named row in that table.
  ///
  /// Reached only through the two `From<UnexpectedEot>` conversions — the syntactic one in this
  /// file and the one `lossless_error_impls!` generates — each of which routes on
  /// `err.is_terminal()`. [`Error::terminal_end_of_input`] is the constructor they and the census
  /// share.
  #[from(skip)]
  TerminalEndOfInput,
  /// The parse tried to descend one level past the nesting budget.
  ///
  /// **A variant rather than an [`Other`](Self::Other) message, and the reason is that something
  /// reads it back.** The conversion this lands through
  /// ([`FromNestingLimit`](crate::lossless::depth::FromNestingLimit)) built
  /// `Other("nesting limit exceeded")`, argued for on the grounds that a depth trip reaches a
  /// consumer through `Parse::diagnostics`, which keeps the span and the severity and drops the
  /// typed payload — so no *consumer* could tell a variant from a message. That was true and was
  /// never the whole question: smear issue #169's repair makes the **parser itself** ask whether
  /// an error is this one, at every document root that catches, and a discriminator that is a
  /// string is one edit away from answering `false` forever with nothing failing.
  ///
  /// The reader is this enum's [`MaybeTerminal`] arm, which answers `true` off the variant
  /// itself — so the ask and the construction are one enum apart rather than one string
  /// comparison apart.
  ///
  /// **It is no longer the only reader, and smear issue #178 is that change.** The substrate's
  /// crate-private `root_turn` reads tokora's own resource-trip counter beside this arm, so a
  /// refusal ends the document even where the arm answers `false`. That mattered most for the
  /// generic layer the substrate used to publish, whose error type was a consumer's rather than
  /// this one; smear PR #189's round 5 withdrew that layer, and the reading stays because it is
  /// what makes the stop the *input's* fact rather than the error value's. The arm is still
  /// asserted at the value in `error/tests/terminal.rs`, because it is what a **scanner** stop
  /// rides and no published witness sees that.
  #[from(skip)]
  NestingLimitExceeded,
  /// The input's **durable** token budget refused an item, so the parse stopped reading — smear
  /// issue #193.
  ///
  /// [`NestingLimitExceeded`](Self::NestingLimitExceeded)'s twin at the other resource, and a
  /// variant for a stronger reason than that one's. There the argument was that the *parser*
  /// reads the discriminator back; here nothing else exists at all. tokora refuses the item at
  /// its lexing chokepoint and states plainly that the refusal *cannot report itself* — there is
  /// no diagnostic channel for it, so the item is dropped silently, the poison boundary is
  /// latched, and a root loop's next peek answers `None`. Measured with the report planted away,
  /// on a document chosen so that nothing else can speak — 400 definitions of `type Tn { f: Int }` — a document every root here takes in silence — under a durable ceiling of 100 — the
  /// parse came back with **0** diagnostics and a tree covering the whole source, which is
  /// exactly what a document that parsed looks like. (The measurement needs that document:
  /// `[ type ] ` x2000, which the rest of this repair is measured on, reports its own grammar
  /// errors and would have hidden the silence behind them.)
  ///
  /// So this is the whole report, and its [`MaybeTerminal`] arm is what keeps the drain above the
  /// root off the tail. `crate::lossless::depth::drain_unless_stopped` mints it, once per
  /// refusal, off the input's own
  /// [`refused_an_item`](tokora::input::TokenBudgetTally::refused_an_item) bit.
  #[from(skip)]
  TokenBudgetExhausted,
  /// Some other error.
  Other(std::borrow::Cow<'static, str>),
}

/// Which of these errors **end the parse** rather than being something to recover from.
///
/// tokora's [`MaybeTerminal`] is the notion, and this is the arm-per-source the trait asks for.
/// The parse side reads it at every document root: a root loop that resynchronises past a
/// terminal error re-reads a document the parse has already stopped on, which is smear issue
/// #169 — and #169's first repair asked a *narrower* question (is this the nesting refusal?) that
/// three other terminal sources walk straight past.
///
/// # The arms, and the rule each one came from
///
/// * **Delegated** — [`UnexpectedEndOfObjectFieldValue`](Self::UnexpectedEndOfObjectFieldValue),
///   the one [`UnexpectedEnd`] variant. `UnexpectedEnd` answers for itself: its flag is raised when
///   a *scanner* stop built it and clear for a genuine end of a production, so the value decides
///   rather than the variant. That is step 1 of the trait's rule.
/// * **`true`, always** — [`NestingLimitExceeded`](Self::NestingLimitExceeded). A frame budget is
///   never cleared by more input.
/// * **`true`, always** — [`TokenBudgetExhausted`](Self::TokenBudgetExhausted). Neither is a
///   durable token budget, and for a reason one scale stronger: the tally lives outside every
///   rollback, no public mutator lowers it, and `is_exhausted` is re-derived from it on every
///   entry. A `false` here would not merely resynchronise past a refusal — it would run the drain
///   over a tail the input has already refused to lex, on the one carrier that reports nothing
///   else.
/// * **`true`, always** — [`TerminalEndOfInput`](Self::TerminalEndOfInput). It exists to carry
///   tokora's own mark, so answering anything else would discard the flag one layer later than the
///   conversion used to.
/// * **`true` when it holds a state refusal** — [`Lexer`](Self::Lexer). This is the arm the trait's
///   rule warns catches people, and smear is exactly the shape it warns about: the lossless keying
///   pins `StateError = LimitExceeded`, so `LexerErrorData::State` **is** a tripped
///   `smear_lexer::limits` budget. It is also where a scanner trip lands when the emitter *rejects*
///   its diagnostic — tokora constructs no `UnexpectedEnd` on that path, so nothing marks it and
///   this arm is what answers. It answers only for the keying that can *hold* the batch, which is
///   the lossless one; the syntactic container pins `StateError = ()` and cannot, so its
///   `From<LexerErrors>` reads the same `State` variant and routes to
///   [`TerminalEndOfInput`](Self::TerminalEndOfInput) instead — one refusal, two carriers, one
///   answer.
/// * **`false`** — everything else, and each is affirmatively recoverable rather than merely
///   unclassified: they are built from a construct the **grammar** rejected, not from a limit the
///   **runtime** refused. [`EndOfInput`](Self::EndOfInput) is now one of them affirmatively: it is
///   the end of input tokora did *not* mark.
///
/// # The arm that was knowingly wrong, and what closed it — smear issue #177
///
/// [`EndOfInput`](Self::EndOfInput) used to answer `false` while being able to hold a terminal
/// stop. `From<UnexpectedEot>` landed *every* end of input on it — the genuine kind and the
/// scanner-stop kind alike — discarding
/// [`UnexpectedEnd::is_terminal`](tokora::error::UnexpectedEnd::is_terminal) before this trait
/// could read it, so the variant could not tell them apart and neither could the arm. What was
/// recorded here was that flipping the arm is the wrong repair — it would re-raise every ordinary
/// end of input and take recovery with it — and that the right one is to stop discarding the flag.
///
/// That is [`TerminalEndOfInput`](Self::TerminalEndOfInput). Both conversions route on the flag,
/// the two ends of input are two variants, and this impl reads the variant as it does everywhere
/// else.
///
/// **Keeping a mark is not the same as making one, and Codex's round 1 on that branch found the
/// two places smear owed the second.** A flag can only be kept where tokora set it, and tokora
/// sets it at the leaves it owns. Where *smear* decides — the `From<LexerErrors>` conversion a
/// rejecting emitter reaches, and every committed read written as `next()` or a raw `peek`, whose
/// tokora contract folds a trip into `None` — the mark had to be made rather than kept. Both are
/// repaired: the conversion inspects `LexerErrorData::State` and routes here, and the committed
/// reads are `next_or_stop` and `peek_head_map`, which raise the marked value the routing above
/// then keeps. No residual is recorded against this impl now.
impl<S, T, Char, Exp, StateError> MaybeTerminal for ErrorData<S, T, Char, Exp, StateError> {
  fn is_terminal(&self) -> bool {
    // Wildcard-free ON PURPOSE. A variant added without an arm is an `E0004` here, which is the
    // only thing that makes "every terminal source has an arm" a fact rather than a claim — and
    // the `false` arms are written out for the same reason the `true` ones are.
    match self {
      Self::Lexer(errors) => errors
        .iter()
        .any(|e| matches!(e.data(), LexerErrorData::State(_))),
      Self::NestingLimitExceeded => true,
      Self::TokenBudgetExhausted => true,
      Self::TerminalEndOfInput => true,
      Self::UnexpectedEndOfObjectFieldValue(e) => e.is_terminal(),
      Self::IntOverflow(_)
      | Self::FloatOverflow(_)
      | Self::Unclosed(_)
      | Self::UnexpectedToken(_)
      | Self::EndOfInput
      | Self::Lexical(_)
      | Self::Other(_) => false,
    }
  }
}

/// Delegated to the payload: the span says where, not whether.
impl<S, T, Char, Exp, StateError> MaybeTerminal for Error<S, T, Char, Exp, StateError> {
  fn is_terminal(&self) -> bool {
    self.data.is_terminal()
  }
}

/// `any`, because a container ends the parse if anything in it does.
///
/// A terminal stop among recoverable failures is still a stop, and the asymmetry the trait's own
/// rule records applies to the fold as much as to an arm: reading it as `all` would spend a real
/// stop the moment one ordinary diagnostic was recorded beside it.
impl<S, T, Char, Exp, StateError> MaybeTerminal for Errors<S, T, Char, Exp, StateError> {
  fn is_terminal(&self) -> bool {
    self.0.iter().any(MaybeTerminal::is_terminal)
  }
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

  /// Creates an unexpected end of input error — the **genuine** kind.
  #[inline]
  pub const fn unexpected_end_of_input(span: Span) -> Self {
    Self::new(span, ErrorData::EndOfInput)
  }

  /// Creates the end-of-input error that stands in for a **terminal scanner stop** — smear issue
  /// #177.
  ///
  /// [`unexpected_end_of_input`](Self::unexpected_end_of_input)'s twin at the same position and
  /// with the opposite [`MaybeTerminal`] answer. The producer
  /// [`ErrorData::TerminalEndOfInput`] is reached through, for the two `From<UnexpectedEot>`
  /// conversions and for the variant census, which may not name a variant directly.
  ///
  /// `span` is the one the ordinary end of input would have carried: tokora builds the marked
  /// value at the same offset as the plain one, because the stop *is* the leaf's end of input as
  /// far as position goes — what differs is whether more input could ever change the answer.
  #[inline]
  pub const fn terminal_end_of_input(span: Span) -> Self {
    Self::new(span, ErrorData::TerminalEndOfInput)
  }

  /// Creates a parser-frame nesting refusal.
  ///
  /// `span` is empty and sits at the parse's committed end: a refused frame has consumed nothing
  /// of its own, so there is no lexeme to point at.
  ///
  /// The producer [`ErrorData::NestingLimitExceeded`] is reached through — by
  /// `lossless_error_impls!`'s
  /// [`FromNestingLimit`](crate::lossless::depth::FromNestingLimit) impl, and by the variant
  /// census, which may not name a variant directly. It is generic over every parameter of this
  /// family, unlike that impl, which is pinned to the *lossless* keying.
  #[inline]
  pub const fn nesting_limit_exceeded(span: Span) -> Self {
    Self::new(span, ErrorData::NestingLimitExceeded)
  }

  /// Creates a durable token-budget refusal.
  ///
  /// `span` is empty and sits at the parse's committed end: tokora drops the refused item where
  /// it stands and publishes no span for it, so there is no lexeme to point at.
  ///
  /// The producer [`ErrorData::TokenBudgetExhausted`] is reached through — by
  /// `lossless_error_impls!`'s
  /// [`FromTokenBudget`](crate::lossless::depth::FromTokenBudget) impl, and by the variant
  /// census, which may not name a variant directly. It is generic over every parameter of this
  /// family, unlike that impl, which is pinned to the *lossless* keying.
  #[inline]
  pub const fn token_budget_exhausted(span: Span) -> Self {
    Self::new(span, ErrorData::TokenBudgetExhausted)
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

  /// Returns the syntax-error kind this error reports as, which is its data's.
  #[inline]
  pub fn kind(&self) -> SyntaxErrorKind {
    self.data.kind()
  }
}

impl<S, T, Char, Exp, StateError> ErrorData<S, T, Char, Exp, StateError> {
  /// Returns the syntax-error kind this error reports as.
  ///
  /// The one dispatch every [`Diagnose`] accessor on [`Error`] delegates to. A [`Lexer`](Self::Lexer)
  /// batch reports its first error's kind; an end of input reports
  /// [`UnexpectedEndOfInput`](SyntaxErrorKind::UnexpectedEndOfInput) when the document genuinely
  /// ended and [`ResourceLimit`](SyntaxErrorKind::ResourceLimit) when it stands in for a scanner
  /// stop, because that end is a limit tripping rather than the document running out.
  pub fn kind(&self) -> SyntaxErrorKind {
    match self {
      Self::Lexer(errors) => errors
        .iter()
        .next()
        .map_or(SyntaxErrorKind::InvalidSyntax, |error| error.kind()),
      Self::Lexical(kind) => *kind,
      Self::IntOverflow(_) => SyntaxErrorKind::IntOverflow,
      Self::FloatOverflow(_) => SyntaxErrorKind::FloatOverflow,
      Self::Unclosed(_) => SyntaxErrorKind::UnclosedDelimiter,
      Self::UnexpectedToken(token) => {
        if token.found().is_some() {
          SyntaxErrorKind::UnexpectedToken
        } else {
          SyntaxErrorKind::UnexpectedEndOfInput
        }
      }
      Self::UnexpectedEndOfObjectFieldValue(end) => {
        if end.is_terminal() {
          SyntaxErrorKind::ResourceLimit
        } else {
          SyntaxErrorKind::UnexpectedEndOfInput
        }
      }
      Self::EndOfInput => SyntaxErrorKind::UnexpectedEndOfInput,
      Self::TerminalEndOfInput => SyntaxErrorKind::ResourceLimit,
      Self::NestingLimitExceeded => SyntaxErrorKind::NestingLimit,
      Self::TokenBudgetExhausted => SyntaxErrorKind::TokenBudget,
      Self::Other(_) => SyntaxErrorKind::InvalidSyntax,
    }
  }
}

/// The one document a parse reads, as a [`Location`] numbers it.
const SOURCE: u32 = 0;

/// Writes `, expected {expected}` — or nothing, when the expectation renders as nothing.
///
/// [`Expectation::Unspecified`] renders empty rather than as a placeholder, and the clause is
/// dropped with it: "unexpected `}`" says what is known, where "unexpected `}`, expected a name"
/// would name a guess. Measured by rendering it once into a sink that only notes whether anything
/// arrived, so the check allocates nothing and asks nothing of `Exp` beyond `Display`.
fn expected_clause<Exp>(f: &mut core::fmt::Formatter<'_>, expected: &Exp) -> core::fmt::Result
where
  Exp: core::fmt::Display,
{
  struct Writes(bool);

  impl core::fmt::Write for Writes {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
      self.0 |= !s.is_empty();
      Ok(())
    }
  }

  let mut probe = Writes(false);
  let _ = core::fmt::write(&mut probe, format_args!("{expected}"));
  if probe.0 {
    write!(f, ", expected {expected}")
  } else {
    Ok(())
  }
}

/// A syntax error, as a sentence: lowercase, no trailing period, spellings in backticks, and never
/// a position — the position is the diagnostic's primary location, and a sentence that spelled it
/// as well would say it twice.
///
/// Nothing from the source is quoted, so there is no bound on `S` and no question of whether a
/// byte source's slice is text: an overflow names the width it was read at, not the literal.
impl<S, T, Char, Exp, StateError> core::fmt::Display for Error<S, T, Char, Exp, StateError>
where
  T: core::fmt::Display,
  Exp: core::fmt::Display,
{
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match &self.data {
      ErrorData::Lexer(errors) => match errors.iter().next() {
        Some(error) => core::fmt::Display::fmt(error, f),
        None => f.write_str(SyntaxErrorKind::InvalidSyntax.message()),
      },
      ErrorData::Lexical(kind) => f.write_str(kind.message()),
      ErrorData::IntOverflow(overflow) => write!(
        f,
        "integer out of range for a {}-bit integer",
        overflow.width().bits()
      ),
      ErrorData::FloatOverflow(_) => f.write_str("float out of range for a 64-bit float"),
      ErrorData::Unclosed(unclosed) => f.write_str(match unclosed {
        Unclosed::Parentheses => "unclosed `(`",
        Unclosed::List => "unclosed `[`",
        Unclosed::Object => "unclosed `{`",
      }),
      ErrorData::UnexpectedToken(token) => {
        match token.found() {
          Some(found) => write!(f, "unexpected {found}")?,
          None => f.write_str("unexpected end of input")?,
        }
        expected_clause(f, token.expected())
      }
      ErrorData::UnexpectedEndOfObjectFieldValue(end) => {
        if end.is_terminal() {
          f.write_str(SyntaxErrorKind::ResourceLimit.message())
        } else {
          write!(f, "unexpected end of input, expected a {}", end.hint())
        }
      }
      ErrorData::EndOfInput => f.write_str(SyntaxErrorKind::UnexpectedEndOfInput.message()),
      ErrorData::TerminalEndOfInput => f.write_str(SyntaxErrorKind::ResourceLimit.message()),
      ErrorData::NestingLimitExceeded => f.write_str(SyntaxErrorKind::NestingLimit.message()),
      ErrorData::TokenBudgetExhausted => f.write_str(SyntaxErrorKind::TokenBudget.message()),
      ErrorData::Other(text) => f.write_str(text),
    }
  }
}

impl<S, T, Char, Exp, StateError> core::error::Error for Error<S, T, Char, Exp, StateError>
where
  S: core::fmt::Debug,
  T: core::fmt::Debug + core::fmt::Display,
  Char: core::fmt::Debug,
  Exp: core::fmt::Debug + core::fmt::Display,
  StateError: core::fmt::Debug,
{
}

/// A draft §2 syntax error, read as structure: the code, severity and help of its
/// [`SyntaxErrorKind`], and the position it refused at in the one document a parse reads.
///
/// The position is the error's own span, with one exception: a [`Lexer`](ErrorData::Lexer) batch
/// points at its first error, because the lossless conversion that builds one carries the empty
/// span `0..0` outside it. An empty batch keeps the outer span. Never
/// [`Location::entire`]: a syntax error always has a place in the text.
///
/// No labels and no path segments: a syntax error points at one place, and it is raised before
/// any request has produced a result.
///
/// The bounds are what the message needs and nothing else — a found token and an expectation that
/// render — and they are spelled `core::fmt::Display` because `ci/source_census` copies them
/// verbatim into a generated crate that imports nothing. Not `::core::…`: the census re-spaces the
/// tokens it copies, and a leading `::` fuses with the bound's own `:` into a `:::` that does not
/// parse.
impl<S, T, Char, Exp, StateError> Diagnose for Error<S, T, Char, Exp, StateError>
where
  T: core::fmt::Display,
  Exp: core::fmt::Display,
{
  #[inline]
  fn code(&self) -> Code {
    self.kind().code()
  }

  #[inline]
  fn severity(&self) -> Severity {
    self.kind().severity()
  }

  #[inline]
  fn primary(&self) -> Location {
    let span = if let ErrorData::Lexer(errors) = &self.data {
      errors.iter().next().map_or(self.span, |error| error.span())
    } else {
      self.span
    };
    Location::new(SOURCE, span)
  }

  #[inline]
  fn primary_label(&self) -> Option<&'static str> {
    Some(self.kind().message())
  }

  #[inline]
  fn labels(&self) -> usize {
    0
  }

  #[inline]
  fn label(&self, _: usize) -> Option<Label> {
    None
  }

  #[inline]
  fn path_segments(&self) -> usize {
    0
  }

  #[inline]
  fn path_segment(&self, _: usize) -> Option<PathSegment<'_>> {
    None
  }

  #[inline]
  fn help(&self) -> Option<&'static str> {
    self.kind().help()
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
// GraphQL expectation, while absent or multi-kind expectations fall back to
// `Unspecified` because [`ErrorData::UnexpectedToken`] has one expectation slot — the
// fallback used to be `Name`, which named one member of a set as though it were the set.
// Diagnostics-carrying families (missing separator/element, full container, too
// few) map to `Other` exactly as the frozen glue did — these are emitter paths a
// `Vec` container and a fail-fast context rarely reach. A lexer batch keeps its first
// error's kind, as `Lexical`; see its conversion at the foot of the file.

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
    // turn "a new token kind falls back to `Unspecified`" into "a new token kind fails to
    // compile" — a real change to this function's contract, and this merge is a relocation.
    // Revisiting that choice is follow-up work, not part of the move.
    #[allow(unreachable_patterns)]
    _ => Expectation::Unspecified,
  }
}

#[inline]
fn expectation_from_tokora(expected: Option<Expected<'_, SyntacticTokenKind>>) -> Expectation {
  match expected {
    Some(Expected::One(kind)) => expectation_from_token_kind(kind),
    _ => Expectation::Unspecified,
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
//
// IT ROUTES ON THE FLAG — smear issue #177. `is_terminal()` is the one bit tokora attaches at the
// attempt/decline leaves and the delimited close, and the offset is identical either way, so this
// `if` is the whole of what keeps a scanner stop distinguishable from an end of input. Reading
// only `offset()` here is what made `MaybeTerminal`'s `EndOfInput` arm a `false` on a stop that
// can be real; the arm is now a decision about which variant this line chose.
impl<S, Lang: ?Sized, Set: Clone + 'static> From<UnexpectedEot<usize, Lang, Set>>
  for GraphqlErrors<S>
{
  #[inline]
  fn from(err: UnexpectedEot<usize, Lang, Set>) -> Self {
    let off = err.offset();
    let span = Span::new(off, off);
    if err.is_terminal() {
      GraphqlError::terminal_end_of_input(span).into()
    } else {
      GraphqlError::unexpected_end_of_input(span).into()
    }
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

// THE BATCH IS INSPECTED, NOT ERASED — smear issue #177, Codex round 1.
//
// This is the conversion tokora reaches when an emitter **rejects** a lexer diagnostic: the scan
// propagates the emitter's own error, built from the lexer error, and no `UnexpectedEnd` is
// constructed anywhere on that path — so nothing is marked and there is nothing for the
// end-of-input routing above to keep. Landing the whole batch on `Other`, whose `MaybeTerminal`
// arm is unconditionally `false`, therefore made a rejecting host's `Errors::is_terminal()` answer
// `false` on a tripped `smear_lexer::limits` budget.
//
// `LexerErrorData::State` is the scanner's own stop kind and the only variant that is: every other
// one is built from a lexeme the **grammar** rejected — a malformed literal, an unknown character,
// an unterminated `...`, invalid UTF-8 — or from a slice a whole-slice door refused, and each is
// recoverable. `State` is the tracker refusing, which no further input clears. It is the same
// variant the [`Lexer`](ErrorData::Lexer) arm keys on one layer up, so the two readings of a state
// refusal cannot drift apart.
//
// # Why `TerminalEndOfInput` and not a payload-carrying variant of its own
//
// A `State` refusal means the scanner has stopped and this is the end of what it will hand over.
// That is the event `TerminalEndOfInput` already names, and a consumer reading `is_terminal()`
// must not be able to tell *who built the value* — tokora at a committed leaf when the emitter
// accepted the diagnostic, or smear here when it rejected — because the fact about the document is
// the same.
//
// A variant carrying the batch is not available at this keying anyway: this impl is generic over
// the batch's `Char` and `StateError` while the container pins its own (`char`, `()`), so
// `ErrorData::Lexer` does not typecheck here. The lossless conversion `lossless_error_impls!`
// generates is pinned to `LexerErrors<char, LimitExceeded>` and therefore keeps the payload; that
// one already routed correctly and is unchanged.
//
// # What every other batch becomes: `Lexical`, keeping the first error's kind
//
// The payload cannot ride, and its kind can: `LexerErrorData::kind` is bound-free, so the first
// error's `SyntaxErrorKind` survives the erasure and an unterminated string, a bad escape or a
// leading zero reaches the caller under its own `smear::syntax` code. It used to be one
// `Other("lexer error")` for every one of them. An **empty** batch has no first error and keeps
// that note, at `0..0` — the one case where there is no kind to keep and no position to point at.
//
// The span is the batch's first, on **both** arms. `Span::new(0, 0)` pointed every lexer error at
// byte zero, which is a position the error is not at; a `State` error minted through
// `From<StateError> for LexerErrors` genuinely carries `0..0`, and that is the value's own
// statement rather than this conversion's.
impl<S, Char, StateError> From<LexerErrors<Char, StateError>> for GraphqlErrors<S> {
  #[inline]
  fn from(err: LexerErrors<Char, StateError>) -> Self {
    let Some(first) = err.iter().next() else {
      return GraphqlError::new(
        Span::new(0, 0),
        ErrorData::Other(std::borrow::Cow::Borrowed("lexer error")),
      )
      .into();
    };
    let span = first.span();
    if err
      .iter()
      .any(|e| matches!(e.data(), LexerErrorData::State(_)))
    {
      return GraphqlError::terminal_end_of_input(span).into();
    }
    GraphqlError::new(span, ErrorData::Lexical(first.kind())).into()
  }
}

#[cfg(test)]
mod tests;
