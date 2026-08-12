//! What the introspection door refuses, and how it says so.
//!
//! # Two vocabularies, because there are two failures
//!
//! Reading an introspection response and validating the schema it describes are different jobs
//! with different audiences, so [`IntrospectionError`] keeps them apart:
//!
//! - [`IntrospectionError::Response`] is *this response is not a draft §4 introspection result* —
//!   not JSON, a required field missing, a `__TypeKind` nobody defines. The audience is whoever
//!   wired up the transport, and there is exactly one: reading stops at the first thing that is
//!   not a §4 result, because everything after it is a guess about what the server meant.
//! - [`IntrospectionError::Schema`] is *it is a result, and the schema it describes is not a
//!   schema* — draft §3 refused it. This is [`SchemaErrors`] unchanged, every reason at once,
//!   because it is literally the value
//!   [`Schema::build`](crate::Schema::build) returned: the door renders SDL and
//!   hands it to the same builder, so a bad schema is refused by the same code with the same kinds
//!   in the same order as the SDL that describes it.
//!
//! That asymmetry — fail-fast on the shape, collect on the schema — is deliberate. An
//! introspection response is machine-generated, so a malformed one has one bug in it and listing
//! twenty consequences of that bug helps nobody. A schema is authored, and its author wants the
//! whole list.

use std::{boxed::Box, format, string::String};

use super::{
  super::error::SchemaErrors,
  decode::Owner,
  json::{Mark, line_column},
};
use crate::diagnostic::{Code, Diagnose, Label, Location, PathSegment, Severity};

/// Which rule of the draft §4 shape a [`ResponseError`] reports.
///
/// The set is closed by what a *translation* can go wrong at, not by the specification: everything
/// the specification says about a schema is checked one layer down, by the same draft §3 pass the
/// SDL door runs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum ResponseErrorKind {
  /// The bytes are not JSON, or nest deeper than the reader will go.
  ///
  /// The nesting bound is 128 containers — the limit the previous reader enforced, kept because a
  /// `__Type` reference chain is walked recursively and this is what keeps a hostile response from
  /// driving that walk off the stack.
  MalformedJson,
  /// The bytes are JSON, but not the shape draft §4 describes — a required field absent or null,
  /// or a value of the wrong JSON type.
  MalformedResponse,
  /// The response carries no `__schema`, under `data` or at its root.
  MissingSchema,
  /// A `__Type.kind` is not one of the eight `__TypeKind` values.
  ///
  /// Reported where the literal is read, not where a rendered kind is compared, so the subject is
  /// the spelling the response actually wrote.
  UnknownTypeKind,
  /// A named type was required and a `LIST` or `NON_NULL` wrapper was given.
  ///
  /// Wrapper kinds name no type: they are the shape of a *reference*, so one cannot be a member of
  /// `__Schema.types` nor the base a reference bottoms out at.
  NotANamedType,
  /// A `LIST` or `NON_NULL` type reference has no `ofType`.
  MissingItemType,
  /// A `NON_NULL` type reference directly wraps another `NON_NULL`.
  ///
  /// `NonNullType` wraps a `NamedType` or a `ListType` and never another `NonNullType`, so
  /// `Int!!` is a type no grammar can spell and no schema can hold.
  DoubleNonNull,
  /// A named type, or the base of a type reference, has a null `name`.
  UnnamedType,
  /// A name is not spelled `/[_A-Za-z][_0-9A-Za-z]*/`.
  InvalidName,
  /// An enum value is spelled `true`, `false` or `null`.
  ///
  /// The grammar's `EnumValue` excludes all three, so such a value cannot be written in SDL and
  /// the schema the response describes is one no document can express.
  InvalidEnumValue,
  /// A `__Directive.locations` entry is not one of the 19 `__DirectiveLocation` values.
  UnknownDirectiveLocation,
  /// An `__InputValue.defaultValue` is not one complete GraphQL const value.
  ///
  /// Refusing here rather than at the SDL parse is what makes the field safe to embed: a value
  /// that parses whole cannot close the definition it sits in and open another.
  MalformedDefaultValue,
  /// The door rendered SDL its own parser refused.
  ///
  /// **Unreachable by construction, and an error rather than a panic because "unreachable" is a
  /// claim.** Every free-form byte the door emits is checked before it is written — names against
  /// the grammar, kinds and locations against closed sets, default values against the const-value
  /// parser — so nothing a response can carry reaches the parser unexamined. If this fires, the
  /// renderer has a bug and the response is innocent.
  UnparsableSdl,
}

impl ResponseErrorKind {
  /// Every kind the door can report.
  ///
  /// The fixtures iterate this rather than a hand-kept list, so a kind added without a response
  /// that makes it fire fails a test instead of shipping unexercised.
  pub const ALL: &'static [Self] = &[
    Self::MalformedJson,
    Self::MalformedResponse,
    Self::MissingSchema,
    Self::UnknownTypeKind,
    Self::NotANamedType,
    Self::MissingItemType,
    Self::DoubleNonNull,
    Self::UnnamedType,
    Self::InvalidName,
    Self::InvalidEnumValue,
    Self::UnknownDirectiveLocation,
    Self::MalformedDefaultValue,
    Self::UnparsableSdl,
  ];

  /// Returns the phrase this kind renders as, with no subject attached.
  pub const fn message(&self) -> &'static str {
    match self {
      Self::MalformedJson => "the response is not JSON",
      Self::MalformedResponse => "the response is not a draft §4 introspection result",
      Self::MissingSchema => "the response carries no `__schema`",
      Self::UnknownTypeKind => "unknown `__TypeKind`",
      Self::NotANamedType => "a wrapper kind where a named type is required",
      Self::MissingItemType => "a wrapper type reference has no `ofType`",
      Self::DoubleNonNull => "a `NON_NULL` reference wraps another `NON_NULL`",
      Self::UnnamedType => "a named type with no name",
      Self::InvalidName => "not a GraphQL name",
      Self::InvalidEnumValue => "not a GraphQL enum value",
      Self::UnknownDirectiveLocation => "unknown `__DirectiveLocation`",
      Self::MalformedDefaultValue => "default value is not a GraphQL const value",
      Self::UnparsableSdl => "the rendered SDL does not parse",
    }
  }

  /// Returns the stable identifier for this kind.
  ///
  /// Third family, one namespace: a consumer that renders a schema refusal beside a door failure
  /// compares `smear::introspection::…` with `smear::schema::…` without knowing which enum either
  /// came from.
  #[inline]
  pub const fn code(&self) -> Code {
    match self {
      Self::MalformedJson => Code::new("smear::introspection::malformed-json"),
      Self::MalformedResponse => Code::new("smear::introspection::malformed-response"),
      Self::MissingSchema => Code::new("smear::introspection::missing-schema"),
      Self::UnknownTypeKind => Code::new("smear::introspection::unknown-type-kind"),
      Self::NotANamedType => Code::new("smear::introspection::not-a-named-type"),
      Self::MissingItemType => Code::new("smear::introspection::missing-item-type"),
      Self::DoubleNonNull => Code::new("smear::introspection::double-non-null"),
      Self::UnnamedType => Code::new("smear::introspection::unnamed-type"),
      Self::InvalidName => Code::new("smear::introspection::invalid-name"),
      Self::InvalidEnumValue => Code::new("smear::introspection::invalid-enum-value"),
      Self::UnknownDirectiveLocation => {
        Code::new("smear::introspection::unknown-directive-location")
      }
      Self::MalformedDefaultValue => Code::new("smear::introspection::malformed-default-value"),
      Self::UnparsableSdl => Code::new("smear::introspection::unparsable-sdl"),
    }
  }
}

impl core::fmt::Display for ResponseErrorKind {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.write_str(self.message())
  }
}

/// One reason a response is not a draft §4 introspection result.
///
/// It names what it refused and, where the artifact has one, what owns it — so `User.pet` reads as
/// the field and `User.pet.first` as its argument, exactly as [`SchemaError`] does. There is no
/// span: the response has no SDL positions to point at, and a JSON offset would point into a blob
/// no human wrote. The owner path is the locator.
///
/// [`SchemaError`]: crate::SchemaError
#[derive(Clone, PartialEq, Eq)]
pub struct ResponseError {
  kind: ResponseErrorKind,
  subject: Box<str>,
  owner: Option<Box<str>>,
  /// The derivations this refusal staged instead of performing, resolved by
  /// [`read`](super::decode::read) on the one refusal the door returns.
  ///
  /// Always `Pending::default()` on any error that has left the door.
  pending: Pending,
  /// Where the reader may carry on, when this refusal names a value it consumed whole.
  ///
  /// A refusal held until its object's closing brace is a refusal *about a value already walked to
  /// its end*, so the member that receives it can put the cursor here rather than rewind and walk
  /// the value a second time — which, without the mark, every enclosing member does in turn.
  ///
  /// Always `None` on any error that has left the door.
  resume: Option<Mark>,
}

/// What a refusal has staged rather than derived, because deriving it walks the response.
///
/// # One invariant, which is why this is a type and not two fields
///
/// **Constructing a refusal does work bounded by the value it names; anything proportional to bytes
/// the refusal did not itself read happens once, on the refusal that escapes.**
///
/// The reader builds refusals it may then discard — a member written twice replaces the first
/// occurrence's failure along with its value, as `decode`'s `Slot` describes — so a derivation
/// performed at construction is performed once per *occurrence*. A response repeating one bad
/// member k times then pays k times whatever a construction costs, for an answer it throws k−1
/// copies of away, and if that cost is proportional to the response the product is quadratic in
/// what a network peer sends.
///
/// Two derivations cost exactly that much, and they were found one review round apart:
///
/// - the **owner**, which is a path and is read back by walking the object that owns it, and
/// - the **position**, which is a line and a column and is counted by walking every byte before the
///   cursor.
///
/// They are one staged field rather than two repairs so that a third expensive derivation added
/// later inherits the invariant instead of re-learning it: being here is what makes a thing staged,
/// and `ResponseError::resolve` is the one place any of it is paid for.
///
/// # What is *not* staged, and why it does not have to be
///
/// A refusal's subject is a copy of the literal it refuses, which is not constant work — but it is
/// bounded by bytes the reader had already read in order to refuse them, so the subjects of every
/// occurrence together are bounded by the response. That is linear and not a product, which is the
/// property that matters; `smear-schema/tests/introspection_allocation.rs` is what measures it.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
struct Pending {
  /// The artifact that owns the subject, as the offsets its name will be read back from.
  owner: Owner,
  /// The offset the refusal was raised at, whose line and column the subject ends with.
  at: Option<usize>,
}

/// The three fields a `ResponseError` reports, and not the two the reader staged them from.
///
/// `pending` and `resume` are implementation details of the reader's error path, and both are their
/// default on every error a caller can hold, so printing them would be printing a constant.
impl core::fmt::Debug for ResponseError {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("ResponseError")
      .field("kind", &self.kind)
      .field("subject", &self.subject)
      .field("owner", &self.owner)
      .finish()
  }
}

impl ResponseError {
  /// Assembles an error naming an unowned artifact.
  pub(super) fn new(kind: ResponseErrorKind, subject: impl Into<String>) -> Self {
    Self {
      kind,
      subject: subject.into().into_boxed_str(),
      owner: None,
      pending: Pending::default(),
      resume: None,
    }
  }

  /// Qualifies the subject with the artifact that owns it.
  pub(super) fn owned_by(mut self, owner: impl Into<String>) -> Self {
    self.owner = Some(owner.into().into_boxed_str());
    self
  }

  /// Records the artifact that owns the subject, as the offsets its name will be read back from.
  pub(super) fn owned_at(mut self, owner: Owner) -> Self {
    self.pending.owner = owner;
    self
  }

  /// Records where in the response the refusal was raised, as the offset its line and column will
  /// be counted from.
  pub(super) const fn raised_at(mut self, at: usize) -> Self {
    self.pending.at = Some(at);
    self
  }

  /// Records that the value this refusal is about was consumed whole, and where it ends.
  pub(super) const fn resume_at(mut self, mark: Mark) -> Self {
    self.resume = Some(mark);
    self
  }

  /// Returns where the reader may carry on, when the refused value was consumed whole.
  pub(super) const fn resume(&self) -> Option<Mark> {
    self.resume
  }

  /// Performs every derivation the refusal staged, which is the last thing the door does.
  ///
  /// One walk of the response per *response*, however many refusals were built and discarded on the
  /// way — which is the whole of `Pending`'s invariant, discharged in the one place.
  pub(super) fn resolve(mut self, response: &str) -> Self {
    // A resume mark describes a cursor inside a reading that has now finished, so it is not a fact
    // about the error a caller holds. Cleared here so that two refusals a caller cannot tell apart
    // do not compare unequal over it.
    self.resume = None;
    let Pending { owner, at } = core::mem::take(&mut self.pending);
    if let Some(at) = at {
      let (line, column) = line_column(response, at);
      self.subject = format!("{} at line {line} column {column}", self.subject).into_boxed_str();
    }
    if !matches!(owner, Owner::Unowned) {
      self.owner = owner.resolve(response).map(String::into_boxed_str);
    }
    self
  }

  /// Returns which rule refused.
  #[inline]
  pub const fn kind(&self) -> ResponseErrorKind {
    self.kind
  }

  /// Returns the rejected artifact, unqualified.
  ///
  /// Usually the literal the response wrote — a name, a `__TypeKind`, a default value. For the two
  /// kinds that refuse the document rather than something in it,
  /// [`MalformedJson`](ResponseErrorKind::MalformedJson) and
  /// [`MalformedResponse`](ResponseErrorKind::MalformedResponse), it is the reader's own message,
  /// which carries the member's name and the line and column it stopped at.
  #[inline]
  pub fn subject(&self) -> &str {
    &self.subject
  }

  /// Returns the artifact that owns the subject, when it has one.
  #[inline]
  pub fn owner(&self) -> Option<&str> {
    self.owner.as_deref()
  }
}

/// A door failure, read as structure rather than as a sentence.
///
/// # The one implementor of [`Location::entire`]
///
/// Everything else in this crate points at bytes somebody typed. This does not, and the type's own
/// documentation says why: the input is a machine-generated JSON blob, and a byte offset into it
/// would name a position in something no human wrote and nobody will edit. A synthetic `0..len`
/// would render as a span, sort as a span and mislead as a span. [`Location::entire`] says the
/// diagnostic is about the response as a whole, which is the truth.
///
/// The consequence is a real loss, not a technicality — an editor gets no underline out of this
/// family — and it is the price of not fabricating one. The locator a reader actually uses is the
/// owner path in the message.
///
/// # No labels, and no path
///
/// Nothing to relate a second position to, since there is no first one. And no path: the dotted
/// owner path this error renders — `User.pet.first` — is a coordinate in the *schema the response
/// describes*, not in a response tree, so publishing it through
/// [`path_segment`](Diagnose::path_segment) would make a §7.1.2 writer emit `errors[].path` for an
/// error that has no field of a result to point at.
///
/// # Severity and help are answered here rather than per kind
///
/// All thirteen kinds are refusals, and all thirteen have the same instruction for the same
/// audience: whoever wired up the transport, whose server did not return a draft §4 result. A
/// per-kind table would be thirteen ways of writing that sentence.
impl Diagnose for ResponseError {
  #[inline]
  fn code(&self) -> Code {
    self.kind.code()
  }

  #[inline]
  fn severity(&self) -> Severity {
    Severity::Error
  }

  #[inline]
  fn primary(&self) -> Location {
    Location::entire(SOURCE)
  }

  #[inline]
  fn primary_label(&self) -> Option<&'static str> {
    Some(self.kind.message())
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
    None
  }
}

/// The one response body the door reads.
///
/// [`Schema::from_introspection`](crate::Schema::from_introspection) takes a
/// single `&str`, so there is no second input for [`Location::source`] to distinguish — only the
/// question of whether there is a position within it, which there is not.
const SOURCE: u32 = 0;

impl core::fmt::Display for ResponseError {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "{}: `", self.kind)?;
    if let Some(owner) = &self.owner {
      write!(f, "{owner}.")?;
    }
    write!(f, "{}`", self.subject)
  }
}

impl core::error::Error for ResponseError {}

/// Why [`Schema::from_introspection`] refused.
///
/// [`Schema::from_introspection`]: crate::Schema::from_introspection
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum IntrospectionError {
  /// The response is not a draft §4 introspection result.
  Response(ResponseError),
  /// It is one, and draft §3 refused the schema it describes.
  ///
  /// These are [`Schema::build`](crate::Schema::build)'s own refusals, not a
  /// re-implementation of them: the door renders SDL and hands it to that builder, so an
  /// introspection response describing a schema with an undefined type is refused by the same rule
  /// and with the same [`SchemaErrorKind`] as the SDL would be.
  ///
  /// [`SchemaErrorKind`]: crate::SchemaErrorKind
  Schema(SchemaErrors),
}

impl IntrospectionError {
  /// Returns which shape rule refused, or `None` when draft §3 did.
  #[inline]
  pub const fn response_kind(&self) -> Option<ResponseErrorKind> {
    match self {
      Self::Response(error) => Some(error.kind()),
      Self::Schema(_) => None,
    }
  }

  /// Returns the draft §3 refusals, or `None` when the response never got that far.
  #[inline]
  pub const fn schema_errors(&self) -> Option<&SchemaErrors> {
    match self {
      Self::Response(_) => None,
      Self::Schema(errors) => Some(errors),
    }
  }
}

impl From<ResponseError> for IntrospectionError {
  #[inline]
  fn from(error: ResponseError) -> Self {
    Self::Response(error)
  }
}

impl From<SchemaErrors> for IntrospectionError {
  #[inline]
  fn from(errors: SchemaErrors) -> Self {
    Self::Schema(errors)
  }
}

impl core::fmt::Display for IntrospectionError {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Response(error) => write!(f, "{error}"),
      Self::Schema(errors) => write!(f, "{errors}"),
    }
  }
}

impl core::error::Error for IntrospectionError {
  fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
    match self {
      Self::Response(error) => Some(error),
      Self::Schema(errors) => Some(errors),
    }
  }
}
