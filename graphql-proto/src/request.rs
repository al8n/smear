//! The obligation `poll_resolve` hands to the driver, and the argument view it carries.
//!
//! # Why the arguments are a decision and not a map
//!
//! Draft §6.4.1 `CoerceArgumentValues` is two things wearing one name. Its control flow — is there
//! a value at all, does it come from a literal or a variable, was the variable supplied, is a
//! missing value fatal because the type is non-null, does the schema's default apply — is
//! conformance, and getting it wrong is how a service accepts a query the specification rejects.
//! Its *result* is a map of coerced values in the service's own representation, and `proto` has
//! none.
//!
//! So the split follows the same line as everything else here: `proto` runs the control flow and
//! raises the three field errors §6.4.1 specifies, and the arguments that survive arrive as an
//! [`ArgumentSource`] — a statement of *which* of the sources supplies this argument, with the
//! literal the document already holds. The driver reads it; the document is not copied and no
//! value is built.
//!
//! A bare variable is the one source that carries a value rather than a reference into the
//! document, because there is no document text to point at: the value is the driver's own, read
//! once by step 5.d and moved through. That is not a copy the split introduced — it replaces the
//! second [`Values::variable`](super::Values::variable) call a name-only source would have forced,
//! and it is what makes the value the driver resolves with the value §6.4.1 checked.
//!
//! # Where that line falls, exactly
//!
//! The control flow `proto` runs is draft §6.4.1 step 5 over each argument *as a whole*: whether a
//! value reached it, whether that value is the request's or the schema's default, and whether a
//! non-null argument was handed `null` or a variable the request never supplied. Those are the
//! three field errors above, and they are raised before the field is ever offered.
//!
//! Step 5.j — "coerce {value} according to the input coercion rules of {argumentType}" — is not,
//! and cannot be: its product is a coerced value in the service's representation, which is the one
//! thing `proto` has no way to build. So the *contents* of a literal are the driver's, and with
//! them draft §3.10's and §3.11's rules about those contents. The reachable consequence is a
//! variable nested inside an input-object or list literal: `f(filter: {flag: $flag})` where
//! `flag` is `Boolean!` is a valid document even when `$flag` is nullable with a non-null default
//! (draft 5.8.5), and a request that supplies `null` for it puts a null at a non-null input field
//! that `proto` does not see. A driver walking the literal to build its value is where that is
//! caught, and [`handle_field_error`](super::Executor::handle_field_error) with
//! [`FieldRequest::path`] is how it is reported.
//!
//! [`ArgumentSource::SchemaDefault`] carries a known limit of the schema representation.
//! [`DefaultKind`](smear_schema::DefaultKind) reduces a declared default to two bits —
//! present, and null — because that is all the validator ever needed. So `proto` can say *that*
//! draft §6.4.1 step 5.h.i applies and cannot say what the default's content is, and the driver,
//! which owns the SDL, supplies it. Teaching the schema to retain default literals would let
//! `proto` supply the content directly; the variant is where that would land, and a driver's
//! `match` would not have to change.

use tokora::SimpleSpan;

use smear_parser::graphql::ast::{Field, InputValue, SelectionSet};
use smear_schema::{PackedType, Schema, Sym, TypeId};

use super::response::{Path, Slot};

/// Identifies an outstanding field request.
///
/// Tagged twice, because a stale id can be stale in two different ways and only one of them is
/// visible inside an operation.
///
/// The **generation** distinguishes this request from an earlier one that held the same slab
/// index: an id already answered, or one [`poll_abandoned`](super::Executor::poll_abandoned) has
/// retired, is recognised and ignored. A driver that answers a cancelled request late is a normal
/// occurrence, not a bug, and it must not be able to write into whatever request took its slot.
///
/// The **epoch** distinguishes one execution from the next. [`Executor::start`](super::Executor::start)
/// empties the slab, so without it the next execution would re-issue index 0 generation 0 and a
/// late answer from the one before could match a live slot by index and generation and land
/// on someone else's field.
///
/// One *execution* and not one operation, which draft §6.2.3 made a distinction: every
/// [`handle_source_event`](super::Executor::handle_source_event) empties the slab too, so an id
/// from the previous event is void the moment the next one is taken, and so is one still
/// outstanding when [`unsubscribe`](super::Executor::unsubscribe) or either source-stream
/// completion ends the subscription. A driver that answers a request belonging to an event that has
/// already produced its execution result is ignored, exactly as one answering a previous operation
/// is — which matters more here than there, because a stream gives it many more chances. Abandoned requests make that reachable without any driver misbehaviour
/// at all: [`poll_response`](super::Executor::poll_response) delivers while they are still
/// outstanding — deliberately, since they cannot affect the response — so a driver can be holding
/// them across the restart. The epoch is what makes every id from a previous operation
/// unconditionally rejectable, rather than making the driver's cancellation bookkeeping
/// load-bearing for correctness.
///
/// It is 64-bit so that the rejection is unconditional rather than nearly so: a 32-bit counter is
/// exhausted by 2^32 operations on one long-lived executor, and this is a `Copy` handle rather
/// than something stored per response node, so the four extra bytes buy the property outright.
///
/// The epoch orders operations on **one** executor. Two executors both begin at the same epoch,
/// and mixing ids between them is the same category of error as mixing indices between two slabs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReqId {
  pub(super) epoch: u64,
  pub(super) index: u32,
  pub(super) generation: u32,
}

impl ReqId {
  /// Returns which [`start`](super::Executor::start) this id belongs to.
  #[inline]
  pub const fn epoch(&self) -> u64 {
    self.epoch
  }

  /// Returns the raw slab index.
  #[inline]
  pub const fn index(&self) -> u32 {
    self.index
  }

  /// Returns the generation that distinguishes this request from an earlier one at the same index.
  #[inline]
  pub const fn generation(&self) -> u32 {
    self.generation
  }
}

/// Where an argument's value comes from, after draft §6.4.1 has decided.
#[derive(Debug)]
#[non_exhaustive]
pub enum ArgumentSource<'a, S, V> {
  /// The document wrote a literal. It may still be the `null` literal — for a nullable argument
  /// that is an explicit null, which §6.4.1 step 5.f keeps distinct from an absent one.
  ///
  /// It may also be an input object or a list holding variables, which reach the driver
  /// unsubstituted. Coercing the contents is step 5.j's and therefore the driver's; see this
  /// module's header for where that line falls and what it costs.
  Literal(&'a InputValue<S>),
  /// The document wrote `$name` and the request supplied a value for it: this value.
  ///
  /// # Why the value and not just the name
  ///
  /// Because draft §6.4.1 steps 5.d through 5.i.i already read it, to decide whether the argument
  /// has a value and whether that value is `null` at a non-null type. Handing the driver the name
  /// and letting it call [`Values::variable`](super::Values::variable) again would put a *second*
  /// read between the check and the use, and the second read is the one that reaches a typed
  /// argument position: a table that changed in between — a driver that consumes on read, an
  /// interner that recycles, a lazily populated map — delivers a value those steps never saw, so a
  /// non-null argument could be checked against a value and resolved against nothing with no error
  /// anywhere. Carrying it makes the checked value and the delivered value the same value by
  /// construction, and it is also one driver call fewer.
  ///
  /// `name` is still here because draft §6.4.1's own errors name the variable, and because a
  /// driver's tracing wants it.
  ///
  /// # How long the executor owns it
  ///
  /// Only while this request is the one being offered. The driver's next call in releases it —
  /// whether that call answers the request, reports a field error for it, retires it, takes the
  /// response or starts another operation — so a value that owns something, a wasm or FFI handle
  /// or a pooled buffer, is handed back inside the operation rather than at the end of it. A
  /// driver that needs it for longer takes its own copy while the request is readable, which the
  /// borrow already requires of anything read out of a [`FieldRequest`].
  Variable {
    /// The variable's name, without the `$`.
    name: &'a str,
    /// Its value, exactly as [`Values::variable`](super::Values::variable) returned it.
    value: V,
  },
  /// No value reached the argument and the schema declares a default, so draft §6.4.1 step 5.b
  /// or 5.d says the default applies.
  SchemaDefault,
}

/// One argument of a field request.
#[derive(Debug)]
pub struct Argument<'a, S, V> {
  pub(super) name: &'a str,
  pub(super) ty: PackedType,
  pub(super) source: ArgumentSource<'a, S, V>,
}

impl<'a, S, V> Argument<'a, S, V> {
  /// Returns the argument's name, as the schema declares it.
  #[inline]
  pub const fn name(&self) -> &'a str {
    self.name
  }

  /// Returns the argument's declared type.
  #[inline]
  pub const fn ty(&self) -> PackedType {
    self.ty
  }

  /// Returns where the value comes from.
  #[inline]
  pub const fn source(&self) -> &ArgumentSource<'a, S, V> {
    &self.source
  }
}

/// A field the executor needs a value for.
///
/// Handed out by [`poll_resolve`](super::Executor::poll_resolve) and answered with
/// [`handle_resolved`](super::Executor::handle_resolved) or
/// [`handle_field_error`](super::Executor::handle_field_error). Borrowed from the executor, so it
/// must be consumed — or its [`id`](FieldRequest::id) kept — before the next call in.
pub struct FieldRequest<'r, 'a, S, V> {
  pub(super) id: ReqId,
  pub(super) schema: &'r Schema,
  pub(super) slots: &'r [Slot<V>],
  pub(super) names: &'r [u8],
  pub(super) name_spans: &'r [(u32, u32)],
  pub(super) slot: u32,
  pub(super) field: &'a Field<S>,
  pub(super) merged: &'r [&'a Field<S>],
  pub(super) field_sym: Sym,
  pub(super) parent_type: TypeId,
  pub(super) parent_value: &'r V,
  pub(super) field_type: PackedType,
  pub(super) arguments: &'r [Argument<'a, S, V>],
}

impl<'r, 'a, S, V> FieldRequest<'r, 'a, S, V> {
  /// Returns the id to answer this request with.
  #[inline]
  pub const fn id(&self) -> ReqId {
    self.id
  }

  /// Returns the field's name as the schema declares it.
  ///
  /// Not the response key: an aliased field reports the schema's name here and the alias in
  /// [`path`](FieldRequest::path), which is the distinction draft §6.4 draws between the field a
  /// driver resolves and the key the answer lands under.
  #[inline]
  pub fn name(&self) -> &'r str {
    self.schema.name(self.field_sym)
  }

  /// Returns the object type the field was selected on.
  ///
  /// It is also the answer to draft §4.4's `__typename`, which is why no request is ever handed out
  /// for that field: the executor already holds this name and has already spent it deciding which
  /// fragments applied. See [`Values`](super::Values) for the other two meta-fields.
  #[inline]
  pub fn parent_type(&self) -> &'r str {
    self
      .schema
      .name(self.schema.type_def(self.parent_type).name())
  }

  /// Returns the field's declared type.
  #[inline]
  pub const fn field_type(&self) -> PackedType {
    self.field_type
  }

  /// Returns the value of the object this field is selected on — draft §6.4's `objectValue`.
  #[inline]
  pub const fn parent_value(&self) -> &'r V {
    self.parent_value
  }

  /// Returns the draft §7.1.2 response path this field will occupy.
  ///
  /// Available *before* the value is resolved, which is what lets a driver attach the path to its
  /// own tracing and to any error it reports back.
  #[inline]
  pub fn path(&self) -> Path<'r, V> {
    Path {
      slots: self.slots,
      names: self.names,
      name_spans: self.name_spans,
      slot: self.slot,
    }
  }

  /// Returns the span of the field draft §6.4 takes the name and arguments from.
  ///
  /// One span, and the *first* of possibly several: a response key that merged a group is as many
  /// places in the document as [`merged`](FieldRequest::merged) has entries, and that is where a
  /// driver attaching source positions to its own tracing should read them from. An error the
  /// executor raises about this field already carries all of them —
  /// [`Error::locations`](super::Error::locations).
  #[inline]
  pub fn location(&self) -> SimpleSpan {
    *self.field.span()
  }

  /// Returns the arguments that survived draft §6.4.1.
  ///
  /// Every argument §6.4.1 rejects *as a whole* — absent and required, explicitly `null` at a
  /// non-null type, or naming a variable the request did not supply — has already become a field
  /// error, so nothing here is an argument the driver should refuse to resolve with. For a bare
  /// variable that guarantee is about the value in
  /// [`ArgumentSource::Variable`](ArgumentSource::Variable) itself, not about whatever a fresh
  /// lookup of the same name would return.
  ///
  /// It is not a guarantee about a literal's contents. Step 5.j's input coercion is the driver's,
  /// for the reason this module's header gives, and a variable nested inside an input-object or
  /// list literal is checked there rather than here.
  #[inline]
  pub const fn arguments(&self) -> &'r [Argument<'a, S, V>] {
    self.arguments
  }

  /// Returns the field's own selection set, if it has one.
  ///
  /// Draft §6.4's `MergeSelectionSets` is not applied here: the sub-selections of *every* field
  /// that merged into this response key are what execution descends into, and that is
  /// [`merged`](FieldRequest::merged). This is the first one, which is the field §6.4 takes the
  /// name and arguments from.
  #[inline]
  pub fn selection_set(&self) -> Option<&'a SelectionSet<S>> {
    self.field.selection_set()
  }

  /// Returns every field that merged into this response key, in document order.
  ///
  /// Always at least one. More than one when draft §6.3 grouped several selections under the same
  /// response key — `{ a { x } a { y } }` — in which case §6.4's `MergeSelectionSets` is the
  /// concatenation of their selection sets, which the executor performs when the value comes back.
  #[inline]
  pub const fn merged(&self) -> &'r [&'a Field<S>] {
    self.merged
  }
}

impl<S, V> core::fmt::Debug for FieldRequest<'_, '_, S, V> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("FieldRequest")
      .field("id", &self.id)
      .field("parent_type", &self.parent_type())
      .field("path", &self.path())
      .finish()
  }
}

/// The one root field draft §6.2.3.1 `ResolveFieldEventStream` is to be called with.
///
/// Handed out by [`Executor::source_field`](super::Executor::source_field) while the response
/// stream is [`Creating`](super::ResponseStream::Creating), and answered by calling the driver's
/// own subscription resolver and then
/// [`handle_source_stream`](super::Executor::handle_source_stream). Borrowed from the executor, so
/// it must be consumed before the next call in.
///
/// # Why this is not a [`FieldRequest`]
///
/// Because its answer is not a value. `ResolveFieldEventStream(subscriptionType, rootValue,
/// fieldName, argumentValues)` returns an *event stream*, and every way of answering a
/// [`FieldRequest`] — [`handle_resolved`](super::Executor::handle_resolved) and
/// [`handle_field_error`](super::Executor::handle_field_error) — would run draft §6.4.3
/// `CompleteValue` on it. Sharing the type would mean a driver could answer this obligation with
/// the wrong call and get a plausible, silently wrong response rather than a compile error, which
/// is the reason this crate has one channel per obligation instead of one `Step` enum.
///
/// Two members of [`FieldRequest`] are therefore deliberately absent. There is no
/// [`ReqId`], because nothing is outstanding: §6.2.3.1 runs no execution, spends no
/// [`max_in_flight`](super::Limits::max_in_flight) slot, and has no request to retire. And there is
/// no `path`, because there is no response to have a position in — the first response a
/// subscription produces belongs to its first *event*, and this field's value never appears in one
/// at all.
///
/// # The name is the schema's, which §6.2.3.1 says twice
///
/// Step 7 is "Let {fieldName} be the name of the first entry in {fields}", with the note "This
/// value is unaffected if an alias is used" — so [`name`](SourceField::name) is the field's
/// declared name and an alias is nowhere in this type. That matches [`FieldRequest::name`], and for
/// the same reason: the alias is a response key, and a resolver is not being asked about a
/// response.
pub struct SourceField<'r, 'a, S, V> {
  pub(super) schema: &'r Schema,
  pub(super) field: &'a Field<S>,
  pub(super) merged: &'r [&'a Field<S>],
  pub(super) field_sym: Sym,
  pub(super) parent_type: TypeId,
  pub(super) parent_value: &'r V,
  pub(super) field_type: PackedType,
  pub(super) arguments: &'r [Argument<'a, S, V>],
}

impl<'r, 'a, S, V> SourceField<'r, 'a, S, V> {
  /// Returns draft §6.2.3.1's `fieldName`: the field's name as the schema declares it, unaffected
  /// by an alias.
  #[inline]
  pub fn name(&self) -> &'r str {
    self.schema.name(self.field_sym)
  }

  /// Returns the root Subscription type the field was selected on.
  #[inline]
  pub fn parent_type(&self) -> &'r str {
    self
      .schema
      .name(self.schema.type_def(self.parent_type).name())
  }

  /// Returns the field's declared type.
  ///
  /// It is the type each *event*'s value will be completed against by draft §6.4.3, not the type of
  /// the stream: the schema has no way to spell an event stream, which is why §6.2.3.1 has a
  /// separate resolver algorithm at all.
  #[inline]
  pub const fn field_type(&self) -> PackedType {
    self.field_type
  }

  /// Returns draft §6.2.3.1's `initialValue` — the `root` given to
  /// [`start`](super::Executor::start), which `ResolveFieldEventStream` takes as its `rootValue`.
  ///
  /// Held only while the response stream is [`Creating`](super::ResponseStream::Creating):
  /// [`handle_source_stream`](super::Executor::handle_source_stream) releases it, because once the
  /// source stream exists the initial value can no longer be read by anything and a subscription
  /// lasts for as long as its client does.
  #[inline]
  pub const fn parent_value(&self) -> &'r V {
    self.parent_value
  }

  /// Returns draft §6.2.3.1 step 8's `argumentValues`: the arguments that survived §6.4.1.
  ///
  /// The guarantee is [`FieldRequest::arguments`]', with one difference in what a failure means.
  /// Every argument §6.4.1 rejects as a whole has already refused the **subscription** — §6.2.3.1
  /// runs `CoerceArgumentValues` inside `CreateSourceEventStream`, where there is no execution
  /// result for a field error to live in, so the refusal is
  /// [`StartError::SourceFieldArguments`](super::StartError::SourceFieldArguments) and this value
  /// does not exist.
  #[inline]
  pub const fn arguments(&self) -> &'r [Argument<'a, S, V>] {
    self.arguments
  }

  /// Returns the span of the field draft §6.2.3.1 takes the name and arguments from.
  ///
  /// The first of possibly several, exactly as [`FieldRequest::location`] is: draft §6.3 merges
  /// every selection sharing the response key, and [`merged`](SourceField::merged) has the rest.
  #[inline]
  pub fn location(&self) -> SimpleSpan {
    *self.field.span()
  }

  /// Returns every field that merged into the source field's response key, in document order.
  ///
  /// Usually one. More than one when the root selection set wrote the same response key twice —
  /// `subscription { newMessage { a } newMessage { b } }` — which draft §6.2.3.1 step 5 counts as
  /// **one** entry, since §6.3 collects them into one field. That is the case a check counting
  /// *selections* rather than collected entries would refuse, and the specification does not.
  #[inline]
  pub const fn merged(&self) -> &'r [&'a Field<S>] {
    self.merged
  }
}

impl<S, V> core::fmt::Debug for SourceField<'_, '_, S, V> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("SourceField")
      .field("name", &self.name())
      .field("parent_type", &self.parent_type())
      .field("arguments", &self.arguments.len())
      .finish()
  }
}

/// Returns a field's name as source bytes, for the schema lookup that resolves it.
#[inline]
pub(super) fn name_bytes<S>(field: &Field<S>) -> &[u8]
where
  S: AsRef<[u8]>,
{
  field.name().source().as_ref()
}
