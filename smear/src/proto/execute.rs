//! Draft §6's execution core for a query, as a Sans-I/O state machine.
//!
//! # Three inputs, three obligations, and no `Step`
//!
//! The driver tells the executor things ([`start`](Executor::start),
//! [`handle_resolved`](Executor::handle_resolved), [`handle_field_error`](Executor::handle_field_error))
//! and asks it what it owes ([`poll_resolve`](Executor::poll_resolve),
//! [`poll_abandoned`](Executor::poll_abandoned), [`poll_response`](Executor::poll_response)). The
//! obvious design is one `poll` returning a `Step` enum. It is rejected here for the reason the
//! author's other Sans-I/O libraries reject it: adding a fourth obligation to a `Step` enum breaks
//! every `match` that ever consumed it, while adding a fourth channel breaks nothing, and a
//! driver's *compiled* code says which obligations it handles instead of a runtime arm saying so.
//!
//! [`poll_abandoned`](Executor::poll_abandoned) is what that buys concretely. It is the
//! null-propagation channel: when draft §6.4.4 nulls an ancestor, every field still outstanding
//! underneath it can no longer affect the response, and a driver holding a database round-trip for
//! one of them should stop. In a single `Step` enum that is one arm among many, easy to leave
//! unhandled and invisible when it is. As a channel it is a function the driver either calls or
//! does not, and one that does not call it stalls at the in-flight ceiling — loudly, rather than
//! silently doing needless work.
//!
//! # Withholding is the only flow control
//!
//! There is no queue between the executor and the driver. Fields waiting to be resolved are slots
//! in the response tree, threaded on an intrusive chain; `poll_resolve` refuses to yield past
//! [`Limits::max_in_flight`] rather than reading ahead. Withholding is also what draft §6.3's
//! serial-mutation rule needs — offer exactly one top-level mutation field, withhold the next
//! until it is answered — so the constraint is structural rather than a contract a driver could
//! forget to honour.

use core::{fmt, num::NonZeroU32};

use tokora::{SimpleSpan, span::AsSpan};

use crate::{
  parser::graphql::ast::{
    ExecutableDefinition, ExecutableDocument, Field, InputValue, OperationDefinition, SelectionSet,
  },
  validator::schema::{PackedType, RootOperation, Schema, Sym, TypeId, TypeKind, builtin},
};

use super::{
  Argument, ArgumentSource, Error, FieldRequest, Leaf, Node, ReqId, Values,
  collect::{Group, Interner, collect_fields},
  error::{Raw, Row},
  request::name_bytes,
  response::{Key, NONE, Slot, State, node},
};

#[cfg(test)]
mod tests;

/// What the executor will not do, whatever the document or the driver asks.
///
/// Two knobs, and they bound two different things: how much work is *outstanding*
/// ([`max_in_flight`](Limits::max_in_flight)) and how much response has been *built*
/// ([`max_response_slots`](Limits::max_response_slots)). Selection depth needs neither — it is
/// bounded by the document, which the validator has already accepted with acyclic fragments
/// (draft 5.5.2.2) — and list *nesting* is bounded at
/// [`MAX_WRAPPERS`](crate::validator::schema::MAX_WRAPPERS) by the schema, so a limit on either
/// could never fire and a limit that cannot fire is not a limit.
///
/// # Why the second knob is not the first one spelled differently
///
/// `max_in_flight` bounds what the driver has been *asked*, and it is checked in `poll_resolve`.
/// Nothing about it bounds what one `handle_resolved` *builds*: draft §6.4.3's list clause is
/// completed synchronously, so a single answer carrying a list of a million elements creates a
/// million positions before the call returns and before the driver is offered another chance to
/// stop. The length comes from [`Values::list_len`](super::Values::list_len) — the driver's answer
/// about the driver's own value — so it is neither the document's size nor anything the schema
/// bounds.
///
/// A *per-list* cap would not close it. List nesting is bounded at `MAX_WRAPPERS`, which is 15, so
/// a cap of `C` on each list still admits `C¹⁵` positions from one answer; at `C = 4` that is
/// already 10⁹. What has to be bounded is the total, which is why the knob counts **positions in
/// the response tree** rather than elements in a list.
///
/// It is enforced inside the one function that creates a position, so a phase that adds a new way
/// to create one inherits the bound rather than having to remember it — the design's standing rule
/// that a wrong answer should be unrepresentable rather than guarded at each call.
///
/// # Why both ceilings are a [`NonZeroU32`]
///
/// A ceiling of zero is not a strict limit, it is a stopped machine: `poll_resolve` withholds
/// because the ceiling is reached, `poll_response` withholds because the ready chain is not empty,
/// `poll_abandoned` has nothing to retire, and a driver spins on a perfectly valid query with no
/// call that can ever make progress. The three alternatives all answer that at runtime — reject it
/// from a now-fallible constructor, clamp it to one and silently execute something the caller did
/// not ask for, or write "must not be zero" in prose and hope. The type answers it before the
/// program runs, costs no branch, and keeps [`Executor::with_limits`] infallible:
///
/// ```compile_fail,E0308
/// use core::num::NonZeroU32;
///
/// use smear::proto::Limits;
///
/// const EIGHT: NonZeroU32 = NonZeroU32::new(8).expect("eight is not zero");
///
/// let stopped = Limits {
///   max_in_flight: 0,
///   max_response_slots: EIGHT,
/// };
/// ```
///
/// ```
/// use core::num::NonZeroU32;
///
/// use smear::proto::Limits;
///
/// const EIGHT: NonZeroU32 = NonZeroU32::new(8).expect("eight is not zero");
///
/// let pool_of_eight = Limits {
///   max_in_flight: EIGHT,
///   ..Limits::default()
/// };
/// assert_eq!(pool_of_eight.max_in_flight.get(), 8);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Limits {
  /// How many field requests may be outstanding at once.
  ///
  /// `poll_resolve` returns `None` at the ceiling even when work remains. This is the whole of the
  /// executor's flow control and it is deliberately the only kind: a driver that resolves fields
  /// against a connection pool of eight sets this to eight and never has to model the pool twice.
  ///
  /// Requests [`poll_abandoned`](Executor::poll_abandoned) has not yet retired count against it,
  /// so a driver that ignores that channel stops making progress rather than accumulating work
  /// nobody wants. That is a stall the driver can always clear — the channel is returning `Some`
  /// — which is what separates it from a ceiling of zero, and why the ceiling cannot be zero.
  pub max_in_flight: NonZeroU32,

  /// How many positions the response tree may hold, counting the root.
  ///
  /// A position is one node of the response: the root, a field of an object, or an element of a
  /// list. Reaching the ceiling is a draft §7.1.2 **field error** at the position that could not be
  /// completed ([`Kind::ResponseBudget`](super::Kind::ResponseBudget)), so draft §6.4.4 nulls that
  /// position and propagates from it exactly as it does for any other field error.
  ///
  /// A field error is the only shape available, and it is also the right one. The alternative —
  /// abandoning the whole execution — is a *request* error, and draft §7.1.2 reserves that for a
  /// failure raised **before** execution begins, which is why [`StartError`] is this module's only
  /// non-field refusal. Once a resolver has answered, a response with no `data` key would be
  /// telling the client the request never ran.
  ///
  /// What the error honestly says is *where execution could not continue*, not *what made the
  /// response large*: the position that happens to cross the line is blamed, and a driver reading
  /// the path should treat it as the point of exhaustion rather than the cause.
  ///
  /// It also makes two narrowings safe by arithmetic rather than by argument. A slot index and a
  /// list index are both `u32`, and `u32::MAX` is the tree's "no such slot" sentinel; because a
  /// position is only created while the count is strictly below this ceiling, the largest index
  /// reachable is `max_response_slots - 1`, which is never the sentinel and never truncates.
  pub max_response_slots: NonZeroU32,
}

/// Enough concurrency that a driver never has to think about the knob, low enough that a runaway
/// document cannot make the executor hand out unbounded work.
const DEFAULT_IN_FLIGHT: NonZeroU32 = NonZeroU32::new(256).expect("256 is not zero");

/// Larger than any response a schema designer meant to return, small enough to be a bound.
///
/// A million positions is far past a page of results and past most bulk exports; a response that
/// genuinely needs more is a service that should say so by raising this. It is a ceiling on a
/// pathological answer, not a tuning knob — which is why it is generous. A driver whose value type
/// is large, or that runs where memory is tight, should lower it: the cost of a position is one
/// slot plus one metadata row, and a slot holds the driver's own `V` inline.
const DEFAULT_RESPONSE_SLOTS: NonZeroU32 = NonZeroU32::new(1 << 20).expect("2^20 is not zero");

impl Default for Limits {
  #[inline]
  fn default() -> Self {
    Self {
      max_in_flight: DEFAULT_IN_FLIGHT,
      max_response_slots: DEFAULT_RESPONSE_SLOTS,
    }
  }
}

/// Why [`Executor::start`] refused.
///
/// Every variant is a draft §6.1 `GetOperation` failure — a *request* error, raised before
/// execution begins, which is why it is returned rather than collected into the response.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum StartError {
  /// The document holds no operation at all.
  NoOperation,
  /// A name was given and no operation carries it.
  UnknownOperation,
  /// No name was given and the document holds more than one operation.
  AmbiguousOperation,
  /// The operation is a `mutation` or a `subscription`. This executor runs queries.
  NotAQuery,
  /// The schema declares no `query` root type.
  NoQueryRoot,
}

impl fmt::Display for StartError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(match self {
      Self::NoOperation => "the document contains no operation",
      Self::UnknownOperation => "the document contains no operation with that name",
      Self::AmbiguousOperation => {
        "the document contains more than one operation and none was named"
      }
      Self::NotAQuery => "the operation is not a query",
      Self::NoQueryRoot => "the schema declares no query root type",
    })
  }
}

#[cfg(feature = "std")]
impl std::error::Error for StartError {}

/// What a slot needs from the document, kept out of [`Slot`] so the response types stay free of
/// the source type `S`.
struct Meta<'a, S> {
  /// The field draft §6.4 takes the name and arguments from — `fields[0]` of the group. `None`
  /// only for the root, which has no field.
  field: Option<&'a Field<S>>,
  /// The whole group, as a range into the executor's flat `merged` vector. Draft §6.4's
  /// `MergeSelectionSets` reads it when the value comes back.
  merged: (u32, u32),
  /// The same group's spans, as a range into the executor's flat `locations` vector — draft
  /// §7.1.2's `locations` for any error raised at this position.
  ///
  /// A second range rather than a reuse of `merged`, because [`Row`] must be readable from
  /// [`Error`](super::Error), which is deliberately not generic over the source type `S` and so
  /// cannot hold a `Field<S>`. The two vectors carry the same group in the same order but are
  /// **not** indexed alike: `fail_at` appends a lone span for the errors draft §6.4.1 points at an
  /// argument rather than at the field, so `locations` also holds entries `merged` has no member
  /// for.
  locations: (u32, u32),
  /// The object type the field was selected on, for a `Type.field` message.
  parent_type: TypeId,
  /// The field's name as a schema symbol.
  field_sym: Sym,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Slotted {
  Free {
    next: u32,
  },
  /// Outstanding, and its value still matters.
  Live,
  /// Outstanding, and draft §6.4.4 has already discarded the position it would fill.
  Abandoned,
}

#[derive(Debug)]
struct Entry {
  generation: u32,
  slot: u32,
  state: Slotted,
}

/// Draft §6's query executor.
///
/// See [`proto`](crate::proto) for the shape, why it has that shape, and a worked example.
pub struct Executor<'a, S, V>
where
  V: Values,
{
  schema: &'a Schema,
  document: &'a ExecutableDocument<S>,
  limits: Limits,
  /// `__typename`'s symbol in this schema, resolved once so that `expand` recognises the
  /// meta-field with an integer comparison rather than a name lookup per collected group.
  ///
  /// `None` means the schema never interned the name, which [`Schema::build`] cannot produce — it
  /// interns all three meta-field names unconditionally. Treating it as "no meta-field here" keeps
  /// the branch total instead of resting on that.
  typename: Option<Sym>,

  slots: std::vec::Vec<Slot<V::Value>>,
  meta: std::vec::Vec<Meta<'a, S>>,
  merged: std::vec::Vec<&'a Field<S>>,
  locations: std::vec::Vec<SimpleSpan>,
  interner: Interner,
  errors: std::vec::Vec<Row>,

  inflight: std::vec::Vec<Entry>,
  epoch: u64,
  free: u32,
  live: u32,
  abandoned: u32,
  abandon_cursor: u32,

  ready_head: u32,
  ready_tail: u32,

  started: bool,
  delivered: bool,

  scratch_fields: std::vec::Vec<(u32, &'a Field<S>)>,
  scratch_groups: std::vec::Vec<Group>,
  scratch_visited: std::vec::Vec<u32>,
  scratch_sets: std::vec::Vec<&'a SelectionSet<S>>,
  /// Draft §6.4.1's surviving arguments, for the one request `poll_resolve` is offering.
  ///
  /// The only driver values the executor owns that are not part of the response, and so the only
  /// ones whose release is not the response tree's. An [`ArgumentSource::Variable`] carries the
  /// very value §6.4.1 checked rather than the name to look it up by — see that variant for why it
  /// must — which makes this vector the owner of a `V::Value` that may be a wasm or FFI handle,
  /// and a handle held past the position it was checked for holds open whatever it names.
  ///
  /// It has exactly one reader, and [`retire_arguments`](Self::retire_arguments) is what that
  /// buys.
  scratch_args: std::vec::Vec<Argument<'a, S, V::Value>>,
}

impl<'a, S, V> Executor<'a, S, V>
where
  S: AsRef<[u8]>,
  V: Values,
{
  /// Creates an executor over a validated document and the schema it was validated against.
  ///
  /// Nothing happens until [`start`](Executor::start). Both borrows outlive the executor, so the
  /// document is never copied and every literal a driver reads out of an
  /// [`ArgumentSource`](super::ArgumentSource) points into it.
  #[inline]
  pub fn new(schema: &'a Schema, document: &'a ExecutableDocument<S>) -> Self {
    Self::with_limits(schema, document, Limits::default())
  }

  /// Creates an executor with a chosen in-flight ceiling.
  pub fn with_limits(
    schema: &'a Schema,
    document: &'a ExecutableDocument<S>,
    limits: Limits,
  ) -> Self {
    Self {
      schema,
      document,
      limits,
      typename: schema.sym(builtin::TYPENAME_FIELD.as_bytes()),
      slots: std::vec::Vec::new(),
      meta: std::vec::Vec::new(),
      merged: std::vec::Vec::new(),
      locations: std::vec::Vec::new(),
      interner: Interner::default(),
      errors: std::vec::Vec::new(),
      inflight: std::vec::Vec::new(),
      epoch: 0,
      free: NONE,
      live: 0,
      abandoned: 0,
      abandon_cursor: 0,
      ready_head: NONE,
      ready_tail: NONE,
      started: false,
      delivered: false,
      scratch_fields: std::vec::Vec::new(),
      scratch_groups: std::vec::Vec::new(),
      scratch_visited: std::vec::Vec::new(),
      scratch_sets: std::vec::Vec::new(),
      scratch_args: std::vec::Vec::new(),
    }
  }

  /// Begins draft §6.2.1 `ExecuteQuery` on `root`, which is §6.2.1's `initialValue`.
  ///
  /// `operation` names the operation to run, as draft §6.1's `GetOperation` takes it: `None`
  /// selects the document's only operation and fails if there is more than one.
  ///
  /// Collecting the root selection set happens here, so a document whose every root field is
  /// `@skip`ped produces a complete, empty response with no call to the driver at all.
  ///
  /// Every [`ReqId`] the previous operation handed out is void from this point, whether or not the
  /// driver ever answered or retired it — see [`ReqId`]'s epoch. Calling this while requests are
  /// still outstanding is therefore an abandonment of the whole operation and not a corruption of
  /// the next one; the driver hears nothing more about them, because there is no longer an
  /// operation for them to belong to.
  ///
  /// Every driver value that operation left behind is released here as well — its response tree,
  /// and the arguments of the last request it was offered. That happens before draft §6.1's lookup
  /// and therefore on the refusals too, so a `start` that returns a [`StartError`] leaves the
  /// executor holding nothing: not the operation it ended, and not the `root` it was handed, which
  /// is never stored on a path that refuses.
  pub fn start(
    &mut self,
    ctx: &mut V,
    operation: Option<&str>,
    root: V::Value,
  ) -> Result<(), StartError> {
    self.reset();
    let index = self.operation_index(operation)?;
    let definition = self
      .document
      .definitions()
      .get(index as usize)
      .map(|described| described.node());
    let Some(ExecutableDefinition::Operation(op)) = definition else {
      return Err(StartError::NoOperation);
    };
    let set = match op {
      OperationDefinition::Shorthand(set) => set,
      OperationDefinition::Named(named) => {
        if !named.operation_type().is_query() {
          return Err(StartError::NotAQuery);
        }
        named.selection_set()
      }
    };
    let root_type = self
      .schema
      .root(RootOperation::Query)
      .ok_or(StartError::NoQueryRoot)?;

    let root_ty = PackedType::named(self.schema.type_def(root_type).name(), root_type);
    self.slots.push(Slot::new(
      NONE,
      Key::Root,
      root_ty,
      State::Object {
        value: root,
        ty: root_type,
      },
    ));
    self.meta.push(Meta {
      field: None,
      merged: (0, 0),
      locations: (0, 0),
      parent_type: root_type,
      field_sym: self.schema.type_def(root_type).name(),
    });
    self.started = true;

    self.scratch_sets.clear();
    self.scratch_sets.push(set);
    self.expand(ctx, 0);
    Ok(())
  }

  /// Returns the next field the driver must resolve, or `None`.
  ///
  /// `None` means one of three things and deliberately does not distinguish them, because a driver
  /// does the same thing in all three: there is no work right now. Either the response is finished
  /// — [`poll_response`](Executor::poll_response) says so — or every remaining field is waiting on
  /// a request already outstanding, or the in-flight ceiling has been reached.
  ///
  /// Draft §6.4.1 runs *here*, before the request is handed out, so an argument the specification
  /// rejects becomes a field error and its field is never offered at all.
  pub fn poll_resolve(&mut self, ctx: &mut V) -> Option<FieldRequest<'_, 'a, S, V::Value>> {
    self.retire_arguments();
    if !self.started {
      return None;
    }
    let found = loop {
      if self.live + self.abandoned >= self.limits.max_in_flight.get() {
        break None;
      }
      let Some(slot) = self.pop_ready() else {
        break None;
      };
      if self.discarded(slot) {
        continue;
      }
      if self.coerce_arguments(ctx, slot) {
        break Some(slot);
      }
      // Argument coercion raised a field error, which nulled the field and may have nulled an
      // ancestor. There is nothing to hand the driver; take the next ready slot.
    };
    let Some(slot) = found else {
      // Withholding is not offering, so there is no `FieldRequest` to read `scratch_args` and the
      // last candidate coerced must not leave its values in it. The reachable case is a field whose
      // draft §6.4.1 raised on one argument after earlier ones had already been checked and pushed.
      self.retire_arguments();
      return None;
    };

    let id = self.acquire(slot);
    self.slots[slot as usize].state = State::InFlight;

    let meta = &self.meta[slot as usize];
    let (start, len) = meta.merged;
    let field = meta
      .field
      .expect("only the root has no field, and it is never ready");
    let parent = self.slots[slot as usize].parent;
    let parent_value = match &self.slots[parent as usize].state {
      State::Object { value, .. } => value,
      // A child slot exists only because its parent completed to an object.
      _ => unreachable!("a field slot's parent is an object"),
    };
    Some(FieldRequest {
      id,
      schema: self.schema,
      slots: &self.slots,
      names: self.interner.bytes(),
      name_spans: self.interner.spans(),
      slot,
      field,
      merged: &self.merged[start as usize..(start + len) as usize],
      field_sym: meta.field_sym,
      parent_type: meta.parent_type,
      parent_value,
      field_type: self.slots[slot as usize].ty,
      arguments: &self.scratch_args,
    })
  }

  /// Supplies the value of a field, running draft §6.4.3 `CompleteValue` on it.
  ///
  /// A stale id — one already answered, or one
  /// [`poll_abandoned`](Executor::poll_abandoned) has retired — is ignored, and so is a value for
  /// a position draft §6.4.4 has since discarded. Both are ordinary: a driver that cancels work
  /// asynchronously will race, and the executor is the side that can tell.
  ///
  /// # How long the executor owns it
  ///
  /// Until the position leaves the response, which is not always the end of the operation. Three
  /// exits, and only the first is the long one:
  ///
  /// - The position reaches the finished response, so the value is held until the next
  ///   [`start`](Executor::start) or until the executor is dropped. A reader can still ask for it,
  ///   which is what makes this the *response* rather than a copy of it.
  /// - Draft §6.4.4 later discards the position, at which point the value is released — including
  ///   when what §6.4.4 nulls is an *ancestor*, because the discard frees everything already
  ///   completed beneath it and not only the ancestor itself. A query that nulls a large subtree
  ///   therefore does not carry that subtree's handles to the end of the operation.
  /// - The id was stale or its position was already discarded, so this call stores nothing and the
  ///   value is released before it returns.
  ///
  /// A serialised leaf is released the same way its resolved value is: draft §6.4.3 step 4 moves
  /// the resolver's value into [`Values::coerce_leaf`](super::Values::coerce_leaf), and the value
  /// that comes back is what the position then owns.
  pub fn handle_resolved(&mut self, ctx: &mut V, id: ReqId, value: V::Value) {
    self.retire_arguments();
    let Some(slot) = self.release(id) else {
      return;
    };
    if self.discarded(slot) {
      return;
    }
    let ty = self.slots[slot as usize].ty;
    self.complete(ctx, slot, ty, value);
  }

  /// Reports that resolving a field raised a field error (draft §6.4.2).
  ///
  /// The message is the driver's; the response path and the location are the executor's, so a
  /// driver never has to track where it was.
  pub fn handle_field_error(&mut self, id: ReqId, message: &str) {
    self.retire_arguments();
    let Some(slot) = self.release(id) else {
      return;
    };
    if self.discarded(slot) {
      return;
    }
    let message = self.interner.intern(message.as_bytes());
    self.fail(slot, Raw::Resolver { message });
  }

  /// Returns a request whose value can no longer affect the response.
  ///
  /// Draft §6.4.4 nulls an ancestor when a non-null position errors, and everything outstanding
  /// under that ancestor becomes irrelevant the instant it does. This is how the driver hears
  /// about it, and cancelling the work is the driver's to do — `proto` performs no I/O and has
  /// nothing to cancel.
  ///
  /// Answering a retired request afterwards is harmless: the id is stale and ignored.
  pub fn poll_abandoned(&mut self) -> Option<ReqId> {
    self.retire_arguments();
    if self.abandoned == 0 {
      return None;
    }
    let len = self.inflight.len() as u32;
    for offset in 0..len {
      let index = (self.abandon_cursor + offset) % len;
      if self.inflight[index as usize].state == Slotted::Abandoned {
        self.abandon_cursor = (index + 1) % len;
        let generation = self.inflight[index as usize].generation;
        self.release_entry(index);
        self.abandoned -= 1;
        return Some(ReqId {
          epoch: self.epoch,
          index,
          generation,
        });
      }
    }
    None
  }

  /// Returns the finished response, once nothing that could still change it is outstanding.
  ///
  /// Yields at most once. A second call returns `None`, which is what makes it usable as the
  /// driver's loop condition rather than something that has to be guarded by a flag the driver
  /// keeps.
  ///
  /// Requests [`poll_abandoned`](Executor::poll_abandoned) has not yet retired do **not** withhold
  /// it. Draft §6.4.4 discarded the positions they would have filled, so by construction no answer
  /// to one can alter a single byte of what is returned here, and withholding on them would turn
  /// the documented soft stall for a driver that ignores that channel into a deadlock at exactly
  /// the moment its remaining work is zero. What that costs is a driver holding live-looking
  /// [`ReqId`]s across a restart, and [`ReqId`]'s epoch is what pays it.
  pub fn poll_response(&mut self) -> Option<Response<'_, V::Value>> {
    self.retire_arguments();
    if !self.started || self.delivered {
      return None;
    }
    self.drain_ready();
    if self.live > 0 || self.ready_head != NONE {
      return None;
    }
    self.delivered = true;
    Some(Response {
      schema: self.schema,
      slots: &self.slots,
      names: self.interner.bytes(),
      name_spans: self.interner.spans(),
      locations: &self.locations,
      errors: &self.errors,
    })
  }

  // ---------------------------------------------------------------------------------------
  // draft §6.4.3 CompleteValue
  // ---------------------------------------------------------------------------------------

  /// Draft §6.4.3 `CompleteValue`, step for step.
  ///
  /// Recursive only through its list arm, and therefore bounded by
  /// [`MAX_WRAPPERS`](crate::validator::schema::MAX_WRAPPERS) — an object's sub-selections do not
  /// recurse, they become ready slots and come back through `poll_resolve`. That is what keeps a
  /// deeply nested query from being a deeply nested call stack, which is the property a Sans-I/O
  /// machine is supposed to have.
  fn complete(&mut self, ctx: &mut V, slot: u32, ty: PackedType, value: V::Value) {
    // Steps 1 and 2 are one question — is this value null — asked once, because the two steps
    // differ only in what they do with the answer. Stripping the non-null wrapper and re-entering
    // to ask again would let a second answer that disagreed with the first write `State::Null`
    // into a position whose slot type is still non-null, with nothing in `errors`: a response
    // carrying a null the schema forbids and no account of it. One read cannot disagree with
    // itself.
    let ty = if ty.is_non_null() {
      // Step 1: a non-null position whose value is null is a field error at *this* position, and
      // §6.4.4 takes it from here.
      if ctx.is_null(&value) {
        let (parent, field) = self.owner(slot);
        self.fail(slot, Raw::NullInNonNull { parent, field });
        return;
      }
      ty.nullable()
    } else {
      // Step 2.
      if ctx.is_null(&value) {
        self.slots[slot as usize].state = State::Null;
        return;
      }
      ty
    };
    // Step 3.
    if let Some(item) = ty.list_item() {
      let Some(len) = ctx.list_len(&value) else {
        let (parent, field) = self.owner(slot);
        self.fail(slot, Raw::NotAList { parent, field });
        return;
      };
      self.slots[slot as usize].state = State::List;
      for index in 0..len {
        // `len` is the driver's answer about the driver's own value, so it is bounded by nothing
        // the schema or the document says. The budget is asked *before* the element is fetched, so
        // an over-long list costs neither the allocation nor the `list_item` call for the part of
        // it that cannot be represented. The narrowing rides on the same question: an index the
        // budget admits is below a `u32` ceiling and cannot truncate, and one it does not admit
        // never reaches `Key::Index`.
        let Some(child) = u32::try_from(index)
          .ok()
          .and_then(|at| self.push_child(slot, Key::Index(at), item, State::Ready))
        else {
          let (parent, field) = self.owner(slot);
          let limit = self.limits.max_response_slots.get();
          self.fail(
            slot,
            Raw::ResponseBudget {
              parent,
              field,
              limit,
            },
          );
          return;
        };
        let element = ctx.list_item(&value, index);
        self.complete(ctx, child, item, element);
        // An element of a `[T!]` that completed to null nulls the whole list, and there is no
        // point completing the rest of it.
        if self.slots[slot as usize].discarded {
          break;
        }
      }
      return;
    }
    // Steps 4 and 5.
    let base = ty.base_id();
    let kind = self.schema.type_def(base).kind();
    if kind.is_leaf() {
      let leaf = self.leaf(base, kind);
      // Declining and answering null are one outcome. Draft §6.4.3 step 4 raises a field error
      // when `CoerceResult` "raises an exception or returns a value other than a legal GraphQL
      // value", and null is not a legal serialised leaf — so a serialiser reports the same failure
      // either by returning `None` or by returning its representation's own null, and both belong
      // in the branch below.
      //
      // The check has to be here rather than in the driver because `Self::Value` is the driver's
      // type: `is_null` is the only question `proto` can ask about one, and asking it in the
      // executor makes the check unconditional where a documented obligation on the driver would
      // be one more thing an implementor can omit. It is also why the answer cannot be made
      // unrepresentable in the signature — a witness type would still be minted from this same
      // `is_null`, and would only move the same question somewhere it might not be asked.
      //
      // Steps 1 and 2 already asked `is_null` about the *resolver's* value; this asks about the
      // *serialiser's*, which is the one that lands in the response. The reference implementation
      // draws the line in the same place — `completeLeafValue` null-checks `serialize`'s result
      // and throws before the position's nullability is consulted — so a nullable leaf earns the
      // error too, rather than quietly keeping a null that would have been legal had a resolver
      // produced it.
      match ctx.coerce_leaf(value, leaf) {
        Some(coerced) if !ctx.is_null(&coerced) => {
          self.slots[slot as usize].state = State::Leaf(coerced);
        }
        _ => {
          let (parent, field) = self.owner(slot);
          let leaf = self.schema.type_def(base).name();
          self.fail(
            slot,
            Raw::LeafCoercion {
              leaf,
              parent,
              field,
            },
          );
        }
      }
      return;
    }

    // `ResolveAbstractType`: only an interface or a union asks the driver, because an object
    // position's runtime type is the one the schema already declared.
    let object = if kind.is_abstract() {
      let resolved = match ctx.type_name(&value) {
        None => None,
        Some(name) => {
          let id = self
            .schema
            .sym(name.as_bytes())
            .and_then(|sym| self.schema.type_of_sym(sym))
            .filter(|&id| self.schema.type_def(id).kind() == TypeKind::Object)
            .filter(|&id| self.schema.is_possible_object(base, id));
          Some((id, self.interner.intern(name.as_bytes())))
        }
      };
      match resolved {
        None => {
          let (parent, field) = self.owner(slot);
          self.fail(
            slot,
            Raw::AbstractUnresolved {
              abstract_ty: base,
              parent,
              field,
            },
          );
          return;
        }
        Some((None, runtime)) => {
          self.fail(
            slot,
            Raw::AbstractNotPossible {
              abstract_ty: base,
              runtime,
            },
          );
          return;
        }
        Some((Some(id), _)) => id,
      }
    } else {
      base
    };

    self.slots[slot as usize].state = State::Object { value, ty: object };

    // `MergeSelectionSets`: every field that shared this response key contributes its
    // sub-selections, and §6.3 collects over the concatenation.
    let (start, len) = self.meta[slot as usize].merged;
    self.scratch_sets.clear();
    for field in &self.merged[start as usize..(start + len) as usize] {
      if let Some(set) = field.selection_set() {
        self.scratch_sets.push(set);
      }
    }
    self.expand(ctx, slot);
  }

  /// Draft §6.3 over `self.scratch_sets`, creating one ready slot per response key.
  ///
  /// A `@skip`/`@include` condition that could not be read is a field error *here*, at the object
  /// whose selection set was being collected — which for the root selection set is the root, whose
  /// path is empty and whose null is the whole of `data`. That is the position the condition
  /// belongs to: the guarded field has no slot yet, and one selection's unreadable guard says
  /// nothing about its siblings' values, only that this object cannot be assembled.
  fn expand(&mut self, ctx: &mut V, slot: u32) {
    let object_type = match &self.slots[slot as usize].state {
      State::Object { ty, .. } => *ty,
      _ => return,
    };
    if self.scratch_sets.is_empty() {
      return;
    }
    // `collect_fields` borrows the scratch vectors; move them out so the borrow checker can see
    // the fields are disjoint, and put them back afterwards.
    let mut sets = core::mem::take(&mut self.scratch_sets);
    let mut fields = core::mem::take(&mut self.scratch_fields);
    let mut groups = core::mem::take(&mut self.scratch_groups);
    let mut visited = core::mem::take(&mut self.scratch_visited);
    let collected = collect_fields(
      self.schema,
      self.document,
      object_type,
      &sets,
      ctx,
      &mut self.interner,
      &mut visited,
      &mut fields,
      &mut groups,
    );
    if let Err(fault) = collected {
      // Nothing was collected, so no child slot exists to leak. The scratch vectors go back before
      // the failure is recorded, because recording it borrows the whole executor.
      sets.clear();
      self.scratch_sets = sets;
      self.scratch_fields = fields;
      self.scratch_groups = groups;
      self.scratch_visited = visited;
      self.fail_at(slot, fault.raw, fault.location);
      return;
    }

    let mut exhausted = false;
    for group in &groups {
      let selections = &fields[group.start as usize..(group.start + group.len) as usize];
      let first = selections[0].1;
      let Some(sym) = self.schema.sym(name_bytes(first)) else {
        // Draft 5.3.1 makes a field the type does not define a validation failure, so a validated
        // document cannot reach this. Omitting the key is the only response that cannot invent one.
        continue;
      };
      let Some(definition) = self.schema.field(object_type, sym) else {
        continue;
      };
      let start = self.merged.len() as u32;
      let located = self.locations.len() as u32;
      for &(_, field) in selections {
        self.merged.push(field);
        self.locations.push(*field.span());
      }
      // Draft §4.4's `__typename` is answered here, complete, and never offered to the driver.
      // `object_type` is the executor's own conclusion — draft §6.3 has already used it to decide
      // which fragment type conditions applied — so handing the same question out would let the
      // answer contradict the selections that answer produced: `... on Cat` fields under a
      // `"Dog"`, with nothing in the machine able to notice.
      //
      // The other two meta-fields are *not* intercepted. `__schema` and `__type` are ordinary
      // fields of the query root as far as this phase is concerned, offered to the driver like any
      // other; executing draft §4.5 introspection is not in scope here, and a driver that resolves
      // them itself is producing the answer from the same schema the executor would have.
      let typename = match self.typename {
        Some(meta) if meta == sym => {
          let name = self.schema.type_def(object_type).name();
          Some(self.interner.intern(self.schema.name_bytes(name)))
        }
        _ => None,
      };
      let state = match typename {
        Some(name) => State::TypeName(name),
        None => State::Ready,
      };
      let Some(child) = self.push_child(slot, Key::Field(group.key), definition.ty(), state) else {
        // The scratch vectors have to go home before the failure is recorded, because recording it
        // borrows the whole executor — the same reason the collection failure above restores first.
        exhausted = true;
        break;
      };
      self.meta[child as usize] = Meta {
        field: Some(first),
        merged: (start, group.len),
        locations: (located, group.len),
        parent_type: object_type,
        field_sym: sym,
      };
      if typename.is_none() {
        self.enqueue(child);
      }
    }

    sets.clear();
    self.scratch_sets = sets;
    self.scratch_fields = fields;
    self.scratch_groups = groups;
    self.scratch_visited = visited;

    // Recorded after the scratch is home, and at the object rather than at the field that could
    // not be created: the field has no slot, so there is no position to hang a path on, and the
    // object is the position that could not be assembled. That is the same choice an unreadable
    // `@skip` condition makes two doc comments up, for the same reason.
    if exhausted {
      let (parent, field) = self.owner(slot);
      let limit = self.limits.max_response_slots.get();
      self.fail(
        slot,
        Raw::ResponseBudget {
          parent,
          field,
          limit,
        },
      );
    }
  }

  // ---------------------------------------------------------------------------------------
  // draft §6.4.1 CoerceArgumentValues
  // ---------------------------------------------------------------------------------------

  /// Runs draft §6.4.1 `CoerceArgumentValues` into `self.scratch_args`.
  ///
  /// Returns whether the field survived. `false` means one of §6.4.1's field errors was raised and
  /// the field has already been nulled, so the caller must not hand it to the driver.
  ///
  /// All of step 5 is here, including 5.f's variable lookup, which is why this needs `ctx`.
  /// Leaving "was this variable provided" to the driver would put a conformance decision on the
  /// far side of the boundary for every driver to re-derive; [`Values::variable`] already answers
  /// it, and the `Option` it returns is exactly §6.4.1's `hasValue`.
  fn coerce_arguments(&mut self, ctx: &mut V, slot: u32) -> bool {
    // Also the release point for a candidate that raised at step 5 with earlier arguments already
    // pushed: `poll_resolve` loops on to the next ready slot, and that slot's coercion starts here.
    self.retire_arguments();
    let meta = &self.meta[slot as usize];
    let field_sym = meta.field_sym;
    let parent_type = meta.parent_type;
    let Some(node) = meta.field else {
      return true;
    };
    let Some(definition) = self.schema.field(parent_type, field_sym) else {
      return true;
    };
    let args = definition.args();
    let count = self.schema.inputs(args).len();
    if count == 0 {
      return true;
    }

    let written = node.arguments().map(|list| list.arguments()).unwrap_or(&[]);
    for index in 0..count {
      let argument = self.schema.inputs(args)[index];
      let name_bytes = self.schema.name_bytes(argument.name());
      let supplied = written
        .iter()
        .find(|written| written.name().source().as_ref() == name_bytes);
      let ty = argument.ty();
      let non_null = ty.is_non_null();
      let default = argument.default_kind().is_present();

      // Steps 5.d and 5.f: `hasValue` is false both for an absent argument and for a variable the
      // request did not supply, and those take the same branch with different messages.
      //
      // The variable is read exactly once, here, and the value is kept rather than the name: what
      // reaches the driver has to be the value these steps checked, which is what
      // `ArgumentSource::Variable` carries and why it carries it.
      let (variable, has_value, is_null) = match supplied.map(|argument| argument.value()) {
        None => (None, false, false),
        Some(InputValue::Variable(spelled)) => {
          let name = core::str::from_utf8(spelled.name().source().as_ref()).unwrap_or("");
          match ctx.variable(name) {
            None => (Some((name, None)), false, false),
            Some(value) => {
              let null = ctx.is_null(&value);
              (Some((name, Some(value))), true, null)
            }
          }
        }
        Some(InputValue::Null(_)) => (None, true, true),
        Some(_) => (None, true, false),
      };

      let value_span = supplied.map(|argument| *argument.value().as_span());

      if !has_value {
        // Step 5.h.i: a declared default applies whatever the type is, so it precedes the non-null
        // check rather than following it.
        if default {
          let name = core::str::from_utf8(name_bytes).unwrap_or("");
          self.scratch_args.push(Argument {
            name,
            ty,
            source: ArgumentSource::SchemaDefault,
          });
          continue;
        }
        if !non_null {
          continue;
        }
        match variable {
          Some((name, _)) => {
            let variable = self.interner.intern(name.as_bytes());
            let location = value_span.unwrap_or(*node.span());
            self.fail_at(
              slot,
              Raw::ArgumentVariableMissing {
                field: field_sym,
                argument: argument.name(),
                ty,
                variable,
              },
              location,
            );
          }
          None => self.fail_at(
            slot,
            Raw::ArgumentMissing {
              field: field_sym,
              argument: argument.name(),
              ty,
            },
            *node.span(),
          ),
        }
        return false;
      }

      // Step 5.i.i.
      if is_null && non_null {
        self.fail_at(
          slot,
          Raw::ArgumentNull {
            field: field_sym,
            argument: argument.name(),
            ty,
          },
          value_span.unwrap_or(*node.span()),
        );
        return false;
      }

      let name = core::str::from_utf8(name_bytes).unwrap_or("");
      let source = match (variable, supplied.map(|argument| argument.value())) {
        (Some((variable, Some(value))), _) => ArgumentSource::Variable {
          name: variable,
          value,
        },
        (None, Some(literal)) => ArgumentSource::Literal(literal),
        // `has_value` is true, so one of the arms above always matches: a variable with no value
        // returned at the step 5.d branch above, and an absent argument at 5.b.
        (Some((_, None)), _) | (None, None) => continue,
      };
      self.scratch_args.push(Argument { name, ty, source });
    }
    true
  }

  // ---------------------------------------------------------------------------------------
  // draft §6.4.4 null propagation
  // ---------------------------------------------------------------------------------------

  /// Records a field error at `slot`, located at every field of its merged group, and runs draft
  /// §6.4.4 from there.
  ///
  /// The error and the propagation are one operation on purpose. Every implementation that gets
  /// §6.4.4 wrong gets it wrong by separating them — recording the error where it happened and
  /// nulling somewhere else, or nulling correctly and attaching the path of whatever it nulled.
  /// The error's path is the path of the field that *raised* it; the null lands at the nearest
  /// nullable ancestor; and they are different slots whenever the field's type is non-null.
  ///
  /// Draft §7.1.2 makes `locations` a *list* because a response key can be several source
  /// positions: `{ a { x } a { x } }` is one field of the response and two places in the document,
  /// and a service that reported only the first would be pointing a client at half of what it
  /// wrote. The reference implementation passes the whole `fieldNodes` array to `locatedError` and
  /// maps one entry per node, which is what this range reproduces, in collection order.
  fn fail(&mut self, slot: u32, raw: Raw) {
    let locations = self.meta[slot as usize].locations;
    self.record(slot, raw, locations);
  }

  /// [`fail`](Self::fail) with a single location the caller chose.
  ///
  /// Draft §6.4.1's errors point at the offending *argument value* — a field with three arguments
  /// and one bad one should say which — while §6.4.3's point at the field, there being nothing
  /// narrower to point at. The reference implementation draws the line in the same place, and on
  /// the same side of the group: `getArgumentValues` is handed `fieldNodes[0]` alone, so an
  /// argument error over a merged group carries one location where a resolver error over the same
  /// group carries every field's.
  fn fail_at(&mut self, slot: u32, raw: Raw, location: SimpleSpan) {
    let start = self.locations.len() as u32;
    self.locations.push(location);
    self.record(slot, raw, (start, 1));
  }

  fn record(&mut self, slot: u32, raw: Raw, locations: (u32, u32)) {
    self.errors.push(Row {
      raw,
      slot,
      locations,
    });
    self.propagate(slot);
  }

  /// Draft §6.4.4: null this position, and keep nulling upward while the position is non-null.
  ///
  /// A list element's position type is the *element* type, which is what makes the specification's
  /// list clause fall out rather than needing a case: `[String!]` gives every element a non-null
  /// position, so one null element nulls the list; `[String]` does not.
  ///
  /// The upward walk only decides *where* the null lands; [`discard`](Self::discard) is what puts
  /// it there, so every position §6.4.4 nulls — the one it stops at and the chain it came up
  /// through — is written by one downward pass rather than twice by two.
  fn propagate(&mut self, slot: u32) {
    let mut cursor = slot;
    loop {
      let position = &self.slots[cursor as usize];
      let parent = position.parent;
      if !position.ty.is_non_null() || parent == NONE {
        break;
      }
      cursor = parent;
    }
    self.discard(cursor);
    self.abandon_under(cursor);
  }

  /// Takes the subtree at `root` out of the response, releasing every driver value in it.
  ///
  /// Reading the response needs none of this: [`Slot::discarded`] at `root` makes the whole subtree
  /// answer `null` without a byte of it being rewritten, which is why the walk is not what produces
  /// the answer. It is what stops the answer from *owning* what it no longer says. A
  /// [`State::Leaf`] or a [`State::Object`] under a discarded ancestor is a `V::Value` nothing in
  /// the program can reach and nothing will ever read, and `V::Value` is the driver's own type — a
  /// wasm or FFI handle, a pooled buffer, a database cursor. Holding one to the end of the
  /// operation holds open whatever it names, and the in-flight ceiling does not bound how many
  /// there are: it bounds outstanding work, and these are finished.
  ///
  /// # Why walking the subtree is affordable
  ///
  /// One call is O(subtree) and the subtree may be most of the response, so the bound worth stating
  /// is the total over an operation, and that total is linear in the number of slots — the same
  /// order as building the tree — however many field errors are raised. Three properties give it:
  ///
  /// - Every slot this reaches ends `discarded` and holding no value, so *emptying* a slot happens
  ///   at most once in an operation.
  /// - Nothing is ever added under a discarded slot afterwards. [`push_child`](Self::push_child)
  ///   has two callers, and both are past a `discarded` check — `expand` runs only on a slot
  ///   `handle_resolved` admitted, and `complete`'s list arm stops the moment the list is
  ///   discarded — so a drained subtree stays drained.
  /// - The descent therefore stops at any child that already carries the flag instead of walking
  ///   into it, and a child it stops at is a maximal discarded root that this call absorbs into a
  ///   larger one. Each call creates exactly one such root, so the stops across the whole operation
  ///   are bounded by the number of calls, which is bounded by the number of field errors.
  ///
  /// The alternative is a second structure listing each nullable position's value-bearing
  /// descendants, so that a discard could unlink them without touching the positions that hold
  /// nothing. It buys no better order — every value is still released exactly once — and costs a
  /// per-slot link maintained on the completion path, which is the path this module spends nothing
  /// per response key on.
  fn discard(&mut self, root: u32) {
    let mut cursor = root;
    loop {
      let slot = &mut self.slots[cursor as usize];
      // Assigning `Null` *is* the release: it drops a leaf's serialised value and an object's
      // resolved one. Nothing else in the slot has to change, because nothing else is owned.
      slot.state = State::Null;
      slot.discarded = true;
      let first_child = slot.first_child;

      if let Some(child) = self.first_live(first_child) {
        cursor = child;
        continue;
      }
      loop {
        if cursor == root {
          return;
        }
        if let Some(sibling) = self.first_live(self.slots[cursor as usize].next_sibling) {
          cursor = sibling;
          break;
        }
        cursor = self.slots[cursor as usize].parent;
      }
    }
  }

  /// The first slot at or after `from` on a sibling chain that a discard has not already emptied.
  ///
  /// Skipping is the whole of the amortised bound in [`discard`](Self::discard): a slot that
  /// carries the flag was the root of an earlier discard, so its subtree holds nothing and
  /// descending into it would rediscover that one slot at a time.
  fn first_live(&self, from: u32) -> Option<u32> {
    let mut cursor = from;
    while cursor != NONE {
      if !self.slots[cursor as usize].discarded {
        return Some(cursor);
      }
      cursor = self.slots[cursor as usize].next_sibling;
    }
    None
  }

  /// Marks every outstanding request under `root` abandoned.
  ///
  /// Walks *up* from each request rather than down from `root`, even though
  /// [`discard`](Self::discard) has just walked down: a slot does not know which entry of the
  /// in-flight table points at it, so folding this into that pass would mean carrying a
  /// back-pointer on every slot to save a walk that is already the product of two bounded numbers
  /// — the table is bounded by [`Limits::max_in_flight`] and a parent chain by the document's
  /// depth.
  fn abandon_under(&mut self, root: u32) {
    for index in 0..self.inflight.len() {
      if self.inflight[index].state != Slotted::Live {
        continue;
      }
      let mut cursor = self.inflight[index].slot;
      let under = loop {
        if cursor == root {
          break true;
        }
        if cursor == NONE {
          break false;
        }
        cursor = self.slots[cursor as usize].parent;
      };
      if under {
        self.inflight[index].state = Slotted::Abandoned;
        self.live -= 1;
        self.abandoned += 1;
      }
    }
  }

  /// Returns whether the slot, or any ancestor, has been discarded.
  fn discarded(&self, slot: u32) -> bool {
    let mut cursor = slot;
    while cursor != NONE {
      if self.slots[cursor as usize].discarded {
        return true;
      }
      cursor = self.slots[cursor as usize].parent;
    }
    false
  }

  // ---------------------------------------------------------------------------------------
  // plumbing
  // ---------------------------------------------------------------------------------------

  /// Drops the argument values `scratch_args` is holding.
  ///
  /// It holds them for exactly one reader — the [`FieldRequest`] `poll_resolve` built out of them
  /// — and that borrow is over before any `&mut self` method can run. So a value still here is one
  /// nothing in the program can observe, and an unobservable value must not be a held one:
  /// `V::Value` may be a handle whose drop closes a connection, frees a table entry or releases a
  /// foreign allocation.
  ///
  /// Every entry point begins with this, which makes the release **per request** rather than per
  /// operation. It is the tightest the API admits: the executor is never told when the driver drops
  /// a [`FieldRequest`], so the first moment it can act on that is the driver's next call in —
  /// except in `poll_resolve`, which also releases before it withholds, a `None` return being the
  /// statement that no request is being offered at all. Tighter than that would mean a
  /// [`FieldRequest`] owning its arguments and releasing them with itself, which costs an
  /// allocation per field request: the one thing this module refuses to spend per response key.
  #[inline]
  fn retire_arguments(&mut self) {
    self.scratch_args.clear();
  }

  fn reset(&mut self) {
    self.slots.clear();
    self.meta.clear();
    self.merged.clear();
    self.locations.clear();
    self.interner.clear();
    self.errors.clear();
    self.inflight.clear();
    self.retire_arguments();
    // Emptying the slab makes every id the last operation issued reusable by index and generation,
    // so the epoch has to move in the same breath. `wrapping_add` because the increment must stay
    // total; at 64 bits the wrap is not reachable by any program that could also still be holding
    // an id from the operation it would collide with.
    self.epoch = self.epoch.wrapping_add(1);
    self.free = NONE;
    self.live = 0;
    self.abandoned = 0;
    self.abandon_cursor = 0;
    self.ready_head = NONE;
    self.ready_tail = NONE;
    self.started = false;
    self.delivered = false;
  }

  fn operation_index(&self, name: Option<&str>) -> Result<u32, StartError> {
    let mut found = None;
    for (index, described) in self.document.definitions().iter().enumerate() {
      let ExecutableDefinition::Operation(operation) = described.node() else {
        continue;
      };
      match name {
        None => {
          if found.is_some() {
            return Err(StartError::AmbiguousOperation);
          }
          found = Some(index as u32);
        }
        Some(wanted) => {
          if let OperationDefinition::Named(named) = operation
            && named
              .name()
              .is_some_and(|name| name.source().as_ref() == wanted.as_bytes())
          {
            return Ok(index as u32);
          }
        }
      }
    }
    match (name, found) {
      (Some(_), _) => Err(StartError::UnknownOperation),
      (None, Some(index)) => Ok(index),
      (None, None) => Err(StartError::NoOperation),
    }
  }

  /// Creates one position under `parent`, or `None` when
  /// [`Limits::max_response_slots`] has no room for it.
  ///
  /// This is the *only* function that creates a position, and the ceiling is checked here rather
  /// than at either caller on purpose. Draft §6.4.3's list clause completes synchronously, so the
  /// caller that most needs bounding is a loop whose trip count is the driver's own
  /// [`Values::list_len`](super::Values::list_len); a guard written into that loop would bound
  /// that loop, and the next phase to grow a position — draft §6.3's mutation path, §6.2.3's
  /// per-event collection — would have to remember to write it again. Returning `None` from here
  /// makes forgetting impossible: a caller has to answer the empty case to get an index at all.
  ///
  /// The `as u32` below is exact for the same reason. `self.slots.len()` is strictly less than the
  /// ceiling whenever this returns `Some`, and the ceiling is a `u32`, so the new index is at most
  /// `max_response_slots - 1` — never [`NONE`], which is `u32::MAX` and means "no such slot".
  fn push_child(
    &mut self,
    parent: u32,
    key: Key,
    ty: PackedType,
    state: State<V::Value>,
  ) -> Option<u32> {
    if self.slots.len() as u64 >= u64::from(self.limits.max_response_slots.get()) {
      return None;
    }
    let index = self.slots.len() as u32;
    self.slots.push(Slot::new(parent, key, ty, state));
    // A list element inherits the field's metadata wholesale, which is what makes an error at
    // `items.1` report `Query.items` and the field group's locations while its path stays the
    // element's. Draft §6.4.3's list clause reuses the field's node for exactly that reason, and
    // sharing the range rather than copying it keeps a thousand-element list at one entry.
    self.meta.push(Meta {
      field: self.meta[parent as usize].field,
      merged: self.meta[parent as usize].merged,
      locations: self.meta[parent as usize].locations,
      parent_type: self.meta[parent as usize].parent_type,
      field_sym: self.meta[parent as usize].field_sym,
    });
    let last = self.slots[parent as usize].last_child;
    if last == NONE {
      self.slots[parent as usize].first_child = index;
    } else {
      self.slots[last as usize].next_sibling = index;
    }
    self.slots[parent as usize].last_child = index;
    Some(index)
  }

  fn enqueue(&mut self, slot: u32) {
    self.slots[slot as usize].next_ready = NONE;
    if self.ready_tail == NONE {
      self.ready_head = slot;
    } else {
      self.slots[self.ready_tail as usize].next_ready = slot;
    }
    self.ready_tail = slot;
  }

  fn pop_ready(&mut self) -> Option<u32> {
    let head = self.ready_head;
    if head == NONE {
      return None;
    }
    self.ready_head = self.slots[head as usize].next_ready;
    if self.ready_head == NONE {
      self.ready_tail = NONE;
    }
    self.slots[head as usize].next_ready = NONE;
    Some(head)
  }

  /// Pops discarded slots off the head of the ready chain, so an empty chain means "no live work".
  fn drain_ready(&mut self) {
    while self.ready_head != NONE && self.discarded(self.ready_head) {
      self.pop_ready();
    }
  }

  fn acquire(&mut self, slot: u32) -> ReqId {
    self.live += 1;
    if self.free != NONE {
      let index = self.free;
      let Slotted::Free { next } = self.inflight[index as usize].state else {
        unreachable!("the free chain only holds free entries");
      };
      self.free = next;
      self.inflight[index as usize].state = Slotted::Live;
      self.inflight[index as usize].slot = slot;
      return ReqId {
        epoch: self.epoch,
        index,
        generation: self.inflight[index as usize].generation,
      };
    }
    let index = self.inflight.len() as u32;
    self.inflight.push(Entry {
      generation: 0,
      slot,
      state: Slotted::Live,
    });
    ReqId {
      epoch: self.epoch,
      index,
      generation: 0,
    }
  }

  /// Retires a live request, returning the slot it was for, or `None` when the id is stale.
  fn release(&mut self, id: ReqId) -> Option<u32> {
    // The epoch first, because it is the only one of the three tags that survives `reset`: after a
    // restart the slab is empty and an id from the operation before matches by index and
    // generation again.
    if id.epoch != self.epoch {
      return None;
    }
    let entry = self.inflight.get(id.index as usize)?;
    if entry.generation != id.generation {
      return None;
    }
    match entry.state {
      Slotted::Live => {
        let slot = entry.slot;
        self.live -= 1;
        self.release_entry(id.index);
        Some(slot)
      }
      Slotted::Abandoned => {
        // The driver answered a request §6.4.4 already discarded. Retiring it here rather than
        // waiting for `poll_abandoned` would let the id be reissued while the driver still holds
        // it, so it stays abandoned and the answer is dropped.
        None
      }
      Slotted::Free { .. } => None,
    }
  }

  fn release_entry(&mut self, index: u32) {
    let entry = &mut self.inflight[index as usize];
    entry.generation = entry.generation.wrapping_add(1);
    entry.state = Slotted::Free { next: self.free };
    self.free = index;
  }

  /// Returns the `Type.field` pair a message names this position by.
  fn owner(&self, slot: u32) -> (TypeId, Sym) {
    let meta = &self.meta[slot as usize];
    (meta.parent_type, meta.field_sym)
  }

  fn leaf(&self, base: TypeId, kind: TypeKind) -> Leaf<'a> {
    let name = self.schema.name(self.schema.type_def(base).name());
    if kind == TypeKind::Enum {
      return Leaf::Enum(name);
    }
    match name {
      "Int" => Leaf::Int,
      "Float" => Leaf::Float,
      "String" => Leaf::String,
      "Boolean" => Leaf::Boolean,
      "ID" => Leaf::Id,
      other => Leaf::Scalar(other),
    }
  }
}

/// A finished draft §7.1 response.
pub struct Response<'r, V> {
  schema: &'r Schema,
  slots: &'r [Slot<V>],
  names: &'r [u8],
  name_spans: &'r [(u32, u32)],
  locations: &'r [SimpleSpan],
  errors: &'r [Row],
}

impl<'r, V> Response<'r, V> {
  /// Returns the response's `data` entry.
  ///
  /// [`Node::Null`] is `"data": null`, which is what draft §6.4.4 produces when a non-null root
  /// field errors — not an absent `data`, which §7.1.1 reserves for a *request* error and which
  /// this executor cannot produce because it is only entered with a validated document.
  #[inline]
  pub fn data(&self) -> Node<'r, V> {
    node(self.slots, self.names, self.name_spans, 0)
  }

  /// Returns the response's `errors`, in the order they were raised.
  #[inline]
  pub fn errors(&self) -> impl ExactSizeIterator<Item = Error<'r, V>> + '_ {
    self.errors.iter().map(move |row| Error {
      schema: self.schema,
      slots: self.slots,
      names: self.names,
      name_spans: self.name_spans,
      locations: self.locations,
      row,
    })
  }

  /// Returns how many field errors the response carries.
  #[inline]
  pub fn error_count(&self) -> usize {
    self.errors.len()
  }

  /// Returns whether any field error was raised.
  #[inline]
  pub fn is_ok(&self) -> bool {
    self.errors.is_empty()
  }
}

impl<V> fmt::Debug for Response<'_, V> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("Response")
      .field("data", &self.data())
      .field("errors", &self.error_count())
      .finish()
  }
}
