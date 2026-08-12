//! Draft §7.1.3's *request error result*: the second of §7.1's three result kinds this crate has.
//!
//! # Why this is a type and not a paragraph
//!
//! §7.1 defines a *response* as "either an *execution result*, a *response stream*, or a *request
//! error result*", and this crate had one of the three. [`Response`](crate::Response) is §7.1.1's
//! execution result; §7.1.2's response stream belongs to the subscription phase; and §7.1.3's
//! request error result was **recorded as unreachable-from-here and left to the driver** — a
//! decision `StartError`'s header carried, and one this module reverses. What changed is not the
//! reachability, which was already correct and already pinned by test: it is the reading of what
//! §7.1.3 needs.
//!
//! §7.1.3 makes four demands, and every one of them is structural:
//!
//! > A *request error result* must be a map. […] The *request error result* map must contain an
//! > entry with key {"errors"}. The value of this entry must be a non-empty list of *request
//! > error* raised during the *request*. […] The *request error result* map must not contain an
//! > entry with key {"data"}. […] The *request error result* map may also contain an entry with
//! > key `extensions`.
//!
//! [`RequestErrorResult`] is each of those made unrepresentable-otherwise, which is the same
//! argument [`Extensions`] makes about §7.1.7's one clause. It is a map by construction; its
//! `errors` list is non-empty by construction, because the error is a field and not a `Vec`; it
//! has **no `data` accessor at all**, so the one entry §7.1.3 forbids cannot be written; and
//! `extensions` is an `Option` around the same budgeted container the execution result carries.
//!
//! The alternative was a documentation note telling a driver to assemble the map itself, and that
//! is what the base shipped. A driver following it correctly produces the same bytes. A driver
//! that reaches for [`Response`](crate::Response) — the only response type the crate had —
//! produces a map with a `data` entry, because [`Response::data`](crate::Response::data) is
//! infallible, and §7.1.3 says that map must not have one.
//!
//! # What the `errors` list holds, and what it deliberately cannot
//!
//! Exactly one entry: the [`StartError`] the refusal raised. That is not a simplification of a
//! list — it is the whole population. [`Executor::start`](crate::Executor::start) returns on its
//! first §6.1 `GetOperation` failure and accumulates nothing, so there is never a second error for
//! this crate to put in.
//!
//! §7.1.3's own note names four ways a request error arises — "missing information, syntax errors,
//! validation failure, coercion failure" — and **three of them are outside this crate by its own
//! division of labour**: a syntax error is `smear-parser`'s, a validation failure is
//! `smear-compiler`'s draft §5, and a variable coercion failure is the driver's, per
//! [`Values::variable`](crate::Values::variable)'s contract that values arriving there are already
//! coerced. None of the three ever reaches an [`Executor`](crate::Executor), so none of them has a
//! `StartError` to build one of these from, and a driver holding one still assembles its own map.
//!
//! **That is the residue, and it is stated as a boundary rather than as a to-do.** Carrying a
//! driver-supplied error would mean a public request-error *entry* type — a message, and the
//! per-error `extensions` §7.1.6 admits — which is exactly the diagnostic contract al8n/smear#126
//! reserves, and it would mean two new ceilings besides, because a driver-grown list of
//! driver-supplied messages is a count multiplied by a length and **a budget on one factor of a
//! product bounds nothing**. So the #126 dependency is real and this module narrows it: #126 gates
//! a *driver-supplied entry*, not §7.1.3's result shape. The shape needs no error type this crate
//! does not already have.
//!
//! # The one entry needs nothing #126 has not already been asked for
//!
//! §7.1.6's *Error Result Format* has exactly one `must`: "Every error must contain an entry with
//! the key {"message"} with a string description of the error". [`StartError`]'s `Display` is that
//! string, rendered rather than stored, for the reason `error.rs` gives about the execution-result
//! side. The other three entries do not apply or do not bind:
//!
//! | §7.1.6 entry | strength | here |
//! |---|---|---|
//! | `message` | must | [`StartError`]'s `Display` |
//! | `path` | must, **if** the error "can be associated to a particular field in the GraphQL result" | never: a request error result has no result to hold a field |
//! | `locations` | should, **if** associable to "a particular point in the requested GraphQL document" | not carried — see below |
//! | `extensions` | may | not carried; this is #126's, and the execution result's [`Error`](crate::Error) does not carry one either |
//!
//! `locations` is the only judgement call, and the honest answer is "for all but one of them, and
//! not for that one". §7.1.6 conditions it on the error being associable to "a particular point in
//! the requested GraphQL document", and a §6.1 `GetOperation` failure is a property of the
//! **request** rather than of a point in the text: which operation to run is chosen by the request's
//! `operationName`, so [`AmbiguousOperation`](StartError::AmbiguousOperation) is about a name that
//! was *not* given, [`UnknownOperation`](StartError::UnknownOperation) about one that names
//! nothing, and [`NoOperation`](StartError::NoOperation) about a document with none — no syntax
//! element to point at in any of the three.
//! [`ResponseStreamOpen`](StartError::ResponseStreamOpen) is a property of the *executor* and not of
//! any document at all: it says a previous subscription's response stream has not been ended, so
//! there is no point in the requested text to associate it with either.
//! [`NoQueryRoot`](StartError::NoQueryRoot) is unreachable: `Schema::build` refuses a schema with
//! no query root (`MissingQueryRootOperationType`, observed) and it is the sole door —
//! `Schema::from_introspection` renders SDL and calls it — so no `Schema` an executor can be handed
//! raises it. `the_errors_entry_is_the_one_refusal_start_raised` records that as the one variant its
//! table cannot reach.
//!
//! **[`NoMutationRoot`](StartError::NoMutationRoot) is the sixth, it *is* associable, and this does
//! not carry its span.** The fault is the schema's, but the operation definition that asked for a
//! mutation root is in the requested document and has a span; `graphql-js` 16.11.0 attaches exactly
//! that node — `new GraphQLError(\`Schema is not configured to execute … operation.\`, { nodes:
//! operation })` in `executeOperation`, against three bare `GraphQLError(message)` constructions
//! for the `GetOperation` three. So this is an unmet "should" and is recorded as one rather than
//! argued away. Closing it means [`StartError`] carrying data, which changes a `Copy`, data-free,
//! `PartialEq` enum that `start` returns and that every case in `proto_execute.rs` compares by
//! value — a change to a shipped surface, for a "should", and not this section's to make.
//!
//! # The map it retains, and what bounds it
//!
//! One resource, and it is the same one [`Executor::set_extensions`](crate::Executor::set_extensions)
//! defends, so it gets the same row the executor's own lifetime table would give it:
//!
//! | member | bound | readers | dead at | released by | what would strand it |
//! |---|---|---|---|---|---|
//! | draft §7.1.7 `extensions` | this result's two ceilings, **and** its spine capacity | [`RequestErrorResult::extensions`] | drop | [`take_extensions`](RequestErrorResult::take_extensions); drop | a refusal that dropped the map instead of handing it back; and a map retained with the capacity a *different* [`Limits`] let it grow to |
//!
//! Three factors and not two, because that is the lesson the execution-result side paid for twice:
//! entries and key bytes bound what a map *reports*, and a `Vec`'s capacity is derived from
//! neither — [`Extensions::remove`] refunds the budget and returns no slot. So
//! [`set_extensions`](RequestErrorResult::set_extensions) re-checks both reported quantities
//! **and** re-derives the spine, exactly as the executor's does. **A ceiling bounds a retained
//! buffer only if every growth of that buffer passed through it**, and the growth site is
//! [`Extensions::insert`], charged against whatever ceilings the *map* was created under.
//!
//! Which is why this type carries ceilings at all rather than accepting any map — and it is worth
//! being exact about what they buy, because the executor's version of this sentence does not
//! transfer. There, the re-check defends a *third party*: the map is retained inside a machine the
//! driver keeps calling, under a budget the driver did not choose. Here there is no third party. A
//! [`RequestErrorResult`] is the caller's value and the caller drops it, so these ceilings are not
//! a claim about anyone else's memory.
//!
//! What they buy is that **one** budget governs the map this result retains — the one named once at
//! construction — instead of whatever budget the map happened to be built under. Pass
//! [`Executor::limits`](crate::Executor::limits) and that is the refusing executor's, which is the
//! intended call and what the worked example shows. Pass something laxer and the caller has widened
//! its own footprint on purpose: the same freedom [`Extensions::new`] already gives, and the reason
//! neither constructor can take it away.
//!
//! # A refusal is repairable, and returns what it refused
//!
//! [`TooLarge`] carries the map back and names which [`Ceiling`] refused. Both halves are
//! obligations rather than courtesies. The map comes back because an entry's value may be a wasm
//! or FFI handle and a refusal that closes one has done more than refuse — the argument
//! [`Full`](crate::Full) and [`SetExtensionsError`](crate::SetExtensionsError) both make. The
//! ceiling is named because the caller's remedy differs: one says drop entries, the other says
//! shorten keys, and a refusal a caller cannot act on is a wall rather than an error.

use core::fmt;

use crate::{Ceiling, Extensions, Limits, StartError};

/// A map [`RequestErrorResult::set_extensions`] refused, carrying it back.
///
/// Separate from [`SetExtensionsError`](crate::SetExtensionsError), whose other two variants are
/// the executor's operation lifecycle. A [`RequestErrorResult`] is a finished value with no
/// lifecycle at all — there is no phase in which attaching a map to it is meaningless — so a
/// signature admitting `NoOperation` or `AlreadyDelivered` here would promise refusals that cannot
/// happen.
pub struct TooLarge<V> {
  ceiling: Ceiling,
  extensions: Extensions<V>,
}

impl<V> TooLarge<V> {
  /// Returns which ceiling refused, so the caller knows which way to shrink.
  #[inline]
  pub const fn ceiling(&self) -> Ceiling {
    self.ceiling
  }

  /// Returns the map that was refused, giving up ownership.
  #[inline]
  pub fn into_extensions(self) -> Extensions<V> {
    self.extensions
  }
}

impl<V> fmt::Debug for TooLarge<V> {
  /// Written out rather than derived, because a derive would bound `V: Debug`.
  ///
  /// [`Extensions`]'s own `Debug` requires nothing of `V` — it renders keys and the *presence* of
  /// a value — so the derive's bound would be strictly larger than the field needs, and it would
  /// reach every caller that wants to print a refusal. That is the same over-bound `Extensions`
  /// avoided so that [`Response`](crate::Response) could keep its `Debug` unbounded.
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("TooLarge")
      .field("ceiling", &self.ceiling)
      .field("extensions", &self.extensions)
      .finish()
  }
}

impl<V> fmt::Display for TooLarge<V> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Display::fmt(&self.ceiling, f)
  }
}

#[cfg(feature = "std")]
impl<V> std::error::Error for TooLarge<V> {}

/// Draft §7.1.3's *request error result*: the response for a request that failed before execution.
///
/// `request_error.rs`'s module header has why this is a type, why its `errors` list holds exactly
/// one entry, and what bounds the map it retains.
///
/// Named as a file rather than linked, because the module is private — the type reaches a consumer
/// through the crate root's `pub use`, so rustdoc renders no page for the module and a `[…](self)`
/// link is a public item pointing at a private one. `extensions.rs` refers to its own header the
/// same way, for the same reason.
///
/// # `data` is absent, and absence is the point
///
/// §7.1.3: "The *request error result* map must not contain an entry with key {"data"}." There is
/// no accessor for one here and there must never be, because a driver serialising from this type
/// has nothing to write it from. That is a stronger guarantee than a documented rule and a weaker
/// thing to state, so it is stated as a gate instead:
///
/// ```compile_fail,E0599
/// use graphql_proto::{Limits, RequestErrorResult, StartError};
///
/// let result: RequestErrorResult<()> =
///   RequestErrorResult::new(&Limits::default(), StartError::AmbiguousOperation);
/// // §7.1.3 forbids the entry, so the accessor does not exist.
/// let _ = result.data();
/// ```
///
/// # Worked example
///
/// The refusal below is a **valid** document's: draft §6.1 `GetOperation` chooses which operation
/// to run from the request's operation name, so draft §5 validation cannot pre-empt an ambiguity
/// between two named operations. That is what makes this shape reachable rather than theoretical.
///
/// ```
/// use graphql_proto::{Executor, Extensions, RequestErrorResult, StartError, Values};
/// # use graphql_proto::Leaf;
/// use smear_parser::{
///   graphql::{
///     GraphQL,
///     ast::{ExecutableDocument, TypeSystemDocument},
///     error::GraphqlErrors,
///     syntactic::{GraphqlLexer, executable_document, type_system_document},
///   },
///   lexer::tokora::{Parse as _, Parser},
/// };
/// use smear_schema::Schema;
///
/// # struct Space;
/// # impl Values for Space {
/// #   type Value = String;
/// #   fn is_null(&self, _: &String) -> bool { false }
/// #   fn as_bool(&self, _: &String) -> Option<bool> { None }
/// #   fn list_len(&self, _: &String) -> Option<usize> { None }
/// #   fn list_item(&mut self, _: &String, _: usize) -> String { String::new() }
/// #   fn type_name<'a>(&'a self, _: &'a String) -> Option<&'a str> { None }
/// #   fn coerce_leaf(&mut self, value: String, _: Leaf<'_>) -> Option<String> { Some(value) }
/// #   fn variable(&mut self, _: &str) -> Option<String> { None }
/// # }
/// let schema = Schema::build(
///   &Parser::with_parser::<GraphqlLexer<'_, str>, TypeSystemDocument<&str>, GraphqlErrors<&str>, _, GraphQL>(
///     type_system_document,
///   )
///   .parse_str("type Query { greeting: String }")
///   .expect("the SDL parses"),
/// )
/// .expect("the SDL is a schema");
///
/// let document = Parser::with_parser::<GraphqlLexer<'_, str>, ExecutableDocument<&str>, GraphqlErrors<&str>, _, GraphQL>(
///   executable_document,
/// )
/// .parse_str("query A { greeting } query B { greeting }")
/// .expect("the query parses");
///
/// let mut space = Space;
/// let mut executor = Executor::new(&schema, &document);
/// let Err(error) = executor.start(&mut space, None, String::new()) else {
///   panic!("two named operations and no name is draft §6.1's ambiguity")
/// };
/// assert_eq!(error, StartError::AmbiguousOperation);
///
/// // The ceilings are the refusing executor's, so the map this result may retain is bounded by
/// // the same budget the operation would have run under.
/// let mut result = RequestErrorResult::new(executor.limits(), error);
/// let mut extensions = Extensions::new(executor.limits());
/// extensions
///   .insert("traceId", "9f2c".to_owned())
///   .expect("well under `max_extension_entries`");
/// result
///   .set_extensions(extensions)
///   .expect("built under the same ceilings this result carries");
///
/// // §7.1.3's map, entry by entry: a non-empty `errors`, an optional `extensions`, and no `data`.
/// let mut errors = result.errors();
/// assert_eq!(errors.len(), 1);
/// assert_eq!(
///   errors.next().expect("non-empty by construction").to_string(),
///   "the document contains more than one operation and none was named",
/// );
/// assert_eq!(
///   result.extensions().and_then(|map| map.get("traceId")).map(String::as_str),
///   Some("9f2c"),
/// );
/// ```
pub struct RequestErrorResult<V> {
  /// The one request error this crate raises, and the whole of the `errors` entry.
  ///
  /// A field rather than a `Vec`, which is what makes "non-empty" a property of the type instead
  /// of an invariant a constructor has to keep.
  error: StartError,
  extensions: Option<Extensions<V>>,
  /// [`Limits::max_extension_entries`], as it stood on the executor that refused.
  ///
  /// Copied rather than borrowed so this value outlives the executor: a driver commonly answers
  /// the request long after dropping the machine that refused it, and a `&Limits` would tie the
  /// response's lifetime to the executor's for no benefit — `Limits` is `Copy` and two `u32` are
  /// all of it that this type reads.
  max_extension_entries: u32,
  /// [`Limits::max_extension_key_bytes`], as it stood on the executor that refused.
  max_extension_key_bytes: u32,
}

impl<V> RequestErrorResult<V> {
  /// Builds the result for a [`StartError`], under `limits`.
  ///
  /// Pass [`Executor::limits`](crate::Executor::limits) — the ceilings of the executor that
  /// refused — so that the map this result may retain is bounded by the budget the operation
  /// itself would have run under. Nothing forces that, for the same reason nothing forces
  /// [`Extensions::new`]'s argument: this value is the caller's and the caller drops it, so the
  /// ceilings bound what *this* result retains and are not a claim about anyone else's memory.
  ///
  /// Nothing is allocated. An `extensions` entry is absent until
  /// [`set_extensions`](RequestErrorResult::set_extensions) is called, which is a different
  /// response from a present empty map — the distinction
  /// [`Extensions::is_empty`] exists to keep.
  #[inline]
  #[must_use]
  pub const fn new(limits: &Limits, error: StartError) -> Self {
    Self {
      error,
      extensions: None,
      max_extension_entries: limits.max_extension_entries.get(),
      max_extension_key_bytes: limits.max_extension_key_bytes.get(),
    }
  }

  /// Returns the request error this result reports.
  ///
  /// The same value [`start`](crate::Executor::start) returned. Kept as an accessor rather than
  /// left to `errors().next()` because branching on the failure is what a driver does with it, and
  /// a classification reached through an iterator reads as though there might be another.
  #[inline]
  pub const fn error(&self) -> StartError {
    self.error
  }

  /// Returns the result's `errors` entry: draft §7.1.3's non-empty list.
  ///
  /// One item, always. The length is in the type — [`ExactSizeIterator`] — so a serialiser writing
  /// this entry never has to check for the empty list §7.1.3 forbids.
  ///
  /// The item is the error itself rather than a diagnostic wrapper. §7.1.6 requires one entry of
  /// an error map, `message`, and [`StartError`]'s `Display` is it; the module header has the
  /// table of the other three and why none of them binds here.
  #[inline]
  pub fn errors(&self) -> impl ExactSizeIterator<Item = StartError> {
    core::iter::once(self.error)
  }

  /// Returns the result's draft §7.1.7 *Extensions* entry, or `None` when it has none.
  ///
  /// `None` is an absent key and never an empty map, exactly as it is on
  /// [`Response::extensions`](crate::Response::extensions): §7.1.3 makes the entry optional
  /// ("**may** also contain"), and `Some` holding an empty [`Extensions`] is the *present* empty
  /// map, which is a different response.
  #[inline]
  pub const fn extensions(&self) -> Option<&Extensions<V>> {
    self.extensions.as_ref()
  }

  /// Attaches the draft §7.1.7 *Extensions* map, returning whatever map it replaced.
  ///
  /// This is the **second** of §7.1.7's two sites — "The `extensions` entry in an *execution
  /// result* or *request error result*" — and the first is
  /// [`Executor::set_extensions`](crate::Executor::set_extensions). The two behave alike on
  /// purpose, because the resource is the same one.
  ///
  /// # Refusal
  ///
  /// One way, and it hands the map back: [`TooLarge`], when the map's entries or key bytes exceed
  /// the ceilings this result was created under. There is no lifecycle refusal, because a result
  /// has no phases — it is already the response.
  ///
  /// # On acceptance the allocation is re-derived, and that is not a refusal
  ///
  /// The two ceilings bound what the map *reports*; this bounds what it *holds*. A map grown to a
  /// million entries under a lax [`Limits`] and emptied back down reports `len() == 0` and
  /// `key_bytes() == 0` through both checks above while still holding a million entry slots,
  /// because [`Extensions::remove`] refunds the budget and returns no slot to the allocator. So an
  /// accepted map's spine is re-derived from the entries it actually carries whenever it is over
  /// [`Limits::max_extension_entries`]. Nothing is cloned, nothing is dropped, and no observable
  /// of [`Extensions`] changes — which is why it is a normalization and not a fourth refusal a
  /// caller could not act on anyway, there being no `shrink` out here to act with.
  ///
  /// It runs **after** the refusal check, so a map this result is not going to keep is not
  /// reallocated on its way back out to the driver.
  pub fn set_extensions(
    &mut self,
    mut extensions: Extensions<V>,
  ) -> Result<Option<Extensions<V>>, TooLarge<V>> {
    if extensions.len() as u64 > u64::from(self.max_extension_entries) {
      return Err(TooLarge {
        ceiling: Ceiling::Entries,
        extensions,
      });
    }
    if u64::from(extensions.key_bytes()) > u64::from(self.max_extension_key_bytes) {
      return Err(TooLarge {
        ceiling: Ceiling::KeyBytes,
        extensions,
      });
    }
    extensions.shrink_to_ceiling(self.max_extension_entries);
    Ok(self.extensions.replace(extensions))
  }

  /// Removes the draft §7.1.7 *Extensions* map, returning it, so the result carries no such entry.
  ///
  /// The counterpart to [`Executor::take_extensions`](crate::Executor::take_extensions) and there
  /// for the same reason: absence must not be a one-way door, and a driver that changes its mind
  /// gets its values back rather than having to drop the whole result to release them — which
  /// matters when one of them is a handle.
  #[inline]
  pub fn take_extensions(&mut self) -> Option<Extensions<V>> {
    self.extensions.take()
  }
}

impl<V> fmt::Debug for RequestErrorResult<V> {
  /// Unbounded in `V`, for the reason [`TooLarge`]'s is.
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("RequestErrorResult")
      .field("errors", &[self.error])
      // `Extensions`'s own `Debug` renders keys without requiring `V: Debug`.
      .field("extensions", &self.extensions)
      .finish()
  }
}

#[cfg(test)]
mod tests;
