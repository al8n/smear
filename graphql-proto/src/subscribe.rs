//! Draft §6.2.3 *Subscription* and draft §7.1.2 *Response Stream*, as one phase of the executor.
//!
//! # There is no second machine here, and that is the whole design
//!
//! A subscription is not a different protocol from a query; it is the same execution run once per
//! event, under a phase that says when an event may arrive and what happens when the events stop.
//! So [`Executor`](crate::Executor) grows an **operation-kind phase** rather than gaining a
//! sibling: query, mutation and subscription are three values of one at-most-one slot, exactly as
//! a Raft endpoint's leader, follower and candidate are three values of one `Role` rather than
//! three endpoint types. The surface stays `handle_*` in and `poll_*` out; the phase decides what
//! it means.
//!
//! The alternative — a `Subscription` type that owns an `Executor` and hands the driver a fresh one
//! per event — was available and is wrong for a reason that is specific rather than stylistic: it
//! puts the **ordering seam in the driver's hands**. §6.2.3.2 requires one execution result per
//! source event, in source order, and a driver holding two machines is the party that would have to
//! keep that true. A phase inside one machine makes it structural: the intake refuses an event
//! while the previous event's result is still undelivered, so the conformant behaviour is the only
//! reachable one.
//!
//! # What the crate owns and what the driver owes
//!
//! The crate owns no stream, no clock and no resolver, so it cannot *be* the source stream. §6.2.3
//! splits cleanly along that line, and the split is the same one the rest of `proto` already makes
//! — structure and control flow here, representation and I/O there:
//!
//! | draft §6.2.3 step | whose |
//! |---|---|
//! | `CreateSourceEventStream` steps 1–4: the root subscription type, and §6.3 over the root selection set | the crate's — [`Executor::start`](crate::Executor::start) |
//! | step 5: "if `collectedFieldsMap` does not have exactly one entry, raise a *request error*" | the crate's — a [`StartError`](crate::StartError) |
//! | steps 6–8: the field, its name, and `CoerceArgumentValues` | the crate's — [`Executor::source_field`](crate::Executor::source_field) carries the result |
//! | step 9 `ResolveFieldEventStream`: call the resolver, get a stream | **the driver's** — it is a resolver call, and this crate never calls one |
//! | `MapSourceToResponseEvent`: observe the source stream | **the driver's** — the crate is told, through `handle_source_*` |
//! | `ExecuteSubscriptionEvent`: one `ExecuteRootSelectionSet` per event | the crate's — [`handle_source_event`](crate::Executor::handle_source_event) and the ordinary poll loop |
//! | §6.2.3.3 `Unsubscribe`: cancel the response stream, then the source stream | split — [`unsubscribe`](crate::Executor::unsubscribe) ends the response stream, and cancelling the source stream is the driver's, because it is the driver's stream |
//!
//! So what the driver owes, in one sentence each: call `ResolveFieldEventStream` with what
//! [`Executor::source_field`](crate::Executor::source_field) gives it and report the outcome; push
//! one event at a time and drain the response before pushing the next; say how the source stream
//! ended; and cancel that stream when [`response_stream`](crate::Executor::response_stream) reads
//! [`Cancelled`](ResponseStream::Cancelled).
//!
//! # Why the intake is serial, measured rather than assumed
//!
//! `graphql-js` 16.11.0's `subscribe()` is `createSourceEventStream` followed by
//! `mapAsyncIterator(stream, payload => execute({...args, rootValue: payload}))`, and
//! **`mapAsyncIterator` does not serialize**: `next()` is `mapResult(await iterator.next())` with no
//! queue, no in-flight flag and no re-entrancy guard, so a consumer calling `next()` twice without
//! awaiting the first gets two concurrent executions resolved in completion order. Upstream's
//! in-order behaviour is its consumer's `for await`, not its library's.
//!
//! A Sans-I/O machine has no `for await`. The driver polls, so nothing supplies the ordering for
//! free, and §6.2.3.2 does require the responses to be in source-event order. That is the one thing
//! this phase has to add, and it adds it the way draft §6.2.2's serial rule is already added — by
//! **withholding**, not by buffering. A machine that accepted events concurrently and restored
//! order on output would have to hold up to *k* completed responses, where *k* is a driver quantity
//! multiplied by the size of a response, which is the product shape [`Limits`](crate::Limits)
//! refuses to meet.
//!
//! The cost is stated rather than hidden: a driver cannot pipeline event *N+1*'s execution behind
//! *N*'s. Upstream permits that and no conformance test can see the difference, because §6.2.3.2
//! constrains the *response* order only. Pipelining would be a new capability with its own product
//! to price, not a defect in this one.
//!
//! # Nothing is retained per event
//!
//! A subscription is unbounded in time, so "what is held between events" is the question that
//! decides whether this is safe, and the answer is *the phase and two borrows of the document*.
//! Every source event begins with the same `reset` a second
//! [`start`](crate::Executor::start) performs: the response tree, the interner, the error rows, the
//! draft §7.1.7 map, the in-flight slab and the whole visit budget go back to where they were, and
//! every driver value the previous event held is dropped. The containers keep their capacity, which
//! the ceilings already bound, so a steady-state event allocates nothing and no ceiling accumulates
//! across a stream.
//!
//! That last clause is a correctness property and not an optimisation. [`Limits`](crate::Limits)'s
//! cumulative ceilings — positions, metadata, interned bytes and above all
//! [`max_selection_visits`](crate::Limits::max_selection_visits) — are **per operation**, and
//! §6.2.3.2 makes each event one whole `ExecuteRootSelectionSet`. Charging a stream cumulatively
//! would mean event *N* failing for work event 1 did, which is the shape a client clears by
//! reconnecting rather than a bound.
//!
//! **The one thing deliberately not hoisted** is draft §6.3's collection. The document, schema and
//! variables are fixed for the subscription's whole life, so the collected plan is loop-invariant
//! and could be built once; upstream rebuilds it per event and so does this. The reason is that the
//! saving is unmeasured and its cost is not: a retained plan is memory held across an unbounded
//! number of events, which is a *different* resource from the one it saves, and a hoist has to
//! price both before it is a win. Re-collecting also keeps every event's verdict a function of that
//! event alone.
//!
//! # §7.1.2 is a state, not a container
//!
//! §7.1.2 in full: "A GraphQL request returns a *response stream* when the GraphQL operation is a
//! subscription and the request included execution. A response stream must be a stream of
//! *execution result*." Two sentences, and neither describes a map — which is what makes this
//! result kind different from the other two. §7.1.1's execution result is
//! [`Response`](crate::Response) and §7.1.3's request error result is
//! [`RequestErrorResult`](crate::RequestErrorResult) because each *is* a map with a specified set
//! of entries. A response stream is a sequence over time, and a crate that owns no stream cannot
//! hold one.
//!
//! So [`ResponseStream`] is the stream's **state**, and the three things §7.1.2 says are each
//! structural rather than documented:
//!
//! - *"when the GraphQL operation is a subscription"* — this type is reachable only through
//!   [`Executor::response_stream`](crate::Executor::response_stream), which answers `None` for
//!   every other operation kind.
//! - *"a stream of execution result"* — each event's result comes out of
//!   [`poll_response`](crate::Executor::poll_response), whose type is
//!   [`Response`](crate::Response) and nothing else. A subscription event cannot produce a
//!   §7.1.3 request error result, because §6.2.3.2's own note closes it: request errors "only occur
//!   during `CreateSourceEventStream`", which here is [`start`](crate::Executor::start).
//! - *a stream ends* — the *event stream* definition requires it ("If an *event stream* encounters
//!   an error, it must complete with that error"; "When an *event stream* is cancelled, it must
//!   complete"), and §6.2.3.2 enumerates exactly which endings there are. They are the last three
//!   variants, and enumerating them once is deliberate: a phase enum that learns a termination per
//!   discovery is one that was written before the algorithm was read.
//!
//! # The one arm of §6.2.3.2 that is unreachable here
//!
//! `MapSourceToResponseEvent` has a fourth behaviour this type does not carry: "If internal
//! {error} was raised: Cancel {sourceStream}. Complete {responseStream} with {error}." Its own note
//! says what that means — "Since `ExecuteSubscriptionEvent()` handles all *execution error*, and
//! *request error* only occur during `CreateSourceEventStream()`, the only remaining error
//! condition handled from `ExecuteSubscriptionEvent()` are internal exceptional errors not
//! described by this specification."
//!
//! This executor raises no such error, and that is by construction rather than by care. Every
//! failure it can reach during an event is a draft §6.4.2 *field error* — an argument refusal, a
//! non-null null, a failed leaf coercion, an unresolvable abstract type, a ceiling — and every one
//! of them is recorded in that event's own [`Response`](crate::Response) and propagated by §6.4.4.
//! There is no path from an event to a value that is not an execution result, so the arm has
//! nothing to fire on and there is no variant for it. A driver whose *own* code fails mid-event
//! ends the stream through [`handle_source_error`](crate::Executor::handle_source_error), which is
//! the same completion by the same name.

use core::fmt;

/// Draft §7.1.2's *response stream*: the third of draft §7.1's three result kinds, as a state.
///
/// `subscribe.rs`'s module header has why this is a state rather than a container, what the driver
/// owes in each of them, and which arm of draft §6.2.3.2 is structurally unreachable.
///
/// Named as a file rather than linked, because the module is private — the type reaches a consumer
/// through the crate root's `pub use`, so rustdoc renders no page for the module and a `[…](self)`
/// link is a public item pointing at a private one. `extensions.rs` and `request_error.rs` refer to
/// their own headers the same way, for the same reason.
///
/// # Not `#[non_exhaustive]`, and the rule says why
///
/// The attribute belongs on vocabulary **smear owns**, not on vocabulary the **specification
/// enumerates** — the rule `7b9b293` recorded when it took the attribute off four AST types whose
/// variant sets are grammar productions. The last three variants here are draft §6.2.3.2's
/// `MapSourceToResponseEvent` arms, and the first two are `Subscribe`'s two steps; the set is the
/// algorithm's rather than this crate's. It is also the case where exhaustiveness costs most: a
/// driver serialising a stream's end has to write something for each ending, and a new one must
/// break that `match` rather than fall silently into a catch-all.
///
/// # Worked example
///
/// Draft §6.2.3's own chat subscription, driven end to end. The four steps are the four the module
/// header's table splits: `start` runs §6.2.3.1 and stops, the driver resolves the event stream and
/// says so, each event is one whole execution, and the source stream's ending is reported.
///
/// ```
/// use graphql_proto::{Executor, Leaf, Node, ResponseStream, Values};
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
/// # #[derive(Clone, Debug)]
/// # enum Json { Null, Str(String), Obj }
/// # struct Space;
/// # impl Values for Space {
/// #   type Value = Json;
/// #   fn is_null(&self, value: &Json) -> bool { matches!(value, Json::Null) }
/// #   fn as_bool(&self, _: &Json) -> Option<bool> { None }
/// #   fn list_len(&self, _: &Json) -> Option<usize> { None }
/// #   fn list_item(&mut self, _: &Json, _: usize) -> Json { Json::Null }
/// #   fn type_name<'a>(&'a self, _: &'a Json) -> Option<&'a str> { None }
/// #   fn coerce_leaf(&mut self, value: Json, _: Leaf<'_>) -> Option<Json> { Some(value) }
/// #   fn variable(&mut self, _: &str) -> Option<Json> { None }
/// # }
/// let schema = Schema::build(
///   &Parser::with_parser::<GraphqlLexer<'_, str>, TypeSystemDocument<&str>, GraphqlErrors<&str>, _, GraphQL>(
///     type_system_document,
///   )
///   .parse_str(
///     "type Query { a: String } \
///      type Subscription { newMessage(roomId: ID!): Message } \
///      type Message { sender: String }",
///   )
///   .expect("the SDL parses"),
/// )
/// .expect("the SDL is a schema");
///
/// let document = Parser::with_parser::<GraphqlLexer<'_, str>, ExecutableDocument<&str>, GraphqlErrors<&str>, _, GraphQL>(
///   executable_document,
/// )
/// .parse_str(r#"subscription NewMessages { newMessage(roomId: 123) { sender } }"#)
/// .expect("the subscription parses");
///
/// let mut space = Space;
/// let mut executor = Executor::new(&schema, &document);
///
/// // Draft §6.2.3.1 `CreateSourceEventStream`: the crate collects, counts and coerces.
/// executor
///   .start(&mut space, Some("NewMessages"), Json::Obj)
///   .expect("exactly one root field with an event stream");
/// assert_eq!(executor.response_stream(), Some(ResponseStream::Creating));
///
/// // Step 9 `ResolveFieldEventStream` is the driver's, because it is a resolver call.
/// let source = executor.source_field().expect("§6.2.3.1 chose a field");
/// assert_eq!(source.name(), "newMessage");
/// assert_eq!(source.arguments()[0].name(), "roomId");
/// // …driver subscribes to its own pub-sub topic here, with `source.parent_value()` as the
/// // `rootValue`…
/// assert!(executor.handle_source_stream());
/// assert_eq!(executor.response_stream(), Some(ResponseStream::Streaming));
///
/// // Draft §6.2.3.2: each source event is one whole execution, and one §7.1.1 execution result.
/// for sender in ["Hagrid", "Hermione"] {
///   executor
///     .handle_source_event(&mut space, Json::Obj)
///     .expect("the stream is open and the last result was taken");
///   while let Some(request) = executor.poll_resolve(&mut space) {
///     let id = request.id();
///     let value = match request.name() {
///       "sender" => Json::Str(sender.to_owned()),
///       _ => Json::Obj,
///     };
///     executor.handle_resolved(&mut space, id, value);
///   }
///   let response = executor.poll_response().expect("the event resolved");
///   let Node::Object(mut fields) = response.data() else { panic!("the root is an object") };
///   let (key, value) = fields.next().expect("one field was selected");
///   assert_eq!(key.to_string(), "newMessage");
///   assert!(matches!(value, Node::Object(_)));
/// }
///
/// // Draft §6.2.3.2's first completion arm, and no further event is taken.
/// assert!(executor.handle_source_complete());
/// assert_eq!(executor.response_stream(), Some(ResponseStream::Completed));
/// assert!(executor.handle_source_event(&mut space, Json::Obj).is_err());
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResponseStream {
  /// Draft §6.2.3.1 `CreateSourceEventStream` has chosen the source field and coerced its
  /// arguments, and the driver has not yet reported a source stream.
  ///
  /// The one state in which [`Executor::source_field`](crate::Executor::source_field) answers
  /// `Some`, and the one in which no execution result exists: `Subscribe` has not returned, so
  /// there is nothing for a response stream to have emitted.
  ///
  /// The driver's obligation here is step 9 — call `ResolveFieldEventStream` and then
  /// [`handle_source_stream`](crate::Executor::handle_source_stream). A driver whose resolver
  /// *fails* reports nothing and builds a [`RequestErrorResult`](crate::RequestErrorResult) of its
  /// own instead, which is where that failure belongs: it is the driver's error, and this crate
  /// carries no driver-supplied error entry (al8n/smear#126).
  Creating,
  /// Open: each source event the machine accepts produces exactly one draft §7.1.1 execution
  /// result.
  Streaming,
  /// Draft §6.2.3.2: "When {sourceStream} completes normally: Complete {responseStream} normally."
  Completed,
  /// Draft §6.2.3.2: "When {sourceStream} completes with {error}: Complete {responseStream} with
  /// {error}."
  ///
  /// The error itself is the **driver's** and stays there. This crate did not raise it, has no type
  /// for a driver-supplied error entry, and inventing one is al8n/smear#126's — the same boundary
  /// [`RequestErrorResult`](crate::RequestErrorResult) draws around the three request-error sources
  /// that arise outside this crate. What the state adds is that the ending is *recorded in one
  /// place*: a serialiser reads how the stream ended off the machine instead of off its own
  /// bookkeeping, and no further event can be accepted.
  Failed,
  /// Draft §6.2.3.3 `Unsubscribe`: the response stream was cancelled, so it "completes" normally
  /// and **the driver owes its source stream a cancellation**.
  ///
  /// That obligation is the whole reason this is a distinct variant rather than
  /// [`Completed`](ResponseStream::Completed). §6.2.3.2's cancellation arm is two steps — "Cancel
  /// {sourceStream}. Complete {responseStream} normally." — and only the second is this crate's,
  /// because the source stream is the driver's. A machine that answered `Completed` here would be
  /// telling the driver the stream ended on its own and quietly dropping the step that releases
  /// whatever the subscription was holding open, which §6.2.3.3 names as the point of the
  /// algorithm: "a good opportunity to clean up any other resources used by the subscription".
  Cancelled,
}

impl ResponseStream {
  /// Returns whether the response stream can still emit an execution result.
  ///
  /// True for [`Creating`](ResponseStream::Creating) and
  /// [`Streaming`](ResponseStream::Streaming), false for the three completions. This is the
  /// predicate the three completions are terminal *by*, so a driver never has to enumerate them to
  /// ask the one question it usually has.
  #[inline]
  pub const fn is_open(&self) -> bool {
    matches!(self, Self::Creating | Self::Streaming)
  }
}

impl fmt::Display for ResponseStream {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(match self {
      Self::Creating => "the source stream is being created",
      Self::Streaming => "the response stream is open",
      Self::Completed => "the response stream completed normally",
      Self::Failed => "the response stream completed with an error",
      Self::Cancelled => "the response stream was cancelled",
    })
  }
}

/// Why [`Executor::handle_source_event`](crate::Executor::handle_source_event) refused, carrying
/// the event back.
///
/// The value is returned in every variant rather than dropped, for the reason a refused
/// [`Extensions::insert`](crate::Extensions::insert) returns its value and a refused
/// [`set_extensions`](crate::Executor::set_extensions) returns its map: a source event is the
/// driver's own `V::Value` and may be a wasm or FFI handle, a pooled buffer or a database cursor,
/// and a refusal that closes one has done more than refuse.
///
/// Both variants are repairable from what the caller already holds, which is the other half of the
/// obligation. [`Outstanding`](SourceEventError::Outstanding) says *drain this event's response
/// first* — the driver has the poll loop that does it and can push the same value again.
/// [`NotStreaming`](SourceEventError::NotStreaming) says *there is no open response stream* —
/// [`response_stream`](crate::Executor::response_stream) says which of the four non-streaming
/// states it is in, so the driver can tell "§6.2.3.1 is not finished" from "the stream has ended"
/// without guessing.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum SourceEventError<V> {
  /// No response stream is open to emit an execution result on.
  ///
  /// Either the executor is not running a subscription at all, or draft §6.2.3.1 has not finished
  /// — [`handle_source_stream`](crate::Executor::handle_source_stream) has not been called — or the
  /// stream has already completed, failed or been cancelled.
  NotStreaming(V),
  /// The previous event's draft §7.1.1 execution result has not been taken yet.
  ///
  /// This is draft §6.2.3.2's ordering, enforced rather than documented: each source event maps to
  /// one execution result, and accepting this one now would discard the previous one. Drain the
  /// executor — [`poll_resolve`](crate::Executor::poll_resolve) until it withholds, then
  /// [`poll_response`](crate::Executor::poll_response) — and offer the same value again.
  Outstanding(V),
}

impl<V> SourceEventError<V> {
  /// Returns the event that was refused, giving up ownership.
  #[inline]
  pub fn into_value(self) -> V {
    match self {
      Self::NotStreaming(value) | Self::Outstanding(value) => value,
    }
  }
}

impl<V> fmt::Display for SourceEventError<V> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(match self {
      Self::NotStreaming(_) => {
        "no draft §7.1.2 response stream is open, so a source event accepted now could not be \
         mapped to an execution result"
      }
      Self::Outstanding(_) => {
        "the previous source event's execution result has not been taken, and draft §6.2.3.2 maps \
         each event to one result"
      }
    })
  }
}

#[cfg(feature = "std")]
impl<V: fmt::Debug> std::error::Error for SourceEventError<V> {}
