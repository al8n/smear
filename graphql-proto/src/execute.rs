//! Draft §6's execution core for all three of §6.2's operations, as one Sans-I/O state machine.
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
//! does not, and one that does not call it narrows its own in-flight ceiling — visibly, rather
//! than silently doing needless work.
//!
//! # Withholding is the only flow control
//!
//! There is no queue between the executor and the driver. Fields waiting to be resolved are slots
//! in the response tree, threaded on an intrusive chain; `poll_resolve` refuses to yield past
//! [`Limits::max_in_flight`] rather than reading ahead.
//!
//! Draft §6.2.2's serial-mutation rule is that same mechanism rather than a second one. A
//! mutation's root expands exactly as a query's does and enqueues every top-level field, and then
//! the chain is **cut** after its head: the fields behind it keep their links and simply are not on
//! it, and one is spliced back each time the previous field's subtree can no longer affect the
//! response. Nothing is allocated, nothing accumulates, and there is no ordering contract for a
//! driver to honour or forget — `poll_resolve` cannot offer what is not on the chain.
//!
//! # What the release actually waits for, stated to its edge
//!
//! Two readings had to be settled and both were settled the same way, by measuring `graphql-js`
//! 16.11.0 rather than arguing from its prose.
//!
//! **It waits for the previous field's subtree, not for its answer.** The readings differ on a
//! nested resolver: upstream's `executeFieldsSerially` awaits `executeField`, whose promise is
//! `completeValue`'s and therefore resolves only once every sub-field has. Run against
//! `mutation { a: m { nested } b: m { nested } }`, upstream calls the resolvers in the order `a`,
//! `nested(a)`, `b`, `nested(b)` — so a rule that released on the answer alone would start `b`'s
//! side effect while `a`'s sub-selection was still running, which is what §6.2.2 exists to forbid.
//!
//! **It does not wait for work draft §6.4.4 has already discarded.** When a nested non-null under
//! `a` fails, §6.4.4 nulls `a` and every request still outstanding beneath it is *abandoned* — see
//! [`poll_abandoned`](Executor::poll_abandoned) — and the release steps over those rather than
//! waiting for them. So the guarantee is **"the previous field's subtree is complete or
//! cancelled"**, and not "complete": an abandoned request under `a` may still be in the in-flight
//! table when `b`'s resolver is offered. Upstream does the same and was measured doing it, on
//! `mutation { first { outstanding failing } second { outstanding } }` with `first.outstanding`
//! held unsettled by hand — `second`'s resolver runs before it settles, because `promiseForObject`
//! is `Promise.all` and a rejected promise chain in JS does not cancel its siblings. Waiting
//! instead would be stricter than the reference implementation *and* would put the next mutation
//! field behind work the driver has just been told to stop doing: retiring the abandoned entries
//! or answering them are the only two things that clear the count, at every ceiling and for every
//! driver.
//!
//! Not waiting is not free either, and the cost is exactly one thing: an abandoned request holds
//! its in-flight slot until the driver retires *or answers* it, so a driver that does neither runs
//! under a ceiling narrowed by however many it has left there. That ceiling never reaches zero —
//! see [`Limits::max_in_flight`] for the bound — so the cost is concurrency and not progress.
//!
//! See [`Executor::release_serial`], and, for the measurements as tests,
//! `an_abandoned_nested_request_does_not_withhold_the_next_mutation_field`,
//! `an_undrained_abandonment_narrows_the_ceiling_without_stopping_the_mutation` and
//! `an_abandonment_can_never_take_the_last_in_flight_slot`.

use core::{fmt, num::NonZeroU32};

use tokora::{SimpleSpan, span::AsSpan};

use smear_parser::graphql::ast::{
  ExecutableDefinition, ExecutableDocument, Field, InputValue, OperationDefinition, OperationType,
  SelectionSet,
};
use smear_schema::{PackedType, RootOperation, Schema, Sym, TypeId, TypeKind, builtin};

use super::{
  Argument, ArgumentSource, Error, Extensions, FieldRequest, Leaf, Node, ReqId, ResponseStream,
  SourceEventError, SourceField, Values,
  collect::{
    Allowance, Fragments, Interner, Scratch, Unstored, Visits, collect_fields, variable_key,
  },
  error::{ConditionFault, Raw, Row},
  request::name_bytes,
  response::{Key, NONE, Slot, State, node},
};

#[cfg(test)]
mod tests;

/// What the executor will not do, whatever the document or the driver asks.
///
/// # One knob per resource, derived rather than accumulated
///
/// Three review rounds added one ceiling each, and each was found to be the wrong unit by the
/// next. The cause was singular and is worth stating plainly: **checks were placed on allocation
/// sites, and several of the resources are not allocation sites.** What follows is the enumeration
/// that replaced that — resource by resource, each with the path that would spend it *without*
/// passing its check, which is the column that makes a row falsifiable rather than reassuring.
///
/// | resource | unit | ceiling | charged where | what would spend it unchecked |
/// |---|---|---|---|---|
/// | outstanding work | requests | [`max_in_flight`](Limits::max_in_flight) | `poll_resolve` withholds, and the slab reuses freed entries | — |
/// | response positions | positions | [`max_response_slots`](Limits::max_response_slots) | `push_child`, the only creator | — |
/// | response metadata | merged selections **and** location spans, counted as one | [`max_response_metadata`](Limits::max_response_metadata) | `expand` before appending, and `fail_at` | a writer appending spans without charging — which `fail_at` did, making the ceiling neither bound it claimed |
/// | collection work | selections **examined**, name-table entries **compared**, and the fragment index's pass, as one | [`max_selection_visits`](Limits::max_selection_visits) | `walk` per selection; `Interner::intern` and `Fragments::get` per entry compared, before comparing it; `Fragments::build` per definition and fragment | charging appends instead of visits; charging visits without their lookups, which is a budget on one factor of a product; and leaving *any* uncharged way into a name table, which a claim about callers cannot prevent and a signature can |
/// | collection depth | — | **removed, not bounded** | `walk` runs on an explicit stack | a recursive walk, which a flat fragment chain drove to `SIGABRT` |
/// | interned text | bytes | [`max_interned_bytes`](Limits::max_interned_bytes) | `Interner::intern` | driver messages, one per failed position, at any length |
/// | draft §7.1.7 `extensions` | entries **and** key bytes, as two ceilings | [`max_extension_entries`](Limits::max_extension_entries), [`max_extension_key_bytes`](Limits::max_extension_key_bytes) | `Extensions::insert`, before the key is boxed; and **each of the two retainers** — [`set_extensions`](Executor::set_extensions) and [`RequestErrorResult::set_extensions`](super::RequestErrorResult::set_extensions), both of which re-derive an accepted map's spine from its entries | bounding either alone: one ceiling leaves a single gigabyte key, the other leaves unbounded empty-keyed entries, because a zero-length key is a legal key. And bounding both while retaining the caller's allocation: `remove` refunds the budget and returns no slot, so an emptied map reports nothing and holds everything it grew to. And a **second** retainer of the same container that re-checks only the two reported quantities, since §7.1.7 admits the entry in a §7.1.3 result too |
/// | error rows | rows | derived: at most one per position | — | a position able to fail twice, or an argument coercion raising more than once |
/// | list nesting | — | [`MAX_WRAPPERS`](smear_schema::MAX_WRAPPERS) = 15, by the schema | `complete`'s list arm strips one wrapper per level | — |
///
/// Two rows are deliberately not ceilings. **Depth was removed rather than bounded**, because a
/// depth ceiling's right value is a function of the deployment's stack and a heap stack has no such
/// question. **Error rows are derived**, and the derivation is the falsifier: `coerce_arguments`
/// returns after its first `fail_at` and a failed position is discarded, so one position
/// contributes one row.
///
/// **The fourth row's unit is the second time this table met the product shape, and this one came
/// from the query on both sides.** Charging one unit per selection said nothing about what
/// examining a selection *costs*, and it cost the document three times over: `Interner::intern`
/// scanned every interned name, the group lookup scanned every group collected so far, and a
/// fragment spread scanned every definition. They grew together — one group per newly interned key
/// — so `n` selections over `n` names was `n²` of work under a ceiling that read `n`, and **fixing
/// one of the three would have changed nothing**, because the other two still ran once per
/// selection. Measured before: 8,000 distinct response keys spent 61 ms inside `start()`, and a
/// 50,000-link fragment chain 2.1 s, all of it before the first request a driver could refuse.
///
/// The group is now a direct index by interner id, and the other two are hash probes whose
/// comparisons are charged — so the ceiling bounds the work and not the trip count. Two residuals
/// are recorded on `collect::Visits` rather than argued away here: a lookup is charged after it
/// answers, and `expand`'s `__typename` and `handle_field_error`'s message intern without charging
/// this budget at all.
///
/// # The other half of the table: what is held, who can still read it, when it dies
///
/// The table above is a *spend* model — what is charged, and what would spend it unchecked. Four
/// adversarial rounds each found one thing it does not say, and they were not four accidents: they
/// were the four missing columns of one artifact. A resource has a bound, an acquisition point,
/// **a reader set**, a death point, and a release on each fate, and a model written from the spend
/// side alone can only see the first two. R5 was "the charge is not released when the work fails";
/// R6 was "the value is not released when the work *succeeds*" — the same column, twice.
///
/// So the lifetimes are written down too. The last column is this table's falsifier, as "what would
/// spend it unchecked" is the other's.
///
/// | member | bound | readers | dead at | released by | what would strand it |
/// |---|---|---|---|---|---|
/// | argument values | one field's arguments | the one offered `FieldRequest` | next entry point | `on_entry`, incl. the withhold exit | an entry point that skips retirement |
/// | **object values** (root included) | object positions | `poll_resolve`'s `parent_value`, once per enqueued child | last enqueued child leaves Ready | the `Object` → `Expanded` transition; `discard`; `reset` | an enqueue or departure path that skips the count |
/// | leaf values | positions | `Response` readers, repeatedly | next `start`/drop | `reset`/drop; `discard` | nothing — they *are* the response |
/// | `TypeName` ids | interner ceiling | `Response` readers | next `start`/drop | `reset` | nothing: no `V`, no handle |
/// | slots, metadata, interned text | their ceilings | tree walks, `Response` | next `start`/drop | `reset`; suffix-`restore` only | a creator that bypasses the sole creator |
/// | error rows | one per position | `Response::errors` | next `start`/drop | `reset` | a position able to fail twice |
/// | draft §7.1.7 `extensions` | its two ceilings, per the row above — **not** the `Option`, which bounds the number of maps and not a map's size, and **not** the entries alone, which bound what the map reports and not the spine it holds | `Response::extensions` | next `start`/drop | `reset`; `take_extensions` | a `reset` that empties the tables and forgets the held `V`s that are in none of them; and a map retained with the capacity a *different* `Limits` let it grow to |
/// | in-flight entries | `max_in_flight` | `release` by id | answered, retired, or reset | slab free chain; epoch voids stale ids | an id honoured across epochs |
/// | visit budget | `max_selection_visits` | `walk` | end of operation | **never** — work done is spent | a refund |
/// | collection scratch | `max_response_metadata`, charged before each push | `expand`, this call only | cleared at the next collection | cleared, never shrunk — capacity is reused | a staged population charged against a *different* ceiling than the one that refuses it |
/// | withheld top-level fields (draft §6.2.2) | the root's own children, so `max_response_slots` | `release_serial`, one splice each | the last splice, or the root's discard | `reset`; a discarded root retires the cursor at the next release | a `restore` truncating over the cursor — asserted, because the argument for it is about creation order and a restore is about indices |
/// | **draft §6.2.3.1's `initialValue`** | one, the root position | [`source_field`](Executor::source_field)'s `parent_value`, any number of times while the stream is `Creating` | the transition out of `Creating` | `reset`, in [`handle_source_stream`](Executor::handle_source_stream), [`unsubscribe`](Executor::unsubscribe) or the next `start` | a `Creating` state with a transition out that does not reset — a subscription is unbounded in time, so a value held from there is held for the client's whole connection |
/// | **the source field's coerced arguments** | one field's arguments | `source_field`'s `arguments`, the same way | the same transition | the same `reset` | `on_entry` retiring them at a *no-op* call in, which would silently hand the driver a source field with no arguments. That is why `on_entry` does not, while `Creating` |
/// | **a subscription's operation** — its root selection set and root type | two `Copy` items: a borrow into the document, and a `TypeId` | [`handle_source_event`](Executor::handle_source_event), once per event | the next `start`, or a drop | overwritten by the phase | nothing: it holds no `V`, allocates nothing, and is the only thing draft §6.2.3 keeps across an event |
///
/// **Draft §6.2.3's per-event populations get no row, and their absence is the answer.** A source
/// event is one whole execution, so every row above is its row — and
/// [`handle_source_event`](Executor::handle_source_event) begins with the same `reset` a second
/// [`start`](Executor::start) performs, which is each row's own release. So a subscription
/// retains, between events, exactly the three rows above and nothing else, and every cumulative
/// ceiling is re-armed. The bound this needs and a query does not is that **no ceiling accumulates
/// across an unbounded stream**, and it is discharged by there being no per-event retention at all
/// rather than by a fourth knob. `each_source_event_charges_what_the_first_did` is the gate, and it
/// is `a_second_operation_charges_what_the_first_did` over a stream, reading the same four
/// quantities.
///
/// One event is the exception, and it is the same exception a query already is. When §6.2.3.2's
/// source stream ends while an event is running, the ending is *recorded* rather than performed —
/// see `Stage` — so that event's populations survive the terminator and die where a query's do, at
/// the next `start` or drop, with `poll_response` reading them in between. Nothing new is retained:
/// the recorded ending is one `Copy` discriminant, and the populations are the ones the row above
/// already covers, kept for the one reader they exist for.
///
/// # Every buffer that survives a `reset`, and what bounds each one
///
/// **The paragraph above is true of *entries* and was published as if it were true of bytes.** It
/// said the containers keep capacities "which the ceilings already bound" — and one of them is not
/// bounded by a ceiling at all. `reset` clears without shrinking on purpose, so the two halves come
/// apart exactly where rule 5 says they do: `each_source_event_charges_what_the_first_did` reads
/// four *spends*, all of which return to zero, and a buffer growing without end would leave every
/// one of them identical. A subscription is unbounded in time, so a census of the *capacities* is
/// the artifact this needed and did not have.
///
/// So here it is, over every buffer an executor carries across a `reset`. The third column is the
/// falsifier — rule 5 asks it of each row, and a row whose growth site is charged by something that
/// is neither a ceiling nor a fixed input is unbounded here.
///
/// | buffer | grown at | bounded by |
/// |---|---|---|
/// | `slots`, `meta` | `push_child`, the sole creator of a position | [`max_response_slots`](Limits::max_response_slots) |
/// | `merged`, `locations` | `expand`'s commit, and `fail_at` for a lone span | [`max_response_metadata`](Limits::max_response_metadata) |
/// | `interner` — arena, spans, chain, buckets | `Interner::insert` | [`max_interned_bytes`](Limits::max_interned_bytes): a GraphQL name is at least one byte, so entries cannot outrun the arena, and the bucket table is a power of two at a load factor of a half |
/// | `errors` | `fail`, `fail_at` | derived: at most one row per position, so `max_response_slots` |
/// | `inflight` | `poll_resolve`, which withholds at the ceiling and reuses freed entries | [`max_in_flight`](Limits::max_in_flight) |
/// | `scratch.fields`, `scratch.groups` | `walk`, charged **before** each push against the ceiling the staging is *for* — rule 3 | `max_response_metadata` |
/// | `scratch.keys` | `walk`, grown one entry at a time to reach an interner id | `max_interned_bytes`, since only an admitted insertion mints an id |
/// | `scratch.visited` | `walk`, one bit per fragment ordinal | **the document's** fragment count; every spread that reaches it is charged a visit |
/// | `scratch.stack` | `walk`, one frame per spread or inline fragment | `max_selection_visits`, charged before the frame is taken |
/// | `scratch_sets` | `complete`'s object arm, one entry per merged selection carrying a sub-selection | `max_response_metadata`, being a slice of `merged` |
/// | `fragments` — the index | `Fragments::build`, once, kept across resets on purpose | **the document's** definition count, and the pass is charged against `max_selection_visits` before the storage exists |
/// | **`scratch_args`** | `coerce_arguments`, which **charges nothing** | **the schema's** widest declared argument list: the function clears and then pushes at most one entry per argument the schema declares on the *one* field being coerced |
///
/// **`scratch_args` is the row this section exists for, and it is the third kind of bound rather
/// than a missing one.** Two rows above are bounded by the document and one by the schema, and a
/// schema-derived bound is already in the spend table — list nesting is `MAX_WRAPPERS`, "by the
/// schema". What separates a bound from an absence is not which artifact it comes from, it is
/// **whether the quantity is one an adversary can move**: positions and list lengths are the
/// driver's, selections and fragments are the document's and therefore the client's, and a field's
/// declared argument list is the *deployment's*. No request and no event changes it, and no number
/// of events accumulates it, because `coerce_arguments` opens with `retire_arguments`.
///
/// It is worth stating what a ceiling would and would not buy, because the answer is "less than it
/// looks". `max_field_arguments` would bound `scratch_args` at a number the operator picked instead
/// of at a number the operator's own SDL already fixes, and it would have to refuse a *valid* field
/// mid-coercion to do it. It is a real option and a breaking one — [`Limits`] is not
/// `#[non_exhaustive]`, and `7b9b293` took that attribute off four types deliberately — so it is
/// recorded here as the owner's call and not taken.
///
/// `no_buffer_a_subscription_retains_grows_with_the_stream` is the gate, and it is this table
/// mechanised: every row bounded, and the whole census identical at event 2 and event 20.
///
/// Five rules keep the table true as phases 2–8 add members, and each is a rule the rounds paid
/// for. (This said "three" over four of them, uncaught since the extraction — the smallest possible
/// instance of the thing the section it introduces is about.)
///
/// 1. **Release is a state transition on the owner, never a call at a site.** Any state holding a
///    `V` must name its transitions out, and every one of them drops the value. For
///    `State::Object` they are exactly: last read reached → `Expanded`,
///    discarded → `Null`, and `reset`. This is why there is no `release_object` to forget, and why
///    a stale count is unrepresentable — the count lives inside the variant the transition
///    overwrites.
/// 2. **The reader column is the falsifier.** A member whose reader set is empty *while it is held*
///    is a defect by inspection. That one sentence is R6, and it is the sweep a later phase runs:
///    enumerate the states holding a `V` or an id, and for each, name the reader or name the defect.
///
///    **A reader is an accessor or a lend, and never an interpreter** — every entry in that column
///    is one, and the row for draft §7.1.7's map says `Response::extensions` precisely because
///    nothing in this crate reads a single value in it. Taking "reader" to mean "something here
///    reads it" would condemn that map, which is shipped and correct. The rule therefore decides
///    whether a member that exists is *released* on time; it does not decide whether the member
///    should exist, and a proposal answered with "nothing reads it" has not yet met this rule. The
///    question that does decide it is whether the specification names a **destination** the crate
///    owns — §7.1.7's map has one and the §6 preamble's request `extensions` has none, which is
///    the crate root's `Scope` section and the one case where the two questions came apart.
/// 3. **A staging buffer belongs to the population it is staging for.** The collection scratch is
///    response metadata in waiting — every selection it holds becomes two metadata entries on
///    commit — so it charges the metadata ceiling, at the push, and not the visit budget it happens
///    to sit under. Getting this wrong does not look like a missing bound, because there *was* one:
///    it looks like a bound that is orders of magnitude looser than the one that decides the
///    outcome, so the buffer outgrows the refusal that was coming. Charge against the ceiling that
///    will decide, not the one the code path is nearest to.
///
///    Moving a refusal earlier is only safe if the earlier site reports what the later one would,
///    and that is a claim about *every* observable, not the ones a suite happens to exercise. This
///    one shipped reporting the field **name's** span where the commit reports the **field's**,
///    which differ only when the selection is aliased — `q: b` against `b` — so eighty-nine passing
///    cases said nothing, none of them having an alias at the crossing selection. Both collection
///    refusals now use `field.span()`, and the pair of alias cases is the evidence. **When two paths
///    are claimed observably identical, the evidence has to include an input on which they could
///    have differed.**
/// 4. **Attach death to the resource, not to a code path.** The root is stored by `start` and not by
///    `complete`, and an object whose selection is only `__typename` enqueues nothing at all — so a
///    release hung off "the completion path" misses the first and a release hung off "the last
///    offer" misses the second. Hanging it off the state catches both by construction.
/// 5. **A ceiling bounds a retained buffer only if every growth of that buffer passed through it.**
///    The `bound` column above names a ceiling, and a ceiling on a *reported* quantity is not
///    automatically a bound on the allocation behind it — a container that refunds its budget on
///    removal returns no capacity to the allocator, so the two diverge and only the reported half
///    is checked. Whether that gap is reachable is decided by one question, and it is the
///    falsifier: **for each retained buffer, name the site that grows it and what bounds that site
///    — and if the answer is neither one of this executor's ceilings nor a fixed input, the buffer
///    is unbounded here.** Slots, metadata, interned text and the error rows are grown only by
///    sites this executor charges, so `reset`'s deliberate never-shrink keeps a capacity that is
///    still bounded by the ceilings — which is what makes a steady-state response allocate nothing.
///    Draft §7.1.7's map was the one member grown somewhere else: `Extensions` carries the ceilings
///    a *caller* chose, `Extensions::insert` charges those, and the executor then retained the
///    result under its own. `set_extensions` re-derives the spine on acceptance, and that is the
///    whole difference.
///
///    **The clause "or a fixed input" is a repair, and it was paid for the same way the rest of
///    this table was.** The rule shipped saying a growth site not charged by this executor's own
///    ceiling makes the buffer unbounded, ran cleanly over the members that existed, and gave the
///    wrong verdict on the very next one: `coerce_arguments` charges nothing and `scratch_args` is
///    nonetheless bounded, by the schema, which no request can move. Three of the twelve rows in
///    the census above are like that. The question the rule is *for* is whether an adversary can
///    make the buffer grow, and "which ceiling charges it" is only a proxy for that — a good proxy,
///    which is why it stands first, and a proxy that has to name its other answer or it starts
///    rejecting sound rows and, worse, teaches the next reader to add a knob instead of a bound.
///    **A rule that has met one counterexample has probably met the class**: run it over every
///    member, not over the one that prompted it.
///
/// # Every event that reaches a death point
///
/// The table above says how each member is released; this says which events actually do it. **Ask
/// it in that form, and not as "every path that can fail".** The narrower question was asked once
/// and it missed a whole quadrant: an answer arriving for a request draft §6.4.4 had already
/// discarded does not *fail* — it succeeds, on something that is gone — and it was holding the
/// in-flight ceiling because nothing retired it. Release-on-success is the same quadrant that
/// produced the object-value finding, in a different resource, and a failure-shaped sweep cannot
/// see either.
///
/// So for each member: what are **all** the events that end its life — an answer, a withhold, a
/// retirement, a reset, a discard, a refusal, and an event that would have been an answer if its
/// subject still existed — and does each one release?
///
/// | failure | allocated before it | restores |
/// |---|---|---|
/// | §6.4.3 step 1 non-null null, step 3 not-a-list, step 4 leaf coercion, step 5 unresolved abstract | nothing in this call | n/a |
/// | step 5 impossible abstract | the runtime name, if it could be interned | no, and correctly: the error's own message reads it |
/// | **list element over the position ceiling** | element slots and metadata, plus every subtree those elements completed | **yes** — the fix this note is attached to |
/// | `expand` collection fault | interned response keys | yes |
/// | `expand` exhausted | committed groups, their slots and metadata | yes |
/// | §6.4.1 argument errors | earlier checked arguments; the variable's name; the error's own span | the arguments, at the next entry point; the name and span are the error's and stay |
/// | `poll_resolve` withhold | a coerced argument set | yes, `retire_arguments` at the exit |
/// | **an answer for an already-abandoned request** | the in-flight entry, still counted against the ceiling | **yes** — it retires like any other answer, and the value is still dropped |
///
/// One path deliberately does **not** restore, and it is worth stating so it is not read as a
/// missing row: when draft §6.4.4 nulls a list from one of its own elements, the elements already
/// built keep their charge. That is not a refusal — the executor did not decline to represent
/// anything, it built a subtree and the response then said null about it — and the ceilings bound
/// cumulative allocation by an argument this design froze. The line is *refusals restore, outcomes
/// do not*, and it is the line row 5's release column already draws by naming reset and
/// suffix-restore as the only two releases.
///
/// The inverse is checked too, because an over-eager restore is the worse defect and no
/// release-only test can see it: a restore must not free something still read. Two things could
/// point into a truncated region — an error row, whose slot index the mark now covers, and the
/// deferral cell, which every entry point settles before any of this runs and which `restore`
/// asserts is empty.
///
/// What none of this bounds is *peak*: draft §6.4.3 completes a list synchronously, so one
/// `handle_resolved` can still momentarily own an element value per element with none of their
/// children offered yet. That set now drains as the driver makes progress instead of persisting to
/// the next `start`, and bounding the peak itself means making list completion resumable, which is
/// backpressure by another name and belongs to the phase that adds it.
///
/// # A budget on one factor of a product bounds nothing
///
/// The rule the third knob exists for, and the one a later phase should check itself against
/// before claiming a resource is bounded. `max_response_slots` bounds positions. It does not bound
/// **memory**, because the executor's cost is `positions × per-position bookkeeping`, and the
/// second factor is the *document's*: draft §6.3 merges every selection sharing a response key
/// into one field, and `{ rows { x x x … } }` gives one position a group of as many selections as
/// the query cares to write. Each is bounded on its own — the driver's list length by the position
/// ceiling, the query's duplicate selections by the document's size — and **neither bound reaches
/// their product**, so a list of `N` elements over a group of `G` selections costs `N × G` while
/// consuming `N` of the position budget.
///
/// The shape recurs wherever a *driver* quantity multiplies a *query* quantity, which is exactly
/// what draft §6.2.3's per-event collection and draft §6.2.2's mutation path both do. The remedy is
/// not a cleverer bound on either factor; it is a ceiling on the product itself, which is what
/// this knob is.
///
/// **The mutation path was one of the two predictions, and it came out the other way.** The product
/// is there to be built: §6.2.2's serial rule has to decide, once per offer, whether the previous
/// top-level field is done with and which field comes next, and *offers* are the driver's while
/// *top-level fields* and the subtree's shape are the document's. Scanning the root's children, or
/// walking the running subtree, makes that `offers × M` — with `M` bounded by this ceiling, offers
/// bounded by the position ceiling, and their product bounded by neither, which is this section
/// verbatim. What it does **not** call for is a fourth knob. The cost is in a mechanism rather than
/// in a population, so withholding deletes it instead: the withheld fields are the ready chain's
/// own tail, so "which is next" is one link, and nothing that can still affect the response is on
/// the chain or counted `live` outside the running subtree, so "is it done with" is two counters.
/// `M` fields cost `M` steps over the whole operation, whatever the driver answers. See
/// `Executor::serial_next`, and `a_serial_release_does_not_grow_with_the_response` for the gate
/// that would fail if a scan came back.
///
/// **The subscription path was the other prediction, and it came out the same way for a different
/// reason.** Draft §6.2.3.2 runs draft §6.3's collection once per source event, so the product is
/// `events × collection` with *events* the driver's and *collection* the document's — and events
/// are bounded by nothing at all, a stream being unbounded in time. That is worse than the shape
/// this section names, and it needs no knob either, because the second factor is not *retained*:
/// [`handle_source_event`](Executor::handle_source_event) resets before it collects, so an event's
/// collection is charged against a budget re-armed for that event and released with it. The
/// product is over time rather than over memory, and a ceiling on cumulative work across a
/// subscription would be a bound a client clears by reconnecting.
///
/// Hoisting the collection out of the loop — the plan is loop-invariant, the document and schema
/// being fixed for the subscription's life — is the optimisation this deliberately does not take.
/// It trades work per event for memory held across an unbounded number of them, which is a
/// *different* resource, and an unmeasured saving does not buy a new retention. See
/// `subscribe.rs`'s header, and `each_source_event_charges_what_the_first_did` for the gate on the
/// half that is claimed.
///
/// The second clause is about what can still *affect the response* and not about what is
/// outstanding, and the two come apart on the error path: draft §6.4.4 moves a discarded subtree's
/// requests from `live` to `abandoned` rather than ending them, so after a release the in-flight
/// table can hold entries belonging to an earlier top-level field. The stronger sentence does not
/// hold there and must not be restored. Excluding those entries from the release's question is
/// deliberate — see `Executor::release_serial` — and it is what keeps the answer two counters
/// instead of a walk.
///
/// Sharing the group between the elements of one list was considered and is **not** a substitute.
/// The collection is not identical per element: `applies` resolves a fragment's type condition
/// against the element's own concrete type, so an abstract element type collects different fields
/// per element, and `@skip`/`@include` are re-read through
/// [`Values::variable`](super::Values::variable), which takes `&mut self` and may legitimately
/// answer differently per position. An attacker picks the varying case, so sharing removes waste in
/// the common case and bounds nothing in the adversarial one.
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
/// Selection *depth* appears nowhere above because it stopped being a resource: draft §6.3's walk
/// runs on an explicit stack, so descending spends heap the visit budget already counts instead of
/// native frames nothing counted.
///
/// It is enforced inside the one function that creates a position, so a phase that adds a new way
/// to create one inherits the bound rather than having to remember it — the design's standing rule
/// that a wrong answer should be unrepresentable rather than guarded at each call.
///
/// # Why every ceiling is a [`NonZeroU32`]
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
/// use graphql_proto::Limits;
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
/// use graphql_proto::Limits;
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
  /// It bounds outstanding *requests* and deliberately not retained *values*: a completed object's
  /// value is not in this slab, and what bounds its lifetime is the lifetime table's row 2, not this
  /// knob. Before that row was enforced the two were confused, and a pool of eight could be facing
  /// a hundred thousand live handles.
  ///
  /// Requests [`poll_abandoned`](Executor::poll_abandoned) has not yet retired count against it,
  /// so a driver that ignores that channel narrows its own ceiling rather than accumulating work
  /// nobody wants. **It cannot close it.** A discard promotes to abandoned only the requests that
  /// were live *besides* the one that caused it, and that one has always been released by the time
  /// it can fail — through [`handle_resolved`](Executor::handle_resolved), through
  /// [`handle_field_error`](Executor::handle_field_error), or, for a draft §6.4.1 argument error,
  /// through the gate in [`poll_resolve`](Executor::poll_resolve) that had already refused to hand
  /// it out at the ceiling. So undrained entries are bounded by this value *minus one*, and a slot
  /// is free again as soon as the live work drains.
  ///
  /// Three paths hold that bound for three different reasons, and the last of them is an ordering
  /// rather than a release — so the bound is checked where the count is raised and not only argued
  /// here: `propagate` asserts it, in debug, on every discard.
  ///
  /// The worst an ignored channel can therefore do is take the driver down to one request at a
  /// time; it never takes it to none, and it can be cleared whenever the driver likes, because the
  /// channel is returning `Some`. A ceiling of zero would admit nothing at all and no call could
  /// clear it, which is the difference, and why this one cannot be zero.
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
  /// It also makes the two *position* narrowings exact: a slot index and a list index are both
  /// `u32`, and `u32::MAX` is the tree's "no such slot" sentinel, so because a position is only
  /// created while the count is strictly below this ceiling, the largest index reachable is
  /// `max_response_slots - 1` — never the sentinel and never truncated.
  ///
  /// **Only those two, and the module has more.** A blanket claim about every `as u32` here would
  /// be false, so the sites are enumerated instead and each is answered by name:
  ///
  /// | site | what bounds it |
  /// |---|---|
  /// | the slot index in `push_child` | this ceiling |
  /// | a list element's `Key::Index` | this ceiling, via a `try_from` on the same decision |
  /// | the two metadata range starts in `expand` | [`max_response_metadata`](Limits::max_response_metadata), via a `try_from` on the same decision |
  /// | the in-flight slab index | [`max_in_flight`](Limits::max_in_flight) |
  /// | `fail_at`'s location index | [`max_response_metadata`](Limits::max_response_metadata), which `fail_at` now charges, plus at most one for the root's fallback — and `try_from`-checked besides |
  /// | the interner's arena offset, entry length and id | [`max_interned_bytes`](Limits::max_interned_bytes), and each `try_from`-checked independently of it |
  ///
  /// The row this table used to end on is gone rather than answered: draft §6.1's lookup produced
  /// an operation *index* whose only bound was the document's own length, and
  /// `Executor::operation_definition` now returns the definition itself. There is no narrowing
  /// left to bound.
  pub max_response_slots: NonZeroU32,

  /// How many merged field selections the executor will record across the whole response.
  ///
  /// Draft §6.3 collects every selection sharing a response key into one field, and the group has
  /// to survive until that field's value comes back so draft §6.4's `MergeSelectionSets` can read
  /// it. One position therefore costs one entry *per selection in its group*, not one entry — and
  /// that count is the document's, while the number of positions is the driver's. This ceiling is
  /// on their product, for the reason the type's own documentation gives at length.
  ///
  /// Reaching it is the same field error as reaching the position ceiling
  /// ([`Kind::ResponseBudget`](super::Kind::ResponseBudget)) and for the same reasons: it is raised
  /// once execution is under way, so §7.1.2 leaves no other shape, and it names the object whose
  /// selection set could not be recorded rather than diagnosing what made the response expensive.
  ///
  /// The check happens **before** anything is appended, so a refused expansion leaves the metadata
  /// vectors exactly as it found them. A budget that appended first and refused afterwards would
  /// leak on every near-miss, which under a client that retries is the same denial of service
  /// wearing a slower coat.
  ///
  /// The default is four times the position ceiling rather than equal to it: a group of one is the
  /// ordinary case, so this is only reached by a document that merges heavily, and a response of
  /// legitimately merged selections should not be refused for being tidy.
  pub max_response_metadata: NonZeroU32,

  /// How much work draft §6.3's collection may do, across the whole operation.
  ///
  /// Charged per selection *looked at*, surviving or not; per entry a name lookup compares, before
  /// it compares it; per definition and fragment the fragment index's one pass handles; and per
  /// definition draft §6.1's operation lookup reads. The first is the difference between this and
  /// every other ceiling here, and it is deliberate: the others count what was produced, and a
  /// document built out of fragments that collect nothing produces nothing while walking as far as
  /// it likes.
  ///
  /// **The last of those is the one charge taken outside collection, and it is why this ceiling
  /// can refuse a `start` before a response exists.** Draft §6.1 walks the definitions to find the
  /// operation, once per [`start`](Executor::start), over a count the document chooses; it was the
  /// module's last uncharged walk over a client quantity (al8n/smear#144) and it now spends this
  /// budget like the rest. The refusal is
  /// [`StartError::OperationLookupRefused`](StartError::OperationLookupRefused) rather than a
  /// field error, because §7.1.2 has no response for a failure raised before execution begins.
  ///
  /// The rest is what makes the first a bound rather than a bound on one factor. Both tables hash
  /// text the *document* chose, and the shared hash is unkeyed and invertible, so a client can put
  /// every one of its names in one bucket deliberately. Charging each comparison first means it
  /// spends this ceiling doing so, and the guarantee does not rest on the hash behaving.
  ///
  /// **What a lookup costs an honest document is the hash's business, and it was asserted before it
  /// was measured (al8n/smear#172).** "About one entry, so roughly twice what it used to be" was
  /// true of `k0…k4095` — 1.89 units per key — and false of every other ordinary spelling: 4,096
  /// **distinct** keys spelled `user0000Name…` charged 15.83 each, `field0…` charged 41.45 and
  /// `h00000000…` charged 64.60, growing quadratically, with no collision search anywhere. That was
  /// [`hash_bytes`](smear_schema::hash_bytes)'s fold leaving a name's late bytes out of the bucket
  /// index; its avalanche step closed it, and all four spellings now measure 1.74–1.79 units, within
  /// 3% of one another, so the sentence is finally true of more than one fixture.
  ///
  /// None of that was ever the *bound* — a constructed pile-up spends this ceiling either way, and
  /// the hash is still invertible. What it decides is whether the default below is a ceiling on
  /// abuse or a lottery on how a client spells its aliases.
  ///
  /// **It is not "collection work" in the narrow sense, and the widening was a defect repair.**
  /// Every name the executor interns charges this, including the ones only a *diagnostic* wants: a
  /// driver's error message, an unresolvable runtime type name, the spelling of a variable an
  /// argument did not get. Two of those are the document's own text, and while they interned
  /// uncharged a client could name its variables into one bucket and walk it once per failed
  /// field — after collection had been admitted, so nothing here saw it. A name that cannot be
  /// charged is now dropped from its message rather than fatal, exactly as one that will not fit
  /// the arena already was.
  ///
  /// It is also what replaced a recursion. `walk` used to descend the native stack once per named
  /// fragment in a chain, and a chain is flat in the document, so no parser depth limit could see
  /// it — a few thousand links aborted the process with `SIGABRT`, at a count that moves with the
  /// platform and the build profile, which `collect::walk` records with both measurements. The walk
  /// now runs on an explicit stack, which turns depth into heap that this budget already bounds, so
  /// there is no depth knob to get wrong per deployment.
  ///
  /// Per operation rather than per call, because `collect_fields` runs once per object position: a
  /// per-call budget would leave the total at `positions × budget`, and positions are the driver's.
  /// Counting down across the operation means no driver answer multiplies collection work.
  ///
  /// Per operation also means *every* operation. An executor reused for a second run of the same
  /// document re-pays the fragment index pass it built the first time, even though the table is
  /// still there — so the verdict on a request is a function of the request and this number, and
  /// never of what the executor happened to be asked before it.
  ///
  /// **And per draft §6.2.3.2 *event*, which is the same sentence and had to be checked rather
  /// than assumed.** §6.2.3.2 makes each source event one whole `ExecuteRootSelectionSet`, so an
  /// event is an operation by this ceiling's own unit — and it must be, because a subscription is
  /// unbounded in time and a budget spent cumulatively across one would refuse event *N* for work
  /// event 1 did. [`handle_source_event`](Executor::handle_source_event) re-arms it through the
  /// same `reset` a second `start` performs, and `each_source_event_charges_what_the_first_did` is
  /// the gate.
  pub max_selection_visits: NonZeroU32,

  /// How many bytes of interned text the response may hold.
  ///
  /// Response keys, `__typename` answers and the names an error message quotes are stored once
  /// each in one arena, and so are the messages
  /// [`handle_field_error`](Executor::handle_field_error) is handed. That last population is the
  /// reason this is a ceiling and not an invariant: its size is the *driver's*, one message per
  /// failed position, and neither the length nor the count is anything the query bounds.
  ///
  /// **This one is a correctness ceiling before it is a resource ceiling.** Offsets into the arena
  /// are `u32`. Without a bound, an arena past four gigabytes does not fail to allocate — it
  /// truncates, and every name interned afterwards reads back the wrong bytes, so a response comes
  /// out well formed and quietly wrong. The narrowing is checked as well, so the two are
  /// independent: the check cannot be defeated by choosing a larger ceiling.
  ///
  /// A refusal degrades rather than propagates. A driver message that will not fit is reported as
  /// a field error without its text; a *name* that will not fit cannot be degraded that way — a
  /// response key is the position — so that one refuses the position.
  ///
  /// **It bounds bytes, and the memory is a multiple of them.** Every entry carries bookkeeping the
  /// arena's length does not count — its span, its hash-chain link, its share of the bucket table
  /// and its slot in collection's key-to-group scratch — and a GraphQL name is at least one byte,
  /// so `B` bytes of ceiling can be `B` entries costing about `25 · B`. That is a constant multiple
  /// and not a second factor, which is the difference between a cost to know about and the product
  /// shape this module refuses; it is stated because a caller choosing this number is choosing
  /// twenty-five times it.
  pub max_interned_bytes: NonZeroU32,

  /// How many entries a draft §7.1.7 [`Extensions`] map may hold.
  ///
  /// Charged by `Extensions::insert`, and re-checked by
  /// [`set_extensions`](Executor::set_extensions) against *this* executor's value — a map built
  /// under a laxer `Limits` is refused rather than trusted, because a ceiling a caller can pick is
  /// advice.
  ///
  /// It bounds the map's **entry slots** and not only its entries. `Extensions::remove` refunds
  /// this budget without returning the slot to the allocator, so `set_extensions` re-derives an
  /// accepted map's spine from its entries whenever it is holding more than this — otherwise a map
  /// grown under the laxer `Limits` and emptied back down would pass every check here and be
  /// retained at its high-water mark. The entries themselves are untouched; see that method.
  ///
  /// One of a **pair**, and neither half bounds anything alone: see
  /// [`max_extension_key_bytes`](Limits::max_extension_key_bytes).
  ///
  /// **Two retainers read it, because §7.1.7 has two sites.**
  /// [`RequestErrorResult`](super::RequestErrorResult) carries the entry for a draft §7.1.3
  /// *request error result* and performs the identical re-check and re-derivation against the
  /// ceilings it was created with. Passing [`limits`](Executor::limits) is what makes those this
  /// executor's; the type's header says why nothing forces it and what that does and does not
  /// bound.
  pub max_extension_entries: NonZeroU32,

  /// How many bytes of *key* a draft §7.1.7 [`Extensions`] map may hold, summed over its entries.
  ///
  /// The other half of the pair. Entries alone would leave one key free to be a gigabyte; key bytes
  /// alone would leave an unbounded number of entries under empty keys, since a zero-length key is
  /// a legal key. Together they also bound the map's linear scan, whose whole-life worst case is
  /// `max_extension_entries × max_extension_key_bytes` byte comparisons.
  ///
  /// **Not** [`max_interned_bytes`](Limits::max_interned_bytes), and the separation is deliberate:
  /// that arena is a response budget shared with field names and driver error messages, so
  /// spending it on protocol metadata would let an extensions map refuse an error message.
  ///
  /// Values are **not** bounded here, by either ceiling, and that is a **non-goal rather than the
  /// third hole in this pair**. `proto` bounds what `proto` allocates, and an entry's value is the
  /// driver's own `V` — the same way a `String` leaf of any length reaches the response without a
  /// byte ceiling looking at it, and the same way `start`'s `root` and every
  /// [`handle_resolved`](Executor::handle_resolved) answer do. There is no ceiling that could
  /// apply: [`Values`](crate::Values) asks seven questions, each discharging a numbered draft §6
  /// step, and none of them is "how big is this" — measuring one would mean a `V` the executor is
  /// allowed to interpret, which is the boundary this crate is built not to cross. `max_in_flight`
  /// and [`max_response_slots`](Limits::max_response_slots) bound how *many* driver values are
  /// held; nothing bounds one of them, here or anywhere else.
  pub max_extension_key_bytes: NonZeroU32,
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

/// Four entries per position, which is generous for a document that merges and unreachable for one
/// that does not: a group of one costs two entries — one merged selection and one location — so an
/// ordinary response spends this at twice the rate it spends positions and still runs out of
/// positions first.
const DEFAULT_RESPONSE_METADATA: NonZeroU32 = NonZeroU32::new(1 << 22).expect("2^22 is not zero");

/// Sixteen million units of draft §6.3 collection work.
///
/// # The unit is not a selection any more, and this number was chosen when it was
///
/// al8n/smear#113 set `2^24` against a budget of one unit per selection *examined*, and against
/// that unit the old sentence here — "a document large enough to spend it has to *be* large, and
/// the parser and the network have opinions about that long before this does" — was arithmetic:
/// selections cost bytes, so the ceiling was a ceiling on document size and the network reached it
/// first. al8n/smear#143 widened the unit to charge every entry a name lookup compares, and left
/// both the constant and that sentence alone. A comparison count is not linear in document size, so
/// the argument stopped describing the thing it was attached to — which is the failure mode worth
/// naming: **the unit moved and the rationale did not.**
///
/// # What the number means now, measured rather than argued (al8n/smear#172)
///
/// Since [`hash_bytes`](smear_schema::hash_bytes) gained its avalanche step, 4,096 distinct
/// response keys cost 1.74–1.79 units each across every ordinary naming scheme tried — one
/// selection plus about three quarters of a comparison, summed over every table size the interner
/// grows through — and scale at exactly ×2.00 per doubling out to 65,536 keys. So this ceiling
/// admits something over nine million response keys, and no honest document reaches it:
/// [`max_response_slots`](Limits::max_response_slots) refuses at 2^20 positions first. Measured
/// against `Limits::default()`, `k{i}`, `field{i}`, `h{i:0>8}` and
/// `user{i:0>4}Name` documents are all served to 1,048,575 keys and all refused at 1,048,576 —
/// which is the slot ceiling, not this one.
///
/// **Before the finalizer that boundary was a lottery on how a client spelled its aliases.** The
/// same four schemes were served to 1,048,575, 968,252, 880,714 and 976,545 keys respectively: this
/// ceiling bit first for three of the four, at a point set by which bytes of a name happened to
/// distinguish it. Uniformity is what the finalizer bought here; the *bound* was never at stake,
/// since a constructed pile-up spends this budget either way.
///
/// # Tightening it is a separate decision, and deliberately not taken here
///
/// A ceiling eight million keys past what any other ceiling admits is not doing much work, and
/// lowering it is only safe now: with the fold unfinished, `2^19` refused an honestly named
/// `h{i:0>8}` document at 5,777 keys — 75 kilobytes — which is a false refusal of legitimate input.
/// But *what* it should be is a question about the population the default is derived over, and no
/// measurement here answers it.
const DEFAULT_SELECTION_VISITS: NonZeroU32 = NonZeroU32::new(1 << 24).expect("2^24 is not zero");

/// Sixteen megabytes: room for every distinct name a large document and schema can name, plus a
/// great many driver messages, and three orders of magnitude below where a `u32` offset stops
/// working.
const DEFAULT_INTERNED_BYTES: NonZeroU32 = NonZeroU32::new(1 << 24).expect("2^24 is not zero");

/// Entries in one draft §7.1.7 `extensions` map.
///
/// Small on purpose, and it is not the shape of the other ceilings here. Those bound populations
/// the *document* or the *response* grows into, and are sized so that no realistic request meets
/// them. This bounds protocol metadata a service writes about itself — tracing, cost, cache hints,
/// a deprecation notice — which is a handful of keys chosen by the implementer at design time, not
/// a quantity that scales with anything. A service that genuinely needs more says so.
const DEFAULT_EXTENSION_ENTRIES: NonZeroU32 = NonZeroU32::new(64).expect("64 is not zero");

/// Key bytes across one draft §7.1.7 `extensions` map, which with the entry count bounds the scan.
const DEFAULT_EXTENSION_KEY_BYTES: NonZeroU32 = NonZeroU32::new(4096).expect("4096 is not zero");

impl Default for Limits {
  #[inline]
  fn default() -> Self {
    Self {
      max_in_flight: DEFAULT_IN_FLIGHT,
      max_response_slots: DEFAULT_RESPONSE_SLOTS,
      max_response_metadata: DEFAULT_RESPONSE_METADATA,
      max_selection_visits: DEFAULT_SELECTION_VISITS,
      max_interned_bytes: DEFAULT_INTERNED_BYTES,
      max_extension_entries: DEFAULT_EXTENSION_ENTRIES,
      max_extension_key_bytes: DEFAULT_EXTENSION_KEY_BYTES,
    }
  }
}

/// Everything one `expand` call can grow, as it stood before the call.
///
/// # Why an expansion has to be all-or-nothing
///
/// `expand` commits a group at a time, and a later group can be refused after earlier ones are
/// already in. The parent is then nulled — so those children never reach the response — but their
/// **charges** used to stay: metadata entries no slot points at, positions nothing reads, arena
/// bytes no key needs. The next sibling then met a smaller budget than it was owed and could be
/// refused when it fitted. That is not a denial and not a degradation; it is a *wrong response* to a
/// valid query, and the one outcome no ceiling is allowed to cause.
///
/// # What makes this list complete, rather than merely current
///
/// Not an inventory of the executor's fields — that would be a list to keep up to date, which is
/// the failure this program keeps meeting. It is closed against the *defect*: the only harm a
/// leaked charge can do is spend a ceiling, every ceiling reads the length of a table below, and a
/// table no ceiling reads cannot cause it. So the set to restore is exactly "the tables a budget
/// measures", and that set is enumerated by [`Limits`] rather than by memory.
///
/// [`Limits::max_selection_visits`] is the deliberate exception, and the reason states the rule.
/// It counts *work already done*, not storage still held. A refused walk really did walk, and
/// giving the budget back would let a client spend it again by faulting on purpose. Restore what a
/// failure did not keep; never restore what a failure spent.
#[derive(Debug, Clone, Copy)]
struct Mark {
  slots: usize,
  meta: usize,
  merged: usize,
  locations: usize,
  interner: (usize, usize),
  /// Error rows, because a row names the slot it happened at.
  ///
  /// Not present until a restore could reach a path that had already failed. `expand`'s two restore
  /// sites record nothing between mark and restore, so for them this is a no-op — but the list arm
  /// can: with a nullable element type an element's own field error is recorded and the list keeps
  /// going, so a later element's refusal restores over a slot an error row still points at.
  /// `Error::path` reads `slots[row.slot]`, and the damage is not the out-of-bounds it looks
  /// like: the next sibling to expand takes the index the truncated element gave up, so the stale
  /// row silently re-points at *that* position. Measured, the leaf error from `bad`'s element came
  /// back reading `other.ok` — a real field, the wrong one, and nothing crashes. Keeping the slot
  /// to preserve the row is the leak; keeping the row is a lie. Both go, together: a refused branch
  /// is one that did not happen, errors included.
  errors: usize,
  first_child: u32,
  last_child: u32,
  ready_head: u32,
  ready_tail: u32,
}

/// What one operation charged against each of the ceilings that accumulate.
///
/// # It is enumerated from [`Limits`], for the reason [`Mark`] is
///
/// The gate this exists for compares two runs of one request on one executor and requires them to
/// have cost the same, which is the general form of "nothing an executor keeps across
/// [`reset`](Executor::reset) may make the second cheaper". A gate like that is only as wide as the
/// quantities it reads, and the first version read [`Limits::max_selection_visits`] alone — so a
/// table kept across a reset that fed slots, metadata or the arena instead was invisible to it,
/// while the sentence next to it claimed the whole class.
///
/// So the fields are the ceilings, one each, and each is the quantity that ceiling's own check
/// compares against rather than a count kept alongside it: `slots.len()` is what `push_child`
/// tests, `merged.len() + locations.len()` is what `metadata_room` tests, and the arena's length is
/// what `Interner::intern` tests. Reading the check's own input is what keeps this from drifting
/// into a second accounting that agrees with itself.
///
/// [`Limits::max_in_flight`] is absent and that is not an omission: it bounds requests outstanding
/// at an instant rather than a population that accumulates, so there is no total for two operations
/// to disagree about. `collect::Visits` records it, with what else the gate does not reach.
#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Charges {
  /// Units of [`Limits::max_selection_visits`].
  visits: u32,
  /// Positions, against [`Limits::max_response_slots`].
  slots: usize,
  /// Merged selections and location spans together, against [`Limits::max_response_metadata`].
  metadata: usize,
  /// Arena bytes, against [`Limits::max_interned_bytes`].
  interned: usize,
}

/// The **capacity** every buffer an [`Executor`] carries across a [`reset`](Executor::reset) is
/// holding, buffer by buffer.
///
/// # Why this is not [`Charges`] with different arithmetic
///
/// [`Charges`] reads what an operation *spent*, which is what each ceiling's own check compares
/// against. This reads what the allocator is still holding afterwards, and the two come apart
/// exactly where review has twice found a defect: a container's reported size and its capacity are
/// different numbers, `reset` deliberately clears without shrinking, and a **subscription is
/// unbounded in time** — so "each event charges what the first did" says nothing at all about what
/// the executor is holding at event a million.
///
/// The gate this exists for is therefore two assertions rather than one: every field is bounded by
/// the ceiling — or the fixed input — its row in [`Limits`]'s census names, and no field moves
/// between one event and the next. One struct rather than eleven accessors, for the reason
/// [`Charges`] is one value: a gate comparing a subset would pass while the buffer it omitted grew.
///
/// **A buffer added to [`Executor`] and not to this is invisible here**, which is the residual and
/// the reason the census in [`Limits`] is prose a reader checks against the struct definition. The
/// mechanical half cannot derive the field list from the type without a proc macro this crate does
/// not have.
#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Retained {
  /// Positions. Bounded by [`Limits::max_response_slots`].
  slots: usize,
  /// One per position, parallel to `slots`.
  meta: usize,
  /// Merged selections. Bounded by [`Limits::max_response_metadata`].
  merged: usize,
  /// Location spans. Bounded by [`Limits::max_response_metadata`].
  locations: usize,
  /// The interner's arena, table and chain, in entries and bytes. Bounded by
  /// [`Limits::max_interned_bytes`].
  interner: (usize, usize),
  /// Error rows. Derived: at most one per position.
  errors: usize,
  /// In-flight slab entries. Bounded by [`Limits::max_in_flight`].
  inflight: usize,
  /// Draft §6.3's five staging buffers, each with its own bound. See `collect::ScratchCapacity`.
  scratch: crate::collect::ScratchCapacity,
  /// The sub-selection sets one `MergeSelectionSets` concatenates. Bounded by
  /// [`Limits::max_response_metadata`].
  scratch_sets: usize,
  /// Draft §6.4.1's coerced arguments. **The one buffer no ceiling bounds** — see [`Limits`]'s
  /// census for the schema-derived bound that does.
  scratch_args: usize,
  /// The fragment index. Bounded by the document, and its pass is charged.
  fragments: usize,
}

/// Which of [`Limits`]'s ceilings `expand` ran into.
///
/// Carried out of the loop rather than reported inside it, because recording a failure borrows the
/// whole executor while the scratch vectors are still moved out of it.
///
/// Several variants and one [`Kind`](super::Kind): a driver branching on the outcome only needs to
/// know that a ceiling was reached, and the remedy is the same in every case, but the *messages*
/// have to name the right knob — one message would be wrong most of the time.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Exhausted {
  /// [`max_response_slots`](Limits::max_response_slots) has no room for the position.
  Positions,
  /// [`max_response_metadata`](Limits::max_response_metadata) has no room for the group.
  Selections,
  /// A `__typename` answer could not be interned. Which ceiling refused, and the number that
  /// belongs to it, travel together — see [`Unstored`].
  Unstored(Unstored),
}

/// Why [`Executor::start`] refused.
///
/// Every variant is a draft §7.1.2 *request* error — raised before execution begins, which is why
/// it is returned rather than collected into the response. Most of them are draft §6.1
/// `GetOperation` failures; the draft §6.2.3.1 refusals below are not, and neither is
/// [`ResponseStreamOpen`](Self::ResponseStreamOpen), which is about the executor rather than the
/// request.
///
/// # A valid document reaches this, and there is a response shape for it
///
/// Worth stating plainly, because the opposite was written here first. These are **not** the
/// residue of documents draft §5 would have rejected. [`AmbiguousOperation`](Self::AmbiguousOperation)
/// is what a perfectly valid two-operation document produces when `start` is given no operation
/// name, and [`UnknownOperation`](Self::UnknownOperation) is what a valid document produces when
/// the driver names an operation it does not define. §6.1 selects *which* operation to run, which
/// is a property of the request rather than of the document, so validation cannot pre-empt it.
///
/// Draft §7.1.3 gives those a response shape — a *request error result*: a map with **no** `data`
/// entry, a non-empty `errors` list, and optionally its own §7.1.7 `extensions`.
/// [`RequestErrorResult::new`](super::RequestErrorResult::new) builds one from this type and the
/// refusing executor's [`limits`](Executor::limits). `start`'s own signature is unchanged: it
/// returns this value and builds no response, because a result the driver may never need is not
/// the refusal's to allocate.
///
/// **This header used to say the shape was gated on al8n/smear#126, and the gate was drawn one
/// step too wide.** §7.1.6's error result format has exactly one `must` — `message` — and this
/// type's `Display` is it, rendered rather than stored the way `error.rs`'s are; per-error
/// `extensions` is a `may`, which the execution result's own [`Error`](super::Error) does not
/// carry either. What #126 does gate is a *driver-supplied* entry in that list — a message this
/// crate did not raise, with a diagnostic code beside it — which is why the list here holds
/// exactly the one error `start` raised. [`RequestErrorResult`](super::RequestErrorResult)'s
/// header has the boundary and the three request-error sources that fall outside it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum StartError {
  /// The document holds no operation at all.
  NoOperation,
  /// A name was given and no operation carries it.
  UnknownOperation,
  /// No name was given and the document holds more than one operation.
  AmbiguousOperation,
  /// The schema declares no `query` root type.
  ///
  /// Draft §5.2.1.1 makes an operation whose root type the schema does not declare a validation
  /// failure, so a validated document never reaches this or [`NoMutationRoot`](Self::NoMutationRoot);
  /// both are here because `start` is total over the schemas it is handed.
  NoQueryRoot,
  /// The schema declares no `mutation` root type.
  NoMutationRoot,
  /// The schema declares no `subscription` root type.
  ///
  /// Draft §6.2.3.1 step 1 asks for it and step 2 asserts it is an Object type; §5.2.1.1 makes an
  /// operation whose root type the schema does not declare a validation failure, so this is here
  /// on the same terms as the two above — `start` is total over the schemas it is handed.
  NoSubscriptionRoot,
  /// Draft §6.2.3.1 step 5: the subscription's root selection set does not name exactly one field
  /// with an event stream.
  ///
  /// Step 5 is a *request error* in the specification's own words — "If {collectedFieldsMap} does
  /// not have exactly one entry, raise a *request error*" — which is why it is a [`StartError`] and
  /// not one of the field errors §6.4 raises. There is no response for a field error to live in:
  /// `CreateSourceEventStream` runs before any execution does.
  ///
  /// Four shapes reach it, and the sentence above is true of each. The root selection set collects
  /// **nothing**; it collects **more than one** entry; the one entry names a field the schema does
  /// not define, so there is no resolver to ask for a stream; or the one entry is draft §4.4's
  /// `__typename`, which the executor answers itself from the object type it already resolved and
  /// which has no event stream to resolve — §5.2.3.1 spells that last one out, requiring the single
  /// entry to be one "which must not be an introspection field".
  ///
  /// Draft §5.2.3.1 *Single Root Field* catches all four statically, and forbids `@skip` and
  /// `@include` in the root selection set precisely so that it can decide this without runtime
  /// variables. So a validated document does not reach this either; it is here because `start` is
  /// total over the documents it is handed.
  NoSourceField,
  /// Collecting the subscription's root selection set raised an error, so draft §6.2.3.1 had no
  /// `collectedFieldsMap` to count.
  ///
  /// Separate from [`NoSourceField`](Self::NoSourceField) because the caller's remedy is a
  /// different one: this is a ceiling of *this executor's*
  /// ([`max_selection_visits`](Limits::max_selection_visits),
  /// [`max_response_slots`](Limits::max_response_slots),
  /// [`max_response_metadata`](Limits::max_response_metadata),
  /// [`max_interned_bytes`](Limits::max_interned_bytes)) or a `@skip`/`@include` condition whose
  /// variable [`Values::variable`](super::Values::variable) did not supply — raise the budget or
  /// supply the variable, rather than rewrite the document's root selection set.
  ///
  /// The specific fault is **not** carried, and that is a boundary rather than an oversight. It is
  /// available as a draft §7.1.2 field error inside the executor, and [`StartError`] is a `Copy`,
  /// data-free enum whose whole `errors` entry in a §7.1.3
  /// [`RequestErrorResult`](super::RequestErrorResult) is one rendered [`Display`](fmt::Display).
  /// Carrying more means a driver-supplied error *entry*, which is al8n/smear#126's — the same line
  /// `request_error.rs` draws around the three request-error sources that arise outside this crate,
  /// and the same one that leaves [`NoMutationRoot`](Self::NoMutationRoot)'s span uncarried.
  SourceSelectionRefused,
  /// Draft §6.2.3.1 step 8: `CoerceArgumentValues` rejected an argument of the source field.
  ///
  /// **This is the one place where §6.4.1's failure changes shape, and the change is the
  /// specification's.** Everywhere else `CoerceArgumentValues` raises a *field error*, which
  /// §6.4.4 turns into a null and an entry in the response's `errors`. §6.2.3.1 calls it inside
  /// `CreateSourceEventStream`, before a response stream exists at all, so there is no execution
  /// result to null a field in and the only shape left is a request error. `graphql-js` 16.11.0
  /// does the same and was read rather than assumed: `executeSubscription` lets
  /// `getArgumentValues`'s throw escape to `createSourceEventStream`, which returns
  /// `{ errors: [error] }` — a map with no `data` key, which is §7.1.3's request error result.
  ///
  /// Reachable with a **validated** document, unlike the three refusals above: §6.4.1 step 5.f's
  /// `hasValue` is false for a variable [`Values::variable`](super::Values::variable) does not
  /// supply, and which variables a request supplies is not a property draft §5 can see.
  ///
  /// Which argument, and which of §6.4.1's three failures, is not carried — see
  /// [`SourceSelectionRefused`](Self::SourceSelectionRefused) for why, and note that upstream does
  /// carry it. This is an unmet "should" of §7.1.6's `locations`, recorded as one.
  SourceFieldArguments,
  /// The document holds more definitions than
  /// [`max_selection_visits`](Limits::max_selection_visits) left the draft §6.1 lookup room to
  /// read.
  ///
  /// Draft §6.1 selects the operation by walking the document's definitions, which is a quantity
  /// the *client* chooses — so the walk is charged the same unit every other document-sized walk
  /// in this crate is charged, and this is what the refusal looks like. It is reachable only by a
  /// document with more definitions than the ceiling, so the remedy is a larger
  /// `max_selection_visits` or a smaller document, and never a change to the operation name.
  ///
  /// **A named lookup that finds its operation early is not refused for the definitions behind
  /// it**: the charge is taken per definition read rather than for the slice, so the walk stops
  /// where the answer does.
  OperationLookupRefused,
  /// A draft §7.1.2 response stream is still open, so beginning another operation would orphan the
  /// source stream behind it.
  ///
  /// **The one refusal that is not about the document, the schema or the request** — and the one
  /// after which the executor is deliberately *not* empty. Draft §6.2.3.3 splits `Unsubscribe` in
  /// two: this crate ends the response stream, and cancelling the source stream is the driver's,
  /// because the source stream is the driver's. The driver learns it owes that cancellation by
  /// reading [`Cancelled`](super::ResponseStream::Cancelled) off
  /// [`response_stream`](Executor::response_stream) — so a `start` that replaced the phase would
  /// delete the obligation's only channel and leave a pub-sub subscription, a cursor or a task
  /// alive with nothing referring to it.
  ///
  /// The remedy is [`unsubscribe`](Executor::unsubscribe): §6.2.3.3, called by name, which
  /// publishes the obligation and leaves a stream that is no longer open. Then `start` proceeds.
  /// Raised from [`Creating`](super::ResponseStream::Creating) as well as
  /// [`Streaming`](super::ResponseStream::Streaming), because `is_open` is one question and a
  /// driver that has already called its resolver is in the first of them.
  ResponseStreamOpen,
}

impl fmt::Display for StartError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(match self {
      Self::NoOperation => "the document contains no operation",
      Self::UnknownOperation => "the document contains no operation with that name",
      Self::AmbiguousOperation => {
        "the document contains more than one operation and none was named"
      }
      Self::NoQueryRoot => "the schema declares no query root type",
      Self::NoMutationRoot => "the schema declares no mutation root type",
      Self::NoSubscriptionRoot => "the schema declares no subscription root type",
      Self::NoSourceField => {
        "the subscription's root selection set does not name exactly one field with an event stream"
      }
      Self::SourceSelectionRefused => {
        "collecting the subscription's root selection set raised an error"
      }
      Self::SourceFieldArguments => {
        "the subscription's source field has an argument the request does not satisfy"
      }
      Self::OperationLookupRefused => {
        "the document has more definitions than `max_selection_visits` leaves room to read"
      }
      Self::ResponseStreamOpen => {
        "a response stream is still open, and its source stream must be cancelled through \
         `unsubscribe` before another operation begins"
      }
    })
  }
}

#[cfg(feature = "std")]
impl std::error::Error for StartError {}

/// Why [`Executor::set_extensions`] refused, carrying the map back.
///
/// The map is returned in every variant rather than dropped, for the reason a refused
/// [`Extensions::insert`] returns its value: an entry may be a wasm or FFI handle, and a refusal
/// that closes one has done more than refuse.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum SetExtensionsError<V> {
  /// No operation is running: [`Executor::start`] has not been called, or the last one refused.
  NoOperation(Extensions<V>),
  /// The response has already been delivered, so no map attached now could ever appear in one.
  AlreadyDelivered(Extensions<V>),
  /// The map is larger than this executor's extension ceilings allow.
  TooLarge(Extensions<V>),
}

impl<V> SetExtensionsError<V> {
  /// Returns the map that was refused, giving up ownership.
  #[inline]
  pub fn into_extensions(self) -> Extensions<V> {
    match self {
      Self::NoOperation(extensions)
      | Self::AlreadyDelivered(extensions)
      | Self::TooLarge(extensions) => extensions,
    }
  }
}

impl<V> fmt::Display for SetExtensionsError<V> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(match self {
      Self::NoOperation(_) => {
        "no operation is running, so a `extensions` map attached now would be released by the next \
         `start`"
      }
      Self::AlreadyDelivered(_) => {
        "the response has already been delivered, so no `extensions` map attached now could appear \
         in one"
      }
      Self::TooLarge(_) => {
        "the `extensions` map exceeds this executor's `max_extension_entries` or \
         `max_extension_key_bytes`"
      }
    })
  }
}

#[cfg(feature = "std")]
impl<V: fmt::Debug> std::error::Error for SetExtensionsError<V> {}

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

/// Draft §6.2.3.2's two source-stream endings that an already-accepted event's execution result is
/// queued in front of.
///
/// Cancellation is deliberately not one of them. §6.2.3.3 *discards*: "Cancel {sourceStream}.
/// Complete {responseStream} normally" ends the stream on the client's instruction, and a result
/// the client has stopped listening for is not owed to anyone. The two here are the arms where the
/// source ended **on its own**, which §6.2.3.2 sequences *after* the events it already emitted.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Ending {
  /// "When {sourceStream} completes normally: Complete {responseStream} normally."
  Completed,
  /// "When {sourceStream} completes with {error}: Complete {responseStream} with {error}."
  Failed,
}

impl Ending {
  /// The stage this becomes once the result it was queued behind has been taken.
  #[inline]
  const fn ended(self) -> Stage {
    match self {
      Self::Completed => Stage::Completed,
      Self::Failed => Stage::Failed,
    }
  }
}

/// Where draft §6.2.3 has got to, which is [`ResponseStream`] plus the one state §7.1.2 does not
/// name.
///
/// # Why the phase does not simply hold the public type
///
/// It did, and that is the shape a review found a lost execution result in. §6.2.3.2 maps each
/// source event to one execution result, and a push source may emit its last value and its
/// completion back to back — so "the source has ended **and** an accepted event still owes its
/// result" is a state the machine can be in, and [`ResponseStream`] has no value for it. Holding
/// the public type meant the terminator had two choices, both wrong: end the stream now, which
/// resets over the undelivered result and drops it; or refuse, which makes the driver responsible
/// for re-reporting a completion it has already observed.
///
/// [`Owed`](Stage::Owed) is the third. The ending is *recorded* rather than performed, the event's
/// execution is untouched, and [`poll_response`](Executor::poll_response) — the one call that can
/// end that event — is the only transition out of it. What was a sentence in a comment saying the
/// driver must drain before completing is now a state the terminator cannot skip, because the
/// terminator's `match` does not reach `reset` from it.
///
/// # `Owed` reads as `Streaming`, and that is the truthful answer
///
/// [`ResponseStream::is_open`] asks exactly one question — "can this still emit an execution
/// result" — and while a result is owed the answer is yes. Reporting the ending early would tell a
/// serialiser the stream had completed and then hand it one more result; reporting `Streaming`
/// tells it there is still one to take, which is precisely the obligation the driver has.
///
/// So `Owed` is unreachable from outside, and the invariant that keeps it honest is narrow enough
/// to state: **`Owed` implies `started && !delivered`**. It is entered only under that condition,
/// `handle_source_event` cannot re-enter `Streaming` from it, and `poll_response` leaves it in the
/// same statement that sets `delivered`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Stage {
  /// Draft §6.2.3.1 `CreateSourceEventStream` is running.
  Creating,
  /// Open, with no ending reported.
  Streaming,
  /// The source stream ended, and the last accepted event's execution result has not been taken.
  Owed(Ending),
  /// Draft §6.2.3.2's normal completion.
  Completed,
  /// Draft §6.2.3.2's completion with an error.
  Failed,
  /// Draft §6.2.3.3 `Unsubscribe`.
  Cancelled,
}

impl Stage {
  /// Draft §7.1.2's state as a driver reads it.
  #[inline]
  const fn published(self) -> ResponseStream {
    match self {
      Self::Creating => ResponseStream::Creating,
      Self::Streaming | Self::Owed(_) => ResponseStream::Streaming,
      Self::Completed => ResponseStream::Completed,
      Self::Failed => ResponseStream::Failed,
      Self::Cancelled => ResponseStream::Cancelled,
    }
  }
}

/// Which of draft §6.2's three operations is running, and where a subscription is inside §6.2.3.
///
/// # The at-most-one slot, which is what makes this one machine
///
/// This is the `Role`-shaped centre of a "super state machine" in the sense this program's other
/// Sans-I/O crates use the phrase — the whole protocol in *one* machine taking events through
/// `handle_*` and emitting obligations through `poll_*`, rather than a coordinator over
/// sub-machines. Its two siblings are shaped the same way: a Raft endpoint's `Role`
/// (`sailing-proto`) and a VSR replica's `Status` (`viewstamp-proto`) are single-valued slots at
/// the heart of a machine that also owns incidental concurrent fan-out. "Only one is live at a
/// time" is not an argument against the pattern; it *is* the pattern's central slot, and the fan-out
/// here is one layer down and already built — a single §6.2.3.2 event is a whole execution, with
/// its own in-flight table, ready chain, ceilings and §6.4.4 propagation.
///
/// The posture was already here and implicit before this type existed. `start` matched on
/// [`RootOperation`] to decide whether [`withhold_serial`](Executor::withhold_serial) ran and then
/// threw the answer away, so "which operation kind is this" was a fact the machine knew for one
/// statement. Making it a field is what let subscription be its third value instead of its
/// refusal.
///
/// # Why the subscription arm carries data and the other two do not
///
/// A query and a mutation are started once and produce one response, so nothing about the operation
/// has to outlive `start`. A subscription runs draft §6.2.3.2's `ExecuteSubscriptionEvent` once per
/// event, and that algorithm is `ExecuteRootSelectionSet` over **the same operation** — so the root
/// selection set and the root type have to survive every `reset`, and they live here rather than in
/// fields beside a discriminant that could disagree with them. Both are `Copy`: a borrow into the
/// document, which outlives the executor, and a [`TypeId`]. Nothing is allocated and no `V` is held.
enum Phase<'a, S> {
  /// No operation: before the first [`start`](Executor::start), or after one that refused.
  Idle,
  /// Draft §6.2.1 `ExecuteQuery`.
  Query,
  /// Draft §6.2.2 `ExecuteMutation`, whose top-level fields are withheld.
  Mutation,
  /// Draft §6.2.3 `Subscribe`.
  Subscription {
    /// Draft §7.1.2's response stream, as a state — see [`Stage`], which is the public
    /// [`ResponseStream`] plus the one state §7.1.2 does not name.
    stage: Stage,
    /// The operation's root selection set, re-collected once per event by §6.2.3.2.
    set: &'a SelectionSet<S>,
    /// The root Subscription type.
    root: TypeId,
    /// The source field's position while §6.2.3.1 is still running, and [`NONE`] afterwards.
    ///
    /// It indexes a response tree that only exists during
    /// [`Creating`](ResponseStream::Creating) — the tree `start` built to collect the root
    /// selection set, which the transition into [`Streaming`](ResponseStream::Streaming) resets
    /// away. Holding [`NONE`] outside that state is what makes a stale index unrepresentable
    /// rather than merely unlikely.
    source: u32,
  },
}

impl<S> Clone for Phase<'_, S> {
  #[inline]
  fn clone(&self) -> Self {
    *self
  }
}

/// `Copy` written out rather than derived, because a derive would bound `S: Copy`.
///
/// The only `S` this holds is behind a `&'a`, which is `Copy` whatever `S` is — the same
/// over-bound [`Extensions`]'s hand-written `Debug` avoids so that [`Response`] can keep its own.
impl<S> Copy for Phase<'_, S> {}

impl<S> fmt::Debug for Phase<'_, S> {
  /// Renders the discriminant and the response stream's state, and no borrow of the document.
  ///
  /// Unbounded in `S` for the reason [`Copy`] above is, and the selection set would be noise: it is
  /// the operation's whole body.
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Idle => f.write_str("Idle"),
      Self::Query => f.write_str("Query"),
      Self::Mutation => f.write_str("Mutation"),
      Self::Subscription { stage, source, .. } => f
        .debug_struct("Subscription")
        .field("stage", stage)
        .field("source", source)
        .finish_non_exhaustive(),
    }
  }
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

/// Draft §6's executor, for a query, a mutation or a subscription.
///
/// See [the crate root](crate) for the shape, why it has that shape, and a worked example.
///
/// # What every public `&mut self` method owes, on two axes
///
/// ## Axis 1 — the release
///
/// A driver's call in is the first moment the executor learns that the *previous* call's borrows
/// have ended: the [`FieldRequest`] it handed out is gone, so the arguments it carried can be
/// retired, and the object value the last offer parked can be settled. Neither release can happen
/// when the lend ends, because at that instant the driver still holds the borrow. **So every public
/// method taking `&mut self` must call `on_entry` before it does anything else** — or discharge the
/// same obligation another way and say which.
///
/// ## Axis 2 — the phase, which is now two axes
///
/// One *execution* is in one of three phases: **Idle** (`!started`), **Running**
/// (`started && !delivered`), **Delivered** (`started && delivered`). A method is not owed a call
/// in every one of them, and accepting a call from a phase where it cannot mean anything is not
/// harmless — it is a success return for work that will never happen. A phase a method excludes has
/// to be kept out by a flag test, or be impossible for a stated structural reason.
///
/// The **operation kind** is the second axis and it is outer: the private `Phase` slot — named
/// rather than linked, being private, the way `request_error.rs` names its own module header —
/// says whether the executor
/// is running a query, a mutation or a subscription, and for a subscription which
/// [`ResponseStream`] state it is in. Draft §6.2.3's five entry points read it and the other eight
/// do not, which is the column the last two rows of the table below add. The two axes are not the
/// same question — a subscription's `Idle` is "between events", where a query's is "before the
/// operation" — and collapsing them is how a machine ends up delivering an event's response into a
/// stream that has already completed.
///
/// | method | discharge | execution phases | stream states | how the rest are kept out |
/// |---|---|---|---|---|
/// | [`start`](Executor::start) | `on_entry`, and then `reset` on every path that begins an operation — which retires the arguments and drops the whole slot vector, parked object included | all three | the three **endings** | tests the stream state and **refuses** an open one, because a `reset` would delete the §6.2.3.3 obligation before the driver could read it |
/// | [`poll_resolve`](Executor::poll_resolve) | `on_entry` | Running | — | tests `started`; Delivered needs none, since delivery implies an empty ready chain and no live requests |
/// | [`handle_resolved`](Executor::handle_resolved) | `on_entry` | Running | — | the [`ReqId`]'s epoch and generation — an id from another phase is stale and ignored |
/// | [`handle_field_error`](Executor::handle_field_error) | `on_entry` | Running | — | the same id validation |
/// | [`poll_abandoned`](Executor::poll_abandoned) | `on_entry` | Running **and Delivered** | — | the abandoned count, which `reset` zeroes. Delivered is deliberate: `poll_response` does not withhold on abandoned entries, so this is how they are retired afterwards |
/// | [`set_extensions`](Executor::set_extensions) | `on_entry` | Running | — | tests both flags and **refuses**, because a map accepted outside Running is one no response will ever carry |
/// | [`take_extensions`](Executor::take_extensions) | `on_entry` | all three | — | — outside Running there is no map, and `None` is the truthful answer |
/// | [`poll_response`](Executor::poll_response) | `on_entry` | Running | Streaming | tests both flags; the stream test is a **transition** rather than a guard — taking the result is what discharges a §6.2.3.2 ending recorded while the event was running |
/// | [`handle_source_stream`](Executor::handle_source_stream) | `on_entry` | Idle | Creating | tests the stream state; Creating implies `!started`, since §6.2.3.1 begins no execution |
/// | [`handle_source_event`](Executor::handle_source_event) | `on_entry` | Idle **and Delivered** | Streaming | tests the stream state, and Running is kept out by the conjunction of both flags — that pair *is* draft §6.2.3.2's ordering |
/// | [`handle_source_complete`](Executor::handle_source_complete) | `on_entry` | all three | Streaming | tests the stream state; every execution phase is reachable when a source stream ends, including mid-event — and in Running the two flags decide whether the ending is *performed* or *recorded* |
/// | [`handle_source_error`](Executor::handle_source_error) | `on_entry` | all three | Streaming | the same |
/// | [`unsubscribe`](Executor::unsubscribe) | `on_entry` | all three | Creating **and** Streaming | tests the stream state; §6.2.3.3 may arrive at any moment, which is what cancellation is |
///
/// A row with no stream state performs no `Phase` test, and means the same thing under every
/// operation kind. That is a claim about the code and it is checked as one, in both directions.
///
/// **Two rows read the slot without gating on it, and both arrived in the same review round.**
/// `start` refuses the states it does not name, and `poll_response` performs a transition in the
/// one it does. Neither is the column drifting: both are the claim it exists to make — that the
/// method does not mean the same thing under every operation kind — and a row that read `Phase`
/// while naming no state would fail the check either way.
///
/// ## What holds the table to the code
///
/// Two tests, and neither is sufficient alone — a table can be complete and wrong, and a release
/// can be real and unlisted.
/// `execute::tests::every_public_entry_point_declares_its_discharge_and_its_phases` derives the set
/// of public `&mut self` methods from this file's own source and fails on one the table does not
/// name, one whose body does not *open* with the discharge it claims, one whose declared flag tests
/// are not the flag tests the body performs, an excluded phase with neither a guard nor a
/// structural reason, or a row whose stream-state column disagrees with whether the body reads
/// `Phase` at all.
/// `execute::tests::every_public_entry_point_settles_the_previous_call` drives each of them with
/// both lends open and fails on one that does not close them.
///
/// **Both axes are here because review found a defect on each, and they were one mistake.**
/// `set_extensions` and `take_extensions` shipped without `on_entry`, so a driver could drop a
/// `FieldRequest`, call one of them, and leave a handle open with nothing in the response to show
/// for it. `set_extensions` then also accepted a map before `start` — which the next `start`
/// dropped — and after delivery, where no response could carry it. The first table was complete
/// along the release axis and blind along the phase one. The obligation had been written down for
/// years, on `on_entry` itself, in a comment that even predicted which later phases would inherit
/// it. Prose one level away from the decision is not a mechanism, and **a table is only as wide as
/// its columns**: when a ninth entry point arrives, the failure to expect is not a missing row.
///
/// Five arrived, and the prediction held exactly. Draft §6.2.3's entry points are legal or illegal
/// on a dimension the two flags cannot express — a subscription that has completed still has
/// `started` and `delivered` saying whatever the last event left them saying — so the stream-state
/// column is the third one, added with the rows rather than after them. It carries its own
/// two-way check for the reason the other two do: a column whose entries nothing compares against
/// the code is a list, and a list is what this table exists instead of.
pub struct Executor<'a, S, V>
where
  V: Values,
{
  schema: &'a Schema,
  document: &'a ExecutableDocument<S>,
  /// The document's named fragments, indexed by name so that a spread costs a probe rather than a
  /// scan of every definition.
  ///
  /// Indexed on the first fragment spread and **charged against
  /// [`max_selection_visits`](Limits::max_selection_visits) when it is**, then kept across every
  /// [`reset`](Self::reset). Indexing in the constructor instead made an executor built per
  /// execution pay `executions × definitions` before any ceiling existed, and filling a table from
  /// names the *document* chooses without charging for it is a hole a charged lookup does not
  /// close. See `collect::Fragments`.
  ///
  /// The **table** survives a reset; the **charge** does not. Every operation pays the pass, and an
  /// operation that finds the table already built pays exactly what building it would have cost, so
  /// a request's answer does not depend on what this executor was asked before it. See
  /// `collect::Fragments::build` for the retry that used to be admitted.
  fragments: Fragments<'a, S>,
  limits: Limits,
  /// `__typename`'s symbol in this schema, resolved once so that `expand` recognises the
  /// meta-field with an integer comparison rather than a name lookup per collected group.
  ///
  /// `None` means the schema never interned the name, which [`Schema::build`] cannot produce — it
  /// interns all three meta-field names unconditionally. Treating it as "no meta-field here" keeps
  /// the branch total instead of resting on that.
  typename: Option<Sym>,

  slots: std::vec::Vec<Slot<V::Value>>,
  /// The greatest [`Slot::depth`] any position in `slots` carries.
  ///
  /// A high-water mark and not a scan, for the reason [`Slot::depth`] is a field and not a walk:
  /// the only two places a position is created both know its depth as they write it, so keeping the
  /// maximum costs one comparison and reading it costs nothing. It is what
  /// [`Response::depth`](Response::depth) answers, and what lets a serialiser size a per-level
  /// structure — one frame per open container — before it descends.
  ///
  /// **Over the positions this executor CREATED, which is not quite the positions a reader will
  /// see.** Draft §6.4.4's discard marks a subtree and leaves it in place — that is the whole of
  /// how it is cheap — so a chain that was built and then nulled still counts here while it renders
  /// as a single `null`. Lowering it again would need the number of live positions at each depth,
  /// which is a table on the executor's hot path; what it is instead is an upper bound, bounded in
  /// turn by [`Limits::max_response_slots`] like the slab it is a fact about. `Response::depth`
  /// says the same thing to a caller.
  max_depth: u32,
  meta: std::vec::Vec<Meta<'a, S>>,
  merged: std::vec::Vec<&'a Field<S>>,
  locations: std::vec::Vec<SimpleSpan>,
  interner: Interner,
  /// Definitions the draft §6.1 operation lookup has read, over the executor's whole life.
  ///
  /// Counted for the reason `collect::Fragments`'s `walked` is: the lookup charges one unit before
  /// each definition it reads, so the budget and the read count agree by construction and a
  /// version that charged *after* the read — or charged the slice length and then walked it —
  /// would agree with itself just as well. Only a count taken at the read itself can say a refused
  /// lookup read nothing.
  #[cfg(test)]
  operation_definitions_walked: u64,
  errors: std::vec::Vec<Row>,
  /// The draft §7.1.7 *Extensions* entry for this operation, or `None` when the response has none.
  ///
  /// # `proto` owns the top level, the driver owns every value under it
  ///
  /// §7.1.7 makes one structural demand — "if set, must have a map as its value" — and then stops:
  /// "there are no additional restrictions on its contents". Those two clauses draw the seam. The
  /// top level is where the specification requires a shape, so `Extensions` owns it and the illegal
  /// shapes cannot be written; everything below is representation, so each value is one `V::Value`
  /// the driver hands over and nothing here reads.
  ///
  /// This was an opaque `V::Value` for the whole entry first, and review corrected it. That version
  /// let safe driver code reach a `Response` carrying `"extensions": null` with no error anywhere,
  /// and `proto` could not detect it: `Values` has no `is_map`, and must not grow one, because each
  /// of its methods discharges a numbered draft §6 step. `extensions.rs` carries the full argument
  /// and prices the container.
  ///
  /// # It is released like an argument, not like a slot
  ///
  /// The response *tree* releases its values by being cleared, and this one is not in the tree, so
  /// `reset` drops it by name. A value in it may be a wasm or FFI handle — `scratch_args` carries
  /// that argument at length — and one kept past the operation it belongs to holds open whatever it
  /// names.
  extensions: Option<Extensions<V::Value>>,

  inflight: std::vec::Vec<Entry>,
  epoch: u64,
  free: u32,
  live: u32,
  abandoned: u32,
  abandon_cursor: u32,

  ready_head: u32,
  ready_tail: u32,

  /// The one object whose last reader departed during the offer being returned, or [`NONE`].
  ///
  /// `poll_resolve` cannot transition that object on the spot: the `FieldRequest` it is about to
  /// return borrows the very value the transition would drop. So the slot is parked here and every
  /// entry point settles it before doing anything else — the discipline `retire_arguments` already
  /// implements, for the same reason, because the executor first learns a lend has ended when the
  /// driver calls back in.
  ///
  /// One cell is enough: an offer takes one slot out of Ready, so at most one parent can reach zero
  /// per offer.
  deferred: u32,

  /// The next top-level field draft §6.2.2's serial rule is withholding, or [`NONE`].
  ///
  /// # It is the ready chain, cut
  ///
  /// A mutation's root expands exactly as a query's does and enqueues every top-level field;
  /// [`withhold_serial`](Self::withhold_serial) then severs the chain after its head. The withheld
  /// fields keep their `next_ready` links — they are still a list, just not one `ready_head`
  /// reaches — and [`release_serial`](Self::release_serial) splices one back each time the previous
  /// field's subtree is complete or cancelled. That is the whole of serial execution: no queue is
  /// allocated, no flag is consulted, and no ordering contract is handed to a driver, because
  /// `poll_resolve` cannot offer what is not on the chain.
  ///
  /// # The cost, because this is where the product would have been
  ///
  /// A splice is one link and the cursor only moves forward, so a mutation of `M` top-level fields
  /// spends `M` steps over the whole operation however many positions the driver's answers build.
  /// The shape being avoided is the obvious implementation: deciding *which field is next* by
  /// scanning the root's children, or *whether the previous subtree is done with* by walking it.
  /// Either is paid once per offer — and offers are the **driver's** quantity while `M` and the
  /// subtree's shape are the **document's**, which is the product [`Limits`] refuses to meet a
  /// fourth time. There is no ceiling here because there is no product: `release_serial` reads two
  /// counters and follows one link. `serial_steps` is what keeps that measured rather than claimed.
  serial_next: u32,

  /// Links [`release_serial`](Self::release_serial) has followed, over this executor's whole life.
  ///
  /// Counted for the reason `collect::Fragments::compares` is. A serial gate that costs `M` and one
  /// that costs `M` per offer produce the same response, so no assertion on a response separates
  /// them and a clock is not a gate.
  serial_steps: u64,

  /// Which of draft §6.2's three operations is running. See [`Phase`].
  ///
  /// The at-most-one operation-kind slot, and the *outer* of the executor's two phase axes: this
  /// says which operation, and `started`/`delivered` say where one execution of it has got to. For
  /// a query or a mutation the two collapse — there is exactly one execution — and for a
  /// subscription they do not, which is the whole of draft §6.2.3.2 inside this machine.
  phase: Phase<'a, S>,

  /// Whether an execution is running: one query, one mutation, or **one draft §6.2.3.2 event**.
  ///
  /// It is per *execution* and not per operation, which is what lets a subscription reuse every
  /// other field here unchanged. A subscription's `start` leaves this `false` — §6.2.3.1 begins no
  /// execution, so there is nothing for `poll_resolve` to offer and nothing for `poll_response` to
  /// deliver — and each `handle_source_event` sets it, exactly as `start` does for the other two.
  started: bool,
  /// Whether the running execution's response has been taken.
  ///
  /// For a query or a mutation this is terminal. For a subscription it is the intake gate: it is
  /// what makes `handle_source_event` refuse while the previous event's draft §7.1.1 execution
  /// result is still undelivered, and the next accepted event clears it by resetting.
  delivered: bool,

  /// Everything one draft §6.3 collection reuses from the last. See `collect::Scratch`.
  scratch: Scratch<'a, S>,
  scratch_sets: std::vec::Vec<&'a SelectionSet<S>>,
  /// What is left of [`Limits::max_selection_visits`] for this operation.
  visits: Visits,
  /// Draft §6.4.1's surviving arguments, for the one request `poll_resolve` is offering.
  ///
  /// The only driver values the executor owns that are not part of the response at all, and so not
  /// released by the response tree — nor is `extensions`, which *is* part of the response and still
  /// outside the tree. Those two are the executor's only values that have to be dropped by name.
  ///
  /// An [`ArgumentSource::Variable`] carries the very value §6.4.1 checked rather than the name to
  /// look it up by — see that variant for why it must — which makes this vector the owner of a
  /// `V::Value` that may be a wasm or FFI handle, and a handle held past the position it was
  /// checked for holds open whatever it names.
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
  /// Nothing happens until [`start`](Executor::start), and nothing is allocated here. The one thing
  /// that could reasonably have happened here — indexing the document's named fragments — happens
  /// on the first fragment spread instead, so that its cost is charged to the operation that needs
  /// it rather than paid once per executor outside every ceiling.
  ///
  /// Both borrows outlive the executor, so the document is never copied and every literal a driver
  /// reads out of an [`ArgumentSource`](super::ArgumentSource) points into it.
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
      scratch: Scratch::default(),
      fragments: Fragments::new(document),
      limits,
      typename: schema.sym(builtin::TYPENAME_FIELD.as_bytes()),
      slots: std::vec::Vec::new(),
      max_depth: 0,
      meta: std::vec::Vec::new(),
      merged: std::vec::Vec::new(),
      locations: std::vec::Vec::new(),
      interner: Interner::new(limits.max_interned_bytes.get()),
      #[cfg(test)]
      operation_definitions_walked: 0,
      errors: std::vec::Vec::new(),
      extensions: None,
      inflight: std::vec::Vec::new(),
      epoch: 0,
      free: NONE,
      live: 0,
      abandoned: 0,
      abandon_cursor: 0,
      ready_head: NONE,
      ready_tail: NONE,
      phase: Phase::Idle,
      started: false,
      delivered: false,
      scratch_sets: std::vec::Vec::new(),
      deferred: NONE,
      serial_next: NONE,
      serial_steps: 0,
      visits: Visits::new(limits.max_selection_visits.get()),
      scratch_args: std::vec::Vec::new(),
    }
  }

  /// Begins draft §6.2.1 `ExecuteQuery`, §6.2.2 `ExecuteMutation` or §6.2.3 `Subscribe` on `root`,
  /// which is their `initialValue`.
  ///
  /// `operation` names the operation to run, as draft §6.1's `GetOperation` takes it: `None`
  /// selects the document's only operation and fails if there is more than one.
  ///
  /// Which of the three it is decides what this call leaves behind, and nothing else about the
  /// surface changes:
  ///
  /// - A **query** leaves every root field ready.
  /// - A **mutation** leaves all but the first withheld, and §6.2.2's "serially" is then a property
  ///   of what [`poll_resolve`](Executor::poll_resolve) is able to offer. Everything below the top
  ///   level — including the sub-selections of a mutation field — is §6.3's ordinary parallel
  ///   collection, which is where the specification draws the line too.
  /// - A **subscription** runs draft §6.2.3.1 `CreateSourceEventStream` and begins **no execution
  ///   at all**. It collects the root selection set to find the single source field and coerces its
  ///   arguments, and then waits: [`source_field`](Executor::source_field) is what the driver calls
  ///   `ResolveFieldEventStream` with, and until it reports back through
  ///   [`handle_source_stream`](Executor::handle_source_stream) there is nothing to resolve and no
  ///   response to deliver. `subscribe.rs`'s header has the whole division of labour.
  ///
  /// **Every refusal §6.2.3.1 can raise is a [`StartError`] and none of them is a field error**,
  /// which is the specification's own shape: `CreateSourceEventStream` runs before any execution,
  /// so there is no response for a field error to live in. That includes step 8's
  /// `CoerceArgumentValues`, whose failures are field errors everywhere else in draft §6 — see
  /// [`SourceFieldArguments`](StartError::SourceFieldArguments).
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
  /// the arguments of the last request it was offered, and any draft §7.1.7 *Extensions* map
  /// [`set_extensions`](Executor::set_extensions) was given. That happens before draft §6.1's
  /// lookup and therefore on the refusals too, so a `start` that returns a [`StartError`] leaves the
  /// executor holding nothing: not the operation it ended, and not the `root` it was handed, which
  /// is never stored on a path that refuses.
  ///
  /// # One refusal comes *before* that, and it is the exception the sentence above needs
  ///
  /// [`ResponseStreamOpen`](StartError::ResponseStreamOpen). An open draft §7.1.2 response stream
  /// is the one thing this executor holds that **is not this executor's to release**: the source
  /// stream behind it is the driver's — a pub-sub subscription, a cursor, a task — and draft
  /// §6.2.3.3 makes cancelling it the driver's half of `Unsubscribe`. The only channel that
  /// obligation travels on is [`response_stream`](Executor::response_stream) reading
  /// [`Cancelled`](ResponseStream::Cancelled), and a `start` that reset would delete the channel
  /// before the driver could read it — so a reused executor would leak one source stream per
  /// restart, silently and with nothing in the state left to say so.
  ///
  /// So this refusal is taken **first, and changes nothing at all**: the phase, the response
  /// stream's state, and everything §6.2.3 was holding are exactly as they were, and the way
  /// forward is [`unsubscribe`](Executor::unsubscribe) — §6.2.3.3 by name, which publishes
  /// `Cancelled` and then lets the next `start` through. It is the one [`StartError`] after which
  /// the executor is not empty, and that is the point of it rather than an omission.
  ///
  /// **The alternative was to cancel on the driver's behalf and hand the obligation back**, either
  /// as a `Cancelled` state a successful `start` leaves behind — which the very next line
  /// overwrites — or as a value in the `Ok`. Both are worse for the same reason: `start(..)?`
  /// discards an `Ok` payload without a diagnostic, and a state the next statement overwrites is
  /// not a channel. A refusal cannot be discarded, because the operation does not begin; and it
  /// makes the driver write `unsubscribe`, which is the point where a human decides what happens
  /// to the source stream. Performing §6.2.3.3 inside an unrelated `start` would also be this
  /// crate running a specification algorithm nobody asked for.
  ///
  /// A stream that has already **ended** — [`Completed`](ResponseStream::Completed),
  /// [`Failed`](ResponseStream::Failed) or [`Cancelled`](ResponseStream::Cancelled) — is not open
  /// and is no obstacle: the source stream is gone or the driver has already been told to end it.
  pub fn start(
    &mut self,
    ctx: &mut V,
    operation: Option<&str>,
    root: V::Value,
  ) -> Result<(), StartError> {
    self.on_entry();
    // Before the reset, because the reset is what would destroy the evidence. `on_entry` above is
    // the discharge every entry point owes; this refusal performs no state change of its own, so
    // "settle the previous call before doing anything else" still holds over it.
    if let Phase::Subscription { stage, .. } = self.phase
      && stage.published().is_open()
    {
      return Err(StartError::ResponseStreamOpen);
    }
    self.reset();
    let op = self.operation_definition(operation)?;
    // Enumerated rather than tested with `is_query()`, so that a fourth operation keyword would be
    // a compile error here instead of an operation silently executed as a query.
    let (set, operation, missing) = match op {
      OperationDefinition::Shorthand(set) => (set, RootOperation::Query, StartError::NoQueryRoot),
      OperationDefinition::Named(named) => match named.operation_type() {
        OperationType::Query(_) => (
          named.selection_set(),
          RootOperation::Query,
          StartError::NoQueryRoot,
        ),
        OperationType::Mutation(_) => (
          named.selection_set(),
          RootOperation::Mutation,
          StartError::NoMutationRoot,
        ),
        OperationType::Subscription(_) => (
          named.selection_set(),
          RootOperation::Subscription,
          StartError::NoSubscriptionRoot,
        ),
      },
    };
    let root_type = self.schema.root(operation).ok_or(missing)?;

    // Draft §6.2.3.1 begins no execution: `CreateSourceEventStream` collects and coerces, and the
    // first thing that could be resolved belongs to the first *event*. Leaving the flag false is
    // what makes `poll_resolve` and `poll_response` answer `None` while the stream is being
    // created, without either of them learning about subscriptions.
    self.started = operation != RootOperation::Subscription;
    let collected = self.root_selection_set(ctx, root_type, set, root);
    match operation {
      RootOperation::Query => self.phase = Phase::Query,
      RootOperation::Mutation => {
        // Draft §6.2.2 runs the top-level selection set serially. The expansion above enqueued
        // every top-level field, exactly as a query's does; this takes all but the first back off
        // the chain.
        self.withhold_serial();
        self.phase = Phase::Mutation;
      }
      RootOperation::Subscription => {
        self.create_source_event_stream(ctx, set, root_type, collected)?
      }
    }
    Ok(())
  }

  /// Draft §6.2.3.1 `CreateSourceEventStream` steps 5 to 8, over the root expansion `start` has
  /// just run.
  ///
  /// Steps 1 to 4 are the caller's — the root type, the top level selection set, and `CollectFields`
  /// — because they are the same four things a query's `start` does and are done by the same code.
  /// Step 9 `ResolveFieldEventStream` is the **driver's**: it is a resolver call, and this crate
  /// never makes one.
  ///
  /// # Step 5 counts collected entries, and so does this
  ///
  /// "If {collectedFieldsMap} does not have exactly one entry, raise a *request error*." The entry
  /// count is `collected`, taken from the collection itself, and it is deliberately not
  /// "how many child positions did the expansion create": `expand` skips a group whose field the
  /// schema does not define — omitting the key being the only response that cannot invent one — so
  /// `{ undefinedField newMessage }` creates one position out of **two** collected entries, and a
  /// count of positions would accept a document step 5 refuses.
  ///
  /// # Every refusal resets first
  ///
  /// [`start`](Executor::start) promises that a refusal leaves the executor holding nothing, and
  /// these refusals happen *after* the root value has been stored and the root selection set
  /// expanded. So each of them resets before returning, which releases the `initialValue`, the
  /// throwaway tree, and any argument coercion had already pushed. Without it a refused
  /// subscription would leave a driver handle alive with no operation to release it.
  fn create_source_event_stream(
    &mut self,
    ctx: &mut V,
    set: &'a SelectionSet<S>,
    root: TypeId,
    collected: usize,
  ) -> Result<(), StartError> {
    let source = match self.one_source_field(collected) {
      Ok(source) => source,
      Err(refusal) => {
        self.reset();
        return Err(refusal);
      }
    };
    // Step 8. Its failure is a *request* error here and a field error everywhere else in draft §6,
    // for the reason `SourceFieldArguments` gives: there is no response yet to null a field in.
    if !self.coerce_arguments(ctx, source) {
      self.reset();
      return Err(StartError::SourceFieldArguments);
    }
    // The root still holds the `initialValue` and `scratch_args` holds step 8's answer, and both
    // are lent by `source_field` for as long as this state lasts. `on_entry` is what would
    // otherwise retire the second of them, and it does not while the stream is `Creating`.
    //
    // **There is deliberately no assertion here, and it is a repair rather than an omission.** One
    // was written — that the root is still `State::Object` — and it duplicated `source_field`'s
    // own `unreachable!`, which covers the identical condition unconditionally and at the reader.
    // A duplicate check one step earlier on the same path is not free: measured, it *moved* where
    // two planted faults fired. Accepting `__typename` as a source field leaves the root expired,
    // and accepting two collected entries leaves it holding two children — both tripped the new
    // line rather than the checks that own them, so the step 5 count's own gate went green under a
    // fault it exists to catch. Put the guard where the value is read, once.
    self.phase = Phase::Subscription {
      stage: Stage::Creating,
      set,
      root,
      source,
    };
    Ok(())
  }

  /// Draft §6.2.3.1 steps 5 to 7: the position of the one source field, or why there is not one.
  fn one_source_field(&self, collected: usize) -> Result<u32, StartError> {
    // A refused collection has no `collectedFieldsMap` to count at all, so it is answered before
    // the count rather than through it. `expand` records its refusal as a field error at the root
    // and §6.4.4 discards it, which is the flag read here.
    if self.slots[0].discarded {
      return Err(StartError::SourceSelectionRefused);
    }
    if collected != 1 {
      return Err(StartError::NoSourceField);
    }
    let source = self.slots[0].first_child;
    // Two shapes reach this with one collected entry and no field to resolve: the schema does not
    // define the field, so `expand` created no position for it; and draft §4.4's `__typename`,
    // which the executor answers itself and never enqueues. §5.2.3.1 rules the second one out by
    // name — the single entry "must not be an introspection field".
    if source == NONE || !matches!(self.slots[source as usize].state, State::Ready) {
      return Err(StartError::NoSourceField);
    }
    Ok(source)
  }

  /// Pushes the root position over `value` and runs draft §6.3 on `set`, returning how many entries
  /// the collection produced.
  ///
  /// Everything `start` does once for a query or a mutation, and once **per source event** for a
  /// subscription: draft §6.2.3.2's `ExecuteSubscriptionEvent` is `ExecuteRootSelectionSet` over
  /// the same operation with the event as `initialValue`, so an event's execution is this
  /// executor's ordinary one and §6.2.3 adds no execution code at all.
  ///
  /// The caller has already `reset`, which is what makes each event pay the same ceilings as the
  /// first. See [`handle_source_event`](Executor::handle_source_event).
  fn root_selection_set(
    &mut self,
    ctx: &mut V,
    root_type: TypeId,
    set: &'a SelectionSet<S>,
    value: V::Value,
  ) -> usize {
    let root_ty = PackedType::named(self.schema.type_def(root_type).name(), root_type);
    self.slots.push(Slot::new(
      NONE,
      // Draft §7.1.2's path of the root is the empty one, so its depth is zero and every depth
      // under it is counted from here — see `push_child`, the only other place the number is set.
      0,
      Key::Root,
      root_ty,
      State::Object {
        value,
        ty: root_type,
        // `expand` writes the real count on its way out, and does so for the root through the very
        // same path it uses for every other object — which is why the root needed no special case.
        pending: 0,
      },
    ));
    self.meta.push(Meta {
      field: None,
      merged: (0, 0),
      locations: (0, 0),
      parent_type: root_type,
      field_sym: self.schema.type_def(root_type).name(),
    });

    self.scratch_sets.clear();
    self.scratch_sets.push(set);
    self.expand(ctx, 0)
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
    self.on_entry();
    if !self.started {
      return None;
    }
    let found = loop {
      // Draft §6.2.2's next top-level field becomes available the moment the previous one's subtree
      // is complete or cancelled, and this is where that is noticed. A no-op for a query, and for a
      // mutation with work still outstanding that could reach the response.
      self.release_serial();
      if self.live + self.abandoned >= self.limits.max_in_flight.get() {
        break None;
      }
      let Some(slot) = self.pop_ready() else {
        break None;
      };
      if self.discarded(slot) {
        // No decrement, and the reason is load-bearing rather than an omission. A slot on the ready
        // chain carrying the flag always has a flagged parent: a chain slot is flagged only by a
        // discard rooted at itself or an ancestor, and it cannot be its own root, because the only
        // path that fails a Ready slot pops it first. So the root is a proper ancestor and the
        // parent was flagged with it — its state is already `Null`, holding no value and no count.
        continue;
      }
      if self.coerce_arguments(ctx, slot) {
        break Some(slot);
      }
      // Argument coercion raised a field error, which nulled the field and may have nulled an
      // ancestor. There is nothing to hand the driver; take the next ready slot.
      //
      // This candidate left Ready without ever being offered, so it is a read that will not happen.
      // Nothing borrows the parent's value here, so the expiry is immediate rather than parked.
      let parent = self.slots[slot as usize].parent;
      self.child_departed(parent, false);
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

    // The offered child is this parent's last read if the count reaches zero — but the request
    // returned below borrows the value, so the expiry is parked for the next call in rather than
    // done here.
    let parent = self.slots[slot as usize].parent;
    self.child_departed(parent, true);

    let meta = &self.meta[slot as usize];
    let (start, len) = meta.merged;
    let field = meta
      .field
      .expect("only the root has no field, and it is never ready");
    let parent_value = match &self.slots[parent as usize].state {
      State::Object { value, .. } => value,
      // Enumerated rather than a `_`, and that is a decision. A wildcard here would have absorbed
      // `Expanded` silently and handed the driver a `parent_value` from a different object; the
      // compiler naming every variant is what makes a future one impossible to mishandle by
      // omission.
      //
      // `Expanded` is unreachable *structurally*, not by hope: a parent transitions only when no
      // child of its remains Ready, and this slot was Ready a moment ago, so no offer can name a
      // parent that has already transitioned.
      //
      // **This panics in release, and that is intended.** `unreachable!` is `panic!`; it is not
      // compiled out. The alternative on a wrong count is to lend the wrong object's value as a
      // resolver's parent — a response that is silently, plausibly incorrect. A library panicking
      // on its own accounting bug is defensible; a library corrupting a response is not, and this
      // is the one place where the two are the choice.
      State::Expanded
      | State::Ready
      | State::InFlight
      | State::Null
      | State::Leaf(_)
      | State::TypeName(_)
      | State::List => unreachable!("a field slot's parent is an object"),
    };
    Some(FieldRequest {
      id,
      schema: self.schema,
      slots: &self.slots,
      names: self.interner.names(),
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
  /// - The position reaches the finished response **as a leaf**, so the value is held until the
  ///   next [`start`](Executor::start) or until the executor is dropped. A reader can still ask for
  ///   it, which is what makes this the *response* rather than a copy of it.
  /// - The position reaches the finished response **as an object**, and that value is released far
  ///   sooner — as soon as the last child the object enqueued has been offered. The justification
  ///   above is *false* for it, and used to be given anyway: no reader can ask for an object's
  ///   value. [`Node`](super::Node) has no variant that carries one, and the single reader in the
  ///   program is the `parent_value` this executor lends to one request at a time. So an object's
  ///   value dies when its reader set empties, which for a `__typename`-only selection is inside
  ///   this very call. See [`Limits`]'s lifetime table, row 2.
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
    self.on_entry();
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
    self.on_entry();
    let Some(slot) = self.release(id) else {
      return;
    };
    if self.discarded(slot) {
      return;
    }
    // The driver's failure is reported whether or not its text can be kept. Dropping the text is
    // a loss; letting it push the arena past what a `u32` offset can address would corrupt every
    // name interned after it, which is not — and the same is true of a lookup nobody paid for, so
    // the budget refuses through the same door the arena does.
    //
    // Through the same door, but not with the same message. The refusal is carried rather than
    // discarded, because the two doors are two different knobs and an operator told the wrong one
    // resizes an arena that was never the constraint.
    let raw = match self.interner.intern(message, &mut self.visits) {
      Ok(message) => Raw::Resolver { message },
      Err(unstored) => Raw::ResolverUnstorable { unstored },
    };
    self.fail(slot, raw);
  }

  /// Returns a request whose value can no longer affect the response.
  ///
  /// Draft §6.4.4 nulls an ancestor when a non-null position errors, and everything outstanding
  /// under that ancestor becomes irrelevant the instant it does. This is how the driver hears
  /// about it, and cancelling the work is the driver's to do — `proto` performs no I/O and has
  /// nothing to cancel.
  ///
  /// Nothing waits for these, and that is meant literally: no release, no offer and no response is
  /// conditioned on one being retired or answered. A mutation's next top-level field is released
  /// over an abandoned request rather than behind it — `release_serial` records the `graphql-js`
  /// measurement that settled it — and [`poll_response`](Self::poll_response) will deliver over
  /// one.
  ///
  /// What an undrained entry does instead is *occupy*. It holds its in-flight slot, so
  /// [`poll_resolve`](Self::poll_resolve)'s effective ceiling is
  /// [`max_in_flight`](Limits::max_in_flight) less however many the driver has left in the table —
  /// and never less than one of them, for the reason that knob's own documentation gives. So what
  /// ignoring this channel costs is concurrency, with a floor of one request at a time; it is not
  /// correctness, and it is not progress.
  ///
  /// Answering a retired request afterwards is harmless: the id is stale and ignored.
  pub fn poll_abandoned(&mut self) -> Option<ReqId> {
    self.on_entry();
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

  /// Returns the ceilings this executor was built with.
  ///
  /// Chiefly so an [`Extensions`] map can be created under the same budget it will be checked
  /// against: `Extensions::new(executor.limits())`.
  #[inline]
  pub const fn limits(&self) -> &Limits {
    &self.limits
  }

  /// Attaches the draft §7.1.7 *Extensions* map to the response this operation will produce, and
  /// returns whatever entry it replaced.
  ///
  /// §7.1.7, in full: "The `extensions` entry in an *execution result* or *request error result*,
  /// if set, must have a map as its value. This entry is reserved for implementers to extend the
  /// protocol however they see fit, and hence there are no additional restrictions on its
  /// contents."
  ///
  /// **This is the response's map and there is no request-side counterpart**, deliberately: the §6
  /// preamble's request `extensions` is the one member of a *request* that `ExecuteRequest` is not
  /// handed, so it has no reader here and no path into a response — §7.1.8 *Additional Entries*
  /// admits no execution-result entry beyond `data`, `errors` and this one. A driver echoing one
  /// into a response does it through this call, with the entries it chooses. The [crate root](crate)'s `Scope` section has the derivation, why an
  /// accessor here would satisfy the lifetime table's rule 2 and still not be a reader, and a
  /// worked example of where the request's map goes instead.
  ///
  /// # Three states, and no two of them are one
  ///
  /// | in the response | here | what §7 says |
  /// |---|---|---|
  /// | no `extensions` key at all | `None`: never set, or [`take_extensions`](Executor::take_extensions) | §7.1.1 *Execution Result*: it "**may** also contain an entry with key `extensions`" |
  /// | `"extensions": { … }`, the empty map included | `Some(`[`Extensions`]`)` | §7.1.7 puts no restriction on the contents, and an empty map is a map |
  /// | `"extensions": null`, or a scalar, or a list | **unrepresentable** | §7.1.7: "if set, **must have a map** as its value" |
  ///
  /// Present-and-empty is a different response from absent, and the `Option` is the only thing that
  /// separates them: [`Extensions::is_empty`] is still a *present* entry.
  ///
  /// # Refusal
  ///
  /// Three ways, all handing the map back — see [`SetExtensionsError`]. Two are the operation
  /// lifecycle and one is the budget:
  ///
  /// - **[`NoOperation`](SetExtensionsError::NoOperation).** Before [`start`](Executor::start), or
  ///   after one that refused. `start` releases the previous operation's values, so a map accepted
  ///   here would be dropped by the very next call — a silent loss dressed as a success.
  /// - **[`AlreadyDelivered`](SetExtensionsError::AlreadyDelivered).**
  ///   [`poll_response`](Executor::poll_response) yields at most once per execution, so a map
  ///   accepted after it has yielded could never appear in any response, and its values would be
  ///   held by an executor with no way to surface them. Under a draft §6.2.3 subscription the
  ///   refusal is per *event* and so is the acceptance: each event's §7.1.7 map is attached while
  ///   that event is running and released with it, so a service that annotates every payload
  ///   attaches one per event rather than one per subscription.
  /// - **[`TooLarge`](SetExtensionsError::TooLarge).** The map exceeds *this* executor's
  ///   [`max_extension_entries`](Limits::max_extension_entries) or
  ///   [`max_extension_key_bytes`](Limits::max_extension_key_bytes). `Extensions` carries the
  ///   ceilings it was created under, so the re-check is what stops a map built under a laxer
  ///   `Limits` from being attached here — a ceiling a caller can pick is advice, and a signature
  ///   is what prevents it.
  ///
  /// The first two are the same defect the third row of `Executor`'s phase table exists for: this
  /// method is meaningful only while an operation is running, and a setter that accepts a value it
  /// can never show anyone is a silent loss whichever side of the operation it happens on.
  ///
  /// # On acceptance the allocation is re-derived, and that is not a refusal
  ///
  /// **The ceilings above bound what the map reports; this bounds what it holds.** `len` and
  /// `key_bytes` are the map's content, and an [`Extensions`]'s spine capacity is derived from
  /// neither — it is the high-water mark of every entry the map has ever held, and
  /// [`Extensions::remove`] refunds the key bytes without returning the slot. A map grown to a
  /// million entries under a lax [`Limits`] and emptied passes both checks above while holding a
  /// million entry slots, and this executor would then keep that allocation until the next
  /// [`start`](Executor::start), a [`take_extensions`](Executor::take_extensions), or drop.
  ///
  /// So an accepted map's spine is re-derived from the entries it actually carries whenever it is
  /// over [`max_extension_entries`](Limits::max_extension_entries). The entries, their order, their
  /// keys and their values are untouched — nothing is cloned, nothing is dropped, and this is
  /// invisible to every observable the type has. It is deliberately **not** a fourth
  /// [`SetExtensionsError`]: a grown-and-emptied map is a valid `extensions` map, its capacity is
  /// not visible through this crate's public API, and a caller handed one back could not shrink it
  /// even if it were. Refusing it would fail a legal program on a property its author cannot see.
  ///
  /// The general rule, which is the half worth carrying to the next resource: **a ceiling bounds a
  /// retained buffer only if every growth of that buffer passed through it.** Everything else this
  /// executor retains satisfies that by construction, because the executor's own ceiling gates the
  /// only site that grows it. A map arrives grown under a [`Limits`] that is not this one's, which
  /// is the same premise the [`TooLarge`](SetExtensionsError::TooLarge) re-check is built on — it
  /// was simply applied to the two quantities that were easy to see.
  ///
  /// # It is an entry point
  ///
  /// Like every other public call in, it settles the lends the driver's previous call left open
  /// before anything else — including on the refusal paths, which is why `on_entry` runs before the
  /// checks rather than after them. See [`Executor`]'s header for the obligation and the table.
  ///
  /// # No tier withholds it
  ///
  /// It is available wherever this crate is, and no feature gates it: `alloc` is unconditional in
  /// this crate, so [`Extensions`] compiles and behaves the same under `--no-default-features` as
  /// it does with `std`.
  pub fn set_extensions(
    &mut self,
    mut extensions: Extensions<V::Value>,
  ) -> Result<Option<Extensions<V::Value>>, SetExtensionsError<V::Value>> {
    self.on_entry();
    if !self.started {
      return Err(SetExtensionsError::NoOperation(extensions));
    }
    if self.delivered {
      return Err(SetExtensionsError::AlreadyDelivered(extensions));
    }
    if extensions.len() as u64 > u64::from(self.limits.max_extension_entries.get())
      || u64::from(extensions.key_bytes()) > u64::from(self.limits.max_extension_key_bytes.get())
    {
      return Err(SetExtensionsError::TooLarge(extensions));
    }
    // After the refusals, so a map this call is not going to keep is not reallocated on its way
    // back out to the driver.
    extensions.shrink_to_ceiling(self.limits.max_extension_entries.get());
    Ok(self.extensions.replace(extensions))
  }

  /// Removes the draft §7.1.7 *Extensions* map, returning it, so that the response carries no such
  /// entry.
  ///
  /// The third transition, so that "absent" is not a one-way door. A driver that attached a map and
  /// then concluded the response should not carry one gets its values back — which matters when one
  /// is a handle — instead of having to end the operation to release them.
  ///
  /// **Legal in every phase, unlike its counterpart**, and infallible with it. Before an operation
  /// and after delivery there is simply no map, so this answers `None` truthfully rather than
  /// accepting something it cannot honour; and after delivery it is the one way a driver reclaims
  /// the values the response was carrying without dropping the executor.
  ///
  /// An entry point, on the same terms as [`set_extensions`](Executor::set_extensions).
  #[inline]
  pub fn take_extensions(&mut self) -> Option<Extensions<V::Value>> {
    self.on_entry();
    self.extensions.take()
  }

  /// Returns the finished response, once nothing that could still change it is outstanding.
  ///
  /// Yields at most once **per execution**. A second call returns `None`, which is what makes it
  /// usable as the driver's loop condition rather than something that has to be guarded by a flag
  /// the driver keeps.
  ///
  /// For a query or a mutation there is one execution, so that is once and then never. For a
  /// subscription each draft §6.2.3.2 source event is one execution and yields one draft §7.1.1
  /// execution result — and this method is not what re-opens it. The next
  /// [`handle_source_event`](Executor::handle_source_event) is, which is why the intake refuses
  /// while a result is undelivered: "delivered" is exactly the condition that lets the next event
  /// in, so the successive yields of this method **are** §7.1.2's response stream, in source order,
  /// with no second surface and no change to this one.
  ///
  /// Requests [`poll_abandoned`](Executor::poll_abandoned) has not yet retired do **not** withhold
  /// it. Draft §6.4.4 discarded the positions they would have filled, so by construction no answer
  /// to one can alter a single byte of what is returned here — and withholding on them would be
  /// the one wait a driver could clear only by acting on work it has just been told to abandon.
  /// Every other cost of leaving an entry in the table is a narrowed
  /// [`max_in_flight`](Limits::max_in_flight) that answering the *live* work reopens; this one
  /// arrives at exactly the moment there is no live work left to answer, so the response would be
  /// reachable two ways and no third — retiring those entries through that channel, or answering
  /// them, since `release` retires an abandoned entry on a late answer as well. What the
  /// non-withholding costs is a driver holding live-looking [`ReqId`]s across a restart, and
  /// [`ReqId`]'s epoch is what pays it.
  ///
  /// A mutation's **withheld** top-level fields do withhold it, and they are the one kind of
  /// outstanding work that is in neither the ready chain nor the in-flight table — draft §6.2.2
  /// keeps them off both on purpose. A response delivered over them would be missing a field the
  /// document asked for, with no error to say so, so this releases the next one before it decides.
  pub fn poll_response(&mut self) -> Option<Response<'_, V::Value>> {
    self.on_entry();
    if !self.started || self.delivered {
      return None;
    }
    self.drain_ready();
    // A mutation with fields still withheld is not finished, even though none of them is on the
    // chain. Releasing here rather than testing the cursor keeps one decision point: the same code
    // that consumes the cursor is the code that retires it, so a mutation whose root draft §6.4.4
    // nulled ends instead of stalling on fields that can no longer be part of any response.
    self.release_serial();
    if self.live > 0 || self.ready_head != NONE {
      return None;
    }
    // Invariant: at delivery no object value is still held — every one has expired on the success
    // path or been overwritten by a discard. Stated as an assertion so a later phase that adds an
    // enqueue or de-queue path and forgets the count gets a red test instead of a silent leak.
    //
    // `debug_assert` deliberately, and the reason is the scan: this is O(positions), and its whole
    // purpose is to fail a test, which runs in debug. A release build gains nothing from paying for
    // it on the success path. That is a different decision from the `unreachable!` on the offer
    // path, which stays unconditional because *there* the alternative is handing a resolver the
    // wrong object's value.
    debug_assert!(
      !self
        .slots
        .iter()
        .any(|slot| matches!(slot.state, State::Object { .. })),
      "a response was delivered with an object value still held: some enqueue or departure path \
       is not accounted"
    );
    // Draft §6.2.3.2's source stream may have ended while this event was running, in which case the
    // ending is recorded on the phase and this result is the one it was queued behind. Taking the
    // result is the transition, so it happens in the statement that marks it taken: `Owed` is
    // entered only while `started && !delivered`, and this is the one line that ends that.
    //
    // The `Response` below borrows the slots, so nothing here may `reset` — and nothing needs to.
    // An event's populations die at the next `start` or drop exactly as a query's do.
    if let Phase::Subscription {
      stage: Stage::Owed(ending),
      set,
      root,
      ..
    } = self.phase
    {
      self.phase = Phase::Subscription {
        stage: ending.ended(),
        set,
        root,
        source: NONE,
      };
    }
    self.delivered = true;
    Some(Response {
      schema: self.schema,
      slots: &self.slots,
      depth: self.max_depth,
      names: self.interner.names(),
      name_spans: self.interner.spans(),
      locations: &self.locations,
      errors: &self.errors,
      extensions: self.extensions.as_ref(),
    })
  }

  // ---------------------------------------------------------------------------------------
  // draft §6.2.3 Subscription, and draft §7.1.2 Response Stream
  // ---------------------------------------------------------------------------------------

  /// Returns draft §7.1.2's *response stream* state, or `None` when this is not a subscription.
  ///
  /// `None` is §7.1.2's own condition, made structural: a response stream is what a request returns
  /// "when the GraphQL operation is a subscription", so a query or a mutation — or an executor that
  /// has not been started — has none, and cannot be asked to serialise one.
  ///
  /// It survives the stream ending, which is the point. The three completions are terminal states
  /// rather than a return to `None`, so a driver reads *how* the stream ended off the machine after
  /// the fact — including [`Cancelled`](ResponseStream::Cancelled), which is the driver's
  /// instruction to cancel its own source stream.
  ///
  /// Takes `&self`, so it is not one of the entry points [`Executor`]'s protocol table governs: it
  /// settles nothing, because it lends nothing.
  #[inline]
  pub const fn response_stream(&self) -> Option<ResponseStream> {
    match self.phase {
      Phase::Idle | Phase::Query | Phase::Mutation => None,
      Phase::Subscription { stage, .. } => Some(stage.published()),
    }
  }

  /// Returns the one root field draft §6.2.3.1 step 9 `ResolveFieldEventStream` is to be called
  /// with, or `None`.
  ///
  /// `Some` exactly while the response stream is [`Creating`](ResponseStream::Creating) — steps 1
  /// to 8 have run and the driver has not yet reported a stream. The driver calls its own
  /// subscription resolver with [`name`](SourceField::name),
  /// [`parent_value`](SourceField::parent_value) and [`arguments`](SourceField::arguments), and then
  /// [`handle_source_stream`](Executor::handle_source_stream).
  ///
  /// # It takes `&self`, and that is a decision rather than an accident
  ///
  /// Every other output on this surface is a `poll_*` that takes `&mut self`, because every other
  /// one *drains* something: a request leaves the ready chain, an abandonment leaves the table, a
  /// response is delivered once. This drains nothing. §6.2.3.1's answer is a property of the state
  /// rather than an item on a channel, it does not change while the state lasts, and reading it
  /// twice is not a driver error.
  ///
  /// The shared borrow is also what keeps the lend honest. The arguments this lends are the ones
  /// [`start`](Executor::start) coerced — §6.2.3.1 puts `CoerceArgumentValues` inside
  /// `CreateSourceEventStream`, and [`Values::variable`](super::Values::variable) takes `&mut V`
  /// and may legitimately answer differently per call, so coercing a second time here could deliver
  /// a value step 8 never checked. Being `&self`, this cannot re-coerce and does not have to: the
  /// values are still where step 8 left them, and `on_entry` does not retire them while the stream
  /// is `Creating` for exactly that reason.
  #[inline]
  pub fn source_field(&self) -> Option<SourceField<'_, 'a, S, V::Value>> {
    let Phase::Subscription {
      stage: Stage::Creating,
      source,
      ..
    } = self.phase
    else {
      return None;
    };
    let meta = &self.meta[source as usize];
    let (start, len) = meta.merged;
    let field = meta
      .field
      .expect("draft §6.2.3.1's source field is a collected field and has a node");
    let State::Object { value, .. } = &self.slots[0].state else {
      // Enumerated by its opposite for the reason the offer path enumerates every variant: the root
      // holds the `initialValue` for as long as `Creating` lasts, because `expand` enqueued exactly
      // one child and nothing has departed. `create_source_event_stream` asserts it on the way in.
      unreachable!("draft §6.2.3.1's root holds its `initialValue` while the stream is created")
    };
    Some(SourceField {
      schema: self.schema,
      field,
      merged: &self.merged[start as usize..(start + len) as usize],
      field_sym: meta.field_sym,
      parent_type: meta.parent_type,
      parent_value: value,
      field_type: self.slots[source as usize].ty,
      arguments: &self.scratch_args,
    })
  }

  /// Reports that draft §6.2.3.1 step 9 `ResolveFieldEventStream` returned a source stream, opening
  /// draft §7.1.2's response stream.
  ///
  /// Returns whether it made the transition: `true` from
  /// [`Creating`](ResponseStream::Creating), and `false` from anywhere else, where there is no
  /// §6.2.3.1 in progress for a stream to belong to.
  ///
  /// # It is the release point for everything §6.2.3.1 held
  ///
  /// The `initialValue` [`start`](Executor::start) was given, the throwaway tree that collection
  /// built, and step 8's coerced arguments are all dead the instant the source stream exists —
  /// nothing reads them again, and each event brings its own root value. So this resets, and a
  /// subscription in [`Streaming`](ResponseStream::Streaming) with no event in flight holds no
  /// driver value at all.
  ///
  /// That is a bound rather than tidiness. A subscription is unbounded in time: a value held from
  /// here would be held for as long as the client stays connected, and `V::Value` is the driver's
  /// own type — a wasm or FFI handle, a pooled buffer, a database cursor. Waiting for the first
  /// event to reset would hold it for a stream that may legitimately never produce one.
  ///
  /// A driver whose resolver *failed* calls nothing here. §6.2.3.1's failure is a request error
  /// that this crate did not raise, so the driver builds its own §7.1.3 response and drops or
  /// restarts the executor; [`unsubscribe`](Executor::unsubscribe) is available if it wants the
  /// state to say so.
  pub fn handle_source_stream(&mut self) -> bool {
    self.on_entry();
    let Phase::Subscription {
      stage: Stage::Creating,
      set,
      root,
      ..
    } = self.phase
    else {
      return false;
    };
    self.reset();
    self.phase = Phase::Subscription {
      stage: Stage::Streaming,
      set,
      root,
      source: NONE,
    };
    true
  }

  /// Supplies one event from the source stream, beginning draft §6.2.3.2's
  /// `ExecuteSubscriptionEvent` on it.
  ///
  /// `event` is the event's value, which §6.2.3.2 makes the execution's `initialValue`. From here
  /// the driver's loop is a query's, unchanged: [`poll_resolve`](Executor::poll_resolve) until it
  /// withholds, answer through [`handle_resolved`](Executor::handle_resolved) and
  /// [`handle_field_error`](Executor::handle_field_error), and take the event's draft §7.1.1
  /// execution result from [`poll_response`](Executor::poll_response). Then offer the next event.
  ///
  /// # The intake is the ordering, and the ordering is the reason this phase exists
  ///
  /// §6.2.3.2 maps each source event to one execution result on the response stream, in source
  /// order. This refuses with [`Outstanding`](SourceEventError::Outstanding) while the previous
  /// event's result has not been taken, which is that requirement made structural — a driver cannot
  /// lose a result by pushing early, and cannot emit two out of order, because the machine will not
  /// take the second event.
  ///
  /// **It is deliberately stricter than the reference implementation, and no conformance test can
  /// see the difference.** `graphql-js` 16.11.0's `mapAsyncIterator` has no queue and no
  /// re-entrancy guard, so a consumer that does not `for await` gets concurrent executions resolved
  /// in completion order; upstream's ordering is its consumer's, not its library's. A Sans-I/O
  /// machine has no `for await` to borrow that from, so the choice is to own the invariant or to
  /// publish it as a rule the driver must remember. `subscribe.rs`'s header has the measurement and
  /// prices the alternative, which is a reordering buffer whose size is a driver quantity times a
  /// response — the product shape [`Limits`] refuses.
  ///
  /// # Each event pays exactly what the first one did
  ///
  /// This resets before it collects, so every cumulative ceiling — positions, metadata, interned
  /// bytes and [`max_selection_visits`](Limits::max_selection_visits) — is re-armed per event, and
  /// every driver value the previous event held is released here. A stream that charged
  /// cumulatively would fail event *N* for work event 1 did, which is a bound a client clears by
  /// reconnecting rather than a bound. What survives is what survives a second
  /// [`start`](Executor::start): the fragment index, which is a function of the document, and the
  /// containers' capacities, which the ceilings bound.
  ///
  /// Draft §6.3's collection is re-run per event rather than hoisted out of the loop, which is what
  /// upstream does. See `subscribe.rs`'s header for why the hoist is not taken.
  ///
  /// # Refusal
  ///
  /// Two ways, both handing the event back — see [`SourceEventError`]. A source event is the
  /// driver's own value and may be a handle, so a refusal that dropped it would have done more than
  /// refuse.
  pub fn handle_source_event(
    &mut self,
    ctx: &mut V,
    event: V::Value,
  ) -> Result<(), SourceEventError<V::Value>> {
    self.on_entry();
    let Phase::Subscription {
      stage: Stage::Streaming,
      set,
      root,
      ..
    } = self.phase
    else {
      return Err(SourceEventError::NotStreaming(event));
    };
    // Draft §6.2.3.2's one-result-per-event, as the two flags rather than as a rule. An execution
    // that has begun and not been delivered is the previous event's, and taking this one would
    // discard it.
    if self.started && !self.delivered {
      return Err(SourceEventError::Outstanding(event));
    }
    self.reset();
    self.phase = Phase::Subscription {
      stage: Stage::Streaming,
      set,
      root,
      source: NONE,
    };
    self.started = true;
    // `ExecuteSubscriptionEvent` is `ExecuteRootSelectionSet` over the same operation, so this is
    // the very call `start` makes — one execution, indistinguishable from a query's from here on.
    let _ = self.root_selection_set(ctx, root, set, event);
    Ok(())
  }

  /// Reports that the source stream completed normally, completing draft §7.1.2's response stream
  /// normally.
  ///
  /// Draft §6.2.3.2: "When {sourceStream} completes normally: Complete {responseStream} normally."
  /// Returns whether it made the transition, which is `true` only from
  /// [`Streaming`](ResponseStream::Streaming) — a stream that was never opened cannot complete, and
  /// one that has already ended does not end twice.
  ///
  /// Terminal. No further event is accepted, and every driver value the last event left behind is
  /// released here rather than waiting for the executor to be dropped or restarted — **unless that
  /// event still owes its execution result**, in which case its populations are the response's and
  /// die where a query's do, at the next [`start`](Executor::start) or drop. See below.
  ///
  /// # It never ends a stream that still owes an execution result
  ///
  /// A push source can emit its last value and its completion back to back, so this call is
  /// reachable with an event accepted and its draft §7.1.1 execution result not yet taken. Ending
  /// the stream there would `reset` over that event — its `data`, its `errors`, its §7.1.7 map and
  /// its execution flags — and [`poll_response`](Executor::poll_response) could then never emit the
  /// result §6.2.3.2 requires. So the ending is **recorded and not performed**: the execution is
  /// left exactly as it was, and the completion happens inside the `poll_response` that takes the
  /// result it was queued behind.
  ///
  /// While that is pending, [`response_stream`](Executor::response_stream) still reads
  /// [`Streaming`](ResponseStream::Streaming) — the honest answer to the one question
  /// [`is_open`](ResponseStream::is_open) asks, since a result is still to come — and no further
  /// source event is accepted, this call included: it returns `false` a second time, the stream
  /// having already ended once.
  ///
  /// **This used to be a sentence saying the driver had to drain first**, on the argument that a
  /// `Response` borrows the executor so the compiler requires it. The compiler requires no such
  /// thing: the borrow ends when the driver drops the `Response`, and it never begins for a driver
  /// that simply did not call `poll_response`. That is the whole lesson — an ordering a comment
  /// states is not an ordering — and the repair is that the state machine has a value for
  /// "accepted and undelivered" which the terminator's `match` cannot reach `reset` from. See
  /// `Stage`.
  ///
  /// # The three completions are written out and not folded into one helper
  ///
  /// This body, [`handle_source_error`](Executor::handle_source_error)'s and
  /// [`unsubscribe`](Executor::unsubscribe)'s differ in a few tokens each, and factoring them into
  /// `complete_stream(end)` is the obvious tidy. It is refused for a mechanical reason:
  /// `execute::tests::every_public_entry_point_declares_its_discharge_and_its_phases` derives each
  /// entry point's guards from **its own body**, so a phase test moved into a shared callee is a
  /// guard the table can no longer see. Three near-identical bodies that a gate reads are worth
  /// more than one body it does not — and they are not identical anyway: `unsubscribe` admits a
  /// second stream state and discards rather than defers, cancellation being the one ending that
  /// owes no result.
  pub fn handle_source_complete(&mut self) -> bool {
    self.on_entry();
    let Phase::Subscription {
      stage: Stage::Streaming,
      set,
      root,
      ..
    } = self.phase
    else {
      return false;
    };
    // Draft §6.2.3.2 maps the accepted event to an execution result, and this completion is queued
    // behind it. The two branches are the whole of the repair: one ends the stream, and the other
    // records that it is ending and leaves the execution alone for `poll_response`.
    let stage = if self.started && !self.delivered {
      Stage::Owed(Ending::Completed)
    } else {
      self.reset();
      Stage::Completed
    };
    self.phase = Phase::Subscription {
      stage,
      set,
      root,
      source: NONE,
    };
    true
  }

  /// Reports that the source stream completed with an error, completing draft §7.1.2's response
  /// stream with it.
  ///
  /// Draft §6.2.3.2: "When {sourceStream} completes with {error}: Complete {responseStream} with
  /// {error}." Returns whether it made the transition, on the same terms as
  /// [`handle_source_complete`](Executor::handle_source_complete), and is terminal in the same way.
  ///
  /// # The error is not carried, and that is the boundary rather than a gap
  ///
  /// It takes no message. The error is the **driver's**: it belongs to a stream this crate does not
  /// own, raised by code this crate did not run, and there is no type here for a driver-supplied
  /// error entry — that is al8n/smear#126's, and it is the same line
  /// [`RequestErrorResult`](super::RequestErrorResult) draws around the three request-error sources
  /// that arise outside this crate. A driver reporting one already holds it.
  ///
  /// What the call buys is that the *ending* is recorded in one place. §6.2.3.2 makes completing
  /// with an error a different completion from completing normally, and a serialiser reads which
  /// one happened off [`response_stream`](Executor::response_stream) rather than off its own
  /// bookkeeping — and the machine refuses further events either way.
  ///
  /// # An accepted event's result survives this too
  ///
  /// Identically to [`handle_source_complete`](Executor::handle_source_complete), and it has to be
  /// identical: §6.2.3.2's two source-stream endings are sequenced after the events the stream
  /// already emitted, and an event this machine accepted is one of those. A source that fails
  /// immediately after pushing its last value is the ordinary shape of that, not an exotic one. So
  /// the ending is recorded, `poll_response` still emits the event's execution result, and the
  /// response stream reads [`Failed`](ResponseStream::Failed) once it has.
  pub fn handle_source_error(&mut self) -> bool {
    self.on_entry();
    let Phase::Subscription {
      stage: Stage::Streaming,
      set,
      root,
      ..
    } = self.phase
    else {
      return false;
    };
    let stage = if self.started && !self.delivered {
      Stage::Owed(Ending::Failed)
    } else {
      self.reset();
      Stage::Failed
    };
    self.phase = Phase::Subscription {
      stage,
      set,
      root,
      source: NONE,
    };
    true
  }

  /// Draft §6.2.3.3 `Unsubscribe`: cancels draft §7.1.2's response stream.
  ///
  /// Returns whether it cancelled an open stream — `true` from
  /// [`Creating`](ResponseStream::Creating) and [`Streaming`](ResponseStream::Streaming), and
  /// `false` from a stream that has already ended or from a query or mutation, neither of which has
  /// a response stream to cancel.
  ///
  /// # Half of §6.2.3.2's cancellation arm, and the other half is the driver's
  ///
  /// "When {responseStream} is cancelled: Cancel {sourceStream}. Complete {responseStream}
  /// normally." The second step is this call. The first cannot be: the source stream is the
  /// driver's, and this crate performs no I/O and has nothing to cancel. So the obligation is
  /// *published as state* instead — [`response_stream`](Executor::response_stream) reads
  /// [`Cancelled`](ResponseStream::Cancelled), which is a different value from
  /// [`Completed`](ResponseStream::Completed) precisely so that a driver cannot mistake "the stream
  /// ended on its own" for "you still owe your source stream a cancellation".
  ///
  /// §6.2.3.3 calls that cancellation "a good opportunity to clean up any other resources used by
  /// the subscription", and this call does its own half of that too: it releases the last event's
  /// response tree, the draft §7.1.7 map, and — from `Creating` — the `initialValue` and the source
  /// field's coerced arguments.
  ///
  /// # Legal at any moment, which is what cancellation means
  ///
  /// It is accepted mid-event, before the first event, and while §6.2.3.1 has not finished. A
  /// client disconnects when it disconnects, and a cancellation that could only be taken at a
  /// quiescent point would be one a driver had to queue — which is the driver owning a protocol
  /// obligation again. Every [`ReqId`] outstanding at that moment is void from here, exactly as it
  /// is across a [`start`](Executor::start), and for the same reason.
  ///
  /// # Cancellation is the ending that discards, and the only one
  ///
  /// [`handle_source_complete`](Executor::handle_source_complete) defers to an accepted event's
  /// undelivered execution result; this does not, and the asymmetry is §6.2.3's rather than an
  /// inconsistency. Those two arms are the source stream ending *on its own*, which §6.2.3.2
  /// sequences after the events it already emitted. §6.2.3.3 is the **client** saying stop, and a
  /// result nobody is listening for is owed to nobody — "a good opportunity to clean up any other
  /// resources used by the subscription" is the specification's own framing, and holding an
  /// undelivered response would be the opposite of it.
  ///
  /// It is therefore also legal from a stream whose ending is already recorded and whose last
  /// result has not been taken: the published state is still
  /// [`Streaming`](ResponseStream::Streaming), the stream is open, and this discards the result and
  /// cancels. The answer is [`Cancelled`](ResponseStream::Cancelled) even though that source stream
  /// has already ended, which costs a driver one no-op cancellation and keeps one rule instead of
  /// two.
  pub fn unsubscribe(&mut self) -> bool {
    self.on_entry();
    let Phase::Subscription {
      stage, set, root, ..
    } = self.phase
    else {
      return false;
    };
    if !stage.published().is_open() {
      return false;
    }
    self.reset();
    self.phase = Phase::Subscription {
      stage: Stage::Cancelled,
      set,
      root,
      source: NONE,
    };
    true
  }

  // ---------------------------------------------------------------------------------------
  // draft §6.4.3 CompleteValue
  // ---------------------------------------------------------------------------------------

  /// Draft §6.4.3 `CompleteValue`, step for step.
  ///
  /// Recursive only through its list arm, and therefore bounded by
  /// [`MAX_WRAPPERS`](smear_schema::MAX_WRAPPERS) — an object's sub-selections do not
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
      // Elements are appended one at a time and a later one can be refused, so the arm is a branch
      // that fails after allocating — the shape `expand`'s rollback was added for, in a path that
      // rollback never reached. Without this an over-budget list nulled itself and went on holding
      // the positions of the elements it had already built, so a sibling that fitted in the correct
      // degraded response was refused to pay for a list that is not in the response at all.
      let mark = self.mark(slot);
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
          // Everything this list built goes back before the refusal is recorded. The elements were
          // about to be nulled with the list, so `data` is unchanged; what changes is that their
          // positions stop being charged against whatever the driver resolves next.
          self.restore(slot, mark);
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
      let Some(name) = ctx.type_name(&value) else {
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
      };
      let resolved = self
        .schema
        .sym(name.as_bytes())
        .and_then(|sym| self.schema.type_of_sym(sym))
        .filter(|&id| self.schema.type_def(id).kind() == TypeKind::Object)
        .filter(|&id| self.schema.is_possible_object(base, id));
      match resolved {
        // The success path does not intern, and must not. The runtime spelling is wanted for one
        // thing only — quoting it in the error below — so interning it here would put a *storage*
        // ceiling on a path that needs no storage, and an exhausted arena would turn a resolvable
        // interface position into `AbstractUnresolved` plus a null while the right answer sat in
        // this very binding. A resource check belongs where running out means the operation cannot
        // proceed; where the resource is only wanted for a diagnostic, running out degrades the
        // diagnostic.
        Some(id) => id,
        None => {
          // Here, and only here, the name has to be said out loud.
          //
          // And when it cannot be, the message says which ceiling silenced it. This arm used to
          // render the arena's cap whatever had refused, so a visit budget exhausted by the probe
          // run reported itself as a full arena.
          let raw = match self.interner.intern(name, &mut self.visits) {
            Ok(runtime) => Raw::AbstractNotPossible {
              abstract_ty: base,
              runtime,
            },
            Err(unstored) => Raw::AbstractNotPossibleUnnamed {
              abstract_ty: base,
              unstored,
            },
          };
          self.fail(slot, raw);
          return;
        }
      }
    } else {
      base
    };

    self.slots[slot as usize].state = State::Object {
      value,
      ty: object,
      pending: 0,
    };

    // `MergeSelectionSets`: every field that shared this response key contributes its
    // sub-selections, and §6.3 collects over the concatenation.
    let (start, len) = self.meta[slot as usize].merged;
    self.scratch_sets.clear();
    for field in &self.merged[start as usize..(start + len) as usize] {
      if let Some(set) = field.selection_set() {
        self.scratch_sets.push(set);
      }
    }
    // Draft §6.2.3.1 step 5's entry count is the only reader of this, and only for the *root*
    // expansion of a subscription. Below the root nothing counts entries.
    let _ = self.expand(ctx, slot);
  }

  /// Draft §6.3 over `self.scratch_sets`, creating one ready slot per response key, and returning
  /// how many entries the collection produced.
  ///
  /// A `@skip`/`@include` condition that could not be read is a field error *here*, at the object
  /// whose selection set was being collected — which for the root selection set is the root, whose
  /// path is empty and whose null is the whole of `data`. That is the position the condition
  /// belongs to: the guarded field has no slot yet, and one selection's unreadable guard says
  /// nothing about its siblings' values, only that this object cannot be assembled.
  ///
  /// # The return value is `collectedFieldsMap`'s size, and only one caller has a use for it
  ///
  /// Draft §6.2.3.1 step 5 counts *entries*, and entries and positions are different numbers: a
  /// group whose field the schema does not define is skipped without a position being created, and
  /// draft §4.4's `__typename` becomes a position that is never enqueued. Every other caller
  /// discards this, which is deliberate — a `#[must_use]` here would be asserting that the count
  /// matters to §6.4.3's completion path, and it does not.
  ///
  /// It is the count *before* any refusal, because that is what step 5 asks about: the map
  /// `CollectFields` produced, not the part of it this expansion managed to commit. A refused
  /// expansion is answered by the root's discard flag instead, which is a different question with
  /// a different remedy.
  fn expand(&mut self, ctx: &mut V, slot: u32) -> usize {
    let object_type = match &self.slots[slot as usize].state {
      State::Object { ty, .. } => *ty,
      // Enumerated for the reason the offer path is. `Expanded` cannot appear because a slot
      // expands once; the rest are the not-an-object cases this always returned on.
      State::Expanded
      | State::Ready
      | State::InFlight
      | State::Null
      | State::Leaf(_)
      | State::TypeName(_)
      | State::List => return 0,
    };
    if self.scratch_sets.is_empty() {
      // An exit that leaves the slot live, so it settles the count like every other. Nothing was
      // enqueued, so the value's reader set is empty the moment this returns and the object expires
      // here rather than at the next `start`.
      self.settle_pending(slot, 0);
      return 0;
    }
    // Taken before the collection, because the collection interns too: a response key belonging to
    // a field that never becomes a position must not be left spending a later sibling's arena.
    let mark = self.mark(slot);
    // `collect_fields` borrows the scratch; move it out so the borrow checker can see the fields
    // are disjoint, and put it back afterwards.
    let mut sets = core::mem::take(&mut self.scratch_sets);
    let mut scratch = core::mem::take(&mut self.scratch);
    let collected = collect_fields(
      self.schema,
      &mut self.fragments,
      object_type,
      &sets,
      ctx,
      &mut self.interner,
      &mut scratch,
      &mut self.visits,
      // The committed half of the population, so collection charges what is left of the same
      // ceiling `expand` and `fail_at` charge.
      Allowance::new(
        u64::from(self.limits.max_response_metadata.get())
          .saturating_sub(self.merged.len() as u64 + self.locations.len() as u64),
        self.limits.max_response_metadata.get(),
      ),
    );
    if let Err(fault) = collected {
      // The scratch goes back before the failure is recorded, because recording it borrows the
      // whole executor.
      sets.clear();
      self.scratch_sets = sets;
      self.scratch = scratch;
      // No child slot exists yet, but the walk interned as it went, and those names belong to
      // fields that will never be positions. Undoing them is what keeps the next sibling's arena
      // the size it is owed.
      self.restore(slot, mark);
      // Only now is there room to spell the name the message wanted, and only now is an id one that
      // will still be valid — which is why the fault carried bytes this far.
      let raw = match (fault.raw, fault.name) {
        // Collection refused for the metadata ceiling; it knows the ceiling but not the position,
        // so the position is attached here, where `owner` is in reach. Rendering then goes through
        // exactly the path a commit-side refusal does — including the root's, which must not say
        // `Query.Query`.
        (Raw::MetadataBudget { limit }, _) => {
          let (parent, field) = self.owner(slot);
          Raw::SelectionBudget {
            parent,
            field,
            limit,
          }
        }
        (
          Raw::DirectiveCondition {
            fault: ConditionFault::VariableMissing { .. },
          },
          Some(bytes),
        ) => Raw::DirectiveCondition {
          fault: ConditionFault::VariableMissing {
            // `.ok()` on purpose, and the only kind of site where discarding the refusal is right:
            // the spelling is decoration on a finding that does not depend on it, so both ceilings
            // shorten the same sentence and the reader is not told about a resource at all. The
            // sites that *name* a ceiling carry the refusal instead — see `Unstored`.
            //
            // A spelling that is not a name shortens the same sentence, through `variable_key`'s
            // `None` rather than through a ceiling. It used to be interned raw and rendered as
            // `""` on the way out, so the honest branch printed `$` with nothing after it — see
            // al8n/smear#139.
            variable: variable_key(bytes)
              .and_then(|name| self.interner.intern(name, &mut self.visits).ok()),
          },
        },
        (raw, _) => raw,
      };
      self.fail_at(slot, raw, fault.location);
      // Zero, because a refused collection produced no `collectedFieldsMap` at all. Draft §6.2.3.1
      // does not read this number on that path — the root's discard flag answers first, and with a
      // different refusal — so this is the truthful value rather than a load-bearing one.
      return 0;
    }

    // Draft §6.2.3.1 step 5's `collectedFieldsMap` size, read before the commit loop can refuse
    // any of it.
    let entries = scratch.groups().len();

    // Counted, not inferred from the group list: a `__typename` child is never enqueued and a
    // group whose field the schema does not define is skipped, so "groups collected" and "reads
    // that will happen" are different numbers.
    let mut enqueued = 0u32;
    let mut exhausted: Option<(Exhausted, SimpleSpan)> = None;
    for group in scratch.groups() {
      let selections = &scratch.fields[group.start as usize..(group.start + group.len) as usize];
      let first = selections[0].1;
      let Some(sym) = self.schema.sym(name_bytes(first)) else {
        // Draft 5.3.1 makes a field the type does not define a validation failure, so a validated
        // document cannot reach this. Omitting the key is the only response that cannot invent one.
        continue;
      };
      let Some(definition) = self.schema.field(object_type, sym) else {
        continue;
      };
      // Decided before a single entry is appended, and it has to be: a group that is refused
      // halfway leaves entries no slot points at, and nothing ever reclaims them, so a client
      // repeating a near-miss request would grow the executor without ever tripping a ceiling.
      // Checking first also means there is nothing to roll back.
      //
      // The two `as u32` below are made total by the same comparison rather than by an argument
      // about the ceiling: the conversion is attempted, and a length that will not narrow is
      // refused through the same door as a length that will not fit.
      // One ceiling over one population. Charging `merged` and `locations` separately against the
      // same number, as this did before, made it neither bound it claimed: `fail_at` fed
      // `locations` without ever consulting it, so error spans could exhaust the *merged* ceiling
      // and the message would name merged selections when what filled it was error locations.
      // Counting the two together, and charging every writer, is the only version of this that
      // does not lie.
      let (Ok(start), Ok(located)) = (
        u32::try_from(self.merged.len()),
        u32::try_from(self.locations.len()),
      ) else {
        exhausted = Some((Exhausted::Selections, group.first));
        break;
      };
      // Defence, not the live path, since collection began charging this ceiling at the staging
      // buffer: collection admits a selection set only if `2 * fields.len()` fits, and the loop
      // here charges `2 * group.len` per group, which sums to exactly that — so this cannot fire
      // for a set that got past collection. It stays because the rule is that *every* writer of a
      // population charges it, and because the guarantee upstream is an argument rather than a
      // type. What is *not* defence is the position and name arms below, which collection cannot
      // pre-empt; those are what `a_position_refusal_mid_expansion_costs_its_siblings_nothing`
      // pins, after the metadata tests moved off this path and left it briefly untested.
      if !self.metadata_room(u64::from(group.len) * 2) {
        exhausted = Some((Exhausted::Selections, group.first));
        break;
      }
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
          // A `__typename` whose answer cannot be stored must not become a field with no name or
          // a name that is somebody else's: the position simply cannot be built.
          let interned = match self
            .interner
            .intern(self.schema.name(name), &mut self.visits)
          {
            Ok(interned) => interned,
            Err(unstored) => {
              exhausted = Some((Exhausted::Unstored(unstored), group.first));
              break;
            }
          };
          Some(interned)
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
        exhausted = Some((Exhausted::Positions, group.first));
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
        enqueued += 1;
      }
    }

    sets.clear();
    self.scratch_sets = sets;
    self.scratch = scratch;

    // The live exit. A refused expansion falls through to the block below instead, and needs no
    // count: it ends in `fail` on this very object, and the discard that follows overwrites the
    // state — value, count and all — so there is nothing left to account.
    if exhausted.is_none() {
      self.settle_pending(slot, enqueued);
    }

    // Recorded after the scratch is home, and at the object rather than at the field that could
    // not be created: the field has no slot, so there is no position to hang a path on, and the
    // object is the position that could not be assembled. That is the same choice an unreadable
    // `@skip` condition makes two doc comments up, for the same reason.
    if let Some((which, location)) = exhausted {
      // Everything the groups before this one committed goes back. They were about to be nulled
      // with the parent anyway, so the response is unchanged — what changes is that their charges
      // stop being held against whatever the driver resolves next.
      self.restore(slot, mark);
      let (parent, field) = self.owner(slot);
      let raw = match which {
        Exhausted::Positions => Raw::ResponseBudget {
          parent,
          field,
          limit: self.limits.max_response_slots.get(),
        },
        Exhausted::Selections => Raw::SelectionBudget {
          parent,
          field,
          limit: self.limits.max_response_metadata.get(),
        },
        // The ceiling arrived with the refusal, so there is no second place for the number to come
        // from and no way to take it from the wrong one.
        Exhausted::Unstored(Unstored::Arena { limit }) => Raw::NameStorage { limit },
        Exhausted::Unstored(Unstored::Budget { limit }) => Raw::CollectionBudget { limit },
      };
      // `fail_at` rather than `fail`, and the span is the refused selection's own.
      //
      // `fail` reads the slot's stored locations range, and the *root's* range is empty by
      // construction — `start` builds it as `(0, 0)` because the root has no field to have a span.
      // So a budget tripped at the top level reached the driver with `locations()` empty, against
      // this crate's own contract that every observable error has at least one. Pointing at the
      // selection that was refused fixes the root and sharpens every other budget error with it:
      // the narrower node rather than the parent's whole merged group, which is the line draft
      // §6.4.1's argument errors already draw.
      self.fail_at(slot, raw, location);
    }
    entries
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
      //
      // `variable`'s inner `Option<&str>` is the spelling *as a lookup key*, and `None` there is
      // draft §6.3's answer read at draft §6.4.1's site: a spelling `variable_key` cannot express
      // names no variable the request could have supplied, so step 5.d's `hasValue = false` is the
      // conclusion and the driver is not asked. This used to be
      // `from_utf8(..).unwrap_or("")`, which asked the driver about a variable named `""`, put
      // that invented name in `ArgumentSource::Variable` for the resolver to read, and printed it
      // as `$` with nothing after it — while §6.3's condition, three hundred lines away, refused
      // the same input. al8n/smear#139.
      let (variable, has_value, is_null) = match supplied.map(|argument| argument.value()) {
        None => (None, false, false),
        Some(InputValue::Variable(spelled)) => {
          match variable_key(spelled.name().source().as_ref()) {
            // A variable *was* written, so this is step 5.d and not step 5.b: the outer `Some`
            // keeps the error `ArgumentVariableMissing` rather than `ArgumentMissing`, which is
            // the truthful one — the argument names a variable, and the variable has no value.
            None => (Some((None, None)), false, false),
            Some(name) => match ctx.variable(name) {
              None => (Some((Some(name), None)), false, false),
              Some(value) => {
                let null = ctx.is_null(&value);
                (Some((Some(name), Some(value))), true, null)
              }
            },
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
          self.scratch_args.push(Argument {
            name: self.schema.name(argument.name()),
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
            let location = value_span.unwrap_or(*node.span());
            // Not `let Some(..) else`: the argument is missing either way, and a table with no
            // room must shorten this message rather than replace it with one about storage.
            //
            // The spelling is the *document's*, which is what made this the counterexample to the
            // claim that only schema and driver bytes reached the uncharged path. There is no
            // uncharged path now, so the site charges like any other and shortens when it cannot.
            //
            // Which ceiling refused is deliberately dropped here, as at the condition fault above:
            // the argument is missing whatever the answer, so there is no knob to recommend and
            // nothing to say about a resource. That is the whole set of sites where `.ok()` is the
            // decision rather than an omission.
            //
            // `and_then` for the same reason: a spelling that is not a name shortens the sentence
            // exactly as a full arena does, and for a better reason — there is nothing to quote.
            let variable = name.and_then(|name| self.interner.intern(name, &mut self.visits).ok());
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

      let source = match (variable, supplied.map(|argument| argument.value())) {
        // The inner `Some` is the reader: `has_value` is true, so the key was readable and the
        // driver answered about it. An unreadable one never reaches here — it left at step 5.d.
        (Some((Some(variable), Some(value))), _) => ArgumentSource::Variable {
          name: variable,
          value,
        },
        (None, Some(literal)) => ArgumentSource::Literal(literal),
        // `has_value` is true, so one of the arms above always matches: a variable with no value
        // returned at the step 5.d branch above — whether the driver said so or the spelling was
        // never a key — and an absent argument at 5.b. The fourth combination is not a state the
        // read above can construct at all: a spelling that is not a key is never handed to the
        // driver, so it cannot come back carrying a value.
        (Some((_, None)), _) | (Some((None, Some(_))), _) | (None, None) => continue,
      };
      self.scratch_args.push(Argument {
        // The *schema's* name for the argument, which its own arena already holds as a `str`: the
        // builder admits nothing outside draft §2.1.9 (`smear_schema::is_name`), so there is no
        // conversion to get wrong. The two `from_utf8(..).unwrap_or("")`s that used to stand here
        // read the same arena the long way round.
        name: self.schema.name(argument.name()),
        ty,
        source,
      });
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
    // An argument error's own span is one more metadata entry, so it is charged like any other.
    // When there is no room the error is still raised — a refusal to record *where* something went
    // wrong must never become a refusal to say that it did — and it falls back to the field's own
    // locations, which are already stored. That is the same range `fail` uses and the same one
    // §6.4.3's errors carry, so the degradation is a loss of precision and not of meaning.
    //
    // The root is the one position whose stored range is empty, and it can fail at most once per
    // operation, so the fallback below overshoots the ceiling by at most a single entry.
    let stored = self.meta[slot as usize].locations;
    if !self.metadata_room(1) && stored.1 > 0 {
      self.record(slot, raw, stored);
      return;
    }
    let Ok(start) = u32::try_from(self.locations.len()) else {
      self.record(slot, raw, stored);
      return;
    };
    self.locations.push(location);
    self.record(slot, raw, (start, 1));
  }

  /// Whether `more` metadata entries fit under [`Limits::max_response_metadata`].
  ///
  /// The population is `merged` and `locations` together, because they are one resource: both are
  /// per-response-key bookkeeping, both are written by `expand`, and a ceiling that counted only
  /// one of them would be a ceiling on half a thing.
  #[inline]
  fn metadata_room(&self, more: u64) -> bool {
    let used = self.merged.len() as u64 + self.locations.len() as u64;
    used + more <= u64::from(self.limits.max_response_metadata.get())
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
    // [`Limits::max_in_flight`] promises that abandoned entries **narrow** the ceiling and never
    // close it, and this is the only place in the executor that can move an entry into that count.
    // So the promise is one predicate at one site, and this is the site.
    //
    // It is a property of *where* a field error may be raised rather than of anything here:
    // `abandon_under` moves entries from `live` to `abandoned` and leaves the sum alone, so what
    // the bound needs is that every path into `record` already holds a free slot.
    // [`handle_resolved`](Self::handle_resolved) and
    // [`handle_field_error`](Self::handle_field_error) release their own request before they can
    // fail; a draft §6.4.1 argument error is raised past
    // [`poll_resolve`](Self::poll_resolve)'s gate, which has already refused to pop at the ceiling;
    // and [`expand`](Self::expand)'s budget refusals run from the root with nothing outstanding at
    // all. Move the gate behind `coerce_arguments` and the third of those stops holding: an
    // argument error at a full ceiling would promote every live sibling, and `poll_resolve` would
    // go on returning `None` to a driver that had answered everything it was asked.
    //
    // Three paths, three different reasons, one predicate — which is why it is checked rather than
    // argued. Debug-only: the same argument bounds the release build, and nothing here is a
    // memory-safety condition.
    debug_assert!(
      self.live + self.abandoned < self.limits.max_in_flight.get(),
      "draft §6.4.4 closed the in-flight ceiling: {} live and {} abandoned at a ceiling of {}",
      self.live,
      self.abandoned,
      self.limits.max_in_flight,
    );
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

  /// How many units of [`Limits::max_selection_visits`] this operation has spent.
  ///
  /// The gate that pins collection's *cost* reads this. Nothing outside the crate can: a quadratic
  /// collection and a linear one produce the same response, so the only black-box separator is a
  /// clock, and a clock is a flaky gate rather than a strict one.
  #[cfg(test)]
  fn collection_work(&self) -> u32 {
    self.visits.spent()
  }

  /// Entries the fragment lookups have compared, counted independently of what they charged.
  ///
  /// The two agree whenever the charge is taken *before* the comparison, which is the property — so
  /// the budget cannot be the witness for it, a version charging afterwards agreeing with itself
  /// just as well. This count is the only thing that separates them.
  #[cfg(test)]
  fn fragment_compares(&self) -> u64 {
    self.fragments.compares()
  }

  /// Definitions the fragment index pass has read, counted independently of what it charged.
  ///
  /// One walk is what the pass is charged for, and the charge is spent up front out of a slice
  /// length — so the budget reads the same over one walk and over two, and a version that walked
  /// the definitions again to sieve the fragments out of them agrees with itself. This count is the
  /// only thing that separates them. See `collect::table`.
  #[cfg(test)]
  fn fragment_definitions_walked(&self) -> u64 {
    self.fragments.walked()
  }

  /// Entries the name lookups have compared, counted independently of what they charged.
  #[cfg(test)]
  fn interner_compares(&self) -> u64 {
    self.interner.compares()
  }

  /// Definitions the draft §6.1 operation lookup has read, counted independently of what it
  /// charged. See the field.
  #[cfg(test)]
  fn operation_definitions_walked(&self) -> u64 {
    self.operation_definitions_walked
  }

  /// Links draft §6.2.2's serial release has followed. See [`serial_next`](Self::serial_next).
  ///
  /// The separator for the one product a mutation could have brought back: a scan over the root's
  /// children per offer, or a walk of the running subtree per event, answers every query and
  /// mutation exactly as the cursor does. Only a count says which one ran.
  #[cfg(test)]
  fn serial_steps(&self) -> u64 {
    self.serial_steps
  }

  /// What this operation has charged against each of the four cumulative ceilings.
  ///
  /// One value rather than four accessors, so the gate compares all four in one `assert_eq!` and
  /// cannot quietly compare three. Nothing here makes a *fifth* ceiling automatic: adding one to
  /// [`Limits`] still means adding a field, and `collect::Visits` records that obligation with the
  /// rest of what the gate does not reach on its own.
  #[cfg(test)]
  fn charges(&self) -> Charges {
    Charges {
      visits: self.visits.spent(),
      slots: self.slots.len(),
      metadata: self.merged.len() + self.locations.len(),
      interned: self.interner.names().len(),
    }
  }

  /// What every buffer this executor carries across a [`reset`](Self::reset) is holding.
  ///
  /// The lifetime census's mechanical half. [`Charges`] reads what an operation spent, this reads
  /// what is still allocated afterwards, and only the second can see a stream that grows without
  /// end — `reset` clears without shrinking on purpose, so the reported quantities go back to zero
  /// while the capacities do not.
  #[cfg(test)]
  fn retained(&self) -> Retained {
    Retained {
      slots: self.slots.capacity(),
      meta: self.meta.capacity(),
      merged: self.merged.capacity(),
      locations: self.locations.capacity(),
      interner: self.interner.capacity(),
      errors: self.errors.capacity(),
      inflight: self.inflight.capacity(),
      scratch: self.scratch.capacities(),
      scratch_sets: self.scratch_sets.capacity(),
      scratch_args: self.scratch_args.capacity(),
      fragments: self.fragments.reserved(),
    }
  }

  /// Entries the fragment index has reserved room for, which no ceiling bounds and
  /// [`reset`](Self::reset) does not clear.
  ///
  /// Not part of [`Charges`], and deliberately: the table is *meant* to outlive an operation, so
  /// its capacity is not a quantity two operations have to agree on. What must hold is narrower —
  /// a **refused** pass reserves none of it. See `collect::table`.
  #[cfg(test)]
  fn fragment_reserved(&self) -> usize {
    self.fragments.reserved()
  }

  /// Puts every per-operation population back to empty, keeping only what is a function of the
  /// document or the schema.
  ///
  /// # What must not survive is the answer, and the gate for that is a totals comparison
  ///
  /// Some state here is kept on purpose — the fragment index, the interner's and the scratch's
  /// capacities, the epoch — because it is either a function of the document or an allocation a
  /// reused executor should not repay. The rule that keeps that from becoming a defect is that
  /// **nothing kept may make a second operation cheaper than the first**: a ceiling a client clears
  /// by sending the same request twice is not a ceiling.
  ///
  /// It has already been broken once, by the charge for the fragment pass being remembered beside
  /// the table it paid for. `a_second_operation_charges_what_the_first_did` is the general gate for
  /// the class, and it reads all four cumulative ceilings — see `collect::Visits` for what that
  /// covers and, precisely, what it does not.
  fn reset(&mut self) {
    self.slots.clear();
    // A fact about the population above, so it dies with it. A subscription event that inherited
    // the previous event's deepest chain would have the next serialiser reserve for a response that
    // is no longer in the tree, which is this rule's own "nothing kept may make a second operation
    // cheaper than the first" read in the direction where the cost is memory rather than admission.
    self.max_depth = 0;
    self.meta.clear();
    self.merged.clear();
    self.locations.clear();
    self.visits = Visits::new(self.limits.max_selection_visits.get());
    self.interner.clear();
    self.interner.set_cap(self.limits.max_interned_bytes.get());
    self.errors.clear();
    // Draft §7.1.7's entry belongs to the response one operation produced, so it is per-operation
    // by this function's own rule: a function of neither the document nor the schema. Dropping it
    // here is also the release — it is a driver value the response tree does not hold, so nothing
    // else would ever let go of it — and it is why `set_extensions` says to call it *after*
    // `start`.
    self.extensions = None;
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
    self.deferred = NONE;
    self.serial_next = NONE;
    // The operation kind goes with the operation, and every caller that is *not* ending one writes
    // it back on the next line. That is deliberate rather than awkward: `Idle` and `!started` mean
    // the same thing, so leaving the phase alone here would let a `start` that refuses at draft
    // §6.1 report a subscription that is no longer running — and it is what makes `on_entry`'s
    // `Creating` test unable to survive the reset that retires the very arguments it protects.
    self.phase = Phase::Idle;
    self.started = false;
    self.delivered = false;
  }

  /// Draft §6.1 `GetOperation`: the operation `name` selects, charged **before** each definition
  /// it reads.
  ///
  /// # It is a walk over a quantity the client controls, so it is charged like every other one
  ///
  /// This used to be the module's one uncharged scan (al8n/smear#144), performed once per
  /// [`start`](Self::start) over `definitions()` — an amount the *document* chooses, against no
  /// ceiling at all. Everything else that walks a document-sized population charges
  /// [`max_selection_visits`](Limits::max_selection_visits) for it, and this now does too, in the
  /// unit the rest of the module uses: one per definition.
  ///
  /// **Charged per definition rather than `definitions.len()` up front**, which is where it
  /// departs from `collect::Fragments::build`'s counting pass, and the difference is a wrong
  /// answer rather than a rounding. That pass reads every definition, so a slice length is what it
  /// costs; this one **early-returns** on a named match, so a document of ten thousand definitions
  /// whose named operation is the third costs three. Charging the length would refuse a lookup
  /// that had budget for the work it was actually going to do. Taking a unit before each read
  /// keeps "units spent" equal to "definitions read" at every instant, which is also what lets the
  /// walk be abandoned at the ceiling instead of after it.
  ///
  /// # Preferring an index was considered and is a loss here, which is worth recording
  ///
  /// The standing preference is to delete a walk rather than price it. An index over the
  /// operations would have to be built by the same walk, and it can only live on the **executor** —
  /// `Executor` borrows the document and cannot attach anything to it. So it amortises only across
  /// repeated [`start`](Self::start) calls on one executor, and it is pure loss under the pattern
  /// `collect::Fragments` documents, where a service caches the *parse* and builds an executor per
  /// execution: there the table is built once, probed once, and dropped. The charge is right under
  /// both patterns; the index is right under one.
  ///
  /// What the walk being unnecessary *did* delete is the second lookup. This returned a `u32` that
  /// `start` immediately resolved back through `definitions()` and re-matched against
  /// [`ExecutableDefinition::Operation`] — an arm that cannot fail, guarded by a refusal that
  /// cannot run — which is the same shape `collect::Fragments` removed by carrying the definition
  /// instead of a key to one. It also removes an `as u32` narrowing whose only bound was the
  /// document's own length, which is the row [`Limits::max_response_slots`]'s table no longer has
  /// to answer for.
  fn operation_definition(
    &mut self,
    name: Option<&str>,
  ) -> Result<&'a OperationDefinition<S>, StartError> {
    let mut found = None;
    for described in self.document.definitions() {
      if !self.visits.take(1) {
        return Err(StartError::OperationLookupRefused);
      }
      #[cfg(test)]
      {
        self.operation_definitions_walked += 1;
      }
      let ExecutableDefinition::Operation(operation) = described.node() else {
        continue;
      };
      match name {
        None => {
          if found.is_some() {
            return Err(StartError::AmbiguousOperation);
          }
          found = Some(operation);
        }
        Some(wanted) => {
          if let OperationDefinition::Named(named) = operation
            && named
              .name()
              .is_some_and(|name| name.source().as_ref() == wanted.as_bytes())
          {
            return Ok(operation);
          }
        }
      }
    }
    match (name, found) {
      (Some(_), _) => Err(StartError::UnknownOperation),
      (None, Some(operation)) => Ok(operation),
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
  /// that loop, and the next phase to grow a position would have to remember to write it again.
  /// Returning `None` from here makes forgetting impossible: a caller has to answer the empty case
  /// to get an index at all.
  ///
  /// **Both phases that were predicted to add a caller have landed and neither did.** Draft
  /// §6.2.2's mutation path re-orders when the top-level fields are *offered* and creates not one
  /// position the root's own expansion did not already create. Draft §6.2.3's per-event collection
  /// creates positions through the very same `expand` a query's root does, because
  /// [`handle_source_event`](Executor::handle_source_event) resets and re-enters
  /// `root_selection_set` — so an event is bounded because it *is* an execution, not because a
  /// second site remembered the ceiling. That is the property this design was for, and it is worth
  /// recording that it held twice rather than leaving the prediction standing.
  ///
  /// The `as u32` below is exact for the same reason. `self.slots.len()` is strictly less than the
  /// ceiling whenever this returns `Some`, and the ceiling is a `u32`, so the new index is at most
  /// `max_response_slots - 1` — never [`NONE`], which is `u32::MAX` and means "no such slot".
  ///
  /// # Where a path's length is written down
  ///
  /// This is also the only place a position's depth is decided, and it costs an increment because
  /// the parent is in hand. That is the whole reason it is recorded here rather than counted later:
  /// draft §7.1.2's order is the reverse of the links, so a serialiser has to turn the chain around
  /// into a buffer, and a buffer with no size hint reallocates and copies its way up. Knowing the
  /// depth without walking is what makes that one exactly-sized allocation —
  /// [`Path::collect_into`](super::Path::collect_into) — instead of `log₂(depth)` of them with two
  /// live at the peak. Measuring the depth first and then filling would trade the quadratic this
  /// crate already removed for a constant factor of two over the same links.
  ///
  /// The addition cannot overflow: a depth is at most one less than the number of slots, and the
  /// ceiling checked above is a `u32`.
  ///
  /// The running maximum is kept in the same breath and for the same kind of reason one buffer
  /// down: a serialiser walking `data` on an explicit stack holds one frame per open container, and
  /// the deepest that stack gets is this number plus one. Both of the writer's per-level buffers
  /// are then sized before they are filled, from two numbers this line writes — one comparison
  /// against a walk of the finished tree. See [`max_depth`](Self::max_depth) for why it is an upper
  /// bound rather than an equality.
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
    let depth = self.slots[parent as usize].depth + 1;
    if depth > self.max_depth {
      self.max_depth = depth;
    }
    self.slots.push(Slot::new(parent, depth, key, ty, state));
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

  /// Where every budgeted table stood, so a refused expansion can be undone.
  fn mark(&self, slot: u32) -> Mark {
    Mark {
      slots: self.slots.len(),
      meta: self.meta.len(),
      merged: self.merged.len(),
      locations: self.locations.len(),
      interner: self.interner.mark(),
      errors: self.errors.len(),
      first_child: self.slots[slot as usize].first_child,
      last_child: self.slots[slot as usize].last_child,
      ready_head: self.ready_head,
      ready_tail: self.ready_tail,
    }
  }

  /// Puts every budgeted table back to `mark`.
  ///
  /// Truncation is sound because one `expand` call appends a *contiguous suffix* to each of them:
  /// it runs start to finish without another position being created in between, so there is no
  /// interleaving to make a hole. That is a property of this call site and not of the executor —
  /// `discard` cannot reclaim the same way, because a discarded subtree's positions are scattered
  /// among positions created after them.
  ///
  /// The ready chain is a linked list rather than a vector, so it is restored by pointer: every
  /// slot this expansion enqueued lies above `mark.slots` and disappears with the truncation, and
  /// the tail that preceded them is below it and survives to be re-terminated.
  fn restore(&mut self, slot: u32, mark: Mark) {
    // Nothing outside the truncated region may point into it. Two things could: an error row,
    // which the mark now covers, and the deferral cell — which is always settled at the entry
    // point before any of this runs, so it is empty rather than merely unlikely.
    debug_assert_eq!(
      self.deferred, NONE,
      "a restore ran with an object value parked for release; the parked slot may be inside the \
       region about to be truncated"
    );
    // A third thing could point in, and the argument that it cannot is about creation order while a
    // restore is about indices — so it is asserted rather than left as prose. Draft §6.2.2's cursor
    // names a top-level field, every one of which the root's own expansion created, and that is the
    // first expansion of the operation; the cursor is still `NONE` while that expansion runs, and
    // every later mark is taken above those slots.
    debug_assert!(
      self.serial_next == NONE || (self.serial_next as usize) < mark.slots,
      "a restore would truncate over the withheld top-level field at slot {}",
      self.serial_next
    );
    self.errors.truncate(mark.errors);
    self.slots.truncate(mark.slots);
    self.meta.truncate(mark.meta);
    self.merged.truncate(mark.merged);
    self.locations.truncate(mark.locations);
    self.interner.restore(mark.interner);
    self.slots[slot as usize].first_child = mark.first_child;
    self.slots[slot as usize].last_child = mark.last_child;
    if mark.ready_tail == NONE {
      self.ready_head = NONE;
    } else {
      self.slots[mark.ready_tail as usize].next_ready = NONE;
    }
    self.ready_head = mark.ready_head;
    self.ready_tail = mark.ready_tail;
  }

  /// Writes an expansion's read count onto the object, expiring it at once when there are none.
  ///
  /// Zero is the case that matters and the one a release-at-last-offer scheme never reaches: an
  /// object whose whole sub-selection is `__typename` enqueues nothing, so no offer will ever
  /// arrive to notice its reader set is empty. Settling on *every* live exit of `expand` — this
  /// one, and the empty-selection early return — is what catches it, and catches the root with it,
  /// because `start` expands the root through the same path.
  fn settle_pending(&mut self, slot: u32, enqueued: u32) {
    if let State::Object { pending, .. } = &mut self.slots[slot as usize].state {
      *pending = enqueued;
    }
    if enqueued == 0 {
      self.expire(slot);
    }
  }

  /// Everything a call in must release before it does anything else.
  ///
  /// Both members are lends that ended when the driver called back: the arguments of the request it
  /// was answering, and the object value the last offer parked. They are one function rather than
  /// two calls because a later phase's entry point — phase 2's mutation withholding, phase 5's
  /// `handle_source_event` — should inherit the whole discipline by calling one thing, not
  /// remember a list.
  ///
  /// That prediction came true and this comment did not stop it: §7.1.7's two setters were added
  /// without the call. [`Executor`]'s own header now carries the protocol and the table of what
  /// discharges it, and `tests::every_public_entry_point_declares_its_discharge` derives the method
  /// set from this file's source so a new one cannot join silently.
  ///
  /// # The one state where the arguments are not a previous call's
  ///
  /// `retire_arguments` releases *a lend that has ended*, and while draft §7.1.2's response stream
  /// is [`Creating`](ResponseStream::Creating) it has not. Those arguments are §6.2.3.1 step 8's,
  /// coerced by [`start`](Self::start) and lent by [`source_field`](Self::source_field) for as long
  /// as the state lasts — so their owner is the state, not an offer, and every transition out of it
  /// resets, which retires them. That keeps [`Limits`]'s rule 1 intact: the release is a transition
  /// on the owner rather than a call at a site.
  ///
  /// Without the test this is a **silent wrong answer** rather than a leak, which is why it is a
  /// condition here and not a note somewhere. Six entry points are no-ops while the stream is being
  /// created — `poll_resolve`, `poll_response`, `poll_abandoned`, `set_extensions`,
  /// `take_extensions`, and either `handle_*` with a stale id — and every one of them would empty
  /// `scratch_args` on its way through. `source_field` would then hand the driver a source field
  /// with **no arguments** and no way to tell that from a field that has none, and the driver would
  /// resolve the wrong event stream. `a_call_in_while_creating_keeps_the_source_field_arguments`
  /// is the gate.
  fn on_entry(&mut self) {
    self.settle_deferred();
    if !matches!(
      self.phase,
      Phase::Subscription {
        stage: Stage::Creating,
        ..
      }
    ) {
      self.retire_arguments();
    }
  }

  /// Drops the object value parked by the previous `poll_resolve`, now that its lend has ended.
  ///
  /// Total by construction: if the parked slot was discarded in between, its state is already
  /// `Null` and there is nothing left to release.
  fn settle_deferred(&mut self) {
    let slot = core::mem::replace(&mut self.deferred, NONE);
    if slot != NONE {
      self.expire(slot);
    }
  }

  /// Transitions an object whose last future read has departed, dropping its value.
  ///
  /// The transition **is** the release — `Expanded` holds no `V` — which is why there is no
  /// `release_object` to forget to call. Writing the state is the whole operation.
  fn expire(&mut self, slot: u32) {
    if matches!(self.slots[slot as usize].state, State::Object { .. }) {
      self.slots[slot as usize].state = State::Expanded;
    }
  }

  /// Records that one enqueued child has left Ready, and expires the parent if it was the last.
  ///
  /// `deferred` is false when the caller still holds a borrow of the value — the offer path — and
  /// true when it does not. Decrement-if-`Object` makes it total: a parent discarded by the very
  /// propagation that removed this child is already `Null` and has nothing to account.
  fn child_departed(&mut self, parent: u32, defer: bool) {
    let State::Object { pending, .. } = &mut self.slots[parent as usize].state else {
      return;
    };
    *pending = pending.saturating_sub(1);
    if *pending == 0 {
      if defer {
        self.deferred = parent;
      } else {
        self.expire(parent);
      }
    }
  }

  /// Cuts the ready chain after its head, so draft §6.2.2's top-level fields are offered one at a
  /// time.
  ///
  /// Called once, from `start`, on a mutation's root expansion and nowhere else. Everything after
  /// the head is still a `next_ready` list — it is simply no longer one `ready_head` reaches, which
  /// is what makes the withholding structural: `poll_resolve` has nothing to decline, because there
  /// is nothing there to decline.
  ///
  /// **The root's read count is deliberately not touched.** `expand` set it to the number of
  /// children it enqueued, and every one of them still departs Ready exactly once — later, through
  /// [`release_serial`](Self::release_serial) — so the count that keeps the root's value readable
  /// across all of them is already right. Counting "children currently on the chain" instead would
  /// expire the root after the first offer and lend the second field a value that has been dropped,
  /// which is the enqueue-path-that-skips-the-count failure [`Limits`]'s lifetime table names.
  fn withhold_serial(&mut self) {
    let head = self.ready_head;
    if head == NONE {
      return;
    }
    // What is cut has to *be* the root's expansion. Nothing else can be on the chain today —
    // `reset` empties it and this runs immediately after the operation's first expansion — but that
    // is an argument about call order, and a later phase that enqueued anything before `start`'s
    // `expand` would cut in the wrong place and withhold something that is not a top-level field.
    // Once per operation, in debug, over a chain the document's size bounds.
    #[cfg(debug_assertions)]
    {
      let mut cursor = head;
      while cursor != NONE {
        debug_assert_eq!(
          self.slots[cursor as usize].parent, 0,
          "the ready chain held something other than the root's own children when draft §6.2.2's \
           cut ran"
        );
        cursor = self.slots[cursor as usize].next_ready;
      }
    }
    self.serial_next = self.slots[head as usize].next_ready;
    self.slots[head as usize].next_ready = NONE;
    self.ready_tail = head;
  }

  /// Splices the next withheld top-level field back onto the ready chain, if draft §6.2.2 permits
  /// it yet.
  ///
  /// The permission is one question — *can anything still under the previous top-level field
  /// affect the response* — and withholding is what makes it two counters instead of a walk:
  /// nothing outside that subtree has ever been on the ready chain or counted `live`, so an empty
  /// chain with no live request **is** that question answered. A queue would not have the property,
  /// because the other fields' work would be sitting in the same structures.
  ///
  /// # The guarantee is "complete or cancelled", and the difference is not cosmetic
  ///
  /// `live` is the counter, and draft §6.4.4 takes requests *out* of it: when a nested non-null
  /// fails, [`abandon_under`](Self::abandon_under) moves everything still outstanding beneath the
  /// nulled position from `live` to `abandoned`. So this can, and does, splice the next top-level
  /// field while the previous one still has entries in the in-flight table. Two counters therefore
  /// answer "nothing left that can reach the response", which is strictly weaker than "the subtree
  /// has finished", and the weaker sentence is the one every other statement of this rule makes —
  /// the `Limits` product argument included, since it is that weaker question the counters are
  /// cheap for.
  ///
  /// Waiting for `abandoned` too was the alternative and it was rejected on evidence, not taste:
  ///
  /// - [`poll_response`](Self::poll_response) already applies the same reasoning — §6.4.4 has
  ///   discarded the positions those requests would have filled, so no answer to one can change a
  ///   byte of the response.
  /// - **It adds no wait on work the driver has been told to stop doing, and waiting would.**
  ///   Under a gate on `abandoned` the next top-level field is reachable exactly two ways, and
  ///   both of them are that work: retiring the entries through
  ///   [`poll_abandoned`](Self::poll_abandoned), or *answering* them, since
  ///   [`release`](Self::release)'s `Slotted::Abandoned` arm decrements the count for a late
  ///   [`handle_resolved`](Self::handle_resolved) or
  ///   [`handle_field_error`](Self::handle_field_error) —
  ///   `answering_an_abandoned_request_frees_its_in_flight_entry` is that second path as a test.
  ///   There is no third: a larger ceiling does not open the gate, which is on the count and not
  ///   on the room. So a driver that neither polls the channel nor completes the cancelled work is
  ///   held there for good. Without the gate there is a cost but no wait: an undrained entry holds
  ///   an in-flight slot, so the driver's effective ceiling is
  ///   [`max_in_flight`](Limits::max_in_flight) less the entries it has not retired. That
  ///   subtraction never reaches zero — a discard can only promote requests that were live
  ///   *besides* the one that caused it, and that one is always released first — so the ceiling
  ///   bottoms out at one request at a time and the mutation still runs to a response.
  ///   `an_undrained_abandonment_narrows_the_ceiling_without_stopping_the_mutation` measures both
  ///   halves at a ceiling of two, and `an_abandonment_can_never_take_the_last_in_flight_slot`
  ///   drives the bound to its edge.
  ///
  ///   The gate's failure is a wrong answer before it is a wait, which is worth knowing because a
  ///   gate is not what a reader expects that from. [`poll_response`](Self::poll_response) has no
  ///   other view of a withheld field than the splice below, so a `release_serial` that declined
  ///   would let it deliver — and what it delivers is a **null, not an absence**. §6.2.2's cut
  ///   takes a top-level field off the ready chain and off nothing else, so its slot is still the
  ///   root's child under its own response key, and `node` renders `State::Ready` as
  ///   `Node::Null`. On `mutation { one { text required } two { text } }` with `one.required`
  ///   failing, a release build under the gate returns `{"one":null,"two":null}` and one error, at
  ///   `one.required` — nothing anywhere in the response says `two` was never run, and every
  ///   top-level field after the first goes the same way.
  ///   `a_withheld_top_level_field_is_in_the_tree_and_reads_as_null` pins the two facts behind it:
  ///   the key is there, and it reads as null. Adding `|| self.abandoned != 0` to the condition
  ///   below is the whole of that
  ///   change: in a debug build it trips the held-object `debug_assert` in `poll_response`,
  ///   because the root still holds the value those withheld fields would have read, and it fails
  ///   the two tests named above plus
  ///   `an_abandoned_nested_request_does_not_withhold_the_next_mutation_field` — and nothing else
  ///   in the suite.
  /// - **The reference implementation does not wait, and this was measured rather than inferred
  ///   from `Promise.all`'s documented behaviour.** On
  ///   `mutation { first { outstanding failing } second { outstanding } }` against `graphql-js`
  ///   16.11.0, with `failing: Int!` resolving null through a promise and `first.outstanding`
  ///   returning a promise the probe settles by hand from a later macrotask, upstream's resolver
  ///   order is `first`, `first.outstanding`, `first.failing`, **`second`** — and only then does
  ///   `first.outstanding` settle. The control run, identical but with `failing` succeeding, puts
  ///   `second` *after* the deferral, which is what rules out a probe that never waits for
  ///   anything. Gating on `abandoned` would make this executor stricter than upstream.
  ///
  /// `an_abandoned_nested_request_does_not_withhold_the_next_mutation_field` is that measurement as
  /// a test. Upstream's own `mutations-test.ts` cannot decide it — the ported corpus stays green
  /// under either rule — which is why the case is hand-built rather than another oracle entry.
  ///
  /// # Why "finished" and not "answered"
  ///
  /// The other half of the same question, measured the same way, per this module's standing method.
  /// `graphql-js` 16.11.0's `executeFieldsSerially` awaits `executeField`, and `executeField`'s
  /// promise is `completeValue`'s — which for an object resolves only once every sub-field has. On
  /// `mutation { a: m { nested } b: m { nested } }` upstream's resolver order is `a`, `nested(a)`,
  /// `b`, `nested(b)`. Releasing on the answer alone would run `b`'s side effect while `a`'s
  /// sub-selection was still resolving.
  fn release_serial(&mut self) {
    if self.serial_next == NONE {
      return;
    }
    if self.live != 0 || self.ready_head != NONE {
      return;
    }
    while self.serial_next != NONE {
      self.serial_steps += 1;
      let slot = self.serial_next;
      self.serial_next = self.slots[slot as usize].next_ready;
      self.slots[slot as usize].next_ready = NONE;
      // Only reachable with the root itself discarded: a top-level field is flagged either by its
      // own failure — impossible before it has been offered — or by a discard rooted at the root,
      // which flags all of them at once. So the skip runs the cursor to its end in one call, which
      // is the same `M` steps the splices would have cost, and never leaves the root's read count
      // stranded, because a discarded root holds no count to strand.
      if self.slots[slot as usize].discarded {
        continue;
      }
      self.enqueue(slot);
      return;
    }
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
        // The driver answered a request draft §6.4.4 had already discarded. The value is still
        // dropped — the position it belonged to is gone — but the *entry* retires here, because an
        // answer is one of the events that ends a request's life and this is one.
        //
        // It used to stay abandoned, on the reasoning that retiring early would let the id be
        // reissued while the driver still held it. That reasoning was wrong, and `poll_abandoned`
        // is the proof: it retires an entry and hands the driver the id it just invalidated,
        // because `release_entry` bumps the generation and a stale id no longer matches. The same
        // mechanism covers this path.
        //
        // What the old behaviour cost: `poll_resolve` gates on `live + abandoned`, so a request the
        // driver had already completed went on consuming the in-flight ceiling until the driver
        // polled an abandonment for work that was finished. A driver that never polls that channel
        // is documented as running under a narrowed ceiling; it should not pay that for requests it
        // has answered.
        self.abandoned -= 1;
        self.release_entry(id.index);
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
///
/// Draft §7.1.8 *Additional Entries* closes the map — an execution result "must not contain any
/// entries other than those described above" — so this type's three accessors are the whole of the
/// response rather than a part of it. [`data`](Response::data) is always an entry,
/// [`errors`](Response::errors) is one when it is non-empty, and
/// [`extensions`](Response::extensions) is one when the driver attached a map.
pub struct Response<'r, V> {
  schema: &'r Schema,
  slots: &'r [Slot<V>],
  depth: u32,
  names: &'r str,
  name_spans: &'r [(u32, u32)],
  locations: &'r [SimpleSpan],
  errors: &'r [Row],
  extensions: Option<&'r Extensions<V>>,
}

impl<'r, V> Response<'r, V> {
  /// Returns the response's `data` entry.
  ///
  /// [`Node::Null`] is `"data": null`, which is what draft §6.4.4 produces when a non-null root
  /// field errors. It is never an *absent* `data`: the draft reserves that for a §7.1.3 *request
  /// error result*, and a `Response` is only ever built by
  /// [`poll_response`](Executor::poll_response), which runs after an execution began.
  ///
  /// **The reason is the construction and not the document, which is a correction.** This said "the
  /// executor cannot produce one because it is only entered with a validated document", and that is
  /// false — validity is not what stops it. A *valid* document with two named operations, started
  /// without an operation name, is draft §6.1 `GetOperation`'s ambiguity and returns
  /// [`StartError::AmbiguousOperation`]; so does a valid document plus a name it does not define.
  /// Those are request errors, they are reachable, and what makes them absent from this type is
  /// that [`start`](Executor::start) refuses *before* any response exists — the refusal is a
  /// `StartError` returned to the driver, not a response with no `data`.
  ///
  /// So the §7.1.3 result shape is real, and it is a **different type**:
  /// [`RequestErrorResult`](super::RequestErrorResult), which has no `data` accessor at all. The
  /// two result kinds are two maps in §7.1 and they are two types here, which is what stops a
  /// driver from reaching for this one — whose `data` is infallible — to answer a request that
  /// never ran.
  #[inline]
  pub fn data(&self) -> Node<'r, V> {
    node(self.slots, self.names, self.name_spans, 0)
  }

  /// Returns an upper bound on how deep this response is, in draft §7.1.2 path segments.
  ///
  /// `0` for a response whose `data` is a leaf or a `null`: the root has no segment, so it has no
  /// depth either.
  ///
  /// # What it is for, which is sizing a buffer and not reporting a statistic
  ///
  /// It is the number a serialiser needs before it can walk `data` without recursing. Such a walk
  /// holds one frame per open container, the deepest it can get is a container at the deepest
  /// position, and a frame is also open for the root — so **`depth() + 1` frames is the whole of
  /// what that stack can reach**, and reserving it once is the difference between one allocation
  /// and a doubling sequence with the old buffer and the new one both live at the peak. It is the
  /// same answer [`Path::len`](super::Path::len) gives for one error's path, asked of the response
  /// instead of a position, and it comes from the same field for the same reason: the executor
  /// wrote each depth down as it created the position, so this is a number rather than a walk of
  /// the finished tree.
  ///
  /// # An upper bound, and here is exactly where the slack is
  ///
  /// Three things put a position in this maximum that a reader will not descend into:
  ///
  /// - **A deepest position that is a leaf.** A leaf takes no frame, so a tree whose deepest
  ///   position is a `String` needs `depth()` frames rather than `depth() + 1`. One frame.
  /// - **A subtree draft §6.4.4 discarded.** A discard marks and leaves the positions in place —
  ///   that is what makes it cheap, see [`Node::Null`] — so a chain that was built and then nulled
  ///   still counts here while it renders as a single `null`. This one is not bounded by a
  ///   constant: a response that executed a deep chain and then nulled it reports the depth it
  ///   *had*.
  /// - **An expansion a ceiling refused.** The positions it created are truncated away and this is
  ///   not lowered with them, which is at most one level: a refused expansion's children are all
  ///   one deeper than the position it was refused at.
  ///
  /// All three are over-*reservation* and never under-reservation, which is the direction that
  /// matters for a caller sizing a buffer from it — a walk can never need more. The first and third
  /// are one level each; the second is the open-ended one, and it is bounded by
  /// [`Limits::max_response_slots`], the same ceiling that bounds the slab those positions are
  /// still sitting in, so it is inside the per-response memory a deployment has already sized.
  /// Removing it would take a live-position count per depth level, maintained on `push_child` and
  /// on every discard, which is a table on the executor's hot path to save a serialiser one
  /// allocation it is going to release immediately.
  #[inline]
  pub const fn depth(&self) -> usize {
    self.depth as usize
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

  /// Returns the response's draft §7.1.7 *Extensions* entry, or `None` when it has none.
  ///
  /// `None` is an absent key and never an empty map: §7.1.1 *Execution Result* makes the entry
  /// optional, and `Some(`[`Extensions`]`)` with [`is_empty`](Extensions::is_empty) true is the
  /// *present* empty map, which is a different response. A `Some` is what
  /// [`Executor::set_extensions`](Executor::set_extensions) was given, entry for entry and in the
  /// same order; §7.1.7 reserves the values for the implementer, so there is nothing here for
  /// `proto` to have interpreted.
  #[inline]
  pub fn extensions(&self) -> Option<&'r Extensions<V>> {
    self.extensions
  }
}

impl<V> fmt::Debug for Response<'_, V> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("Response")
      .field("data", &self.data())
      .field("errors", &self.error_count())
      // `Extensions`'s own `Debug` renders keys without requiring `V: Debug`, for the reason
      // `Node::Leaf` renders as `<leaf>`.
      .field("extensions", &self.extensions)
      .finish()
  }
}
