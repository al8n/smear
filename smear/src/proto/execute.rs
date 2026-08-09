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
  collect::{Allowance, Fragments, Interner, Scratch, Unstored, Visits, collect_fields},
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
/// | error rows | rows | derived: at most one per position | — | a position able to fail twice, or an argument coercion raising more than once |
/// | list nesting | — | [`MAX_WRAPPERS`](crate::validator::schema::MAX_WRAPPERS) = 15, by the schema | `complete`'s list arm strips one wrapper per level | — |
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
/// | in-flight entries | `max_in_flight` | `release` by id | answered, retired, or reset | slab free chain; epoch voids stale ids | an id honoured across epochs |
/// | visit budget | `max_selection_visits` | `walk` | end of operation | **never** — work done is spent | a refund |
/// | collection scratch | `max_response_metadata`, charged before each push | `expand`, this call only | cleared at the next collection | cleared, never shrunk — capacity is reused | a staged population charged against a *different* ceiling than the one that refuses it |
///
/// Three rules keep the table true as phases 2–8 add members, and each is a rule the rounds paid
/// for:
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
/// what draft §6.2.3's per-event collection and draft §6.3's mutation path both do. The remedy is
/// not a cleverer bound on either factor; it is a ceiling on the product itself, which is what
/// this knob is.
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
  /// It bounds outstanding *requests* and deliberately not retained *values*: a completed object's
  /// value is not in this slab, and what bounds its lifetime is the lifetime table's row 2, not this
  /// knob. Before that row was enforced the two were confused, and a pool of eight could be facing
  /// a hundred thousand live handles.
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
  /// | the operation index in `operation_index` | the document's own length, which no ceiling here bounds and nothing in this module creates |
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
  /// it compares it; and per definition and fragment the fragment index's one pass handles. The
  /// first is the difference between this and every other ceiling here, and it is deliberate: the
  /// others count what was produced, and a document built out of fragments that collect nothing
  /// produces nothing while walking as far as it likes.
  ///
  /// The rest is what makes the first a bound rather than a bound on one factor. Both tables hash
  /// text the *document* chose, and the shared multiply-fold is unkeyed and invertible, so a client
  /// can put every one of its names in one bucket deliberately. Charging each comparison first
  /// means it spends this ceiling doing so, and the guarantee does not rest on the hash behaving.
  /// In the ordinary case a lookup compares about one entry, so a document spends roughly twice
  /// what it used to — against a default of sixteen million.
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

/// Sixteen million selections examined, which no honest query approaches and no dishonest one
/// passes: a document large enough to spend it has to *be* large, and the parser and the network
/// have opinions about that long before this does.
const DEFAULT_SELECTION_VISITS: NonZeroU32 = NonZeroU32::new(1 << 24).expect("2^24 is not zero");

/// Sixteen megabytes: room for every distinct name a large document and schema can name, plus a
/// great many driver messages, and three orders of magnitude below where a `u32` offset stops
/// working.
const DEFAULT_INTERNED_BYTES: NonZeroU32 = NonZeroU32::new(1 << 24).expect("2^24 is not zero");

impl Default for Limits {
  #[inline]
  fn default() -> Self {
    Self {
      max_in_flight: DEFAULT_IN_FLIGHT,
      max_response_slots: DEFAULT_RESPONSE_SLOTS,
      max_response_metadata: DEFAULT_RESPONSE_METADATA,
      max_selection_visits: DEFAULT_SELECTION_VISITS,
      max_interned_bytes: DEFAULT_INTERNED_BYTES,
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

/// Which of [`Limits`]'s two response ceilings `expand` ran into.
///
/// Carried out of the loop rather than reported inside it, because recording a failure borrows the
/// whole executor while the scratch vectors are still moved out of it.
///
/// Two variants and one [`Kind`](super::Kind): a driver branching on the outcome only needs to know
/// that a ceiling was reached, and the remedy is the same either way, but the two *messages* have
/// to name the right knob — a single message would be wrong half the time.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Exhausted {
  Positions,
  Selections,
  Names,
  /// The name lookup had no visit budget left. Distinct from `Names` because a message naming the
  /// arena's ceiling when the *work* ceiling refused would send an operator to tune the wrong knob.
  Work,
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
  /// The document's named fragments, indexed by name so that a spread costs a probe rather than a
  /// scan of every definition.
  ///
  /// Indexed on the first fragment spread and **charged against
  /// [`max_selection_visits`](Limits::max_selection_visits) when it is**, then kept across every
  /// [`reset`](Self::reset). Indexing in the constructor instead made an executor built per
  /// execution pay `executions × definitions` before any ceiling existed, and filling a table from
  /// names the *document* chooses without charging for it is a hole a charged lookup does not
  /// close. See `collect::Fragments`.
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

  started: bool,
  delivered: bool,

  /// Everything one draft §6.3 collection reuses from the last. See `collect::Scratch`.
  scratch: Scratch<'a, S>,
  scratch_sets: std::vec::Vec<&'a SelectionSet<S>>,
  /// What is left of [`Limits::max_selection_visits`] for this operation.
  visits: Visits,
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
      meta: std::vec::Vec::new(),
      merged: std::vec::Vec::new(),
      locations: std::vec::Vec::new(),
      interner: Interner::new(limits.max_interned_bytes.get()),
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
      scratch_sets: std::vec::Vec::new(),
      deferred: NONE,
      visits: Visits::new(limits.max_selection_visits.get()),
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
    self.on_entry();
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
    let raw = match self.interner.intern(message.as_bytes(), &mut self.visits) {
      Ok(message) => Raw::Resolver { message },
      Err(_) => Raw::ResolverUnstorable,
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
    self.on_entry();
    if !self.started || self.delivered {
      return None;
    }
    self.drain_ready();
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
          let raw = match self.interner.intern(name.as_bytes(), &mut self.visits) {
            Ok(runtime) => Raw::AbstractNotPossible {
              abstract_ty: base,
              runtime,
            },
            Err(_) => Raw::AbstractNotPossibleUnnamed {
              abstract_ty: base,
              limit: self.interner.cap(),
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
      // Enumerated for the reason the offer path is. `Expanded` cannot appear because a slot
      // expands once; the rest are the not-an-object cases this always returned on.
      State::Expanded
      | State::Ready
      | State::InFlight
      | State::Null
      | State::Leaf(_)
      | State::TypeName(_)
      | State::List => return,
    };
    if self.scratch_sets.is_empty() {
      // An exit that leaves the slot live, so it settles the count like every other. Nothing was
      // enqueued, so the value's reader set is empty the moment this returns and the object expires
      // here rather than at the next `start`.
      self.settle_pending(slot, 0);
      return;
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
            variable: self.interner.intern(bytes, &mut self.visits).ok(),
          },
        },
        (raw, _) => raw,
      };
      self.fail_at(slot, raw, fault.location);
      return;
    }

    // Counted, not inferred from the group list: a `__typename` child is never enqueued and a
    // group whose field the schema does not define is skipped, so "groups collected" and "reads
    // that will happen" are different numbers.
    let mut enqueued = 0u32;
    let mut exhausted: Option<(Exhausted, SimpleSpan)> = None;
    for group in &scratch.groups {
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
            .intern(self.schema.name_bytes(name), &mut self.visits)
          {
            Ok(interned) => interned,
            Err(unstored) => {
              exhausted = Some((
                match unstored {
                  Unstored::Arena => Exhausted::Names,
                  Unstored::Budget => Exhausted::Work,
                },
                group.first,
              ));
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
        Exhausted::Names => Raw::NameStorage {
          limit: self.interner.cap(),
        },
        Exhausted::Work => Raw::CollectionBudget {
          limit: self.limits.max_selection_visits.get(),
        },
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
            let location = value_span.unwrap_or(*node.span());
            // Not `let Some(..) else`: the argument is missing either way, and a table with no
            // room must shorten this message rather than replace it with one about storage.
            //
            // The spelling is the *document's*, which is what made this the counterexample to the
            // claim that only schema and driver bytes reached the uncharged path. There is no
            // uncharged path now, so the site charges like any other and shortens when it cannot.
            let variable = self.interner.intern(name.as_bytes(), &mut self.visits).ok();
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

  /// Entries the name lookups have compared, counted independently of what they charged.
  #[cfg(test)]
  fn interner_compares(&self) -> u64 {
    self.interner.compares()
  }

  fn reset(&mut self) {
    self.slots.clear();
    self.meta.clear();
    self.merged.clear();
    self.locations.clear();
    self.visits = Visits::new(self.limits.max_selection_visits.get());
    self.interner.clear();
    self.interner.set_cap(self.limits.max_interned_bytes.get());
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
    self.deferred = NONE;
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
  fn on_entry(&mut self) {
    self.settle_deferred();
    self.retire_arguments();
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
        // is documented as stalling; it should not stall on requests it has answered.
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
