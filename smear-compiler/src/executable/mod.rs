//! Executable-document validation: the entry point, and the passes that do not ride the selection
//! walk.
//!
//! # The shape of a run
//!
//! 1. **Prep** records the operations and fragments, sorts a fragment index, and collects every
//!    fragment spread as an edge of the fragment graph. Draft 5.5.1.1 and 5.5.2.1 fall out of it.
//! 2. **Operation rules** (5.2.1.1, 5.2.2.1, 5.2.3.1) read the operation list.
//! 3. **Fragment declaration rules** (5.5.1.2, 5.5.1.3) read the fragment table.
//! 4. **The fragment graph** answers 5.5.2.2 and 5.5.1.4 — with integers, over an explicit stack.
//!    Cycles are established here, before anything expands a fragment, so no later rule can meet
//!    an unestablished graph. Unlike apollo-compiler, unused fragments are included: a rule that
//!    trusts the graph must be able to trust all of it.
//! 5. **Subscriptions** get 5.2.4.1's own root-field collection.
//! 6. **One selection walk per operation**, following spreads with a visited bitset, carries every
//!    remaining rule. Rules that are properties of a *definition* rather than of an operation fire
//!    the first time that definition is reached and are suppressed afterwards, so a fragment three
//!    operations share reports its bad field once. Rules that are properties of an *operation* —
//!    the variable rules — run every time, which is what the specification asks for: the same
//!    fragment may be valid under one operation's variables and invalid under another's.
//! 7. **A final pass over fragments no operation reached**, so their structure is validated too.
//! 8. **Draft 5.3.2's merge engine**, last of all, over every selection set the document defines.
//!    It is the only rule with a working set and the only one an attacker can make expensive, so
//!    every cheap structural refusal — undefined spreads, fragment cycles — is already established
//!    before it expands anything, and what it may spend is the caller's [`Budget`].
//!
//! # Every repair here was swept for siblings, and three of the sweeps found one
//!
//! al8n/smear#198 landed a work ledger over these passes and then spent two review rounds on
//! repairs that were true of the site they were written for and false of its twin. The pattern is
//! worth naming because it is not carelessness: each repair was correct, and each was written by
//! looking at the site the review named.
//!
//! - **A charge must sit in front of the work it prices.** The table on
//!   [`Validator::spend`] is that question asked of every pass — and it replaced an earlier table
//!   that asked "does this pass charge?", which every one of them did.
//! - **A coordinate resolver costs the depth it descends.** `values::walk_value` was repaired;
//!   `selections::resolve` — the same shape, three call sites, named in the same review sentence —
//!   was not, and stayed quadratic for a round.
//! - **A ledger's "off" must be a state.** [`Ledger::Off`] is closed under [`Ledger::take`]
//!   because `u32::MAX` as a large number is a budget the first charge shrinks. That defect has
//!   now been written four times in this repository under four different names.
//! - **A state that must not be read must not exist.** [`Ledger::Off`] carries no number,
//!   `LosslessInvalid::recovery` carries no [`Recovery`] when nothing was projected, and one
//!   `tripped` carries the whole of "a bound abandoned this run". Each replaced a value plus a
//!   caveat — a maximum documented as an absence, a count documented as a floor, a flag documented
//!   as needing a second flag read with it — and a caveat is only ever as good as the next reader.
//! - **A charge is three facts, not one.** Where it sits, what dimension it prices, and what gates
//!   it. The table on [`Validator::spend`] asks all three, and each time the question got sharper
//!   it found sites the previous version could not see: a charge counting **selections** in front
//!   of a comparison measured in **bytes**, and a charge gated on one of a list's two readers while
//!   the other read it for free.
//! - **The branch a document takes is the branch that decides what it owes.** A charge above a
//!   `match`, an `||`, an `any` or a `find_map` prices the arm that costs most, and the arms below
//!   it may cost nothing at all: an empty population searched with no comparisons, a first element
//!   stored rather than compared, a custom scalar accepted without being read, an intersection that
//!   overlaps in its first word. Four sub-shapes, and the audit found three more of them than the
//!   review named — the enum arm reached by a non-enum literal, 5.4.3's presence scan stopping at
//!   the argument it finds, and the `@skip`/`@include` test that reads seven bytes of a spelling
//!   however long it is. **A charge is correct only when every path below it performs the work it
//!   prices; otherwise it belongs on the arm that does.** Moving one there has a symmetric
//!   hazard — a per-step charge is sound only when the step it prices has not yet been read, which
//!   is why 5.5.2.3's intersection is a loop here rather than a `zip(..).any(..)` with a charge
//!   above it.
//! - **An over-charge is a wrong answer too, and it has two shapes.** Neither lets anything
//!   through, so neither moves a number the way a bypass does; both refuse honest documents.
//!   A charge sized to the work's **worst path** rather than its taken one — 5.5.2.3 billing the
//!   width of a bitset for a comparison that answers on `target == parent` and never reads one.
//!   And a prepayment whose **reader has moved** — the operation-name charge existed partly because
//!   5.2.1.1 cloned a spelling for free, and centralising that charge in [`Validator::subject`] left
//!   the prepayment billing every named operation for a rule that performs one `O(1)` lookup.
//!   Which implies a sweep of its own: a repair that gives a cost a home can leave every earlier
//!   charge that named it over-charging, so **a prepayment is redundant exactly when its only
//!   remaining reader is a clone**. Twenty-three prepayments in this module, one such, repaired;
//!   every other has a sort, a hash, a search or a scan behind it.
//! - **Setup is work, and a per-operation walk is where it hides.** A ledger charges what a pass
//!   examines; what a pass *prepares* has no row unless one is written for it. Every `reset_bits`,
//!   `resize`, `fill` and `clear` on a document-sized buffer therefore owes **three** numbers — how
//!   large, how many times, and **whether the allocation sits behind a charge that prices it**.
//!
//!   It owed two for five rounds, and the third is the one that was missing. Only two of the
//!   fourteen sites here were sized to the whole document *and* performed per operation: the
//!   per-walk "already entered" set, in [`Validator::walk_operations`] and in
//!   [`Validator::check_subscription_roots`]. [`Visited`](super::scratch::Visited)'s generation
//!   stamp deletes both at once rather than pricing either, which is why it was preferred to the
//!   charge. But draft 5.8.4's marks bitset answered *both* questions well — sized to the
//!   operation's own declaration list, once per operation — and was reset **before** the first
//!   charge that could refuse it, so a spent ledger still bought `V / 64` words of allocation and
//!   zeroing. Size and frequency are not an answer about ordering, and a row that gives them has
//!   not cleared the site. al8n/smear#198.
//!
//! # Every allocation whose size a caller decides
//!
//! Enumerated rather than audited, because three audits asked a question and missed a site the
//! question did not cover. The list is the artifact; the question is not.
//!
//! | site | sized by | behind which charge |
//! |---|---|---|
//! | `prep`'s `order.extend(0..count)` | fragments | prep, ≥ 1 unit per fragment, taken above it |
//! | `check_fragment_cycles`' two `reset_bits` | fragments / 64 | ” — over-prepaid 64× |
//! | `check_fragments_used`' `reset_bits` | fragments / 64 | ” — same |
//! | `begin_fragment`'s `grow_bits` | one fragment ordinal | ” — and it only ever grows |
//! | `check_variable_definitions`' `used.resize` | the operation's variables / 64 | **its own**, `count_units`, taken here |
//! | `Visited::begin`'s `stamps.resize` | fragments | prep — and it only ever grows |
//! | `Names::intern`'s `bytes.extend_from_slice` | the key | `Work::take_bytes`, taken above it |
//! | `Names::relink`'s `heads.resize` | names, ×2 + a 64 floor | `Work::take(id + 1)`, taken above it |
//! | `build_merge_index`'s two `resize`s | definitions + fragments | **its own**, `count_units` |
//! | `relink_memo`'s `merge_slots.resize` | memo entries, ×2 + a 64 floor | `charge(len + 1)` in `claim` |
//! | `Validator::index`'s `merge_buckets.resize` | the bucket width | `charge(width)`, saturating |
//!
//! Three things the table is saying. **Prepaid is an answer**, and the commonest one: a buffer
//! sized to a population the ledger has already been charged for, one unit at a time, needs no
//! charge of its own — and adding one would be an over-charge, which this module treats as a wrong
//! answer too. **The charged sites all read alike**: `clear`, then the true count, then
//! [`count_units`](super::scratch::count_units) — which saturates into a refusal rather than
//! truncating into a budget it fits — then the `resize`. And **a refusal leaves the buffer empty**,
//! never at the previous document's width, because the `clear` is unconditional and `O(1)` while
//! only the `resize` is work.
//!
//! Everything else that grows is one element per node already charged (`push` inside a loop with
//! its charge at the entrance), an `O(1)` `truncate` of a stack of integers, or a sort — `N log N`
//! comparisons against `N` units, a bounded constant multiple. The parser's projection has no
//! bulk-sizing site at all: every allocation there is one element per green node, and the lossless
//! door prepays `units(max(source.len(), parse.green().text_len()))` in front of the whole
//! projection.

//! # Every charge whose quantity is a depth
//!
//! The list above is the model, and this is the same enumeration over the other population an
//! audit can get wrong by asking about the wrong part of it. A depth **is** a population: the
//! question a list, a group or a bitset gets — *is the quantity the part traversed, or the whole?*
//! — is the same question, and two of these five answered "the whole".
//!
//! | charge | quantity | the part it walks |
//! |---|---|---|
//! | `resolve_frames` | `suffix_of(frames).len()` | the suffix — **was the whole stack** |
//! | [`Validator::resolve_roots`] | `suffix_of(roots).len()` | the suffix — **was the whole stack** |
//! | `walk_value` | `depth - base` | the slice it then resolves, written once |
//! | `resolve_merge_set` | `merge_path.len() + 1` | exact, and it cannot be otherwise |
//! | `same_value` | `merge_compare.len()` | exact; two descents a unit, a bounded constant |
//!
//! The two that were wrong share a stack that **spans definitions**: a fragment spread pushes a
//! definition-root frame, so a resolution starts partway up and the levels below it are not walked.
//! The two merge-engine rows cannot have the defect at all — a merge set's parent chain never
//! crosses a definition, so its stack *is* its suffix — and `walk_value` never had it because its
//! charge and its slice are the same expression, `values[base..depth]`. These two had the quantity
//! in one place and the slice computed in another, and that is where they disagreed; both callers
//! now charge `suffix_of(..).len()` and resolve `suffix_of(..)`.
//!
//! `descend`'s `merge_stack.len() >= merge_depth()` is not on the list: it is a **limit**, which
//! refuses a depth rather than buying one, and nothing is debited for it.

//! # Every traversal that repeats, and the reader the repeat is for
//!
//! The third enumeration, over the thing the first two do not cover: work **performed**, neither
//! priced wrongly nor allocated. A repeat is introduced because some reader needs the second pass,
//! and the question is whether it is gated on that reader or on the family the reader belongs to.
//!
//! | traversal | repeats over | the reader | gate | the iteration's own charge |
//! |---|---|---|---|---|
//! | a spread's fragment body | operations | operation-local variable usages | [`collects_usages`] — **was ungated** | one unit at the top of `walk_selections` |
//! | `begin_fragment`'s directives | operations | the same usages, on the definition's own directives | `reaches_directives`, which reduces to `descends_for_usages` | one per directive — **was nothing** |
//! | [`Validator::check_subscription_roots`] | subscription operations | 5.2.4.1's collection, which *is* per operation | `SingleRootField` | one unit at the top of its walk |
//! | draft 5.3.2's expansion | operations, then unreached fragments | the merge engine | [`merges`], and its memo answers a repeated expansion from the first one | `fields + kids` per set, one per queued set |
//! | [`Validator::walk_unreached_fragments`] | nothing — once per unchecked fragment | definition-local rules | the `checked` bit, which the entry sets | prepaid: prep charges a unit a fragment |
//!
//! The first row is al8n/smear#198's seventeenth round. The eighth round made a fragment's body
//! walk repeat per operation, which was the right repair for the reader that needed it: a fragment
//! definition's directives are the non-constant family, the usages in them are operation-local, and
//! without the repeat the verdict depended on the order the operations were written in. The repeat
//! was then performed for **every** rule set, including the ones with no usage rule at all, where
//! the walk carries `check = false` and every reader below it reduces to `descends_for_usages` —
//! `Θ(O · W)` of examination off `O(O + W)` of syntax, for conclusions nothing could act on.
//!
//! Which is the pattern the four enumerations share, and the reason there are four:
//!
//! - a **charge** whose reader has moved is a prepayment for nobody (round 6);
//! - a **gate** that skips more than its reader needs is a missing diagnostic (the skip audit);
//! - an **allocation** ahead of its charge is a buffer nobody paid for (round 15);
//! - and a **repeat** introduced for one reader is a cost every other configuration pays.
//!
//! **A repeat must be gated on the reader it was introduced for**, in the same place and by the
//! same predicate — never on the family that reader belongs to, and never not at all.

//! # The fifth crossing: is the repeat's own iteration priced?
//!
//! The column above is al8n/smear#198's eighteenth round, and it is the question the traversal list
//! did not ask. `begin_fragment`'s directive list reached `check_directives`' **descent-only** arm —
//! the path taken when the only reason to be here is to find variable leaves — and that arm walked
//! the whole list with no charge at all. `check_arguments` had the identical arm and the identical
//! gap. `O` operations over one fragment's `D` bare directives ran `Θ(O · D)` while the ledger saw
//! only the surrounding constant, and here the repeat is *correct*: `collects_usages` is what puts
//! the walk on that path, so the work is real and nothing priced it. A bypass, not an over-charge.
//!
//! The shape it came from is worth naming, because it is how a repair makes one: the seventh round
//! removed the **name** charge from those arms and was right to — nothing there reads a spelling,
//! so pricing one refuses a document for bytes nobody looks at. The charge on the **iteration**
//! went with it, and that was never part of the finding. *A charge removed as an over-charge takes
//! the iteration's price with it unless the iteration is charged separately.*
//!
//! So the inventory this time is over **loops**: every one in this module and in `values.rs` and
//! `selections.rs` whose trip count a caller decides. There are thirty-four, and after the two
//! repairs each is in one of four states.
//!
//! - **Charged per iteration**, at the top and before the step: both selection walks, the
//!   subscription walk, the value walk's depth, 5.5.2.3's per-word intersection,
//!   `conditional_directive`'s per-directive unit, the variable-definition loop, and both
//!   descent-only arms — which now charge the list once, in front of it, in `count_units`.
//! - **Prepaid over the same population**: everything the prep sweep sized — the fragment index and
//!   its grouping, the operation loops, the cycle and reachability walks, `walk_unreached_fragments`
//!   — plus the uniqueness scans, which run over a `keys` range no longer than a list `spend_names`
//!   already paid for, and 5.8.4's unused report, whose population the variable index charged a
//!   unit at a time because `AllVariablesUsed` is what guarantees the index was built.
//! - **Bounded by a constant the document cannot move**: `pack_type`'s wrapper loop, at
//!   `MAX_WRAPPERS`.
//! - **Bounded at `n <= 1`**: the two key-pushing loops whose prepayment is gated on `len > 1` —
//!   the variable index and 5.6.3's — where the ungated case runs at most one iteration. That gate
//!   is the eleventh round's singleton repair, and this is the check that it did not open the same
//!   hole one dimension down.

//! # The sixth crossing: is anything performed before the check that says it is unnecessary?
//!
//! A gate is a claim about *whether* work is needed. What sits **above** it is work done before
//! that claim was made, and al8n/smear#198's nineteenth round found the shape twice in one
//! function. One line per check that can decide work away.
//!
//! | check | what it decides | what ran ahead of it |
//! |---|---|---|
//! | `Visited::visit`, at a spread | whether *this* spread expands the body | the target condition's charge and its `Schema::sym` hash — **now below it** |
//! | `resolves_positions`, in `check_field` | whether a field name has a reader | the name's own charge — **now inside the arm that reads it** |
//! | `resolves_positions`, in `check_inline_fragment` | whether a condition has a reader | the same charge — **same repair** |
//! | `Visited::visit`, in [`Validator::check_subscription_roots`] | the same decision, other walk | nothing: it already asked before charging the condition |
//! | the `checked` bit | whether definition-local rules run | the row and body lookups, which are how the definition is named at all |
//! | `enters_body` | whether an entry is on the table | the spread's own name and directives, which are read at the site whether or not the body is entered |
//! | `reaches_spread_target` | whether 5.5.2.3 runs | nothing; it is computed above the resolution it gates |
//! | `resolves_positions`, in both descent-only arms | whether ancestors are resolved | nothing; it is the arm's own condition |
//! | the `n <= 1` sort gates | whether a prepayment is owed | nothing; `len()` is `O(1)` |
//! | `walks_values`, `merges` | whether a pass runs at all | nothing; each is its function's first statement |
//! | `Claim::Done`, in the merge memo | whether a pass has already run | the hash fold and the chain walk — which *are* the lookup, not work ahead of it, and both charged |
//!
//! The distinction the last row turns on is the one to keep: work that **computes** the check is
//! not work performed ahead of it. A memo cannot answer without hashing its key. A spread can
//! answer `Visited::visit` without resolving anything.

//! ## Crossed with: where a constant-time discriminator exists, what runs before it?
//!
//! Not a sixth list — the same population, one question further in. A check that costs two integer
//! loads and a check that costs a walk are both "checks", and the twentieth round found the cheap
//! one placed under the expensive one twice.
//!
//! | discriminator | costs | what ran before it |
//! |---|---|---|
//! | length, in `verify_source` | two loads | the lossless door's `units(max(parse, source))` prepayment — **now below it** |
//! | `len() > 1`, for 5.4.2 and 5.7.3 | one compare | the whole resolving path, including `spend_names` over every spelling — **now below it** |
//! | a rule-set bit, for a resolved position | one mask | the field, condition and spread resolutions on every first visit — **now below them** |
//! | `Visited::visit` | one stamp compare | the target condition's hash (nineteenth round) |
//! | `is_empty()`, before `find_fragment` | one compare | nothing; the spread-name charge is already under it |
//! | `len() > 1`, for 5.6.3 and the variable index | one compare | nothing; both gates predate this |
//! | a stored hash and length, in `Names::intern` | two compares | nothing — a bucket collision is rejected "without touching a byte" |
//! | `known.hash == hash && rows.len()`, in the memo | two compares | nothing; the contents comparison is under both |
//! | `wrappers()`, in `same_response_shape` | one compare | nothing; no type is looked up until it passes |
//! | `target == parent`, in 5.5.2.3 | one compare | nothing, since the eleventh round put it above the charge as well as the scan |
//!
//! And the inverse, recorded so a later sweep does not "repair" it. **A discriminator that cannot
//! be answered without the expensive step is not an instance.**
//!
//! - `Claim::Done` needs the memo's hash, and hashing the key *is* the lookup.
//! - [`Recovery::is_complete`](smear_parser::graphql::lossless::Recovery::is_complete) needs the
//!   walk: what was skipped is not knowable from a length.
//! - `condition_applies` needs `Schema::sym`; there is no cheaper answer than hashing the name, and
//!   the one unit a directive charged before examining it is already the floor.
//! - `find_fragment` is a binary search, and emptiness is the only constant-time thing to ask about
//!   it — which it does ask.
//! - [`merges`] and `walks_values` **are** the discriminators; nothing precedes them.
//!
//! One thing the first row dissolved rather than reordered. The second round's finding was that the
//! projection prepayment must be priced over *both* inputs, because `parse` and `source` are two
//! parameters and pricing from one meant spending on the other. Once the lengths must agree before
//! anything is priced, there is no maximum left to take: the two inputs are one number, and the
//! defect that needed `max(..)` cannot be constructed. A cheap gate placed correctly can retire a
//! question rather than answer it.
//!
//! # And a fifth way a repair goes wrong
//!
//! The catalogue above is about *what* is charged, allocated, repeated or gated. This one is about
//! the repair itself, and it cost a round:
//!
//! - a **charge** whose reader has moved is a prepayment for nobody;
//! - a **gate** that skips more than its reader needs is a missing diagnostic;
//! - an **allocation** ahead of its charge is a buffer nobody paid for;
//! - a **repeat** introduced for one reader is a cost every other configuration pays;
//! - and a **diagnosis can name the right function and the repair still not call it.**
//!
//! The fourteenth round's own report identified `verify_source` as the reason the fail-fast
//! projections never had the prefix defect — *"they open with `verify_source` over the whole green
//! root, whose first comparison is of lengths"* — and then wrote a second whole-root check beside
//! it, `parse.syntax().text() == source`, which materialises rowan's red cursor and allocates one
//! node's worth of cursor data per element it walks past. Measured at zero against fourteen
//! thousand for a fourteen-kilobyte document. The diagnosis was right and the repair did not use
//! it; `smear/tests/validator_allocation.rs` now has the lossless door in its population, which is
//! the gate that was not looking.
//! - **A gate that under-charges is a bypass; a gate that SKIPS is a wrong answer.** Only the first
//!   shows up as a number moving, and a budget test cannot see the second at all. So every gate owes
//!   two answers: what does it skip, and does any consumer need it? `Scratch::reachable` had a
//!   second reader in another module; the variable index had one that survived an empty range; and
//!   `begin_fragment` — behind the bit that deduplicates *reporting* — skipped the
//!   operation-local usages on a fragment definition's own directives, which made the verdict
//!   depend on the order the operations were written in.
//! - **"Not the caller's population" is an answer about size, and half an answer.** The other half
//!   is how many times a caller can reach it. Three scans over schema-sized groups — 5.4.3's
//!   argument list, 5.6.4's input-field list, and 5.5.2.3's possible-object bitset — are reached
//!   once per position a request writes, so each is a product with one caller-controlled factor and
//!   each is charged in its own dimension: entries, entries, words.
//! - **A charge lives where the work happens, not where a caller remembered to put it.** Four
//!   rounds sharpened *what* is charged; this one is about *where*. A charge separated from its
//!   work by a **call boundary** depends on the caller naming the right subject — `report_name`
//!   cloned whatever it was handed, and two callers handed it a different string than the one they
//!   had charged. A charge separated by a **loop** pays at the entrance for work inside. So the
//!   subject clone is now unobtainable without paying: [`Validator::subject`] is the only way to
//!   get one, and the hand-written form is gone from the module.
//! - **A gate is named after its readers, not after a rule family.** The charge went
//!   *exists → in front of the work → in the right dimension → over the right population*, and a
//!   gate has the same four steps: the last one is asking which rules actually **read** what the
//!   gated work produces. `Scratch::used` has exactly one reader and was filled for three;
//!   a variable's packed type has two and was resolved for every definition, with or without a
//!   default; and a constant tree cannot contain a variable at all, which is now
//!   [`ValueLike::HAS_VARIABLES`](super::nodes::ValueLike::HAS_VARIABLES) — an associated const
//!   with no default, so a third value family has to state its own case rather than inherit an
//!   answer that happens to fit two.
//! - **A gate belongs on the call path, not only on the condition.** Round three swept the
//!   conditions that had been written and missed the paths that reach a charge without passing one:
//!   a directive list precharged before anything asked whether a directive, argument, value or
//!   usage rule was on; an argument list the same; a variable's declared type resolved for two
//!   readers that could both be off; a variable leaf searching an index no enabled rule reads. Each
//!   is a **false refusal** — a caller handed `Err` for work nobody asked for — so the sweep that
//!   matters names, for every `spend` site, the predicate that gates it.
//! - **An activation condition with more than one rule in it gets a name.** [`merges`],
//!   [`checks_values`], [`collects_usages`] and [`reports_type_conditions`] exist because each has
//!   two readers, and a condition with two readers written out twice is two conditions. One of
//!   them shipped naming two of draft 5.3.2's three activating rules, so a rule set that started
//!   the engine without starting the pass that fills what it reads produced false refusals.
//! - **A rule that is off must not read caller-sized data.** Draft 5.6.1's leaf arms hashed an
//!   enum spelling and parsed a digit string before asking whether 5.6.1 was on. Sweeping the
//!   class found four more: the value walk itself, the fragment-declaration pass, 5.6.3's
//!   prepayment, and the variable index — and it found that the *reachability* pass could not join
//!   them, because draft 5.3.2's engine reads the bitset it fills. That last one is the reason the
//!   sweep is worth running even where it finds nothing to repair: it is what stops a repair from
//!   becoming the next defect.
//!
//! # Nothing recurses on document shape
//!
//! Selection sets and value literals are both walked with explicit stacks living in the caller's
//! [`Scratch`]. The frames are coordinates rather than references — a definition index plus the
//! chain of child indices that reaches a level — which is what lets a stack that knows nothing
//! about the document's source slice type replace recursion over an attacker-chosen shape. Draft
//! 5.3.2's two merge recursions and its value comparison are written the same way, for the same
//! reason and with more at stake.

mod merge;
mod nodes;
mod selections;
mod values;

use core::ops::ControlFlow;

use tokora::{SimpleSpan, span::AsSpan};

use smear_parser::graphql::ast::{
  DescribedVariableDefinition, ExecutableDefinition, ExecutableDocument, Name, OperationDefinition,
  OperationType, Selection, Type,
};

use super::{
  Budget, Diagnostic, Rule, RuleSet, Scratch, Sink,
  diagnostic::Context,
  schema::{
    DirectiveLocation, MAX_WRAPPERS, PackedType, Range32, RootOperation, Schema, Sym, TypeId,
    is_reserved,
  },
  scratch::{
    Edge, FragmentRow, Frame, GraphFrame, NONE, OperationRow, Work, byte_units, clear_bit,
    count_units, get_bit, push_frame, reset_bits, set_bit,
  },
};

use nodes::{child_selection_set, definition, fragment, name_bytes, operation, root_selection_set};
use values::ValueLocation;

/// The verdict of a failed validation.
///
/// Returned when the document was refused: because at least one diagnostic was emitted, or because
/// a resource bound abandoned a pass partway through. What the diagnostics *were* is the sink's
/// business — this is only the count, whether a bound refused, and whether the sink asked to stop
/// before the document had been fully examined.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Invalid {
  emitted: u32,
  stopped: bool,
  refusal: Option<Refusal>,
}

/// Why a validation abandoned a document, when one did.
///
/// # Why this is a type and not a combination of flags
///
/// An [`Invalid`] with [`Invalid::emitted`] zero is a verdict that examined less than the whole
/// document, and there is more than one way to reach it. The contract used to say there was exactly
/// one, and named it — and then a second arrived and the sentence did not notice. Asking a caller
/// to tell two refusals apart by reading three booleans in the right combination is the shape this
/// crate has already replaced three times: an `Option` carrying two kinds of abandonment, a
/// [`u32::MAX`] carrying "off", and a zero-and-one [`Recovery`](super::Recovery) carrying "never
/// ran". Each became a type, and each stopped needing prose to be read correctly.
///
/// It is `#[non_exhaustive]` for the reason the flags were not: a fourth way to refuse should cost
/// a `match` arm at the call sites that care, not a sweep of every published sentence.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Refusal {
  /// A [`Budget`] bound abandoned a pass.
  ///
  /// [`Invalid::emitted`] is zero when that bound's own rule was outside the
  /// [`RuleSet`](super::RuleSet), and non-zero when it was in it or when other rules had already
  /// fired. The document is **invalid**, not "unvalidated": the validator refuses rather than
  /// passing what it could not finish examining.
  Budget,
  /// The lossless door was handed a `parse` and a `source` that do not describe one document.
  ///
  /// Nothing was projected and nothing was validated, so [`Invalid::emitted`] is always zero and
  /// [`LosslessInvalid::recovery`](super::LosslessInvalid::recovery) is always `None`. Not a
  /// resource problem, which is why it is not [`Refusal::Budget`].
  SourceMismatch,
}

impl Invalid {
  /// Returns how many diagnostics were produced.
  ///
  /// This counts what the validator emitted, not what the sink kept: [`Ignore`](super::Ignore)
  /// discards everything and the count is still right.
  ///
  /// **Zero is a possible count on a verdict that is still `Err`**, and it means one thing: a
  /// resource bound refused the document with that bound's own rule outside the
  /// [`RuleSet`](super::RuleSet), so there was nothing to emit. [`Invalid::budget_tripped`] is
  /// true whenever that happens, and a caller that reported "no findings" without reading it would
  /// be describing a check the validator abandoned.
  #[inline]
  pub const fn emitted(&self) -> u32 {
    self.emitted
  }

  /// Returns whether the **sink** stopped validation before the document was fully examined.
  ///
  /// True for [`First`](super::First) on any invalid document *that produced a diagnostic*. The
  /// qualification is not pedantry: there is exactly one verdict that is `Err` with nothing
  /// emitted — a resource bound refusing with its own rule outside the
  /// [`RuleSet`](super::RuleSet) — and no diagnostic ever reaches the sink there, so the sink
  /// never asks for anything to stop and this reads `false` on a document that was very much not
  /// fully examined. Read as "was the whole document looked at", it says the opposite of the truth
  /// on the one case where it matters most.
  ///
  /// So the two flags answer two questions and neither answers the other's: this one says **who**
  /// stopped the walk, and [`Invalid::budget_tripped`] says whether a bound refused. A caller who
  /// wants "is anything about this document still unknown" reads both.
  ///
  /// When it is true, the absence of a diagnostic says nothing: the rest of the document was never
  /// looked at. al8n/smear#196.
  #[inline]
  pub const fn stopped(&self) -> bool {
    self.stopped
  }

  /// Returns whether a [`Budget`] refused the document.
  ///
  /// True when draft 5.3.2's merge engine reached
  /// [`Budget::merge_depth`](super::Budget::merge_depth) or
  /// [`Budget::merge_work`](super::Budget::merge_work). With the bound's own rule in the
  /// [`RuleSet`](super::RuleSet) the diagnostic is
  /// [`Rule::MergeDepthBudget`](super::Rule::MergeDepthBudget) or
  /// [`Rule::MergeWorkBudget`](super::Rule::MergeWorkBudget) and says which; **without it there is
  /// no diagnostic and this flag is the whole of the report**, on a verdict whose
  /// [`Invalid::emitted`] is zero. Filtering a bound's rule out switches off its diagnostic, not
  /// the refusal: an engine that stopped and then answered `Ok` would be reporting a clean result
  /// for a check it never finished. al8n/smear#196.
  ///
  /// **And when any other pass reached this crate's absolute validation ceiling**, whose diagnostic
  /// is [`Rule::ValidationWorkBudget`](super::Rule::ValidationWorkBudget). That bound answers the
  /// paragraph above identically — the refusal does not depend on its rule being enabled, only
  /// being *told* which bound refused does — which is what makes this one flag rather than three.
  /// al8n/smear#198.
  ///
  /// When it is true the document is **invalid**, not "unvalidated": the engine refuses rather
  /// than passing what it could not finish examining. What it does *not* mean is that the rest of
  /// the document is clean — the merge engine stopped, so anything it had not reached is unknown,
  /// exactly as [`Invalid::stopped`] means for the sink.
  #[inline]
  pub const fn budget_tripped(&self) -> bool {
    matches!(self.refusal, Some(Refusal::Budget))
  }

  /// Returns why validation abandoned the document, when it did.
  ///
  /// `None` means it did not: every finding came from a rule that ran to completion, and
  /// [`Invalid::emitted`] is non-zero. `Some` is the single place a caller reads to learn that part
  /// of the document was never examined and why — see [`Refusal`].
  #[inline]
  pub const fn refusal(&self) -> Option<Refusal> {
    self.refusal
  }
}

/// The lossless door's constructor, and nothing else's.
///
/// Gated to its one caller rather than left ungated: without `rowan` there is no door that refuses
/// before a [`Validator`] exists, so an ungated constructor is dead code in exactly the feature
/// selection `cargo clippy --no-default-features -D warnings` builds.
#[cfg(feature = "rowan")]
impl Invalid {
  /// The verdict of a run a resource bound abandoned, with `emitted` diagnostics behind it and
  /// `stopped` saying whether the sink asked to stop on one of them.
  ///
  /// `pub(crate)` and used by exactly one caller: the lossless door refuses **before** it projects,
  /// so there is no [`Validator`] in existence to carry the flag out. The shape is the same one
  /// `validate_charged` produces — `Err`, `budget_tripped`, and an `emitted` that may be zero —
  /// because a second spelling of "gave up" is how a caller ends up reading one of them as
  /// "finished".
  pub(crate) const fn refused(emitted: u32, stopped: bool) -> Self {
    Self {
      emitted,
      stopped,
      refusal: Some(Refusal::Budget),
    }
  }

  /// The verdict of a run whose inputs did not describe one document.
  ///
  /// The lossless door's other refusal, and the reason [`Invalid::budget_tripped`] is what
  /// separates them: a `parse` and a `source` that disagree are not a resource problem, so this
  /// reports `false` there and zero emitted. Nothing was validated either way, and a caller who
  /// reads only the `Result` learns that from the `Err` alone.
  pub(crate) const fn unexamined() -> Self {
    Self {
      emitted: 0,
      stopped: false,
      refusal: Some(Refusal::SourceMismatch),
    }
  }
}

impl core::fmt::Display for Invalid {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    if self.emitted == 0 {
      // Every way to be `Err` with nothing emitted, matched rather than inferred. "0 validation
      // errors" would read as the opposite of what happened for all of them.
      return f.write_str(match self.refusal {
        Some(Refusal::Budget) => "resource budget exceeded before the document was fully examined",
        Some(Refusal::SourceMismatch) => {
          "the parse and the source are not the same document, so nothing was validated"
        }
        // Unreachable: `validate_charged` answers `Ok` for a zero count with no refusal, so this
        // combination is not constructed. Rendered rather than asserted — a `Display` that panics
        // is a worse answer than a vague one.
        None => "validation examined less than the whole document",
      });
    }
    let plural = if self.emitted == 1 { "" } else { "s" };
    write!(f, "{} validation error{plural}", self.emitted)?;
    if self.stopped {
      f.write_str(" (validation stopped early)")?;
    }
    match self.refusal {
      Some(Refusal::Budget) => f.write_str(" (resource budget exceeded)")?,
      Some(Refusal::SourceMismatch) => f.write_str(" (the parse and the source disagree)")?,
      None => {}
    }
    Ok(())
  }
}

impl core::error::Error for Invalid {}

/// Validates an executable document against a schema, checking every draft §5 rule.
///
/// `scratch` is the caller's reusable working set and `sink` the caller's diagnostic storage; the
/// validator owns neither, which is what lets the steady state allocate nothing. `budget` bounds
/// draft 5.3.2's merge engine and, separately, every other pass.
///
/// Returns `Err(Invalid)` when at least one rule fired **or** a [`Budget`] bound refused the
/// document. Those are not the same thing and [`Invalid::budget_tripped`] is what separates them:
/// a refusal with the bound's own rule filtered out has an [`Invalid::emitted`] of zero.
///
/// # Example
///
/// ```
/// use smear_compiler::{Budget, First, Schema, Scratch, validate_executable};
/// use smear_parser::{
///   graphql::{
///     GraphQL,
///     ast::{ExecutableDocument, TypeSystemDocument},
///     error::GraphqlErrors,
///     syntactic::{GraphqlLexer, executable_document, type_system_document},
///   },
///   lexer::tokora::{Parse as _, Parser},
/// };
///
/// let schema = Schema::build(
///   &Parser::with_parser::<GraphqlLexer<'_, str>, TypeSystemDocument<&str>, GraphqlErrors<&str>, _, GraphQL>(
///     type_system_document,
///   )
///   .parse_str("type Query { hero: Character } interface Character { name: String! }")
///   .expect("the SDL parses"),
/// )
/// .expect("the SDL is a schema");
///
/// let parse = |src| {
///   Parser::with_parser::<GraphqlLexer<'_, str>, ExecutableDocument<&str>, GraphqlErrors<&str>, _, GraphQL>(
///     executable_document,
///   )
///   .parse_str(src)
///   .expect("the query parses")
/// };
///
/// let mut scratch = Scratch::new();
/// let budget = Budget::default();
///
/// let good = parse("{ hero { name } }");
/// let mut sink = First::new();
/// assert!(validate_executable(&schema, &good, &mut scratch, &budget, &mut sink).is_ok());
///
/// let bad = parse("{ hero { nickname } }");
/// let mut sink = First::new();
/// let invalid = validate_executable(&schema, &bad, &mut scratch, &budget, &mut sink)
///   .expect_err("`nickname` is not on `Character`");
/// assert_eq!(invalid.emitted(), 1);
/// assert_eq!(
///   sink.get().expect("a diagnostic").display(&schema).to_string(),
///   "5.3.1 Field Selections: `nickname` on type `Character` (9..17)"
/// );
/// ```
pub fn validate_executable<S, K>(
  schema: &Schema,
  document: &ExecutableDocument<S>,
  scratch: &mut Scratch,
  budget: &Budget,
  sink: &mut K,
) -> Result<(), Invalid>
where
  S: AsRef<[u8]> + Clone,
  K: Sink<S>,
{
  validate_executable_with(schema, document, scratch, budget, RuleSet::ALL, sink)
}

/// Validates an executable document against a subset of the rules.
///
/// A draft §5 rule outside `rules` is not evaluated, not merely filtered: a consumer that wants
/// only the fragment rules does not pay for value coercion. With [`RuleSet::ALL`] this is exactly
/// [`validate_executable`].
///
/// **`rules` does not reach the resource bounds.** Narrowing removes a bound's *diagnostic*, never
/// the bound: a caller who asks for
/// [`Rule::FieldSelectionMerging`](super::Rule::FieldSelectionMerging) alone is still handed `Err`
/// with [`Invalid::emitted`] zero and [`Invalid::budget_tripped`] set when `budget` refuses.
///
/// What narrowing *can* do is leave a bound with nothing to bound, and this said the opposite —
/// that `budget` is enforced whatever the set contains.
/// [`Budget::merge_work`](super::Budget::merge_work) and
/// [`Budget::merge_depth`](super::Budget::merge_depth) are spent by draft 5.3.2's engine, and that
/// engine is started by draft 5.3.2's own rule: with
/// [`Rule::FieldSelectionMerging`](super::Rule::FieldSelectionMerging),
/// [`Rule::MergeDepthBudget`](super::Rule::MergeDepthBudget) and
/// [`Rule::MergeWorkBudget`](super::Rule::MergeWorkBudget) all absent it does not run, so nothing
/// is expanded, interned or compared and a `merge_work` of zero has nothing to refuse. **That is
/// vacuity and not an exemption** — nothing expensive was let through, because nothing expensive
/// happened. A bound whose passes *do* run is enforced whether or not its rule is in `rules`.
///
/// So `budget` is not an admission policy. Deciding whether to accept a document by what it would
/// cost means leaving the rule that does the costing switched on: `rules` chooses what is
/// **checked** and [`Budget`] chooses what is **afforded**, and neither answers the other's
/// question. al8n/smear#196.
pub fn validate_executable_with<S, K>(
  schema: &Schema,
  document: &ExecutableDocument<S>,
  scratch: &mut Scratch,
  budget: &Budget,
  rules: RuleSet,
  sink: &mut K,
) -> Result<(), Invalid>
where
  S: AsRef<[u8]> + Clone,
  K: Sink<S>,
{
  validate_charged(
    schema,
    document,
    scratch,
    budget,
    rules,
    sink,
    Ledger::open(budget),
  )
}

/// [`validate_executable_with`] continuing an already-opened [`Ledger`] rather than opening one.
///
/// The lossless door needs this. Its projection builds the whole AST **before** any rule exists to
/// charge, so the ledger has to be opened before the [`Validator`] is and carried into it; a
/// second, private ledger would be a second number for a caller to reason about and a second place
/// for the two to disagree. See `lossless::validate_executable_lossless_with`.
pub(crate) fn validate_charged<S, K>(
  schema: &Schema,
  document: &ExecutableDocument<S>,
  scratch: &mut Scratch,
  budget: &Budget,
  rules: RuleSet,
  sink: &mut K,
  left: Ledger,
) -> Result<(), Invalid>
where
  S: AsRef<[u8]> + Clone,
  K: Sink<S>,
{
  scratch.reset();
  let mut validator = Validator {
    schema,
    document,
    scratch,
    sink,
    rules,
    budget: *budget,
    scalars: Scalars::resolve(schema),
    checks_directives: checks_directives(rules),
    checks_arguments: checks_arguments(rules),
    checks_values: checks_values(rules),
    reads_argument_positions: reads_argument_positions(rules),
    reads_field_positions: reads_field_positions(rules),
    reports_type_conditions: reports_type_conditions(rules),
    collects_usages: collects_usages(rules),
    resolves_variable_types: rules.contains(Rule::VariablesAreInputTypes),
    marks_usage: rules.contains(Rule::AllVariablesUsed),
    reads_usage_positions: rules.contains(Rule::AllVariableUsagesAreAllowed),
    visits_variable_definitions: visits_variable_definitions(rules),
    variables: &[],
    in_operation: false,
    variable_index: Range32::new(0, 0),
    emitted: 0,
    stopped: false,
    work: Work::new(budget.merge_work()),
    left,
    generation: 0,
    tripped: false,
    blame: SimpleSpan::const_new(0, 0),
  };
  let _ = validator.run();
  // ONE field, read once. Both branches that met here arrived at the same repair from opposite
  // sides: al8n/smear#196 collapsed the merge engine's "stopped" and "reported" flags onto
  // `tripped`, and al8n/smear#198 added `refused` for its own ledger — so the rebased tail briefly
  // read two fields for one fact, which is the shape that produced the original defect. With draft
  // 5.3.2 enabled and its two budget *rules* filtered out, a tail that read only one of them
  // returned `Ok` on a document the engine had abandoned. There is one fact now and no way to
  // consult half of it.
  let tripped = validator.tripped;
  let (emitted, stopped) = (validator.emitted, validator.stopped);
  let refusal = tripped.then_some(Refusal::Budget);
  if emitted == 0 && !tripped {
    Ok(())
  } else {
    Err(Invalid {
      emitted,
      stopped,
      refusal,
    })
  }
}

/// What is left of [`Budget::validation_work`](super::Budget::validation_work), or the explicit
/// absence of a bound.
///
/// # Disabled is a state, not a large number
///
/// [`u32::MAX`] is the *spelling* a caller uses to turn the bound off, and it stops being a number
/// here rather than staying one and being trusted to stay large. A counter that encodes "off" as
/// its maximum is a counter the first charge converts into a very large *finite* budget — and this
/// program has now written that same defect four times: `Work::take`'s saturation, `Visits::take`,
/// tokora's regime generation, and this. Repairing it as arithmetic repairs one of them.
///
/// [`Ledger::Off`] is closed under [`Ledger::take`]: no sequence of charges, and no prepayment the
/// lossless door subtracts before the validator exists, can turn it into [`Ledger::Left`]. That is
/// the property, and it is a property of the type rather than of every call site that spends.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Ledger {
  /// No bound. Every charge succeeds and changes nothing.
  Off,
  /// This many units remain.
  Left(u32),
}

impl Ledger {
  /// The ledger a [`Budget`] opens.
  #[inline]
  pub(crate) const fn open(budget: &Budget) -> Self {
    match budget.validation_work() {
      u32::MAX => Self::Off,
      left => Self::Left(left),
    }
  }

  /// Spends `units`, or answers `None` when there was not room for them.
  #[inline]
  pub(crate) const fn take(self, units: u32) -> Option<Self> {
    match self {
      Self::Off => Some(Self::Off),
      Self::Left(left) => match left.checked_sub(units) {
        Some(left) => Some(Self::Left(left)),
        None => None,
      },
    }
  }
}

/// The unit [`Budget::validation_work`](super::Budget::validation_work) is spent in.
///
/// One per node examined, plus one per eight bytes of any **document-chosen** name a pass reads.
///
/// # Charged in front of the work, which is the property and not the convention
///
/// A charge taken after the step it prices bounds nothing: the work is already spent by the time
/// the counter can refuse it, so the ceiling is exceeded by whatever that step cost. The question
/// a reader must ask of every charge here is therefore not "does this pass charge?" but "is the
/// charge in front of the work it prices?" — and four sites answered the first question and not
/// the second before al8n/smear#198's first review. [`Validator::spend`] carries the re-derived
/// table.
///
/// # Bytes and not entries, and the difference is measurable rather than theoretical
///
/// A GraphQL name has no length ceiling, every name comparison here is `[u8] == [u8]`, and *where*
/// two names differ decides what that costs. At 2,000 variable definitions and 2,000 usages, names
/// padded to 200 bytes **after** the distinguishing digits measured 8.8 ms — no worse than
/// one-byte names, because the comparison exits at the first differing byte — and the same padding
/// written **before** them measured 17.8 ms. A ledger over entries cannot see that factor at all.
///
/// # Why eight
///
/// Roughly the word a comparison, a hash or a copy advances per step, and it is the unit
/// `graphql-proto`'s collection ledger already charges in — two ledgers counting different things
/// in the same crate family would be two units to re-derive.
#[inline]
pub(crate) const fn units(len: usize) -> u32 {
  // `as` after the shift, so a name longer than `u32::MAX * 8` cannot wrap the charge down to
  // nothing on a 64-bit target. Nothing can allocate one; a bound that rests on that is not a
  // bound.
  let chunks = len >> 3;
  if chunks >= u32::MAX as usize {
    u32::MAX
  } else {
    chunks as u32 + 1
  }
}

/// Which of this crate's two ledgers pays for a diagnostic subject's copy.
///
/// [`Validator::subject`] is the only door to a subject and it charges before it copies; this says
/// *where*. Draft 5.3.2 spends [`Budget::merge_work`](super::Budget::merge_work) and every other
/// pass spends [`Budget::validation_work`](super::Budget::validation_work), and a door serving both
/// has to be told which — an enum rather than a boolean, so a third ledger cannot arrive as a
/// silent `false`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Charged {
  /// Every draft §5 pass outside 5.3.2.
  Validation,
  /// Draft 5.3.2's engine.
  Merge,
}

/// Whether draft 5.3.2's engine runs.
///
/// **One definition, read by the engine and by everything that feeds it.** The engine is activated
/// by three rules, not one: [`Rule::FieldSelectionMerging`] is the rule it implements, and
/// [`Rule::MergeDepthBudget`] and [`Rule::MergeWorkBudget`] each start it as well, because a
/// caller who wants only the resource refusals still needs the pass that can produce them.
///
/// It exists as a named predicate because it has a **producer** as well as a consumer.
/// `check_fragments_used` fills `Scratch::reachable`, and the engine reads it to skip the fragments
/// an operation's own merge already covered; a hand-written condition on the producer that listed
/// two of the three rules left `RuleSet::only(MergeWorkBudget)` reading a cleared bitset, so every
/// fragment looked unreached and a chain already expanded from an operation was merged again from
/// every suffix — linear work inflated toward quadratic, and false budget refusals out of it.
///
/// A fourth merge rule would desynchronise a copied condition and cannot desynchronise this one.
const fn merges(rules: RuleSet) -> bool {
  rules.contains(Rule::FieldSelectionMerging)
    || rules.contains(Rule::MergeDepthBudget)
    || rules.contains(Rule::MergeWorkBudget)
}

/// Whether either rule a type condition is reported against is enabled.
///
/// The third named predicate, and the third for the same reason: `check_fragment_declarations`
/// exists only to run [`Validator::check_type_condition`] for its reports, and that function's
/// reports are gated one rule at a time inside it. A hand-written pair on the caller and two
/// separate guards on the callee is a condition in two places; a third rule added to the callee
/// would be silently skipped by the caller. Any rule `check_type_condition` can report belongs
/// here.
const fn reports_type_conditions(rules: RuleSet) -> bool {
  rules.contains(Rule::FragmentSpreadTypeExistence)
    || rules.contains(Rule::FragmentsOnCompositeTypes)
}

/// Whether anything inside a variable definition's own body can act.
///
/// Named after the **readers**, not a family: draft 5.8.2's report, the value rules over a default
/// value, and the directive and argument rules over the definition's directives. A default value
/// and a definition's directives are both **constant** trees, so no usage rule appears here — a
/// `ConstInputValue` has no variable arm, and a gate that admitted `collects_usages` was admitting
/// a reader that cannot act.
const fn visits_variable_definitions(rules: RuleSet) -> bool {
  rules.contains(Rule::VariablesAreInputTypes)
    || checks_values(rules)
    || checks_directives(rules)
    || checks_arguments(rules)
}

/// Whether draft 5.7's directive rules are enabled.
const fn checks_directives(rules: RuleSet) -> bool {
  rules.contains(Rule::DirectivesAreDefined)
    || rules.contains(Rule::DirectivesAreInValidLocations)
    || rules.contains(Rule::DirectivesAreUniquePerLocation)
}

/// Whether draft 5.4's argument rules are enabled.
const fn checks_arguments(rules: RuleSet) -> bool {
  rules.contains(Rule::ArgumentNames)
    || rules.contains(Rule::ArgumentUniqueness)
    || rules.contains(Rule::RequiredArguments)
}

/// Whether draft 5.6's literal rules are enabled.
///
/// **A property of a definition**, which is why it is not enough on its own to start a value walk:
/// these rules fire under `Frame::CHECK` and a definition carries that flag exactly once, however
/// many operations reach it. See [`collects_usages`] for the half that is a property of an
/// *operation*, and `values::walk_value` for what asking one question where there were two cost.
const fn checks_values(rules: RuleSet) -> bool {
  rules.contains(Rule::ValuesOfCorrectType)
    || rules.contains(Rule::InputObjectFieldNames)
    || rules.contains(Rule::InputObjectFieldUniqueness)
    || rules.contains(Rule::InputObjectRequiredFields)
}

/// Whether any enabled rule reads an **argument's declared type**.
///
/// [`checks_arguments`] minus [`Rule::ArgumentUniqueness`], which is the odd one out and the reason
/// this exists: 5.4.2 compares the spellings a request *wrote* against each other and never asks
/// the schema what they mean. A rule set holding only that one needs no resolution — and needs no
/// comparison either on a list of fewer than two, which is why its callers pair this with a length.
const fn reads_argument_positions(rules: RuleSet) -> bool {
  rules.contains(Rule::ArgumentNames)
    || rules.contains(Rule::RequiredArguments)
    || checks_values(rules)
}

/// Whether any enabled rule reads a **selection level's resolved type**.
///
/// The definition-local half of [`Validator::resolves_positions`] for the three selection-walk
/// callers, and the correction to what they were given first. al8n/smear#198's nineteenth round
/// handed that predicate `Frame::CHECK`, which says *this is the definition's first visit* — a
/// **when**, not a **who**. `RuleSet::EMPTY` and `only(AllVariableUsesDefined)` both set it, and
/// both then charged and hashed every field name and type condition on that first visit for
/// nobody. A reader predicate fed visit state is not a reader predicate.
///
/// The consumers, all of them: 5.3.1 and 5.3.3 read a field's definition directly; 5.5.2.3 reads
/// the *level's* type as the parent of a spread beneath it; and everything in
/// [`reads_argument_positions`] reaches its own answer through `definition.args()`, so a level that
/// does not resolve cannot supply one.
const fn reads_field_positions(rules: RuleSet) -> bool {
  rules.contains(Rule::FieldSelections)
    || rules.contains(Rule::LeafFieldSelections)
    || rules.contains(Rule::FragmentSpreadIsPossible)
    || reads_argument_positions(rules)
}

/// Whether the variable rules that read a usage's position are enabled.
///
/// **A property of an operation.** The same fragment is valid under one operation's variables and
/// invalid under another's, so these are collected on every visit rather than on the first — which
/// is the whole reason the selection walk repeats per operation at all.
const fn collects_usages(rules: RuleSet) -> bool {
  rules.contains(Rule::AllVariableUsesDefined)
    || rules.contains(Rule::AllVariablesUsed)
    || rules.contains(Rule::AllVariableUsagesAreAllowed)
}

/// The five built-in scalar names, resolved against this schema once per run.
///
/// Coercion is decided by *name*, not by the built-in flag: a document may spell `scalar String`
/// out — a printed schema does — and it is still the specification's `String`. Everything else is
/// a custom scalar, and a custom scalar accepts any literal because only the service knows how to
/// read it.
#[derive(Debug, Clone, Copy)]
struct Scalars {
  int: Option<Sym>,
  float: Option<Sym>,
  string: Option<Sym>,
  boolean: Option<Sym>,
  id: Option<Sym>,
}

impl Scalars {
  fn resolve(schema: &Schema) -> Self {
    Self {
      int: schema.sym(b"Int"),
      float: schema.sym(b"Float"),
      string: schema.sym(b"String"),
      boolean: schema.sym(b"Boolean"),
      id: schema.sym(b"ID"),
    }
  }
}

/// One validation run.
struct Validator<'a, 'd, S, K> {
  schema: &'a Schema,
  document: &'d ExecutableDocument<S>,
  scratch: &'a mut Scratch,
  sink: &'a mut K,
  rules: RuleSet,
  budget: Budget,
  scalars: Scalars,
  /// [`checks_directives`] for this run's [`RuleSet`], resolved once.
  checks_directives: bool,
  /// [`checks_arguments`] for this run's [`RuleSet`], resolved once.
  checks_arguments: bool,
  /// [`checks_values`] for this run's [`RuleSet`], resolved once.
  checks_values: bool,
  /// [`reads_argument_positions`] for this run's [`RuleSet`], resolved once.
  reads_argument_positions: bool,
  /// [`reads_field_positions`] for this run's [`RuleSet`], resolved once.
  reads_field_positions: bool,
  /// [`reports_type_conditions`] for this run's [`RuleSet`], resolved once.
  reports_type_conditions: bool,
  /// Whether draft 5.8.2 is enabled, which is one of the two readers of a variable's packed type.
  resolves_variable_types: bool,
  /// Whether draft 5.8.4 is enabled, which is the **only** reader of `Scratch::used`.
  pub(super) marks_usage: bool,
  /// Whether draft 5.8.5 is enabled, which is the **only** usage rule that reads a name above the
  /// variable leaf.
  reads_usage_positions: bool,
  /// [`visits_variable_definitions`] for this run's [`RuleSet`], resolved once.
  visits_variable_definitions: bool,
  /// [`collects_usages`] for this run's [`RuleSet`], resolved once.
  collects_usages: bool,
  /// The variable definitions of the operation being walked; empty outside one.
  variables: &'d [DescribedVariableDefinition<S>],
  /// Whether a variable scope is in effect. Distinguishes "an operation with no variables" from
  /// "a fragment nothing reached", which is the difference between draft 5.8.3 firing and not.
  in_operation: bool,
  /// The current operation's variable-name index, as a range into [`Scratch::keys`].
  ///
  /// The index is the ordinals of [`Validator::variables`] sorted by name with ties broken on the
  /// ordinal, built once per operation by [`Validator::check_variable_definitions`] and read by
  /// every usage. It outlives the pass that builds it, which is why it is recorded: every other
  /// user of `keys` pushes at the buffer's current length and truncates back, so a prefix that
  /// stays put is safe.
  ///
  /// A **range** and not a base, because the index is not always built — the rules that read it
  /// can all be off — and a base plus an assumed length would then name whatever the next pass
  /// pushed. Empty is the representation of "not built"; there is no second field saying so.
  variable_index: Range32,
  emitted: u32,
  stopped: bool,
  /// How much of the [`Budget`]'s work knob draft 5.3.2's engine has spent, and the ceiling it is
  /// spending against.
  work: Work,
  /// What is left of [`Budget::validation_work`](super::Budget::validation_work), or
  /// [`Ledger::Off`]. Charged by every pass that is not draft 5.3.2's engine, and pre-spent by the
  /// lossless door's projection before this struct exists.
  left: Ledger,
  /// Distinguishes one fragment expansion from the next without clearing a bitset per expansion.
  generation: u32,
  /// Whether **any** resource bound abandoned this run.
  ///
  /// Draft 5.3.2's engine writes it through `merge::trip`, and this crate's validation ledger
  /// writes it through [`Validator::refuse`]. One field because it is one fact, and because both
  /// alternatives were tried and both failed the same way: a field for the walk plus a field for
  /// whether anything was emitted (al8n/smear#196), and that pair plus a third for the ledger
  /// (al8n/smear#198). Set even when the bound's own rule is switched off — the bound holds either
  /// way, and only the diagnostic is optional.
  tripped: bool,
  /// The definition to blame for a budget diagnostic — the one being merged when the bound hit.
  blame: SimpleSpan,
}

impl<'d, S, K> Validator<'_, 'd, S, K>
where
  S: AsRef<[u8]> + Clone,
  K: Sink<S>,
{
  // -- reporting ------------------------------------------------------------------------------

  /// Returns whether a rule is enabled. Checked before building a diagnostic, so a disabled rule
  /// costs nothing.
  #[inline]
  fn on(&self, rule: Rule) -> bool {
    self.rules.contains(rule)
  }

  fn emit(&mut self, diagnostic: Diagnostic<S>) -> ControlFlow<()> {
    self.emitted = self.emitted.saturating_add(1);
    match self.sink.diagnostic(diagnostic) {
      ControlFlow::Continue(()) => ControlFlow::Continue(()),
      ControlFlow::Break(()) => {
        self.stopped = true;
        ControlFlow::Break(())
      }
    }
  }

  // -- the validation-wide ledger ---------------------------------------------------------------

  /// Charges `units` against [`Budget::validation_work`](super::Budget::validation_work)
  /// **before** the step they price.
  ///
  /// `blame` is the span the refusal points at — the node whose examination could not be afforded,
  /// which is the narrowest true answer to "where did this document stop being validated".
  ///
  /// # Every charge, in three facts
  ///
  /// The question this table asks has been sharpened twice, and each sharpening found sites the
  /// previous one could not see.
  ///
  /// - *"Does this pass charge?"* — every pass did, and four charged **behind** work a caller
  ///   sizes.
  /// - *"Is the charge in front of the work it prices?"* — that found those four, and missed two
  ///   more that pass it: one charging **depth** for a cost measured in **bytes**, and one whose
  ///   charge existed but was **gated on a rule that may be off** while the walk computing it was
  ///   unpaid.
  /// - So: **where it sits, what dimension it prices, and what gates it.** A row with no dimension
  ///   named is unfinished, and a row whose gate is "none" is a claim that the work is the
  ///   traversal itself rather than a rule's.
  ///
  /// # A clearance has to say which question it answers
  ///
  /// The three facts are asked of every row this table *has*. They cannot be asked of a row that
  /// was cleared, and a clearance is where the last two defects of al8n/smear#198 lived.
  ///
  /// The `5.3.2 merge engine` row below says "its own ledger". That answers **which ledger pays**.
  /// It does not answer **whether that ledger's own setup is charged**, and an initialisation
  /// sweep that read the row wrote *"not mine — safe"*: two different claims in one column, of
  /// which only the first had been established. `build_merge_index` then sat at a
  /// [`Budget::merge_work`](super::Budget::merge_work) of **zero** doing
  /// `O(definitions + fragments + condition bytes)` of work and allocation before its first
  /// charge — through the eight rounds of the branch that wrote it and thirteen of the branch that
  /// read it. The same shape one axis over cleared the sibling lossless door as "out of scope by
  /// API shape: it takes no `Budget`", which was true of the ledger question and silent about the
  /// whole-root question that actually mattered.
  ///
  /// **A clearance must record which question it answers, and "out of scope" is not an answer to
  /// "is it bounded".** Each deferral row below therefore names where the answer was checked, and
  /// the merge engine's own three-fact accounting is on `build_merge_index` in [`merge`].
  ///
  /// The same sentence has a second half, because the next round found the same shape with no
  /// ownership boundary to blame — draft 5.8.4's marks bitset, in this file, cleared by the
  /// initialisation audit as *"the operation's own variable count | per operation | safe"*. Both
  /// facts were true and neither was about ordering. **A clearance is scoped to the question the
  /// audit was asking, and an audit that asks size and frequency has said nothing about when the
  /// allocation happens relative to the charge.** Which is why the module header now carries an
  /// enumerated list of the sites rather than a fourth question to ask about them.
  ///
  /// | charge | sits | dimension | gate |
  /// |---|---|---|---|
  /// | projection (lossless door only) | before `project_*` | bytes of `max(source, parse text)` | none — it is the door |
  /// | prep, per operation | before the row is pushed | one node | none — builds what every rule reads |
  /// | prep, per fragment | before the row is pushed | bytes, for `index_fragments`' sort | none — same |
  /// | `collect_definition_edges` | loop top, before `resolve` | depth | none — it is the traversal |
  /// | ” spread name | before `find_fragment` | bytes | none |
  /// | 5.2.2.1 operation names | before the sort | bytes | `OperationNameUniqueness` && **> 1 named operation** |
  /// | 5.5.1.2/5.5.1.3 | before `check_type_condition` | bytes | [`reports_type_conditions`] |
  /// | 5.5.2.2 cycles | before the edge is read | one edge | `FragmentSpreadsMustNotFormCycles` |
  /// | ” cycle subject | before the clone | bytes | ” — the population is edges, not fragments |
  /// | 5.5.1.4 reachability | before the group loop | entries | `FragmentsMustBeUsed` \|\| [`merges`] |
  /// | 5.2.4.1 collection | loop top, before `resolve` | depth | `SingleRootField` |
  /// | ” directive scan | before `conditional_directive` reads them | bytes | ” |
  /// | ” **response name** | before the alias comparison | **bytes** | ” |
  /// | ” condition / spread name | before `type_of` / `find_fragment` | bytes | ” && **a non-empty fragment table** |
  /// | selection walk | loop top, before `resolve` | depth | none — it is the traversal |
  /// | ” field / inline condition | before each resolution | bytes | none |
  /// | ” spread name | before `find_fragment` | bytes | **a non-empty fragment table** |
  /// | 5.7.x directives | head of `check_directives` | bytes | [`Validator::reaches_directives`] |
  /// | 5.4.x arguments | head of `check_arguments` | bytes | [`Validator::reaches_arguments`] |
  /// | 5.4.3 presence half | before **each written argument** the scan resolves | bytes | `check` && `RequiredArguments`; the scan stops where it matches |
  /// | 5.6.3 field list | head of `check_input_object` | bytes | `InputObjectFieldUniqueness` && **> 1 field** |
  /// | 5.6.4 presence half | before **each written field** the scan resolves | bytes | `InputObjectRequiredFields`; the scan stops where it matches |
  /// | value walk | loop top, before `resolve` | depth | [`Validator::walks_values`], and the family's `HAS_VARIABLES` |
  /// | ” object field name | before the schema lookup | bytes | ” |
  /// | scalar / enum literal | before the coercion reads it | bytes | `check` && `ValuesOfCorrectType` |
  /// | 5.8.1 index build | in the collection loop, before the sort | bytes | (`collects_usages` \|\| `VariableUniqueness`) && **> 1 declaration** |
  /// | variable declared type | before `pack_type` | bytes | `VariablesAreInputTypes` \|\| (`checks_values` && this definition has a default) |
  /// | 5.8.3/5.8.5 usage | before the index search | bytes | [`collects_usages`] |
  /// | ” duplicate run | before the marking | entries | `AllVariablesUsed` — the bitset's only reader |
  /// | 5.6.1 OneOf subject | before the clone | bytes | `ValuesOfCorrectType` |
  /// | 5.8.5 usage type | before `pack_type` | bytes | `AllVariableUsagesAreAllowed` |
  /// | 5.8.4 | — | reads bits | charged when the index was built |
  /// | 5.2.1.1, 5.2.3.1 | — | `O(1)` per operation | charged at prep |
  /// | 5.3.2 merge engine | its own ledger, `build_merge_index` first | [`Budget::merge_work`](super::Budget::merge_work) | [`merges`] |
  /// | 5.3.2 conflict subject | before the clone | bytes | `FieldSelectionMerging` |
  /// | 5.5.2.3 possible objects | before **each word** the intersection reads | words | `FragmentSpreadIsPossible` && the types differ |
  /// | projection + whole-root check (lossless) | before both | bytes of `max(source, parse text)` | none — it is the door |
  ///
  /// # And three populations every row answers for separately
  ///
  /// **n = 0**, **n = 1**, and **the shortest-circuiting path**. They are three questions and not
  /// one: an empty population is a comparator never invoked, a singleton is a sort that compares
  /// nothing while the *search* over it still reads a name, and a short circuit is a scan that
  /// stops. Answering only one of the three is how a charge for a sort survived onto a list with
  /// one element in it, and how a charge for a binary search survived onto an index with none.
  ///
  /// # Four standing arguments the rows lean on
  ///
  /// - **Sorts.** 5.2.2.1, 5.4.2, 5.6.3, 5.7.3, 5.8.1 and the fragment index do `N log N`
  ///   comparisons against `N` units of prepayment. `log N` is at most thirty-two whatever the
  ///   document does — a bounded constant multiple of the charge, not a second factor a client can
  ///   grow.
  /// - **Binary searches.** `find_fragment` and the variable index: the same argument one
  ///   dimension smaller.
  /// - **Schema lookups keyed by an already-charged name.** Free by construction — the schema is
  ///   the server's, so its group sizes are not an input.
  /// - **A diagnostic's subject clone.** Charged in the name's own bytes in front of every clone,
  ///   which bounds how many clones a run makes and how long a name each one names — and **not**
  ///   what `S::clone` does with them, because `AsRef<[u8]> + Clone` promises no relationship
  ///   between the two. `&str` is `O(1)` and `String` is `O(L)` against `L / 8`; anything else is
  ///   the caller's, and [`Budget::validation_work`](super::Budget::validation_work) says so where
  ///   a consumer reads the ceiling. Three sites cloned ahead of that charge until al8n/smear#198:
  ///   5.2.1.1's operation name, 5.5.2.2's cycle target, and 5.6.1's OneOf field.
  #[inline]
  pub(super) fn spend(&mut self, units: u32, blame: SimpleSpan) -> ControlFlow<()> {
    match self.left.take(units) {
      Some(left) => {
        self.left = left;
        ControlFlow::Continue(())
      }
      None => self.refuse(blame),
    }
  }

  /// Refuses the document for reaching
  /// [`Budget::validation_work`](super::Budget::validation_work), and abandons every remaining
  /// pass.
  ///
  /// Always [`ControlFlow::Break`], including when the rule is filtered out and there is nothing
  /// to emit. That is the whole difference between a bound and a suggestion: the caller switching
  /// the diagnostic off must not switch the *stopping* off with it.
  ///
  /// Idempotent in what it emits, for the reason the merge engine's own trip is: a collecting sink
  /// does not stop the unwinding, and one refusal per remaining unit of work is noise.
  #[cold]
  fn refuse(&mut self, blame: SimpleSpan) -> ControlFlow<()> {
    // `Left(0)` and not `Off`: this is an exhausted bound, which is the opposite of an absent one.
    self.left = Ledger::Left(0);
    if !self.tripped {
      self.tripped = true;
      if self.on(Rule::ValidationWorkBudget) {
        let diagnostic = Diagnostic::new(Rule::ValidationWorkBudget, blame)
          .context(Context::Count(self.budget.validation_work()));
        let _ = self.emit(diagnostic);
      }
    }
    ControlFlow::Break(())
  }

  /// Charges one node plus the bytes of the name it is identified by.
  #[inline]
  pub(super) fn spend_name(&mut self, name: &Name<S>) -> ControlFlow<()> {
    self.spend(units(name_bytes(name).len()), *name.as_span())
  }

  /// Whether anything a **directive list** can reach is enabled for this visit.
  ///
  /// A directive list leads to draft 5.7's own rules, to the argument rules over its arguments, to
  /// the value rules under those, and to the variable usages inside them. The first three are
  /// properties of a definition and are asked with `check`; the last is a property of an operation
  /// and is asked every visit. With none of them enabled, precharging and resolving every
  /// directive name is a refusal a caller can be handed for a long spelling nothing reads.
  ///
  /// `has_variables` is [`ValueLike::HAS_VARIABLES`](super::nodes::ValueLike::HAS_VARIABLES) for
  /// the family under this list. A **constant** directive's arguments cannot contain a variable, so
  /// no usage rule can act on one however many are enabled — the gate used to admit them anyway,
  /// which walked and charged a variable definition's default directives for a rule set that could
  /// only ever conclude nothing.
  #[inline]
  fn reaches_directives(&self, check: bool, has_variables: bool) -> bool {
    (check && (self.checks_directives || self.checks_arguments || self.checks_values))
      || self.descends_for_usages(has_variables)
  }

  /// Whether a **variable leaf** under here can still be read.
  ///
  /// Three conditions, and the middle one is a *scope* rather than a rule.
  /// `check_variable_usage` discards every leaf outside an operation — the specification scopes
  /// draft 5.8 to one — so a fragment nothing reached descends a variable-capable value tree for
  /// nobody, and the predicate that decided to descend it did not mention `in_operation` at all.
  #[inline]
  fn descends_for_usages(&self, has_variables: bool) -> bool {
    has_variables && self.in_operation && self.collects_usages
  }

  /// Whether a name on the path **down** to a variable leaf has a reader.
  ///
  /// Descending and *resolving* are different work. Draft 5.8.3 asks whether a name was declared
  /// and 5.8.4 whether it was used, and both are answered at the leaf itself; only 5.8.5 reads
  /// anything above it, because the position's expected type comes from resolving the argument,
  /// field or input-object name the leaf sits under. One predicate gated both, so a
  /// `AllVariableUsesDefined`-only rule set charged and schema-resolved every ancestor spelling on
  /// the way to a leaf that needed none of them.
  ///
  /// `local` is the definition-local half the caller has already computed, since which rules those
  /// are differs by one entry between a directive list and an argument list.
  #[inline]
  fn resolves_positions(&self, local: bool) -> bool {
    local || (self.in_operation && self.reads_usage_positions)
  }

  /// Whether anything an **argument list** can reach is enabled for this visit.
  ///
  /// [`Validator::reaches_directives`] one level in: draft 5.7 is no longer downstream.
  #[inline]
  fn reaches_arguments(&self, check: bool, has_variables: bool) -> bool {
    (check && (self.checks_arguments || self.checks_values))
      || self.descends_for_usages(has_variables)
  }

  /// Whether anything a **value literal** can reach is enabled for this visit.
  #[inline]
  pub(super) fn walks_values(&self, check: bool, has_variables: bool) -> bool {
    (check && self.checks_values) || self.descends_for_usages(has_variables)
  }

  /// Charges a whole list of names before anything sorts, hashes or compares any of them.
  ///
  /// The prepayment shape. Three rules in this crate — 5.4.2, 5.6.3 and 5.7.3 — begin by *sorting*
  /// a list of names the client wrote, and a per-item charge inside the loop that follows arrives
  /// after `O(N log N)` comparisons have already happened.
  pub(super) fn spend_names<'n, I>(&mut self, names: I) -> ControlFlow<()>
  where
    I: IntoIterator<Item = &'n Name<S>>,
    S: 'n,
  {
    for name in names {
      self.spend_name(name)?;
    }
    ControlFlow::Continue(())
  }

  /// Charges the base name of a type reference before [`Validator::pack_type`] hashes it.
  ///
  /// The walk to the base is bounded by [`MAX_WRAPPERS`] — that is what makes it safe to do
  /// *before* the charge rather than after — and the name it finds is not bounded by anything.
  /// Draft 5.8.5 calls `pack_type` once per **usage**, so a variable used `U` times had its
  /// declared type's spelling hashed `U` times for nothing.
  pub(super) fn spend_type(&mut self, ty: &Type<Name<S>>, blame: SimpleSpan) -> ControlFlow<()> {
    let mut cursor = ty;
    let mut depth = 0usize;
    loop {
      match cursor {
        Type::Name(named) => return self.spend(units(name_bytes(named.name()).len()), blame),
        Type::List(list) => {
          if depth >= MAX_WRAPPERS as usize {
            // Deeper than the packed representation admits, so `pack_type` will refuse before it
            // reaches a name. One unit for the walk that got here.
            return self.spend(1, blame);
          }
          depth += 1;
          cursor = list.ty();
        }
      }
    }
  }

  /// The spelling a diagnostic will carry, charged in front of the copy that produces it.
  ///
  /// **The ledger is a parameter, because this crate has two and the door serves both.** Draft
  /// 5.3.2's public contract assigns its work to
  /// [`Budget::merge_work`](super::Budget::merge_work) and reserves
  /// [`Budget::validation_work`](super::Budget::validation_work) for every other pass. Centralising
  /// the clone charge here was right about *charging* and silent about *which ledger*, so a merge
  /// conflict's subject debited the validation one: a tight `validation_work` could replace a real
  /// draft 5.3.2 diagnostic with a resource refusal while `merge_work` still had room, and with the
  /// validation bound switched off the copy was not accounted anywhere. Both directions are
  /// defects and the first is the worse one.
  ///
  /// Every caller answers the same question — *which ledger owns the pass this subject belongs
  /// to?* — and there are fifteen of them: fourteen draft §5 passes outside 5.3.2, and
  /// `merge::report_merge`.
  ///
  /// **The only way to get one.** Every direct `Diagnostic::subject` call goes through here, so a
  /// site that wants to name a spelling has to pay for it to obtain one — the charge cannot be
  /// forgotten, put on the wrong string, or written by a caller who did not know the callee would
  /// clone. Two sites had it on the wrong string and two more had it only after the copy before
  /// al8n/smear#198; centralising is what stops the next one, since the hand-written form is no
  /// longer reachable.
  ///
  /// What the charge covers, and what it does not, is on
  /// [`Budget::validation_work`](super::Budget::validation_work): the name's bytes and the number
  /// of copies, not whatever a caller's `S::clone` chooses to do.
  #[inline]
  fn subject_v(&mut self, name: &Name<S>) -> ControlFlow<(), S> {
    self.subject(name, Charged::Validation)
  }

  /// [`Validator::subject_v`]'s general form: the ledger is named at the call site.
  fn subject(&mut self, name: &Name<S>, ledger: Charged) -> ControlFlow<(), S> {
    match ledger {
      Charged::Validation => {
        self.spend_name(name)?;
      }
      Charged::Merge => {
        if !self.charge(byte_units(name_bytes(name).len())) {
          let limit = self.budget.merge_work();
          self.trip(Rule::MergeWorkBudget, limit)?;
          // The bound refused, so there is no subject to hand back and no report to make: `trip`
          // has recorded the refusal and the engine unwinds from here.
          return ControlFlow::Break(());
        }
      }
    }
    ControlFlow::Continue(name.source().clone())
  }

  /// Emits a diagnostic naming a source spelling, at that spelling's own span.
  ///
  /// **Charges the spelling it is about to clone, here, rather than trusting a caller to have
  /// charged the right one.** Every caller does charge *something* before reaching this, and twice
  /// that something was a different string: 5.2.4.1 charged a short response alias and cloned the
  /// arbitrarily long reserved field name underneath it, and a `VariablesAreInputTypes`-only rule
  /// set charged the declared type and cloned an unindexed variable name. A charge that is in
  /// front, in the right dimension and over the right population still bounds nothing if it names
  /// a different string than the work reads.
  ///
  /// This is the double charge the callers keep, and deliberately: theirs prices a *resolution* or
  /// a *comparison* over the name they pass, and this prices the copy. See
  /// [`Budget::validation_work`](super::Budget::validation_work) for what the copy's price does and
  /// does not cover.
  fn report_name(&mut self, rule: Rule, name: &Name<S>, context: Context) -> ControlFlow<()> {
    let subject = self.subject_v(name)?;
    let diagnostic = Diagnostic::new(rule, *name.as_span())
      .subject(subject)
      .context(context);
    self.emit(diagnostic)
  }

  // -- schema helpers -------------------------------------------------------------------------

  /// Resolves a source name to a schema type.
  fn type_of(&self, name: &Name<S>) -> Option<TypeId> {
    self.schema.type_of_sym(self.schema.sym(name_bytes(name))?)
  }

  /// Flattens an AST type reference into the schema's packed form.
  ///
  /// `None` when the base type is not in the schema or the reference nests deeper than the
  /// representation admits. The walk is iterative for the same reason every other walk here is.
  fn pack_type(&self, ty: &Type<Name<S>>) -> Option<PackedType> {
    let mut list_required = [false; MAX_WRAPPERS as usize];
    let mut depth = 0usize;
    let mut cursor = ty;
    let named = loop {
      match cursor {
        Type::Name(named) => break named,
        Type::List(list) => {
          if depth >= list_required.len() {
            return None;
          }
          list_required[depth] = list.required();
          depth += 1;
          cursor = list.ty();
        }
      }
    };
    let sym = self.schema.sym(name_bytes(named.name()))?;
    let id = self.schema.type_of_sym(sym)?;
    let mut packed = PackedType::named(sym, id);
    if named.required() {
      packed = packed.push_non_null()?;
    }
    for required in list_required[..depth].iter().rev() {
      packed = packed.push_list()?;
      if *required {
        packed = packed.push_non_null()?;
      }
    }
    Some(packed)
  }

  // -- the run --------------------------------------------------------------------------------

  fn run(&mut self) -> ControlFlow<()> {
    self.prep()?;
    self.check_operations()?;
    self.check_fragment_declarations()?;
    self.check_fragment_cycles()?;
    self.check_fragments_used()?;
    self.check_subscriptions()?;
    self.walk_operations()?;
    self.walk_unreached_fragments()?;
    // Last, and deliberately so. Draft 5.3.2 is the only rule with a working set and the only one
    // an attacker can make expensive, so every cheap structural refusal — undefined spreads,
    // fragment cycles — is already established before it expands anything. Reordering this would
    // change the threat model, not just the diagnostic order.
    self.check_field_merging()
  }

  // -- 1. prep --------------------------------------------------------------------------------

  fn prep(&mut self) -> ControlFlow<()> {
    let document = self.document;
    for (index, described) in document.definitions().iter().enumerate() {
      let index = index as u32;
      match described.node() {
        ExecutableDefinition::Operation(operation) => {
          let (root, named, span) = match operation {
            OperationDefinition::Named(named) => (
              root_operation(named.operation_type()),
              named.name().is_some(),
              match named.name() {
                Some(name) => *name.as_span(),
                None => *named.operation_type().span(),
              },
            ),
            // Query shorthand: an anonymous query, blamed at its own braces.
            OperationDefinition::Shorthand(set) => (RootOperation::Query, false, *set.span()),
          };
          // The prep sweep is linear in the document, but it is also where the fragment index is
          // sorted by name — `F log F` comparisons over bytes the client chose. The sort's log
          // factor is at most thirty-two whatever the document does, which is a constant multiple
          // of what is charged here and not a second factor.
          self.spend(1, span)?;
          self.scratch.operations.push(OperationRow {
            definition: index,
            root: root.index() as u8,
            named,
            span,
            edges: Default::default(),
          });
        }
        ExecutableDefinition::Fragment(fragment) => {
          self.spend_name(fragment.name())?;
          self.scratch.fragments.push(FragmentRow {
            definition: index,
            span: *fragment.name().as_span(),
            group: Default::default(),
            edges: Default::default(),
          });
        }
      }
    }

    self.index_fragments()?;
    self.collect_edges()
  }

  /// Sorts the fragment ordinals by name, records each name's group, and reports draft 5.5.1.1.
  fn index_fragments(&mut self) -> ControlFlow<()> {
    let document = self.document;
    let count = self.scratch.fragments.len() as u32;
    {
      let scratch = &mut *self.scratch;
      scratch.order.extend(0..count);
      let rows = &scratch.fragments;
      // Ties break on the ordinal, so the order is total and the first definition of a name is
      // always the group's first element — which is the one spreads resolve to.
      scratch.order.sort_unstable_by(|a, b| {
        let left = fragment_name(document, rows, *a);
        let right = fragment_name(document, rows, *b);
        left.cmp(right).then(a.cmp(b))
      });
    }

    let mut start = 0usize;
    while start < self.scratch.order.len() {
      let first = self.scratch.order[start];
      let name = fragment_name(document, &self.scratch.fragments, first);
      let mut end = start + 1;
      while end < self.scratch.order.len()
        && fragment_name(document, &self.scratch.fragments, self.scratch.order[end]) == name
      {
        end += 1;
      }
      let group = range32(start as u32, end as u32);
      for slot in start..end {
        let ordinal = self.scratch.order[slot] as usize;
        self.scratch.fragments[ordinal].group = group;
      }
      if end - start > 1 && self.on(Rule::FragmentNameUniqueness) {
        let related = self.scratch.fragments[first as usize].span;
        for slot in start + 1..end {
          let ordinal = self.scratch.order[slot];
          let row = self.scratch.fragments[ordinal as usize];
          let Some(fragment) = fragment(document, row.definition) else {
            continue;
          };
          let subject = self.subject_v(fragment.name())?;
          let diagnostic = Diagnostic::new(Rule::FragmentNameUniqueness, row.span)
            .subject(subject)
            .related(related);
          self.emit(diagnostic)?;
        }
      }
      start = end;
    }
    ControlFlow::Continue(())
  }

  /// Returns the ordinal a fragment name resolves to — the first definition of that name.
  fn find_fragment(&self, name: &[u8]) -> Option<u32> {
    let document = self.document;
    let rows = &self.scratch.fragments;
    let order = &self.scratch.order;
    let slot = order
      .binary_search_by(|ordinal| fragment_name(document, rows, *ordinal).cmp(name))
      .ok()?;
    // Ties are ordered by ordinal, so walking back to the group's start finds the first
    // definition rather than whichever one the search landed on.
    let ordinal = order[slot];
    let group = rows[ordinal as usize].group;
    order.get(group.start() as usize).copied()
  }

  /// Walks every definition once, recording its fragment spreads as graph edges and reporting
  /// draft 5.5.2.1 for the ones that name nothing.
  fn collect_edges(&mut self) -> ControlFlow<()> {
    let document = self.document;
    let total = document.definitions().len() as u32;
    let mut operations = 0usize;
    let mut fragments = 0usize;
    for index in 0..total {
      let start = self.scratch.edges.len() as u32;
      self.collect_definition_edges(index)?;
      let range = range32(start, self.scratch.edges.len() as u32);
      match definition(document, index) {
        Some(ExecutableDefinition::Operation(_)) => {
          self.scratch.operations[operations].edges = range;
          operations += 1;
        }
        Some(ExecutableDefinition::Fragment(_)) => {
          self.scratch.fragments[fragments].edges = range;
          fragments += 1;
        }
        None => {}
      }
    }
    ControlFlow::Continue(())
  }

  fn collect_definition_edges(&mut self, index: u32) -> ControlFlow<()> {
    let document = self.document;
    self.scratch.frames.clear();
    push_frame(&mut self.scratch.frames, Frame::root(index, NONE, 0));
    let mut current = root_selection_set(document, index);
    let blame = current.map_or(SimpleSpan::const_new(0, 0), |set| *set.span());
    while let Some(frame) = self.scratch.frames.last().copied() {
      // One for the iteration; the depth is charged where it is spent. See
      // `selections::walk_selections` for the whole argument.
      self.spend(1, blame)?;
      let Some(set) = current else {
        self.scratch.frames.pop();
        current = self.resolve_frames(blame)?;
        continue;
      };
      let selections = set.selections();
      let Some(selection) = selections.get(frame.cursor as usize) else {
        self.scratch.frames.pop();
        current = self.resolve_frames(blame)?;
        continue;
      };
      if let Some(top) = self.scratch.frames.last_mut() {
        top.cursor += 1;
      }
      match selection {
        Selection::FragmentSpread(spread) => {
          let name = spread.name();
          // `find_fragment` binary-searches the name index, so this reads the spelling about
          // `log F` times — **when there is an index**. With no fragment declared the search
          // invokes its comparator zero times and reads nothing, and the report below charges its
          // own copy through `Validator::subject`. al8n/smear#198.
          if !self.scratch.fragments.is_empty() {
            self.spend_name(name)?;
          }
          let to = self.find_fragment(name_bytes(name));
          if to.is_none() && self.on(Rule::FragmentSpreadTargetDefined) {
            let subject = self.subject_v(name)?;
            let diagnostic =
              Diagnostic::new(Rule::FragmentSpreadTargetDefined, *name.as_span()).subject(subject);
            self.emit(diagnostic)?;
          }
          self.scratch.edges.push(Edge {
            to: to.unwrap_or(NONE),
            span: *spread.span(),
          });
        }
        _ => {
          if let Some(child) = child_selection_set(selection) {
            push_frame(
              &mut self.scratch.frames,
              Frame::child(index, frame.cursor, NONE, 0),
            );
            current = Some(child);
          }
        }
      }
    }
    ControlFlow::Continue(())
  }

  // -- 2. operation rules ---------------------------------------------------------------------

  fn check_operations(&mut self) -> ControlFlow<()> {
    let document = self.document;

    // Prepaid for the **sort**, and for nothing else.
    //
    // It used to name 5.2.1.1 as a second reader, because that rule clones an operation's spelling
    // into a diagnostic's subject and the clone was unpriced. [`Validator::subject`] prices it now,
    // at the clone, so this prepayment is the sort's alone — and leaving 5.2.1.1 in the condition
    // billed every named operation's bytes for a rule that otherwise performs one `O(1)` root
    // lookup and reads no name at all. A long enough name could exhaust the ledger and refuse a
    // document that is valid under the rule the caller selected.
    //
    // The shape is worth naming: **centralising a charge can make an older prepayment redundant**,
    // and a repair that removes a reader leaves every prepayment that named it over-charging. See
    // this module's header for the sweep that question implies.
    // Same shape one rule over, and found by crossing the audit's sub-shapes rather than named by
    // review: this prepays 5.2.2.1's **sort**, and a sort of one named operation compares nothing,
    // as the group scan that reads it walks `start + 1..end` over an empty range.
    let named = self
      .scratch
      .operations
      .iter()
      .filter(|row| row.named)
      .count();
    if self.on(Rule::OperationNameUniqueness) && named > 1 {
      for index in 0..self.scratch.operations.len() {
        if !self.scratch.operations[index].named {
          continue;
        }
        let name = operation_name_bytes(document, &self.scratch.operations, index as u32);
        let span = self.scratch.operations[index].span;
        self.spend(units(name.len()), span)?;
      }
    }

    // 5.2.1.1 — the schema must provide the root operation type the operation needs.
    if self.on(Rule::OperationTypeExistence) {
      for index in 0..self.scratch.operations.len() {
        let row = self.scratch.operations[index];
        let root = RootOperation::ALL[row.root as usize];
        if self.schema.root(root).is_none() {
          let diagnostic =
            Diagnostic::new(Rule::OperationTypeExistence, row.span).context(Context::Root(root));
          let diagnostic = match operation_name(document, row.definition) {
            Some(name) => {
              let subject = self.subject_v(name)?;
              diagnostic.subject(subject)
            }
            None => diagnostic,
          };
          self.emit(diagnostic)?;
        }
      }
    }

    // 5.2.2.1 — named operations must not collide. Sorted index scan, no map.
    if self.on(Rule::OperationNameUniqueness) {
      let base = self.scratch.keys.len();
      for index in 0..self.scratch.operations.len() {
        if self.scratch.operations[index].named {
          self.scratch.keys.push(index as u32);
        }
      }
      {
        let scratch = &mut *self.scratch;
        let rows = &scratch.operations;
        // Ties break on the document index, so the first definition of a name is always the
        // group's first element and every later one is blamed against it.
        scratch.keys[base..].sort_unstable_by(|a, b| {
          let left = operation_name_bytes(document, rows, *a);
          let right = operation_name_bytes(document, rows, *b);
          left.cmp(right).then(a.cmp(b))
        });
      }
      let mut start = base;
      while start < self.scratch.keys.len() {
        let first = self.scratch.keys[start];
        let name = operation_name_bytes(document, &self.scratch.operations, first);
        let mut end = start + 1;
        while end < self.scratch.keys.len()
          && operation_name_bytes(document, &self.scratch.operations, self.scratch.keys[end])
            == name
        {
          end += 1;
        }
        let related = self.scratch.operations[first as usize].span;
        for slot in start + 1..end {
          let row = self.scratch.operations[self.scratch.keys[slot] as usize];
          if let Some(name) = operation_name(document, row.definition) {
            let subject = self.subject_v(name)?;
            let diagnostic = Diagnostic::new(Rule::OperationNameUniqueness, row.span)
              .subject(subject)
              .related(related);
            self.emit(diagnostic)?;
          }
        }
        start = end;
      }
      self.scratch.keys.truncate(base);
    }

    // 5.2.3.1 — an anonymous operation must be the document's only one.
    if self.on(Rule::LoneAnonymousOperation) && self.scratch.operations.len() > 1 {
      for index in 0..self.scratch.operations.len() {
        let row = self.scratch.operations[index];
        if !row.named {
          self.emit(
            Diagnostic::new(Rule::LoneAnonymousOperation, row.span)
              .context(Context::Count(self.scratch.operations.len() as u32)),
          )?;
        }
      }
    }

    ControlFlow::Continue(())
  }

  // -- 3. fragment declaration rules ------------------------------------------------------------

  fn check_fragment_declarations(&mut self) -> ControlFlow<()> {
    // Asked before the condition names are read. `check_type_condition` resolves each one through
    // `Schema::sym`, and here — unlike at an inline fragment — the resolved type is discarded, so
    // with nothing to report the pass hashes a document-chosen name per fragment for nothing.
    // Through [`reports_type_conditions`] rather than a pair written out here: the callee's own
    // guards are the other copy of this condition.
    if !reports_type_conditions(self.rules) {
      return ControlFlow::Continue(());
    }
    let document = self.document;
    for ordinal in 0..self.scratch.fragments.len() {
      let row = self.scratch.fragments[ordinal];
      let Some(fragment) = fragment(document, row.definition) else {
        continue;
      };
      let condition = fragment.type_condition().name();
      // `check_type_condition` resolves it through `Schema::sym`, which hashes every byte.
      self.spend_name(condition)?;
      self.check_type_condition(condition)?;
    }
    ControlFlow::Continue(())
  }

  /// Draft 5.5.1.2 and 5.5.1.3, shared by fragment definitions and inline fragments.
  ///
  /// **A rule added here belongs in [`reports_type_conditions`].** `check_fragment_declarations`
  /// calls this only for what it reports and skips itself when that predicate is false, so a rule
  /// reported here and missing there would never fire on a fragment definition.
  ///
  /// Returns the condition's type when it resolved to a composite one, which is also the scope its
  /// selection set is written against.
  fn check_type_condition(&mut self, condition: &Name<S>) -> ControlFlow<(), Option<TypeId>> {
    let Some(id) = self.type_of(condition) else {
      if self.on(Rule::FragmentSpreadTypeExistence) {
        self.report_name(Rule::FragmentSpreadTypeExistence, condition, Context::None)?;
      }
      return ControlFlow::Continue(None);
    };
    if !self.schema.type_def(id).kind().is_composite() {
      if self.on(Rule::FragmentsOnCompositeTypes) {
        let context = Context::Type(self.schema.type_def(id).name());
        self.report_name(Rule::FragmentsOnCompositeTypes, condition, context)?;
      }
      return ControlFlow::Continue(None);
    }
    ControlFlow::Continue(Some(id))
  }

  // -- 4. the fragment graph --------------------------------------------------------------------

  /// Draft 5.5.2.2, as an iterative depth-first search over integers.
  fn check_fragment_cycles(&mut self) -> ControlFlow<()> {
    if !self.on(Rule::FragmentSpreadsMustNotFormCycles) {
      return ControlFlow::Continue(());
    }
    let count = self.scratch.fragments.len();
    reset_bits(&mut self.scratch.on_path, count);
    reset_bits(&mut self.scratch.done, count);
    for start in 0..count as u32 {
      if get_bit(&self.scratch.done, start) {
        continue;
      }
      self.scratch.graph.clear();
      set_bit(&mut self.scratch.on_path, start);
      self.scratch.graph.push(GraphFrame {
        fragment: start,
        edge: self.scratch.fragments[start as usize].edges.start(),
      });
      while let Some(top) = self.scratch.graph.last().copied() {
        let row = self.scratch.fragments[top.fragment as usize];
        if top.edge >= row.edges.end() {
          clear_bit(&mut self.scratch.on_path, top.fragment);
          set_bit(&mut self.scratch.done, top.fragment);
          self.scratch.graph.pop();
          continue;
        }
        if let Some(frame) = self.scratch.graph.last_mut() {
          frame.edge += 1;
        }
        let edge = self.scratch.edges[top.edge as usize];
        self.spend(1, edge.span)?;
        if edge.to == NONE {
          continue;
        }
        if get_bit(&self.scratch.on_path, edge.to) {
          let subject = self.scratch.fragments[edge.to as usize];
          let Some(target) = fragment(self.document, subject.definition) else {
            continue;
          };
          // Prep charges this name once per fragment and this clones it once per **edge**, so the
          // helper's charge is the one that matches the population.
          let named = self.subject_v(target.name())?;
          let diagnostic = Diagnostic::new(Rule::FragmentSpreadsMustNotFormCycles, edge.span)
            .subject(named)
            .related(subject.span);
          self.emit(diagnostic)?;
          continue;
        }
        if get_bit(&self.scratch.done, edge.to) {
          continue;
        }
        set_bit(&mut self.scratch.on_path, edge.to);
        self.scratch.graph.push(GraphFrame {
          fragment: edge.to,
          edge: self.scratch.fragments[edge.to as usize].edges.start(),
        });
      }
    }
    ControlFlow::Continue(())
  }

  /// Draft 5.5.1.4, as reachability from the operations over the same graph.
  ///
  /// Reachability propagates across every definition sharing a name, so one duplicated fragment
  /// name reports 5.5.1.1 and nothing else — the copy is not also "unused".
  fn check_fragments_used(&mut self) -> ControlFlow<()> {
    // The `reachable` bitset reads like 5.5.1.4's private working set and is not: draft 5.3.2's
    // engine reads it too. So this pass runs for its own rule **or** for anything that starts that
    // engine — and what starts it is [`merges`], the one definition the engine itself is gated on,
    // rather than a copy of its condition. The copy is how this guard shipped naming two of the
    // engine's three activating rules.
    if !self.on(Rule::FragmentsMustBeUsed) && !merges(self.rules) {
      return ControlFlow::Continue(());
    }
    let count = self.scratch.fragments.len();
    reset_bits(&mut self.scratch.reachable, count);
    self.scratch.graph.clear();
    for index in 0..self.scratch.operations.len() {
      let edges = self.scratch.operations[index].edges;
      for slot in edges.start()..edges.end() {
        let to = self.scratch.edges[slot as usize].to;
        self.mark_reachable(to)?;
      }
    }
    while let Some(frame) = self.scratch.graph.pop() {
      let edges = self.scratch.fragments[frame.fragment as usize].edges;
      for slot in edges.start()..edges.end() {
        let to = self.scratch.edges[slot as usize].to;
        self.mark_reachable(to)?;
      }
    }

    if !self.on(Rule::FragmentsMustBeUsed) {
      return ControlFlow::Continue(());
    }
    for ordinal in 0..count as u32 {
      if get_bit(&self.scratch.reachable, ordinal) {
        continue;
      }
      let row = self.scratch.fragments[ordinal as usize];
      let Some(target) = fragment(self.document, row.definition) else {
        continue;
      };
      let subject = self.subject_v(target.name())?;
      let diagnostic = Diagnostic::new(Rule::FragmentsMustBeUsed, row.span).subject(subject);
      self.emit(diagnostic)?;
    }
    ControlFlow::Continue(())
  }

  /// Marks a fragment and every same-named definition reachable, queueing the newly marked.
  ///
  /// The whole group, not one member, because reachability propagates across a duplicated name —
  /// which is also the quadratic here: `E` spreads of one name shared by `G` definitions walk
  /// `E · G` members off `O(E + G)` of syntax. 5.5.1.1 has already reported the duplication, but
  /// the walk still happens, so it is charged.
  fn mark_reachable(&mut self, ordinal: u32) -> ControlFlow<()> {
    if ordinal == NONE {
      return ControlFlow::Continue(());
    }
    let group = self.scratch.fragments[ordinal as usize].group;
    self.spend(
      group.end().saturating_sub(group.start()),
      self.scratch.fragments[ordinal as usize].span,
    )?;
    for slot in group.start()..group.end() {
      let member = self.scratch.order[slot as usize];
      if !set_bit(&mut self.scratch.reachable, member) {
        self.scratch.graph.push(GraphFrame {
          fragment: member,
          edge: 0,
        });
      }
    }
    ControlFlow::Continue(())
  }

  // -- 5. subscriptions ---------------------------------------------------------------------------

  fn check_subscriptions(&mut self) -> ControlFlow<()> {
    if !self.on(Rule::SingleRootField) {
      return ControlFlow::Continue(());
    }
    let Some(root) = self.schema.root(RootOperation::Subscription) else {
      return ControlFlow::Continue(());
    };
    for index in 0..self.scratch.operations.len() {
      let row = self.scratch.operations[index];
      if RootOperation::ALL[row.root as usize] != RootOperation::Subscription {
        continue;
      }
      self.check_subscription_roots(row, root)?;
    }
    ControlFlow::Continue(())
  }

  /// Draft 5.2.4.1's `CollectSubscriptionFields`, as an explicit walk.
  ///
  /// It only has to answer "is the collected map a set of one, and is that one an introspection
  /// field", so it never builds the map: two distinct response names are enough to fail, and the
  /// first response name seen is the only one it needs to keep.
  fn check_subscription_roots(&mut self, row: OperationRow, root: TypeId) -> ControlFlow<()> {
    let document = self.document;
    let fragments = self.scratch.fragments.len();
    self.scratch.visited.begin(fragments);
    self.scratch.roots.clear();
    push_frame(
      &mut self.scratch.roots,
      Frame::root(row.definition, NONE, 0),
    );
    let mut current = root_selection_set(document, row.definition);

    // The rule only asks whether the collected map is a set of one, so the first response name
    // seen is all that has to be kept: any later name that differs makes at least two entries.
    // The *field* name is kept alongside it, because "must not be an introspection field" is a
    // question about the field and not about the key an alias would give it.
    let mut first_response: Option<&'d Name<S>> = None;
    let mut first_field: Option<&'d Name<S>> = None;
    let mut multiple = false;
    let blame = current.map_or(SimpleSpan::const_new(0, 0), |set| *set.span());

    while let Some(frame) = self.scratch.roots.last().copied() {
      // The other stack, same shape: one for the iteration, the depth at the resolution.
      self.spend(1, blame)?;
      let Some(set) = current else {
        self.scratch.roots.pop();
        current = self.resolve_roots(blame)?;
        continue;
      };
      let Some(selection) = set.selections().get(frame.cursor as usize) else {
        self.scratch.roots.pop();
        current = self.resolve_roots(blame)?;
        continue;
      };
      if let Some(top) = self.scratch.roots.last_mut() {
        top.cursor += 1;
      }

      // "{selection} must not provide the `@skip`/`@include` directive" — the whole reason this
      // collection exists is that it has no runtime variables to evaluate them with.
      if let Some(directive) = self.conditional_directive(selection)? {
        let subject = self.subject_v(directive)?;
        let diagnostic =
          Diagnostic::new(Rule::SingleRootField, *directive.as_span()).subject(subject);
        self.emit(diagnostic)?;
      }

      match selection {
        Selection::Field(field) => {
          let response = nodes::response_name(field);
          match first_response {
            // The first root field is *stored*, not compared: two references move and no byte is
            // read. The charge below used to sit above this match and bill it anyway.
            None => {
              first_response = Some(response);
              first_field = Some(field.name());
            }
            Some(seen) => {
              // Charged in **bytes**, because what happens next is a byte comparison, and the
              // iteration charge at the top of this loop prices a coordinate walk that costs no
              // bytes at all. Two very long aliases sharing a prefix are compared end to end.
              self.spend_name(response)?;
              if name_bytes(seen) != name_bytes(response) {
                multiple = true;
              }
            }
          }
        }
        Selection::InlineFragment(inline) => {
          let applies = match inline.type_condition() {
            Some(condition) => {
              // `condition_applies` resolves the name through `Schema::sym`.
              self.spend_name(condition.name())?;
              self.condition_applies(condition.name(), root)
            }
            None => true,
          };
          if applies {
            push_frame(
              &mut self.scratch.roots,
              Frame::child(frame.definition, frame.cursor, NONE, 0),
            );
            current = Some(inline.selection_set());
          }
        }
        Selection::FragmentSpread(spread) => {
          // `find_fragment` binary-searches the name index and `condition_applies` below hashes
          // the target's condition; both read bytes the document chose — and neither happens with
          // no fragment declared, where the search compares nothing and there is no target.
          if !self.scratch.fragments.is_empty() {
            self.spend_name(spread.name())?;
          }
          let Some(ordinal) = self.find_fragment(name_bytes(spread.name())) else {
            continue;
          };
          if self.scratch.visited.visit(ordinal) {
            continue;
          }
          let target = self.scratch.fragments[ordinal as usize];
          let Some(body) = fragment(document, target.definition) else {
            continue;
          };
          let condition = body.type_condition().name();
          self.spend_name(condition)?;
          if !self.condition_applies(condition, root) {
            continue;
          }
          push_frame(
            &mut self.scratch.roots,
            Frame::root(target.definition, NONE, 0),
          );
          current = Some(body.selection_set());
        }
      }
    }

    if multiple {
      // At least two entries. The exact count would cost a second walk and change nothing about
      // the verdict, so the context reports the bound it established.
      self.emit(Diagnostic::new(Rule::SingleRootField, row.span).context(Context::Count(2)))?;
    } else {
      match first_field {
        None => {
          self.emit(Diagnostic::new(Rule::SingleRootField, row.span).context(Context::Count(0)))?;
        }
        Some(field) if is_reserved(name_bytes(field)) => {
          self.report_name(Rule::SingleRootField, field, Context::None)?;
        }
        Some(_) => {}
      }
    }
    ControlFlow::Continue(())
  }

  /// [`selections::resolve`] over the subscription root stack, charged for the descent it makes.
  ///
  /// [`Validator::resolve_frames`]'s twin over the other stack, and for the same reason — including
  /// the reason its number was wrong. This stack enters a fragment body through
  /// [`Frame::root`](crate::scratch::Frame::root) exactly as the selection stack does, so a
  /// subscription whose root fields sit inside a fragment reached at depth `D` billed `D` for each
  /// `O(1)` resolution in it. Named in the same finding, repaired in the same way, and through the
  /// same helper rather than a second copy of the arithmetic. al8n/smear#198.
  fn resolve_roots(
    &mut self,
    blame: SimpleSpan,
  ) -> ControlFlow<(), Option<&'d smear_parser::graphql::ast::SelectionSet<S>>> {
    self.spend(
      selections::suffix_of(&self.scratch.roots).len() as u32,
      blame,
    )?;
    ControlFlow::Continue(selections::resolve(
      self.document,
      selections::suffix_of(&self.scratch.roots),
    ))
  }

  /// Returns the `@skip` or `@include` a selection carries, if it carries one.
  ///
  /// `&mut self` and a [`ControlFlow`] because it **scans every directive on the selection**, and a
  /// selection carries as many as the document writes. The subscription pass charged one flat unit
  /// per selection and then came here, so `O` selections each carrying `D` directives spent `O`
  /// units on `O · D` of work off `O + D` of syntax.
  fn conditional_directive(
    &mut self,
    selection: &'d Selection<S>,
  ) -> ControlFlow<(), Option<&'d Name<S>>> {
    let directives = match selection {
      Selection::Field(field) => field.directives(),
      Selection::FragmentSpread(spread) => spread.directives(),
      Selection::InlineFragment(inline) => inline.directives(),
    };
    let Some(directives) = directives else {
      return ControlFlow::Continue(None);
    };
    let directives = directives.directives();
    // One unit per directive **examined**, charged before examining it, and one unit is the right
    // dimension: the test is `matches!(bytes, b"skip" | b"include")`, which compares a length and
    // then at most seven bytes however long the spelling is. The scan also stops at the first
    // match. Charging every name's full length up front was the wrong dimension *and* the wrong
    // count — the two halves of al8n/smear#198's tenth and eleventh rounds in one line. Found by
    // the taken-branch audit rather than named by the review.
    for directive in directives {
      self.spend(1, *directive.name().as_span())?;
      let name = directive.name();
      if matches!(name_bytes(name), b"skip" | b"include") {
        return ControlFlow::Continue(Some(name));
      }
    }
    ControlFlow::Continue(None)
  }

  /// `DoesFragmentTypeApply` against a known object type.
  fn condition_applies(&self, condition: &Name<S>, object: TypeId) -> bool {
    match self.type_of(condition) {
      // An undefined or non-composite condition is 5.5.1.2/5.5.1.3's business; here it simply
      // cannot contribute a root field.
      Some(id) => self.schema.is_possible_object(id, object),
      None => false,
    }
  }

  // -- 6. the per-operation walks ------------------------------------------------------------

  fn walk_operations(&mut self) -> ControlFlow<()> {
    let document = self.document;
    for index in 0..self.scratch.operations.len() {
      let row = self.scratch.operations[index];
      let Some(operation) = operation(document, row.definition) else {
        continue;
      };

      self.variables = match operation {
        OperationDefinition::Named(named) => match named.variable_definitions() {
          Some(definitions) => definitions.variable_definitions(),
          None => &[],
        },
        OperationDefinition::Shorthand(_) => &[],
      };
      self.in_operation = true;

      self.check_variable_definitions()?;

      let root = RootOperation::ALL[row.root as usize];
      if let OperationDefinition::Named(named) = operation
        && let Some(directives) = named.directives()
      {
        self.check_directives(directives.directives(), directive_location(root), true)?;
      }

      let scope = self.schema.root(root).map_or(NONE, |id| id.get());
      let fragments = self.scratch.fragments.len();
      self.scratch.visited.begin(fragments);
      self.walk_selections(Frame::root(row.definition, scope, Frame::CHECK))?;

      self.check_variables_used()?;

      // The index is a persistent prefix of `keys` for exactly the length of this operation. Every
      // other user of the buffer pushes at its current length and truncates back, so it survives
      // the walk; this is where it stops.
      self
        .scratch
        .keys
        .truncate(self.variable_index.start() as usize);
      self.variables = &[];
      self.in_operation = false;
    }
    ControlFlow::Continue(())
  }

  // -- 7. fragments nothing reached -------------------------------------------------------------

  fn walk_unreached_fragments(&mut self) -> ControlFlow<()> {
    let document = self.document;
    for ordinal in 0..self.scratch.fragments.len() {
      let row = self.scratch.fragments[ordinal];
      if get_bit(&self.scratch.checked, row.definition) {
        continue;
      }
      let Some(body) = fragment(document, row.definition) else {
        continue;
      };
      // The condition was already reported on, if it needed reporting, by the declaration pass —
      // but resolving it still hashes it, so it is charged, and only when the scope it produces has
      // a reader. `in_operation` is false here, so this is the definition-local half alone: the
      // sibling of the three selection-walk sites, named in the same finding so it does not become
      // the next one. al8n/smear#198.
      let condition = body.type_condition().name();
      let scope = if self.reads_field_positions {
        self.spend_name(condition)?;
        self.composite_of(condition).map_or(NONE, |id| id.get())
      } else {
        NONE
      };
      self.begin_fragment(row.definition, true)?;
      self.walk_selections(Frame::root(row.definition, scope, Frame::CHECK))?;
    }
    ControlFlow::Continue(())
  }

  /// Visits a fragment definition's own directives, and records that its definition-local rules
  /// have run.
  ///
  /// `check` is the **definition-local** half: draft 5.7's rules over these directives fire once
  /// however many operations spread the fragment, so the `checked` bit and the reporting move
  /// together. It is deliberately not the whole of what this function does.
  ///
  /// A fragment definition's directives are the **non-constant** family — `fragment F on Dog
  /// @include(if: $v)` puts a variable usage on the definition itself — and a usage is
  /// *operation-local*: draft 5.8 scopes to one operation, and the same fragment is valid under one
  /// operation's variables and invalid under another's. So this runs on **every** operation's
  /// expansion and only the reporting is deduplicated.
  ///
  /// It used to be called only on the first expansion, behind the same bit that gates the
  /// reporting. Every later operation therefore missed those usages: 5.8.3 and 5.8.5 could accept
  /// an undefined or incompatible variable, 5.8.4 could call a used one unused, and **the verdict
  /// depended on the order the operations were written in**. Present on a46ab95 and on every
  /// commit of this branch before al8n/smear#198's eighth round; not a consequence of any gate this
  /// branch added, which the diff over `selections.rs` shows.
  pub(super) fn begin_fragment(&mut self, index: u32, check: bool) -> ControlFlow<()> {
    if check {
      grow_bits(&mut self.scratch.checked, index as usize + 1);
      set_bit(&mut self.scratch.checked, index);
    }
    let Some(body) = fragment(self.document, index) else {
      return ControlFlow::Continue(());
    };
    match body.directives() {
      Some(directives) => self.check_directives(
        directives.directives(),
        DirectiveLocation::FragmentDefinition,
        check,
      ),
      None => ControlFlow::Continue(()),
    }
  }

  // -- variables ------------------------------------------------------------------------------

  /// Draft 5.8.1 and 5.8.2, plus the value and directive rules over a variable definition's own
  /// default value and directives — the one place in an executable document where constant values
  /// appear.
  fn check_variable_definitions(&mut self) -> ControlFlow<()> {
    let definitions = self.variables;
    // Read by draft 5.8.4 and nothing else, so it is gated — clearing it for a rule set that never
    // looks is work in front of every gate this function has.
    //
    // And **sized by the operation's own declaration list**, which is where `reset_bits` stopped
    // being safe here. The other three bitsets in this module are sized to the document's
    // *fragment* count, and the prep sweep charges at least one unit per fragment before any of
    // them is reset, so `count / 64` words of zeroing sit behind `count` units already taken. Prep
    // charges per **definition**, not per variable: one operation declaring `V` variables is one
    // definition, so this buffer's `V / 64` words sat behind a single unit, and a `validation_work`
    // already spent to its last unit still bought the allocation and the zeroing before the next
    // `spend` could refuse. al8n/smear#198.
    //
    // The repair is `build_merge_index`'s, move for move, because it is the same defect: `clear`
    // first — `O(1)` on a `Vec<u64>`, no drop glue, no allocation, and it is what leaves the table
    // **empty** rather than the previous operation's width on a refusal — then the true word count,
    // then the charge in `count_units`, which saturates into a refusal rather than truncating into
    // a budget it fits, and only then the `resize` that does the writing.
    if self.marks_usage {
      self.scratch.used.clear();
      if let Some(first) = definitions.first() {
        let words = definitions.len().div_ceil(64);
        let variable = first.node().variable();
        self.spend(count_units(words), *variable.span())?;
        self.scratch.used.resize(words, 0);
      }
    }

    // The operation's variable-name index, built once and read by every usage.
    //
    // It replaces the scan over *every* definition that each usage used to run: `U` usages against
    // `V` definitions was `U · V` name comparisons before any ledger was consulted, and 4,000 of
    // each measured 60 ms off 250 KB of syntax, against 0.33 ms for the same declarations with one
    // usage. al8n/smear#198.
    //
    // Ordinals sorted by name with ties broken on the ordinal — the order draft 5.8.1 already
    // needed, so this is one sort where there were two. **The tie-break is what keeps the marking
    // semantics exactly as they were.** A name's definitions form a contiguous run ordered by
    // ordinal, so the run's first element is the lowest-numbered definition of that name — the one
    // a usage resolves against — and the run entire is what gets marked used. Every definition of
    // a duplicated name is marked, not only the first: that duplication is 5.8.1's business, and
    // calling the copy "never used" as well would report one mistake twice.
    //
    // Built whether or not 5.8.1 is enabled, and that is not a rule being evaluated when it is
    // off: the marks feed 5.8.4 and the lookup feeds 5.8.3 and 5.8.5, and a binary search over
    // this index is cheaper than the linear scan it replaces in every rule set, empty ones
    // included.
    let base = self.scratch.keys.len();
    self.variable_index = Range32::new(base as u32, base as u32);
    // 5.8.1 reads the index directly and 5.8.3/5.8.5 read it through the value walk. Nothing else
    // looks at it, so with none of them on it is a sort over document-chosen names, charged, for
    // nobody. Not an early return: 5.8.2 and the default-value and directive walks below are a
    // different question and answer it themselves.
    // 5.8.1 reads the index directly; 5.8.3 and 5.8.5 read it through the value walk. Nothing
    // else looks at it.
    let indexed = self.collects_usages || self.on(Rule::VariableUniqueness);
    // The charge below is for the **sort**, and a one-element sort performs no comparison — as the
    // duplicate scan that reads its output starts at `base + 1` and performs none either. The
    // search side is charged separately, at the usage, where a singleton index does invoke its
    // predicate once. al8n/smear#198.
    let sorted = indexed && definitions.len() > 1;
    if indexed {
      for (index, described) in definitions.iter().enumerate() {
        if sorted {
          let variable = described.node().variable();
          self.spend(units(name_bytes(variable.name()).len()), *variable.span())?;
        }
        self.scratch.keys.push(index as u32);
      }
      let end = self.scratch.keys.len();
      self.scratch.keys[base..end].sort_unstable_by(|a, b| {
        let left = name_bytes(definitions[*a as usize].node().variable().name());
        let right = name_bytes(definitions[*b as usize].node().variable().name());
        left.cmp(right).then(a.cmp(b))
      });
      self.variable_index = Range32::new(base as u32, end as u32);
    }
    let end = self.variable_index.end() as usize;

    // 5.8.1 — one type per variable name, per operation, off the adjacent pairs of that index.
    if indexed && self.on(Rule::VariableUniqueness) {
      let mut slot = base + 1;
      while slot < end {
        let earlier = self.scratch.keys[slot - 1].min(self.scratch.keys[slot]);
        let later = self.scratch.keys[slot - 1].max(self.scratch.keys[slot]);
        let first = definitions[earlier as usize].node().variable();
        let repeat = definitions[later as usize].node().variable();
        if name_bytes(first.name()) == name_bytes(repeat.name()) {
          let subject = self.subject_v(repeat.name())?;
          let diagnostic = Diagnostic::new(Rule::VariableUniqueness, *repeat.span())
            .subject(subject)
            .related(*first.span());
          self.emit(diagnostic)?;
        }
        slot += 1;
      }
    }

    // Nothing in the loop below has a reader. 5.8.2, the value rules over a default and the
    // directive rules over the definition's own directives are the whole of it, and an operation's
    // declaration list is caller-sized, so iterating it for an empty rule set is a walk with no
    // conclusion at the end of it.
    if !self.visits_variable_definitions {
      return ControlFlow::Continue(());
    }

    for described in definitions {
      let definition = described.node();
      let variable = definition.variable();
      // One per definition examined, and it belongs **here** rather than at the gate above. The
      // gate decides whether to loop; nothing was charging for going round it, so a rule set that
      // opened this loop and then found every branch inside it inapplicable — a value-only set over
      // declarations with no defaults — walked an arbitrarily long public-AST declaration list for
      // a constant budget.
      self.spend(1, *variable.span())?;
      // `pack_type` hashes the declared type's base name, and exactly two things read what it
      // returns: draft 5.8.2's report, and the expected type a **default value** is walked
      // against. So the second reader is per-definition — a declaration with no default has only
      // the first — and it is `checks_values`, not `walks_values`: a default is a constant tree
      // and no usage rule can act on one. The gate was run-wide and admitted both.
      let resolves_type = self.resolves_variable_types
        || (self.checks_values && definition.default_value().is_some());
      let declared = if resolves_type {
        self.spend_type(definition.ty(), *variable.span())?;
        self.pack_type(definition.ty())
      } else {
        None
      };

      // 5.8.2 — objects, interfaces and unions cannot be variable types, and neither can a name
      // the schema does not have.
      let is_input =
        declared.is_some_and(|packed| self.schema.type_def(packed.base_id()).kind().is_input());
      if !is_input && self.on(Rule::VariablesAreInputTypes) {
        let context = match declared {
          Some(packed) => Context::Expected(packed),
          None => Context::None,
        };
        self.report_name(Rule::VariablesAreInputTypes, variable.name(), context)?;
      }

      if let Some(default) = definition.default_value() {
        self.walk_value(default.value(), declared, ValueLocation::PLAIN, true)?;
      }

      if let Some(directives) = definition.directives() {
        self.check_directives(
          directives.directives(),
          DirectiveLocation::VariableDefinition,
          true,
        )?;
      }
    }
    ControlFlow::Continue(())
  }

  /// Draft 5.8.4, read off the marks the usages left.
  fn check_variables_used(&mut self) -> ControlFlow<()> {
    if !self.on(Rule::AllVariablesUsed) {
      return ControlFlow::Continue(());
    }
    for index in 0..self.variables.len() {
      if get_bit(&self.scratch.used, index as u32) {
        continue;
      }
      let variable = self.variables[index].node().variable();
      let subject = self.subject_v(variable.name())?;
      let diagnostic = Diagnostic::new(Rule::AllVariablesUsed, *variable.span()).subject(subject);
      self.emit(diagnostic)?;
    }
    ControlFlow::Continue(())
  }
}

// ---------------------------------------------------------------------------------------------
// free helpers
// ---------------------------------------------------------------------------------------------

/// Builds a table range.
fn range32(start: u32, end: u32) -> super::schema::Range32 {
  super::schema::Range32::new(start, end)
}

/// Grows a bitset to hold at least `bits` bits, preserving what is already set.
fn grow_bits(words: &mut std::vec::Vec<u64>, bits: usize) {
  let needed = bits.div_ceil(64);
  if words.len() < needed {
    words.resize(needed, 0);
  }
}

/// The root operation slot an operation keyword needs.
fn root_operation(keyword: &OperationType) -> RootOperation {
  match keyword {
    OperationType::Query(_) => RootOperation::Query,
    OperationType::Mutation(_) => RootOperation::Mutation,
    OperationType::Subscription(_) => RootOperation::Subscription,
  }
}

/// The directive location an operation of this kind is.
fn directive_location(root: RootOperation) -> DirectiveLocation {
  match root {
    RootOperation::Query => DirectiveLocation::Query,
    RootOperation::Mutation => DirectiveLocation::Mutation,
    RootOperation::Subscription => DirectiveLocation::Subscription,
  }
}

/// Returns an operation's name node, when it has one.
fn operation_name<S>(document: &ExecutableDocument<S>, index: u32) -> Option<&Name<S>> {
  match operation(document, index)? {
    OperationDefinition::Named(named) => named.name(),
    OperationDefinition::Shorthand(_) => None,
  }
}

/// Returns an operation's name bytes, or the empty slice when it is anonymous.
fn operation_name_bytes<'d, S>(
  document: &'d ExecutableDocument<S>,
  rows: &[OperationRow],
  index: u32,
) -> &'d [u8]
where
  S: AsRef<[u8]>,
{
  rows
    .get(index as usize)
    .and_then(|row| operation_name(document, row.definition))
    .map_or(&[][..], name_bytes)
}

/// Returns a fragment's name bytes.
fn fragment_name<'d, S>(
  document: &'d ExecutableDocument<S>,
  rows: &[FragmentRow],
  ordinal: u32,
) -> &'d [u8]
where
  S: AsRef<[u8]>,
{
  rows
    .get(ordinal as usize)
    .and_then(|row| fragment(document, row.definition))
    .map_or(&[][..], |fragment| name_bytes(fragment.name()))
}

#[cfg(test)]
mod tests {
  use super::{Ledger, units};
  use crate::Budget;

  /// An absent bound is closed under spending.
  ///
  /// The property the four previous versions of this defect did not have. `u32::MAX` was the
  /// documented spelling of "never refuse" and was also just a number, so the first charge turned
  /// it into a very large *finite* budget and the lossless door's prepayment turned it into a
  /// smaller one again. Reachable at about 65,000 operations over a 65,000-selection fragment,
  /// which is a document this suite is not going to build — so the property is asserted where it
  /// lives, over the type, rather than inferred from one document that happens to be big enough.
  #[test]
  fn an_absent_bound_is_closed_under_spending() {
    let off = Ledger::open(&Budget::default().with_validation_work(u32::MAX));
    assert_eq!(off, Ledger::Off);

    // Every charge shape that exists: a single unit, a whole default ceiling, the largest charge
    // `units` can produce, and the prepayment the lossless door takes before a validator exists.
    let mut ledger = off;
    for charge in [
      1,
      Budget::DEFAULT_VALIDATION_WORK,
      units(usize::MAX),
      u32::MAX,
      0,
    ] {
      ledger = ledger
        .take(charge)
        .expect("an absent bound has room for anything");
      assert_eq!(
        ledger,
        Ledger::Off,
        "a charge of {charge} turned an absent bound into a number"
      );
    }

    // And a bound that IS set is not confused with one that is not, at the value next to it.
    let bounded = Ledger::open(&Budget::default().with_validation_work(u32::MAX - 1));
    assert_eq!(bounded, Ledger::Left(u32::MAX - 1));
    assert_eq!(
      bounded.take(u32::MAX),
      None,
      "an exhausted bound must refuse"
    );
    assert_eq!(bounded.take(u32::MAX - 1), Some(Ledger::Left(0)));

    // `Left(0)` is an exhausted bound, which is the opposite of an absent one and must not spend.
    assert_eq!(Ledger::Left(0).take(1), None);
    assert_eq!(Ledger::Left(0).take(0), Some(Ledger::Left(0)));
  }
}
