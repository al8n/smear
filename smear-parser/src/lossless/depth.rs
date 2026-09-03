//! The **parser-frame** nesting bound: one level per recursive production entry, released by an
//! RAII guard on every exit path.
//!
//! # Why this exists beside the lexer's bracket tally
//!
//! The lexer already carries a nesting counter, and #61's first fix put a measured ceiling on it.
//! That counter is a **proxy**: it tallies every `{`, `[`, `(` — and, in GraphQLx, `<` and `>` —
//! pair-blind, one saturating scalar, decrementing on *any* closer regardless of which opener the
//! parser is actually inside. The parse's own recursion is a different quantity, and recovery is
//! where the two come apart:
//!
//! - a selection set's loop meets a closer no opener matched;
//! - [`recover::unexpected`](super::recover::unexpected) reports it, `sync_balanced` finds it is
//!   itself a sync point and skips nothing, so the fallback consumes it into an `Error` node;
//! - the loop continues, and the next `f {` opens **another** selection set.
//!
//! Over `{ ) f { ) f { …` the tally therefore oscillates between 1 and 0 for the whole document
//! while every level leaves a `field` frame and a `selection_set` frame live. Measured on this
//! tree at `6f39cb9`: the tally's maximum is **1** at every depth, the ceiling never fires, and a
//! 3.5 KB document aborts a 2 MiB thread with `fatal runtime error: stack overflow` at 702 levels
//! (GraphQLx: 700, with `)` and with its angle closer `>` alike). That is the failure #61 is
//! about, reached past the fix for it.
//!
//! # What is counted here, and why it is every delimiter rather than the shape that was reported
//!
//! One level per **nesting delimiter the parse actually descends through** — the same population
//! the lexer tallies, counted by the side that recurses. A production reaches this exactly where
//! it commits an opener it will parse the interior of: `selection_set`, `arguments`,
//! `variable_definitions`, `list_value`, `object_value`, `list_type`, every `{ … }` member block,
//! and GraphQLx's generic `< … >`. So `MAX_NESTING_DEPTH` keeps the meaning it was derived with —
//! "simultaneously open brackets" — and the derivation behind the number is untouched. What
//! changed is that a closer no opener matched can no longer discount a level, because a level is
//! now released by leaving the frame rather than by seeing a byte.
//!
//! **The selection-set shape is not the only cycle, and that is measured rather than assumed.**
//! `{ f(a: [ ) [ ) [ …` runs `list_value` into `list_value` and `{ f(a: { ) k: { ) k: …` runs
//! `object_value` into `object_value`, both with the tally pinned at 3, and both abort a 2 MiB
//! thread at 2 000 levels when only `selection_set` holds a level. A fix derived from the
//! reported exemplar would have left them live; `nesting_depth.rs` pins all three.
//!
//! # The cell, and why it is tokora's rather than the lexer state's
//!
//! [`InputRef::descend`](tokora::InputRef::descend)'s, whose [`tokora::input::Descent`] guard releases the level
//! on return, on `?` and on an unwind alike. It is deliberately **not** in
//! [`tokora::Lexer::State`], where the lexer's tally lives, and the difference is
//! not stylistic:
//!
//! - a [`Checkpoint`](tokora::input::Checkpoint) carries `L::State`, so a speculative branch's
//!   rollback would restore a depth whose frames are still live;
//! - a **cached** token carries the state that lexed it, and committing it installs that state —
//!   so a token read during lookahead at one depth would reset the cell when it is committed at
//!   another.
//!
//! Both are correct for a lexer tally, which is a function of the token prefix and nothing else,
//! and both are wrong for a control-stack fact. tokora's cell is outside the checkpoint set for
//! exactly that reason.
//!
//! # The ceiling arrives as a `usize`, not as a type
//!
//! This module may not name `smear-lexer` — the substrate is dialect-generic and
//! `lossless_isolation.rs` enforces it — so the number crosses that line as a plain `usize`, once
//! per parse, through the door's own context step. Each dialect's `lossless/mod.rs` still has a
//! `descend` wrapper so a production writes one call, but the wrapper carries no number any more.
//!
//! # There is one ceiling now, and it is the parse's own limiter
//!
//! `descend` used to take a `ceiling` argument and refuse at `min(ceiling, limitation())`, and
//! that second number was tokora's `RecursionLimiter::PARSE_DEFAULT_DEPTH` — installed by
//! `InputContext::new`, which [`parse_lossless`](tokora::cst::parse_lossless) called with no hook
//! for a caller to raise. So the effective bound was `min(what the caller asked for, whatever
//! upstream currently defaults to)`, a number this tree does not choose and upstream moved three
//! times inside one unreleased window (64, then 16, then 32) with nothing here failing to compile
//! over any of it.
//!
//! [`cst::parse_lossless_with_context`](tokora::cst::parse_lossless_with_context) is the hook that
//! removes it. Each door installs `min(the caller's ceiling, smear's own stack-safety maximum)` as
//! **the** limiter, so what [`InputRef::descend`](tokora::InputRef::descend) checks against IS the
//! ceiling and there is no second number to reconcile. What `descend` still does is **emit**:
//! a trip is returned and never emitted, and the lossless doors discard the `Result`, so a refusal
//! that only rode the `Result` would leave a consumer with a truncated tree and no diagnostic.
//!
//! Two things follow, and only the second was ever true by construction:
//!
//! - **The lexer's tally and this budget read one number and are not one mechanism.** The lexer's
//!   trip latches tokora's poison boundary and ends the document; this one reports and returns.
//!   For a well-formed over-deep document at equal ceilings the lexer runs ahead and trips first,
//!   which is why one refusal used to look like one diagnostic — an accident of two numbers being
//!   equal, not an enforced property. The clamp above breaks the equality from the other side: a
//!   caller who raises past the maximum gets a lexer tally at the number they asked for and a
//!   parse budget at the maximum, so the parse refuses first and the tally never fires.
//! - **Equal numbers would not have been enough anyway.** The tally is pair-blind, so a closer no
//!   opener matched discounts a level the parse still holds. Measured on this tree at the
//!   *shipped* ceiling of 24, with both numbers equal: `{` then 60 repetitions of `) f {` then 61
//!   `}` — 483 bytes — leaves the tally at 1 for the whole document while the parse reaches 24,
//!   and produced **115** diagnostics against 61 for the same input under a ceiling it does not
//!   reach. So the parser side has to end the document itself. `descend` does, and its
//!   `The refusal ends the document` note is where that lives.
//!
//! # Ending the document is a call, not a predicate — smear issue #178
//!
//! A refusal ends the document because the roots stop on it, and for three rounds *how* they knew
//! to stop was a predicate written out at each catch site over something a **caller** implements:
//! a `Cow` discriminator, then a variant, then [`tokora::error::MaybeTerminal`]
//! over that variant. The last one
//! is safe for smear's own containers — the orphan rule leaves smear's impl the only possible one
//! — and was not for the generic layer this module used to publish, where a consumer's error type
//! answers for itself and `()` discards [`tokora::error::RecursionLimitReached`] outright.
//!
//! `root_turn` is the one door that closes it, and it reads tokora's **resource-trip counter**
//! beside the trait: written by the trip arm before any grammar code runs, outside the checkpoint
//! set, one writer, no public route to lower it. It takes the counter's baseline itself, at the
//! granularity of the attempt it judges — **one entry** — because the baseline is a value whose
//! *placement* is the whole verdict and a misplaced one is silent.
//!
//! The trait is still read, and beside rather than under: a **scanner** stop moves no descent
//! counter, and tokora's scanner-side witness is withdrawn for cause (al8n/tokora#311). Each term
//! is alone on a population, and `smear-parser/src/graphql/lossless/tests.rs` reddens separately
//! for each deletion. That path is the dialect's and not this directory's, for the reason the
//! comment at the end of this file records; `lossless/depth/tests.rs` does not exist and creating
//! it reddens gate 6.
//!
//! # The drain does not re-derive the verdict, and cannot be handed a forged one — smear PR #189
//!
//! `drain_unless_stopped` used to take the counter's baseline itself, around the **whole root**,
//! and that placement is wrong for the same reason hoisting one out of a root loop is: the counter
//! is monotone, so one entry that catches a refusal and carries on makes every later failure in
//! that root read as *tripped*. The drain is then skipped, the valid suffix is left opaque, and
//! the diagnostics that would have covered it are never emitted.
//!
//! So the drain no longer asks *that* question of the counter. `root_turn` already decided which
//! of `RootTurn`'s three arms the entry ended on, and that classification is **carried** to the
//! drain — through `RootStop`, the slot a root threads down to its loop, which only `root_turn`
//! writes. The four steps are **one call**: `drain_unless_stopped` mints the slot, lends it to
//! that root for the duration of that call, spends *that* slot against *that* root's `Result`,
//! and drains or does not. The claim and the subject are joined by the frame that owns both.
//!
//! What the drain still reads for itself is a **residual**, not the verdict: a trip that reached
//! it that no turn of this root judged. `drain_unless_stopped`'s `The witness is read again above
//! the root` note carries the cells and the measurements.
//!
//! # The verdict machinery and the descent it judges are crate-private — smear PR #189, round 5
//!
//! `drain_unless_stopped`, `root_turn`, `RootStop`, `RootTurn` and `descend` are `pub(crate)`.
//! They were
//! public, on the reading that this substrate is dialect-generic and that a consumer assembling
//! its own document root is one of the things it is for. Four adversarial rounds found four
//! defects and **every one of them was on that surface**: a verdict re-derived over the whole
//! root when the question's span is one entry; a verdict a caller could mint outright, from a
//! `Default` slot, a `Copy` of someone else's, or a variant written by hand; a nested drain's
//! correct verdict that could not travel in the `Result` it had to return through; and a trip
//! taken **outside** any `root_turn` — a speculative prefix probe, or a caught nested stop —
//! which moves the counter, leaves the slot's latch `false`, and makes the root's next *ordinary*
//! failure read as a stop. That last one is round 1's signature relocated from catches inside
//! turns to catches outside them, measured at `(Err(Ordinary), 0 tail diagnostics)` for n = 1, 4,
//! 16 where the control gives `(Err(Ordinary), n)`.
//!
//! **It cannot be closed here, and not for want of paying.** Telling *which* trip a turn judged
//! needs the slot to carry a moving baseline, and tokora's baseline is
//! `ResourceTripBaseline<'closure>` — a value in the handle region. A root runs inside
//! `node(…).parse_input(inp)`, and tokora's closure `ParseInput` impl is higher-ranked in that
//! region, so a baseline minted inside the loop cannot flow into a slot minted outside it;
//! tokora's own variance table pins that shape as refused. It is not a price in signature count.
//! It does not compile at any price. What would close it is hand-rolled
//! `cst_start`/`cst_finish`/`cst_demote` brackets in place of `node` at each of the six roots, or
//! an acknowledgement API on tokora's side.
//!
//! So shipping the capability means shipping an obligation on the caller — *every attempt that can
//! descend runs inside a `root_turn`* — that nothing enforces and nothing announces when it is
//! broken. That is a to-do wearing a limitation's clothes, and the repair for that shape is a type
//! rather than a tighter sentence. The type is unavailable, so the surface is withdrawn instead.
//!
//! **`descend` goes with them, and the reason is not round 4's.** It is worth being exact here,
//! because the tempting argument is wrong: `descend` is the only route smear published to *move*
//! the counter, so it looks like the writer whose retraction the four rounds demand. It is not.
//! Round 4's false stop happens **inside** `drain_unless_stopped`, and that is now crate-private,
//! so a consumer who moves the counter has no published reader left to mislead. Narrowing the
//! readers alone would have closed the finding.
//!
//! What narrows `descend` is the test round 5 already applied to `drain_unless_terminal` one
//! section down: **a door whose guarantee the crate's public API can no longer deliver is not a
//! door a caller should be offered.** `descend`'s contract is *the refusal ends the document* —
//! its own `The refusal ends the document` note is the longest thing in this file — and every
//! mechanism that ends one is now `pub(crate)`: the six roots, `root_turn`, both drains. A
//! consumer left holding a public `descend` gets a terminal value and has to write its own catch
//! predicate over it, which is the `1 + n` amplification of smear issue #169 arriving by exactly
//! the route this module spends its length saying must be **one call and not a pattern to copy**.
//! Against that, what the function still offers over calling
//! [`InputRef::descend`](tokora::InputRef::descend) directly is the emission — and the emission is
//! only correct in a parse whose roots stop.
//!
//! It is the wider of the two available boundaries and it is recorded as a choice, not a
//! consequence: the finding is closed either way.
//!
//! **The mechanism is unchanged**, and it is correct for the population that remains. The only
//! roots left are the six this crate writes and the driver macro's one-entry root; `recover.rs`
//! and `trivia.rs` contain no `descend`, so every trip a shipped parse can take is inside a
//! judged entry window, and on the shipped error types the witness and the arm agree on every
//! path.
//!
//! What stays public is [`FromNestingLimit`] and [`FromTokenBudget`] — the two dialect
//! conversions a refusal lands through. Neither reaches a drain, neither takes a level and neither
//! builds a context, so a consumer holding both cannot assemble a root, and the three gates below
//! are what say so:
//!
//! ```compile_fail
//! // The readers' door is shut. This reddens by COMPILING if the verdict machinery is ever
//! // widened back to `pub` — which is the only way the retraction above gets undone by accident.
//! use smear_parser::lossless::depth::drain_unless_stopped;
//! ```
//!
//! ```compile_fail
//! // And the writer's. Separate from the one above ON PURPOSE: a single gate naming both items
//! // stays red while *either* is private, so re-widening `descend` alone — the case that matters,
//! // since it is the counter's only published mover — would pass it.
//! use smear_parser::lossless::depth::descend;
//! ```
//!
//! ```compile_fail
//! // And the DRAIN'S OWN STOP VALUE — the last thing in this module that touches the budget.
//! // `lossless_context` used to be the third gate here; round 7 deleted it rather than narrow it
//! // again, and this took its place because it is what is left. Reddens by COMPILING.
//! use smear_parser::lossless::depth::token_budget_stop;
//! ```
//!
//! ```
//! // THE POSITIVE CONTROL for the three gates above, and it is not decoration: a `compile_fail`
//! // block passes for ANY compile error, a misspelled crate or a typo in the path included, so a
//! // fence written that way proves nothing on its own. This imports from the SAME module by the
//! // SAME path shape and compiles, which is what says the three above fail on the item's
//! // visibility rather than on the route to it.
//! //
//! // Deliberately names only the two ungated items: this header is compiled in every feature
//! // cell, and a control that reached for a dialect door would fail the cell that has no dialect.
//! use smear_parser::lossless::depth::{FromNestingLimit, FromTokenBudget};
//! fn _both_are_reachable<T: FromNestingLimit + FromTokenBudget>() {}
//! ```
//!
//! # The residual is tokora's writer, and the retraction is what prices it — smear PR #189
//!
//! A consumer holding an `InputRef` can call [`InputRef::descend`](tokora::InputRef::descend)
//! itself and move the resource-trip counter. That is tokora's published API, not smear's, and
//! narrowing anything here does not reach it. It is recorded rather than closed because with the
//! machinery above crate-private it costs nothing: **smear publishes no reader of that counter.**
//! `root_turn` is the only thing in this crate that turns a counter movement into a verdict, and
//! it is `pub(crate)`; nothing a consumer can reach draws a conclusion from a trip it took by
//! hand. The defect the four rounds found was never "the counter moved" — it was "the counter
//! moved and a *published* reader spent that movement against an unrelated failure." Retracting
//! the reader is what removes the second half, and the second half is the whole cost.
//!
//! What would change that is publishing any door that reads the counter. Whatever the shape, it
//! re-acquires this residual on the day it ships, and the entry above records that the type which
//! would make it safe does not compile.

// WHAT SURVIVES THE DIALECT-LESS CELL, AND NOTHING ELSE. `FromNestingLimit` and `FromTokenBudget`
// are the two items here that are not gated, and between them they name exactly this one.
//
// `lossless_context` used to be a third. Round 4 made it private and moved its three types into
// the gated block; round 7 DELETED it — each dialect's door builds its own `InputContext` from
// tokora's API, so there is no mint in this crate to widen. The lesson the two rounds leave is the
// same either way: a private item with no caller in the dialect-less cell is `dead_code`, and
// imports it alone names are `unused_imports`, both denials under `-Dwarnings` on the one cell a
// plain local build does not run.
use tokora::SimpleSpan;

// SAME CFG AS THE ITEMS THAT NAME THEM. Everything below `FromNestingLimit` in this file — the
// verdict cluster and `descend` — is compiled only where a dialect is, so under
// `--no-default-features --features rowan` each of these imports is an `unused_imports` denial
// under `-Dwarnings`. Splitting the `use` is the repair, and it has a second half that is easy to
// miss AND that has exactly one spelling, because two rustdoc lints close on it from opposite
// sides. Every link to one of these names from an item that is NOT gated — the module header
// above, `FromNestingLimit`, `FromTokenBudget` — is a **bare link whose label is the full
// `tokora::` path**, and neither obvious alternative survives both feature cells:
//
//   - a bare short label resolves *through the import*, so in the cell that turns the import off
//     it is a `broken_intra_doc_links` warning;
//   - a short label with an explicit target is `redundant_explicit_links` in the cell where the
//     import is ON, because there the label alone already resolves to the same item.
//
// The full path as the label has neither problem: it resolves without the import, so nothing is
// broken and nothing is redundant. All five links written the other two ways failed one cell or
// the other before they were written this way.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
use tokora::{
  InputRef, Lexer, ParseContext,
  error::{MaybeTerminal, RecursionLimitReached},
  input::Descent,
  span::Spanned,
};

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
use crate::combinator::ErrorOf;

/// A dialect error container that can name a refused descent.
///
/// One method rather than a `From` impl, because the payload is three plain numbers and a span
/// rather than a type: a `From<…>` would need a shared error struct in the substrate, and the
/// substrate may not put a type into a dialect's error enum.
pub trait FromNestingLimit {
  /// The parse tried to enter level `attempted` under a ceiling of `limit`.
  ///
  /// `span` is empty and sits at the parse's committed end — the position the descent was refused
  /// at. A refused frame has consumed nothing of its own yet, so there is no lexeme to point at.
  ///
  /// **The value this builds should answer
  /// [`tokora::error::MaybeTerminal::is_terminal`] with `true`**, and
  /// it is
  /// no longer the thing that stops a document root from resynchronising past the refusal — the
  /// crate-private `root_turn` reads the input's own trip witness beside the arm, and smear issue
  /// #178 is that change. What the arm is still alone on is a **scanner** stop, which no published witness sees,
  /// so it is a requirement of the dialect containers rather than a hope: the arm censuses in
  /// `smear-parser/src/*/error/tests/terminal.rs` assert it at the value, and flipping it there
  /// reddens them.
  ///
  /// The bound is not on this trait — a `From`-style constructor cannot state a property of what it
  /// returns — so it is carried where the readers are, in the `lossless_production!` bundle's
  /// `Error: MaybeTerminal` clause. tokora's own rule says the same thing from the other side: a
  /// frame budget is never cleared by more input, so `false` here is the arm its table calls spent
  /// silently — and the witness is what stops that spending from costing the document.
  ///
  /// # It stays public and unsealed while its only consumers are crate-private, deliberately
  ///
  /// The rest of this module's cluster was withdrawn on one test — *a door whose guarantee the
  /// crate's public API can no longer deliver is not a door a caller should be offered* — and that
  /// test does not reach this trait: what `descend` promised was **the refusal ends the document**,
  /// which needs readers this crate no longer publishes, whereas what this trait asks for is a
  /// property of the **value** a dialect container mints, and every public door still delivers it.
  /// That nothing publicly reachable consumes an out-of-crate impl today — `descend` and the
  /// production bundle are both `pub(crate)`, and every shipped door pins smear's own containers —
  /// is consistency residue rather than a promise the API cannot keep, and narrowing it would be a
  /// further public-API removal on no finding at all.
  fn nesting_limit_exceeded(span: SimpleSpan, attempted: usize, limit: usize) -> Self;
}

/// A dialect error container that can name a refused **durable token budget** — smear issue #193.
///
/// [`FromNestingLimit`]'s twin on the other resource axis, and one method for the same reason: the
/// payload is two plain numbers and a span, and the substrate may not put a type into a dialect's
/// error enum.
///
/// # The refusal has no channel of its own, which is why this trait exists
///
/// A [`TokenBudget`](tokora::input::TokenBudget) refusal latches tokora's poison boundary and
/// counts a scanner trip, so it travels the pipeline a lexer-side resource trip already travels —
/// and tokora states plainly that *the one thing the refusal cannot do is report itself*. There is
/// no diagnostic channel for it: a report would have to be built as the emitter's own error type,
/// which needs a `From` bound tokora deliberately does not add to every consume path, so the item
/// that would have exhausted the budget is refused **silently**.
///
/// Left there, a refused document is a `Parse` with a truncated tree, a gap-tiled tail and
/// [`has_errors`](crate::lossless::runner::Parse::has_errors) answering `false` — indistinguishable
/// from a document that parsed. Measured on this tree with the report planted away, over 400
/// definitions of `type Tn { f: Int }` — a document every root here takes in silence — under a
/// durable ceiling of 100: **0** diagnostics, and a tree covering the whole source.
///
/// The document matters and picking the wrong one hides the finding. `[ type ] ` x2000 — the shape
/// the rest of this repair is measured on — reports its own grammar errors, so the same plant
/// moves it from 3 diagnostics to 2 and the silence is invisible. The reading above is taken on a
/// document every root accepts, where the refusal is the only thing there is to say.
///
/// So smear mints the diagnostic, at the one frame that can see the refusal without being able to
/// miss it — `drain_unless_stopped`, whose own note carries where the reading is taken and why a
/// baseline is subtracted from it.
///
/// **The value this builds must answer [`tokora::error::MaybeTerminal::is_terminal`] with `true`.**
/// A budget that has refused is never cleared by more input — the tally is monotone and outside
/// every rollback — so `false` here would be the arm tokora's rule calls spent silently. The bound
/// is not on this trait, for [`FromNestingLimit`]'s reason: a constructor cannot state a property
/// of what it returns. It rides the `lossless_production!` bundle's `Error: MaybeTerminal` clause,
/// and the arm censuses in `smear-parser/src/*/error/tests/terminal.rs` assert it at the value.
pub trait FromTokenBudget {
  /// The parse's lexer produced `spent` items under a durable ceiling of `limit`, and the next one
  /// was refused.
  ///
  /// `span` is empty and sits at the parse's committed end. That is the only honest position
  /// available: tokora drops the refused item where it stands and publishes no span for it, so
  /// there is no lexeme to point at — the same reason [`FromNestingLimit`] reports at an empty
  /// span, arrived at from the other direction.
  ///
  /// `spent` is [`TokenBudgetTally::spent`](tokora::input::TokenBudgetTally::spent) at the refusal
  /// and `limit` is the configured ceiling. They are equal at every refusal this crate can reach,
  /// because the gate is `spent >= limit` and the charge saturates one item at a time — both are
  /// passed anyway so a consumer's container is not forced to re-derive one from the other, and so
  /// that a future charging site that skips more than one item does not silently change what the
  /// diagnostic says.
  fn token_budget_exhausted(span: SimpleSpan, spent: usize, limit: usize) -> Self;
}

/// Drains whatever a document production left uncommitted — unless the **error value** says the
/// parse ended.
///
/// This is the trait half alone, and no dialect entry calls it directly: [`drain_unless_stopped`]
/// is the door, and it is handed the witness's verdict beside this.
///
/// # It was the first item of this cluster to be narrowed, and now they all are
///
/// It was `pub`, on the reasoning that it is the primitive composition is built from and that a
/// consumer holding a `Result` produced some other way still has a use for it. What that missed is
/// that the signature **structurally cannot** carry the other term: it takes an already-evaluated
/// `Result`, so there is no pre-attempt baseline for it to have taken and no verdict for it to
/// have been handed. A downstream error whose [`MaybeTerminal`] arm answers `false` for a
/// converted descent refusal therefore got a full tail drain through this door — the exact `1 + n`
/// amplification this branch exists to close — and a call site reaching for it compiled with
/// nothing on any channel saying so.
///
/// A door that cannot ask the question is not a door a caller should be offered. Round 5 reached
/// the same verdict about the doors that *can* ask it, for a different reason — see the module
/// header — so the whole cluster is `pub(crate)` and this is no longer the odd one out.
///
/// **Narrowing an item in this substrate is not the local tidy-up it looks like** — see
/// `lossless/mod.rs`'s `pub` IS LOAD-BEARING note. A `pub` item is exempt from `dead_code`; a
/// `pub(crate)` one is not, and under `--no-default-features --features rowan` this substrate is
/// compiled with **no dialect in the crate**, so nothing calls any of this cluster there. That is
/// why every item of it carries the same `any(graphql, graphqlx)` cfg `mod macros` carries: not to
/// silence a lint, but because a verdict machinery whose only callers are dialect assemblies has
/// nothing to mean in a build that contains none.
///
/// # The drain is still not optional, and on the terminal path it is worse than optional
///
/// A production that returns `Err` has committed a prefix and left the rest, so the drain is what
/// keeps the tail in the tree as committed tokens rather than as an opaque gap run. That is right
/// for a syntax error and wrong for a stop, because a stop has already decided the parse ends:
/// reading the tail then costs a full lex of it and, on a tail that does not lex cleanly, **one
/// lexer diagnostic per malformed lexeme** — tokora emits every lexer error it crosses, and
/// `skip_while` crosses all of them. Measured through `parse_document_with_limits` at a ceiling
/// above the door's own clamp, so the *parse* refuses: 300 nested selection sets with a
/// tail of `n` invalid lexemes returned `1 + n` diagnostics (1, 2, 5, 17, 65 for n = 0, 1, 4, 16,
/// 64), in both dialects. The refusal is one diagnostic only if nothing reads the tail.
///
/// So on a terminal stop the tail is left to
/// [`finish_partial`](tokora::cst::Cst::finish_partial), which tiles it as a gap run — **exactly
/// what the lexer's own trip already does**, since a latched poison boundary makes the tail
/// unreachable to any drain. `lossless::runner::finish_root`'s `The trip ends the document,
/// deliberately` note carries that posture and the price of it: the tree still covers every byte,
/// and the tail's tokens are opaque.
///
/// # Terminality, not the nesting refusal — and the first version asked the narrower question
///
/// This tested [`FromNestingLimit`] for one variant. That is narrower than the situation in two
/// ways that both bite. An **emitter rejection** is a stop by tokora's own contract — `Err` from
/// [`Emitter::emit_error`](tokora::Emitter::emit_error) means *processing stops immediately* — and
/// the rejecting emitter may return any same-typed value it likes rather than the payload it was
/// handed, so a refusal whose emission was rejected arrived here as something the narrow predicate
/// answered `false` for; the drain then ran over the tail and its own rejection displaced that
/// value in turn. Measured: `{ f } ~ ~` under an emitter that rejects `emit_error` with a budget
/// sentinel returned `LexerError` — a third value, neither the refusal nor the sentinel. And a
/// **scanner trip the emitter rejected** is the case tokora documents as reaching a caller with
/// nothing marking it, on a carrier the narrow predicate was never going to recognise.
///
/// [`MaybeTerminal`] is the notion tokora already has for exactly this, so it is what this reads
/// and what [`root_turn`] reads first. A predicate written against one defect is how the narrow one
/// got here — and a predicate written against a *value* is how smear issue #178 got here, which is
/// why [`root_turn`] asks the input as well, and why its verdict is **carried** to
/// [`drain_unless_stopped`] rather than re-derived there.
///
/// # Why the drain's error may still displace a non-terminal outcome
///
/// It always could, and that is unchanged: `skip_while` returns the emitter's fatal rejection, and
/// for a *lexer* error that `Err` **is** the delivery — the input layer advances its dedup
/// watermark before calling, so the diagnostic is offered exactly once and dropping the `Err`
/// would drop it. That is the case [`descend`] is careful to distinguish from its own.
///
/// # The skip is charged to the durable budget, and the frame above polls afterwards
///
/// `skip_while` lexes, so every item it crosses is a produce-event charged to the input's
/// [`TokenBudget`](tokora::input::TokenBudget) — the tally no [`Checkpoint`](tokora::input::Checkpoint)
/// refunds. This drain can therefore be the **first** reader of the input to meet a caller's
/// ceiling, and tokora answers `Ok` on that terminal `Scan::Tripped`, so nothing in the value
/// handed back says a refusal happened. [`drain_unless_stopped`] polls the tally again after
/// calling this, for that reason and no other; its `The durable budget is asked twice` note
/// carries the measurement and what the report may and may not displace.
///
/// # What pins the early return, and the round it spent unpinned
///
/// `a_terminal_failure_no_turn_classified_stops_the_drain_on_the_trait_alone`, in
/// `smear-parser/src/graphql/lossless/tests.rs`. It hands this function a terminal, **untripped**
/// `Err` — directly, and again through [`drain_unless_stopped`]'s `Recoverable` arm — and reads
/// the tail diagnostics: `0`, against `n` for the same unclassified failure on a non-terminal
/// value.
///
/// It was written because for one round the population had no cell at all.
/// `a_refusal_is_the_error_returned_even_under_a_rejecting_emitter` used to call this function
/// directly and was the pin; round 5 rewrote it to go through [`drain_unless_stopped`], whose root
/// descends outside every [`root_turn`], so its residual reading answers first and this function
/// is never reached on those cells. Measured with the terminality check deleted and before the new
/// cell existed: 363/363 `smear-parser --lib`, 14/14 `nesting_depth`, 16/16 `resync_allowance` and
/// 5/5 `lossless_isolation` — the whole claimed guard population green over a `1 + n`
/// amplification that had been reopened.
#[inline]
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) fn drain_unless_terminal<'inp, L, Ctx, Lang, T>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  out: Result<T, ErrorOf<'inp, L, Ctx, Lang>>,
) -> Result<T, ErrorOf<'inp, L, Ctx, Lang>>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseContext<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: MaybeTerminal,
{
  if out.as_ref().err().is_some_and(MaybeTerminal::is_terminal) {
    return out;
  }
  inp.skip_while(|_| true)?;
  out
}

/// What one turn of a document-root loop decided about one entry — and, one scale up, how the root
/// itself ended.
///
/// Three arms rather than a `Result` plus a predicate, because **the predicate is the thing this
/// module keeps having to stop a root from re-deriving**. Every earlier round of smear issue #169
/// left the "does this failure end the document" decision at the catch site, spelled out by hand,
/// and each round only changed *which* material the hand-written spelling read: a `Cow`
/// discriminator, then a dedicated variant, then [`MaybeTerminal`] over that variant. All three
/// are things a caller writes. This enum moves the decision itself into [`root_turn`] and leaves
/// the root with a value to match on, so the six catch sites — and the seventh a later dialect
/// adds — copy a **call** rather than a predicate.
///
/// # One type at two scales, because it is one question
///
/// [`drain_unless_stopped`] builds one of these too, out of what the root's **last** turn decided
/// and the `Result` that root returned. The scales differ — the loop asks about one entry, the
/// drain about the whole root — and the question does not: *did this failure end the document, or
/// is there a tail still worth committing?* A second three-arm type spelled the same way would be
/// two places for one answer to drift, and drift between exactly these two readers is smear
/// PR #189.
///
/// # It used to be forgeable, and then it stopped being reachable — smear PR #189
///
/// Round 2's finding was that this enum was public **data**: a caller who found [`root_turn`]
/// inconvenient could write out the arm they preferred and hand it to the drain, which is
/// [`root_turn`]'s whole job re-implemented by the party the input's trip witness exists to be
/// independent of — in both directions. A fabricated `Recoverable` runs the tail drain over a
/// genuine refusal (`1 + n` diagnostics, the amplification smear issue #178 closes); a fabricated
/// `EndsTheDocument` suppresses the drain and leaves a valid suffix opaque.
///
/// Round 3 answered it with `#[non_exhaustive]` on each variant, which out of crate removes the
/// constructor and leaves the pattern. **That attribute is gone with round 5's narrowing**, and
/// not because the property stopped mattering: `#[non_exhaustive]` has no effect inside the
/// defining crate, so on a `pub(crate)` enum it is inert — an attribute claiming a guarantee it no
/// longer provides, which is the shape this module has spent four rounds removing. What guards the
/// forgery now is that nothing outside this crate can name the type at all.
///
/// **The variants stay braced.** Round 3's reason was the attribute — on a *tuple* variant
/// `#[non_exhaustive]` privates the constructor and an out-of-crate tuple pattern resolves through
/// it — and the shape survives it because six match sites read these arms and a named field costs
/// one word at each.
///
/// `#[must_use]`, and that is the last hand-copied step this could still lose: a root that calls
/// [`root_turn`] and drops the verdict compiles, parses, and resynchronises past every refusal —
/// the whole of smear issue #169 back, at a call site that looks like it consulted something.
/// Under `-Dwarnings` it is a build failure instead.
#[must_use = "the verdict is the whole point of the call: dropping it resynchronises past a \
              refusal, which is smear issue #169"]
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) enum RootTurn<T, E> {
  /// The entry parsed. The loop takes another turn.
  Parsed {
    /// What the entry produced — `()` for every production in this workspace.
    parsed: T,
  },
  /// The entry failed, and the failure **ends the document**: return it, resynchronise past
  /// nothing, and read no more input.
  ///
  /// Two independent things can put a failure here — the error value's own
  /// [`MaybeTerminal::is_terminal`] arm, and the input's resource-trip witness — and
  /// [`root_turn`] documents why neither covers the other.
  EndsTheDocument {
    /// The failure, already reported at the point of failure.
    error: E,
  },
  /// The entry failed with an ordinary syntax error, already reported at the point of failure.
  /// The root drops it and resynchronises.
  Recoverable {
    /// The syntax error, already reported at the point of failure.
    error: E,
  },
}

/// Carries what [`root_turn`] decided out of the loop that asked it and up to the drain above the
/// root — smear PR #189.
///
/// # Why a slot rather than a return value
///
/// [`root_turn`] already returns its verdict, and for the loop's own arm that is all a root needs.
/// The drain sits one frame further out, on the other side of a [`node`](tokora::parser::node)
/// bracket the root's loop runs *inside*: the loop signals a stop by returning `Err` from that
/// closure, because `Err` is what demotes the node, and an `Err` is exactly what cannot say which
/// of the two failures it is. The verdict has to cross that boundary out of band or be
/// reconstructed on the far side — and reconstructing it from tokora's session counter is the
/// defect PR #189 repairs, because that counter's span is the whole root and the question's span
/// is one entry.
///
/// So a root takes one of these as a parameter and threads it into the closure. Nothing else can
/// write it: the field is private and [`root_turn`] is the only writer in the module, so a root
/// loop cannot mark a stop by hand.
///
/// # Nothing outside this module mints one, copies one, or spends one — smear PR #189, round 3
///
/// Making the field private closed the *marking* path and left the *minting* one open, and a fresh
/// slot already carries the dangerous verdict. `Default` and a public constructor meant a caller
/// could produce a slot that says "no stop" and pair it with any `Result` at all — a full tail
/// drain over a genuine refusal, reached without asking [`root_turn`] anything, which is smear
/// issue #178's amplification through the front door. `Copy` meant a classified slot could be
/// duplicated and one copy paired with a *different* root's result, so "spent exactly once, by the
/// frame that owns both" described a guarantee the type did not have. `Clone` alone meant the
/// same: clone the fresh slot, call and match [`root_turn`], restore the clone over what it wrote,
/// and the written verdict is gone without the private field ever being touched.
///
/// So it has no `Default`, no `Clone`, no `Copy`, a private field, a private constructor and a
/// private spend, and the only handle any caller holds is the `&mut` [`drain_unless_stopped`]
/// lends to the root it is about to judge, for that one call. That borrow is higher-ranked — the
/// `Root` bound elides its lifetime, so it is `for<'s> &'s mut RootStop` — which is what stops it
/// being stashed anywhere that outlives the call, and what keeps two nested drains from reaching
/// each other's slot.
///
/// Four `compile_fail` doctests used to pin the three derives and the mint from **outside** the
/// crate, and round 5 deleted them with the surface they were about: on a `pub(crate)` type every
/// one of them fails at the visibility boundary rather than at the property, which is a green gate
/// whose subject is gone. The derives are still absent and still deliberate — in-crate they bound
/// what the six roots and the driver macro can do with a slot — and the boundary itself has one
/// gate, in the module header.
///
/// # The verdict is assigned; the judged trip beside it is latched
///
/// Each [`root_turn`] **overwrites** the verdict, so it always describes the most recent entry
/// rather than any entry. Latching it would put PR #189's own defect back one scale in: a root
/// that catches a refusal and takes another turn would carry the first turn's verdict into the
/// second's ordinary failure, skip the drain, and leave the valid suffix opaque. The roots in this
/// workspace return `Err` immediately on [`RootTurn::EndsTheDocument`], so for them the last turn
/// is the only turn that can be a stop; assignment is what keeps that true for a root that does
/// not.
///
/// The second field is the opposite, and deliberately: *has any entry of this root already judged
/// a tripped attempt*. That is a fact about the past, so it **latches**, and it is not the verdict
/// — it is the term [`drain_unless_stopped`] subtracts from its own reading of the witness so that
/// an already-judged, recovered-from trip is not judged a second time above the root. Assigning it
/// would put round 1's defect back; assigning the verdict alongside is what keeps round 1's repair.
/// Neither field is readable, writable or constructible from outside this module.
#[derive(Debug)]
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) struct RootStop {
  ends_the_document: bool,
  a_classified_entry_saw_a_trip: bool,
}

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
impl RootStop {
  /// A fresh slot, recording no stop and no judged trip.
  ///
  /// Private, and that is the round-3 half of smear PR #189: [`drain_unless_stopped`] is the only
  /// frame that mints one, and it mints it for the root it is about to run. See the type's own
  /// note for what a public one bought a caller.
  #[inline]
  const fn new() -> Self {
    Self {
      ends_the_document: false,
      a_classified_entry_saw_a_trip: false,
    }
  }

  /// Records what one [`root_turn`] decided, and whether that entry's own attempt tripped.
  ///
  /// The verdict is **assigned** — the type's own note says why latching it puts PR #189's defect
  /// back one scale in. `tripped` is **latched**, and the asymmetry is the whole of round 4,
  /// because the two answer different questions. The verdict is *this failure ends the document*,
  /// which is about the most recent entry and stops being true when a later one recovers.
  /// `tripped` is *some entry of this root has already judged a trip*, which is a fact about the
  /// past and cannot stop being true; it is what keeps [`drain_unless_stopped`]'s own reading of
  /// the witness from mistaking an already-judged, recovered-from trip for a live one.
  #[inline]
  fn record(&mut self, ends_the_document: bool, tripped: bool) {
    self.ends_the_document = ends_the_document;
    self.a_classified_entry_saw_a_trip |= tripped;
  }

  /// Whether any [`root_turn`] in this root has already judged a tripped attempt.
  ///
  /// Read by [`drain_unless_stopped`] **before** the slot is spent, and only to scope its own
  /// reading of the witness — see that function's `The witness is read again above the root` note.
  #[inline]
  const fn a_classified_entry_saw_a_trip(&self) -> bool {
    self.a_classified_entry_saw_a_trip
  }

  /// Spends the slot: pairs the root's own `Result` with what its last turn decided.
  ///
  /// Private, and consuming `self`, so the classification and the outcome are joined exactly once
  /// and only by [`drain_unless_stopped`] — which holds both because it minted the one and ran the
  /// root that produced the other. A public spend is a `fn(verdict, any Result) -> verdict about
  /// that Result`, which is the forgery this module's two earlier rounds each relocated.
  ///
  /// No `#[must_use]` here: [`RootTurn`] already carries one, with the message that says what
  /// dropping it costs, and a second bare one is `clippy::double_must_use`.
  #[inline]
  fn ending<T, E>(self, out: Result<T, E>) -> RootTurn<T, E> {
    match out {
      Ok(parsed) => RootTurn::Parsed { parsed },
      Err(error) if self.ends_the_document => RootTurn::EndsTheDocument { error },
      Err(error) => RootTurn::Recoverable { error },
    }
  }
}

/// Runs one document-root entry and says what its outcome means for the document.
///
/// This is the one place the six root loops' catch arm lives. Five of them write
///
/// ```text
/// match depth::root_turn(inp, stop, one_entry::<Src, Ctx>) {
///   RootTurn::Parsed { .. } => {}
///   RootTurn::EndsTheDocument { error } => return Err(error),
///   RootTurn::Recoverable { .. } => recover::resync_to_definition::<Src, Ctx>(inp)?,
/// }
/// ```
///
/// and nothing else. The sixth propagates both failure arms instead of resynchronising on the
/// third — a dialect's own divergence, recorded where that root is defined (smear issue #168).
///
/// The six root loops are not the whole caller set — the driver macro and the in-crate cells call
/// this too — and the set is pinned rather than narrated, for the reason
/// [`drain_unless_stopped`]'s `Who calls it is not written here` note gives.
///
/// # `stop` is written here, and only here
///
/// The verdict this returns is what the loop needs; [`RootStop`] is what the **drain above the
/// root** needs, and the two are the same value written twice because the loop's `Err` cannot
/// carry it across the [`node`](tokora::parser::node) bracket the loop runs inside. Writing it
/// here rather than at the arm is what makes it unforgettable: a root that reaches the arm has
/// already been recorded, whatever it then does with the verdict. See [`RootStop`] for why the
/// write is an assignment rather than a latch.
///
/// # The witness is read *beside* the trait, not instead of it
///
/// The verdict is `e.is_terminal() || inp.tripped_during_attempt(since)`, and both terms are
/// load-bearing over a population the other one misses:
///
/// * [`InputRef::tripped_during_attempt`](tokora::InputRef::tripped_during_attempt) is the only
///   term that survives a **caller-implemented** [`MaybeTerminal`] arm. It reads a counter written
///   by tokora's own trip arm before any grammar code runs, on the input rather than in the error:
///   no `From` conversion can discard it — the grammar's error type may be `()`, whose conversion
///   drops [`RecursionLimitReached`] outright — no [`Checkpoint`](tokora::input::Checkpoint)
///   rollback refunds it, and no public API lowers it. tokora's own note on `resource_trips` is
///   the ruling this implements: *"read the error" is exactly the design this cell exists to
///   replace*.
/// * [`MaybeTerminal::is_terminal`] is the only term that sees a **scanner** stop. The descent
///   counter counts descent refusals and nothing else, and tokora's scanner twin of this pair —
///   `scanner_trip_snapshot` / `scanner_tripped_during_attempt` — is deliberately unpublished: it
///   answers `true` for a document fully recovered through the documented
///   [`set_state`](tokora::InputRef::set_state) path, so it would end a document that was fine
///   (al8n/tokora#311). smear's own lexer tally is such a stop, and the population it is alone on
///   is stated narrowly because it is narrow: with an **accepting** emitter a lexer state trip
///   latches tokora's poison boundary, the root's next peek answers `None`, and the loop exits
///   with no error to classify — so through the shipped doors, which pin `Verbose`, nothing
///   reaches this arm at all. It reaches it for a consumer whose emitter **rejects**, which is
///   the caller tokora's rule tells to write the arm.
///
/// So the disjunction is not belt-and-braces, and each term is pinned separately rather than
/// argued for: `each_term_of_a_roots_stop_is_alone_on_a_population`, in
/// `smear-parser/src/graphql/lossless/tests.rs`, drives this function with a rejected scanner stop
/// and with a real descent trip on an arm that answers `false`, and deleting either term turns
/// exactly one of those cells from `EndsTheDocument` into `Recoverable` while the ordinary-error
/// control stays put under both.
///
/// # The baseline is taken here, per entry, and that is the whole verdict
///
/// tokora's counter is a **monotone session fact**, so the reading is only meaningful against a
/// baseline, and *where* the baseline is taken decides what the reading means. The unit is **one
/// attempt** — tokora states the floor as one speculative parse for a recovery, one retry cycle
/// for `skip_then_retry`, one *element* for its resilient collection loops — and a root
/// loop's attempt is **one entry**: one definition, one type-system definition-or-extension, one
/// import-or-executable-definition. That is the unit this function takes it at.
///
/// Hoisting it above the loop compiles and is arithmetically a session-absolute read for every
/// entry after the first: the counter never comes back down, so one entry that catches a trip and
/// carries on makes every later failure in the document read as "tripped". tokora measures the
/// difference on a document whose first entry catches its own refusal and whose next three fail
/// ordinarily — the per-entry loop files all three, the hoisted loop files none and ends the
/// document on the first (`tokora/tests/root_loop_trip_witness.rs`, section 2) — and its own
/// wording for that failure is that it *ends a document that was fine and discards the valid
/// suffix*, returning `Ok`, so the mistake survives testing and points at nothing.
///
/// **That is why the baseline is not a parameter.** It is a value a caller places, no type can
/// say where it belongs, and the placement is silent when it is wrong. A root that never holds one
/// cannot hoist one, take it after the attempt it is judging, or carry it into a second loop.
///
/// # No production below a root catches, and this does not depend on that
///
/// The granularity floor tokora documents is that a trip *caught inside the attempt* followed by
/// an ordinary failure in the same attempt is re-raised as the trip. No production in either
/// dialect catches — the six catch sites are the roots themselves — so the floor is not reached
/// today. It fails **closed** when it is: an ordinary failure sharing its entry with a swallowed
/// refusal ends the document, which is the direction that costs a suffix rather than the direction
/// that costs the whole amplification back.
#[inline]
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) fn root_turn<'inp, 'closure, L, Ctx, Lang, T, Entry>(
  inp: &mut InputRef<'inp, 'closure, L, Ctx, Lang>,
  stop: &mut RootStop,
  entry: Entry,
) -> RootTurn<T, ErrorOf<'inp, L, Ctx, Lang>>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseContext<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: MaybeTerminal,
  Entry:
    FnOnce(&mut InputRef<'inp, 'closure, L, Ctx, Lang>) -> Result<T, ErrorOf<'inp, L, Ctx, Lang>>,
{
  // PER ENTRY, INSIDE THE CALL, AND NOT A PARAMETER. See this function's `The baseline is taken
  // here` note: hoisting one of these out of a root loop compiles, degrades into a
  // session-absolute read, and ends a document that was fine with no test failing.
  let since = inp.trip_snapshot();
  let out = entry(inp);
  // READ UNCONDITIONALLY, INCLUDING ON `Ok`. The verdict below only needs it on the failing path,
  // and the slot needs it on every path: an entry that tripped, CAUGHT the refusal and returned
  // `Ok` has still judged that trip, and recording that is what stops the drain above this root
  // from judging it a second time — smear PR #189, round 4. It costs a nonce compare and a `u64`
  // compare per entry, against an entry that has already parsed a whole definition.
  let tripped = inp.tripped_during_attempt(since);
  // BOTH TERMS. Neither covers the other — the note above enumerates the population each one is
  // alone on.
  let ends_the_document = out.as_ref().is_err_and(|e| e.is_terminal() || tripped);
  // ASSIGNED HERE, NOT AT THE ARM — and the trip beside it, LATCHED. See `RootStop::record` for
  // why one of the two may not be a latch and the other may not be an assignment.
  stop.record(ends_the_document, tripped);
  match out {
    Ok(parsed) => RootTurn::Parsed { parsed },
    Err(error) if ends_the_document => RootTurn::EndsTheDocument { error },
    Err(error) => RootTurn::Recoverable { error },
  }
}

/// Runs a document root, then drains whatever it left uncommitted — unless the root said its
/// failure stopped the parse.
///
/// `drain_unless_terminal` over an ending that has already been classified, rather than over a
/// bare `Result`. The classification is [`root_turn`]'s. An `*_entry` production writes
///
/// ```text
/// depth::drain_unless_stopped(inp, the_root::<Src, Ctx>)
/// ```
///
/// and nothing else.
///
/// # It runs the root, and that is the whole of smear PR #189's round 3
///
/// It used to take the finished [`RootTurn`], leaving four separately spellable steps: mint a
/// slot, run a root with it, spend the slot against a `Result`, drain. Three of the four were
/// public, so the pairing they exist to make was a pairing a caller could make differently — a
/// `Default` slot spent against a genuine refusal, a `Copy` of one root's classified slot spent
/// against another root's result, or a [`RootTurn`] variant written out with no root behind it at
/// all. Each is a verdict that describes nothing, and the drain has no way to tell.
///
/// **The invariant is *the verdict handed to the drain describes the failure being drained*, and
/// four steps cannot state it.** One call can: this function mints the slot, lends it to `root`
/// for exactly that call, spends *that* slot against *that* root's `Result`, and drains on the
/// answer. Nothing else mints, copies or spends a slot, and there is no argument position for a
/// verdict — the drain takes the root, not a classification of it.
///
/// A `compile_fail` doctest used to pin the missing argument position from outside the crate, and
/// round 5 deleted it with the surface it was about: on a `pub(crate)` function it fails at the
/// visibility boundary rather than at the signature. The shape is still what the signature says.
///
/// # Who calls it is not written here, and that is a repair rather than an omission
///
/// This paragraph used to end by naming the callers, and it named them **wrongly three times
/// running**: five roots, then six roots, then six roots plus the driver macro. Each of those
/// revisions was written by a sweep that reported itself complete, and each of them left out the
/// in-crate cells that drive this function directly — the ones this module's closing comment
/// sends a reader to by name. A caller set is a claim about the whole crate, it changes whenever
/// anyone writes a test, and nothing in a build says a word when it goes stale.
///
/// So it is data now: `DECLARED`, in `ci/source_census/src/roots.rs`, keyed by callee, file and
/// enclosing item and carrying a count, because a total cannot see a swap. It is checked against
/// the **token tree** of every file in this crate — a walk that descends into macro bodies, where
/// fourteen of the twenty-five calls are written, and that refuses a file it cannot parse rather
/// than reporting no callers in it. A caller added without its row is a red gate, and so is a row
/// whose caller went away. Four families: the six `*_entry` productions here, the six document
/// roots' [`root_turn`] calls, the driver macro's own driver, and the cells in
/// `smear-parser/src/graphql/lossless/tests.rs`.
///
/// # The witness is read again above the root, and the slot is what scopes it — round 4
///
/// The seal above is about who may **mint** a verdict. It says nothing about a failure for which
/// no verdict was ever minted, and that population is not the exotic one it looks like:
///
/// * the shipped loops' `peek_kind(inp)?`, `report_unexpected` and `resync_to_definition(inp)?`
///   all return `Err` without going through [`root_turn`];
/// * a root may call [`descend`] outside one;
/// * and a root may **compose**, by returning a nested `drain_unless_stopped` call.
///
/// The third is the one that pays for this section. `Root` returns a plain `Result`, so a root
/// that delegates to another root's drain hands back an `Err` and never touches the slot it was
/// lent. The inner drain classified correctly — recorded [`RootTurn::EndsTheDocument`] in *its*
/// slot, skipped *its* drain — and the `Err` carries none of that. Nothing in the shape is forged,
/// copied, or dropped, so no seal on the *minting* side reaches it, and the outer slot reading
/// `false` turns a stop back into a `Recoverable`: `1 + n` diagnostics again. Measured through two
/// nested drains at a descent budget of `0`, with the consumer's `MaybeTerminal` arm answering
/// `false` for its own refusal, over a tail of `n` lexemes that do not lex: **n** tail diagnostics
/// at n = 0, 1, 4, 16, against **0** for the identical refusal through a single drain
/// (`a_nested_drains_stop_is_not_reclassified_by_the_drain_above_it`).
///
/// **A type cannot close that population, and trying is what this round measured first.** The
/// amplification does not need the inner drain's `Result` at all — the caller owns `E`, so
/// `Err(anything)` out of a root with an unwritten slot reproduces it in one line. Sealing the
/// drain's return value so that a nested one cannot become a `Result` pins a *spelling*: it makes
/// the honest composition unspellable and leaves the amplification exactly where it was.
///
/// So this frame asks the witness too, over its own baseline — and the slot **scopes** the
/// reading. [`RootStop`] latches whether any [`root_turn`] of this root has already judged a
/// tripped attempt, and the drain stops on `Recoverable` only when a trip happened during the root
/// that no such turn judged.
///
/// **The axes are three, not two, and an earlier version of this table had only two.** What the
/// arm reads is *(the root's ending, the counter, the latch)* — and it never reads what the
/// failure **is**. So the row that used to read "a refusal no entry classified" was naming a
/// failure the arm cannot see; the condition it actually tests is *a trip moved the counter and no
/// turn judged one*, which an **ordinary** failure satisfies too whenever the trip was taken and
/// handled outside every turn. That cell was unrepresentable in the two-axis table and it is
/// round 5's finding, so it has a row of its own:
///
/// | the root's ending | trip during the root | judged by a `root_turn` | drain | |
/// |---|---|---|---|---|
/// | `Err`, no turn said stop | no | — | **runs** — the tail is committed and its diagnostics emitted | correct |
/// | `Err`, no turn said stop | yes | yes | **runs** — round 1's cell, and `a_caught_trip_does_not_silence_a_later_failures_drain` is it | correct |
/// | `Err`, last turn said stop | yes | yes | **skipped**, by [`RootTurn::EndsTheDocument`] before the reading is reached | correct |
/// | `Err`, no turn said stop, and the failure IS a refusal — `descend` outside a turn, or a nested drain's | yes | no | **skipped** — round 4's cell | correct |
/// | `Err`, no turn said stop, and the failure is **ordinary**, after a trip taken and handled outside every turn | yes | no | **skipped** | **wrong — a false stop** |
///
/// The whole-root reading on its own is round 1: it cannot subtract, so it ends a document that
/// was fine. No reading at all is round 4: it drains a document that ended. The subtraction is the
/// difference between them, and it is only available because [`root_turn`] wrote down what it
/// judged — but it subtracts a *count*, and the last row is what a count cannot answer.
///
/// [`MaybeTerminal`] still runs on both remaining arms, through [`drain_unless_terminal`], and is
/// still alone on the population the counter cannot see: a **scanner** stop moves no descent
/// counter, and tokora's scanner-side twin is withdrawn for cause (al8n/tokora#311).
/// `a_terminal_failure_no_turn_classified_stops_the_drain_on_the_trait_alone` is the cell that
/// says so, and it exists because for one round nothing did — see
/// [`drain_unless_terminal`]'s own note.
///
/// # The residue, stated
///
/// The two directions are not symmetrical and the residue below names which one each cell is in.
/// A **drained tail** costs diagnostics; a **false stop** costs the valid suffix of a document
/// that was fine, returns without saying so, and is the failure tokora's own note says survives
/// testing and points at nothing. The false-stop direction is **not** confined to a root that
/// returns `Ok` — an earlier version of this section said it was, and the second bullet is the
/// counter-example.
///
/// * **A judged trip, then an unjudged one, in one root.** The latch says *some* trip was judged,
///   not *which*, so a root that catches a classified refusal and later takes an unjudged one
///   drains. The drained-tail direction, chosen deliberately by round 1's repair.
/// * **A trip taken and handled outside every [`root_turn`], then an ordinary failure.** The
///   counter has moved and the latch is `false`, so the arm below reads the ordinary failure as a
///   stop and leaves the valid suffix opaque — the **false-stop** direction, on the `Err` path.
///   Measured at `(Err(Ordinary), 0)` tail diagnostics for n = 1, 4, 16 where the control gives
///   `(Err(Ordinary), n)`. It is why the machinery is `pub(crate)`: closing it needs the slot to
///   record *which* trip was judged, that needs a moving `ResourceTripBaseline<'closure>` in the
///   slot, and a root runs inside `node(…).parse_input(inp)` whose closure `ParseInput` impl is
///   higher-ranked in the handle region — so a baseline minted in the loop cannot flow into a slot
///   minted outside it, at any price in signatures. **No root in this crate reaches this cell**:
///   the six document roots and the driver macro descend only inside a turn, and `recover.rs` and
///   `trivia.rs` contain no [`descend`] at all.
/// * **A root that returns `Ok`.** Then it has said it parsed, and the drain takes it at its word.
///   `Parsed` is deliberately not on the stopping arm — ending a document on a successful root is
///   the false-stop direction too, and this is the cell where refusing to enter it is cheap.
/// * **A stale failure.** The slot is assigned, so a root that keeps parsing after a stop and then
///   returns an *earlier* entry's error pairs this scope's verdict with a stale failure. That is a
///   root discarding a verdict [`root_turn`] handed it — `#[must_use]` fires if it is dropped
///   rather than matched — and not a forged one.
///
/// # Why the drain needs the verdict, and it is not a smaller version of the root loop's point
///
/// A root loop that stops on the witness still returns the failure to its `*_entry` production,
/// and a drain that judges only the error *value* then reads the tail of a document the loop
/// already refused to read. That costs a full lex of the tail and one lexer diagnostic per
/// malformed lexeme — the `1 + n` amplification `drain_unless_terminal` measures — so a
/// consumer whose [`MaybeTerminal`] arm answers `false` gets the amplification back through the
/// drain even though every root arm stopped. Measured, with the roots witnessed and this one not,
/// over 300 nested selection sets past the clamp with a dialect's `NestingLimitExceeded` arm
/// flipped to `false`: **1, 2, 5 and 65** diagnostics for tails of 0, 1, 4 and 64 invalid lexemes,
/// identical in both dialects — `1 + n`, the same curve the trait-only drain was written to
/// prevent, reached through the one term it does not have.
///
/// # Why its own baseline is not the verdict — smear PR #189
///
/// It used to read `inp.tripped_during_attempt(since)` over the whole root **as** the verdict, on
/// the reasoning that the attempt being judged here *is* the whole root. The arithmetic does not
/// follow: tokora's counter is monotone, so that reading answers `true` for a root in which
/// **any** entry ever tripped, including one that was caught and recovered from. `root` is a
/// closure parameter and this module was publicly reachable when the defect was found, so a root
/// that catches an early refusal and later fails ordinarily — a shape a consumer could write, and
/// one no arm of theirs was wrong for — satisfied both conjuncts, skipped the drain, and left a
/// valid suffix opaque with its diagnostics unemitted. That is the *false-stop* direction: it truncates a document that was
/// fine, which is the failure tokora's own note says survives testing and points at nothing.
///
/// The information was never missing — [`root_turn`] had already decided, per entry, at the only
/// granularity where the question means anything. It was being discarded at the arm and rebuilt
/// here out of a counter whose span is the wrong span. So it is carried instead, and the reading
/// that remains here is a **residual** rather than the verdict: not *did this root trip*, which is
/// the question with the wrong span, but *did a trip reach this frame that no turn of this root
/// judged*, which is the question the slot's latch makes answerable at all.
///
/// # The durable token budget is asked here too, and it is the one stop no arm above can see
///
/// A [`TokenBudget`](tokora::input::TokenBudget) refusal reaches the roots as **nothing at all**,
/// and that is the finding rather than an implementation detail. tokora refuses the item silently
/// (see [`FromTokenBudget`]), latches the poison boundary, and takes `scan_with`'s
/// `Scan::Tripped` exit — so a root loop's next `peek_kind` answers `Ok(None)`, the `while` exits,
/// and the root returns **`Ok`**. Nothing fails, nothing is classified, and no
/// [`RootTurn`] arm is ever reached: measured with the report planted away, over 400 definitions
/// of `type Tn { f: Int }` — a document every root here takes in silence — under a durable ceiling
/// of 100, the parse returned a `Parse` with **0** diagnostics whose tree covered the whole
/// source. So the roots as smear PR #189 left them do not resynchronise past the refusal — the
/// boundary makes the tail unreachable — but they do not stop *on* it either. They complete, over
/// a document the parse never read.
///
/// Neither of the two terms [`root_turn`] reads covers it, and each misses for its own reason:
///
/// * the resource-trip counter behind
///   [`tripped_during_attempt`](tokora::InputRef::tripped_during_attempt) counts **descent**
///   refusals. A scanner stop moves the scanner counter, which is a different cell, and tokora's
///   snapshot pair for it is withdrawn for cause (al8n/tokora#311);
/// * [`MaybeTerminal`] is asked of an error **value**, and on this path there is no `Err` to ask.
///
/// tokora's own root-loop example carries a third term for exactly this —
/// `e.is_terminal() || inp.tripped_during_attempt(trips) || inp.at_scanner_stop()` — and
/// [`at_scanner_stop`](tokora::InputRef::at_scanner_stop) is the published reading. It is not what
/// this frame asks, and the difference is deliberate: that predicate is the **disjunction** of the
/// budget's refusal and the positional poison boundary, and the boundary's other writer is the
/// lexer's own limit trip, which already ends the document *with* a diagnostic on the channel.
/// Reading the disjunction here would mint a second diagnostic for every lexer trip. So this frame
/// reads the durable half alone,
/// [`TokenBudgetTally::refused_an_item`](tokora::input::TokenBudgetTally::refused_an_item) — the
/// bit tokora describes as *the one question a host that caught an unwind and concluded can still
/// ask*, written at the refusal, carried by no [`Checkpoint`](tokora::input::Checkpoint), dropped
/// by no state re-key, and lowered by nothing.
///
/// **The reading is not differenced against anything, and it used to be.** Round 1 took a baseline
/// before the root ran and asked *did a refusal happen inside this frame*, on the reasoning that a
/// sibling root under an earlier refusal must not re-report. Both halves of that went: the frame
/// no longer reports at all, so there is nothing here to re-do; and a frame running under a
/// refusal that predates it **should** stop, because the input is poisoned and every reader below
/// it is reading a document that ended. The absolute reading is the right one for a stop for
/// exactly the reason it was the wrong one for a report.
///
/// # The report has an owner, and it is not the value — smear issue #193, rounds 2 to 4
///
/// This frame **stops** and never emits. The dialect's door, generated by
/// [`lossless_door!`](crate::lossless::lossless_door), is
/// what emits. Three rounds arrived at that split and each of the two before it got the same thing
/// wrong, so the wrong versions are kept here: they are a sequence, not three unrelated slips.
///
/// **Round 1 differenced the bit.** `refused_an_item` is *input-absolute and monotone*, so every
/// frame whose baseline predates the refusal sees the same `false -> true`. Two nested drains both
/// minted a diagnostic for one refusal: measured at **2** against a single drain's **1**, at every
/// ceiling. The discipline was copied from [`root_turn`]'s witness without the thing that made it
/// work — `trip_snapshot` returns a **counter**, and a counter's value is unique to the trip that
/// moved it, so a difference over it names an owner. *A monotone boolean can say that something
/// happened inside you; it can never say it happened to you rather than to a frame inside you.*
///
/// **Rounds 2 and 3 asked whether the value in hand is terminal.** `FromTokenBudget` requires the
/// refusal to be terminal, so a frame handed a terminal `Err` concluded that some frame below it
/// had already reported. That reads a fact about the **log** off a fact about the **value**, and
/// Codex round 3 pulled the two apart in both directions:
///
/// * [`try_attempt`](tokora::InputRef::try_attempt) rolls back on `Err`, and `restore_unchecked`
///   restores the cursor, the span, the lexer state, **the emitter**, `emitted_error_end`,
///   `front_reported_end`, **the poison boundary** and the regime —
///   [`Checkpoint`](tokora::input::Checkpoint) has no token-budget field, so the diagnostic is
///   undone while `refused_an_item` stays set. The outer frame reads a terminal value that no
///   longer stands for a report and stays silent;
/// * a composed root that **catches** that terminal `Err` and returns an ordinary one keeps the
///   diagnostic and loses the term, so the outer frame reports a second time.
///
/// Measured at the frame over four compositions of one refusal — direct, inside a rolled-back
/// `try_attempt`, inside a committed one, and caught-and-replaced — the surviving diagnostic counts
/// were **1, 0, 2, 2** before the repair and are **1, 1, 1, 1** after it
/// (`the_report_has_an_owner_and_terminality_is_not_it`).
///
/// # What the two frames each answer
///
/// * *Does this frame keep reading?* A question about the **value in hand**, and this frame's, at
///   any nesting depth. Both polls below are pure stops: they build the refusal through
///   [`token_budget_stop`] and emit nothing. Repeating the stop at every level costs one value per
///   level and no diagnostic at all, which is why nothing here has to know whether it is the
///   outermost;
/// * *has this parse said the refusal out loud?* A question about the **log**, answered once, by
///   the door's own closure body, after every composition the parse contains and off the one cell
///   nothing restores. [`lossless_door!`](crate::lossless::lossless_door)'s note carries why that
///   position is the only one where neither direction can bite, and why the emission is generated
///   into the dialect's module rather than living here.
///
/// **Two things the stop deliberately is not.** It is not `out.is_ok()`: a root that fails
/// *ordinarily* while the budget also refused has to stop too, and that spelling would let it
/// carry on. And it is not a new `is_this_the_budget_refusal` predicate on [`FromTokenBudget`] —
/// tokora's ruling that *"read the error" is exactly the design this cell exists to replace* is
/// about the **stop**, and the stop is still
/// [`refused_an_item`](tokora::input::TokenBudgetTally::refused_an_item), which no caller
/// implements, which no rollback lowers and which has no public mutator.
///
/// # Both polls, and why the second one exists — smear issue #193, round 2 (Codex round 2)
///
/// The first is **before the arm below and ahead of the root's own outcome**, because it outranks
/// both. A root that returns `Ok` there did not parse the document — it ran out of the budget
/// partway and saw an end of input that was not one, which is the exact reading `Parsed`'s "takes
/// it at its word" arm must not take. A root that returns `Err` has a syntax error already
/// reported at the point of failure, and the value is discarded rather than the report —
/// [`descend`]'s `The emitted `Err` is dropped` note is the same trade at the other resource.
///
/// The second is **after the arm**, because the arm is not the end of this frame's reading. Two of
/// the three arms call [`drain_unless_terminal`], whose `skip_while(|_| true)` lexes the whole tail
/// against the **same durable tally** — and tokora answers `Ok` on the terminal `Scan::Tripped`, so
/// without it this frame would hand up a value saying the document is fine. Measured before it
/// existed, at `parse_executable_document_with_limits` over `query Q type T { f: Int }` — the
/// executable root takes `query Q`, wants a selection set, is handed `type` and reports an ordinary
/// syntax error at `8..12`:
///
/// | `max_produce_events` | tree tokens of 16 | diagnostics | refusals named |
/// |---|---|---|---|
/// | 0 – 4 | 1 – 5 | 1 | 1 — the root itself met the ceiling |
/// | **5 – 14** | 6 – 15 | 1 | **0** |
/// | 15 – 20 | 16 | 1 | 0, and none owed |
///
/// **The two arms that read nothing return ahead of it** — `EndsTheDocument`, and `Recoverable`
/// carrying an unjudged trip — because a frame that read nothing more has nothing more to stop on.
///
/// **What the second poll can displace, measured rather than argued.**
/// [`drain_unless_terminal`]'s note says a lexer error's `Err` out of `skip_while` *is* its
/// delivery, so a stop that replaced one would drop a diagnostic rather than a value. It cannot:
/// the two are exclusive within one `skip_while`, because whichever comes first ends the scan.
/// Measured over an emitter that rejects `emit_lexer_error`, a tail whose thirteenth item does not
/// lex, and every ceiling from 0 to 24 — 13 ceilings returned the refusal with the lexer error
/// never offered, 12 returned the rejection with nothing refused, and **0** did both. On the
/// ordinary `Recoverable` arm the stop does displace the root's syntax error, and that is the first
/// poll's trade unchanged.
///
/// **The two polls are independently pinned, and one plant each.** Deleting
/// the door's emission reddens every cell that counts a diagnostic and leaves every
/// terminality assertion green; deleting the post-drain poll here reddens exactly the two cells
/// whose refusal is first taken in a drain — on the **value**, `Ok(())` where `Err(Budget)`
/// belongs, with the diagnostic count still **1** — and leaves all seven door cells green. That
/// asymmetry is the split working: the report does not depend on the stop.
///
/// # The residual, stated — for a budget refusal there is none
///
/// Rounds 2 and 3 each carried one here, and both were consequences of inferring the report from
/// the value: a root ending on a *different* terminal failure while the budget also refused, and a
/// drain handing back a terminal non-budget value in the same situation, each got one diagnostic
/// naming the other resource and none naming the budget. The door does not read the value
/// at all, so both documents now get **one diagnostic per resource** — the descent refusal
/// [`descend`] already emitted, and the budget refusal beside it. Nothing about a budget refusal is
/// left unnamed.
///
/// What a wrong `is_terminal` arm on the budget refusal still costs is the other direction, and
/// that is unchanged: one extra diagnostic per nesting level, on a container that has already
/// broken the contract [`FromTokenBudget`] states.
///
#[inline]
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) fn drain_unless_stopped<'inp, 'closure, L, Ctx, Lang, T, Root>(
  inp: &mut InputRef<'inp, 'closure, L, Ctx, Lang>,
  root: Root,
) -> Result<T, ErrorOf<'inp, L, Ctx, Lang>>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseContext<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: MaybeTerminal + FromTokenBudget,
  Root: FnOnce(
    &mut InputRef<'inp, 'closure, L, Ctx, Lang>,
    &mut RootStop,
  ) -> Result<T, ErrorOf<'inp, L, Ctx, Lang>>,
{
  // THIS FRAME'S OWN BASELINE, over the whole root, and it is not the verdict — it is the
  // residue: the trips that happen inside this root that no `root_turn` of this root judges. See
  // `The witness is read again above the root`.
  let since = inp.trip_snapshot();
  // THE DURABLE BUDGET'S BASELINE. It answers ONE question — did a refusal happen inside this
  // frame at all — and it cannot answer the other one, which is whether this frame is the one
  // that owns the report. See `The durable token budget is asked here too`.
  // MINTED, LENT AND SPENT HERE, AND NOWHERE ELSE. The three lines below are the whole
  // transaction: the slot this frame created, the root this frame ran, and the pairing of the
  // one against the other. None of the three steps is reachable on its own — see this function's
  // `It runs the root` note for what each separately spellable step bought a caller.
  let mut stop = RootStop::new();
  let out = root(inp, &mut stop);
  // BEFORE THE ARM, AND AHEAD OF THE ROOT'S OWN OUTCOME. A refusal is a stop whatever the root
  // returned, and on the shipped path the root returns `Ok` — the loop's peek answered `None`
  // over a poisoned input and nothing failed. `out` is dropped rather than propagated: on `Ok` it
  // is `()`, and on `Err` the failure was already reported at the point of failure, so what the
  // drop costs is a value and not a report.
  //
  // A PURE STOP: it builds the value and emits nothing. Nothing here asks whether some frame
  // below already reported, because that question has no answer a value can carry — see
  // `The report has an owner, and it is not the value`. Repeating the stop at every nesting level
  // costs one value per level and no diagnostic at all.
  if inp.token_budget().refused_an_item() {
    return Err(token_budget_stop(inp));
  }
  // READ BEFORE THE SLOT IS SPENT, because spending consumes it. The conjunction is the scoping:
  // a trip an entry of this root already judged is subtracted, and what is left is a trip that
  // reached this frame unjudged.
  let unjudged_trip = !stop.a_classified_entry_saw_a_trip() && inp.tripped_during_attempt(since);
  // BOUND, NOT RETURNED. The two arms below run a further reader of this frame's input, and the
  // poll after them is what covers it — see `The durable budget is asked twice`. The two arms
  // that read nothing return here, ahead of that poll, because a frame that read nothing more
  // has nothing more to report.
  let out = match stop.ending(out) {
    // The root said it stopped. Nothing reads the tail, which is what makes the refusal one
    // diagnostic — `drain_unless_terminal`'s own note carries the count.
    RootTurn::EndsTheDocument { error } => return Err(error),
    // The trait is still asked, on both remaining arms, by `drain_unless_terminal`: a terminal
    // value reaching a drain by a path no `root_turn` classified — a scanner stop, which moves no
    // descent counter — is the population it is alone on.
    RootTurn::Parsed { parsed } => drain_unless_terminal(inp, Ok(parsed)),
    // A failure carrying a trip this root never judged. It is a stop whatever the error value
    // says, for the reason `root_turn` reads the witness beside the trait at all — smear PR #189,
    // round 4. `Parsed` is deliberately not here: a root that returns `Ok` has said it parsed,
    // and ending a document on that word would be the false-stop direction.
    RootTurn::Recoverable { error } if unjudged_trip => return Err(error),
    RootTurn::Recoverable { error } => drain_unless_terminal(inp, Err(error)),
  };
  // AFTER THE DRAIN, WHICH IS THE ONE FURTHER READER THIS FRAME RUNS. `skip_while` lexes the tail
  // against the same durable tally and tokora answers `Ok` on the terminal `Scan::Tripped`, so a
  // refusal whose first occurrence was in there would otherwise leave this frame handing up a
  // value that says the document is fine. The same pure stop as above, for the same reason.
  if inp.token_budget().refused_an_item() {
    return Err(token_budget_stop(inp));
  }
  out
}

/// The right to hold a dialect's lossless **door**, held by exactly one type per dialect.
///
/// No methods, no members, and nothing reads it. Its only job is to be unimplementable twice:
/// [`lossless_door!`](crate::lossless::lossless_door)'s expansion carries
/// `impl DoorOwner for <that dialect's brand>`, so a second invocation naming the same dialect —
/// anywhere in this crate — is a second impl of one trait for one type and fails with **E0119**
/// before any test runs.
///
/// That is the round-7 half of smear issue #193. The door itself is generated into each dialect's
/// own module, which is what keeps its report site private; what a macro cannot make private is
/// *how many times it is invoked*, and coherence is the only thing in the language that answers
/// that. Without it, an in-crate module could invoke the macro against the real dialect, obtain a
/// private report function over the real `InputRef` type in its own module, and call it from a
/// composed root inside the real parse — Codex round 4's forgery, one level up.
///
/// `smear/tests/lossless_isolation.rs` counts the invocations as the standing half; this is the
/// compile-time half, and the two fail in different ways on purpose.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) trait DoorOwner {}

/// The value that **stops** a frame on a durable token-budget refusal — built, never emitted.
///
/// [`drain_unless_stopped`]'s two polls hand this up, and a frame above may hand up another one
/// built the same way; a stop repeated at every nesting level costs a value per level and no
/// diagnostic at all, which is the whole reason the stop and the report are two functions.
/// The dialect's door owns the emission, and **this module cannot make one** — round 7 moved the
/// only `emit_error` of a budget refusal out of the substrate entirely.
///
/// All three readings — the committed end, `spent` and `limitation` — are taken in one frame, so
/// a stop and the report beside it cannot describe different refusals.
#[inline]
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
fn token_budget_stop<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> ErrorOf<'inp, L, Ctx, Lang>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseContext<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: FromTokenBudget,
{
  // THE COMMITTED END. tokora drops the refused item where it stands and publishes no span for
  // it, so this is the only position that describes anything real: the last byte the parse
  // actually committed. See `FromTokenBudget::token_budget_exhausted`.
  let end = inp.span().end();
  let span = SimpleSpan::new(end, end);
  let spent = inp.token_budget().spent();
  let limit = inp.token_budget().limitation();
  ErrorOf::<'inp, L, Ctx, Lang>::token_budget_exhausted(span, spent, limit)
}

/// Enters one level of parser recursion, or reports and refuses.
///
/// **Bind the guard for the whole frame.** It is `#[must_use]` and dropping it early releases the
/// level before the recursion it was taken for — tokora's `Descent` docs measure four spellings
/// that do exactly that and only one of which warns. The shape every call site here writes is:
///
/// ```text
/// let mut frame = super::descend::<Src, Ctx>(inp)?;
/// let inp = &mut *frame;
/// ```
///
/// # It is `pub(crate)`, and it is the item the retraction is actually about
///
/// **Not because round 4's finding needs it.** That defect — a trip taken where no `root_turn`
/// sees it, then an ordinary failure read as a stop — is produced *inside* `drain_unless_stopped`,
/// which is `pub(crate)` now. A consumer can still move the counter and has nothing published left
/// to mislead with it. The finding is closed by the readers alone, and claiming otherwise would
/// make this narrowing look forced when it is chosen.
///
/// What it is chosen on is this function's **contract**, which the crate can no longer keep in
/// public. `The refusal ends the document` below is the guarantee, and every mechanism that ends
/// one — the six roots, `root_turn`, both drains — is crate-private. A consumer holding a public
/// `descend` is handed a terminal value and left to write the catch predicate themselves; that is
/// smear issue #169's `1 + n` amplification arriving by the one route this module keeps saying
/// must be a call and not a pattern to copy. What the function would still be offering them over
/// [`InputRef::descend`](tokora::InputRef::descend) is the emission — and the emission is only
/// correct in a parse whose roots stop.
///
/// **What remains reachable is tokora's own, and the retraction is what prices it.**
/// A consumer holding an `InputRef` can call
/// [`InputRef::descend`](tokora::InputRef::descend) directly and move the same counter; that is
/// tokora's published API and smear cannot withdraw it. It costs nothing here because smear now
/// publishes no **reader** of that counter — `root_turn`, `RootStop` and `drain_unless_stopped`
/// are all crate-private — so there is no smear-side verdict for a counter moved that way to be
/// wrongly spent against. The pair is what matters: a public writer is only a defect while some
/// published reader draws a conclusion from it.
///
/// # The trip is emitted *and* returned
///
/// Emitted because the lossless door discards the parser's `Result` — `parse_document` keeps the
/// tree and the diagnostics — so a trip that only rode the `Result` would leave a consumer with a
/// truncated tree, no diagnostic and no way to ask. Returned because the frame must not run, and
/// because the returned value is the *only* thing that reaches the loops which have to tell a
/// resource refusal from a syntax error.
///
/// # Why the descent is probed
///
/// The refusal is [`InputRef::descend`](tokora::InputRef::descend)'s, not this function's: the
/// budget the door installed is the only ceiling, tokora's trip arm decides against it, and the
/// trip is counted on the input's own resource-trip cell before any code here runs. What this
/// function adds is the **emission**, because tokora returns a trip and never emits one and the
/// lossless doors discard the `Result`.
///
/// Getting both from one call is what the borrow checker refuses. `inp.descend()`'s `Ok` holds a
/// [`Descent`] borrowing `inp` for this function's own output lifetime, and NLL keeps that borrow
/// live across every arm of a `match` on it — so the refusal arm cannot reach `inp.emit_error`.
/// Mapping the guard away (`inp.descend().map(|_| ())`) leaves a `Result<(), _>` that borrows
/// nothing, which releases `inp` for the emission, and the level is then taken for real on the
/// accepting path.
///
/// So an accepted descent raises and releases the level once before raising it for good. That
/// costs an increment, a comparison of two `usize`s and a decrement — tokora documents its own
/// raise as exactly that and its `Descent` destructor as "a load, a subtract and a store" — and
/// it buys a refusal whose decision, whose arithmetic and whose terminality latch are all the
/// substrate's. A refused descent is probed once and not re-taken, so the trip is counted exactly
/// once per refusal.
///
/// # The refusal ends the document — smear issue #169
///
/// A refusal is **one** diagnostic, not one per remaining token, and what makes it one is that
/// nothing reads the rest of the document afterwards.
///
/// Left to itself the `Err` unwinds the whole nest and lands in a root loop's `if
/// definition(inp).is_err() { resync_to_definition(inp)? }` — which is correct for a syntax error
/// and wrong for a resource refusal. The loop resynchronises and carries on at the *document*
/// level, where every closer of the abandoned nest is now an unexpected token with an `Error` of
/// its own. Measured: 66 nested selection sets under a ceiling of 66 returned **67** diagnostics —
/// the refusal, then one per remaining significant token — and the count grew with the document
/// (804 at 800 levels). Every recursive cycle in both dialects behaved the same way, `list_value`,
/// `object_value`, `list_type`, GraphQLx's `type_generics`, `set_or_map_type` and
/// `collection_body` included.
///
/// **It is the same posture the lexer's trip already has**: end the document.
/// `lossless::runner::finish_root`'s `The trip ends the document, deliberately` note carries the
/// reasoning — a depth trip is a resource refusal, not a syntax error, and a ceiling re-armed
/// after every trip bounds one region and not the parse.
///
/// ## Where the terminal state lives, and why it is the error value
///
/// **In the `Err` this returns**, read through
/// [`MaybeTerminal::is_terminal`](tokora::error::MaybeTerminal::is_terminal) by the crate-private
/// `root_turn` and `drain_unless_terminal` — beside tokora's own resource-trip counter, which is
/// where the same fact lives on the input side and which those two read first. Two other homes were available for
/// smear's own copy and neither works:
///
/// - **A cell.** al8n/tokora#285 is why not: the two homes a budget can have in that crate have
///   opposite durability. `L::State` — where smear's lexer tally lives — is a
///   [`Checkpoint`](tokora::input::Checkpoint) field, so a speculative rollback refunds it and
///   `set_state`/`state_mut` drop it outright; so is `poison_boundary`, which is why the lexer's
///   own latch is a lineage memo rather than a durable fact. The durable cells (`recursion`, and
///   the token budget #285 added) live on the `Input` with no public mutator, so smear cannot
///   write one. A latch in the refundable cell is the category error that issue names.
/// - **Spending the input**, by draining here to end of stream so every loop above re-derives
///   "stop" from its next peek. That is what this function did when the repair first landed, and
///   it is wrong for a reason no cheap reading of tokora shows: **there is no quiet drain.**
///   `scan_with` — the driver behind `next`, `try_expect*`, `skip_while` and the whole `sync`
///   family — emits every lexer error it crosses, and the peek fill is the only other lexing
///   driver and does the same. So a drain over a tail that does not lex cleanly reports one
///   diagnostic per malformed lexeme: `1 + n` for `n` invalid tail lexemes, measured at 1, 2, 5,
///   17 and 65, which is the amplification back in a smaller font and with the allocation still
///   proportional to the tail. Worse, `skip_while`'s `Err` — the emitter's fatal rejection of one
///   of those lexer errors — displaced the saved refusal through the `?` that propagated it: an
///   emitter accepting the refusal and rejecting a later lexer error got the **lexer error** back
///   from a function whose contract names the refusal.
///
/// The error value has neither problem. It is not a cell, so nothing refunds it; it is produced by
/// the frame that refused and consumed by the frames that must stop, which is exactly the distance
/// it has to travel; and it costs no pass over the tail at all, because the tail is never read.
/// [`finish_partial`](tokora::cst::Cst::finish_partial) tiles it, as it already tiles the lexer
/// trip's.
///
/// The one thing a cell would have bought and this does not is a record for a production that
/// **catches** the refusal and carries on. No production in either dialect does — the six catch
/// sites are the document roots, and all six now stop — but a dialect added later could, and its
/// loop would re-descend and refuse again rather than reading a latch. That is the residual, and
/// what bounds it has changed: tokora's resource-trip counter **is** the durable cell this section
/// says smear cannot write, published read-only, and `root_turn` reads it. A catch site that goes
/// through `root_turn` cannot reach the residual whatever its `MaybeTerminal` arm says; a catch
/// site that writes its own predicate still can, which is why the arm is one call and not a
/// pattern to copy — and, since smear PR #189's round 5, why `root_turn` is the crate's own and
/// not a consumer's.
///
/// ## The emitted `Err` is dropped, and this is the one call where that is right
///
/// [`Emitter::emit_error`](tokora::Emitter::emit_error) may reject, and a rejecting emitter is
/// entitled to return **any** value of the error type rather than the payload it was handed — an
/// error-budget sentinel, say. Propagating that with `?` returned a non-refusal out of a function
/// whose contract names the refusal, and then, because the value was neither the refusal nor
/// marked, the entry drain read the tail and *its* rejection displaced the sentinel too:
/// measured at `LexerError` for `{ f } ~ ~`, a third value. That is smear issue #169's own repair
/// opening the next instance of itself one call earlier.
///
/// So the result is consumed and the saved refusal is returned unconditionally. **The asymmetry
/// with [`emit_lexer_error`](tokora::Emitter::emit_lexer_error) is what makes that sound rather
/// than merely convenient**: there, the input layer advances its dedup watermark *before* calling,
/// so the `Err` **is** the diagnostic's only delivery and dropping it drops the report. Here
/// nothing was deduped and the refusal is still in hand — it is what this returns — so what the
/// drop costs is the emitter's *choice of value*, not the report. The stop it was signalling is
/// honoured either way, because the value returned is terminal and every document root stops on
/// it.
///
/// What it does cost is real and worth naming: a host that rejects in order to say *"I am at my
/// diagnostic limit"* cannot distinguish that from *"this document is too deep"* at this one call
/// site. The alternative — propagate, and rely on the caller's
/// [`MaybeTerminal`](tokora::error::MaybeTerminal) arm to keep the loops from resynchronising —
/// makes correctness depend on an
/// arm the caller might not write, and tokora's own rule already records that a wrong `false` arm
/// is spent silently. An unconditional guarantee here and a terminality check at the readers is
/// the pair that needs neither.
#[inline]
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) fn descend<'r, 'inp, 'closure, L, Ctx, Lang>(
  inp: &'r mut InputRef<'inp, 'closure, L, Ctx, Lang>,
) -> Result<Descent<'r, 'inp, 'closure, L, Ctx, Lang>, ErrorOf<'inp, L, Ctx, Lang>>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseContext<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<RecursionLimitReached<usize, Lang>> + FromNestingLimit,
{
  // THE LEVEL IS TAKEN TWICE ON THE ACCEPTING PATH, AND THAT IS FORCED BY THE BORROW CHECKER.
  // See this function's `Why the descent is probed` note. `.map(|_| ())` is what releases the
  // borrow: the probe's `Ok` holds a `Descent` that borrows `inp` for this function's OUTPUT
  // lifetime, and NLL keeps that borrow live across both arms of any `match` on it, so the
  // refusal arm could not reach `inp.emit_error`. Mapping the guard away leaves a `Result<(), _>`
  // that borrows nothing, and the borrow ends at the scrutinee.
  if let Err(refusal) = inp.descend().map(|_| ()) {
    // The depth is already back down — tokora's trip arm decrements before it latches — so these
    // two reads describe the frame that was refused rather than a live one.
    let end = inp.span().end();
    let span = SimpleSpan::new(end, end);
    let attempted = inp.recursion().depth() + 1;
    let limit = inp.recursion().limitation();
    // DROPPED, NOT `?`. A rejecting emitter may return any same-typed value here rather than the
    // payload, and propagating it returned a non-refusal from a function whose contract names the
    // refusal. See this function's `The emitted `Err` is dropped` note for why that is sound at
    // THIS call and would not be at `emit_lexer_error`.
    let _ = inp.emit_error(Spanned::new(
      span,
      ErrorOf::<'inp, L, Ctx, Lang>::nesting_limit_exceeded(span, attempted, limit),
    ));
    // NOTHING BETWEEN THE EMIT AND THE RETURN, AND THAT IS THE REPAIR FOR #169. See this
    // function's `The refusal ends the document` note: the tail is not read, so it cannot add a
    // diagnostic and cannot displace this one. The loops above stop on the value itself.
    return Err(refusal);
  }

  // Cannot trip: the probe above raised and released the same level against the same cell, and
  // nothing between the two calls can change the depth.
  inp.descend()
}

// THE CELLS THAT DRIVE THIS MODULE DIRECTLY LIVE IN `smear-parser/src/graphql/lossless/tests.rs`,
// AND NOT HERE — smear PR #189, round 5. Four of them had to come in-crate: they call `root_turn`,
// `RootStop` and `drain_unless_stopped`, which `smear/tests/nesting_depth.rs` cannot reach any more
// because an integration test is a separate crate and sees `pub` and nothing else. A fifth was
// written in crate in round 6, for the population `drain_unless_terminal`'s own note describes.
// In-crate is not the same as in *this* module, though, and putting them here was wrong on the rule
// this directory exists to keep: every one of them pins one dialect's lexer, its `Lang` marker and
// the lexer crate's limits, so as a `mod tests` under `lossless/` they put a dialect import each
// and thirty-odd dialect-typed signatures inside the dialect-generic substrate. Gate 6,
// `lossless_isolation.rs`, reddens on exactly that and did. A dialect assembly driving the
// substrate is what the Lego rule is *for*; the substrate hosting a dialect-naming test is what it
// forbids.
//
// SPELLING THE PATH HERE IS ALLOWED, and an earlier version of this comment said it was not. Gate
// 6 classifies every line of this directory that names a dialect: a comment is prose and passes,
// and only a line that is neither a comment nor the one feature gate is an offender. What it
// forbids over every line, prose included, is the *spellings* its `SUBSTRATE_FORBIDDEN` table
// lists — a `crate::`-rooted dialect module path, either dialect's `Lang` marker, the lexer
// crate's own path prefix — and a source-tree path is none of those. Do not write one of them out
// here, not even as an example: this paragraph reddened the gate on its first draft, which is the
// gate working. Declining to name the file at all is what left three pointers aimed at
// `lossless/depth/tests.rs`, which does not exist and which reddens gate 6 if anyone recreates it.
