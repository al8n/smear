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
//! [`InputRef::descend`](tokora::InputRef::descend)'s, whose [`Descent`] guard releases the level
//! on return, on `?` and on an unwind alike. It is deliberately **not** in
//! [`Lexer::State`], where the lexer's tally lives, and the difference is
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
//! per parse, through [`lossless_context`]. Each dialect's `lossless/mod.rs` still has a
//! `descend` wrapper so a production writes one call, but the wrapper carries no number any more.
//!
//! # There is one ceiling now, and it is the parse's own limiter
//!
//! [`descend`] used to take a `ceiling` argument and refuse at `min(ceiling, limitation())`, and
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
//! ceiling and there is no second number to reconcile. What [`descend`] still does is **emit**:
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
//!   reach. So the parser side has to end the document itself. [`descend`] does, and its
//!   `The refusal ends the document` note is where that lives.
//!
//! # Ending the document is a call, not a predicate — smear issue #178
//!
//! A refusal ends the document because the roots stop on it, and for three rounds *how* they knew
//! to stop was a predicate written out at each catch site over something a **caller** implements:
//! a `Cow` discriminator, then a variant, then [`MaybeTerminal`] over that variant. The last one
//! is safe for smear's own containers — the orphan rule leaves smear's impl the only possible one
//! — and not for the public generic layer, where a consumer's error type answers for itself and
//! `()` discards [`RecursionLimitReached`] outright.
//!
//! [`root_turn`] is the one door that closes it, and it reads tokora's **resource-trip counter**
//! beside the trait: written by the trip arm before any grammar code runs, outside the checkpoint
//! set, one writer, no public route to lower it. It takes the counter's baseline itself, at the
//! granularity of the attempt it judges — **one entry** — because the baseline is a value whose
//! *placement* is the whole verdict and a misplaced one is silent.
//!
//! The trait is still read, and beside rather than under: a **scanner** stop moves no descent
//! counter, and tokora's scanner-side witness is withdrawn for cause (al8n/tokora#311). Each term
//! is alone on a population, and `nesting_depth.rs` reddens separately for each deletion.
//!
//! # The drain does not re-derive the verdict, it is handed one — smear PR #189
//!
//! [`drain_unless_stopped`] used to take the counter's baseline itself, around the **whole root**,
//! and that placement is wrong for the same reason hoisting one out of a root loop is: the counter
//! is monotone, so one entry that catches a refusal and carries on makes every later failure in
//! that root read as *tripped*. The drain is then skipped, the valid suffix is left opaque, and
//! the diagnostics that would have covered it are never emitted. `root` was a caller-supplied
//! closure and this module is publicly reachable, so that is not a hypothetical shape — it is one
//! a consumer composing the generic layer can write, and no arm of theirs is wrong when it does.
//!
//! So the drain no longer asks. [`root_turn`] already decided which of [`RootTurn::Parsed`],
//! [`RootTurn::EndsTheDocument`] and [`RootTurn::Recoverable`] the entry ended on, and that
//! classification is now **carried** to the drain — through [`RootStop`], the slot a root threads
//! down to its loop, which only [`root_turn`] writes and which [`RootStop::ending`] spends to
//! build the [`RootTurn`] [`drain_unless_stopped`] takes by value. The drain reads no counter, and
//! there is no way to reach it without having said which of the three endings the root had.

use tokora::{
  InputRef, Lexer, ParseContext, SimpleSpan,
  error::{MaybeTerminal, RecursionLimitReached},
  input::{Descent, InputContext},
  span::Spanned,
  state::recursion_tracker::RecursionLimiter,
};

use crate::combinator::ErrorOf;

/// The [`InputContext`] a lossless door drives its parse under: the caller's emitter and cache,
/// and `ceiling` as **the** recursion budget.
///
/// # Why this is a function and not one line at each door
///
/// [`InputContext::new`] seeds
/// [`RecursionLimiter::PARSE_DEFAULT_DEPTH`](tokora::state::recursion_tracker::RecursionLimiter::PARSE_DEFAULT_DEPTH),
/// and that seed is a number smear does not choose and upstream has already moved it twice — 64
/// at the version this workspace shipped against, then 16, then 32, neither move announced by a
/// compile error. A door that builds its own context and forgets `with_recursion_limiter` is
/// therefore not a door with no budget; it is a door running under **whatever tokora currently
/// defaults to**, with the caller's ceiling silently discarded and nothing on any channel saying
/// so. tokora's own `sink_context` carries the same warning about the same constructor.
///
/// So there is one place that turns a ceiling into a context, and every door goes through it. The
/// argument is a plain `usize` because this module is the dialect-generic substrate and may not
/// name `smear-lexer`; clamping the caller's request against the stack-safety maximum happens on
/// the other side of that line, in `LosslessLimits::parse_ceiling`.
///
/// # This is the whole ceiling, and it was not before
///
/// [`descend`] used to take a `ceiling` argument and refuse at `min(ceiling, limitation())`,
/// because a lossless parse could not install a limiter at all and tokora's default could sit
/// below the caller's request. `cst::parse_lossless_with_context` is the hook that removes the
/// second number: the budget installed here IS what
/// [`InputRef::descend`](tokora::InputRef::descend) checks against, so a refusal is the
/// substrate's own trip rather than smear arithmetic that happens to agree with it.
#[inline]
pub fn lossless_context<E, C>(inner: E, cache: C, ceiling: usize) -> InputContext<E, C> {
  InputContext::new(inner, cache).with_recursion_limiter(RecursionLimiter::with_limitation(ceiling))
}

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
  /// **The value this builds should answer [`MaybeTerminal::is_terminal`] with `true`**, and it is
  /// no longer the thing that stops a document root from resynchronising past the refusal —
  /// [`root_turn`] reads the input's own trip witness beside the arm, and smear issue #178 is that
  /// change. What the arm is still alone on is a **scanner** stop, which no published witness sees,
  /// so it is a requirement of the dialect containers rather than a hope: the arm censuses in
  /// `smear-parser/src/*/error/tests/terminal.rs` assert it at the value, and flipping it there
  /// reddens them.
  ///
  /// The bound is not on this trait — a `From`-style constructor cannot state a property of what it
  /// returns — so it is carried where the readers are, in the `lossless_production!` bundle's
  /// `Error: MaybeTerminal` clause. tokora's own rule says the same thing from the other side: a
  /// frame budget is never cleared by more input, so `false` here is the arm its table calls spent
  /// silently — and the witness is what stops that spending from costing the document.
  fn nesting_limit_exceeded(span: SimpleSpan, attempted: usize, limit: usize) -> Self;
}

/// Drains whatever a document production left uncommitted — unless the **error value** says the
/// parse ended.
///
/// This is the trait half alone, and no dialect entry calls it directly: [`drain_unless_stopped`]
/// is the door, and it is handed the witness's verdict beside this.
///
/// # It is `pub(crate)`, and that is the second half of smear PR #189
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
/// A door that cannot ask the question is not a door a caller should be offered. The trait-only
/// primitive stays, because [`drain_unless_stopped`]'s three arms are written in terms of it and
/// splitting the two is what keeps the terminality check in one place; what goes is the route to
/// it from outside the crate.
///
/// **Narrowing an item in this substrate is not the local tidy-up it looks like** — see
/// `lossless/mod.rs`'s `pub` IS LOAD-BEARING note, which used to name this function among the
/// items whose `pub` was the only thing keeping `dead_code` off them under
/// `--no-default-features --features rowan`. It is no longer among them: [`drain_unless_stopped`]
/// is `pub`, is compiled in that cell, and calls this, so the lint has a live caller to see.
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
#[inline]
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
/// [`drain_unless_stopped`] takes this too, built by [`RootStop::ending`] out of what the root's
/// **last** turn decided and the `Result` the root returned. The scales differ — the loop asks
/// about one entry, the drain about the whole root — and the question does not: *did this failure
/// end the document, or is there a tail still worth committing?* A second three-arm type spelled
/// the same way would be two places for one answer to drift, and drift between exactly these two
/// readers is smear PR #189.
///
/// `#[must_use]`, and that is the last hand-copied step this could still lose: a root that calls
/// [`root_turn`] and drops the verdict compiles, parses, and resynchronises past every refusal —
/// the whole of smear issue #169 back, at a call site that looks like it consulted something.
/// Under `-Dwarnings` it is a build failure instead.
#[must_use = "the verdict is the whole point of the call: dropping it resynchronises past a \
              refusal, which is smear issue #169"]
pub enum RootTurn<T, E> {
  /// The entry parsed. The loop takes another turn.
  Parsed(T),
  /// The entry failed, and the failure **ends the document**: return it, resynchronise past
  /// nothing, and read no more input.
  ///
  /// Two independent things can put a failure here — the error value's own
  /// [`MaybeTerminal::is_terminal`] arm, and the input's resource-trip witness — and
  /// [`root_turn`] documents why neither covers the other.
  EndsTheDocument(E),
  /// The entry failed with an ordinary syntax error, already reported at the point of failure.
  /// The root drops it and resynchronises.
  Recoverable(E),
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
/// So a root takes one of these as a parameter, threads it into the closure, and hands it back to
/// its `*_entry` production, which spends it with [`ending`](Self::ending). Nothing else can write
/// it: the field is private and [`root_turn`] is the only writer in the module, so a root loop
/// cannot mark a stop by hand, and a drain cannot be reached without one of the three arms
/// [`ending`](Self::ending) produces.
///
/// # It is assigned, not latched
///
/// Each [`root_turn`] **overwrites** it, so it always describes the most recent entry rather than
/// any entry. Latching would put PR #189's own defect back one scale in: a root that catches a
/// refusal and takes another turn would carry the first turn's verdict into the second's ordinary
/// failure, skip the drain, and leave the valid suffix opaque. The roots in this workspace return
/// `Err` immediately on [`RootTurn::EndsTheDocument`], so for them the last turn is the only turn
/// that can be a stop; assignment is what keeps that true for a root that does not.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct RootStop {
  ends_the_document: bool,
}

impl RootStop {
  /// A fresh slot, recording no stop.
  ///
  /// A root's `*_entry` production mints one, hands `&mut` it to the root, and spends it with
  /// [`ending`](Self::ending).
  #[inline]
  #[must_use]
  pub const fn new() -> Self {
    Self {
      ends_the_document: false,
    }
  }

  /// Records what one [`root_turn`] decided. Assignment, not a latch — see the type's own note.
  #[inline]
  fn record(&mut self, ends_the_document: bool) {
    self.ends_the_document = ends_the_document;
  }

  /// Spends the slot: pairs the root's own `Result` with what its last turn decided.
  ///
  /// This is the only constructor of the value [`drain_unless_stopped`] takes, and it consumes
  /// `self`, so the classification and the outcome are joined exactly once, by the frame that
  /// owns both.
  ///
  /// No `#[must_use]` here: [`RootTurn`] already carries one, with the message that says what
  /// dropping it costs, and a second bare one is `clippy::double_must_use`.
  #[inline]
  pub fn ending<T, E>(self, out: Result<T, E>) -> RootTurn<T, E> {
    match out {
      Ok(parsed) => RootTurn::Parsed(parsed),
      Err(e) if self.ends_the_document => RootTurn::EndsTheDocument(e),
      Err(e) => RootTurn::Recoverable(e),
    }
  }
}

/// Runs one document-root entry and says what its outcome means for the document.
///
/// This is the one place the six root loops' catch arm lives. Each of them writes
///
/// ```text
/// match depth::root_turn(inp, stop, one_entry::<Src, Ctx>) {
///   RootTurn::Parsed(()) => {}
///   RootTurn::EndsTheDocument(e) => return Err(e),
///   RootTurn::Recoverable(_) => recover::resync_to_definition::<Src, Ctx>(inp)?,
/// }
/// ```
///
/// and nothing else.
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
/// argued for: `each_term_of_a_roots_stop_is_alone_on_a_population` in `nesting_depth.rs` drives
/// this function with a rejected scanner stop and with a real descent trip on an arm that answers
/// `false`, and deleting either term turns exactly one of those cells from `EndsTheDocument` into
/// `Recoverable` while the ordinary-error control stays put under both.
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
pub fn root_turn<'inp, 'closure, L, Ctx, Lang, T, Entry>(
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
  // BOTH TERMS. Neither covers the other — the note above enumerates the population each one is
  // alone on.
  let ends_the_document = out
    .as_ref()
    .is_err_and(|e| e.is_terminal() || inp.tripped_during_attempt(since));
  // ASSIGNED HERE, NOT AT THE ARM. The drain above the root reads this and never the counter,
  // because the counter's span is the whole root and this question's span is one entry — smear
  // PR #189.
  stop.record(ends_the_document);
  match out {
    Ok(parsed) => RootTurn::Parsed(parsed),
    Err(e) if ends_the_document => RootTurn::EndsTheDocument(e),
    Err(e) => RootTurn::Recoverable(e),
  }
}

/// Drains whatever a root left uncommitted — unless the root said its failure stopped the parse.
///
/// `drain_unless_terminal` over an ending that has already been classified, rather than over a
/// bare `Result`. The classification is [`root_turn`]'s, spent into a [`RootTurn`] by
/// [`RootStop::ending`]; an `*_entry` production writes
///
/// ```text
/// let mut stop = depth::RootStop::new();
/// let out = the_root::<Src, Ctx>(inp, &mut stop);
/// depth::drain_unless_stopped(inp, stop.ending(out))
/// ```
///
/// and nothing else.
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
/// # Why it does not take its own baseline any more — smear PR #189
///
/// It used to run the root itself and read `inp.tripped_during_attempt(since)` over the whole of
/// it, on the reasoning that the attempt being judged here *is* the whole root. The arithmetic
/// does not follow: tokora's counter is monotone, so that reading answers `true` for a root in
/// which **any** entry ever tripped, including one that was caught and recovered from. `root` was
/// a caller-supplied closure and this module is publicly reachable, so a root that catches an
/// early refusal and later fails ordinarily — a shape a consumer can write, and one no arm of
/// theirs is wrong for — satisfied both conjuncts, skipped the drain, and left a valid suffix
/// opaque with its diagnostics unemitted. That is the *false-stop* direction: it truncates a
/// document that was fine, which is the failure tokora's own note says survives testing and points
/// at nothing.
///
/// The information was never missing — [`root_turn`] had already decided, per entry, at the only
/// granularity where the question means anything. It was being discarded at the arm and rebuilt
/// here out of a counter whose span is the wrong span. So it is carried instead, and this function
/// reads no counter at all: the fix is that the verdict arrives, not that the re-derivation got a
/// tighter comment.
#[inline]
pub fn drain_unless_stopped<'inp, L, Ctx, Lang, T>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  ending: RootTurn<T, ErrorOf<'inp, L, Ctx, Lang>>,
) -> Result<T, ErrorOf<'inp, L, Ctx, Lang>>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseContext<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: MaybeTerminal,
{
  match ending {
    // The root said it stopped. Nothing reads the tail, which is what makes the refusal one
    // diagnostic — `drain_unless_terminal`'s own note carries the count.
    RootTurn::EndsTheDocument(e) => Err(e),
    // The trait is still asked, on both remaining arms, by `drain_unless_terminal`: a terminal
    // value reaching a drain by a path no `root_turn` classified — a scanner stop, which moves no
    // descent counter — is the population it is alone on.
    RootTurn::Parsed(parsed) => drain_unless_terminal(inp, Ok(parsed)),
    RootTurn::Recoverable(e) => drain_unless_terminal(inp, Err(e)),
  }
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
/// **In the `Err` this returns**, read through [`MaybeTerminal::is_terminal`] by [`root_turn`] and
/// `drain_unless_terminal` — beside tokora's own resource-trip counter, which is where the same
/// fact lives on the input side and which those two read first. Two other homes were available for
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
/// says smear cannot write, published read-only, and [`root_turn`] reads it. A catch site that
/// goes through [`root_turn`] cannot reach the residual whatever its `MaybeTerminal` arm says; a
/// catch site that writes its own predicate still can, which is why the arm is one call and not a
/// pattern to copy.
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
/// [`MaybeTerminal`] arm to keep the loops from resynchronising — makes correctness depend on an
/// arm the caller might not write, and tokora's own rule already records that a wrong `false` arm
/// is spent silently. An unconditional guarantee here and a terminality check at the readers is
/// the pair that needs neither.
#[inline]
pub fn descend<'r, 'inp, 'closure, L, Ctx, Lang>(
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
