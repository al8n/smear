//! The recovery primitives, generic over the dialect: report an error, make progress, let the
//! sink attribute what was skipped.
//!
//! # What is here and what stayed behind, and why the line is where it is
//!
//! Task 0's classifier put a dialect's `recover.rs` at 266 shared / 8 hooked / 247 forked, and
//! named the forked mass as **tables, not logic** — the head-set constants, the sync predicate,
//! the definition-start predicate, the delimiter classifier. Task 6b measured each remaining
//! function against a stated threshold (a function lifts if it needs **≤ 2** `fn`-pointer
//! parameters) and all eight cleared it, so the *logic* is here and the *tables* are in each
//! dialect's own `recover.rs`, which passes them in.
//!
//! The `'static` blocker Task 0b found does not reach this module, and the distinction is exact:
//! `UnexpectedToken::expected_one_of` demands a `&'static [Kind]` and such a slice cannot be
//! **built** in generic code. [`report_unexpected`] does not build one — it **receives** one, and
//! receiving a `&'static [<L::Token as Token<'inp>>::Kind]` is ordinary.
//!
//! # Built on tokora's sync family, not on hand-rolled skip loops
//!
//! The members behave differently and the differences decide which one each helper uses:
//!
//! - `sync_balanced(classifier, pred)` — **two** args: `classifier` names which kinds open and
//!   close pairs (`Balance`), `pred` is the depth-0 sync predicate. On a **successful** sync that
//!   skipped ≥1 token it reports the region once through `emit_skipped_region`, and the sink wraps
//!   those tokens in the profile's `error_kind` **automatically** — no explicit `node` call.
//! - `sync_balanced` **makes no progress in two cases**, and both are reachable here. A no-match
//!   run to end of input "commits nothing and returns `Ok(None)`, leaving no trace"; and a
//!   *successful* sync whose predicate matches the very first token returns
//!   `Some(Hole { skipped: 0 })` — the sync point was already at hand — consuming nothing and
//!   emitting nothing. See [`unexpected`] for why that pair is a termination hazard rather than a
//!   curiosity.
//!
//! # Every helper called from inside a loop must consume at least one token
//!
//! A helper that returns `Ok` without consuming turns its caller's `while` into an infinite loop.
//! [`unexpected`] is in that class and guarantees progress explicitly; [`unclosed`] and
//! [`report_unexpected`] are not, because their call sites `return` out of the loop rather than
//! falling through it. The distinction is not "does it consume" but "does the call site
//! continue".
//!
//! # `sync_balanced` needs no `L::State: Clone` clause here
//!
//! It is stated on tokora's own impl block (`input/input_ref/sync_balanced.rs:153-157`), which
//! makes writing it again look obligatory. It is not: `Lexer::State: State` and
//! `trait State: Debug + Clone`, so `L: Lexer<'inp>` implies it. Task 6's mutation proof measured
//! that — the clause deletes green — so it is absent rather than carried.
//!
//! **Do not write `node(error_kind, |_| Ok(()))`.** It is a no-op: an empty, zero-width `Error`
//! node that consumes nothing, which is the rule above violated in the one place it looks like
//! recovery.
//!
//! # The scan allowance — smear issue #168
//!
//! `sync_balanced`'s no-match exit **rewinds the whole scan**: the position, the lexer state, the
//! dedup watermark and every emission come back off, and the drained cache prefix comes back too,
//! "at the cost of re-lexing those tokens on the next read". Both helpers here then advance by
//! exactly one token and the enclosing loop asks again. A tail containing no restart point is
//! therefore walked once per token in it — **Θ(n²)**, measured at ×4.0 per doubling on twelve
//! shapes across both dialects and all six document roots, 24.6 s for 72 KB.
//!
//! A *matching* scan has none of that shape: it commits, so what it crossed is never re-crossed
//! and the cost is amortised. The defect is exactly the no-match branch repeating, and it repeats
//! in **both** helpers — [`unexpected`] over a tail of punctuation the wide `is_sync_point` set
//! does not name, [`resync_to`] over a tail whose definition heads all sit at depth ≥ 1, where the
//! scan never consults its predicate. `[ type ] ` repeated is the second one alone; `( type ) ` is
//! both at once.
//!
//! ## What is bounded, and why it is not the scan
//!
//! Not the scan, because no constant bounds one: a legitimate resync crosses **20 005 tokens in a
//! single committed hole** on a 28 KB document, and that number grows with the input. Capping a
//! scan shreds that recovery into one `Error` node per token, which is smear issue #169's
//! diagnostic amplification returning. (Capping in *bytes* fails for pql#39's reason as well — one
//! GraphQL block string is a single token spanning the whole document.)
//!
//! What is bounded is **total wasted lexing**, because only the failing scans accumulate. The
//! guard is a rate limiter on the parse's own amplification:
//!
//! ```text
//! spent > SCAN_ALLOWANCE_FACTOR * committed + SCAN_ALLOWANCE_FLOOR   ⇒   do not scan
//! ```
//!
//! and a refused scan takes the `None` arm each helper already has — one token into an `Error`
//! node — so nothing new reaches the tree, the diagnostics or the caller.
//!
//! ## Where the two numbers come from
//!
//! `spent` is [`TokenBudgetTally::spent`](tokora::input::TokenBudgetTally::spent): items the
//! lexer produced for this input, charged at tokora's single lexing chokepoint. It is the one
//! counter in that crate's cell taxonomy that a rollback does not touch (`input/lineage.rs`'s
//! table: *"a budget a rollback refunds is not a budget"*), which is what makes it the only
//! reading here that `sync_balanced`'s own internal rewind cannot refund. It is charged even when
//! no ceiling is configured, so reading it costs nothing and configures nothing.
//!
//! `committed` is `span().end()`, which is what tokora's [`InputRef::cursor`] doc names for
//! committed progress. It is deliberately **not** `offset()`, whose own doc says *"It is **not** a
//! progress metric"*: `offset()` reports the end of the newest *cached* token, so a peeked
//! 30 KB block string would inflate the denominator and make the guard less eager — the wrong
//! direction for a resource guard. Measured over ~50 000 recovery-helper entries the two differ by
//! at most one byte and neither ever moves backwards, so choosing the honest one costs nothing.
//!
//! ## Why `spent ≤ committed` is the calibration, and what it rests on
//!
//! Every produced item covers at least one byte (tokora `debug_assert`s a nonempty span), so a
//! parse that lexes each byte **once** satisfies `spent ≤ committed` — a ratio of 1.0 items/byte,
//! and that ceiling is *reached*, by a document of single-byte tokens with no separators.
//!
//! `spent` counts produce-events, though, not distinct tokens: **a re-lex is charged again**. So
//! the ceiling holds only while nothing re-lexes, and that premise has to be named rather than
//! assumed. In this tree it holds because:
//!
//! - smear never calls `InputRef::state_mut`, `set_state`, `restore` or any rollback door — the
//!   cache-clearing surgeries. (The `state_mut` calls in `smear-lexer` are `Lexer::state_mut`, a
//!   lexer mutating its own bracket counter, which evicts nothing.)
//! - the lossless tree's only speculation is four single-token probes — `try_eat(Colon)` in both
//!   dialects' `field`, `try_eat(Bang)` in `type_ref`, and one always-rolled-back trivia
//!   lookahead — none of which can enclose a call to either helper. Documents built to maximise
//!   them measure **0.667 – 1.000** items/byte, i.e. at or below the ceiling.
//! - the remaining re-lex source is the failed scan itself, which is the quantity this guard
//!   exists to bound. Charging it is the mechanism, not a leak in it.
//!
//! Measured worst honest ratio: **1.000**, over 278 repository fixtures and ten deliberately dense
//! constructions. Against that, `! ` repeated measures **2 001**, `[ type ] ` **1 334** and
//! `( type ) ` **2 668** — three orders of magnitude of separation, so the factor is chosen for
//! margin rather than for discrimination.
//!
//! ## What the guard costs when it fires
//!
//! On every shape measured, nothing: the tree, the diagnostics and the text are byte-identical,
//! because those documents already take the `None` arm at every call and refusing a scan that was
//! going to fail is a no-op on the output. It is **not** identity-preserving in general, and
//! `tests/resync_allowance.rs` pins the one falsifier — a resync-quadratic prefix that burns the
//! allowance, followed by a junk run whose only sync point is an `Int` (a sync point that is not a
//! definition start, so it cannot rescue the earlier resyncs). There the recovery granularity
//! changes, in the direction this module already prices as the cheap one: *stopping early costs at
//! most one extra `Error` node; stopping late costs a subtree.*

use tokora::{
  InputRef, Lexer, ParseContext, SimpleSpan, Token,
  emitter::{CstEmitter, FromUnclosed},
  error::{Unclosed, UnexpectedEot, token::UnexpectedToken},
  input::Balance,
  lexer::FromLogos,
  span::Spanned,
  utils::DowncastRef,
};

use crate::combinator::ErrorOf;

/// The span of the single-byte delimiter an `expect` has just committed, given the input's
/// committed extent.
///
/// `expect` reports only whether it matched, so the opener's own span has to be recovered from
/// the input: its end is the delimiter's end, and every delimiter either suite opens (`[`, `{`,
/// `(`, and GraphQLx's `<`) is exactly one byte. That span is what an unclosed-delimiter
/// diagnostic points at — the opener that was never closed, not the end of input where the
/// absence was noticed.
#[inline]
pub fn opener_span(end: usize) -> SimpleSpan {
  SimpleSpan::new(end.saturating_sub(1), end)
}

/// [`kind_of`](crate::lossless::trivia::kind_of)'s twin for the spelling: `DowncastRef`, reached
/// without letting method resolution pick the wrong `Self`.
///
/// `sync_balanced` hands its predicate a `Spanned<&Token, &Span>`, so the same `&&Token` receiver
/// that costs `kind_of` its own helper applies here. The projection is owned and `Copy`, so
/// nothing borrowed escapes.
///
/// The projection is a type parameter for the reason
/// [`peek_as`](crate::lossless::trivia::peek_as) records: a dialect's contextual-keyword enum is
/// a concrete dialect type and this module may not name one.
#[inline]
pub fn keyword_of<Kw, T: DowncastRef<Kw>>(token: &T) -> Option<Kw> {
  token.downcast_ref()
}

/// A balanced pair ran to end of input before its closer arrived.
///
/// `err` carries the **opener's** span, which is where the diagnostic points — the closer that
/// never came has no position of its own. It is built by the caller rather than here because the
/// delimiter marker (`UnclosedBracket`, `UnclosedBrace`, `UnclosedParen`, …) is the one part of
/// this that differs per pair, and `from_unclosed` is generic over exactly that marker. One
/// function therefore covers every pair a dialect opens, including one this crate has never seen.
///
/// **This helper opens no node and consumes nothing, and that is correct rather than a
/// shortcoming.** Its callers reach it when the atom set reported end of input, so there is no
/// token left to skip, nothing to attribute to an `Error` node, and nothing for `sync_balanced`
/// to settle: at end of input it would commit nothing, wrap nothing and emit no hole diagnostic.
/// Calling it here would be dead code that *reads* as the mechanism.
///
/// **Loop safety does not depend on consuming here.** Every caller's `while` is guarded by an
/// end-of-input test and this helper's result is `return`ed out of that loop rather than
/// continuing it, so there is no iteration to starve. The missing closer's absence is recorded in
/// the diagnostic; the source bytes are already accounted for by the tokens committed before end
/// of input.
pub fn unclosed<'inp, L, Ctx, Lang, Delimiter>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  err: Unclosed<Delimiter, SimpleSpan, Lang>,
) -> Result<(), ErrorOf<'inp, L, Ctx, Lang>>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  L::Token: FromLogos<'inp>,
  Ctx: ParseContext<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: FromUnclosed<'inp, L, Lang>,
{
  let open = err.span();
  let err = <ErrorOf<'inp, L, Ctx, Lang> as FromUnclosed<'inp, L, Lang>>::from_unclosed(err);
  inp.emit_error(Spanned::new(open, err))?;
  Ok(())
}

/// Nothing that could start one of `expected` is here. **Report, and consume nothing.**
///
/// # When to reach for this rather than [`unexpected`]
///
/// Whenever the offending token is worth more to the *next* production than it would be inside an
/// `Error` node. Three shapes are in that class, and each one is a bug if it consumes:
///
/// - **A required keyword is absent**, as with a type condition's `on`. The name that *is* there
///   is still the condition's type, so eating it would trade one diagnostic for a lost subtree.
/// - **A delimited shape is empty** where the grammar says `+` — `{}`, `()`. The report points at
///   the closer, which the enclosing loop is about to eat as its own.
/// - **A prefix has no tail**, as with a `...` at the end of a selection set. The `}` after it
///   belongs to the enclosing set; [`unexpected`] would swallow it (a closer *is* a sync point)
///   and the set would then run to end of input looking for a closer it had already consumed.
///
/// **Consuming nothing makes this unsafe inside a loop that continues.** Every caller must either
/// `return` afterwards or have already made progress on its own — the same distinction
/// [`unclosed`] documents, and the reason [`unexpected`] exists at all.
pub fn report_unexpected<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  expected: &'static [<L::Token as Token<'inp>>::Kind],
) -> Result<(), ErrorOf<'inp, L, Ctx, Lang>>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  L::Token: FromLogos<'inp> + Clone,
  Ctx: ParseContext<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<usize, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, SimpleSpan, Lang>>,
{
  // `Clone::clone(t.data)`, not `t.data.clone()`: `Spanned<&Token, &Span>`'s `data` field is
  // already a reference, so the method form resolves to `<&Token as Clone>::clone` and hands back
  // another borrow — which then infers `UnexpectedToken`'s `T` as `&Token` and fails the `From`
  // bound a long way from here.
  match inp.peek_head_map(|t| Spanned::new(*t.span, Clone::clone(t.data)))? {
    Some(found) => {
      let span = found.span;
      let err =
        UnexpectedToken::<_, _, _, Lang>::expected_one_of(span, expected).with_found(found.data);
      inp.emit_error(Spanned::new(span, err.into()))?;
    }
    None => {
      let end = inp.span().end();
      let span = SimpleSpan::new(end, end);
      let err = UnexpectedEot::<usize, Lang>::eot_of(end);
      inp.emit_error(Spanned::new(span, err.into()))?;
    }
  }
  Ok(())
}

/// Nothing that could start one of `expected` is here, and there is still input.
///
/// Reports once through [`report_unexpected`], then makes progress — in that order, because the
/// diagnostic names the token that is about to be skipped.
///
/// # Why this cannot be `sync_balanced` alone
///
/// The obvious implementation is a bare `inp.sync_balanced(…)`, on the reasoning that a balanced
/// skip both makes progress and reports itself. It does neither reliably, and the two gaps are
/// exactly the inputs a recovery path meets:
///
/// - **A stray closer is itself a sync point.** At depth zero `pred` is consulted *first*, so over
///   `[1 ) 2]` the scan matches the `)` the caller is standing on, returns
///   `Some(Hole { skipped: 0 })` and consumes nothing. The caller's `while` then re-reads that
///   same `)` — forever.
/// - **Garbage running to end of input never matches.** Over `[1 ! ! !` there is no sync point, so
///   the scan rewinds wholesale and returns `Ok(None)`. Same spin.
///
/// So the skip is *attempted first* — it is the good outcome: one hole diagnostic for a whole
/// garbage run, nesting-aware, wrapped by the sink itself — and a fallback consumes exactly one
/// token into an `Error` node when the skip made no progress. Progress is then unconditional
/// whenever input remains, which is the enclosing loop's whole safety argument.
///
/// The fallback node is opened **only once the token is in hand**, by hand rather than through the
/// `node` combinator, so a caller that reaches this at genuine end of input gets no node at all
/// rather than an empty zero-width `Error` one.
///
/// # The two `fn` pointers
///
/// `delimiters` is the dialect's pair classifier and `is_sync_point` its depth-zero restart
/// predicate. Both are tables rather than logic, which is why they are arguments and why this
/// function is here: Task 6b's threshold admits a helper needing at most two of them.
pub fn unexpected<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  expected: &'static [<L::Token as Token<'inp>>::Kind],
  delimiters: fn(&<L::Token as Token<'inp>>::Kind) -> Balance<u8>,
  is_sync_point: fn(&L::Token) -> bool,
  error_kind: u16,
) -> Result<(), ErrorOf<'inp, L, Ctx, Lang>>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  L::Token: FromLogos<'inp> + Clone,
  Ctx: ParseContext<'inp, L, Lang>,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<usize, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, SimpleSpan, Lang>>,
{
  report_unexpected(inp, expected)?;

  // #168's guard. `None` is safe to substitute *here* — unlike in [`resync_to`], which needs a
  // peek in its place — because the test below folds `None` and a zero-skip `Some` into the same
  // arm: this helper's caller has already ruled the token at hand junk, so consuming one is right
  // either way. Progress, which is this helper's whole contract, is therefore unaffected. See
  // `scan_allowance_exhausted`.
  let hole = if scan_allowance_exhausted(inp) {
    None
  } else {
    inp.sync_balanced(delimiters, |t| is_sync_point(t.data))?
  };
  if hole.is_some_and(|h| h.skipped() > 0) {
    // The sink wrapped the skipped region in the profile's `error_kind` on its own.
    return Ok(());
  }

  let mark = inp.cst_mark();
  if inp.try_expect(|_| true)?.is_some() {
    inp.cst_start_at(mark, error_kind);
    inp.cst_finish(error_kind);
  }
  Ok(())
}

/// Lexer items a parse may produce per committed byte before recovery stops scanning ahead.
///
/// An honest parse produces **at most one item per byte** — see the module docs for the ceiling
/// and for the premise it rests on — so eight is a margin over a measured 1.000, not a
/// discrimination threshold: the shapes this exists to stop measure 1 334 to 2 668.
///
/// Raising it does not make recovery better on any document that is not already quadratic; it
/// only buys an attacker a longer run before the guard engages. Lowering it below **2** would
/// start refusing scans on honest input, because 1.0 is reached rather than approached.
pub(crate) const SCAN_ALLOWANCE_FACTOR: usize = 8;

/// The allowance every parse gets regardless of how little it has committed.
///
/// **Deleting this is the way to get the guard wrong that costs the most and shows the least.**
/// The denominator is committed bytes, and at the first recovery call of a document that is
/// frequently `0` — `SCAN_ALLOWANCE_FACTOR * 0` refuses the very first scan of every input,
/// turning the one recovery a truncated document needs into a token-by-token walk. The floor is
/// also the absolute bound on what a *small* document can waste: nothing can spend more than this
/// before its committed length starts paying for the scans.
pub(crate) const SCAN_ALLOWANCE_FLOOR: usize = 4_096;

/// Whether this parse has already produced more lexer items than its committed length pays for.
///
/// Both scanning helpers consult this, and **both must**: [`unexpected`] carries 47 of the tree's
/// 52 recovery call sites and [`resync_to`] the other 5, and each reaches the quadratic on tails
/// the other's predicate is blind to. Guarding one leaves the other live — which is smear issue
/// #167 round 2's finding on this very subsystem, restated one helper along.
///
/// # The two readings, and why neither is the obvious one
///
/// `spent` must be a counter no rewind refunds, and inside this crate there is exactly one:
/// `L::State` is cloned into `sync_balanced`'s own `ThroughEntry` and restored on the no-match
/// exit, so a tally kept there would be refunded by the very scan whose cost it records.
///
/// `committed` must be a *progress* reading. `offset()` is not one by its own documentation — it
/// is the end of the newest cached token, so lookahead inflates it and inflating the denominator
/// makes a resource guard less eager. `span().end()` is the reading tokora names for committed
/// progress.
///
/// # It is a rate limiter, not a latch
///
/// The comparison is re-derived per call against a denominator that keeps growing, so a parse that
/// stops wasting recovers its allowance and scans again. That is why the degradation stays local
/// to the pathological region instead of poisoning the rest of the document — measured on the
/// falsifier, where a 3 000-token junk run past a burnt allowance lost 212 holes rather than 3 000.
///
/// # If the premise under it ever breaks, it breaks safe
///
/// Both readings are monotone at these call sites because no recovery call is inside a speculative
/// attempt — the lossless tree's only `attempt`/`try_parse_input` bodies are single-token probes.
/// Should that stop being true, a rollback lowers `committed` and never lowers `spent`, so the
/// guard engages *earlier*: recovery coarsens, the bound holds, and nothing becomes unsound.
#[inline]
fn scan_allowance_exhausted<'inp, L, Ctx, Lang>(inp: &InputRef<'inp, '_, L, Ctx, Lang>) -> bool
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseContext<'inp, L, Lang>,
{
  let spent = inp.token_budget().spent();
  #[cfg(all(feature = "test-support", feature = "std"))]
  scan_allowance::record_spent(spent);
  let exhausted = spent
    > SCAN_ALLOWANCE_FACTOR
      .saturating_mul(inp.span().end())
      .saturating_add(SCAN_ALLOWANCE_FLOOR);
  #[cfg(all(feature = "test-support", feature = "std"))]
  if exhausted {
    scan_allowance::record_refusal();
  }
  exhausted
}

/// What `tests/resync_allowance.rs` reads: how often the guard refused, and the numerator it
/// refused on.
///
/// Test-only because the shipped parser has no reason to publish either and a `pub` counter is a
/// public surface to keep.
///
/// # Thread-local, and that is not a detail
///
/// The first version of this was a pair of process-wide atomics with "serialize with
/// `--test-threads=1`" written next to them. libtest runs test *functions* on their own threads by
/// default, so three assertions in this crate's own suite failed the moment they ran beside
/// anything else — a gate that only holds under a flag nobody passes in CI is not a gate. A
/// thread-local pair makes each reading belong to the parse that produced it, and the ordering
/// constraint disappears rather than being documented.
///
/// [`peak_spent`] exists because the property under test is **linearity**, and wall-clock is a bad
/// witness for it: the machine these were first measured on sat at load average 54 and the same
/// parse varied 1.6× between runs. `spent` is the produce-event count the guard is denominated in;
/// it is deterministic, machine-independent, and it is what actually grows quadratically. A gate
/// on it fails for the reason the defect exists rather than for the reason the box was busy.
#[cfg(all(feature = "test-support", feature = "std"))]
pub mod scan_allowance {
  use core::cell::Cell;

  std::thread_local! {
    static REFUSALS: Cell<usize> = const { Cell::new(0) };
    static PEAK_SPENT: Cell<usize> = const { Cell::new(0) };
  }

  #[inline]
  pub(super) fn record_refusal() {
    REFUSALS.with(|c| c.set(c.get().saturating_add(1)));
  }

  #[inline]
  pub(super) fn record_spent(spent: usize) {
    PEAK_SPENT.with(|c| c.set(c.get().max(spent)));
  }

  /// Zeroes both tallies for the calling thread. Call immediately before the parse under test.
  pub fn reset() {
    REFUSALS.with(|c| c.set(0));
    PEAK_SPENT.with(|c| c.set(0));
  }

  /// How many scans this thread has refused since its [`reset`].
  pub fn refusals() -> usize {
    REFUSALS.with(Cell::get)
  }

  /// The largest `TokenBudgetTally::spent` this thread saw at a recovery call since [`reset`].
  ///
  /// Zero when the parse made no recovery call at all, which is not the same as "spent nothing" —
  /// a clean document reaches neither helper.
  pub fn peak_spent() -> usize {
    PEAK_SPENT.with(Cell::get)
  }
}

/// A production returned `Err`: skip its wreckage and stop **before** the next restart point.
///
/// # This helper reports nothing, and that is the point
///
/// Its callers are the document loops, and they reach it *because* a production failed — which
/// means `expect` already emitted at the position the failure happened. A second diagnostic here
/// would point at whatever the resync happens to start on, which is not where anything went
/// wrong.
///
/// # The fallback is **not** [`unexpected`]'s, and copying it there was a defect
///
/// [`unexpected`] consumes one token whenever its balanced skip made no progress, because its
/// caller has already ruled the token at hand junk. Here the opposite is true: a scan that stops
/// having skipped **zero** tokens has stopped *on a restart point* — the exact token this helper
/// went looking for — and eating it costs the whole definition. `type T { a scalar S }` loses its
/// `ScalarTypeDefinition` that way.
///
/// So the outcomes are told apart rather than lumped together:
///
/// - **`Some(hole)`** — a restart point is at hand, whether the scan crossed anything to reach it
///   or not. Whatever it crossed is already wrapped in `error_kind` by the sink; return, and the
///   caller's next turn parses the head.
/// - **`None`** — the scan ran to end of input without finding one, rewound, and committed
///   nothing. Everything left is junk, so consuming one token into an `Error` node can eat no
///   definition, and it is the branch that keeps this helper self-sufficient.
///
/// # Termination does not rest on that fallback
///
/// A document loop's dispatcher consumes at least one token before it can fail — a description
/// commits its string, every keyword arm commits its keyword, and the fall-through is
/// [`unexpected`], which guarantees progress on its own — so a resync that consumes nothing cannot
/// starve the loop. Deleting the `None` branch is therefore **not** observable as a hang; it is
/// kept because a helper whose safety depends on a caller's internals is one refactor away from
/// being wrong, and because a token nobody attributes is a token that reaches the tree as a loose
/// child of the root.
///
/// The leading peek is not decoration either. `sync_balanced` counts *every* token into its hole,
/// trivia included, so trivia the failed production left uncrossed would otherwise land inside the
/// `Error` node instead of beside it.
pub fn resync_to<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  delimiters: fn(&<L::Token as Token<'inp>>::Kind) -> Balance<u8>,
  is_restart_point: fn(&L::Token) -> bool,
  error_kind: u16,
) -> Result<(), ErrorOf<'inp, L, Ctx, Lang>>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  L::Token: FromLogos<'inp>,
  Ctx: ParseContext<'inp, L, Lang>,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<usize, Lang>>,
{
  if crate::lossless::trivia::peek_kind(inp)?.is_none() {
    return Ok(());
  }

  // #168's guard, and this is the site the issue does *not* name. Its witness — `!` repeated —
  // never reaches this function at all (`unexpected.calls = 4000, resync.calls = 0`); what reaches
  // it is a definition head parked at depth 1, where the scan never consults the predicate.
  // `[ type ] ` repeated is that shape with `unexpected` held at a zero-skip, so it isolates this
  // helper's own Θ(n²).
  //
  // A refused scan is **not** a bare `None` here, and that asymmetry with [`unexpected`] is the
  // whole reason this is four lines rather than one. That function treats `None` and a zero-skip
  // `Some` identically — both fall through to its one-token fallback — so substituting `None`
  // there is exactly behaviour-preserving. This one does not: the section above spends its length
  // on why, and the answer is that a zero-skip `Some` means the restart point is the token *at
  // hand*, so taking the `None` arm would eat it. That is the `type T { a scalar S }` loses its
  // `ScalarTypeDefinition` defect, pinned by
  // `a_resync_that_lands_on_a_definition_head_does_not_eat_it`.
  //
  // So a refused scan still answers the zero-skip question — one peek, no scan, no walk. It is
  // written unconditionally rather than on an argument that the two can never co-occur: burning
  // the allowance needs a tail with no restart point, and landing on a restart point needs one, so
  // they look mutually exclusive. Two structural arguments of exactly that shape were already
  // wrong about this subsystem in this issue's own investigation, and a peek is cheaper than being
  // right about it.
  let restart_point_at_hand = if scan_allowance_exhausted(inp) {
    inp.head_satisfies(is_restart_point)?
  } else {
    inp
      .sync_balanced(delimiters, |t| is_restart_point(t.data))?
      .is_some()
  };
  if restart_point_at_hand {
    return Ok(());
  }

  let mark = inp.cst_mark();
  if inp.try_expect(|_| true)?.is_some() {
    inp.cst_start_at(mark, error_kind);
    inp.cst_finish(error_kind);
  }
  Ok(())
}
