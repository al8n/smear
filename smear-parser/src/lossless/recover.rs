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
//! ## Where the two numbers come from, and why they are the same unit
//!
//! `spent` is [`TokenBudgetTally::spent`](tokora::input::TokenBudgetTally::spent): items the lexer
//! produced for this input, charged at tokora's single lexing chokepoint. It is the one counter in
//! that crate's cell taxonomy a rollback does not touch (`input/lineage.rs`'s table: *"a budget a
//! rollback refunds is not a budget"*), which is what makes it the only reading here that
//! `sync_balanced`'s own internal rewind cannot refund. It is charged even when no ceiling is
//! configured, so reading it costs nothing and configures nothing.
//!
//! `committed` is the lexer state's own token tally, which each dialect passes in. It counts the
//! same events `spent` counts and is incremented at the same moment — but it lives in `L::State`,
//! which `sync_balanced` clones into its `ThroughEntry` and **restores** on the no-match exit. So
//! the two readings are a matched pair by construction: `spent` is every item this parse ever
//! lexed, `committed` is the subset that survived, and their ratio *is* the amplification factor.
//! A parse that never re-lexes has `spent == committed` exactly.
//!
//! **Both sides must be in events, and getting that wrong is not a conservative error.** The
//! guard's first form divided produce-events by *committed bytes*, on the reasoning that an
//! item-denominated numerator against a byte denominator only ever makes the guard more generous —
//! the safe direction. Generous without a bound is not a direction. Bytes-per-event is chosen by
//! the document: a GraphQL comment runs to end of line, so one event can carry as many bytes as
//! the attacker likes. Alternating a one-byte junk atom with an `L`-byte comment lets committed
//! bytes grow ~`L` per trip while `spent` grows ~1, so the allowance outran the meter by a factor
//! of `L` and the guard stayed open — measured at 1 024 bytes of padding, `spent/bytes` read
//! **3.90** against a factor of 8 (never refusing) while `spent/committed` read **1 002**. The
//! shape ran 64 MB in 106 s with **zero** refusals, growing ×7.1 in time for every ×4 in bytes.
//! `tests/resync_allowance.rs` carries that axis now; a census of one-byte atoms cannot see it.
//!
//! ## What the factor is a margin over, stated exactly
//!
//! `spent == committed` holds for a parse in which **every produced item both survives and
//! increments the tally**. That is narrower than "an honest parse", and the wording here has been
//! wrong twice already — first as `spent <= source bytes`, then as an unqualified identity — so
//! the two exceptions are named rather than left to be discovered a third time.
//!
//! **Exception 1 — re-lexing.** An item lexed twice is charged twice to `spent` and once to the
//! tally, because the tally lives in `L::State` and comes back with the rewind. In this tree the
//! sources are enumerable: smear calls no cache-clearing door (`InputRef::state_mut`, `set_state`,
//! `restore`, `rollback_*` appear nowhere; the `state_mut` calls in `smear-lexer` are
//! `Lexer::state_mut`, a lexer touching its own bracket counter), the lossless tree's only
//! speculation is four single-token probes, and the remainder is the failed scan this guard exists
//! to bound. This exception is the mechanism, not a leak in it.
//!
//! **Exception 2 — lexer errors.** tokora charges `spent` for *every* item the lexer hands back,
//! errors included ("a lexer error is charged. This is the shape the budget exists for"). smear's
//! tally does not: `smear-lexer`'s `tt_hook_and_then` and `tt_hook_and_then_into_errors` increment
//! through `Result::inspect`, which runs on `Ok` alone, and the rules routed through them include
//! several that **never** succeed — `.` and `..` (unterminated spread), `-` and `+` (unexpected
//! character) — plus every malformed number and unterminated string. `tt_hook`, `tt_hook_map`,
//! `increase_recursion_depth_and_token` and `decrease_recursion_depth_and_increase_token` are
//! unconditional; those four cover all punctuation, all trivia, identifiers and comments.
//!
//! So error density inflates the ratio without bound — measured at **514** over a document of one
//! `!` per 256 **unspaced** `-` — and unlike re-lexing it is *linear* work being charged, which the
//! guard has no business rationing.
//!
//! The spacing is load-bearing and the first version of this sentence omitted it. A space is a
//! counted commit, so `! - - - …` with the same 256 dashes reads **8.00** rather than 514: it is
//! the *unbroken* error run that separates the two counters, and any committed token between them
//! closes the gap again. Sixty-four unspaced dashes read 130; sixty-four spaced read 8.02.
//!
//! ## Why exception 2 is a bounded cost rather than a repair
//!
//! Because it is nearly unreachable, and where it is reachable it is small and self-clearing.
//! A run of error lexemes does not become tokens the parser recovers from — the scanner emits a
//! diagnostic per bad lexeme and keeps looking — so a document dense in them reaches **no recovery
//! call at all**: `- + 00 1.` repeated 4 000 times each measure `spent = 0`, `committed = 0`,
//! `refusals = 0`. Interleaving them with junk that *does* reach recovery inflates the ratio to
//! 514, and refusing there costs nothing, because those scans were going to fail anyway.
//!
//! The reachable cost needs a third thing: an error run of `k` lexemes, long enough to blow the
//! allowance, followed by a junk run whose scan would have **succeeded**.
//!
//! Below [`SCAN_ALLOWANCE_FLOOR`] nothing happens at all. Above it the guard refuses until the
//! denominator catches up, and the rate it catches up at is **not a constant** — it is set by how
//! many items the junk commits per refusal. Refusals continue while `k + c > FACTOR * c + floor`,
//! so they stop at `c = (k - floor) / (FACTOR - 1)` committed items; a junk run committing `m`
//! items per refusal therefore takes
//!
//! ```text
//! refusals ≈ (k - SCAN_ALLOWANCE_FLOOR) / ((SCAN_ALLOWANCE_FACTOR - 1) * m)
//! ```
//!
//! | junk | `m` | `k` = 20 000 | 33 000 | 80 000 |
//! |---|---|---|---|---|
//! | `! ` (bang + space) | 2 | 1 137 | 2 065 | 5 422 |
//! | `!` (dense) | 1 | 2 273 | 4 130 | 10 844 |
//! | formula | | 2 272 / 1 136 | 4 129 / 2 064 | 10 843 / 5 421 |
//!
//! **`m` is why the first version of this note was wrong.** It read `(k - floor) / 16`, which is
//! `m = 2` — a property of the `! ` witness it was measured on, not of the guard. A dense `!!!!`
//! suffix commits one item per refusal instead of two and doubles the count, and the pin written
//! beside it (`beyond <= k / 8`) held for `! ` at every size and **broke for the dense shape at
//! k = 33 000**, exactly where `k/7 - 585 > k/8` predicts. The pin now asserts the formula and the
//! gate runs the dense shape, which is the one that stresses it.
//!
//! **`m` cannot be zero, which is what makes this self-clearing at all.** [`unexpected`]'s
//! no-progress fallback consumes exactly one token through `try_expect`, and a consumed token is
//! by definition one the lexer produced *and* the tally counted — error lexemes never reach the
//! parser, because the scanner absorbs them into diagnostics on the way past. So every refusal
//! moves the denominator by at least one. Measured over every single-token junk alphabet in both
//! dialects, per refusal rather than averaged over the run: the smallest gap between two
//! consecutive refusals is **1** for dense junk and 2 for spaced, and zero never occurs.
//! `every_refusal_commits_at_least_one_item` runs both dialects, and it has to — GraphQLx
//! carries 46 `tt_hook_and_then` rules of its own, so the arithmetic behind exception 2 is its
//! arithmetic too, even though its richer token set commits most ASCII junk and makes the regime
//! harder to reach there.
//!
//! **That is a floor on `m`, not a promise that the guard re-closes.** Re-closing needs the local
//! commit rate to outpace the local spend rate, so junk carrying more than `FACTOR - 1` error
//! lexemes per committed token holds the guard shut for as long as that regime lasts — it re-opens
//! when the regime ends, not after a bounded number of refusals. The table above prices exactly
//! that case. What `m >= 1` buys is that the denominator is never *frozen*, which is what makes
//! the count proportional to `k` rather than unbounded.
//!
//! Repairing it belongs in `smear-lexer`, not here — the tally would have to increment before the
//! rule runs rather than after it succeeds — and that changes what `LosslessLimits::max_tokens`
//! counts, which is a public knob with its own contract. It is worth doing on its own terms: that
//! same asymmetry means `max_tokens` bounds **nothing** over malformed input today, and a budget
//! of 100 truncates 4 000 `!` at 2 diagnostics while letting 4 000 `-` through at 4 001.
//!
//! ## The bound is on the whole parse, and it does not depend on the shape
//!
//! Everything above prices one *episode*. The bound that matters is the total, and it falls out of
//! the denominator rather than out of any shape: the guard permits scanning only while
//! `spent <= FACTOR * committed + floor`, and `committed` can never exceed `T`, the number of items
//! the document actually contains. So the total lexing a parse can be made to do is
//! `FACTOR * T + floor` plus the one scan in flight when the guard closes — **linear in the
//! document, whatever order the shape puts its progress and its scans in**.
//!
//! That is worth stating because the self-clearing property reads as a liability from the other
//! side: a shape that alternates cheap commits with expensive failed scans re-opens the guard on
//! purpose. It cannot win, because refilling the allowance costs exactly the committed items that
//! bound it — a *successful* scan commits everything it crossed, so it adds equally to both sides
//! and buys back only `FACTOR` times its own cost, out of a total that is already capped at `T`.
//! `the_guard_cannot_be_refilled_into_superlinearity` asserts that, and it asserts it by
//! **doubling** rather than by reading `spent / committed`. That ratio is not a work bound and must
//! not be pinned as one: on an error-dense construction it climbs 73 → 93 → 109 → 118 across four
//! doublings *while `spent` doubles at ×1.99, ×1.99, ×2.00* — perfectly linear work, a metric that
//! grows 1.6× over the same range. The numerator counts error lexemes the denominator does not
//! (exception 2), so their ratio drifts with error density by construction, and a threshold on it
//! fails with no defect present. Doubling the construction and watching the numerator is the
//! mechanism; the ratio was an artifact of the four error-free shapes it was read on.
//!
//! **What that gate cannot see, stated here so it is not rediscovered.** One doubling at one
//! size is blind to superlinearity milder than about `n^1.38` — the gap between the 2.6
//! threshold and the 2.0 a linear shape reads — and blind to a defect whose onset lies past the
//! sizes it runs. Neither is a hole in the bound, because the bound is structural
//! (`spent <= FACTOR * committed + floor`, with `committed <= T`) and the gate is a tripwire on
//! it rather than its proof. Covering the milder band means a third size, not a tighter
//! threshold.
//!
//! Its five constructions are also not five witnesses of the same thing: **two run at zero
//! refusals at both sizes**, so they pin alternation with *successful* scans and say nothing
//! about refill. Three carry the refill property. Counting the cases overcounts the evidence by
//! two.
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
//!
//! The falsifier is one measured instance; the **bound** over all inputs is structural and does
//! not need one. A refusal replaces one committed hole with the fallback's single `Error` node, so
//! it trades the hole's skipped-region note for whatever the fallback emits, and the bound is
//! **two-sided**: `|Δ diagnostics| <= refusals`.
//!
//! **It is not one-sided, and the pinned falsifier is misleading about which way it goes.** That
//! document nets `+123` because its junk run is long: one refused scan becomes many one-token
//! `Error` nodes, each with its own report. Chop the same junk fine — `"[ type ] "×k` then
//! `"! 1 "×n`, where every junk run is a single token ending at an `Int` sync point — and each unit
//! goes from 3 diagnostics to 2, because the skipped-region note is suppressed and there is no
//! second token for a replacement report to attach to. Measured **−242 / −543 / −843** at
//! `k = n = 2 000 / 4 000 / 6 000` against a derived oracle (`2k + 3n`) that is exact on every
//! refusal-free size. So the guard can report *fewer* diagnostics than the unfused parser, and the
//! census that bounded it in one direction had no row of this shape — the same blindness the
//! numbers above were corrected for, inside the gate written to bound the previous instance.
//!
//! What survives: `|Δ| <= refusals`; **every consumed token still carries its own report**, so
//! nothing reaches the tree undiagnosed; refusals are zero on every document that does not blow the
//! allowance, so an honest corpus is byte-identical rather than merely similar; and the lossless
//! text is invariant in every case, because the fallback commits the token it consumes exactly as
//! the scan would have.
//!
//! Emitting a note from the fallback to make the count monotone would be the wrong repair — it
//! would make a refusal observable on documents the guard never refuses on, which is precisely the
//! unguarded equivalence `the_guard_did_not_change_the_answer` exists to hold.

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
  allowance_exhausted: bool,
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

  // #168's guard, computed by the dialect wrapper — see `scan_allowance_exhausted` for why the
  // number arrives rather than being read here. `None` is safe to substitute *at this site* —
  // unlike in [`resync_to`], which needs a peek in its place — because the test below folds `None`
  // and a zero-skip `Some` into the same arm: this helper's caller has already ruled the token at
  // hand junk, so consuming one is right either way. Progress, which is this helper's whole
  // contract, is therefore unaffected.
  let hole = if allowance_exhausted {
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

/// Lexer items a parse may produce per **surviving** item before recovery stops scanning ahead.
///
/// Both sides count produce-events, so a parse whose items all survive *and* all increment the
/// tally sits at exactly 1. The module docs state that condition precisely and name its two
/// exceptions — re-lexing, which is what this bounds, and lexer errors, which inflate the ratio
/// without bound and are pinned as a known cost. The shapes it exists to stop measure 40 to 1 002.
///
/// Raising it does not make recovery better on any document that is not already quadratic; it only
/// buys an attacker a longer run before the guard engages. Lowering it to **1** would start
/// refusing scans on honest input, because 1.0 is the identity rather than a bound approached from
/// below.
pub(crate) const SCAN_ALLOWANCE_FACTOR: usize = 8;

/// The allowance every parse gets regardless of how few items have survived.
///
/// **Deleting this is the way to get the guard wrong that costs the most and shows the least.**
/// At the first recovery call of a document the denominator is frequently near `0`, and
/// `SCAN_ALLOWANCE_FACTOR * 0` refuses the very first scan of every input — turning the one
/// recovery a truncated document needs into a token-by-token walk. The floor is also the absolute
/// bound on what a *small* document can waste: nothing can spend more than this before its own
/// surviving items start paying for the scans.
pub(crate) const SCAN_ALLOWANCE_FLOOR: usize = 4_096;

/// Whether this parse has produced more lexer items than the ones it kept can pay for.
///
/// `spent` is every item the lexer produced; `committed` is the subset a rewind did not take back.
/// **They must be the same unit.** Dividing events by bytes is what the first version of this did,
/// and the module docs carry the measurement that killed it.
///
/// # Why this takes numbers instead of the handle
///
/// `committed` is the lexer state's token tally, and reaching it means naming `smear-lexer` —
/// which this module may not do, and `lossless_isolation.rs` enforces that. The number crosses the
/// line the way [`crate::lossless::depth`]'s ceiling does: as a plain `usize`, read by dialect code
/// that is allowed to know where it lives. Passing a `fn(&L::State) -> usize` instead would put a
/// third `fn` pointer on [`unexpected`], over the threshold this module's header states for
/// lifting a helper into the substrate at all.
///
/// Each dialect reads it immediately before the call. Nothing between there and the scan commits a
/// token, so the two readings differ by at most the peek [`report_unexpected`] takes — a couple of
/// events against a floor of four thousand.
///
/// # Both helpers consult it, and both must
///
/// [`unexpected`] carries 47 of the tree's 52 recovery call sites and [`resync_to`] the other 5,
/// and each reaches the quadratic on tails the other's predicate is blind to. Guarding one leaves
/// the other live — which is smear issue #167 round 2's finding on this very subsystem, restated
/// one helper along.
///
/// # It is a rate limiter, not a latch
///
/// The comparison is re-derived per call against a denominator that keeps growing, so a parse that
/// stops wasting recovers its allowance and scans again. That is why the degradation stays local to
/// the pathological region instead of poisoning the rest of the document.
///
/// # If the premise under it ever breaks, it breaks safe
///
/// Both readings are monotone at these call sites because no recovery call is inside a speculative
/// attempt — the lossless tree's only `attempt`/`try_parse_input` bodies are single-token probes.
/// Should that stop being true, a rollback lowers `committed` and never lowers `spent`, so the
/// guard engages *earlier*: recovery coarsens, the bound holds, and nothing becomes unsound.
#[inline]
pub(crate) fn scan_allowance_exhausted(spent: usize, committed: usize) -> bool {
  #[cfg(all(feature = "test-support", feature = "std"))]
  scan_allowance::record(spent, committed);
  let exhausted = spent
    > SCAN_ALLOWANCE_FACTOR
      .saturating_mul(committed)
      .saturating_add(SCAN_ALLOWANCE_FLOOR);
  #[cfg(all(feature = "test-support", feature = "std"))]
  if exhausted {
    scan_allowance::record_refusal(committed);
  }
  exhausted
}

/// What `tests/resync_allowance.rs` reads: how often the guard refused, and the two readings it
/// refused on.
///
/// Test-only because the shipped parser has no reason to publish any of it and a `pub` counter is
/// a public surface to keep.
///
/// # Thread-local, and that is not a detail
///
/// The first version of this was a set of process-wide atomics with "serialize with
/// `--test-threads=1`" written next to them. libtest runs test *functions* on their own threads by
/// default, so three assertions in this crate's own suite failed the moment they ran beside
/// anything else — a gate that only holds under a flag nobody passes in CI is not a gate. A
/// thread-local set makes each reading belong to the parse that produced it, and the ordering
/// constraint disappears rather than being documented.
///
/// [`peak_spent`] and [`peak_committed`] exist because the property under test is **linearity**,
/// and wall-clock is a bad witness for it: the machine these were first measured on sat at load
/// average 54 and the same parse varied 1.6× between runs. These are the produce-event counts the
/// guard is denominated in — deterministic, machine-independent, and the quantities that actually
/// grow superlinearly. A gate on them fails for the reason the defect exists rather than for the
/// reason the box was busy.
#[cfg(all(feature = "test-support", feature = "std"))]
pub mod scan_allowance {
  use core::cell::Cell;

  std::thread_local! {
    static REFUSALS: Cell<usize> = const { Cell::new(0) };
    static PEAK_SPENT: Cell<usize> = const { Cell::new(0) };
    static PEAK_COMMITTED: Cell<usize> = const { Cell::new(0) };
    static LAST_REFUSAL_COMMITTED: Cell<usize> = const { Cell::new(0) };
    static MIN_COMMIT_BETWEEN_REFUSALS: Cell<usize> = const { Cell::new(usize::MAX) };
  }

  #[inline]
  pub(super) fn record_refusal(committed: usize) {
    let n = REFUSALS.with(|c| {
      let n = c.get().saturating_add(1);
      c.set(n);
      n
    });
    // `m` per refusal, not averaged: the gap between this refusal and the previous one. The first
    // refusal has no predecessor to measure against.
    if n > 1 {
      let prev = LAST_REFUSAL_COMMITTED.with(Cell::get);
      MIN_COMMIT_BETWEEN_REFUSALS.with(|c| c.set(c.get().min(committed.saturating_sub(prev))));
    }
    LAST_REFUSAL_COMMITTED.with(|c| c.set(committed));
  }

  #[inline]
  pub(super) fn record(spent: usize, committed: usize) {
    PEAK_SPENT.with(|c| c.set(c.get().max(spent)));
    PEAK_COMMITTED.with(|c| c.set(c.get().max(committed)));
  }

  /// Zeroes every tally for the calling thread. Call immediately before the parse under test.
  pub fn reset() {
    REFUSALS.with(|c| c.set(0));
    PEAK_SPENT.with(|c| c.set(0));
    PEAK_COMMITTED.with(|c| c.set(0));
    LAST_REFUSAL_COMMITTED.with(|c| c.set(0));
    MIN_COMMIT_BETWEEN_REFUSALS.with(|c| c.set(usize::MAX));
  }

  /// How many scans this thread has refused since its [`reset`].
  pub fn refusals() -> usize {
    REFUSALS.with(Cell::get)
  }

  /// The largest `spent` this thread saw at a recovery call since [`reset`] — every item lexed.
  ///
  /// Zero when the parse made no recovery call at all, which is not the same as "spent nothing":
  /// a clean document reaches neither helper.
  pub fn peak_spent() -> usize {
    PEAK_SPENT.with(Cell::get)
  }

  /// The largest `committed` this thread saw at a recovery call — the items a rewind did not take
  /// back. `peak_spent / peak_committed` is the parse's amplification factor, and the guard's
  /// whole subject.
  pub fn peak_committed() -> usize {
    PEAK_COMMITTED.with(Cell::get)
  }

  /// The **smallest** number of committed items between two consecutive refusals — `m` at its
  /// worst for this parse, not averaged over it.
  ///
  /// `None` when fewer than two refusals happened, because a gap needs two ends. An average over
  /// thousands of refusals cannot see a single zero-commit one; this can.
  pub fn min_commit_between_refusals() -> Option<usize> {
    let v = MIN_COMMIT_BETWEEN_REFUSALS.with(Cell::get);
    (v != usize::MAX).then_some(v)
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
  allowance_exhausted: bool,
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
  let restart_point_at_hand = if allowance_exhausted {
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
