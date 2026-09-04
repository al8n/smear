//! The resource budget a smear lex runs under, and the nesting ceiling it defaults to.
//!
//! # Why these types exist at all
//!
//! Both lexers already carried a bracket-depth counter — tokora's
//! [`RecursionLimiter`] for the syntactic
//! stream, its [`Limiter`] for the lossless one — and both
//! *inherited* that counter's ceiling instead of choosing it. tokora's own docs are explicit that
//! the inherited 500 "was never sized against anything", because nothing about tallying lexer
//! nesting implies a native-stack cost for it to protect against, and that "a grammar that parses
//! untrusted, deeply nested input should still set its own limit […] against the stack the parse
//! will actually run on".
//!
//! smear never did, and the consequence was measured (issue #61): a **valid** GraphQL document of
//! about a kilobyte — `{ ... on Query { ... on Query { … } } }` nested 58 deep — overflowed the
//! native stack of a 2 MiB thread and killed the process with `SIGABRT`. The inherited 500 sat an
//! order of magnitude above that, so it could not fire first; nothing rejected the document and
//! there was no diagnostic, because there was no return.
//!
//! # Why a newtype rather than a number passed at the call site
//!
//! The number has to arrive at the doors that already exist, and one of those doors is not
//! smear's to change. The lossless entry points are all smear's own six functions, so a limiter
//! passed there would reach every one of them. The **syntactic** layer ships productions and no
//! runner: a consumer drives it with `Parser::with_parser(…).parse_str(src)`, which is what this
//! workspace's own README, `smear-compiler`'s crate docs and `smear-schema`'s builder all write —
//! and `parse_str` seeds the lexer with `L::State::default()`. The only place smear can name a
//! number that reaches *that* call is the `Default` of `L::State` itself, and `L::State` was
//! tokora's type.
//!
//! So these two newtypes are the decision: they are what [`Lexer::State`](tokora::Lexer::State)
//! resolves to for smear's four lexers, and their `Default` is [`MAX_NESTING_DEPTH`]. Every
//! existing call site — `parse_str`, `Lexer::new`, each `parse_lossless` — picks the ceiling up
//! without being edited, and a caller who wants a different one hands a different value to
//! `with_state` / `parse_*_with_state` / a lossless `*_with_limits` entry point.
//!
//! # What this counter is **not**, and the correction that says so
//!
//! It is not the stack-safety boundary, and the first version of this module said it was. The
//! tally here is one saturating scalar over every opener and every closer, **pair-blind**: it
//! decrements on any `}`, `]`, `)` — or GraphQLx `>` — regardless of which opener the parser is
//! actually inside. The parse's own recursion is a different quantity, and the lossless recovery
//! path is where they come apart, because it *consumes* a closer no opener matched and carries on
//! in the same selection-set loop. Over `{ ) f { ) f { …` this counter's maximum is **1** for the
//! whole document while every level leaves two more parser frames live: measured at `6f39cb9`,
//! 3.5 KB of that shape aborted a 2 MiB thread at 702 levels with the ceiling never firing.
//!
//! The bound that holds the native stack is therefore `smear-parser`'s `lossless::depth` — one
//! RAII level per nesting delimiter the **parse** descends through, released by leaving the frame
//! rather than by seeing a byte. That module carries the reasoning, and every number below is
//! still what sizes it: the ceiling it enforces is read from these types.
//!
//! What this counter keeps is a real job, and two of them:
//!
//! - **It is the cheaper check, and it fires first for well-formed input at the same ceiling.**
//!   The lexer runs ahead of the parser, so on a document whose closers match, the tally reaches
//!   the ceiling one token before the parser reaches it — which is why the diagnostics for
//!   ordinary over-deep input are unchanged by the parser-side budget. **Both qualifiers are
//!   load-bearing and neither is guaranteed**: the parse refuses at `min(this number,
//!   [`HARD_MAX`])`, so a caller who raises past the maximum gets a tally at the number they asked
//!   for and a parse budget below it, and the parse fires first; and on input whose closers do
//!   *not* match, pair-blindness pins the tally below the parse's depth however the two numbers
//!   compare. That is smear issue #169, and the parse side now ends the document itself rather
//!   than relying on either qualifier.
//! - **It is what latches tokora's poison boundary**, and that latch is what stops a
//!   machine-generated file from being lexed to the end after it has already proved it goes too
//!   deep — including stopping the *diagnostics* the rest of the file would otherwise produce,
//!   since tokora reports every lexer error a scan crosses. See
//!   `smear-parser/src/lossless/runner.rs` for why smear keeps the latch, and
//!   `smear-parser/src/lossless/depth`'s `The refusal ends the document` for how the parse side
//!   reaches the same end with no cell at all: the refusal rides out on the error value and the
//!   document roots stop on it.

use tokora::state::{
  State,
  recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
  token_tracker::TokenLimiter,
  tracker::{LimitExceeded, Limiter},
};

/// The greatest number of **simultaneously open** brackets a smear parse accepts by default. The
/// next one is refused.
///
/// One global count over `{`, `[` and `(` — and, in GraphQLx, over `<` and `>` as well, because
/// that dialect delimits generics with them. So a selection set inside an argument list inside a
/// list value spends three of the 24, not one, and a GraphQLx generic path spends one per
/// parameter level.
///
/// # Two counters read this number, and only one of them is the stack-safety boundary
///
/// The **population is the same** — one per nesting delimiter — but they are counted on different
/// sides and only one of them equals what recurses:
///
/// - the **lexer's** tally, in the types below, which is pair-blind and steps on bytes;
/// - the **parse's** own frame budget, `smear-parser`'s `lossless::depth`, one RAII level per
///   delimiter a lossless production descends through.
///
/// The second is the one this number exists for. The first is a cheap pre-filter that fires
/// earlier for well-formed input, and it was the *only* check for one release — which is the
/// defect the module header records: a closer no opener matched decrements it, so recovery could
/// walk the parse arbitrarily deep with the tally reading 1.
///
/// # The measurement this is derived from
///
/// Every figure below is a **native stack** measurement, and it is the number of *live frames*
/// that costs the stack — which is what the parse-side budget counts and what the lexer's tally
/// only approximates. Bisected on this tree with one parse per process on an explicitly sized
/// thread, greatest depth that returns before the next one aborts with
/// `fatal runtime error: stack overflow`:
///
/// | stack | 512 KiB | 1 MiB | **2 MiB** | 4 MiB | 8 MiB |
/// |---|---|---|---|---|---|
/// | syntactic door, GraphQL, `str`, debug | 12 | 27 | **57** | 118 | 239 |
///
/// Linear at roughly 34 KiB per level. **2 MiB is the stack this number is derived from**, and
/// that is the claim: it is what `std::thread::spawn` gives every thread, what a tokio worker
/// runs on, and what the libtest harness hands every `#[test]` — the smallest stack a smear parse
/// is realistically handed. A caller who deliberately spawns a smaller thread, or who wants
/// deeper documents on a larger one, is the reason this is configurable rather than a hard-coded
/// wall.
///
/// The 2 MiB column varies by shape, dialect, source backing and architecture, and **all of it
/// was measured** rather than assumed, because the binding cell is the worst one:
///
/// | cell (2 MiB, debug) | last depth that returns |
/// |---|---|
/// | GraphQL syntactic, inline fragments, `str`, aarch64 | 57 |
/// | GraphQL syntactic, inline fragments, `str`, x86_64 | 60 |
/// | GraphQLx syntactic, inline fragments, `str`, aarch64 | **53** |
/// | GraphQLx syntactic, generic angle brackets, `str`, aarch64 | **52** |
/// | GraphQL syntactic, inline fragments, `bytes::Bytes`, aarch64 | **51** |
/// | GraphQL lossless, inline fragments, `str`, aarch64 | 722 (12.7x cheaper per level) |
/// | GraphQL lossless, **the recovery bypass** `{ ) f {`, `str`, aarch64 | **673** |
/// | GraphQLx lossless, the same with `)` and with `>` alike, `str`, aarch64 | **671** |
///
/// The last two rows are the shape that walked past the first fix, and they are why the count
/// moved to the parse. They cost about the same per level as the row above them — a `field` frame
/// plus a `selection_set` frame — and needed 3.5 KB of input to reach 673, with the lexer's tally
/// reading **1** the whole way. Under the parse-side budget the same input refuses at 24 and
/// returns; the mapping did not move, because a level of that shape is a level of any other.
///
/// **The three lossless rows are re-measurements and the numbers moved.** They read 745 / 702 /
/// 700 when they were taken at `6f39cb9`, through a stand-in emitter, because `Sink::new` is
/// crate-private and no door existed that would install a budget of the caller's. The figures
/// above are bisected through the shipped `parse_document_with_limits` itself, at
/// `LosslessLimits::unlimited()`, so the real `cst::Sink` is in the loop — see [`HARD_MAX`], which
/// is derived from them and carries the full table and the method.
///
/// Five shapes were probed per door — inline fragments, field selections, list values, input
/// objects and list types, plus GraphQLx's generic angle brackets — and inline fragments are the
/// most expensive of them. x86_64 is not the worse architecture of the two measured. GraphQLx
/// costs 0.93x of GraphQL and a `bytes::Bytes` backing 0.89x, so the worst *shipped
/// configuration* — both together, which has no probe of its own — extrapolates to about **47**.
///
/// # Why 24, and not the release figure
///
/// **24 clears 47 by 1.96x.** That is deliberately the same margin tokora derived its own
/// parser-facing default at, and for the same asymmetry: a limit that is too low returns a clean,
/// catchable, documented diagnostic telling the caller to raise it, while a limit that is too high
/// aborts the process with no diagnostic at all and takes every other request on that process with
/// it. Only one of those is recoverable, so the default is set where every measured configuration
/// survives.
///
/// The release figures are far higher — the syntactic door costs 4.0 KiB per level optimised
/// rather than 34, and the lossless door 0.44 — and sizing against them is exactly the mistake
/// this number avoids. A debug build is what `cargo test` runs, in this workspace and in every
/// downstream that parses a document in a test, and a `SIGABRT` there does not fail a test: it
/// kills the runner. Worth recording, because it is the sharpest statement of how thin the
/// inherited ceiling was: in a *release* build on a 2 MiB thread, 500 levels of the syntactic door
/// need about 1.95 MiB of the 2 MiB available. It survived by roughly 2%, which is not a margin,
/// it is a coincidence.
///
/// # What it costs a real document
///
/// Nothing measurable. The deepest GraphQL document in this repository — 472 fixtures, including
/// real-world subgraph schemas and one named `bench_07_large_deep_nesting.graphql` — reaches
/// bracket depth **11**, and the next deepest 9. 24 leaves 2.2x headroom over the deepest document
/// anyone here has written, and clears the canonical introspection query with room to spare.
/// `smear/tests/nesting_depth.rs` re-derives that 11 from the fixtures themselves, so the claim
/// cannot go stale as the corpus grows.
///
/// # What it costs a caller who genuinely needs more
///
/// One call. Two places in this workspace already need it and say so: `validator_merge.rs`, whose
/// fixtures nest 200 levels to reach the *validator's* own `merge_depth` budget of 128, and one
/// GraphQLx generic-lookahead probe that nests 33 angle brackets. Both run under a raised ceiling
/// rather than a lowered claim — which also records the ordering, because at the shipped defaults
/// this ceiling binds long before the validator's does.
///
/// **At the lossless door, raising it past [`HARD_MAX`] does not reach the parse.** That is
/// smear's own wall now, derived against the same 2 MiB stack this constant is, and the doors
/// install `min(what the caller asked for, HARD_MAX)` as *the* recursion budget through
/// `cst::parse_lossless_with_context`. A request above it is answered with a positioned
/// diagnostic at [`HARD_MAX`] rather than with the depth that was asked for.
///
/// It used to be tokora's `PARSE_DEFAULT_DEPTH` that supplied that wall, by accident: the
/// lossless drivers built their own context and there was no hook for a caller to raise it, so a
/// lossless parse refused at `min(this number, whatever tokora defaulted to)`. That number is not
/// smear's, and upstream moved it twice inside one unreleased window — 64, then 16, then 32
/// — with nothing in this workspace failing to compile over any of it.
///
/// The *syntactic* door has no such cap and needs none, because the context there is the
/// consumer's: `Parser::with_parser_and_context` takes a `ParserContext::with_recursion_limiter`,
/// so a caller who raises the ceiling there has said something about their own stack by
/// construction.
pub const MAX_NESTING_DEPTH: usize = 24;

/// The worst depth in the table above that still returns: GraphQLx's syntactic door on a 2 MiB
/// debug thread (53), discounted by the 0.89x a `bytes::Bytes` backing costs.
///
/// A constant rather than only prose so that the assertion below can read it.
const WORST_MEASURED_BOUNDARY: usize = 47;

/// The deepest GraphQL document in this repository, over 472 fixtures.
const DEEPEST_DOCUMENT_IN_TREE: usize = 11;

// THE DERIVATION IS AN OBLIGATION, NOT A COMMENT, and it is checked at compile time rather than by
// a test for a reason that was measured rather than guessed: **a test cannot guard this**. Raising
// the constant makes every *other* nesting test parse deeper, and past the native boundary those
// tests do not go red — they abort the harness, which is the very failure #61 is about. Planting
// `MAX_NESTING_DEPTH = 200` killed `smear/tests/nesting_depth.rs` with `SIGABRT` before that
// file's own arithmetic check could run, because libtest gives no ordering between tests. A
// `const` assertion cannot be outrun: a ceiling raised past what the measurement supports fails to
// *build*.
//
// Raising it is therefore deliberately a two-line edit, and the second line is
// `WORST_MEASURED_BOUNDARY`. Moving that means re-running the bisection behind the table above —
// which is the point, because a number that can be raised without re-measuring has stopped being
// derived.
const _: () = assert!(
  MAX_NESTING_DEPTH * 19 <= WORST_MEASURED_BOUNDARY * 10,
  "MAX_NESTING_DEPTH leaves less than the 1.9x margin it was derived at under the worst measured \
   native-stack boundary. Raising it needs a new measurement, not a new constant."
);

// The other side of it, and the cheap direction to get wrong: a ceiling below what real documents
// need refuses real input while every gate in the tree still passes, because the fixtures are all
// far shallower than the ceiling. Nothing in the suite would notice a ceiling set at 4 until a
// consumer did.
const _: () = assert!(
  MAX_NESTING_DEPTH >= DEEPEST_DOCUMENT_IN_TREE * 2,
  "MAX_NESTING_DEPTH must keep the documents this repository actually contains clear of the \
   ceiling by at least 2x."
);

/// The greatest nesting ceiling a **lossless** parse will run under, whatever a caller asks for.
///
/// [`MAX_NESTING_DEPTH`] is the default; this is the wall. `LosslessLimits::parse_ceiling` clamps
/// the caller's request to it, and that number is installed as *the* recursion budget of the parse
/// through `cst::parse_lossless_with_context`, so a request above this one is answered with a
/// positioned diagnostic at this depth rather than with the depth that was asked for.
///
/// # Why a lossless-only constant, and why a wall at all
///
/// The two doors do not have the same shape of escape hatch. The **syntactic** door's context is
/// the consumer's — `Parser::with_parser_and_context` takes a
/// [`ParserContext::with_recursion_limiter`](tokora::ParserContext) — so a caller who raises the
/// ceiling there has, by construction, said something about the stack their own code runs on. The
/// **lossless** doors are smear's own six functions: they build the context, so a `usize` handed
/// to one of them is a request smear is obliged to either honour or refuse, and honouring an
/// arbitrary one is honouring a `SIGABRT`. Before `parse_lossless_with_context` existed the
/// accident of tokora's own parse-side default supplied the wall; it is a number smear does not
/// choose and upstream moved it twice inside one unreleased window (64, then 16, then 32), so it
/// was never a wall this workspace could point at.
///
/// # The measurement this is derived from
///
/// Bisected on this tree, one parse per process on an explicitly sized **2 MiB** thread, through
/// the shipped `parse_document_with_limits` at `LosslessLimits::unlimited()` — so the real
/// `cst::Sink` is in the loop and the native stack is the only wall left. Greatest depth that
/// returns before the next one aborts with `fatal runtime error: stack overflow`, aarch64, debug:
///
/// | shape (2 MiB, debug, `str`) | GraphQL | GraphQLx |
/// |---|---|---|
/// | recovery bypass `{ ) f {` | 673 | **671** |
/// | recovery bypass `{ > f {` | — | **671** |
/// | selection sets `{ f … }` | 673 | **671** |
/// | inline fragments | 722 | 720 |
/// | generic angle brackets `A< … >` | — | 850 |
/// | collection body `set { … }` | — | 832 |
/// | input objects `{k: … }` | 886 | 884 |
/// | list types `[ … ]Int` | 1083 | 1384 |
/// | list values `[ … ]` | 1303 | 1299 |
/// | set-or-map type `< … >` | — | 1873 |
///
/// **The shape list is the suite's own, not a guess.** It is the strongly-connected components of
/// each dialect's lossless call graph — the enumeration `smear/tests/nesting_depth.rs`'s
/// `a_refusal_is_one_diagnostic_at_every_cycle` derives its cells from — plus inline fragments and
/// both recovery-bypass closer families, which is every shape that file already probes.
///
/// The binding cell is **671**, at roughly 3.05 KiB a level, and three shapes reach it. Two
/// independent bisections agree to the level; at 672 the child prints `fatal runtime error: stack
/// overflow, aborting` and exits 134, at 671 it exits 0; and 2 MiB / 671 = 3.05 KiB a level lands
/// on the ~3.1 KiB al8n/tokora#297 measured, from outside this tree, for a lossless consumer's
/// descent.
///
/// # A stand-in emitter does not bound this in either direction
///
/// An earlier figure of **644** was taken with `Verbose` standing in for the sink and a no-op
/// `CstEmitter` beside it, because `Sink::new` is crate-private and no door existed that would
/// install a budget of the caller's. It was recorded as an *upper* bound on the real door, on the
/// reasoning that the real sink does strictly more work per level. **The real door measures 671 —
/// above it — so the reasoning was wrong, and wrong about the direction rather than the size.**
///
/// The arithmetic is what makes the mechanism legible. Over a 2 MiB thread, 644 levels is 3 256
/// bytes a level and 671 levels is 3 125, so the whole discrepancy is **131 bytes per level, 4.0%**
/// — the size of a handful of stack slots, not of anything an emitter does. What the sink does per
/// level is push an event into a growable buffer, which is **heap** traffic in a callee that has
/// returned before the production recurses; it is not on the frame that stacks up. A stand-in does
/// not remove that cost from the recursion, because the recursion never carried it.
///
/// What a stand-in *does* change is the **monomorphisation**. `Ctx` is a type parameter of every
/// production in the nest, and at `opt-level = 0` each MIR local is materialised as its own slot,
/// so substituting the emitter re-lays-out every frame in the recursion. 131 bytes a level is
/// exactly that scale. Which way it moves is a codegen fact about slot sizes, ordering and
/// padding, and **nothing about the emitter's workload predicts its sign** — which is the whole
/// error: a claim about *work* was read as a bound on *frame size*.
///
/// So a stand-in measurement is not a conservative reading of the real one; it is a different one,
/// and it bounds the real door in neither direction. The lesson is recorded here rather than in a
/// report because the next person tempted to substitute a type parameter and keep the number will
/// be reading this table.
///
/// # What the `Bytes` discount does not apply to
///
/// [`MAX_NESTING_DEPTH`]'s table discounts by 0.89x for a `bytes::Bytes` backing. **That discount
/// is not owed here and cannot be.** Every lossless entry point takes `&str` and pins
/// `Lexer<'_, str>`; there is no lossless door over any other backing to measure, so the two
/// columns above are not a sample of a wider space, they are the space. GraphQLx is measured
/// directly rather than extrapolated, and it is the worse of the two by 0.3%.
///
/// # Why 256 — the interval, then the pick inside it
///
/// Three constraints bound the admissible values, and none of them is a preference:
///
/// | bound | from | value |
/// |---|---|---|
/// | upper: the 1.9x margin | `floor(671 x 10 / 19)`, asserted below | **353** |
/// | upper: the suite's own deep cell | see below | **299** |
/// | lower: the documented raise | `MAX_NESTING_DEPTH * 4`, asserted below | **96** |
///
/// The second row is the one that is not obvious. `a_refusal_is_one_diagnostic_at_every_cycle`
/// nests **300** levels under a deliberately huge caller ceiling and asserts that the parse
/// *refuses*; a wall at or above 300 means it never refuses at all, so every cell in that file
/// becomes a clean parse and the property it pins evaporates without a single assertion firing.
///
/// So the admissible interval is **[96, 299]**, and 353 is refused by the second row before any
/// judgement is applied. Inside the interval the value is taken at the **top of the power-of-two
/// ladder**, 256, and the reason to go to the top rather than to 299 is the same asymmetry
/// [`MAX_NESTING_DEPTH`] is derived under — too low is a clean, positioned, catchable diagnostic,
/// too high is a process abort that takes every other request on the process with it — while the
/// reason not to go past 256 to 299 is that the remaining headroom buys a caller almost nothing
/// (17%) and costs the one thing this constant has no gate for:
///
/// **These exact rows drift, and nothing here re-measures them.** The table above replaces figures
/// of 745 / 702 / 700 recorded at `6f39cb9`; the same three shapes bisect to 722 / 673 / 671
/// today. No commit set out to move them — a lossless production's frame is grammar code and every
/// grammar edit reprices it — and the assertion below is checked against a *recorded* number, not
/// against the machine. At 353 a further 10% of that same drift breaches the 1.9x margin silently;
/// at 299 it takes 15%; at 256 it takes 27%. One architecture was bisected, too, where
/// [`MAX_NESTING_DEPTH`]'s two-architecture table found the other one *better* rather than worse —
/// a sample of size one about the direction, not a rule.
///
/// **256 was not chosen to match tokora.** It happens to equal
/// `RecursionLimiter::OPTIMIZED_PARSE_DEPTH`, and that is a coincidence of two derivations rather
/// than a shared source: that figure is a *release* number for tokora's own Pratt frames, this one
/// is a *debug* number for smear's lossless productions, and neither reads the other. Nor is it a
/// round number for its own sake — the ladder is the tie-break inside an interval the three rows
/// above had already narrowed to 204 values.
///
/// # What it costs a caller who has the stack
///
/// A caller on an 8 MiB thread can afford roughly four times 671 and is still clamped at 256,
/// which is a real cost and is stated rather than hidden. It is the same trade the default makes:
/// smear cannot see the stack its caller is on, and the number has to hold on the smallest stack a
/// parse is realistically handed — 2 MiB, which is what `std::thread::spawn`, a tokio worker and
/// the libtest harness each give. 256 is 10.7x the default and 23x the deepest document in this
/// repository, so nothing shipped is anywhere near it.
pub const HARD_MAX: usize = 256;

/// The worst cell in [`HARD_MAX`]'s table: GraphQLx's lossless door on a 2 MiB debug thread,
/// reached by the recovery bypass with either closer family and by plain selection sets alike.
///
/// A constant rather than only prose so that the assertion below can read it.
const WORST_LOSSLESS_BOUNDARY: usize = 671;

// The same obligation `MAX_NESTING_DEPTH` carries, for the same reason it is a `const` assertion
// rather than a test: past the native boundary a nesting test does not go red, it aborts the
// harness. Raising `HARD_MAX` is a two-line edit and the second line is `WORST_LOSSLESS_BOUNDARY`,
// which means re-running the bisection behind the table above.
const _: () = assert!(
  HARD_MAX * 19 <= WORST_LOSSLESS_BOUNDARY * 10,
  "HARD_MAX leaves less than the 1.9x margin it was derived at under the worst measured lossless \
   native-stack boundary. Raising it needs a new measurement, not a new constant."
);

// The other side of it. A wall at or below the default would make `with_max_nesting_depth` a knob
// that only ever lowers, and every gate in the tree would still pass because the fixtures are all
// far shallower than either number. `MAX_NESTING_DEPTH * 4` is the raise this workspace's own
// suite pins and the one `parse_document_with_limits` promises in prose — "a server on an 8 MiB
// main thread can afford roughly four times the depth" — so it is the floor that has a reader.
const _: () = assert!(
  HARD_MAX >= MAX_NESTING_DEPTH * 4,
  "HARD_MAX must leave room for the 4x raise the lossless doors document and the suite pins, or \
   `with_max_nesting_depth` clamps the one use case it exists for."
);

/// The budget a **syntactic** lex runs under: nesting depth, and nothing else.
///
/// This is [`Lexer::State`](tokora::Lexer::State) for
/// [`graphql::syntactic::SyntacticLexer`](crate::graphql::syntactic::SyntacticLexer) and its
/// GraphQLx twin, so [`Default`] is what `Parser::with_parser(…).parse_str(src)` seeds a parse
/// with — which is the whole reason the type exists rather than a bare
/// [`RecursionLimiter`]. See the module
/// header.
///
/// ```
/// use smear_lexer::limits::{MAX_NESTING_DEPTH, SyntacticLimits};
///
/// // What every unconfigured syntactic parse gets.
/// assert_eq!(SyntacticLimits::default().max_nesting_depth(), MAX_NESTING_DEPTH);
///
/// // What a caller on an 8 MiB thread with deeper documents asks for instead.
/// assert_eq!(SyntacticLimits::with_max_nesting_depth(96).max_nesting_depth(), 96);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SyntacticLimits(RecursionLimiter);

impl Default for SyntacticLimits {
  #[inline(always)]
  fn default() -> Self {
    Self::new()
  }
}

impl SyntacticLimits {
  /// A budget at smear's own [`MAX_NESTING_DEPTH`].
  #[inline(always)]
  pub const fn new() -> Self {
    Self(RecursionLimiter::with_limitation(MAX_NESTING_DEPTH))
  }

  /// A budget at `max` simultaneously open brackets.
  ///
  /// The number to pass is a function of the stack the parse will run on; see
  /// [`MAX_NESTING_DEPTH`] for the measured cost of one level.
  #[inline(always)]
  pub const fn with_max_nesting_depth(max: usize) -> Self {
    Self(RecursionLimiter::with_limitation(max))
  }

  /// A budget whose **lexer-side** tally never trips.
  ///
  /// The depth is still counted, so [`depth`](Self::depth) stays readable. At the syntactic door
  /// this removes the last check smear applies, so nothing then stands between a deeply nested
  /// document and the native stack: it is for a parse whose input is trusted or whose depth is
  /// bounded before it arrives.
  ///
  /// It is deliberately not spelled "unbounded". See [`MAX_NESTING_DEPTH`] for the second counter
  /// and where it does and does not still apply.
  #[inline(always)]
  pub const fn unlimited() -> Self {
    Self(RecursionLimiter::unlimited())
  }

  /// The ceiling this budget refuses past.
  #[inline(always)]
  pub const fn max_nesting_depth(&self) -> usize {
    self.0.limitation()
  }

  /// How many brackets are open right now.
  #[inline(always)]
  pub const fn depth(&self) -> usize {
    self.0.depth()
  }

  /// Opens one level.
  #[inline(always)]
  pub const fn increase(&mut self) {
    self.0.increase();
  }

  /// Closes one level.
  #[inline(always)]
  pub const fn decrease(&mut self) {
    self.0.decrease();
  }

  /// Whether the ceiling is still respected.
  #[inline(always)]
  pub const fn check(&self) -> Result<(), RecursionLimitExceeded> {
    self.0.check()
  }
}

impl From<RecursionLimiter> for SyntacticLimits {
  #[inline(always)]
  fn from(limiter: RecursionLimiter) -> Self {
    Self(limiter)
  }
}

impl From<SyntacticLimits> for RecursionLimiter {
  #[inline(always)]
  fn from(limits: SyntacticLimits) -> Self {
    limits.0
  }
}

impl State for SyntacticLimits {
  type Error = RecursionLimitExceeded;

  #[inline(always)]
  fn check(&self) -> Result<(), Self::Error> {
    Self::check(self)
  }
}

/// The budget a **lossless** parse runs under: nesting depth, token count, and the durable
/// produce-event ceiling.
///
/// This is [`Lexer::State`](tokora::Lexer::State) for
/// [`graphql::lossless::LosslessLexer`](crate::graphql::lossless::LosslessLexer) and its GraphQLx
/// twin — the Logos `Extras` those token grammars declare — so [`Default`] is what every
/// `parse_lossless` call seeds a parse with. See the module header.
///
/// The **token** half is left at tokora's unlimited default and is not part of issue #61's
/// decision: a token count is bounded by the input length, so unlike nesting depth it cannot
/// exhaust the native stack. It is carried here because the lossless lexer's `Extras` is the
/// combined tracker, and it is settable so that a caller who wants the lex to stop early can say
/// where.
///
/// **Neither of those two is a total-work bound**, and an earlier revision of this paragraph said
/// the token one was. The `Limiter` half of this type is the lexer's rewindable state, so a
/// recovery scan that finds nothing restores it and refunds every charge it made; what survives is
/// the count of lexemes that survived. [`max_tokens`](Self::max_tokens) states what that ceiling
/// therefore bounds.
///
/// # Two ceilings, and they count different things — smear issue #193
///
/// [`max_produce_events`](Self::max_produce_events) is the third number, and it is the one a
/// **defence** is sized against. It is not a second spelling of the token ceiling and the pair is
/// not a redundancy:
///
/// | | counts | lives in | a rollback | what it is for |
/// |---|---|---|---|---|
/// | [`max_tokens`](Self::max_tokens) | lexemes the scanner attempted, that a rewind kept | this type, i.e. `Lexer::State` | **refunds it** | *how much document do I want looked at* |
/// | [`max_produce_events`](Self::max_produce_events) | every item the lexer handed back, **a re-lex included** | tokora's `Input` | cannot reach it | *how much work may this document buy* |
///
/// Measured, because the gap is the whole reason both exist: `[ type ] ` repeated 2 000 times is
/// 12 000 lexical items, and it needs recovery at every one of them. Under
/// `with_max_tokens(12_000)` it parses to the end, and the parse produces **99 963** items —
/// 8.33× the ceiling, which is smear issue #168's scan allowance (`8n + 4096`) rather than
/// anything the caller asked for. Under `with_max_produce_events(12_000)` the same document is
/// **refused**, with four lexemes committed: the first recovery scan alone spends the budget. Both
/// answers are correct for the question their knob asks, and neither answers the other's.
///
/// So a caller who wants a document ceiling sets the first, a caller defending against a hostile
/// document sets the second, and a caller who wants both sets both. The durable one is checked in
/// front of the lexer, so where both are configured it is the one that fires.
///
/// The durable half reaches a parse only through a **lossless door**, which is the one place an
/// `InputContext` is built. A bare lexer driven through
/// [`Lexer::with_state`](tokora::Lexer::with_state) has no `Input` and therefore no durable tally
/// at all, so the number is inert there; `max_tokens` is the ceiling that surface honours, and
/// `smear/tests/lossless_ceiling_doors.rs` is the gate on it.
///
/// ```
/// use smear_lexer::limits::{LosslessLimits, MAX_NESTING_DEPTH};
///
/// assert_eq!(LosslessLimits::default().max_nesting_depth(), MAX_NESTING_DEPTH);
/// assert_eq!(LosslessLimits::default().max_tokens(), usize::MAX);
/// assert_eq!(LosslessLimits::default().max_produce_events(), usize::MAX);
///
/// // The two are independent, and setting one leaves the other alone.
/// let limits = LosslessLimits::default().with_max_tokens(1_000);
/// assert_eq!(limits.max_produce_events(), usize::MAX);
/// let limits = limits.with_max_produce_events(8_000);
/// assert_eq!(limits.max_tokens(), 1_000);
/// assert_eq!(limits.max_produce_events(), 8_000);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LosslessLimits {
  /// The lexer's own two tallies: the rewindable token count and the bracket depth.
  limiter: Limiter,
  /// The **durable** produce-event ceiling a lossless parse door installs on the input.
  ///
  /// Configuration only: nothing here ever charges it. The cell it configures is tokora's
  /// [`TokenBudgetTally`](tokora::input::TokenBudgetTally), which lives on the `Input` and which
  /// this type cannot reach — see [`max_produce_events`](Self::max_produce_events).
  produce_events: usize,
}

impl Default for LosslessLimits {
  #[inline(always)]
  fn default() -> Self {
    Self::new()
  }
}

impl LosslessLimits {
  /// A budget at smear's own [`MAX_NESTING_DEPTH`], with no token ceiling.
  #[inline(always)]
  pub const fn new() -> Self {
    Self {
      limiter: Limiter::with_trackers(
        TokenLimiter::new(),
        RecursionLimiter::with_limitation(MAX_NESTING_DEPTH),
      ),
      produce_events: usize::MAX,
    }
  }

  /// A budget at `max` simultaneously open brackets, with no token ceiling.
  #[inline(always)]
  pub const fn with_max_nesting_depth(max: usize) -> Self {
    Self {
      limiter: Limiter::with_trackers(TokenLimiter::new(), RecursionLimiter::with_limitation(max)),
      produce_events: usize::MAX,
    }
  }

  /// A budget whose lexer-side tally never trips on either axis.
  ///
  /// See [`SyntacticLimits::unlimited`] for what removing the nesting ceiling gives up — and note
  /// the difference at *this* door: only the **lexer's** tally is unlimited by this value. The
  /// parse still descends under [`parse_ceiling`](Self::parse_ceiling), which clamps to
  /// [`HARD_MAX`], so the effective bound here is [`HARD_MAX`] rather than no bound at all. That
  /// is what makes this value safe to hand a lossless door: it removes the cheap pre-filter and
  /// leaves the stack-safety wall standing.
  #[inline(always)]
  pub const fn unlimited() -> Self {
    Self {
      limiter: Limiter::with_trackers(TokenLimiter::new(), RecursionLimiter::unlimited()),
      produce_events: usize::MAX,
    }
  }

  /// The same budget with `max` as its token ceiling.
  ///
  /// See [`max_tokens`](Self::max_tokens) for what the number counts — lexemes attempted, tallied
  /// in state a failed recovery scan gives back — and
  /// [`with_max_produce_events`](Self::with_max_produce_events) for the durable ceiling beside it,
  /// which is the one a defence is sized against.
  #[inline(always)]
  pub const fn with_max_tokens(self, max: usize) -> Self {
    Self {
      limiter: Limiter::with_trackers(
        TokenLimiter::with_limitation(max),
        *self.limiter.recursion(),
      ),
      produce_events: self.produce_events,
    }
  }

  /// The nesting ceiling this budget refuses past.
  #[inline(always)]
  pub const fn max_nesting_depth(&self) -> usize {
    self.limiter.recursion().limitation()
  }

  /// The same budget with `max` as its **durable** produce-event ceiling — smear issue #193.
  ///
  /// See [`max_produce_events`](Self::max_produce_events) for what the number counts and why it is
  /// a different quantity from [`max_tokens`](Self::max_tokens) rather than a second spelling of
  /// it, and the type's own header for the table and the measurement that separate them.
  #[inline(always)]
  pub const fn with_max_produce_events(self, max: usize) -> Self {
    Self {
      limiter: self.limiter,
      produce_events: max,
    }
  }

  /// The **durable** ceiling on items this parse's lexer may produce, and the one no rollback
  /// refunds — smear issue #193.
  ///
  /// `usize::MAX` by default, which is tokora's own sentinel for *no ceiling*, so a caller who
  /// never names this gets exactly the parse they got before it existed.
  ///
  /// # Where it is enforced, and why that is the whole of it
  ///
  /// Each lossless door installs it as tokora's
  /// [`TokenBudget`](tokora::input::TokenBudget), through `lossless_context`. That budget is
  /// charged by tokora's driver at its single lexing chokepoint, **in front of the lexer**, into a
  /// cell that is not a [`Checkpoint`](tokora::input::Checkpoint) field, that the state re-key
  /// behind `set_state`/`state_mut` does not reach, and that has no public mutator. So no rollback
  /// can hand a charge back, and no grammar code can lower the count.
  ///
  /// [`max_tokens`](Self::max_tokens) cannot carry that guarantee and the reason is its
  /// *location*, not the direction of its increment: its tally is a field of this type, this type
  /// is [`Lexer::State`](tokora::Lexer::State), and lossless recovery's `sync_balanced` restores
  /// the lexer state on its no-match exit. smear issue #183 moved the increment to the right place
  /// inside the hook and could not move the hook.
  ///
  /// # What it counts, which is not the document
  ///
  /// Every item the lexer hands back — tokens, trivia and lexer errors alike, **and a re-lex
  /// again**. A region a rollback made the cache unable to keep is lexed twice and charged twice,
  /// so this is a bound on **work** and not a token census. tokora states the same thing from its
  /// side: *calibrate against produce-events, not against a token census.*
  ///
  /// Measured, and the number is the reason the two knobs are two: `[ type ] ` repeated 2 000
  /// times is 12 000 lexical items and needs recovery at each of them.
  /// `with_max_produce_events(12_000)` **refuses** it after four committed lexemes, because the
  /// first recovery scan alone spends the whole budget. The same document under
  /// `with_max_tokens(12_000)` parses to the end and costs 99 963 produce-events. A caller sizing
  /// this number against a document's lexeme count is sizing it against the wrong quantity; what
  /// it is for is *how much scanning may an untrusted document buy*, and #168's scan allowance —
  /// `8 * committed + 4096` — is the multiplier to size it with.
  ///
  /// # A refusal is terminal and it is one diagnostic
  ///
  /// tokora refuses the item **silently**: the refusal has no diagnostic channel, so the item is
  /// dropped, the poison boundary is latched, and a root loop's next peek answers end of input.
  /// smear mints the report — the dialect's `TokenBudgetExhausted`, at an empty span on the
  /// parse's committed end — in `lossless::depth::drain_unless_stopped`, which is the one frame
  /// every document root's entry production goes through. The tail is never read, so the refusal
  /// costs one diagnostic rather than one per remaining lexeme, and the tree still covers every
  /// byte with the unread tail tiled as a gap run.
  ///
  /// # It is inert on a bare lexer, deliberately
  ///
  /// A durable tally is a property of an `Input`, and
  /// [`Lexer::with_state`](tokora::Lexer::with_state) builds none. A caller driving the lexer
  /// directly is bounded by [`max_tokens`](Self::max_tokens) and by nothing here.
  #[inline(always)]
  pub const fn max_produce_events(&self) -> usize {
    self.produce_events
  }

  /// The token ceiling this budget refuses past — counted in lexemes the scanner **attempted**,
  /// and counted in **rewindable** lexer state, so what it bounds is one scan attempt rather than
  /// the parse. It is not a durable work budget;
  /// [`max_produce_events`](Self::max_produce_events) is the one that is, and the type's own
  /// header carries the table that tells the two apart.
  ///
  /// # What it bounds: the lexemes that survive, plus the attempt in flight
  ///
  /// Every lossless lexeme charges this tally before its rule runs, so the unit is the lexeme the
  /// scanner *tried* rather than the token it managed to produce. Rules reach the charge through
  /// the five hooks in `smear-lexer`'s `handlers` module; input **no** rule matches reaches it
  /// through `cst_default_error`, the logos `error(…)` callback, which is a charge route and not a
  /// rule. But the tally is a field of this type, this type is
  /// [`Lexer::State`](tokora::Lexer::State), and lossless recovery scans ahead through tokora's
  /// `InputRef::sync_balanced`, whose no-match exit **restores the lexer state** along with the
  /// position, the dedup watermark and the emissions. Every charge that scan made comes back with
  /// it, so a lexeme crossed by eight failed scans is charged once.
  ///
  /// What this ceiling refuses past is therefore *surviving lexemes plus the attempt in flight*,
  /// and what `with_max_tokens(n)` buys is a lex that stops one lexeme after its `n`th survivor —
  /// measured, GraphQL `-` repeated 4 000 times under `with_max_tokens(100)` reports exactly 101
  /// diagnostics and stops. That is a real ceiling on how much document a parse will look at. It
  /// is not a statement about how much scanning happened on the way there.
  ///
  /// # The durable number, which is the one a defence is sized against
  ///
  /// The count no rollback refunds is the input layer's — `TokenBudgetTally::spent`, every item
  /// tokora's driver ever handed back, a re-lex included. What bounds *that* is smear issue #168's
  /// scan allowance, `scan_allowance_exhausted` in `smear-parser/src/lossless/recover.rs`, which
  /// refuses to start a recovery scan once
  ///
  /// ```text
  /// spent > SCAN_ALLOWANCE_FACTOR * committed + SCAN_ALLOWANCE_FLOOR
  /// ```
  ///
  /// and whose `committed` is this tally, so it is at most `max_tokens + 1`. **A
  /// `with_max_tokens(n)` budget therefore permits on the order of `8n + 4096` produce-events
  /// rather than `n`**, and that module's docs are where the two constants and their derivation
  /// live. A caller who wants that number bounded outright, rather than bounded by a multiple,
  /// sets [`max_produce_events`](Self::max_produce_events) beside this one.
  ///
  /// Measured, because the multiplier is the whole of the difference: `[ type ] ` repeated 2 000
  /// times is 12 000 lexical items; `with_max_tokens(12_000)` **completes** — the tally is refunded
  /// by every failed scan, so it never gets past the 12 000 lexemes the document actually contains
  /// and nothing is ever refused on its account — and the parse records **99 963** produce-events.
  /// That ratio belongs to the two constants and not to that document: it is
  /// `SCAN_ALLOWANCE_FACTOR + SCAN_ALLOWANCE_FLOOR / n`.
  /// `max_tokens_does_not_bound_the_work_the_scan_allowance_does`, in
  /// `smear/tests/resync_allowance.rs`, is the pin on both sides of that ceiling, and what it runs
  /// is two shapes at two sizes in each dialect: **8.330 / 8.164** for `[ type ] ` at 12 000 and
  /// 24 000 items, **8.337 / 8.169** for `! ` at the same two. Those four are the readings
  /// something in this tree reproduces.
  ///
  /// Wider than that gate, and marked so because nothing reproduces it: 8.330 / 8.334 / 8.337 over
  /// #168's four census shapes at 12 000 items in both dialects, and
  /// 9.320 / 8.655 / 8.330 / 8.164 / 8.082 over `[ type ] ` at
  /// 3 000 / 6 000 / 12 000 / 24 000 / 48 000 items as the floor amortises. That is a campaign
  /// measurement, kept because re-deriving the bound wants it.
  ///
  /// **The durable cell is now reachable, and it is a knob of its own** — smear issue #193.
  /// [`max_produce_events`](Self::max_produce_events) is tokora's `TokenBudget`, installed by
  /// every lossless door through `lossless_context`, and it is the number to size a defence
  /// against. It is not a re-pointing of *this* one, and that choice was made on a measurement:
  /// the two count different quantities, and on the very document above they disagree by three
  /// orders of magnitude. `with_max_tokens(12_000)` parses `[ type ] ` x2000 to the end;
  /// `with_max_produce_events(12_000)` refuses it after **four** committed lexemes, because the
  /// first recovery scan alone spends the budget.
  ///
  /// Re-pointing this name at that cell was the alternative and it is worse in two ways that are
  /// not matters of taste. It would silently change the knob's **unit** from lexemes to
  /// produce-events, so an existing caller who sized it against a document would start getting
  /// truncated trees on exactly the malformed input a lossless parser exists for — and it would
  /// make one public name mean two different things at the two surfaces that honour it, because a
  /// bare [`Lexer::with_state`](tokora::Lexer::with_state) has no `Input` and therefore no durable
  /// tally, which `smear/tests/lossless_ceiling_doors.rs` pins. Two names for two quantities is
  /// the smaller hazard, and the type's own header carries the table that tells them apart.
  ///
  /// # The unit changed, and the direction it changed in
  ///
  /// Every lossless lexeme charges this tally before its rule runs. Until smear issue #183 the
  /// two hooks that wrap a fallible rule — `tt_hook_and_then` and `tt_hook_and_then_into_errors` —
  /// charged through `Result::inspect`, which runs on `Ok` alone, so a rule that failed cost
  /// nothing and four rules that can only fail (`.` and `..`, GraphQL's `-` and `+`) could never
  /// charge at all.
  ///
  /// A ceiling whose whole job is to bound what an untrusted document can cost therefore bounded
  /// **nothing** over malformed input, and it failed open on the cheaper document to write:
  /// measured at `with_max_tokens(100)`, 4 000 `!` truncated at 2 diagnostics while 4 000 `-`
  /// parsed to the end at 4 001. Both truncate now, at 2 and 101.
  ///
  /// For well-formed input the two units are identical, because a rule that succeeds was also a
  /// rule that was attempted. For malformed input the attempted count is the larger of the two, so
  /// an existing budget became **stricter** rather than looser — the safe direction for a defence,
  /// and the reason this is a documented change of meaning rather than a silent one. A caller
  /// parsing input that is expected to carry lexer errors and who sized a budget against the old
  /// unit should raise it by the number of bad lexemes it must tolerate.
  #[inline(always)]
  pub const fn max_tokens(&self) -> usize {
    self.limiter.token().limitation()
  }

  /// The recursion budget a lossless **parse** actually runs under: [`max_nesting_depth`] clamped
  /// to [`HARD_MAX`].
  ///
  /// This is the number each lossless door installs through `InputContext::with_recursion_limiter`
  /// and therefore the number `InputRef::descend` checks against — one budget, not two. The clamp
  /// is here, on the type that owns both constants, rather than at six doors, because a door that
  /// applied it differently would be a door with a different ceiling and nothing would say so.
  ///
  /// The lexer's own tally still refuses at [`max_nesting_depth`] unclamped, and that asymmetry is
  /// deliberate: the tally is a byte count with no native-stack cost behind it, so there is
  /// nothing for [`HARD_MAX`] to protect there, and clamping it would silently change what a
  /// caller asking for a very deep *lex* gets.
  ///
  /// [`max_nesting_depth`]: Self::max_nesting_depth
  #[inline(always)]
  pub const fn parse_ceiling(&self) -> usize {
    let requested = self.max_nesting_depth();
    if requested < HARD_MAX {
      requested
    } else {
      HARD_MAX
    }
  }

  /// How many brackets are open right now.
  #[inline(always)]
  pub const fn depth(&self) -> usize {
    self.limiter.recursion().depth()
  }

  /// The token half, for the handlers that step it.
  #[inline(always)]
  pub const fn token(&self) -> &TokenLimiter {
    self.limiter.token()
  }

  /// Counts one token.
  #[inline(always)]
  pub const fn increase_token(&mut self) {
    self.limiter.increase_token();
  }

  /// Opens one level.
  #[inline(always)]
  pub const fn increase_recursion(&mut self) {
    self.limiter.increase_recursion();
  }

  /// Closes one level.
  #[inline(always)]
  pub const fn decrease_recursion(&mut self) {
    self.limiter.decrease_recursion();
  }

  /// Whether both ceilings are still respected.
  #[inline(always)]
  pub fn check(&self) -> Result<(), LimitExceeded> {
    self.limiter.check()
  }
}

/// A [`Limiter`] widened into a lossless budget, with **no** durable ceiling.
///
/// [`max_produce_events`](LosslessLimits::max_produce_events) is seeded at `usize::MAX`, which is
/// the documented default and tokora's own sentinel for *no ceiling* — so this is a total
/// widening rather than a lossy one: a `Limiter` never held that number and this conversion
/// invents nothing.
///
/// # Its partner is gone, and that is smear issue #193 round 2
///
/// `From<LosslessLimits> for Limiter` used to sit under this impl. It **dropped**
/// `max_produce_events`, and together with the defaulting above that made
/// `LosslessLimits::from(Limiter::from(configured))` a silent conversion of a configured work
/// ceiling into no ceiling — the issue-193 amplification reintroduced, in one line, before any
/// budget-aware door runs. A lossy `From` in one direction and a defaulting one in the other is
/// *rebuild through a defaulting constructor*, which is the shape tokora's own #266/#300 had.
///
/// The removal is what makes the hazard unspellable rather than discouraged. The alternative —
/// keeping it and documenting the loss — is a sentence, and this workspace's most repeated defect
/// is a load-bearing claim that nothing compiles. `Limiter` is tokora's type and has exactly two
/// tracker slots, so it genuinely cannot carry the field; the direction that cannot carry it is
/// the direction that goes. Nothing in this workspace called either one.
///
/// This direction stays because it loses nothing and because a caller holding a bare `Limiter`
/// still needs a way in. To go the other way, read the parts that exist —
/// [`token`](LosslessLimits::token) and [`max_nesting_depth`](LosslessLimits::max_nesting_depth) —
/// so that what a rebuilt `Limiter` drops is written at the call site rather than hidden in an
/// `.into()`.
///
/// ```
/// use smear_lexer::{limits::LosslessLimits, tokora::state::tracker::Limiter};
///
/// // The widening compiles, and it seeds the documented default.
/// let widened = LosslessLimits::from(Limiter::default());
/// assert_eq!(widened.max_produce_events(), usize::MAX);
/// ```
///
/// ```compile_fail
/// use smear_lexer::{limits::LosslessLimits, tokora::state::tracker::Limiter};
///
/// // The round trip does not compile: there is no `From<LosslessLimits> for Limiter` to erase
/// // the ceiling with. THE CONTROL FOR THIS FENCE IS THE DOCTEST ABOVE — it uses the same two
/// // paths and the same constructor, so this one fails on the missing impl rather than on a
/// // spelling.
/// let configured = LosslessLimits::default().with_max_produce_events(8_000);
/// let round_tripped = LosslessLimits::from(Limiter::from(configured));
/// ```
impl From<Limiter> for LosslessLimits {
  #[inline(always)]
  fn from(limiter: Limiter) -> Self {
    Self {
      limiter,
      produce_events: usize::MAX,
    }
  }
}

impl State for LosslessLimits {
  type Error = LimitExceeded;

  #[inline(always)]
  fn check(&self) -> Result<(), Self::Error> {
    Self::check(self)
  }
}
