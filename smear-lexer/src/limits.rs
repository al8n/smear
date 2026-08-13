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
//! - **It is the cheaper check, and it fires first for well-formed input.** The lexer runs ahead
//!   of the parser, so on a document whose closers match, the tally reaches the ceiling one token
//!   before the parser reaches it — which is why the diagnostics for ordinary over-deep input are
//!   unchanged by the parser-side budget.
//! - **It is what latches tokora's poison boundary**, and that latch is what stops a
//!   machine-generated file from being lexed to the end after it has already proved it goes too
//!   deep. See `smear-parser/src/lossless/runner.rs` for why smear keeps the latch.

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
/// | GraphQL lossless, inline fragments, `str`, aarch64 | ~745 (13x cheaper per level) |
/// | GraphQL lossless, **the recovery bypass** `{ ) f {`, `str`, aarch64 | **702** |
/// | GraphQLx lossless, the same with `)` and with `>` alike, `str`, aarch64 | **700** |
///
/// The last two rows are the shape that walked past the first fix, and they are why the count
/// moved to the parse. They cost about the same per level as the row above them — a `field` frame
/// plus a `selection_set` frame — and needed 3.5 KB of input to reach 702, with the lexer's tally
/// reading **1** the whole way. Under the parse-side budget the same input refuses at 24 and
/// returns; the mapping did not move, because a level of that shape is a level of any other.
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
/// **At the lossless door, raising it past 64 does not reach the parse.** tokora's own
/// parse-side recursion budget is depth 64 and is installed by `InputContext::new`, which
/// `tokora::cst::parse_lossless` calls with no hook for a caller to raise — so
/// a lossless parse refuses at `min(this number, 64)`, cleanly and with a positioned diagnostic
/// rather than an abort. 64 is 5.8x the deepest document in this repository and 2.7x this
/// default, so nothing shipped is near it; the *syntactic* door has no such cap, because the
/// context there is the consumer's and `Parser::with_parser_and_context` takes a
/// `ParserContext::with_recursion_limiter`.
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

/// The budget a **lossless** lex runs under: nesting depth and token count.
///
/// This is [`Lexer::State`](tokora::Lexer::State) for
/// [`graphql::lossless::LosslessLexer`](crate::graphql::lossless::LosslessLexer) and its GraphQLx
/// twin — the Logos `Extras` those token grammars declare — so [`Default`] is what every
/// `parse_lossless` call seeds a parse with. See the module header.
///
/// The **token** half is left at tokora's unlimited default and is not part of issue #61's
/// decision: a token count is bounded by the input length, so unlike nesting depth it cannot
/// exhaust the native stack. It is carried here because the lossless lexer's `Extras` is the
/// combined tracker, and it is settable so that a caller who wants a total-work bound has one.
///
/// ```
/// use smear_lexer::limits::{LosslessLimits, MAX_NESTING_DEPTH};
///
/// assert_eq!(LosslessLimits::default().max_nesting_depth(), MAX_NESTING_DEPTH);
/// assert_eq!(LosslessLimits::default().max_tokens(), usize::MAX);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LosslessLimits(Limiter);

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
    Self(Limiter::with_trackers(
      TokenLimiter::new(),
      RecursionLimiter::with_limitation(MAX_NESTING_DEPTH),
    ))
  }

  /// A budget at `max` simultaneously open brackets, with no token ceiling.
  #[inline(always)]
  pub const fn with_max_nesting_depth(max: usize) -> Self {
    Self(Limiter::with_trackers(
      TokenLimiter::new(),
      RecursionLimiter::with_limitation(max),
    ))
  }

  /// A budget whose lexer-side tally never trips on either axis.
  ///
  /// See [`SyntacticLimits::unlimited`] for what removing the nesting ceiling gives up — and note
  /// the difference at *this* door: a lossless parse still descends through `smear-parser`'s
  /// `lossless::depth`, whose ceiling this value feeds, so the effective bound here is tokora's
  /// own parse-side budget of 64 rather than no bound at all. [`MAX_NESTING_DEPTH`] records why
  /// that cap cannot be raised from a lossless entry point.
  #[inline(always)]
  pub const fn unlimited() -> Self {
    Self(Limiter::with_trackers(
      TokenLimiter::new(),
      RecursionLimiter::unlimited(),
    ))
  }

  /// The same budget with `max` as its token ceiling.
  #[inline(always)]
  pub const fn with_max_tokens(self, max: usize) -> Self {
    Self(Limiter::with_trackers(
      TokenLimiter::with_limitation(max),
      *self.0.recursion(),
    ))
  }

  /// The nesting ceiling this budget refuses past.
  #[inline(always)]
  pub const fn max_nesting_depth(&self) -> usize {
    self.0.recursion().limitation()
  }

  /// The token ceiling this budget refuses past.
  #[inline(always)]
  pub const fn max_tokens(&self) -> usize {
    self.0.token().limitation()
  }

  /// How many brackets are open right now.
  #[inline(always)]
  pub const fn depth(&self) -> usize {
    self.0.recursion().depth()
  }

  /// The token half, for the handlers that step it.
  #[inline(always)]
  pub const fn token(&self) -> &TokenLimiter {
    self.0.token()
  }

  /// Counts one token.
  #[inline(always)]
  pub const fn increase_token(&mut self) {
    self.0.increase_token();
  }

  /// Opens one level.
  #[inline(always)]
  pub const fn increase_recursion(&mut self) {
    self.0.increase_recursion();
  }

  /// Closes one level.
  #[inline(always)]
  pub const fn decrease_recursion(&mut self) {
    self.0.decrease_recursion();
  }

  /// Whether both ceilings are still respected.
  #[inline(always)]
  pub fn check(&self) -> Result<(), LimitExceeded> {
    self.0.check()
  }
}

impl From<Limiter> for LosslessLimits {
  #[inline(always)]
  fn from(limiter: Limiter) -> Self {
    Self(limiter)
  }
}

impl From<LosslessLimits> for Limiter {
  #[inline(always)]
  fn from(limits: LosslessLimits) -> Self {
    limits.0
  }
}

impl State for LosslessLimits {
  type Error = LimitExceeded;

  #[inline(always)]
  fn check(&self) -> Result<(), Self::Error> {
    Self::check(self)
  }
}
