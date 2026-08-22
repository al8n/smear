//! The four nesting-depth cells that drive the **crate-private** verdict machinery.
//!
//! # Why these four are here and the other fourteen are not
//!
//! `smear/tests/nesting_depth.rs` is where the nesting ceiling is pinned, and it is an integration
//! test — a separate crate, which sees `pub` and nothing else. These four drive
//! [`root_turn`](crate::lossless::depth::root_turn), [`RootStop`](crate::lossless::depth::RootStop) and
//! [`drain_unless_stopped`](crate::lossless::depth::drain_unless_stopped) *directly*, because what they ask cannot
//! be asked through a shipped door:
//!
//! * which **term** of a root's stop is alone on which population, which needs an error type whose
//!   [`MaybeTerminal`](tokora::error::MaybeTerminal) arm is deliberately wrong and an emitter that
//!   rejects — neither of which any shipped door installs;
//! * and what the drain does with a failure **no turn classified**, which needs a root written by
//!   hand.
//!
//! Round 5 of smear PR #189 narrowed that machinery to `pub(crate)`, which put these four out of
//! reach from `tests/`. Moving them is the alternative to losing them, and
//! [`crate::lossless::depth`]'s module header carries why the narrowing happened. The other
//! fourteen cells reach the ceiling through the shipped doors — `parse_document`,
//! `parse_type_system_document`, `parse_executable_document` and the syntactic ones — which are
//! still public, name nothing that moved, and so stay where they are. `descend` went `pub(crate)`
//! in the same round, but no cell over there calls it; the four here are the only ones that did.
//!
//! # Why this is in the dialect and not beside the code it drives
//!
//! `lossless/depth/tests.rs` is where they landed first, and gate 6 —
//! `smear/tests/lossless_isolation.rs` — reddened on it. Every cell below pins **GraphQL's**
//! lexer, its `Lang` marker and `smear_lexer::limits`, so as a `mod tests` under `lossless/` they
//! put four `use crate::graphql::…` lines and some thirty dialect-typed signatures inside the
//! dialect-generic substrate. That is precisely what the Lego rule forbids, and the direction it
//! permits is this one: a dialect assembly may reach down into the substrate, so the four cells
//! sit in `graphql/` and call up. Nothing about what they measure changed with the address.
//!
//! # The two suites are one population
//!
//! Every plant recorded in either file is stated over all eighteen cells. A plant that reddens
//! here and nowhere else, or there and nowhere else, is what makes each cell about its own term;
//! splitting the file did not split the population and no count below was re-derived from the
//! smaller half.

/// A refusal is the error [`descend`](crate::lossless::depth::descend) returns, whichever
/// emission a rejecting emitter refuses and whatever value it substitutes — smear issue #169.
///
/// # Why this needs an emitter no shipped door installs
///
/// The lossless doors pin `tokora::emitter::Verbose`, which records everything and returns `Ok`
/// from every method, so no `Err` can arrive from the emitter and this path is unreachable through
/// `parse_document`. `descend` is nevertheless generic over any `ParseContext`, and the emitter is
/// the parameter the doors happen to pin rather than one the function constrains — so this cell
/// drives it with a rejecting one directly. Round 5's narrowing to `pub(crate)` removed the
/// *consumer* who could reach this path and did not remove the path: the day any door in this
/// crate installs an emitter that can say no, `descend`'s contract — the saved refusal is what
/// comes back — is what stops smear issue #169 from reopening, and this is where it is pinned.
///
/// # Two rejection sites, and the second one was found by review rather than by this test
///
/// The first version of this test rejected **`emit_lexer_error`** and accepted `emit_error`, which
/// covers the drain: the drain sat between the emit and the return and was propagated with `?`, so
/// `skip_while`'s fatal exit replaced the refusal (`Refusal` for `{ f }`, `LexerError` for
/// `{ f } ~ ~`). Removing the drain closed that and left the *same defect one call earlier*, on
/// the emission the test accepted: `emit_error(...)?` propagated whatever the emitter returned.
/// Tokora permits a rejecting emitter to return **any same-typed value**, not the payload it was
/// handed, so a host rejecting with an error-budget sentinel got the sentinel — and then, since
/// the sentinel is not the refusal, the entry drain ran over the tail and *its* rejection replaced
/// the sentinel in turn. Measured against that version: `Budget` for `{ f }`, and **`LexerError`
/// for `{ f } ~ ~` via the entry** — a third value, neither the refusal nor the host's.
///
/// So the axis is *which* emission is rejected and *what* it substitutes, and the assertion is the
/// same in every cell: the saved refusal comes back.
///
/// # Which value the refusal IS, and why this cell is where that shows
///
/// `Which::Recursion`, and it used to be `Which::Refusal`. `descend` no longer decides the
/// refusal — it takes the level through
/// [`InputRef::descend`](tokora::InputRef::descend) and hands back what tokora returns — so the
/// value that comes out is built by `From<RecursionLimitReached>`, while the value it *emits* is
/// still built by [`FromNestingLimit`]. Every shipped dialect lands both on the same variant
/// (smear PR #180), so no other test in this file can see the difference; `Which` maps them apart
/// on purpose, which is what makes this the cell that says which path is live.
///
/// The property is unchanged and still discriminating: `Budget` here would mean the rejecting
/// emitter's substituted value displaced the refusal, and `LexerError` would mean the entry drain
/// ran and displaced it. Both were measured before the #169 repair and both are still what a
/// regression looks like.
///
/// # What this still does not cover
///
/// * **A host whose own [`MaybeTerminal`](tokora::error::MaybeTerminal) arm is wrong.** `descend`
///   needs no cooperation — it drops the emit result — and since smear issue #178 the document
///   roots do not either: `root_turn` reads the input's resource-trip witness beside
///   `is_terminal()`, so a caller whose error type answers `false` for its own refusal still ends
///   the document. `each_term_of_a_roots_stop_is_alone_on_a_population` is the cell for that;
///   what is still uncovered here is a wrong arm on a **scanner** stop, which no published witness
///   sees. `Which` below deliberately answers `false` for `LexerError` so that a cell returning it
///   is visibly the drain having run.
/// * **The shipped doors.** `Verbose` cannot reject, so none of this is reachable through
///   `parse_document`; `a_refusal_is_one_diagnostic_at_every_cycle` and
///   `a_refusal_ends_every_document_root`, in `smear/tests/nesting_depth.rs`, are what cover that
///   path.
#[test]
fn a_refusal_is_the_error_returned_even_under_a_rejecting_emitter() {
  use crate::{
    graphql::{GraphQL, lossless::GraphqlLosslessLexer},
    lossless::depth::{FromNestingLimit, RootStop, descend, drain_unless_stopped},
  };
  use tokora::{
    Emitter, Lexer, ParserContext, SimpleSpan, Token,
    cache::DefaultCache,
    error::{MaybeTerminal, RecursionLimitReached},
    prelude::UnexpectedTokenOf,
    span::Spanned,
    state::recursion_tracker::RecursionLimiter,
  };

  type Lx<'inp> = GraphqlLosslessLexer<'inp, str>;
  // A `ParserContext` rather than the `(emitter, cache)` tuple, because the ceiling this cell
  // needs is now a property of the parse rather than an argument to `descend`: the tuple's
  // `ParseContext` impl seeds tokora's default budget and has no door to set one.
  type Ctx<'inp> = ParserContext<'inp, Lx<'inp>, Rejecting, DefaultCache<'inp, Lx<'inp>>, GraphQL>;

  /// Which error came back — the whole observation.
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  enum Which {
    Refusal,
    /// The host's own "I am at my diagnostic limit" value: a fatal stop that is **not** the
    /// payload it was handed.
    Budget,
    LexerError,
    Unexpected,
    Recursion,
  }

  impl FromNestingLimit for Which {
    fn nesting_limit_exceeded(_span: SimpleSpan, _attempted: usize, _limit: usize) -> Self {
      Which::Refusal
    }
  }

  impl MaybeTerminal for Which {
    fn is_terminal(&self) -> bool {
      // `LexerError` answers `false` ON PURPOSE. It is the value the drain produces, so leaving it
      // non-terminal keeps a cell that returns it a visible failure rather than one the predicate
      // absorbs.
      matches!(self, Which::Refusal | Which::Budget | Which::Recursion)
    }
  }

  impl<Lang: ?Sized> From<RecursionLimitReached<usize, Lang>> for Which {
    fn from(_: RecursionLimitReached<usize, Lang>) -> Self {
      Which::Recursion
    }
  }

  /// What the emitter does with the refusal's own `emit_error`.
  #[derive(Clone, Copy, Debug)]
  enum OnError {
    /// A collecting host: records it and carries on. `Verbose`'s behaviour.
    Accept,
    /// `Fatal`'s behaviour: reject, returning the payload it was handed.
    RejectWithPayload,
    /// The case the review found: reject, returning a value of the host's own choosing.
    RejectWithSentinel,
  }

  struct Rejecting {
    on_error: OnError,
    reject_lexer: bool,
  }

  impl<'inp, L: Lexer<'inp>> Emitter<'inp, L, GraphQL> for Rejecting {
    type Error = Which;

    fn emit_lexer_error(
      &mut self,
      _err: Spanned<<L::Token as Token<'inp>>::Error, L::Span>,
    ) -> Result<(), Self::Error> {
      if self.reject_lexer {
        Err(Which::LexerError)
      } else {
        Ok(())
      }
    }

    fn emit_error(&mut self, err: Spanned<Self::Error, L::Span>) -> Result<(), Self::Error> {
      match self.on_error {
        OnError::Accept => Ok(()),
        OnError::RejectWithPayload => Err(*err.data()),
        OnError::RejectWithSentinel => Err(Which::Budget),
      }
    }

    fn emit_unexpected_token(
      &mut self,
      _err: UnexpectedTokenOf<'inp, L, GraphQL>,
    ) -> Result<(), Self::Error> {
      Err(Which::Unexpected)
    }

    fn rewind(&mut self, _cursor: &tokora::input::Cursor<'inp, '_, L>, _checkpoint: u64) {}
  }

  // A recursion budget of **0**: the first descent is over budget, so the whole of `src` is the
  // tail a drain would cross. That is the shape, minus a 64-level nest that would prove nothing
  // extra. It used to be spelled `descend(inp, 0)`; the ceiling is the parse's own limiter now,
  // which is the same statement one layer down and is what makes the refusal below tokora's own
  // trip rather than a smear pre-check that agreed with it.
  /// The root the entry drain runs, and it classifies **nothing**: no `root_turn` call, so the
  /// slot `drain_unless_stopped` mints for it stays fresh, the ending is `Recoverable`, and
  /// `MaybeTerminal` is the only term left that can stop the tail from being read. That is the
  /// trait half this cell measures — the witness half has
  /// `each_term_of_a_roots_stop_is_alone_on_a_population` — and it is exactly the population
  /// `drain_unless_stopped`'s own note assigns to the trait: a failure that reached the drain by
  /// a path no `root_turn` classified.
  ///
  /// It used to be a hand-written `RootTurn::Recoverable(..)` handed straight to the drain. That
  /// spelling is gone — the variants do not build out of crate — and this is the same cell
  /// through the door that remains.
  fn refuse_without_classifying<'inp>(
    inp: &mut tokora::InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), Which> {
    descend::<Lx<'inp>, Ctx<'inp>, GraphQL>(inp).map(|_| ())
  }

  fn run<'inp>(src: &'inp str, via_entry: bool, on_error: OnError, reject_lexer: bool) -> Which {
    tokora::parse_with::<Lx<'inp>, str, _, (), Ctx<'inp>, GraphQL>(
      |inp: &mut tokora::InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>| {
        if via_entry {
          drain_unless_stopped(inp, refuse_without_classifying)
        } else {
          descend::<Lx<'inp>, Ctx<'inp>, GraphQL>(inp).map(|_| ())
        }
      },
      src,
      ParserContext::of(Rejecting {
        on_error,
        reject_lexer,
      })
      .with_recursion_limiter(RecursionLimiter::with_limitation(0)),
    )
    .expect_err("a budget of 0 refuses the first descent")
  }

  let mut cells = 0usize;
  for on_error in [
    OnError::Accept,
    OnError::RejectWithPayload,
    OnError::RejectWithSentinel,
  ] {
    for reject_lexer in [false, true] {
      // A clean tail and one that does not lex, because only the second makes a drain observable.
      for src in ["{ f }", "{ f } ~ ~", "~ { f }", "~"] {
        for via_entry in [false, true] {
          assert_eq!(
            run(src, via_entry, on_error, reject_lexer),
            Which::Recursion,
            "{src:?} (via_entry={via_entry}, on_error={on_error:?}, \
             reject_lexer={reject_lexer}): the saved refusal was displaced"
          );
          cells += 1;
        }
      }
    }
  }
  assert_eq!(cells, 3 * 2 * 4 * 2, "the cell set collapsed");
}

/// The two terms of a root's stop, each pinned on the population the other one misses.
///
/// # Why this cell set exists, and why nothing above it could ask this
///
/// A document root stops on `e.is_terminal() || inp.tripped_during_attempt(since)`, and smear
/// issue #178 is the second half: the first term is a **caller-implemented** answer, so while this
/// layer was publicly composable a consumer's own error type could answer `false` for its own
/// refusal and get the pre-#169 amplification back. That consumer is gone with round 5's
/// narrowing, and the term is not — a dialect added inside this crate implements the same arm, and
/// gets it wrong the same way. The witness is the repair, and the plant that
/// proves it is end-to-end — flip a dialect's `NestingLimitExceeded` arm to `false` and the three
/// refusal cells above stay green.
///
/// The **converse** plant is the one no cell here can see. Deleting `e.is_terminal()` from
/// [`root_turn`](crate::lossless::depth::root_turn) left every other nesting-depth cell green — measured, with this
/// cell removed, over the whole population these four and `smear/tests/nesting_depth.rs` make up —
/// and the reason is the one
/// `smear-parser/src/graphql/error/tests/terminal.rs` already records about the `Lexer` arm: with
/// an **accepting** emitter a lexer state trip latches tokora's poison boundary, the root loop's
/// next peek answers `None`, and the loop exits with no error to classify — so through the shipped
/// doors, which pin `Verbose`, no scanner stop ever reaches a catch arm as an `Err` at all. It
/// reaches one only for a consumer whose emitter **rejects**, which is the caller tokora's rule
/// tells to write a `MaybeTerminal` arm and the caller no in-tree parse is.
///
/// So the cells drive [`root_turn`](crate::lossless::depth::root_turn) directly, which is
/// public and is now the one place the five roots' arm lives.
///
/// # The three cells, and what each one is alone on
///
/// * **Scanner.** A real `smear-lexer` state trip, whose diagnostic the emitter rejects, arriving
///   on the parser's channel as an `Err`. `descend` is never called, so tokora's resource-trip
///   counter cannot have moved and the witness answers `false` by construction — the trait is the
///   only term that can see it. This is the population the withdrawn scanner witness would have
///   covered; it is withdrawn for cause (al8n/tokora#311: a document fully recovered through the
///   documented `set_state` path still reads as truncated), so "beside, not instead of" is not a
///   posture here but the only available answer.
/// * **Refusal.** A real descent trip under a budget of `0`, on an error type whose
///   [`MaybeTerminal`] arm answers **`false`** for it. That is #178's consumer, written out: the
///   witness is the only term that can see it.
/// * **Ordinary.** A plain syntax error, no trip, arm `false`. Neither term fires and the root
///   resynchronises — without it a `root_turn` that answered `EndsTheDocument` unconditionally
///   would pass the other two.
///
/// # The plants
///
/// Deleting `e.is_terminal()` turns the scanner cell from `Ends` into `Recoverable`; deleting
/// `inp.tripped_during_attempt(since)` turns the refusal cell from `Ends` into `Recoverable`; the
/// ordinary cell is `Recoverable` under both, which is what makes the other two readings about the
/// term and not about the function. All three were run.
#[test]
fn each_term_of_a_roots_stop_is_alone_on_a_population() {
  use core::cell::Cell as StdCell;

  use crate::{
    graphql::{GraphQL, lossless::GraphqlLosslessLexer},
    lossless::depth::{
      FromNestingLimit, RootStop, RootTurn, descend, drain_unless_stopped, root_turn,
    },
  };
  use tokora::{
    Emitter, Lexer, ParserContext, SimpleSpan, Token,
    cache::DefaultCache,
    error::{MaybeTerminal, RecursionLimitReached},
    prelude::UnexpectedTokenOf,
    span::Spanned,
    state::recursion_tracker::RecursionLimiter,
  };

  type Lx<'inp> = GraphqlLosslessLexer<'inp, str>;
  type Ctx<'inp> = ParserContext<'inp, Lx<'inp>, Rejecting, DefaultCache<'inp, Lx<'inp>>, GraphQL>;

  /// The consumer's error type — and the whole point is that **only `Scanner` is terminal**.
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  enum E {
    /// A scanner stop the emitter rejected onto the parser's channel.
    Scanner,
    /// An ordinary syntax error.
    Ordinary,
    /// A descent refusal, on an arm that answers **`false`** for it. This is the wrong answer a
    /// consumer is free to write, and #178 is the statement that a root must stop anyway.
    Refusal,
  }

  impl MaybeTerminal for E {
    fn is_terminal(&self) -> bool {
      matches!(self, E::Scanner)
    }
  }

  impl FromNestingLimit for E {
    fn nesting_limit_exceeded(_span: SimpleSpan, _attempted: usize, _limit: usize) -> Self {
      E::Refusal
    }
  }

  impl<Lang: ?Sized> From<RecursionLimitReached<usize, Lang>> for E {
    fn from(_: RecursionLimitReached<usize, Lang>) -> Self {
      E::Refusal
    }
  }

  /// Rejects every lexer diagnostic, which is the only shape that puts a scanner stop on the
  /// parser's channel. `emit_error` accepts, so the refusal cell below measures the witness rather
  /// than a rejection.
  struct Rejecting;

  impl<'inp, L: Lexer<'inp>> Emitter<'inp, L, GraphQL> for Rejecting {
    type Error = E;

    fn emit_lexer_error(
      &mut self,
      _err: Spanned<<L::Token as Token<'inp>>::Error, L::Span>,
    ) -> Result<(), Self::Error> {
      Err(E::Scanner)
    }

    fn emit_error(&mut self, _err: Spanned<Self::Error, L::Span>) -> Result<(), Self::Error> {
      Ok(())
    }

    fn emit_unexpected_token(
      &mut self,
      _err: UnexpectedTokenOf<'inp, L, GraphQL>,
    ) -> Result<(), Self::Error> {
      Ok(())
    }

    fn rewind(&mut self, _cursor: &tokora::input::Cursor<'inp, '_, L>, _checkpoint: u64) {}
  }

  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  enum Cell {
    Scanner,
    Ordinary,
    Refusal,
  }

  /// [`RootTurn`] flattened to something comparable — it carries a parse's error type and is not
  /// asked to be `Debug` or `PartialEq` for the sake of one test.
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  enum Verdict {
    Parsed,
    Ends(E),
    Recoverable(E),
  }

  // The `'inp` is NAMED, threaded from `src`: elided, it varies independently of the error type
  // and the closure `E0521`s — the same reason the driver macro names it.
  fn drive<'inp>(src: &'inp str, limit: usize, cell: Cell) -> Verdict {
    // The verdict leaves through a `Cell` rather than through the parse's own `Result`, because
    // the two failure arms both return `Err` and the whole question is *which* of them it was.
    let observed: StdCell<Option<Verdict>> = StdCell::new(None);

    let _ = tokora::parse_with::<Lx<'inp>, str, _, (), Ctx<'inp>, GraphQL>(
      |inp: &mut tokora::InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>| {
        // THE SLOT IS THE DRAIN'S, LENT FOR THIS ONE CALL. A cell that wants to read
        // `root_turn`'s verdict has to sit inside a `drain_unless_stopped`, because that is the
        // only frame that mints a `RootStop` — which is the shape the seal forces on every
        // consumer, this test included. Before smear PR #189's round 3 the slot was `RootStop::new()`
        // here, which is the minting door round 2 found.
        drain_unless_stopped(
          inp,
          |inp: &mut tokora::InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
           stop: &mut RootStop| {
            let turn = root_turn(
              inp,
              stop,
              |inp: &mut tokora::InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>| match cell {
                // A real scan. The lexer's own nesting tally trips on `src`, the emitter rejects
                // the diagnostic, and the rejection is what leaves this entry as an `Err`.
                // Nothing here descends, so tokora's resource-trip counter cannot have moved.
                Cell::Scanner => {
                  inp.skip_while(|_| true)?;
                  Ok(())
                }
                Cell::Ordinary => Err(E::Ordinary),
                // A real descent trip: the budget below is `0`, so the first descent is over it.
                Cell::Refusal => descend(inp).map(|_| ()),
              },
            );
            // The verdict and what the root returns are the same decision, and the root returns
            // its failure the way every shipped root does.
            let (verdict, out) = match turn {
              RootTurn::Parsed { .. } => (Verdict::Parsed, Ok(())),
              RootTurn::EndsTheDocument { error, .. } => (Verdict::Ends(error), Err(error)),
              RootTurn::Recoverable { error, .. } => (Verdict::Recoverable(error), Err(error)),
            };
            observed.set(Some(verdict));
            out
          },
        )
      },
      src,
      ParserContext::of(Rejecting).with_recursion_limiter(RecursionLimiter::with_limitation(limit)),
    );

    observed.get().expect("the root ran")
  }

  // Past `MAX_NESTING_DEPTH`, which is what the lexer's tally is seeded with by default, so the
  // scan below trips a real `smear_lexer::limits` budget rather than a manufactured one.
  let deep = "{".repeat(smear_lexer::limits::MAX_NESTING_DEPTH * 2);

  assert_eq!(
    drive(&deep, smear_lexer::limits::MAX_NESTING_DEPTH, Cell::Scanner),
    Verdict::Ends(E::Scanner),
    "a scanner stop ends the document, and `is_terminal()` is the only term that can see it — no \
     descent ran, so the trip witness answers `false` by construction"
  );
  assert_eq!(
    drive("{ f }", 0, Cell::Refusal),
    Verdict::Ends(E::Refusal),
    "a descent trip ends the document even though the caller's `MaybeTerminal` arm answers \
     `false` for it — smear issue #178"
  );
  assert_eq!(
    drive(
      "{ f }",
      smear_lexer::limits::MAX_NESTING_DEPTH,
      Cell::Ordinary
    ),
    Verdict::Recoverable(E::Ordinary),
    "an ordinary syntax error must still resynchronise, or the two readings above are about a \
     function that stops on everything"
  );
}

/// A trip **caught** in one entry does not silence the drain a *later* entry's ordinary failure
/// needs — smear PR #189.
///
/// # The defect this replays
///
/// `drain_unless_stopped` used to run the root itself and read
/// `inp.tripped_during_attempt(since)` with `since` taken **before the whole root**. tokora's
/// resource-trip counter is a monotone session fact, so that reading answers `true` for a root in
/// which *any* entry ever tripped — including one that tripped, was caught, and was recovered
/// from. Pair it with an ordinary failure later in the same root and both conjuncts hold: the
/// drain is skipped, the valid tail is left uncommitted, and every diagnostic that reading it
/// would have produced is never emitted.
///
/// That is the **false-stop** direction. It does not add diagnostics, it removes them, and it
/// truncates a document that was fine — the failure tokora's own note says survives testing and
/// points at nothing. `root` was a caller-supplied closure on a publicly reachable module, so the
/// root below is not a contrivance: it is a consumer that reports a too-deep entry and carries on,
/// which is what `RootTurn::EndsTheDocument` being a *value* rather than a `panic!` invites.
///
/// # Why the repair is structural
///
/// Nothing here needed a new measurement. `root_turn` had already decided, per entry, at the only
/// granularity where "did this failure end the document" means anything — and the arm threw the
/// answer away, after which the drain rebuilt it from a counter whose span is the whole root. The
/// classification is carried now, in `RootStop`, and a drain cannot be reached without one of
/// `RootTurn`'s three arms having been named.
///
/// The drain does read the counter again — smear PR #189 round 4, for the failures no `root_turn`
/// judged — and this test is the cell that says that reading is **scoped**: the slot latches that
/// an entry here already judged the caught trip, so the frame above subtracts it and drains.
/// Deleting the subtraction reddens the first cell below and nothing else.
///
/// # The three cells
///
/// * **Caught, then ordinary.** The defect. Before the repair: `0` tail diagnostics at every tail
///   length, the error `Ordinary` with its tail unread. After: `n`, one per malformed lexeme.
/// * **Ordinary alone.** The control that says the assertion is about the *caught trip* and not
///   about the drain having been disabled outright.
/// * **A refusal that is not caught.** The property the whole branch exists for, asserted from the
///   other side: the last turn ends the document, so the tail is never read and the refusal stays
///   one diagnostic. A repair that simply deleted the drain's stop condition would redden here.
#[test]
fn a_caught_trip_does_not_silence_a_later_failures_drain() {
  use core::cell::Cell as StdCell;

  use crate::{
    graphql::{GraphQL, lossless::GraphqlLosslessLexer},
    lossless::depth::{
      FromNestingLimit, RootStop, RootTurn, descend, drain_unless_stopped, root_turn,
    },
  };
  use tokora::{
    Emitter, InputRef, Lexer, ParserContext, SimpleSpan, Token,
    cache::DefaultCache,
    error::{MaybeTerminal, RecursionLimitReached},
    prelude::UnexpectedTokenOf,
    span::Spanned,
    state::recursion_tracker::RecursionLimiter,
  };

  type Lx<'inp> = GraphqlLosslessLexer<'inp, str>;
  type Ctx<'inp> = ParserContext<'inp, Lx<'inp>, Counting, DefaultCache<'inp, Lx<'inp>>, GraphQL>;

  thread_local! {
    /// One per `emit_lexer_error`, which is what a drain over a tail that does not lex produces.
    /// Thread-local rather than a borrow in the emitter because `ParserContext::of` takes the
    /// emitter by value and the harness runs each `#[test]` on its own thread.
    static TAIL_DIAGNOSTICS: StdCell<usize> = const { StdCell::new(0) };
  }

  /// The consumer's error type, with the arm #178's consumer gets wrong: a refusal answers
  /// **`false`** for `is_terminal`, so the witness is the only term that can classify it and the
  /// cells below measure the carried verdict rather than the trait.
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  enum E {
    Refusal,
    Ordinary,
  }

  impl MaybeTerminal for E {
    fn is_terminal(&self) -> bool {
      false
    }
  }

  impl FromNestingLimit for E {
    fn nesting_limit_exceeded(_span: SimpleSpan, _attempted: usize, _limit: usize) -> Self {
      E::Refusal
    }
  }

  impl<Lang: ?Sized> From<RecursionLimitReached<usize, Lang>> for E {
    fn from(_: RecursionLimitReached<usize, Lang>) -> Self {
      E::Refusal
    }
  }

  /// Accepts everything — a collecting host, `Verbose`'s posture — and counts the lexer
  /// diagnostics. Rejecting would stop the drain at the first bad lexeme and make the count
  /// answer a different question.
  struct Counting;

  impl<'inp, L: Lexer<'inp>> Emitter<'inp, L, GraphQL> for Counting {
    type Error = E;

    fn emit_lexer_error(
      &mut self,
      _err: Spanned<<L::Token as Token<'inp>>::Error, L::Span>,
    ) -> Result<(), Self::Error> {
      TAIL_DIAGNOSTICS.with(|n| n.set(n.get() + 1));
      Ok(())
    }

    fn emit_error(&mut self, _err: Spanned<Self::Error, L::Span>) -> Result<(), Self::Error> {
      Ok(())
    }

    fn emit_unexpected_token(
      &mut self,
      _err: UnexpectedTokenOf<'inp, L, GraphQL>,
    ) -> Result<(), Self::Error> {
      Ok(())
    }

    fn rewind(&mut self, _cursor: &tokora::input::Cursor<'inp, '_, L>, _checkpoint: u64) {}
  }

  /// One turn of the root loop below.
  #[derive(Debug, Clone, Copy)]
  enum Entry {
    /// A real descent trip the root **catches** and carries on from. The plausible consumer:
    /// "this definition is too deep, it is already reported, parse the next one".
    CaughtRefusal,
    /// A real descent trip the root propagates, the way every shipped root does.
    Refusal,
    /// An ordinary syntax error, already reported at the point of failure.
    Ordinary,
  }

  /// A document root a consumer could plausibly write: one `root_turn` per entry, matching its
  /// verdict, threading the slot its drain will read.
  fn root<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    stop: &mut RootStop,
    entries: &[Entry],
  ) -> Result<(), E> {
    for entry in entries {
      // The `..` in every pattern is `#[non_exhaustive]` on the variants: out of crate a verdict
      // still matches and still gets exhaustiveness checking, and no longer BUILDS — smear
      // PR #189, round 3. The variants are braced for the same reason: on a TUPLE variant the
      // attribute privates the constructor, and a tuple pattern out of crate resolves through it.
      match *entry {
        Entry::CaughtRefusal => {
          match root_turn(inp, stop, |inp: &mut InputRef<'inp, '_, _, _, _>| {
            descend(inp).map(|_| ())
          }) {
            RootTurn::Parsed { .. } => {}
            // CAUGHT AND CARRIED ON. Nothing in either shipped dialect does this; the public
            // generic layer lets a consumer, and `RootTurn::EndsTheDocument` is a value rather
            // than a stop the type system forces.
            RootTurn::EndsTheDocument { .. } | RootTurn::Recoverable { .. } => {}
          }
        }
        Entry::Refusal => {
          match root_turn(inp, stop, |inp: &mut InputRef<'inp, '_, _, _, _>| {
            descend(inp).map(|_| ())
          }) {
            RootTurn::Parsed { .. } => {}
            RootTurn::EndsTheDocument { error, .. } | RootTurn::Recoverable { error, .. } => {
              return Err(error);
            }
          }
        }
        Entry::Ordinary => {
          // `Err::<(), E>`: the `Parsed` arm below binds nothing now that the variants are
          // braced, so nothing else in this call fixes the entry's `T`.
          match root_turn(inp, stop, |_inp: &mut InputRef<'inp, '_, _, _, _>| {
            Err::<(), E>(E::Ordinary)
          }) {
            RootTurn::Parsed { .. } => {}
            RootTurn::EndsTheDocument { error, .. } | RootTurn::Recoverable { error, .. } => {
              return Err(error);
            }
          }
        }
      }
    }
    Ok(())
  }

  /// The root plus its drain, exactly as an `*_entry` production writes it — and the tail
  /// diagnostics that drain produced.
  ///
  /// A budget of `0` refuses the first descent, so `src` is entirely tail: no entry consumes
  /// anything, and what the drain crosses is the whole document.
  fn drive<'inp>(src: &'inp str, entries: &[Entry]) -> (Result<(), E>, usize) {
    TAIL_DIAGNOSTICS.with(|n| n.set(0));
    let out = tokora::parse_with::<Lx<'inp>, str, _, (), Ctx<'inp>, GraphQL>(
      |inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>| {
        // Exactly what an `*_entry` production writes: the drain runs the root, mints the slot
        // for it, and spends that slot against what that root returned. The three-step form this
        // used to spell — `RootStop::new()`, run, `stop.ending(out)` — is gone, and with it every
        // way to reach this drain with a verdict about some other root.
        drain_unless_stopped(
          inp,
          |inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>, stop: &mut RootStop| {
            root(inp, stop, entries)
          },
        )
      },
      src,
      ParserContext::of(Counting).with_recursion_limiter(RecursionLimiter::with_limitation(0)),
    );
    (out, TAIL_DIAGNOSTICS.with(StdCell::get))
  }

  // `~` does not lex in either dialect, so one per lexeme is what a drain over this tail reports.
  // `n = 0` is carried because it is the cell that would stay green under the defect and says so.
  for n in [0usize, 1, 4, 16] {
    let src = "~ ".repeat(n);

    assert_eq!(
      drive(&src, &[Entry::CaughtRefusal, Entry::Ordinary]),
      (Err(E::Ordinary), n),
      "n={n}: an entry that caught a refusal and carried on must not cost the NEXT entry's \
       ordinary failure its drain — before smear PR #189 this read 0 at every n, with the tail \
       left uncommitted and its diagnostics unemitted"
    );
    assert_eq!(
      drive(&src, &[Entry::Ordinary]),
      (Err(E::Ordinary), n),
      "n={n}: the control — the same ordinary failure with no earlier trip. If this ever \
       disagrees with the cell above, the reading there is about the drain and not about the \
       caught trip"
    );
    assert_eq!(
      drive(&src, &[Entry::Refusal]),
      (Err(E::Refusal), 0),
      "n={n}: a refusal that is NOT caught still ends the document, so nothing reads the tail \
       and the refusal stays one diagnostic — the `1 + n` amplification this branch closes. A \
       repair that deleted the drain's stop condition instead of scoping it reddens here"
    );
  }
}

/// A **nested** drain's stop is not reclassified as recoverable by the drain above it —
/// smear PR #189, round 4.
///
/// # The defect this replays
///
/// A root returns `Result` independently of the slot it is handed, so a downstream root can
/// return a nested `drain_unless_stopped` call and never touch its own slot. On a genuine descent
/// refusal whose caller-defined [`MaybeTerminal`] arm answers `false`, the inner drain classifies
/// the entry correctly, records `EndsTheDocument` in *its* slot and skips *its* drain — and the
/// `Err` it hands back carries none of that. The outer slot is untouched, so the frame above reads
/// the same failure as `Recoverable` and takes the malformed tail: `1 + n` diagnostics for a tail
/// of `n` invalid lexemes, which is the amplification smear issue #178 closes, back on the public
/// generic surface.
///
/// Every operation in that shape is legitimate. Nothing is forged, nothing is copied, no
/// `#[must_use]` value is dropped and no borrow escapes — which is why the round-3 seal, which is
/// about who may *mint* a verdict, does not reach it. What reaches it is the input's own trip
/// witness, read at the frame that is about to drain and scoped to what no `root_turn` in that
/// frame has already judged.
///
/// # The three cells
///
/// * **Nested, uncaught.** The defect. Before the repair: `n` tail diagnostics at every tail
///   length. After: `0` — the refusal is one diagnostic, which is what a stop means.
/// * **A tail with nothing to say.** `n = 0` is carried because it is the cell that stays green
///   under the defect and therefore says the others are about the tail rather than the shape.
/// * **The single-level control.** The same refusal through one drain, which was already right,
///   so a repair that stopped on every failure rather than on a tripped one is visible here as a
///   changed *error* rather than a changed count.
///
/// # Each term of the drain's reading is alone on a population
///
/// `drain_unless_stopped`'s stop condition for an unclassified failure is
/// `!a_classified_entry_saw_a_trip && tripped_during_attempt(since)`, and the two conjuncts are
/// pinned separately rather than argued for. Deleting the **subtraction** leaves a whole-root
/// reading, which is round 1: 17 pass and only
/// [`a_caught_trip_does_not_silence_a_later_failures_drain`] reddens. Deleting the **whole
/// reading** leaves round 3: 17 pass and only this test reddens. Neither deletion moves any other
/// cell. The eighteen are the fourteen in `smear/tests/nesting_depth.rs` and these four; the split
/// is smear PR #189's round 5 and the counts are unchanged by it.
#[test]
fn a_nested_drains_stop_is_not_reclassified_by_the_drain_above_it() {
  use core::cell::Cell as StdCell;

  use crate::{
    graphql::{GraphQL, lossless::GraphqlLosslessLexer},
    lossless::depth::{
      FromNestingLimit, RootStop, RootTurn, descend, drain_unless_stopped, root_turn,
    },
  };
  use tokora::{
    Emitter, InputRef, Lexer, ParserContext, SimpleSpan, Token,
    cache::DefaultCache,
    error::{MaybeTerminal, RecursionLimitReached},
    prelude::UnexpectedTokenOf,
    span::Spanned,
    state::recursion_tracker::RecursionLimiter,
  };

  type Lx<'inp> = GraphqlLosslessLexer<'inp, str>;
  type Ctx<'inp> = ParserContext<'inp, Lx<'inp>, Counting, DefaultCache<'inp, Lx<'inp>>, GraphQL>;

  thread_local! {
    /// One per `emit_lexer_error` — what a drain over a tail that does not lex produces.
    static TAIL_DIAGNOSTICS: StdCell<usize> = const { StdCell::new(0) };
  }

  /// The consumer's error type, with #178's arm: a refusal answers **`false`** for
  /// `is_terminal`, so the trait cannot classify it and the witness is the only term left.
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  enum E {
    Refusal,
  }

  impl MaybeTerminal for E {
    fn is_terminal(&self) -> bool {
      false
    }
  }

  impl FromNestingLimit for E {
    fn nesting_limit_exceeded(_span: SimpleSpan, _attempted: usize, _limit: usize) -> Self {
      E::Refusal
    }
  }

  impl<Lang: ?Sized> From<RecursionLimitReached<usize, Lang>> for E {
    fn from(_: RecursionLimitReached<usize, Lang>) -> Self {
      E::Refusal
    }
  }

  /// Accepts everything and counts the lexer diagnostics, for
  /// `a_caught_trip_does_not_silence_a_later_failures_drain`'s reason: rejecting would stop the
  /// drain at the first bad lexeme and make the count answer a different question.
  struct Counting;

  impl<'inp, L: Lexer<'inp>> Emitter<'inp, L, GraphQL> for Counting {
    type Error = E;

    fn emit_lexer_error(
      &mut self,
      _err: Spanned<<L::Token as Token<'inp>>::Error, L::Span>,
    ) -> Result<(), Self::Error> {
      TAIL_DIAGNOSTICS.with(|n| n.set(n.get() + 1));
      Ok(())
    }

    fn emit_error(&mut self, _err: Spanned<Self::Error, L::Span>) -> Result<(), Self::Error> {
      Ok(())
    }

    fn emit_unexpected_token(
      &mut self,
      _err: UnexpectedTokenOf<'inp, L, GraphQL>,
    ) -> Result<(), Self::Error> {
      Ok(())
    }

    fn rewind(&mut self, _cursor: &tokora::input::Cursor<'inp, '_, L>, _checkpoint: u64) {}
  }

  /// The inner root: one classified entry, and it is a genuine descent refusal. Its own drain is
  /// correctly skipped — the verdict this cell is about exists and is right.
  fn inner<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    stop: &mut RootStop,
  ) -> Result<(), E> {
    match root_turn(inp, stop, |inp: &mut InputRef<'inp, '_, _, _, _>| {
      descend(inp).map(|_| ())
    }) {
      RootTurn::Parsed { .. } => Ok(()),
      RootTurn::EndsTheDocument { error, .. } | RootTurn::Recoverable { error, .. } => Err(error),
    }
  }

  /// The outer root: it returns the nested drain's `Result` and touches its own slot not at all.
  /// Nothing here is a misuse — a `Root` is a `fn(&mut Input, &mut RootStop) -> Result<…>` and
  /// this is one.
  fn outer<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), E> {
    drain_unless_stopped(inp, inner)
  }

  /// One drain over `root`, and the tail diagnostics that drain produced. A budget of `0` refuses
  /// the first descent, so `src` is entirely tail.
  fn drive<'inp, R>(src: &'inp str, root: R) -> (Result<(), E>, usize)
  where
    R:
      FnOnce(&mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>, &mut RootStop) -> Result<(), E>,
  {
    TAIL_DIAGNOSTICS.with(|n| n.set(0));
    let mut root = Some(root);
    let out = tokora::parse_with::<Lx<'inp>, str, _, (), Ctx<'inp>, GraphQL>(
      |inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>| {
        drain_unless_stopped(inp, root.take().expect("the production runs once"))
      },
      src,
      ParserContext::of(Counting).with_recursion_limiter(RecursionLimiter::with_limitation(0)),
    );
    (out, TAIL_DIAGNOSTICS.with(StdCell::get))
  }

  // `~` does not lex in either dialect, so one per lexeme is what a drain over this tail reports.
  for n in [0usize, 1, 4, 16] {
    let src = "~ ".repeat(n);

    assert_eq!(
      drive(&src, outer),
      (Err(E::Refusal), 0),
      "n={n}: the inner drain classified this refusal as ending the document and skipped its own \
       drain; the frame above must not read the same failure as recoverable and take the tail. \
       Before smear PR #189 round 4 this read n at every n — the `1 + n` amplification, through \
       a composition in which every operation is legitimate"
    );
    assert_eq!(
      drive(&src, inner),
      (Err(E::Refusal), 0),
      "n={n}: the single-level control. One drain over the same refusal was already right, so a \
       repair that stopped on every failure rather than on a tripped one shows up here as a \
       changed error rather than a changed count"
    );
  }
}
