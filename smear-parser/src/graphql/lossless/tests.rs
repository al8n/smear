//! The five nesting-depth cells that drive the **crate-private** verdict machinery.
//!
//! # Why these are here and the other fourteen are not
//!
//! `smear/tests/nesting_depth.rs` is where the nesting ceiling is pinned, and it is an integration
//! test — a separate crate, which sees `pub` and nothing else. These cells drive
//! [`root_turn`](crate::lossless::depth::root_turn), [`RootStop`](crate::lossless::depth::RootStop),
//! [`drain_unless_stopped`](crate::lossless::depth::drain_unless_stopped) and
//! [`drain_unless_terminal`](crate::lossless::depth::drain_unless_terminal) *directly*, because
//! what they ask cannot be asked through a shipped door:
//!
//! * which **term** of a root's stop is alone on which population, which needs an error type whose
//!   [`MaybeTerminal`](tokora::error::MaybeTerminal) arm is deliberately wrong and an emitter that
//!   rejects — neither of which any shipped door installs;
//! * and what the drain does with a failure **no turn classified**, which needs a root written by
//!   hand.
//!
//! Round 5 of smear PR #189 narrowed that machinery to `pub(crate)`, which put four of them out of
//! reach from `tests/`. Moving those four is the alternative to losing them, and
//! [`crate::lossless::depth`]'s module header carries why the narrowing happened. The other
//! fourteen cells reach the ceiling through the shipped doors — `parse_document`,
//! `parse_type_system_document`, `parse_executable_document` and the syntactic ones — which are
//! still public, name nothing that moved, and so stay where they are. `descend` went `pub(crate)`
//! in the same round, but no cell over there calls it; the four that moved are the only ones that
//! did.
//!
//! The fifth, `a_terminal_failure_no_turn_classified_stops_the_drain_on_the_trait_alone`, was
//! **born here** in round 6 rather than moved. Round 5's rewrite of
//! `a_refusal_is_the_error_returned_even_under_a_rejecting_emitter` sent its entry cells through
//! `drain_unless_stopped`, whose residual reading of the trip witness answers first — so those
//! cells moved onto the opposite term from the one their note names and
//! [`MaybeTerminal`](tokora::error::MaybeTerminal)'s own population was left with no cell in the
//! workspace. That cell's note carries the measurement.
//!
//! # Why this is in the dialect and not beside the code it drives
//!
//! `lossless/depth/tests.rs` is where they landed first, and gate 6 —
//! `smear/tests/lossless_isolation.rs` — reddened on it. Every cell below pins **GraphQL's**
//! lexer, its `Lang` marker and `smear_lexer::limits`, so as a `mod tests` under `lossless/` they
//! put a `use crate::graphql::…` line each and some thirty dialect-typed signatures inside the
//! dialect-generic substrate. That is precisely what the Lego rule forbids, and the direction it
//! permits is this one: a dialect assembly may reach down into the substrate, so the cells sit in
//! `graphql/` and call up. The address is not what changed any of them: what changed the first
//! cell was round 5's rewrite of the door it calls, and its own note says so.
//!
//! # The two suites are one population
//!
//! Every plant recorded in either file is stated over all nineteen cells — the fourteen there and
//! these five. A plant that reddens here and nowhere else, or there and nowhere else, is what
//! makes each cell about its own term; splitting the file did not split the population and no
//! count below was re-derived from the smaller half.

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
    lossless::depth::{FromNestingLimit, FromTokenBudget, RootStop, descend, drain_unless_stopped},
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

  // NEVER CALLED, AND THAT IS PROVABLE HERE RATHER THAN HOPED. `drain_unless_stopped` mints a
  // token-budget refusal only off the input's own `refused_an_item` bit, and these cells drive a
  // `ParserContext` whose token budget is tokora's default — `TokenBudget::unlimited()`, whose
  // gate excludes its own `usize::MAX` sentinel, so no item is ever refused. A variant for a value
  // this cell cannot produce would widen a `MaybeTerminal` arm set whose subject is the descent
  // axis.
  impl FromTokenBudget for Which {
    fn token_budget_exhausted(_span: SimpleSpan, _spent: usize, _limit: usize) -> Self {
      unreachable!("this cell configures no token budget, so nothing can refuse an item")
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
  /// slot `drain_unless_stopped` mints for it stays fresh and the ending is `Recoverable`. It is
  /// the population `drain_unless_stopped`'s own note assigns to that arm: a failure that reached
  /// the drain by a path no `root_turn` classified.
  ///
  /// **Which term stops it is the witness, and that changed in round 5.** This root *descends*,
  /// inside the drain's own baseline window and outside every `root_turn`, so `unjudged_trip` is
  /// true and `drain_unless_stopped` returns on its residual arm before `drain_unless_terminal` is
  /// consulted at all. Round 4's version of this function was handed to `drain_unless_terminal`
  /// directly, where `Which::Recursion.is_terminal()` was the only thing standing between the
  /// refusal and a tail drain — so what this cell pins is one term further out than it was, and
  /// the trait half is now
  /// `a_terminal_failure_no_turn_classified_stops_the_drain_on_the_trait_alone`. The witness half
  /// at the root's own scale is `each_term_of_a_roots_stop_is_alone_on_a_population`.
  ///
  /// What this cell measures is the same either way, and it is the assertion below rather than the
  /// term: whichever term stops the tail from being read, the value that comes back is the saved
  /// refusal.
  ///
  /// It used to be a hand-written `RootTurn::Recoverable(..)` handed straight to the drain. That
  /// spelling is gone, and the operative reason is round 3's **signature** rather than round 5's
  /// visibility: `drain_unless_stopped` takes the root, not a verdict about one, so there is no
  /// argument position a written-out variant could occupy. In crate — which this file now is —
  /// the variants build perfectly well.
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
/// crate-private and is now the one place the six roots' arm lives.
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
      FromNestingLimit, FromTokenBudget, RootStop, RootTurn, descend, drain_unless_stopped,
      root_turn,
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

  // NEVER CALLED, AND THAT IS PROVABLE HERE RATHER THAN HOPED. `drain_unless_stopped` mints a
  // token-budget refusal only off the input's own `refused_an_item` bit, and these cells drive a
  // `ParserContext` whose token budget is tokora's default — `TokenBudget::unlimited()`, whose
  // gate excludes its own `usize::MAX` sentinel, so no item is ever refused. A variant for a value
  // this cell cannot produce would widen a `MaybeTerminal` arm set whose subject is the descent
  // axis.
  impl FromTokenBudget for E {
    fn token_budget_exhausted(_span: SimpleSpan, _spent: usize, _limit: usize) -> Self {
      unreachable!("this cell configures no token budget, so nothing can refuse an item")
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
      FromNestingLimit, FromTokenBudget, RootStop, RootTurn, descend, drain_unless_stopped,
      root_turn,
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

  // NEVER CALLED, AND THAT IS PROVABLE HERE RATHER THAN HOPED. `drain_unless_stopped` mints a
  // token-budget refusal only off the input's own `refused_an_item` bit, and these cells drive a
  // `ParserContext` whose token budget is tokora's default — `TokenBudget::unlimited()`, whose
  // gate excludes its own `usize::MAX` sentinel, so no item is ever refused. A variant for a value
  // this cell cannot produce would widen a `MaybeTerminal` arm set whose subject is the descent
  // axis.
  impl FromTokenBudget for E {
    fn token_budget_exhausted(_span: SimpleSpan, _spent: usize, _limit: usize) -> Self {
      unreachable!("this cell configures no token budget, so nothing can refuse an item")
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
      // The `..` in every pattern below is round 3's shape outliving its reason. Then these
      // variants were `#[non_exhaustive]` and this cell was out of crate, so the attribute left
      // the pattern working and removed the constructor. Round 5 deleted the attribute — on a
      // `pub(crate)` enum it is inert, which `depth.rs` records as deliberate — and moved the cell
      // in crate, so neither half is load-bearing here any more and the `..` is habit. What still
      // is load-bearing is the BRACES: six match sites read these arms and a named field costs one
      // word at each.
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
/// reading, which is round 1: 18 pass and only
/// [`a_caught_trip_does_not_silence_a_later_failures_drain`] reddens. Deleting the **whole
/// reading** leaves round 3: 18 pass and only this test reddens. Neither deletion moves any other
/// cell. The nineteen are the fourteen in `smear/tests/nesting_depth.rs` and the five here; the
/// split is smear PR #189's round 5, the fifth cell is round 6, and both deletions were re-run
/// over the larger population rather than carried forward.
#[test]
fn a_nested_drains_stop_is_not_reclassified_by_the_drain_above_it() {
  use core::cell::Cell as StdCell;

  use crate::{
    graphql::{GraphQL, lossless::GraphqlLosslessLexer},
    lossless::depth::{
      FromNestingLimit, FromTokenBudget, RootStop, RootTurn, descend, drain_unless_stopped,
      root_turn,
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

  // NEVER CALLED, AND THAT IS PROVABLE HERE RATHER THAN HOPED. `drain_unless_stopped` mints a
  // token-budget refusal only off the input's own `refused_an_item` bit, and these cells drive a
  // `ParserContext` whose token budget is tokora's default — `TokenBudget::unlimited()`, whose
  // gate excludes its own `usize::MAX` sentinel, so no item is ever refused. A variant for a value
  // this cell cannot produce would widen a `MaybeTerminal` arm set whose subject is the descent
  // axis.
  impl FromTokenBudget for E {
    fn token_budget_exhausted(_span: SimpleSpan, _spent: usize, _limit: usize) -> Self {
      unreachable!("this cell configures no token budget, so nothing can refuse an item")
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

/// A **terminal** failure no `root_turn` classified still stops the drain, and the trait is the
/// only term that can do it — smear PR #189, round 6.
///
/// # Why this cell exists, and what was measured before it did
///
/// `a_refusal_is_the_error_returned_even_under_a_rejecting_emitter`'s entry cells used to hand
/// `drain_unless_terminal` a `Result` directly, so `MaybeTerminal` was the only term standing
/// between a refusal and a tail drain and those cells were the trait's pin. They go through
/// `drain_unless_stopped` now — the door that remains — and the root they hand it **descends**,
/// inside that frame's own baseline window and outside every `root_turn`. So `unjudged_trip` is
/// true there and `drain_unless_stopped`'s residual arm returns before `drain_unless_terminal` is
/// consulted at all: those cells moved onto the *witness* term, which is the opposite term from
/// the one they were written for, and the trait's own population was left with no cell anywhere.
///
/// Measured before this cell existed, with `drain_unless_terminal`'s `is_terminal` early return
/// deleted: 363/363 `smear-parser --lib`, 14/14 `nesting_depth`, 16/16 `resync_allowance`, 5/5
/// `lossless_isolation` — the whole claimed guard population green, and
/// `drain_unless_stopped`'s "`MaybeTerminal` still runs on both remaining arms" note enforced by
/// nothing. A refactor dropping that early return would have shipped green and reopened the
/// `1 + n` amplification for exactly the failure the trait is alone on.
///
/// # The population, and why no other cell reaches it
///
/// Unclassified, **untripped**, terminal. A trip is what the witness sees, so a failure the
/// witness can see is not this cell's; what is left is a stop that moved no descent counter and
/// that no turn judged — a **scanner** stop the emitter rejected, reaching an entry drain from a
/// place no `root_turn` wraps. `drain_unless_stopped` lists those places itself: the shipped
/// loops' `peek_kind(inp)?`, `report_unexpected` and `resync_to_definition(inp)?` all return `Err`
/// without going through a turn. The root below is that shape reduced to its one fact — it returns
/// the failure and classifies nothing — and nothing in it descends, so the witness answers `false`
/// by construction and the trait is what is left.
///
/// # The two vias, and the control
///
/// * **`Via::Terminality`** hands the primitive a terminal `Err` directly. That is
///   `drain_unless_terminal`'s own contract, one frame below the door.
/// * **`Via::Entry`** reaches it through `drain_unless_stopped`, which is the claim at issue: the
///   trait runs on the `Recoverable` arm, after the residual reading has declined to stop.
/// * **`E::Ordinary`** is the control in both vias. It is not terminal, so the drain runs and the
///   tail costs one lexer diagnostic per malformed lexeme — `n` at every `n`, with the plant and
///   without it. Without that reading a terminal cell at `0` would be equally satisfied by a drain
///   that never runs at all.
///
/// `n = 0` is carried for this file's usual reason: it is the cell that stays green under the
/// plant, which is what makes the others about the tail rather than about the shape.
///
/// # The plant
///
/// Deleting the `is_terminal` early return turns both `E::Scanner` readings from `0` tail
/// diagnostics into `n` at n = 1, 4 and 16, in both vias, and moves no other cell in either file.
#[test]
fn a_terminal_failure_no_turn_classified_stops_the_drain_on_the_trait_alone() {
  use core::cell::Cell as StdCell;

  use crate::{
    graphql::{GraphQL, lossless::GraphqlLosslessLexer},
    lossless::depth::{FromTokenBudget, RootStop, drain_unless_stopped, drain_unless_terminal},
  };
  use tokora::{
    Emitter, InputRef, Lexer, ParserContext, SimpleSpan, Token, cache::DefaultCache,
    error::MaybeTerminal, prelude::UnexpectedTokenOf, span::Spanned,
  };

  type Lx<'inp> = GraphqlLosslessLexer<'inp, str>;
  type Ctx<'inp> = ParserContext<'inp, Lx<'inp>, Counting, DefaultCache<'inp, Lx<'inp>>, GraphQL>;

  thread_local! {
    /// One per `emit_lexer_error` — what a drain over a tail that does not lex produces.
    static TAIL_DIAGNOSTICS: StdCell<usize> = const { StdCell::new(0) };
  }

  /// The consumer's error type. `Scanner` is terminal and `Ordinary` is not, and that one bit is
  /// the whole observation.
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  enum E {
    /// A scanner stop the emitter rejected onto the parser's channel. Nothing descended to produce
    /// it, so tokora's resource-trip counter cannot have moved.
    Scanner,
    /// An ordinary syntax error, already reported at the point of failure.
    Ordinary,
  }

  impl MaybeTerminal for E {
    fn is_terminal(&self) -> bool {
      matches!(self, E::Scanner)
    }
  }

  // NEVER CALLED, AND THAT IS PROVABLE HERE RATHER THAN HOPED. `drain_unless_stopped` mints a
  // token-budget refusal only off the input's own `refused_an_item` bit, and this cell drives a
  // `ParserContext` whose token budget is tokora's default — `TokenBudget::unlimited()`, whose
  // gate excludes its own `usize::MAX` sentinel, so no item is ever refused.
  impl FromTokenBudget for E {
    fn token_budget_exhausted(_span: SimpleSpan, _spent: usize, _limit: usize) -> Self {
      unreachable!("this cell configures no token budget, so nothing can refuse an item")
    }
  }

  /// Accepts everything and counts the lexer diagnostics, for the reason
  /// `a_caught_trip_does_not_silence_a_later_failures_drain` records: rejecting would stop the
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

  /// Which frame the failure is handed to.
  #[derive(Debug, Clone, Copy)]
  enum Via {
    /// The primitive, directly — its own contract, with no door above it.
    Terminality,
    /// The door an `*_entry` production writes, whose `Recoverable` arm is where the claim under
    /// test lives.
    Entry,
  }

  /// One drain over a root that fails with `failure` and classifies nothing, and the tail
  /// diagnostics that drain produced.
  ///
  /// The `'inp` is NAMED, threaded from `src`, for the reason
  /// `each_term_of_a_roots_stop_is_alone_on_a_population` records: elided, it varies independently
  /// of the error type and the closure `E0521`s.
  fn drive<'inp>(src: &'inp str, via: Via, failure: E) -> (Result<(), E>, usize) {
    TAIL_DIAGNOSTICS.with(|n| n.set(0));
    let out = tokora::parse_with::<Lx<'inp>, str, _, (), Ctx<'inp>, GraphQL>(
      |inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>| match via {
        Via::Terminality => drain_unless_terminal(inp, Err(failure)),
        // NO `root_turn` AND NO DESCENT. The slot the drain mints for this root stays fresh, so
        // the ending is `Recoverable`; nothing moved the counter, so the residual reading is
        // `false` and the arm below it is where this lands.
        Via::Entry => drain_unless_stopped(
          inp,
          |_inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>, _stop: &mut RootStop| {
            Err(failure)
          },
        ),
      },
      src,
      // No `with_recursion_limiter`: nothing here descends, and a budget would be a number this
      // cell does not read.
      ParserContext::of(Counting),
    );
    (out, TAIL_DIAGNOSTICS.with(StdCell::get))
  }

  // `~` does not lex in either dialect, so one per lexeme is what a drain over this tail reports.
  for n in [0usize, 1, 4, 16] {
    let src = "~ ".repeat(n);

    for via in [Via::Terminality, Via::Entry] {
      assert_eq!(
        drive(&src, via, E::Scanner),
        (Err(E::Scanner), 0),
        "n={n}, via={via:?}: a terminal failure no turn classified must not have its tail read — \
         the witness answers `false` here by construction, so `MaybeTerminal` is the only term \
         left and deleting `drain_unless_terminal`'s early return reads `n` instead"
      );
      assert_eq!(
        drive(&src, via, E::Ordinary),
        (Err(E::Ordinary), n),
        "n={n}, via={via:?}: the control — the same unclassified, untripped failure on a \
         NON-terminal value must still be drained. If this ever agrees with the cell above, the \
         reading there is about the drain being off and not about the term"
      );
    }
  }
}

/// A durable token-budget refusal is reported **once**, however many drains are stacked over the
/// root that took it — smear issue #193, round 2.
///
/// # The defect this is the cell for
///
/// `drain_unless_stopped` polls
/// [`TokenBudgetTally::refused_an_item`](tokora::input::TokenBudgetTally::refused_an_item) and
/// differences it against a baseline taken before its root runs. That reading is
/// **input-absolute and monotone** — it is a `bool` on a cell no rollback reaches and no mutator
/// lowers — so *every* frame that took its baseline before the refusal sees the same `false ->
/// true` transition. A boolean difference cannot say which frame's root produced it.
///
/// Two nested drains therefore both mint a diagnostic for one refusal: the inner reports and
/// returns `Err`, and the outer, whose baseline predates the same refusal, reports again.
/// Measured before the repair: **2** at every budget, against **1** through a single drain.
///
/// This is round 1's own claim inverted. That commit body said the baseline "makes a nested drain
/// emit once", which is the sentence Codex round 1 falsified — and the reason it was wrong is worth
/// keeping: the differencing discipline was copied from `root_turn`'s descent witness, where
/// `trip_snapshot` returns a **counter** whose value is unique per trip. Copying the discipline
/// without copying the *carrier* is what left an ownership signal that cannot distinguish owners.
///
/// # The repair, and why it is a generation rather than another baseline
///
/// [`RootStop`] gains the fact the boolean cannot carry: *this frame's root already reported the
/// refusal*. The inner drain writes it before returning `Err`; nothing else writes it. But a slot
/// is per frame and the outer frame holds its own, so the marker cannot travel in the slot alone —
/// it travels the way tokora's own answer to this shape does, on the **input**, as the durable
/// count of refusals smear has already reported. The outer frame subtracts what it reported
/// itself, which is a number rather than a bit, and a number can tell two frames apart.
///
/// # The three cells
///
/// * **Nested.** The defect: 2 before, 1 after.
/// * **The single-level control.** Already right at 1, so a repair that suppressed the report
///   outright is visible here as a 0 rather than as an unchanged number.
/// * **A budget nothing reaches.** The control that stays green under the defect, so the two
///   above are about the refusal rather than about the shape.
#[test]
fn a_budget_refusal_is_reported_once_however_many_drains_are_stacked() {
  use crate::{
    graphql::{
      GraphQL,
      lossless::{GraphqlLosslessErrors, GraphqlLosslessLexer, runner::LosslessEmitter},
    },
    lossless::depth::{RootStop, drain_unless_stopped},
  };
  use smear_lexer::limits::LosslessLimits;
  use tokora::{InputRef, cache::DefaultCache, cst::Sink};

  type Lx<'inp> = GraphqlLosslessLexer<'inp, str>;
  /// THE DOOR'S OWN CONTEXT, and there is no other now. `parse_lossless_document` mints the
  /// `Sink` from the source, pairs it with the cache, and builds the emitter itself — so a cell
  /// that asks how many diagnostics a parse ends up with is parameterised by the pair and reads
  /// the dialect's own error container.
  type Ctx<'inp> = (
    Sink<'inp, Lx<'inp>, LosslessEmitter<'inp>>,
    DefaultCache<'inp, Lx<'inp>>,
  );
  type Errs<'inp> = GraphqlLosslessErrors<&'inp str>;

  /// How many budget refusals **survived** in this parse's log.
  ///
  /// Read off the door's own `Cst` rather than a thread-local counter, and that is Codex round 5's
  /// repair showing through: a counter shared between two parses is exactly the aliasing the door
  /// no longer permits, so the cell cannot use one either. `finish_partial` hands the emitter back
  /// whether or not the green tree is refused, and `Verbose`'s rewind drops rolled-back emissions
  /// — so this log **is** "surviving diagnostics", which is the quantity every count below means.
  fn budget_reports(parse: &crate::graphql::lossless::runner::Parse) -> usize {
    // WHAT THE OUTPUT SAYS, and the door hands it over directly — smear issue #193, rounds 7 and
    // 8. The door no longer emits: it builds its verdict, drops every budget diagnostic the
    // grammar emitted, appends its own, and returns the finished `Parse`. Neither the `Cst` nor
    // the verdict is in a caller's hands, so a cell about the COUNT has nothing to assemble and
    // nothing it could assemble wrongly.
    //
    // A zero-width `Error` is the door's report's shape, and each cell carries the control that
    // says so: the same parse with nothing refused has none.
    parse
      .diagnostics()
      .iter()
      .filter(|d| {
        d.severity() == tokora::emitter::Severity::Error && d.span().start == d.span().end
      })
      .count()
  }

  /// The inner root: it drives the scanner to the end of the source, which is where the budget
  /// refuses. It returns `Ok` — and that is the shipped shape rather than a contrivance, because
  /// a refusal latches the poison boundary and every loop above it then reads an end of input.
  fn inner<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), Errs<'inp>> {
    inp.skip_while(|_| true)?;
    Ok(())
  }

  /// The outer root: it returns the nested drain's `Result` and touches its own slot not at all,
  /// exactly as `a_nested_drains_stop_is_not_reclassified_by_the_drain_above_it`'s does.
  fn outer<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), Errs<'inp>> {
    drain_unless_stopped(inp, inner)
  }

  /// The inner root that reads **nothing**, so the refusal's first occurrence is inside that
  /// frame's own tail drain rather than inside its root — smear issue #193, Codex round 2.
  ///
  /// The nesting question this adds is the one the post-drain poll makes reachable: the frame the
  /// ceiling is met in is now the one whose *drain* met it, and one refusal still has to be one
  /// diagnostic. Round 4 changed WHY it is: no frame here reports at all — each one stops, hands
  /// up a terminal value and emits nothing — and the door, which `drive` runs because the six
  /// shipped doors run it, emits once off the durable bit. The count is a property of the door
  /// rather than of what any frame under it concluded, which is what Codex round 3 showed a
  /// terminality reading could not deliver.
  fn drains<'inp>(
    _inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), Errs<'inp>> {
    Ok(())
  }

  /// [`drains`] under a second drain: two stacked frames whose refusal is taken in the inner
  /// frame's drain.
  fn outer_over_a_draining_inner<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), Errs<'inp>> {
    drain_unless_stopped(inp, drains)
  }

  /// A root that spends the budget **inside a speculative attempt and then declines it**.
  ///
  /// [`InputRef::attempt`](tokora::InputRef::attempt) rolls back on `None`: the cursor, the lexer
  /// state and the poison boundary all go back to what the checkpoint saved, and the checkpoint
  /// predates the refusal, so every positional witness reads clean afterwards. The refusal does
  /// not: `TokenBudgetTally` is not a `Checkpoint` field. This is Codex round 1's second axis —
  /// a rollback between the drain's baseline and its poll — and what it asserts is that the
  /// baseline still means what it meant, because the cell it is a baseline of is outside the
  /// rollback set.
  fn speculating<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), Errs<'inp>> {
    inp.attempt(
      |inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>| {
        let _ = inp.skip_while(|_| true);
        // DECLINED. Everything the attempt did to the input is given back — except the charge.
        None::<()>
      },
    );
    Ok(())
  }

  /// One parse of `src` under a token budget of `ceiling`: what it ended as, and how many budget
  /// refusals its own log kept.
  fn drive<'inp, R>(src: &'inp str, ceiling: usize, root: R) -> usize
  where
    R: for<'c> FnOnce(
      &mut InputRef<'inp, 'c, Lx<'inp>, Ctx<'inp>, GraphQL>,
      &mut RootStop,
    ) -> Result<(), Errs<'inp>>,
  {
    let limits = LosslessLimits::default();
    let parse = crate::graphql::lossless::runner::parse_lossless_document(
      src,
      limits.with_max_produce_events(ceiling),
      root,
    );
    budget_reports(&parse)
  }

  // Forty lexemes, so every ceiling below is reached well inside the source.
  let src = "a b c d e f g h i j k l m n o p q r s t ".repeat(2);

  for ceiling in [1usize, 4, 16] {
    assert_eq!(
      drive(&src, ceiling, outer),
      1,
      "ceiling={ceiling}: one refusal reported twice. `refused_an_item` is input-absolute and \
       monotone, so both nested frames see the same `false -> true` transition and a boolean \
       difference cannot attribute it to the frame whose root produced it — smear issue #193 \
       round 2"
    );
    assert_eq!(
      drive(&src, ceiling, inner),
      1,
      "ceiling={ceiling}: the single-level control. One drain over the same refusal was already \
       right, so a repair that suppressed the report rather than de-duplicating it reads 0 here"
    );
  }

  // THE REFUSAL PLACED IN THE INNER FRAME'S DRAIN rather than in its root — smear issue #193,
  // Codex round 2. The inner frame reads nothing, so its own tail drain is what meets the ceiling.
  // Still one diagnostic through two drains, and since round 4 that is the door's property rather
  // than any frame's: every frame stops and none of them emits.
  for ceiling in [1usize, 4, 16] {
    assert_eq!(
      drive(&src, ceiling, outer_over_a_draining_inner),
      1,
      "ceiling={ceiling}: a refusal first taken in the INNER frame's drain was reported {} times \
       through two stacked drains. Every frame stops and only the door decides, so a count other \
       than one is the door putting more than one report in the output for one refusal",
      drive(&src, ceiling, outer_over_a_draining_inner)
    );
    assert_eq!(
      drive(&src, ceiling, drains),
      1,
      "ceiling={ceiling}: the single-level control for the same placement. One drain over the \
       same refusal must read 1, so a repair that suppressed the report rather than \
       de-duplicating it reads 0 here"
    );
  }

  // A ROLLBACK BETWEEN THE BASELINE AND THE POLL. The refusal happens inside a speculative
  // attempt that is then declined, so the cursor, the lexer state and the poison boundary are all
  // restored to a checkpoint that predates it. The charge is not, and the report must still be
  // made exactly once.
  for ceiling in [1usize, 4, 16] {
    assert_eq!(
      drive(&src, ceiling, speculating),
      1,
      "ceiling={ceiling}: a refusal taken inside a declined attempt went unreported. \
       `TokenBudgetTally` is not a `Checkpoint` field, so the rollback restores the boundary and \
       the cursor and cannot give the charge back — a parse that reads the difference as `no \
       refusal happened` is reading a cell the rollback does not reach"
    );
  }

  // The control that stays green under the defect: a ceiling nothing reaches reports nothing, so
  // every count above is about the refusal and not about the shape of these roots.
  assert_eq!(
    drive(&src, 10_000, outer),
    0,
    "a budget no parse reaches must mint no diagnostic and must not end the document"
  );
  assert_eq!(drive(&src, 10_000, inner), 0, "the same, through one drain");
  assert_eq!(
    drive(&src, 10_000, speculating),
    0,
    "the same, through a declined attempt — which is what says the reading above is the refusal \
     rather than the attempt"
  );
  assert_eq!(
    drive(&src, 10_000, outer_over_a_draining_inner),
    0,
    "the same, through two drains whose reading is the drain's own"
  );
}

/// A refusal whose **first occurrence is inside the tail drain** is this frame's to report, on
/// both arms that drain — smear issue #193, Codex round 2.
///
/// # The defect
///
/// `drain_unless_stopped` polled the durable budget once, between the root's return and the arm
/// below it. The `Parsed` and ordinary `Recoverable` arms then call [`drain_unless_terminal`],
/// whose `skip_while(|_| true)` reads the same input against the same tally — and tokora answers
/// `Ok` on the terminal `Scan::Tripped`, so a refusal the drain was the first to take left the
/// frame carrying the root's own outcome and nothing naming the refusal. The door discards the
/// parser's `Result` and `Cst::finish_partial` tiles the unread tail as a gap run, so a consumer
/// gets a short tree, no refusal diagnostic, and no way to ask.
///
/// # Why the cell is here and not only at a shipped door
///
/// Five of the six shipped roots **resynchronise** on an ordinary failure, so their loop only
/// exits at an end of input and there is no tail left for the drain to be the first reader of;
/// GraphQL's executable root is the one that propagates both failure arms (smear issue #168), and
/// `smear/tests/durable_token_budget.rs` carries that door's cell. A cell that only ran there
/// would be a guard on one root's recovery choice rather than on the frame, and the frame is what
/// the repair is in. These two roots reach the two draining arms directly:
///
/// * `Ok(())` without reading — the `Parsed` arm, over a tail the drain then lexes;
/// * a non-terminal `Err` without reading — the ordinary `Recoverable` arm, same tail.
///
/// Both are the shapes a sixth root can be written in, and the `Parsed` one is **not** reachable
/// at a shipped door today: a shipped root returns `Ok` only when its peek answered `None`, which
/// over a non-empty tail means a latched poison boundary, and a latched boundary makes the tail
/// unreachable to the drain. Measured at the doors: with `max_tokens(k)` tripping the lexer, the
/// durable tally stops at `k` and no produce ceiling above `k` changes the parse.
///
/// # What the report displaces, measured rather than argued
///
/// On the `Recoverable` arm the frame returns the refusal instead of the root's syntax error.
/// That is the same trade the first poll already makes and the reason is the same one it gives:
/// the syntax error was reported at the point of failure, so what the displacement costs is a
/// **value** and not a report.
///
/// The case worth measuring is the other one. [`drain_unless_terminal`]'s own note says a lexer
/// error's `Err` out of `skip_while` **is** its delivery — the input layer advances the dedup
/// watermark before calling, so dropping that `Err` drops the diagnostic. If a drain could both
/// have that rejection to hand back *and* have refused an item, the report would displace a
/// delivery rather than a value. The third axis below is that measurement: an emitter that
/// rejects `emit_lexer_error`, a tail with one invalid lexeme, and every ceiling from before it
/// to past it.
#[test]
fn a_refusal_first_taken_inside_the_drain_is_reported_on_both_draining_arms() {
  use crate::{
    graphql::{
      GraphQL,
      error::{Error as DialectError, ErrorData},
      lossless::{GraphqlLosslessErrors, GraphqlLosslessLexer, runner::LosslessEmitter},
    },
    lossless::depth::RootStop,
  };
  use smear_lexer::limits::LosslessLimits;
  use tokora::{InputRef, SimpleSpan, cache::DefaultCache, cst::Sink};

  type Lx<'inp> = GraphqlLosslessLexer<'inp, str>;
  type Ctx<'inp> = (
    Sink<'inp, Lx<'inp>, LosslessEmitter<'inp>>,
    DefaultCache<'inp, Lx<'inp>>,
  );
  type Errs<'inp> = GraphqlLosslessErrors<&'inp str>;

  /// Budget refusals that survived in this parse's log.
  fn budget_reports(parse: &crate::graphql::lossless::runner::Parse) -> usize {
    // WHAT THE OUTPUT SAYS, and the door hands it over directly — smear issue #193, rounds 7 and
    // 8. The door no longer emits: it builds its verdict, drops every budget diagnostic the
    // grammar emitted, appends its own, and returns the finished `Parse`. Neither the `Cst` nor
    // the verdict is in a caller's hands, so a cell about the COUNT has nothing to assemble and
    // nothing it could assemble wrongly.
    //
    // A zero-width `Error` is the door's report's shape, and each cell carries the control that
    // says so: the same parse with nothing refused has none.
    parse
      .diagnostics()
      .iter()
      .filter(|d| {
        d.severity() == tokora::emitter::Severity::Error && d.span().start == d.span().end
      })
      .count()
  }

  /// The `Parsed` arm: the root says it parsed and reads nothing, so the whole source is the tail
  /// and the drain behind it is the first — and only — reader of the budget.
  fn parsed_with_tail<'inp>(
    _inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), Errs<'inp>> {
    Ok(())
  }

  /// The ordinary `Recoverable` arm: a non-terminal failure over the same untouched tail.
  ///
  /// `FloatOverflow` is the cheapest dialect error whose `MaybeTerminal` arm is `false`
  /// (`graphql/error.rs`'s wildcard-free `is_terminal` match), which is the only property this
  /// root needs of it. The bespoke `E::Syntax` it replaces went with the emitter: the door owns
  /// the emitter now, so the error type is the dialect's.
  fn recoverable_with_tail<'inp>(
    _inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), Errs<'inp>> {
    Err(Errs::from(DialectError::new(
      SimpleSpan::new(0, 0),
      ErrorData::FloatOverflow("x"),
    )))
  }

  /// One parse of `src` under a token budget of `ceiling`.
  fn drive<'inp, R>(src: &'inp str, ceiling: usize, root: R) -> usize
  where
    R: for<'c> FnOnce(
      &mut InputRef<'inp, 'c, Lx<'inp>, Ctx<'inp>, GraphQL>,
      &mut RootStop,
    ) -> Result<(), Errs<'inp>>,
  {
    let limits = LosslessLimits::default();
    let parse = crate::graphql::lossless::runner::parse_lossless_document(
      src,
      limits.with_max_produce_events(ceiling),
      root,
    );
    budget_reports(&parse)
  }

  // Forty lexemes, so every ceiling below is met well inside the tail the drain reads.
  let src = "a b c d e f g h i j k l m n o p q r s t ".repeat(2);

  for ceiling in [1usize, 4, 16] {
    assert_eq!(
      drive(&src, ceiling, parsed_with_tail),
      1,
      "ceiling={ceiling}, the `Parsed` arm: the drain met the ceiling and the parse came back \
       with nothing naming the refusal. `skip_while` answers `Ok` on the terminal \
       `Scan::Tripped`, so the drain is a reader whose refusal only the door above it can report \
       — smear issue #193, Codex round 2"
    );
    assert_eq!(
      drive(&src, ceiling, recoverable_with_tail),
      1,
      "ceiling={ceiling}, the ordinary `Recoverable` arm: the drain met the ceiling and the parse \
       came back on the root's own syntax error, truncated and unnamed. The refusal outranks the \
       syntax error for the same reason the poll above the arm outranks it — the syntax error was \
       reported at the point of failure, so what is displaced is a value"
    );
  }

  // The control that stays green under the defect: a ceiling nothing reaches mints nothing and
  // leaves each root's own outcome alone, so every reading above is about the refusal rather than
  // about the shape of these two roots.
  assert_eq!(
    drive(&src, 10_000, parsed_with_tail),
    0,
    "a budget no parse reaches must mint no diagnostic and must not end the document"
  );
  assert_eq!(
    drive(&src, 10_000, recoverable_with_tail),
    0,
    "the same, and the root's own NON-TERMINAL value must reach the caller as one: a `Stopped` \
     here would be the frame ending a document on an ordinary failure"
  );
}

/// What the frame hands **up** after a refusal first taken in its drain, and what a rejected lexer
/// error does beside it — smear issue #193, rounds 2 and 5.
///
/// # Why this cell does not go through the door, and what that costs
///
/// It pins [`drain_unless_stopped`], not [`parse_lossless_document`]. Two things it needs the door
/// cannot give it, and since Codex round 5 that is deliberate rather than incidental: an error type
/// whose [`MaybeTerminal`](tokora::error::MaybeTerminal) arms this cell chooses, and an emitter
/// that **rejects** `emit_lexer_error`. The door builds `Verbose` itself precisely so that no
/// caller supplies either.
///
/// So it builds a `ParserContext` — tokora's own public door onto a context, which no smear
/// mint is involved in — and runs the frame directly. That is not a forgery route and it does not
/// weaken round 5: the frame it reaches **stops and never emits**, so nothing here can put a
/// diagnostic anywhere. The count half of both arms lives in
/// [`a_refusal_first_taken_inside_the_drain_is_reported_on_both_draining_arms`] and goes through
/// the real door, because a count is the door's property.
///
/// # What it asserts
///
/// * both draining arms — `Ok` without reading, and a non-terminal `Err` without reading — hand up
///   a **terminal** value once the drain has met the ceiling. That value is what every composition
///   above the frame reads;
/// * the displacement axis. [`drain_unless_terminal`]'s note says a lexer error's `Err` out of
///   `skip_while` **is** its delivery, so a stop that replaced one would drop a diagnostic rather
///   than a value. It cannot: the two are exclusive within one `skip_while`, because whichever
///   comes first ends the scan. Measured over every ceiling from 0 to 24 — some return the
///   refusal with the lexer error never offered, some return the rejection with nothing refused,
///   and **none** does both.
#[test]
fn the_value_a_frame_hands_up_after_a_drain_refusal_is_terminal() {
  use core::cell::Cell as StdCell;

  use crate::{
    graphql::{GraphQL, lossless::GraphqlLosslessLexer},
    lossless::depth::{FromNestingLimit, FromTokenBudget, RootStop, drain_unless_stopped},
  };
  use tokora::{
    Emitter, InputRef, Lexer, ParserContext, SimpleSpan, Token,
    cache::DefaultCache,
    error::{MaybeTerminal, RecursionLimitReached},
    input::TokenBudget,
    prelude::UnexpectedTokenOf,
    span::Spanned,
  };

  type Lx<'inp> = GraphqlLosslessLexer<'inp, str>;
  type Ctx<'inp> = ParserContext<'inp, Lx<'inp>, Recording, DefaultCache<'inp, Lx<'inp>>, GraphQL>;

  thread_local! {
    /// One per `emit_lexer_error` OFFERED, whether or not this emitter accepts it.
    static LEXER_OFFERS: StdCell<usize> = const { StdCell::new(0) };
    /// Whether `emit_lexer_error` rejects. The displacement axis turns it on.
    static REJECT_LEXER_ERRORS: StdCell<bool> = const { StdCell::new(false) };
  }

  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  enum E {
    /// What `FromTokenBudget` mints. Terminal, as every shipped container's arm is.
    Budget,
    /// An ordinary syntax error: **not** terminal, so the root that returns it takes the
    /// `Recoverable` arm and the drain runs.
    Syntax,
    /// What a rejecting `emit_lexer_error` hands back — the displacement axis's subject.
    LexerRejected,
    /// The descent refusal, which this cell never reaches — no production here descends.
    Refusal,
  }

  impl MaybeTerminal for E {
    fn is_terminal(&self) -> bool {
      matches!(self, E::Budget)
    }
  }

  impl FromNestingLimit for E {
    fn nesting_limit_exceeded(_span: SimpleSpan, _attempted: usize, _limit: usize) -> Self {
      E::Refusal
    }
  }

  impl FromTokenBudget for E {
    fn token_budget_exhausted(_span: SimpleSpan, _spent: usize, _limit: usize) -> Self {
      E::Budget
    }
  }

  impl<Lang: ?Sized> From<RecursionLimitReached<usize, Lang>> for E {
    fn from(_: RecursionLimitReached<usize, Lang>) -> Self {
      E::Refusal
    }
  }

  /// Counts the lexer errors offered, and rejects them when the displacement axis asks it to.
  ///
  /// It counts NO budget reports, and there are none to count: the frame this cell drives stops
  /// and never emits. That is the round-5 split showing through — a cell about the value cannot
  /// also be a cell about the log.
  struct Recording;

  impl<'inp, L: Lexer<'inp>> Emitter<'inp, L, GraphQL> for Recording {
    type Error = E;

    fn emit_lexer_error(
      &mut self,
      _err: Spanned<<L::Token as Token<'inp>>::Error, L::Span>,
    ) -> Result<(), Self::Error> {
      LEXER_OFFERS.with(|n| n.set(n.get() + 1));
      if REJECT_LEXER_ERRORS.with(StdCell::get) {
        return Err(E::LexerRejected);
      }
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

  /// The `Parsed` arm: the root says it parsed and reads nothing.
  fn parsed_with_tail<'inp>(
    _inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), E> {
    Ok(())
  }

  /// The ordinary `Recoverable` arm: a non-terminal failure over the same untouched tail.
  fn recoverable_with_tail<'inp>(
    _inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), E> {
    Err(E::Syntax)
  }

  /// The INNER FRAME alone, over a context this cell builds: what it handed up, and how many lexer
  /// errors the tail offered.
  fn frame<'inp, R>(src: &'inp str, ceiling: usize, root: R) -> (Result<(), E>, usize)
  where
    R:
      FnOnce(&mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>, &mut RootStop) -> Result<(), E>,
  {
    LEXER_OFFERS.with(|n| n.set(0));
    let mut root = Some(root);
    let out = tokora::parse_with::<Lx<'inp>, str, _, (), Ctx<'inp>, GraphQL>(
      |inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>| {
        drain_unless_stopped(inp, root.take().expect("the production runs once"))
      },
      src,
      ParserContext::of(Recording).with_token_budget(TokenBudget::with_limitation(ceiling)),
    );
    (out, LEXER_OFFERS.with(StdCell::get))
  }

  let src = "a b c d e f g h i j k l m n o p q r s t ".repeat(2);

  for ceiling in [1usize, 4, 16] {
    assert_eq!(
      frame(&src, ceiling, parsed_with_tail),
      (Err(E::Budget), 0),
      "ceiling={ceiling}, the `Parsed` arm: the drain met the ceiling and the frame handed up the \
       root's `Ok`. A frame that says the document is fine over a poisoned input is one every \
       composition above it will carry on from"
    );
    assert_eq!(
      frame(&src, ceiling, recoverable_with_tail),
      (Err(E::Budget), 0),
      "ceiling={ceiling}, the ordinary `Recoverable` arm: the drain met the ceiling and the frame \
       handed up the root's own non-terminal syntax error. The stop outranks it, and what is \
       displaced is a value that was already reported at the point of failure"
    );
  }

  assert_eq!(
    frame(&src, 10_000, parsed_with_tail),
    (Ok(()), 0),
    "a budget no parse reaches must not end the document"
  );
  assert_eq!(
    frame(&src, 10_000, recoverable_with_tail),
    (Err(E::Syntax), 0),
    "the same, and the root's own value must reach the caller unchanged"
  );

  // ── THE DISPLACEMENT AXIS ────────────────────────────────────────────────────────────────
  //
  // An emitter that REJECTS `emit_lexer_error`, and a tail whose thirteenth item does not lex. The
  // question is whether one drain can both hand back that rejection and have refused an item — if
  // it can, the stop above it displaces a DELIVERY rather than a value.
  REJECT_LEXER_ERRORS.with(|f| f.set(true));
  let poisoned = "a b c d e f ~ g h i j k l m n o p q r s t ";
  println!("\n== the drain's rejection against the drain's refusal ==");
  println!("  {:>8} {:>16} {:>7}", "ceiling", "out", "lexer");
  let mut rejections = 0usize;
  let mut refusals = 0usize;
  let mut both = 0usize;
  for ceiling in 0..=24usize {
    let (out, lexer) = frame(poisoned, ceiling, parsed_with_tail);
    if out == Err(E::LexerRejected) {
      rejections += 1;
    }
    if out == Err(E::Budget) {
      refusals += 1;
    }
    if out == Err(E::LexerRejected) && lexer > 0 && out == Err(E::Budget) {
      both += 1;
    }
    println!("  {ceiling:>8} {out:>16?} {lexer:>7}");
  }
  REJECT_LEXER_ERRORS.with(|f| f.set(false));

  assert!(
    rejections > 0 && refusals > 0,
    "the displacement axis collapsed: {rejections} ceilings handed back the emitter's rejection \
     and {refusals} handed back the refusal. Both populations have to be non-empty for the count \
     of their intersection to mean anything"
  );
  assert_eq!(
    both, 0,
    "{both} ceilings had the drain both hand back a rejected lexer error AND stop on the budget. \
     The two are exclusive within one `skip_while` — whichever comes first ends the scan — and \
     this assertion is what says so. If it ever fails, the post-drain stop in \
     `drain_unless_stopped` is displacing a diagnostic's only delivery and must keep it instead"
  );
  assert_eq!(
    rejections + refusals,
    25,
    "every ceiling from 0 to 24 must land in exactly one of the two populations; {rejections} + \
     {refusals} does not cover the sweep, which means a third outcome exists that this axis does \
     not name"
  );
}

/// The report has an **owner**, and terminality was never it — smear issue #193, Codex round 3.
///
/// # The defect
///
/// Rounds 2 and 3 de-duplicated the report by asking whether the value in hand is terminal:
/// a frame handed a terminal `Err` concluded that some frame below it had already reported. That
/// infers a fact about the **log** from a fact about the **value**, and the two come apart in both
/// directions once a composed root wraps the inner frame:
///
/// * `InputRef::try_attempt` rolls back on `Err`. tokora's `restore_unchecked` restores the
///   cursor, the span, the lexer state, **the emitter (rewound to its checkpoint)**,
///   `emitted_error_end`, `front_reported_end`, **the poison boundary** and the regime — and
///   `Checkpoint` has no token-budget field at all. So an inner frame that reported and returned
///   terminal `Err` has its diagnostic **undone** while `refused_an_item` stays `true`; the outer
///   frame reads the surviving terminal value, concludes the report was made, and the parse comes
///   back truncated and silent;
/// * an outer root that **catches** that terminal `Err` and returns an ordinary value keeps the
///   emission and loses the terminality, so the outer frame reports a **second** time.
///
/// # The four cases, and what each is for
///
/// Each drives the same inner frame — a root that reads to the end, so the refusal lands inside
/// the inner frame rather than in the composed root above it — through a different composition:
///
/// * **(a) direct.** The inner value propagates unchanged. The control: this is the shape rounds 2
///   and 3 measured, and it was already right;
/// * **(b) attempt, propagated.** The inner frame runs inside `try_attempt` whose closure hands
///   the terminal `Err` back, so the attempt rolls the emission back. Codex's first direction;
/// * **(c) attempt, committed.** The same, with the closure mapping the `Err` to `Ok` so the
///   attempt commits. The control that separates *the rollback* from *the attempt*: if (b) and (c)
///   read alike, the reading is about `try_attempt` rather than about what it restores;
/// * **(d) caught and replaced.** No attempt at all: the composed root catches the inner terminal
///   `Err` and returns an ordinary one. Codex's second direction.
///
/// The inner frame's own returned value is asserted terminal in all four — that is the stop's
/// pin, and it is deliberately separate from the count, because the repair splits the two: the
/// inner frame stops and the door reports.
#[test]
fn the_report_has_an_owner_and_terminality_is_not_it() {
  use core::cell::Cell as StdCell;

  use crate::{
    graphql::{
      GraphQL,
      error::{Error as DialectError, ErrorData},
      lossless::{GraphqlLosslessErrors, GraphqlLosslessLexer, runner::LosslessEmitter},
    },
    lossless::depth::{RootStop, drain_unless_stopped},
  };
  use smear_lexer::limits::LosslessLimits;
  use tokora::{
    InputRef, SimpleSpan, cache::DefaultCache, cst::Sink, error::MaybeTerminal, span::Spanned,
  };

  type Lx<'inp> = GraphqlLosslessLexer<'inp, str>;
  type Ctx<'inp> = (
    Sink<'inp, Lx<'inp>, LosslessEmitter<'inp>>,
    DefaultCache<'inp, Lx<'inp>>,
  );
  type Errs<'inp> = GraphqlLosslessErrors<&'inp str>;

  thread_local! {
    /// Whether the inner frame's returned value answered `is_terminal`.
    ///
    /// A thread-local for a VALUE, which is not the shape Codex round 5 is about: what may not be
    /// shared across parses is a diagnostic timeline, and this holds neither a diagnostic nor
    /// anything a parse writes twice.
    static INNER_WAS_TERMINAL: StdCell<Option<bool>> = const { StdCell::new(None) };
  }

  /// Budget refusals that **survived** in this parse's log.
  ///
  /// Read off the door's own `Cst`, and that is the round-5 repair showing through: the counter
  /// this replaces was a thread-local shared between whatever parses ran, which is exactly the
  /// aliasing the door no longer permits a caller to build. `Verbose`'s rewind drops rolled-back
  /// emissions, so the log is "surviving diagnostics" — which is what case (b) needs it to be.
  fn budget_reports(parse: &crate::graphql::lossless::runner::Parse) -> usize {
    // WHAT THE OUTPUT SAYS, and the door hands it over directly — smear issue #193, rounds 7 and
    // 8. The door no longer emits: it builds its verdict, drops every budget diagnostic the
    // grammar emitted, appends its own, and returns the finished `Parse`. Neither the `Cst` nor
    // the verdict is in a caller's hands, so a cell about the COUNT has nothing to assemble and
    // nothing it could assemble wrongly.
    //
    // A zero-width `Error` is the door's report's shape, and each cell carries the control that
    // says so: the same parse with nothing refused has none.
    parse
      .diagnostics()
      .iter()
      .filter(|d| {
        d.severity() == tokora::emitter::Severity::Error && d.span().start == d.span().end
      })
      .count()
  }

  /// The inner root: reads to the end, so the refusal is taken inside the inner frame.
  fn inner<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), Errs<'inp>> {
    inp.skip_while(|_| true)?;
    Ok(())
  }

  /// Runs the inner frame and records whether what it handed back is terminal.
  fn inner_frame<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
  ) -> Result<(), Errs<'inp>> {
    let out = drain_unless_stopped(inp, inner);
    INNER_WAS_TERMINAL.with(|c| {
      c.set(Some(
        out.as_ref().err().is_some_and(MaybeTerminal::is_terminal),
      ))
    });
    out
  }

  /// (a) The inner value propagates unchanged.
  fn direct<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), Errs<'inp>> {
    inner_frame(inp)
  }

  /// (b) Inside `try_attempt`, propagating the terminal `Err` — so the attempt ROLLS BACK.
  fn attempt_propagated<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), Errs<'inp>> {
    inp.try_attempt(inner_frame)
  }

  /// (c) Inside `try_attempt`, mapping the terminal `Err` to `Ok` — so the attempt COMMITS.
  fn attempt_committed<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), Errs<'inp>> {
    inp.try_attempt(
      |inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>| match inner_frame(inp) {
        Ok(()) | Err(_) => Ok::<(), Errs<'inp>>(()),
      },
    )
  }

  /// (d) Caught and replaced by an ordinary value — no attempt, so nothing rolls back.
  ///
  /// `FloatOverflow` is the cheapest dialect error whose `MaybeTerminal` arm is `false`, which is
  /// the only property this case needs of it.
  fn caught_and_replaced<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), Errs<'inp>> {
    match inner_frame(inp) {
      Ok(()) => Ok(()),
      Err(_) => Err(Errs::from(DialectError::new(
        SimpleSpan::new(0, 0),
        ErrorData::FloatOverflow("x"),
      ))),
    }
  }

  /// (e) A composed root that **catches the nested stop and emits it** while recovering.
  ///
  /// Codex round 7's natural case, and it needs no forgery: the terminal value
  /// `drain_unless_stopped` hands up IS the budget variant, `InputRef::emit_error` is public, and
  /// a root that reports what it caught and carries on is ordinary-looking recovery code. Before
  /// round 8 the door then observed the same durable refusal and reported again — **2** surviving
  /// diagnostics for one refusal.
  ///
  /// It emits at `0..0` deliberately: the door's own report sits at the committed end, so the
  /// assertion below can tell which one survived by its span alone.
  fn catches_and_emits<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), Errs<'inp>> {
    match drain_unless_stopped(inp, inner) {
      Ok(()) => Ok(()),
      Err(caught) => {
        let _ = inp.emit_error(Spanned::new(SimpleSpan::new(0, 0), caught));
        Ok(())
      }
    }
  }

  /// (f) A root that **forges** the variant with nothing refused, beside one ordinary diagnostic.
  ///
  /// Before round 8 this marked a complete parse as truncated: one budget diagnostic in the output
  /// of a parse whose budget never refused. The ordinary `FloatOverflow` beside it is what makes
  /// "only that variant is dropped" checkable rather than asserted.
  fn forges_without_refusal<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), Errs<'inp>> {
    let forged = SimpleSpan::new(0, 0);
    let _ = inp.emit_error(Spanned::new(
      forged,
      Errs::from(DialectError::new(forged, ErrorData::TokenBudgetExhausted)),
    ));
    let ordinary = SimpleSpan::new(1, 2);
    let _ = inp.emit_error(Spanned::new(
      ordinary,
      Errs::from(DialectError::new(ordinary, ErrorData::FloatOverflow("x"))),
    ));
    inp.skip_while(|_| true)?;
    Ok(())
  }

  /// (g)/(h) One emission carrying **both** the door's variant and an ordinary error.
  ///
  /// Codex round 8, and it is a precision defect inside round 8's own repair rather than a new
  /// class. Both dialects' payloads are `Vec`-backed multi-error containers, so a root can put
  /// `[TokenBudgetExhausted, FloatOverflow]` into ONE `Spanned` — and a filter that decided at the
  /// payload threw the `FloatOverflow` away with the budget member. (e) and (f) miss it because
  /// they emit the two in separate calls.
  ///
  /// The container is emitted at the ORDINARY error's span, so the surviving diagnostic's span is
  /// the reading that says which member kept it alive.
  fn mixed_emission<'inp>(inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>) {
    let ordinary = SimpleSpan::new(1, 2);
    let mut mixed = Errs::from(DialectError::new(
      SimpleSpan::new(0, 0),
      ErrorData::TokenBudgetExhausted,
    ));
    mixed.extend(core::iter::once(DialectError::new(
      ordinary,
      ErrorData::FloatOverflow("x"),
    )));
    let _ = inp.emit_error(Spanned::new(ordinary, mixed));
  }

  /// (g) The mixed emission with **no** refusal, over a document the parse completes.
  fn mixed_without_refusal<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), Errs<'inp>> {
    mixed_emission(inp);
    inp.skip_while(|_| true)?;
    Ok(())
  }

  /// (h) The same emission **with** a real refusal.
  fn mixed_with_refusal<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), Errs<'inp>> {
    mixed_emission(inp);
    inp.skip_while(|_| true)?;
    Ok(())
  }

  /// (i)/(j) A root that emits an **empty** container — smear issue #193, Codex round 9.
  ///
  /// Both dialect `Errors` types implement `Default`, and tokora records whatever payload it is
  /// handed. `any` over an empty `Vec` is `false`, so round 9's classifier read `Errors::default()`
  /// as "nothing but the door's variant" and dropped the record; with no refusal to replace it the
  /// finished parse had no diagnostics and `has_errors()` was false.
  ///
  /// **Which channels the finished `Parse` carries**, because the cell only means something if the
  /// answer is written down: all three that `Verbose::diagnostics()` yields — errors, warnings and
  /// recovery holes. The first two carry a payload and go through the classifier, so both are
  /// covered here. A hole carries none and is never classified at all; the substrate's
  /// `is_none_or` is what says so and it has said so since round 8.
  fn emits_empty<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), Errs<'inp>> {
    let _ = inp.emit_error(Spanned::new(SimpleSpan::new(3, 4), Errs::default()));
    inp.skip_while(|_| true)?;
    Ok(())
  }

  /// (j) The same on the **warning** channel.
  fn emits_empty_warning<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), Errs<'inp>> {
    let _ = inp.emit_warning(Spanned::new(SimpleSpan::new(5, 6), Errs::default()));
    inp.skip_while(|_| true)?;
    Ok(())
  }

  /// Every diagnostic the finished parse carries, as `(start, end, is_error)`, and what
  /// `has_errors()` answers.
  ///
  /// The second reading is the one Codex round 8's finding is about: a dropped payload does not
  /// only lose a diagnostic, it can turn a document that HAS an error into one that reports none.
  fn output<'inp, R>(
    src: &'inp str,
    ceiling: usize,
    root: R,
  ) -> (std::vec::Vec<(usize, usize, bool)>, bool)
  where
    R: for<'c> FnOnce(
      &mut InputRef<'inp, 'c, Lx<'inp>, Ctx<'inp>, GraphQL>,
      &mut RootStop,
    ) -> Result<(), Errs<'inp>>,
  {
    let limits = LosslessLimits::default();
    let parse = crate::graphql::lossless::runner::parse_lossless_document(
      src,
      limits.with_max_produce_events(ceiling),
      root,
    );
    let spans = parse
      .diagnostics()
      .iter()
      .map(|d| {
        (
          d.span().start,
          d.span().end,
          d.severity() == tokora::emitter::Severity::Error,
        )
      })
      .collect();
    //  is a TRAIT in tokora and an inherent method here; the inherent one is what a
    // consumer calls, so it is what this reads.
    let has_errors = parse.has_errors();
    (spans, has_errors)
  }

  /// One parse through the door, and what survived it.
  fn drive<'inp, R>(src: &'inp str, ceiling: usize, root: R) -> (usize, Option<bool>)
  where
    R: for<'c> FnOnce(
      &mut InputRef<'inp, 'c, Lx<'inp>, Ctx<'inp>, GraphQL>,
      &mut RootStop,
    ) -> Result<(), Errs<'inp>>,
  {
    INNER_WAS_TERMINAL.with(|c| c.set(None));
    let limits = LosslessLimits::default();
    let parse = crate::graphql::lossless::runner::parse_lossless_document(
      src,
      limits.with_max_produce_events(ceiling),
      root,
    );
    (
      budget_reports(&parse),
      INNER_WAS_TERMINAL.with(StdCell::get),
    )
  }

  let src = "a b c d e f g h i j k l m n o p q r s t ".repeat(2);

  println!("\n== who owns the report ==");
  println!("  {:<22} {:>8} {:>16}", "case", "budget", "inner-terminal");

  //
  // EVERY CASE RUNS BEFORE ANY IS JUDGED. Asserting inside the loop aborts at the first bad one
  // and hides the other three, and the four are one reading: two of them are the two DIRECTIONS
  // the inference fails in, and a run that shows only one of them cannot say that.
  macro_rules! case {
    ($label:literal, $root:expr) => {{
      let (budget, inner_terminal) = drive(&src, 4, $root);
      println!("  {:<22} {budget:>8} {inner_terminal:>16?}", $label);
      ($label, budget, inner_terminal)
    }};
  }

  let measured = [
    case!("a direct", direct),
    case!("b attempt/rollback", attempt_propagated),
    case!("c attempt/commit", attempt_committed),
    case!("d caught+replaced", caught_and_replaced),
  ];

  for (label, _budget, inner_terminal) in measured {
    assert_eq!(
      inner_terminal,
      Some(true),
      "{label}: the inner frame handed back a NON-terminal value over a refused input. The stop \
       is what every composition above it reads, and a stop that is not terminal is a document \
       that carries on"
    );
  }
  // ── (e) CATCH AND EMIT, and (f) FORGE WITH NO REFUSAL — smear issue #193, Codex round 7 ──
  //
  // These two are not about who can obtain the capability; they are about the grammar writing the
  // same channel the door writes. What answers them is that the door DECIDES at the end.
  //
  // BOTH RUN BEFORE EITHER IS JUDGED, for the reason the four above give: they are the two
  // directions of one finding, and a run that aborts at the first shows only one of them.
  let (e, _) = output(&src, 4, catches_and_emits);
  let (f, _) = output(&src, 10_000, forges_without_refusal);
  let (g, g_has_errors) = output(&src, 10_000, mixed_without_refusal);
  let (h, _) = output(&src, 4, mixed_with_refusal);
  let (i, i_has_errors) = output(&src, 10_000, emits_empty);
  let (j, j_has_errors) = output(&src, 10_000, emits_empty_warning);
  let e_budget: std::vec::Vec<_> = e.iter().filter(|(a, b, err)| a == b && *err).collect();
  println!("  e catch+emit           {e:?}");
  println!("  f forge, no refusal    {f:?}");
  assert_eq!(
    e_budget.len(),
    1,
    "(e) a root that caught the nested stop and emitted it left {} budget diagnostics in the \
     output. Before round 8 this was 2 — the grammar's and the door's — and no fence on who may \
     CONSTRUCT the report could have prevented it, because the root was handed the value by the \
     frame below it: {e:?}",
    e_budget.len()
  );
  assert!(
    e_budget[0].0 > 0,
    "(e) the surviving budget diagnostic is at {:?}, which is where the ROOT emitted its copy \
     (0..0) rather than where the door's report sits (the committed end). The normalisation kept \
     the grammar's and dropped the door's, which is the decision inverted",
    (e_budget[0].0, e_budget[0].1)
  );

  assert_eq!(
    f.iter().filter(|(a, b, err)| a == b && *err).count(),
    0,
    "(f) a parse whose budget never refused came back carrying a budget diagnostic. Before round 8 \
     this was 1: a root can construct the dialect's own variant and emit it, marking a complete \
     parse as truncated. The door's verdict is the only thing entitled to put one there: {f:?}"
  );
  assert_eq!(
    f,
    std::vec![(1, 2, true)],
    "(f) the normalisation removed something other than the forged budget variant. The ordinary \
     `FloatOverflow` at 1..2 the same root emitted must survive untouched — dropping ONLY that one \
     variant is the claim, and a filter that took more would be a different and worse repair"
  );

  // ── (g) AND (h) THE MIXED CONTAINER — smear issue #193, Codex round 8 ──────────────────────
  println!("  g mixed, no refusal    {g:?}  has_errors={g_has_errors}");
  println!("  h mixed, with refusal  {h:?}");
  assert_eq!(
    g,
    std::vec![(1, 2, true)],
    "(g) one emission carrying [TokenBudgetExhausted, FloatOverflow] and NO refusal came back as \
     {g:?}. Before round 9 it was empty: the filter decided at the payload, so the ordinary error \
     went out with the budget member. The container is the grammar's unit of emission and only \
     the variant is the door's"
  );
  assert!(
    g_has_errors,
    "(g) `has_errors()` is false on a document that emitted a `FloatOverflow`. That is the \
     consequence that matters — not a diagnostic missing from a list, but a parse with a real \
     error reporting itself clean"
  );
  assert_eq!(
    h,
    std::vec![(1, 2, true), (4, 4, true)],
    "(h) the same emission WITH a refusal came back as {h:?} rather than the surviving \
     `FloatOverflow` at its own span followed by the door's report at the refusal point. Before \
     round 9 it was the door's alone: the ordinary error had been dropped and only the \
     replacement remained, which reads as one error where there are two"
  );

  // ── (i) AND (j) THE EMPTY CONTAINER — smear issue #193, Codex round 9 ─────────────────────
  println!("  i empty, error chan    {i:?}  has_errors={i_has_errors}");
  println!("  j empty, warning chan  {j:?}  has_errors={j_has_errors}");
  assert_eq!(
    i,
    std::vec![(3, 4, true)],
    "(i) a root emitted `Errors::default()` with no refusal and the finished parse came back as \
     {i:?}. Before round 10 it was empty: `any` over an empty container is `false`, so the \
     classifier read it as nothing-but-the-door's-variant and dropped a record that holds no \
     member of the door's at all"
  );
  assert!(
    i_has_errors,
    "(i) `has_errors()` is false on a parse that recorded an error diagnostic. An empty payload is \
     still a record on the error channel, and dropping it is the same `has_errors()` consequence \
     round 9 closed for the mixed container"
  );
  assert_eq!(
    j,
    std::vec![(5, 6, false)],
    "(j) the same on the WARNING channel came back as {j:?}. The classifier runs over every \
     payload `Verbose::diagnostics()` yields, and a warning carries one, so the empty case has to \
     hold on both channels or it holds on neither"
  );
  assert!(
    !j_has_errors,
    "(j) `has_errors()` is true on a parse whose only record is a warning. It counts \
     `Severity::Error` alone, and a warning surviving must not change that — this is the control \
     that says (j) measured the warning channel and not the error one"
  );

  let counts = measured.map(|(_, budget, _)| budget);
  assert_eq!(
    counts,
    [1, 1, 1, 1],
    "budget diagnostics that SURVIVED, by case (a direct, b attempt/rollback, c attempt/commit, \
     d caught+replaced): {counts:?}. Terminality is a fact about a VALUE and a report is an entry \
     in a LOG. `try_attempt` restores the emitter and the poison boundary and leaves \
     `refused_an_item` set, so a `0` in (b) is a rolled-back report a frame credited itself with; \
     a `2` in (d) is a surviving report a frame could not see. smear issue #193, Codex round 3"
  );
}

/// A nested door reports into **its own** parse, and not into the enclosing one — smear issue
/// #193, Codex round 5.
///
/// # The defect
///
/// The door used to accept any caller emitter under `Emitter + ValueKeyedEmitter`. That marker
/// constrains checkpoint semantics, not exclusive ownership: an in-crate caller could write a
/// perfectly value-keyed collector over shared state, hand one handle to the outer door and
/// capture another for a nested one, and the two parses — distinct `Input`s, distinct `Sink`s —
/// would forward into **one diagnostic timeline**. Two indistinguishable budget reports in the
/// enclosing result, which contradicts the door's own `Nesting this function is not a forgery`
/// note in its own terms.
///
/// Measured on `1ec827c`, with a composed root calling the door again under a second `Counting`
/// whose reports land in a thread-local: **2**. After the repair the door builds
/// `Verbose::default()` itself, so a shared collector cannot be expressed at all — there is no
/// parameter to pass one to and no type variable to instantiate — and this cell reads **1**.
///
/// # What it asserts
///
/// The outer parse's log holds exactly one budget report, and the inner parse's log holds its own
/// one. Both halves matter: the first says the enclosing timeline is untouched, and the second
/// says the nested parse still reported — so the reading is *separation*, not *suppression*.
#[test]
fn a_nested_door_reports_into_its_own_parse_and_not_the_enclosing_one() {
  use core::cell::Cell as StdCell;

  use crate::{
    graphql::{
      GraphQL,
      lossless::{GraphqlLosslessErrors, GraphqlLosslessLexer, runner::LosslessEmitter},
    },
    lossless::depth::RootStop,
  };
  use smear_lexer::limits::LosslessLimits;
  use tokora::{InputRef, cache::DefaultCache, cst::Sink};

  type Lx<'inp> = GraphqlLosslessLexer<'inp, str>;
  type Ctx<'inp> = (
    Sink<'inp, Lx<'inp>, LosslessEmitter<'inp>>,
    DefaultCache<'inp, Lx<'inp>>,
  );
  type Errs<'inp> = GraphqlLosslessErrors<&'inp str>;

  thread_local! {
    /// What the NESTED parse's own log held. A value carried out of the inner door, not a
    /// diagnostic channel two parses write to — which is the distinction the whole cell is about.
    static NESTED_REPORTS: StdCell<usize> = const { StdCell::new(0) };
  }

  fn budget_reports(parse: &crate::graphql::lossless::runner::Parse) -> usize {
    // WHAT THE OUTPUT SAYS, and the door hands it over directly — smear issue #193, rounds 7 and
    // 8. The door no longer emits: it builds its verdict, drops every budget diagnostic the
    // grammar emitted, appends its own, and returns the finished `Parse`. Neither the `Cst` nor
    // the verdict is in a caller's hands, so a cell about the COUNT has nothing to assemble and
    // nothing it could assemble wrongly.
    //
    // A zero-width `Error` is the door's report's shape, and each cell carries the control that
    // says so: the same parse with nothing refused has none.
    parse
      .diagnostics()
      .iter()
      .filter(|d| {
        d.severity() == tokora::emitter::Severity::Error && d.span().start == d.span().end
      })
      .count()
  }

  /// Reads to the end, so the budget refuses inside whichever parse runs it.
  fn reads_everything<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), Errs<'inp>> {
    inp.skip_while(|_| true)?;
    Ok(())
  }

  /// The composed root: it calls the DOOR again, on its own source, from inside the outer parse.
  fn nests_a_door<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    stop: &mut RootStop,
  ) -> Result<(), Errs<'inp>> {
    let limits = LosslessLimits::default();
    let parse = crate::graphql::lossless::runner::parse_lossless_document(
      "a b c d e f g h i j ",
      limits.with_max_produce_events(2),
      reads_everything,
    );
    NESTED_REPORTS.with(|n| n.set(budget_reports(&parse)));
    // And then the outer parse's own root does its own reading, so the outer budget refuses too.
    reads_everything(inp, stop)
  }

  NESTED_REPORTS.with(|n| n.set(0));
  let src = "a b c d e f g h i j k l m n o p q r s t ".repeat(2);
  let limits = LosslessLimits::default();
  let parse = crate::graphql::lossless::runner::parse_lossless_document(
    &src,
    limits.with_max_produce_events(4),
    nests_a_door,
  );
  let outer = budget_reports(&parse);
  let nested = NESTED_REPORTS.with(StdCell::get);

  println!("\n== a nested door's report goes to its own log ==");
  println!("  outer log: {outer}   nested log: {nested}");

  assert_eq!(
    outer, 1,
    "the enclosing parse's log holds {outer} budget reports. Two is Codex round 5's finding: a \
     nested door forwarding into the same diagnostic timeline as the parse it runs inside. It \
     cannot be expressed any more — the door builds its own `Verbose` — so anything but one here \
     is the door emitting more than once for one refusal"
  );
  assert_eq!(
    nested, 1,
    "the NESTED parse's own log holds {nested} budget reports rather than one. This half is why \
     the cell is about separation and not suppression: a nested parse that refused must still say \
     so, in its own result"
  );
}
