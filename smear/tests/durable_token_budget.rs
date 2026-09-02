#![cfg(all(
  feature = "rowan",
  feature = "parser",
  feature = "test-support",
  feature = "std",
  any(feature = "graphql", feature = "graphqlx")
))]

//! `LosslessLimits::max_produce_events` — the **durable** ceiling on the items a lossless parse
//! produces, and the one no rollback refunds. smear issue #193.
//!
//! # The defect this is the gate for
//!
//! Before this file existed the only token ceiling a caller could set was `max_tokens`, which is
//! enforced from the lexer's own tally — a field of `LosslessLimits`, which is `Lexer::State`,
//! which lossless recovery's `sync_balanced` **restores** on its no-match exit. A failed recovery
//! scan therefore refunded every charge it had made, so the ceiling was reached only by lexemes
//! that *survived* and a lexeme crossed by eight failed scans was charged once. What actually
//! bounded the parse was smear issue #168's scan allowance, at `8 * committed + 4096`.
//!
//! Measured at `fcd7f5e`, which is the number the issue is about: `[ type ] ` repeated 2 000 times
//! is 12 000 lexical items, `with_max_tokens(12_000)` **completed** on it, and the parse produced
//! **99 963** items. A documented ceiling of 12 000, reached at 8.33 times its own number.
//!
//! The repair installs tokora's input-layer `TokenBudget` at every lossless door. That cell is
//! charged before the lexer runs, lives outside every `Checkpoint`, and has no public mutator — so
//! a rollback cannot refund it. **The property comes from the location, not from the direction of
//! the increment.**
//!
//! # Why a second knob and not a re-pointed one
//!
//! The issue put both on the table and named the hazard in the second: two knobs that both sound
//! like ceilings. They are two anyway, because they count two things. `max_tokens` counts lexemes
//! a rewind kept — *how much document do I want looked at* — and `max_produce_events` counts every
//! item the lexer handed back, a re-lex included — *how much work may this document buy*. On the
//! document above they disagree by three orders of magnitude, which
//! [`a_ceiling_refuses_the_parse_whose_work_exceeds_it`] measures on both arms.
//!
//! Re-pointing would also have made one public name mean two different things at the two surfaces
//! that honour it: a bare `Lexer::with_state` has no `Input` and therefore no durable tally at
//! all, and `lossless_ceiling_doors.rs` is the shipped gate on `max_tokens` at exactly that
//! surface. `resync_allowance.rs`'s `max_tokens_does_not_bound_the_work_the_scan_allowance_does`
//! is the other half of the pair and stands unedited: its subject — the ceiling that is *not* a
//! work bound, and the guard that bounds an unbudgeted parse — did not move.
//!
//! # The three things this file pins
//!
//! - **the door population.** Six shipped `parse_*_with_limits` entry points, and a budget is only
//!   a budget if every one of them installs it. `lossless_context` is the single funnel and adding
//!   its fourth argument broke all six loudly — but a door added later can call it and pass
//!   `usize::MAX`, which compiles. No type closes that: both numbers are `usize`s because the type
//!   that would tell them apart lives in `smear-lexer`, which the dialect-generic substrate may
//!   not name. This file is the population gate instead;
//! - **the terminal diagnostic.** tokora refuses the item **silently** — its own docs say the
//!   refusal *cannot report itself* — so without a smear-minted report a refused document comes
//!   back as a `Parse` with a truncated tree, a gap-tiled tail and `has_errors()` answering
//!   `false`, which is what a document that parsed looks like. One diagnostic, and no tail scan
//!   behind it;
//! - **that it refuses the work and not the document.** Both arms, on one document and one
//!   ceiling.

use smear::{lexer::limits::LosslessLimits, parser::lossless::recover::scan_allowance};

/// What one budgeted parse is reduced to.
struct Run {
  /// Tokens in the materialised tree — the lexemes the parse actually kept.
  items: usize,
  /// Diagnostics on the channel.
  diagnostics: usize,
  /// Whether any of them is an error rather than a warning or a recovery hole.
  errors: bool,
  /// Bytes the tree covers. A lossless tree always covers the whole source; a refusal's tail
  /// arrives as a gap run rather than as committed tokens, so this stays equal to `src.len()` and
  /// is asserted rather than read for information.
  covered: usize,
  /// `TokenBudgetTally::spent` at the last recovery call — the durable produce-event count.
  spent: usize,
  /// The **lexer's** own tally at the same call — the rewindable one, and the whole finding is
  /// that these two are different numbers.
  committed: usize,
  /// How many diagnostics carry `report_token_budget`'s own shape: a **zero-width error**.
  ///
  /// The public `Parse` drops the typed payload — `Diagnostic` is `span`, `severity` and
  /// `skipped_tokens` and nothing else (`smear-parser/src/lossless/runner.rs:22`) — so a cell at
  /// a shipped door cannot ask a diagnostic which variant it is. What it can ask is the shape,
  /// and the refusal has one no grammar error in this file's corpus shares: `report_token_budget`
  /// mints at `SimpleSpan::new(end, end)` where `end` is the committed end, because tokora
  /// publishes no span for the item it dropped.
  ///
  /// **The attribution is a measurement, not an assumption.** Every cell that reads this field
  /// first reads it on the *unbudgeted* parse of the same document at the same door and requires
  /// a `0` there. A document that mints a zero-width error of its own therefore reddens the
  /// control rather than passing the claim off as the refusal.
  budget_reports: usize,
  /// Every other diagnostic, as `(start, end)`.
  ///
  /// Compared against the unbudgeted parse's list to answer *did the root run to its own end and
  /// report what it always reports* — which is what separates a refusal the root's own return
  /// carried from one whose first occurrence was in the tail drain behind it.
  others: std::vec::Vec<(usize, usize)>,
}

/// One shipped lossless door, under a token ceiling or under none.
macro_rules! doors {
  ($($name:ident => $path:path),+ $(,)?) => {
    $(
      /// `ceiling` is the durable produce-event budget and `tokens` the rewindable lexer
      /// ceiling. Both are optional and independent, which is what
      /// [`the_two_ceilings_compose_and_the_tighter_one_governs`] needs.
      fn $name(src: &str, ceiling: Option<usize>, tokens: Option<usize>) -> Run {
        let limits = LosslessLimits::default();
        let limits = match ceiling {
          Some(max) => limits.with_max_produce_events(max),
          None => limits,
        };
        let limits = match tokens {
          Some(max) => limits.with_max_tokens(max),
          None => limits,
        };
        scan_allowance::reset();
        let parse = $path(src, limits);
        Run {
          items: parse
            .syntax()
            .descendants_with_tokens()
            .filter(|element| element.as_token().is_some())
            .count(),
          diagnostics: parse.diagnostics().len(),
          errors: parse.has_errors(),
          covered: parse.syntax().text().to_string().len(),
          spent: scan_allowance::peak_spent(),
          committed: scan_allowance::peak_committed(),
          budget_reports: parse
            .diagnostics()
            .iter()
            .filter(|d| {
              d.severity() == tokora::emitter::Severity::Error && d.span().start == d.span().end
            })
            .count(),
          others: parse
            .diagnostics()
            .iter()
            .filter(|d| {
              !(d.severity() == tokora::emitter::Severity::Error && d.span().start == d.span().end)
            })
            .map(|d| (d.span().start, d.span().end))
            .collect(),
        }
      }
    )+
  };
}

#[cfg(feature = "graphql")]
doors! {
  gql_mixed      => smear::parser::graphql::lossless::parse_document_with_limits,
  gql_sdl        => smear::parser::graphql::lossless::parse_type_system_document_with_limits,
  gql_executable => smear::parser::graphql::lossless::parse_executable_document_with_limits,
}

#[cfg(feature = "graphqlx")]
doors! {
  gqlx_mixed      => smear::parser::graphqlx::lossless::parse_document_with_limits,
  gqlx_sdl        => smear::parser::graphqlx::lossless::parse_type_system_document_with_limits,
  gqlx_executable => smear::parser::graphqlx::lossless::parse_executable_document_with_limits,
}

/// Every shipped lossless door, labelled.
type Door = (&'static str, fn(&str, Option<usize>, Option<usize>) -> Run);

/// How many doors there are, stated here so a door that stops being listed reddens rather than
/// silently leaving the population.
///
/// Three roots — mixed, SDL-only, executable-only — in each of the two dialects. It is derived
/// from the tree the same way `ci/source_census/src/roots.rs` derives its caller set: by naming
/// each one. The count is checked against the enabled dialect set below so a single-dialect build
/// is not silently a three-cell run against a six-cell claim.
#[allow(clippy::vec_init_then_push)]
fn doors() -> Vec<Door> {
  let mut doors: Vec<Door> = Vec::new();
  #[cfg(feature = "graphql")]
  {
    doors.push(("gql/mixed", gql_mixed));
    doors.push(("gql/sdl", gql_sdl));
    doors.push(("gql/executable", gql_executable));
  }
  #[cfg(feature = "graphqlx")]
  {
    doors.push(("gqlx/mixed", gqlx_mixed));
    doors.push(("gqlx/sdl", gqlx_sdl));
    doors.push(("gqlx/executable", gqlx_executable));
  }
  let dialects = usize::from(cfg!(feature = "graphql")) + usize::from(cfg!(feature = "graphqlx"));
  assert_eq!(
    doors.len(),
    3 * dialects,
    "the door population is three roots per enabled dialect; a door that left this list is a door \
     no cell below is about"
  );
  doors
}

/// The document the issue measured, and the ceiling equal to its own lexeme count.
///
/// `[ type ] ` drives `resync_to` — its `type` is a definition head at depth 1 — and is the shape
/// #193's 8.33 was read off. Six lexemes per repetition.
const SHAPE: &str = "[ type ] ";
const REPS: usize = 2_000;
const ITEMS: usize = REPS * 6;

/// **Every** lossless door installs the durable budget, and a ceiling below the document's own
/// size truncates the parse at each one.
///
/// # Why the assertion is on the tree's token count
///
/// It is the one reading that is a fact about *lexing* rather than about a dialect's grammar. The
/// three roots disagree about this document — the mixed and SDL roots take a `type`, the
/// executable root reports it — so a diagnostic count differs per door for reasons that have
/// nothing to do with a budget. A token cannot be in the tree without having been lexed, so the
/// same claim reads at all six.
///
/// **`ceiling + 1`, and the `+ 1` is not slack.** The unread tail is tiled by
/// `Cst::finish_partial` as a **gap run**, and a gap run is one token in the green tree — so a
/// refused parse carries the lexemes it committed plus exactly one token that was never lexed.
/// Measured: `ceiling + 1` at 3 000 and at 100, and `4 + 1` where the first recovery scan spends
/// a 12 000 budget outright. The unbudgeted control below has no gap and reads the document's own
/// lexeme count, which is what says the `+ 1` belongs to the refusal rather than to the tree.
///
/// # The plant
///
/// Change one door's `lossless_context` call to pass `usize::MAX` instead of
/// `limits.max_produce_events()` — the one shape that compiles and is wrong — and that door alone
/// reddens, naming itself. Removing the argument entirely does not reach this cell, because it
/// does not compile.
#[test]
fn every_lossless_door_installs_the_durable_budget() {
  let src = SHAPE.repeat(REPS);
  let ceiling = ITEMS / 4;

  println!("\n== every door under a durable ceiling ==");
  println!(
    "  {:<16} {:>8} {:>8} {:>6} {:>9} {:>10}",
    "door", "ceiling", "items", "diags", "errors", "covered"
  );
  for (label, door) in doors() {
    let unbudgeted = door(&src, None, None);
    assert_eq!(
      unbudgeted.items, ITEMS,
      "{label}: the unbudgeted tree carries {} tokens where the document has {ITEMS} lexemes, so \
       the ceiling below is not a fraction of the document's own size",
      unbudgeted.items
    );

    let got = door(&src, Some(ceiling), None);
    assert!(
      got.items <= ceiling + 1,
      "{label}: max_produce_events({ceiling}) let {} tokens into the tree, past the {} a refused \
       parse can hold — the lexemes the ceiling paid for, plus the one gap-run token covering the \
       tail nothing lexed. A token cannot be in the tree without having been lexed, so this door \
       produced past the ceiling: it is not installing the caller's number at `lossless_context`",
      got.items,
      ceiling + 1
    );
    assert!(
      got.items < unbudgeted.items,
      "{label}: the budgeted tree is the same size as the unbudgeted one ({}), so nothing was \
       refused and the bound above is satisfied by a parse that ignored the ceiling",
      unbudgeted.items
    );
    assert!(
      got.errors,
      "{label}: a refused document reported no error. tokora refuses the item silently, so the \
       diagnostic `drain_unless_stopped` mints through `FromTokenBudget` is the only thing that \
       distinguishes this parse from one that succeeded"
    );
    assert_eq!(
      got.covered,
      src.len(),
      "{label}: the tree stopped covering the source. A refusal's tail is tiled as a gap run by \
       `finish_partial`, so truncating the parse must not truncate the tree's text"
    );
    println!(
      "  {label:<16} {ceiling:>8} {:>8} {:>6} {:>9} {:>10}",
      got.items, got.diagnostics, got.errors, got.covered
    );
  }
}

/// A refused document reports **one** diagnostic, and nothing reads the tail behind it.
///
/// # Why one, and why that is the whole cell
///
/// tokora emits a diagnostic for every lexer error a scan crosses, so any drain over an unread
/// tail turns one refusal into `1 + n` — the amplification smear issue #169 closed at the descent
/// budget and `drain_unless_terminal` measures at 1, 2, 5, 17 and 65 for tails of 0, 1, 4, 16 and
/// 64 invalid lexemes. The refusal is one diagnostic only if nothing reads the tail, and what
/// stops the read is `ErrorData::TokenBudgetExhausted`'s `MaybeTerminal` arm together with
/// `drain_unless_stopped` returning before the drain.
///
/// The document is chosen so that **nothing else can report**: a prefix of valid definitions the
/// dialect's mixed root takes without a murmur, then a tail of `n` lexemes that do not lex. Under
/// a ceiling inside the valid prefix the only diagnostic that can exist is the refusal, and the
/// `n` axis is what says the tail was never crossed.
///
/// # The plant
///
/// Deleting the early return in `drain_unless_stopped` — leaving the emission — turns the tail
/// axis from a flat `1` into `1 + n`.
#[test]
fn a_refused_document_reports_one_terminal_diagnostic_and_reads_no_tail() {
  // 12 lexemes each, and every one of them lexes and parses cleanly under both dialects' mixed
  // root: `type Tn { f: Int }` is `type`, name, `{`, `f`, `:`, `Int`, `}` — seven — so a prefix of
  // 400 of them is comfortably past any ceiling this cell sets.
  let prefix: String = (0..400)
    .map(|n| format!("type T{n} {{ f: Int }}\n"))
    .collect();

  println!("\n== one diagnostic, and no tail behind it ==");
  println!(
    "  {:<16} {:>6} {:>8} {:>6}",
    "door", "tail", "items", "diags"
  );
  for (label, door) in doors() {
    // The executable-only root reports every one of these definitions, so it is not a door at
    // which "the only diagnostic that can exist is the refusal". The mixed and SDL roots both
    // take them.
    if label.ends_with("/executable") {
      continue;
    }
    let clean = door(&prefix, None, None);
    assert_eq!(
      clean.diagnostics, 0,
      "{label}: the prefix is meant to parse silently, and it reported {} diagnostics — every \
       count below would then be measuring those instead of the refusal",
      clean.diagnostics
    );

    for tail in [0usize, 1, 4, 16, 64] {
      let src = format!("{prefix}{}", "~ ".repeat(tail));
      let got = door(&src, Some(100), None);
      assert_eq!(
        got.diagnostics, 1,
        "{label}: a refusal over a tail of {tail} lexemes that do not lex reported {} \
         diagnostics. One means nothing read the tail; `1 + n` means the drain ran over it, which \
         is smear issue #169's amplification reached at the other resource",
        got.diagnostics
      );
      assert!(
        got.items <= 101,
        "{label}: the ceiling let {} tokens into the tree, past the 100 it paid for plus the one \
         gap-run token that covers the tail",
        got.items
      );
      println!(
        "  {label:<16} {tail:>6} {:>8} {:>6}",
        got.items, got.diagnostics
      );
    }
  }
}

/// A ceiling refuses exactly the parse whose **work** exceeds it, and the same document decides
/// both ways depending on what it costs.
///
/// # The two readings, and why both are needed
///
/// `spent` is `TokenBudgetTally::spent` — every item the lexer produced, a re-lex included, in a
/// cell no rollback refunds. `committed` is the **lexer's** own tally at the same recovery call —
/// the rewindable one, which `sync_balanced` restores. Both are sampled by
/// `scan_allowance::record`, at recovery decision points only, so each is a lower bound on this
/// parse's total rather than the total itself; what they are exact about is the **gap between
/// them**, because they are read one after the other at the same call.
///
/// Measured on `[ type ] ` x2000 — 12 000 lexical items — with no ceiling:
///
/// | door | spent | committed | ratio |
/// |---|---|---|---|
/// | mixed, both dialects | 99 963 | 11 998 | **8.332** |
/// | SDL-only, both dialects | 99 963 | 11 998 | **8.332** |
/// | executable-only, both dialects | 11 999 | 11 998 | **1.000** |
///
/// That is the whole of smear issue #193 in one table. The first two rows recover through
/// `resync_to`, whose `sync_balanced` scan rewinds and refunds the lexer's tally while the input
/// layer keeps every produce-event; the third reports each definition at its head and resyncs
/// without scanning, so the two counters stay together.
///
/// # Which is why this cell has both arms, and would be worth much less with one
///
/// A ceiling of 12 000 — the document's own lexeme count — **refuses** the first two and does
/// **not** refuse the third. Same document, same number, opposite answers, and the thing that
/// differs is how much work the parse does. A cell that only ran the refusing arm would be equally
/// satisfied by a budget that refuses everything; a cell that only ran the other would be
/// satisfied by one that refuses nothing. The population each arm gets is derived from the
/// unbudgeted reading rather than from a door's name, and both are asserted non-empty.
#[test]
fn a_ceiling_refuses_the_parse_whose_work_exceeds_it() {
  let src = SHAPE.repeat(REPS);
  let mut refused = 0usize;
  let mut completed = 0usize;

  println!("\n== the durable ceiling against the work it bounds ==");
  println!(
    "  {:<16} {:>9} {:>11} {:>7} {:>10} {:>10}",
    "door", "spent", "committed", "ratio", "no-cap", "at-ceiling"
  );
  for (label, door) in doors() {
    let unbudgeted = door(&src, None, None);
    assert_eq!(
      unbudgeted.items, ITEMS,
      "{label}: the unbudgeted tree carries {} tokens where the document has {ITEMS} lexemes, so \
       the ceiling below is not the document\'s own size",
      unbudgeted.items
    );
    assert!(
      unbudgeted.spent >= unbudgeted.committed,
      "{label}: `spent` ({}) fell below `committed` ({}). They are read one after the other at \
       the same recovery call and the durable count includes every item the rewindable one \
       kept, so this ordering cannot fail without one of the two being read from the wrong cell",
      unbudgeted.spent,
      unbudgeted.committed
    );

    let got = door(&src, Some(ITEMS), None);
    assert!(
      got.items <= ITEMS + 1,
      "{label}: the tree carries {} tokens under a ceiling of {ITEMS}. The gate is `spent >= max` \
       in front of the lexer, so nothing can be produced past it — the one token above the \
       ceiling is the gap run over the tail",
      got.items
    );

    // WHICH ARM THIS DOOR IS IN IS DERIVED, NOT NAMED. `spent` at the last recovery call is a
    // lower bound on the parse's total, so a door that already exceeds the ceiling at that
    // sample exceeds it outright; the two populations sit at 99 963 and 11 999 against a
    // ceiling of 12 000, which is not a boundary either of them is near.
    if unbudgeted.spent > ITEMS {
      refused += 1;
      assert!(
        got.items < unbudgeted.items,
        "{label}: a parse that produced {} items under a ceiling of {ITEMS} came back with the \
         whole document in the tree. The ceiling is on produce-events and this parse spent more \
         than it, so it must have been refused",
        unbudgeted.spent
      );
      assert!(
        got.errors,
        "{label}: a refused document reported no error. tokora refuses the item silently, so the \
         diagnostic `drain_unless_stopped` mints through `FromTokenBudget` is the only thing that \
         distinguishes this parse from one that succeeded"
      );
    } else {
      completed += 1;
      assert_eq!(
        got.items, unbudgeted.items,
        "{label}: a parse whose whole cost is {} produce-events was truncated by a ceiling of \
         {ITEMS}. The budget must refuse the work that exceeds it and nothing else",
        unbudgeted.spent
      );
    }

    println!(
      "  {label:<16} {:>9} {:>11} {:>7.3} {:>10} {:>10}",
      unbudgeted.spent,
      unbudgeted.committed,
      unbudgeted.spent as f64 / unbudgeted.committed.max(1) as f64,
      unbudgeted.items,
      got.items
    );
  }

  assert!(
    refused > 0 && completed > 0,
    "the cell collapsed to one arm — {refused} refused and {completed} completed. Both are the \
     claim: the same document and the same number decide opposite ways on what the parse costs"
  );
}

/// A ceiling of **zero** refuses the first item there is, and refuses nothing where there is none.
///
/// Codex round 1 named this axis because the boundary is where the two questions tokora's driver
/// has to tell apart come together: *would an item be refused* and *is there an item*. At
/// `max_produce_events(0)` the tally is exhausted before the first lex, so every entry takes the
/// cold path — and the answer must still be "there is no item" over an empty source, positionally,
/// without a lexer call and without a diagnostic.
///
/// Getting that wrong in the other direction is the failure tokora's own docs single out: a
/// fully-drained input reported as a terminal stop. Here it would be an empty document coming back
/// with an error on it.
#[test]
fn a_ceiling_of_zero_refuses_the_first_item_and_an_empty_source_has_none() {
  println!("\n== the boundary at zero ==");
  println!(
    "  {:<16} {:>8} {:>8} {:>6} {:>9}",
    "door", "source", "items", "diags", "errors"
  );
  for (label, door) in doors() {
    // A source with something in it: the very first item is refused.
    let got = door("type T { f: Int }", Some(0), None);
    assert_eq!(
      got.diagnostics, 1,
      "{label}: a zero ceiling over a non-empty source reported {} diagnostics rather than the \
       one refusal. tokora runs its one-shot probe here — the ceiling is met before the first \
       lex, so the cold path is the only path this parse takes",
      got.diagnostics
    );
    assert!(
      got.errors,
      "{label}: a zero ceiling refused the document and said so on no channel"
    );
    assert_eq!(
      got.covered,
      "type T { f: Int }".len(),
      "{label}: the tree must still cover every byte — a refusal's tail is a gap run, and at a \
       ceiling of zero the whole document is that tail"
    );
    println!(
      "  {label:<16} {:>8} {:>8} {:>6} {:>9}",
      "17 bytes", got.items, got.diagnostics, got.errors
    );

    // And the control that says the refusal is about an item rather than about the ceiling: with
    // no source there is no item, so a zero ceiling must change nothing.
    //
    // MEASURED AGAINST THE UNBUDGETED PARSE OF THE SAME SOURCE, not against zero. Every root here
    // requires at least one definition, so an empty document is a grammar error and reports one
    // diagnostic with no budget configured at all. Asserting `0` would be asserting the grammar's
    // answer rather than the budget's, and it would pass only for a root that accepts an empty
    // document — which is the axis this control is not about.
    let empty_unbudgeted = door("", None, None);
    let empty = door("", Some(0), None);
    assert_eq!(
      (empty.diagnostics, empty.errors, empty.items),
      (
        empty_unbudgeted.diagnostics,
        empty_unbudgeted.errors,
        empty_unbudgeted.items
      ),
      "{label}: a zero ceiling changed the parse of an EMPTY source. There is no item to refuse — \
       the lex position is already at the end — so tokora's positional step answers before its \
       one-shot probe and nothing is charged. A difference here is a fully-parsed document coming \
       back as a terminal stop, which is the defect that probe exists to avoid"
    );
    println!(
      "  {label:<16} {:>8} {:>8} {:>6} {:>9}",
      "empty", empty.items, empty.diagnostics, empty.errors
    );
  }
}

/// The two ceilings compose, the tighter one governs, and it is the same on every door.
///
/// # Why this axis exists
///
/// `max_tokens` and `max_produce_events` are enforced by different mechanisms in different cells:
/// the first by the logos adapter's post-scan `check()` against the lexer's rewindable tally, the
/// second by tokora's driver in front of the lexer against a tally on the input. Setting both is
/// the configuration a caller who read both doc entries would write, and nothing until now said
/// what it does.
///
/// # What has to hold
///
/// `committed <= spent` always, so the durable gate is reached no later than the rewindable one at
/// equal numbers, and a parse under both can never keep more lexemes than either would allow
/// alone. That is asserted as a bound on the tree rather than as a claim about which mechanism
/// fired — the tree's token count is the one reading that is a fact about lexing at every door,
/// and it is `min(a, b)` plus at most the one gap-run token a refusal's tail is tiled with.
///
/// The grid is deliberately asymmetric on both sides of the diagonal, because a bound that only
/// held where the two agree would be satisfied by either mechanism being ignored.
#[test]
fn the_two_ceilings_compose_and_the_tighter_one_governs() {
  // Six lexemes per repetition, and this shape re-lexes, so `spent` outruns `committed` — which
  // is what makes the two ceilings land in different places rather than together.
  let src = SHAPE.repeat(200);
  let lexemes = 200 * 6;

  println!("\n== both knobs at once ==");
  println!(
    "  {:<16} {:>11} {:>11} {:>8} {:>6} {:>9}",
    "door", "produce", "tokens", "items", "diags", "bound"
  );
  let mut rows = 0usize;
  for (label, door) in doors() {
    for produce in [50usize, 400, 5_000] {
      for tokens in [50usize, 400, 5_000] {
        let got = door(&src, Some(produce), Some(tokens));
        // Neither ceiling can be exceeded, and the tree cannot hold more than the smaller of them
        // plus the single gap-run token that covers whatever went unread.
        let bound = produce.min(tokens).min(lexemes) + 1;
        assert!(
          got.items <= bound,
          "{label}: produce={produce}, tokens={tokens} left {} tokens in the tree, past the \
           {bound} the tighter of the two allows. A token cannot be in the tree without having \
           been lexed, so one of the two ceilings did not fire",
          got.items
        );
        assert_eq!(
          got.covered,
          src.len(),
          "{label}: produce={produce}, tokens={tokens} stopped covering the source"
        );
        assert!(
          got.errors,
          "{label}: produce={produce}, tokens={tokens} truncated a {lexemes}-lexeme document and \
           reported no error"
        );
        println!(
          "  {label:<16} {produce:>11} {tokens:>11} {:>8} {:>6} {:>9}",
          got.items, got.diagnostics, bound
        );
        rows += 1;
      }
    }
  }
  assert_eq!(
    rows,
    9 * doors().len(),
    "the grid collapsed — every door must run all nine combinations"
  );
}

/// Codex round 2's document, and the shape its finding is about: a root that fails **ordinarily**,
/// returns, and leaves a tail for the drain behind it to lex.
///
/// Nine lexemes — `query`, `Q`, `type`, `T`, `{`, `f`, `:`, `Int`, `}`. An executable root takes
/// `query Q`, wants a selection set, is handed `type`, and reports the missing set at `8..12`.
/// That is an ordinary syntax error, so the root's frame returns it and
/// `drain_unless_stopped`'s `Recoverable` arm hands it to `drain_unless_terminal`, whose
/// `skip_while(|_| true)` lexes everything after `type` against the same durable tally.
const DRAIN_DOC: &str = "query Q type T { f: Int }";

/// A refusal whose **first occurrence is inside the tail drain** is reported — smear issue #193,
/// Codex round 2.
///
/// # The defect this is the cell for
///
/// `drain_unless_stopped` took its budget reading once, between the root's return and the arm
/// below it. Two of those arms call `drain_unless_terminal`, and that drain's `skip_while` reads
/// the same input against the same durable tally — so a refusal the drain was the first to take
/// was seen by nothing: tokora answers `Ok` on the terminal `Scan::Tripped`, the frame returned
/// the root's own outcome, and `Cst::finish_partial` tiled the unread tail as a gap run. A
/// truncated parse carrying only an unrelated syntax diagnostic, which is the silent truncation
/// the whole commit exists to close, relocated from the root to the drain.
///
/// # Measured before the repair, on [`DRAIN_DOC`] at `gql/executable`
///
/// | `max_produce_events` | tree tokens | diagnostics | budget refusals named |
/// |---|---|---|---|
/// | 0 – 4 | 1 – 5 | 1 | **1** — the root itself met the ceiling, so the old poll saw it |
/// | **5 – 14** | 6 – 15 | 1 | **0** — the root reported `8..12` and the drain met the ceiling |
/// | 15 – 20 | 16 | 1 | 0, and none is owed: nothing was refused |
///
/// `5` is Codex's own number, and it is the work through the cached `type` token: `query`, the
/// space, `Q`, the space, `type`. Below it the root has not yet reached the token it reports on;
/// at it and above, the root's `8..12` is in the sink and the tree is still short of the
/// document's sixteen.
///
/// # What the derived column says, and what it does not
///
/// `root-end` is *the parse is truncated and its non-refusal diagnostics are exactly the ones the
/// unbudgeted parse reports*. That is **necessary** for the refusal to have been taken after the
/// root returned — a root cut short mid-loop reports less than it always does — and it is not
/// sufficient: on a document this short a root that resynchronises has nothing left to report
/// either, so five of the six doors also read `true` here while the ceiling was in fact met inside
/// their own loop. The column is asserted non-empty at every door so the sweep cannot slide off
/// the ceilings the finding is about; it is not read as a claim about which reader spent the last
/// produce event.
///
/// Two cells carry that claim where it can be made sharply.
/// [`every_door_names_a_refusal_whichever_reader_first_took_it`] runs the same predicate over
/// documents with a sixty-lexeme tail, where a resynchronising root's own diagnostic set grows
/// with the ceiling and the predicate separates cleanly — 44 rows at `gql/executable` and **0** at
/// the other twenty-three (door, document) pairs. And
/// `a_refusal_first_taken_inside_the_drain_is_reported_on_both_draining_arms`, in
/// `smear-parser/src/graphql/lossless/tests.rs`, drives the frame with a root that provably reads
/// nothing, so the drain is the only reader there is.
#[test]
fn a_refusal_first_taken_inside_the_tail_drain_is_reported() {
  let mut drained = 0usize;

  println!("\n== a refusal taken inside the tail drain ==");
  println!(
    "  {:<16} {:>8} {:>8} {:>7} {:>8} {:>8}",
    "door", "ceiling", "items", "budget", "root-end", "diags"
  );
  for (label, door) in doors() {
    let unbudgeted = door(DRAIN_DOC, None, None);
    assert_eq!(
      unbudgeted.budget_reports, 0,
      "{label}: the unbudgeted parse of `{DRAIN_DOC}` already carries {} zero-width errors, so \
       the shape this cell reads as the refusal is one this document mints on its own and every \
       count below is measuring the wrong thing",
      unbudgeted.budget_reports
    );

    let mut here = 0usize;
    for ceiling in 0..=20usize {
      let got = door(DRAIN_DOC, Some(ceiling), None);
      let truncated = got.items < unbudgeted.items;
      let root_reached_its_own_end = got.others == unbudgeted.others;

      assert!(
        got.budget_reports <= 1,
        "{label}: ceiling={ceiling} named the refusal {} times. One refusal is one diagnostic, \
         whichever reader of this frame's input was the first to meet the ceiling",
        got.budget_reports
      );
      if truncated {
        assert_eq!(
          got.budget_reports, 1,
          "{label}: ceiling={ceiling} truncated the parse to {} tokens of {} and named no \
           refusal. tokora refuses the item silently and answers `Ok` on the tripped scan, so a \
           refusal the tail drain was the first to take reaches a consumer as a short tree with \
           an unrelated syntax diagnostic on it — smear issue #193, Codex round 2",
          got.items, unbudgeted.items
        );
        assert_eq!(
          got.covered,
          DRAIN_DOC.len(),
          "{label}: ceiling={ceiling} stopped covering the source. The tail behind a refusal is \
           tiled as a gap run, so truncating the parse must not truncate the tree's text"
        );
      }
      if truncated && root_reached_its_own_end {
        here += 1;
        drained += 1;
      }
      println!(
        "  {label:<16} {ceiling:>8} {:>8} {:>7} {:>8} {:>8}",
        got.items,
        got.budget_reports,
        root_reached_its_own_end,
        got.others.len() + got.budget_reports
      );
    }
    println!("  {label:<16} -> {here} ceilings truncated with the root's own diagnostics complete");
    assert!(
      here > 0,
      "{label}: no ceiling in 0..=20 left this door truncated with its own diagnostics already \
       complete, so the sweep never reaches the region the finding is about. Either the document \
       stopped costing more than 20 produce events or this root stopped reporting before the \
       ceiling — re-derive the range, do not widen it and move on"
    );
  }

  assert!(
    drained > 0,
    "the sweep produced no row at all in which a truncated parse had the root's own diagnostics \
     complete, so every assertion above ran on the region that was already right"
  );
}

/// Every door, every ceiling: a truncated parse names **exactly one** refusal.
///
/// # What this is the population gate for, and why it is not the same claim as the cell above
///
/// The cell above is about one arm at one door. This one asks the question the repair actually
/// has to answer — *is there any (door, document, ceiling) at which a parse comes back short and
/// says nothing* — over a grid, with the arm left underived. A refusal is a refusal whether the
/// root's own return carried it, the drain behind the root took it, or the ceiling was met before
/// the first lex; all three are in the grid and all three must read `1`.
///
/// **`<= 1` is asserted everywhere, not only on the truncated rows.** Two reports for one refusal
/// is the round-2 defect at the other end, and the second poll this round adds is exactly the kind
/// of edit that could reopen it — so the grid pins both directions at once.
///
/// # The documents
///
/// Four shapes with a sixty-lexeme tail behind them, so that a root which returns early leaves
/// the drain real work: the executable root's ordinary failure ([`DRAIN_DOC`]), a bare name no
/// root accepts, a type-system definition the executable roots reject, and an operation the
/// SDL-only roots reject.
///
/// The tail is what makes the `drain-first` column sharp here where it is blunt in the cell above.
/// A root that **resynchronises** past an ordinary failure reads that tail itself and reports a
/// diagnostic for every step of it, so its non-refusal diagnostic set grows with the ceiling and
/// never matches the unbudgeted one; a root that **propagates** returns at the first failure and
/// leaves the whole tail to the drain, so it does match. Measured: 44 rows at
/// `gql/executable`/`exec-fails` and **0** at the other twenty-three pairs.
///
/// That number is a fact about the shipped roots and is deliberately not the assertion. **Five of
/// the six resynchronise** —
/// `grep -rcE '^ +RootTurn::Recoverable . [.][.] . => recover::resync_to_definition'` over
/// `smear-parser/src` lists exactly five code sites (the sixth hit of the looser pattern is
/// `depth::root_turn`'s own doc block, which is why the anchor is there) — and the sixth root,
/// GraphQL's executable one, propagates both failure arms
/// instead: its own divergence, smear issue #168, and the one shipped root that can hand the
/// drain a tail. A cell that required the drain arm at every door would be requiring five roots
/// to change their recovery; what is required of all six is the line above it, that a truncated
/// parse names its refusal.
#[test]
fn every_door_names_a_refusal_whichever_reader_first_took_it() {
  let tail: String = (0..60).map(|n| format!("z{n} ")).collect();
  let documents = [
    ("exec-fails", format!("{DRAIN_DOC} {tail}")),
    ("bare-name", format!("zzz {tail}")),
    ("sdl-only", format!("type T {{ f: Int }} {tail}")),
    ("exec-only", format!("query Q {{ f }} {tail}")),
  ];

  println!("\n== every door, every ceiling ==");
  println!(
    "  {:<16} {:<12} {:>10} {:>10} {:>10}",
    "door", "document", "unbudgeted", "truncated", "drain-first"
  );
  let mut rows = 0usize;
  let mut drain_first_total = 0usize;
  for (label, door) in doors() {
    for (name, src) in &documents {
      let unbudgeted = door(src, None, None);
      assert_eq!(
        unbudgeted.budget_reports, 0,
        "{label}/{name}: the unbudgeted parse already carries a zero-width error, so the shape \
         this grid reads as the refusal is not the refusal"
      );

      let mut truncated_rows = 0usize;
      let mut drain_first = 0usize;
      for ceiling in 0..=48usize {
        let got = door(src, Some(ceiling), None);
        assert!(
          got.budget_reports <= 1,
          "{label}/{name}: ceiling={ceiling} named the refusal {} times. One refusal is one \
           diagnostic — the round-2 property, which the second poll must not reopen",
          got.budget_reports
        );
        if got.items < unbudgeted.items {
          truncated_rows += 1;
          assert_eq!(
            got.budget_reports, 1,
            "{label}/{name}: ceiling={ceiling} came back with {} of {} tokens and named no \
             refusal. A parse this short is a refused parse, and a refused parse that says so on \
             no channel is what `has_errors()` cannot distinguish from one that succeeded",
            got.items, unbudgeted.items
          );
          assert_eq!(
            got.covered,
            src.len(),
            "{label}/{name}: ceiling={ceiling} stopped covering the source"
          );
          if got.others == unbudgeted.others {
            drain_first += 1;
          }
        }
        rows += 1;
      }

      // A ceiling nothing reaches: the control that says every `1` above is about the refusal and
      // not about the shape of these documents.
      let generous = door(src, Some(unbudgeted.items * 10 + 100), None);
      assert_eq!(
        (generous.budget_reports, generous.items),
        (0, unbudgeted.items),
        "{label}/{name}: a ceiling ten times the document's own token count changed the parse. \
         The budget must refuse the work that exceeds it and nothing else"
      );

      println!(
        "  {label:<16} {name:<12} {:>10} {:>10} {:>10}",
        unbudgeted.items, truncated_rows, drain_first
      );
      drain_first_total += drain_first;
    }
  }
  assert!(
    drain_first_total > 0,
    "no (door, document, ceiling) in the grid left a truncated parse whose non-refusal \
     diagnostics were already complete, so the grid contains no row in which the tail drain can \
     have been the reader that met the ceiling — and the assertion above is then a claim about \
     the roots' own polls only. One shipped root propagates an ordinary failure instead of \
     resynchronising; if that stopped being true, this cell has to be re-derived rather than \
     relaxed"
  );
  assert_eq!(
    rows,
    49 * documents.len() * doors().len(),
    "the grid collapsed — every door must run all forty-nine ceilings on all four documents"
  );
}
