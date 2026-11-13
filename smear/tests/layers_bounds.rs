#![cfg(all(
  any(feature = "std", feature = "alloc"),
  feature = "parser",
  any(feature = "graphql", feature = "graphqlx")
))]

//! The ceilings of the parser layers, in both dialects: where a text nested too deep ends in each
//! layer, what a small produce-event ceiling leaves of a text, how many records such a ceiling
//! lets a text dense with errors keep, and the length a tree cannot address. And the ceiling on
//! the diagnostics a lossless parse keeps: what a small one keeps, counts and leaves whole, that
//! the default keeps exactly its records, what the records kept at the default cost, and that all
//! twelve tree doors keep the ceiling they run under.
//!
//! At the default limits there is no produce-event ceiling to reach. What bounds the lexing a
//! text can buy there is the scan allowance, `8T + 4 096` events and one scan for a text of `T`
//! items: `resync_allowance.rs` gates it and `resync_cost.rs` measures it, and nothing here
//! repeats them.

// The instrument the memory cell reads: `support/counting.rs`, whose header says why it is a file
// and which targets include it. It is this binary's `#[global_allocator]`, so it is included once,
// here, and only where the lossless tower is: the Miri selection and the rows without it compile
// this file with no allocator of its own.
#[cfg(all(feature = "rowan_0_17", feature = "std"))]
#[path = "support/counting.rs"]
mod counting;

/// One dialect's cells. Both dialects spell the type-system shapes below alike, so the cells are
/// written once: `$error` is the dialect's syntactic error and `$lent` its lossless one.
macro_rules! dialect {
  ($module:ident, $feature:literal, $dialect:ident, $error:ident, $lent:ident) => {
    #[cfg(feature = $feature)]
    mod $module {
      #[cfg(all(feature = "rowan_0_17", feature = "std"))]
      use smear::lexer::{
        limits::{LosslessLimits, MAX_NESTING_DEPTH},
        tokora::SimpleSpan,
      };
      #[cfg(all(feature = "rowan_0_17", feature = "std"))]
      use smear::parser::$dialect::{Lossless, kinds::SyntaxKind, lossless::$lent};
      use smear::{
        lexer::SyntaxErrorKind,
        parser::$dialect::{Syntactic, error::$error},
      };

      /// A text whose one field's type is a list nested `depth` deep, between a definition that
      /// stands before it and one that stands after it.
      fn nested(depth: usize) -> String {
        format!(
          "scalar A\ntype T {{ f: {}Int{} }}\nscalar Z\n",
          "[".repeat(depth),
          "]".repeat(depth)
        )
      }

      /// `count` definitions, each of which the type-system root reads in silence, and where
      /// each one starts.
      #[cfg(all(feature = "rowan_0_17", feature = "std"))]
      fn definitions(count: usize) -> (String, Vec<usize>) {
        let mut text = String::new();
        let mut starts = Vec::with_capacity(count);
        for n in 0..count {
          starts.push(text.len());
          text.push_str(&format!("type T{n} {{ f: Int }}\n"));
        }
        (text, starts)
      }

      /// The syntactic layer stops at its lexer's ceiling and answers no document, with the
      /// refusal lent. The lossless layer stops at the ceiling of its parse and answers what
      /// stands before the refusal: one refusal lent, and nothing behind it read or projected.
      #[test]
      fn a_text_nested_past_the_ceiling_ends_at_it_in_each_layer() {
        // Past every ceiling below: the lexers' default, `MAX_NESTING_DEPTH`, and the deepest a
        // lossless parse descends, `HARD_MAX`.
        let text = nested(300);

        let mut lent: Vec<$error<&str>> = Vec::new();
        let Err(rejected) = Syntactic::new().type_system_document(text.as_str(), &mut lent) else {
          panic!("a text nested past the ceiling is a document for the syntactic layer");
        };
        assert_eq!(lent.len(), rejected.error_count());
        let mut limit = 0usize;
        for error in &lent {
          if error.kind() == SyntaxErrorKind::ResourceLimit {
            limit += 1;
          }
        }
        assert!(
          limit >= 1,
          "the syntactic layer rejects the text and lends no resource-limit error"
        );

        #[cfg(all(feature = "rowan_0_17", feature = "std"))]
        {
          // The lexer's tally is lifted over the text's depth, so that what refuses is the parse,
          // whose ceiling a door clamps at `HARD_MAX`. `nesting_depth.rs` holds that such a
          // refusal is one diagnostic, at every document root.
          let limits = LosslessLimits::with_max_nesting_depth(MAX_NESTING_DEPTH * 64);
          let mut lent: Vec<$lent<&str>> = Vec::new();
          let parsed = Lossless::with_limits(limits)
            .type_system_document(text.as_str(), &mut lent)
            .expect("short enough for a tree");
          assert!(!parsed.is_valid());
          assert_eq!(lent.len(), 1, "one refusal is lent, and nothing behind it");
          assert_eq!(lent[0].kind(), SyntaxErrorKind::NestingLimit);
          assert_eq!(parsed.error_count(), 1);
          assert_eq!(parsed.tree().syntax().text().to_string(), text);
          // `A` stands before the refusal, `T` holds it, and `Z` was never read: the one
          // definition projected is the one the text starts with.
          let projected = parsed.document().definitions();
          assert_eq!(projected.len(), 1);
          let first: &SimpleSpan = projected[0].span();
          assert_eq!(first.start(), 0, "the definition projected is not `A`");
          assert!(!parsed.recovery().is_complete());
        }
      }

      /// A produce-event ceiling inside a valid text: exactly one error is lent, the door's
      /// refusal; the unread tail is one gap; and no definition that starts in the gap is
      /// projected.
      #[cfg(all(feature = "rowan_0_17", feature = "std"))]
      #[test]
      fn a_small_produce_event_ceiling_lends_one_refusal_and_leaves_one_gap() {
        const CEILING: usize = 100;
        let (text, starts) = definitions(400);

        let whole = Lossless::new()
          .type_system_document(text.as_str(), &mut ())
          .expect("short enough for a tree");
        assert!(
          whole.is_valid(),
          "the text is valid when nothing refuses it"
        );
        assert_eq!(whole.document().definitions().len(), starts.len());

        let limits = LosslessLimits::new().with_max_produce_events(CEILING);
        let mut lent: Vec<$lent<&str>> = Vec::new();
        let parsed = Lossless::with_limits(limits)
          .type_system_document(text.as_str(), &mut lent)
          .expect("short enough for a tree");
        assert_eq!(
          lent.len(),
          1,
          "the refusal is the one error of a valid text"
        );
        assert_eq!(lent[0].kind(), SyntaxErrorKind::TokenBudget);
        assert_eq!(parsed.error_count(), 1);
        assert!(!parsed.is_valid());
        assert_eq!(parsed.tree().syntax().text().to_string(), text);

        let mut tokens = 0usize;
        let mut gaps: Vec<(usize, usize)> = Vec::new();
        for element in parsed.tree().syntax().descendants_with_tokens() {
          if let Some(token) = element.into_token() {
            tokens += 1;
            if token.kind() == SyntaxKind::Gap {
              let range = token.text_range();
              gaps.push((usize::from(range.start()), usize::from(range.end())));
            }
          }
        }
        assert_eq!(gaps.len(), 1, "the unread tail is one gap");
        let (gap_start, gap_end) = gaps[0];
        assert_eq!(gap_end, text.len(), "the gap runs to the end of the text");
        assert!(
          tokens <= CEILING + 1,
          "{tokens} tokens are in the tree under a ceiling of {CEILING}: the ceiling's own and \
           the one gap"
        );

        for definition in parsed.document().definitions() {
          let at: &SimpleSpan = definition.span();
          assert!(
            at.start() < gap_start,
            "a definition that starts at {} is projected, where the gap starts at {gap_start}",
            at.start()
          );
        }
        let projected = parsed.document().definitions().len();
        let read = starts
          .iter()
          .filter(|start: &&usize| **start < gap_start)
          .count();
        assert!(projected >= 1, "nothing before the ceiling is projected");
        assert!(
          projected <= read,
          "{projected} definitions are projected where {read} start before the gap"
        );
        assert!(read < starts.len(), "the ceiling falls inside the text");
      }

      /// A produce-event ceiling bounds the records a parse keeps too, whatever its ceiling on kept
      /// diagnostics is: a text dense with errors keeps a bounded multiple of it.
      ///
      /// The multiple is not one. A lexer error is an event and a recovery skips a token, but
      /// reports that consume nothing between two events are bounded only by how deep the
      /// grammar nests. So what is asserted is that weakest bound, one record for each level of
      /// the nesting ceiling and one more, for each event, and the door's own refusal beside
      /// them; the multiple a run measures is printed for each text.
      #[cfg(all(feature = "rowan_0_17", feature = "std"))]
      #[test]
      fn records_under_a_produce_event_ceiling_stay_within_its_multiple() {
        let dense = [
          ("characters no token starts with", "? ".repeat(4_000)),
          ("tokens no definition starts with", "! ".repeat(4_000)),
          ("definitions with no name", "type { ".repeat(2_000)),
          ("definitions inside brackets", "[ type ] ".repeat(2_000)),
        ];
        for (what, text) in &dense {
          for ceiling in [64usize, 256, 1_024] {
            let limits = LosslessLimits::new().with_max_produce_events(ceiling);
            let mut lent: Vec<$lent<&str>> = Vec::new();
            let parsed = Lossless::with_limits(limits)
              .type_system_document(text.as_str(), &mut lent)
              .expect("short enough for a tree");
            let records = parsed.tree().diagnostics().len();
            let bound = (MAX_NESTING_DEPTH + 1) * ceiling + 1;
            println!(
              "{what}, ceiling {ceiling}: {records} records and {} lent errors, {:.2} records \
               an event",
              lent.len(),
              records as f64 / ceiling as f64
            );
            assert!(
              records <= bound,
              "{what}: {records} records under a ceiling of {ceiling} events, past {bound}"
            );
            assert!(
              lent.len() <= bound,
              "{what}: {} lent errors under a ceiling of {ceiling} events, past {bound}",
              lent.len()
            );
            assert_eq!(lent.len(), parsed.error_count());
            assert_eq!(parsed.tree().syntax().text().to_string(), *text);
          }
        }
      }

      /// `count` characters no token starts with, each one lexer error of its own, and then a
      /// definition, so that the document is not empty.
      #[cfg(all(feature = "rowan_0_17", feature = "std"))]
      fn bad_lexemes(count: usize) -> String {
        format!("{}scalar S\n", "? ".repeat(count))
      }

      /// Each lent error as a cell compares it: its kind, and where its span starts and ends.
      #[cfg(all(feature = "rowan_0_17", feature = "std"))]
      fn seen(errors: &[$lent<&str>]) -> Vec<(SyntaxErrorKind, usize, usize)> {
        errors
          .iter()
          .map(|error: &$lent<&str>| (error.kind(), error.span().start(), error.span().end()))
          .collect()
      }

      /// How many of a tree's diagnostics are errors.
      #[cfg(all(feature = "rowan_0_17", feature = "std"))]
      fn error_records(tree: &smear::parser::$dialect::lossless::Parse) -> usize {
        tree
          .diagnostics()
          .iter()
          .filter(|diagnostic| diagnostic.severity() == tokora::emitter::Severity::Error)
          .count()
      }

      /// The peak bytes one parse of `text` under `limits` allocates, its answer dropped inside
      /// the window, and how many diagnostics its tree kept.
      #[cfg(all(feature = "rowan_0_17", feature = "std"))]
      fn peak_of(text: &str, limits: LosslessLimits) -> (usize, usize) {
        let mut kept = 0usize;
        let peak = crate::counting::peak_bytes(|| {
          let parsed = Lossless::with_limits(limits)
            .type_system_document(text, &mut ())
            .expect("short enough for a tree");
          kept = parsed.tree().diagnostics().len();
        });
        (peak, kept)
      }

      /// T10 at a small ceiling on kept diagnostics: 400 bad lexemes under a ceiling of 100,
      /// beside the same text under no ceiling. The tree keeps exactly the first 100 records the
      /// whole parse keeps, the errors lent are the first the whole parse lent, every error past
      /// the ceiling is counted, and the tree, the document and the recovery are the whole
      /// parse's.
      #[cfg(all(feature = "rowan_0_17", feature = "std"))]
      #[test]
      fn a_small_record_ceiling_keeps_its_records_counts_the_rest_and_leaves_the_tree_whole() {
        const CEILING: usize = 100;
        let text = bad_lexemes(400);

        let mut every: Vec<$lent<&str>> = Vec::new();
        let whole = Lossless::with_limits(LosslessLimits::new().with_max_diagnostics(usize::MAX))
          .type_system_document(text.as_str(), &mut every)
          .expect("short enough for a tree");
        let mut lent: Vec<$lent<&str>> = Vec::new();
        let parsed = Lossless::with_limits(LosslessLimits::new().with_max_diagnostics(CEILING))
          .type_system_document(text.as_str(), &mut lent)
          .expect("short enough for a tree");

        let kept = parsed.tree().diagnostics();
        let all = whole.tree().diagnostics();
        let refused = parsed.tree().refused_errors();
        println!(
          "T10, a ceiling of {CEILING}: {} of {} records kept, {} of {} errors lent, {refused} \
           refused, {} errors counted where the whole parse counts {}",
          kept.len(),
          all.len(),
          lent.len(),
          every.len(),
          parsed.error_count(),
          whole.error_count()
        );
        assert!(
          all.len() > CEILING,
          "the whole parse keeps {} records, no more than the ceiling",
          all.len()
        );
        assert_eq!(whole.tree().refused_errors(), 0, "no ceiling refused an error");
        assert_eq!(
          kept.len(),
          CEILING,
          "the tree does not keep exactly the ceiling's records"
        );
        assert_eq!(
          kept,
          &all[..CEILING],
          "the records kept are not the first the whole parse keeps"
        );
        let (lent_seen, every_seen) = (seen(&lent), seen(&every));
        assert!(
          lent_seen.len() <= every_seen.len(),
          "more errors are lent under the ceiling than under none"
        );
        assert_eq!(
          lent_seen.as_slice(),
          &every_seen[..lent_seen.len()],
          "what was lent is not the beginning of what the whole parse lent"
        );
        assert_eq!(
          refused,
          error_records(whole.tree()).saturating_sub(error_records(parsed.tree())),
          "the errors refused are not the whole parse's error records less the tree's"
        );
        assert_eq!(
          lent.len() + refused,
          parsed.error_count(),
          "a sink that never breaks is not lent the count less the refused errors"
        );
        assert!(!parsed.is_valid());
        assert!(parsed.tree().has_errors());
        assert_eq!(parsed.tree().syntax().text().to_string(), text);
        assert_eq!(
          parsed.tree().green(),
          whole.tree().green(),
          "the ceiling moved the tree"
        );
        assert_eq!(
          parsed.recovery(),
          whole.recovery(),
          "the ceiling moved the recovery"
        );
        assert_eq!(
          parsed.document(),
          whole.document(),
          "the ceiling moved the document"
        );
        // The premise the count rests on: each `?` is one record of one error, so the whole
        // parse lends one error for each of its error records, and a refused record is one error.
        assert_eq!(
          every.len(),
          error_records(whole.tree()),
          "the premise that each bad lexeme is one record of one error does not hold"
        );
        assert_eq!(parsed.error_count(), whole.error_count());
      }

      /// T10 at the default ceiling on kept diagnostics: 40 000 bad lexemes, 80 009 bytes, through
      /// `Lossless::new()`. The tree keeps exactly the default's records, the default is 2^15, and
      /// every error the parse found is counted, lent or refused.
      #[cfg(all(feature = "rowan_0_17", feature = "std"))]
      #[test]
      fn the_default_record_ceiling_keeps_exactly_its_records() {
        let default = LosslessLimits::new().max_diagnostics();
        assert_eq!(
          default,
          1usize << 15,
          "the default ceiling on kept diagnostics is not 2^15"
        );
        let text = bad_lexemes(40_000);
        assert_eq!(text.len(), 80_009);
        let mut lent: Vec<$lent<&str>> = Vec::new();
        let parsed = Lossless::new()
          .type_system_document(text.as_str(), &mut lent)
          .expect("short enough for a tree");
        let kept = parsed.tree().diagnostics().len();
        let refused = parsed.tree().refused_errors();
        println!(
          "T10, the default ceiling: {kept} records kept, {} errors lent, {refused} refused, {} \
           errors counted",
          lent.len(),
          parsed.error_count()
        );
        assert_eq!(
          kept, default,
          "the default ceiling does not keep exactly its records"
        );
        assert!(
          parsed.error_count() >= 40_000,
          "{} errors are counted for 40 000 bad lexemes",
          parsed.error_count()
        );
        assert_eq!(
          lent.len() + refused,
          parsed.error_count(),
          "a sink that never breaks is not lent the count less the refused errors"
        );
        assert_eq!(parsed.tree().syntax().text().to_string(), text);
      }

      /// T10's memory cell: what the records a parse keeps at the default ceiling cost, read with
      /// the allocation counter `validator_allocation.rs` uses.
      ///
      /// Five peaks, each of one parse whose answer is dropped inside the window: 40 000 bad
      /// lexemes under a ceiling of 0, under the default and under no ceiling, and 80 000 under
      /// the default and under no ceiling. The records' share is the peak at the default less the
      /// peak at 0, and it stays within `RECORD_BOUND` for each record. Two strict inequalities
      /// follow. The second holds what no estimate can break: 40 000 more bad lexemes cost less
      /// under the default than without a ceiling. The first, that at 40 000 the default already
      /// holds less than no ceiling does, rests also on the peak falling after the ceiling is
      /// reached, where the log, the tree and every kept record are alive together, which a run
      /// measures and no reading proves.
      #[cfg(all(feature = "rowan_0_17", feature = "std"))]
      #[test]
      fn the_records_kept_at_the_default_stay_within_their_memory() {
        /// The most bytes a kept record may cost: four times the design's estimate of half a
        /// kibibyte, so that a first run is not red on an estimate, and pinned from the first
        /// run's print. The figure is of a lexer-error record that has one member, at a span of
        /// its own.
        const RECORD_BOUND: usize = 2 * 1024;

        let default = LosslessLimits::new().max_diagnostics();
        let once = bad_lexemes(40_000);
        let twice = bad_lexemes(80_000);
        let none = LosslessLimits::new().with_max_diagnostics(0);
        let every = LosslessLimits::new().with_max_diagnostics(usize::MAX);

        // One parse that is not measured, so that what a thread's first parse allocates once is
        // charged to no reading.
        let _ = peak_of(&once, LosslessLimits::new());

        let (none_1, none_1_kept) = peak_of(&once, none);
        let (kept_1, kept_1_kept) = peak_of(&once, LosslessLimits::new());
        let (every_1, every_1_kept) = peak_of(&once, every);
        let (kept_2, kept_2_kept) = peak_of(&twice, LosslessLimits::new());
        let (every_2, every_2_kept) = peak_of(&twice, every);

        let records = kept_1.saturating_sub(none_1);
        let readings = format!(
          "peaks in bytes, with the diagnostics each tree kept: none_1 {none_1} ({none_1_kept}), \
           kept_1 {kept_1} ({kept_1_kept}), every_1 {every_1} ({every_1_kept}), kept_2 {kept_2} \
           ({kept_2_kept}), every_2 {every_2} ({every_2_kept})"
        );
        println!("T10, the memory cell: {readings}");
        println!(
          "T10, the memory cell: {:.1} bytes for each kept record, {:.1} for each bad lexeme past \
           the ceiling, {:.1} for each bad lexeme with no ceiling",
          records as f64 / default as f64,
          kept_2.saturating_sub(kept_1) as f64 / 40_000.0,
          every_2.saturating_sub(every_1) as f64 / 40_000.0
        );
        assert_eq!(none_1_kept, 0, "{readings}");
        assert_eq!(kept_1_kept, default, "{readings}");
        assert!(every_1_kept >= 40_000, "{readings}");
        assert_eq!(kept_2_kept, default, "{readings}");
        assert!(every_2_kept >= 80_000, "{readings}");
        assert!(
          records <= default * RECORD_BOUND,
          "the records kept at the default cost {records} bytes, past {RECORD_BOUND} for each of \
           {default}: {readings}"
        );
        assert!(
          kept_1 < every_1,
          "at 40 000 bad lexemes the default holds no less than no ceiling: {readings}"
        );
        assert!(
          kept_2.saturating_sub(kept_1) < every_2.saturating_sub(every_1),
          "40 000 more bad lexemes cost no less under the default than with no ceiling: \
           {readings}"
        );
      }

      /// Every tree door keeps the ceiling it runs under. The six that take limits, over 64 bad
      /// lexemes under a ceiling of 8, each keep exactly 8 diagnostics, refuse an error, and keep
      /// the text whole; the six that take none, over 40 000 bad lexemes, each keep exactly the
      /// default's records and refuse an error. The `_from` doors read the text's bytes.
      #[cfg(all(feature = "rowan_0_17", feature = "std"))]
      #[test]
      fn all_twelve_doors_keep_the_ceiling_they_run_under() {
        const CEILING: usize = 8;
        let small = bad_lexemes(64);
        let limits = LosslessLimits::new().with_max_diagnostics(CEILING);
        let bytes = small.as_bytes();
        let limited = [
          (
            "parse_document_with_limits",
            smear::parser::$dialect::lossless::parse_document_with_limits(small.as_str(), limits),
          ),
          (
            "parse_type_system_document_with_limits",
            smear::parser::$dialect::lossless::parse_type_system_document_with_limits(
              small.as_str(),
              limits,
            ),
          ),
          (
            "parse_executable_document_with_limits",
            smear::parser::$dialect::lossless::parse_executable_document_with_limits(
              small.as_str(),
              limits,
            ),
          ),
          (
            "parse_document_from_with_limits",
            smear::parser::$dialect::lossless::parse_document_from_with_limits(bytes, limits)
              .expect("UTF-8, and short enough for a tree"),
          ),
          (
            "parse_type_system_document_from_with_limits",
            smear::parser::$dialect::lossless::parse_type_system_document_from_with_limits(
              bytes, limits,
            )
            .expect("UTF-8, and short enough for a tree"),
          ),
          (
            "parse_executable_document_from_with_limits",
            smear::parser::$dialect::lossless::parse_executable_document_from_with_limits(
              bytes, limits,
            )
            .expect("UTF-8, and short enough for a tree"),
          ),
        ];
        for (door, parse) in &limited {
          let kept = parse.diagnostics().len();
          let refused = parse.refused_errors();
          println!("{door}, a ceiling of {CEILING}: {kept} records kept, {refused} refused");
          assert_eq!(
            kept, CEILING,
            "{door}: the parse does not keep exactly the ceiling's records"
          );
          assert!(refused > 0, "{door}: no error was refused");
          assert!(parse.has_errors(), "{door}: the parse has no error");
          assert_eq!(
            parse.syntax().text().to_string(),
            small,
            "{door}: the tree is not the text"
          );
        }

        let default = LosslessLimits::new().max_diagnostics();
        let large = bad_lexemes(40_000);
        let bytes = large.as_bytes();
        let plain = [
          (
            "parse_document",
            smear::parser::$dialect::lossless::parse_document(large.as_str()),
          ),
          (
            "parse_type_system_document",
            smear::parser::$dialect::lossless::parse_type_system_document(large.as_str()),
          ),
          (
            "parse_executable_document",
            smear::parser::$dialect::lossless::parse_executable_document(large.as_str()),
          ),
          (
            "parse_document_from",
            smear::parser::$dialect::lossless::parse_document_from(bytes)
              .expect("UTF-8, and short enough for a tree"),
          ),
          (
            "parse_type_system_document_from",
            smear::parser::$dialect::lossless::parse_type_system_document_from(bytes)
              .expect("UTF-8, and short enough for a tree"),
          ),
          (
            "parse_executable_document_from",
            smear::parser::$dialect::lossless::parse_executable_document_from(bytes)
              .expect("UTF-8, and short enough for a tree"),
          ),
        ];
        for (door, parse) in &plain {
          let kept = parse.diagnostics().len();
          let refused = parse.refused_errors();
          println!("{door}, the default ceiling: {kept} records kept, {refused} refused");
          assert_eq!(
            kept, default,
            "{door}: the parse does not keep exactly the default's records"
          );
          assert!(refused > 0, "{door}: no error was refused");
        }
      }
    }
  };
}

dialect!(
  graphql,
  "graphql",
  graphql,
  GraphqlError,
  GraphqlLosslessErrorValue
);
dialect!(
  graphqlx,
  "graphqlx",
  graphqlx,
  GraphqlxError,
  GraphqlxLosslessErrorValue
);

/// The one failure of the lossless layer is a length, and it is decided on the number alone,
/// before any scan: the layer asks `Refused::for_source_len` first. The rule is held there,
/// because a text one byte past the bound is four gibibytes.
#[cfg(all(feature = "rowan_0_17", feature = "std"))]
#[test]
fn a_length_past_what_a_tree_addresses_is_refused_before_any_scan() {
  use smear::parser::lossless::Refused;

  assert_eq!(Refused::for_source_len(0), None);
  assert_eq!(Refused::for_source_len(Refused::MAX_SOURCE_LEN), None);
  // No length is past the bound where a `usize` cannot hold one.
  #[cfg(target_pointer_width = "64")]
  {
    let over = Refused::MAX_SOURCE_LEN + 1;
    assert_eq!(
      Refused::for_source_len(over),
      Some(Refused::SourceTooLong {
        source_len: over,
        max: Refused::MAX_SOURCE_LEN,
      })
    );
    assert_eq!(
      Refused::for_source_len(usize::MAX),
      Some(Refused::SourceTooLong {
        source_len: usize::MAX,
        max: Refused::MAX_SOURCE_LEN,
      })
    );
  }
}
