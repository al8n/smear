#![cfg(all(
  any(feature = "std", feature = "alloc"),
  feature = "parser",
  any(feature = "graphql", feature = "graphqlx")
))]

//! The ceilings of the parser layers, in both dialects: where a text nested too deep ends in each
//! layer, what a small produce-event ceiling leaves of a text, how many records such a ceiling
//! lets a text dense with errors keep, and the length a tree cannot address.
//!
//! At the default limits there is no produce-event ceiling to reach. What bounds the lexing a
//! text can buy there is the scan allowance, `8T + 4 096` events and one scan for a text of `T`
//! items: `resync_allowance.rs` gates it and `resync_cost.rs` measures it, and nothing here
//! repeats them.

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

      /// Until a parse has a ceiling on the records it keeps, the bound for untrusted text is the
      /// produce-event ceiling: a text dense with errors keeps a bounded multiple of it.
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
