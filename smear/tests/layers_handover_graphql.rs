#![cfg(all(
  any(feature = "std", feature = "alloc"),
  feature = "graphql",
  feature = "parser"
))]

//! What GraphQL's parser layers lend their sink, over the corpus, and the three laws that tie the
//! hand-over to the answer:
//!
//! 1. a sink that never breaks is called exactly `error_count()` times, less the errors the
//!    lossless layer refused past its ceiling on kept diagnostics, which its tree counts as
//!    `refused_errors()`;
//! 2. `is_valid()` is true exactly when `error_count()` is zero, whatever the sink did;
//! 3. for the lossless layer, `is_valid()` is true exactly when its tree's `has_errors()` is
//!    false.
//!
//! Each entry is read as both document kinds a layer has. The corpus is the mixed root's, so an
//! entry is a broken text for at least one of them, which is as good an input for a law as any
//! other.

use core::ops::ControlFlow;
use std::path::PathBuf;

use smear::parser::{
  graphql::{Syntactic, error::GraphqlError},
  layer::from_fn,
};

/// Every corpus entry whose name starts with `prefix`, as `(name, text)`, in name order.
fn corpus(prefix: &str) -> Vec<(String, String)> {
  let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("corpus");
  let mut files: Vec<PathBuf> = std::fs::read_dir(&dir)
    .unwrap_or_else(|error| panic!("the corpus at {} is unreadable: {error}", dir.display()))
    .map(|entry| entry.expect("a corpus directory entry").path())
    .filter(|path: &PathBuf| path.extension().is_some_and(|ext| ext == "graphql"))
    .collect();
  files.sort();
  let mut entries = Vec::new();
  for path in files {
    let name = path
      .file_name()
      .expect("a corpus entry has a file name")
      .to_string_lossy()
      .to_string();
    if name.starts_with(prefix) {
      let text = std::fs::read_to_string(&path)
        .unwrap_or_else(|error| panic!("{name} is unreadable: {error}"));
      entries.push((name, text));
    }
  }
  entries
}

/// The syntactic layer's rejection of a text read as one document kind: how many errors the sink
/// was lent, and how many the rejection counts.
macro_rules! rejection {
  ($method:ident, $name:expr, $text:expr) => {{
    let mut lent = 0usize;
    let mut sink = from_fn(|_: &GraphqlError<&str>| {
      lent += 1;
      ControlFlow::Continue(())
    });
    let Err(failure) = Syntactic::new().$method($text, &mut sink) else {
      panic!(
        "{}: the syntactic layer answers a document for an invalid entry",
        $name
      );
    };
    (lent, failure.error_count())
  }};
}

/// The syntactic layer answers no document for a broken text, and lends every error of the
/// rejection: the sink is called `Rejected::error_count()` times, which is at least once.
#[test]
fn the_syntactic_layer_lends_what_it_rejects() {
  let mut rejected = 0usize;
  for (name, text) in corpus("invalid_") {
    for (lent, counted) in [
      rejection!(type_system_document, name, text.as_str()),
      rejection!(executable_document, name, text.as_str()),
    ] {
      assert!(counted >= 1, "{name}: a rejection counts no error");
      assert_eq!(
        lent, counted,
        "{name}: the sink was not lent every error of the rejection"
      );
      rejected += 1;
    }
  }
  assert!(
    rejected >= 40,
    "only {rejected} rejections were read; the corpus is not where this looked"
  );
}

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
mod lossless {
  use core::ops::ControlFlow;
  use std::path::PathBuf;

  use smear::{
    diagnostic::Diagnose,
    lexer::{SyntaxErrorKind, limits::LosslessLimits},
    parser::{
      graphql::{
        Lossless, Syntactic,
        error::GraphqlError,
        lossless::{GraphqlLosslessErrorValue, Parse},
      },
      layer::from_fn,
    },
  };
  use tokora::emitter::Severity;

  use super::corpus;

  /// One error the lossless layer lends.
  type Lent<'s> = GraphqlLosslessErrorValue<&'s str>;

  /// What the lossless layer answers for a text read as one document kind: the errors it lent a
  /// sink that never breaks, its count, its verdict, whether its tree has errors, and how many
  /// error diagnostics its tree holds.
  macro_rules! answer {
    ($method:ident, $text:expr) => {{
      let mut lent: Vec<Lent<'_>> = Vec::new();
      let parsed = Lossless::new()
        .$method($text, &mut lent)
        .expect("a corpus text is short enough for a tree");
      let recorded = parsed
        .tree()
        .diagnostics()
        .iter()
        .filter(|diagnostic| diagnostic.severity() == Severity::Error)
        .count();
      (
        lent,
        parsed.error_count(),
        parsed.is_valid(),
        parsed.tree().has_errors(),
        recorded,
      )
    }};
  }

  #[test]
  fn the_three_laws_hold_over_the_whole_corpus() {
    let mut valid = 0usize;
    let mut broken = 0usize;
    for (name, text) in corpus("") {
      for (kind, (lent, count, is_valid, tree_has_errors, recorded)) in [
        ("schema", answer!(type_system_document, text.as_str())),
        ("query", answer!(executable_document, text.as_str())),
      ] {
        assert_eq!(
          lent.len(),
          count,
          "{name} as a {kind}: a sink that never breaks was not called `error_count()` times"
        );
        assert_eq!(
          is_valid,
          count == 0,
          "{name} as a {kind}: the verdict is not the count's"
        );
        assert_eq!(
          is_valid, !tree_has_errors,
          "{name} as a {kind}: the verdict is not the tree's"
        );
        assert!(
          count >= recorded,
          "{name} as a {kind}: {count} errors were lent for the tree's {recorded} error \
           diagnostics"
        );
        if is_valid {
          valid += 1;
        } else {
          broken += 1;
        }
      }
    }
    assert!(
      valid >= 20 && broken >= 20,
      "{valid} valid and {broken} broken readings; the laws are held over too few of one"
    );
  }

  /// How many of a tree's diagnostics are errors.
  fn error_diagnostics(tree: &Parse) -> usize {
    tree
      .diagnostics()
      .iter()
      .filter(|diagnostic| diagnostic.severity() == Severity::Error)
      .count()
  }

  /// `answer!`'s reading under ceilings of 0, 1 and 2 on kept diagnostics, beside the same
  /// reading under no ceiling: the three laws, and the answer the ceiling leaves as it was. Answers
  /// how many of the three ceilings refused an error.
  macro_rules! under_ceilings {
    ($method:ident, $name:expr, $kind:expr, $text:expr) => {{
      let whole = Lossless::with_limits(LosslessLimits::new().with_max_diagnostics(usize::MAX))
        .$method($text, &mut ())
        .expect("a corpus text is short enough for a tree");
      let mut refusing = 0usize;
      for ceiling in [0usize, 1, 2] {
        let mut lent: Vec<Lent<'_>> = Vec::new();
        let parsed = Lossless::with_limits(LosslessLimits::new().with_max_diagnostics(ceiling))
          .$method($text, &mut lent)
          .expect("a corpus text is short enough for a tree");
        let refused = parsed.tree().refused_errors();
        assert_eq!(
          lent.len() + refused,
          parsed.error_count(),
          "{} as a {}, ceiling {ceiling}: a sink that never breaks was not called `error_count()` \
           times less the refused errors",
          $name,
          $kind
        );
        assert_eq!(
          parsed.is_valid(),
          parsed.error_count() == 0,
          "{} as a {}, ceiling {ceiling}: the verdict is not the count's",
          $name,
          $kind
        );
        assert_eq!(
          parsed.is_valid(),
          !parsed.tree().has_errors(),
          "{} as a {}, ceiling {ceiling}: the verdict is not the tree's",
          $name,
          $kind
        );
        assert_eq!(
          parsed.is_valid(),
          whole.is_valid(),
          "{} as a {}, ceiling {ceiling}: the ceiling moved the verdict",
          $name,
          $kind
        );
        assert_eq!(
          parsed.tree().green(),
          whole.tree().green(),
          "{} as a {}, ceiling {ceiling}: the ceiling moved the tree",
          $name,
          $kind
        );
        assert_eq!(
          parsed.document(),
          whole.document(),
          "{} as a {}, ceiling {ceiling}: the ceiling moved the document",
          $name,
          $kind
        );
        assert_eq!(
          parsed.recovery(),
          whole.recovery(),
          "{} as a {}, ceiling {ceiling}: the ceiling moved the recovery",
          $name,
          $kind
        );
        let kept = parsed.tree().diagnostics();
        let all = whole.tree().diagnostics();
        assert!(
          kept.len() <= ceiling && kept.len() <= all.len(),
          "{} as a {}, ceiling {ceiling}: the tree keeps {} diagnostics, of the whole parse's {}",
          $name,
          $kind,
          kept.len(),
          all.len()
        );
        assert_eq!(
          kept,
          &all[..kept.len()],
          "{} as a {}, ceiling {ceiling}: the diagnostics kept are not the first the whole parse \
           keeps",
          $name,
          $kind
        );
        assert_eq!(
          refused,
          error_diagnostics(whole.tree()).saturating_sub(error_diagnostics(parsed.tree())),
          "{} as a {}, ceiling {ceiling}: the errors refused are not the whole parse's error \
           diagnostics less the tree's",
          $name,
          $kind
        );
        if refused > 0 {
          refusing += 1;
        }
      }
      refusing
    }};
  }

  /// The three laws under a ceiling on kept diagnostics, over the whole corpus read as both
  /// document kinds, at ceilings of 0, 1 and 2, beside the same reading under no ceiling. Law 1
  /// counts what the tree refused. The verdict, the tree, the document and the recovery are the
  /// whole parse's; the tree keeps at most the ceiling's diagnostics, and they are the first the
  /// whole parse keeps; and what it refused is the whole parse's error diagnostics less its own.
  ///
  /// The floor: the test above holds at least 20 broken readings, and at a ceiling of 0 each of
  /// them refuses an error, so at least 20 pairs of a reading and a ceiling refuse one.
  #[test]
  fn the_laws_hold_and_the_answer_stays_under_a_ceiling_on_kept_records() {
    let mut refusing = 0usize;
    for (name, text) in corpus("") {
      refusing += under_ceilings!(type_system_document, name, "schema", text.as_str());
      refusing += under_ceilings!(executable_document, name, "query", text.as_str());
    }
    println!("{refusing} pairs of a corpus reading and a ceiling refused an error");
    assert!(
      refusing >= 20,
      "only {refusing} pairs of a corpus reading and a ceiling refused an error"
    );
  }

  #[test]
  fn every_lent_error_has_a_syntax_code_and_a_span_inside_the_text() {
    let mut seen = 0usize;
    for (name, text) in corpus("") {
      for (kind, (lent, _, _, _, _)) in [
        ("schema", answer!(type_system_document, text.as_str())),
        ("query", answer!(executable_document, text.as_str())),
      ] {
        for error in &lent {
          let code = error.code();
          assert!(
            code.as_str().starts_with("smear::syntax::"),
            "{name} as a {kind}: `{}` is not a syntax code",
            code.as_str()
          );
          let at = error.primary();
          assert_eq!(
            at.source(),
            0,
            "{name} as a {kind}: a lent error points at the one text the parse read"
          );
          let span = at
            .span()
            .unwrap_or_else(|| panic!("{name} as a {kind}: a lent error has no place in the text"));
          assert!(
            span.start() <= span.end() && span.end() <= text.len(),
            "{name} as a {kind}: {}..{} is not inside the text's {} bytes",
            span.start(),
            span.end(),
            text.len()
          );
          seen += 1;
        }
      }
    }
    assert!(seen >= 20, "only {seen} lent errors were read");
  }

  /// `count` definitions, each of which the type-system root reads in silence.
  fn definitions(count: usize) -> String {
    let mut text = String::new();
    for n in 0..count {
      text.push_str(&format!("type T{n} {{ f: Int }}\n"));
    }
    text
  }

  /// Where in `lent` the token-budget refusals stand.
  fn budget_refusals(lent: &[Lent<'_>]) -> Vec<usize> {
    let mut at = Vec::new();
    for (index, error) in lent.iter().enumerate() {
      if error.kind() == SyntaxErrorKind::TokenBudget {
        at.push(index);
      }
    }
    at
  }

  /// A produce-event ceiling that falls inside the text is one lent error, the last, however many
  /// errors of the grammar's stand before it: the grammar's own budget members are never lent,
  /// and the door's refusal is lent once.
  #[test]
  fn a_budget_refusal_is_lent_exactly_once() {
    let text = format!("type Half {{ f: }}\n{}", definitions(400));
    let limits = LosslessLimits::new().with_max_produce_events(100);
    let mut lent: Vec<Lent<'_>> = Vec::new();
    let parsed = Lossless::with_limits(limits)
      .type_system_document(text.as_str(), &mut lent)
      .expect("short enough for a tree");
    assert!(
      lent.len() >= 2,
      "the broken definition's error and the refusal are two lent errors, and {} were lent",
      lent.len()
    );
    assert_eq!(
      budget_refusals(&lent),
      [lent.len() - 1],
      "one budget refusal is lent, and it is the last"
    );
    assert_eq!(parsed.error_count(), lent.len());
    assert!(!parsed.is_valid());
  }

  #[test]
  fn a_text_within_its_budget_lends_no_budget_error() {
    let text = format!("type Half {{ f: }}\n{}", definitions(40));
    for limits in [
      LosslessLimits::new(),
      LosslessLimits::new().with_max_produce_events(1 << 20),
    ] {
      let mut lent: Vec<Lent<'_>> = Vec::new();
      let parsed = Lossless::with_limits(limits)
        .type_system_document(text.as_str(), &mut lent)
        .expect("short enough for a tree");
      assert!(!lent.is_empty(), "the broken definition's error is lent");
      assert!(
        budget_refusals(&lent).is_empty(),
        "a budget the text stays within refuses nothing"
      );
      assert_eq!(parsed.error_count(), lent.len());
    }
  }

  /// `Break` ends the hand-over and nothing else: the count, the verdict, the document, the
  /// recovery and the tree are what a sink that never breaks is answered, and the sink that broke
  /// was called once.
  #[test]
  fn a_sink_that_breaks_changes_nothing_but_what_it_was_lent() {
    let text = "type A { f: }\ntype B { g: }\nscalar C";
    let mut all: Vec<Lent<'_>> = Vec::new();
    let whole = Lossless::new()
      .type_system_document(text, &mut all)
      .expect("short enough for a tree");
    assert!(
      whole.error_count() >= 2,
      "the text makes two mistakes, so a sink that breaks at the first leaves errors unlent"
    );
    assert_eq!(all.len(), whole.error_count());

    let mut taken = 0usize;
    let mut sink = from_fn(|_: &Lent<'_>| {
      taken += 1;
      ControlFlow::Break(())
    });
    let stopped = Lossless::new()
      .type_system_document(text, &mut sink)
      .expect("short enough for a tree");
    assert_eq!(taken, 1, "a sink that breaks is called once");
    assert_eq!(stopped.error_count(), whole.error_count());
    assert_eq!(stopped.is_valid(), whole.is_valid());
    assert_eq!(stopped.document(), whole.document());
    assert_eq!(stopped.recovery(), whole.recovery());
    assert_eq!(stopped.tree().green(), whole.tree().green());
    assert_eq!(stopped.tree().diagnostics(), whole.tree().diagnostics());
    assert_eq!(stopped.tree().syntax().text().to_string(), text);
  }

  // ── The census of single mistakes ──────────────────────────────────────────────────────────

  /// One error as the census reads it: its code, its primary span, and its message.
  struct Seen {
    code: String,
    start: usize,
    end: usize,
    message: String,
  }

  /// The first of `errors` by position: by where its primary span starts, then by where it ends,
  /// and the first lent among equals.
  fn first<E: Diagnose>(errors: &[E]) -> Option<Seen> {
    let mut first: Option<Seen> = None;
    for error in errors {
      let span = error.primary().span();
      let candidate = Seen {
        code: String::from(error.code().as_str()),
        start: span.map_or(0, |span| span.start()),
        end: span.map_or(0, |span| span.end()),
        message: error.to_string(),
      };
      let earlier = match &first {
        None => true,
        Some(held) => (candidate.start, candidate.end) < (held.start, held.end),
      };
      if earlier {
        first = Some(candidate);
      }
    }
    first
  }

  /// A first error, as a row writes it.
  fn shown(seen: &Option<Seen>) -> String {
    match seen {
      Some(seen) => format!("{} at {}..{}", seen.code, seen.start, seen.end),
      None => String::from("nothing"),
    }
  }

  /// Whether two readings agree, as a row writes it.
  fn agreement(equal: bool) -> &'static str {
    if equal { "same" } else { "differs" }
  }

  /// One row of the census: how many errors each layer lends for an entry read as one document
  /// kind, each layer's first error by position, and whether the two first errors agree in code,
  /// in span and in message.
  fn row(
    name: &str,
    kind: &str,
    lossless: &[Lent<'_>],
    syntactic: &[GraphqlError<&str>],
  ) -> String {
    let ours = first(lossless);
    let theirs = first(syntactic);
    let agrees = match (&ours, &theirs) {
      (Some(ours), Some(theirs)) => format!(
        "code {}, span {}, message {}",
        agreement(ours.code == theirs.code),
        agreement((ours.start, ours.end) == (theirs.start, theirs.end)),
        agreement(ours.message == theirs.message)
      ),
      _ => String::from("one layer lends nothing"),
    };
    format!(
      "{name} as a {kind}: lossless {} first {}; syntactic {} first {}; {agrees}",
      lossless.len(),
      shown(&ours),
      syntactic.len(),
      shown(&theirs)
    )
  }

  /// Two rows for each invalid corpus entry, one for each document kind a layer reads.
  fn census() -> Vec<String> {
    let mut rows = Vec::new();
    for (name, text) in corpus("invalid_") {
      let (lossless, _, _, _, _) = answer!(type_system_document, text.as_str());
      let mut syntactic: Vec<GraphqlError<&str>> = Vec::new();
      let _ = Syntactic::new().type_system_document(text.as_str(), &mut syntactic);
      rows.push(row(&name, "schema", &lossless, &syntactic));

      let (lossless, _, _, _, _) = answer!(executable_document, text.as_str());
      let mut syntactic: Vec<GraphqlError<&str>> = Vec::new();
      let _ = Syntactic::new().executable_document(text.as_str(), &mut syntactic);
      rows.push(row(&name, "query", &lossless, &syntactic));
    }
    rows
  }

  /// What the census file holds while no census has been recorded.
  const UNRECORDED: &str = "UNRECORDED";

  /// The recorded census, row by row: the lines of `tests/golden/layers_census.txt` that are
  /// neither blank nor a `#` comment.
  fn recorded() -> Vec<String> {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests")
      .join("golden")
      .join("layers_census.txt");
    let text = std::fs::read_to_string(&path).unwrap_or_else(|error| {
      panic!(
        "the census at {} is unreadable: {error}; a missing census is a failure, not an \
         invitation to write one",
        path.display()
      )
    });
    text
      .lines()
      .map(str::trim_end)
      .filter(|line: &&str| !line.is_empty() && !line.starts_with('#'))
      .map(String::from)
      .collect()
  }

  /// The two layers do not report one mistake alike, by design in several classes: a text nested
  /// too deep, a lexical error with a clause of its own, an expectation one layer names and the
  /// other does not, an unclosed delimiter. So what is held is the census itself, row for row: a
  /// row that moves is a change in what a layer says about a mistake, and is read before it is
  /// recorded.
  ///
  /// While the file holds the one line `UNRECORDED` the census is printed and nothing is
  /// compared: it cannot be derived by reading, so its first recording is a run's.
  #[test]
  fn the_census_of_single_mistakes_is_the_recorded_one() {
    let census = census();
    assert!(
      census.len() >= 40,
      "the census has {} rows; the corpus is not where this looked",
      census.len()
    );
    let recorded = recorded();
    if recorded == [UNRECORDED] {
      println!(
        "the census of single mistakes, {} rows, not yet recorded:",
        census.len()
      );
      for row in &census {
        println!("{row}");
      }
      return;
    }
    assert_eq!(
      census, recorded,
      "what the layers say about a single mistake moved; read each row that differs before \
       recording it in tests/golden/layers_census.txt"
    );
  }

  // ── The message bound ──────────────────────────────────────────────────────────────────────

  /// The most bytes a syntax error's message holds.
  ///
  /// Derived from the texts the error modules can write, none of which quotes the input:
  ///
  /// - a kind's own phrase, `SyntaxErrorKind::message`, of which the longest is 42 bytes;
  /// - a lexer error's sentence: a kind's phrase; a fixed sentence of
  ///   `smear-lexer/src/error/describe.rs`, of which the longest is 63 bytes; the count of what a
  ///   record did not keep, a `usize` of at most 20 digits and a fixed 59 bytes for a string
  ///   literal (52 for a comment, in the dialect's own `error.rs`): 79 bytes; or a kind's phrase,
  ///   `, expected ` and one of that file's hints, of which the longest is 28 bytes: 81 bytes at
  ///   most;
  /// - a parser error's sentence, `unexpected <found>, expected <expectation>`: 11 bytes, the
  ///   longest token kind (`carriage return`, 15), 11 bytes, and the longest expectation, which
  ///   is the longest production a grammar names (62 bytes, in GraphQLx): 99 bytes.
  ///
  /// The bound leaves a later production's name some room and no room for a quoted spelling.
  const MESSAGE_BOUND: usize = 128;

  /// Holds each text of `error` to its bound.
  fn within_bounds<E: Diagnose>(error: &E, what: &str, label_bound: usize, help_bound: usize) {
    let message = error.to_string();
    assert!(
      message.len() <= MESSAGE_BOUND,
      "{what}: a message of {} bytes: {message:?}",
      message.len()
    );
    let label = error.primary_label().map_or(0, str::len);
    assert!(
      label <= label_bound,
      "{what}: a primary label of {label} bytes"
    );
    let help = error.help().map_or(0, str::len);
    assert!(help <= help_bound, "{what}: a help of {help} bytes");
  }

  /// No text of a lent error grows with the input: over the invalid corpus, and over a garbage
  /// text of several kilobytes that holds one very long name, every message stays within
  /// [`MESSAGE_BOUND`], and every primary label and help within the longest a kind has.
  #[test]
  fn no_lent_message_is_longer_than_the_bound() {
    let mut label_bound = 0usize;
    let mut help_bound = 0usize;
    for kind in SyntaxErrorKind::ALL {
      label_bound = label_bound.max(kind.message().len());
      help_bound = help_bound.max(kind.help().map_or(0, str::len));
    }
    assert!(
      label_bound <= MESSAGE_BOUND,
      "a kind's phrase is itself a message, and the longest is {label_bound} bytes"
    );

    let long = "x".repeat(4096);
    let garbage = format!(
      "type {long} {{ {long}: }} ]] @@ {long} ((( \"{long}\n ? $ {long} 1.5x 007 {long} {{ {{"
    );
    let mut texts: Vec<(String, String)> = corpus("invalid_");
    texts.push((String::from("the garbage text"), garbage));

    let mut held = 0usize;
    for (name, text) in &texts {
      let (schema, _, _, _, _) = answer!(type_system_document, text.as_str());
      let (query, _, _, _, _) = answer!(executable_document, text.as_str());
      for error in schema.iter().chain(query.iter()) {
        within_bounds(error, name, label_bound, help_bound);
        held += 1;
      }
      let mut syntactic: Vec<GraphqlError<&str>> = Vec::new();
      let _ = Syntactic::new().type_system_document(text.as_str(), &mut syntactic);
      let _ = Syntactic::new().executable_document(text.as_str(), &mut syntactic);
      for error in &syntactic {
        within_bounds(error, name, label_bound, help_bound);
        held += 1;
      }
    }
    assert!(held >= 40, "only {held} lent errors were held to the bound");
  }
}
