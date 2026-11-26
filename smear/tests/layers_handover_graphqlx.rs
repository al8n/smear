#![cfg(all(
  any(feature = "std", feature = "alloc"),
  feature = "graphqlx",
  feature = "parser"
))]

//! What GraphQLx's parser layers lend their sink, over the corpus, and the three laws that tie the
//! hand-over to the answer:
//!
//! 1. a sink that never breaks is called exactly `error_count()` times, less the errors the
//!    lossless layer refused past its ceiling on kept diagnostics, which its tree counts as
//!    `refused_errors()`;
//! 2. `is_valid()` is true exactly when `error_count()` is zero, whatever the sink did;
//! 3. for the lossless layer, `is_valid()` is true exactly when its tree's `has_errors()` is
//!    false.
//!
//! The layers read the type-system root, and the corpus is the mixed root's: an entry that is an
//! operation is a broken text here, which is as good an input for a law as any other.

use core::ops::ControlFlow;
use std::path::PathBuf;

use smear::parser::{
  graphqlx::{Syntactic, error::GraphqlxError},
  layer::from_fn,
};

/// Every corpus entry whose name starts with `prefix`, as `(name, text)`, in name order.
fn corpus(prefix: &str) -> Vec<(String, String)> {
  let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("corpusx");
  let mut files: Vec<PathBuf> = std::fs::read_dir(&dir)
    .unwrap_or_else(|error| panic!("the corpus at {} is unreadable: {error}", dir.display()))
    .map(|entry| entry.expect("a corpus directory entry").path())
    .filter(|path: &PathBuf| path.extension().is_some_and(|ext| ext == "graphqlx"))
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

/// The syntactic layer answers no document for a broken text, and lends every error of the
/// rejection: the sink is called `Rejected::error_count()` times, which is at least once.
#[test]
fn the_syntactic_layer_lends_what_it_rejects() {
  let mut rejected = 0usize;
  for (name, text) in corpus("invalid_") {
    let mut lent = 0usize;
    let mut sink = from_fn(|_: &GraphqlxError<&str>| {
      lent += 1;
      ControlFlow::Continue(())
    });
    let Err(failure) = Syntactic::new().type_system_document(text.as_str(), &mut sink) else {
      panic!("{name}: the syntactic layer answers a document for an invalid entry");
    };
    assert!(
      failure.error_count() >= 1,
      "{name}: a rejection counts no error"
    );
    assert_eq!(
      lent,
      failure.error_count(),
      "{name}: the sink was not lent every error of the rejection"
    );
    rejected += 1;
  }
  assert!(
    rejected >= 20,
    "only {rejected} invalid entries were read; the corpus is not where this looked"
  );
}

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
mod lossless {
  use core::ops::ControlFlow;
  use std::path::PathBuf;

  use smear::{
    diagnostic::Diagnose,
    lexer::{
      SyntaxErrorKind,
      limits::{LosslessLimits, SyntacticLimits},
    },
    parser::{
      graphqlx::{
        Lossless, Syntactic,
        error::GraphqlxError,
        lossless::{GraphqlxLosslessErrorValue, Parse},
      },
      layer::from_fn,
    },
  };
  use tokora::emitter::Severity;

  use super::corpus;

  /// One error the lossless layer lends.
  type Lent<'s> = GraphqlxLosslessErrorValue<&'s str>;

  #[test]
  fn the_three_laws_hold_over_the_whole_corpus() {
    let mut valid = 0usize;
    let mut broken = 0usize;
    for (name, text) in corpus("") {
      let mut lent: Vec<Lent<'_>> = Vec::new();
      let parsed = Lossless::new()
        .type_system_document(text.as_str(), &mut lent)
        .expect("a corpus text is short enough for a tree");
      assert_eq!(
        lent.len(),
        parsed.error_count(),
        "{name}: a sink that never breaks was not called `error_count()` times"
      );
      assert_eq!(
        parsed.is_valid(),
        parsed.error_count() == 0,
        "{name}: the verdict is not the count's"
      );
      assert_eq!(
        parsed.is_valid(),
        !parsed.tree().has_errors(),
        "{name}: the verdict is not the tree's"
      );
      let recorded = parsed
        .tree()
        .diagnostics()
        .iter()
        .filter(|diagnostic| diagnostic.severity() == Severity::Error)
        .count();
      assert!(
        parsed.error_count() >= recorded,
        "{name}: {} errors were lent for the tree's {recorded} error diagnostics",
        parsed.error_count()
      );
      if parsed.is_valid() {
        valid += 1;
      } else {
        broken += 1;
      }
    }
    assert!(
      valid >= 20 && broken >= 20,
      "{valid} valid and {broken} broken texts were read; the laws are held over too few of one"
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

  /// The three laws under a ceiling on kept diagnostics, over the whole corpus, at ceilings of 0,
  /// 1 and 2, beside the same text under no ceiling. Law 1 counts what the tree refused. The
  /// verdict, the tree, the document and the recovery are the whole parse's; the tree keeps at
  /// most the ceiling's diagnostics, and they are the first the whole parse keeps; and what it
  /// refused is the whole parse's error diagnostics less its own.
  ///
  /// The floor: the test above holds at least 20 broken texts, and at a ceiling of 0 each of them
  /// refuses an error, so at least 20 pairs of a text and a ceiling refuse one.
  #[test]
  fn the_laws_hold_and_the_answer_stays_under_a_ceiling_on_kept_records() {
    let mut refusing = 0usize;
    for (name, text) in corpus("") {
      let whole = Lossless::with_limits(LosslessLimits::new().with_max_diagnostics(usize::MAX))
        .type_system_document(text.as_str(), &mut ())
        .expect("a corpus text is short enough for a tree");
      for ceiling in [0usize, 1, 2] {
        let mut lent: Vec<Lent<'_>> = Vec::new();
        let parsed = Lossless::with_limits(LosslessLimits::new().with_max_diagnostics(ceiling))
          .type_system_document(text.as_str(), &mut lent)
          .expect("a corpus text is short enough for a tree");
        let refused = parsed.tree().refused_errors();
        assert_eq!(
          lent.len() + refused,
          parsed.error_count(),
          "{name}, ceiling {ceiling}: a sink that never breaks was not called `error_count()` \
           times less the refused errors"
        );
        assert_eq!(
          parsed.is_valid(),
          parsed.error_count() == 0,
          "{name}, ceiling {ceiling}: the verdict is not the count's"
        );
        assert_eq!(
          parsed.is_valid(),
          !parsed.tree().has_errors(),
          "{name}, ceiling {ceiling}: the verdict is not the tree's"
        );
        assert_eq!(
          parsed.is_valid(),
          whole.is_valid(),
          "{name}, ceiling {ceiling}: the ceiling moved the verdict"
        );
        assert_eq!(
          parsed.tree().green(),
          whole.tree().green(),
          "{name}, ceiling {ceiling}: the ceiling moved the tree"
        );
        assert_eq!(
          parsed.document(),
          whole.document(),
          "{name}, ceiling {ceiling}: the ceiling moved the document"
        );
        assert_eq!(
          parsed.recovery(),
          whole.recovery(),
          "{name}, ceiling {ceiling}: the ceiling moved the recovery"
        );
        let kept = parsed.tree().diagnostics();
        let all = whole.tree().diagnostics();
        assert!(
          kept.len() <= ceiling && kept.len() <= all.len(),
          "{name}, ceiling {ceiling}: the tree keeps {} diagnostics, of the whole parse's {}",
          kept.len(),
          all.len()
        );
        assert_eq!(
          kept,
          &all[..kept.len()],
          "{name}, ceiling {ceiling}: the diagnostics kept are not the first the whole parse keeps"
        );
        assert_eq!(
          refused,
          error_diagnostics(whole.tree()).saturating_sub(error_diagnostics(parsed.tree())),
          "{name}, ceiling {ceiling}: the errors refused are not the whole parse's error \
           diagnostics less the tree's"
        );
        if refused > 0 {
          refusing += 1;
        }
      }
    }
    println!("{refusing} pairs of a corpus text and a ceiling refused an error");
    assert!(
      refusing >= 20,
      "only {refusing} pairs of a corpus text and a ceiling refused an error"
    );
  }

  #[test]
  fn every_lent_error_has_a_syntax_code_and_a_span_inside_the_text() {
    let mut seen = 0usize;
    for (name, text) in corpus("") {
      let mut lent: Vec<Lent<'_>> = Vec::new();
      let _ = Lossless::new()
        .type_system_document(text.as_str(), &mut lent)
        .expect("a corpus text is short enough for a tree");
      for error in &lent {
        let code = error.code();
        assert!(
          code.as_str().starts_with("smear::syntax::"),
          "{name}: `{}` is not a syntax code",
          code.as_str()
        );
        let at = error.primary();
        assert_eq!(
          at.source(),
          0,
          "{name}: a lent error points at the one text the parse read"
        );
        let span = at
          .span()
          .unwrap_or_else(|| panic!("{name}: a lent error has no place in the text"));
        assert!(
          span.start() <= span.end() && span.end() <= text.len(),
          "{name}: {}..{} is not inside the text's {} bytes",
          span.start(),
          span.end(),
          text.len()
        );
        seen += 1;
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

  /// One row for each invalid corpus entry: how many errors each layer lends, each layer's first
  /// error by position, and whether the two first errors agree in code, in span and in message.
  fn census() -> Vec<String> {
    let mut rows = Vec::new();
    for (name, text) in corpus("invalid_") {
      let mut lossless: Vec<Lent<'_>> = Vec::new();
      let _ = Lossless::new()
        .type_system_document(text.as_str(), &mut lossless)
        .expect("a corpus text is short enough for a tree");
      let mut syntactic: Vec<GraphqlxError<&str>> = Vec::new();
      let _ = Syntactic::new().type_system_document(text.as_str(), &mut syntactic);

      let ours = first(&lossless);
      let theirs = first(&syntactic);
      let agrees = match (&ours, &theirs) {
        (Some(ours), Some(theirs)) => format!(
          "code {}, span {}, message {}",
          agreement(ours.code == theirs.code),
          agreement((ours.start, ours.end) == (theirs.start, theirs.end)),
          agreement(ours.message == theirs.message)
        ),
        _ => String::from("one layer lends nothing"),
      };
      rows.push(format!(
        "{name}: lossless {} first {}; syntactic {} first {}; {agrees}",
        lossless.len(),
        shown(&ours),
        syntactic.len(),
        shown(&theirs)
      ));
    }
    rows
  }

  /// What the census file holds while no census has been recorded.
  const UNRECORDED: &str = "UNRECORDED";

  /// The recorded census, row by row: the lines of `tests/goldenx/layers_census.txt` that are
  /// neither blank nor a `#` comment.
  fn recorded() -> Vec<String> {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests")
      .join("goldenx")
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
      census.len() >= 20,
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
       recording it in tests/goldenx/layers_census.txt"
    );
  }

  // ── The block a schema position offers ─────────────────────────────────────────────────────

  /// What each layer says first about `text`, read as a type-system document, as
  /// `code at start..end: message`, and how many errors the lossless layer lends.
  fn said(text: &str) -> (String, String, usize) {
    let mut lossless: Vec<Lent<'_>> = Vec::new();
    let _ = Lossless::new()
      .type_system_document(text, &mut lossless)
      .expect("a short text is short enough for a tree");
    let mut syntactic: Vec<GraphqlxError<&str>> = Vec::new();
    let _ = Syntactic::new().type_system_document(text, &mut syntactic);
    let line = |seen: Option<Seen>| match seen {
      Some(seen) => format!(
        "{} at {}..{}: {}",
        seen.code, seen.start, seen.end, seen.message
      ),
      None => String::from("nothing"),
    };
    (
      line(first(&lossless)),
      line(first(&syntactic)),
      lossless.len(),
    )
  }

  /// An operator at a schema map's key is refused against the block, by both layers alike, and the
  /// lossless layer reads the entry on, its `=>` and its value with it.
  #[test]
  fn an_operator_at_a_maps_key_offers_the_block_in_both_layers() {
    let (ours, theirs, lent) = said("input I { a: T = map!{ -KEY => 1 } b: Int }");
    assert_eq!(ours, theirs);
    assert!(ours.contains("at 23..24"), "{ours}");
    assert!(ours.contains("`const { … }` block"), "{ours}");
    assert_eq!(lent, 1, "one report, at the `-`");
  }

  /// After a value where no second value may follow, a number written with its `-` and a `(` are
  /// refused against the block by both layers alike; where a second value may follow, each stays
  /// a value.
  #[test]
  fn a_signed_number_or_a_parenthesis_after_one_value_offers_the_block_in_both_layers() {
    for (text, at) in [
      ("input I { a: u32 = HOUR-1 }", "at 23..25"),
      ("input I { a: u32 = 1 (2) }", "at 21..22"),
    ] {
      let (ours, theirs, lent) = said(text);
      assert_eq!(ours, theirs, "{text}");
      assert!(ours.contains(at), "{text}: {ours}");
      assert!(ours.contains("`const { … }` block"), "{text}: {ours}");
      assert_eq!(lent, 1, "{text}: one report");
    }
    for text in [
      "input I { a: [Int] = [1 -2] }",
      "input I { a: M = map!{ 1 => 2 -3 => 4 } }",
    ] {
      let nothing = String::from("nothing");
      assert_eq!(said(text), (nothing.clone(), nothing, 0), "{text}");
    }
  }

  /// A run's first error is one in both layers, whatever the run reads after it: at each position a
  /// run stands at, its operand a list left open with a `>>` inside it, the syntactic layer's one
  /// error is the lossless layer's first, in code and in span, at the run's first token and against
  /// the block.
  #[test]
  fn the_first_error_of_a_run_is_one_in_both_layers() {
    let code_and_span = |line: &str| line.split(": ").next().unwrap_or_default().to_string();
    for text in [
      "input I { a: T = 1 + [1 + 2 >> 3 zz: Int } scalar S",
      "input I { a: T = + [1 + 2 >> 3 zz: Int } scalar S",
      "type T { f(a: T = 1 + [1 + 2 >> 3 zz: Int): Int g: Int } scalar S",
      "input I { a: T = { f: 1 + [1 + 2 >> 3 g: 2 } zz: Int } scalar S",
      "input I { a: T @d(x: 1 + [1 + 2 >> 3 y: 2) zz: Int } scalar S",
      "input I { a: T @d(x: 1 + [1 + 2 >> 3) zz: Int } scalar S",
      "input I { a: T = map!{ 1 + [1 + 2 >> 3 => 2 3 => 4 } zz: Int } scalar S",
      "input I { a: T = map!{ 1 => + [1 + 2 >> 3 3 => 4 } zz: Int } scalar S",
      "input I { a: T = [1 + [1 + 2 >> 3 5] zz: Int } scalar S",
      "input I { a: T = set!{ 1 + [1 + 2 >> 3 5 } zz: Int } scalar S",
      "input I { a: T = Url(1 + [1 + 2 >> 3) zz: Int } scalar S",
    ] {
      let at = text.find('+').expect("the run's first token");
      let (ours, theirs, _) = said(text);
      assert_eq!(code_and_span(&ours), code_and_span(&theirs), "{text}");
      assert_eq!(
        code_and_span(&ours),
        format!("smear::syntax::unexpected-token at {at}..{}", at + 1),
        "{text}"
      );
      assert!(ours.contains("`const { … }` block"), "{text}: {ours}");
    }
  }

  /// The first error each layer lends for `text`, read as a type-system document, as
  /// `code at start..end`: the lossless layer's first in the order it lends them, which is the
  /// order it makes them in, and the syntactic layer's one.
  fn first_lent(text: &str) -> (Option<String>, Option<String>) {
    fn code_at<E: Diagnose>(error: &E) -> String {
      let span = error.primary().span();
      format!(
        "{} at {}..{}",
        error.code().as_str(),
        span.map_or(0, |span| span.start()),
        span.map_or(0, |span| span.end())
      )
    }
    let mut lossless: Vec<Lent<'_>> = Vec::new();
    let _ = Lossless::new()
      .type_system_document(text, &mut lossless)
      .expect("a short text is short enough for a tree");
    let mut syntactic: Vec<GraphqlxError<&str>> = Vec::new();
    let _ = Syntactic::new().type_system_document(text, &mut syntactic);
    (
      lossless.first().map(code_at),
      syntactic.first().map(code_at),
    )
  }

  /// A list on a header that has lost its `)` and ends there reports first what the lossless layer
  /// reported first before, and that is the syntactic layer's one error, in code and in span: the
  /// unexpected token at a `{`, a string, an `=` or an `@` the list ends in front of, and the `:`
  /// missing behind a keyword, a word its owner reads, or a description's name.
  #[test]
  fn a_headers_stop_reports_first_the_one_error_of_both_layers() {
    for (text, token, len) in [
      ("type T @d(x: 1 { f: Int } scalar S", "{", 1),
      ("type T @d(x: 1 scalar S", "S", 1),
      ("type T @d(x: 1 \"doc\" scalar S", "\"doc\"", 5),
      ("directive @d(a: Int on FIELD scalar S", "FIELD", 5),
      ("directive @d(a: Int \"doc\" scalar S", "S", 1),
      ("union U @d(x: 1 = A | B scalar S", "=", 1),
      ("type T @d(x: 1 @e { f: Int } scalar S", "@e", 1),
      ("type T @d(x: 1 where K: L { f: Int } scalar S", "K", 1),
    ] {
      let at = text.rfind(token).expect("the token the error stands at");
      let (ours, theirs) = first_lent(text);
      assert_eq!(ours, theirs, "{text}");
      assert_eq!(
        ours,
        Some(format!(
          "smear::syntax::unexpected-token at {at}..{}",
          at + len
        )),
        "{text}"
      );
    }
  }

  /// A `::` at a block operand's head begins no operand, and both layers refuse it as they refuse
  /// every head that begins none: one code, at one token.
  #[test]
  fn a_leading_separator_in_a_block_is_one_code_in_both_layers() {
    let (ours, theirs, _) = said("input I { a: T = const { ::X } }");
    let code_and_span = |line: &str| line.split(": ").next().unwrap_or_default().to_string();
    assert_eq!(code_and_span(&ours), code_and_span(&theirs));
    assert_eq!(
      code_and_span(&ours),
      "smear::syntax::unexpected-token at 25..27"
    );
  }

  /// Every error each layer lends for `text`, read as a type-system document, each as
  /// `code at start..end`: the lossless layer's, and the syntactic layer's.
  fn lent(text: &str) -> (Vec<String>, Vec<String>) {
    fn shown_all<E: Diagnose>(errors: &[E]) -> Vec<String> {
      errors
        .iter()
        .map(|error| {
          let span = error.primary().span();
          format!(
            "{} at {}..{}",
            error.code().as_str(),
            span.map_or(0, |span| span.start()),
            span.map_or(0, |span| span.end())
          )
        })
        .collect()
    }
    let mut lossless: Vec<Lent<'_>> = Vec::new();
    let _ = Lossless::new()
      .type_system_document(text, &mut lossless)
      .expect("a short text is short enough for a tree");
    let mut syntactic: Vec<GraphqlxError<&str>> = Vec::new();
    let _ = Syntactic::new().type_system_document(text, &mut syntactic);
    (shown_all(&lossless), shown_all(&syntactic))
  }

  /// A block, a parenthesis, a list and a typed form left unfinished at the end of input: the
  /// syntactic layer's one error is the lossless layer's first emitted error. After an operand the
  /// opener is unclosed; a part missing at the end is refused where the input ends.
  #[test]
  fn an_unfinished_block_is_one_error_in_both_layers() {
    for (text, expected) in [
      (
        "input I { a: T = const { 1",
        "smear::syntax::unclosed-delimiter at 23..24",
      ),
      (
        "input I { a: T = const { (1",
        "smear::syntax::unclosed-delimiter at 25..26",
      ),
      (
        "input I { a: T = const { [1",
        "smear::syntax::unclosed-delimiter at 25..26",
      ),
      (
        "input I { a: T = const { # pending",
        "smear::syntax::unexpected-end-of-input at 34..34",
      ),
      (
        "input I { a: T = const { Box<i32> # c",
        "smear::syntax::unexpected-end-of-input at 37..37",
      ),
    ] {
      let (lossless, syntactic) = lent(text);
      assert_eq!(syntactic, [expected], "{text}");
      assert_eq!(
        lossless.first().map(String::as_str),
        Some(expected),
        "{text}: the lossless layer's first emitted error: {lossless:?}"
      );
    }
  }

  /// Type arguments that hold no type, at a typed form's entry out of a block and in one: one code
  /// and one span in both layers, the unexpected token at the `>`.
  #[test]
  fn empty_type_arguments_of_a_typed_form_are_one_error_in_both_layers() {
    for (text, expected) in [
      (
        "input I { a: T = Box<>(1) }",
        "smear::syntax::unexpected-token at 21..22",
      ),
      (
        "input I { a: T = const { Box<>(1) } }",
        "smear::syntax::unexpected-token at 29..30",
      ),
    ] {
      let (ours, theirs, _) = said(text);
      let code_and_span = |line: &str| line.split(": ").next().unwrap_or_default().to_string();
      assert_eq!(code_and_span(&ours), expected, "{text}");
      assert_eq!(code_and_span(&theirs), expected, "{text}");
    }
  }

  /// A block object field's `:`, a block map entry's `=>` and a typed value's value, each missing at
  /// the end of input behind trivia: both layers refuse it where the input ends, and it is the
  /// lossless layer's first emitted error.
  #[test]
  fn a_part_missing_at_the_end_is_refused_where_the_input_ends_in_both_layers() {
    for (text, expected) in [
      (
        "input I { a: T = const { {a # c",
        "smear::syntax::unexpected-end-of-input at 31..31",
      ),
      (
        "input I { a: T = const { map!{ 1 # c",
        "smear::syntax::unexpected-end-of-input at 36..36",
      ),
      (
        "input I { a: T = Url( # c",
        "smear::syntax::unexpected-end-of-input at 25..25",
      ),
    ] {
      let (lossless, syntactic) = lent(text);
      assert_eq!(syntactic, [expected], "{text}");
      assert_eq!(
        lossless.first().map(String::as_str),
        Some(expected),
        "{text}: the lossless layer's first emitted error: {lossless:?}"
      );
    }
  }

  /// Errors as their codes and their primary spans, in the order lent.
  type Spans = Vec<(String, usize, usize)>;

  /// Every error `errors` holds, in the order lent, as its code and its primary span.
  fn spanned<E: Diagnose>(errors: &[E]) -> Spans {
    errors
      .iter()
      .map(|error| {
        let span = error.primary().span();
        (
          String::from(error.code().as_str()),
          span.map_or(0, |span| span.start()),
          span.map_or(0, |span| span.end()),
        )
      })
      .collect()
  }

  /// What each layer lends for `text`, read as a type-system document, each error as its code and
  /// its primary span: the lossless layer's in the order it emits them, and the syntactic layer's.
  fn spans_of(text: &str) -> (Spans, Spans) {
    let mut lossless: Vec<Lent<'_>> = Vec::new();
    let _ = Lossless::new()
      .type_system_document(text, &mut lossless)
      .expect("a short text is short enough for a tree");
    let mut syntactic: Vec<GraphqlxError<&str>> = Vec::new();
    let _ = Syntactic::new().type_system_document(text, &mut syntactic);
    (spanned(&lossless), spanned(&syntactic))
  }

  /// The valid texts behind the audit's table of truncations inside a block, one for each form a
  /// block holds, their tokens a space apart.
  const BLOCK_FORMS: &[&str] = &[
    "const { 1 }",
    "const { ( 1 ) }",
    "const { [ 1 , 2 ] }",
    "const { { a : 1 , b : 2 } }",
    "const { set ! { 1 , 2 } }",
    "const { map ! { 1 => 2 , 3 => 4 } }",
    "const { Box < i32 [ u8 ] > ( 1 ) }",
    "const { P < T > { x : 1 , y : 2 } }",
    "const { Url ( 1 ) }",
    "const { P { x : 1 } }",
    "const { const { 1 } }",
    "const { ~ :: a :: b }",
    "const { a :: b }",
    "const { .. :: .. :: a }",
    "const { ^ :: a < b :: c > ( 1 ) }",
  ];

  /// The bytes at which the openers `text` leaves unclosed stand. The text's tokens are a space
  /// apart, and its closers answer its openers in order, as a prefix of a valid text's do.
  fn unclosed_openers(text: &str) -> Vec<usize> {
    let mut open: Vec<usize> = Vec::new();
    let mut at = 0usize;
    for token in text.split(' ') {
      match token {
        "{" | "[" | "(" | "<" => open.push(at),
        "}" | "]" | ")" | ">" => {
          open.pop();
        }
        _ => {}
      }
      at += token.len() + 1;
    }
    open
  }

  /// Every valid text a block holds in the audit's table, cut after each of its tokens from its
  /// `const` to the one before the block's own `}`, bare and behind a comment:
  ///
  /// - T1: both layers refuse;
  /// - T2: the syntactic layer's one error is the lossless layer's first emitted error, in code
  ///   and span, which is the one a ceiling of one kept diagnostic keeps;
  /// - T3: every later lossless error is an opener around the cut left unclosed, at that opener,
  ///   or the end of input refused where the text ends.
  ///
  /// The bare cuts are the table's 98 points.
  #[test]
  fn every_cut_inside_a_block_is_one_first_error_in_both_layers() {
    const HEAD: &str = "input I { a : T = ";
    let mut points: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    for form in BLOCK_FORMS {
      let whole = format!("{HEAD}{form} }}");
      assert_eq!(
        spans_of(&whole),
        (Vec::new(), Vec::new()),
        "{whole} is a valid text"
      );
      let tokens: Vec<&str> = form.split(' ').collect();
      for cut in 1..tokens.len() {
        let bare = format!("{HEAD}{}", tokens[..cut].join(" "));
        points.insert(bare.clone());
        for text in [bare.clone(), format!("{bare} # c")] {
          let (ours, theirs) = spans_of(&text);
          assert_eq!(theirs.len(), 1, "{text}: {theirs:?}");
          assert_eq!(ours.first(), theirs.first(), "{text}: T2, {ours:?}");
          let openers = unclosed_openers(&text);
          for (code, start, end) in ours.iter().skip(1) {
            let at_an_opener = code == "smear::syntax::unclosed-delimiter"
              && *end == start + 1
              && openers.contains(start);
            let at_the_end = code == "smear::syntax::unexpected-end-of-input"
              && *start == text.len()
              && *end == text.len();
            assert!(
              at_an_opener || at_the_end,
              "{text}: T3, {code} at {start}..{end}: {ours:?}"
            );
          }
        }
      }
    }
    assert_eq!(points.len(), 98, "the table's points");
  }

  /// A typed form's `<` at the end of input, out of a block and in one, and one level down: the
  /// `<` unclosed is both layers' first error, as it is where a type's arguments meet the end.
  #[test]
  fn type_arguments_at_the_end_are_the_angle_unclosed_in_both_layers() {
    for (text, expected) in [
      (
        "input I { a: T = Box<",
        "smear::syntax::unclosed-delimiter at 20..21",
      ),
      (
        "input I { a: T = Box<Vec<",
        "smear::syntax::unclosed-delimiter at 24..25",
      ),
      (
        "input I { a: T = const { Box<Vec<",
        "smear::syntax::unclosed-delimiter at 32..33",
      ),
      (
        "input I { a: Box<",
        "smear::syntax::unclosed-delimiter at 16..17",
      ),
    ] {
      let (lossless, syntactic) = lent(text);
      assert_eq!(syntactic, [expected], "{text}");
      assert_eq!(
        lossless.first().map(String::as_str),
        Some(expected),
        "{text}: {lossless:?}"
      );
    }
  }

  /// A `set!` or a `map!` with no `{` inside a block, under a nesting ceiling the two braces
  /// around it fill: both layers refuse the missing `{` where it should be, and neither refuses
  /// the depth, since no body is there to spend a level on.
  #[test]
  fn a_collection_with_no_brace_is_refused_at_the_brace_and_not_the_depth_in_both_layers() {
    fn shown<E: Diagnose>(errors: &[E]) -> Vec<String> {
      errors
        .iter()
        .map(|error| {
          let span = error.primary().span();
          format!(
            "{} at {}..{}",
            error.code().as_str(),
            span.map_or(0, |span| span.start()),
            span.map_or(0, |span| span.end())
          )
        })
        .collect()
    }
    for text in [
      "input I { a: T = const { set! } b: Int }",
      "input I { a: T = const { map! } b: Int }",
    ] {
      let expected = "smear::syntax::unexpected-token at 30..31";
      let mut lossless: Vec<Lent<'_>> = Vec::new();
      let _ = Lossless::with_limits(LosslessLimits::with_max_nesting_depth(2))
        .type_system_document(text, &mut lossless)
        .expect("a short text is short enough for a tree");
      let mut syntactic: Vec<GraphqlxError<&str>> = Vec::new();
      let _ = Syntactic::with_limits(SyntacticLimits::with_max_nesting_depth(2))
        .type_system_document(text, &mut syntactic);
      let (ours, theirs) = (shown(&lossless), shown(&syntactic));
      assert_eq!(theirs, [expected], "{text}");
      assert_eq!(
        ours.first().map(String::as_str),
        Some(expected),
        "{text}: {ours:?}"
      );
      assert!(
        !ours
          .iter()
          .any(|error| error.starts_with("smear::syntax::nesting-limit")),
        "{text}: {ours:?}"
      );
    }
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
  ///   is the longest production a grammar names (62 bytes): 99 bytes.
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
      "type {long} {{ {long}: }} ]] @@ {long} ((( \"{long}\n ? $ {long} 1.5x 0x {long} {{ {{"
    );
    let mut texts: Vec<(String, String)> = corpus("invalid_");
    texts.push((String::from("the garbage text"), garbage));

    let mut held = 0usize;
    for (name, text) in &texts {
      let mut lossless: Vec<Lent<'_>> = Vec::new();
      let _ = Lossless::new()
        .type_system_document(text.as_str(), &mut lossless)
        .expect("short enough for a tree");
      for error in &lossless {
        within_bounds(error, name, label_bound, help_bound);
        held += 1;
      }
      let mut syntactic: Vec<GraphqlxError<&str>> = Vec::new();
      let _ = Syntactic::new().type_system_document(text.as_str(), &mut syntactic);
      for error in &syntactic {
        within_bounds(error, name, label_bound, help_bound);
        held += 1;
      }
    }
    assert!(held >= 40, "only {held} lent errors were held to the bound");
  }
}
