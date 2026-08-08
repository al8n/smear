//! The diagnostic contract's pins: the code namespace, and the properties a renderer relies on.
//!
//! # What the golden is, and what it is not
//!
//! `tests/golden/diagnostic_codes.txt` is a **pin on the spelling**, not the inventory. The
//! inventory is derived — every row comes from a `*Kind::ALL` — so a variant added without a code
//! is an `E0004` in the crate long before it reaches here. What the compiler cannot notice is a
//! code being *renamed*, which is the one change that silently breaks every consumer keying off
//! it. That is what this file refuses.
//!
//! Bless it with `UPDATE_GOLDEN=1`, from a build that has every family. An update run still
//! fails, exactly as `lossless_golden.rs` arranges: a rewrite is not a pass, and a stray
//! `UPDATE_GOLDEN` in an environment can never turn a rename into a green run.
//!
//! # The properties beside it
//!
//! - Codes are unique across families and spelled `smear::<family>::<kebab-case>`, because a
//!   renderer sees all three families in one pass and cannot disambiguate by enum.
//! - Every current variant is [`Severity::Error`]. The severity axis exists for a deprecation-lint
//!   class that does not exist yet, and saying so here stops anyone inventing a warning to justify
//!   the field.
//! - An error carrying a related span always has a phrase for it — checked over the two corpora
//!   that fire every variant, because the two halves live in different files and a kind that
//!   started attaching a related span without one would drop it with nothing to say so.
//! - [`Location::entire`] has exactly one user. A family that stopped pointing at its spans would
//!   pass every other gate in this file.

// Everything below names `smear::validator`, which does not exist in the crate's API surface with
// the feature off; the introspection door needs its own feature on top of that and is gated per
// item rather than per file, so the two families that are always present stay checked in a
// `--features validator` build.
#![cfg(feature = "validator")]
#![allow(missing_docs)]

use std::{path::PathBuf, string::String, vec::Vec};

use smear::{
  diagnostic::{Diagnose, DiagnoseExt, Severity},
  lexer::tokora::{Parse as _, Parser},
  parser::graphql::{
    GraphQL,
    ast::{ExecutableDocument, TypeSystemDocument},
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, executable_document, type_system_document},
  },
  validator::{
    Budget, Collect, Diagnostic, Rule, Schema, SchemaError, SchemaErrorKind, Scratch,
    validate_executable,
  },
};

// Both corpora are read here — `SCHEMA_FIXTURES` for the draft §3 half of the related-label
// property and `FIXTURES` for the draft §5 half. The module also carries items neither half
// touches, which would be a `dead_code` denial under CI's `-Dwarnings`; allowed at the include, as
// this module's four other readers do.
#[allow(dead_code)]
#[path = "support/validator_corpus.rs"]
mod corpus;

use corpus::{FIXTURES, SCHEMA, SCHEMA_FIXTURES};

// ---------------------------------------------------------------------------------------------
// harness
// ---------------------------------------------------------------------------------------------

fn parse_sdl(sdl: &str) -> TypeSystemDocument<&str> {
  Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(sdl)
  .unwrap_or_else(|errors| panic!("fixture SDL does not parse: {errors:?}\n---\n{sdl}"))
}

fn parse_query(source: &str) -> ExecutableDocument<&str> {
  Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(source)
  .unwrap_or_else(|errors| panic!("fixture query does not parse: {errors:?}\n---\n{source}"))
}

fn refused(sdl: &str) -> Vec<SchemaError> {
  match Schema::build(&parse_sdl(sdl)) {
    Err(errors) => errors.errors().to_vec(),
    Ok(_) => panic!("expected a refusal, got a schema\n---\n{sdl}"),
  }
}

fn built(sdl: &str) -> Schema {
  Schema::build(&parse_sdl(sdl))
    .unwrap_or_else(|errors| panic!("fixture SDL is not a schema:\n{errors}\n---\n{sdl}"))
}

fn diagnose<'a>(schema: &Schema, source: &'a str, budget: &Budget) -> Vec<Diagnostic<&'a str>> {
  let document = parse_query(source);
  let mut scratch = Scratch::new();
  let mut collected = Vec::new();
  let mut sink = Collect::new(&mut collected);
  let _ = validate_executable(schema, &document, &mut scratch, budget, &mut sink);
  collected
}

// ---------------------------------------------------------------------------------------------
// the code namespace
// ---------------------------------------------------------------------------------------------

/// One golden row: the variant that answers, and what it answers.
fn row(variant: &str, code: &str) -> String {
  std::format!("{variant} = {code}")
}

/// The two families every `--features validator` build has.
fn always_present_rows() -> Vec<String> {
  let mut rows: Vec<String> = SchemaErrorKind::ALL
    .iter()
    .map(|kind| {
      row(
        &std::format!("SchemaErrorKind::{kind:?}"),
        kind.code().as_str(),
      )
    })
    .chain(
      Rule::ALL
        .iter()
        .map(|rule| row(&std::format!("Rule::{rule:?}"), rule.code().as_str())),
    )
    .collect();
  rows.sort();
  rows
}

/// The third family, present only where the introspection door is compiled.
#[cfg(feature = "introspection")]
fn introspection_rows() -> Vec<String> {
  use smear::validator::schema::introspection::ResponseErrorKind;

  let mut rows: Vec<String> = ResponseErrorKind::ALL
    .iter()
    .map(|kind| {
      row(
        &std::format!("ResponseErrorKind::{kind:?}"),
        kind.code().as_str(),
      )
    })
    .collect();
  rows.sort();
  rows
}

/// Every row this build can derive, sorted.
fn derived_rows() -> Vec<String> {
  let mut rows = always_present_rows();
  #[cfg(feature = "introspection")]
  rows.extend(introspection_rows());
  rows.sort();
  rows
}

/// Whether a golden row belongs to a family this build compiled.
///
/// A `--features validator` build has no `ResponseErrorKind`, so its rows are not evidence of
/// anything here and are skipped rather than reported as missing. Nothing is lost by that: the
/// complete build checks the file's length as well, so a row belonging to no family at all is
/// still caught.
fn in_this_build(line: &str) -> bool {
  if line.starts_with("SchemaErrorKind::") || line.starts_with("Rule::") {
    return true;
  }
  cfg!(feature = "introspection") && line.starts_with("ResponseErrorKind::")
}

const UPDATE_VAR: &str = "UPDATE_GOLDEN";

fn golden_path() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("golden")
    .join("diagnostic_codes.txt")
}

fn read_golden() -> Vec<String> {
  let path = golden_path();
  let text = std::fs::read_to_string(&path).unwrap_or_else(|error| {
    panic!(
      "the code golden at {} is unreadable: {error} — a missing golden is a failure, not an \
       invitation to write one",
      path.display()
    )
  });
  text
    .lines()
    .map(str::trim_end)
    .filter(|line| !line.is_empty() && !line.starts_with('#'))
    .map(String::from)
    .collect()
}

/// Every code, as committed.
///
/// The comparison is scoped to the families this build has, so the gate does not disappear under
/// `--features validator`; the completeness half runs where every family exists.
#[test]
fn codes_match_the_golden() {
  let derived = derived_rows();

  // Blessing needs the whole file, so only the complete build may write one. A partial build that
  // wrote would delete the third family's rows and call it an update.
  #[cfg(feature = "introspection")]
  if std::env::var(UPDATE_VAR).as_deref() == Ok("1") {
    let body = std::format!(
      "# Every diagnostic code, one per line, sorted. Derived from the kind enumerations and\n\
       # pinned here so that a RENAME — the one change no compiler notices and every consumer\n\
       # feels — has to be an edit to this file.\n\
       #\n\
       # Bless with `UPDATE_GOLDEN=1 cargo test -p smear --features validator,introspection \\\n\
       # --test diagnostic_codes`, which rewrites this and then fails.\n{}\n",
      derived.join("\n")
    );
    std::fs::write(golden_path(), body).expect("the code golden is writable");
    panic!(
      "{UPDATE_VAR}=1: rewrote the code golden. Read the diff — every changed line is a code some \
       consumer may be keying off — then re-run WITHOUT {UPDATE_VAR} set."
    );
  }

  let golden = read_golden();
  let scoped: Vec<&String> = golden
    .iter()
    .filter(|line| in_this_build(line))
    .collect::<Vec<_>>();

  let derived_refs: Vec<&String> = derived.iter().collect();
  assert_eq!(
    scoped,
    derived_refs,
    "the codes this build derives are not the ones committed in {}; if the change is intended, \
     bless it with `{UPDATE_VAR}=1` from a build with `introspection` on",
    golden_path().display()
  );

  // Only the complete build can say the file holds nothing else: under a narrower selection a row
  // for an absent family is not stale, it is merely out of scope.
  #[cfg(feature = "introspection")]
  assert_eq!(
    golden.len(),
    derived.len(),
    "the golden holds {} rows and every family together produces {} — a row belongs to no family \
     any more, or to none that ever existed",
    golden.len(),
    derived.len()
  );
}

/// A code names exactly one rule, and is spelled the way the namespace says.
///
/// Uniqueness is the property that makes one flat namespace usable at all: a renderer holding
/// diagnostics from three families keys off the code and nothing else, so two families answering
/// the same string would silently merge two rules.
#[test]
fn codes_are_unique_and_well_formed() {
  let rows = derived_rows();
  let mut codes: Vec<&str> = rows
    .iter()
    .map(|line| line.split(" = ").nth(1).expect("a golden row"))
    .collect();

  for code in &codes {
    let mut parts = code.split("::");
    assert_eq!(
      parts.next(),
      Some("smear"),
      "{code} is not in the namespace"
    );
    let family = parts
      .next()
      .unwrap_or_else(|| panic!("{code} has no family"));
    let rule = parts.next().unwrap_or_else(|| panic!("{code} has no rule"));
    assert_eq!(parts.next(), None, "{code} has more than three segments");
    assert!(
      matches!(family, "schema" | "validation" | "introspection"),
      "{code} names the family {family:?}, which is not one of the three"
    );
    assert!(
      !rule.is_empty()
        && !rule.starts_with('-')
        && !rule.ends_with('-')
        && !rule.contains("--")
        && rule
          .bytes()
          .all(|byte| byte.is_ascii_lowercase() || byte.is_ascii_digit() || byte == b'-'),
      "{code} is not kebab-case"
    );
  }

  let count = codes.len();
  codes.sort_unstable();
  codes.dedup();
  assert_eq!(
    codes.len(),
    count,
    "two variants answer the same code; the namespace is flat across families, so a collision is \
     two rules a renderer cannot tell apart"
  );
}

/// Every diagnostic this crate can emit today is an error.
///
/// The axis exists for the deprecation-lint class — a selected field that is `@deprecated`, which
/// changes no verdict — and until one of those ships, a `Warning` anywhere means somebody invented
/// one to justify the field.
#[test]
fn every_current_variant_is_an_error() {
  for kind in SchemaErrorKind::ALL {
    assert_eq!(kind.severity(), Severity::Error, "{kind:?}");
  }
  for rule in Rule::ALL {
    assert_eq!(rule.severity(), Severity::Error, "{rule:?}");
  }
  assert!(
    SchemaErrorKind::ALL.len() >= 66 && Rule::ALL.len() >= 31,
    "read {} kinds and {} rules; the enumerations are wrong, not the assertion",
    SchemaErrorKind::ALL.len(),
    Rule::ALL.len()
  );
}

// ---------------------------------------------------------------------------------------------
// the properties a renderer relies on
// ---------------------------------------------------------------------------------------------

/// `labels()` and `label(i)` agree, for every diagnostic either corpus produces.
///
/// A renderer sizes its storage from the count and then walks the indices, so a count that
/// over-reports panics an LSP server and one that under-reports drops a span.
fn labels_are_consistent(diagnostic: &dyn Diagnose, what: &str) {
  let count = diagnostic.labels();
  for index in 0..count {
    assert!(
      diagnostic.label(index).is_some(),
      "{what} reports {count} labels and has none at {index}"
    );
  }
  assert!(
    diagnostic.label(count).is_none(),
    "{what} reports {count} labels and has one past the end"
  );
  assert_eq!(
    diagnostic.labels_iter().count(),
    count,
    "{what}'s iterator and its count disagree"
  );

  let segments = diagnostic.path_segments();
  for index in 0..segments {
    assert!(
      diagnostic.path_segment(index).is_some(),
      "{what} reports {segments} path segments and has none at {index}"
    );
  }
  assert!(
    diagnostic.path_segment(segments).is_none(),
    "{what} reports {segments} path segments and has one past the end"
  );
}

/// An error that points at a second place always has something to say about it.
///
/// The span and the phrase are set in different files — the builder attaches one, the kind
/// enumeration carries the other — so a kind that started reporting a related span without adding
/// a phrase would drop the span on the floor, rendering identically to one that never had it. The
/// corpora here are the ones that fire every variant, which is what makes the check a census
/// rather than a spot inspection.
#[test]
fn a_related_span_always_has_a_phrase() {
  for (kind, sdl, _) in SCHEMA_FIXTURES {
    for error in refused(sdl) {
      if error.related().is_some() {
        assert!(
          error.kind().related_label().is_some(),
          "the fixture for {kind:?} produced a {:?} carrying a related span, and that kind has no \
           phrase for one — the span is being discarded",
          error.kind()
        );
      }
      assert_eq!(
        error.labels(),
        usize::from(error.related().is_some()),
        "{:?} has a related span the contract does not report",
        error.kind()
      );
      labels_are_consistent(&error, &std::format!("{:?}", error.kind()));
    }
  }

  for fixture in FIXTURES {
    let schema = built(fixture.schema.unwrap_or(SCHEMA));
    let budget = fixture.budget.unwrap_or_default();
    for diagnostic in diagnose(&schema, fixture.invalid, &budget) {
      if diagnostic.related_span().is_some() {
        assert!(
          diagnostic.rule().related_label().is_some(),
          "the fixture for {:?} produced a {:?} carrying a related span, and that rule has no \
           phrase for one — the span is being discarded",
          fixture.rule,
          diagnostic.rule()
        );
      }
      let view = diagnostic.display(&schema);
      assert_eq!(
        view.labels(),
        usize::from(diagnostic.related_span().is_some()),
        "{:?} has a related span the contract does not report",
        diagnostic.rule()
      );
      labels_are_consistent(&view, &std::format!("{:?}", diagnostic.rule()));
    }
  }
}

/// [`Location::entire`] has one user, and it is the one whose input has no positions.
///
/// A family that quietly started answering it would keep every other gate in this file green while
/// throwing away the spans an editor exists to draw.
#[test]
fn only_the_introspection_door_answers_entire() {
  for (kind, sdl, _) in SCHEMA_FIXTURES {
    for error in refused(sdl) {
      let primary = error.primary();
      assert!(
        !primary.is_entire(),
        "the fixture for {kind:?} produced a {:?} with no primary span; a schema refusal always \
         has one",
        error.kind()
      );
      assert_eq!(primary.source(), error.document());
      for label in error.labels_iter() {
        assert!(!label.location().is_entire(), "{:?}", error.kind());
      }
    }
  }

  for fixture in FIXTURES {
    let schema = built(fixture.schema.unwrap_or(SCHEMA));
    let budget = fixture.budget.unwrap_or_default();
    for diagnostic in diagnose(&schema, fixture.invalid, &budget) {
      let view = diagnostic.display(&schema);
      assert_eq!(
        view.primary().span(),
        Some(diagnostic.span()),
        "{:?} reports a primary that is not its span",
        diagnostic.rule()
      );
      for label in view.labels_iter() {
        assert!(!label.location().is_entire(), "{:?}", diagnostic.rule());
      }
    }
  }
}

/// The introspection door is the exception, and says so.
#[cfg(feature = "introspection")]
#[test]
fn the_introspection_door_is_the_exception() {
  use smear::validator::IntrospectionError;

  let refusal = Schema::from_introspection("{ not a response").expect_err("not JSON");
  let IntrospectionError::Response(error) = refusal else {
    panic!("expected a shape refusal, got {refusal}");
  };

  assert_eq!(error.primary(), smear::diagnostic::Location::entire(0));
  assert!(error.primary().is_entire());
  assert_eq!(error.labels(), 0);
  assert_eq!(error.path_segments(), 0);
  assert_eq!(error.severity(), Severity::Error);
  assert_eq!(error.code(), error.kind().code());
  labels_are_consistent(&error, "a response error");
}

/// The whole contract is readable through `&dyn Diagnose`, which is the shape it exists for.
///
/// A server renders schema refusals and validation diagnostics in one pass, so the two have to
/// erase to the same type. This is the compile-time half of what
/// `validator_allocation.rs::only_rendering_allocates` then measures.
#[test]
fn the_contract_survives_erasure() {
  let schema = built(SCHEMA);
  let diagnostics = diagnose(
    &schema,
    "query dup { dog { name } } query dup { dog { nickname } }",
    &Budget::default(),
  );
  let diagnostic = diagnostics.first().expect("a diagnostic");
  let view = diagnostic.display(&schema);
  let refusal = refused("type Query { ok: Int } type Query { ok: Int }");
  let refusal = refusal.first().expect("a refusal");

  let erased: [&dyn Diagnose; 2] = [refusal, &view];
  for subject in erased {
    assert!(subject.code().as_str().starts_with("smear::"));
    assert_eq!(subject.severity(), Severity::Error);
    assert!(subject.primary_label().is_some());
    assert_eq!(subject.labels(), 1, "both fixtures are duplicates");
    assert_eq!(subject.labels_iter().len(), 1);
    assert_eq!(subject.path_segments_iter().count(), 0);
    assert!(!std::string::ToString::to_string(subject).is_empty());
  }
}
