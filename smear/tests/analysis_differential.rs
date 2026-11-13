//! The differential test: the GraphQLx analyzer gives every GraphQL document of smear's corpora the
//! verdict GraphQL's own validator, `smear-schema`, gives it, code for code.
//!
//! # What is compared
//!
//! The corpus is every file of `tests/fixtures/schemas`, named by its file name; every row of the
//! draft §3 refusal corpus, `SCHEMA_FIXTURES`, named by its kind; and the schemas the draft §5
//! corpus validates against, `SCHEMA`, `QUERY_ONLY` and each fixture's own, each once. For each
//! document, in this order:
//!
//! 1. GraphQL's type-system parser reads it. A document it refuses is out of scope.
//! 2. `Schema::build` answers it: the multiset of its errors' codes, empty for a schema. A list a
//!    ceiling stopped is a failure, since what is compared is counts.
//! 3. `analyze` answers the same text, as the one file `schema.graphqlx` of the `Flat` root package
//!    `app` at tier `Std`, with no dependency, under a diagnostic ceiling no document reaches. A
//!    truncated list is a failure too.
//! 4. Parse parity: no diagnostic of the analysis is a syntax error, `smear::syntax::*`. A document
//!    GraphQLx cannot parse is a failure, and no row excuses it.
//! 5. The analysis's multiset is the codes of its errors that are GraphQL's, `smear::schema::*`.
//! 6. Each code whose count differs between the two multisets is excused by a row of the
//!    allow-list, or it is a failure, said with the document, the code and both counts, and with
//!    each place `smear-schema` reports it and whether a phase's error lies in an extent that holds
//!    that place.
//! 7. When `Schema::build` answers a schema, an error of the analysis whose code is not GraphQL's
//!    is a failure, whatever rows were used: GraphQLx alone would refuse a valid GraphQL document.
//!
//! Every failure of the whole corpus is collected and said at once, so that one run shows every
//! divergence.
//!
//! # The allow-list
//!
//! [`ALLOWED`] holds each divergence the test accepts: a code, in every document or in one, with
//! its reason. A row that excuses nothing over the whole corpus is a failure as well, so the list
//! holds the divergences there are and no other. Its target is the three rows of the capacity
//! ceilings `smear-schema`'s representation has and GraphQLx has not; each other row names the work
//! that removes it.
#![cfg(all(feature = "std", feature = "analysis", feature = "validator"))]

use std::{
  collections::{BTreeMap, BTreeSet},
  path::PathBuf,
  sync::OnceLock,
};

use smear::{
  analysis::graphqlx::{
    Analysis, Layout, Limits, Package, SourceFile, SourceId, Tier, analyze,
    test_support::{self, RULES},
  },
  diagnostic::{Code, Diagnose, Location, Severity},
  lexer::tokora::{Parse as _, Parser},
  parser::graphql::{
    GraphQL,
    ast::TypeSystemDocument,
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, type_system_document},
  },
  validator::{Schema, SchemaError, SchemaErrorKind},
};

#[allow(dead_code)]
#[path = "support/validator_corpus.rs"]
mod corpus;

use corpus::{FIXTURES, QUERY_ONLY, SCHEMA, SCHEMA_FIXTURES};

/// The family of GraphQL's own codes.
const GRAPHQL: &str = "smear::schema::";

/// The family of syntax errors, in both dialects.
const SYNTAX: &str = "smear::syntax::";

/// One allowed divergence: the count of `code` may differ between the two dialects, in every
/// document or in `document` alone, for `reason`.
struct Allowed {
  code: &'static str,
  document: Option<&'static str>,
  reason: &'static str,
}

/// A row that allows `code` to diverge in every document, for `reason`.
const fn every(code: &'static str, reason: &'static str) -> Allowed {
  Allowed {
    code,
    document: None,
    reason,
  }
}

/// A row that allows `code` to diverge in `document` alone, where `smear-schema` reports a mistake
/// in a contribution that also holds a phase's error, beside which the rule is silent.
const fn beside_a_phase(code: &'static str, document: &'static str) -> Allowed {
  Allowed {
    code,
    document: Some(document),
    reason: BESIDE_A_PHASE,
  }
}

/// Why a row of `[Z31]`'s class exists.
const BESIDE_A_PHASE: &str = "[Z31]: the contribution that holds the mistake also names an \
                              undefined type, interface or directive, and a rule says nothing \
                              where a phase refused";

/// Why a row of the capacity ceilings exists.
const CEILING: &str =
  "[Z6]: a capacity ceiling of smear-schema's representation, which GraphQLx does not have";

/// Why a row of coercion exists.
const COERCION: &str = "the coercion phase reports it: pull request 1, parts 1.1 and 1.2";

/// Why a row of the implementation phase exists.
const IMPLEMENTATION: &str = "the implementation phase reports it: pull request 1, part 1.6 (the \
                              two duplicate kinds by the rulings' round 2, item 3)";

/// Why a row of the input-object cycles exists.
const CYCLES: &str = "the cycle rules report it: the last part of this pull request";

/// Why a row of a rule of part 2.2 exists.
const PART_2_2: &str = "its rule is written in part 2.2 of this pull request";

/// Why a row of a rule of part 2.3 exists.
const PART_2_3: &str = "its rule is written in part 2.3 of this pull request";

/// Why a row of a rule of part 2.4 exists.
const PART_2_4: &str = "its rule is written in part 2.4 of this pull request";

/// Every divergence the test allows.
const ALLOWED: &[Allowed] = &[
  every("smear::schema::type-reference-too-deep", CEILING),
  every("smear::schema::too-many-field-arguments", CEILING),
  every("smear::schema::too-many-directive-arguments", CEILING),
  every("smear::schema::invalid-default-value", COERCION),
  every(
    "smear::schema::missing-required-directive-argument",
    COERCION,
  ),
  every("smear::schema::invalid-directive-argument-value", COERCION),
  every("smear::schema::undefined-input-object-field", COERCION),
  every(
    "smear::schema::missing-required-input-object-field",
    COERCION,
  ),
  every(
    "smear::schema::missing-transitive-interface",
    IMPLEMENTATION,
  ),
  every("smear::schema::missing-interface-field", IMPLEMENTATION),
  every(
    "smear::schema::invalid-interface-field-type",
    IMPLEMENTATION,
  ),
  every(
    "smear::schema::missing-interface-field-argument",
    IMPLEMENTATION,
  ),
  every(
    "smear::schema::invalid-interface-field-argument-type",
    IMPLEMENTATION,
  ),
  every(
    "smear::schema::unexpected-required-argument",
    IMPLEMENTATION,
  ),
  every(
    "smear::schema::interface-field-not-deprecated",
    IMPLEMENTATION,
  ),
  every(
    "smear::schema::duplicate-implements-interface",
    IMPLEMENTATION,
  ),
  every("smear::schema::duplicate-union-member", IMPLEMENTATION),
  every("smear::schema::circular-non-null-input-field", CYCLES),
  every("smear::schema::circular-one-of-input-field", CYCLES),
  every("smear::schema::input-object-default-value-cycle", CYCLES),
  every("smear::schema::deprecated-required-argument", PART_2_2),
  every("smear::schema::deprecated-required-input-field", PART_2_2),
  every("smear::schema::one-of-field-not-nullable", PART_2_2),
  every("smear::schema::one-of-field-has-default", PART_2_2),
  every("smear::schema::one-of-on-input-object-extension", PART_2_2),
  every("smear::schema::unsupported-directive-location", PART_2_3),
  every("smear::schema::duplicate-directive-use", PART_2_3),
  every("smear::schema::duplicate-directive-argument-use", PART_2_3),
  every("smear::schema::self-referential-directive", PART_2_3),
  every("smear::schema::specified-by-on-built-in-scalar", PART_2_3),
  every("smear::schema::root-operation-type-not-object", PART_2_3),
  every("smear::schema::shared-root-operation-type", PART_2_3),
  every("smear::schema::schema-extension-without-schema", PART_2_3),
  every("smear::schema::missing-query-root-operation-type", PART_2_4),
  Allowed {
    code: "smear::schema::undefined-implements-interface",
    document: Some("implements.graphql"),
    reason: "[Z40]: the document defines `Type1` twice, and smear-schema drops the second \
             definition unread where the analyzer reads both",
  },
  beside_a_phase(
    "smear::schema::duplicate-field-name",
    "kitchen-sink.graphql",
  ),
  beside_a_phase(
    "smear::schema::duplicate-field-name",
    "kitchen-sink_canonical.graphql",
  ),
  beside_a_phase(
    "smear::schema::duplicate-field-name",
    "schema_kitchen_sink.graphql",
  ),
  beside_a_phase(
    "smear::schema::duplicate-field-name",
    "schema-derivatives-perpfutures.graphql",
  ),
  beside_a_phase(
    "smear::schema::empty-fields-definition",
    "implements.graphql",
  ),
  beside_a_phase(
    "smear::schema::empty-fields-definition",
    "implements_amp.graphql",
  ),
  beside_a_phase(
    "smear::schema::empty-fields-definition",
    "implements_amp_canonical.graphql",
  ),
  beside_a_phase(
    "smear::schema::empty-fields-definition",
    "implements_interface.graphql",
  ),
  beside_a_phase(
    "smear::schema::empty-fields-definition",
    "omen-subgraph_schema.graphql",
  ),
  beside_a_phase(
    "smear::schema::empty-fields-definition",
    "pancake-subgraph_subgraphs_pairs_schema.graphql",
  ),
  beside_a_phase(
    "smear::schema::empty-fields-definition",
    "pancake-subgraph_subgraphs_pancake-squad_schema.graphql",
  ),
];

/// GraphQL's type-system document of `text`, or `None` when GraphQL's parser refuses it.
fn parsed(text: &str) -> Option<TypeSystemDocument<&str>> {
  Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(text)
  .ok()
}

/// An analysis of `text` as the one file `schema.graphqlx` of the `Flat` root package `app`, at
/// tier `Std`, with no dependency, under a diagnostic ceiling no document of the corpus reaches.
fn analyzed(text: &str) -> Analysis {
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", text)]);
  analyze(&root, &[], &Limits::new().with_max_diagnostics(1 << 20))
}

/// Every code a rule of smear's lists.
fn rule_codes() -> Vec<Code> {
  let mut codes: Vec<Code> = Vec::new();
  for (_, listed) in RULES {
    codes.extend_from_slice(listed);
  }
  codes
}

/// Whether `at` lies within `extent`: in its source, and between its first byte and its last.
fn holds(extent: Location, at: Location) -> bool {
  match (extent.span(), at.span()) {
    (Some(outer), Some(inner)) => {
      extent.source() == at.source() && outer.start() <= inner.start() && inner.end() <= outer.end()
    }
    _ => false,
  }
}

/// Each place `smear-schema` reports `code` among `errors`, as a line, saying whether an extent of
/// `analysis`'s model holds that place and the primary location of an error a phase reported. Such
/// a place is one a rule is silent at, beside the phase.
fn places(analysis: &Analysis, errors: &[SchemaError], code: &str) -> String {
  let model = analysis.model();
  let sources = analysis.sources();
  let file: Option<SourceId> = model
    .package(model.root_package())
    .sources()
    .into_iter()
    .find(|id: &SourceId| sources.source(*id).path() == "schema.graphqlx");
  let rules = rule_codes();
  let mut phases: Vec<Location> = Vec::new();
  for diagnostic in analysis.diagnostics() {
    if diagnostic.severity() == Severity::Error && !rules.contains(&diagnostic.code()) {
      phases.push(diagnostic.primary());
    }
  }
  let extents = test_support::extents(model);
  let mut lines = String::new();
  for error in errors {
    if error.kind().code().as_str() != code {
      continue;
    }
    let span = error.span();
    let beside = file.is_some_and(|file: SourceId| {
      let at = Location::new(file.get(), span);
      extents.iter().any(|extent: &Location| {
        holds(*extent, at) && phases.iter().any(|phase: &Location| holds(*extent, *phase))
      })
    });
    let note = if beside {
      ", in an extent that holds a phase's error"
    } else {
      ""
    };
    lines.push_str(&format!(
      "\n    smear-schema reports it at {}..{}{note}",
      span.start(),
      span.end()
    ));
  }
  lines
}

/// What the comparison of one document found.
struct Compared {
  /// GraphQL's codes, each with its count.
  graphql: BTreeMap<&'static str, usize>,
  /// The codes of the analysis's errors that are GraphQL's, each with its count.
  graphqlx: BTreeMap<&'static str, usize>,
  /// The codes whose counts differ and a row excused.
  excused: BTreeSet<&'static str>,
}

/// The comparisons of one run under the allow-list `rows`: which rows excused a difference, what
/// failed, and what the documents compared held.
struct Run<'r> {
  rows: &'r [Allowed],
  /// For each row, whether it excused a difference.
  used: Vec<bool>,
  failures: Vec<String>,
  /// Documents GraphQL's type-system parser refused.
  out_of_scope: usize,
  /// Documents compared.
  compared: usize,
  /// Documents both dialects refused.
  refused_by_both: usize,
  /// Documents both dialects accepted.
  accepted_by_both: usize,
  /// Each code found, in some document, with the same count above zero on both sides.
  equal: BTreeSet<&'static str>,
}

impl<'r> Run<'r> {
  /// A run with nothing compared yet.
  fn new(rows: &'r [Allowed]) -> Self {
    Self {
      rows,
      used: vec![false; rows.len()],
      failures: Vec::new(),
      out_of_scope: 0,
      compared: 0,
      refused_by_both: 0,
      accepted_by_both: 0,
      equal: BTreeSet::new(),
    }
  }

  /// Compares the document `name`, whose text is `graphql` for GraphQL's validator and `graphqlx`
  /// for the analyzer, which are one text but in the test that the comparison can answer no.
  /// `None` when GraphQL's type-system parser refuses `graphql`.
  fn compare(&mut self, name: &str, graphql: &str, graphqlx: &str) -> Option<Compared> {
    let Some(document) = parsed(graphql) else {
      self.out_of_scope += 1;
      return None;
    };
    self.compared += 1;

    let built = Schema::build(&document);
    let errors: &[SchemaError] = match &built {
      Ok(_) => &[],
      Err(errors) => {
        if !errors.is_exhaustive() {
          self.failures.push(format!(
            "{name}: smear-schema's list of errors is not every refusal, as a ceiling stopped it"
          ));
        }
        errors.errors()
      }
    };
    let mut graphql_codes: BTreeMap<&'static str, usize> = BTreeMap::new();
    for error in errors {
      *graphql_codes
        .entry(error.kind().code().as_str())
        .or_default() += 1;
    }

    let analysis = analyzed(graphqlx);
    let diagnostics = analysis.diagnostics();
    if diagnostics.truncated() {
      self.failures.push(format!(
        "{name}: the analysis's list of diagnostics is truncated"
      ));
    }
    let mut graphqlx_codes: BTreeMap<&'static str, usize> = BTreeMap::new();
    let mut every_error: Vec<&'static str> = Vec::new();
    let mut syntax: Vec<&'static str> = Vec::new();
    for diagnostic in diagnostics {
      let code = diagnostic.code().as_str();
      if code.starts_with(SYNTAX) {
        syntax.push(code);
      }
      if diagnostic.severity() != Severity::Error {
        continue;
      }
      every_error.push(code);
      if code.starts_with(GRAPHQL) {
        *graphqlx_codes.entry(code).or_default() += 1;
      }
    }
    if !syntax.is_empty() {
      self.failures.push(format!(
        "{name}: GraphQLx's parser refuses what GraphQL's reads: {syntax:?}"
      ));
      return None;
    }

    let mut codes: BTreeSet<&'static str> = graphql_codes.keys().copied().collect();
    codes.extend(graphqlx_codes.keys().copied());
    let mut excused: BTreeSet<&'static str> = BTreeSet::new();
    for code in codes {
      let ours = graphql_codes.get(code).copied().unwrap_or(0);
      let theirs = graphqlx_codes.get(code).copied().unwrap_or(0);
      if ours == theirs {
        self.equal.insert(code);
        continue;
      }
      let mut allowed = false;
      for (row, used) in self.rows.iter().zip(self.used.iter_mut()) {
        if row.code == code && row.document.is_none_or(|document: &str| document == name) {
          *used = true;
          allowed = true;
        }
      }
      if allowed {
        excused.insert(code);
        continue;
      }
      let at = places(&analysis, errors, code);
      self.failures.push(format!(
        "{name}: `{code}` is reported {ours} times by smear-schema and {theirs} times by the \
         analyzer, and no row allows it{at}"
      ));
    }

    match (built.is_ok(), analysis.is_ok()) {
      (true, true) => self.accepted_by_both += 1,
      (false, false) => self.refused_by_both += 1,
      _ => {}
    }
    if built.is_ok()
      && every_error
        .iter()
        .any(|code: &&str| !code.starts_with(GRAPHQL))
    {
      self.failures.push(format!(
        "{name}: smear-schema builds it, and the analyzer refuses it with {every_error:?}"
      ));
    }

    Some(Compared {
      graphql: graphql_codes,
      graphqlx: graphqlx_codes,
      excused,
    })
  }

  /// Ends the run: each row that excused nothing is a failure.
  fn finish(&mut self) {
    for (row, used) in self.rows.iter().zip(self.used.iter()) {
      if *used {
        continue;
      }
      let place = row.document.unwrap_or("every document");
      self.failures.push(format!(
        "the row for `{}` in {place} excuses nothing: {}",
        row.code, row.reason
      ));
    }
  }
}

/// The run over the whole corpus, and how many files of `tests/fixtures/schemas` it compared.
struct Corpus {
  run: Run<'static>,
  files: usize,
}

/// The run over the whole corpus under [`ALLOWED`], made once for every test that reads it.
fn whole_corpus() -> &'static Corpus {
  static CORPUS: OnceLock<Corpus> = OnceLock::new();
  CORPUS.get_or_init(|| {
    let mut run = Run::new(ALLOWED);

    let directory = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests")
      .join("fixtures")
      .join("schemas");
    let entries = std::fs::read_dir(&directory).unwrap_or_else(|error: std::io::Error| {
      panic!(
        "the corpus at {} is unreadable: {error}",
        directory.display()
      )
    });
    let mut paths: Vec<PathBuf> = Vec::new();
    for entry in entries {
      let path = entry.expect("a corpus directory entry").path();
      if path.is_file() {
        paths.push(path);
      }
    }
    paths.sort();
    for path in &paths {
      let text = std::fs::read_to_string(path).unwrap_or_else(|error: std::io::Error| {
        panic!("{} is unreadable: {error}", path.display())
      });
      let name = path
        .file_name()
        .map(|name: &std::ffi::OsStr| name.to_string_lossy().into_owned())
        .unwrap_or_default();
      // The path first: a panic inside the comparison cannot say which file it was reading.
      println!("{}", path.display());
      run.compare(&name, &text, &text);
    }
    let files = run.compared;

    for (kind, sdl, _) in SCHEMA_FIXTURES {
      run.compare(&format!("{kind:?}"), sdl, sdl);
    }

    let mut accepted: Vec<(String, &str)> = vec![
      ("SCHEMA".to_owned(), SCHEMA),
      ("QUERY_ONLY".to_owned(), QUERY_ONLY),
    ];
    for fixture in FIXTURES {
      if let Some(sdl) = fixture.schema
        && !accepted
          .iter()
          .any(|(_, seen): &(String, &str)| *seen == sdl)
      {
        accepted.push((
          format!("the schema of the fixture for {:?}", fixture.rule),
          sdl,
        ));
      }
    }
    for (name, sdl) in &accepted {
      run.compare(name, sdl, sdl);
    }

    run.finish();
    Corpus { run, files }
  })
}

/// The two dialects give every document of the corpus one verdict, but for the divergences the
/// allow-list holds, each of which occurs.
#[test]
fn every_graphql_document_gets_smear_schemas_verdict() {
  let run = &whole_corpus().run;
  assert!(
    run.failures.is_empty(),
    "{} failures:\n{}",
    run.failures.len(),
    run.failures.join("\n")
  );
}

/// The comparison reports a difference no row excuses, an error of GraphQLx's own on a document
/// GraphQL accepts, and a row that excuses nothing; and a row that excuses a difference is no
/// failure. Without this, a comparison that never fails would pass every test here.
#[test]
fn the_comparison_can_answer_no() {
  let empty = SCHEMA_FIXTURES
    .iter()
    .find(
      |(kind, ..): &&(SchemaErrorKind, &str, &[SchemaErrorKind])| {
        *kind == SchemaErrorKind::EmptyFieldsDefinition
      },
    )
    .expect("the census has a row for every kind")
    .1;

  // GraphQL refuses an empty type, and the analyzer reads a text without one.
  let mut run = Run::new(&[]);
  let compared = run
    .compare("the control", empty, QUERY_ONLY)
    .expect("the control parses");
  assert_eq!(
    compared
      .graphql
      .get("smear::schema::empty-fields-definition"),
    Some(&1)
  );
  assert!(compared.graphqlx.is_empty());
  assert_eq!(run.failures.len(), 1, "{:#?}", run.failures);
  assert!(
    run.failures[0].contains("`smear::schema::empty-fields-definition` is reported 1 times"),
    "{:#?}",
    run.failures
  );

  // A row for that code in that document excuses it, and is used.
  let rows = [Allowed {
    code: "smear::schema::empty-fields-definition",
    document: Some("the control"),
    reason: "the control",
  }];
  let mut run = Run::new(&rows);
  let compared = run
    .compare("the control", empty, QUERY_ONLY)
    .expect("the control parses");
  assert!(
    compared
      .excused
      .contains("smear::schema::empty-fields-definition")
  );
  run.finish();
  assert!(run.failures.is_empty(), "{:#?}", run.failures);

  // The same row on another document excuses nothing there, and then excuses nothing at all.
  let mut run = Run::new(&rows);
  run.compare("another document", empty, QUERY_ONLY);
  run.finish();
  assert_eq!(run.failures.len(), 2, "{:#?}", run.failures);
  assert!(
    run.failures[1].contains("excuses nothing"),
    "{:#?}",
    run.failures
  );

  // GraphQL builds a schema of a text, and the analyzer refuses a text with a code of its own.
  let mut run = Run::new(ALLOWED);
  run.compare(
    "the control",
    QUERY_ONLY,
    "type Query {\n  ok: Int\n}\nenum Choice<T> {\n  YES\n}\n",
  );
  assert_eq!(run.failures.len(), 1, "{:#?}", run.failures);
  assert!(
    run.failures[0].contains("smear::graphqlx::generic-enum"),
    "{:#?}",
    run.failures
  );
}

/// The corpus is what this test claims to compare: most of `tests/fixtures/schemas` is in scope,
/// both dialects refuse most of it and accept some of it, and many codes are found with one count
/// on both sides.
#[test]
fn the_corpus_is_not_vacuous() {
  let whole = whole_corpus();
  let run = &whole.run;
  println!(
    "compared {} documents, {} of them files of the schema fixtures, and left {} out of scope; \
     both dialects refused {} and accepted {}; {} codes were found with one count on both sides",
    run.compared,
    whole.files,
    run.out_of_scope,
    run.refused_by_both,
    run.accepted_by_both,
    run.equal.len()
  );
  assert!(whole.files >= 200, "{} files compared", whole.files);
  assert!(
    run.refused_by_both >= 150,
    "{} refused by both",
    run.refused_by_both
  );
  assert!(
    run.accepted_by_both >= 5,
    "{} accepted by both",
    run.accepted_by_both
  );
  assert!(run.equal.len() >= 10, "{:?}", run.equal);
}

/// Each code a rule lists in GraphQL's family is the code of one of `smear-schema`'s kinds: a
/// constant mistyped by one letter fails here, by its rule's name.
#[test]
fn every_shared_code_a_rule_lists_is_one_of_smear_schemas() {
  for (rule, codes) in RULES {
    for code in *codes {
      if !code.as_str().starts_with(GRAPHQL) {
        continue;
      }
      assert!(
        SchemaErrorKind::ALL
          .iter()
          .any(|kind: &SchemaErrorKind| kind.code().as_str() == code.as_str()),
        "the rule `{rule}` lists `{code}`, which is no code of smear-schema's"
      );
    }
  }
}

/// Each kind of the refusal corpus is one both dialects answer on its row with one count, or one a
/// row of the allow-list excuses there.
#[test]
fn every_fixture_kind_is_accounted_for() {
  let mut run = Run::new(ALLOWED);
  let mut agreed = 0usize;
  let mut allowed = 0usize;
  for (kind, sdl, _) in SCHEMA_FIXTURES {
    let code = kind.code().as_str();
    let compared = run
      .compare(&format!("{kind:?}"), sdl, sdl)
      .unwrap_or_else(|| panic!("the fixture for {kind:?} does not parse"));
    let ours = compared.graphql.get(code).copied().unwrap_or(0);
    let theirs = compared.graphqlx.get(code).copied().unwrap_or(0);
    if ours > 0 && ours == theirs {
      agreed += 1;
    } else if compared.excused.contains(code) {
      allowed += 1;
    } else {
      panic!(
        "the fixture for {kind:?} has `{code}` {ours} times in smear-schema and {theirs} in the \
         analyzer, and no row allows it"
      );
    }
  }
  println!(
    "of the {} kinds of the refusal corpus, the dialects agree on {agreed}, and {allowed} are \
     allowed",
    SCHEMA_FIXTURES.len()
  );
}
