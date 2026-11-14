//! The GraphQLx analyzer's implementation phase over smear's corpora: on every GraphQL document it
//! gives the codes `smear-schema` gives for the nine kinds it owns, but for the classes of
//! divergence recorded below; and every file of the corpora passes far below the phase's budget.
//!
//! # The parity sweep
//!
//! Each document of `fixtures/schemas`, each row of the validator corpus's `SCHEMA_FIXTURES` and
//! each schema that corpus accepts is read by GraphQL's type-system parser, a document it refuses
//! skipped and counted, then built by `Schema::build` and analyzed as the one file of a `Flat` root
//! package at tier `Std`. The multiset of the nine codes is compared. A document is skipped, and
//! counted by reason, when the analyzer's parser refuses it, when `smear-schema`'s list is not
//! exhaustive, or when either side reports `duplicate-type-name` or `unresolved-path`, a second
//! definition of one name that `smear-schema` drops unread, `implements-non-interface` or
//! `self-implementing-interface`, rules the analyzer gives another phase, or one of the three
//! ceilings of `smear-schema`'s representation. A difference is excused in one direction alone,
//! and each excuse used is printed:
//!
//! - `smear-schema` gives more `invalid-interface-field-type` or
//!   `invalid-interface-field-argument-type` on a document where the analyzer reports
//!   `undefined-type`: a type that does not resolve is the resolver's alone here, and no subtype of
//!   anything there;
//! - `smear-schema` gives more `duplicate-implements-interface` or `duplicate-union-member` where
//!   the analyzer reports `undefined-implements-interface` or `undefined-union-member`: a name that
//!   does not resolve is compared with nothing here, and by its spelling there.
#![cfg(all(feature = "std", feature = "analysis", feature = "validator"))]

use std::path::PathBuf;

use smear::{
  analysis::graphqlx::{Analysis, Diagnostic, Layout, Limits, Package, SourceFile, Tier, analyze},
  diagnostic::Diagnose,
  lexer::tokora::{Parse as _, Parser},
  parser::graphql::{
    GraphQL,
    ast::TypeSystemDocument,
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, type_system_document},
  },
  validator::schema::{Schema, SchemaError, SchemaErrorKind},
};

// The validator corpus, read for its documents: `SCHEMA_FIXTURES`' rows and the schemas its
// fixtures are validated against.
#[allow(dead_code)]
#[path = "support/validator_corpus.rs"]
mod corpus;

/// The nine codes the implementation phase reports, each `smear-schema`'s for the same rule.
const NINE: [&str; 9] = [
  "smear::schema::duplicate-implements-interface",
  "smear::schema::duplicate-union-member",
  "smear::schema::missing-transitive-interface",
  "smear::schema::missing-interface-field",
  "smear::schema::invalid-interface-field-type",
  "smear::schema::interface-field-not-deprecated",
  "smear::schema::missing-interface-field-argument",
  "smear::schema::invalid-interface-field-argument-type",
  "smear::schema::unexpected-required-argument",
];

/// The `SchemaErrorKind`s of the nine codes: the rows of `SCHEMA_FIXTURES` each must agree on.
const NINE_KINDS: [SchemaErrorKind; 9] = [
  SchemaErrorKind::DuplicateImplementsInterface,
  SchemaErrorKind::DuplicateUnionMember,
  SchemaErrorKind::MissingTransitiveInterface,
  SchemaErrorKind::MissingInterfaceField,
  SchemaErrorKind::InvalidInterfaceFieldType,
  SchemaErrorKind::InterfaceFieldNotDeprecated,
  SchemaErrorKind::MissingInterfaceFieldArgument,
  SchemaErrorKind::InvalidInterfaceFieldArgumentType,
  SchemaErrorKind::UnexpectedRequiredArgument,
];

/// Codes that put a document out of the sweep's scope, on either side: a second definition of one
/// name, which `smear-schema` drops unread; a path that names nothing, the analyzer's alone; the
/// rules on `implements` entries the analyzer gives another phase; and the three ceilings of
/// `smear-schema`'s representation, which GraphQLx does not have.
const OUT_OF_SCOPE: [&str; 7] = [
  "smear::schema::duplicate-type-name",
  "smear::graphqlx::unresolved-path",
  "smear::schema::implements-non-interface",
  "smear::schema::self-implementing-interface",
  "smear::schema::type-reference-too-deep",
  "smear::schema::too-many-field-arguments",
  "smear::schema::too-many-directive-arguments",
];

/// The step limit's name, which a report of it quotes.
const STEPS: &str = "`Limits::max_implementation_steps`";

/// Every file of the given directories under `tests`, by path, each with its bytes.
fn files(directories: &[&str]) -> Vec<(PathBuf, Vec<u8>)> {
  let tests = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
  let mut paths: Vec<PathBuf> = Vec::new();
  for directory in directories {
    let directory = tests.join(directory);
    let entries = std::fs::read_dir(&directory).unwrap_or_else(|error: std::io::Error| {
      panic!(
        "the corpus at {} is unreadable: {error}",
        directory.display()
      )
    });
    for entry in entries {
      let path = entry.expect("a corpus directory entry").path();
      if path.is_file() {
        paths.push(path);
      }
    }
  }
  paths.sort();
  paths
    .into_iter()
    .map(|path: PathBuf| {
      let bytes = std::fs::read(&path).unwrap_or_else(|error: std::io::Error| {
        panic!("{} is unreadable: {error}", path.display())
      });
      (path, bytes)
    })
    .collect()
}

/// An analysis of `text` as the one file `schema.graphqlx` of the `Flat` root package `app`, at
/// tier `Std`, within `limits`.
fn analyzed(text: &[u8], limits: &Limits) -> Analysis {
  let root: Package<&[u8]> = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", text)]);
  analyze(&root, &[], limits)
}

/// `sdl` read by GraphQL's type-system parser: `None` when it refuses it.
fn parse(sdl: &str) -> Option<TypeSystemDocument<&str>> {
  Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(sdl)
  .ok()
}

/// How many of `codes` are each of the nine.
fn nine_counts(codes: impl Iterator<Item = &'static str>) -> [usize; 9] {
  let mut counts = [0usize; 9];
  for code in codes {
    if let Some(at) = NINE.iter().position(|nine: &&str| *nine == code)
      && let Some(count) = counts.get_mut(at)
    {
      *count += 1;
    }
  }
  counts
}

/// What comparing one document came to.
enum Compared {
  /// Out of scope, for this reason.
  Skipped(&'static str),
  /// Compared: whether the nine counts agreed with no excuse, and whether the document writes an
  /// `implements` entry.
  Done { equal: bool, implements: bool },
}

/// The sweep's tallies, and every failure, said in full.
#[derive(Default)]
struct Sweep {
  compared: usize,
  implementing: usize,
  skipped: Vec<(&'static str, usize)>,
  excused: Vec<String>,
  failures: Vec<String>,
}

impl Sweep {
  /// Counts a document skipped for `reason`.
  fn skip(&mut self, reason: &'static str) {
    match self
      .skipped
      .iter_mut()
      .find(|(why, _): &&mut (&'static str, usize)| *why == reason)
    {
      Some((_, count)) => *count += 1,
      None => self.skipped.push((reason, 1)),
    }
  }

  /// Compares the document `name`, whose text is `sdl`: `smear-schema`'s nine counts against the
  /// analyzer's, each difference excused in its class's direction or a failure.
  fn compare(&mut self, name: &str, sdl: &str) -> Compared {
    let Some(document) = parse(sdl) else {
      return Compared::Skipped("refused by GraphQL's parser");
    };
    let schema_codes: Vec<&'static str> = match Schema::build(&document) {
      Ok(_) => Vec::new(),
      Err(errors) => {
        if !errors.is_exhaustive() {
          return Compared::Skipped("smear-schema's list is not exhaustive");
        }
        errors
          .errors()
          .iter()
          .map(|error: &SchemaError| error.kind().code().as_str())
          .collect()
      }
    };
    let analysis = analyzed(sdl.as_bytes(), &Limits::new().with_max_diagnostics(1 << 20));
    assert!(
      !analysis.diagnostics().truncated(),
      "{name}: the analysis's list is truncated"
    );
    let analysis_codes: Vec<&'static str> = analysis
      .diagnostics()
      .iter()
      .map(|diagnostic: &Diagnostic| diagnostic.code().as_str())
      .collect();
    if analysis_codes
      .iter()
      .any(|code: &&str| code.starts_with("smear::syntax::"))
    {
      return Compared::Skipped("refused by the analyzer's parser");
    }
    if schema_codes
      .iter()
      .chain(analysis_codes.iter())
      .any(|code: &&str| OUT_OF_SCOPE.contains(code))
    {
      return Compared::Skipped("a rule or a ceiling out of the sweep's scope");
    }
    let wanted = nine_counts(schema_codes.iter().copied());
    let found = nine_counts(analysis_codes.iter().copied());
    let reports = |code: &'static str| analysis_codes.contains(&code);
    let mut equal = true;
    for (at, code) in NINE.iter().enumerate() {
      let (Some(&theirs), Some(&ours)) = (wanted.get(at), found.get(at)) else {
        continue;
      };
      if theirs == ours {
        continue;
      }
      equal = false;
      let excused = theirs > ours
        && match *code {
          "smear::schema::invalid-interface-field-type"
          | "smear::schema::invalid-interface-field-argument-type" => {
            reports("smear::schema::undefined-type")
          }
          "smear::schema::duplicate-implements-interface"
          | "smear::schema::duplicate-union-member" => {
            reports("smear::schema::undefined-implements-interface")
              || reports("smear::schema::undefined-union-member")
          }
          _ => false,
        };
      let line = format!("{name}: `{code}` smear-schema {theirs}, the analyzer {ours}");
      if excused {
        self.excused.push(line);
      } else {
        self.failures.push(line);
      }
    }
    let model = analysis.model();
    let implements = model
      .defs()
      .into_iter()
      .any(|def| !model.def(def).implements().is_empty());
    Compared::Done { equal, implements }
  }

  /// Compares the document `name`, tallying it, and answers whether its nine counts agreed with
  /// no excuse.
  fn sweep(&mut self, name: &str, sdl: &str) -> bool {
    match self.compare(name, sdl) {
      Compared::Skipped(reason) => {
        self.skip(reason);
        false
      }
      Compared::Done { equal, implements } => {
        self.compared += 1;
        if implements {
          self.implementing += 1;
        }
        equal
      }
    }
  }
}

/// On every GraphQL document of smear's corpora, the analyzer gives the multiset of the nine codes
/// `smear-schema` gives, but for the classes of divergence the file's header records. At least 200
/// documents are compared, 80 of them writing an `implements` entry; `perpfutures`, whose
/// `Liquidate` omits a field of its `Event`, is compared with one `missing-interface-field` on each
/// side; and each of the nine rows of `SCHEMA_FIXTURES` this phase owns is compared and agrees.
#[test]
fn implementations_agree_with_graphqls_validator() {
  let mut sweep = Sweep::default();
  let mut perpfutures: Option<bool> = None;
  for (path, bytes) in files(&["fixtures/schemas"]) {
    let name = path
      .file_name()
      .map_or_else(String::new, |name| name.to_string_lossy().into_owned());
    let Ok(sdl) = String::from_utf8(bytes) else {
      sweep.skip("not UTF-8");
      continue;
    };
    let agreed = sweep.sweep(&name, &sdl);
    if name == "schema-derivatives-perpfutures.graphql" {
      let both = parse(&sdl)
        .and_then(|document| Schema::build(&document).err())
        .is_some_and(|errors| {
          nine_counts(
            errors
              .errors()
              .iter()
              .map(|error: &SchemaError| error.kind().code().as_str()),
          ) == nine_counts(std::iter::once("smear::schema::missing-interface-field"))
        });
      perpfutures = Some(agreed && both);
    }
  }
  let mut owned: Vec<SchemaErrorKind> = Vec::new();
  for (kind, sdl, _) in corpus::SCHEMA_FIXTURES {
    let agreed = sweep.sweep(&format!("SCHEMA_FIXTURES {kind:?}"), sdl);
    if NINE_KINDS.contains(kind) && agreed {
      owned.push(*kind);
    }
  }
  let mut accepted: Vec<&str> = vec![corpus::SCHEMA, corpus::QUERY_ONLY];
  for fixture in corpus::FIXTURES {
    if let Some(sdl) = fixture.schema
      && !accepted.contains(&sdl)
    {
      accepted.push(sdl);
    }
  }
  for (index, sdl) in accepted.into_iter().enumerate() {
    sweep.sweep(&format!("accepted schema {index}"), sdl);
  }

  println!(
    "compared {} documents, {} writing an `implements` entry; skipped {:?}",
    sweep.compared, sweep.implementing, sweep.skipped
  );
  for line in &sweep.excused {
    println!("excused: {line}");
  }
  assert!(
    sweep.failures.is_empty(),
    "{} divergences:\n{}",
    sweep.failures.len(),
    sweep.failures.join("\n")
  );
  assert!(sweep.compared >= 200, "{} compared", sweep.compared);
  assert!(
    sweep.implementing >= 80,
    "{} compared write an `implements` entry",
    sweep.implementing
  );
  assert_eq!(
    perpfutures,
    Some(true),
    "`schema-derivatives-perpfutures.graphql` compared with one `missing-interface-field` each"
  );
  let missing: Vec<SchemaErrorKind> = NINE_KINDS
    .into_iter()
    .filter(|kind: &SchemaErrorKind| !owned.contains(kind))
    .collect();
  assert!(
    missing.is_empty(),
    "rows of the nine kinds not compared, or not agreed on: {missing:?}"
  );
}

/// Every file of the GraphQLx corpus and of the GraphQL schema fixtures passes under 2^15 steps,
/// 32 times below the default: no report of `Limits::max_implementation_steps`.
#[test]
fn smears_corpora_pass_far_below_the_implementation_budget() {
  let limits = Limits::new().with_max_implementation_steps(1 << 15);
  for (path, bytes) in files(&["corpusx", "fixtures/schemas"]) {
    let analysis = analyzed(&bytes, &limits);
    assert!(
      !analysis
        .diagnostics()
        .iter()
        .any(|diagnostic: &Diagnostic| diagnostic.to_string().contains(STEPS)),
      "{} passes 2^15 steps",
      path.display()
    );
  }
}

/// GitLab's schema, the largest fixture, costs far below the default: the least budget under which
/// it reports no stop of `Limits::max_implementation_steps`, bisected over `0..=2^15`, is at least
/// 1,000 and at most 2^15. Refuses a phase that charges nothing, and a realistic schema near the
/// default.
#[test]
fn the_largest_fixture_costs_far_below_the_default() {
  let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("fixtures")
    .join("schemas")
    .join("gitlab_schema.graphql");
  let bytes = std::fs::read(&path)
    .unwrap_or_else(|error: std::io::Error| panic!("{} is unreadable: {error}", path.display()));
  let passes = |budget: u32| -> bool {
    !analyzed(&bytes, &Limits::new().with_max_implementation_steps(budget))
      .diagnostics()
      .iter()
      .any(|diagnostic: &Diagnostic| diagnostic.to_string().contains(STEPS))
  };
  let (mut low, mut high): (u32, u32) = (0, 1 << 15);
  assert!(passes(high), "GitLab's schema passes 2^15 steps");
  while low < high {
    let middle = low + (high - low) / 2;
    if passes(middle) {
      high = middle;
    } else {
      low = middle + 1;
    }
  }
  println!("GitLab's schema takes {low} implementation steps");
  assert!(
    (1000..=1 << 15).contains(&low),
    "GitLab's schema takes {low} implementation steps"
  );
}
