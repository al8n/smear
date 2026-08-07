//! The differential oracle: smear's draft §5 validator against `apollo-compiler` 1.32.0.
//!
//! Issue #85's G2. Phase B's gate 1 used the syntactic parser as an oracle for the lossless one;
//! this is the same move one layer up, and it is what turns "our rules agree with themselves"
//! into "our rules agree with a reference implementation".
//!
//! # Verdicts, not diagnostics
//!
//! The comparison is `valid` / `invalid`, and nothing finer. Two mechanical facts decide that:
//!
//! * **apollo skips validate-stage rules inside unused fragments.** A document whose only fault is
//!   in a fragment nobody spreads is invalid to smear (which validates every fragment, by design —
//!   a later merge pass must never meet an unestablished fragment graph) and produces no
//!   validate-stage diagnostic from apollo. The verdicts still agree, because apollo's *build*
//!   stage catches enough of these; the diagnostic sets never would.
//! * **apollo splits its rules across two stages.** Operation-name uniqueness, 5.3.1 and the
//!   negative half of 5.3.3 are build errors, not validation errors, so a harness that called
//!   `validate` alone would silently drop them. Everything here goes through
//!   [`ExecutableDocument::parse_and_validate`][apollo_compiler::ExecutableDocument::parse_and_validate],
//!   which is the only entry point whose result covers both stages.
//!
//! # Two exception lists, pointing opposite ways, and they are not interchangeable
//!
//! * [`whitelist`] holds four **classes**, each a place `apollo-compiler` 1.32.0 is laxer than the
//!   draft. A class excuses smear *rejecting what apollo accepts*, and nothing else. It is declared
//!   per case, by hand, on a document somebody verified.
//! * [`gaps`] holds the rules apollo has and this build of smear does not. A gap excuses smear
//!   *accepting what apollo rejects* — the direction that hides bugs — so it cannot be declared on
//!   a case at all: it is matched from apollo's own typed error names, and it self-destructs when
//!   the rule lands or when nothing exercises it any more.
//!
//! The asymmetry is the whole design. Used naively, an oracle that is missing draft 5.6.3 would not
//! merely fail to catch a bug in smear's 5.6.3 — it would *demand* one, calling smear's correct
//! duplicate-key diagnostic a divergence. Directional exceptions invert that, and keeping the two
//! directions in separate types with separate admission rules is what stops one becoming the other.
//!
//! # What the harness refuses to do
//!
//! It refuses to be a test that agrees on a corpus neither implementation finds interesting.
//! [`Report::compared`] counts only the cases that reached a state where a disagreement was
//! *possible* — both schemas built, both parsers accepted the document — and it is reported next
//! to the total rather than instead of it. [`Report::class_hits`] and [`Report::gap_hits`] count
//! each exception separately, and [`Report::passed`] requires every one of them to be non-zero: an
//! exception nothing exercises is either dead or evidence that the corpus is too thin, and both
//! need saying out loud.
//!
//! It also refuses to be a test that cannot fail. `tests/validator_oracle.rs` runs the whole corpus
//! once per rule with that rule switched off and requires a red each time, so "the oracle agrees"
//! is never true merely because the oracle cannot tell.

pub mod corpus;
pub mod gaps;
pub mod mutate;
pub mod whitelist;

use std::{
  collections::BTreeMap,
  fmt::{self, Write as _},
};

use apollo_compiler::{
  ExecutableDocument as ApolloExecutable, Schema as ApolloSchema, ast as apollo_ast,
  validation::{DiagnosticList, Valid as ApolloValid},
};
use smear::{
  lexer::tokora::{Parse as _, Parser as SmearParser},
  parser::graphql::{
    GraphQL,
    ast::{ExecutableDocument as SmearExecutable, TypeSystemDocument as SmearTypeSystem},
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, executable_document, type_system_document},
  },
  validator::{
    Budget, Collect, Diagnostic, Rule, RuleSet, Schema as SmearSchema, Scratch,
    validate_executable_with,
  },
};

pub use corpus::{Case, Provenance, Suite};
pub use gaps::{Gap, Stage};
pub use whitelist::{Class, Expectation};

// ------------------------------------------------------------------------------------------
// verdicts
// ------------------------------------------------------------------------------------------

/// What a validator said about a document.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Verdict {
  /// No diagnostic: the document is accepted.
  Valid,
  /// At least one diagnostic: the document is rejected.
  Invalid,
}

impl Verdict {
  /// Builds a verdict from the shape every entry point in both libraries returns.
  #[inline]
  const fn from_ok(ok: bool) -> Self {
    if ok { Self::Valid } else { Self::Invalid }
  }
}

impl fmt::Display for Verdict {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(match self {
      Self::Valid => "valid",
      Self::Invalid => "invalid",
    })
  }
}

// ------------------------------------------------------------------------------------------
// the two sides
// ------------------------------------------------------------------------------------------

/// Both implementations' schemas, built from one SDL text.
///
/// Building is the expensive half on both sides and every case in a [`Suite`] shares it, which is
/// also the shape a server has: one schema, many requests.
pub struct Schemas {
  smear: SmearSchema,
  apollo: ApolloValid<ApolloSchema>,
}

impl Schemas {
  /// smear's built schema, for callers that want to time validation against it.
  #[inline]
  pub const fn smear(&self) -> &SmearSchema {
    &self.smear
  }

  /// apollo's validated schema.
  #[inline]
  pub const fn apollo(&self) -> &ApolloValid<ApolloSchema> {
    &self.apollo
  }
}

/// Why a suite produced no comparison.
#[derive(Debug, Clone)]
pub enum SchemaOutcome {
  /// Neither side built it.
  ///
  /// **Agreement, not failure.** The SDL is not a schema and both implementations say so; the
  /// suite simply contributes nothing to the validator comparison. `apollo-rs`'s `diagnostics`
  /// corpus is full of these by design.
  BothRejected,
  /// smear's `Schema::build` rejected an SDL apollo accepted. A §3 divergence, always a failure.
  SmearRejected(String),
  /// apollo rejected an SDL smear built, and no declared gap explains it. A §3 divergence.
  ApolloRejected(String),
  /// apollo rejected an SDL smear built, and every one of apollo's diagnostics belongs to a
  /// declared schema-stage [`Gap`].
  GapExplained(&'static Gap),
}

/// Builds both sides' schemas from one SDL, or says why not.
pub fn build_schemas(sdl: &str) -> Result<Schemas, SchemaOutcome> {
  let smear = build_smear_schema(sdl);
  let apollo = ApolloSchema::parse_and_validate(sdl, "schema.graphql");

  match (smear, apollo) {
    (Ok(smear), Ok(apollo)) => Ok(Schemas { smear, apollo }),
    (Err(smear), Ok(_)) => Err(SchemaOutcome::SmearRejected(smear)),
    (Ok(_), Err(errors)) => {
      let diagnostics = collect_apollo(&errors.errors);
      Err(
        match gaps::attribute(Stage::Schema, &names_of(&diagnostics)) {
          Some(gap) => SchemaOutcome::GapExplained(gap),
          None => SchemaOutcome::ApolloRejected(render(&diagnostics)),
        },
      )
    }
    (Err(_), Err(_)) => Err(SchemaOutcome::BothRejected),
  }
}

/// One of apollo's diagnostics, classified.
#[derive(Debug, Clone)]
pub struct ApolloDiagnostic {
  /// apollo's own internal name for the error variant, when it has one.
  ///
  /// See [`Gap::apollo_error_names`] for why the harness reads this rather than the rendered
  /// message.
  pub name: Option<&'static str>,
  /// The one-line message, for the report.
  pub message: String,
}

fn collect_apollo(errors: &DiagnosticList) -> Vec<ApolloDiagnostic> {
  errors
    .iter()
    .map(|diagnostic| ApolloDiagnostic {
      name: diagnostic.error.unstable_error_name(),
      message: diagnostic.error.to_string(),
    })
    .collect()
}

fn names_of(diagnostics: &[ApolloDiagnostic]) -> Vec<Option<&'static str>> {
  diagnostics
    .iter()
    .map(|diagnostic| diagnostic.name)
    .collect()
}

fn render(diagnostics: &[ApolloDiagnostic]) -> String {
  diagnostics
    .iter()
    .map(|diagnostic| {
      format!(
        "[{}] {}",
        diagnostic.name.unwrap_or("?"),
        diagnostic.message.lines().next().unwrap_or_default()
      )
    })
    .collect::<Vec<_>>()
    .join("; ")
}

/// Parses an SDL and runs smear's draft §3 pass over it, rendering any error.
///
/// Public so `benches/validator_comparison.rs` can time smear's schema build on its own —
/// [`build_schemas`] builds both sides and would put apollo's cost inside smear's row.
pub fn build_smear_schema(sdl: &str) -> Result<SmearSchema, String> {
  let document = SmearParser::with_parser::<
    GraphqlLexer<'_, str>,
    SmearTypeSystem<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(sdl)
  .map_err(|errors| format!("{errors:?}"))?;

  SmearSchema::build(&document).map_err(|errors| errors.to_string())
}

/// Parses an executable document with smear's syntactic parser.
///
/// The error is rendered rather than returned structurally: nothing downstream matches on it, and
/// a `String` keeps the harness's types free of the parser's error generics.
pub fn smear_parse(source: &str) -> Result<SmearExecutable<&str>, String> {
  SmearParser::with_parser::<
    GraphqlLexer<'_, str>,
    SmearExecutable<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(source)
  .map_err(|errors| format!("{errors:?}"))
}

/// Whether `apollo-parser` accepts `source` as syntax, independent of any schema.
///
/// This is deliberately *not*
/// [`ExecutableDocument::parse`][apollo_compiler::ExecutableDocument::parse]: that entry point
/// folds apollo's build-stage rules — operation-name uniqueness among them — in with the syntax
/// errors, so it cannot answer "did the parser accept this". [`apollo_ast::Document::parse`] can,
/// and separating the two is what keeps a parser disagreement from being reported as a validator
/// disagreement.
pub fn apollo_syntax(source: &str) -> Result<(), String> {
  apollo_ast::Document::parse(source, "query.graphql")
    .map(|_| ())
    .map_err(|errors| errors.errors.to_string())
}

// ------------------------------------------------------------------------------------------
// outcomes
// ------------------------------------------------------------------------------------------

/// A comparison that failed.
#[derive(Debug, Clone)]
pub enum Divergence {
  /// **smear accepted a document apollo rejected.**
  ///
  /// The one direction no whitelist class can excuse, because every class records a place apollo
  /// is *missing* a check. If apollo has one that smear does not, smear is missing it — and the
  /// only admissible answer is a declared [`Gap`], which is checked before this variant is
  /// constructed and is not reachable from a case's own declarations.
  SmearLaxer {
    /// apollo's diagnostics, which name what smear failed to catch.
    apollo: String,
  },
  /// smear rejected a document apollo accepted, and no whitelist class covers the case.
  ///
  /// Either a real over-strictness in smear or a fifth apollo gap that has to be measured and
  /// written down before it can be excused.
  UndeclaredStricter {
    /// The rules smear fired, which is the fastest route to deciding which of the two it is.
    rules: Vec<Rule>,
  },
  /// A whitelist class was declared and the verdicts did not come out the way it says they must.
  ///
  /// Usually means the class is stale: apollo implemented the rule, or changed its mind about a
  /// shared deviation. Occasionally means smear lost the rule the class exists to protect.
  WhitelistViolated {
    /// The class the case declared.
    class: Class,
    /// What it required.
    expected: Expectation,
    /// smear's verdict.
    smear: Verdict,
    /// apollo's verdict.
    apollo: Verdict,
    /// The rules smear fired, if any.
    rules: Vec<Rule>,
    /// apollo's diagnostics, if any.
    apollo_diagnostics: String,
  },
  /// One parser accepted what the other rejected.
  ///
  /// Not a validator finding, and reported under its own name so that it cannot be mistaken for
  /// one — but still a failure, because a case that only one side can read is a case the oracle
  /// silently stops covering.
  ParseDivergence {
    /// smear's parse error, when smear is the side that refused.
    smear: Option<String>,
    /// apollo's parse error, when apollo is the side that refused.
    apollo: Option<String>,
  },
}

impl fmt::Display for Divergence {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::SmearLaxer { apollo } => write!(
        f,
        "smear says VALID, apollo says INVALID — smear is LAXER than the oracle, which no \
         whitelist class may excuse.\n    apollo: {}",
        first_line(apollo)
      ),
      Self::UndeclaredStricter { rules } => write!(
        f,
        "smear says INVALID, apollo says VALID, and no whitelist class covers this case.\n    \
         smear fired: {}",
        render_rules(rules)
      ),
      Self::WhitelistViolated {
        class,
        expected,
        smear,
        apollo,
        rules,
        apollo_diagnostics,
      } => write!(
        f,
        "whitelist {class:?} requires {expected:?}, got smear={smear} apollo={apollo}.\n    \
         {class}\n    smear fired: {}\n    apollo: {}",
        render_rules(rules),
        first_line(apollo_diagnostics)
      ),
      Self::ParseDivergence { smear, apollo } => match (smear, apollo) {
        (Some(smear), None) => write!(
          f,
          "smear's parser rejected a document apollo-parser accepted.\n    smear: {}",
          first_line(smear)
        ),
        (None, Some(apollo)) => write!(
          f,
          "apollo-parser rejected a document smear's parser accepted.\n    apollo: {}",
          first_line(apollo)
        ),
        _ => f.write_str("parse divergence with no error recorded (harness bug)"),
      },
    }
  }
}

/// A case that reached a verdict on both sides and matched.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Agreement {
  /// Both said the same thing.
  Identical(Verdict),
  /// They differed exactly as the declared whitelist class says they must.
  Whitelisted(Class),
  /// smear was laxer, and every one of apollo's diagnostics belongs to a rule smear openly does
  /// not implement yet.
  ///
  /// Not agreement in any comfortable sense — it is the harness saying "this is the rule that is
  /// missing, by name, and it is the one you said was missing". [`gaps`] is where the conditions
  /// on that are argued.
  OpenGap(&'static Gap),
}

/// What running one case produced.
#[derive(Debug, Clone)]
pub enum Outcome {
  /// Compared, and the comparison held.
  Agreed(Agreement),
  /// Compared, and it did not.
  Diverged(Divergence),
  /// Both parsers refused the document.
  ///
  /// Not compared and not a failure: the two agree at the syntactic level, which is all a
  /// validator oracle can ask of a document neither validator will ever see. Generated mutants
  /// produce these in bulk and they are counted rather than hidden.
  BothRejectedSyntax,
}

/// One failing case, with everything needed to act on it without re-running the harness.
#[derive(Debug, Clone)]
pub struct Failure {
  /// `suite/case`.
  pub case: String,
  /// Where the document came from.
  pub provenance: Provenance,
  /// The rule the case was written to exercise, when it was written for one.
  ///
  /// Carried for the message only — the per-rule liveness floor in `smear/tests/validator_rules.rs`
  /// is what asserts a rule fires. Here it answers "which rule did this stop covering", which is
  /// what makes a red actionable.
  pub exercises: Option<Rule>,
  /// The document, so a failure can be reproduced by hand.
  pub document: String,
  /// What went wrong.
  pub divergence: Divergence,
}

impl fmt::Display for Failure {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "  {} [{}]", self.case, self.provenance)?;
    if let Some(rule) = self.exercises {
      write!(f, " exercises {rule}")?;
    }
    write!(
      f,
      "\n    {}\n    document: {}",
      self.divergence,
      oneline(&self.document)
    )
  }
}

// ------------------------------------------------------------------------------------------
// the report
// ------------------------------------------------------------------------------------------

/// Everything one run of the oracle observed.
#[derive(Debug, Default, Clone)]
pub struct Report {
  /// Cases run, including the ones no comparison was possible for.
  pub cases: usize,
  /// Cases that reached a **disagreement-capable state**: both schemas built, both parsers
  /// accepted the document, both validators produced a verdict.
  ///
  /// Reported next to [`Report::cases`] rather than instead of it, because the difference between
  /// the two is exactly the amount of corpus the oracle is not actually checking.
  pub compared: usize,
  /// Of the compared cases, how many produced identical verdicts, split by verdict.
  pub agreed_valid: usize,
  /// Compared cases where both said invalid.
  pub agreed_invalid: usize,
  /// Cases where neither parser accepted the document.
  pub both_rejected_syntax: usize,
  /// How many compared cases each whitelist class accounted for.
  pub class_hits: BTreeMap<Class, usize>,
  /// How many cases each declared [`Gap`] accounted for, keyed by draft section.
  pub gap_hits: BTreeMap<&'static str, usize>,
  /// Suites where both sides refused the SDL. Agreement, counted rather than dropped.
  pub schema_both_rejected: usize,
  /// Suites whose SDL only one side accepted, and why. Always a failure unless a schema-stage
  /// [`Gap`] explains it, in which case it is counted in [`Report::gap_hits`] instead.
  pub schema_failures: Vec<(String, SchemaOutcome)>,
  /// Every failure, in corpus order.
  pub failures: Vec<Failure>,
  /// Per-provenance case and comparison counts, so a thin corpus cannot hide behind a fat one.
  pub by_provenance: BTreeMap<Provenance, (usize, usize)>,
}

impl Report {
  /// Whether the run passed.
  ///
  /// Four clauses, and only the first is about divergence:
  ///
  /// 1. no divergence and no one-sided schema;
  /// 2. every whitelist class hit — an excuse nothing exercises is an excuse nobody has checked;
  /// 3. every declared gap hit — a gap that has closed must be deleted, not carried;
  /// 4. no gap left declared for a rule that has since landed.
  ///
  /// Clauses 2 to 4 are what keep the exception list from outgrowing the checks.
  pub fn passed(&self) -> bool {
    self.failures.is_empty()
      && self.schema_failures.is_empty()
      && self.unexercised_classes().is_empty()
      && self.closed_gaps().is_empty()
      && gaps::stale().is_empty()
  }

  /// Classes with no case exercising them.
  pub fn unexercised_classes(&self) -> Vec<Class> {
    Class::ALL
      .iter()
      .copied()
      .filter(|class| self.class_hits.get(class).copied().unwrap_or(0) == 0)
      .collect()
  }

  /// Declared gaps that nothing hit — the corpus still holds the documents and they now agree, so
  /// the rule has landed and the declaration is dead.
  pub fn closed_gaps(&self) -> Vec<&'static Gap> {
    gaps::GAPS
      .iter()
      .filter(|gap| gap.is_live() && self.gap_hits.get(gap.section).copied().unwrap_or(0) == 0)
      .collect()
  }

  fn record(&mut self, provenance: Provenance, compared: bool) {
    let entry = self.by_provenance.entry(provenance).or_insert((0, 0));
    entry.0 += 1;
    if compared {
      entry.1 += 1;
    }
  }
}

impl fmt::Display for Report {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(
      f,
      "differential oracle vs apollo-compiler {APOLLO_VERSION}\n\
       ------------------------------------------------------------------\n\
       cases run                     {:>6}\n\
       reached a comparison          {:>6}\n\
       .. agreed valid               {:>6}\n\
       .. agreed invalid             {:>6}\n\
       .. whitelisted (apollo laxer) {:>6}\n\
       .. open gap    (smear laxer)  {:>6}\n\
       both parsers rejected         {:>6}\n\
       SDL rejected by both          {:>6}",
      self.cases,
      self.compared,
      self.agreed_valid,
      self.agreed_invalid,
      self.class_hits.values().sum::<usize>(),
      self.gap_hits.values().sum::<usize>(),
      self.both_rejected_syntax,
      self.schema_both_rejected,
    )?;

    writeln!(
      f,
      "\nwhitelist classes — apollo laxer than the draft, excused in one direction"
    )?;
    for class in Class::ALL {
      let hits = self.class_hits.get(class).copied().unwrap_or(0);
      writeln!(
        f,
        "  {:<4} {:>4} hit(s)  draft {:<14} {}",
        format!("{class:?}"),
        hits,
        class.section(),
        class.summary()
      )?;
    }

    writeln!(
      f,
      "\nopen gaps — rules apollo has and this build of smear does not"
    )?;
    if gaps::GAPS.is_empty() {
      writeln!(f, "  (none)")?;
    }
    for gap in gaps::GAPS {
      let hits = self.gap_hits.get(gap.section).copied().unwrap_or(0);
      writeln!(
        f,
        "  {:<7} {:>4} hit(s)  {:<10} {} — {}",
        gap.section,
        hits,
        if gap.is_live() { "LIVE" } else { "STALE" },
        gap.title,
        gap.tracking
      )?;
    }

    writeln!(f, "\nby provenance (cases / compared)")?;
    for (provenance, (cases, compared)) in &self.by_provenance {
      writeln!(f, "  {provenance:<16} {cases:>6} / {compared:>6}")?;
    }

    if !self.schema_failures.is_empty() {
      writeln!(f, "\nONE-SIDED SCHEMAS ({})", self.schema_failures.len())?;
      for (suite, failure) in &self.schema_failures {
        writeln!(f, "  {suite}: {failure:?}")?;
      }
    }

    if self.failures.is_empty() {
      writeln!(f, "\nno divergence")
    } else {
      writeln!(f, "\n{} DIVERGENCE(S)", self.failures.len())?;
      for failure in &self.failures {
        writeln!(f, "{failure}")?;
      }
      Ok(())
    }
  }
}

/// The pinned oracle version, reproduced in the report so a printed run identifies its oracle.
pub const APOLLO_VERSION: &str = "1.32.0";

// ------------------------------------------------------------------------------------------
// the runner
// ------------------------------------------------------------------------------------------

/// The differential runner.
///
/// Holds the one piece of state the smear side is designed around — a caller-owned [`Scratch`] —
/// so that running a thousand cases reuses one working set, exactly as a server would.
pub struct Oracle {
  scratch: Scratch,
  budget: Budget,
  rules: RuleSet,
}

impl Default for Oracle {
  fn default() -> Self {
    Self::new()
  }
}

impl Oracle {
  /// A runner with a fresh scratch, the default budget and every rule on.
  pub fn new() -> Self {
    Self {
      scratch: Scratch::new(),
      budget: Budget::default(),
      rules: RuleSet::ALL,
    }
  }

  /// A runner with a **narrowed** rule set — a deliberately weakened smear.
  ///
  /// This is how the harness proves it discriminates. Dropping a rule makes smear genuinely laxer
  /// on every document that rule was the only thing catching, and a harness worth having must
  /// then red. `tests/validator_oracle.rs` runs the whole corpus once per rule this way and
  /// asserts the red, so the discrimination property is gated on every run rather than
  /// demonstrated once by hand.
  pub fn without(rule: Rule) -> Self {
    Self {
      rules: RuleSet::ALL.without(rule),
      ..Self::new()
    }
  }

  /// Runs every suite and folds the outcomes into one report.
  pub fn run(&mut self, suites: &[Suite]) -> Report {
    let mut report = Report::default();
    for suite in suites {
      self.run_suite(suite, &mut report);
    }
    report
  }

  /// Runs one suite into an existing report.
  pub fn run_suite(&mut self, suite: &Suite, report: &mut Report) {
    let schemas = match build_schemas(&suite.sdl) {
      Ok(schemas) => schemas,
      Err(outcome) => {
        report.cases += suite.cases.len();
        for case in &suite.cases {
          report.record(case.provenance, false);
        }
        match outcome {
          SchemaOutcome::BothRejected => report.schema_both_rejected += 1,
          SchemaOutcome::GapExplained(gap) => {
            *report.gap_hits.entry(gap.section).or_insert(0) += 1;
          }
          failure => report.schema_failures.push((suite.name.clone(), failure)),
        }
        return;
      }
    };

    for case in &suite.cases {
      report.cases += 1;
      let outcome = self.run_case(&schemas, case);
      let compared = matches!(outcome, Outcome::Agreed(_) | Outcome::Diverged(_))
        && !matches!(
          outcome,
          Outcome::Diverged(Divergence::ParseDivergence { .. })
        );
      report.record(case.provenance, compared);

      match outcome {
        Outcome::Agreed(Agreement::Identical(Verdict::Valid)) => report.agreed_valid += 1,
        Outcome::Agreed(Agreement::Identical(Verdict::Invalid)) => report.agreed_invalid += 1,
        Outcome::Agreed(Agreement::Whitelisted(class)) => {
          *report.class_hits.entry(class).or_insert(0) += 1;
        }
        Outcome::Agreed(Agreement::OpenGap(gap)) => {
          *report.gap_hits.entry(gap.section).or_insert(0) += 1;
        }
        Outcome::BothRejectedSyntax => report.both_rejected_syntax += 1,
        Outcome::Diverged(divergence) => report.failures.push(Failure {
          case: format!("{}/{}", suite.name, case.name),
          provenance: case.provenance,
          exercises: case.exercises,
          document: case.document.clone(),
          divergence,
        }),
      }

      if compared {
        report.compared += 1;
      }
    }
  }

  /// Runs one case against an already-built pair of schemas.
  pub fn run_case(&mut self, schemas: &Schemas, case: &Case) -> Outcome {
    let smear_parsed = smear_parse(&case.document);
    let apollo_parsed = apollo_syntax(&case.document);

    let smear_document = match (smear_parsed, apollo_parsed) {
      (Ok(document), Ok(())) => document,
      (Err(_), Err(_)) => return Outcome::BothRejectedSyntax,
      (Err(smear), Ok(())) => {
        return Outcome::Diverged(Divergence::ParseDivergence {
          smear: Some(smear),
          apollo: None,
        });
      }
      (Ok(_), Err(apollo)) => {
        return Outcome::Diverged(Divergence::ParseDivergence {
          smear: None,
          apollo: Some(apollo),
        });
      }
    };

    let mut diagnostics = Vec::new();
    let mut sink = Collect::new(&mut diagnostics);
    let smear_verdict = Verdict::from_ok(
      validate_executable_with(
        schemas.smear(),
        &smear_document,
        &mut self.scratch,
        &self.budget,
        self.rules,
        &mut sink,
      )
      .is_ok(),
    );
    let rules = distinct_rules(&diagnostics);

    // `parse_and_validate` rather than `validate`: apollo's build stage carries rules its validate
    // stage does not, and only this entry point's result covers both. See the module header.
    let apollo_result =
      ApolloExecutable::parse_and_validate(schemas.apollo(), &case.document, "query.graphql");
    let (apollo_verdict, apollo_diagnostics) = match apollo_result {
      Ok(_) => (Verdict::Valid, Vec::new()),
      Err(errors) => (Verdict::Invalid, collect_apollo(&errors.errors)),
    };

    compare(
      case,
      smear_verdict,
      &rules,
      apollo_verdict,
      &apollo_diagnostics,
    )
  }
}

/// The whole judgement, in one place, so the asymmetry is readable rather than distributed.
fn compare(
  case: &Case,
  smear: Verdict,
  rules: &[Rule],
  apollo: Verdict,
  apollo_diagnostics: &[ApolloDiagnostic],
) -> Outcome {
  let agreed = smear == apollo;

  match case.class {
    // A declared class is a *requirement*, both ways. If the verdicts stop matching what the class
    // says, the class is stale (apollo grew the rule) or smear lost the rule the class protects,
    // and either way it must be looked at rather than absorbed.
    Some(class) => {
      let held = match class.expectation() {
        Expectation::SmearStricter => smear == Verdict::Invalid && apollo == Verdict::Valid,
        Expectation::AgreeValid => smear == Verdict::Valid && apollo == Verdict::Valid,
      };
      if held {
        Outcome::Agreed(Agreement::Whitelisted(class))
      } else {
        Outcome::Diverged(Divergence::WhitelistViolated {
          class,
          expected: class.expectation(),
          smear,
          apollo,
          rules: rules.to_vec(),
          apollo_diagnostics: render(apollo_diagnostics),
        })
      }
    }
    None if agreed => Outcome::Agreed(Agreement::Identical(smear)),
    // The two directions are not symmetric and must never be written as one branch.
    //
    // smear laxer is admissible only against a *declared* gap, and only when every one of apollo's
    // diagnostics belongs to it. The case cannot opt into that: unlike a whitelist class, a gap is
    // matched from apollo's own typed error names, so no corpus entry can silently acquire
    // permission to be laxer by having something written next to it.
    None if smear == Verdict::Valid => {
      match gaps::attribute(Stage::Executable, &names_of(apollo_diagnostics)) {
        Some(gap) => Outcome::Agreed(Agreement::OpenGap(gap)),
        None => Outcome::Diverged(Divergence::SmearLaxer {
          apollo: render(apollo_diagnostics),
        }),
      }
    }
    // smear stricter. Excusable by a class the case *permits* — the generated family's only door,
    // decided at the seed. `SmearStricter` is spelled out rather than assumed: a permission for a
    // class whose expectation is agreement would otherwise excuse a divergence its own class says
    // should not happen.
    None => match case.permits {
      Some(class) if class.expectation() == Expectation::SmearStricter => {
        Outcome::Agreed(Agreement::Whitelisted(class))
      }
      _ => Outcome::Diverged(Divergence::UndeclaredStricter {
        rules: rules.to_vec(),
      }),
    },
  }
}

fn distinct_rules(diagnostics: &[Diagnostic<&str>]) -> Vec<Rule> {
  let mut rules: Vec<_> = diagnostics.iter().map(Diagnostic::rule).collect();
  rules.sort_unstable();
  rules.dedup();
  rules
}

fn render_rules(rules: &[Rule]) -> String {
  if rules.is_empty() {
    return "(nothing)".to_owned();
  }
  let mut out = String::new();
  for (index, rule) in rules.iter().enumerate() {
    if index > 0 {
      out.push_str(", ");
    }
    let _ = write!(out, "{rule}");
  }
  out
}

fn first_line(text: &str) -> &str {
  text
    .lines()
    .find(|line| !line.trim().is_empty())
    .unwrap_or("(no diagnostics)")
}

fn oneline(text: &str) -> String {
  let flat: String = text.split_whitespace().collect::<Vec<_>>().join(" ");
  if flat.len() > 200 {
    format!("{}…", &flat[..200])
  } else {
    flat
  }
}
