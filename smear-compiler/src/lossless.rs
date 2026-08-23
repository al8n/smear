//! The lossless door: draft §5 and draft §3 over a rowan CST.
//!
//! This module is private and its header is not published. What a caller has to know is stated on
//! [`validate_executable_lossless`], on [`validate_schema_lossless`] and on [`Recovery`], where
//! the published documentation will find it; what is here is the reasoning behind the shape, for a
//! reader of the source.
//!
//! # One validator, two doors
//!
//! [`validate_executable`](super::validate_executable) takes the AST the syntactic parser
//! produces. This takes the [`Parse`] the lossless parser produces, and answers the same
//! diagnostics — the same rules, the same order, the same spans into the same bytes — because it
//! **is** that function. There is no second validator, no shared view trait, no enum a rule
//! dispatches through: the CST is projected to the AST by
//! [`project_executable_document_recovered`], and the projection is handed to
//! [`validate_executable_with`](super::validate_executable_with) unchanged.
//!
//! [`validate_schema_lossless`] is that sentence again one section of the specification over.
//! Draft §3 lives inside [`Schema::build`], which is where an SDL author's refusals come from
//! through every other door — the syntactic parser's, and
//! `Schema::from_introspection`'s, which renders the response as SDL and hands it to the same
//! builder. So the third door does the same: project, then build. One §3 implementation, reached
//! three ways, and `tests/validator_lossless_schema.rs` compares two of them over the SDL corpus
//! the way `tests/validator_lossless.rs` compares the other pair.
//!
//! That is a deliberate refusal of the obvious alternative. A validator generic over "AST or CST"
//! would put a branch, or a virtual call, on the hot path of every rule — to buy nothing, since
//! the two inputs describe the same document. Composition costs one projection, once, on the leg
//! that already allocated a whole tree, and it makes the two doors incapable of drifting apart:
//! `tests/validator_lossless.rs` compares them over the rule corpus and would have nothing to
//! compare if they were one call.
//!
//! # Why the recovering projection, and not the fail-fast one
//!
//! A lossless CST exists so it can represent a document somebody is still typing. That is the
//! only case this door is for — a caller holding a document that parses cleanly can use either
//! door and should use the cheaper one — and [`project_executable_document`]'s answer for a
//! half-typed document is `Err`, which for an editor means no diagnostics at all on the four
//! definitions that were fine.
//!
//! So the door recovers: each top-level definition is projected on its own, the ones that have an
//! AST image are validated, and the ones that do not are counted into a [`Recovery`] the caller
//! gets back in **both** arms of the result. Read it. A document one definition was dropped from
//! can both hide a finding and invent one, and [`Recovery`] is the only thing that says it
//! happened — the sink cannot, because a projection refusal is not a draft §5 finding and does
//! not become one here.
//!
//! # What a skipped definition does to the verdict
//!
//! It is not neutral, and the alternative is worse. Draft §5 has rules that read the document as
//! a whole — 5.5.2.1 (a spread must name a defined fragment), 5.5.1.4 (a fragment must be used) —
//! and those rules cannot distinguish a fragment that was never written from one that was
//! dropped. `query { ...f } fragment f on Dog { name @ }` therefore reports 5.5.2.1 against
//! `...f`, a line the author did not break.
//!
//! The three ways out were weighed:
//!
//! - **Suppress the affected rules when anything was skipped.** It forks the rule set by input
//!   kind, which is the second validator this design exists to refuse.
//! - **Refuse the document, as the fail-fast projection does.** It is the outcome that makes the
//!   lossless leg pointless, and it is what the door is here to replace.
//! - **Report it, and say so.** A caller with [`Recovery::is_complete`] false knows the verdict
//!   is partial and can render accordingly; a caller with it true is holding exactly what the
//!   syntactic door would have said. That is this door's choice, and
//!   `tests/validator_lossless.rs` pins the artifact rather than leaving it to be discovered.
//!
//! # What a skipped definition does to a *schema*
//!
//! The same shape of answer, and a larger blast radius, which is why
//! [`validate_schema_lossless`] states it separately rather than pointing here. Draft §5's
//! whole-document rules are two; draft §3 is nothing but whole-document rules, because a type is
//! what every reference to it resolves against. Dropping one definition therefore invents an
//! [`UndefinedType`](super::SchemaErrorKind::UndefinedType) at every mention of it, and dropping
//! the one that happened to be `Query` invents a
//! [`MissingQueryRootOperationType`](super::SchemaErrorKind::MissingQueryRootOperationType) for
//! the whole document.
//!
//! The three ways out were weighed again and answered the same way, for the same reasons: a §3
//! pass that skipped the rules a skip can disturb would be a second builder, and refusing the
//! document is what this door exists to replace. So it reports, the [`Recovery`] rides along in
//! both arms, and `tests/validator_lossless_schema.rs` pins both artifacts.
//!
//! [`project_executable_document`]: smear_parser::graphql::lossless::project_executable_document
//! [`project_executable_document_recovered`]: smear_parser::graphql::lossless::project_executable_document_recovered
//! [`Schema::build`]: super::Schema::build

use smear_parser::graphql::lossless::{
  Parse, project_executable_document_recovered, project_type_system_document_recovered,
};

pub use smear_parser::lossless::project::Recovery;

use tokora::SimpleSpan;

use super::{
  Budget, Diagnostic, Invalid, Rule, RuleSet, Schema, SchemaErrors, Scratch, Sink,
  diagnostic::Context,
  executable::{Ledger, units, validate_charged},
};

/// The verdict of a failed lossless validation.
///
/// [`Invalid`] plus the [`Recovery`] the successful arm carries, so the two facts a caller needs
/// — *what was wrong* and *how much of the document was looked at* — arrive together whichever
/// way the result went.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LosslessInvalid {
  invalid: Invalid,
  recovery: Recovery,
  projection_ran: bool,
}

impl LosslessInvalid {
  /// Returns the verdict the rules produced, exactly as the syntactic door would report it.
  #[inline]
  pub const fn invalid(&self) -> Invalid {
    self.invalid
  }

  /// Returns how much of the parse had an AST image.
  ///
  /// [`Recovery::is_complete`] false means at least one of these diagnostics may be an artifact
  /// of a definition that was dropped rather than of one the author wrote — see
  /// [`validate_executable_lossless`].
  ///
  /// **Read [`projection_ran`](Self::projection_ran) first.** When it is false this is not a
  /// measurement of anything: nothing was projected and nothing counted what was not.
  #[inline]
  pub const fn recovery(&self) -> Recovery {
    self.recovery
  }

  /// Whether the projection ran at all.
  ///
  /// False on exactly one path: [`Budget::validation_work`](super::Budget::validation_work) could
  /// not pay for the projection, so the door refused before building any AST. On that path
  /// [`recovery`](Self::recovery) reports `0` projected and `1` skipped, and **that `1` is a floor
  /// rather than [`Recovery::skipped`]'s usual ceiling** — every top-level element was dropped and
  /// counting them is precisely the walk the refusal declined to make.
  ///
  /// It is an accessor and not a sentence in a doc comment because the two readings of `skipped`
  /// are opposite: the ordinary one over-counts and this one under-counts, and a caller comparing
  /// it against anything needs to know which it holds. The first version of this path shipped the
  /// `1` with the disclosure in prose, which is the same defect one level up — a wrong number is
  /// not made right by being documented.
  #[inline]
  pub const fn projection_ran(&self) -> bool {
    self.projection_ran
  }
}

impl From<LosslessInvalid> for Invalid {
  #[inline]
  fn from(value: LosslessInvalid) -> Self {
    value.invalid
  }
}

impl core::fmt::Display for LosslessInvalid {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Display::fmt(&self.invalid, f)?;
    if !self.recovery.is_complete() {
      write!(f, " ({} skipped by recovery)", self.recovery.skipped())?;
    }
    Ok(())
  }
}

impl core::error::Error for LosslessInvalid {}

/// Validates a lossless executable parse against a schema, checking every draft §5 rule.
///
/// [`validate_executable`](super::validate_executable)'s twin for the CST. `parse` is what
/// [`parse_executable_document`](smear_parser::graphql::lossless::parse_executable_document)
/// returned and `source` is the text it was parsed from — the pair is **verified, not trusted**,
/// so a mismatched one is refused by the projection rather than validated against unrelated
/// bytes. `scratch`, `budget` and `sink` are the syntactic door's, unchanged and reusable across
/// both.
///
/// Returns `Ok(recovery)` when no rule fired and `Err` when at least one did; either way the
/// [`Recovery`] says how much of the parse had an AST image.
///
/// # One validator, not two
///
/// This is [`validate_executable`](super::validate_executable): the CST is projected to the AST
/// the syntactic parser would have built for the same bytes, and that AST is validated by the
/// same code, under the same rules, in the same order. There is no second rule engine, no shared
/// view trait and no branch on the input kind anywhere in a rule.
/// `tests/validator_lossless.rs` compares the two doors over the whole draft §5 rule corpus,
/// diagnostic for diagnostic and span for span, so the composition is measured rather than
/// asserted.
///
/// # It recovers, and the [`Recovery`] is part of the answer
///
/// A CST exists so it can represent a document somebody is still typing, so this door does not
/// refuse one. Each top-level definition is projected on its own; the ones with an AST image are
/// validated and the ones without are skipped and counted.
///
/// **Read the [`Recovery`] before you believe the verdict.** With
/// [`is_complete`](Recovery::is_complete) true, this is exactly what the syntactic door would
/// have said. With it false, two things follow, and neither is correctable here:
///
/// - a clean verdict is weaker than it looks — nothing examined what was skipped, and a parse
///   with nothing projectable in it validates as `Ok` with an empty sink; and
/// - a finding may be an **artifact** of the skip. Draft §5 has rules that read the document as a
///   whole — 5.5.2.1 (a spread names a defined fragment), 5.5.1.4 (a fragment is used) — and none
///   of them can tell a fragment that was never written from one that was dropped. Suppressing
///   them would fork the rule set by input kind, which is the second validator this design
///   refuses; refusing the document is the outcome this door exists to replace. So it reports,
///   and says so here.
///
/// A projection refusal never reaches the `sink`: it is not a draft §5 finding and does not
/// become one. The parse's own diagnostics already describe the syntax that broke.
///
/// # Example
///
/// ```
/// # #[cfg(feature = "rowan")] {
/// use smear_compiler::{Budget, First, Rule, Schema, Scratch, validate_executable_lossless};
/// use smear_parser::{
///   graphql::{
///     GraphQL,
///     ast::TypeSystemDocument,
///     error::GraphqlErrors,
///     lossless::parse_executable_document,
///     syntactic::{GraphqlLexer, type_system_document},
///   },
///   lexer::tokora::{Parse as _, Parser},
/// };
///
/// let schema = Schema::build(
///   &Parser::with_parser::<GraphqlLexer<'_, str>, TypeSystemDocument<&str>, GraphqlErrors<&str>, _, GraphQL>(
///     type_system_document,
///   )
///   .parse_str("type Query { hero: Character } interface Character { name: String! }")
///   .expect("the SDL parses"),
/// )
/// .expect("the SDL is a schema");
///
/// let mut scratch = Scratch::new();
/// let budget = Budget::default();
///
/// // A document an editor is in the middle of: the first operation is finished, the second is
/// // three keystrokes in. The finished one is still validated.
/// let source = "{ hero { title } }\nquery Half(";
/// let parse = parse_executable_document(source);
/// assert!(parse.has_errors());
///
/// let mut sink = First::new();
/// let refused =
///   validate_executable_lossless(&schema, &parse, source, &mut scratch, &budget, &mut sink)
///     .expect_err("`title` is not a field of `Character`");
///
/// assert_eq!(refused.invalid().emitted(), 1);
/// assert_eq!(refused.recovery().projected(), 1);
/// assert!(!refused.recovery().is_complete());
///
/// let diagnostic = sink.get().expect("a diagnostic");
/// assert_eq!(diagnostic.rule(), Rule::FieldSelections);
/// // The same bytes the syntactic door would have blamed.
/// assert_eq!(&source[diagnostic.span().start()..diagnostic.span().end()], "title");
/// # }
/// ```
pub fn validate_executable_lossless<'src, K>(
  schema: &Schema,
  parse: &Parse,
  source: &'src str,
  scratch: &mut Scratch,
  budget: &Budget,
  sink: &mut K,
) -> Result<Recovery, LosslessInvalid>
where
  K: Sink<&'src str>,
{
  validate_executable_lossless_with(schema, parse, source, scratch, budget, RuleSet::ALL, sink)
}

/// Validates a lossless executable parse against a subset of the rules.
///
/// [`validate_executable_with`](super::validate_executable_with)'s twin, and
/// [`validate_executable_lossless`]'s `rules` sibling: a rule outside `rules` is not evaluated,
/// not merely filtered. With [`RuleSet::ALL`] this is exactly [`validate_executable_lossless`].
///
/// The projection is not part of the rule set and runs whatever `rules` says — it is what
/// produces the document the rules read, so an empty [`RuleSet`] still costs it. What that buys
/// is a caller who wants only the [`Recovery`]: `RuleSet::empty()` answers "how much of this
/// parse has an AST image" and nothing else.
///
/// # The projection is charged, and it is charged before it runs
///
/// [`Budget::validation_work`](super::Budget::validation_work) opens **here**, not inside the
/// validator, and the projection is the first thing to spend from it. That is the whole reason it
/// is one ledger and not two: the projection allocates an entire AST before a rule exists to
/// refuse anything, and a bound that starts after it has already lost.
///
/// The charge is `units` of the prepayment size below, taken in one payment rather than
/// incrementally. Incrementally would be better and is not available:
/// `project_executable_document_recovered` lives in `smear-parser`, and threading a validation
/// ledger into it would put a compiler concept in the parser. What makes the prepayment sound
/// instead of merely convenient is that it is an **upper bound** obtainable in constant time — the
/// projection builds at most one AST node per CST token and a token is at least one byte — and
/// `units` is what turns that into the same unit every rule spends.
///
/// # It is priced over **both** inputs, because they are two parameters and nothing pairs them
///
/// `parse` and `source` are separate arguments and no type says they describe the same bytes. The
/// prepayment was `source.len()` alone, and the projector walks the **`Parse`**: a tree of `N`
/// top-level definitions handed in beside an *empty* source paid one unit, and the recovering
/// projector then visited all `N` CST children, rejected each on a source mismatch, and returned
/// `Ok` with an empty AST and an incomplete [`Recovery`]. The bound was priced from one input and
/// spent on the other.
///
/// So it is priced from `max(source.len(), parse.green().text_len())` — still constant time, both
/// a green length and a slice length being `O(1)` — which upper-bounds the traversal whichever
/// input is the larger and whether or not the two agree. Rejecting a mismatch outright was the
/// alternative and is worse: the projection already answers a mismatch with [`Recovery`] rather
/// than with a refusal, and turning that into an error would be a new way to fail for callers who
/// are not doing anything wrong.
///
/// It was the pass al8n/smear#198's table could not place, and "bounded by the document" was the
/// wrong answer: the parser's own limits bound the CST's *shape* and not its *size*, and the size
/// is the thing an adversary picks.
///
/// **This is the one place the two doors do not answer identically, and it is not a drift.** The
/// module header's promise — the same rules, the same order, the same spans — is about the *rules*,
/// and it holds: they are the same call. What differs is the resource, because this door does
/// strictly more work than the syntactic one and is charged for it, so with
/// [`Budget::validation_work`](super::Budget::validation_work) set low enough to refuse a document
/// at all, this door refuses earlier — at the whole input's span, before a rule ran — where the
/// syntactic door would have refused mid-walk at a node. At any budget that does not refuse, the
/// two are diagnostic-for-diagnostic identical, which is what `tests/validator_lossless.rs`
/// compares.
///
/// # What a refusal here looks like
///
/// The same thing every other refusal looks like: `Err`, with
/// [`Invalid::budget_tripped`](super::Invalid::budget_tripped) set, and
/// [`Rule::ValidationWorkBudget`](super::Rule::ValidationWorkBudget) in the sink when the rule set
/// contains it. [`LosslessInvalid::projection_ran`] is **false**, and that is the load-bearing
/// half: on this path [`Recovery`] is not a measurement. It reports `0` projected, which is exact,
/// and `1` skipped, which is a floor rather than [`Recovery::skipped`]'s usual ceiling.
///
/// The `1` shipped once as a floor disclosed in prose, and prose is the wrong place for it: the two
/// readings of `skipped` are opposite — the ordinary one over-counts, this one under-counts — so a
/// caller needs the difference in the type. Counting the elements exactly is the walk the refusal
/// declined to make, and doing it here from the green root's children would put a second copy of
/// `recovered_top_level`'s idea of where the top level is into this crate, where it could drift.
/// [`Recovery::is_complete`] is false either way, which is the half nothing may get wrong.
pub fn validate_executable_lossless_with<'src, K>(
  schema: &Schema,
  parse: &Parse,
  source: &'src str,
  scratch: &mut Scratch,
  budget: &Budget,
  rules: RuleSet,
  sink: &mut K,
) -> Result<Recovery, LosslessInvalid>
where
  K: Sink<&'src str>,
{
  let size = source.len().max(usize::from(parse.green().text_len()));
  let Some(left) = Ledger::open(budget).take(units(size)) else {
    let (emitted, stopped) = refuse_projection(source, budget, rules, sink);
    return Err(LosslessInvalid {
      invalid: Invalid::refused(emitted, stopped),
      // Nothing was projected, and the count of what was not is the green root's child list:
      // exact or over, never under, and `O(1)`. See this function's header.
      // A floor, not a ceiling, and `projection_ran` is what says so.
      recovery: Recovery::new(0, 1),
      projection_ran: false,
    });
  };
  let (document, recovery) = project_executable_document_recovered(parse, source);
  match validate_charged(schema, &document, scratch, budget, rules, sink, left) {
    Ok(()) => Ok(recovery),
    Err(invalid) => Err(LosslessInvalid {
      invalid,
      recovery,
      projection_ran: true,
    }),
  }
}

/// Reports a projection the budget would not pay for.
///
/// Returns how many diagnostics that was and whether the sink asked to stop. Zero and `false` when
/// the rule is filtered out, which is the case the verdict has to survive: switching a bound's rule
/// off switches off its *diagnostic*, never the refusal.
///
/// The sink's answer is **returned rather than discarded**. A [`First`](super::First) sink breaks
/// on the diagnostic it keeps, and dropping that made a verdict which had told a sink to stop
/// report [`Invalid::stopped`](super::Invalid::stopped) as false — the same "gave up, said
/// finished" shape one axis over.
fn refuse_projection<'src, K>(
  source: &'src str,
  budget: &Budget,
  rules: RuleSet,
  sink: &mut K,
) -> (u32, bool)
where
  K: Sink<&'src str>,
{
  if !rules.contains(Rule::ValidationWorkBudget) {
    return (0, false);
  }
  // The whole input, because the whole input is what could not be afforded. There is no narrower
  // node to point at: the nodes are what the projection would have built.
  let span = SimpleSpan::new(0, source.len());
  let diagnostic = Diagnostic::new(Rule::ValidationWorkBudget, span)
    .context(Context::Count(budget.validation_work()));
  let stopped = sink.diagnostic(diagnostic).is_break();
  (1, stopped)
}

// ---------------------------------------------------------------------------------------------
// the SDL half
// ---------------------------------------------------------------------------------------------

/// The verdict of a failed lossless schema build.
///
/// [`SchemaErrors`] plus the [`Recovery`] the successful arm carries — [`LosslessInvalid`]'s twin,
/// so the two facts a caller needs, *what was wrong* and *how much of the document was looked at*,
/// arrive together whichever way the result went.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LosslessSchemaErrors {
  errors: SchemaErrors,
  recovery: Recovery,
}

impl LosslessSchemaErrors {
  /// Returns why the build refused, exactly as [`Schema::build`](super::Schema::build) reports it.
  #[inline]
  pub const fn errors(&self) -> &SchemaErrors {
    &self.errors
  }

  /// Returns how much of the parse had an AST image.
  ///
  /// [`Recovery::is_complete`] false means at least one of these refusals may be an artifact of a
  /// definition that was dropped rather than of one the author wrote — see
  /// [`validate_schema_lossless`].
  #[inline]
  pub const fn recovery(&self) -> Recovery {
    self.recovery
  }

  /// Consumes this verdict and returns the refusals alone.
  #[inline]
  pub fn into_errors(self) -> SchemaErrors {
    self.errors
  }
}

impl From<LosslessSchemaErrors> for SchemaErrors {
  #[inline]
  fn from(value: LosslessSchemaErrors) -> Self {
    value.errors
  }
}

impl core::fmt::Display for LosslessSchemaErrors {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Display::fmt(&self.errors, f)?;
    if !self.recovery.is_complete() {
      write!(f, "\n  ({} skipped by recovery)", self.recovery.skipped())?;
    }
    Ok(())
  }
}

impl core::error::Error for LosslessSchemaErrors {}

/// Builds a schema from a lossless type-system parse, checking every draft §3 rule.
///
/// [`validate_executable_lossless`]'s twin one section of the specification over, and
/// [`Schema::build`](super::Schema::build)'s twin for the CST. `parse` is what
/// [`parse_type_system_document`](smear_parser::graphql::lossless::parse_type_system_document)
/// returned and `source` is the text it was parsed from — the pair is **verified, not trusted**,
/// so a mismatched one is refused by the projection rather than built against unrelated bytes.
///
/// Returns `Ok((schema, recovery))` when the SDL is a schema and `Err` when it is not; either way
/// the [`Recovery`] says how much of the parse had an AST image.
///
/// # One §3 pass, not two
///
/// This *is* [`Schema::build`](super::Schema::build): the CST is projected to the
/// `TypeSystemDocument` the syntactic parser would have built for the same bytes, and that
/// document is handed to the same builder, which runs the same rules in the same order and points
/// at the same bytes. There is no second type-system pass anywhere in the crate — the
/// introspection door renders its response as SDL and arrives at the same builder for the same
/// reason. `tests/validator_lossless_schema.rs` compares this door against the syntactic one over
/// the SDL refusal corpus, error for error and span for span.
///
/// # It recovers, and the [`Recovery`] is part of the answer
///
/// A CST exists so it can represent a document somebody is still typing, so this door does not
/// refuse one. Each top-level definition is projected on its own; the ones with an AST image are
/// built and the ones without are skipped and counted.
///
/// **Read the [`Recovery`] before you believe the verdict**, and here more carefully than at the
/// executable door. With [`is_complete`](Recovery::is_complete) true this is exactly what
/// [`Schema::build`](super::Schema::build) would have said. With it false, draft §3 is a
/// whole-document pass over a document that is missing a piece, so:
///
/// - a refusal may be an **artifact** of the skip. Every reference to a dropped type is an
///   [`UndefinedType`](super::SchemaErrorKind::UndefinedType), and a dropped `Query` is a
///   [`MissingQueryRootOperationType`](super::SchemaErrorKind::MissingQueryRootOperationType) for
///   the whole document; and
/// - an `Ok` is a schema built from **less SDL than the author wrote**. It is a real, internally
///   consistent schema, and it is not the one on screen — validating an operation against it can
///   blame a field the missing half defines.
///
/// A projection refusal never reaches the returned [`SchemaErrors`]: it is not a draft §3 finding
/// and does not become one. The parse's own diagnostics already describe the syntax that broke.
///
/// # Several documents
///
/// [`SchemaBuilder`](super::SchemaBuilder) is still the door for a schema that spans more than one
/// file, and it is reachable from here:
/// [`project_type_system_document_recovered`](smear_parser::graphql::lossless::project_type_system_document_recovered)
/// is public and answers the `(document, recovery)` pair this function feeds the one-document
/// case with.
///
/// # Example
///
/// ```
/// # #[cfg(feature = "rowan")] {
/// use smear_compiler::{SchemaErrorKind, validate_schema_lossless};
/// use smear_parser::graphql::lossless::parse_type_system_document;
///
/// // An SDL an editor is in the middle of: the interface is finished and wrong, the field after
/// // it has no type yet. The finished half is still checked.
/// let source = "type Query { hero: Character }\ninterface Character { name: Nope }\ntype Half { f: }";
/// let parse = parse_type_system_document(source);
/// assert!(parse.has_errors());
///
/// let refused = validate_schema_lossless(&parse, source).expect_err("`Nope` is not a type");
///
/// assert_eq!(refused.errors().kinds(), [SchemaErrorKind::UndefinedType]);
/// assert_eq!(refused.recovery().projected(), 2);
/// assert!(!refused.recovery().is_complete());
///
/// let error = &refused.errors().errors()[0];
/// // The same bytes the syntactic door would have blamed.
/// let span = error.span();
/// assert_eq!(&source[span.start()..span.end()], "Nope");
/// # }
/// ```
pub fn validate_schema_lossless(
  parse: &Parse,
  source: &str,
) -> Result<(Schema, Recovery), LosslessSchemaErrors> {
  let (document, recovery) = project_type_system_document_recovered(parse, source);
  match Schema::build(&document) {
    Ok(schema) => Ok((schema, recovery)),
    Err(errors) => Err(LosslessSchemaErrors { errors, recovery }),
  }
}
