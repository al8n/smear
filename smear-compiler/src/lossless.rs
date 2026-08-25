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
  Budget, Diagnostic, Invalid, Refusal, Rule, RuleSet, Schema, SchemaErrors, Scratch, Sink,
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
  /// `None` when the projection never ran, which is a *different thing* from a projection that ran
  /// and dropped nothing — and the only representation that cannot be confused with one.
  recovery: Option<Recovery>,
}

impl LosslessInvalid {
  /// Returns the verdict the rules produced, exactly as the syntactic door would report it.
  #[inline]
  pub const fn invalid(&self) -> Invalid {
    self.invalid
  }

  /// Returns how much of the parse had an AST image, when the projection ran.
  ///
  /// [`Recovery::is_complete`] false means at least one of these diagnostics may be an artifact
  /// of a definition that was dropped rather than of one the author wrote — see
  /// [`validate_executable_lossless`].
  ///
  /// # `None` means the projection never ran
  ///
  /// One path produces it: [`Budget::validation_work`](super::Budget::validation_work) could not
  /// pay for the projection, so the door refused before building any AST. There is no [`Recovery`]
  /// to report because nothing examined anything, and a caller that unwraps this gets an
  /// `Option`'s answer rather than a number.
  ///
  /// It took three rounds to arrive at the absence. The count was `1`, disclosed in prose as a
  /// floor; then `1` with a `projection_ran()` flag beside it saying which way to read it. Both
  /// still *constructed* the number, so both still let it be compared, printed and believed — and
  /// for an empty or trivia-only parse the real skipped count is **zero**, so `1` was not even the
  /// floor it claimed. A value that must not be read is a value that must not exist; that is the
  /// same repair as `Ledger::Off`, one type over. al8n/smear#198.
  #[inline]
  pub const fn recovery(&self) -> Option<Recovery> {
    self.recovery
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
    // Branching on the state, not on a count synthesised to keep this arm total. The `1` this used
    // to be handed rendered as the exact text "1 skipped by recovery" for a projection that had not
    // looked at anything.
    match self.recovery {
      None => f.write_str(" (nothing was projected)"),
      Some(recovery) if !recovery.is_complete() => {
        write!(f, " ({} skipped by recovery)", recovery.skipped())
      }
      Some(_) => Ok(()),
    }
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
/// let recovery = refused.recovery().expect("the projection ran");
/// assert_eq!(recovery.projected(), 1);
/// assert!(!recovery.is_complete());
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
/// # A mismatched pair is an error, not a weakened answer
///
/// `parse` and `source` are two arguments and nothing pairs them, so a caller can hand over a parse
/// of different bytes. When they do, this returns `Err` with
/// [`LosslessInvalid::recovery`] `None` and
/// [`Invalid::budget_tripped`](super::Invalid::budget_tripped) **false** — the one verdict that
/// means "these inputs do not describe a document", distinct from the budget refusal below, which
/// sets that flag.
///
/// The alternative was an incomplete [`Recovery`], and it is worse here for two reasons. It needs a
/// `skipped` count, and counting what was not projected is exactly the walk this declined to make —
/// the same wall al8n/smear#198's eighth round hit, where inventing the number was worse than not
/// having one. And it would be `Ok`: a caller who does not read the recovery sees a clean verdict,
/// which is the failure this check exists to remove, one indirection later. This crate has already
/// ruled on "nothing was examined" once, for the budget refusal, and ruled `Err`.
///
/// # What a refusal here looks like
///
/// The same thing every other refusal looks like: `Err`, with
/// [`Invalid::budget_tripped`](super::Invalid::budget_tripped) set, and
/// [`Rule::ValidationWorkBudget`](super::Rule::ValidationWorkBudget) in the sink when the rule set
/// contains it. [`LosslessInvalid::recovery`] is **`None`**: nothing was projected, so there is no
/// [`Recovery`] to report and no synthetic count for a caller to compare against. Counting the
/// elements exactly is the walk the refusal declined to make, and doing it here from the green
/// root's children would put a second copy of `recovered_top_level`'s idea of where the top level
/// is into this crate, where it could drift.
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
  // **The lengths first.** They are two integer loads and they are the whole answer whenever they
  // differ: a parse and a source of different sizes are not the same document, and no amount of
  // budget makes them one.
  //
  // The prepayment below used to sit above this. A caller holding a stale pair and a finite
  // `validation_work` too small for `max(parse, source)` was told `Refusal::Budget` — and possibly
  // handed a `ValidationWorkBudget` diagnostic — for a pair that `verify_source` would have refused
  // on its first comparison without walking a single token. That is a wrong causal verdict, and the
  // kind that costs a caller something: the remedy it names is raising a limit or retrying, and
  // neither can help. al8n/smear#198's twentieth round, and the same class as #196's arena refusal
  // wearing the budget's `None` — except that here the two abandonments already had different
  // names and the *ordering* handed out the wrong one.
  //
  // Round 14's own diagnosis named this gate: the fail-fast doors were safe because `verify_source`
  // "compares lengths first, so an extended source is refused before a byte is walked". This door
  // now asks the same question before it prices anything.
  let size = usize::from(parse.green().text_len());
  if size != source.len() {
    return Err(LosslessInvalid {
      invalid: Invalid::unexamined(),
      recovery: None,
    });
  }
  let Some(left) = Ledger::open(budget).take(units(size)) else {
    let (emitted, stopped) = refuse_projection(source, budget, rules, sink);
    return Err(LosslessInvalid {
      invalid: Invalid::refused(emitted, stopped),
      // Nothing looked at anything, so there is nothing to report.
      recovery: None,
    });
  };
  // The pair is verified as a **whole root**, byte for byte, before anything is projected — by the
  // projector, which answers [`SourceMismatch`] instead of an AST.
  //
  // The projector verifies each *definition* it projects against the source at that definition's
  // own range, and al8n/smear#198's third round took that to mean a mismatched pair answers itself
  // — a bad definition is refused and counted into [`Recovery`], so the caller is told. That is
  // true of every mismatch the projector **sees**. It is not true of a `source` that begins with
  // the parse's text and then adds to it: every projected definition matches, nothing is skipped,
  // [`Recovery::is_complete`] reports `true`, and the operations the caller appended are never
  // looked at. A clean verdict on a document nobody validated, which is worse than either answer
  // this could give instead.
  //
  // The check belongs to the projector rather than to this door because this door is not the only
  // caller: one holding no validator and no door projects the same pair directly, and a guard this
  // door performs is a guard that caller never gets — al8n/smear#198's fourteenth round, where the
  // door was right and the API it called was not.
  //
  // It is still *this* door's charge. The comparison walks the parse's **green** root against the
  // source, so it is `O(tokens)` and allocates nothing — bounded by, and already paid for by, the
  // prepayment above, which prices `max(source.len(), parse.green().text_len())`. Moving the work
  // behind a call boundary did not move the charge: the prepayment still sits in front of it.
  //
  // Both halves of that sentence were false for one round. The check was written as
  // `parse.syntax().text() == source`, which materialises rowan's red cursor and allocates one
  // node's worth of cursor data per element as the comparison walks past it — on every call of this
  // door. `matches_source` routes through `verify_source` now, which is the function the fourteenth
  // round's own diagnosis had already named as the reason the fail-fast doors were safe.
  let (document, recovery) =
    project_executable_document_recovered(parse, source).map_err(|_| LosslessInvalid {
      invalid: Invalid::unexamined(),
      recovery: None,
    })?;
  match validate_charged(schema, &document, scratch, budget, rules, sink, left) {
    Ok(()) => Ok(recovery),
    Err(invalid) => Err(LosslessInvalid {
      invalid,
      recovery: Some(recovery),
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
#[non_exhaustive]
pub enum LosslessSchemaErrors {
  /// The `parse` and the `source` do not describe one document, so nothing was projected and
  /// [`Schema::build`](super::Schema::build) was never asked.
  ///
  /// A separate state rather than an empty error list beside a flag: the §3 refusals a caller would
  /// otherwise read here — "no `Query` root", and so on — would be true of the empty document this
  /// door declined to build from, and false of anything the caller wrote.
  SourceMismatch,
  /// The projected document is not a schema, exactly as
  /// [`Schema::build`](super::Schema::build) reports it.
  Refused {
    /// Why the build refused.
    errors: SchemaErrors,
    /// How much of the parse had an AST image.
    recovery: Recovery,
  },
}

impl LosslessSchemaErrors {
  /// Returns why the build refused, exactly as [`Schema::build`](super::Schema::build) reports it.
  ///
  /// `None` when the build was never asked — see [`LosslessSchemaErrors::SourceMismatch`].
  #[inline]
  pub const fn errors(&self) -> Option<&SchemaErrors> {
    match self {
      Self::Refused { errors, .. } => Some(errors),
      _ => None,
    }
  }

  /// Returns why this door refused, when the reason was not the schema itself.
  ///
  /// [`Invalid::refusal`](super::Invalid::refusal)'s twin for the SDL side, and the same reason for
  /// existing: a caller should read one value to learn *which* refusal this is, not infer it from
  /// which accessors happen to answer.
  #[inline]
  pub const fn refusal(&self) -> Option<Refusal> {
    match self {
      Self::SourceMismatch => Some(Refusal::SourceMismatch),
      _ => None,
    }
  }

  /// Returns how much of the parse had an AST image.
  ///
  /// [`Recovery::is_complete`] false means at least one of these refusals may be an artifact of a
  /// definition that was dropped rather than of one the author wrote — see
  /// [`validate_schema_lossless`].
  #[inline]
  pub const fn recovery(&self) -> Option<Recovery> {
    match self {
      Self::Refused { recovery, .. } => Some(*recovery),
      _ => None,
    }
  }

  /// Consumes this verdict and returns the refusals alone.
  #[inline]
  pub fn into_errors(self) -> Option<SchemaErrors> {
    match self {
      Self::Refused { errors, .. } => Some(errors),
      _ => None,
    }
  }
}

/// The refusals alone, when there were any.
///
/// `From` and not `TryFrom` would have to invent a [`SchemaErrors`] for
/// [`LosslessSchemaErrors::SourceMismatch`], where the build was never asked and there is nothing
/// to invent one from.
impl TryFrom<LosslessSchemaErrors> for SchemaErrors {
  type Error = LosslessSchemaErrors;

  #[inline]
  fn try_from(value: LosslessSchemaErrors) -> Result<Self, Self::Error> {
    match value {
      LosslessSchemaErrors::Refused { errors, .. } => Ok(errors),
      other => Err(other),
    }
  }
}

impl core::fmt::Display for LosslessSchemaErrors {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::SourceMismatch => {
        f.write_str("the parse and the source are not the same document, so nothing was built")
      }
      Self::Refused { errors, recovery } => {
        core::fmt::Display::fmt(errors, f)?;
        if !recovery.is_complete() {
          write!(f, "\n  ({} skipped by recovery)", recovery.skipped())?;
        }
        Ok(())
      }
    }
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
/// let errors = refused.errors().expect("the pair matches, so the build was asked");
/// assert_eq!(errors.kinds(), [SchemaErrorKind::UndefinedType]);
/// let recovery = refused.recovery().expect("the pair matches, so the build was asked");
/// assert_eq!(recovery.projected(), 2);
/// assert!(!recovery.is_complete());
///
/// let error = &errors.errors()[0];
/// // The same bytes the syntactic door would have blamed.
/// let span = error.span();
/// assert_eq!(&source[span.start()..span.end()], "Nope");
/// # }
/// ```
pub fn validate_schema_lossless(
  parse: &Parse,
  source: &str,
) -> Result<(Schema, Recovery), LosslessSchemaErrors> {
  // The same whole-root verification the executable door gets, from the same shared projector.
  //
  // This door was cleared in al8n/smear#198's third-round sweep — "out of scope by API shape: it
  // takes no `Budget`". That was right about the *ledger* question and had nothing to say about
  // this one, and nothing re-read the clearance when the question changed. Without it, a `source`
  // that is the parse's text plus trailing SDL projected every stale definition, reported a
  // complete recovery, and let `Schema::build` answer `Ok` for the prefix while the appended
  // definitions were silently absent.
  //
  // Neither door spells the check itself any more: the twelfth round asked for it "in the shared
  // recovering-projection API" precisely so a door cannot be written without it, and the
  // fourteenth showed a caller that has no door at all.
  let (document, recovery) = project_type_system_document_recovered(parse, source)
    .map_err(|_| LosslessSchemaErrors::SourceMismatch)?;
  match Schema::build(&document) {
    Ok(schema) => Ok((schema, recovery)),
    Err(errors) => Err(LosslessSchemaErrors::Refused { errors, recovery }),
  }
}
