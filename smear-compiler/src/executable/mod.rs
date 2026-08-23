//! Executable-document validation: the entry point, and the passes that do not ride the selection
//! walk.
//!
//! # The shape of a run
//!
//! 1. **Prep** records the operations and fragments, sorts a fragment index, and collects every
//!    fragment spread as an edge of the fragment graph. Draft 5.5.1.1 and 5.5.2.1 fall out of it.
//! 2. **Operation rules** (5.2.1.1, 5.2.2.1, 5.2.3.1) read the operation list.
//! 3. **Fragment declaration rules** (5.5.1.2, 5.5.1.3) read the fragment table.
//! 4. **The fragment graph** answers 5.5.2.2 and 5.5.1.4 — with integers, over an explicit stack.
//!    Cycles are established here, before anything expands a fragment, so no later rule can meet
//!    an unestablished graph. Unlike apollo-compiler, unused fragments are included: a rule that
//!    trusts the graph must be able to trust all of it.
//! 5. **Subscriptions** get 5.2.4.1's own root-field collection.
//! 6. **One selection walk per operation**, following spreads with a visited bitset, carries every
//!    remaining rule. Rules that are properties of a *definition* rather than of an operation fire
//!    the first time that definition is reached and are suppressed afterwards, so a fragment three
//!    operations share reports its bad field once. Rules that are properties of an *operation* —
//!    the variable rules — run every time, which is what the specification asks for: the same
//!    fragment may be valid under one operation's variables and invalid under another's.
//! 7. **A final pass over fragments no operation reached**, so their structure is validated too.
//! 8. **Draft 5.3.2's merge engine**, last of all, over every selection set the document defines.
//!    It is the only rule with a working set and the only one an attacker can make expensive, so
//!    every cheap structural refusal — undefined spreads, fragment cycles — is already established
//!    before it expands anything, and what it may spend is the caller's [`Budget`].
//!
//! # Every repair here was swept for siblings, and three of the sweeps found one
//!
//! al8n/smear#198 landed a work ledger over these passes and then spent two review rounds on
//! repairs that were true of the site they were written for and false of its twin. The pattern is
//! worth naming because it is not carelessness: each repair was correct, and each was written by
//! looking at the site the review named.
//!
//! - **A charge must sit in front of the work it prices.** The table on
//!   [`Validator::spend`] is that question asked of every pass — and it replaced an earlier table
//!   that asked "does this pass charge?", which every one of them did.
//! - **A coordinate resolver costs the depth it descends.** `values::walk_value` was repaired;
//!   `selections::resolve` — the same shape, three call sites, named in the same review sentence —
//!   was not, and stayed quadratic for a round.
//! - **A ledger's "off" must be a state.** [`Ledger::Off`] is closed under [`Ledger::take`]
//!   because `u32::MAX` as a large number is a budget the first charge shrinks. That defect has
//!   now been written four times in this repository under four different names.
//! - **A state that must not be read must not exist.** [`Ledger::Off`] carries no number,
//!   `LosslessInvalid::recovery` carries no [`Recovery`] when nothing was projected, and one
//!   `tripped` carries the whole of "a bound abandoned this run". Each replaced a value plus a
//!   caveat — a maximum documented as an absence, a count documented as a floor, a flag documented
//!   as needing a second flag read with it — and a caveat is only ever as good as the next reader.
//! - **An activation condition with more than one rule in it gets a name.** [`merges`],
//!   [`checks_values`], [`collects_usages`] and [`reports_type_conditions`] exist because each has
//!   two readers, and a condition with two readers written out twice is two conditions. One of
//!   them shipped naming two of draft 5.3.2's three activating rules, so a rule set that started
//!   the engine without starting the pass that fills what it reads produced false refusals.
//! - **A rule that is off must not read caller-sized data.** Draft 5.6.1's leaf arms hashed an
//!   enum spelling and parsed a digit string before asking whether 5.6.1 was on. Sweeping the
//!   class found four more: the value walk itself, the fragment-declaration pass, 5.6.3's
//!   prepayment, and the variable index — and it found that the *reachability* pass could not join
//!   them, because draft 5.3.2's engine reads the bitset it fills. That last one is the reason the
//!   sweep is worth running even where it finds nothing to repair: it is what stops a repair from
//!   becoming the next defect.
//!
//! # Nothing recurses on document shape
//!
//! Selection sets and value literals are both walked with explicit stacks living in the caller's
//! [`Scratch`]. The frames are coordinates rather than references — a definition index plus the
//! chain of child indices that reaches a level — which is what lets a stack that knows nothing
//! about the document's source slice type replace recursion over an attacker-chosen shape. Draft
//! 5.3.2's two merge recursions and its value comparison are written the same way, for the same
//! reason and with more at stake.

mod merge;
mod nodes;
mod selections;
mod values;

use core::ops::ControlFlow;

use tokora::{SimpleSpan, span::AsSpan};

use smear_parser::graphql::ast::{
  DescribedVariableDefinition, ExecutableDefinition, ExecutableDocument, Name, OperationDefinition,
  OperationType, Selection, Type,
};

use super::{
  Budget, Diagnostic, Rule, RuleSet, Scratch, Sink,
  diagnostic::Context,
  schema::{
    DirectiveLocation, MAX_WRAPPERS, PackedType, Range32, RootOperation, Schema, Sym, TypeId,
    is_reserved,
  },
  scratch::{
    Edge, FragmentRow, Frame, GraphFrame, NONE, OperationRow, Work, clear_bit, get_bit, reset_bits,
    set_bit,
  },
};

use nodes::{child_selection_set, definition, fragment, name_bytes, operation, root_selection_set};
use values::ValueLocation;

/// The verdict of a failed validation.
///
/// Returned when the document was refused: because at least one diagnostic was emitted, or because
/// a resource bound abandoned a pass partway through. What the diagnostics *were* is the sink's
/// business — this is only the count, whether a bound refused, and whether the sink asked to stop
/// before the document had been fully examined.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Invalid {
  emitted: u32,
  stopped: bool,
  budget: bool,
}

impl Invalid {
  /// Returns how many diagnostics were produced.
  ///
  /// This counts what the validator emitted, not what the sink kept: [`Ignore`](super::Ignore)
  /// discards everything and the count is still right.
  ///
  /// **Zero is a possible count on a verdict that is still `Err`**, and it means one thing: a
  /// resource bound refused the document with that bound's own rule outside the
  /// [`RuleSet`](super::RuleSet), so there was nothing to emit. [`Invalid::budget_tripped`] is
  /// true whenever that happens, and a caller that reported "no findings" without reading it would
  /// be describing a check the validator abandoned.
  #[inline]
  pub const fn emitted(&self) -> u32 {
    self.emitted
  }

  /// Returns whether the **sink** stopped validation before the document was fully examined.
  ///
  /// True for [`First`](super::First) on any invalid document *that produced a diagnostic*. The
  /// qualification is not pedantry: there is exactly one verdict that is `Err` with nothing
  /// emitted — a resource bound refusing with its own rule outside the
  /// [`RuleSet`](super::RuleSet) — and no diagnostic ever reaches the sink there, so the sink
  /// never asks for anything to stop and this reads `false` on a document that was very much not
  /// fully examined. Read as "was the whole document looked at", it says the opposite of the truth
  /// on the one case where it matters most.
  ///
  /// So the two flags answer two questions and neither answers the other's: this one says **who**
  /// stopped the walk, and [`Invalid::budget_tripped`] says whether a bound refused. A caller who
  /// wants "is anything about this document still unknown" reads both.
  ///
  /// When it is true, the absence of a diagnostic says nothing: the rest of the document was never
  /// looked at. al8n/smear#196.
  #[inline]
  pub const fn stopped(&self) -> bool {
    self.stopped
  }

  /// Returns whether a [`Budget`] refused the document.
  ///
  /// True when draft 5.3.2's merge engine reached
  /// [`Budget::merge_depth`](super::Budget::merge_depth) or
  /// [`Budget::merge_work`](super::Budget::merge_work). With the bound's own rule in the
  /// [`RuleSet`](super::RuleSet) the diagnostic is
  /// [`Rule::MergeDepthBudget`](super::Rule::MergeDepthBudget) or
  /// [`Rule::MergeWorkBudget`](super::Rule::MergeWorkBudget) and says which; **without it there is
  /// no diagnostic and this flag is the whole of the report**, on a verdict whose
  /// [`Invalid::emitted`] is zero. Filtering a bound's rule out switches off its diagnostic, not
  /// the refusal: an engine that stopped and then answered `Ok` would be reporting a clean result
  /// for a check it never finished. al8n/smear#196.
  ///
  /// **And when any other pass reached this crate's absolute validation ceiling**, whose diagnostic
  /// is [`Rule::ValidationWorkBudget`](super::Rule::ValidationWorkBudget). That bound answers the
  /// paragraph above identically — the refusal does not depend on its rule being enabled, only
  /// being *told* which bound refused does — which is what makes this one flag rather than three.
  /// al8n/smear#198.
  ///
  /// When it is true the document is **invalid**, not "unvalidated": the engine refuses rather
  /// than passing what it could not finish examining. What it does *not* mean is that the rest of
  /// the document is clean — the merge engine stopped, so anything it had not reached is unknown,
  /// exactly as [`Invalid::stopped`] means for the sink.
  #[inline]
  pub const fn budget_tripped(&self) -> bool {
    self.budget
  }
}

/// The lossless door's constructor, and nothing else's.
///
/// Gated to its one caller rather than left ungated: without `rowan` there is no door that refuses
/// before a [`Validator`] exists, so an ungated constructor is dead code in exactly the feature
/// selection `cargo clippy --no-default-features -D warnings` builds.
#[cfg(feature = "rowan")]
impl Invalid {
  /// The verdict of a run a resource bound abandoned, with `emitted` diagnostics behind it and
  /// `stopped` saying whether the sink asked to stop on one of them.
  ///
  /// `pub(crate)` and used by exactly one caller: the lossless door refuses **before** it projects,
  /// so there is no [`Validator`] in existence to carry the flag out. The shape is the same one
  /// `validate_charged` produces — `Err`, `budget_tripped`, and an `emitted` that may be zero —
  /// because a second spelling of "gave up" is how a caller ends up reading one of them as
  /// "finished".
  pub(crate) const fn refused(emitted: u32, stopped: bool) -> Self {
    Self {
      emitted,
      stopped,
      budget: true,
    }
  }
}

impl core::fmt::Display for Invalid {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    if self.emitted == 0 {
      // A bound refused with its own rule filtered out. "0 validation errors" would read as the
      // opposite of what happened.
      return f.write_str("resource budget exceeded before the document was fully examined");
    }
    let plural = if self.emitted == 1 { "" } else { "s" };
    write!(f, "{} validation error{plural}", self.emitted)?;
    if self.stopped {
      f.write_str(" (validation stopped early)")?;
    }
    if self.budget {
      f.write_str(" (resource budget exceeded)")?;
    }
    Ok(())
  }
}

impl core::error::Error for Invalid {}

/// Validates an executable document against a schema, checking every draft §5 rule.
///
/// `scratch` is the caller's reusable working set and `sink` the caller's diagnostic storage; the
/// validator owns neither, which is what lets the steady state allocate nothing. `budget` bounds
/// draft 5.3.2's merge engine and, separately, every other pass.
///
/// Returns `Err(Invalid)` when at least one rule fired **or** a [`Budget`] bound refused the
/// document. Those are not the same thing and [`Invalid::budget_tripped`] is what separates them:
/// a refusal with the bound's own rule filtered out has an [`Invalid::emitted`] of zero.
///
/// # Example
///
/// ```
/// use smear_compiler::{Budget, First, Schema, Scratch, validate_executable};
/// use smear_parser::{
///   graphql::{
///     GraphQL,
///     ast::{ExecutableDocument, TypeSystemDocument},
///     error::GraphqlErrors,
///     syntactic::{GraphqlLexer, executable_document, type_system_document},
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
/// let parse = |src| {
///   Parser::with_parser::<GraphqlLexer<'_, str>, ExecutableDocument<&str>, GraphqlErrors<&str>, _, GraphQL>(
///     executable_document,
///   )
///   .parse_str(src)
///   .expect("the query parses")
/// };
///
/// let mut scratch = Scratch::new();
/// let budget = Budget::default();
///
/// let good = parse("{ hero { name } }");
/// let mut sink = First::new();
/// assert!(validate_executable(&schema, &good, &mut scratch, &budget, &mut sink).is_ok());
///
/// let bad = parse("{ hero { nickname } }");
/// let mut sink = First::new();
/// let invalid = validate_executable(&schema, &bad, &mut scratch, &budget, &mut sink)
///   .expect_err("`nickname` is not on `Character`");
/// assert_eq!(invalid.emitted(), 1);
/// assert_eq!(
///   sink.get().expect("a diagnostic").display(&schema).to_string(),
///   "5.3.1 Field Selections: `nickname` on type `Character` (9..17)"
/// );
/// ```
pub fn validate_executable<S, K>(
  schema: &Schema,
  document: &ExecutableDocument<S>,
  scratch: &mut Scratch,
  budget: &Budget,
  sink: &mut K,
) -> Result<(), Invalid>
where
  S: AsRef<[u8]> + Clone,
  K: Sink<S>,
{
  validate_executable_with(schema, document, scratch, budget, RuleSet::ALL, sink)
}

/// Validates an executable document against a subset of the rules.
///
/// A draft §5 rule outside `rules` is not evaluated, not merely filtered: a consumer that wants
/// only the fragment rules does not pay for value coercion. With [`RuleSet::ALL`] this is exactly
/// [`validate_executable`].
///
/// **`rules` does not reach the resource bounds.** Narrowing removes a bound's *diagnostic*, never
/// the bound: a caller who asks for
/// [`Rule::FieldSelectionMerging`](super::Rule::FieldSelectionMerging) alone is still handed `Err`
/// with [`Invalid::emitted`] zero and [`Invalid::budget_tripped`] set when `budget` refuses.
///
/// What narrowing *can* do is leave a bound with nothing to bound, and this said the opposite —
/// that `budget` is enforced whatever the set contains.
/// [`Budget::merge_work`](super::Budget::merge_work) and
/// [`Budget::merge_depth`](super::Budget::merge_depth) are spent by draft 5.3.2's engine, and that
/// engine is started by draft 5.3.2's own rule: with
/// [`Rule::FieldSelectionMerging`](super::Rule::FieldSelectionMerging),
/// [`Rule::MergeDepthBudget`](super::Rule::MergeDepthBudget) and
/// [`Rule::MergeWorkBudget`](super::Rule::MergeWorkBudget) all absent it does not run, so nothing
/// is expanded, interned or compared and a `merge_work` of zero has nothing to refuse. **That is
/// vacuity and not an exemption** — nothing expensive was let through, because nothing expensive
/// happened. A bound whose passes *do* run is enforced whether or not its rule is in `rules`.
///
/// So `budget` is not an admission policy. Deciding whether to accept a document by what it would
/// cost means leaving the rule that does the costing switched on: `rules` chooses what is
/// **checked** and [`Budget`] chooses what is **afforded**, and neither answers the other's
/// question. al8n/smear#196.
pub fn validate_executable_with<S, K>(
  schema: &Schema,
  document: &ExecutableDocument<S>,
  scratch: &mut Scratch,
  budget: &Budget,
  rules: RuleSet,
  sink: &mut K,
) -> Result<(), Invalid>
where
  S: AsRef<[u8]> + Clone,
  K: Sink<S>,
{
  validate_charged(
    schema,
    document,
    scratch,
    budget,
    rules,
    sink,
    Ledger::open(budget),
  )
}

/// [`validate_executable_with`] continuing an already-opened [`Ledger`] rather than opening one.
///
/// The lossless door needs this. Its projection builds the whole AST **before** any rule exists to
/// charge, so the ledger has to be opened before the [`Validator`] is and carried into it; a
/// second, private ledger would be a second number for a caller to reason about and a second place
/// for the two to disagree. See `lossless::validate_executable_lossless_with`.
pub(crate) fn validate_charged<S, K>(
  schema: &Schema,
  document: &ExecutableDocument<S>,
  scratch: &mut Scratch,
  budget: &Budget,
  rules: RuleSet,
  sink: &mut K,
  left: Ledger,
) -> Result<(), Invalid>
where
  S: AsRef<[u8]> + Clone,
  K: Sink<S>,
{
  scratch.reset();
  let mut validator = Validator {
    schema,
    document,
    scratch,
    sink,
    rules,
    budget: *budget,
    scalars: Scalars::resolve(schema),
    checks_values: checks_values(rules),
    collects_usages: collects_usages(rules),
    variables: &[],
    in_operation: false,
    variable_index: Range32::new(0, 0),
    emitted: 0,
    stopped: false,
    work: Work::new(budget.merge_work()),
    left,
    generation: 0,
    tripped: false,
    blame: SimpleSpan::const_new(0, 0),
  };
  let _ = validator.run();
  // ONE field, read once. Both branches that met here arrived at the same repair from opposite
  // sides: al8n/smear#196 collapsed the merge engine's "stopped" and "reported" flags onto
  // `tripped`, and al8n/smear#198 added `refused` for its own ledger — so the rebased tail briefly
  // read two fields for one fact, which is the shape that produced the original defect. With draft
  // 5.3.2 enabled and its two budget *rules* filtered out, a tail that read only one of them
  // returned `Ok` on a document the engine had abandoned. There is one fact now and no way to
  // consult half of it.
  let tripped = validator.tripped;
  let (emitted, stopped) = (validator.emitted, validator.stopped);
  if emitted == 0 && !tripped {
    Ok(())
  } else {
    Err(Invalid {
      emitted,
      stopped,
      budget: tripped,
    })
  }
}

/// What is left of [`Budget::validation_work`](super::Budget::validation_work), or the explicit
/// absence of a bound.
///
/// # Disabled is a state, not a large number
///
/// [`u32::MAX`] is the *spelling* a caller uses to turn the bound off, and it stops being a number
/// here rather than staying one and being trusted to stay large. A counter that encodes "off" as
/// its maximum is a counter the first charge converts into a very large *finite* budget — and this
/// program has now written that same defect four times: `Work::take`'s saturation, `Visits::take`,
/// tokora's regime generation, and this. Repairing it as arithmetic repairs one of them.
///
/// [`Ledger::Off`] is closed under [`Ledger::take`]: no sequence of charges, and no prepayment the
/// lossless door subtracts before the validator exists, can turn it into [`Ledger::Left`]. That is
/// the property, and it is a property of the type rather than of every call site that spends.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Ledger {
  /// No bound. Every charge succeeds and changes nothing.
  Off,
  /// This many units remain.
  Left(u32),
}

impl Ledger {
  /// The ledger a [`Budget`] opens.
  #[inline]
  pub(crate) const fn open(budget: &Budget) -> Self {
    match budget.validation_work() {
      u32::MAX => Self::Off,
      left => Self::Left(left),
    }
  }

  /// Spends `units`, or answers `None` when there was not room for them.
  #[inline]
  pub(crate) const fn take(self, units: u32) -> Option<Self> {
    match self {
      Self::Off => Some(Self::Off),
      Self::Left(left) => match left.checked_sub(units) {
        Some(left) => Some(Self::Left(left)),
        None => None,
      },
    }
  }
}

/// The unit [`Budget::validation_work`](super::Budget::validation_work) is spent in.
///
/// One per node examined, plus one per eight bytes of any **document-chosen** name a pass reads.
///
/// # Charged in front of the work, which is the property and not the convention
///
/// A charge taken after the step it prices bounds nothing: the work is already spent by the time
/// the counter can refuse it, so the ceiling is exceeded by whatever that step cost. The question
/// a reader must ask of every charge here is therefore not "does this pass charge?" but "is the
/// charge in front of the work it prices?" — and four sites answered the first question and not
/// the second before al8n/smear#198's first review. [`Validator::spend`] carries the re-derived
/// table.
///
/// # Bytes and not entries, and the difference is measurable rather than theoretical
///
/// A GraphQL name has no length ceiling, every name comparison here is `[u8] == [u8]`, and *where*
/// two names differ decides what that costs. At 2,000 variable definitions and 2,000 usages, names
/// padded to 200 bytes **after** the distinguishing digits measured 8.8 ms — no worse than
/// one-byte names, because the comparison exits at the first differing byte — and the same padding
/// written **before** them measured 17.8 ms. A ledger over entries cannot see that factor at all.
///
/// # Why eight
///
/// Roughly the word a comparison, a hash or a copy advances per step, and it is the unit
/// `graphql-proto`'s collection ledger already charges in — two ledgers counting different things
/// in the same crate family would be two units to re-derive.
#[inline]
pub(crate) const fn units(len: usize) -> u32 {
  // `as` after the shift, so a name longer than `u32::MAX * 8` cannot wrap the charge down to
  // nothing on a 64-bit target. Nothing can allocate one; a bound that rests on that is not a
  // bound.
  let chunks = len >> 3;
  if chunks >= u32::MAX as usize {
    u32::MAX
  } else {
    chunks as u32 + 1
  }
}

/// Whether draft 5.3.2's engine runs.
///
/// **One definition, read by the engine and by everything that feeds it.** The engine is activated
/// by three rules, not one: [`Rule::FieldSelectionMerging`] is the rule it implements, and
/// [`Rule::MergeDepthBudget`] and [`Rule::MergeWorkBudget`] each start it as well, because a
/// caller who wants only the resource refusals still needs the pass that can produce them.
///
/// It exists as a named predicate because it has a **producer** as well as a consumer.
/// `check_fragments_used` fills `Scratch::reachable`, and the engine reads it to skip the fragments
/// an operation's own merge already covered; a hand-written condition on the producer that listed
/// two of the three rules left `RuleSet::only(MergeWorkBudget)` reading a cleared bitset, so every
/// fragment looked unreached and a chain already expanded from an operation was merged again from
/// every suffix — linear work inflated toward quadratic, and false budget refusals out of it.
///
/// A fourth merge rule would desynchronise a copied condition and cannot desynchronise this one.
const fn merges(rules: RuleSet) -> bool {
  rules.contains(Rule::FieldSelectionMerging)
    || rules.contains(Rule::MergeDepthBudget)
    || rules.contains(Rule::MergeWorkBudget)
}

/// Whether either rule a type condition is reported against is enabled.
///
/// The third named predicate, and the third for the same reason: `check_fragment_declarations`
/// exists only to run [`Validator::check_type_condition`] for its reports, and that function's
/// reports are gated one rule at a time inside it. A hand-written pair on the caller and two
/// separate guards on the callee is a condition in two places; a third rule added to the callee
/// would be silently skipped by the caller. Any rule `check_type_condition` can report belongs
/// here.
const fn reports_type_conditions(rules: RuleSet) -> bool {
  rules.contains(Rule::FragmentSpreadTypeExistence)
    || rules.contains(Rule::FragmentsOnCompositeTypes)
}

/// Whether draft 5.6's literal rules are enabled.
///
/// **A property of a definition**, which is why it is not enough on its own to start a value walk:
/// these rules fire under `Frame::CHECK` and a definition carries that flag exactly once, however
/// many operations reach it. See [`collects_usages`] for the half that is a property of an
/// *operation*, and `values::walk_value` for what asking one question where there were two cost.
const fn checks_values(rules: RuleSet) -> bool {
  rules.contains(Rule::ValuesOfCorrectType)
    || rules.contains(Rule::InputObjectFieldNames)
    || rules.contains(Rule::InputObjectFieldUniqueness)
    || rules.contains(Rule::InputObjectRequiredFields)
}

/// Whether the variable rules that read a usage's position are enabled.
///
/// **A property of an operation.** The same fragment is valid under one operation's variables and
/// invalid under another's, so these are collected on every visit rather than on the first — which
/// is the whole reason the selection walk repeats per operation at all.
const fn collects_usages(rules: RuleSet) -> bool {
  rules.contains(Rule::AllVariableUsesDefined)
    || rules.contains(Rule::AllVariablesUsed)
    || rules.contains(Rule::AllVariableUsagesAreAllowed)
}

/// The five built-in scalar names, resolved against this schema once per run.
///
/// Coercion is decided by *name*, not by the built-in flag: a document may spell `scalar String`
/// out — a printed schema does — and it is still the specification's `String`. Everything else is
/// a custom scalar, and a custom scalar accepts any literal because only the service knows how to
/// read it.
#[derive(Debug, Clone, Copy)]
struct Scalars {
  int: Option<Sym>,
  float: Option<Sym>,
  string: Option<Sym>,
  boolean: Option<Sym>,
  id: Option<Sym>,
}

impl Scalars {
  fn resolve(schema: &Schema) -> Self {
    Self {
      int: schema.sym(b"Int"),
      float: schema.sym(b"Float"),
      string: schema.sym(b"String"),
      boolean: schema.sym(b"Boolean"),
      id: schema.sym(b"ID"),
    }
  }
}

/// One validation run.
struct Validator<'a, 'd, S, K> {
  schema: &'a Schema,
  document: &'d ExecutableDocument<S>,
  scratch: &'a mut Scratch,
  sink: &'a mut K,
  rules: RuleSet,
  budget: Budget,
  scalars: Scalars,
  /// [`checks_values`] for this run's [`RuleSet`], resolved once.
  checks_values: bool,
  /// [`collects_usages`] for this run's [`RuleSet`], resolved once.
  collects_usages: bool,
  /// The variable definitions of the operation being walked; empty outside one.
  variables: &'d [DescribedVariableDefinition<S>],
  /// Whether a variable scope is in effect. Distinguishes "an operation with no variables" from
  /// "a fragment nothing reached", which is the difference between draft 5.8.3 firing and not.
  in_operation: bool,
  /// The current operation's variable-name index, as a range into [`Scratch::keys`].
  ///
  /// The index is the ordinals of [`Validator::variables`] sorted by name with ties broken on the
  /// ordinal, built once per operation by [`Validator::check_variable_definitions`] and read by
  /// every usage. It outlives the pass that builds it, which is why it is recorded: every other
  /// user of `keys` pushes at the buffer's current length and truncates back, so a prefix that
  /// stays put is safe.
  ///
  /// A **range** and not a base, because the index is not always built — the rules that read it
  /// can all be off — and a base plus an assumed length would then name whatever the next pass
  /// pushed. Empty is the representation of "not built"; there is no second field saying so.
  variable_index: Range32,
  emitted: u32,
  stopped: bool,
  /// How much of the [`Budget`]'s work knob draft 5.3.2's engine has spent, and the ceiling it is
  /// spending against.
  work: Work,
  /// What is left of [`Budget::validation_work`](super::Budget::validation_work), or
  /// [`Ledger::Off`]. Charged by every pass that is not draft 5.3.2's engine, and pre-spent by the
  /// lossless door's projection before this struct exists.
  left: Ledger,
  /// Distinguishes one fragment expansion from the next without clearing a bitset per expansion.
  generation: u32,
  /// Whether **any** resource bound abandoned this run.
  ///
  /// Draft 5.3.2's engine writes it through `merge::trip`, and this crate's validation ledger
  /// writes it through [`Validator::refuse`]. One field because it is one fact, and because both
  /// alternatives were tried and both failed the same way: a field for the walk plus a field for
  /// whether anything was emitted (al8n/smear#196), and that pair plus a third for the ledger
  /// (al8n/smear#198). Set even when the bound's own rule is switched off — the bound holds either
  /// way, and only the diagnostic is optional.
  tripped: bool,
  /// The definition to blame for a budget diagnostic — the one being merged when the bound hit.
  blame: SimpleSpan,
}

impl<'d, S, K> Validator<'_, 'd, S, K>
where
  S: AsRef<[u8]> + Clone,
  K: Sink<S>,
{
  // -- reporting ------------------------------------------------------------------------------

  /// Returns whether a rule is enabled. Checked before building a diagnostic, so a disabled rule
  /// costs nothing.
  #[inline]
  fn on(&self, rule: Rule) -> bool {
    self.rules.contains(rule)
  }

  fn emit(&mut self, diagnostic: Diagnostic<S>) -> ControlFlow<()> {
    self.emitted = self.emitted.saturating_add(1);
    match self.sink.diagnostic(diagnostic) {
      ControlFlow::Continue(()) => ControlFlow::Continue(()),
      ControlFlow::Break(()) => {
        self.stopped = true;
        ControlFlow::Break(())
      }
    }
  }

  // -- the validation-wide ledger ---------------------------------------------------------------

  /// Charges `units` against [`Budget::validation_work`](super::Budget::validation_work)
  /// **before** the step they price.
  ///
  /// `blame` is the span the refusal points at — the node whose examination could not be afforded,
  /// which is the narrowest true answer to "where did this document stop being validated".
  ///
  /// # Where every charge sits, relative to the work it prices
  ///
  /// The first version of this table asked "does this pass charge?". Every pass did, and four of
  /// them charged *behind* work a caller sizes — which bounds nothing, because the work is spent
  /// by the time the counter can refuse it. The question is where the charge sits, so that is what
  /// the table records. A row that says "postpaid" is a defect, not a note.
  ///
  /// | pass | charge site | what it prices |
  /// |---|---|---|
  /// | projection (lossless door only) | `lossless.rs`, before `project_*` | one prepayment of `units(source.len())`, an upper bound on the AST it builds |
  /// | `prep`, per definition | before the row is pushed | the row, and the fragment-name sort `index_fragments` runs next |
  /// | `collect_definition_edges` | after the cursor bump, before the arm | the selection; a spread's name before `find_fragment` searches it |
  /// | 5.2.2.1 operation names | in the collection loop, before the sort | every name the sort will compare |
  /// | 5.5.1.2/5.5.1.3 declarations | before `check_type_condition` | the `Schema::sym` hash of the condition |
  /// | 5.5.2.2 cycles | before the edge is read | one edge |
  /// | 5.5.1.4 used | `mark_reachable`, before the group loop | every member of a duplicated name's group |
  /// | 5.2.4.1 subscription roots | after the cursor bump, before `conditional_directive` | the selection, every directive on it, and every condition name it resolves |
  /// | selection walk | after the cursor bump, before the arm | the selection; then the field, spread or inline-condition name before each is resolved |
  /// | 5.7.x directives | `spend_names` at the head of `check_directives` | every directive name, ahead of 5.7.3's sort |
  /// | 5.4.x arguments | `spend_names` at the head of `check_arguments` | every argument name, ahead of 5.4.2's sort |
  /// | 5.4.3 presence half | before each declared argument's rescan | one rescan of the written list |
  /// | 5.6.3/5.6.4 input objects | `spend_names` at the head of `check_input_object` | every field name, ahead of 5.6.3's sort and 5.6.4's rescan |
  /// | value walk | top of the loop, before `resolve` | the descent `resolve` makes, which is the frame depth |
  /// | scalar and enum literals | before the kind dispatch | the literal's own spelling, which `fits_i32`, `is_finite` and `has_enum_value` read |
  /// | 5.8.1/5.8.2 definitions | in the collection loop, before the sort | every variable name, and the declared type's base name before `pack_type` |
  /// | 5.8.3/5.8.5 usages | before the index search, and again before the marking | the search, and the run of definitions sharing the name |
  /// | 5.8.4 used | — | reads bits; the definitions were charged when the index was built |
  /// | 5.2.1.1, 5.2.3.1 | — | `O(1)` per operation, and the operations were charged at `prep` |
  /// | 5.3.2 merge engine | its own ledger | [`Budget::merge_work`](super::Budget::merge_work), unchanged and not double-charged |
  ///
  /// Three rows deserve their reason stated rather than assumed. The **sorts** — 5.2.2.1, 5.4.2,
  /// 5.6.3, 5.7.3, 5.8.1 and the fragment index — do `N log N` comparisons against `N` units of
  /// prepayment; `log N` is at most thirty-two whatever the document does, so that is a bounded
  /// constant multiple of the charge and not a second factor the client can grow. The **binary
  /// searches** — `find_fragment`, the variable index — are the same argument one dimension
  /// smaller. And **`Schema` lookups keyed by an already-charged name** are free by construction:
  /// the schema is the server's, so its group sizes are not an input.
  #[inline]
  pub(super) fn spend(&mut self, units: u32, blame: SimpleSpan) -> ControlFlow<()> {
    match self.left.take(units) {
      Some(left) => {
        self.left = left;
        ControlFlow::Continue(())
      }
      None => self.refuse(blame),
    }
  }

  /// Refuses the document for reaching
  /// [`Budget::validation_work`](super::Budget::validation_work), and abandons every remaining
  /// pass.
  ///
  /// Always [`ControlFlow::Break`], including when the rule is filtered out and there is nothing
  /// to emit. That is the whole difference between a bound and a suggestion: the caller switching
  /// the diagnostic off must not switch the *stopping* off with it.
  ///
  /// Idempotent in what it emits, for the reason the merge engine's own trip is: a collecting sink
  /// does not stop the unwinding, and one refusal per remaining unit of work is noise.
  #[cold]
  fn refuse(&mut self, blame: SimpleSpan) -> ControlFlow<()> {
    // `Left(0)` and not `Off`: this is an exhausted bound, which is the opposite of an absent one.
    self.left = Ledger::Left(0);
    if !self.tripped {
      self.tripped = true;
      if self.on(Rule::ValidationWorkBudget) {
        let diagnostic = Diagnostic::new(Rule::ValidationWorkBudget, blame)
          .context(Context::Count(self.budget.validation_work()));
        let _ = self.emit(diagnostic);
      }
    }
    ControlFlow::Break(())
  }

  /// Charges one node plus the bytes of the name it is identified by.
  #[inline]
  pub(super) fn spend_name(&mut self, name: &Name<S>) -> ControlFlow<()> {
    self.spend(units(name_bytes(name).len()), *name.as_span())
  }

  /// Charges a whole list of names before anything sorts, hashes or compares any of them.
  ///
  /// The prepayment shape. Three rules in this crate — 5.4.2, 5.6.3 and 5.7.3 — begin by *sorting*
  /// a list of names the client wrote, and a per-item charge inside the loop that follows arrives
  /// after `O(N log N)` comparisons have already happened.
  pub(super) fn spend_names<'n, I>(&mut self, names: I) -> ControlFlow<()>
  where
    I: IntoIterator<Item = &'n Name<S>>,
    S: 'n,
  {
    for name in names {
      self.spend_name(name)?;
    }
    ControlFlow::Continue(())
  }

  /// Charges the base name of a type reference before [`Validator::pack_type`] hashes it.
  ///
  /// The walk to the base is bounded by [`MAX_WRAPPERS`] — that is what makes it safe to do
  /// *before* the charge rather than after — and the name it finds is not bounded by anything.
  /// Draft 5.8.5 calls `pack_type` once per **usage**, so a variable used `U` times had its
  /// declared type's spelling hashed `U` times for nothing.
  pub(super) fn spend_type(&mut self, ty: &Type<Name<S>>, blame: SimpleSpan) -> ControlFlow<()> {
    let mut cursor = ty;
    let mut depth = 0usize;
    loop {
      match cursor {
        Type::Name(named) => return self.spend(units(name_bytes(named.name()).len()), blame),
        Type::List(list) => {
          if depth >= MAX_WRAPPERS as usize {
            // Deeper than the packed representation admits, so `pack_type` will refuse before it
            // reaches a name. One unit for the walk that got here.
            return self.spend(1, blame);
          }
          depth += 1;
          cursor = list.ty();
        }
      }
    }
  }

  /// Emits a diagnostic naming a source spelling, at that spelling's own span.
  fn report_name(&mut self, rule: Rule, name: &Name<S>, context: Context) -> ControlFlow<()> {
    let diagnostic = Diagnostic::new(rule, *name.as_span())
      .subject(name.source().clone())
      .context(context);
    self.emit(diagnostic)
  }

  // -- schema helpers -------------------------------------------------------------------------

  /// Resolves a source name to a schema type.
  fn type_of(&self, name: &Name<S>) -> Option<TypeId> {
    self.schema.type_of_sym(self.schema.sym(name_bytes(name))?)
  }

  /// Flattens an AST type reference into the schema's packed form.
  ///
  /// `None` when the base type is not in the schema or the reference nests deeper than the
  /// representation admits. The walk is iterative for the same reason every other walk here is.
  fn pack_type(&self, ty: &Type<Name<S>>) -> Option<PackedType> {
    let mut list_required = [false; MAX_WRAPPERS as usize];
    let mut depth = 0usize;
    let mut cursor = ty;
    let named = loop {
      match cursor {
        Type::Name(named) => break named,
        Type::List(list) => {
          if depth >= list_required.len() {
            return None;
          }
          list_required[depth] = list.required();
          depth += 1;
          cursor = list.ty();
        }
      }
    };
    let sym = self.schema.sym(name_bytes(named.name()))?;
    let id = self.schema.type_of_sym(sym)?;
    let mut packed = PackedType::named(sym, id);
    if named.required() {
      packed = packed.push_non_null()?;
    }
    for required in list_required[..depth].iter().rev() {
      packed = packed.push_list()?;
      if *required {
        packed = packed.push_non_null()?;
      }
    }
    Some(packed)
  }

  // -- the run --------------------------------------------------------------------------------

  fn run(&mut self) -> ControlFlow<()> {
    self.prep()?;
    self.check_operations()?;
    self.check_fragment_declarations()?;
    self.check_fragment_cycles()?;
    self.check_fragments_used()?;
    self.check_subscriptions()?;
    self.walk_operations()?;
    self.walk_unreached_fragments()?;
    // Last, and deliberately so. Draft 5.3.2 is the only rule with a working set and the only one
    // an attacker can make expensive, so every cheap structural refusal — undefined spreads,
    // fragment cycles — is already established before it expands anything. Reordering this would
    // change the threat model, not just the diagnostic order.
    self.check_field_merging()
  }

  // -- 1. prep --------------------------------------------------------------------------------

  fn prep(&mut self) -> ControlFlow<()> {
    let document = self.document;
    for (index, described) in document.definitions().iter().enumerate() {
      let index = index as u32;
      match described.node() {
        ExecutableDefinition::Operation(operation) => {
          let (root, named, span) = match operation {
            OperationDefinition::Named(named) => (
              root_operation(named.operation_type()),
              named.name().is_some(),
              match named.name() {
                Some(name) => *name.as_span(),
                None => *named.operation_type().span(),
              },
            ),
            // Query shorthand: an anonymous query, blamed at its own braces.
            OperationDefinition::Shorthand(set) => (RootOperation::Query, false, *set.span()),
          };
          // The prep sweep is linear in the document, but it is also where the fragment index is
          // sorted by name — `F log F` comparisons over bytes the client chose. The sort's log
          // factor is at most thirty-two whatever the document does, which is a constant multiple
          // of what is charged here and not a second factor.
          self.spend(1, span)?;
          self.scratch.operations.push(OperationRow {
            definition: index,
            root: root.index() as u8,
            named,
            span,
            edges: Default::default(),
          });
        }
        ExecutableDefinition::Fragment(fragment) => {
          self.spend_name(fragment.name())?;
          self.scratch.fragments.push(FragmentRow {
            definition: index,
            span: *fragment.name().as_span(),
            group: Default::default(),
            edges: Default::default(),
          });
        }
      }
    }

    self.index_fragments()?;
    self.collect_edges()
  }

  /// Sorts the fragment ordinals by name, records each name's group, and reports draft 5.5.1.1.
  fn index_fragments(&mut self) -> ControlFlow<()> {
    let document = self.document;
    let count = self.scratch.fragments.len() as u32;
    {
      let scratch = &mut *self.scratch;
      scratch.order.extend(0..count);
      let rows = &scratch.fragments;
      // Ties break on the ordinal, so the order is total and the first definition of a name is
      // always the group's first element — which is the one spreads resolve to.
      scratch.order.sort_unstable_by(|a, b| {
        let left = fragment_name(document, rows, *a);
        let right = fragment_name(document, rows, *b);
        left.cmp(right).then(a.cmp(b))
      });
    }

    let mut start = 0usize;
    while start < self.scratch.order.len() {
      let first = self.scratch.order[start];
      let name = fragment_name(document, &self.scratch.fragments, first);
      let mut end = start + 1;
      while end < self.scratch.order.len()
        && fragment_name(document, &self.scratch.fragments, self.scratch.order[end]) == name
      {
        end += 1;
      }
      let group = range32(start as u32, end as u32);
      for slot in start..end {
        let ordinal = self.scratch.order[slot] as usize;
        self.scratch.fragments[ordinal].group = group;
      }
      if end - start > 1 && self.on(Rule::FragmentNameUniqueness) {
        let related = self.scratch.fragments[first as usize].span;
        for slot in start + 1..end {
          let ordinal = self.scratch.order[slot];
          let row = self.scratch.fragments[ordinal as usize];
          let Some(fragment) = fragment(document, row.definition) else {
            continue;
          };
          let diagnostic = Diagnostic::new(Rule::FragmentNameUniqueness, row.span)
            .subject(fragment.name().source().clone())
            .related(related);
          self.emit(diagnostic)?;
        }
      }
      start = end;
    }
    ControlFlow::Continue(())
  }

  /// Returns the ordinal a fragment name resolves to — the first definition of that name.
  fn find_fragment(&self, name: &[u8]) -> Option<u32> {
    let document = self.document;
    let rows = &self.scratch.fragments;
    let order = &self.scratch.order;
    let slot = order
      .binary_search_by(|ordinal| fragment_name(document, rows, *ordinal).cmp(name))
      .ok()?;
    // Ties are ordered by ordinal, so walking back to the group's start finds the first
    // definition rather than whichever one the search landed on.
    let ordinal = order[slot];
    let group = rows[ordinal as usize].group;
    order.get(group.start() as usize).copied()
  }

  /// Walks every definition once, recording its fragment spreads as graph edges and reporting
  /// draft 5.5.2.1 for the ones that name nothing.
  fn collect_edges(&mut self) -> ControlFlow<()> {
    let document = self.document;
    let total = document.definitions().len() as u32;
    let mut operations = 0usize;
    let mut fragments = 0usize;
    for index in 0..total {
      let start = self.scratch.edges.len() as u32;
      self.collect_definition_edges(index)?;
      let range = range32(start, self.scratch.edges.len() as u32);
      match definition(document, index) {
        Some(ExecutableDefinition::Operation(_)) => {
          self.scratch.operations[operations].edges = range;
          operations += 1;
        }
        Some(ExecutableDefinition::Fragment(_)) => {
          self.scratch.fragments[fragments].edges = range;
          fragments += 1;
        }
        None => {}
      }
    }
    ControlFlow::Continue(())
  }

  fn collect_definition_edges(&mut self, index: u32) -> ControlFlow<()> {
    let document = self.document;
    self.scratch.frames.clear();
    self.scratch.frames.push(Frame::root(index, NONE, 0));
    let mut current = root_selection_set(document, index);
    let blame = current.map_or(SimpleSpan::const_new(0, 0), |set| *set.span());
    while let Some(frame) = self.scratch.frames.last().copied() {
      // `selections::resolve` costs the frame depth and runs after every pop. See
      // `selections::walk_selections` for the whole argument.
      self.spend(self.scratch.frames.len() as u32, blame)?;
      let Some(set) = current else {
        self.scratch.frames.pop();
        current = selections::resolve(document, &self.scratch.frames);
        continue;
      };
      let selections = set.selections();
      let Some(selection) = selections.get(frame.cursor as usize) else {
        self.scratch.frames.pop();
        current = selections::resolve(document, &self.scratch.frames);
        continue;
      };
      if let Some(top) = self.scratch.frames.last_mut() {
        top.cursor += 1;
      }
      match selection {
        Selection::FragmentSpread(spread) => {
          let name = spread.name();
          // `find_fragment` binary-searches the name index, so this reads the spelling about
          // `log F` times. Same constant multiple as the sort that built the index.
          self.spend_name(name)?;
          let to = self.find_fragment(name_bytes(name));
          if to.is_none() && self.on(Rule::FragmentSpreadTargetDefined) {
            let diagnostic = Diagnostic::new(Rule::FragmentSpreadTargetDefined, *name.as_span())
              .subject(name.source().clone());
            self.emit(diagnostic)?;
          }
          self.scratch.edges.push(Edge {
            to: to.unwrap_or(NONE),
            span: *spread.span(),
          });
        }
        _ => {
          if let Some(child) = child_selection_set(selection) {
            self
              .scratch
              .frames
              .push(Frame::child(index, frame.cursor, NONE, 0));
            current = Some(child);
          }
        }
      }
    }
    ControlFlow::Continue(())
  }

  // -- 2. operation rules ---------------------------------------------------------------------

  fn check_operations(&mut self) -> ControlFlow<()> {
    let document = self.document;

    // 5.2.1.1 — the schema must provide the root operation type the operation needs.
    if self.on(Rule::OperationTypeExistence) {
      for index in 0..self.scratch.operations.len() {
        let row = self.scratch.operations[index];
        let root = RootOperation::ALL[row.root as usize];
        if self.schema.root(root).is_none() {
          let diagnostic =
            Diagnostic::new(Rule::OperationTypeExistence, row.span).context(Context::Root(root));
          let diagnostic = match operation_name(document, row.definition) {
            Some(name) => diagnostic.subject(name.source().clone()),
            None => diagnostic,
          };
          self.emit(diagnostic)?;
        }
      }
    }

    // 5.2.2.1 — named operations must not collide. Sorted index scan, no map.
    if self.on(Rule::OperationNameUniqueness) {
      let base = self.scratch.keys.len();
      for index in 0..self.scratch.operations.len() {
        if self.scratch.operations[index].named {
          let name = operation_name_bytes(document, &self.scratch.operations, index as u32);
          let span = self.scratch.operations[index].span;
          self.spend(units(name.len()), span)?;
          self.scratch.keys.push(index as u32);
        }
      }
      {
        let scratch = &mut *self.scratch;
        let rows = &scratch.operations;
        // Ties break on the document index, so the first definition of a name is always the
        // group's first element and every later one is blamed against it.
        scratch.keys[base..].sort_unstable_by(|a, b| {
          let left = operation_name_bytes(document, rows, *a);
          let right = operation_name_bytes(document, rows, *b);
          left.cmp(right).then(a.cmp(b))
        });
      }
      let mut start = base;
      while start < self.scratch.keys.len() {
        let first = self.scratch.keys[start];
        let name = operation_name_bytes(document, &self.scratch.operations, first);
        let mut end = start + 1;
        while end < self.scratch.keys.len()
          && operation_name_bytes(document, &self.scratch.operations, self.scratch.keys[end])
            == name
        {
          end += 1;
        }
        let related = self.scratch.operations[first as usize].span;
        for slot in start + 1..end {
          let row = self.scratch.operations[self.scratch.keys[slot] as usize];
          if let Some(name) = operation_name(document, row.definition) {
            let diagnostic = Diagnostic::new(Rule::OperationNameUniqueness, row.span)
              .subject(name.source().clone())
              .related(related);
            self.emit(diagnostic)?;
          }
        }
        start = end;
      }
      self.scratch.keys.truncate(base);
    }

    // 5.2.3.1 — an anonymous operation must be the document's only one.
    if self.on(Rule::LoneAnonymousOperation) && self.scratch.operations.len() > 1 {
      for index in 0..self.scratch.operations.len() {
        let row = self.scratch.operations[index];
        if !row.named {
          self.emit(
            Diagnostic::new(Rule::LoneAnonymousOperation, row.span)
              .context(Context::Count(self.scratch.operations.len() as u32)),
          )?;
        }
      }
    }

    ControlFlow::Continue(())
  }

  // -- 3. fragment declaration rules ------------------------------------------------------------

  fn check_fragment_declarations(&mut self) -> ControlFlow<()> {
    // Asked before the condition names are read. `check_type_condition` resolves each one through
    // `Schema::sym`, and here — unlike at an inline fragment — the resolved type is discarded, so
    // with nothing to report the pass hashes a document-chosen name per fragment for nothing.
    // Through [`reports_type_conditions`] rather than a pair written out here: the callee's own
    // guards are the other copy of this condition.
    if !reports_type_conditions(self.rules) {
      return ControlFlow::Continue(());
    }
    let document = self.document;
    for ordinal in 0..self.scratch.fragments.len() {
      let row = self.scratch.fragments[ordinal];
      let Some(fragment) = fragment(document, row.definition) else {
        continue;
      };
      let condition = fragment.type_condition().name();
      // `check_type_condition` resolves it through `Schema::sym`, which hashes every byte.
      self.spend_name(condition)?;
      self.check_type_condition(condition)?;
    }
    ControlFlow::Continue(())
  }

  /// Draft 5.5.1.2 and 5.5.1.3, shared by fragment definitions and inline fragments.
  ///
  /// **A rule added here belongs in [`reports_type_conditions`].** `check_fragment_declarations`
  /// calls this only for what it reports and skips itself when that predicate is false, so a rule
  /// reported here and missing there would never fire on a fragment definition.
  ///
  /// Returns the condition's type when it resolved to a composite one, which is also the scope its
  /// selection set is written against.
  fn check_type_condition(&mut self, condition: &Name<S>) -> ControlFlow<(), Option<TypeId>> {
    let Some(id) = self.type_of(condition) else {
      if self.on(Rule::FragmentSpreadTypeExistence) {
        self.report_name(Rule::FragmentSpreadTypeExistence, condition, Context::None)?;
      }
      return ControlFlow::Continue(None);
    };
    if !self.schema.type_def(id).kind().is_composite() {
      if self.on(Rule::FragmentsOnCompositeTypes) {
        let context = Context::Type(self.schema.type_def(id).name());
        self.report_name(Rule::FragmentsOnCompositeTypes, condition, context)?;
      }
      return ControlFlow::Continue(None);
    }
    ControlFlow::Continue(Some(id))
  }

  // -- 4. the fragment graph --------------------------------------------------------------------

  /// Draft 5.5.2.2, as an iterative depth-first search over integers.
  fn check_fragment_cycles(&mut self) -> ControlFlow<()> {
    if !self.on(Rule::FragmentSpreadsMustNotFormCycles) {
      return ControlFlow::Continue(());
    }
    let count = self.scratch.fragments.len();
    reset_bits(&mut self.scratch.on_path, count);
    reset_bits(&mut self.scratch.done, count);
    for start in 0..count as u32 {
      if get_bit(&self.scratch.done, start) {
        continue;
      }
      self.scratch.graph.clear();
      set_bit(&mut self.scratch.on_path, start);
      self.scratch.graph.push(GraphFrame {
        fragment: start,
        edge: self.scratch.fragments[start as usize].edges.start(),
      });
      while let Some(top) = self.scratch.graph.last().copied() {
        let row = self.scratch.fragments[top.fragment as usize];
        if top.edge >= row.edges.end() {
          clear_bit(&mut self.scratch.on_path, top.fragment);
          set_bit(&mut self.scratch.done, top.fragment);
          self.scratch.graph.pop();
          continue;
        }
        if let Some(frame) = self.scratch.graph.last_mut() {
          frame.edge += 1;
        }
        let edge = self.scratch.edges[top.edge as usize];
        self.spend(1, edge.span)?;
        if edge.to == NONE {
          continue;
        }
        if get_bit(&self.scratch.on_path, edge.to) {
          let subject = self.scratch.fragments[edge.to as usize];
          let Some(target) = fragment(self.document, subject.definition) else {
            continue;
          };
          let diagnostic = Diagnostic::new(Rule::FragmentSpreadsMustNotFormCycles, edge.span)
            .subject(target.name().source().clone())
            .related(subject.span);
          self.emit(diagnostic)?;
          continue;
        }
        if get_bit(&self.scratch.done, edge.to) {
          continue;
        }
        set_bit(&mut self.scratch.on_path, edge.to);
        self.scratch.graph.push(GraphFrame {
          fragment: edge.to,
          edge: self.scratch.fragments[edge.to as usize].edges.start(),
        });
      }
    }
    ControlFlow::Continue(())
  }

  /// Draft 5.5.1.4, as reachability from the operations over the same graph.
  ///
  /// Reachability propagates across every definition sharing a name, so one duplicated fragment
  /// name reports 5.5.1.1 and nothing else — the copy is not also "unused".
  fn check_fragments_used(&mut self) -> ControlFlow<()> {
    // The `reachable` bitset reads like 5.5.1.4's private working set and is not: draft 5.3.2's
    // engine reads it too. So this pass runs for its own rule **or** for anything that starts that
    // engine — and what starts it is [`merges`], the one definition the engine itself is gated on,
    // rather than a copy of its condition. The copy is how this guard shipped naming two of the
    // engine's three activating rules.
    if !self.on(Rule::FragmentsMustBeUsed) && !merges(self.rules) {
      return ControlFlow::Continue(());
    }
    let count = self.scratch.fragments.len();
    reset_bits(&mut self.scratch.reachable, count);
    self.scratch.graph.clear();
    for index in 0..self.scratch.operations.len() {
      let edges = self.scratch.operations[index].edges;
      for slot in edges.start()..edges.end() {
        let to = self.scratch.edges[slot as usize].to;
        self.mark_reachable(to)?;
      }
    }
    while let Some(frame) = self.scratch.graph.pop() {
      let edges = self.scratch.fragments[frame.fragment as usize].edges;
      for slot in edges.start()..edges.end() {
        let to = self.scratch.edges[slot as usize].to;
        self.mark_reachable(to)?;
      }
    }

    if !self.on(Rule::FragmentsMustBeUsed) {
      return ControlFlow::Continue(());
    }
    for ordinal in 0..count as u32 {
      if get_bit(&self.scratch.reachable, ordinal) {
        continue;
      }
      let row = self.scratch.fragments[ordinal as usize];
      let Some(target) = fragment(self.document, row.definition) else {
        continue;
      };
      let diagnostic = Diagnostic::new(Rule::FragmentsMustBeUsed, row.span)
        .subject(target.name().source().clone());
      self.emit(diagnostic)?;
    }
    ControlFlow::Continue(())
  }

  /// Marks a fragment and every same-named definition reachable, queueing the newly marked.
  ///
  /// The whole group, not one member, because reachability propagates across a duplicated name —
  /// which is also the quadratic here: `E` spreads of one name shared by `G` definitions walk
  /// `E · G` members off `O(E + G)` of syntax. 5.5.1.1 has already reported the duplication, but
  /// the walk still happens, so it is charged.
  fn mark_reachable(&mut self, ordinal: u32) -> ControlFlow<()> {
    if ordinal == NONE {
      return ControlFlow::Continue(());
    }
    let group = self.scratch.fragments[ordinal as usize].group;
    self.spend(
      group.end().saturating_sub(group.start()),
      self.scratch.fragments[ordinal as usize].span,
    )?;
    for slot in group.start()..group.end() {
      let member = self.scratch.order[slot as usize];
      if !set_bit(&mut self.scratch.reachable, member) {
        self.scratch.graph.push(GraphFrame {
          fragment: member,
          edge: 0,
        });
      }
    }
    ControlFlow::Continue(())
  }

  // -- 5. subscriptions ---------------------------------------------------------------------------

  fn check_subscriptions(&mut self) -> ControlFlow<()> {
    if !self.on(Rule::SingleRootField) {
      return ControlFlow::Continue(());
    }
    let Some(root) = self.schema.root(RootOperation::Subscription) else {
      return ControlFlow::Continue(());
    };
    for index in 0..self.scratch.operations.len() {
      let row = self.scratch.operations[index];
      if RootOperation::ALL[row.root as usize] != RootOperation::Subscription {
        continue;
      }
      self.check_subscription_roots(row, root)?;
    }
    ControlFlow::Continue(())
  }

  /// Draft 5.2.4.1's `CollectSubscriptionFields`, as an explicit walk.
  ///
  /// It only has to answer "is the collected map a set of one, and is that one an introspection
  /// field", so it never builds the map: two distinct response names are enough to fail, and the
  /// first response name seen is the only one it needs to keep.
  fn check_subscription_roots(&mut self, row: OperationRow, root: TypeId) -> ControlFlow<()> {
    let document = self.document;
    reset_bits(&mut self.scratch.visited, self.scratch.fragments.len());
    self.scratch.roots.clear();
    self
      .scratch
      .roots
      .push(Frame::root(row.definition, NONE, 0));
    let mut current = root_selection_set(document, row.definition);

    // The rule only asks whether the collected map is a set of one, so the first response name
    // seen is all that has to be kept: any later name that differs makes at least two entries.
    // The *field* name is kept alongside it, because "must not be an introspection field" is a
    // question about the field and not about the key an alias would give it.
    let mut first_response: Option<&'d Name<S>> = None;
    let mut first_field: Option<&'d Name<S>> = None;
    let mut multiple = false;
    let blame = current.map_or(SimpleSpan::const_new(0, 0), |set| *set.span());

    while let Some(frame) = self.scratch.roots.last().copied() {
      // `selections::resolve` again, over the other stack. Same cost, same placement.
      self.spend(self.scratch.roots.len() as u32, blame)?;
      let Some(set) = current else {
        self.scratch.roots.pop();
        current = selections::resolve(document, &self.scratch.roots);
        continue;
      };
      let Some(selection) = set.selections().get(frame.cursor as usize) else {
        self.scratch.roots.pop();
        current = selections::resolve(document, &self.scratch.roots);
        continue;
      };
      if let Some(top) = self.scratch.roots.last_mut() {
        top.cursor += 1;
      }

      // "{selection} must not provide the `@skip`/`@include` directive" — the whole reason this
      // collection exists is that it has no runtime variables to evaluate them with.
      if let Some(directive) = self.conditional_directive(selection)? {
        let diagnostic = Diagnostic::new(Rule::SingleRootField, *directive.as_span())
          .subject(directive.source().clone());
        self.emit(diagnostic)?;
      }

      match selection {
        Selection::Field(field) => {
          let response = nodes::response_name(field);
          match first_response {
            None => {
              first_response = Some(response);
              first_field = Some(field.name());
            }
            Some(seen) if name_bytes(seen) != name_bytes(response) => multiple = true,
            Some(_) => {}
          }
        }
        Selection::InlineFragment(inline) => {
          let applies = match inline.type_condition() {
            Some(condition) => {
              // `condition_applies` resolves the name through `Schema::sym`.
              self.spend_name(condition.name())?;
              self.condition_applies(condition.name(), root)
            }
            None => true,
          };
          if applies {
            self
              .scratch
              .roots
              .push(Frame::child(frame.definition, frame.cursor, NONE, 0));
            current = Some(inline.selection_set());
          }
        }
        Selection::FragmentSpread(spread) => {
          // `find_fragment` binary-searches the name index and `condition_applies` below hashes
          // the target's condition; both read bytes the document chose.
          self.spend_name(spread.name())?;
          let Some(ordinal) = self.find_fragment(name_bytes(spread.name())) else {
            continue;
          };
          if set_bit(&mut self.scratch.visited, ordinal) {
            continue;
          }
          let target = self.scratch.fragments[ordinal as usize];
          let Some(body) = fragment(document, target.definition) else {
            continue;
          };
          let condition = body.type_condition().name();
          self.spend_name(condition)?;
          if !self.condition_applies(condition, root) {
            continue;
          }
          self
            .scratch
            .roots
            .push(Frame::root(target.definition, NONE, 0));
          current = Some(body.selection_set());
        }
      }
    }

    if multiple {
      // At least two entries. The exact count would cost a second walk and change nothing about
      // the verdict, so the context reports the bound it established.
      self.emit(Diagnostic::new(Rule::SingleRootField, row.span).context(Context::Count(2)))?;
    } else {
      match first_field {
        None => {
          self.emit(Diagnostic::new(Rule::SingleRootField, row.span).context(Context::Count(0)))?;
        }
        Some(field) if is_reserved(name_bytes(field)) => {
          self.report_name(Rule::SingleRootField, field, Context::None)?;
        }
        Some(_) => {}
      }
    }
    ControlFlow::Continue(())
  }

  /// Returns the `@skip` or `@include` a selection carries, if it carries one.
  ///
  /// `&mut self` and a [`ControlFlow`] because it **scans every directive on the selection**, and a
  /// selection carries as many as the document writes. The subscription pass charged one flat unit
  /// per selection and then came here, so `O` selections each carrying `D` directives spent `O`
  /// units on `O · D` of work off `O + D` of syntax.
  fn conditional_directive(
    &mut self,
    selection: &'d Selection<S>,
  ) -> ControlFlow<(), Option<&'d Name<S>>> {
    let directives = match selection {
      Selection::Field(field) => field.directives(),
      Selection::FragmentSpread(spread) => spread.directives(),
      Selection::InlineFragment(inline) => inline.directives(),
    };
    let Some(directives) = directives else {
      return ControlFlow::Continue(None);
    };
    let directives = directives.directives();
    self.spend_names(directives.iter().map(|directive| directive.name()))?;
    ControlFlow::Continue(directives.iter().find_map(|directive| {
      let name = directive.name();
      matches!(name_bytes(name), b"skip" | b"include").then_some(name)
    }))
  }

  /// `DoesFragmentTypeApply` against a known object type.
  fn condition_applies(&self, condition: &Name<S>, object: TypeId) -> bool {
    match self.type_of(condition) {
      // An undefined or non-composite condition is 5.5.1.2/5.5.1.3's business; here it simply
      // cannot contribute a root field.
      Some(id) => self.schema.is_possible_object(id, object),
      None => false,
    }
  }

  // -- 6. the per-operation walks ------------------------------------------------------------

  fn walk_operations(&mut self) -> ControlFlow<()> {
    let document = self.document;
    for index in 0..self.scratch.operations.len() {
      let row = self.scratch.operations[index];
      let Some(operation) = operation(document, row.definition) else {
        continue;
      };

      self.variables = match operation {
        OperationDefinition::Named(named) => match named.variable_definitions() {
          Some(definitions) => definitions.variable_definitions(),
          None => &[],
        },
        OperationDefinition::Shorthand(_) => &[],
      };
      self.in_operation = true;

      self.check_variable_definitions()?;

      let root = RootOperation::ALL[row.root as usize];
      if let OperationDefinition::Named(named) = operation
        && let Some(directives) = named.directives()
      {
        self.check_directives(directives.directives(), directive_location(root), true)?;
      }

      let scope = self.schema.root(root).map_or(NONE, |id| id.get());
      reset_bits(&mut self.scratch.visited, self.scratch.fragments.len());
      self.walk_selections(Frame::root(row.definition, scope, Frame::CHECK))?;

      self.check_variables_used()?;

      // The index is a persistent prefix of `keys` for exactly the length of this operation. Every
      // other user of the buffer pushes at its current length and truncates back, so it survives
      // the walk; this is where it stops.
      self
        .scratch
        .keys
        .truncate(self.variable_index.start() as usize);
      self.variables = &[];
      self.in_operation = false;
    }
    ControlFlow::Continue(())
  }

  // -- 7. fragments nothing reached -------------------------------------------------------------

  fn walk_unreached_fragments(&mut self) -> ControlFlow<()> {
    let document = self.document;
    for ordinal in 0..self.scratch.fragments.len() {
      let row = self.scratch.fragments[ordinal];
      if get_bit(&self.scratch.checked, row.definition) {
        continue;
      }
      let Some(body) = fragment(document, row.definition) else {
        continue;
      };
      // The condition was already reported on, if it needed reporting, by the declaration pass —
      // but resolving it still hashes it, so it is still charged.
      let condition = body.type_condition().name();
      self.spend_name(condition)?;
      let scope = self.composite_of(condition).map_or(NONE, |id| id.get());
      self.begin_fragment(row.definition)?;
      self.walk_selections(Frame::root(row.definition, scope, Frame::CHECK))?;
    }
    ControlFlow::Continue(())
  }

  /// Records that a fragment's definition-local rules are running now, and checks the directives
  /// on the definition itself.
  pub(super) fn begin_fragment(&mut self, index: u32) -> ControlFlow<()> {
    grow_bits(&mut self.scratch.checked, index as usize + 1);
    set_bit(&mut self.scratch.checked, index);
    let Some(body) = fragment(self.document, index) else {
      return ControlFlow::Continue(());
    };
    match body.directives() {
      Some(directives) => self.check_directives(
        directives.directives(),
        DirectiveLocation::FragmentDefinition,
        true,
      ),
      None => ControlFlow::Continue(()),
    }
  }

  // -- variables ------------------------------------------------------------------------------

  /// Draft 5.8.1 and 5.8.2, plus the value and directive rules over a variable definition's own
  /// default value and directives — the one place in an executable document where constant values
  /// appear.
  fn check_variable_definitions(&mut self) -> ControlFlow<()> {
    let definitions = self.variables;
    reset_bits(&mut self.scratch.used, definitions.len());

    // The operation's variable-name index, built once and read by every usage.
    //
    // It replaces the scan over *every* definition that each usage used to run: `U` usages against
    // `V` definitions was `U · V` name comparisons before any ledger was consulted, and 4,000 of
    // each measured 60 ms off 250 KB of syntax, against 0.33 ms for the same declarations with one
    // usage. al8n/smear#198.
    //
    // Ordinals sorted by name with ties broken on the ordinal — the order draft 5.8.1 already
    // needed, so this is one sort where there were two. **The tie-break is what keeps the marking
    // semantics exactly as they were.** A name's definitions form a contiguous run ordered by
    // ordinal, so the run's first element is the lowest-numbered definition of that name — the one
    // a usage resolves against — and the run entire is what gets marked used. Every definition of
    // a duplicated name is marked, not only the first: that duplication is 5.8.1's business, and
    // calling the copy "never used" as well would report one mistake twice.
    //
    // Built whether or not 5.8.1 is enabled, and that is not a rule being evaluated when it is
    // off: the marks feed 5.8.4 and the lookup feeds 5.8.3 and 5.8.5, and a binary search over
    // this index is cheaper than the linear scan it replaces in every rule set, empty ones
    // included.
    let base = self.scratch.keys.len();
    self.variable_index = Range32::new(base as u32, base as u32);
    // 5.8.1 reads the index directly and 5.8.3/5.8.5 read it through the value walk. Nothing else
    // looks at it, so with none of them on it is a sort over document-chosen names, charged, for
    // nobody. Not an early return: 5.8.2 and the default-value and directive walks below are a
    // different question and answer it themselves.
    // 5.8.1 reads the index directly; 5.8.3 and 5.8.5 read it through the value walk. Nothing
    // else looks at it.
    let indexed = self.collects_usages || self.on(Rule::VariableUniqueness);
    if indexed {
      for (index, described) in definitions.iter().enumerate() {
        let variable = described.node().variable();
        self.spend(units(name_bytes(variable.name()).len()), *variable.span())?;
        self.scratch.keys.push(index as u32);
      }
      let end = self.scratch.keys.len();
      self.scratch.keys[base..end].sort_unstable_by(|a, b| {
        let left = name_bytes(definitions[*a as usize].node().variable().name());
        let right = name_bytes(definitions[*b as usize].node().variable().name());
        left.cmp(right).then(a.cmp(b))
      });
      self.variable_index = Range32::new(base as u32, end as u32);
    }
    let end = self.variable_index.end() as usize;

    // 5.8.1 — one type per variable name, per operation, off the adjacent pairs of that index.
    if indexed && self.on(Rule::VariableUniqueness) {
      let mut slot = base + 1;
      while slot < end {
        let earlier = self.scratch.keys[slot - 1].min(self.scratch.keys[slot]);
        let later = self.scratch.keys[slot - 1].max(self.scratch.keys[slot]);
        let first = definitions[earlier as usize].node().variable();
        let repeat = definitions[later as usize].node().variable();
        if name_bytes(first.name()) == name_bytes(repeat.name()) {
          let diagnostic = Diagnostic::new(Rule::VariableUniqueness, *repeat.span())
            .subject(repeat.name().source().clone())
            .related(*first.span());
          self.emit(diagnostic)?;
        }
        slot += 1;
      }
    }

    for described in definitions {
      let definition = described.node();
      let variable = definition.variable();
      self.spend_type(definition.ty(), *variable.span())?;
      let declared = self.pack_type(definition.ty());

      // 5.8.2 — objects, interfaces and unions cannot be variable types, and neither can a name
      // the schema does not have.
      let is_input =
        declared.is_some_and(|packed| self.schema.type_def(packed.base_id()).kind().is_input());
      if !is_input && self.on(Rule::VariablesAreInputTypes) {
        let context = match declared {
          Some(packed) => Context::Expected(packed),
          None => Context::None,
        };
        self.report_name(Rule::VariablesAreInputTypes, variable.name(), context)?;
      }

      if let Some(default) = definition.default_value() {
        self.walk_value(default.value(), declared, ValueLocation::PLAIN, true)?;
      }

      if let Some(directives) = definition.directives() {
        self.check_directives(
          directives.directives(),
          DirectiveLocation::VariableDefinition,
          true,
        )?;
      }
    }
    ControlFlow::Continue(())
  }

  /// Draft 5.8.4, read off the marks the usages left.
  fn check_variables_used(&mut self) -> ControlFlow<()> {
    if !self.on(Rule::AllVariablesUsed) {
      return ControlFlow::Continue(());
    }
    for index in 0..self.variables.len() {
      if get_bit(&self.scratch.used, index as u32) {
        continue;
      }
      let variable = self.variables[index].node().variable();
      let diagnostic = Diagnostic::new(Rule::AllVariablesUsed, *variable.span())
        .subject(variable.name().source().clone());
      self.emit(diagnostic)?;
    }
    ControlFlow::Continue(())
  }
}

// ---------------------------------------------------------------------------------------------
// free helpers
// ---------------------------------------------------------------------------------------------

/// Builds a table range.
fn range32(start: u32, end: u32) -> super::schema::Range32 {
  super::schema::Range32::new(start, end)
}

/// Grows a bitset to hold at least `bits` bits, preserving what is already set.
fn grow_bits(words: &mut std::vec::Vec<u64>, bits: usize) {
  let needed = bits.div_ceil(64);
  if words.len() < needed {
    words.resize(needed, 0);
  }
}

/// The root operation slot an operation keyword needs.
fn root_operation(keyword: &OperationType) -> RootOperation {
  match keyword {
    OperationType::Query(_) => RootOperation::Query,
    OperationType::Mutation(_) => RootOperation::Mutation,
    OperationType::Subscription(_) => RootOperation::Subscription,
  }
}

/// The directive location an operation of this kind is.
fn directive_location(root: RootOperation) -> DirectiveLocation {
  match root {
    RootOperation::Query => DirectiveLocation::Query,
    RootOperation::Mutation => DirectiveLocation::Mutation,
    RootOperation::Subscription => DirectiveLocation::Subscription,
  }
}

/// Returns an operation's name node, when it has one.
fn operation_name<S>(document: &ExecutableDocument<S>, index: u32) -> Option<&Name<S>> {
  match operation(document, index)? {
    OperationDefinition::Named(named) => named.name(),
    OperationDefinition::Shorthand(_) => None,
  }
}

/// Returns an operation's name bytes, or the empty slice when it is anonymous.
fn operation_name_bytes<'d, S>(
  document: &'d ExecutableDocument<S>,
  rows: &[OperationRow],
  index: u32,
) -> &'d [u8]
where
  S: AsRef<[u8]>,
{
  rows
    .get(index as usize)
    .and_then(|row| operation_name(document, row.definition))
    .map_or(&[][..], name_bytes)
}

/// Returns a fragment's name bytes.
fn fragment_name<'d, S>(
  document: &'d ExecutableDocument<S>,
  rows: &[FragmentRow],
  ordinal: u32,
) -> &'d [u8]
where
  S: AsRef<[u8]>,
{
  rows
    .get(ordinal as usize)
    .and_then(|row| fragment(document, row.definition))
    .map_or(&[][..], |fragment| name_bytes(fragment.name()))
}

#[cfg(test)]
mod tests {
  use super::{Ledger, units};
  use crate::Budget;

  /// An absent bound is closed under spending.
  ///
  /// The property the four previous versions of this defect did not have. `u32::MAX` was the
  /// documented spelling of "never refuse" and was also just a number, so the first charge turned
  /// it into a very large *finite* budget and the lossless door's prepayment turned it into a
  /// smaller one again. Reachable at about 65,000 operations over a 65,000-selection fragment,
  /// which is a document this suite is not going to build — so the property is asserted where it
  /// lives, over the type, rather than inferred from one document that happens to be big enough.
  #[test]
  fn an_absent_bound_is_closed_under_spending() {
    let off = Ledger::open(&Budget::default().with_validation_work(u32::MAX));
    assert_eq!(off, Ledger::Off);

    // Every charge shape that exists: a single unit, a whole default ceiling, the largest charge
    // `units` can produce, and the prepayment the lossless door takes before a validator exists.
    let mut ledger = off;
    for charge in [
      1,
      Budget::DEFAULT_VALIDATION_WORK,
      units(usize::MAX),
      u32::MAX,
      0,
    ] {
      ledger = ledger
        .take(charge)
        .expect("an absent bound has room for anything");
      assert_eq!(
        ledger,
        Ledger::Off,
        "a charge of {charge} turned an absent bound into a number"
      );
    }

    // And a bound that IS set is not confused with one that is not, at the value next to it.
    let bounded = Ledger::open(&Budget::default().with_validation_work(u32::MAX - 1));
    assert_eq!(bounded, Ledger::Left(u32::MAX - 1));
    assert_eq!(
      bounded.take(u32::MAX),
      None,
      "an exhausted bound must refuse"
    );
    assert_eq!(bounded.take(u32::MAX - 1), Some(Ledger::Left(0)));

    // `Left(0)` is an exhausted bound, which is the opposite of an absent one and must not spend.
    assert_eq!(Ledger::Left(0).take(1), None);
    assert_eq!(Ledger::Left(0).take(0), Some(Ledger::Left(0)));
  }
}
