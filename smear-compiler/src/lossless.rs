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
//! - **Report it, and say so.** [`Recovery::is_complete`] false says at least one top-level
//!   element had no AST image. That is this door's choice, and
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
//! document is what this door exists to replace. So it reports, the [`Recovery`] rides along
//! wherever a projection ran — which is both arms of the build, and neither arm of a refusal taken
//! before it — and `tests/validator_lossless_schema.rs` pins both artifacts.
//!
//! # Why these four doors take `&str` while the parse doors do not — al8n/smear#121
//!
//! Every public lossless **parse** door has a `_from` sibling taking anything the parser's
//! `LosslessSource` admits — `&str`, `&[u8]` and every backing this crate ships — and answering
//! `Result<Parse, Refused>`, so a source a green tree cannot hold comes back from **a sibling** as
//! `Err` with no `Parse` at all: no tree, no diagnostic and no panic. Two things make a source
//! unholdable and the sibling checks both — it is not valid UTF-8, or it is longer than `rowan`'s
//! `u32` text sizes can address. Through a concrete `&str` door an over-length source still
//! panics, which is pre-existing and documented there. The doors here have no such sibling, and
//! that is a decision.
//!
//! **Four doors, and they are not held by the same thing.**
//!
//! - `validate_executable_lossless<'src, K>(…, source: &'src str, …, sink: &mut K)
//!   where K: Sink<&'src str>` — **two bindings, one lifetime.** `source` is `&'src str` because
//!   `Verified::new` stores it and the projection re-slices it into an
//!   `ExecutableDocument<&'src str>`: the AST borrows the caller's buffer, so this parameter *is*
//!   the AST's source type. `K: Sink<&'src str>` because the diagnostics that reach the sink carry
//!   those same slices, so the sink's source type is the AST's. Widening one without the other is
//!   not expressible, and widening both changes what every consumer of the lossless half receives.
//! - `validate_executable_lossless_with` — the same signature with a `RuleSet`, and the same two
//!   reasons unchanged.
//! - `validate_executable_lossless_verified_with<'src, K>(…, pair: Verified<'_, 'src>, …)` —
//!   **there is no `source` parameter here at all.** `'src` arrives through the pair, whose
//!   `source()` is the `&'src str` the first bullet's reason binds, so the type is not this
//!   signature's to widen: it is `Verified`'s, and `Verified`'s is the projection's. The
//!   `K: Sink<&'src str>` half is unchanged, for the first bullet's reason.
//! - `validate_schema_lossless(parse: &Parse, source: &str) -> Result<(Schema, Recovery), …>` —
//!   **no `K`, no sink, and no borrow in the output at all.** The `Schema` it returns is owned, so
//!   "the AST borrows the buffer" is false here. What pins it is one step earlier: the door runs
//!   `project_type_system_document_recovered`, which is `&str`-keyed and hands back a
//!   `TypeSystemDocument<&'src str>`. `Schema::build` would take a byte-keyed document; the
//!   projection in front of it would not.
//!
//! **A `_from` sibling for the schema door is additive, and deliberately not in this change.** It
//! would need a state on `LosslessSchemaErrors` for a source that is not UTF-8. That enum is
//! `#[non_exhaustive]`, so adding one breaks nobody — but the obvious name is taken: its `Refused`
//! variant already means *the projected document is not a schema*, carrying `SchemaErrors` from
//! `Schema::build`, which is a different event with a different remedy. Naming a second refusal
//! well is a public-API decision.
//!
//! **Nothing in any of the four needs UTF-8.** The pair's verification walks the green tree
//! comparing `token.text().as_bytes()` against `source.as_bytes()`; the projection re-slices
//! `source` by byte ranges the tree already holds, and every one of those is a token boundary, so
//! the slicing is not what pins the type either. Where the `rowan` constraint genuinely binds is
//! materialisation, one layer below — tokora's `CstText`, in the parse door — and it is
//! *refusable* there rather than binding here.
//!
//! `ci/source_census`'s table records **three** of the four against #121 rather than closing it,
//! so the narrowing stays visible in a run's own output for as long as it stands. The one it does
//! not record is `_verified_with`: the census convicts concrete text *parameters*, that door has
//! none, and an entry for it would match nothing and fail the table's own stale-exemption check.
//!
//! **What it costs a byte-backed caller, exactly.** The parse spares them the *pre*-validation:
//! `parse_*_from` takes the bytes as they are. Every door above then wants a `&str` — three of
//! them as a parameter and `_verified_with` through the `Verified` a caller builds — so a
//! byte-backed caller performs a fresh `core::str::from_utf8` before validating, and **handles**
//! its error rather than unwrapping it, unless they are holding the exact buffer the parse ran
//! over and it has not changed since.
//!
//! That proviso is load-bearing. A `Parse` is deliberately **lifetime-free** — that is what lets a
//! consumer cache one per file — so it borrows nothing and pins nothing. `parse_*_from` borrows
//! its source for the call and returns a value that outlives the borrow. What the caller still
//! owns can then change: a `Vec<u8>` is pushed to or truncated, in safe code and with nothing to
//! stop it; the variable that held a `bytes::Bytes` is reassigned to a different one; a `String`'s
//! bytes are rewritten through `as_mut_vec`, which is `unsafe` and therefore the caller's own
//! contract rather than this crate's, but is still a supported way for the buffer to stop being
//! text. So the conversion can genuinely fail on a buffer whose parse reported nothing.
//! **A successful parse is a fact about bytes that were read, never about the bytes a variable
//! holds now.**
//!
//! What would make the stronger claim true is a type, not a sentence: a source-bound proof —
//! something the parse hands back that *borrows* the buffer, so the borrow checker refuses the
//! mutation rather than a paragraph asking the caller not to make it. This crate does not have one
//! and this note is not proposing it; the gap is structural, and prose is not a guarantee.
//!
//! The residual, then, is one validation pass and a branch on its result — linear in the buffer
//! rather than a copy of it, and an `Err` a caller must have an answer for. It runs over whatever
//! that variable holds at the moment the conversion runs, not over the buffer the parse walked.
//!
//! **And a source the parser would not read never becomes a `Parse` at all**, because
//! `parse_*_from` answers `Result<Parse, Refused>` and a concrete `&str` door is never reached
//! with one. The two refusal matches below name `Unverified::TooDeep`, `Unverified::WrongRoot` and
//! `Unverified::SourceMismatch` in arms of their own, and their wildcard covers only what a later
//! `#[non_exhaustive]` variant might add.
//!
//! [`project_executable_document`]: smear_parser::graphql::lossless::project_executable_document
//! [`project_executable_document_recovered`]: smear_parser::graphql::lossless::project_executable_document_recovered
//! [`Schema::build`]: super::Schema::build

use smear_parser::graphql::lossless::{
  Parse, project_executable_document_verified, project_type_system_document_recovered,
};

pub use smear_parser::{
  graphql::lossless::{Unverified, Verified},
  lossless::project::Recovery,
};

use tokora::SimpleSpan;

use super::{
  Budget, Diagnostic, Invalid, Refusal, Rule, RuleSet, Schema, SchemaErrors, Scratch, Sink,
  diagnostic::Context,
  executable::{Ledger, units, validate_charged},
};

/// The verdict of a failed lossless validation.
///
/// [`Invalid`] plus the [`Recovery`] the successful arm carries, when the projection ran; see
/// [`LosslessInvalid::recovery`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LosslessInvalid {
  invalid: Invalid,
  /// `None` exactly when the projection never ran.
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
  /// # `None` means the projection never ran
  ///
  /// `Some` exactly when the projection ran, whatever the rules then found; `None` when the call
  /// stopped before projecting — the pair was refused, or the budget could not pay for the
  /// projection; [`Invalid::refusal`](super::Invalid::refusal) is then `Some` and names which.
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
    // Branching on the state, not on a count synthesised to keep this arm total: a synthesised `1`
    // would render as "1 skipped by recovery" for a projection that had not looked at anything.
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
/// Returns `Ok(recovery)` only when the pair verified, the projection ran, and the rules emitted
/// nothing and no budget tripped. Everything else is `Err`, and three accessors each report one
/// independent fact about it:
///
/// - [`LosslessInvalid::recovery`] is `Some` when the projection ran and `None` when the call
///   stopped before projecting.
/// - [`Invalid::refusal`](super::Invalid::refusal) is [`Refusal::Budget`] when a budget tripped —
///   before the projection or during the rules alike — [`Refusal::SourceMismatch`],
///   [`Refusal::TooDeep`] or [`Refusal::WrongRoot`] for a refused pair, and `None` otherwise.
/// - [`Invalid::stopped`](super::Invalid::stopped) is `true` when the sink returned `Break` on a
///   diagnostic.
///
/// The [`Recovery`] says how much of the parse had an AST image.
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
/// With [`is_complete`](Recovery::is_complete) false, nothing examined what was skipped: a parse
/// with nothing projectable in it validates as `Ok` with an empty sink.
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
/// produces the document the rules read, so an empty [`RuleSet`] still costs it.
///
/// # What the ledger bounds here, and what it does not
///
/// [`Budget::validation_work`](super::Budget::validation_work) bounds **the projection and the
/// validation**. It does not bound the pair's *verification*: on this entry point
/// [`Verified::new`] runs first, outside the ledger, and costs `O(green elements + source bytes)`
/// — it visits every node and token of the green tree and compares every token's bytes, and a
/// `Parse` minted through [`finish_root`](smear_parser::lossless::runner::finish_root) can hold any
/// number of zero-width nodes, so neither the token count nor the source length bounds it.
/// [`validate_executable_lossless_verified_with`] takes a [`Verified`] and runs no verification
/// before its ledger opens. al8n/smear#198.
///
/// # What the prepayment prices
///
/// One payment, before the projection, of two terms that do not bound each other:
///
/// - `units(source.len())`, for the **bytes** — the projector re-reads every token whose text
///   reaches the AST (names, numbers, strings, the spellings a position classifies) through the
///   lexer's own doors; and
/// - the pair's own
///   [`projection_cost`](smear_parser::graphql::lossless::Verified::projection_cost), for the
///   **elements** — one per green node and one per token, counted by the walk that verified the
///   pair.
///
/// The second term exists because bytes do not bound structure: a zero-width node adds structure
/// without adding a byte.
///
/// **This is the one place the two doors do not answer identically.** The rules are the same call;
/// the resource is not, because this door charges the prepayment before any rule runs. With
/// [`Budget::validation_work`](super::Budget::validation_work) below the prepayment this door
/// refuses at the whole input's span before a rule ran, where the syntactic door charges node by
/// node. `tests/validator_lossless.rs` compares the two doors' diagnostics.
///
/// # A mismatched pair is an error, not a weakened answer
///
/// When `parse` and `source` do not describe one document this returns `Err` with
/// [`LosslessInvalid::recovery`] `None` and
/// [`Invalid::budget_tripped`](super::Invalid::budget_tripped) **false**. The same holds for a pair
/// too deep to descend and a parse whose root is not this dialect's document root;
/// [`Invalid::refusal`](super::Invalid::refusal) names which: [`Refusal::SourceMismatch`],
/// [`Refusal::TooDeep`] or [`Refusal::WrongRoot`].
///
/// The pair is checked before the ledger opens, so a mismatch answers
/// [`Refusal::SourceMismatch`](super::Refusal::SourceMismatch) at every budget including zero.
///
/// # When the budget cannot pay for the projection
///
/// `Err`, with [`Invalid::budget_tripped`](super::Invalid::budget_tripped) set,
/// [`Rule::ValidationWorkBudget`](super::Rule::ValidationWorkBudget) in the sink at the whole
/// input's span when the rule set contains it, and [`LosslessInvalid::recovery`] **`None`**:
/// nothing was projected. A budget that trips later, during the rules, also sets
/// `budget_tripped`, and there the projection ran and `recovery` is `Some`.
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
  // Verifies, then delegates. The verification is `O(green elements + source bytes)` and it runs
  // **before the ledger is opened**. al8n/smear#198.
  let pair = Verified::new(parse, source).map_err(|refusal| LosslessInvalid {
    // Three reasons, three `Refusal`s.
    invalid: match refusal {
      Unverified::TooDeep { .. } => Invalid::too_deep(),
      Unverified::WrongRoot { raw } => Invalid::wrong_root(raw),
      Unverified::SourceMismatch => Invalid::unexamined(),
      _ => Invalid::unexamined(),
    },
    recovery: None,
  })?;
  validate_executable_lossless_verified_with(schema, pair, scratch, budget, rules, sink)
}

/// [`validate_executable_lossless_with`] for a pair that already carries its verification.
///
/// It runs no verification: the first thing it does is price the projection and open the ledger.
/// The projection is infallible here because it runs no verification either.
pub fn validate_executable_lossless_verified_with<'src, K>(
  schema: &Schema,
  pair: Verified<'_, 'src>,
  scratch: &mut Scratch,
  budget: &Budget,
  rules: RuleSet,
  sink: &mut K,
) -> Result<Recovery, LosslessInvalid>
where
  K: Sink<&'src str>,
{
  let source = pair.source();
  // **Two dimensions, and neither bounds the other.** Bytes: the projector re-reads every token
  // whose text reaches the AST through the lexer's own doors, which reads them. Elements: it visits
  // a node to dispatch on its kind and a token to read its text. The element count is
  // [`Verified`]'s, established by the same walk that verified the pair.
  let cost = units(source.len()).saturating_add(pair.projection_cost());
  let Some(left) = Ledger::open(budget).take(cost) else {
    let (emitted, stopped) = refuse_projection(source, budget, rules, sink);
    return Err(LosslessInvalid {
      invalid: Invalid::refused(emitted, stopped),
      // Nothing looked at anything, so there is nothing to report.
      recovery: None,
    });
  };
  let (document, recovery) = project_executable_document_verified(pair);
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
/// The sink's answer is **returned rather than discarded**, and becomes
/// [`Invalid::stopped`](super::Invalid::stopped): `true` when the sink returned `Break` on the
/// budget diagnostic.
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
/// [`SchemaErrors`] plus the [`Recovery`] the successful arm carries — [`LosslessInvalid`]'s twin.
/// [`LosslessSchemaErrors::Refused`] carries both, and is the one variant made after the
/// projection ran. The three made before it — [`SourceMismatch`](Self::SourceMismatch),
/// [`TooDeep`](Self::TooDeep) and [`WrongRoot`](Self::WrongRoot) — carry neither; for them
/// [`LosslessSchemaErrors::refusal`] names the reason and [`LosslessSchemaErrors::recovery`]
/// answers `None`.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum LosslessSchemaErrors {
  /// The `parse` and the `source` do not describe one document, so nothing was projected and
  /// [`Schema::build`](super::Schema::build) was never asked.
  ///
  SourceMismatch,
  /// The supplied tree is deeper than `smear_parser::lossless::project::MAX_GREEN_DEPTH`, so
  /// nothing was projected and [`Schema::build`](super::Schema::build) was never asked.
  ///
  /// A refusal of the tree, not of its bytes, which may agree with the source exactly. A parse of
  /// the same source through the dialect's own door is never this deep, so the tree was built
  /// outside that door. al8n/smear#198.
  TooDeep,
  /// The `parse`'s root is not the dialect's document root, so nothing was projected and
  /// [`Schema::build`](super::Schema::build) was never asked.
  ///
  /// No parse this crate's doors produce has one: the pair was minted through the public, generic
  /// `finish_root`, whose root kind is the caller's argument. The bytes may agree exactly.
  /// al8n/smear#218.
  WrongRoot {
    /// The root's raw kind, as the green tree stores it.
    raw: u16,
  },
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
  /// `None` when the build was never asked — [`LosslessSchemaErrors::SourceMismatch`],
  /// [`LosslessSchemaErrors::TooDeep`] and [`LosslessSchemaErrors::WrongRoot`].
  #[inline]
  pub const fn errors(&self) -> Option<&SchemaErrors> {
    match self {
      Self::Refused { errors, .. } => Some(errors),
      _ => None,
    }
  }

  /// Returns why this door refused, when the reason was not the schema itself.
  ///
  /// [`Invalid::refusal`](super::Invalid::refusal)'s twin for the SDL side: `Some` for the three
  /// variants made before the projection, `None` for [`LosslessSchemaErrors::Refused`].
  #[inline]
  pub const fn refusal(&self) -> Option<Refusal> {
    match self {
      Self::SourceMismatch => Some(Refusal::SourceMismatch),
      Self::TooDeep => Some(Refusal::TooDeep),
      Self::WrongRoot { raw } => Some(Refusal::WrongRoot { raw: *raw }),
      _ => None,
    }
  }

  /// Returns how much of the parse had an AST image.
  ///
  /// `Some` exactly for [`LosslessSchemaErrors::Refused`], the one variant made after the
  /// projection ran.
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
      Self::TooDeep => {
        f.write_str("the parse nests deeper than a projection will descend, so nothing was built")
      }
      Self::WrongRoot { raw } => write!(
        f,
        "the parse's root has raw kind {raw}, which is not this dialect's document root, so the \
         parse was minted outside its door and nothing was built"
      ),
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
/// Returns `Ok((schema, recovery))` when the projection ran and the projected document is a schema.
/// The build is not gated on [`Recovery::is_complete`]: `Ok` may carry an incomplete [`Recovery`]
/// when the definitions that were projected form a valid schema on their own. `Err` covers either
/// a build refusal — [`LosslessSchemaErrors::Refused`], where the projection ran and the
/// [`Recovery`] rides along — or a refusal made before the projection could run —
/// [`LosslessSchemaErrors::SourceMismatch`],
/// [`LosslessSchemaErrors::TooDeep`] and [`LosslessSchemaErrors::WrongRoot`], which carry none.
/// [`LosslessSchemaErrors::refusal`] is `Some` for those three and `None` for `Refused`;
/// [`LosslessSchemaErrors::recovery`] is `Some` for `Refused` and `None` for those three.
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
/// With [`is_complete`](Recovery::is_complete) false the builder was handed only the definitions
/// that had an AST image.
///
/// A projection refusal never reaches the returned [`SchemaErrors`]: it is not a draft §3 finding
/// and does not become one. The parse's own diagnostics already describe the syntax that broke.
///
/// # Several documents
///
/// For a schema that spans more than one file, [`SchemaBuilder`](super::SchemaBuilder) takes
/// several documents, and
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
  // The same whole-root verification the executable door gets, made inside the shared recovering
  // projection rather than spelled here.
  let (document, recovery) =
    project_type_system_document_recovered(parse, source).map_err(|refusal| match refusal {
      Unverified::TooDeep { .. } => LosslessSchemaErrors::TooDeep,
      Unverified::WrongRoot { raw } => LosslessSchemaErrors::WrongRoot { raw },
      Unverified::SourceMismatch => LosslessSchemaErrors::SourceMismatch,
      _ => LosslessSchemaErrors::SourceMismatch,
    })?;
  match Schema::build(&document) {
    Ok(schema) => Ok((schema, recovery)),
    Err(errors) => Err(LosslessSchemaErrors::Refused { errors, recovery }),
  }
}
