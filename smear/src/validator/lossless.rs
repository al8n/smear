//! The lossless door: draft §5 over a rowan CST.
//!
//! This module is private and its header is not published. What a caller has to know is stated on
//! [`validate_executable_lossless`] and on [`Recovery`], where the published documentation will
//! find it; what is here is the reasoning behind the shape, for a reader of the source.
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
//! [`project_executable_document`]: crate::parser::graphql::lossless::project_executable_document
//! [`project_executable_document_recovered`]: crate::parser::graphql::lossless::project_executable_document_recovered

use crate::parser::graphql::lossless::{Parse, project_executable_document_recovered};

pub use crate::parser::lossless::project::Recovery;

use super::{Budget, Invalid, RuleSet, Schema, Scratch, Sink, validate_executable_with};

/// The verdict of a failed lossless validation.
///
/// [`Invalid`] plus the [`Recovery`] the successful arm carries, so the two facts a caller needs
/// — *what was wrong* and *how much of the document was looked at* — arrive together whichever
/// way the result went.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LosslessInvalid {
  invalid: Invalid,
  recovery: Recovery,
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
  #[inline]
  pub const fn recovery(&self) -> Recovery {
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
/// [`parse_executable_document`](crate::parser::graphql::lossless::parse_executable_document)
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
/// # #[cfg(all(feature = "graphql", feature = "rowan"))] {
/// use smear::{
///   lexer::tokora::{Parse as _, Parser},
///   parser::graphql::{
///     GraphQL,
///     ast::TypeSystemDocument,
///     error::GraphqlErrors,
///     lossless::parse_executable_document,
///     syntactic::{GraphqlLexer, type_system_document},
///   },
///   validator::{Budget, First, Rule, Schema, Scratch, validate_executable_lossless},
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
  let (document, recovery) = project_executable_document_recovered(parse, source);
  match validate_executable_with(schema, &document, scratch, budget, rules, sink) {
    Ok(()) => Ok(recovery),
    Err(invalid) => Err(LosslessInvalid { invalid, recovery }),
  }
}
