//! The GraphQL CST → AST projection.
//!
//! [`project`] turns a lossless parse plus the text it was parsed from into **the AST the
//! syntactic parser produces for that text**, without re-parsing. It is the door an editor
//! goes through: parse losslessly once, format and highlight off the tree, and hand the same
//! parse to a consumer that wants typed nodes — a validator (#85), a name resolver, a codegen
//! back end.
//!
//! ```
//! # #[cfg(all(feature = "graphql", feature = "rowan"))] {
//! use smear_parser::graphql::lossless::{parse_document, project};
//!
//! let source = "type Query { field: Int }";
//! let parse = parse_document(source);
//! assert!(!parse.has_errors());
//!
//! let ast = project(&parse, source).expect("a well-shaped tree projects");
//! assert_eq!(ast.definitions().len(), 1);
//! # }
//! ```
//!
//! # Why `source` is a parameter, and why the pair is checked
//!
//! The AST is keyed by `S = &'src str` — the syntactic parser has the same property — and the
//! bytes it borrows are the **caller's**, not the tree's. A green token does carry its own text
//! and could lend it, but that text belongs to the parse: an AST borrowing it could not outlive
//! the [`Parse`] it came from, and `Parse` is deliberately lifetime-free precisely so an editor
//! can cache one per file and drop it on the next keystroke. The caller who wants "the AST
//! without re-parsing" is holding the source by construction, so it is an argument.
//!
//! It is **verified, not trusted**: each door compares the whole of the tree's text against
//! `source` before it walks anything, and a mismatch is [`ProjectErrorKind::SourceMismatch`]
//! rather than a silently wrong AST pointing into unrelated bytes. The comparison is one green
//! walk — see [`verify_source`] — so it covers punctuation and trivia too, which a per-token
//! check never reached, and every slice taken afterwards is a plain index.
//!
//! # Shape-faithful, not verdict-faithful
//!
//! The projection succeeds iff the tree's **shape** determines a well-formed AST. It does not
//! re-derive the acceptance verdict, which stays [`Parse::has_errors`].
//!
//! The two come apart in one measured direction. `type T { x: Int` — no closing brace — leaves a
//! shape-complete tree with no recovery hole in it, so it projects, while the syntactic parser
//! rejects it. Making the projection refuse it would mean re-implementing delimiter accounting,
//! which is the grammar, which is the drift this door exists to avoid. **Check
//! [`has_errors`](Parse::has_errors) first**; a projection of an errorful tree is best-effort.
//!
//! In the other direction the projection is stricter than shape alone: a tree carrying an
//! [`Error`](SyntaxKind::Error) hole or a [`Gap`](SyntaxKind::Gap) tile is refused outright,
//! before any walk, because a hole is a region with no AST image and skipping it would be data
//! loss wearing a success type.
//!
//! # What is walked, and how a span is folded
//!
//! Every function below takes a [`Node`](crate::lossless::project::Node) — a green node
//! plus where it starts — and never a rowan cursor. See
//! [the substrate](crate::lossless::project#what-a-projection-walks-the-green-tree-not-a-cursor)
//! for why: a cursor's parent pointer is what the caller's own frame already is, and its offset
//! is what [`Node::children`](crate::lossless::project::Node::children) accumulates, so
//! the allocation per element it costs buys nothing here. The two whole-tree checks a door makes
//! were already green; now the walk between them is too, and a fail-fast projection allocates for
//! the AST it builds and for nothing else.
//!
//! Every span is the **token extent** of the constituents it covers — never the node's own
//! range, which includes committed trivia. See [`crate::lossless::project`] for that
//! rule and the measurement behind it.
//!
//! It is folded **bottom-up, once**. Each node function walks its own
//! [`children`](crate::lossless::project::Node::children) a single time: it covers the
//! ranges of its non-trivia tokens,
//! dispatches its child nodes into slots by kind, and covers the extent each projected child
//! hands back beside its AST value — which is why so many of them answer
//! `(value, TextRange)`. Nothing re-descends a subtree an ancestor has already walked. A child
//! the projection has no place for is the one exception: its bytes still belong to the parent's
//! span, so [`node_extent`] reads them, and that is the only call site left for it.
//!
//! Two places the tree's geometry and the AST's differ, and both are span-relevant:
//!
//! - **Descriptions hoist.** The CST hangs a [`Description`](SyntaxKind::Description) node
//!   *inside* the definition it precedes; the AST lifts it into the [`Described`] wrapper. At
//!   document level the wrapper spans description-through-definition while the inner definition
//!   starts *after* the description, so the inner span is synthesised by folding the node's
//!   constituents with the description excluded.
//! - **…except in three node types**, where the syntactic parser gives the wrapper and the inner
//!   node the *same* span, description included: `FieldDefinition`, `InputValueDefinition` and
//!   `EnumValueDefinition`. `VariableDefinition` — the fourth described node below document
//!   level — follows the document-level rule instead, so the four do not agree with each other.
//!   That asymmetry is trunk's, not this module's, and it is reproduced rather than corrected:
//!   `tests/lossless_project.rs` compares against the parser, so a "fix" here would be a
//!   divergence.

use std::vec::Vec;

use rowan::{NodeOrToken, TextRange, TextSize};
use tokora::SimpleSpan;

use smear_lexer::{
  LitStr,
  graphql::{ContextualKeyword, keyword::contextual_keyword},
  keywords::{Mutation, Query, Subscription},
};

use crate::{
  graphql::{
    ast::{
      Alias, Argument, Arguments, BooleanValue, ConstArgument, ConstArguments, ConstDirective,
      ConstDirectives, ConstInputValue, ConstList, ConstObject, ConstObjectField,
      DefaultInputValue, DefinitionOrExtension, Described, DescribedExecutableDefinition,
      DescribedVariableDefinition, Directive, Directives, Document, EnumTypeDefinition,
      EnumTypeExtension, EnumValue, EnumValuesDefinition, ExecutableDefinition, ExecutableDocument,
      Field, FieldsDefinition, FloatValue, FragmentName, FragmentSpread, ImplementInterfaces,
      InlineFragment, InputFieldsDefinition, InputObjectTypeDefinition, InputObjectTypeExtension,
      InputValue, IntValue, InterfaceTypeDefinition, InterfaceTypeExtension, List, ListType,
      Location, Name, NamedOperationDefinition, NamedType, NullValue, Object, ObjectField,
      ObjectTypeDefinition, ObjectTypeExtension, OperationDefinition, OperationType,
      RootOperationTypeDefinition, RootOperationTypesDefinition, ScalarTypeDefinition,
      ScalarTypeExtension, SchemaDefinition, SchemaExtension, Selection, SelectionSet, StringValue,
      Type, TypeCondition, TypeDefinition, TypeExtension, TypeSystemDefinition,
      TypeSystemDefinitionOrExtension, TypeSystemDocument, TypeSystemExtension, UnionMemberTypes,
      UnionTypeDefinition, UnionTypeExtension, VariableDefinition, VariableValue,
      VariablesDefinition,
    },
    kinds::{GraphQLLang, SyntaxKind},
    lossless::Parse,
    syntactic::definition::classify_location,
  },
  lossless::project::{
    Recovery, Unverified, node_extent, reject_holes, to_range, to_span, verify_source,
    verify_source_at, verify_source_counted,
  },
};

// Spelled out rather than folded into the group above, so `tests/lossless_isolation.rs`'s source
// census — which reads `crate::…` prefixes out of the text — can see the edge. These are the
// **shared, dialect-free** AST carriers: the undescribed cores three `Described<…>` aliases wrap,
// and the six `…Data` enums an extension's alternatives are encoded in. Neither has a spelling
// under `graphql::ast`, and a projection has to construct both.
use crate::type_system::{
  ArgumentsDefinition, DirectiveDefinition, DirectiveLocations, EnumTypeExtensionData,
  EnumValueDefinition, FieldDefinition, InputObjectTypeExtensionData, InputValueDefinition,
  InterfaceTypeExtensionData, ObjectTypeExtensionData, SchemaExtensionData, UnionTypeExtensionData,
};

use SyntaxKind as K;

/// A refusal from the GraphQL projection, keyed by this dialect's [`SyntaxKind`].
pub type ProjectError = crate::lossless::project::ProjectError<SyntaxKind>;

/// Why the GraphQL projection refused, keyed by this dialect's [`SyntaxKind`].
pub type ProjectErrorKind = crate::lossless::project::ProjectErrorKind<SyntaxKind>;

/// A green node and where it starts, in this dialect's kind space.
///
/// The unit every function below walks. See
/// [the substrate](crate::lossless::project#what-a-projection-walks-the-green-tree-not-a-cursor)
/// for why the traversal is green and what a cursor would have cost.
type Node<'g> = crate::lossless::project::Node<'g, GraphQLLang>;

/// [`Node`]'s other half.
type Token<'g> = crate::lossless::project::Token<'g, GraphQLLang>;

type Out<T> = Result<T, ProjectError>;

impl Unverified {
  /// The reason a [`ProjectError`] from a whole-root verification names.
  ///
  /// The two are established by one walk and were collapsed into one name at this boundary — the
  /// third time on al8n/smear#198 that two abandonments with different remedies met a channel that
  /// could carry one. The others were an arena refusal wearing the budget's `None` and a stale pair
  /// wearing the budget's refusal; this one is a shape wearing a mismatch.
  ///
  /// # Why the constructor is here and the type is in the substrate
  ///
  /// [`Unverified`] is what a dialect-free verification answers, so it belongs to the substrate.
  /// *Building* one out of a [`ProjectError`] is something only a projection door does, and the
  /// only doors that exist are this module's two — [`Verified::new`] and
  /// [`recovered_top_level`]. A `pub(crate)` item in the substrate with no in-crate caller is
  /// `dead_code` under `-Dwarnings` whenever the crate is built with no dialect at all, which is
  /// what `lossless-coverage` and `cargo hack --each-feature` do.
  ///
  /// The first repair for that was a `#[cfg(feature = "graphql")]` on the item where it stood,
  /// and it was the wrong one: `tests/lossless_isolation.rs` pins every dialect-facing gate in the
  /// substrate at `any(feature = "graphql", feature = "graphqlx")` precisely so that a generic
  /// layer cannot acquire a favourite dialect, and the narrower gate is that drift by definition.
  /// The `dead_code` denial was not noise to silence — it was the substrate reporting that the
  /// item had no business living there. Moving it to the dialect that reads it discharges both:
  /// the gate comes from `pub mod graphql`, which already carries one, and the substrate goes back
  /// to naming no dialect at all.
  ///
  /// A GraphQLx projection door, when there is one, writes its own — over its own `SyntaxKind`,
  /// with its own `ProjectErrorKind`. That is a second three-line `match` rather than a shared
  /// generic one, and it is the right trade: the alternative puts a `pub(crate)` item back in the
  /// substrate whose liveness depends on which dialects are compiled.
  pub(crate) fn of(error: &ProjectError) -> Self {
    match error.kind() {
      ProjectErrorKind::TooDeep { limit } => Self::TooDeep { limit: *limit },
      _ => Self::SourceMismatch,
    }
  }
}

/// Project a lossless parse to the AST the syntactic parser produces for `source`.
///
/// The root this reads is the mixed [`Document`](SyntaxKind::Document) that
/// [`parse_document`](super::parse_document) builds.
///
/// See the module header for the contract; the short version is: check
/// [`has_errors`](Parse::has_errors) first, pass the same text the tree was parsed from, and
/// expect a refusal rather than a guess when the tree carries a hole.
pub fn project<'src>(parse: &Parse, source: &'src str) -> Out<Document<&'src str>> {
  let root = parse_root(parse);
  open(root, source)?;
  let node = child_node(root, K::Document).ok_or_else(|| missing(root, "a document"))?;
  document(node, source)
}

impl super::ast::Document {
  /// Project this document node to the AST the syntactic parser produces for `source`.
  ///
  /// The compositional form of [`project`], for a caller that already holds the typed wrapper.
  /// Unlike [`project`] it does **not** scan for recovery holes up front — a hole inside the
  /// subtree still refuses when the walk reaches it, but a hole elsewhere in the parse is not
  /// this node's business.
  pub fn to_ast<'src>(&self, source: &'src str) -> Out<Document<&'src str>> {
    let node = Node::of(self.syntax());
    open_node(node, source)?;
    document(node, source)
  }
}

/// Project a lossless **executable** parse to the AST the syntactic parser produces for `source`.
///
/// [`project`]'s root swapped: this reads the
/// [`ExecutableDocument`](SyntaxKind::ExecutableDocument) that
/// [`parse_executable_document`](super::parse_executable_document) builds, and answers the
/// `ExecutableDocument<&str>` that `syntactic::executable_document` answers for the same bytes.
/// Everything else — the hole scan, the verified `(tree, source)` pair, the token-extent span
/// rule — is [`project`]'s, unchanged.
///
/// The root matters. A mixed parse holds a [`Document`](SyntaxKind::Document) node, so it is
/// refused here rather than filtered: dropping the type-system half of a mixed document would
/// answer a different question from the one the executable root asks, and the executable root is
/// the one that reports an SDL definition *at the parser's own position*.
///
/// ```
/// # #[cfg(all(feature = "graphql", feature = "rowan"))] {
/// use smear_parser::graphql::lossless::{parse_executable_document, project_executable_document};
///
/// let source = "query Q { hero { name } }";
/// let parse = parse_executable_document(source);
/// assert!(!parse.has_errors());
///
/// let ast = project_executable_document(&parse, source).expect("a well-shaped tree projects");
/// assert_eq!(ast.definitions().len(), 1);
/// # }
/// ```
pub fn project_executable_document<'src>(
  parse: &Parse,
  source: &'src str,
) -> Out<ExecutableDocument<&'src str>> {
  let root = parse_root(parse);
  open(root, source)?;
  let node = executable_root(root).ok_or_else(|| missing(root, "an executable document"))?;
  executable_document(node, source)
}

/// Project every definition of a lossless **executable** parse that has an AST image, and count
/// the ones that do not.
///
/// [`project_executable_document`] is fail-fast: one hole anywhere and the whole document is
/// refused. That is the right answer for a caller that wants the AST or nothing, and the wrong
/// one for an editor — a lossless CST exists precisely so it can represent a document somebody is
/// still typing, and "no AST, no answer" is the outcome that makes the lossless leg pointless.
///
/// This door walks the top level instead, projects each definition **independently**, and keeps
/// the ones that succeeded. What it could see is the [`Recovery`], and that value is the
/// contract: read it before reading anything off the AST, because a document one definition was
/// dropped from can both hide a finding and invent one. [`Recovery`]'s own documentation states
/// which, and why neither can be corrected here.
///
/// # What counts as a top-level element
///
/// Every child of the tree's [`Root`](SyntaxKind::Root), with the
/// [`ExecutableDocument`](SyntaxKind::ExecutableDocument) node stepped *through* rather than
/// descended into as the whole population. Both halves of that are load-bearing. The lost-node
/// recovery class drops a failed document production's children straight under the root, so
/// `"{ a }\nquery Bad("` has no document node at all and its one good operation is reachable only
/// from the root; and a gap tile can land beside an *empty* document node, so `"%"` has one
/// top-level element and it is not the document node's child. Reading only the document node's
/// children answered a complete [`Recovery`] for that second shape.
///
/// ```
/// # #[cfg(all(feature = "graphql", feature = "rowan"))] {
/// use smear_parser::graphql::lossless::{
///   parse_executable_document, project_executable_document_recovered,
/// };
///
/// // The second operation is half-typed; the first one is not.
/// let source = "{ hero { name } }\nquery Bad(";
/// let parse = parse_executable_document(source);
/// assert!(parse.has_errors());
///
/// let (ast, recovery) =
///   project_executable_document_recovered(&parse, source).expect("one document");
/// assert_eq!(ast.definitions().len(), 1);
/// assert_eq!(recovery.projected(), 1);
/// assert!(!recovery.is_complete());
///
/// // A `source` the parse does not describe is refused rather than projected — including one that
/// // merely *extends* the parse's text, where every definition would have matched at its own
/// // range and the recovery would have read as complete.
/// let longer = format!("{source} query More {{ hero {{ name }} }}");
/// assert!(project_executable_document_recovered(&parse, &longer).is_err());
/// # }
/// ```
pub fn project_executable_document_recovered<'src>(
  parse: &Parse,
  source: &'src str,
) -> Result<(ExecutableDocument<&'src str>, Recovery), Unverified> {
  let (span, definitions, recovery) =
    recovered_top_level(parse, K::ExecutableDocument, recoverable_entry, source)?;
  Ok((ExecutableDocument::new(span, definitions), recovery))
}

/// [`project_executable_document_recovered`] for a pair that already carries its verification.
///
/// Infallible: a [`Verified`] is the proof the fallible form's error half exists to report, so
/// there is no error half left. See [`Verified`] for why the type exists rather than the check
/// simply being moved.
pub fn project_executable_document_verified<'src>(
  pair: Verified<'_, 'src>,
) -> (ExecutableDocument<&'src str>, Recovery) {
  let (span, definitions, recovery) = recovered_top_level_verified(
    pair.parse(),
    K::ExecutableDocument,
    recoverable_entry,
    pair.source(),
  );
  (ExecutableDocument::new(span, definitions), recovery)
}

impl super::ast::ExecutableDocument {
  /// Project this executable-document node to the AST the syntactic parser produces for `source`.
  ///
  /// The compositional form of [`project_executable_document`], and
  /// [`Document::to_ast`](super::ast::Document::to_ast)'s twin: like it, and unlike the free
  /// function, it does **not** scan the whole parse for recovery holes up front.
  pub fn to_ast<'src>(&self, source: &'src str) -> Out<ExecutableDocument<&'src str>> {
    let node = Node::of(self.syntax());
    open_node(node, source)?;
    executable_document(node, source)
  }
}

/// Project a lossless **type-system** parse to the AST the syntactic parser produces for `source`.
///
/// [`project`]'s root swapped the other way: this reads the
/// [`TypeSystemDocument`](SyntaxKind::TypeSystemDocument) that
/// [`parse_type_system_document`](super::parse_type_system_document) builds, and answers the
/// `TypeSystemDocument<&str>` that `syntactic::type_system_document` answers for the same bytes —
/// which is the document [`Schema::build`] consumes. Everything else — the hole scan, the verified
/// `(tree, source)` pair, the token-extent span rule — is [`project`]'s, unchanged.
///
/// The root matters, for [`project_executable_document`]'s reason mirrored: a mixed parse holds a
/// [`Document`](SyntaxKind::Document) node and is refused here rather than filtered, because
/// dropping the executable half of a mixed document would answer a different question from the one
/// the SDL root asks, and the SDL root is the one that reports an operation *at the parser's own
/// position*.
///
/// ```
/// # #[cfg(all(feature = "graphql", feature = "rowan"))] {
/// use smear_parser::graphql::lossless::{
///   parse_type_system_document, project_type_system_document,
/// };
///
/// let source = "type Query { hero: String }";
/// let parse = parse_type_system_document(source);
/// assert!(!parse.has_errors());
///
/// let ast = project_type_system_document(&parse, source).expect("a well-shaped tree projects");
/// assert_eq!(ast.definitions().len(), 1);
/// # }
/// ```
///
/// [`Schema::build`]: https://docs.rs/smear/latest/smear/validator/struct.Schema.html
pub fn project_type_system_document<'src>(
  parse: &Parse,
  source: &'src str,
) -> Out<TypeSystemDocument<&'src str>> {
  let root = parse_root(parse);
  open(root, source)?;
  let node = type_system_root(root).ok_or_else(|| missing(root, "a type system document"))?;
  type_system_document(node, source)
}

/// Project every definition of a lossless **type-system** parse that has an AST image, and count
/// the ones that do not.
///
/// [`project_executable_document_recovered`]'s mirror at the SDL root, walking the same top level
/// with the same accounting — see it for why a fail-fast projection is the wrong answer for an
/// editor and why the [`Recovery`] is part of the contract rather than decoration.
///
/// What differs is only what a dropped definition costs. Draft §3 is not resolved per definition:
/// a type this document defines is what every reference to it elsewhere resolves against, so
/// dropping one turns every mention of it into an undefined-type refusal, and dropping the one
/// that happened to be `Query` turns the whole document into a schema with no query root. A
/// [`Recovery`] with [`is_complete`](Recovery::is_complete) false therefore says rather more here
/// than at the executable root, and the direction is the same: the refusals may be artifacts of
/// what was skipped, and a *clean* build over a partial projection is a schema missing whatever
/// the author was mid-way through typing.
///
/// ```
/// # #[cfg(all(feature = "graphql", feature = "rowan"))] {
/// use smear_parser::graphql::lossless::{
///   parse_type_system_document, project_type_system_document_recovered,
/// };
///
/// // The second type's field has no type yet; the first one is finished.
/// let source = "type Query { hero: String }\ntype Half { f: }";
/// let parse = parse_type_system_document(source);
/// assert!(parse.has_errors());
///
/// let (ast, recovery) =
///   project_type_system_document_recovered(&parse, source).expect("one document");
/// assert_eq!(ast.definitions().len(), 1);
/// assert_eq!(recovery.projected(), 1);
/// assert!(!recovery.is_complete());
///
/// // And a `source` the parse does not describe is refused rather than projected.
/// let longer = format!("{source} type Extra {{ n: Int }}");
/// assert!(project_type_system_document_recovered(&parse, &longer).is_err());
/// # }
/// ```
pub fn project_type_system_document_recovered<'src>(
  parse: &Parse,
  source: &'src str,
) -> Result<(TypeSystemDocument<&'src str>, Recovery), Unverified> {
  let (span, definitions, recovery) = recovered_top_level(
    parse,
    K::TypeSystemDocument,
    recoverable_type_system_entry,
    source,
  )?;
  Ok((TypeSystemDocument::new(span, definitions), recovery))
}

/// [`project_type_system_document_recovered`] for a pair that already carries its verification.
///
/// [`project_executable_document_verified`]'s mirror at the SDL root.
pub fn project_type_system_document_verified<'src>(
  pair: Verified<'_, 'src>,
) -> (TypeSystemDocument<&'src str>, Recovery) {
  let (span, definitions, recovery) = recovered_top_level_verified(
    pair.parse(),
    K::TypeSystemDocument,
    recoverable_type_system_entry,
    pair.source(),
  );
  (TypeSystemDocument::new(span, definitions), recovery)
}

impl super::ast::TypeSystemDocument {
  /// Project this type-system-document node to the AST the syntactic parser produces for `source`.
  ///
  /// The compositional form of [`project_type_system_document`], and
  /// [`ExecutableDocument::to_ast`](super::ast::ExecutableDocument::to_ast)'s twin: like it, and
  /// unlike the free function, it does **not** scan the whole parse for recovery holes up front.
  pub fn to_ast<'src>(&self, source: &'src str) -> Out<TypeSystemDocument<&'src str>> {
    let node = Node::of(self.syntax());
    open_node(node, source)?;
    type_system_document(node, source)
  }
}

/// A parse and the source it was produced from, **verified once**.
///
/// # Why this is a type
///
/// Two properties a lossless door is asked for, which cannot both hold when the door is handed an
/// unverified pair:
///
/// - a **source mismatch outranks a budget refusal**, because a stale pair is not a resource
///   problem and telling a caller to raise a limit names a remedy that cannot work; and
/// - the **ceiling is absolute**, because no input-linear work may run outside the ledger.
///
/// Deciding the first requires *finishing* the comparison; honouring the second requires being able
/// to stop before finishing it. Whichever runs first, the other loses — and that is a property of
/// the arguments, not of the ordering, so no rearrangement inside such a door satisfies both.
///
/// Moving the verification out of the bounded call is the only shape that does. A `Verified` is
/// checked by whoever constructs it, once, and every door that takes one has nothing left to
/// verify: it opens its ledger first and everything it then does is under it.
/// [`project_executable_document_verified`] and its twin are infallible for the same reason — the
/// only thing their fallible forms can answer with is the check this value already carries.
/// al8n/smear#198.
#[derive(Clone, Copy)]
pub struct Verified<'p, 'src> {
  parse: &'p Parse,
  source: &'src str,
  /// What projecting this pair costs, in elements — see [`Verified::projection_cost`].
  elements: u32,
}

impl core::fmt::Debug for Verified<'_, '_> {
  /// The source, and that the pair is verified. `Parse` is not `Debug` — a green tree has no useful
  /// rendering — so the half that can be shown is.
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("Verified")
      .field("source", &self.source)
      .finish_non_exhaustive()
  }
}

impl<'p, 'src> Verified<'p, 'src> {
  /// Verifies that `source` is the whole text `parse` was produced from.
  ///
  /// `O(tokens)` over the borrowed green root and allocation-free — see [`matches_source`], which
  /// is the same comparison. This is where a caller pays for it, and paying for it here is what
  /// lets a validation of this pair be bounded.
  pub fn new(parse: &'p Parse, source: &'src str) -> Result<Self, Unverified> {
    // Counted by the same walk that verifies, so the proof and the price are established together
    // and cost one pass between them. See [`Verified::projection_cost`].
    match verify_source_counted::<SyntaxKind>(parse.green(), source) {
      Ok(elements) => Ok(Self {
        parse,
        source,
        elements,
      }),
      // The counted walk distinguishes a byte divergence from a shape refusal; this used to answer
      // one name for both, so a pair whose bytes agree exactly was reported as stale.
      Err(refusal) => Err(Unverified::of(&refusal)),
    }
  }

  /// What projecting this pair costs, in **elements** — one per green node and one per token.
  ///
  /// # A proof that does not bound what its consumer charges for is not a proof
  ///
  /// `Verified` proves the *bytes* agree. A door that then prices the projection from
  /// `source.len()` is assuming bytes bound structure, and they do not:
  /// [`finish_root`](crate::lossless::runner::finish_root) is public, so a caller can mint a
  /// `Parse` from its own CST event stream, and a balanced pair of **zero-width** GraphQL nodes adds
  /// structure without adding a byte. An empty source over a tree of a million empty top-level
  /// nodes verifies against `""`, paid one unit, and then had every node visited.
  ///
  /// So the value carries the cost of the thing it proves, and the two measure the same quantity.
  /// Saturating at [`u32::MAX`] — no budget any caller can name pays that, so a tree too large to
  /// price refuses rather than wrapping into one it fits.
  /// al8n/smear#198.
  #[inline]
  pub const fn projection_cost(&self) -> u32 {
    self.elements
  }

  /// The parse half of the pair.
  #[inline]
  pub const fn parse(&self) -> &'p Parse {
    self.parse
  }

  /// The source half of the pair.
  #[inline]
  pub const fn source(&self) -> &'src str {
    self.source
  }
}

/// Whether `parse` and `source` describe the same bytes, over the **whole root**.
///
/// The recovering projection's precondition, and the one thing it cannot establish definition by
/// definition. Each definition it projects is verified against `source` at that definition's own
/// range, which refuses a pair whose bytes differ — and says nothing at all about a `source` that
/// *begins* with the parse's text and then adds to it. There every definition matches, nothing is
/// skipped, the [`Recovery`] reports complete, and whatever the caller appended is silently absent
/// from the AST. A consumer that validated or built from that result would be answering about a
/// prefix while believing it had the document.
///
/// [`verify_source`] over the parse's **green** root, which is the same comparison the fail-fast
/// doors make and the reason they were safe. It is `O(tokens)`, it reads no `Parse` state beyond a
/// borrow, and it allocates nothing.
///
/// This was `parse.syntax().text() == source` for one round, and that sentence was written about it
/// too — wrongly. [`Parse::syntax`](crate::lossless::runner::Parse::syntax) *materialises rowan's red
/// cursor*: it clones the `Arc` and allocates the root's cursor data, and `SyntaxText`'s equality
/// then walks descendants through red nodes and tokens that are allocated and dropped as the walk
/// passes them. A token-dense parse therefore made `Θ(elements)` transient allocations here, on
/// every recovered projection and every lossless validation, and this helper is public and holds no
/// budget. al8n/smear#198's nineteenth round.
///
/// The failure is worth recording where the mechanism is, because the fix is a function the crate
/// already had and the round that introduced the defect is the round that **named** it: the
/// fourteenth round's own diagnosis said the fail-fast doors never had the prefix defect precisely
/// because they open with [`verify_source`] over the whole green root — and then wrote a second
/// whole-root check beside it instead of calling that one.
///
/// The fail-fast doors never needed it: [`project_executable_document`] and its SDL twin open with
/// [`verify_source`] over the whole green root, and that check compares *lengths* first, so an
/// extended source is refused before a byte is walked. The recovering door was written from the
/// **compositional** check instead — [`verify_source_at`], run once per definition — and that is
/// the check with nothing to say about bytes no definition covers.
///
/// It is public because the check belongs to whoever holds the pair, and a caller may want the
/// answer without projecting. Nothing has to call it to be safe: the recovering projections make
/// the check themselves and answer [`Unverified`], so a consumer using them **directly** —
/// with no validator and no door in front — cannot be handed a stale prefix either. al8n/smear#198.
pub fn matches_source(parse: &Parse, source: &str) -> bool {
  verify_source::<SyntaxKind>(parse.green(), source).is_ok()
}

/// The recovering top-level walk, shared by both single-half roots.
///
/// One implementation rather than one per root, because what it computes is [`Recovery`], and
/// `Recovery`'s documented meaning — *`skipped` is a bound on what was lost, counted per element*
/// — has to be one statement about both doors rather than two statements that happen to agree
/// today. The root's kind and the per-definition projection are the only things that differ, so
/// they are the arguments; `entry_of` is a `fn` pointer rather than a generic parameter so this
/// monomorphises once per root and nothing here becomes a shape a caller can dispatch through.
///
/// # Every element of the root, not every element of the document node
///
/// The walk starts at the **root** and steps *through* the document node rather than starting
/// inside it. The two are not the same population: the parser can leave a gap tile beside the
/// document node instead of within it, and `"%"` parses to exactly that — `ExecutableDocument@0..0`
/// with `Gap@0..1` as its sibling. Iterating the document node's children saw nothing, counted
/// nothing, and answered a complete [`Recovery`] over a document whose every byte had no AST
/// image. Which is [`Unverified`]'s defect one level down: state derived from a population
/// that can be empty while the thing it describes is not.
///
/// Answers the surviving definitions, their span, and the tally — or [`Unverified`],
/// which is none of those three and so is not spelled as a value of any of them.
fn recovered_top_level<'src, T>(
  parse: &Parse,
  root_kind: SyntaxKind,
  entry_of: fn(Node<'_>, &'src str) -> Out<(T, TextRange)>,
  source: &'src str,
) -> Result<(SimpleSpan, Vec<T>, Recovery), Unverified> {
  // Established once, over the whole root, before a single element is projected, and **returned**
  // rather than folded into the tally. See [`matches_source`] for why a per-definition check
  // cannot see a prefix, and [`Unverified`] for why the answer is not a [`Recovery`]: a
  // mismatched pair used to project nothing and count every top-level element as skipped, which is
  // a true statement about a parse that *has* elements and an empty one about a parse that does
  // not — and `Recovery::is_complete` answers `true` at zero skipped.
  // `verify_source` rather than `matches_source`: the two reasons a pair can be refused are
  // established by one walk, and the predicate's `bool` is where the distinction used to be thrown
  // away. al8n/smear#198.
  if let Err(refusal) = verify_source::<SyntaxKind>(parse.green(), source) {
    return Err(Unverified::of(&refusal));
  }
  Ok(recovered_top_level_verified(
    parse, root_kind, entry_of, source,
  ))
}

/// [`recovered_top_level`] for a pair whose verification is already established.
///
/// Infallible, because the only thing the fallible form can answer with is the check a
/// [`Verified`] already carries. Splitting it is what lets a **bounded** caller exist at all: the
/// check is `O(tokens)`, so a door that verifies inside its own call has an input-linear walk in
/// front of its ledger, and one that takes a verified pair does not. al8n/smear#198.
fn recovered_top_level_verified<'src, T>(
  parse: &Parse,
  root_kind: SyntaxKind,
  entry_of: fn(Node<'_>, &'src str) -> Out<(T, TextRange)>,
  source: &'src str,
) -> (SimpleSpan, Vec<T>, Recovery) {
  let root = parse_root(parse);
  let container = child_node(root, root_kind).unwrap_or(root);

  let mut definitions = Vec::new();
  let mut skipped = 0u32;
  // The document's own span is the extent of the tokens under the definitions that **survived**,
  // not of the bytes that were dropped: an AST span is an extent of the tokens its node covers,
  // and a skipped region is not one of them.
  let mut extent = Extent::default();
  let mut take = |element: NodeOrToken<Node<'_>, Token<'_>>| match element {
    // Rubble the parser could not attach to a definition. Counted per token rather than per run:
    // a bound on what was lost, which is what `Recovery::skipped` promises.
    NodeOrToken::Token(token) if !is_trivia(token.kind()) => skipped = skipped.saturating_add(1),
    NodeOrToken::Token(_) => {}
    NodeOrToken::Node(child) => match entry_of(child, source) {
      Ok((entry, piece)) => {
        extent.cover(piece);
        definitions.push(entry);
      }
      Err(_) => skipped = skipped.saturating_add(1),
    },
  };
  for element in root.children() {
    match element {
      // The document node is stepped *through*: its children are the definitions. Everything else
      // under the root is a top-level element in its own right — the lost-node class puts a failed
      // production's children there, and the lexer's gap tiles land there when the document node
      // came out empty.
      NodeOrToken::Node(child) if child.kind() == root_kind => child.children().for_each(&mut take),
      other => take(other),
    }
  }

  // With nothing projected there is no extent, and the zero-width span at the container's start
  // is the only position that is not a claim about text no node holds.
  let span = match extent.get() {
    Some(range) => to_span(range),
    None => {
      let start = usize::from(container.text_range().start());
      SimpleSpan::new(start, start)
    }
  };
  let recovery = Recovery::new(definitions.len() as u32, skipped);
  (span, definitions, recovery)
}

/// The [`ExecutableDocument`](SyntaxKind::ExecutableDocument) node under a parse's root.
fn executable_root(root: Node<'_>) -> Option<Node<'_>> {
  child_node(root, K::ExecutableDocument)
}

/// The [`TypeSystemDocument`](SyntaxKind::TypeSystemDocument) node under a parse's root.
fn type_system_root(root: Node<'_>) -> Option<Node<'_>> {
  child_node(root, K::TypeSystemDocument)
}

/// One top-level definition, with the holes in **its own** subtree refused.
///
/// The scan is scoped rather than global on purpose. [`project_executable_document`] refuses a
/// tree carrying a hole anywhere, because a hole is a region with no AST image and a fail-fast
/// door must not silently omit one; the recovering door makes the same refusal, one definition at
/// a time, so a hole is charged to the definition that holds it and to no other. Without the
/// scan a hole would instead be *skipped* by whichever `child(node, kind)` lookup walked past it,
/// which is the data loss under a success type that both doors exist to refuse.
/// The `(tree, source)` pair is checked here too, for the reason the hole scan is: the recovering
/// door has no error channel of its own, so a definition whose bytes are not the caller's bytes
/// has to fail *as that definition* to be counted as skipped rather than silently projected. The
/// scope is the same one the hole scan uses, which keeps the two refusals attributed alike.
fn recoverable_entry<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(DescribedExecutableDefinition<&'src str>, TextRange)> {
  open_node(node, source)?;
  scan_holes(node)?;
  executable_entry(node, source)
}

/// [`recoverable_entry`]'s twin at the SDL root, scoped for the same reason.
fn recoverable_type_system_entry<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(TypeSystemDefinitionOrExtension<&'src str>, TextRange)> {
  open_node(node, source)?;
  scan_holes(node)?;
  type_system_entry(node, source)
}

// ---------------------------------------------------------------------------------------------
// the doors' two whole-tree checks
// ---------------------------------------------------------------------------------------------

/// Open a fail-fast door: the `(tree, source)` pair verified whole, then every recovery hole
/// refused.
///
/// Both checks read the **green** tree, so together they cost two passes over the file's bytes
/// and not a single cursor allocation. The pair is checked first because nothing else the door
/// could report means anything if the tree is not this text's: a hole's range, a missing
/// constituent's span and every slice below are all statements about `source`.
fn open(root: Node<'_>, source: &str) -> Out<()> {
  verify_source(root.green(), source)?;
  scan_holes(root)
}

/// [`open`]'s subtree form: `node`'s own text is checked where `node` sits.
///
/// The compositional doors' check, and the recovering door's per-definition one. No hole scan —
/// [`Document::to_ast`](super::ast::Document::to_ast) and its twins deliberately do not make one,
/// and the recovering door makes its own so it can charge the hole to a definition.
fn open_node(node: Node<'_>, source: &str) -> Out<()> {
  verify_source_at(node.green(), source, usize::from(node.start()))
}

/// Refuse a tree that carries any recovery hole or gap tile.
///
/// Scanned over the whole subtree rather than per walked node, so the answer does not depend on
/// which nodes a particular walk happens to descend into: a hole anywhere is a region of the
/// document with no AST image, and a projection that silently omitted it would be losing data
/// under a success type. At a fail-fast door the subtree is the **root's**, which is what reaches
/// a hole the parser left beside the document node rather than inside it — see
/// [`reject_holes`].
fn scan_holes(node: Node<'_>) -> Out<()> {
  // `Gap` is a **token** kind, so `reject_holes` — which tests node kinds — can never match it and
  // the arm is dead as written. It stays because what it names is the refusal's scope rather than
  // a live branch: were the parser ever to tile a gap as a node, dropping the arm is what would
  // let it through, and nothing else records that a gap has no AST image.
  reject_holes(node, |kind| matches!(kind, K::Error | K::Gap))
}

/// A parse's green root, as the walk's first node.
fn parse_root(parse: &Parse) -> Node<'_> {
  Node::new(parse.green(), TextSize::new(0))
}

/// The first direct child of `parent` whose kind is `kind`.
fn child_node(parent: Node<'_>, kind: SyntaxKind) -> Option<Node<'_>> {
  parent.children().find_map(|child| match child {
    NodeOrToken::Node(child) if child.kind() == kind => Some(child),
    _ => None,
  })
}

fn missing(parent: Node<'_>, wanted: &'static str) -> ProjectError {
  ProjectError::new(
    ProjectErrorKind::MissingChild {
      parent: parent.kind(),
      wanted,
    },
    to_range(parent.text_range()),
  )
}

fn unexpected(parent: Node<'_>, found: SyntaxKind, at: TextRange) -> ProjectError {
  ProjectError::new(
    ProjectErrorKind::UnexpectedChild {
      parent: parent.kind(),
      found,
    },
    to_range(at),
  )
}

fn unexpected_node(parent: Node<'_>, found: Node<'_>) -> ProjectError {
  unexpected(parent, found.kind(), found.text_range())
}

// ---------------------------------------------------------------------------------------------
// the substrate this dialect binds
// ---------------------------------------------------------------------------------------------

/// The six ignorable token images. The tree keeps them; no AST span may contain one.
const fn is_trivia(kind: SyntaxKind) -> bool {
  matches!(
    kind,
    K::Space | K::Tab | K::Newline | K::Comma | K::Comment | K::Bom
  )
}

/// A node's token extent, folded as its children are dispatched.
///
/// One of these lives on the stack of every node function: the function covers each non-trivia
/// token it walks past and each extent a projected child hands back, and what is left at the end
/// is the node's span. `None` — no non-trivia token anywhere under the node — is a finding rather
/// than a fallback to the node's own range, which is why [`range`](Self::range) is fallible.
#[derive(Debug, Clone, Copy, Default)]
struct Extent {
  /// The cover of every non-trivia token folded in so far.
  range: Option<TextRange>,
  /// Set when [`Extent::unread`]'s descent hit
  /// [`MAX_GREEN_DEPTH`](crate::lossless::project::MAX_GREEN_DEPTH).
  ///
  /// The fold has no error channel — it is a `&mut self` accumulator threaded through
  /// thirty-five call sites — so the refusal is **carried** rather than returned, and
  /// [`Extent::range`] is where it becomes one. That reader already answers `Out<TextRange>`, so
  /// nothing above it changes shape; what it must not do is answer a range it cannot establish.
  /// al8n/smear#198.
  too_deep: bool,
}

impl Extent {
  /// Widen to include `piece`.
  ///
  /// `cover` rather than `start..piece.end()`: a fold that assumed document order would produce
  /// an inverted range the moment it was handed a stream that was not in it, and an inverted span
  /// is exactly the class `tests/support/span_extent.rs` exists to catch.
  #[inline]
  fn cover(&mut self, piece: TextRange) {
    self.range = Some(match self.range {
      Some(seen) => seen.cover(piece),
      None => piece,
    });
  }

  /// Widen to include `token`, unless it is trivia.
  #[inline]
  fn token(&mut self, token: Token<'_>) {
    if !is_trivia(token.kind()) {
      self.cover(token.text_range());
    }
  }

  /// Widen to include a child the projection does not read.
  ///
  /// The one place a subtree is still descended: an element with no AST image is not part of the
  /// AST, but its bytes are part of the parent's, and nothing else has walked it.
  #[inline]
  fn unread(&mut self, child: Node<'_>) {
    match node_extent(child, is_trivia) {
      Ok(Some(piece)) => self.cover(piece),
      Ok(None) => {}
      // The subtree is deeper than a walk will descend, so its extent is not knowable — and an
      // all-trivia subtree's honest answer is `None`, which makes a manufactured range a different
      // answer rather than a wider one. Carried to `Extent::range`.
      Err(_) => self.too_deep = true,
    }
  }

  /// Widen to include a projected child's extent, and keep the child.
  ///
  /// The bottom-up fold, spelled once: a child function answers its AST value beside the extent
  /// it folded, and the parent covers the second while binding the first.
  #[inline]
  fn keep<T>(&mut self, projected: (T, TextRange)) -> T {
    let (value, piece) = projected;
    self.cover(piece);
    value
  }

  /// [`keep`](Self::keep) for a constituent the grammar makes optional.
  #[inline]
  fn keep_opt<T>(&mut self, projected: Option<(T, TextRange)>) -> Option<T> {
    projected.map(|projected| self.keep(projected))
  }

  #[inline]
  const fn get(self) -> Option<TextRange> {
    self.range
  }

  #[inline]
  fn range(self, node: Node<'_>, wanted: &'static str) -> Out<TextRange> {
    if self.too_deep {
      return Err(ProjectError::new(
        ProjectErrorKind::TooDeep {
          limit: crate::lossless::project::MAX_GREEN_DEPTH,
        },
        to_range(node.text_range()),
      ));
    }
    self.range.ok_or_else(|| missing(node, wanted))
  }

  #[inline]
  fn span(self, node: Node<'_>, wanted: &'static str) -> Out<SimpleSpan> {
    self.range(node, wanted).map(to_span)
  }
}

/// The outer and inner extents of a described node, from the one fold.
///
/// `inner` is everything the node's fold covered *except* its description; `described` is the
/// description's own extent when the node carries one. The wrapper's span is their cover and the
/// definition's is `inner`, which is where the hoist shows up as a number — see the module
/// header, and note the three node types that give both the same span instead.
fn described_extents(
  node: Node<'_>,
  inner: Extent,
  described: Option<TextRange>,
) -> Out<(TextRange, TextRange)> {
  let inner = inner.range(
    node,
    match described {
      Some(_) => "a constituent other than its description",
      None => "a token",
    },
  )?;
  let outer = match described {
    Some(described) => inner.cover(described),
    None => inner,
  };
  Ok((outer, inner))
}

/// The first three direct `Name` tokens of a node, in document order.
///
/// Three because that is the deepest index the grammar reaches: an extension spells `extend`,
/// then the shape keyword, then the extended type's name. Direct children only — a name inside a
/// child node belongs to that child, and this is filled from the parent's own walk, so a
/// descendant can never reach it.
#[derive(Debug, Default)]
struct Names<'g>([Option<Token<'g>>; 3]);

impl<'g> Names<'g> {
  /// Record `token` if any of the three slots is still free.
  ///
  /// A fourth name is dropped rather than stored: no production reads one, and the extent it
  /// belongs to was covered when the walk passed it.
  #[inline]
  fn push(&mut self, token: Token<'g>) {
    if let Some(slot) = self.0.iter_mut().find(|slot| slot.is_none()) {
      *slot = Some(token);
    }
  }

  #[inline]
  fn get(&self, index: usize) -> Option<Token<'g>> {
    self.0[index]
  }

  #[inline]
  fn at(&self, index: usize, node: Node<'_>, wanted: &'static str) -> Out<Token<'g>> {
    self.get(index).ok_or_else(|| missing(node, wanted))
  }
}

/// The source text under `token`.
///
/// Bounds-checked, not compared: the door already verified every byte of the tree against
/// `source`, so a token's range is in bounds and on a character boundary by construction. The
/// refusal below is that invariant's receipt rather than a second check — see
/// [`verify_source`](crate::lossless::project::verify_source).
#[inline]
fn slice<'src>(source: &'src str, token: Token<'_>) -> Out<&'src str> {
  let range = token.text_range();
  source
    .get(usize::from(range.start())..usize::from(range.end()))
    .ok_or_else(|| ProjectError::new(ProjectErrorKind::SourceMismatch, to_range(range)))
}

fn name<'src>(source: &'src str, token: Token<'_>) -> Out<Name<&'src str>> {
  Ok(Name::new(
    to_span(token.text_range()),
    slice(source, token)?,
  ))
}

/// The `Name` of a node whose only content is one — `NamedType`, `EnumValue`, `Variable`.
///
/// Answers the name and the node's extent, which for these three is the name token's own range
/// unless the tree put something else under them.
fn inner_name<'src>(node: Node<'_>, source: &'src str) -> Out<(Name<&'src str>, TextRange)> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => extent.unread(child),
    }
  }
  let token = names.at(0, node, "a name")?;
  Ok((name(source, token)?, extent.range(node, "a token")?))
}

/// The keyword a `Name` token spells, classified through the lexer's own table.
fn keyword_of(token: Token<'_>) -> Option<ContextualKeyword> {
  contextual_keyword(token.text().as_bytes())
}

// ---------------------------------------------------------------------------------------------
// document
// ---------------------------------------------------------------------------------------------

fn document<'src>(node: Node<'_>, source: &'src str) -> Out<Document<&'src str>> {
  let mut extent = Extent::default();
  let mut definitions = Vec::new();
  for element in node.children() {
    match element {
      // Rubble: the lost-node recovery class drops a failed definition's bytes straight under
      // the document, where the AST has no place for them.
      NodeOrToken::Token(token) if !is_trivia(token.kind()) => {
        return Err(unexpected(node, token.kind(), token.text_range()));
      }
      NodeOrToken::Token(_) => {}
      NodeOrToken::Node(child) => {
        let (entry, piece) = document_entry(child, source)?;
        extent.cover(piece);
        definitions.push(entry);
      }
    }
  }
  Ok(Document::new(extent.span(node, "a token")?, definitions))
}

fn document_entry<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(DefinitionOrExtension<&'src str>, TextRange)> {
  if let Some((extension, extent)) = type_system_extension(node, source)? {
    return Ok((DefinitionOrExtension::Extension(extension), extent));
  }
  let (description, definition, outer) = definition(node, source)?;
  Ok((
    DefinitionOrExtension::Definition(Described::new(to_span(outer), description, definition)),
    outer,
  ))
}

/// [`document`]'s SDL-only twin, over the `TypeSystemDefinitionOrExtension+` root.
fn type_system_document<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<TypeSystemDocument<&'src str>> {
  let mut extent = Extent::default();
  let mut definitions = Vec::new();
  for element in node.children() {
    match element {
      // Rubble, exactly as at the mixed root and for the same reason.
      NodeOrToken::Token(token) if !is_trivia(token.kind()) => {
        return Err(unexpected(node, token.kind(), token.text_range()));
      }
      NodeOrToken::Token(_) => {}
      NodeOrToken::Node(child) => {
        let (entry, piece) = type_system_entry(child, source)?;
        extent.cover(piece);
        definitions.push(entry);
      }
    }
  }
  Ok(TypeSystemDocument::new(
    extent.span(node, "a token")?,
    definitions,
  ))
}

/// [`document_entry`]'s SDL-only twin.
///
/// The extension arm stays — `extend` is type-system syntax and this root is the one that builds
/// it — and what goes is the executable half: an `OperationDefinition` or a `FragmentDefinition`
/// under this root has no image in a `TypeSystemDocument`, and the SDL root reports one at the
/// parser's own position rather than shaping it, so reaching this arm means the tree is not the
/// one this door was handed.
fn type_system_entry<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(TypeSystemDefinitionOrExtension<&'src str>, TextRange)> {
  if let Some((extension, extent)) = type_system_extension(node, source)? {
    return Ok((
      TypeSystemDefinitionOrExtension::Extension(extension),
      extent,
    ));
  }
  let (description, definition, outer) = type_system_definition(node, source)?;
  Ok((
    TypeSystemDefinitionOrExtension::Definition(Described::new(
      to_span(outer),
      description,
      definition,
    )),
    outer,
  ))
}

/// [`document`]'s executable-only twin, over the `ExecutableDefinition+` root.
fn executable_document<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<ExecutableDocument<&'src str>> {
  let mut extent = Extent::default();
  let mut definitions = Vec::new();
  for element in node.children() {
    match element {
      // Rubble, exactly as at the mixed root and for the same reason.
      NodeOrToken::Token(token) if !is_trivia(token.kind()) => {
        return Err(unexpected(node, token.kind(), token.text_range()));
      }
      NodeOrToken::Token(_) => {}
      NodeOrToken::Node(child) => {
        let (entry, piece) = executable_entry(child, source)?;
        extent.cover(piece);
        definitions.push(entry);
      }
    }
  }
  Ok(ExecutableDocument::new(
    extent.span(node, "a token")?,
    definitions,
  ))
}

/// [`document_entry`]'s executable-only twin.
///
/// No extension arm — `extend` is not executable syntax and the root that produced this node
/// reports one rather than building it — and the description hoist is the document-level one: the
/// wrapper spans description-through-definition and the inner node starts after the description.
/// A standard executable definition carries no description at all, so on standard input the two
/// spans coincide and the hoist is the dialect-compatibility path only.
fn executable_entry<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(DescribedExecutableDefinition<&'src str>, TextRange)> {
  let (description, definition, outer) = executable_definition(node, source)?;
  Ok((
    Described::new(to_span(outer), description, definition),
    outer,
  ))
}

/// A described definition's three answers, folded from the node's single walk.
///
/// The hoisted description, the definition itself — whose own span is the inner extent, already
/// built into it — and the node's full token extent, which is both the `Described` wrapper's span
/// and what the enclosing document covers.
type Definition<'src, T> = (Option<StringValue<&'src str>>, T, TextRange);

fn executable_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, ExecutableDefinition<&'src str>>> {
  Ok(match node.kind() {
    K::OperationDefinition => {
      let (description, operation, outer) = operation_definition(node, source)?;
      (
        description,
        ExecutableDefinition::Operation(operation),
        outer,
      )
    }
    K::FragmentDefinition => {
      let (description, fragment, outer) = fragment_definition(node, source)?;
      (description, ExecutableDefinition::Fragment(fragment), outer)
    }
    found => return Err(unexpected(node, found, node.text_range())),
  })
}

fn definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, crate::graphql::ast::Definition<&'src str>>> {
  use crate::graphql::ast::Definition as D;

  // The two executable kinds, then the nine type-system ones through the shared arm. One list of
  // the nine, not two: `type_system_definition` is what the SDL root reaches them by, and a
  // second copy here would be nine chances for the mixed root and the SDL root to build different
  // ASTs out of the same node. Its refusal for an unknown kind is this one's, unchanged.
  Ok(match node.kind() {
    K::OperationDefinition => {
      let (description, operation, outer) = operation_definition(node, source)?;
      (
        description,
        D::Executable(ExecutableDefinition::Operation(operation)),
        outer,
      )
    }
    K::FragmentDefinition => {
      let (description, fragment, outer) = fragment_definition(node, source)?;
      (
        description,
        D::Executable(ExecutableDefinition::Fragment(fragment)),
        outer,
      )
    }
    _ => {
      let (description, definition, outer) = type_system_definition(node, source)?;
      (description, D::TypeSystem(definition), outer)
    }
  })
}

/// The nine type-system definition kinds, shared by the mixed root and the SDL-only one.
fn type_system_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, TypeSystemDefinition<&'src str>>> {
  Ok(match node.kind() {
    K::ScalarTypeDefinition => {
      let (description, definition, outer) = scalar_type_definition(node, source)?;
      (
        description,
        TypeSystemDefinition::Type(TypeDefinition::Scalar(definition)),
        outer,
      )
    }
    K::ObjectTypeDefinition => {
      let (description, definition, outer) = object_type_definition(node, source)?;
      (
        description,
        TypeSystemDefinition::Type(TypeDefinition::Object(definition)),
        outer,
      )
    }
    K::InterfaceTypeDefinition => {
      let (description, definition, outer) = interface_type_definition(node, source)?;
      (
        description,
        TypeSystemDefinition::Type(TypeDefinition::Interface(definition)),
        outer,
      )
    }
    K::UnionTypeDefinition => {
      let (description, definition, outer) = union_type_definition(node, source)?;
      (
        description,
        TypeSystemDefinition::Type(TypeDefinition::Union(definition)),
        outer,
      )
    }
    K::EnumTypeDefinition => {
      let (description, definition, outer) = enum_type_definition(node, source)?;
      (
        description,
        TypeSystemDefinition::Type(TypeDefinition::Enum(definition)),
        outer,
      )
    }
    K::InputObjectTypeDefinition => {
      let (description, definition, outer) = input_object_type_definition(node, source)?;
      (
        description,
        TypeSystemDefinition::Type(TypeDefinition::InputObject(definition)),
        outer,
      )
    }
    K::DirectiveDefinition => {
      let (description, definition, outer) = directive_definition(node, source)?;
      (
        description,
        TypeSystemDefinition::Directive(definition),
        outer,
      )
    }
    K::SchemaDefinition => {
      let (description, definition, outer) = schema_definition(node, source)?;
      (description, TypeSystemDefinition::Schema(definition), outer)
    }
    found => return Err(unexpected(node, found, node.text_range())),
  })
}

/// `Some` when `node` is one of the seven extension kinds, `None` when it is anything else.
///
/// An extension carries no description — `extend` heads its own production — so this answers
/// before the hoist rather than inside it, and its extent is the node's own with nothing lifted
/// out of it.
fn type_system_extension<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Option<(TypeSystemExtension<&'src str>, TextRange)>> {
  Ok(Some(match node.kind() {
    K::ScalarTypeExtension => {
      let (extension, extent) = scalar_type_extension(node, source)?;
      (
        TypeSystemExtension::Type(TypeExtension::Scalar(extension)),
        extent,
      )
    }
    K::ObjectTypeExtension => {
      let (extension, extent) = object_type_extension(node, source)?;
      (
        TypeSystemExtension::Type(TypeExtension::Object(extension)),
        extent,
      )
    }
    K::InterfaceTypeExtension => {
      let (extension, extent) = interface_type_extension(node, source)?;
      (
        TypeSystemExtension::Type(TypeExtension::Interface(extension)),
        extent,
      )
    }
    K::UnionTypeExtension => {
      let (extension, extent) = union_type_extension(node, source)?;
      (
        TypeSystemExtension::Type(TypeExtension::Union(extension)),
        extent,
      )
    }
    K::EnumTypeExtension => {
      let (extension, extent) = enum_type_extension(node, source)?;
      (
        TypeSystemExtension::Type(TypeExtension::Enum(extension)),
        extent,
      )
    }
    K::InputObjectTypeExtension => {
      let (extension, extent) = input_object_type_extension(node, source)?;
      (
        TypeSystemExtension::Type(TypeExtension::InputObject(extension)),
        extent,
      )
    }
    K::SchemaExtension => {
      let (extension, extent) = schema_extension(node, source)?;
      (TypeSystemExtension::Schema(extension), extent)
    }
    _ => return Ok(None),
  }))
}

// ---------------------------------------------------------------------------------------------
// descriptions
// ---------------------------------------------------------------------------------------------

/// The string a `Description` node holds, and the node's extent.
///
/// The extent is the parent's business rather than this node's: a description has no AST node of
/// its own — it is hoisted into a [`Described`] wrapper — so what the parent needs back is where
/// the description sat, which is the difference between the wrapper's span and the definition's.
fn description<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(StringValue<&'src str>, TextRange)> {
  let mut extent = Extent::default();
  let mut literal = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if literal.is_none() && matches!(token.kind(), K::String | K::BlockString) {
          literal = Some(token);
        }
      }
      NodeOrToken::Node(child) => extent.unread(child),
    }
  }
  let token = literal.ok_or_else(|| missing(node, "a string token"))?;
  Ok((string_value(token, source)?, extent.range(node, "a token")?))
}

/// Project the `Description` a node's walk collected, if it collected one.
///
/// Answers the hoisted string and its extent side by side, because
/// [`described_extents`] needs the second to tell the wrapper's span from the definition's.
fn hoisted_description<'src>(
  node: Option<Node<'_>>,
  source: &'src str,
) -> Out<(Option<StringValue<&'src str>>, Option<TextRange>)> {
  match node {
    Some(node) => {
      let (value, extent) = description(node, source)?;
      Ok((Some(value), Some(extent)))
    }
    None => Ok((None, None)),
  }
}

/// Re-cook a string literal through the **same** door the lexer's payload comes from.
///
/// [`LitStr`]'s `TryFrom<&str>` is the string lexer, so the `Plain`/`Complex` discriminant and
/// the `required_capacity` a consumer allocates against are the lexer's answers rather than a
/// second implementation of the escape rules. A refusal here means the two disagree about bytes
/// the lossless lexer already accepted, which is a lexer finding, not a projection one.
fn string_value<'src>(token: Token<'_>, source: &'src str) -> Out<StringValue<&'src str>> {
  let slice = slice(source, token)?;
  let lit = LitStr::try_from(slice).map_err(|_| {
    ProjectError::new(
      ProjectErrorKind::MalformedToken { kind: token.kind() },
      to_range(token.text_range()),
    )
  })?;
  Ok(StringValue::new(to_span(token.text_range()), lit))
}

// ---------------------------------------------------------------------------------------------
// executable definitions
// ---------------------------------------------------------------------------------------------

fn operation_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, OperationDefinition<&'src str>>> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  let mut description_node = None;
  let mut operation_type_node = None;
  let mut variables_node = None;
  let mut directives_node = None;
  let mut selection_set_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => match child.kind() {
        K::Description if description_node.is_none() => description_node = Some(child),
        K::OperationType if operation_type_node.is_none() => operation_type_node = Some(child),
        K::VariablesDefinition if variables_node.is_none() => variables_node = Some(child),
        K::Directives if directives_node.is_none() => directives_node = Some(child),
        K::SelectionSet if selection_set_node.is_none() => selection_set_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }

  let (description, described) = hoisted_description(description_node, source)?;
  let set = selection_set_node.ok_or_else(|| missing(node, "a selection set"))?;
  let selections = extent.keep(selection_set(set, source)?);

  let Some(keyword_node) = operation_type_node else {
    // Query shorthand: the definition *is* its selection set, and the AST's span for it is the
    // selection set's own — which is why this arm builds no span of its own. The grammar gives a
    // shorthand operation no other constituent, but the node's extent is still the whole of what
    // the tree put under it, so anything else there is folded rather than dropped.
    for child in [variables_node, directives_node].into_iter().flatten() {
      extent.unread(child);
    }
    let (outer, _) = described_extents(node, extent, described)?;
    return Ok((
      description,
      OperationDefinition::Shorthand(selections),
      outer,
    ));
  };

  let operation_type = extent.keep(operation_type(keyword_node)?);
  let name = match names.get(0) {
    Some(token) => Some(name(source, token)?),
    None => None,
  };
  let variables = match variables_node {
    Some(child) => Some(extent.keep(variables_definition(child, source)?)),
    None => None,
  };
  let directives = extent.keep_opt(optional_directives(directives_node, source)?);

  let (outer, inner) = described_extents(node, extent, described)?;
  Ok((
    description,
    OperationDefinition::Named(NamedOperationDefinition::new(
      to_span(inner),
      operation_type,
      name,
      variables,
      directives,
      selections,
    )),
    outer,
  ))
}

fn operation_type(node: Node<'_>) -> Out<(OperationType, TextRange)> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => extent.unread(child),
    }
  }
  let token = names.at(0, node, "an operation keyword")?;
  let span = to_span(token.text_range());
  let operation_type = match keyword_of(token) {
    Some(ContextualKeyword::Query) => OperationType::Query(Query::new(span)),
    Some(ContextualKeyword::Mutation) => OperationType::Mutation(Mutation::new(span)),
    Some(ContextualKeyword::Subscription) => OperationType::Subscription(Subscription::new(span)),
    _ => {
      return Err(ProjectError::new(
        ProjectErrorKind::MalformedToken { kind: token.kind() },
        to_range(token.text_range()),
      ));
    }
  };
  Ok((operation_type, extent.range(node, "a token")?))
}

fn variables_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(VariablesDefinition<&'src str>, TextRange)> {
  let mut extent = Extent::default();
  let mut definitions = Vec::new();
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => extent.token(token),
      NodeOrToken::Node(child) => match child.kind() {
        K::VariableDefinition => {
          definitions.push(extent.keep(variable_definition(child, source)?));
        }
        _ => return Err(unexpected_node(node, child)),
      },
    }
  }
  let extent = extent.range(node, "a token")?;
  Ok((
    VariablesDefinition::new(to_span(extent), definitions),
    extent,
  ))
}

fn variable_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(DescribedVariableDefinition<&'src str>, TextRange)> {
  let mut extent = Extent::default();
  let mut description_node = None;
  let mut variable_node = None;
  let mut type_node = None;
  let mut default_node = None;
  let mut directives_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => extent.token(token),
      NodeOrToken::Node(child) => match child.kind() {
        K::Description if description_node.is_none() => description_node = Some(child),
        K::Variable if variable_node.is_none() => variable_node = Some(child),
        kind if type_node.is_none() && TYPE_KINDS.contains(&kind) => type_node = Some(child),
        K::DefaultValue if default_node.is_none() => default_node = Some(child),
        K::Directives if directives_node.is_none() => directives_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }

  let (description, described) = hoisted_description(description_node, source)?;
  let variable = variable_node.ok_or_else(|| missing(node, "a variable"))?;
  let variable = extent.keep(variable_value(variable, source)?);
  let ty = extent.keep(require_type(node, type_node, source)?);
  let default_value = extent.keep_opt(optional_default_value(default_node, source)?);
  let directives = extent.keep_opt(optional_const_directives(directives_node, source)?);

  // The one described node below document level whose inner span excludes the description — see
  // the module header for the three that do not.
  let (outer, inner) = described_extents(node, extent, described)?;
  Ok((
    Described::new(
      to_span(outer),
      description,
      VariableDefinition::new(to_span(inner), variable, ty, default_value, directives),
    ),
    outer,
  ))
}

fn fragment_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, crate::graphql::ast::FragmentDefinition<&'src str>>> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  let mut description_node = None;
  let mut condition_node = None;
  let mut directives_node = None;
  let mut selection_set_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => match child.kind() {
        K::Description if description_node.is_none() => description_node = Some(child),
        K::NamedType if condition_node.is_none() => condition_node = Some(child),
        K::Directives if directives_node.is_none() => directives_node = Some(child),
        K::SelectionSet if selection_set_node.is_none() => selection_set_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }

  let (description, described) = hoisted_description(description_node, source)?;
  // `fragment` is the first `Name` token, the fragment's own name the second, `on` the third.
  let name = fragment_name(names.at(1, node, "a fragment name")?, source)?;

  let on_token = names.at(2, node, "an `on` keyword")?;
  let condition = condition_node.ok_or_else(|| missing(node, "a type condition"))?;
  let condition_name = extent.keep(inner_name(condition, source)?);
  let type_condition = TypeCondition::new(
    SimpleSpan::new(
      usize::from(on_token.text_range().start()),
      condition_name.span().end(),
    ),
    condition_name,
  );

  let directives = extent.keep_opt(optional_directives(directives_node, source)?);
  let set = selection_set_node.ok_or_else(|| missing(node, "a selection set"))?;
  let selections = extent.keep(selection_set(set, source)?);

  let (outer, inner) = described_extents(node, extent, described)?;
  Ok((
    description,
    crate::graphql::ast::FragmentDefinition::new(
      to_span(inner),
      name,
      type_condition,
      directives,
      selections,
    ),
    outer,
  ))
}

/// A fragment name, with the one semantic rule the tree records only as a diagnostic.
///
/// `FragmentName::new` is deliberately crate-private so the syntactic productions are the single
/// place that establishes `Name but not on`. This is the second custodian, and it exists because
/// the lossless productions record the violation on the diagnostic channel and still build the
/// node — so the shape alone cannot tell a legal fragment name from an illegal one.
fn fragment_name<'src>(token: Token<'_>, source: &'src str) -> Out<FragmentName<&'src str>> {
  let slice = slice(source, token)?;
  if slice == "on" {
    return Err(ProjectError::new(
      ProjectErrorKind::SemanticRule {
        rule: "a fragment may not be named `on`",
      },
      to_range(token.text_range()),
    ));
  }
  Ok(FragmentName::new(to_span(token.text_range()), slice))
}

// ---------------------------------------------------------------------------------------------
// selections
// ---------------------------------------------------------------------------------------------

fn selection_set<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(SelectionSet<&'src str>, TextRange)> {
  let mut extent = Extent::default();
  let mut selections = Vec::new();
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => extent.token(token),
      NodeOrToken::Node(child) => selections.push(match child.kind() {
        K::Field => Selection::Field(extent.keep(field(child, source)?)),
        K::FragmentSpread => {
          Selection::FragmentSpread(extent.keep(fragment_spread(child, source)?))
        }
        K::InlineFragment => {
          Selection::InlineFragment(extent.keep(inline_fragment(child, source)?))
        }
        _ => return Err(unexpected_node(node, child)),
      }),
    }
  }
  let extent = extent.range(node, "a token")?;
  Ok((
    SelectionSet::new(to_span(extent), selections.into()),
    extent,
  ))
}

fn field<'src>(node: Node<'_>, source: &'src str) -> Out<(Field<&'src str>, TextRange)> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  let mut alias_node = None;
  let mut arguments_node = None;
  let mut directives_node = None;
  let mut selection_set_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => match child.kind() {
        K::Alias if alias_node.is_none() => alias_node = Some(child),
        K::Arguments if arguments_node.is_none() => arguments_node = Some(child),
        K::Directives if directives_node.is_none() => directives_node = Some(child),
        K::SelectionSet if selection_set_node.is_none() => selection_set_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }

  let alias = match alias_node {
    Some(child) => {
      // The alias node holds the `:`, and the AST's alias span holds it too — which the fold
      // gives for free, since the `:` is one of the node's own non-trivia tokens.
      let (alias_name, piece) = inner_name(child, source)?;
      extent.cover(piece);
      Some(Alias::new(to_span(piece), alias_name))
    }
    None => None,
  };
  // The alias's own `Name` lives inside the `Alias` node, so a direct scan answers the field's.
  let name = name(source, names.at(0, node, "a field name")?)?;
  let arguments = extent.keep_opt(optional_arguments(arguments_node, source)?);
  let directives = extent.keep_opt(optional_directives(directives_node, source)?);
  let selections = match selection_set_node {
    Some(set) => Some(extent.keep(selection_set(set, source)?)),
    None => None,
  };
  let extent = extent.range(node, "a token")?;
  Ok((
    Field::new(
      to_span(extent),
      alias,
      name,
      arguments,
      directives,
      selections,
    ),
    extent,
  ))
}

fn fragment_spread<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(FragmentSpread<&'src str>, TextRange)> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  let mut directives_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => match child.kind() {
        K::Directives if directives_node.is_none() => directives_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }
  let name = fragment_name(names.at(0, node, "a fragment name")?, source)?;
  let directives = extent.keep_opt(optional_directives(directives_node, source)?);
  let extent = extent.range(node, "a token")?;
  Ok((
    FragmentSpread::new(to_span(extent), name, directives),
    extent,
  ))
}

fn inline_fragment<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(InlineFragment<&'src str>, TextRange)> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  let mut condition_node = None;
  let mut directives_node = None;
  let mut selection_set_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => match child.kind() {
        K::NamedType if condition_node.is_none() => condition_node = Some(child),
        K::Directives if directives_node.is_none() => directives_node = Some(child),
        K::SelectionSet if selection_set_node.is_none() => selection_set_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }

  let type_condition = match condition_node {
    Some(child) => {
      let on_token = names.at(0, node, "an `on` keyword")?;
      let condition_name = extent.keep(inner_name(child, source)?);
      Some(TypeCondition::new(
        SimpleSpan::new(
          usize::from(on_token.text_range().start()),
          condition_name.span().end(),
        ),
        condition_name,
      ))
    }
    None => None,
  };
  let directives = extent.keep_opt(optional_directives(directives_node, source)?);
  let set = selection_set_node.ok_or_else(|| missing(node, "a selection set"))?;
  let selections = extent.keep(selection_set(set, source)?);
  let extent = extent.range(node, "a token")?;
  Ok((
    InlineFragment::new(to_span(extent), type_condition, directives, selections),
    extent,
  ))
}

// ---------------------------------------------------------------------------------------------
// type references
// ---------------------------------------------------------------------------------------------

const TYPE_KINDS: [SyntaxKind; 3] = [K::NamedType, K::ListType, K::NonNullType];

/// The type reference a node's walk collected, refused when there is none.
///
/// `child` is the slot the caller's own dispatch filled; `node` is only ever the refusal's owner.
fn require_type<'src>(
  node: Node<'_>,
  child: Option<Node<'_>>,
  source: &'src str,
) -> Out<(Type<Name<&'src str>>, TextRange)> {
  let child = child.ok_or_else(|| missing(node, "a type reference"))?;
  ty(child, source)
}

/// The `!` folds into the node it wraps, exactly as the syntactic parser folds it.
///
/// A `NonNullType` has no AST image of its own: `T!` is a `NamedType` with `required` set, and
/// its span is the extent that includes the `!`.
fn ty<'src>(node: Node<'_>, source: &'src str) -> Out<(Type<Name<&'src str>>, TextRange)> {
  match node.kind() {
    K::NamedType => {
      let (name, extent) = inner_name(node, source)?;
      Ok((
        Type::Name(NamedType::new(to_span(extent), name, false)),
        extent,
      ))
    }
    K::ListType => {
      let (element, extent) = list_element(node, source)?;
      Ok((
        ListType::new(to_span(extent), element, false).into(),
        extent,
      ))
    }
    K::NonNullType => non_null_type(node, source),
    found => Err(unexpected(node, found, node.text_range())),
  }
}

/// The element type a `ListType` node wraps, with the list node's own extent — brackets included.
///
/// Split out because the `!` folds: `[T]!` builds one AST list from **two** tree nodes, and the
/// span it carries is the outer one's, so the inner node has to answer its element and its extent
/// without also building a list of its own.
fn list_element<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(Type<Name<&'src str>>, TextRange)> {
  let mut extent = Extent::default();
  let mut wrapped = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => extent.token(token),
      NodeOrToken::Node(child) => match child.kind() {
        kind if wrapped.is_none() && TYPE_KINDS.contains(&kind) => wrapped = Some(child),
        _ => extent.unread(child),
      },
    }
  }
  let element = extent.keep(require_type(node, wrapped, source)?);
  Ok((element, extent.range(node, "a token")?))
}

/// `T!` and `[T]!`, where the `!` is a token of this node and the span it produces is this node's.
fn non_null_type<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(Type<Name<&'src str>>, TextRange)> {
  let mut extent = Extent::default();
  let mut wrapped = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => extent.token(token),
      NodeOrToken::Node(child) => match child.kind() {
        kind if wrapped.is_none() && TYPE_KINDS.contains(&kind) => wrapped = Some(child),
        _ => extent.unread(child),
      },
    }
  }
  let inner = wrapped.ok_or_else(|| missing(node, "a wrapped type"))?;
  match inner.kind() {
    K::NamedType => {
      let name = extent.keep(inner_name(inner, source)?);
      let extent = extent.range(node, "a token")?;
      Ok((
        Type::Name(NamedType::new(to_span(extent), name, true)),
        extent,
      ))
    }
    K::ListType => {
      let element = extent.keep(list_element(inner, source)?);
      let extent = extent.range(node, "a token")?;
      Ok((ListType::new(to_span(extent), element, true).into(), extent))
    }
    // `T!!` has no production, so a nested `NonNullType` is not a shape the AST can hold.
    found => Err(unexpected(node, found, inner.text_range())),
  }
}

// ---------------------------------------------------------------------------------------------
// directives and arguments
// ---------------------------------------------------------------------------------------------

/// The `Directives` node exists only where at least one directive was written, and the AST
/// records an absent run as `None` — so the two agree without a zero-width placeholder.
///
/// `run` is the slot the caller's own dispatch filled, which is why this takes an `Option` rather
/// than looking the child up again.
fn optional_directives<'src>(
  run: Option<Node<'_>>,
  source: &'src str,
) -> Out<Option<(Directives<&'src str>, TextRange)>> {
  let Some(run) = run else { return Ok(None) };
  let mut extent = Extent::default();
  let mut directives = Vec::new();
  for element in run.children() {
    match element {
      NodeOrToken::Token(token) => extent.token(token),
      NodeOrToken::Node(child) => match child.kind() {
        K::Directive => directives.push(extent.keep(directive(child, source)?)),
        _ => return Err(unexpected_node(run, child)),
      },
    }
  }
  let extent = extent.range(run, "a token")?;
  Ok(Some((Directives::new(to_span(extent), directives), extent)))
}

fn directive<'src>(node: Node<'_>, source: &'src str) -> Out<(Directive<&'src str>, TextRange)> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  let mut arguments_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => match child.kind() {
        K::Arguments if arguments_node.is_none() => arguments_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }
  let name = name(source, names.at(0, node, "a directive name")?)?;
  let arguments = extent.keep_opt(optional_arguments(arguments_node, source)?);
  let extent = extent.range(node, "a token")?;
  Ok((Directive::new(to_span(extent), name, arguments), extent))
}

fn optional_arguments<'src>(
  list: Option<Node<'_>>,
  source: &'src str,
) -> Out<Option<(Arguments<&'src str>, TextRange)>> {
  let Some(list) = list else { return Ok(None) };
  let mut extent = Extent::default();
  let mut arguments = Vec::new();
  for element in list.children() {
    match element {
      NodeOrToken::Token(token) => extent.token(token),
      NodeOrToken::Node(child) => match child.kind() {
        K::Argument => arguments.push(extent.keep(argument(child, source)?)),
        _ => return Err(unexpected_node(list, child)),
      },
    }
  }
  let extent = extent.range(list, "a token")?;
  Ok(Some((Arguments::new(to_span(extent), arguments), extent)))
}

fn argument<'src>(node: Node<'_>, source: &'src str) -> Out<(Argument<&'src str>, TextRange)> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  let mut value_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => match child.kind() {
        kind if value_node.is_none() && VALUE_KINDS.contains(&kind) => value_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }
  let name = name(source, names.at(0, node, "an argument name")?)?;
  let value_node = value_node.ok_or_else(|| missing(node, "a value"))?;
  let value = extent.keep(value(value_node, source)?);
  let extent = extent.range(node, "a token")?;
  Ok((Argument::new(to_span(extent), name, value), extent))
}

fn optional_const_directives<'src>(
  run: Option<Node<'_>>,
  source: &'src str,
) -> Out<Option<(ConstDirectives<&'src str>, TextRange)>> {
  let Some(run) = run else { return Ok(None) };
  let mut extent = Extent::default();
  let mut directives = Vec::new();
  for element in run.children() {
    match element {
      NodeOrToken::Token(token) => extent.token(token),
      NodeOrToken::Node(child) => match child.kind() {
        K::Directive => directives.push(extent.keep(const_directive(child, source)?)),
        _ => return Err(unexpected_node(run, child)),
      },
    }
  }
  let extent = extent.range(run, "a token")?;
  Ok(Some((
    ConstDirectives::new(to_span(extent), directives),
    extent,
  )))
}

fn const_directive<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(ConstDirective<&'src str>, TextRange)> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  let mut arguments_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => match child.kind() {
        K::Arguments if arguments_node.is_none() => arguments_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }
  let name = name(source, names.at(0, node, "a directive name")?)?;
  let arguments = extent.keep_opt(optional_const_arguments(arguments_node, source)?);
  let extent = extent.range(node, "a token")?;
  Ok((
    ConstDirective::new(to_span(extent), name, arguments),
    extent,
  ))
}

fn optional_const_arguments<'src>(
  list: Option<Node<'_>>,
  source: &'src str,
) -> Out<Option<(ConstArguments<&'src str>, TextRange)>> {
  let Some(list) = list else { return Ok(None) };
  let mut extent = Extent::default();
  let mut arguments = Vec::new();
  for element in list.children() {
    match element {
      NodeOrToken::Token(token) => extent.token(token),
      NodeOrToken::Node(child) => match child.kind() {
        K::Argument => arguments.push(extent.keep(const_argument(child, source)?)),
        _ => return Err(unexpected_node(list, child)),
      },
    }
  }
  let extent = extent.range(list, "a token")?;
  Ok(Some((
    ConstArguments::new(to_span(extent), arguments),
    extent,
  )))
}

fn const_argument<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(ConstArgument<&'src str>, TextRange)> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  let mut value_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => match child.kind() {
        kind if value_node.is_none() && VALUE_KINDS.contains(&kind) => value_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }
  let name = name(source, names.at(0, node, "an argument name")?)?;
  let value_node = value_node.ok_or_else(|| missing(node, "a value"))?;
  let value = extent.keep(const_value(node, value_node, source)?);
  let extent = extent.range(node, "a token")?;
  Ok((ConstArgument::new(to_span(extent), name, value), extent))
}

// ---------------------------------------------------------------------------------------------
// values
// ---------------------------------------------------------------------------------------------

const VALUE_KINDS: [SyntaxKind; 9] = [
  K::Variable,
  K::IntValue,
  K::FloatValue,
  K::StringValue,
  K::BooleanValue,
  K::NullValue,
  K::EnumValue,
  K::ListValue,
  K::ObjectValue,
];

/// A leaf value's extent and the one token it carries, from the node's single walk.
///
/// `kinds` is what the leaf's own production committed, so a token of any other kind is not the
/// literal even when it comes first — which is the distinction between "no literal here" and "the
/// tree put something else here".
fn leaf_token<'g>(node: Node<'g>, kinds: &[SyntaxKind]) -> (Extent, Option<Token<'g>>) {
  let mut extent = Extent::default();
  let mut literal = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if literal.is_none() && kinds.contains(&token.kind()) {
          literal = Some(token);
        }
      }
      NodeOrToken::Node(child) => extent.unread(child),
    }
  }
  (extent, literal)
}

/// A leaf value's slice, span and extent — every leaf but the string one, which has to cook.
fn leaf<'src>(
  node: Node<'_>,
  source: &'src str,
  kinds: &[SyntaxKind],
  wanted: &'static str,
) -> Out<(&'src str, SimpleSpan, TextRange)> {
  let (extent, token) = leaf_token(node, kinds);
  let extent = extent.range(node, "a token")?;
  let token = token.ok_or_else(|| missing(node, wanted))?;
  Ok((slice(source, token)?, to_span(extent), extent))
}

/// A string leaf: the [`StringValue`]'s span is its **token's**, not the node's extent, which is
/// what the syntactic parser produces and therefore what the differential gate compares against.
fn string_literal<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(StringValue<&'src str>, TextRange)> {
  let (extent, token) = leaf_token(node, &[K::String, K::BlockString]);
  let extent = extent.range(node, "a token")?;
  let token = token.ok_or_else(|| missing(node, "a string literal"))?;
  Ok((string_value(token, source)?, extent))
}

fn boolean_literal<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(BooleanValue<&'src str>, TextRange)> {
  let (slice, span, extent) = leaf(node, source, &[K::Name], "a `true` or `false` keyword")?;
  match slice {
    "true" => Ok((BooleanValue::new(span, true), extent)),
    "false" => Ok((BooleanValue::new(span, false), extent)),
    _ => Err(ProjectError::new(
      ProjectErrorKind::MalformedToken { kind: K::Name },
      to_range(node.text_range()),
    )),
  }
}

fn value<'src>(node: Node<'_>, source: &'src str) -> Out<(InputValue<&'src str>, TextRange)> {
  Ok(match node.kind() {
    K::Variable => {
      let (variable, extent) = variable_value(node, source)?;
      (InputValue::Variable(variable), extent)
    }
    K::IntValue => {
      let (text, span, extent) = leaf(node, source, &[K::Int], "an integer literal")?;
      (InputValue::Int(IntValue::new(span, text)), extent)
    }
    K::FloatValue => {
      let (text, span, extent) = leaf(node, source, &[K::Float], "a float literal")?;
      (InputValue::Float(FloatValue::new(span, text)), extent)
    }
    K::StringValue => {
      let (string, extent) = string_literal(node, source)?;
      (InputValue::String(string), extent)
    }
    K::BooleanValue => {
      let (boolean, extent) = boolean_literal(node, source)?;
      (InputValue::Boolean(boolean), extent)
    }
    K::NullValue => {
      let (text, span, extent) = leaf(node, source, &[K::Name], "a `null` keyword")?;
      (InputValue::Null(NullValue::new(span, text)), extent)
    }
    K::EnumValue => {
      let (text, span, extent) = leaf(node, source, &[K::Name], "an enum value")?;
      (InputValue::Enum(EnumValue::new(span, text)), extent)
    }
    K::ListValue => {
      let mut extent = Extent::default();
      let mut values = Vec::new();
      for element in node.children() {
        match element {
          NodeOrToken::Token(token) => extent.token(token),
          NodeOrToken::Node(child) => values.push(extent.keep(value(child, source)?)),
        }
      }
      let extent = extent.range(node, "a token")?;
      (
        InputValue::List(List::new(to_span(extent), values.into())),
        extent,
      )
    }
    K::ObjectValue => {
      let mut extent = Extent::default();
      let mut fields = Vec::new();
      for element in node.children() {
        match element {
          NodeOrToken::Token(token) => extent.token(token),
          NodeOrToken::Node(child) => match child.kind() {
            K::ObjectField => fields.push(extent.keep(object_field(child, source)?)),
            _ => return Err(unexpected_node(node, child)),
          },
        }
      }
      let extent = extent.range(node, "a token")?;
      (
        InputValue::Object(Object::new(to_span(extent), fields.into())),
        extent,
      )
    }
    found => return Err(unexpected(node, found, node.text_range())),
  })
}

/// A constant value position, where the AST's own type system forbids a variable.
///
/// [`ConstInputValue`] has no `Variable` variant, so the refusal is not a policy this module
/// invented: there is nothing to construct.
///
/// `parent` is the node whose dispatch reached `node`. A green tree carries no parent pointer, and
/// the caller's own frame is where the one the refusal names comes from — see
/// [`Node`](crate::lossless::project::Node).
fn const_value<'src>(
  parent: Node<'_>,
  node: Node<'_>,
  source: &'src str,
) -> Out<(ConstInputValue<&'src str>, TextRange)> {
  Ok(match node.kind() {
    // The refusal is attributed to the position, not to the variable: a `Variable` node is
    // perfectly legal, and what is wrong is the const context that is holding one.
    K::Variable => return Err(unexpected(parent, K::Variable, node.text_range())),
    K::IntValue => {
      let (text, span, extent) = leaf(node, source, &[K::Int], "an integer literal")?;
      (ConstInputValue::Int(IntValue::new(span, text)), extent)
    }
    K::FloatValue => {
      let (text, span, extent) = leaf(node, source, &[K::Float], "a float literal")?;
      (ConstInputValue::Float(FloatValue::new(span, text)), extent)
    }
    K::StringValue => {
      let (string, extent) = string_literal(node, source)?;
      (ConstInputValue::String(string), extent)
    }
    K::BooleanValue => {
      let (boolean, extent) = boolean_literal(node, source)?;
      (ConstInputValue::Boolean(boolean), extent)
    }
    K::NullValue => {
      let (text, span, extent) = leaf(node, source, &[K::Name], "a `null` keyword")?;
      (ConstInputValue::Null(NullValue::new(span, text)), extent)
    }
    K::EnumValue => {
      let (text, span, extent) = leaf(node, source, &[K::Name], "an enum value")?;
      (ConstInputValue::Enum(EnumValue::new(span, text)), extent)
    }
    K::ListValue => {
      let mut extent = Extent::default();
      let mut values = Vec::new();
      for element in node.children() {
        match element {
          NodeOrToken::Token(token) => extent.token(token),
          NodeOrToken::Node(child) => values.push(extent.keep(const_value(node, child, source)?)),
        }
      }
      let extent = extent.range(node, "a token")?;
      (
        ConstInputValue::List(ConstList::new(to_span(extent), values.into())),
        extent,
      )
    }
    K::ObjectValue => {
      let mut extent = Extent::default();
      let mut fields = Vec::new();
      for element in node.children() {
        match element {
          NodeOrToken::Token(token) => extent.token(token),
          NodeOrToken::Node(child) => match child.kind() {
            K::ObjectField => fields.push(extent.keep(const_object_field(child, source)?)),
            _ => return Err(unexpected_node(node, child)),
          },
        }
      }
      let extent = extent.range(node, "a token")?;
      (
        ConstInputValue::Object(ConstObject::new(to_span(extent), fields.into())),
        extent,
      )
    }
    found => return Err(unexpected(node, found, node.text_range())),
  })
}

fn variable_value<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(VariableValue<&'src str>, TextRange)> {
  let (name, extent) = inner_name(node, source)?;
  Ok((VariableValue::new(to_span(extent), name), extent))
}

fn object_field<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(ObjectField<&'src str>, TextRange)> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  let mut value_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => match child.kind() {
        kind if value_node.is_none() && VALUE_KINDS.contains(&kind) => value_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }
  let name = name(source, names.at(0, node, "a field name")?)?;
  let value_node = value_node.ok_or_else(|| missing(node, "a value"))?;
  let value = extent.keep(value(value_node, source)?);
  let extent = extent.range(node, "a token")?;
  Ok((ObjectField::new(to_span(extent), name, value), extent))
}

fn const_object_field<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(ConstObjectField<&'src str>, TextRange)> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  let mut value_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => match child.kind() {
        kind if value_node.is_none() && VALUE_KINDS.contains(&kind) => value_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }
  let name = name(source, names.at(0, node, "a field name")?)?;
  let value_node = value_node.ok_or_else(|| missing(node, "a value"))?;
  let value = extent.keep(const_value(node, value_node, source)?);
  let extent = extent.range(node, "a token")?;
  Ok((ConstObjectField::new(to_span(extent), name, value), extent))
}

fn optional_default_value<'src>(
  default: Option<Node<'_>>,
  source: &'src str,
) -> Out<Option<(DefaultInputValue<&'src str>, TextRange)>> {
  let Some(default) = default else {
    return Ok(None);
  };
  let mut extent = Extent::default();
  let mut value_node = None;
  for element in default.children() {
    match element {
      NodeOrToken::Token(token) => extent.token(token),
      NodeOrToken::Node(child) => match child.kind() {
        kind if value_node.is_none() && VALUE_KINDS.contains(&kind) => value_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }
  let value_node = value_node.ok_or_else(|| missing(default, "a value"))?;
  let value = extent.keep(const_value(default, value_node, source)?);
  // The span covers the `=` and the value, which is the node's own token extent.
  let extent = extent.range(default, "a token")?;
  Ok(Some((
    DefaultInputValue::new(to_span(extent), value),
    extent,
  )))
}

// ---------------------------------------------------------------------------------------------
// SDL definitions
// ---------------------------------------------------------------------------------------------

/// The name behind a definition's keyword: `scalar S`, `type T`, `directive @d` all reach it at
/// index 1 among the node's direct `Name` tokens, the keyword being index 0.
const KEYWORD_NAMED: usize = 1;

/// An extension's name is its **third** `Name`: `extend`, the shape keyword, then the name.
const EXTENSION_NAMED: usize = 2;

fn optional_implements<'src>(
  clause: Option<Node<'_>>,
  source: &'src str,
) -> Out<Option<(ImplementInterfaces<Name<&'src str>>, TextRange)>> {
  let Some(clause) = clause else {
    return Ok(None);
  };
  let mut extent = Extent::default();
  let mut interfaces = Vec::new();
  for element in clause.children() {
    match element {
      NodeOrToken::Token(token) => extent.token(token),
      NodeOrToken::Node(child) => match child.kind() {
        // The AST holds `Name`s, not `NamedType`s: an implemented interface can carry no `!`
        // and no brackets, so the type-reference level would be a wrapper over nothing.
        K::NamedType => interfaces.push(extent.keep(inner_name(child, source)?)),
        _ => return Err(unexpected_node(clause, child)),
      },
    }
  }
  let extent = extent.range(clause, "a token")?;
  Ok(Some((
    ImplementInterfaces::new(to_span(extent), interfaces),
    extent,
  )))
}

fn optional_union_members<'src>(
  clause: Option<Node<'_>>,
  source: &'src str,
) -> Out<Option<(UnionMemberTypes<Name<&'src str>>, TextRange)>> {
  let Some(clause) = clause else {
    return Ok(None);
  };
  let mut extent = Extent::default();
  let mut members = Vec::new();
  for element in clause.children() {
    match element {
      NodeOrToken::Token(token) => extent.token(token),
      NodeOrToken::Node(child) => match child.kind() {
        K::NamedType => members.push(extent.keep(inner_name(child, source)?)),
        _ => return Err(unexpected_node(clause, child)),
      },
    }
  }
  let extent = extent.range(clause, "a token")?;
  Ok(Some((
    UnionMemberTypes::new(to_span(extent), members),
    extent,
  )))
}

fn optional_fields_definition<'src>(
  block: Option<Node<'_>>,
  source: &'src str,
) -> Out<Option<(FieldsDefinition<&'src str>, TextRange)>> {
  let Some(block) = block else {
    return Ok(None);
  };
  let mut extent = Extent::default();
  let mut fields = Vec::new();
  for element in block.children() {
    match element {
      NodeOrToken::Token(token) => extent.token(token),
      NodeOrToken::Node(child) => match child.kind() {
        K::FieldDefinition => fields.push(extent.keep(field_definition(child, source)?)),
        _ => return Err(unexpected_node(block, child)),
      },
    }
  }
  let extent = extent.range(block, "a token")?;
  Ok(Some((
    FieldsDefinition::new(to_span(extent), fields),
    extent,
  )))
}

fn field_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(crate::graphql::ast::FieldDefinition<&'src str>, TextRange)> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  let mut description_node = None;
  let mut arguments_node = None;
  let mut type_node = None;
  let mut directives_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => match child.kind() {
        K::Description if description_node.is_none() => description_node = Some(child),
        K::ArgumentsDefinition if arguments_node.is_none() => arguments_node = Some(child),
        kind if type_node.is_none() && TYPE_KINDS.contains(&kind) => type_node = Some(child),
        K::Directives if directives_node.is_none() => directives_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }

  // One span for both halves, description included — trunk's rule for this node, see the header.
  // The description is therefore folded straight in rather than kept apart for the hoist.
  let (description, described) = hoisted_description(description_node, source)?;
  if let Some(described) = described {
    extent.cover(described);
  }
  let name = name(source, names.at(0, node, "a field name")?)?;
  let arguments_definition =
    extent.keep_opt(optional_arguments_definition(arguments_node, source)?);
  let ty = extent.keep(require_type(node, type_node, source)?);
  let directives = extent.keep_opt(optional_const_directives(directives_node, source)?);

  let extent = extent.range(node, "a token")?;
  let span = to_span(extent);
  Ok((
    Described::new(
      span,
      description,
      FieldDefinition::new(span, name, arguments_definition, ty, directives),
    ),
    extent,
  ))
}

fn optional_arguments_definition<'src>(
  block: Option<Node<'_>>,
  source: &'src str,
) -> Out<
  Option<(
    crate::graphql::ast::ArgumentsDefinition<&'src str>,
    TextRange,
  )>,
> {
  let Some(block) = block else {
    return Ok(None);
  };
  let mut extent = Extent::default();
  let mut definitions = Vec::new();
  for element in block.children() {
    match element {
      NodeOrToken::Token(token) => extent.token(token),
      NodeOrToken::Node(child) => match child.kind() {
        K::InputValueDefinition => {
          definitions.push(extent.keep(input_value_definition(child, source)?));
        }
        _ => return Err(unexpected_node(block, child)),
      },
    }
  }
  let extent = extent.range(block, "a token")?;
  Ok(Some((
    ArgumentsDefinition::new(to_span(extent), definitions),
    extent,
  )))
}

fn input_value_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(
  crate::graphql::ast::InputValueDefinition<&'src str>,
  TextRange,
)> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  let mut description_node = None;
  let mut type_node = None;
  let mut default_node = None;
  let mut directives_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => match child.kind() {
        K::Description if description_node.is_none() => description_node = Some(child),
        kind if type_node.is_none() && TYPE_KINDS.contains(&kind) => type_node = Some(child),
        K::DefaultValue if default_node.is_none() => default_node = Some(child),
        K::Directives if directives_node.is_none() => directives_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }

  // The second of the three node types whose wrapper and inner span agree — see the header.
  let (description, described) = hoisted_description(description_node, source)?;
  if let Some(described) = described {
    extent.cover(described);
  }
  let name = name(source, names.at(0, node, "an input value name")?)?;
  let ty = extent.keep(require_type(node, type_node, source)?);
  let default_value = extent.keep_opt(optional_default_value(default_node, source)?);
  let directives = extent.keep_opt(optional_const_directives(directives_node, source)?);

  let extent = extent.range(node, "a token")?;
  let span = to_span(extent);
  Ok((
    Described::new(
      span,
      description,
      InputValueDefinition::new(span, name, ty, default_value, directives),
    ),
    extent,
  ))
}

fn optional_input_fields_definition<'src>(
  block: Option<Node<'_>>,
  source: &'src str,
) -> Out<Option<(InputFieldsDefinition<&'src str>, TextRange)>> {
  let Some(block) = block else {
    return Ok(None);
  };
  let mut extent = Extent::default();
  let mut definitions = Vec::new();
  for element in block.children() {
    match element {
      NodeOrToken::Token(token) => extent.token(token),
      NodeOrToken::Node(child) => match child.kind() {
        K::InputValueDefinition => {
          definitions.push(extent.keep(input_value_definition(child, source)?));
        }
        _ => return Err(unexpected_node(block, child)),
      },
    }
  }
  let extent = extent.range(block, "a token")?;
  Ok(Some((
    InputFieldsDefinition::new(to_span(extent), definitions),
    extent,
  )))
}

fn optional_enum_values<'src>(
  block: Option<Node<'_>>,
  source: &'src str,
) -> Out<Option<(EnumValuesDefinition<&'src str>, TextRange)>> {
  let Some(block) = block else {
    return Ok(None);
  };
  let mut extent = Extent::default();
  let mut values = Vec::new();
  for element in block.children() {
    match element {
      NodeOrToken::Token(token) => extent.token(token),
      NodeOrToken::Node(child) => match child.kind() {
        K::EnumValueDefinition => values.push(extent.keep(enum_value_definition(child, source)?)),
        _ => return Err(unexpected_node(block, child)),
      },
    }
  }
  let extent = extent.range(block, "a token")?;
  Ok(Some((
    EnumValuesDefinition::new(to_span(extent), values),
    extent,
  )))
}

fn enum_value_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(
  crate::graphql::ast::EnumValueDefinition<&'src str>,
  TextRange,
)> {
  let mut extent = Extent::default();
  let mut description_node = None;
  let mut value_node = None;
  let mut directives_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => extent.token(token),
      NodeOrToken::Node(child) => match child.kind() {
        K::Description if description_node.is_none() => description_node = Some(child),
        K::EnumValue if value_node.is_none() => value_node = Some(child),
        K::Directives if directives_node.is_none() => directives_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }

  // The third of the three node types whose wrapper and inner span agree — see the header.
  let (description, described) = hoisted_description(description_node, source)?;
  if let Some(described) = described {
    extent.cover(described);
  }
  // The value's name lives inside the `EnumValue` node the tree opens for it, but the AST holds
  // a bare `Name` — the enum-value level has nothing else to carry.
  let value_node = value_node.ok_or_else(|| missing(node, "an enum value"))?;
  let value = extent.keep(inner_name(value_node, source)?);
  let directives = extent.keep_opt(optional_const_directives(directives_node, source)?);

  let extent = extent.range(node, "a token")?;
  let span = to_span(extent);
  Ok((
    Described::new(
      span,
      description,
      EnumValueDefinition::new(span, value, directives),
    ),
    extent,
  ))
}

fn scalar_type_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, ScalarTypeDefinition<&'src str>>> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  let mut description_node = None;
  let mut directives_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => match child.kind() {
        K::Description if description_node.is_none() => description_node = Some(child),
        K::Directives if directives_node.is_none() => directives_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }

  let (description, described) = hoisted_description(description_node, source)?;
  let name = name(
    source,
    names.at(KEYWORD_NAMED, node, "a name after the keyword")?,
  )?;
  let directives = extent.keep_opt(optional_const_directives(directives_node, source)?);

  let (outer, inner) = described_extents(node, extent, described)?;
  Ok((
    description,
    ScalarTypeDefinition::new(to_span(inner), name, directives),
    outer,
  ))
}

fn object_type_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, ObjectTypeDefinition<&'src str>>> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  let mut description_node = None;
  let mut implements_node = None;
  let mut directives_node = None;
  let mut fields_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => match child.kind() {
        K::Description if description_node.is_none() => description_node = Some(child),
        K::ImplementsInterfaces if implements_node.is_none() => implements_node = Some(child),
        K::Directives if directives_node.is_none() => directives_node = Some(child),
        K::FieldsDefinition if fields_node.is_none() => fields_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }

  let (description, described) = hoisted_description(description_node, source)?;
  let name = name(
    source,
    names.at(KEYWORD_NAMED, node, "a name after the keyword")?,
  )?;
  let implements = extent.keep_opt(optional_implements(implements_node, source)?);
  let directives = extent.keep_opt(optional_const_directives(directives_node, source)?);
  let fields_definition = extent.keep_opt(optional_fields_definition(fields_node, source)?);

  let (outer, inner) = described_extents(node, extent, described)?;
  Ok((
    description,
    ObjectTypeDefinition::new(
      to_span(inner),
      name,
      implements,
      directives,
      fields_definition,
    ),
    outer,
  ))
}

fn interface_type_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, InterfaceTypeDefinition<&'src str>>> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  let mut description_node = None;
  let mut implements_node = None;
  let mut directives_node = None;
  let mut fields_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => match child.kind() {
        K::Description if description_node.is_none() => description_node = Some(child),
        K::ImplementsInterfaces if implements_node.is_none() => implements_node = Some(child),
        K::Directives if directives_node.is_none() => directives_node = Some(child),
        K::FieldsDefinition if fields_node.is_none() => fields_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }

  let (description, described) = hoisted_description(description_node, source)?;
  let name = name(
    source,
    names.at(KEYWORD_NAMED, node, "a name after the keyword")?,
  )?;
  let implements = extent.keep_opt(optional_implements(implements_node, source)?);
  let directives = extent.keep_opt(optional_const_directives(directives_node, source)?);
  let fields_definition = extent.keep_opt(optional_fields_definition(fields_node, source)?);

  let (outer, inner) = described_extents(node, extent, described)?;
  Ok((
    description,
    InterfaceTypeDefinition::new(
      to_span(inner),
      name,
      implements,
      directives,
      fields_definition,
    ),
    outer,
  ))
}

fn union_type_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, UnionTypeDefinition<&'src str>>> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  let mut description_node = None;
  let mut directives_node = None;
  let mut members_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => match child.kind() {
        K::Description if description_node.is_none() => description_node = Some(child),
        K::Directives if directives_node.is_none() => directives_node = Some(child),
        K::UnionMemberTypes if members_node.is_none() => members_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }

  let (description, described) = hoisted_description(description_node, source)?;
  let name = name(
    source,
    names.at(KEYWORD_NAMED, node, "a name after the keyword")?,
  )?;
  let directives = extent.keep_opt(optional_const_directives(directives_node, source)?);
  let members = extent.keep_opt(optional_union_members(members_node, source)?);

  let (outer, inner) = described_extents(node, extent, described)?;
  Ok((
    description,
    UnionTypeDefinition::new(to_span(inner), name, directives, members),
    outer,
  ))
}

fn enum_type_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, EnumTypeDefinition<&'src str>>> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  let mut description_node = None;
  let mut directives_node = None;
  let mut values_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => match child.kind() {
        K::Description if description_node.is_none() => description_node = Some(child),
        K::Directives if directives_node.is_none() => directives_node = Some(child),
        K::EnumValuesDefinition if values_node.is_none() => values_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }

  let (description, described) = hoisted_description(description_node, source)?;
  let name = name(
    source,
    names.at(KEYWORD_NAMED, node, "a name after the keyword")?,
  )?;
  let directives = extent.keep_opt(optional_const_directives(directives_node, source)?);
  let values = extent.keep_opt(optional_enum_values(values_node, source)?);

  let (outer, inner) = described_extents(node, extent, described)?;
  Ok((
    description,
    EnumTypeDefinition::new(to_span(inner), name, directives, values),
    outer,
  ))
}

fn input_object_type_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, InputObjectTypeDefinition<&'src str>>> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  let mut description_node = None;
  let mut directives_node = None;
  let mut fields_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => match child.kind() {
        K::Description if description_node.is_none() => description_node = Some(child),
        K::Directives if directives_node.is_none() => directives_node = Some(child),
        K::InputFieldsDefinition if fields_node.is_none() => fields_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }

  let (description, described) = hoisted_description(description_node, source)?;
  let name = name(
    source,
    names.at(KEYWORD_NAMED, node, "a name after the keyword")?,
  )?;
  let directives = extent.keep_opt(optional_const_directives(directives_node, source)?);
  let fields = extent.keep_opt(optional_input_fields_definition(fields_node, source)?);

  let (outer, inner) = described_extents(node, extent, described)?;
  Ok((
    description,
    InputObjectTypeDefinition::new(to_span(inner), name, directives, fields),
    outer,
  ))
}

fn directive_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, crate::graphql::ast::DirectiveDefinition<&'src str>>> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  let mut description_node = None;
  let mut arguments_node = None;
  let mut locations_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => match child.kind() {
        K::Description if description_node.is_none() => description_node = Some(child),
        K::ArgumentsDefinition if arguments_node.is_none() => arguments_node = Some(child),
        K::DirectiveLocations if locations_node.is_none() => locations_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }

  let (description, described) = hoisted_description(description_node, source)?;
  let name = name(
    source,
    names.at(KEYWORD_NAMED, node, "a name after the keyword")?,
  )?;
  let arguments_definition =
    extent.keep_opt(optional_arguments_definition(arguments_node, source)?);
  // `repeatable` is optional and `on` is not, so the token at index 2 is one or the other. The
  // spelling is read rather than the count: an index that only counted would answer `on`.
  let repeatable = names
    .get(2)
    .is_some_and(|token| keyword_of(token) == Some(ContextualKeyword::Repeatable));
  let locations_node = locations_node.ok_or_else(|| missing(node, "a location list"))?;
  let locations = extent.keep(directive_locations(locations_node)?);

  let (outer, inner) = described_extents(node, extent, described)?;
  Ok((
    description,
    DirectiveDefinition::new(
      to_span(inner),
      name,
      arguments_definition,
      repeatable,
      locations,
    ),
    outer,
  ))
}

fn directive_locations(node: Node<'_>) -> Out<(DirectiveLocations<Location>, TextRange)> {
  let mut extent = Extent::default();
  let mut locations = Vec::new();
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() != K::Name {
          continue;
        }
        let location = keyword_of(token)
          .and_then(|keyword| classify_location(keyword, to_span(token.text_range())))
          .ok_or_else(|| {
            ProjectError::new(
              ProjectErrorKind::MalformedToken { kind: token.kind() },
              to_range(token.text_range()),
            )
          })?;
        locations.push(location);
      }
      NodeOrToken::Node(child) => extent.unread(child),
    }
  }
  if locations.is_empty() {
    return Err(missing(node, "a directive location"));
  }
  let extent = extent.range(node, "a token")?;
  Ok((DirectiveLocations::new(to_span(extent), locations), extent))
}

fn schema_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, SchemaDefinition<&'src str>>> {
  let mut extent = Extent::default();
  let mut description_node = None;
  let mut directives_node = None;
  let mut roots_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => extent.token(token),
      NodeOrToken::Node(child) => match child.kind() {
        K::Description if description_node.is_none() => description_node = Some(child),
        K::Directives if directives_node.is_none() => directives_node = Some(child),
        K::RootOperationTypeDefinitions if roots_node.is_none() => roots_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }

  let (description, described) = hoisted_description(description_node, source)?;
  let directives = extent.keep_opt(optional_const_directives(directives_node, source)?);
  let roots_node = roots_node.ok_or_else(|| missing(node, "a root operation types block"))?;
  let roots = extent.keep(root_operation_types(roots_node, source)?);

  let (outer, inner) = described_extents(node, extent, described)?;
  Ok((
    description,
    SchemaDefinition::new(to_span(inner), directives, roots),
    outer,
  ))
}

fn root_operation_types<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(RootOperationTypesDefinition<&'src str>, TextRange)> {
  let mut extent = Extent::default();
  let mut roots = Vec::new();
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => extent.token(token),
      NodeOrToken::Node(child) => match child.kind() {
        K::RootOperationTypeDefinition => {
          roots.push(extent.keep(root_operation_type(child, source)?));
        }
        _ => return Err(unexpected_node(node, child)),
      },
    }
  }
  let extent = extent.range(node, "a token")?;
  Ok((
    RootOperationTypesDefinition::new(to_span(extent), roots),
    extent,
  ))
}

fn root_operation_type<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(RootOperationTypeDefinition<&'src str>, TextRange)> {
  let mut extent = Extent::default();
  let mut keyword_node = None;
  let mut named_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => extent.token(token),
      NodeOrToken::Node(child) => match child.kind() {
        K::OperationType if keyword_node.is_none() => keyword_node = Some(child),
        K::NamedType if named_node.is_none() => named_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }
  let keyword_node = keyword_node.ok_or_else(|| missing(node, "an operation keyword"))?;
  let operation_type = extent.keep(operation_type(keyword_node)?);
  let named_node = named_node.ok_or_else(|| missing(node, "a root type name"))?;
  let named = extent.keep(inner_name(named_node, source)?);
  let extent = extent.range(node, "a token")?;
  Ok((
    RootOperationTypeDefinition::new(to_span(extent), operation_type, named),
    extent,
  ))
}

// ---------------------------------------------------------------------------------------------
// SDL extensions
// ---------------------------------------------------------------------------------------------

/// The four constituents every extension's tail is assembled from, and its extent.
///
/// The seven extension productions differ only in which of these the grammar lets them carry and
/// in how the combination is encoded, so the walk is written once and each of them reads the slots
/// it has a place for. A slot the production has no place for stays `None` — the walk never fills
/// one that is not in the tree.
struct ExtensionParts<'src> {
  name: Name<&'src str>,
  implements: Option<ImplementInterfaces<Name<&'src str>>>,
  directives: Option<ConstDirectives<&'src str>>,
  fields: Option<FieldsDefinition<&'src str>>,
  input_fields: Option<InputFieldsDefinition<&'src str>>,
  members: Option<UnionMemberTypes<Name<&'src str>>>,
  values: Option<EnumValuesDefinition<&'src str>>,
  extent: TextRange,
}

fn extension_parts<'src>(node: Node<'_>, source: &'src str) -> Out<ExtensionParts<'src>> {
  let mut extent = Extent::default();
  let mut names = Names::default();
  let mut implements_node = None;
  let mut directives_node = None;
  let mut fields_node = None;
  let mut input_fields_node = None;
  let mut members_node = None;
  let mut values_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => {
        extent.token(token);
        if token.kind() == K::Name {
          names.push(token);
        }
      }
      NodeOrToken::Node(child) => match child.kind() {
        K::ImplementsInterfaces if implements_node.is_none() => implements_node = Some(child),
        K::Directives if directives_node.is_none() => directives_node = Some(child),
        K::FieldsDefinition if fields_node.is_none() => fields_node = Some(child),
        K::InputFieldsDefinition if input_fields_node.is_none() => input_fields_node = Some(child),
        K::UnionMemberTypes if members_node.is_none() => members_node = Some(child),
        K::EnumValuesDefinition if values_node.is_none() => values_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }

  let name = name(
    source,
    names.at(EXTENSION_NAMED, node, "an extended type's name")?,
  )?;
  let implements = extent.keep_opt(optional_implements(implements_node, source)?);
  let directives = extent.keep_opt(optional_const_directives(directives_node, source)?);
  let fields = extent.keep_opt(optional_fields_definition(fields_node, source)?);
  let input_fields = extent.keep_opt(optional_input_fields_definition(input_fields_node, source)?);
  let members = extent.keep_opt(optional_union_members(members_node, source)?);
  let values = extent.keep_opt(optional_enum_values(values_node, source)?);

  Ok(ExtensionParts {
    name,
    implements,
    directives,
    fields,
    input_fields,
    members,
    values,
    extent: extent.range(node, "a token")?,
  })
}

fn scalar_type_extension<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(ScalarTypeExtension<&'src str>, TextRange)> {
  let parts = extension_parts(node, source)?;
  // The one extension whose directives the grammar makes mandatory: it has no other tail.
  let directives = parts
    .directives
    .ok_or_else(|| missing(node, "the directives a scalar extension must add"))?;
  Ok((
    ScalarTypeExtension::new(to_span(parts.extent), parts.name, directives),
    parts.extent,
  ))
}

fn object_type_extension<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(ObjectTypeExtension<&'src str>, TextRange)> {
  let parts = extension_parts(node, source)?;
  let data = match (parts.implements, parts.directives, parts.fields) {
    (implements, directives, Some(fields_definition)) => ObjectTypeExtensionData::Fields {
      implements,
      directives,
      fields_definition,
    },
    (implements, Some(directives), None) => ObjectTypeExtensionData::Directives {
      implements,
      directives,
    },
    (Some(implements), None, None) => ObjectTypeExtensionData::Implements(implements),
    (None, None, None) => {
      return Err(missing(
        node,
        "interfaces, directives or fields for the extension to add",
      ));
    }
  };
  Ok((
    ObjectTypeExtension::new(to_span(parts.extent), parts.name, data),
    parts.extent,
  ))
}

fn interface_type_extension<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(InterfaceTypeExtension<&'src str>, TextRange)> {
  let parts = extension_parts(node, source)?;
  let data = match (parts.implements, parts.directives, parts.fields) {
    (implements, directives, Some(fields_definition)) => InterfaceTypeExtensionData::Fields {
      implements,
      directives,
      fields_definition,
    },
    (implements, Some(directives), None) => InterfaceTypeExtensionData::Directives {
      implements,
      directives,
    },
    (Some(implements), None, None) => InterfaceTypeExtensionData::Implements(implements),
    (None, None, None) => {
      return Err(missing(
        node,
        "interfaces, directives or fields for the extension to add",
      ));
    }
  };
  Ok((
    InterfaceTypeExtension::new(to_span(parts.extent), parts.name, data),
    parts.extent,
  ))
}

fn union_type_extension<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(UnionTypeExtension<&'src str>, TextRange)> {
  let parts = extension_parts(node, source)?;
  let data = match (parts.directives, parts.members) {
    (directives, Some(member_types)) => UnionTypeExtensionData::Members {
      directives,
      member_types,
    },
    (Some(directives), None) => UnionTypeExtensionData::Directives(directives),
    (None, None) => {
      return Err(missing(
        node,
        "directives or members for the extension to add",
      ));
    }
  };
  Ok((
    UnionTypeExtension::new(to_span(parts.extent), parts.name, data),
    parts.extent,
  ))
}

fn enum_type_extension<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(EnumTypeExtension<&'src str>, TextRange)> {
  let parts = extension_parts(node, source)?;
  let data = match (parts.directives, parts.values) {
    (directives, Some(enum_values_definition)) => EnumTypeExtensionData::Values {
      directives,
      enum_values_definition,
    },
    (Some(directives), None) => EnumTypeExtensionData::Directives(directives),
    (None, None) => {
      return Err(missing(
        node,
        "directives or values for the extension to add",
      ));
    }
  };
  Ok((
    EnumTypeExtension::new(to_span(parts.extent), parts.name, data),
    parts.extent,
  ))
}

fn input_object_type_extension<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(InputObjectTypeExtension<&'src str>, TextRange)> {
  let parts = extension_parts(node, source)?;
  let data = match (parts.directives, parts.input_fields) {
    (directives, Some(fields_definition)) => InputObjectTypeExtensionData::Fields {
      directives,
      fields_definition,
    },
    (Some(directives), None) => InputObjectTypeExtensionData::Directives(directives),
    (None, None) => {
      return Err(missing(
        node,
        "directives or fields for the extension to add",
      ));
    }
  };
  Ok((
    InputObjectTypeExtension::new(to_span(parts.extent), parts.name, data),
    parts.extent,
  ))
}

/// The one extension with no name: `extend schema` names nothing, so this walks the node itself
/// rather than going through [`extension_parts`].
fn schema_extension<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(SchemaExtension<&'src str>, TextRange)> {
  let mut extent = Extent::default();
  let mut directives_node = None;
  let mut roots_node = None;
  for element in node.children() {
    match element {
      NodeOrToken::Token(token) => extent.token(token),
      NodeOrToken::Node(child) => match child.kind() {
        K::Directives if directives_node.is_none() => directives_node = Some(child),
        K::RootOperationTypeDefinitions if roots_node.is_none() => roots_node = Some(child),
        _ => extent.unread(child),
      },
    }
  }
  let directives = extent.keep_opt(optional_const_directives(directives_node, source)?);
  let roots = match roots_node {
    Some(block) => Some(extent.keep(root_operation_types(block, source)?)),
    None => None,
  };
  let data = match (directives, roots) {
    (directives, Some(root_operation_types_definition)) => SchemaExtensionData::Operations {
      directives,
      root_operation_types_definition,
    },
    (Some(directives), None) => SchemaExtensionData::Directives(directives),
    (None, None) => {
      return Err(missing(
        node,
        "directives or root operation types for the extension to add",
      ));
    }
  };
  let extent = extent.range(node, "a token")?;
  Ok((SchemaExtension::new(to_span(extent), data), extent))
}
