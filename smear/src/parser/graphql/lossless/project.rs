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
//! use smear::parser::graphql::lossless::{parse_document, project};
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
//! The AST is keyed by `S = &'src str` — the syntactic parser has the same property — and
//! rowan's cursor API cannot lend a `&str` that outlives the transient [`SyntaxToken`] it came
//! from, so the slices cannot be borrowed from the green tree. The caller who wants "the AST
//! without re-parsing" is holding the source by construction, so it is an argument.
//!
//! It is **verified, not trusted**: every slice is compared against its token's own text before
//! any constructor sees it, and a mismatch is [`ProjectErrorKind::SourceMismatch`] rather than
//! a silently wrong AST pointing into unrelated bytes.
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
//! # Spans
//!
//! Every span is the **token extent** of the constituents it covers — never
//! [`SyntaxNode::text_range`], which includes committed trivia. See
//! [`crate::parser::lossless::project`] for that rule and the measurement behind it.
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

use std::{boxed::Box, vec::Vec};

use rowan::{NodeOrToken, TextRange};
use tokora::SimpleSpan;

use crate::{
  lexer::{
    LitStr,
    graphql::{ContextualKeyword, keyword::contextual_keyword},
    keywords::{Mutation, Query, Subscription},
  },
  parser::{
    graphql::{
      ast::{
        Alias, Argument, Arguments, BooleanValue, ConstArgument, ConstArguments, ConstDirective,
        ConstDirectives, ConstInputValue, ConstList, ConstObject, ConstObjectField,
        DefaultInputValue, DefinitionOrExtension, Described, DescribedExecutableDefinition,
        DescribedVariableDefinition, Directive, Directives, Document, EnumTypeDefinition,
        EnumTypeExtension, EnumValue, EnumValuesDefinition, ExecutableDefinition,
        ExecutableDocument, Field, FieldsDefinition, FloatValue, FragmentName, FragmentSpread,
        ImplementInterfaces, InlineFragment, InputFieldsDefinition, InputObjectTypeDefinition,
        InputObjectTypeExtension, InputValue, IntValue, InterfaceTypeDefinition,
        InterfaceTypeExtension, List, ListType, Location, Name, NamedOperationDefinition,
        NamedType, NullValue, Object, ObjectField, ObjectTypeDefinition, ObjectTypeExtension,
        OperationDefinition, OperationType, RootOperationTypeDefinition,
        RootOperationTypesDefinition, ScalarTypeDefinition, ScalarTypeExtension, SchemaDefinition,
        SchemaExtension, Selection, SelectionSet, StringValue, Type, TypeCondition, TypeDefinition,
        TypeExtension, TypeSystemDefinition, TypeSystemExtension, UnionMemberTypes,
        UnionTypeDefinition, UnionTypeExtension, VariableDefinition, VariableValue,
        VariablesDefinition,
      },
      kinds::SyntaxKind,
      lossless::{Parse, SyntaxNode, SyntaxToken},
      syntactic::definition::classify_location,
    },
    lossless::project::{Recovery, extent_of, node_extent, to_range, to_span, verify_slice},
  },
};

// Spelled out rather than folded into the group above, so `tests/lossless_isolation.rs`'s source
// census — which reads `crate::…` prefixes out of the text — can see the edge. These are the
// **shared, dialect-free** AST carriers: the undescribed cores three `Described<…>` aliases wrap,
// and the six `…Data` enums an extension's alternatives are encoded in. Neither has a spelling
// under `graphql::ast`, and a projection has to construct both.
use crate::parser::type_system::{
  ArgumentsDefinition, DirectiveDefinition, DirectiveLocations, EnumTypeExtensionData,
  EnumValueDefinition, FieldDefinition, InputObjectTypeExtensionData, InputValueDefinition,
  InterfaceTypeExtensionData, ObjectTypeExtensionData, SchemaExtensionData, UnionTypeExtensionData,
};

use SyntaxKind as K;

/// A refusal from the GraphQL projection, keyed by this dialect's [`SyntaxKind`].
pub type ProjectError = crate::parser::lossless::project::ProjectError<SyntaxKind>;

/// Why the GraphQL projection refused, keyed by this dialect's [`SyntaxKind`].
pub type ProjectErrorKind = crate::parser::lossless::project::ProjectErrorKind<SyntaxKind>;

type Out<T> = Result<T, ProjectError>;

/// Project a lossless parse to the AST the syntactic parser produces for `source`.
///
/// The root this reads is the mixed [`Document`](SyntaxKind::Document) that
/// [`parse_document`](super::parse_document) builds.
///
/// See the module header for the contract; the short version is: check
/// [`has_errors`](Parse::has_errors) first, pass the same text the tree was parsed from, and
/// expect a refusal rather than a guess when the tree carries a hole.
pub fn project<'src>(parse: &Parse, source: &'src str) -> Out<Document<&'src str>> {
  let root = parse.syntax();
  reject_holes(&root)?;
  let node = root
    .children()
    .find(|child| child.kind() == K::Document)
    .ok_or_else(|| {
      ProjectError::new(
        ProjectErrorKind::MissingChild {
          parent: root.kind(),
          wanted: "a document",
        },
        to_range(root.text_range()),
      )
    })?;
  document(&node, source)
}

impl super::ast::Document {
  /// Project this document node to the AST the syntactic parser produces for `source`.
  ///
  /// The compositional form of [`project`], for a caller that already holds the typed wrapper.
  /// Unlike [`project`] it does **not** scan for recovery holes up front — a hole inside the
  /// subtree still refuses when the walk reaches it, but a hole elsewhere in the parse is not
  /// this node's business.
  pub fn to_ast<'src>(&self, source: &'src str) -> Out<Document<&'src str>> {
    document(self.syntax(), source)
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
/// use smear::parser::graphql::lossless::{parse_executable_document, project_executable_document};
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
  let root = parse.syntax();
  reject_holes(&root)?;
  let node = executable_root(&root).ok_or_else(|| {
    ProjectError::new(
      ProjectErrorKind::MissingChild {
        parent: root.kind(),
        wanted: "an executable document",
      },
      to_range(root.text_range()),
    )
  })?;
  executable_document(&node, source)
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
/// The definitions of the [`ExecutableDocument`](SyntaxKind::ExecutableDocument) node when the
/// parse has one — and the children of the tree's [`Root`](SyntaxKind::Root) when it does not.
/// The second case is not hypothetical: the lost-node recovery class drops a failed document
/// production's children straight under the root, so `"{ a }\nquery Bad("` has no document node
/// at all and its one good operation is reachable only this way.
///
/// ```
/// # #[cfg(all(feature = "graphql", feature = "rowan"))] {
/// use smear::parser::graphql::lossless::{
///   parse_executable_document, project_executable_document_recovered,
/// };
///
/// // The second operation is half-typed; the first one is not.
/// let source = "{ hero { name } }\nquery Bad(";
/// let parse = parse_executable_document(source);
/// assert!(parse.has_errors());
///
/// let (ast, recovery) = project_executable_document_recovered(&parse, source);
/// assert_eq!(ast.definitions().len(), 1);
/// assert_eq!(recovery.projected(), 1);
/// assert!(!recovery.is_complete());
/// # }
/// ```
pub fn project_executable_document_recovered<'src>(
  parse: &Parse,
  source: &'src str,
) -> (ExecutableDocument<&'src str>, Recovery) {
  let root = parse.syntax();
  let container = executable_root(&root).unwrap_or(root);

  let mut definitions = Vec::new();
  let mut skipped = 0u32;
  // The document's own span is the extent of the tokens under the definitions that **survived**,
  // not of the bytes that were dropped: an AST span is an extent of the tokens its node covers,
  // and a skipped region is not one of them.
  let mut extent: Option<TextRange> = None;
  for element in container.children_with_tokens() {
    match element {
      // Rubble the parser could not attach to a definition. Counted per token rather than per
      // run: a bound on what was lost, which is what `Recovery::skipped` promises.
      NodeOrToken::Token(token) if !is_trivia(token.kind()) => skipped = skipped.saturating_add(1),
      NodeOrToken::Token(_) => {}
      NodeOrToken::Node(child) => match recoverable_entry(&child, source) {
        Ok(entry) => {
          if let Some(piece) = node_extent(&child, is_trivia) {
            extent = Some(match extent {
              Some(seen) => seen.cover(piece),
              None => piece,
            });
          }
          definitions.push(entry);
        }
        Err(_) => skipped = skipped.saturating_add(1),
      },
    }
  }

  // With nothing projected there is no extent, and the zero-width span at the container's start
  // is the only position that is not a claim about text no node holds.
  let span = match extent {
    Some(range) => to_span(range),
    None => {
      let start = usize::from(container.text_range().start());
      SimpleSpan::new(start, start)
    }
  };
  let recovery = Recovery::new(definitions.len() as u32, skipped);
  (ExecutableDocument::new(span, definitions), recovery)
}

impl super::ast::ExecutableDocument {
  /// Project this executable-document node to the AST the syntactic parser produces for `source`.
  ///
  /// The compositional form of [`project_executable_document`], and
  /// [`Document::to_ast`](super::ast::Document::to_ast)'s twin: like it, and unlike the free
  /// function, it does **not** scan the whole parse for recovery holes up front.
  pub fn to_ast<'src>(&self, source: &'src str) -> Out<ExecutableDocument<&'src str>> {
    executable_document(self.syntax(), source)
  }
}

/// The [`ExecutableDocument`](SyntaxKind::ExecutableDocument) node under a parse's root.
fn executable_root(root: &SyntaxNode) -> Option<SyntaxNode> {
  root
    .children()
    .find(|child| child.kind() == K::ExecutableDocument)
}

/// One top-level definition, with the holes in **its own** subtree refused.
///
/// The scan is scoped rather than global on purpose. [`project_executable_document`] refuses a
/// tree carrying a hole anywhere, because a hole is a region with no AST image and a fail-fast
/// door must not silently omit one; the recovering door makes the same refusal, one definition at
/// a time, so a hole is charged to the definition that holds it and to no other. Without the
/// scan a hole would instead be *skipped* by whichever `child(node, kind)` lookup walked past it,
/// which is the data loss under a success type that both doors exist to refuse.
fn recoverable_entry<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<DescribedExecutableDefinition<&'src str>> {
  reject_holes(node)?;
  executable_entry(node, source)
}

// ---------------------------------------------------------------------------------------------
// refusals
// ---------------------------------------------------------------------------------------------

/// Refuse a tree that carries any recovery hole or gap tile.
///
/// Scanned once over the whole parse rather than per node, so the answer does not depend on
/// which nodes a particular walk happens to descend into: a hole anywhere is a region of the
/// document with no AST image, and a projection that silently omitted it would be losing data
/// under a success type.
fn reject_holes(root: &SyntaxNode) -> Out<()> {
  for node in root.descendants() {
    if matches!(node.kind(), K::Error | K::Gap) {
      let parent = node.parent().map_or_else(|| node.kind(), |p| p.kind());
      return Err(ProjectError::new(
        ProjectErrorKind::UnexpectedChild {
          parent,
          found: node.kind(),
        },
        to_range(node.text_range()),
      ));
    }
  }
  Ok(())
}

fn missing(parent: &SyntaxNode, wanted: &'static str) -> ProjectError {
  ProjectError::new(
    ProjectErrorKind::MissingChild {
      parent: parent.kind(),
      wanted,
    },
    to_range(parent.text_range()),
  )
}

fn unexpected(parent: &SyntaxNode, found: SyntaxKind, at: TextRange) -> ProjectError {
  ProjectError::new(
    ProjectErrorKind::UnexpectedChild {
      parent: parent.kind(),
      found,
    },
    to_range(at),
  )
}

fn unexpected_node(parent: &SyntaxNode, found: &SyntaxNode) -> ProjectError {
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

/// The token extent of `node`, as an AST span.
fn extent(node: &SyntaxNode) -> Out<SimpleSpan> {
  node_extent(node, is_trivia)
    .map(to_span)
    .ok_or_else(|| missing(node, "a token"))
}

/// The token extent of `node` with its leading description excluded.
///
/// The inner half of the description hoist — see the module header.
fn extent_without_description(node: &SyntaxNode) -> Out<SimpleSpan> {
  extent_of(
    node
      .children_with_tokens()
      .filter(|element| !matches!(element, NodeOrToken::Node(n) if n.kind() == K::Description)),
    is_trivia,
  )
  .map(to_span)
  .ok_or_else(|| missing(node, "a constituent other than its description"))
}

fn child(node: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxNode> {
  node.children().find(|child| child.kind() == kind)
}

fn require_child(node: &SyntaxNode, kind: SyntaxKind, wanted: &'static str) -> Out<SyntaxNode> {
  child(node, kind).ok_or_else(|| missing(node, wanted))
}

/// This node's own direct `Name` tokens, in document order.
///
/// Direct children only: a name inside a child node belongs to that child, and a descendant
/// scan would let a parent answer with it.
fn name_tokens(node: &SyntaxNode) -> impl Iterator<Item = SyntaxToken> + '_ {
  node
    .children_with_tokens()
    .filter_map(NodeOrToken::into_token)
    .filter(|token| token.kind() == K::Name)
}

fn name_token_at(node: &SyntaxNode, index: usize, wanted: &'static str) -> Out<SyntaxToken> {
  name_tokens(node)
    .nth(index)
    .ok_or_else(|| missing(node, wanted))
}

fn name<'src>(source: &'src str, token: &SyntaxToken) -> Out<Name<&'src str>> {
  Ok(Name::new(
    to_span(token.text_range()),
    verify_slice(source, token)?,
  ))
}

/// The `Name` of a node whose only content is one — `NamedType`, `EnumValue`, `Variable`.
fn inner_name<'src>(node: &SyntaxNode, source: &'src str) -> Out<Name<&'src str>> {
  let token = name_token_at(node, 0, "a name")?;
  name(source, &token)
}

/// The keyword a `Name` token spells, classified through the lexer's own table.
fn keyword_of(token: &SyntaxToken) -> Option<ContextualKeyword> {
  contextual_keyword(token.text().as_bytes())
}

// ---------------------------------------------------------------------------------------------
// document
// ---------------------------------------------------------------------------------------------

fn document<'src>(node: &SyntaxNode, source: &'src str) -> Out<Document<&'src str>> {
  let span = extent(node)?;
  let mut definitions = Vec::new();
  for element in node.children_with_tokens() {
    match element {
      // Rubble: the lost-node recovery class drops a failed definition's bytes straight under
      // the document, where the AST has no place for them.
      NodeOrToken::Token(token) if !is_trivia(token.kind()) => {
        return Err(unexpected(node, token.kind(), token.text_range()));
      }
      NodeOrToken::Token(_) => {}
      NodeOrToken::Node(child) => definitions.push(document_entry(&child, source)?),
    }
  }
  Ok(Document::new(span, definitions))
}

fn document_entry<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<DefinitionOrExtension<&'src str>> {
  if let Some(extension) = type_system_extension(node, source)? {
    return Ok(DefinitionOrExtension::Extension(extension));
  }
  let outer = extent(node)?;
  let description = description(node, source)?;
  let inner = extent_without_description(node)?;
  let definition = definition(node, inner, source)?;
  Ok(DefinitionOrExtension::Definition(Described::new(
    outer,
    description,
    definition,
  )))
}

/// [`document`]'s executable-only twin, over the `ExecutableDefinition+` root.
fn executable_document<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<ExecutableDocument<&'src str>> {
  let span = extent(node)?;
  let mut definitions = Vec::new();
  for element in node.children_with_tokens() {
    match element {
      // Rubble, exactly as at the mixed root and for the same reason.
      NodeOrToken::Token(token) if !is_trivia(token.kind()) => {
        return Err(unexpected(node, token.kind(), token.text_range()));
      }
      NodeOrToken::Token(_) => {}
      NodeOrToken::Node(child) => definitions.push(executable_entry(&child, source)?),
    }
  }
  Ok(ExecutableDocument::new(span, definitions))
}

/// [`document_entry`]'s executable-only twin.
///
/// No extension arm — `extend` is not executable syntax and the root that produced this node
/// reports one rather than building it — and the description hoist is the document-level one: the
/// wrapper spans description-through-definition and the inner node starts after the description.
/// A standard executable definition carries no description at all, so on standard input the two
/// spans coincide and the hoist is the dialect-compatibility path only.
fn executable_entry<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<DescribedExecutableDefinition<&'src str>> {
  let outer = extent(node)?;
  let description = description(node, source)?;
  let inner = extent_without_description(node)?;
  let definition = executable_definition(node, inner, source)?;
  Ok(Described::new(outer, description, definition))
}

fn executable_definition<'src>(
  node: &SyntaxNode,
  span: SimpleSpan,
  source: &'src str,
) -> Out<ExecutableDefinition<&'src str>> {
  Ok(match node.kind() {
    K::OperationDefinition => {
      ExecutableDefinition::Operation(operation_definition(node, span, source)?)
    }
    K::FragmentDefinition => {
      ExecutableDefinition::Fragment(fragment_definition(node, span, source)?)
    }
    found => return Err(unexpected(node, found, node.text_range())),
  })
}

fn definition<'src>(
  node: &SyntaxNode,
  span: SimpleSpan,
  source: &'src str,
) -> Out<crate::parser::graphql::ast::Definition<&'src str>> {
  use crate::parser::graphql::ast::Definition as D;

  Ok(match node.kind() {
    K::OperationDefinition => D::Executable(ExecutableDefinition::Operation(operation_definition(
      node, span, source,
    )?)),
    K::FragmentDefinition => D::Executable(ExecutableDefinition::Fragment(fragment_definition(
      node, span, source,
    )?)),
    K::ScalarTypeDefinition => D::TypeSystem(TypeSystemDefinition::Type(TypeDefinition::Scalar(
      scalar_type_definition(node, span, source)?,
    ))),
    K::ObjectTypeDefinition => D::TypeSystem(TypeSystemDefinition::Type(TypeDefinition::Object(
      object_type_definition(node, span, source)?,
    ))),
    K::InterfaceTypeDefinition => D::TypeSystem(TypeSystemDefinition::Type(
      TypeDefinition::Interface(interface_type_definition(node, span, source)?),
    )),
    K::UnionTypeDefinition => D::TypeSystem(TypeSystemDefinition::Type(TypeDefinition::Union(
      union_type_definition(node, span, source)?,
    ))),
    K::EnumTypeDefinition => D::TypeSystem(TypeSystemDefinition::Type(TypeDefinition::Enum(
      enum_type_definition(node, span, source)?,
    ))),
    K::InputObjectTypeDefinition => D::TypeSystem(TypeSystemDefinition::Type(
      TypeDefinition::InputObject(input_object_type_definition(node, span, source)?),
    )),
    K::DirectiveDefinition => D::TypeSystem(TypeSystemDefinition::Directive(directive_definition(
      node, span, source,
    )?)),
    K::SchemaDefinition => D::TypeSystem(TypeSystemDefinition::Schema(schema_definition(
      node, span, source,
    )?)),
    found => return Err(unexpected(node, found, node.text_range())),
  })
}

/// `Some` when `node` is one of the seven extension kinds, `None` when it is anything else.
///
/// An extension carries no description — `extend` heads its own production — so this answers
/// before the hoist rather than inside it.
fn type_system_extension<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<Option<TypeSystemExtension<&'src str>>> {
  let span = |node: &SyntaxNode| extent(node);
  Ok(Some(match node.kind() {
    K::ScalarTypeExtension => TypeSystemExtension::Type(TypeExtension::Scalar(
      scalar_type_extension(node, span(node)?, source)?,
    )),
    K::ObjectTypeExtension => TypeSystemExtension::Type(TypeExtension::Object(
      object_type_extension(node, span(node)?, source)?,
    )),
    K::InterfaceTypeExtension => TypeSystemExtension::Type(TypeExtension::Interface(
      interface_type_extension(node, span(node)?, source)?,
    )),
    K::UnionTypeExtension => TypeSystemExtension::Type(TypeExtension::Union(union_type_extension(
      node,
      span(node)?,
      source,
    )?)),
    K::EnumTypeExtension => TypeSystemExtension::Type(TypeExtension::Enum(enum_type_extension(
      node,
      span(node)?,
      source,
    )?)),
    K::InputObjectTypeExtension => TypeSystemExtension::Type(TypeExtension::InputObject(
      input_object_type_extension(node, span(node)?, source)?,
    )),
    K::SchemaExtension => TypeSystemExtension::Schema(schema_extension(node, span(node)?, source)?),
    _ => return Ok(None),
  }))
}

// ---------------------------------------------------------------------------------------------
// descriptions
// ---------------------------------------------------------------------------------------------

fn description<'src>(node: &SyntaxNode, source: &'src str) -> Out<Option<StringValue<&'src str>>> {
  match child(node, K::Description) {
    Some(description) => {
      let token = description
        .children_with_tokens()
        .filter_map(NodeOrToken::into_token)
        .find(|token| matches!(token.kind(), K::String | K::BlockString))
        .ok_or_else(|| missing(&description, "a string token"))?;
      Ok(Some(string_value(&token, source)?))
    }
    None => Ok(None),
  }
}

/// Re-cook a string literal through the **same** door the lexer's payload comes from.
///
/// [`LitStr`]'s `TryFrom<&str>` is the string lexer, so the `Plain`/`Complex` discriminant and
/// the `required_capacity` a consumer allocates against are the lexer's answers rather than a
/// second implementation of the escape rules. A refusal here means the two disagree about bytes
/// the lossless lexer already accepted, which is a lexer finding, not a projection one.
fn string_value<'src>(token: &SyntaxToken, source: &'src str) -> Out<StringValue<&'src str>> {
  let slice = verify_slice(source, token)?;
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
  node: &SyntaxNode,
  span: SimpleSpan,
  source: &'src str,
) -> Out<OperationDefinition<&'src str>> {
  let selection_set_node = require_child(node, K::SelectionSet, "a selection set")?;
  let selections = selection_set(&selection_set_node, source)?;

  let Some(operation_type_node) = child(node, K::OperationType) else {
    // Query shorthand: the definition *is* its selection set, and the AST's span for it is the
    // selection set's own — which is why this arm ignores the caller's.
    return Ok(OperationDefinition::Shorthand(selections));
  };

  let operation_type = operation_type(&operation_type_node, source)?;
  let name = match name_tokens(node).next() {
    Some(token) => Some(name(source, &token)?),
    None => None,
  };
  let variables = match child(node, K::VariablesDefinition) {
    Some(node) => Some(variables_definition(&node, source)?),
    None => None,
  };
  let directives = optional_directives(node, source)?;

  Ok(OperationDefinition::Named(NamedOperationDefinition::new(
    span,
    operation_type,
    name,
    variables,
    directives,
    selections,
  )))
}

fn operation_type(node: &SyntaxNode, source: &str) -> Out<OperationType> {
  let token = name_token_at(node, 0, "an operation keyword")?;
  verify_slice(source, &token)?;
  let span = to_span(token.text_range());
  Ok(match keyword_of(&token) {
    Some(ContextualKeyword::Query) => OperationType::Query(Query::new(span)),
    Some(ContextualKeyword::Mutation) => OperationType::Mutation(Mutation::new(span)),
    Some(ContextualKeyword::Subscription) => OperationType::Subscription(Subscription::new(span)),
    _ => {
      return Err(ProjectError::new(
        ProjectErrorKind::MalformedToken { kind: token.kind() },
        to_range(token.text_range()),
      ));
    }
  })
}

fn variables_definition<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<VariablesDefinition<&'src str>> {
  let span = extent(node)?;
  let mut definitions = Vec::new();
  for child in node.children() {
    match child.kind() {
      K::VariableDefinition => definitions.push(variable_definition(&child, source)?),
      _ => return Err(unexpected_node(node, &child)),
    }
  }
  Ok(VariablesDefinition::new(span, definitions))
}

fn variable_definition<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<DescribedVariableDefinition<&'src str>> {
  let outer = extent(node)?;
  let description = description(node, source)?;
  // The one described node whose inner span excludes the description — see the module header.
  let inner = extent_without_description(node)?;

  let variable_node = require_child(node, K::Variable, "a variable")?;
  let variable = variable_value(&variable_node, source)?;
  let ty = require_type(node, source)?;
  let default_value = optional_default_value(node, source)?;
  let directives = optional_const_directives(node, source)?;

  Ok(Described::new(
    outer,
    description,
    VariableDefinition::new(inner, variable, ty, default_value, directives),
  ))
}

fn fragment_definition<'src>(
  node: &SyntaxNode,
  span: SimpleSpan,
  source: &'src str,
) -> Out<crate::parser::graphql::ast::FragmentDefinition<&'src str>> {
  // `fragment` is the first `Name` token, the fragment's own name the second, `on` the third.
  let name_token = name_token_at(node, 1, "a fragment name")?;
  let name = fragment_name(&name_token, source)?;

  let on_token = name_token_at(node, 2, "an `on` keyword")?;
  verify_slice(source, &on_token)?;
  let condition_node = require_child(node, K::NamedType, "a type condition")?;
  let condition_name = inner_name(&condition_node, source)?;
  let type_condition = TypeCondition::new(
    SimpleSpan::new(
      usize::from(on_token.text_range().start()),
      condition_name.span().end(),
    ),
    condition_name,
  );

  let directives = optional_directives(node, source)?;
  let selection_set_node = require_child(node, K::SelectionSet, "a selection set")?;
  let selections = selection_set(&selection_set_node, source)?;

  Ok(crate::parser::graphql::ast::FragmentDefinition::new(
    span,
    name,
    type_condition,
    directives,
    selections,
  ))
}

/// A fragment name, with the one semantic rule the tree records only as a diagnostic.
///
/// `FragmentName::new` is deliberately crate-private so the syntactic productions are the single
/// place that establishes `Name but not on`. This is the second custodian, and it exists because
/// the lossless productions record the violation on the diagnostic channel and still build the
/// node — so the shape alone cannot tell a legal fragment name from an illegal one.
fn fragment_name<'src>(token: &SyntaxToken, source: &'src str) -> Out<FragmentName<&'src str>> {
  let slice = verify_slice(source, token)?;
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

fn selection_set<'src>(node: &SyntaxNode, source: &'src str) -> Out<SelectionSet<&'src str>> {
  let span = extent(node)?;
  let mut selections = Vec::new();
  for child in node.children() {
    selections.push(match child.kind() {
      K::Field => Selection::Field(field(&child, source)?),
      K::FragmentSpread => Selection::FragmentSpread(fragment_spread(&child, source)?),
      K::InlineFragment => Selection::InlineFragment(inline_fragment(&child, source)?),
      _ => return Err(unexpected_node(node, &child)),
    });
  }
  Ok(SelectionSet::new(span, selections))
}

fn field<'src>(node: &SyntaxNode, source: &'src str) -> Out<Field<&'src str>> {
  let span = extent(node)?;
  let alias = match child(node, K::Alias) {
    Some(alias_node) => Some(Alias::new(
      // The alias node holds the `:`, and the AST's alias span holds it too.
      extent(&alias_node)?,
      inner_name(&alias_node, source)?,
    )),
    None => None,
  };
  // The alias's own `Name` lives inside the `Alias` node, so a direct scan answers the field's.
  let name_token = name_token_at(node, 0, "a field name")?;
  let name = name(source, &name_token)?;
  let arguments = optional_arguments(node, source)?;
  let directives = optional_directives(node, source)?;
  let selections = match child(node, K::SelectionSet) {
    Some(set) => Some(selection_set(&set, source)?),
    None => None,
  };
  Ok(Field::new(
    span, alias, name, arguments, directives, selections,
  ))
}

fn fragment_spread<'src>(node: &SyntaxNode, source: &'src str) -> Out<FragmentSpread<&'src str>> {
  let span = extent(node)?;
  let name_token = name_token_at(node, 0, "a fragment name")?;
  let name = fragment_name(&name_token, source)?;
  let directives = optional_directives(node, source)?;
  Ok(FragmentSpread::new(span, name, directives))
}

fn inline_fragment<'src>(node: &SyntaxNode, source: &'src str) -> Out<InlineFragment<&'src str>> {
  let span = extent(node)?;
  let type_condition = match child(node, K::NamedType) {
    Some(condition_node) => {
      let on_token = name_token_at(node, 0, "an `on` keyword")?;
      verify_slice(source, &on_token)?;
      let condition_name = inner_name(&condition_node, source)?;
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
  let directives = optional_directives(node, source)?;
  let set = require_child(node, K::SelectionSet, "a selection set")?;
  let selections = selection_set(&set, source)?;
  Ok(InlineFragment::new(
    span,
    type_condition,
    directives,
    selections,
  ))
}

// ---------------------------------------------------------------------------------------------
// type references
// ---------------------------------------------------------------------------------------------

const TYPE_KINDS: [SyntaxKind; 3] = [K::NamedType, K::ListType, K::NonNullType];

fn type_child(node: &SyntaxNode) -> Option<SyntaxNode> {
  node
    .children()
    .find(|child| TYPE_KINDS.contains(&child.kind()))
}

fn require_type<'src>(node: &SyntaxNode, source: &'src str) -> Out<Type<Name<&'src str>>> {
  let child = type_child(node).ok_or_else(|| missing(node, "a type reference"))?;
  ty(&child, source)
}

/// The `!` folds into the node it wraps, exactly as the syntactic parser folds it.
///
/// A `NonNullType` has no AST image of its own: `T!` is a `NamedType` with `required` set, and
/// its span is the extent that includes the `!`.
fn ty<'src>(node: &SyntaxNode, source: &'src str) -> Out<Type<Name<&'src str>>> {
  let span = extent(node)?;
  match node.kind() {
    K::NamedType => Ok(Type::Name(NamedType::new(
      span,
      inner_name(node, source)?,
      false,
    ))),
    K::ListType => Ok(Type::List(Box::new(ListType::new(
      span,
      require_type(node, source)?,
      false,
    )))),
    K::NonNullType => {
      let inner = type_child(node).ok_or_else(|| missing(node, "a wrapped type"))?;
      match inner.kind() {
        K::NamedType => Ok(Type::Name(NamedType::new(
          span,
          inner_name(&inner, source)?,
          true,
        ))),
        K::ListType => Ok(Type::List(Box::new(ListType::new(
          span,
          require_type(&inner, source)?,
          true,
        )))),
        // `T!!` has no production, so a nested `NonNullType` is not a shape the AST can hold.
        found => Err(unexpected(node, found, inner.text_range())),
      }
    }
    found => Err(unexpected(node, found, node.text_range())),
  }
}

// ---------------------------------------------------------------------------------------------
// directives and arguments
// ---------------------------------------------------------------------------------------------

/// The `Directives` node exists only where at least one directive was written, and the AST
/// records an absent run as `None` — so the two agree without a zero-width placeholder.
fn optional_directives<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<Option<Directives<&'src str>>> {
  match child(node, K::Directives) {
    Some(run) => {
      let span = extent(&run)?;
      let mut directives = Vec::new();
      for child in run.children() {
        match child.kind() {
          K::Directive => directives.push(directive(&child, source)?),
          _ => return Err(unexpected_node(&run, &child)),
        }
      }
      Ok(Some(Directives::new(span, directives)))
    }
    None => Ok(None),
  }
}

fn directive<'src>(node: &SyntaxNode, source: &'src str) -> Out<Directive<&'src str>> {
  let span = extent(node)?;
  let name_token = name_token_at(node, 0, "a directive name")?;
  Ok(Directive::new(
    span,
    name(source, &name_token)?,
    optional_arguments(node, source)?,
  ))
}

fn optional_arguments<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<Option<Arguments<&'src str>>> {
  match child(node, K::Arguments) {
    Some(list) => {
      let span = extent(&list)?;
      let mut arguments = Vec::new();
      for child in list.children() {
        match child.kind() {
          K::Argument => arguments.push(argument(&child, source)?),
          _ => return Err(unexpected_node(&list, &child)),
        }
      }
      Ok(Some(Arguments::new(span, arguments)))
    }
    None => Ok(None),
  }
}

fn argument<'src>(node: &SyntaxNode, source: &'src str) -> Out<Argument<&'src str>> {
  let span = extent(node)?;
  let name_token = name_token_at(node, 0, "an argument name")?;
  let value_node = require_value(node)?;
  Ok(Argument::new(
    span,
    name(source, &name_token)?,
    value(&value_node, source)?,
  ))
}

fn optional_const_directives<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<Option<ConstDirectives<&'src str>>> {
  match child(node, K::Directives) {
    Some(run) => {
      let span = extent(&run)?;
      let mut directives = Vec::new();
      for child in run.children() {
        match child.kind() {
          K::Directive => directives.push(const_directive(&child, source)?),
          _ => return Err(unexpected_node(&run, &child)),
        }
      }
      Ok(Some(ConstDirectives::new(span, directives)))
    }
    None => Ok(None),
  }
}

fn const_directive<'src>(node: &SyntaxNode, source: &'src str) -> Out<ConstDirective<&'src str>> {
  let span = extent(node)?;
  let name_token = name_token_at(node, 0, "a directive name")?;
  Ok(ConstDirective::new(
    span,
    name(source, &name_token)?,
    optional_const_arguments(node, source)?,
  ))
}

fn optional_const_arguments<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<Option<ConstArguments<&'src str>>> {
  match child(node, K::Arguments) {
    Some(list) => {
      let span = extent(&list)?;
      let mut arguments = Vec::new();
      for child in list.children() {
        match child.kind() {
          K::Argument => arguments.push(const_argument(&child, source)?),
          _ => return Err(unexpected_node(&list, &child)),
        }
      }
      Ok(Some(ConstArguments::new(span, arguments)))
    }
    None => Ok(None),
  }
}

fn const_argument<'src>(node: &SyntaxNode, source: &'src str) -> Out<ConstArgument<&'src str>> {
  let span = extent(node)?;
  let name_token = name_token_at(node, 0, "an argument name")?;
  let value_node = require_value(node)?;
  Ok(ConstArgument::new(
    span,
    name(source, &name_token)?,
    const_value(&value_node, source)?,
  ))
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

fn require_value(node: &SyntaxNode) -> Out<SyntaxNode> {
  node
    .children()
    .find(|child| VALUE_KINDS.contains(&child.kind()))
    .ok_or_else(|| missing(node, "a value"))
}

fn value<'src>(node: &SyntaxNode, source: &'src str) -> Out<InputValue<&'src str>> {
  let span = extent(node)?;
  Ok(match node.kind() {
    K::Variable => InputValue::Variable(variable_value(node, source)?),
    K::IntValue => InputValue::Int(IntValue::new(
      span,
      literal_slice(node, source, &[K::Int], "an integer literal")?,
    )),
    K::FloatValue => InputValue::Float(FloatValue::new(
      span,
      literal_slice(node, source, &[K::Float], "a float literal")?,
    )),
    K::StringValue => InputValue::String(string_literal(node, source)?),
    K::BooleanValue => InputValue::Boolean(boolean_literal(node, source)?),
    K::NullValue => InputValue::Null(NullValue::new(
      span,
      literal_slice(node, source, &[K::Name], "a `null` keyword")?,
    )),
    K::EnumValue => InputValue::Enum(EnumValue::new(
      span,
      literal_slice(node, source, &[K::Name], "an enum value")?,
    )),
    K::ListValue => {
      let mut values = Vec::new();
      for child in node.children() {
        values.push(value(&child, source)?);
      }
      InputValue::List(List::new(span, values))
    }
    K::ObjectValue => {
      let mut fields = Vec::new();
      for child in node.children() {
        match child.kind() {
          K::ObjectField => fields.push(object_field(&child, source)?),
          _ => return Err(unexpected_node(node, &child)),
        }
      }
      InputValue::Object(Object::new(span, fields))
    }
    found => return Err(unexpected(node, found, node.text_range())),
  })
}

/// A constant value position, where the AST's own type system forbids a variable.
///
/// [`ConstInputValue`] has no `Variable` variant, so the refusal is not a policy this module
/// invented: there is nothing to construct.
fn const_value<'src>(node: &SyntaxNode, source: &'src str) -> Out<ConstInputValue<&'src str>> {
  let span = extent(node)?;
  Ok(match node.kind() {
    // The refusal is attributed to the position, not to the variable: a `Variable` node is
    // perfectly legal, and what is wrong is the const context that is holding one.
    K::Variable => {
      let parent = node.parent();
      return Err(unexpected(
        parent.as_ref().unwrap_or(node),
        K::Variable,
        node.text_range(),
      ));
    }
    K::IntValue => ConstInputValue::Int(IntValue::new(
      span,
      literal_slice(node, source, &[K::Int], "an integer literal")?,
    )),
    K::FloatValue => ConstInputValue::Float(FloatValue::new(
      span,
      literal_slice(node, source, &[K::Float], "a float literal")?,
    )),
    K::StringValue => ConstInputValue::String(string_literal(node, source)?),
    K::BooleanValue => ConstInputValue::Boolean(boolean_literal(node, source)?),
    K::NullValue => ConstInputValue::Null(NullValue::new(
      span,
      literal_slice(node, source, &[K::Name], "a `null` keyword")?,
    )),
    K::EnumValue => ConstInputValue::Enum(EnumValue::new(
      span,
      literal_slice(node, source, &[K::Name], "an enum value")?,
    )),
    K::ListValue => {
      let mut values = Vec::new();
      for child in node.children() {
        values.push(const_value(&child, source)?);
      }
      ConstInputValue::List(ConstList::new(span, values))
    }
    K::ObjectValue => {
      let mut fields = Vec::new();
      for child in node.children() {
        match child.kind() {
          K::ObjectField => fields.push(const_object_field(&child, source)?),
          _ => return Err(unexpected_node(node, &child)),
        }
      }
      ConstInputValue::Object(ConstObject::new(span, fields))
    }
    found => return Err(unexpected(node, found, node.text_range())),
  })
}

fn literal_slice<'src>(
  node: &SyntaxNode,
  source: &'src str,
  kinds: &[SyntaxKind],
  wanted: &'static str,
) -> Out<&'src str> {
  let token = node
    .children_with_tokens()
    .filter_map(NodeOrToken::into_token)
    .find(|token| kinds.contains(&token.kind()))
    .ok_or_else(|| missing(node, wanted))?;
  verify_slice(source, &token)
}

fn string_literal<'src>(node: &SyntaxNode, source: &'src str) -> Out<StringValue<&'src str>> {
  let token = node
    .children_with_tokens()
    .filter_map(NodeOrToken::into_token)
    .find(|token| matches!(token.kind(), K::String | K::BlockString))
    .ok_or_else(|| missing(node, "a string literal"))?;
  string_value(&token, source)
}

fn boolean_literal<'src>(node: &SyntaxNode, source: &'src str) -> Out<BooleanValue<&'src str>> {
  let span = extent(node)?;
  let slice = literal_slice(node, source, &[K::Name], "a `true` or `false` keyword")?;
  match slice {
    "true" => Ok(BooleanValue::new(span, true)),
    "false" => Ok(BooleanValue::new(span, false)),
    _ => Err(ProjectError::new(
      ProjectErrorKind::MalformedToken { kind: K::Name },
      to_range(node.text_range()),
    )),
  }
}

fn variable_value<'src>(node: &SyntaxNode, source: &'src str) -> Out<VariableValue<&'src str>> {
  let span = extent(node)?;
  Ok(VariableValue::new(span, inner_name(node, source)?))
}

fn object_field<'src>(node: &SyntaxNode, source: &'src str) -> Out<ObjectField<&'src str>> {
  let span = extent(node)?;
  let name_token = name_token_at(node, 0, "a field name")?;
  let value_node = require_value(node)?;
  Ok(ObjectField::new(
    span,
    name(source, &name_token)?,
    value(&value_node, source)?,
  ))
}

fn const_object_field<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<ConstObjectField<&'src str>> {
  let span = extent(node)?;
  let name_token = name_token_at(node, 0, "a field name")?;
  let value_node = require_value(node)?;
  Ok(ConstObjectField::new(
    span,
    name(source, &name_token)?,
    const_value(&value_node, source)?,
  ))
}

fn optional_default_value<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<Option<DefaultInputValue<&'src str>>> {
  match child(node, K::DefaultValue) {
    Some(default) => {
      // The span covers the `=` and the value, which is the node's own token extent.
      let span = extent(&default)?;
      let value_node = require_value(&default)?;
      Ok(Some(DefaultInputValue::new(
        span,
        const_value(&value_node, source)?,
      )))
    }
    None => Ok(None),
  }
}

// ---------------------------------------------------------------------------------------------
// SDL definitions
// ---------------------------------------------------------------------------------------------

/// The name behind a definition's keyword: `scalar S`, `type T`, `directive @d` all reach it at
/// index 1 among the node's direct `Name` tokens, the keyword being index 0.
fn keyword_named<'src>(node: &SyntaxNode, source: &'src str) -> Out<Name<&'src str>> {
  let token = name_token_at(node, 1, "a name after the keyword")?;
  name(source, &token)
}

/// An extension's name is its **third** `Name`: `extend`, the shape keyword, then the name.
fn extension_named<'src>(node: &SyntaxNode, source: &'src str) -> Out<Name<&'src str>> {
  let token = name_token_at(node, 2, "an extended type's name")?;
  name(source, &token)
}

fn optional_implements<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<Option<ImplementInterfaces<Name<&'src str>>>> {
  match child(node, K::ImplementsInterfaces) {
    Some(clause) => {
      let span = extent(&clause)?;
      let mut interfaces = Vec::new();
      for child in clause.children() {
        match child.kind() {
          // The AST holds `Name`s, not `NamedType`s: an implemented interface can carry no `!`
          // and no brackets, so the type-reference level would be a wrapper over nothing.
          K::NamedType => interfaces.push(inner_name(&child, source)?),
          _ => return Err(unexpected_node(&clause, &child)),
        }
      }
      Ok(Some(ImplementInterfaces::new(span, interfaces)))
    }
    None => Ok(None),
  }
}

fn optional_union_members<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<Option<UnionMemberTypes<Name<&'src str>>>> {
  match child(node, K::UnionMemberTypes) {
    Some(clause) => {
      let span = extent(&clause)?;
      let mut members = Vec::new();
      for child in clause.children() {
        match child.kind() {
          K::NamedType => members.push(inner_name(&child, source)?),
          _ => return Err(unexpected_node(&clause, &child)),
        }
      }
      Ok(Some(UnionMemberTypes::new(span, members)))
    }
    None => Ok(None),
  }
}

fn optional_fields_definition<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<Option<FieldsDefinition<&'src str>>> {
  match child(node, K::FieldsDefinition) {
    Some(block) => {
      let span = extent(&block)?;
      let mut fields = Vec::new();
      for child in block.children() {
        match child.kind() {
          K::FieldDefinition => fields.push(field_definition(&child, source)?),
          _ => return Err(unexpected_node(&block, &child)),
        }
      }
      Ok(Some(FieldsDefinition::new(span, fields)))
    }
    None => Ok(None),
  }
}

fn field_definition<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<crate::parser::graphql::ast::FieldDefinition<&'src str>> {
  // One span for both halves, description included — trunk's rule for this node, see the header.
  let span = extent(node)?;
  let description = description(node, source)?;
  let name_token = name_token_at(node, 0, "a field name")?;
  let arguments_definition = optional_arguments_definition(node, source)?;
  let ty = require_type(node, source)?;
  let directives = optional_const_directives(node, source)?;
  Ok(Described::new(
    span,
    description,
    FieldDefinition::new(
      span,
      name(source, &name_token)?,
      arguments_definition,
      ty,
      directives,
    ),
  ))
}

fn optional_arguments_definition<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<Option<crate::parser::graphql::ast::ArgumentsDefinition<&'src str>>> {
  match child(node, K::ArgumentsDefinition) {
    Some(block) => {
      let span = extent(&block)?;
      let mut definitions = Vec::new();
      for child in block.children() {
        match child.kind() {
          K::InputValueDefinition => definitions.push(input_value_definition(&child, source)?),
          _ => return Err(unexpected_node(&block, &child)),
        }
      }
      Ok(Some(ArgumentsDefinition::new(span, definitions)))
    }
    None => Ok(None),
  }
}

fn input_value_definition<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<crate::parser::graphql::ast::InputValueDefinition<&'src str>> {
  let span = extent(node)?;
  let description = description(node, source)?;
  let name_token = name_token_at(node, 0, "an input value name")?;
  let ty = require_type(node, source)?;
  let default_value = optional_default_value(node, source)?;
  let directives = optional_const_directives(node, source)?;
  Ok(Described::new(
    span,
    description,
    InputValueDefinition::new(
      span,
      name(source, &name_token)?,
      ty,
      default_value,
      directives,
    ),
  ))
}

fn optional_input_fields_definition<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<Option<InputFieldsDefinition<&'src str>>> {
  match child(node, K::InputFieldsDefinition) {
    Some(block) => {
      let span = extent(&block)?;
      let mut definitions = Vec::new();
      for child in block.children() {
        match child.kind() {
          K::InputValueDefinition => definitions.push(input_value_definition(&child, source)?),
          _ => return Err(unexpected_node(&block, &child)),
        }
      }
      Ok(Some(InputFieldsDefinition::new(span, definitions)))
    }
    None => Ok(None),
  }
}

fn optional_enum_values<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<Option<EnumValuesDefinition<&'src str>>> {
  match child(node, K::EnumValuesDefinition) {
    Some(block) => {
      let span = extent(&block)?;
      let mut values = Vec::new();
      for child in block.children() {
        match child.kind() {
          K::EnumValueDefinition => values.push(enum_value_definition(&child, source)?),
          _ => return Err(unexpected_node(&block, &child)),
        }
      }
      Ok(Some(EnumValuesDefinition::new(span, values)))
    }
    None => Ok(None),
  }
}

fn enum_value_definition<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<crate::parser::graphql::ast::EnumValueDefinition<&'src str>> {
  let span = extent(node)?;
  let description = description(node, source)?;
  // The value's name lives inside the `EnumValue` node the tree opens for it, but the AST holds
  // a bare `Name` — the enum-value level has nothing else to carry.
  let value_node = require_child(node, K::EnumValue, "an enum value")?;
  let value = inner_name(&value_node, source)?;
  let directives = optional_const_directives(node, source)?;
  Ok(Described::new(
    span,
    description,
    EnumValueDefinition::new(span, value, directives),
  ))
}

fn scalar_type_definition<'src>(
  node: &SyntaxNode,
  span: SimpleSpan,
  source: &'src str,
) -> Out<ScalarTypeDefinition<&'src str>> {
  Ok(ScalarTypeDefinition::new(
    span,
    keyword_named(node, source)?,
    optional_const_directives(node, source)?,
  ))
}

fn object_type_definition<'src>(
  node: &SyntaxNode,
  span: SimpleSpan,
  source: &'src str,
) -> Out<ObjectTypeDefinition<&'src str>> {
  Ok(ObjectTypeDefinition::new(
    span,
    keyword_named(node, source)?,
    optional_implements(node, source)?,
    optional_const_directives(node, source)?,
    optional_fields_definition(node, source)?,
  ))
}

fn interface_type_definition<'src>(
  node: &SyntaxNode,
  span: SimpleSpan,
  source: &'src str,
) -> Out<InterfaceTypeDefinition<&'src str>> {
  Ok(InterfaceTypeDefinition::new(
    span,
    keyword_named(node, source)?,
    optional_implements(node, source)?,
    optional_const_directives(node, source)?,
    optional_fields_definition(node, source)?,
  ))
}

fn union_type_definition<'src>(
  node: &SyntaxNode,
  span: SimpleSpan,
  source: &'src str,
) -> Out<UnionTypeDefinition<&'src str>> {
  Ok(UnionTypeDefinition::new(
    span,
    keyword_named(node, source)?,
    optional_const_directives(node, source)?,
    optional_union_members(node, source)?,
  ))
}

fn enum_type_definition<'src>(
  node: &SyntaxNode,
  span: SimpleSpan,
  source: &'src str,
) -> Out<EnumTypeDefinition<&'src str>> {
  Ok(EnumTypeDefinition::new(
    span,
    keyword_named(node, source)?,
    optional_const_directives(node, source)?,
    optional_enum_values(node, source)?,
  ))
}

fn input_object_type_definition<'src>(
  node: &SyntaxNode,
  span: SimpleSpan,
  source: &'src str,
) -> Out<InputObjectTypeDefinition<&'src str>> {
  Ok(InputObjectTypeDefinition::new(
    span,
    keyword_named(node, source)?,
    optional_const_directives(node, source)?,
    optional_input_fields_definition(node, source)?,
  ))
}

fn directive_definition<'src>(
  node: &SyntaxNode,
  span: SimpleSpan,
  source: &'src str,
) -> Out<crate::parser::graphql::ast::DirectiveDefinition<&'src str>> {
  let name = keyword_named(node, source)?;
  let arguments_definition = optional_arguments_definition(node, source)?;
  // `repeatable` is optional and `on` is not, so the token at index 2 is one or the other. The
  // spelling is read rather than the count: an index that only counted would answer `on`.
  let repeatable = name_tokens(node)
    .nth(2)
    .is_some_and(|token| keyword_of(&token) == Some(ContextualKeyword::Repeatable));
  let locations_node = require_child(node, K::DirectiveLocations, "a location list")?;
  let locations = directive_locations(&locations_node, source)?;
  Ok(DirectiveDefinition::new(
    span,
    name,
    arguments_definition,
    repeatable,
    locations,
  ))
}

fn directive_locations(node: &SyntaxNode, source: &str) -> Out<DirectiveLocations<Location>> {
  let span = extent(node)?;
  let mut locations = Vec::new();
  for token in name_tokens(node) {
    verify_slice(source, &token)?;
    let location = keyword_of(&token)
      .and_then(|keyword| classify_location(keyword, to_span(token.text_range())))
      .ok_or_else(|| {
        ProjectError::new(
          ProjectErrorKind::MalformedToken { kind: token.kind() },
          to_range(token.text_range()),
        )
      })?;
    locations.push(location);
  }
  if locations.is_empty() {
    return Err(missing(node, "a directive location"));
  }
  Ok(DirectiveLocations::new(span, locations))
}

fn schema_definition<'src>(
  node: &SyntaxNode,
  span: SimpleSpan,
  source: &'src str,
) -> Out<SchemaDefinition<&'src str>> {
  let directives = optional_const_directives(node, source)?;
  let roots_node = require_child(
    node,
    K::RootOperationTypeDefinitions,
    "a root operation types block",
  )?;
  Ok(SchemaDefinition::new(
    span,
    directives,
    root_operation_types(&roots_node, source)?,
  ))
}

fn root_operation_types<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<RootOperationTypesDefinition<&'src str>> {
  let span = extent(node)?;
  let mut roots = Vec::new();
  for child in node.children() {
    match child.kind() {
      K::RootOperationTypeDefinition => roots.push(root_operation_type(&child, source)?),
      _ => return Err(unexpected_node(node, &child)),
    }
  }
  Ok(RootOperationTypesDefinition::new(span, roots))
}

fn root_operation_type<'src>(
  node: &SyntaxNode,
  source: &'src str,
) -> Out<RootOperationTypeDefinition<&'src str>> {
  let span = extent(node)?;
  let keyword_node = require_child(node, K::OperationType, "an operation keyword")?;
  let operation_type = operation_type(&keyword_node, source)?;
  let named = require_child(node, K::NamedType, "a root type name")?;
  Ok(RootOperationTypeDefinition::new(
    span,
    operation_type,
    inner_name(&named, source)?,
  ))
}

// ---------------------------------------------------------------------------------------------
// SDL extensions
// ---------------------------------------------------------------------------------------------

fn scalar_type_extension<'src>(
  node: &SyntaxNode,
  span: SimpleSpan,
  source: &'src str,
) -> Out<ScalarTypeExtension<&'src str>> {
  let name = extension_named(node, source)?;
  // The one extension whose directives the grammar makes mandatory: it has no other tail.
  let directives = optional_const_directives(node, source)?
    .ok_or_else(|| missing(node, "the directives a scalar extension must add"))?;
  Ok(ScalarTypeExtension::new(span, name, directives))
}

fn object_type_extension<'src>(
  node: &SyntaxNode,
  span: SimpleSpan,
  source: &'src str,
) -> Out<ObjectTypeExtension<&'src str>> {
  let name = extension_named(node, source)?;
  let implements = optional_implements(node, source)?;
  let directives = optional_const_directives(node, source)?;
  let fields_definition = optional_fields_definition(node, source)?;
  let data = match (implements, directives, fields_definition) {
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
  Ok(ObjectTypeExtension::new(span, name, data))
}

fn interface_type_extension<'src>(
  node: &SyntaxNode,
  span: SimpleSpan,
  source: &'src str,
) -> Out<InterfaceTypeExtension<&'src str>> {
  let name = extension_named(node, source)?;
  let implements = optional_implements(node, source)?;
  let directives = optional_const_directives(node, source)?;
  let fields_definition = optional_fields_definition(node, source)?;
  let data = match (implements, directives, fields_definition) {
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
  Ok(InterfaceTypeExtension::new(span, name, data))
}

fn union_type_extension<'src>(
  node: &SyntaxNode,
  span: SimpleSpan,
  source: &'src str,
) -> Out<UnionTypeExtension<&'src str>> {
  let name = extension_named(node, source)?;
  let directives = optional_const_directives(node, source)?;
  let member_types = optional_union_members(node, source)?;
  let data = match (directives, member_types) {
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
  Ok(UnionTypeExtension::new(span, name, data))
}

fn enum_type_extension<'src>(
  node: &SyntaxNode,
  span: SimpleSpan,
  source: &'src str,
) -> Out<EnumTypeExtension<&'src str>> {
  let name = extension_named(node, source)?;
  let directives = optional_const_directives(node, source)?;
  let enum_values_definition = optional_enum_values(node, source)?;
  let data = match (directives, enum_values_definition) {
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
  Ok(EnumTypeExtension::new(span, name, data))
}

fn input_object_type_extension<'src>(
  node: &SyntaxNode,
  span: SimpleSpan,
  source: &'src str,
) -> Out<InputObjectTypeExtension<&'src str>> {
  let name = extension_named(node, source)?;
  let directives = optional_const_directives(node, source)?;
  let fields_definition = optional_input_fields_definition(node, source)?;
  let data = match (directives, fields_definition) {
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
  Ok(InputObjectTypeExtension::new(span, name, data))
}

fn schema_extension<'src>(
  node: &SyntaxNode,
  span: SimpleSpan,
  source: &'src str,
) -> Out<SchemaExtension<&'src str>> {
  let directives = optional_const_directives(node, source)?;
  let roots = match child(node, K::RootOperationTypeDefinitions) {
    Some(block) => Some(root_operation_types(&block, source)?),
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
  Ok(SchemaExtension::new(span, data))
}
