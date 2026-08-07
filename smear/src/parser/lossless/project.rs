//! The dialect-free substrate for the CST → AST projection.
//!
//! # What a projection is, and why one exists
//!
//! A consumer that parsed **losslessly** holds a rowan tree: every byte, every comment, every
//! comma. A consumer that parsed **syntactically** holds an AST: no trivia, cooked literals,
//! typed nodes. An editor needs the first (to format, to highlight, to edit incrementally) and
//! a validator needs the second (issue #85 consumes `ast::ExecutableDocument` directly and
//! deliberately rejected a shared view trait). A projection is the door between them, so the
//! IDE path does not have to parse the same bytes twice.
//!
//! This module owns everything about that door which is independent of a grammar: the error
//! type, the span arithmetic, and the slice check. Everything that names a node kind, a
//! wrapper or an AST target lives in a dialect's own `lossless::project`.
//!
//! # The span rule, in one sentence
//!
//! **A composite node's span is the extent of the tokens it contains** — its first token's
//! start to its last token's end, trivia excluded at both ends. That is the syntactic parser's
//! own rule since #72 (`tests/syntactic_span_extent.rs` pins it over the padded corpus), so a
//! projection that folds token extents lands on the same numbers the parser does, and the
//! differential gate compares with plain `==`.
//!
//! It is emphatically **not** [`rowan::SyntaxNode::text_range`]. A CST node's range is the
//! extent of everything committed *inside* it, trivia included: for `"type T  { f : Int }"`
//! the tree's `FieldDefinition` runs `12..20` — it holds the space after `Int` — where the
//! AST's is `12..19`. The document node is worse: its range covers the file's leading and
//! trailing trivia, which no AST span ever does. [`node_extent`] is the correction, and it is
//! the only span door a projection should use.

use core::{fmt, ops::Range};

use rowan::{Language, SyntaxElement, SyntaxNode, SyntaxToken, TextRange};
use tokora::SimpleSpan;

/// Why a projection refused.
///
/// Positioned, single, and fail-fast. The projection is **not** a diagnostics channel: the
/// parse's own diagnostics already exist on `Parse`, and a second, subtly different set would
/// drift from them by construction. One typed refusal, at the first obstruction.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum ProjectErrorKind<K> {
  /// A constituent the AST shape requires is absent from the tree.
  ///
  /// The recovered-in-place class: `type T { x: }` keeps its `FieldDefinition` node and hangs
  /// an `Error` hole where the type should be, so the node exists and its type does not.
  MissingChild {
    /// The node kind that is missing a constituent.
    parent: K,
    /// What was wanted, in the grammar's vocabulary.
    wanted: &'static str,
  },
  /// An element the AST shape has no place for.
  ///
  /// Three sources, all real: a recovery hole or gap tile anywhere the projection walks; the
  /// rubble a failed definition leaves as bare children of the document; and a `Variable` in a
  /// constant position, which the AST's own type system forbids (`ConstInputValue` has no
  /// `Variable` variant).
  UnexpectedChild {
    /// The node kind whose children were being read.
    parent: K,
    /// The kind that has no place there.
    found: K,
  },
  /// A token was present but would not cook.
  ///
  /// Today this is reachable only for string literals, which are re-lexed through the same
  /// `impl TryFrom<&str> for LitStr` door the syntactic lexer's payload comes from. An
  /// internal-inconsistency class: the lossless lexer already accepted these bytes.
  MalformedToken {
    /// The token kind that refused.
    kind: K,
  },
  /// `source` is not the text this tree was parsed from.
  ///
  /// Every slice the projection takes is compared against the token's own text before it is
  /// handed to a constructor, so a mismatched pair is refused rather than silently projected
  /// into a wrong AST.
  SourceMismatch,
  /// A grammar rule the tree records only as a diagnostic.
  ///
  /// Today exactly one: a fragment may not be named `on`. The lossless productions record that
  /// as an error diagnostic and still build the node, so the shape alone cannot tell the two
  /// apart and the projection re-checks it — the second in-crate custodian of the invariant
  /// `FragmentName::new` is kept crate-private to protect.
  SemanticRule {
    /// The rule, named for a human.
    rule: &'static str,
  },
}

/// A projection refusal, with the byte range of the element that caused it.
///
/// `span` uses the same `Range<usize>` vocabulary as `Diagnostic::span`, so a consumer routing
/// a refusal into an editor's diagnostic channel does not have to convert.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProjectError<K> {
  kind: ProjectErrorKind<K>,
  span: Range<usize>,
}

impl<K> ProjectError<K> {
  /// Builds a refusal at `span`.
  #[inline]
  pub const fn new(kind: ProjectErrorKind<K>, span: Range<usize>) -> Self {
    Self { kind, span }
  }

  /// Why the projection refused.
  #[inline]
  pub const fn kind(&self) -> &ProjectErrorKind<K> {
    &self.kind
  }

  /// The byte range of the obstructing element.
  #[inline]
  pub const fn span(&self) -> &Range<usize> {
    &self.span
  }
}

impl<K: fmt::Debug> fmt::Display for ProjectError<K> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let Range { start, end } = self.span;
    match &self.kind {
      ProjectErrorKind::MissingChild { parent, wanted } => write!(
        f,
        "{start}..{end}: {parent:?} has no {wanted}, so no AST node can be built for it"
      ),
      ProjectErrorKind::UnexpectedChild { parent, found } => write!(
        f,
        "{start}..{end}: {found:?} has no place inside {parent:?}"
      ),
      ProjectErrorKind::MalformedToken { kind } => write!(
        f,
        "{start}..{end}: the {kind:?} token did not cook, though the lossless lexer accepted it"
      ),
      ProjectErrorKind::SourceMismatch => write!(
        f,
        "{start}..{end}: the source text is not what this tree was parsed from"
      ),
      ProjectErrorKind::SemanticRule { rule } => write!(f, "{start}..{end}: {rule}"),
    }
  }
}

impl<K: fmt::Debug> core::error::Error for ProjectError<K> {}

/// How much of a tree a **recovering** projection could see.
///
/// The fail-fast projection answers `Result<Ast, ProjectError>`: one refusal, at the first
/// obstruction, for a caller that wants the AST or nothing. A recovering projection answers the
/// question an editor asks instead — *what does the part that is still well-formed say?* — and
/// this is the honesty half of that answer.
///
/// # Read it before you read the verdict
///
/// A consumer that skips it is reading a statement about **some** of the document as though it
/// were a statement about all of it. `skipped() > 0` means at least one top-level element had no
/// AST image, so:
///
/// - an **absence** of findings is weaker than it looks — nothing examined what was skipped; and
/// - a **presence** of findings may include an artifact of the skip, because a rule that reads
///   the document as a whole (an undefined fragment spread, an unused fragment) cannot tell a
///   definition that was never written from one that was dropped.
///
/// [`is_complete`](Self::is_complete) is the one-call form of that question, and it is the only
/// state in which the recovering answer and the fail-fast one are the same value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Recovery {
  projected: u32,
  skipped: u32,
}

impl Recovery {
  /// Builds a recovery tally.
  #[inline]
  pub const fn new(projected: u32, skipped: u32) -> Self {
    Self { projected, skipped }
  }

  /// Returns how many top-level definitions were projected into the AST.
  #[inline]
  pub const fn projected(&self) -> u32 {
    self.projected
  }

  /// Returns how many top-level elements had no AST image and were dropped.
  ///
  /// An *element*, not a definition: the count includes a definition the projection refused, a
  /// recovery hole or gap tile the parser left in the definition's place, and any rubble the
  /// parser could not attach to a definition at all. One mistyped keyword can therefore leave
  /// more than one behind, so this is evidence that something was dropped and a bound on how
  /// much — not a count of the constructs the author meant to write.
  #[inline]
  pub const fn skipped(&self) -> u32 {
    self.skipped
  }

  /// Returns whether every top-level element had an AST image.
  ///
  /// When it is true the recovering projection produced exactly what the fail-fast one would
  /// have, and anything read off the result is a statement about the whole document.
  #[inline]
  pub const fn is_complete(&self) -> bool {
    self.skipped == 0
  }
}

impl fmt::Display for Recovery {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let Self { projected, skipped } = self;
    write!(f, "{projected} projected, {skipped} skipped")
  }
}

/// [`TextRange`] as the AST's span type.
#[inline]
pub fn to_span(range: TextRange) -> SimpleSpan {
  SimpleSpan::new(usize::from(range.start()), usize::from(range.end()))
}

/// [`TextRange`] as the error vocabulary's byte range.
#[inline]
pub fn to_range(range: TextRange) -> Range<usize> {
  usize::from(range.start())..usize::from(range.end())
}

/// The token extent of `node` — its first non-trivia token's start to its last one's end.
///
/// `None` when the subtree holds no non-trivia token at all, which for a node the grammar
/// requires to have content is itself a finding and is why this returns an `Option` rather
/// than falling back on [`SyntaxNode::text_range`].
///
/// See this module's header for why the node's own range is the wrong answer.
#[inline]
pub fn node_extent<L: Language>(
  node: &SyntaxNode<L>,
  is_trivia: impl Fn(L::Kind) -> bool + Copy,
) -> Option<TextRange> {
  extent_of(node.children_with_tokens(), is_trivia)
}

/// The token extent of a run of elements, descending into every node it contains.
///
/// The general form [`node_extent`] is written in terms of. A projection that has to exclude
/// one constituent — the description a definition node holds but the AST hoists out — folds
/// the filtered child stream through here rather than reaching for the node's range.
pub fn extent_of<L: Language, I>(
  elements: I,
  is_trivia: impl Fn(L::Kind) -> bool + Copy,
) -> Option<TextRange>
where
  I: IntoIterator<Item = SyntaxElement<L>>,
{
  let mut extent: Option<TextRange> = None;
  for element in elements {
    let piece = match element {
      SyntaxElement::Token(token) => {
        if is_trivia(token.kind()) {
          continue;
        }
        Some(token.text_range())
      }
      SyntaxElement::Node(node) => node_extent(&node, is_trivia),
    };
    if let Some(piece) = piece {
      extent = Some(match extent {
        // `cover` rather than `start..piece.end()`: a fold that assumed document order would
        // produce an inverted range the moment it was handed a stream that was not in it, and
        // an inverted span is exactly the class `tests/support/span_extent.rs` exists to catch.
        Some(seen) => seen.cover(piece),
        None => piece,
      });
    }
  }
  extent
}

/// The source text under `token`, checked against the token's own text.
///
/// The `(tree, source)` pair is a **caller** obligation — rowan's cursors cannot lend `&str`s
/// that outlive them, so the AST's slices must come from the caller's buffer — and this is
/// where that obligation is verified rather than trusted. The cost is a `memcmp` over bytes
/// the projection was going to copy a pointer to anyway; the alternative is a silently wrong
/// AST whose spans point into unrelated text.
pub fn verify_slice<'src, L: Language>(
  source: &'src str,
  token: &SyntaxToken<L>,
) -> Result<&'src str, ProjectError<L::Kind>> {
  let range = token.text_range();
  source
    .get(usize::from(range.start())..usize::from(range.end()))
    .filter(|slice| *slice == token.text())
    .ok_or_else(|| ProjectError::new(ProjectErrorKind::SourceMismatch, to_range(range)))
}
