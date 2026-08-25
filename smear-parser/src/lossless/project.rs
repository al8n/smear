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
//! type, the span arithmetic, and the two whole-tree checks a door makes before it walks.
//! Everything that names a node kind, a wrapper or an AST target lives in a dialect's own
//! `lossless::project`.
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
//! trailing trivia, which no AST span ever does.
//!
//! # What a projection walks: the green tree, not a cursor
//!
//! [`Node`] and [`Token`] are this module's traversal unit — a `&GreenNodeData` or
//! `&GreenTokenData` paired with where it starts in the source. Two words, [`Copy`], nothing
//! allocated.
//!
//! rowan's [`SyntaxNode`](rowan::SyntaxNode) is the other candidate, and what it buys over the
//! green tree is a parent pointer and an absolute offset. A projection needs the second and gets
//! the first from its own call frame: the parent a refusal names is the node whose walk reached
//! the child. It pays for both with a heap allocation per element materialised — rowan boxes
//! every node *and* token a cursor yields — which smear #120 measured at 2,447 allocations and
//! 96 ns per source byte for a document the syntactic parser builds in 18.
//!
//! What the green tree does not carry is an absolute offset, so [`Node::children`] accumulates
//! one: a child's start is the parent's start plus the lengths of its preceding siblings. That
//! accumulator only runs forward, which is why [`Children`] is not a [`DoubleEndedIterator`].
//!
//! # How a projection is expected to compute the span rule, and why it matters
//!
//! [`node_extent`] answers the rule for one node by descending its whole subtree. That is the
//! right shape for a node a projection reads *once* — an unread child whose bytes still belong
//! to the parent's span — and the **wrong** shape for the projection's own recursion: a walk
//! that calls it at every level re-visits every token once per ancestor.
//!
//! A projection therefore **folds bottom-up**: each node function walks its own
//! [`children`](Node::children) once, covers its non-trivia tokens' ranges, and covers the
//! extents its children hand back — so every element is visited by exactly one parent and
//! [`node_extent`] is left for the unread-child case it is right for.
//!
//! # The `(tree, source)` pair, checked once at the door
//!
//! The AST borrows `&'src str` from a buffer the *caller* supplies. A green token would lend its
//! own text, but that text belongs to the parse, and [`Parse`](super::runner::Parse) is
//! deliberately lifetime-free so a consumer can cache one per file and drop it on the next
//! keystroke. So nothing in the type system ties a parse to the bytes it was parsed from, and the
//! realistic misuse — an editor validating a stale buffer against a cached parse — is uncheckable
//! except by comparing.
//!
//! [`verify_source`] is that comparison, made **once per door**: a preorder walk with an offset
//! accumulator, effectively a chunked `memcmp` over the whole file. Per-token access after it is
//! plain slicing.

use core::{fmt, iter::FusedIterator, marker::PhantomData, ops::Range};

use rowan::{
  GreenNodeData, GreenTokenData, Language, NodeOrToken, SyntaxToken, TextRange, TextSize,
};
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
  /// Every byte the tree holds is compared against `source` at the door, before any walk, so a
  /// mismatched pair is refused rather than silently projected into a wrong AST. The span names
  /// the first bytes that diverge.
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
/// [`is_complete`](Self::is_complete) is the one-call form of that question. It is a statement
/// about **loss**, not about validity: it is the only state in which the AST covers the whole
/// document, and a fail-fast projection of the same parse can still refuse it — a document that is
/// empty, or nothing but trivia, lost nothing and has no definition either.
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
  /// When it is true, anything read off the result is a statement about the whole document rather
  /// than about a surviving part of it — which is the question a consumer of a recovering
  /// projection has to answer before it reads anything else.
  ///
  /// It does not say the fail-fast projection would have succeeded. That door additionally refuses
  /// a document with no definition in it, and an empty or trivia-only parse loses nothing while
  /// having nothing: complete, with [`projected`](Self::projected) zero. The two answers coincide
  /// everywhere else.
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

/// The `(parse, source)` pair handed to a recovering projection does not describe one document.
///
/// # Why this is a type and not a [`Recovery`] with nothing projected
///
/// It was one, briefly. A mismatched pair projected nothing and reported every top-level element
/// as skipped, which reads as "the whole document was dropped" and is true — **unless the parse has
/// no top-level elements to report**. An empty or trivia-only parse handed a different, non-empty
/// source counted zero skipped, and [`Recovery::is_complete`] answers `true` at zero: an empty AST
/// marked complete, over source nothing examined.
///
/// A count cannot carry a state. `skipped` answers *how much of this parse had no AST image*, and
/// "these are not the same document" is not a quantity of anything — at every size, including none.
/// So the mismatch leaves [`Recovery`] entirely and becomes the error half of a [`Result`], which a
/// caller cannot read past without deciding what to do about it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub struct SourceMismatch;

impl fmt::Display for SourceMismatch {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("the parse and the source are not the same document")
  }
}

impl core::error::Error for SourceMismatch {}

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

/// A node of a green tree, and where it starts in the source.
///
/// See this module's header for why the traversal is green rather than a cursor, and for the
/// offset accumulation that stands in for what a cursor would have carried.
pub struct Node<'g, L> {
  green: &'g GreenNodeData,
  start: TextSize,
  // `PhantomData<fn() -> L>` rather than `PhantomData<L>`, for the reason
  // [`Parse`](super::runner::Parse) has: the covariant function-pointer form imposes no
  // `L`-shaped auto-trait or drop obligation on a view that never holds an `L`.
  language: PhantomData<fn() -> L>,
}

impl<L> Clone for Node<'_, L> {
  #[inline]
  fn clone(&self) -> Self {
    *self
  }
}

impl<L> Copy for Node<'_, L> {}

impl<L: Language> fmt::Debug for Node<'_, L> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{:?}@{:?}", self.kind(), self.text_range())
  }
}

impl<'g, L> Node<'g, L> {
  /// Views `green` as a node starting at `start` bytes into the source.
  #[inline]
  pub const fn new(green: &'g GreenNodeData, start: TextSize) -> Self {
    Self {
      green,
      start,
      language: PhantomData,
    }
  }

  /// The green node this views.
  #[inline]
  pub const fn green(self) -> &'g GreenNodeData {
    self.green
  }

  /// Where this node starts in the source.
  #[inline]
  pub const fn start(self) -> TextSize {
    self.start
  }

  /// The bytes this node covers, **trivia included** — see this module's header for why that is
  /// not a span.
  #[inline]
  pub fn text_range(self) -> TextRange {
    TextRange::at(self.start, self.green.text_len())
  }

  /// This node's direct children, each carrying its own absolute start.
  #[inline]
  pub fn children(self) -> Children<'g, L> {
    Children {
      raw: self.green.children(),
      offset: self.start,
      language: PhantomData,
    }
  }
}

impl<'g, L: Language> Node<'g, L> {
  /// The green node under a cursor, at the offset the cursor already knows.
  ///
  /// The bridge from rowan's API into this one, for a caller that holds a
  /// [`SyntaxNode`](rowan::SyntaxNode) — a typed CST wrapper, say — and wants the walk below it
  /// to materialise nothing further.
  #[inline]
  pub fn of(node: &'g rowan::SyntaxNode<L>) -> Self {
    Self::new(node.green(), node.text_range().start())
  }

  /// This node's kind, in `L`'s vocabulary.
  #[inline]
  pub fn kind(self) -> L::Kind {
    L::kind_from_raw(self.green.kind())
  }
}

/// A token of a green tree, and where it starts in the source.
///
/// [`Node`]'s other half; see it for why the traversal is green.
pub struct Token<'g, L> {
  green: &'g GreenTokenData,
  start: TextSize,
  language: PhantomData<fn() -> L>,
}

impl<L> Clone for Token<'_, L> {
  #[inline]
  fn clone(&self) -> Self {
    *self
  }
}

impl<L> Copy for Token<'_, L> {}

impl<L: Language> fmt::Debug for Token<'_, L> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{:?}@{:?}", self.kind(), self.text_range())
  }
}

impl<'g, L> Token<'g, L> {
  /// Views `green` as a token starting at `start` bytes into the source.
  #[inline]
  pub const fn new(green: &'g GreenTokenData, start: TextSize) -> Self {
    Self {
      green,
      start,
      language: PhantomData,
    }
  }

  /// The green token this views.
  #[inline]
  pub const fn green(self) -> &'g GreenTokenData {
    self.green
  }

  /// Where this token starts in the source.
  #[inline]
  pub const fn start(self) -> TextSize {
    self.start
  }

  /// The token's own text, as the tree recorded it.
  ///
  /// A door that has run [`verify_source`] holds the same bytes in `source`, and everything the
  /// AST keeps is sliced from **there** so it borrows the caller's buffer rather than the parse.
  /// This is for a read that does not escape the walk — classifying a contextual keyword against
  /// the lexer's own table is the one the GraphQL projection makes.
  #[inline]
  pub fn text(self) -> &'g str {
    self.green.text()
  }

  /// The bytes this token covers.
  #[inline]
  pub fn text_range(self) -> TextRange {
    TextRange::at(self.start, self.green.text_len())
  }
}

impl<L: Language> Token<'_, L> {
  /// This token's kind, in `L`'s vocabulary.
  #[inline]
  pub fn kind(self) -> L::Kind {
    L::kind_from_raw(self.green.kind())
  }
}

/// One child of a [`Node`]: another node, or a token.
pub type Element<'g, L> = NodeOrToken<Node<'g, L>, Token<'g, L>>;

/// [`Node::children`]'s iterator.
///
/// **Forward only.** Each item's start is the running sum of its preceding siblings' lengths, so
/// there is no [`DoubleEndedIterator`] to be had without a second accumulator running the other
/// way — and a reversed walk is not something the span fold or the hole scan wants.
pub struct Children<'g, L> {
  raw: rowan::Children<'g>,
  offset: TextSize,
  language: PhantomData<fn() -> L>,
}

impl<L> Clone for Children<'_, L> {
  #[inline]
  fn clone(&self) -> Self {
    Self {
      raw: self.raw.clone(),
      offset: self.offset,
      language: PhantomData,
    }
  }
}

impl<L: Language> fmt::Debug for Children<'_, L> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("Children")
      .field("remaining", &self.raw.len())
      .field("offset", &self.offset)
      .finish()
  }
}

impl<'g, L> Iterator for Children<'g, L> {
  type Item = Element<'g, L>;

  #[inline]
  fn next(&mut self) -> Option<Self::Item> {
    let start = self.offset;
    Some(match self.raw.next()? {
      NodeOrToken::Node(green) => {
        self.offset += green.text_len();
        NodeOrToken::Node(Node::new(green, start))
      }
      NodeOrToken::Token(green) => {
        self.offset += green.text_len();
        NodeOrToken::Token(Token::new(green, start))
      }
    })
  }

  #[inline]
  fn size_hint(&self) -> (usize, Option<usize>) {
    self.raw.size_hint()
  }
}

impl<L> ExactSizeIterator for Children<'_, L> {
  #[inline]
  fn len(&self) -> usize {
    self.raw.len()
  }
}

impl<L> FusedIterator for Children<'_, L> {}

/// The token extent of `node` — its first non-trivia token's start to its last one's end.
///
/// `None` when the subtree holds no non-trivia token at all, which for a node the grammar
/// requires to have content is itself a finding and is why this returns an `Option` rather
/// than falling back on [`Node::text_range`].
///
/// **This descends the whole subtree**, so it is for a node the caller reads once and does not
/// project — a child the AST has no place for whose bytes still belong to the parent's span. A
/// projection that calls it at every level pays the subtree again per ancestor; see this
/// module's header for the fold that does not.
///
/// See this module's header for why the node's own range is the wrong answer.
#[inline]
pub fn node_extent<L: Language>(
  node: Node<'_, L>,
  is_trivia: impl Fn(L::Kind) -> bool + Copy,
) -> Option<TextRange> {
  extent_of(node.children(), is_trivia)
}

/// The token extent of a run of elements, descending into every node it contains.
///
/// The general form [`node_extent`] is written in terms of. A projection that has to exclude
/// one constituent — the description a definition node holds but the AST hoists out — folds
/// the filtered child stream through here rather than reaching for the node's range.
pub fn extent_of<'g, L: Language, I>(
  elements: I,
  is_trivia: impl Fn(L::Kind) -> bool + Copy,
) -> Option<TextRange>
where
  I: IntoIterator<Item = Element<'g, L>>,
{
  let mut extent: Option<TextRange> = None;
  for element in elements {
    let piece = match element {
      NodeOrToken::Token(token) => {
        if is_trivia(token.kind()) {
          continue;
        }
        Some(token.text_range())
      }
      NodeOrToken::Node(node) => node_extent(node, is_trivia),
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
/// The one-token form of [`verify_source`], for a caller that holds a token and no tree. It is
/// **not** what a projection door should use: checking per token leaves punctuation and trivia
/// bytes — everything whose text no constructor reads — unexamined, so a same-length divergence
/// in an unchecked position passes it, and one whole-tree comparison costs less than one of
/// these per token over the same bytes.
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

/// [`verify_source`], answering how many **elements** the tree holds when it agrees.
///
/// # Why a count, and why here
///
/// A `Parse`'s bytes do not bound its structure. `smear_parser::lossless::runner::finish_root` is
/// public, so a caller can mint one from its own CST event stream, and a balanced pair of zero-width
/// nodes adds structure without adding a byte — an empty source over a tree with a million empty
/// top-level nodes is a legal `Parse` that verifies against `""`.
///
/// A door that prices a projection from `source.len()` therefore charges one unit and then visits a
/// million nodes. The verification is the only walk that already sees the whole tree, so counting
/// here is free, and pairing the count with the proof is what lets a consumer charge for the thing
/// it is about to do rather than for a proxy that does not bound it. al8n/smear#198.
///
/// Nodes and tokens both count: the projection visits a node to dispatch on its kind and a token to
/// read its text. Saturating at [`u32::MAX`], which no ledger can pay, so a tree too large to
/// price refuses rather than wrapping into a budget it fits.
pub fn verify_source_counted<K>(
  root: &GreenNodeData,
  source: &str,
) -> Result<u32, ProjectError<K>> {
  let len = usize::from(root.text_len());
  if len != source.len() {
    return Err(ProjectError::new(
      ProjectErrorKind::SourceMismatch,
      len.min(source.len())..len.max(source.len()),
    ));
  }
  // The same recursion `verify_source_at` makes, with a counter threaded through it; see that
  // function for the depth argument.
  fn walk(
    green: &GreenNodeData,
    source: &[u8],
    offset: &mut usize,
    elements: &mut u32,
  ) -> Result<(), Range<usize>> {
    for child in green.children() {
      *elements = elements.saturating_add(1);
      match child {
        NodeOrToken::Node(node) => walk(node, source, offset, elements)?,
        NodeOrToken::Token(token) => {
          let text = token.text().as_bytes();
          let end = *offset + text.len();
          if source.get(*offset..end) != Some(text) {
            return Err(*offset..end);
          }
          *offset = end;
        }
      }
    }
    Ok(())
  }

  let mut offset = 0usize;
  let mut elements = 1u32;
  walk(root, source.as_bytes(), &mut offset, &mut elements)
    .map_err(|at| ProjectError::new(ProjectErrorKind::SourceMismatch, at))?;
  Ok(elements)
}

/// Verify that `source` is the whole text `root` was parsed from, byte for byte.
///
/// The door check. It covers **every** byte the tree holds — punctuation, trivia and the leading
/// and trailing bytes no node's extent reaches — where a per-token comparison only ever sees the
/// tokens some constructor reads, and it is cheaper than that comparison because it reads the
/// green tree: nothing is materialised, so nothing is allocated.
///
/// A door that has run this may slice `source` by any token range in the tree directly: the
/// ranges are in bounds and land on character boundaries by construction.
///
/// The refusal names the first bytes that diverge — the divergent token's range, or the length
/// the two disagree about when one runs out first.
pub fn verify_source<K>(root: &GreenNodeData, source: &str) -> Result<(), ProjectError<K>> {
  let len = usize::from(root.text_len());
  if len != source.len() {
    return Err(ProjectError::new(
      ProjectErrorKind::SourceMismatch,
      len.min(source.len())..len.max(source.len()),
    ));
  }
  verify_source_at(root, source, 0)
}

/// [`verify_source`] for a subtree: `node`'s text must be the bytes of `source` it sits over.
///
/// The compositional door's form — a caller projecting one node of a larger parse holds the whole
/// file, so the node's text is checked where the node sits rather than against the whole buffer,
/// and bytes outside it are neither read nor claimed.
pub fn verify_source_at<K>(
  green: &GreenNodeData,
  source: &str,
  base: usize,
) -> Result<(), ProjectError<K>> {
  // Recursive rather than an explicit stack, which would need a `Vec` this walk otherwise has no
  // reason to allocate. The depth is the tree's, and the tree's is the lexer's bracket budget
  // (the lexer crate's `MAX_NESTING_DEPTH`) plus a grammar constant. That budget used to be
  // tokora's inherited 500, which put this walk at a few tens of KiB of frames; smear issue #61
  // replaced it with a number measured against a 2 MiB stack, so the bound this comment relies on
  // got an order of magnitude cheaper rather than merely staying true.
  fn walk(green: &GreenNodeData, source: &[u8], offset: &mut usize) -> Result<(), Range<usize>> {
    for child in green.children() {
      match child {
        NodeOrToken::Node(node) => walk(node, source, offset)?,
        NodeOrToken::Token(token) => {
          let text = token.text().as_bytes();
          let end = *offset + text.len();
          if source.get(*offset..end) != Some(text) {
            return Err(*offset..end);
          }
          *offset = end;
        }
      }
    }
    Ok(())
  }

  let mut offset = base;
  walk(green, source.as_bytes(), &mut offset)
    .map_err(|at| ProjectError::new(ProjectErrorKind::SourceMismatch, at))
}

/// Refuse a subtree that carries a node the AST has no image for, in preorder.
///
/// The recovery-hole scan. `is_hole` names the kinds — a dialect's error and gap tiles — and the
/// refusal reports the first one document order reaches, its parent's kind, and its byte range,
/// which is what [`ProjectErrorKind::UnexpectedChild`] wants.
///
/// **A separate pass, deliberately.** Folding it into the projection's own node dispatch would
/// scan strictly less: the walk a door makes starts at the *document* node, and a hole the parser
/// left as a sibling of that node — the shape smear #57 produces — is never a child of anything
/// the walk descends into. A hole anywhere in the scanned subtree is a region with no AST image,
/// and a projection that silently omitted one would be losing data under a success type.
pub fn reject_holes<L: Language>(
  node: Node<'_, L>,
  is_hole: impl Fn(L::Kind) -> bool + Copy,
) -> Result<(), ProjectError<L::Kind>> {
  fn walk<L: Language>(
    node: Node<'_, L>,
    parent: L::Kind,
    is_hole: impl Fn(L::Kind) -> bool + Copy,
  ) -> Option<ProjectError<L::Kind>> {
    let kind = node.kind();
    if is_hole(kind) {
      return Some(ProjectError::new(
        ProjectErrorKind::UnexpectedChild {
          parent,
          found: kind,
        },
        to_range(node.text_range()),
      ));
    }
    for child in node.children() {
      if let NodeOrToken::Node(child) = child
        && let Some(refusal) = walk(child, kind, is_hole)
      {
        return Some(refusal);
      }
    }
    None
  }

  match walk(node, node.kind(), is_hole) {
    Some(refusal) => Err(refusal),
    None => Ok(()),
  }
}
