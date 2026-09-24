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
//! the first from whatever it descends on: the parent a refusal names is the node whose dispatch
//! reached the child, which is a call frame where the walk still has one and a worklist entry
//! where it does not. It pays for both with a heap allocation per element materialised — rowan
//! boxes every node *and* token a cursor yields — which smear #120 measured at 2,447 allocations
//! and 96 ns per source byte for a document the syntactic parser builds in 18.
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
//!
//! # What a caller-minted `Parse` is and is not proven to be
//!
//! [`finish_root`](super::runner::finish_root) is public and generic over the language: it
//! materialises any tokora `Cst` a caller produced, under the caller's own `CstProfile` and the
//! caller's own `root` argument, into a dialect's `Parse`. So the facts a dialect door establishes
//! by construction are, for such a `Parse`, claims: that the root is a document root, that every
//! element's kind is in the space, and the rest of the rows below. This table is the enumeration:
//! every assumption the projection doors and the compiler's lossless doors make about a `Parse`,
//! and which of the three things answers for it.
//!
//! | assumption | `finish_root` | the projection | status |
//! |---|---|---|---|
//! | the root is the dialect's document root | **no** — the root kind is the caller's argument, checked only against the caller's profile | [`verify_root_kind`] against `KindSpace::ROOT`, the constant the dialect doors pass as that argument: in every fail-fast door's opening check, `Verified::new`, `verify_parse` and the recovering doors; the typed door's cast compares the wrapper's kind raw | closed — `WrongRoot` |
//! | every element's kind is in the dialect's space | **no** — the caller's validator admits what it admits | [`reject_foreign_kinds_and_holes`], the scan every fail-fast door, typed `to_ast` and recovering entry runs before its first `kind()`; the recovering root walk compares kinds raw | closed — `InvalidRawKind`, or the entry counted skipped |
//! | the tree's text is the source | **no** — it slices the `Cst`'s own buffer, not the caller's | [`verify_source`], [`verify_source_at`] and [`verify_source_counted`], byte for byte, at every door | closed — `SourceMismatch` |
//! | the tree is no deeper than [`MAX_GREEN_DEPTH`] | not relied on | the verifications refuse past [`MAX_GREEN_DEPTH`] before any projection | closed — `TooDeep` |
//! | the source is UTF-8 and its length addressable | yes — `NonUtf8Source`, `OffsetOverflow` | — | enforced upstream |
//! | the root holds one document container of the door's kind, plus trivia | **no** | the fail-fast doors assert it (`sole_document`, `UnexpectedChild { Root, .. }`); the recovering doors do not — every root child is an element, stepped through when it is a container of the door's kind and otherwise counted skipped, so a root with two valid containers projects both and is complete. That difference is the contract, not a gap: each container is a legitimate document image and the recovering contract is per entry — see [`Recovery`] | closed, differently per door family |
//! | the recovery tiles are the dialect's `Error` and `Gap` | **no** — the profile names its own | nothing reads a tile *as* a tile except the hole scan, which refuses the dialect's two; a tile of any other kind is an element like any other and meets the production that holds it | closed by not being assumed |
//! | the element count a `Verified` pair is priced by | — | counted by [`verify_source_counted`] from the tree itself, never read from the `Parse` | closed by not being assumed |
//! | `Parse::diagnostics` and `Parse::has_errors` describe the tree | **no** — they are the caller's emitter's | no projection door and no compiler lossless door reads either; they are a consumer's, and a consumer of a minted `Parse` reads what its minter wrote | not relied on |
//! | a token's text is what its kind spells | **no** | re-cooked, through the lexer's own doors, wherever the text reaches the AST — names, numbers, strings, the spellings a walk classifies. A token whose text reaches **no** AST field — trivia, punctuation, a keyword read by kind — is taken at its label | **out of contract, by decision** — see below |
//!
//! **The last row is the one not closed, and deliberately.** It is al8n/smear#58's leaf-table
//! decision and the same class as the split-token case that header pins: the projection is a
//! function of the **tree**. A `Space` token over the bytes `garbage` is, to the tree, trivia, and
//! the walks step over trivia without reading its text.
//!
//! The typed layer beyond `cast_node` — a wrapper's own child and token accessors, and tokora's
//! `cast` helpers they build on — reads kinds through `rowan::Language::kind_from_raw` and is not a
//! projection door; it is `rowan`'s contract, not this module's.
//!
//! # No walk here recurses
//!
//! The four walks — the two verifications, `node_extent` and the hole scan — run on `Descent`: it
//! adopts the tree's own child iterators rather than copying children out, keeps one entry per
//! branching ancestor, and drops a source the moment its last child is taken. Each refuses at
//! [`MAX_GREEN_DEPTH`]. The dialect projections those walks gate are worklists too, in the four
//! cycles a value, a selection set and a type reference form — see each dialect module's own
//! header. al8n/smear#201.

use core::{fmt, iter::FusedIterator, marker::PhantomData, ops::Range};

use rowan::{
  GreenNodeData, GreenTokenData, Language, NodeOrToken, SyntaxToken, TextRange, TextSize,
};
use tokora::SimpleSpan;

/// The deepest green tree a projection door will accept.
///
/// The substrate walks — [`verify_source`], [`verify_source_at`], [`verify_source_counted`],
/// [`extent_of`] and the hole scan — each count levels down from this number and refuse with
/// `TooDeep` when it is exhausted. No dialect projection constructs that refusal: every projection
/// door runs one of those verifications over the tree it is handed before it projects, so the
/// projection is never handed a tree deeper than this.
///
/// The walks run on `Descent`, which holds one entry per branching ancestor — the first sixteen in
/// an inline array, the rest in a `Vec` — so this number is also the most entries a walk holds.
/// These helpers take a `&GreenNodeData` and `rowan`'s builder is public, so the tree can be one
/// no parser built; this counter is what stops such a tree growing that `Vec`.
///
/// It bounds what these walks descend, not the tree's construction or destruction; see
/// `crate::lossless::runner::finish_root`.
///
/// # Why 1024
///
/// | bound | from | value |
/// |---|---|---|
/// | lower: the deepest tree the doors were measured to produce | `WORST_DOOR_GREEN_TREE` | **516** |
/// | upper: none | — | — |
///
/// The assertion below keeps `WORST_DOOR_GREEN_TREE` under this constant, and `MAX_DOOR_BRACKETS`
/// carries the crate root's assertion against the lexer's `HARD_MAX`.
pub const MAX_GREEN_DEPTH: usize = 1024;

/// The deepest green tree either dialect's own lossless doors will produce.
///
/// A constant rather than only prose so the assertion below can read it, exactly as
/// `HARD_MAX`'s own `WORST_LOSSLESS_BOUNDARY` is.
///
/// Measured on `parse_document_with_limits` at a `HARD_MAX` ceiling, taking for each shape the
/// deepest bracket count that still parses **clean**, over both dialects:
///
/// | shape | brackets | levels | per bracket |
/// |---|---|---|---|
/// | object value `{ a: { a: … } }` | 254 | **516** | 2.020 |
/// | selection set `{ a { a … } }` | 255 | 515 | 2.008 |
/// | list value `[[…]]` | 254 | 262 | 1.020 |
/// | list type `[[…]]` | 255 | 261 | 1.012 |
///
/// GraphQLx measures identically on every row it shares and is never worse, which is worth
/// stating because `HARD_MAX`'s own table found GraphQLx the worse of the two by 0.3%.
///
/// **The number is recorded rather than computed.** The assertions use `GREEN_LEVELS_PER_BRACKET`,
/// an integer above every row of this table, rather than a formula fitted to one row.
const WORST_DOOR_GREEN_TREE: usize = 516;

/// Green levels one open bracket can add to the tree.
///
/// Three, where the worst row of `WORST_DOOR_GREEN_TREE`'s table measures 2.020. It is the
/// coefficient the crate root's assertion uses through `MAX_DOOR_BRACKETS`.
const GREEN_LEVELS_PER_BRACKET: usize = 3;

/// The deepest bracket ceiling a lossless door may clamp to and still produce a tree these walks
/// will descend.
///
/// **The obligation this module owes the other side of a relationship it must not name.**
/// `MAX_GREEN_DEPTH` and the lexer's `HARD_MAX` live in different crates, and their relationship is
/// one comparison — and this module is the dialect-*generic* substrate, which is parameterised over
/// `L: Lexer` and may not name a concrete lexer crate at all: the rule is
/// `lossless_isolation::SUBSTRATE_FORBIDDEN`, and `ALLOWED_CRATE_ROOTS` sanctions the lexer crate's
/// `limits` root for the two dialect trees and deliberately not for this one. That scan is textual
/// and carries no prose carve-out, which is why this paragraph does not spell the path either.
///
/// So the substrate states what it **affords**, in its own constants, and the crate root performs
/// the comparison: `smear_parser`'s root asserts `HARD_MAX <= MAX_DOOR_BRACKETS`. At a `HARD_MAX`
/// of 342 that assertion fails, since `MAX_DOOR_BRACKETS` is 1024 / 3 = 341.
pub(crate) const MAX_DOOR_BRACKETS: usize = MAX_GREEN_DEPTH / GREEN_LEVELS_PER_BRACKET;

// -- THE INVARIANT ------------------------------------------------------------------------------
//
// Two assertions hold it, and they fail on different edits: the crate root's on a `HARD_MAX` raise
// — see `MAX_DOOR_BRACKETS` for why it is written there and not here — and the one below on a
// `MAX_GREEN_DEPTH` cut.
const _: () = assert!(
  WORST_DOOR_GREEN_TREE <= MAX_GREEN_DEPTH,
  "the deepest tree the lossless doors were measured to produce does not fit under \
   MAX_GREEN_DEPTH, so a projection refuses a parse this crate just produced"
);

// The assertion above compares the deepest tree the doors were measured to produce with the
// ceiling the walks refuse at.

/// How a depth-bounded green walk stopped: on a divergence, or on the ceiling.
///
/// Two reasons one `Result` has to carry, so the walk stays a single loop with a single exit.
enum Depth {
  /// The bytes stopped agreeing, over this range.
  Diverged(Range<usize>),
  /// [`MAX_GREEN_DEPTH`] was reached.
  TooDeep,
}

impl Depth {
  /// The refusal a door reports, with the span each reason can honestly name.
  fn into_error<K>(self) -> ProjectError<K> {
    match self {
      Self::Diverged(at) => ProjectError::new(ProjectErrorKind::SourceMismatch, at),
      // No byte range is the answer here — the tree's shape is — so the span is empty rather than
      // pointing at whichever token the walk happened to be under.
      Self::TooDeep => ProjectError::new(
        ProjectErrorKind::TooDeep {
          limit: MAX_GREEN_DEPTH,
        },
        0..0,
      ),
    }
  }
}

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
  /// The node is there and a member its production requires is not: a present-but-empty `X+`
  /// container (`type T { }`, a directive run with no directive), a separator with nothing after
  /// it, an extension with no tail, or a caller-built node missing a required child. A recovery
  /// hole standing where the member should be is not this refusal — the hole scan answers
  /// [`UnexpectedChild`](Self::UnexpectedChild) for it first.
  MissingChild {
    /// The node kind that is missing a constituent.
    parent: K,
    /// What was wanted, in the grammar's vocabulary.
    wanted: &'static str,
  },
  /// An element the AST shape has no place for, named where it stands.
  ///
  /// Every element a production does not spell at its position: a recovery hole or gap tile; the
  /// rubble a failed definition leaves as bare children of the document; a description a
  /// production reports and builds around but has no slot for; a second copy of an expected-once
  /// child, a stray token, a foreign child or a doubled separator in a caller-built tree; a sibling
  /// of the document container under the root; and a `Variable` in a constant position, which the
  /// AST's own type system forbids (`ConstInputValue` has no `Variable` variant).
  UnexpectedChild {
    /// The node kind whose children were being read.
    parent: K,
    /// The kind that has no place there.
    found: K,
  },
  /// A token's text cannot be read in the role its kind and position give it.
  ///
  /// Two ways, one answer. Every token whose text reaches the AST is re-read through the dialect's
  /// own lexer door — a name through the identifier door, a number through the integer or float
  /// door, a string through `LitStr`'s — and when the door will not read the whole slice back as
  /// the declared kind, the label is the caller's: a parse's tokens come from that lexer, so this
  /// half is reachable only from a caller-minted tree. And at a position that reads a **spelling**
  /// — an operation keyword, a directive location, a root operation type, `true`/`false`/`null` — a
  /// slice that lexes perfectly well as a `Name` can still be none of the spellings the position
  /// classifies. The lossless productions report that and still build the node (`directive @d on
  /// FOO`, `schema { foo: Q }`), so this half **is** reachable from a parse. The variant says which
  /// token and where; it says nothing about who produced the bytes.
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
  /// A spelling a grammar rule forbids at a name position.
  ///
  /// The positions are the ones each dialect's projection header derives from its syntactic
  /// parser's refusals — a fragment's name and a spread's target that is `on`, an enum value or an
  /// enum value definition spelled `true`, `false` or `null`. Where the lossless productions record
  /// the violation only as a diagnostic and still build the node, the shape alone cannot tell the
  /// two apart and the projection re-checks it; where they never build it, a caller-built tree can,
  /// and the same check answers.
  SemanticRule {
    /// The rule, named for a human.
    rule: &'static str,
  },
  /// The supplied tree is deeper than [`MAX_GREEN_DEPTH`].
  ///
  /// A refusal of the tree's **shape**; its bytes may agree with the source exactly. Only the
  /// substrate walks construct it — the byte verifications, [`extent_of`] and the hole scan — and
  /// no dialect projection does. The crate's own lossless doors produce no tree this deep: the
  /// assertions beside [`MAX_GREEN_DEPTH`] and at the crate root keep their measured deepest tree
  /// under it. The tree that reaches it is one a caller built — with `rowan`'s public builder, or
  /// through `finish_root` — and whose shape its bytes do not determine.
  TooDeep {
    /// The limit that was reached.
    limit: usize,
  },
  /// An element carries a raw kind outside the dialect's kind space.
  ///
  /// Not reachable from a dialect door's parse: every such door runs the emitted kinds through the
  /// dialect's validator first. It exists because a caller can mint a green tree with rowan's
  /// public builder and cast it to a typed wrapper whose *root* kind is legal, or mint a `Parse`
  /// through the public, generic [`finish_root`](super::runner::finish_root) under a profile of
  /// its own, and hand either to a projection door — and `rowan::Language::kind_from_raw` has no
  /// fallible form, so asking such an element its kind would panic.
  /// [`reject_foreign_kinds_and_holes`] reads the raw value through the dialect's
  /// [`KindSpace::from_raw`](super::KindSpace::from_raw) before anything else asks, and answers
  /// this with the element's range. al8n/smear#218.
  InvalidRawKind {
    /// The raw value, as the green tree stores it.
    raw: u16,
  },
  /// A parse's root is not the dialect's document root.
  ///
  /// [`InvalidRawKind`](Self::InvalidRawKind) is about an *element*; this is about the tree's
  /// identity, and it answers for an out-of-space root and an in-space wrong one alike, because a
  /// root that is not [`KindSpace::ROOT`](super::KindSpace::ROOT) is a tree no dialect door
  /// finished. See [`verify_root_kind`]. al8n/smear#218.
  WrongRoot {
    /// The root's raw kind, as the green tree stores it.
    raw: u16,
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
        "{start}..{end}: the {kind:?} token's text cannot be read in the role this position gives \
         it"
      ),
      ProjectErrorKind::SourceMismatch => write!(
        f,
        "{start}..{end}: the source text is not what this tree was parsed from"
      ),
      ProjectErrorKind::SemanticRule { rule } => write!(f, "{start}..{end}: {rule}"),
      ProjectErrorKind::TooDeep { limit } => write!(
        f,
        "{start}..{end}: the tree nests deeper than the {limit} levels a projection will descend"
      ),
      ProjectErrorKind::InvalidRawKind { raw } => write!(
        f,
        "{start}..{end}: raw kind {raw} is outside this dialect's syntax-kind space"
      ),
      ProjectErrorKind::WrongRoot { raw } => write!(
        f,
        "{start}..{end}: the parse's root has raw kind {raw}, which is not this dialect's document \
         root"
      ),
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
/// # What `skipped` says
///
/// `skipped() > 0` means at least one top-level element had no AST image, and nothing in the AST
/// covers it.
///
/// [`is_complete`](Self::is_complete) is the one-call form of that question. It is a statement
/// about **loss**, not about validity: it is the only state in which the AST covers the whole
/// document, and it does not imply that a fail-fast projection of the same parse succeeds.
///
/// # Complete here, refused fail-fast — deliberately
///
/// The two door families answer different contracts. A recovering door steps through **every**
/// document container under the root and answers per entry; a fail-fast door asserts the root holds
/// **exactly one** container plus trivia, and that the document has a definition. So two shapes are
/// complete here and refused there:
///
/// - a caller-minted root holding two valid containers: every definition of both is projected,
///   `skipped` is zero, and the fail-fast door answers `UnexpectedChild { parent: Root, .. }` at
///   the second container. This is kept, not closed: **each container is a legitimate document
///   image, and the recovering door's contract is per entry** — it reports what had an AST image
///   and what did not, and every definition of the second container has one; and
/// - an empty or trivia-only parse: nothing is lost and nothing is projected, and the fail-fast
///   door refuses a document with no definition.
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
  /// recovery hole or gap tile the parser left in the definition's place, and each non-trivia token
  /// the parser could not attach to a definition. One mistyped keyword can leave several.
  #[inline]
  pub const fn skipped(&self) -> u32 {
    self.skipped
  }

  /// Returns whether every top-level element had an AST image.
  ///
  /// True exactly when [`skipped`](Self::skipped) is zero: every top-level element had an AST
  /// image.
  ///
  /// It does not say the fail-fast projection would have succeeded. That door additionally asserts
  /// the root holds exactly one document container and that the document has a definition, so a
  /// caller-minted root with two valid containers and an empty or trivia-only parse are both
  /// complete here and refused there — see the type's documentation for why.
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
/// A mismatched pair projects nothing, and reporting that as a [`Recovery`] would need a `skipped`
/// count — which reads as "the whole document was dropped" only while the parse has top-level
/// elements to count. An empty or trivia-only parse handed a different, non-empty source counts
/// zero skipped, and [`Recovery::is_complete`] answers `true` at zero: an empty AST marked
/// complete, over source nothing examined.
///
/// A count cannot carry a state. `skipped` answers *how much of this parse had no AST image*, and
/// "these are not the same document" is not a quantity of anything — at every size, including none.
/// So the mismatch is the error half of a [`Result`] rather than a value of [`Recovery`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Unverified {
  /// The parse and the source do not describe one document.
  ///
  /// The tree's text and `source` differ, in length or in a byte.
  SourceMismatch,
  /// The supplied tree is deeper than [`MAX_GREEN_DEPTH`].
  ///
  /// A refusal of the tree, not of its bytes, which may agree with the source exactly. A parse of
  /// the same source through the dialect's own door is never this deep — see
  /// [`ProjectErrorKind::TooDeep`] — so the tree was built outside that door.
  TooDeep {
    /// The limit that was reached.
    limit: usize,
  },
  /// The parse's root is not this dialect's document root.
  ///
  /// Nothing a dialect door parses can have one: every door finishes its tree at the dialect's
  /// [`KindSpace::ROOT`](super::KindSpace::ROOT). It exists because
  /// [`finish_root`](super::runner::finish_root) is public and generic, takes the root kind as an
  /// argument, and checks it only against the caller's own profile — so a caller can mint a
  /// dialect's `Parse` rooted at raw 60000, or at an in-space kind such as `Name`, and either one
  /// over an empty source verifies byte for byte. One variant for both. al8n/smear#218.
  WrongRoot {
    /// The root's raw kind, as the green tree stores it.
    raw: u16,
  },
}

impl fmt::Display for Unverified {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::SourceMismatch => f.write_str("the parse and the source are not the same document"),
      Self::TooDeep { limit } => write!(
        f,
        "the parse nests deeper than the {limit} levels a projection will descend"
      ),
      Self::WrongRoot { raw } => write!(
        f,
        "the parse's root has raw kind {raw}, which is not this dialect's document root, so the \
         parse was minted outside its door and nothing was projected"
      ),
    }
  }
}

impl core::error::Error for Unverified {}

/// Refuse a root that is not `K`'s document root, at the root's range.
///
/// The part of a pair's proof the byte comparison cannot give: [`verify_source`] reads green data
/// only. Compared raw against `K::ROOT`, the constant each dialect's door passes to
/// [`finish_root`](super::runner::finish_root) as its `root`. Every door that takes a `Parse` runs
/// it right after the bytes; the typed `to_ast` doors take a wrapper whose cast already compared
/// its kind.
///
/// Membership in the kind space is not enough: `Name` is in the space, and a parse rooted at it is
/// not a document. al8n/smear#218.
pub fn verify_root_kind<K: super::KindSpace>(root: &GreenNodeData) -> Result<(), ProjectError<K>> {
  let raw = root.kind().0;
  if raw == K::ROOT.raw() {
    return Ok(());
  }
  Err(ProjectError::new(
    ProjectErrorKind::WrongRoot { raw },
    0..usize::from(root.text_len()),
  ))
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

/// The kind when the language's space names it, the raw value when it does not — never a panic,
/// so a refusal's `Debug` over a caller-minted tree can always be printed.
impl<L> fmt::Debug for Node<'_, L>
where
  L: Language,
  L::Kind: super::KindSpace,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let raw = self.green.kind().0;
    match <L::Kind as super::KindSpace>::from_raw(raw) {
      Some(kind) => write!(f, "{kind:?}@{:?}", self.text_range()),
      None => write!(f, "raw({raw})@{:?}", self.text_range()),
    }
  }
}

impl<'g, L> Node<'g, L> {
  /// Views `green` as a node starting at `start` bytes into the source.
  ///
  /// **Nothing is checked**: not that `start` is where `green` sits in any source, and not that
  /// `green`'s kinds — its own or any descendant's — are in `L`'s kind space. `green` can come
  /// from rowan's public builder. Every public walk in this module that reads a kind checks it
  /// raw first and answers [`ProjectErrorKind::InvalidRawKind`]; [`kind`](Self::kind) itself does
  /// not, and panics — see its own section.
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
  ///
  /// # Panics
  ///
  /// When the raw kind is outside `L`'s space: this is `rowan::Language::kind_from_raw`, which
  /// has no fallible form, and a caller-built tree can hold such a kind. The public walks in this
  /// module check raw kinds before they call it; a caller holding a tree it did not get from a
  /// parse reads `self.green().kind()` and checks it the same way.
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

/// [`Node`]'s `Debug`, and for the same reason.
impl<L> fmt::Debug for Token<'_, L>
where
  L: Language,
  L::Kind: super::KindSpace,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let raw = self.green.kind().0;
    match <L::Kind as super::KindSpace>::from_raw(raw) {
      Some(kind) => write!(f, "{kind:?}@{:?}", self.text_range()),
      None => write!(f, "raw({raw})@{:?}", self.text_range()),
    }
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
  ///
  /// # Panics
  ///
  /// As [`Node::kind`], and for the same reason.
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

/// One source of children a walk has reached but not drained.
///
/// The container is the tree's own — `rowan` hands out an iterator over the children it already
/// allocated — so a source is **adopted** rather than copied into, and a node contributes exactly
/// one of these however wide it is. That is the property `value/nesting.rs` records for the release
/// walk, reached here through a borrowed iterator instead of an owned `Vec`.
struct Source<T, I> {
  /// The budget the walk still had when it entered the node whose children these are.
  ///
  /// Carried **per source** rather than as one counter beside the stack, and that is what lets a
  /// spent source be dropped the moment its last child is taken: with the depth on the entry, the
  /// walk reads its level off whatever source it comes back to, so pruning does not lose it. A
  /// single counter would have to stay in step with a stack that no longer has one entry per
  /// level, and the chain shapes are exactly where it would drift.
  left: usize,
  /// What the walk must remember about that node — its kind, for a refusal that names the parent,
  /// or `()` for the three walks that need nothing.
  tag: T,
  children: I,
}

/// A depth-first descent over a borrowed tree, with the native stack left out of it.
///
/// # What replaced the native frame, and what it costs
///
/// These walks recursed, one frame per green level, and each carried a counter that refused at
/// [`MAX_GREEN_DEPTH`]. **A counter cannot bound a native stack**: the frames are the host's and
/// the stack is the caller's, so a tree the counter would have refused at 1024 levels aborted the
/// process first on any thread too small to hold 1024 of them. Measured on `aarch64-apple-darwin`,
/// unoptimised, one child process per depth, the walk run on a thread of the stated size while the
/// tree was built on another: on 512 KiB `node_extent` aborted at 726 levels and `reject_holes` at
/// 927, and on 256 KiB the two verifications aborted at 566 and 530 — every one of them below the
/// ceiling they were supposed to refuse at, and none of them reached a `TooDeep` anybody could read.
///
/// What stands in the frame's place is one entry per **ancestor of the node in hand that still has
/// an unvisited child**, and nothing else. A node is handed over whole, so a container ancestor
/// costs one entry however wide it is; a source is dropped the moment its last child is taken, so a
/// chain of single-child nodes costs one entry at any depth rather than one per level. The peak
/// therefore follows the tree's *branching* nesting, and neither its width nor its depth — and it
/// is additionally capped by [`MAX_GREEN_DEPTH`], which no longer bounds a stack but does still
/// bound this.
///
/// # Why the first sources are held in the walk's own frame
///
/// **`verify_parse` allocates nothing, and that is gated.** `validator_allocation.rs`'s
/// `the_whole_root_check_allocates_nothing` measures it at zero, because the round that gave that
/// helper one rowan cursor allocation per element got through every other gate in the repository:
/// same diagnostics, same verdicts, only the allocator saw it. A worklist that reached the heap on
/// the first branching level would have turned that zero into a one.
///
/// So [`INLINE`] sources live in the array below, in the caller's frame, and only what is nested
/// deeper than that reaches the heap. That is a fixed **512 bytes** of frame for the widest of the
/// three walks and it does not grow with anything.
///
/// **It is a fixture-sized answer, not a bound**, and the difference is worth stating: the property
/// is *no allocation up to `INLINE` branching levels*, not *no allocation*. Past it the walk spills
/// into a `Vec` and grows through the infallible `push`, like the release walk and unlike
/// `smear::json`'s value walk, which grows through `try_reserve` and reports `Error::Allocation`.
/// What is bought there is a failure that needs the allocator exhausted by a request proportional
/// to the branching nesting of a tree already in memory, in place of one that arrives at a fixed
/// depth on every machine.
struct Descent<T, I> {
  /// The innermost sources, held where the walk's own frame is.
  ///
  /// Occupied from index zero up to `filled`, and never read past it.
  inline: [Option<Source<T, I>>; INLINE],
  /// How many of `inline` are occupied.
  filled: usize,
  /// Everything nested deeper than [`INLINE`] branching levels, innermost last.
  ///
  /// Empty for every tree in this repository's corpus, whose deepest green tree is twelve levels
  /// **in total** — so its branching nesting cannot exceed that either. Not empty for a document
  /// anybody could type: `{ a { a … { b } … } }` at fifteen nested selection sets is 95 bytes and
  /// reaches this, measured.
  spill: Vec<Source<T, I>>,
}

/// How many sources a walk holds before it reaches the heap.
///
/// Sixteen, against a corpus whose deepest green tree is twelve levels of *any* kind and a gated
/// fixture whose branching nesting is three. It is chosen to keep the allocation gate's reading
/// honest rather than to bound anything: see [`Descent`]'s header for what is and is not promised
/// past it.
///
/// **Four public contracts spell this number out**, because rustdoc will not resolve a link from a
/// public item to a private one and a `-D warnings` build says so. Changing it means changing all
/// of them, and they are named here so that the change is a list rather than a search:
/// [`verify_source`] and [`MAX_GREEN_DEPTH`] in this module, and `Verified::new` and `verify_parse`
/// in the GraphQL dialect's own `lossless::project`. That one is named by dialect rather than by
/// path because `smear/tests/lossless_isolation.rs`'s `the_substrate_names_no_dialect` reads this
/// file for the path's spelling and cannot tell a doc comment from a use — the substrate naming a
/// dialect module is what that gate refuses, and it grants prose no exemption. The same number is
/// also read by `smear/tests/validator_allocation.rs`'s `the_whole_root_check_allocates_nothing`,
/// which measures the zero the four of them promise.
const INLINE: usize = 16;

impl<T: Copy, I: ExactSizeIterator> Descent<T, I> {
  /// An empty descent, allocating nothing.
  ///
  /// A subtree with no node in it never grows past this, so the three walks that fold over a
  /// token run allocate exactly what they allocated when they recursed: nothing.
  const fn new() -> Self {
    Self {
      inline: [const { None }; INLINE],
      filled: 0,
      spill: Vec::new(),
    }
  }

  /// Hands a node's children over whole.
  ///
  /// An empty run is not pushed: a leaf would otherwise cost an entry to say it has no children,
  /// which is the per-element path of every one of these walks.
  fn open(&mut self, left: usize, tag: T, children: I) {
    if children.len() == 0 {
      return;
    }
    let source = Source {
      left,
      tag,
      children,
    };
    if self.filled < INLINE && self.spill.is_empty() {
      self.inline[self.filled] = Some(source);
      self.filled += 1;
    } else {
      self.spill.push(source);
    }
  }

  /// The innermost source, wherever it is held.
  fn innermost(&mut self) -> Option<&mut Source<T, I>> {
    if let Some(last) = self.spill.last_mut() {
      return Some(last);
    }
    self.inline.get_mut(self.filled.checked_sub(1)?)?.as_mut()
  }

  /// Drops the innermost source.
  fn close(&mut self) {
    if self.spill.pop().is_none() {
      self.filled -= 1;
      self.inline[self.filled] = None;
    }
  }

  /// Takes one child from the innermost source that still has one, with that source's budget and
  /// tag.
  ///
  /// The source is dropped the moment its last child is taken rather than when it is next reached.
  /// Without that a chain of one-child nodes would leave a spent iterator behind per level and the
  /// storage would follow the depth after all — which is the trade the first round of #199
  /// rejected an iterator stack for, and the reason this one does not make it.
  fn take(&mut self) -> Option<(usize, T, I::Item)> {
    while let Some(source) = self.innermost() {
      let Some(item) = source.children.next() else {
        self.close();
        continue;
      };
      let (left, tag) = (source.left, source.tag);
      if source.children.len() == 0 {
        self.close();
      }
      return Some((left, tag, item));
    }
    None
  }
}

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
pub fn node_extent<L>(
  node: Node<'_, L>,
  is_trivia: impl Fn(L::Kind) -> bool + Copy,
) -> Result<Option<TextRange>, ProjectError<L::Kind>>
where
  L: Language,
  L::Kind: super::KindSpace,
{
  extent_of(node.children(), is_trivia)
}

/// The token extent of a run of elements, descending into every node it contains.
///
/// The general form [`node_extent`] is written in terms of. A projection that has to exclude
/// one constituent — the description a definition node holds but the AST hoists out — folds
/// the filtered child stream through here rather than reaching for the node's range.
///
/// Every element's raw kind is checked against `L`'s space before the trivia predicate is asked
/// anything, nodes included, and one outside it is [`ProjectErrorKind::InvalidRawKind`] at its
/// range. al8n/smear#218.
pub fn extent_of<'g, L, I>(
  elements: I,
  is_trivia: impl Fn(L::Kind) -> bool + Copy,
) -> Result<Option<TextRange>, ProjectError<L::Kind>>
where
  L: Language,
  L::Kind: super::KindSpace,
  I: IntoIterator<Item = Element<'g, L>>,
{
  extent_of_bounded(elements, is_trivia, MAX_GREEN_DEPTH)
}

/// [`extent_of`] with the descent it and [`node_extent`] make into each other counted.
///
/// The pair used to be **mutually recursive** and both halves are `pub`, so a caller-supplied tree
/// drove the native stack. al8n/smear#198's audit of this named three recursive walks and missed
/// this one, which is what a general claim recorded without enumerating its members looks like when
/// the artifact *is* the enumeration.
///
/// # The ceiling is a refusal here, and the first version of it was not
///
/// It was a **stand-in**: past the ceiling the node's own [`TextRange`] took the place of its token
/// extent, recorded as "a superset — imprecise rather than wrong". That reasoning was about the
/// wrong axis. These functions promise `None` when a run holds **no non-trivia token**, and an
/// all-trivia subtree past the ceiling then answered `Some` — not a wider range, a different answer
/// to a different question, on a promise the signature makes explicitly.
///
/// An approximate success is worse than a new channel, so the two public forms return
/// [`ProjectErrorKind::TooDeep`] and say nothing they cannot establish. al8n/smear#198.
///
/// # Why the descent is a loop and the ceiling stayed anyway
///
/// The counter never bounded what it was written to bound — see [`Descent`], which measures where
/// the native stack ended this walk *below* the ceiling on two ordinary thread sizes. So the walk
/// is a loop and the counter is now only what it says it is: a refusal at a stated depth, reached
/// on any stack.
///
/// **The fold does not care in what order it is folded**, which is what makes the loop a
/// substitution rather than a rewrite: `TextRange::cover` is the least range containing both, so
/// the answer is the cover of every non-trivia token in the run whatever order they arrive in. The
/// loop still walks in document order, because a refusal has to name the *first* node past the
/// ceiling and not whichever one a different order reached first.
fn extent_of_bounded<'g, L, I>(
  elements: I,
  is_trivia: impl Fn(L::Kind) -> bool + Copy,
  left: usize,
) -> Result<Option<TextRange>, ProjectError<L::Kind>>
where
  L: Language,
  L::Kind: super::KindSpace,
  I: IntoIterator<Item = Element<'g, L>>,
{
  let mut extent: Option<TextRange> = None;
  // The caller's own stream is level zero and is not a source: it is not a `Children` and there is
  // nothing to come back to it for, since the loop below drains everything a top-level element
  // opens before the next one is read.
  let mut descent: Descent<(), Children<'g, L>> = Descent::new();
  for element in elements {
    let mut item = Some((left, element));
    while let Some((left, element)) = item {
      // Raw first: a kind outside the space is refused here rather than handed to
      // `kind_from_raw`, which can only panic.
      let (raw, at) = match element {
        NodeOrToken::Token(token) => (token.green().kind().0, token.text_range()),
        NodeOrToken::Node(node) => (node.green().kind().0, node.text_range()),
      };
      let Some(kind) = <L::Kind as super::KindSpace>::from_raw(raw) else {
        return Err(ProjectError::new(
          ProjectErrorKind::InvalidRawKind { raw },
          to_range(at),
        ));
      };
      let piece = match element {
        NodeOrToken::Token(token) => (!is_trivia(kind)).then(|| token.text_range()),
        NodeOrToken::Node(node) => {
          match left.checked_sub(1) {
            Some(left) => descent.open(left, (), node.children()),
            None => {
              return Err(ProjectError::new(
                ProjectErrorKind::TooDeep {
                  limit: MAX_GREEN_DEPTH,
                },
                to_range(node.text_range()),
              ));
            }
          }
          None
        }
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
      item = descent.take().map(|(left, (), element)| (left, element));
    }
  }
  Ok(extent)
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
/// read its text. The count saturates at [`u32::MAX`] rather than wrapping. No finite validation
/// budget covers a saturated count; a disabled ledger and a projection that takes no budget
/// proceed.
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
  // The same walk `verify_source_at` makes, with a counter threaded through it; see that function
  // for the descent and for the depth argument.
  fn walk(
    green: &GreenNodeData,
    source: &[u8],
    offset: &mut usize,
    elements: &mut u32,
    left: usize,
  ) -> Result<(), Depth> {
    let Some(left) = left.checked_sub(1) else {
      return Err(Depth::TooDeep);
    };
    let mut descent: Descent<(), rowan::Children<'_>> = Descent::new();
    descent.open(left, (), green.children());
    while let Some((left, (), child)) = descent.take() {
      *elements = elements.saturating_add(1);
      match child {
        NodeOrToken::Node(node) => {
          let Some(left) = left.checked_sub(1) else {
            return Err(Depth::TooDeep);
          };
          descent.open(left, (), node.children());
        }
        NodeOrToken::Token(token) => {
          let text = token.text().as_bytes();
          let end = *offset + text.len();
          if source.get(*offset..end) != Some(text) {
            return Err(Depth::Diverged(*offset..end));
          }
          *offset = end;
        }
      }
    }
    Ok(())
  }

  let mut offset = 0usize;
  let mut elements = 1u32;
  walk(
    root,
    source.as_bytes(),
    &mut offset,
    &mut elements,
    MAX_GREEN_DEPTH,
  )
  .map_err(Depth::into_error)?;
  Ok(elements)
}

/// Verify that `source` is the whole text `root` was parsed from, byte for byte.
///
/// The door check. It covers **every** byte the tree holds — punctuation, trivia and the leading
/// and trailing bytes no node's extent reaches — where a per-token comparison only ever sees the
/// tokens some constructor reads, and it is cheaper than that comparison because it reads the
/// green tree: nothing is materialised.
///
/// A door that has run this may slice `source` by any token range in the tree directly: the
/// ranges are in bounds and land on character boundaries by construction.
///
/// # Allocation
///
/// **Nothing through sixteen branching ancestors, and not at every shape.** Materialising nothing
/// is not the same as allocating nothing, and this sentence used to say the second: the walk keeps
/// one entry per ancestor of the node in hand that still has an unvisited child, the first sixteen
/// of them in a fixed array in its own frame, and a seventeenth spills into a `Vec` through an
/// infallible `push` — 24 bytes an entry, bounded by [`MAX_GREEN_DEPTH`] and by nothing smaller.
///
/// Sixteen **branching** ancestors is neither sixteen levels nor sixteen children. A chain of
/// single-child nodes holds one entry however long it is, because a source is dropped the moment
/// its last child is taken; a node is handed over whole, so a wide one is one entry too. What
/// spends the array is a nesting in which each level still has something unread — which an ordinary
/// nested selection set is. Measured on a GraphQL parse: `{ a { a … { b } … } }` at fifteen nested
/// selection sets, **95 bytes**, green tree 35 levels, allocates once, for 96 bytes; at fourteen it
/// allocates nothing. See this module's `Descent` for the trade that spill is, and why it is the
/// better of the two failures on offer.
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
  // It recursed, on the argument that the depth is the tree's and the tree's is the lexer's
  // bracket budget plus a grammar constant. **That argument is about the wrong tree**: this takes a
  // `&GreenNodeData`, `rowan`'s builder is public, and a stack this crate does not own is what the
  // frames are spent from. The explicit stack the old comment declined to allocate is the repair —
  // see [`Descent`] for what it costs, which for a run of tokens is still nothing.
  fn walk(
    green: &GreenNodeData,
    source: &[u8],
    offset: &mut usize,
    left: usize,
  ) -> Result<(), Depth> {
    let Some(left) = left.checked_sub(1) else {
      return Err(Depth::TooDeep);
    };
    let mut descent: Descent<(), rowan::Children<'_>> = Descent::new();
    descent.open(left, (), green.children());
    while let Some((left, (), child)) = descent.take() {
      match child {
        NodeOrToken::Node(node) => {
          let Some(left) = left.checked_sub(1) else {
            return Err(Depth::TooDeep);
          };
          descent.open(left, (), node.children());
        }
        NodeOrToken::Token(token) => {
          let text = token.text().as_bytes();
          let end = *offset + text.len();
          if source.get(*offset..end) != Some(text) {
            return Err(Depth::Diverged(*offset..end));
          }
          *offset = end;
        }
      }
    }
    Ok(())
  }

  let mut offset = base;
  walk(green, source.as_bytes(), &mut offset, MAX_GREEN_DEPTH).map_err(Depth::into_error)
}

/// [`reject_holes`], reading every element's kind **raw** and refusing one outside the dialect's
/// kind space before anything asks it the question `rowan::Language::kind_from_raw` can only
/// answer by panicking.
///
/// # Why the hole scan is where the check lives
///
/// Every projection door already runs a whole-subtree scan right after it verifies the bytes and
/// before its first walk, and that scan is the first code to ask an element its kind. So the check
/// that every raw kind names one [`KindSpace::from_raw`](super::KindSpace::from_raw) admits — the
/// predicate the sink's validator already applies to every kind a parse emits, reused rather than
/// restated — is folded into that pass: one walk, and no element's kind is read before it is
/// checked. A parse never trips it; a tree minted with rowan's public builder and cast to a typed
/// wrapper over a legal root kind can, and the answer is
/// [`InvalidRawKind`](ProjectErrorKind::InvalidRawKind) at the element's range rather than a panic
/// out of a safe door. al8n/smear#218.
///
/// The root's own kind is checked too, though a typed wrapper can only be cast from a legal one:
/// the fail-fast doors hand a `Parse`'s root here, and nothing about that root is the caller's
/// promise either.
pub fn reject_foreign_kinds_and_holes<L>(
  node: Node<'_, L>,
  is_hole: impl Fn(L::Kind) -> bool + Copy,
) -> Result<(), ProjectError<L::Kind>>
where
  L: Language,
  L::Kind: super::KindSpace,
{
  fn checked<K: super::KindSpace>(
    raw: rowan::SyntaxKind,
    at: TextRange,
  ) -> Result<K, ProjectError<K>> {
    K::from_raw(raw.0).ok_or_else(|| {
      ProjectError::new(
        ProjectErrorKind::InvalidRawKind { raw: raw.0 },
        to_range(at),
      )
    })
  }

  let root = checked::<L::Kind>(node.green().kind(), node.text_range())?;
  let mut descent: Descent<L::Kind, Children<'_, L>> = Descent::new();
  let mut visiting = Some((MAX_GREEN_DEPTH, root, node, root));
  while let Some((left, parent, node, kind)) = visiting {
    let Some(left) = left.checked_sub(1) else {
      return Err(ProjectError::new(
        ProjectErrorKind::TooDeep {
          limit: MAX_GREEN_DEPTH,
        },
        to_range(node.text_range()),
      ));
    };
    if is_hole(kind) {
      return Err(ProjectError::new(
        ProjectErrorKind::UnexpectedChild {
          parent,
          found: kind,
        },
        to_range(node.text_range()),
      ));
    }
    descent.open(left, kind, node.children());
    visiting = loop {
      match descent.take() {
        Some((left, parent, NodeOrToken::Node(child))) => {
          let kind = checked::<L::Kind>(child.green().kind(), child.text_range())?;
          break Some((left, parent, child, kind));
        }
        Some((_, parent, NodeOrToken::Token(token))) => {
          let kind = checked::<L::Kind>(token.green().kind(), token.text_range())?;
          if is_hole(kind) {
            return Err(ProjectError::new(
              ProjectErrorKind::UnexpectedChild {
                parent,
                found: kind,
              },
              to_range(token.text_range()),
            ));
          }
        }
        None => break None,
      }
    };
  }
  Ok(())
}

/// Refuse a subtree that carries an element the AST has no image for, in preorder.
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
///
/// # It tested node kinds only, and a gap tile is a token
///
/// Both dialects pass `Error | Gap` here and both spell `Gap` as a **token** image — bytes no
/// committed token covered are tiled, not wrapped. So the arm that named it could never match, and
/// each dialect's own `scan_holes` carried a comment saying the arm was dead as written and kept
/// for its scope. That reading was half right: the arm was dead, and what it was dead *about* was
/// not a shape the parser has yet to produce but the one it produces today.
///
/// What that cost is a hole the preflight declared absent. A gap beside otherwise complete children
/// is folded by the projection's own walk as an ordinary non-trivia token — into the enclosing
/// node's extent, silently — so a fail-fast door could answer `Ok` and a recovering one could
/// answer *complete* over source bytes with no AST image at all, which is exactly the data loss
/// under a success type the paragraph above says this pass exists to refuse.
///
/// So the scan is token-aware, and `is_hole` is asked of every element rather than of every node.
/// A token has no children, so nothing else about the walk changes. al8n/smear#58.
pub fn reject_holes<L>(
  node: Node<'_, L>,
  is_hole: impl Fn(L::Kind) -> bool + Copy,
) -> Result<(), ProjectError<L::Kind>>
where
  L: Language,
  L::Kind: super::KindSpace,
{
  // The same walk, with every kind read raw first, so a caller-built tree with a kind outside the
  // space is refused rather than panicking a safe `Result` function. al8n/smear#218.
  reject_foreign_kinds_and_holes(node, is_hole)
}

// ---------------------------------------------------------------------------------------------
// the transcription atoms
// ---------------------------------------------------------------------------------------------

/// The atoms a dialect's projection transcribes its productions with — hoisted here because none of
/// them names a dialect.
///
/// # Why these are the substrate's
///
/// al8n/smear#58 wrote them for the second dialect's projection, and every one of them takes the
/// node kinds it tests as **parameters**: a cursor over a node's children, the extent fold, the
/// `token`/`node`/`one_of`/`many`/`separated`/`end` atoms, and the three refusal constructors. The
/// only thing any of them needs from a dialect is which token kinds are trivia, which is the
/// [`Trivia`](walk::Trivia) bound. al8n/smear#217 and #218 asked the first dialect's projection for the same form,
/// and a second copy of a machine whose whole value is that it is written once is the drift this
/// module exists to prevent — the Lego rule, applied to a walk rather than to a production.
///
/// **What stays with a dialect** is every atom that needs its lexer or its keyword table: reading a
/// `Name` for its spelling, re-cooking one through the dialect's identifier door, a reserved-spelling
/// rule, a description's shape. Those are written per dialect over this cursor's
/// [`peek`](walk::Cursor::peek) and [`bump`](walk::Cursor::bump), and they are where the two projections differ.
///
/// # Why it is gated
///
/// Everything here is `pub(crate)` and its only callers are the dialect projections, so a build
/// with no dialect has nothing to mean by it and `dead_code` would say so under `-Dwarnings` — the
/// `recover.rs` shape `tests/lossless_isolation.rs` records. One gate over the module, rather than
/// one per item.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) mod walk {
  use std::vec::Vec;

  use rowan::{Language, NodeOrToken, TextRange};

  use super::{Children, Element, Node, ProjectError, ProjectErrorKind, Token, to_range};

  /// What a dialect's language marker tells the cursor: which of its token kinds are trivia.
  ///
  /// The one fact every atom below needs from a dialect, and the only one. The tree keeps trivia;
  /// no atom consumes it and no span contains it.
  pub(crate) trait Trivia: Language {
    /// Whether `kind` is one of the dialect's ignorable token images.
    fn is_trivia(kind: Self::Kind) -> bool;
  }

  /// A constituent whose node the tree opens even where the AST records nothing for it.
  ///
  /// `( )` is a written-down empty argument list: the tree gives it a node and the syntactic parser
  /// answers `None` for it while still covering the parentheses. So the value and the extent travel
  /// separately, and [`Extent::keep_optional`] is where a parent puts them back together.
  pub(crate) type Optional<T> = (Option<T>, Option<TextRange>);

  /// A node's token extent, folded as its children are consumed.
  ///
  /// One of these lives in every [`Cursor`]: a token atom covers the token it consumed and
  /// [`keep`](Cursor::keep) covers the extent a projected child hands back, so what is left at the
  /// end is the node's span. `None` — no non-trivia token anywhere under the node — is a finding
  /// rather than a fallback to the node's own range, which is why [`range`](Self::range) is
  /// fallible.
  #[derive(Debug, Clone, Copy, Default)]
  pub(crate) struct Extent {
    /// The cover of every non-trivia token folded in so far.
    range: Option<TextRange>,
  }

  impl Extent {
    /// Widen to include `piece`.
    ///
    /// `cover` rather than `start..piece.end()`: a fold that assumed document order would produce
    /// an inverted range the moment it was handed a stream that was not in it, and an inverted span
    /// is exactly the class `tests/support/span_extent.rs` exists to catch.
    #[inline]
    pub(crate) fn cover(&mut self, piece: TextRange) {
      self.range = Some(match self.range {
        Some(seen) => seen.cover(piece),
        None => piece,
      });
    }

    /// Widen to include a projected child's extent, and keep the child.
    ///
    /// The bottom-up fold, spelled once: a child function answers its AST value beside the extent
    /// it folded, and the parent covers the second while binding the first.
    #[inline]
    pub(crate) fn keep<T>(&mut self, projected: (T, TextRange)) -> T {
      let (value, piece) = projected;
      self.cover(piece);
      value
    }

    /// [`keep`](Self::keep) for a constituent the grammar makes optional.
    #[inline]
    pub(crate) fn keep_opt<T>(&mut self, projected: Option<(T, TextRange)>) -> Option<T> {
      projected.map(|projected| self.keep(projected))
    }

    /// [`keep`](Self::keep) for a constituent whose node the tree opens even where the AST records
    /// nothing for it — see [`Optional`].
    #[inline]
    pub(crate) fn keep_optional<T>(&mut self, projected: Optional<T>) -> Option<T> {
      let (value, piece) = projected;
      if let Some(piece) = piece {
        self.cover(piece);
      }
      value
    }

    /// The cover so far, or `None` when nothing has been folded.
    #[inline]
    pub(crate) const fn get(self) -> Option<TextRange> {
      self.range
    }

    /// The cover so far, refused as [`MissingChild`](ProjectErrorKind::MissingChild) on `node`
    /// when nothing has been folded.
    #[inline]
    pub(crate) fn range<L: Language>(
      self,
      node: Node<'_, L>,
      wanted: &'static str,
    ) -> Result<TextRange, ProjectError<L::Kind>> {
      self.range.ok_or_else(|| missing(node, wanted))
    }
  }

  /// The outer and inner extents of a described node, from the one fold.
  ///
  /// `inner` is everything the node's fold covered *except* its description; `described` is the
  /// description's own extent when the node carries one. The wrapper's span is their cover and the
  /// definition's is `inner`, which is where the hoist shows up as a number — and note the three
  /// node types each dialect's header lists, which give both the same span instead.
  pub(crate) fn described_extents<L: Language>(
    node: Node<'_, L>,
    inner: Extent,
    described: Option<TextRange>,
  ) -> Result<(TextRange, TextRange), ProjectError<L::Kind>> {
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

  /// [`MissingChild`](ProjectErrorKind::MissingChild) on `parent`, over the parent's own range.
  pub(crate) fn missing<L: Language>(
    parent: Node<'_, L>,
    wanted: &'static str,
  ) -> ProjectError<L::Kind> {
    ProjectError::new(
      ProjectErrorKind::MissingChild {
        parent: parent.kind(),
        wanted,
      },
      to_range(parent.text_range()),
    )
  }

  /// [`UnexpectedChild`](ProjectErrorKind::UnexpectedChild) of kind `found` under `parent`, at `at`.
  pub(crate) fn unexpected<L: Language>(
    parent: Node<'_, L>,
    found: L::Kind,
    at: TextRange,
  ) -> ProjectError<L::Kind> {
    ProjectError::new(
      ProjectErrorKind::UnexpectedChild {
        parent: parent.kind(),
        found,
      },
      to_range(at),
    )
  }

  /// [`unexpected`] at a child node.
  pub(crate) fn unexpected_node<L: Language>(
    parent: Node<'_, L>,
    found: Node<'_, L>,
  ) -> ProjectError<L::Kind> {
    unexpected(parent, found.kind(), found.text_range())
  }

  /// [`unexpected`] at a child token.
  pub(crate) fn unexpected_token<L: Language>(
    parent: Node<'_, L>,
    found: Token<'_, L>,
  ) -> ProjectError<L::Kind> {
    unexpected(parent, found.kind(), found.text_range())
  }

  /// Whether a separated list may open with its separator — `on | FIELD | QUERY` may, `A & B` may
  /// not.
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub(crate) enum Leading {
    Allowed,
    // Every separated list the vanilla dialect has — an interface list, a union's members, a
    // directive's locations — admits a leading separator, so a build with only that dialect never
    // constructs this. It is not dead in the substrate's sense: the other dialect's paths and
    // `where` bounds forbid one. An `allow` rather than a gate, because a gate here would have to
    // name a dialect.
    #[allow(dead_code)]
    Forbidden,
  }

  /// A node's child sequence, read in the order its production writes it.
  ///
  /// # Why a cursor and not a slot dispatch
  ///
  /// Through al8n/smear#58's fourth round every walk in the second dialect's projection was a
  /// `match child.kind()` filling slots behind `is_none()` guards, with a token vocabulary beside
  /// it — and al8n/smear#218 measured the first dialect's still was. That form can express a
  /// shape's *set* of children and cannot express their **sequence** or **multiplicity**, and the
  /// same kinds in a different order or count are a different sentence wherever a production has a
  /// committing prefix or a separator. Each round had found the previous such gap, because a
  /// vocabulary is an *approximation* of the production it stands in for and the two differ
  /// somewhere.
  ///
  /// So a walk is its production **transcribed**: a sequence of atoms over this cursor, in the
  /// grammar's order, ending in [`end`](Self::end). *Represented* stops being a property a reviewer
  /// checks against a table and becomes *consumed by an atom*; sequence, multiplicity, vocabulary,
  /// one-of exclusivity, committing prefixes and separators are consequences of the transcription
  /// rather than rules laid over it.
  ///
  /// # What an atom does
  ///
  /// Trivia is skipped, everywhere. A **token** atom covers what it consumed, because a token's
  /// bytes are the node's own. A **node** atom does not: a child's contribution is its *token*
  /// extent, which only the child's own walk knows, so the caller folds it back with
  /// [`keep`](Self::keep) when it projects the child — or, for a worklist cycle, when the descent
  /// returns. A node handed back and neither projected nor descended into is the one thing this
  /// form cannot make impossible; each dialect's mutation law is what finds it.
  ///
  /// **No atom here consumes a name.** A dialect's `Name` reaches its walk as a keyword, as a name
  /// re-cooked through that dialect's identifier door, or as a spelling its caller classifies —
  /// never as "some `Name`, any number of them" — and every one of those needs the dialect's
  /// lexer, so they are written beside the dialect's projection over [`peek`](Self::peek) and
  /// [`bump`](Self::bump).
  ///
  /// Refusals are positioned: [`MissingChild`](ProjectErrorKind::MissingChild) when the children
  /// run out, [`UnexpectedChild`](ProjectErrorKind::UnexpectedChild) at the element actually in
  /// hand.
  pub(crate) struct Cursor<'g, L> {
    /// The node whose children are being read, and the parent every refusal names.
    pub(crate) node: Node<'g, L>,
    children: Children<'g, L>,
    peeked: Option<Element<'g, L>>,
    /// The fold over what the atoms have consumed so far.
    pub(crate) extent: Extent,
  }

  impl<'g, L: Trivia> Cursor<'g, L> {
    #[inline]
    pub(crate) fn new(node: Node<'g, L>) -> Self {
      Self {
        node,
        children: node.children(),
        peeked: None,
        extent: Extent::default(),
      }
    }

    /// The next non-trivia element, without consuming it.
    #[inline]
    pub(crate) fn peek(&mut self) -> Option<Element<'g, L>> {
      if self.peeked.is_none() {
        for element in self.children.by_ref() {
          match element {
            NodeOrToken::Token(token) if L::is_trivia(token.kind()) => {}
            other => {
              self.peeked = Some(other);
              break;
            }
          }
        }
      }
      self.peeked
    }

    /// Consume the element [`peek`](Self::peek) answered.
    #[inline]
    pub(crate) fn bump(&mut self) {
      self.peeked = None;
    }

    /// [`missing`] on this cursor's node.
    #[inline]
    pub(crate) fn missing(&self, wanted: &'static str) -> ProjectError<L::Kind> {
      missing(self.node, wanted)
    }

    /// [`unexpected`] at `element`, under this cursor's node.
    #[inline]
    pub(crate) fn unexpected(&self, element: Element<'g, L>) -> ProjectError<L::Kind> {
      match element {
        NodeOrToken::Node(child) => unexpected_node(self.node, child),
        NodeOrToken::Token(token) => unexpected_token(self.node, token),
      }
    }

    /// The refusal for a required constituent that is not next: **what is in hand decides it**.
    ///
    /// [`MissingChild`](ProjectErrorKind::MissingChild) when the children ran out, or when the next
    /// element is the enclosing `closer` — a present-but-empty `{ }` is row one of the container
    /// table, and its answer is that the member is missing. Anything else is a present element the
    /// production has no place for, refused as
    /// [`UnexpectedChild`](ProjectErrorKind::UnexpectedChild) **at that element**: a refusal that
    /// named the parent while the obstruction sat in plain view would point a caller at the wrong
    /// bytes, which is al8n/smear#58 Codex round five's third finding.
    pub(crate) fn absent(
      &mut self,
      closer: Option<L::Kind>,
      wanted: &'static str,
    ) -> ProjectError<L::Kind> {
      match self.peek() {
        None => self.missing(wanted),
        Some(NodeOrToken::Token(token)) if Some(token.kind()) == closer => self.missing(wanted),
        Some(element) => self.unexpected(element),
      }
    }

    /// The next element must be a token of `kind`.
    pub(crate) fn token(
      &mut self,
      kind: L::Kind,
      wanted: &'static str,
    ) -> Result<Token<'g, L>, ProjectError<L::Kind>> {
      self.token_of(&[kind], wanted)
    }

    /// The next element must be a token whose kind is one of `kinds` — a literal leaf's one token.
    pub(crate) fn token_of(
      &mut self,
      kinds: &[L::Kind],
      wanted: &'static str,
    ) -> Result<Token<'g, L>, ProjectError<L::Kind>> {
      match self.peek() {
        Some(NodeOrToken::Token(token)) if kinds.contains(&token.kind()) => {
          self.bump();
          self.extent.cover(token.text_range());
          Ok(token)
        }
        Some(element) => Err(self.unexpected(element)),
        None => Err(self.missing(wanted)),
      }
    }

    /// A token of `kind` if one is next.
    pub(crate) fn opt_token(&mut self, kind: L::Kind) -> Option<Token<'g, L>> {
      match self.peek() {
        Some(NodeOrToken::Token(token)) if token.kind() == kind => {
          self.bump();
          self.extent.cover(token.text_range());
          Some(token)
        }
        _ => None,
      }
    }

    /// The next element must be a node of `kind`. **Not covered** — see the type's header.
    pub(crate) fn node(
      &mut self,
      kind: L::Kind,
      wanted: &'static str,
    ) -> Result<Node<'g, L>, ProjectError<L::Kind>> {
      self.one_of(&[kind], wanted)
    }

    /// A node of `kind` if one is next.
    pub(crate) fn opt_node(&mut self, kind: L::Kind) -> Option<Node<'g, L>> {
      self.opt_one_of(&[kind])
    }

    /// A node whose kind is one of `kinds` — a slot the production fills from a set.
    pub(crate) fn one_of(
      &mut self,
      kinds: &[L::Kind],
      wanted: &'static str,
    ) -> Result<Node<'g, L>, ProjectError<L::Kind>> {
      match self.peek() {
        Some(NodeOrToken::Node(child)) if kinds.contains(&child.kind()) => {
          self.bump();
          Ok(child)
        }
        Some(element) => Err(self.unexpected(element)),
        None => Err(self.missing(wanted)),
      }
    }

    /// [`one_of`](Self::one_of) where the production makes the slot optional.
    pub(crate) fn opt_one_of(&mut self, kinds: &[L::Kind]) -> Option<Node<'g, L>> {
      match self.peek() {
        Some(NodeOrToken::Node(child)) if kinds.contains(&child.kind()) => {
          self.bump();
          Some(child)
        }
        _ => None,
      }
    }

    /// Every node of one of `kinds`, greedily — an undelimited `*` repetition.
    pub(crate) fn many(&mut self, kinds: &[L::Kind]) -> Vec<Node<'g, L>> {
      let mut taken = Vec::new();
      while let Some(child) = self.opt_one_of(kinds) {
        taken.push(child);
      }
      taken
    }

    /// `X+` — [`many`](Self::many) with the first member required, refused through
    /// [`absent`](Self::absent) so a present-but-empty container is `MissingChild` and a present
    /// stranger is `UnexpectedChild` at the stranger.
    ///
    /// `closer` is the delimiter that ends the run inside its container, and `None` for an
    /// undelimited run.
    pub(crate) fn many1(
      &mut self,
      kinds: &[L::Kind],
      closer: Option<L::Kind>,
      wanted: &'static str,
    ) -> Result<Vec<Node<'g, L>>, ProjectError<L::Kind>> {
      let taken = self.many(kinds);
      if taken.is_empty() {
        return Err(self.absent(closer, wanted));
      }
      Ok(taken)
    }

    /// `sep? item (sep item)*` — one separator between adjacent items, the leading one only where
    /// `leading` allows it. Answers the items and whether a leading separator was written.
    ///
    /// `item` probes for one item **without consuming anything else**: `Ok(None)` means the next
    /// element is not an item. An item is required after the opening position and after every
    /// separator the list consumes, and where one is not there the refusal is decided by what *is*
    /// ([`absent`](Self::absent)): `MissingChild` when the node's children ran out, and
    /// `UnexpectedChild` at the element in hand otherwise — a leading separator where none is
    /// allowed, a doubled separator, a node of the wrong kind.
    pub(crate) fn separated<T>(
      &mut self,
      mut item: impl FnMut(&mut Self) -> Result<Option<T>, ProjectError<L::Kind>>,
      separator: L::Kind,
      leading: Leading,
      wanted: &'static str,
    ) -> Result<(Vec<T>, bool), ProjectError<L::Kind>> {
      let led = leading == Leading::Allowed && self.opt_token(separator).is_some();
      let mut taken = Vec::new();
      match item(self)? {
        Some(first) => taken.push(first),
        None => return Err(self.absent(None, wanted)),
      }
      while self.opt_token(separator).is_some() {
        match item(self)? {
          Some(next) => taken.push(next),
          None => return Err(self.absent(None, wanted)),
        }
      }
      Ok((taken, led))
    }

    /// [`separated`](Self::separated) over **nodes** of one of `kinds`.
    pub(crate) fn separated_nodes(
      &mut self,
      kinds: &[L::Kind],
      separator: L::Kind,
      leading: Leading,
      wanted: &'static str,
    ) -> Result<Vec<Node<'g, L>>, ProjectError<L::Kind>> {
      self
        .separated(
          |cursor| Ok(cursor.opt_one_of(kinds)),
          separator,
          leading,
          wanted,
        )
        .map(|(taken, _)| taken)
    }

    /// Fold a projected child's extent in, and keep its value.
    #[inline]
    pub(crate) fn keep<T>(&mut self, projected: (T, TextRange)) -> T {
      self.extent.keep(projected)
    }

    /// [`keep`](Self::keep) for a constituent the grammar makes optional.
    #[inline]
    pub(crate) fn keep_opt<T>(&mut self, projected: Option<(T, TextRange)>) -> Option<T> {
      self.extent.keep_opt(projected)
    }

    /// [`keep`](Self::keep) for a constituent whose node the tree opens even where the AST records
    /// nothing for it — see [`Optional`].
    #[inline]
    pub(crate) fn keep_optional<T>(&mut self, projected: Optional<T>) -> Option<T> {
      self.extent.keep_optional(projected)
    }

    /// **The totality obligation.** Nothing may be left over.
    pub(crate) fn end(&mut self) -> Result<(), ProjectError<L::Kind>> {
      match self.peek() {
        Some(element) => Err(self.unexpected(element)),
        None => Ok(()),
      }
    }

    /// The node's token extent, after [`end`](Self::end).
    #[inline]
    pub(crate) fn range(&self, wanted: &'static str) -> Result<TextRange, ProjectError<L::Kind>> {
      self.extent.range(self.node, wanted)
    }

    /// Finish: nothing left over, and the extent the fold arrived at.
    #[inline]
    pub(crate) fn finish(
      &mut self,
      wanted: &'static str,
    ) -> Result<TextRange, ProjectError<L::Kind>> {
      self.end()?;
      self.range(wanted)
    }
  }
}
