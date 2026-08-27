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
//!
//! # No walk here spends a native frame per level
//!
//! All four of them recursed, and each carried a counter that refused at [`MAX_GREEN_DEPTH`]. A
//! counter cannot bound a native stack — the frames are the host's and the stack is the caller's —
//! so a tree the counter would have refused took the process first on any thread too small to hold
//! the ceiling's worth of frames, and the typed refusal was never reached. [`Descent`] is what they
//! run on now: it adopts the tree's own child iterators rather than copying children out, keeps one
//! entry per branching ancestor, and drops a source the moment its last child is taken. The counter
//! survives, and its header says what it now answers for.

use core::{fmt, iter::FusedIterator, marker::PhantomData, ops::Range};

use rowan::{
  GreenNodeData, GreenTokenData, Language, NodeOrToken, SyntaxToken, TextRange, TextSize,
};
use tokora::SimpleSpan;

/// The deepest green tree a projection door will accept.
///
/// # It stopped bounding this module's walks, and that is the whole of this branch
///
/// It used to read *the deepest green tree any walk in this module will descend*, and every one of
/// those walks recursed with this number as its counter. **A counter cannot bound a native stack.**
/// The frames belong to the host and the stack belongs to whichever thread the caller walks on, so
/// a tree this constant would have refused at 1024 levels took the process first on any thread too
/// small to hold 1024 frames — and the typed refusal it was supposed to produce was never reached.
/// Measured on `aarch64-apple-darwin`, unoptimised, one child process per depth, the tree built on
/// one thread and the walk run on another of the stated size: `node_extent` aborted at **726**
/// levels on 512 KiB and `reject_holes` at **927**; `verify_source` and `verify_source_counted`
/// aborted at **566** and **530** on 256 KiB. All four are below the ceiling. See [`Descent`],
/// which is what they run on now and which reaches the verdict on any stack.
///
/// # So what does it bound, and why does it stay
///
/// **The recursion behind these walks, which is the dialect projection's own.** A fail-fast door
/// opens with [`verify_source`] over the whole green root and a [`reject_holes`] scan over the same
/// tree, and only then dispatches on node kinds — and *that* dispatch is a native recursion, one
/// frame per grammar-nesting level, with no counter of its own in either dialect. The two gate
/// walks are what stand in front of it, and this is the depth they let past.
///
/// A caller who calls the four walks directly gets a refusal at this depth for the same reason: the
/// number is the door's admission ceiling, and answering it in one place is what keeps the doors and
/// the helpers from disagreeing about which trees are projectable.
///
/// # The population this is derived over, which is not the one it used to name
///
/// This header used to rest on two figures — the deepest green tree in the repository's 472
/// corpus fixtures is **12** levels, and the deepest document at
/// the lexer's default `MAX_NESTING_DEPTH` of 24 open brackets materialises
/// **51** — and conclude that *nothing a parser produces comes near it*. **That conclusion was
/// false, and the reason is the population.** The lossless doors do not clamp to
/// `MAX_NESTING_DEPTH`; they clamp to the lexer's `HARD_MAX`, which is 256.
/// A margin derived at 24 brackets was being stated over 256 of them, and at the top of that
/// range the tree really did cross the old ceiling: a 254-bracket object-value chain parses
/// clean and materialises **516** levels, so an ordinary `project` answered
/// `TooDeep { limit: 512 }` on a document this crate's own parser had just accepted with no
/// diagnostic at all. The window was per-shape rather than per-bracket-count — it opened at 253
/// brackets for an object-value chain and at 255 for a selection chain — which is itself the
/// tell that a single fitted formula was the wrong instrument. al8n/smear#198.
///
/// So the figure below is derived over the population that reaches it. `WORST_DOOR_GREEN_TREE` is
/// what the doors produce and it is asserted; `WORST_PROJECTION_GREEN_TREE` is what the recursion
/// this gate stands in front of can afford, and it is **not** asserted, for the reason recorded
/// beside it.
///
/// # Why any bound is needed here at all
///
/// These helpers take a `&GreenNodeData`, and `rowan`'s builder is public, so the tree can come
/// from anywhere — including `finish_root`, which finishes an event stream this crate did not
/// emit. A projection over an unproved tree is a stack overflow rather than a refusal, and a crash
/// is worse than every charge defect al8n/smear#198 has found. The gate walks are what refuse on
/// the projection's behalf, which is what "independently bounded" has to mean when the caller
/// supplies the tree and the walk that would die is not the one holding the counter.
///
/// What this does **not** bound is the tree's *construction* or its *destruction*: `rowan` drops a
/// green tree recursively, so a tree deep enough to overflow the projection was already deep enough
/// to overflow its own `Drop`, in the caller's code, before any of these functions saw it. That
/// route is `rowan`'s and is reachable without this crate; see `crate::lossless::runner::finish_root`.
///
/// # Why 1024
///
/// The interval it used to be cut from had this constant's own native-stack boundary at the top of
/// it. There is no such boundary any more, so what is left is the lower bound and the tie-break:
///
/// | bound | from | value |
/// |---|---|---|
/// | lower: the tree the doors produce | `WORST_DOOR_GREEN_TREE` | **516** |
/// | upper: none this module can state | see `WORST_PROJECTION_GREEN_TREE` | — |
///
/// 1024 is the value the old interval `[516, 1505]` was taken at, and it is kept rather than raised
/// because nothing here wants a wider one: raising it widens only what the projection is handed.
///
/// **What it costs a caller is now nothing.** The walks allocate one worklist entry per branching
/// ancestor and no native frame at all, so a tree at the full ceiling occupies a few tens of bytes
/// of heap instead of the **750 KiB** of stack the old header priced at 733 bytes a level.
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
/// **The obvious relationship is wrong, and this is why the number is recorded rather than
/// computed.** `2 x brackets + 3` is what a selection chain costs and it gives 515; the object
/// value chain costs 2.020 a bracket and reaches 516, one level *above* it. A formula fitted to
/// the first shape anyone measures is how this drifts again, so the fitted relationship is not
/// what the assertions use — `GREEN_LEVELS_PER_BRACKET` is, and it is an integer above every
/// row of this table.
const WORST_DOOR_GREEN_TREE: usize = 516;

/// Green levels one open bracket can add to the tree.
///
/// Three, where the worst row of `WORST_DOOR_GREEN_TREE`'s table measures 2.020. It is the
/// coefficient the scaling assertion below uses, and it is deliberately the next integer above
/// every measured shape rather than the measured maximum: the table is nine shape-and-dialect
/// pairs, which is an enumeration and not a proof, so the coefficient carries the margin for the
/// shape nobody has written yet.
const GREEN_LEVELS_PER_BRACKET: usize = 3;

/// The deepest green tree a **dialect projection** was measured to descend before the native stack
/// ends it.
///
/// Not this module's walks: they no longer have such a number, which is what
/// [`MAX_GREEN_DEPTH`]'s header is about. This is the recursion those walks gate — the projection's
/// own node dispatch, one native frame per grammar-nesting level, in both dialects and with no
/// counter of its own.
///
/// Measured on `aarch64-apple-darwin`, **unoptimised**, on a 2 MiB thread — what
/// `std::thread::spawn`, a tokio worker and the libtest harness each give — with the parse
/// performed on another thread so only the projection's frames are on this one. One child process
/// per bracket count, `project_type_system_document` over `scalar Foo @x(a: {a: … 1 … })`:
/// 253 brackets and **514** levels returned, 254 brackets and **516** levels aborted with
/// `SIGABRT`. That is 4 080 bytes of frame per green level, against a green *walk*'s 733.
///
/// # The window this opens, which is not this branch's to close
///
/// `WORST_DOOR_GREEN_TREE` is **516**, so at the top of `HARD_MAX` the doors produce exactly the
/// tree the projection cannot descend: 254 and 255 brackets of object value parse clean and abort
/// the process in `project_type_system_document`. No ceiling reachable from here closes it. Cutting
/// [`MAX_GREEN_DEPTH`] under 516 makes a projection refuse a parse this crate just produced, which
/// is the window al8n/smear#198 closed; and it would take `MAX_DOOR_BRACKETS` below `HARD_MAX`,
/// which the crate root refuses to compile. The two repairs that do close it are a lower `HARD_MAX`
/// and a projection that does not recurse, and both are a different change from this one.
///
/// **In an optimised build there is no window at all**, and that is the qualification the number
/// above must be read with: release, same host, `project_type_system_document` returned at every
/// bracket count `HARD_MAX` admits, on stacks down to 256 KiB, at 393 bytes of frame per green
/// level. So what is recorded here is a debug-profile abort — which is the profile `cargo test`
/// runs in, and the profile every other boundary in this file is measured in.
const WORST_PROJECTION_GREEN_TREE: usize = 514;

/// The deepest bracket ceiling a lossless door may clamp to and still produce a tree these walks
/// will descend.
///
/// **The obligation this module owes the other side of a relationship it must not name.**
/// `MAX_GREEN_DEPTH` and the lexer's `HARD_MAX` live in different crates and had a relationship
/// nothing enforced, so a margin derived at 24 brackets went on being stated over 256 of them.
/// What closes that is one comparison — and this module is the dialect-*generic* substrate, which
/// is parameterised over `L: Lexer` and may not name a concrete lexer crate at all: the rule is
/// `lossless_isolation::SUBSTRATE_FORBIDDEN`, and `ALLOWED_CRATE_ROOTS` sanctions the lexer
/// crate's `limits` root for the two dialect trees and deliberately not for this one. That scan
/// is textual and carries no prose carve-out, which is why this paragraph does not spell the
/// path either.
///
/// So the substrate states what it **affords**, in its own constants, and the crate root — which
/// assembles the lexer, the substrate and the dialects, and is the one place entitled to see all
/// three — performs the comparison. `smear_parser`'s own root carries it, unconditionally in every
/// configuration that compiles this module, so the guarantee is one site and not one per dialect.
///
/// `MAX_GREEN_DEPTH / GREEN_LEVELS_PER_BRACKET` is the same predicate the assertion here used to
/// spell as `HARD_MAX * GREEN_LEVELS_PER_BRACKET <= MAX_GREEN_DEPTH`: over integers the two agree
/// at every value, so the move is a relocation and not a loosening. The plant that proves it is
/// still live is a `HARD_MAX` of 342 — that value passes `HARD_MAX`'s OWN 1.9x margin assertion,
/// so every gate that existed before al8n/smear#198's round admits it, and it is exactly the edit
/// that reopens the projection window. 342 > 341, so the crate root refuses to compile.
pub(crate) const MAX_DOOR_BRACKETS: usize = MAX_GREEN_DEPTH / GREEN_LEVELS_PER_BRACKET;

// -- THE INVARIANT THAT WAS MISSING, AND THE WINDOW IT WOULD HAVE CLOSED ----------------------
//
// Two assertions hold it now, and they fail on different edits: the crate root's on a `HARD_MAX`
// raise — see `MAX_DOOR_BRACKETS` for why it is written there and not here — and the first below
// on a `MAX_GREEN_DEPTH` cut.
const _: () = assert!(
  WORST_DOOR_GREEN_TREE <= MAX_GREEN_DEPTH,
  "the deepest tree the lossless doors were measured to produce does not fit under \
   MAX_GREEN_DEPTH, so a projection refuses a parse this crate just produced"
);

// THE THIRD ASSERTION IS GONE, AND IT IS NOT TIDYING. It read
//
//   MAX_GREEN_DEPTH * 19 <= WORST_GREEN_WALK_BOUNDARY * 10
//
// over `WORST_GREEN_WALK_BOUNDARY = 2861`, the depth at which the worst of the four walks here ran
// out of native stack on a 2 MiB debug thread. Those walks no longer run out of native stack at any
// depth, so its subject does not exist and a passing assertion over it would say something true
// about nothing.
//
// What it was *standing in for* is a real obligation and it does survive: the doors must not
// produce a tree the walk behind this gate cannot descend. That walk is the projection's own
// recursion, `WORST_PROJECTION_GREEN_TREE` is what it affords, and the comparison
//
//   WORST_DOOR_GREEN_TREE <= WORST_PROJECTION_GREEN_TREE
//
// is FALSE today — 516 against 514 — which is why it is written here rather than asserted. The
// assertion below is a tripwire on that record instead: it fires the day the gap closes, so the
// paragraph above cannot outlive the defect it describes.
const _: () = assert!(
  WORST_PROJECTION_GREEN_TREE < WORST_DOOR_GREEN_TREE,
  "the projection now descends every tree the doors produce, so WORST_PROJECTION_GREEN_TREE and \
   the window its header records should be replaced by the assertion they stand in for"
);

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
  /// The tree nests deeper than a projection will descend.
  ///
  /// Not reachable from a parsed document — [`MAX_GREEN_DEPTH`] carries the assertion that keeps
  /// it unreachable, and the window where it briefly was not. It
  /// exists because these helpers take an arbitrary `GreenNodeData`, and what runs behind them once
  /// they pass is a projection whose own node dispatch recurses.
  TooDeep {
    /// The limit that was reached.
    limit: usize,
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
      ProjectErrorKind::TooDeep { limit } => write!(
        f,
        "{start}..{end}: the tree nests deeper than the {limit} levels a projection will descend"
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
pub enum Unverified {
  /// The parse and the source do not describe one document.
  ///
  /// A caller's remedy is to re-parse the source, or to stop holding a stale pair. Nothing about
  /// the resources it would take is at stake.
  SourceMismatch,
  /// The tree nests deeper than a projection will descend.
  ///
  /// Nothing about the *bytes* is wrong — they may agree exactly — so reporting this as a mismatch
  /// tells the caller to fix the one thing that is not the problem. See
  /// [`MAX_GREEN_DEPTH`].
  TooDeep {
    /// The limit that was reached.
    limit: usize,
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
    }
  }
}

impl core::error::Error for Unverified {}

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
/// It grows through `Vec`'s infallible `push`, like the release walk and unlike `smear::json`'s
/// value walk: what is bought is a failure that needs the allocator exhausted by a request
/// proportional to the branching nesting of a tree already in memory, in place of one that arrives
/// at a fixed depth on every machine.
struct Descent<T, I> {
  /// The sources the walk has reached but not drained, innermost last.
  sources: Vec<Source<T, I>>,
}

impl<T: Copy, I: ExactSizeIterator> Descent<T, I> {
  /// An empty descent, allocating nothing.
  ///
  /// A subtree with no node in it never grows past this, so the three walks that fold over a
  /// token run allocate exactly what they allocated when they recursed: nothing.
  const fn new() -> Self {
    Self {
      sources: Vec::new(),
    }
  }

  /// Hands a node's children over whole.
  ///
  /// An empty run is not pushed: a leaf would otherwise cost an entry to say it has no children,
  /// which is the per-element path of every one of these walks.
  fn open(&mut self, left: usize, tag: T, children: I) {
    if children.len() != 0 {
      self.sources.push(Source {
        left,
        tag,
        children,
      });
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
    while let Some(source) = self.sources.last_mut() {
      let Some(item) = source.children.next() else {
        self.sources.pop();
        continue;
      };
      let (left, tag) = (source.left, source.tag);
      if source.children.len() == 0 {
        self.sources.pop();
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
pub fn node_extent<L: Language>(
  node: Node<'_, L>,
  is_trivia: impl Fn(L::Kind) -> bool + Copy,
) -> Result<Option<TextRange>, ProjectError<L::Kind>> {
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
) -> Result<Option<TextRange>, ProjectError<L::Kind>>
where
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
fn extent_of_bounded<'g, L: Language, I>(
  elements: I,
  is_trivia: impl Fn(L::Kind) -> bool + Copy,
  left: usize,
) -> Result<Option<TextRange>, ProjectError<L::Kind>>
where
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
      let piece = match element {
        NodeOrToken::Token(token) => (!is_trivia(token.kind())).then(|| token.text_range()),
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
  // Preorder, in document order, so the refusal names the first hole the document reaches — which
  // is what makes the answer independent of how the scan is written. The parent kind travels on the
  // source rather than in a parameter: it is the kind of the node whose children are being drained,
  // and [`Descent::take`] hands it back with the child.
  let mut descent: Descent<L::Kind, Children<'_, L>> = Descent::new();
  let mut visiting = Some((MAX_GREEN_DEPTH, node.kind(), node));
  while let Some((left, parent, node)) = visiting {
    // Its own counter, on its own terms: this takes a caller-supplied tree, and what the counter
    // answers for is the depth a projection will accept — not this walk's frames, which it does not
    // have. See [`MAX_GREEN_DEPTH`] and [`Descent`].
    let Some(left) = left.checked_sub(1) else {
      return Err(ProjectError::new(
        ProjectErrorKind::TooDeep {
          limit: MAX_GREEN_DEPTH,
        },
        to_range(node.text_range()),
      ));
    };
    let kind = node.kind();
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
        Some((left, parent, NodeOrToken::Node(child))) => break Some((left, parent, child)),
        // A token has no kind this scan can refuse and no children to descend into.
        Some((_, _, NodeOrToken::Token(_))) => {}
        None => break None,
      }
    };
  }
  Ok(())
}
