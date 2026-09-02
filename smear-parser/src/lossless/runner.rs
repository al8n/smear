//! The `Parse` surface every dialect's runner hands back, and the materialization step that
//! builds it.
//!
//! What is *not* here is everything that names a concrete lexer, emitter or kind space: the
//! `CstProfile`, the emitter and sink aliases, and `parse_document` itself stay in each dialect's
//! own `runner`, because every one of them has to spell a dialect type to exist at all. What is
//! here is the shape of the answer, which does not.

use tokora::{Lexer, cst::Cst, emitter::Severity};

/// One diagnostic a lossless parse recorded, owned and source-independent.
///
/// **Why not the typed payload.** `Verbose::diagnostics()` hands back `Diagnostic<'_, S, Error>`
/// borrowed from the emitter, and a dialect's error is keyed to the *source slice*, so carrying
/// the payload would give [`Parse`] the parse's lifetime. A lifetime-free `Parse` is what lets a
/// consumer cache one per file, so the payload is dropped at this boundary and the two facts that
/// survive are the two an IDE actually routes on.
///
/// It is also why this type needs no parameters at all, and therefore why it is shared rather
/// than duplicated: nothing in it is per-dialect.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
  span: core::ops::Range<usize>,
  severity: Severity,
  skipped_tokens: Option<usize>,
}

impl Diagnostic {
  /// The byte range this diagnostic covers.
  pub fn span(&self) -> core::ops::Range<usize> {
    self.span.clone()
  }

  /// Whether this is a hard error or a soft one.
  pub fn severity(&self) -> Severity {
    self.severity
  }

  /// For a recovery hole, how many tokens were skipped; `None` for an error or a warning.
  ///
  /// A hole reports [`Severity::Warning`] like a genuine warning does, so this is the field
  /// that tells the two apart.
  pub fn skipped_tokens(&self) -> Option<usize> {
    self.skipped_tokens
  }
}

/// The result of a lossless parse in `L`'s kind space.
///
/// Each dialect re-exports this under an alias with its own language pinned — a **type alias**,
/// not a newtype: a newtype would need every getter re-written per dialect, which is the
/// duplication this module exists to remove, and an alias keeps `parse_document(&str) -> Parse`
/// reading exactly as it did at every call site.
pub struct Parse<L: rowan::Language> {
  green: rowan::GreenNode,
  diagnostics: std::vec::Vec<Diagnostic>,
  // `PhantomData<fn() -> L>`, not `PhantomData<L>`: the covariant function-pointer form keeps
  // `Parse` `Send`/`Sync` regardless of `L` and imposes no `L`-shaped drop obligation. A bare
  // `PhantomData<L>` would make `Parse` inherit auto-traits from a marker type it never holds.
  language: core::marker::PhantomData<fn() -> L>,
}

impl<L: rowan::Language> Parse<L> {
  /// The raw `rowan` tree. Walk this for generic tooling — formatters, highlighters.
  pub fn syntax(&self) -> rowan::SyntaxNode<L> {
    rowan::SyntaxNode::new_root(self.green.clone())
  }

  /// The tree's green root, borrowed rather than materialised.
  ///
  /// [`syntax`](Self::syntax) is the surface for a consumer that wants rowan's cursor API —
  /// parents, siblings, absolute offsets — and building one clones the `Arc` and allocates the
  /// root's cursor data. A walk that needs none of that reads the green tree directly, which is
  /// what the projection does and what [`Node`](super::project::Node) is; this is where such a
  /// walk starts, and it costs nothing.
  pub fn green(&self) -> &rowan::GreenNode {
    &self.green
  }

  /// Every diagnostic the parse recorded, in emission order — errors, warnings and recovery
  /// holes alike.
  pub fn diagnostics(&self) -> &[Diagnostic] {
    &self.diagnostics
  }

  /// Whether any grammar **error** was reported.
  ///
  /// This is the verdict the acceptance-parity gate compares against `syntactic`: both suites
  /// must agree here for every input, though their diagnostic *sets* need not match.
  ///
  /// Note what it does not count. A recovery hole and a warning both report
  /// [`Severity::Warning`], and neither is a rejection — a parse that recovered still accepted
  /// the document. Counting diagnostics instead of errors would make every recovered parse read
  /// as a failure.
  pub fn has_errors(&self) -> bool {
    self
      .diagnostics
      .iter()
      .any(|d| d.severity == Severity::Error)
  }
}

/// Why [`finish_root`] refused to mint a [`Parse`] out of a caller-built [`Cst`].
///
/// # Why this forwards the substrate's refusal instead of re-spelling it
///
/// The two mistakes a caller can make at this door have different remedies — a malformed event
/// stream is a bug in the sink that emitted it, and a tree too deep for `rowan` to *release* is a
/// shape that has to get shallower — so a caller has to be able to tell them apart. The type that
/// tells them apart is [`FinishError`](tokora::cst::FinishError), and it is upstream's.
///
/// A smear-side enum mirroring it was considered, and it is worse for a reason that is not taste.
/// `FinishError` is `#[non_exhaustive]`, this workspace tracks tokora's `main` **by branch** and
/// commits no lock, so what the crate compiles against moves between builds of the same commit —
/// the manifest says so outright. The depth refusal is the example rather than a hypothetical: it
/// is absent from the revision this workspace resolved when the door was made fallible and
/// present one commit later. A mirror could not carry the one variant that matters without
/// pinning the dependency, and a mirror that omitted it would be a smear type asserting a variant
/// set it does not own and cannot keep current.
///
/// So the coupling is real, and it is chosen rather than inherited: [`MintError::refusal`] hands
/// back upstream's own enum and a caller classifies with exactly the precision the tokora it
/// compiled against has. Against a revision that has no depth refusal there is nothing to
/// classify, because every refusal really is a malformed stream — a fact about that revision, not
/// a limitation of this type.
///
/// What the wrapper adds over returning `FinishError` bare is [`MintError::space`], the kind space
/// whose sink emitted the stream. That is what the panic this replaced carried, and it is the only
/// thing a runner shared by every dialect can say about *whose* stream it was.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MintError {
  space: &'static str,
  refusal: tokora::cst::FinishError,
}

impl MintError {
  /// The kind space whose sink emitted the refused stream.
  #[inline]
  pub const fn space(&self) -> &'static str {
    self.space
  }

  /// The substrate's own refusal, which is what classifies it.
  #[inline]
  pub const fn refusal(&self) -> tokora::cst::FinishError {
    self.refusal
  }
}

impl core::fmt::Display for MintError {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(
      f,
      "the {} lossless event stream was refused: {}",
      self.space, self.refusal
    )
  }
}

// No `source`: that would need `FinishError: core::error::Error` to be true of every revision this
// crate may be compiled against, and the whole point of the type above is that the upstream enum
// moves. `Display` carries the refusal's own message instead, so nothing is lost to a reader.
impl core::error::Error for MintError {}

/// Materialize `cst` at `root` and collect its diagnostics.
///
/// Shared by every dialect's `parse_document` and by its per-production drivers, so the
/// fallible-materialization contract and the diagnostic projection are stated once. The
/// materialization door names the root kind — it is NOT profile data — and hands back the inner
/// emitter, which is where the diagnostics live.
///
/// `space` is a `&'static str` argument rather than a [`KindSpace::NAME`](super::KindSpace::NAME)
/// lookup so this function needs no `KindSpace` bound at all; the caller already has the name.
/// [`MintError::space`] keeps naming the dialect, which is the one thing that made it useful.
///
/// # Why the partial door
///
/// The door is [`Cst::finish_partial`], not [`Cst::finish`], and the difference is exactly two
/// refusals: `finish` rejects an event stream that leaves a node open (`UnclosedNodes`) or leaves
/// a source byte covered by neither a committed token nor a recorded lexer-error diagnostic
/// (`UncoveredGap`); `finish_partial` closes the one and tiles the other as a `gap_kind` run.
/// **Both of those are reachable from ordinary input**, so under `finish` they were a panic on a
/// public entry point:
///
/// - When #57 was filed the nesting budget was the **lexer's** alone: every `{`, `[` and `(`
///   stepped the budget carried in the Logos `Extras`, nothing in this crate descended through
///   [`InputRef::descend`](tokora::InputRef::descend), and its ceiling was tokora's inherited,
///   general-purpose **500**. Both halves of that have since moved — see issue #61 for why an
///   inherited ceiling was not a ceiling, and [`crate::lossless::depth`] for why a pair-blind
///   lexer tally was not the parse's recursion. The lexer's counter is still there, still steps
///   on every delimiter, and is still what latches the boundary below; what it is no longer is
///   the only thing between a document and the stack.
/// - The bracket **past** the budget therefore fails its lex, and a resource-limit trip *latches a
///   poison boundary*: the scanner refuses to rebuild a lexer past that offset. No token and no
///   diagnostic can ever cover the tail, and a dialect's `document_entry` `skip_while` drain
///   cannot reach it either — that drain is the mechanism which otherwise guarantees coverage.
/// - So `finish` reported `UncoveredGap` and `parse_document` panicked, at 501 open brackets, for
///   input an IDE produces by typing. That was smear issue #57.
///
/// Under this door the same parse hands back a tree over every byte (`tree.text() == source`
/// still holds, the un-lexable tail tiled as gaps) plus the limit trip already on the diagnostic
/// channel, which is where a consumer routes on it.
///
/// # The trip ends the document, deliberately — smear issue #61's second half
///
/// The latch above is tokora's design and smear **keeps** it, which is a posture and therefore
/// owes a reason. The reason is what the boundary means: a depth trip is a *resource refusal*, not
/// a syntax error, and `poison_boundary` states exactly that — "no amount of further input will
/// fix this". Resuming past it would mean re-entering the same unbounded descent the budget exists
/// to stop, on a document that has already proved it goes there; a ceiling that is re-armed after
/// every trip bounds one region and not the parse.
///
/// The case against it is an IDE, where one over-deep region voiding the rest of the file is close
/// to the worst available behaviour, and #61 is right that lowering the ceiling from 500 to a
/// stack-derived number makes the latch that much easier to reach. Three measured facts are why it
/// is still the right posture:
///
/// - **It is not reachable by typing.** A trip needs more simultaneously open brackets than the
///   ceiling. The deepest GraphQL document in this repository — 472 fixtures, real-world subgraph
///   schemas included — reaches **11**. A file that trips is machine-generated or hostile.
/// - **The alternative is worse for the same consumer.** The tree still covers every byte and the
///   prefix is fully structured; a parse that refused the *whole* document on a depth trip would
///   give an IDE strictly less.
/// - **The syntactic door cannot express the question.** There the whole parse returns `Err`, so
///   there is no partial result for a latch to void. Keeping the two doors' answers the same is
///   what the acceptance-parity gate compares.
///
/// What the latch costs is that a consumer sees the trip as an ordinary error diagnostic on the
/// tripping bracket plus one opaque region, rather than as "this document is too deeply nested".
/// [`Diagnostic`] drops the typed payload at this boundary on purpose — that is what keeps
/// [`Parse`] lifetime-free — so distinguishing the two would be a change to the diagnostic
/// surface rather than to the latch, and it is recorded here as the residual it is.
///
/// # The parse-side budget ends the document too, and reaches the same tail the same way
///
/// The posture above is the *lexer's* trip. `crate::lossless::depth::descend` is the other
/// mechanism the same ceiling feeds, and until smear issue #169 it did not share the posture: it
/// reported and returned, the `Err` unwound the nest, and a root loop resynchronised and re-read
/// the abandoned tail one token at a time — 67 diagnostics for one refusal, growing with the
/// document. It *looked* correct only because the lexer's cheaper check normally trips first on a
/// well-formed document, which is an accident of two numbers being equal rather than a property
/// anything enforces.
///
/// **The refusal's tail arrives here as gaps, exactly as the lexer trip's does**, and for a
/// stronger reason than symmetry: tokora emits a diagnostic for every lexer error a scan crosses,
/// so any drain over an unparsed tail turns one refusal into one-plus-`n`. The lexer trip is
/// spared that by the poison boundary, which makes the tail unreachable; the parse-side refusal is
/// spared it by not reading the tail at all. Both therefore land on the partial door above, and
/// what it costs — an opaque region rather than committed tokens — is the same cost, recorded once
/// in `Why the partial door`. `descend`'s `The refusal ends the document` note carries the repair
/// and where the terminal state lives.
///
/// Nothing else is relaxed. Balance underflow, close identity, retro-wrap integrity, kind
/// hygiene, span discipline and the token-channel wall are enforced identically through both
/// doors, so the [`MintError`] below still reports a genuine sink bug — and it names the
/// [`FinishError`](tokora::cst::FinishError) it refused, because a message that dropped it made
/// #57 diagnosable only by patching this line.
///
/// The widening costs nothing for input that already worked: gap **placement** differs between
/// the two doors only for a run that trails no committed token, and only when the stream is
/// unbalanced — a shape `finish` refuses outright rather than places differently.
///
/// # What downstream assumes about a [`Parse`], and what this door does not promise
///
/// This is `pub`, and it is the only route to a [`Parse`] that does not go through one of this
/// crate's parsers. That is deliberate — a dialect built on `tokora` outside this crate needs a way
/// to finish its own event stream — but it means **a `Parse` is not evidence that a parser produced
/// it**, and several consumers were written as though it were. al8n/smear#198's twenty-third round
/// found the first of them; the rest are recorded here rather than repaired, because deciding what
/// this door should promise is a design question and not a bug fix.
///
/// - **Bytes do not bound structure.** A balanced pair of zero-width nodes adds elements and no
///   text, so an empty source can carry an arbitrarily large tree. A consumer pricing a walk from
///   `source.len()` charges one unit and does unbounded work. A dialect's verified-pair type now
///   carries its own element count for exactly this reason; anything else that sizes work from a
///   length is making the same assumption.
/// - **Depth is not bounded by the lexer's ceiling.** **Four** green-tree walks in
///   `crate::lossless::project` recursed — `verify_source_at`, `verify_source_counted`,
///   `reject_holes`, and the mutually recursive `node_extent`/`extent_of` pair — and each rested on
///   a comment naming the lexer's `MAX_NESTING_DEPTH` as the bound on the tree's depth. A minted
///   stream honours no such ceiling.
///
///   Each carried a counter and refused at
///   [`MAX_GREEN_DEPTH`](crate::lossless::project::MAX_GREEN_DEPTH), which was called safe on any
///   tree and was not: a counter bounds a depth and the frames were the host's, so a walk on a
///   thread too small to hold the ceiling aborted before reaching the refusal. **None of the four
///   spends a native frame per level now**, so the refusal is reached on any stack. Three halves of
///   the problem were listed here as still open; one of them has since been closed, and each is
///   named below with which:
///
///   - **Construction.** The over-deep tree is built by `finish_partial`, from events already
///     recorded in the `Cst` this function is handed. There is nothing *here* to inspect and no way
///     to refuse before the tree exists — [`Cst`] publishes no depth, no event count and no event
///     access, and `finish_partial` consumes it by value — so the check belongs upstream, in the
///     builder that sees the events as they arrive.
///
///     **Upstream now makes it, and this door forwards the refusal as a value.** tokora gates the
///     one replay-walk door that pushes a builder node and answers a typed refusal, which arrives
///     here as [`MintError`] rather than as a panic. What this function no longer does is assert
///     that a refusal means a malformed stream: a depth refusal is a well-formed tree nobody could
///     dispose of, which is a different mistake with a different remedy, and the caller is the one
///     who can tell them apart.
///
///     **The forwarding is only as good as the revision it is compiled against.** This workspace
///     names tokora by branch and commits no lock, so a build of this very commit may resolve a
///     revision whose `finish_partial` has no depth ceiling at all; against that one the hazard
///     below stands exactly as it did, because there is no refusal to forward. That is the
///     residual, and it is the dependency edge's rather than this door's.
///   - **Destruction.** `rowan` drops a green tree recursively, so a tree deep enough to overflow
///     is a crash in its own destructor. That is reachable through `rowan`'s public builder without
///     this crate being involved at all, and no guard placed after materialisation can help: the
///     value's mere existence is the hazard. Refusing at the open that would cross the ceiling is
///     what makes the difference — the tree the failing call drops is one at or under it — and
///     that is upstream's to do, for the same reason construction is.
///   - **Projection.** What the four walks above *gate* is the dialect projection's own node
///     dispatch. It was a native recursion with no counter of its own, bounded only by
///     `MAX_GREEN_DEPTH` being small enough — and in an unoptimised build it was not: on a 2 MiB
///     thread it descended 514 green levels and aborted at 516, which is exactly what the doors
///     produce at the lexer's `HARD_MAX`. **That half is closed.** The four cycles it recursed
///     through are worklists now (al8n/smear#201), so the dispatch spends no native frame per
///     level either and the ceiling it inherits from the walks above is a bound on heap rather
///     than a wish about stack.
///
///   So the honest statement of this entry is that **neither the walks nor the projection behind
///   them spends native stack, construction is bounded by whatever substrate revision is resolved,
///   and `rowan`'s own builder — and its destructor — are bounded by nothing this crate can
///   reach**. Of the three halves listed here the first is upstream's, the second is `rowan`'s and
///   the third is closed.
///
///   The first version of this list said *three* walks and did not mention either lifecycle half.
///   That is worth recording where it happened: a general claim written without enumerating what it
///   ranges over is this workspace's most repeated defect, and this is the instance where the
///   incomplete enumeration **was itself the deliverable**. al8n/smear#198.
/// - **`has_errors` and `diagnostics` are whatever the stream said.** A minted parse can report a
///   clean document it did not parse, so a consumer using them as a precondition is trusting the
///   caller.
/// - **Kind placement is checked, not assumed.** A dialect's projection matches on its own kind
///   enum and answers `UnexpectedChild` or `MissingChild` for a shape it does not expect, so an
///   arbitrary tree is *refused* rather than misread. This one is already safe and is listed so the
///   list is not read as "everything here is broken".
///
/// What is **not** in question is memory safety: every walk above is safe Rust over a green tree,
/// and the failure modes are a stack overflow and an unpriced walk, not corruption.
///
/// # Errors
///
/// [`MintError`] when the substrate refuses to materialise the recorded stream, which its own
/// [`FinishError`](tokora::cst::FinishError) classifies. It replaced a `panic!` on this line: a
/// safe public function that aborts its caller's process cannot be the way a caller learns its
/// own event stream was rejected, and the message it panicked with asserted a malformed stream,
/// which is exactly what a depth refusal is not.
pub fn finish_root<'inp, L, Lx, Em>(
  cst: Cst<'inp, Lx, Em>,
  root: u16,
  space: &'static str,
) -> Result<Parse<L>, MintError>
where
  L: rowan::Language,
  Lx: Lexer<'inp>,
  // The two clauses the materialization door itself carries. Neither is implied by `Lexer<'inp>`:
  // the sink
  // slices the *source* to build green tokens, and it stores offsets as `u32` because a rowan
  // green tree does. They are restated rather than hidden behind a helper trait because they are
  // upstream's requirement, not this module's, and a reader chasing a failure at a dialect's
  // `finish_root` should land on the same two names tokora reports.
  Lx::Source: tokora::cst::CstText,
  Lx::Offset: TryInto<u32>,
  Em: DiagnosticSource,
{
  // `finish_partial`, NOT `finish`. See this function's `Why the partial door` note.
  let (green, emitter) = cst.finish_partial(root);
  // The emitter is dropped on the refusal path on purpose. Its diagnostics describe a parse whose
  // tree does not exist, and there is no `Parse` to hang them on; a caller whose stream was
  // refused has a stream to fix, not diagnostics to route.
  let green = green.map_err(|refusal| MintError { space, refusal })?;

  Ok(Parse {
    green,
    diagnostics: emitter.collect_diagnostics(),
    language: core::marker::PhantomData,
  })
}

/// [`finish_root`] for a [`Cst`] one of *this crate's own* parsers built, where the refusal is
/// unreachable and the dialect doors keep their infallible signatures.
///
/// # Panics
///
/// If the substrate refuses the stream, which no *production* in this crate can make it do. The
/// depth refusal in particular cannot fire here, and the reason is three numbers rather than a
/// wish:
///
/// - Every lossless door installs `min(requested, HARD_MAX)` as the parse's recursion budget, so
///   no parse this crate performs holds more than the lexer's `HARD_MAX` brackets open.
/// - A selection chain at that maximum materialises **515** green levels. The tree costs two
///   levels per open bracket plus three, measured on this crate's own `parse_document_with_limits`
///   over `1..=256`, and the 24-bracket row of that same measurement is the **51** that
///   `crate::lossless::project::MAX_GREEN_DEPTH`'s header already records — so the figure is
///   checkable against a number that was recorded before this function existed.
/// - tokora's tree ceiling, where the resolved revision has one, is above 515; where it has none
///   there is nothing to trip.
///
/// So the claim is that 515 clears the substrate's ceiling, not that a refusal "cannot happen".
/// Should a substrate ceiling ever drop under 515, or a door stop clamping to `HARD_MAX`, this
/// panics with the ceiling in the message.
///
/// The other refusals are reachable, deliberately: a dialect's `test_support` probe severs the
/// token channel to prove the `space` argument is threaded rather than assumed. That is why the
/// message below reports the refusal it got rather than naming a cause.
///
/// `HARD_MAX` is the lexer crate's, and this module does not name it: the substrate is generic
/// over `L: Lexer`, and `crate::lossless::project::MAX_DOOR_BRACKETS` is the affordance it states
/// instead — the crate root asserts the two against each other.
// Gated on having a caller. `rowan` alone compiles this module with neither dialect, and the two
// dialect runners are the only callers, so without this the one configuration that builds the
// substrate without a dialect reports dead code — which `-D warnings` turns into a failure of a
// leg nothing else in this file exercises.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) fn finish_parsed_root<'inp, L, Lx, Em>(
  cst: Cst<'inp, Lx, Em>,
  root: u16,
  space: &'static str,
) -> Parse<L>
where
  L: rowan::Language,
  Lx: Lexer<'inp>,
  Lx::Source: tokora::cst::CstText,
  Lx::Offset: TryInto<u32>,
  Em: DiagnosticSource,
{
  finish_root(cst, root, space).unwrap_or_else(|refused| {
    panic!(
      "{refused}. No production in this crate emits a stream this door refuses, and depth in \
       particular cannot: every lossless door clamps its recursion budget to a bracket ceiling the \
       crate root asserts is at most MAX_DOOR_BRACKETS = {}, and a chain at that maximum \
       materialises fewer green levels than MAX_GREEN_DEPTH = {}",
      crate::lossless::project::MAX_DOOR_BRACKETS,
      crate::lossless::project::MAX_GREEN_DEPTH,
    )
  })
}

/// The emitter half of a lossless context, reduced to the one thing `finish_root` asks of it.
///
/// # Why a trait rather than naming `Verbose<Err, SimpleSpan, Brand>`
///
/// `Verbose`'s error parameter is the dialect's own error container, and its brand parameter is
/// the dialect's grammar marker, so naming the concrete emitter here would put two dialect types
/// into this module's signature — the Lego rule's exact prohibition. Naming them as *parameters*
/// instead (`Verbose<Err, SimpleSpan, Brand>` with `Err` and `Brand` free) is worse in a subtler
/// way: it pins the emitter's shape, so a dialect that ever needed a different recording emitter
/// could not use the shared runner at all.
///
/// One method, and it is the projection `Parse` needs. The blanket impl below covers `Verbose`
/// for every error and brand, so no dialect writes an impl.
pub trait DiagnosticSource {
  /// Project every recorded diagnostic into the owned, source-independent form.
  fn collect_diagnostics(&self) -> std::vec::Vec<Diagnostic>;
}

impl<Err, Brand: ?Sized> DiagnosticSource
  for tokora::emitter::Verbose<Err, tokora::SimpleSpan, Brand>
{
  fn collect_diagnostics(&self) -> std::vec::Vec<Diagnostic> {
    // `Verbose` exposes `diagnostics()` and nothing else — there is no `errors()` and no
    // `warnings()`. Each item carries `span()`, `labels()`, `kind()`, `severity()`, `payload()`.
    self
      .diagnostics()
      .map(|d| Diagnostic {
        span: d.span().start()..d.span().end(),
        severity: d.severity(),
        skipped_tokens: match d.kind() {
          tokora::emitter::DiagnosticKind::SkippedRegion(n) => Some(n),
          _ => None,
        },
      })
      .collect()
  }
}
