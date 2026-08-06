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

/// Materialize `cst` at `root` and collect its diagnostics.
///
/// Shared by every dialect's `parse_document` and by its per-production drivers, so the
/// fallible-materialization contract and the diagnostic projection are stated once. The
/// materialization door names the root kind — it is NOT profile data — and hands back the inner
/// emitter, which is where the diagnostics live.
///
/// `space` is a `&'static str` argument rather than a [`KindSpace::NAME`](super::KindSpace::NAME)
/// lookup so this function needs no `KindSpace` bound at all; the caller already has the name.
/// The panic message keeps naming the dialect, which is the one thing that made it useful.
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
/// - The nesting budget is the **lexer's**, not the parser's. Every `{`, `[` and `(` steps the
///   [`Limiter`](tokora::state::tracker::Limiter) carried in the Logos `Extras`, and that
///   tracker's inherited ceiling is tokora's *general-purpose* **500**
///   ([`RecursionLimiter::new`](tokora::state::recursion_tracker::RecursionLimiter)), not the
///   parser-facing 64 — nothing in this crate descends through
///   [`InputRef::descend`](tokora::InputRef::descend), so the parser-side limiter is never
///   consulted at all.
/// - The **501st** simultaneously-open bracket therefore fails its lex, and a resource-limit trip
///   *latches a poison boundary*: the scanner refuses to rebuild a lexer past that offset. No
///   token and no diagnostic can ever cover the tail, and a dialect's `document_entry`
///   `skip_while` drain cannot reach it either — that drain is the mechanism which otherwise
///   guarantees coverage.
/// - So `finish` reported `UncoveredGap` and `parse_document` panicked, at 501 open brackets, for
///   input an IDE produces by typing. That was smear issue #57.
///
/// Under this door the same parse hands back a tree over every byte (`tree.text() == source`
/// still holds, the un-lexable tail tiled as gaps) plus the limit trip already on the diagnostic
/// channel, which is where a consumer routes on it.
///
/// Nothing else is relaxed. Balance underflow, close identity, retro-wrap integrity, kind
/// hygiene, span discipline and the token-channel wall are enforced identically through both
/// doors, so the panic below still guards a genuine sink bug — and it now names the
/// [`FinishError`](tokora::cst::FinishError) it refused, because a message that dropped it made
/// #57 diagnosable only by patching this line.
///
/// The widening costs nothing for input that already worked: gap **placement** differs between
/// the two doors only for a run that trails no committed token, and only when the stream is
/// unbalanced — a shape `finish` refuses outright rather than places differently.
pub fn finish_root<'inp, L, Lx, Em>(
  cst: Cst<'inp, Lx, Em>,
  root: u16,
  space: &'static str,
) -> Parse<L>
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
  let green = green
    .unwrap_or_else(|e| panic!("the {space} lossless sink emitted a malformed event stream: {e}"));

  Parse {
    green,
    diagnostics: emitter.collect_diagnostics(),
    language: core::marker::PhantomData,
  }
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
