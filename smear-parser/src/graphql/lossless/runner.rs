//! The Sink runner: binds a source to a `cst::Sink`, drives the document production, and
//! materializes a `rowan` tree.

use tokora::{
  Source,
  cst::{CstProfile, KindValidator, Sink},
  emitter::Severity,
};
// `parse_str` lives on tokora's `Parse` trait, whose name this module already uses for its own
// result type — imported anonymously so the trait is in scope without shadowing it.
use tokora::Parse as _;

use super::{GraphqlLosslessLexer, GraphqlLosslessSlice, GraphqlLosslessToken, SyntaxNode};
use crate::graphql::kinds::SyntaxKind as K;

/// The profile every GraphQL lossless parse uses.
///
/// **Four facts, none defaulted** (`tokora/src/cst/profile.rs:140`), and note what is *not*
/// among them: there is **no root kind**. The root is named once, at `finish(root_kind)`.
///
/// - **arg 1, the mapper** — `fn(&T) -> u16`, which makes `CstProfile` generic over the token
///   type. It is [`super::kind_map::token_kind`], the one place the lexer's vocabulary and this
///   crate's kind space are put in correspondence.
/// - **arg 2, the validator** — `KindValidator::new(fn(u16) -> bool)`, a plain **non-capturing**
///   fn pointer. There is no `with_validator` builder method; the validator is not optional. It
///   rides as data rather than on a type parameter because `rowan::Language::kind_from_raw` has
///   no fallible form — a bad kind could only panic at query time, so it is refused at the emit
///   door instead.
/// - **args 3 and 4** — `error_kind` and `gap_kind`. `CstProfile::new` asserts in *every* build
///   that its own validator admits both, so a profile cannot describe a sink that would refuse
///   its own output.
pub fn profile<'inp, Src>() -> CstProfile<GraphqlLosslessToken<'inp, Src>>
where
  Src: Source<usize> + ?Sized,
{
  CstProfile::new(
    super::kind_map::token_kind::<GraphqlLosslessSlice<'inp, Src>>,
    KindValidator::new(|raw| K::from_raw(raw).is_some()),
    K::Error.raw(),
    K::Gap.raw(),
  )
}

/// The recording emitter every lossless driver pins.
///
/// `Verbose<Error, S = SimpleSpan, Lang = ()>` — the **third** parameter is the grammar brand,
/// and `Emitter<'inp, L, Lang>` is implemented only where it matches. A bare
/// `Verbose::default()` leaves it at `()` and the context then fails to be a
/// `ParseContext<…, GraphQL>`, so a branded grammar has to spell all three.
pub(crate) type LosslessEmitter<'inp> = tokora::emitter::Verbose<
  super::GraphqlLosslessErrors<&'inp str>,
  tokora::SimpleSpan,
  crate::graphql::GraphQL,
>;

/// The `Sink` every lossless driver records into.
pub(crate) type LosslessSink<'inp> =
  Sink<'inp, GraphqlLosslessLexer<'inp, str>, LosslessEmitter<'inp>>;

/// Materialize `sink` at the root kind and collect its diagnostics.
///
/// Shared by [`parse_str`] and by the per-production drivers under `test_support`, so the
/// root kind, the fallible-materialization contract and the diagnostic projection are stated
/// once. `finish` names the root kind — it is NOT profile data — and hands back the inner
/// emitter, which is where the diagnostics live.
pub(crate) fn finish_root(sink: LosslessSink<'_>) -> Parse {
  let (green, emitter) = sink.finish(K::Root.raw());
  let green = green.expect("the GraphQL lossless sink emitted a malformed event stream");

  // `Verbose` exposes `diagnostics()` and nothing else — there is no `errors()` and no
  // `warnings()`. Each item carries `span()`, `labels()`, `kind()`, `severity()`, `payload()`.
  let diagnostics = emitter
    .diagnostics()
    .map(|d| Diagnostic {
      span: d.span().start()..d.span().end(),
      severity: d.severity(),
      skipped_tokens: match d.kind() {
        tokora::emitter::DiagnosticKind::SkippedRegion(n) => Some(n),
        _ => None,
      },
    })
    .collect();

  Parse { green, diagnostics }
}

/// One diagnostic a lossless parse recorded, owned and source-independent.
///
/// **Why not the typed payload.** `Verbose::diagnostics()` hands back `Diagnostic<'_, S, Error>`
/// borrowed from the emitter, and this crate's dialect error is keyed to the *source slice*
/// (`GraphqlError<S>`), so carrying the payload would give [`Parse`] the parse's lifetime. A
/// lifetime-free `Parse` is what lets a consumer cache one per file, so the payload is dropped
/// at this boundary and the two facts that survive are the two an IDE actually routes on.
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

/// The result of a GraphQL lossless parse.
pub struct Parse {
  green: rowan::GreenNode,
  diagnostics: std::vec::Vec<Diagnostic>,
}

impl Parse {
  /// The raw `rowan` tree. Walk this for generic tooling — formatters, highlighters.
  pub fn syntax(&self) -> SyntaxNode {
    SyntaxNode::new_root(self.green.clone())
  }

  /// Every diagnostic the parse recorded, in emission order — errors, warnings and recovery
  /// holes alike.
  pub fn diagnostics(&self) -> &[Diagnostic] {
    &self.diagnostics
  }

  /// Whether any grammar **error** was reported.
  ///
  /// This is the verdict the acceptance-parity gate compares against `syntactic`: both
  /// suites must agree here for every input, though their diagnostic *sets* need not match.
  ///
  /// Note what it does not count. A recovery hole and a warning both report
  /// [`Severity::Warning`], and neither is a rejection — a parse that recovered still accepted
  /// the document. Counting diagnostics instead of errors would make every recovered parse
  /// read as a failure.
  pub fn has_errors(&self) -> bool {
    self
      .diagnostics
      .iter()
      .any(|d| d.severity == Severity::Error)
  }
}

/// Parse a `&str` as a GraphQL document, losslessly.
pub fn parse_str(src: &str) -> Parse {
  // Sink::new takes the source at construction rather than at finish, which removes the one
  // way a caller could hand materialization a different buffer than the spans were measured
  // against. Argument order is (source, inner emitter, profile) — the emitter is SECOND.
  let mut sink: LosslessSink<'_> = Sink::new(src, LosslessEmitter::default(), profile::<str>());

  // Productions take `&mut InputRef`, never `&mut Sink`. The sink reaches them as the emitter
  // half of the parse context: `(&mut sink, cache)` is a `ParseContext`, and `Parser` drives it.
  //
  // `Lang` needs the turbofish. `apply`'s signature uses it only in bounds — the returned
  // `Parser<F, L, O, Ctx>` does not carry it, and `Ctx: ParseContext<'inp, L, Lang>` holds for
  // every `Lang` — so nothing forces it to `GraphQL` and inference silently settles on `()`,
  // which then fails to match this production's branded `InputRef`.
  //
  // `Src` needs its own turbofish for a second reason: `str` and `&str` both project
  // `Slice<'inp> = &'inp str`, so the lexer type alone leaves the production's source parameter
  // genuinely ambiguous. `str` is the one that matches `parse_str`'s `L::Source = str`.
  let _out = tokora::Parser::with_context((
    &mut sink,
    tokora::cache::DefaultCache::<GraphqlLosslessLexer<'_, str>>::default(),
  ))
  // `document_entry`, not `document`: the driver's result is discarded below, so an `Err` that
  // escaped the document production would leave the rest of the source uncommitted and
  // `finish` would refuse it as an `UncoveredGap`. The entry drains what an escape left behind,
  // which turns the one failure mode `parse_str` cannot report into a reportable parse.
  .apply::<_, crate::graphql::GraphQL>(super::document::document_entry::<str, _>)
  .parse_str(src);

  finish_root(sink)
}

/// Test-only scaffolding for probing the sink's own kind-validator door.
///
/// Every driver elsewhere in this suite (`lossless_drivers!`, `trivia.rs`'s `drive!`) runs a
/// *production* — a function that only ever names a kind from
/// [`K::ALL`](crate::graphql::kinds::SyntaxKind::ALL)'s own space, because that space is all a
/// production can spell. There is therefore no production-shaped way to observe [`profile`]'s
/// validator refuse a kind: the refusal only has something to refuse when the caller hands the
/// sink a kind no production would ever construct. This module is that caller — a direct spend
/// of the sink's own retro-wrap door, through the crate's real, shipped `profile()`, so the
/// validator under test is the one every parse actually runs.
#[doc(hidden)]
pub mod test_support {
  use tokora::{InputRef, Parse as _, cache::DefaultCache, emitter::CstEmitter as _};

  use super::{
    GraphqlLosslessLexer, LosslessEmitter, LosslessSink, Parse, Sink, finish_root, profile,
  };
  use crate::graphql::GraphQL;

  type TestCtx<'inp, 'sink> = (
    &'sink mut LosslessSink<'inp>,
    DefaultCache<'inp, GraphqlLosslessLexer<'inp, str>>,
  );
  type TestInput<'inp, 'input, 'sink> =
    InputRef<'inp, 'input, GraphqlLosslessLexer<'inp, str>, TestCtx<'inp, 'sink>, GraphQL>;

  /// Opens a node at `kind` over `src` and materializes.
  ///
  /// The body is the exact retro-wrap sequence every `node`/`node_at` production spends to
  /// open its own node — [`cst_mark`] then [`cst_start_at`] then [`cst_finish`]
  /// (`tokora/src/parser/node.rs`'s own `wrap`) — with `kind` standing in for a production's.
  /// `'inp` is named and threaded from `src`, not elided, for the reason `trivia.rs`'s driver
  /// and `lossless_drivers!` both record: a closure's parameter type is spelled out explicitly
  /// (see [`TestInput`]), and an elided lifetime there is free to be inferred shorter than
  /// `sink`'s, which the borrow checker then refuses. Nothing else in the crate calls this; it
  /// exists for `tests/lossless_runner.rs`'s validator-discrimination test, which always passes
  /// `""` — the node this probes wraps zero tokens either way.
  ///
  /// # Panics
  ///
  /// Whatever the sink's own [`cst_start_at`] panics on. Under the shipped [`profile`], any
  /// `kind` at or past `K::ALL.len()` (`K` is
  /// [`crate::graphql::kinds::SyntaxKind`]) — the reserved tombstone (`u16::MAX`) panics too,
  /// but is refused by every validator, including
  /// [`KindValidator::accept_all`](tokora::cst::KindValidator::accept_all), so it would not
  /// discriminate the real validator from a permissive one.
  ///
  /// [`cst_mark`]: tokora::emitter::CstEmitter::cst_mark
  /// [`cst_start_at`]: tokora::emitter::CstEmitter::cst_start_at
  /// [`cst_finish`]: tokora::emitter::CstEmitter::cst_finish
  pub fn open_raw_kind<'inp>(src: &'inp str, kind: u16) -> Parse {
    let mut sink: LosslessSink<'inp> = Sink::new(src, LosslessEmitter::default(), profile::<str>());

    let _out = tokora::Parser::with_context::<GraphqlLosslessLexer<'_, str>, (), _>((
      &mut sink,
      DefaultCache::<GraphqlLosslessLexer<'_, str>>::default(),
    ))
    .apply::<_, GraphQL>(|inp: &mut TestInput<'inp, '_, '_>| {
      let mark = inp.emitter().cst_mark();
      inp.emitter().cst_start_at(mark, kind);
      inp.emitter().cst_finish(kind);
      inp.skip_while(|_| true)
    })
    .parse_str(src);

    finish_root(sink)
  }
}
