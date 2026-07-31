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
  let mut sink: Sink<'_, GraphqlLosslessLexer<'_, str>, _> = Sink::new(
    src,
    // `Verbose<Error, S = SimpleSpan, Lang = ()>` — the **third** parameter is the grammar
    // brand, and `Emitter<'inp, L, Lang>` is implemented only where it matches. A bare
    // `Verbose::default()` leaves it at `()` and the context then fails to be a
    // `ParseContext<…, GraphQL>`, so a branded grammar has to spell all three.
    tokora::emitter::Verbose::<
      super::GraphqlLosslessErrors<&str>,
      tokora::SimpleSpan,
      crate::graphql::GraphQL,
    >::default(),
    profile::<str>(),
  );

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
  .apply::<_, crate::graphql::GraphQL>(super::document::document::<str, _>)
  .parse_str(src);

  // `finish` names the root kind — it is NOT profile data — and materialization is FALLIBLE.
  // It hands back the inner emitter, which is where the diagnostics live.
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
