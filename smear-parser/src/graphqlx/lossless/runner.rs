//! The Sink runner: binds a source to a `cst::Sink`, drives the document production, and
//! materializes a `rowan` tree.

use tokora::{
  Source,
  cst::{Cst, CstProfile, KindValidator, Sink, parse_lossless},
};

use super::{GraphqlxLosslessLexer, GraphqlxLosslessSlice, GraphqlxLosslessToken};
use crate::{graphqlx::kinds::SyntaxKind as K, lossless::KindSpace};

/// The profile every GraphQLx lossless parse uses.
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
pub fn profile<'inp, Src>() -> CstProfile<GraphqlxLosslessToken<'inp, Src>>
where
  Src: Source<usize> + ?Sized,
{
  CstProfile::new(
    super::kind_map::token_kind::<GraphqlxLosslessSlice<'inp, Src>>,
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
/// `ParseContext<…, GraphQLx>`, so a branded grammar has to spell all three.
pub(crate) type LosslessEmitter<'inp> = tokora::emitter::Verbose<
  super::GraphqlxLosslessErrors<&'inp str>,
  tokora::SimpleSpan,
  crate::graphqlx::GraphQLx,
>;

/// The `Sink` every lossless driver records into.
///
/// Named only as the emitter half of a driver's **context pair** — `Sink::new` is tokora-private,
/// so the one way to mint one is [`parse_lossless`], which takes the source once and uses that
/// same argument for the sink and the input.
pub(crate) type LosslessSink<'inp> =
  Sink<'inp, GraphqlxLosslessLexer<'inp, str>, LosslessEmitter<'inp>>;

/// The spent sink [`parse_lossless`] hands back — the one door to materialization.
pub(crate) type LosslessCst<'inp> =
  Cst<'inp, GraphqlxLosslessLexer<'inp, str>, LosslessEmitter<'inp>>;

/// Materialize `cst` at the root kind and collect its diagnostics.
///
/// Shared by [`parse_str`] and by the per-production drivers under `test_support`, so the
/// root kind is named once. Everything below that — the fallible-materialization contract and
/// the diagnostic projection — is [`crate::lossless::runner::finish_root`]'s; this wrapper's
/// whole content is *which* root kind and *which* dialect the panic names.
pub(crate) fn finish_root(cst: LosslessCst<'_>) -> Parse {
  crate::lossless::runner::finish_root(cst, K::Root.raw(), <K as KindSpace>::NAME)
}

/// One diagnostic a GraphQLx lossless parse recorded.
///
/// Nothing in it is per-dialect — it is owned, source-independent and lifetime-free by design —
/// so it is the substrate's type re-exported rather than a second copy of it.
pub use crate::lossless::runner::Diagnostic;

/// The result of a GraphQLx lossless parse.
///
/// A **type alias**, not a newtype, for the reason the substrate's own `Parse` records: a newtype
/// would need `syntax`, `diagnostics` and `has_errors` re-written per dialect, which is the
/// duplication the lift exists to remove.
///
/// It is a *different* alias from GraphQL's, and deliberately incompatible with it: the language
/// parameter is [`GraphQLxLang`](crate::graphqlx::kinds::GraphQLxLang), so a GraphQLx tree cannot
/// be handed to a GraphQL typed wrapper. The two spaces do not even agree on what raw `0` means.
pub type Parse = crate::lossless::runner::Parse<crate::graphqlx::kinds::GraphQLxLang>;

/// Parse a `&str` as a GraphQLx document, losslessly.
///
/// The production is `document.rs`'s `document_entry` — the mixed
/// root, which admits imports, executable definitions, type-system definitions and extensions in
/// any order, followed by a drain. The **entry** and not `document` itself: this function discards
/// its parser's result, so an `Err` escaping the document production would leave the rest of the
/// source uncommitted and `finish` would refuse it as a `FinishError::UncoveredGap`. The drain
/// turns the one failure mode `parse_str` cannot report into a reportable parse.
///
/// The SDL-only root has no entry here; a schema-only consumer calls `document.rs`'s
/// `type_system_document` directly.
pub fn parse_str(src: &str) -> Parse {
  // `parse_lossless` is the only door that mints a `Sink`: it takes the source ONCE and uses
  // that one argument for both the sink and the input, so the buffer the tree's text comes from
  // and the buffer the parse reads cannot be two different buffers. Argument order is
  // (source, lexer state, inner emitter, profile, cache, parser).
  //
  // `Lang` needs the turbofish. The driver's signature uses it only in bounds — nothing in the
  // argument list carries it, and `Ctx: ParseContext<'inp, L, Lang>` holds for every `Lang` —
  // so inference silently settles on `()`, which then fails to match this production's branded
  // `InputRef`. The lexer is spelled alongside it because `Lang` is the SECOND parameter.
  //
  // `Src` needs its own turbofish for a second reason: `str` and `&str` both project
  // `Slice<'inp> = &'inp str`, so the lexer type alone leaves the production's source parameter
  // genuinely ambiguous. `str` is the one that matches `parse_str`'s `L::Source = str`.
  let (cst, _out) =
    parse_lossless::<GraphqlxLosslessLexer<'_, str>, crate::graphqlx::GraphQLx, _, _, _, _>(
      src,
      Default::default(),
      LosslessEmitter::default(),
      profile::<str>(),
      tokora::cache::DefaultCache::<GraphqlxLosslessLexer<'_, str>>::default(),
      super::document::document_entry::<str, _>,
    );

  finish_root(cst)
}

/// Test-only scaffolding for probing the sink's own kind-validator door.
///
/// Every driver elsewhere in this suite runs a *production* — a function that only ever names a
/// kind from [`K::ALL`](crate::graphqlx::kinds::SyntaxKind::ALL)'s own space, because that space
/// is all a production can spell. There is therefore no production-shaped way to observe
/// [`profile`]'s validator refuse a kind: the refusal only has something to refuse when the caller
/// hands the sink a kind no production would ever construct. This module is that caller — a direct
/// spend of the sink's own retro-wrap door, through the crate's real, shipped `profile()`, so the
/// validator under test is the one every parse actually runs.
#[doc(hidden)]
pub mod test_support {
  use tokora::{InputRef, cache::DefaultCache};

  use super::{
    GraphqlxLosslessLexer, LosslessEmitter, LosslessSink, Parse, finish_root, parse_lossless,
    profile,
  };
  use crate::graphqlx::GraphQLx;

  type TestCtx<'inp> = (
    LosslessSink<'inp>,
    DefaultCache<'inp, GraphqlxLosslessLexer<'inp, str>>,
  );
  type TestInput<'inp, 'input> =
    InputRef<'inp, 'input, GraphqlxLosslessLexer<'inp, str>, TestCtx<'inp>, GraphQLx>;

  /// Opens a node at `kind` over `src` and materializes.
  ///
  /// The body is the exact retro-wrap sequence every `node`/`node_at` production spends to open
  /// its own node — [`cst_mark`] then [`cst_start_at`] then [`cst_finish`]
  /// (`tokora/src/parser/node.rs`'s own `wrap`) — with `kind` standing in for a production's.
  /// `'inp` is named and threaded from `src`, not elided: a closure's parameter type is spelled
  /// out explicitly (the private `TestInput` alias above), and an elided lifetime there is free to
  /// be inferred shorter than the source's, which the borrow checker then refuses. Nothing else in
  /// the crate calls this; it exists for `tests/lossless_x_runner.rs`'s validator-discrimination
  /// test, which always passes `""` — the node this probes wraps zero tokens either way.
  ///
  /// # Panics
  ///
  /// Whatever the sink's own [`cst_start_at`] panics on. Under the shipped [`profile`], any
  /// `kind` at or past `K::ALL.len()` (`K` is [`crate::graphqlx::kinds::SyntaxKind`]) — the
  /// reserved tombstone (`u16::MAX`) panics too, but is refused by every validator, including
  /// [`KindValidator::accept_all`](tokora::cst::KindValidator::accept_all), so it would not
  /// discriminate the real validator from a permissive one.
  ///
  /// [`cst_mark`]: tokora::InputRef::cst_mark
  /// [`cst_start_at`]: tokora::InputRef::cst_start_at
  /// [`cst_finish`]: tokora::InputRef::cst_finish
  pub fn open_raw_kind<'inp>(src: &'inp str, kind: u16) -> Parse {
    let (cst, _out) = parse_lossless::<GraphqlxLosslessLexer<'inp, str>, GraphQLx, _, _, _, _>(
      src,
      Default::default(),
      LosslessEmitter::default(),
      profile::<str>(),
      DefaultCache::<GraphqlxLosslessLexer<'_, str>>::default(),
      |inp: &mut TestInput<'inp, '_>| {
        let mark = inp.cst_mark();
        inp.cst_start_at(mark, kind);
        inp.cst_finish(kind);
        inp.skip_while(|_| true)
      },
    );

    finish_root(cst)
  }

  /// Wraps a node over a nonempty `src` and commits **no token**, so materialization fails.
  ///
  /// The one shape no production can produce: every production that opens a node either commits
  /// what it matched or reports what it did not, and the `lossless_production!` bundle gives it
  /// no other door. This probe opens and closes a node and drains nothing, which is what makes
  /// [`crate::lossless::runner::finish_root`]'s `FinishError` arm reachable at all — and that
  /// arm's panic message is the only place the *dialect's* name appears in a materialization
  /// failure, so without a caller that reaches it, the `space` argument is threaded on trust.
  ///
  /// **The severed token channel, not an unclosed node.** The obvious probe — spend
  /// [`cst_start_at`] without its [`cst_finish`] — does not reach the arm, because the door is
  /// [`Cst::finish_partial`](tokora::cst::Cst::finish_partial): that door *closes* an open node
  /// rather than refusing it, since an unbalanced stream is one of the two shapes ordinary input
  /// can force (smear issue #57). A **balanced** stream that builds structure over a nonempty
  /// source without one committed token is corruption instead —
  /// `FinishError::StructureWithoutTokens`, tokora's token-channel wall — and both doors refuse
  /// it. `src` must be nonempty: the wall is stated over a source there was something to commit
  /// from.
  ///
  /// The orphan-finish shape is *not* the substitute either: `cst_finish` with nothing open
  /// panics at the emit door, so it never reaches materialization at all.
  ///
  /// # Panics
  ///
  /// Always, with the message `finish_root` composes.
  ///
  /// [`cst_start_at`]: tokora::InputRef::cst_start_at
  /// [`cst_finish`]: tokora::InputRef::cst_finish
  pub fn structure_without_tokens<'inp>(src: &'inp str, kind: u16) -> Parse {
    let (cst, _out) = parse_lossless::<GraphqlxLosslessLexer<'inp, str>, GraphQLx, _, _, _, _>(
      src,
      Default::default(),
      LosslessEmitter::default(),
      profile::<str>(),
      DefaultCache::<GraphqlxLosslessLexer<'_, str>>::default(),
      |inp: &mut TestInput<'inp, '_>| {
        let mark = inp.cst_mark();
        inp.cst_start_at(mark, kind);
        inp.cst_finish(kind);
        Ok(())
      },
    );

    finish_root(cst)
  }
}
