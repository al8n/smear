//! The Sink runner: binds a source to a `cst::Sink`, drives the document production, and
//! materializes a `rowan` tree.

use smear_lexer::limits::LosslessLimits;
use tokora::{
  Source,
  cst::{CstProfile, KindValidator},
};
// ── THE `test-support` HALF OF THIS FILE'S IMPORTS ───────────────────────────────────────────
//
// Four names, and every one of them is reached only from the `test_support` drivers at the bottom:
// `Sink` and `GraphqlLosslessLexer` through `LosslessSink`, `Cst` and `GraphqlLosslessLexer` again
// through `LosslessCst`, and `KindSpace` through `finish_root`. Round 8 folded the shipped doors'
// finishing step into the door macro's own body, which took the last non-driver caller off all
// four, and the door's expansion spells its tokora paths absolutely — so with `test-support` off
// they are `unused_imports`, which `-Dwarnings` makes a build failure. Four CI jobs on the trunk
// are what found it, and gating them beside the items they serve is the same repair as those
// items got.
#[cfg(feature = "test-support")]
use tokora::cst::{Cst, Sink};

#[cfg(feature = "test-support")]
use super::GraphqlLosslessLexer;
use super::{GraphqlLosslessErrors, GraphqlLosslessSlice, GraphqlLosslessToken};
use crate::graphql::{error::ErrorData, kinds::SyntaxKind as K};
#[cfg(feature = "test-support")]
use crate::lossless::KindSpace;

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
///
/// Named only as the emitter half of a driver's **context pair** — `Sink::new` is tokora-private,
/// so the one way to mint one is [`parse_lossless`], which takes the source once and uses that
/// same argument for the sink and the input.
///
/// Which is why it is gated with the drivers: the shipped [`parse_document`] path names
/// [`LosslessCst`] and never this, so with `test-support` off it has no reference at all.
#[cfg(feature = "test-support")]
pub(crate) type LosslessSink<'inp> =
  Sink<'inp, GraphqlLosslessLexer<'inp, str>, LosslessEmitter<'inp>>;

/// The spent sink [`parse_lossless`] hands back — the one door to materialization.
// WITH ITS ONE USER. `finish_root` below is the only thing that names this alias, and `finish_root`
// is only reachable from the `test_support` drivers — the six shipped doors return the `Parse` the
// door macro builds and never see a `Cst` at all.
#[cfg(feature = "test-support")]
pub(crate) type LosslessCst<'inp> =
  Cst<'inp, GraphqlLosslessLexer<'inp, str>, LosslessEmitter<'inp>>;

// THE DIALECT'S DOOR, generated here rather than written here — smear issue #193, round 7. The
// macro text is dialect-generic and lives in the substrate; the expansion fixes every type to this
// dialect's own and keeps its `report_token_budget` private to this module, which is what makes
// the report unreachable from anywhere a second parse could call it. Invoking it twice for this
// dialect is `E0119` on the `DoorOwner` impl it carries.
crate::lossless::lossless_door! {
  dialect = graphql::lossless;
  errors  = GraphqlLosslessErrors;
}

/// Materialize `cst` at the root kind and collect its diagnostics.
///
/// Shared by the three document-root entry points — [`parse_document`],
/// [`parse_type_system_document`] and [`parse_executable_document`] — and by the per-production
/// drivers under `test_support`, so the root kind is named once. Note that the root kind is the
/// *tree's* root (`K::Root`) rather than the production's, which is why one wrapper covers three
/// different document roots. Everything below that — the **partial** materialization door
/// (`Cst::finish_partial`, smear issue #57 — see [`crate::lossless::runner::finish_root`]'s
/// `Why the partial door` note), the fallible-materialization contract and the diagnostic
/// projection — is [`crate::lossless::runner::finish_root`]'s; this wrapper's whole content is
/// *which* root kind and *which* dialect the refusal names.
///
/// The refusal itself is `crate::lossless::runner::finish_parsed_root`'s: these doors build the
/// `Cst` they finish, under their own clamped recursion budget, so the substrate's refusal is
/// unreachable from here and that function carries the numbers that say why. A caller finishing a
/// `Cst` it built itself goes through the public [`crate::lossless::runner::finish_root`] and gets
/// the refusal as a value.
// THE DRIVERS' FINISH, and theirs alone. Round 8 folded the shipped doors' finishing step into
// the door macro's own body, which left this wrapper with exactly one caller family: the
// `test_support` drivers below.
#[cfg(feature = "test-support")]
pub(crate) fn finish_root(cst: LosslessCst<'_>) -> Parse {
  crate::lossless::runner::finish_parsed_root(cst, K::Root.raw(), <K as KindSpace>::NAME)
}

/// One diagnostic a GraphQL lossless parse recorded.
///
/// Nothing in it is per-dialect — it is owned, source-independent and lifetime-free by design —
/// so it is the substrate's type re-exported rather than a second copy of it.
pub use crate::lossless::runner::Diagnostic;

/// The result of a GraphQL lossless parse.
///
/// A **type alias**, not a newtype. A newtype would need `syntax`, `diagnostics` and
/// `has_errors` re-written per dialect, which is the duplication the lift exists to remove; an
/// alias keeps `parse_document(&str) -> Parse` reading exactly as it did at every call site.
pub type Parse = crate::lossless::runner::Parse<crate::graphql::kinds::GraphQLLang>;

/// Parse a `&str` as a GraphQL document, losslessly.
///
/// This is the **mixed** root: executable definitions, type-system definitions and type-system
/// extensions in any order, which is the `Document` of the specification's grammar and the tree
/// [`ast::Document`](super::ast::Document) wraps.
///
/// A consumer that will only accept one half of the language has a root of its own rather than a
/// filter to write afterwards — see [`parse_type_system_document`] and
/// [`parse_executable_document`]. The difference is not cosmetic: those roots reject the other
/// half *at the parser's own position*, which a caller walking a mixed tree cannot reconstruct.
///
/// # The nesting ceiling
///
/// [`LosslessLimits::default`], so at most
/// [`MAX_NESTING_DEPTH`](smear_lexer::limits::MAX_NESTING_DEPTH) simultaneously open brackets;
/// the next one is reported. That default is derived against a 2 MiB stack, which is what
/// `std::thread::spawn`, a tokio worker and the libtest harness each give a thread. A caller on a
/// different stack, or with deeper documents, uses [`parse_document_with_limits`].
pub fn parse_document(src: &str) -> Parse {
  parse_document_with_limits(src, LosslessLimits::default())
}

/// [`parse_document`] under a caller-chosen resource budget.
///
/// The reason to reach for this is a stack that is not the 2 MiB
/// [`MAX_NESTING_DEPTH`](smear_lexer::limits::MAX_NESTING_DEPTH) is derived against — a server on
/// an 8 MiB main thread can afford roughly four times the depth, and a worker deliberately spawned
/// smaller can afford less. The cost of one level is measured on
/// [`MAX_NESTING_DEPTH`](smear_lexer::limits::MAX_NESTING_DEPTH) itself.
///
/// # The ceiling is clamped, and the clamp is not negotiable
///
/// What this installs as the parse's recursion budget is `min(limits.max_nesting_depth(),
/// `[`HARD_MAX`](smear_lexer::limits::HARD_MAX)`)`. A request above that maximum is answered with a
/// positioned diagnostic at the maximum rather than with the depth that was asked for, because
/// this function builds the parse's context and therefore owns whether it returns or aborts — and
/// smear cannot see the stack its caller is on. The **lexer's** own tally still reads the
/// unclamped number; see [`HARD_MAX`](smear_lexer::limits::HARD_MAX) for why only one of the two
/// has a native-stack cost behind it.
///
/// # A caller supplies nothing that carries diagnostics — smear issue #193, Codex round 5
///
/// A source and a budget. **No emitter**, and that is a guarantee rather than an omission: the
/// substrate door this runs
/// (`parse_lossless_document`, generated into this module) builds tokora's `Verbose` itself, so
/// there
/// is no argument anywhere on this path that a caller could use to hand two parses one diagnostic
/// timeline. The round that removed it measured the alternative at **2** budget reports in an
/// enclosing parse.
///
/// ```compile_fail
/// // E0061. Restoring an emitter parameter anywhere on this path reddens this by COMPILING.
/// use smear_lexer::limits::LosslessLimits;
/// let mut collector = std::vec::Vec::<u8>::new();
/// let _ = smear_parser::graphql::lossless::parse_document_with_limits(
///   "type T { f: Int }",
///   LosslessLimits::default(),
///   &mut collector,
/// );
/// ```
///
/// ```
/// // THE POSITIVE CONTROL. A `compile_fail` block passes for any compile error — a bad path or a
/// // misspelled crate included — so the same call, correct, is what says the one above fails on
/// // the ARITY.
/// use smear_lexer::limits::LosslessLimits;
/// use tokora::Parse as _;
/// let parse = smear_parser::graphql::lossless::parse_document_with_limits(
///   "type T { f: Int }",
///   LosslessLimits::default(),
/// );
/// assert!(!parse.has_errors());
/// ```
///
/// The **in-crate** half of the same claim is not a doctest and cannot be: a doctest compiles as a
/// separate crate, so it can say nothing about a `pub(crate)` item's signature. What enforces it
/// there is rustc at the seven call sites — six doors and the driver macro — every one of which
/// stops compiling if the parameter comes back, and `ci/source_census`, which derives that caller
/// set from the tokens.
pub fn parse_document_with_limits(src: &str, limits: LosslessLimits) -> Parse {
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
  // genuinely ambiguous. `str` is the one that matches `parse_document`'s `L::Source = str`.
  //
  // The production goes to the door RAW: the door is what drains what an escape left behind, so
  // the `_entry` wrapper that used to do it here is gone. Its reason is unchanged and now lives on
  // the door — an `Err` escaping the document production leaves the rest of the source uncommitted
  // and `finish` refuses it as an `UncoveredGap`.
  // ONE CALL, AND IT IS THE WHOLE PARSE. `parse_lossless_document` is generated into this module
  // by `lossless_door!`: it builds the context, installs both budgets off `limits` itself, runs the
  // driver over this root, drains what an escape left behind and reports a budget refusal if there
  // was one. Every type it runs over is its own choice — smear issue #193, Codex rounds 4 to 6 —
  // and the production is the one thing this door still names.
  parse_lossless_document(src, limits, super::document::document::<str, _>)
}

/// Parse a `&str` as a GraphQL **type-system** (SDL-only) document, losslessly.
///
/// [`parse_document`]'s root without the executable half: `TypeSystemDefinitionOrExtension+`, the
/// tree [`ast::TypeSystemDocument`](super::ast::TypeSystemDocument) wraps. An `operation`, a
/// shorthand `{ … }` or a `fragment` is **reported here**, with the span the parser was standing
/// on, rather than accepted into a mixed tree for the caller to find and reject with a position it
/// has to reconstruct.
///
/// Everything else is [`parse_document`]'s contract unchanged: the same lexer, the same profile,
/// the same [`Parse`], the same recovery. Only the root differs.
///
/// ```
/// # use smear_parser::graphql::lossless::{parse_document, parse_type_system_document};
/// # use tokora::Parse as _;
/// // The mixed root takes it; the SDL-only root reports it.
/// assert!(!parse_document("query Q { f }").has_errors());
/// assert!(parse_type_system_document("query Q { f }").has_errors());
/// assert!(!parse_type_system_document("type T { f: Int }").has_errors());
/// ```
pub fn parse_type_system_document(src: &str) -> Parse {
  parse_type_system_document_with_limits(src, LosslessLimits::default())
}

/// [`parse_type_system_document`] under a caller-chosen resource budget.
///
/// See [`parse_document_with_limits`] for when to reach for one.
pub fn parse_type_system_document_with_limits(src: &str, limits: LosslessLimits) -> Parse {
  // The turbofishes and the `_entry` suffix are `parse_document`'s, for `parse_document`'s
  // reasons; see the comment there rather than a second copy of it here.
  // ONE CALL, AND IT IS THE WHOLE PARSE. `parse_lossless_document` is generated into this module
  // by `lossless_door!`: it builds the context, installs both budgets off `limits` itself, runs the
  // driver over this root, drains what an escape left behind and reports a budget refusal if there
  // was one. Every type it runs over is its own choice — smear issue #193, Codex rounds 4 to 6 —
  // and the production is the one thing this door still names.
  parse_lossless_document(src, limits, super::document::type_system_document::<str, _>)
}

/// Parse a `&str` as a GraphQL **executable** document, losslessly.
///
/// [`parse_type_system_document`]'s mirror: `ExecutableDefinition+`, the tree
/// [`ast::ExecutableDocument`](super::ast::ExecutableDocument) wraps. Every type-system
/// definition and every `extend` is reported, at the parser's own position.
///
/// ```
/// # use smear_parser::graphql::lossless::{parse_document, parse_executable_document};
/// # use tokora::Parse as _;
/// // The mixed root takes it; the executable-only root reports it.
/// assert!(!parse_document("type T { f: Int }").has_errors());
/// assert!(parse_executable_document("type T { f: Int }").has_errors());
/// assert!(!parse_executable_document("query Q { f }").has_errors());
/// ```
pub fn parse_executable_document(src: &str) -> Parse {
  parse_executable_document_with_limits(src, LosslessLimits::default())
}

/// [`parse_executable_document`] under a caller-chosen resource budget.
///
/// See [`parse_document_with_limits`] for when to reach for one.
pub fn parse_executable_document_with_limits(src: &str, limits: LosslessLimits) -> Parse {
  // ONE CALL, AND IT IS THE WHOLE PARSE. `parse_lossless_document` is generated into this module
  // by `lossless_door!`: it builds the context, installs both budgets off `limits` itself, runs the
  // driver over this root, drains what an escape left behind and reports a budget refusal if there
  // was one. Every type it runs over is its own choice — smear issue #193, Codex rounds 4 to 6 —
  // and the production is the one thing this door still names.
  parse_lossless_document(
    src,
    limits,
    super::executable::executable_document::<str, _>,
  )
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
///
/// Behind `feature = "test-support"`, and hidden even then: both entry points exist to build a
/// tree the grammar cannot produce, and one of them panics by design. `pub` is forced only
/// because `tests/lossless_runner.rs` is a separate crate.
#[cfg(feature = "test-support")]
#[doc(hidden)]
pub mod test_support {
  use tokora::{InputRef, cache::DefaultCache};

  use tokora::cst::parse_lossless;

  use super::{GraphqlLosslessLexer, LosslessEmitter, LosslessSink, Parse, finish_root, profile};
  use crate::graphql::GraphQL;

  type TestCtx<'inp> = (
    LosslessSink<'inp>,
    DefaultCache<'inp, GraphqlLosslessLexer<'inp, str>>,
  );
  type TestInput<'inp, 'input> =
    InputRef<'inp, 'input, GraphqlLosslessLexer<'inp, str>, TestCtx<'inp>, GraphQL>;

  /// Opens a node at `kind` over `src` and materializes.
  ///
  /// The body is the exact retro-wrap sequence every `node`/`node_at` production spends to
  /// open its own node — [`cst_mark`] then [`cst_start_at`] then [`cst_finish`]
  /// (`tokora/src/parser/node.rs`'s own `wrap`) — with `kind` standing in for a production's.
  /// `'inp` is named and threaded from `src`, not elided, for the reason `trivia.rs`'s driver
  /// and `lossless_drivers!` both record: a closure's parameter type is spelled out explicitly
  /// (the private `TestInput` alias above), and an elided lifetime there is free to be inferred
  /// shorter than the source's, which the borrow checker then refuses. Nothing else in the crate
  /// calls this; it exists for `tests/lossless_runner.rs`'s validator-discrimination test, which
  /// always passes `""` — the node this probes wraps zero tokens either way.
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
  /// [`cst_mark`]: tokora::InputRef::cst_mark
  /// [`cst_start_at`]: tokora::InputRef::cst_start_at
  /// [`cst_finish`]: tokora::InputRef::cst_finish
  pub fn open_raw_kind<'inp>(src: &'inp str, kind: u16) -> Parse {
    let (cst, _out) = parse_lossless::<GraphqlLosslessLexer<'inp, str>, GraphQL, _, _, _, _>(
      src,
      Default::default(),
      LosslessEmitter::default(),
      profile::<str>(),
      DefaultCache::<GraphqlLosslessLexer<'_, str>>::default(),
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
  /// Always, with the message `crate::lossless::runner::finish_parsed_root` composes around the
  /// substrate's own refusal — which is why this probe is what proves the `space` argument is
  /// threaded rather than assumed.
  ///
  /// [`cst_start_at`]: tokora::InputRef::cst_start_at
  /// [`cst_finish`]: tokora::InputRef::cst_finish
  pub fn structure_without_tokens<'inp>(src: &'inp str, kind: u16) -> Parse {
    let (cst, _out) = parse_lossless::<GraphqlLosslessLexer<'inp, str>, GraphQL, _, _, _, _>(
      src,
      Default::default(),
      LosslessEmitter::default(),
      profile::<str>(),
      DefaultCache::<GraphqlLosslessLexer<'_, str>>::default(),
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
