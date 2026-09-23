//! The Sink runner: binds a source to a `cst::Sink`, drives the document production, and
//! materializes a `rowan` tree.

use smear_lexer::limits::LosslessLimits;
use tokora::{
  Source,
  cst::{CstProfile, KindValidator},
};
// ── THE `test-support` HALF OF THIS FILE'S IMPORTS ───────────────────────────────────────────
//
// Four names, and every one of them is reached only under `test-support`: `Sink` and
// `GraphqlxLosslessLexer` through `LosslessSink`, which the `test_support` probes at the bottom
// and the `lossless_drivers!` drivers name; `Cst` and `GraphqlxLosslessLexer` again through
// `LosslessCst`, and `KindSpace` through `finish_root`, which only the probes reach. Round 8
// folded the shipped doors' finishing step into the door macro's own body, which took the last
// shipped user off all four, and the door's expansion spells its tokora paths absolutely — so
// with `test-support` off they are `unused_imports`, which `-Dwarnings` makes a build failure.
// Four CI jobs on the trunk are what found it, and gating them beside the items they serve is the
// same repair as those items got.
#[cfg(feature = "test-support")]
use tokora::cst::{Cst, Sink};

#[cfg(feature = "test-support")]
use super::GraphqlxLosslessLexer;
use super::{GraphqlxLosslessErrors, GraphqlxLosslessSlice, GraphqlxLosslessToken};
use crate::graphqlx::{error::ErrorData, kinds::SyntaxKind as K};
#[cfg(feature = "test-support")]
use crate::lossless::KindSpace;
use crate::lossless::{LosslessSource, Refused};

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

/// The recording emitter every lossless parse pins — the door's, the drivers' and the probes'.
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

/// The `Sink` type every lossless parse records into, under the name its `test-support` users
/// spell.
///
/// Named as the emitter half of a **context pair** by exactly two users: the `lossless_drivers!`
/// drivers' `TestCtx` and the `test_support` probes' `TestCtx` below. `Sink::new` is
/// tokora-private, so the one way to mint one is a tokora driver that takes the source once and
/// uses that same argument for the sink and the input — `parse_lossless` for the probes, and
/// `parse_lossless_with_context` inside `parse_lossless_document` for the drivers.
///
/// Which is why it is gated with those two: the door macro spells the same type out in its own
/// `DoorCtx`, so the shipped doors name neither this alias nor [`LosslessCst`], and with
/// `test-support` off it has no reference at all.
#[cfg(feature = "test-support")]
pub(crate) type LosslessSink<'inp> =
  Sink<'inp, GraphqlxLosslessLexer<'inp, str>, LosslessEmitter<'inp>>;

/// The spent sink tokora's `parse_lossless` hands back to a `test_support` probe.
// WITH ITS ONE USER. `finish_root` below is the only thing that names this alias, and `finish_root`
// is only reachable from the `test_support` probes — the shipped doors and the `lossless_drivers!`
// drivers hand back what the door macro builds and never see a `Cst` at all.
#[cfg(feature = "test-support")]
pub(crate) type LosslessCst<'inp> =
  Cst<'inp, GraphqlxLosslessLexer<'inp, str>, LosslessEmitter<'inp>>;

// THE DIALECT'S DOOR, generated here rather than written here — smear issue #193, round 7. The
// macro text is dialect-generic and lives in the substrate; the expansion fixes every type to this
// dialect's own and keeps its `report_token_budget` private to this module, which is what makes
// the report unreachable from anywhere a second parse could call it. Invoking it twice for this
// dialect is `E0119` on the `DoorOwner` impl it carries.
crate::lossless::lossless_door! {
  dialect  = graphqlx::lossless;
  errors   = GraphqlxLosslessErrors;
  document = document::document;
  schema   = document::type_system_document;
  request  = executable::executable_document;
}

/// Materialize a probe's `cst` at the root kind and collect its diagnostics.
///
/// **The `test_support` probes' finish, and theirs alone.** The shipped doors and the
/// `lossless_drivers!` drivers do not come through here: they materialise inside the door macro's
/// own body, through
/// [`crate::lossless::runner::finish_parsed_root_with`], which is where what a finished parse says
/// about a refusal is decided. This wrapper exists so the probes name the root kind once — the
/// *tree's* root (`K::Root`) rather than any production's. Below it is
/// `crate::lossless::runner::finish_parsed_root`, which calls
/// [`crate::lossless::runner::finish_root`] — the partial-materialization door
/// (`Cst::finish_partial`, smear issue #57) and the diagnostic projection — and turns its refusal
/// into a panic, so this wrapper returns a bare [`Parse`].
///
/// A caller finishing a `Cst` it built itself goes through the public
/// [`crate::lossless::runner::finish_root`] and gets the refusal as a value.
// GATED ON HAVING A CALLER: the `test_support` probes below are the only ones.
#[cfg(feature = "test-support")]
pub(crate) fn finish_root(cst: LosslessCst<'_>) -> Parse {
  crate::lossless::runner::finish_parsed_root(cst, K::Root.raw(), <K as KindSpace>::NAME)
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

/// Parse a source as a GraphQLx document, losslessly.
///
/// The production is `document.rs`'s `document` under the door's drain — the mixed
/// root, which admits imports, executable definitions, type-system definitions and extensions in
/// any order, followed by a drain. The door discards the production's result, so an `Err` escaping
/// it leaves the rest of the source with no committed token. Materialisation is
/// `Cst::finish_partial`, which would tile that tail as one `gap_kind` token carrying its original
/// text: the tree would still cover every byte, and any lexer error a lookahead already raised over
/// the tail stays recorded, but nothing past that lookahead is lexed, so the parse would
/// under-report. The drain lexes the tail and commits its tokens — unless a refusal ended the
/// document, when it deliberately reads nothing — so the tail's remaining lexer errors are
/// reported and its tokens are charged to the budget.
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
///
/// # Holding something that is not a `&str`
///
/// [`parse_document_from`] is this door over any [`LosslessSource`] — `&[u8]`, `bytes::Bytes`,
/// `bstr::BStr`, `HipStr`, `HipByt`, the smol-bytes forms — and answers byte-identically to this
/// one over the same bytes. It is a sibling rather than this signature because a generic parameter
/// cannot be inferred where a `&str` could be coerced; that door's own note says why.
///
/// It is fallible and this one is not, because a green tree requires two things of a source —
/// valid UTF-8, and a length it can address — and `&str` proves only the first. **This signature
/// therefore still has a precondition**: see below.
///
/// # One precondition, and the sibling is the way around it
///
/// A source longer than [`Refused::MAX_SOURCE_LEN`](crate::lossless::Refused::MAX_SOURCE_LEN)
/// panics at materialisation — `rowan` addresses text with `u32` — and this signature returns a
/// [`Parse`], so it has nowhere to report that. It applies to every concrete `&str` door; a caller
/// that cannot bound its input uses [`parse_document_from`], which classifies the length before it
/// scans and answers `Err`.
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
/// # One precondition, and the sibling is the way around it
///
/// A source longer than [`Refused::MAX_SOURCE_LEN`](crate::lossless::Refused::MAX_SOURCE_LEN)
/// panics at materialisation — `rowan` addresses text with `u32` — and this signature returns a
/// [`Parse`], so it has nowhere to report that. It applies to every concrete `&str` door; a caller
/// that cannot bound its input uses [`parse_document_from`], which classifies the length before it
/// scans and answers `Err`.
pub fn parse_document_with_limits(src: &str, limits: LosslessLimits) -> Parse {
  // The `&str` half of the root pair. `str` discharges the UTF-8 requirement by type and not the
  // length one, which is why this signature is not a `Result` and why an over-length source still
  // panics at materialisation — the precondition documented above.
  document_root(src, limits)
}

/// [`parse_document`] over any source [`LosslessSource`] admits.
///
/// `&str`, `&[u8]`, and every backing this crate ships an integration for: `bytes::Bytes`,
/// `bstr::BStr`, `HipStr`, `HipByt` and the smol-bytes forms, plus `String`, `Vec<u8>`, a `Cow`, a
/// `Box`, an `Rc`, an `Arc` and a reference to any of them.
///
/// # Why this is a sibling and not the door itself
///
/// Because a generic parameter cannot be inferred where `&str` could be coerced. `String`
/// implements `AsRef<str>` **and** `AsRef<[u8]>`, and both targets are a [`LosslessSource`], so
/// `parse_document(s.as_ref())` — which compiles against the `&str` door — is `E0283` against a
/// generic one. So the concrete door keeps its signature and its callers, and the wide capability
/// sits beside it: nothing to choose unless you hold something that is not a `&str`.
///
/// # The parse is the same parse whichever you hand over
///
/// A source is borrowed as text or as bytes, a byte view is validated once, and one scanner runs
/// over the text either way — so this and [`parse_document`] return byte-identical trees,
/// diagnostics and verdicts over the same bytes, non-ASCII outside strings and comments included.
/// [`LosslessSource`] carries why there is only one scanner.
///
/// A green tree requires **two** things of a source and this door asks both once, before anything
/// is scanned: valid UTF-8, and no longer than [`Refused::MAX_SOURCE_LEN`] because `rowan`
/// addresses text with `u32`. They are independent — a 5 GiB `&str` is good text and still cannot
/// be materialised. Either failure is `Err(`[`Refused`]`)` naming which, with the numbers, and no
/// [`Parse`] for a consumer to reprint or to pair with a source.
///
/// # Both halves, compiled
///
/// The same bytes through this door and through [`parse_document`] are the same parse. `Parse` is
/// not `PartialEq` — it holds a green tree — so the comparison is over everything it publishes.
///
/// ```
/// # #[cfg(all(feature = "graphqlx", feature = "rowan"))] {
/// use smear_parser::graphqlx::lossless::{parse_document, parse_document_from};
///
/// let src = "type T { f: Int } query Q { f }";
///
/// let narrow = parse_document(src);
/// let wide = parse_document_from(src.as_bytes())
///   .expect("valid UTF-8, and short enough for a green tree to address");
///
/// assert_eq!(wide.green(), narrow.green());
/// assert_eq!(wide.diagnostics(), narrow.diagnostics());
/// assert_eq!(wide.has_errors(), narrow.has_errors());
/// # }
/// ```
///
/// And bytes that are not UTF-8 are an `Err` carrying both numbers — no `Parse`, so there is no
/// tree to reprint and nothing to hand a door that pairs a parse with a source.
///
/// ```
/// # #[cfg(all(feature = "graphqlx", feature = "rowan"))] {
/// use smear_parser::{graphqlx::lossless::parse_document_from, lossless::Refused};
///
/// // Nineteen bytes; byte 13 is not a UTF-8 lead.
/// let bad: &[u8] = b"query Q { f }\xFF\xFEmore";
///
/// match parse_document_from(bad) {
///   Ok(_) => panic!("a source that is not UTF-8 must not produce a parse"),
///   Err(Refused::NonUtf8Source { valid_up_to, source_len }) => {
///     assert_eq!(valid_up_to, 13);
///     assert_eq!(source_len, 19);
///   }
///   Err(other) => panic!("unexpected refusal: {other}"),
/// }
/// # }
/// ```
pub fn parse_document_from<Src: LosslessSource + ?Sized>(src: &Src) -> Result<Parse, Refused> {
  parse_document_from_with_limits(src, LosslessLimits::default())
}

/// [`parse_document_from`] under a caller-chosen resource budget.
///
/// See [`parse_document_with_limits`] for when to reach for one.
pub fn parse_document_from_with_limits<Src: LosslessSource + ?Sized>(
  src: &Src,
  limits: LosslessLimits,
) -> Result<Parse, Refused> {
  // ONE CALL, AND IT IS THE WHOLE PARSE — and the only place in this root's four doors where a
  // source can be refused. The fallible half of the root pair takes the source's text, validating
  // it once if the source is bytes, and hands it to the `&str` half.
  document_root_from(src, limits)
}

/// Parse a source as a GraphQLx **type-system** (SDL-only) document, losslessly.
///
/// [`parse_document`]'s root without the executable half:
/// `ImportOrTypeSystemDefinitionOrExtension+`, the tree
/// [`ast::TypeSystemDocument`](super::ast::TypeSystemDocument) wraps. Imports stay in — a
/// GraphQLx schema is the thing that imports — but an `operation`, a shorthand `{ … }` or a
/// `fragment` is **reported here**, with the span the parser was standing on, rather than
/// accepted into a mixed tree for the caller to find and reject with a position it has to
/// reconstruct.
///
/// Everything else is [`parse_document`]'s contract unchanged: the same lexer, the same profile,
/// the same [`Parse`], the same recovery. Only the root differs.
///
/// ```
/// # use smear_parser::graphqlx::lossless::{parse_document, parse_type_system_document};
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
  // The `&str` half of the root pair. `str` discharges the UTF-8 requirement by type and not the
  // length one, which is why this signature is not a `Result` and why an over-length source still
  // panics at materialisation — the precondition documented above.
  type_system_document_root(src, limits)
}

/// [`parse_type_system_document`] over any source [`LosslessSource`] admits.
///
/// See [`parse_document_from`] for why the wide form is a sibling rather than the door itself, and
/// what it accepts.
pub fn parse_type_system_document_from<Src: LosslessSource + ?Sized>(
  src: &Src,
) -> Result<Parse, Refused> {
  parse_type_system_document_from_with_limits(src, LosslessLimits::default())
}

/// [`parse_type_system_document_from`] under a caller-chosen resource budget.
///
/// See [`parse_document_with_limits`] for when to reach for one.
pub fn parse_type_system_document_from_with_limits<Src: LosslessSource + ?Sized>(
  src: &Src,
  limits: LosslessLimits,
) -> Result<Parse, Refused> {
  // ONE CALL, AND IT IS THE WHOLE PARSE — and the only place in this root's four doors where a
  // source can be refused. The fallible half of the root pair takes the source's text, validating
  // it once if the source is bytes, and hands it to the `&str` half.
  type_system_document_root_from(src, limits)
}

/// Parse a source as a GraphQLx **executable** document, losslessly.
///
/// [`parse_type_system_document`]'s mirror: `ImportOrExecutableDefinition+`, the tree
/// [`ast::ExecutableDocument`](super::ast::ExecutableDocument) wraps. Imports stay in here too;
/// every type-system definition and every `extend` is reported, at the parser's own position.
///
/// ```
/// # use smear_parser::graphqlx::lossless::{parse_document, parse_executable_document};
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
  // The `&str` half of the root pair. `str` discharges the UTF-8 requirement by type and not the
  // length one, which is why this signature is not a `Result` and why an over-length source still
  // panics at materialisation — the precondition documented above.
  executable_document_root(src, limits)
}

/// [`parse_executable_document`] over any source [`LosslessSource`] admits.
///
/// See [`parse_document_from`] for why the wide form is a sibling rather than the door itself, and
/// what it accepts.
pub fn parse_executable_document_from<Src: LosslessSource + ?Sized>(
  src: &Src,
) -> Result<Parse, Refused> {
  parse_executable_document_from_with_limits(src, LosslessLimits::default())
}

/// [`parse_executable_document_from`] under a caller-chosen resource budget.
///
/// See [`parse_document_with_limits`] for when to reach for one.
pub fn parse_executable_document_from_with_limits<Src: LosslessSource + ?Sized>(
  src: &Src,
  limits: LosslessLimits,
) -> Result<Parse, Refused> {
  // ONE CALL, AND IT IS THE WHOLE PARSE — and the only place in this root's four doors where a
  // source can be refused. The fallible half of the root pair takes the source's text, validating
  // it once if the source is bytes, and hands it to the `&str` half.
  executable_document_root_from(src, limits)
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
///
/// Behind `feature = "test-support"`, and hidden even then: both entry points exist to build a
/// tree the grammar cannot produce, and one of them panics by design. `pub` is forced only
/// because `tests/lossless_x_runner.rs` is a separate crate.
#[cfg(feature = "test-support")]
#[doc(hidden)]
pub mod test_support {
  use tokora::{InputRef, cache::DefaultCache};

  use tokora::cst::parse_lossless;

  use super::{GraphqlxLosslessLexer, LosslessEmitter, LosslessSink, Parse, finish_root, profile};
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
  /// panics at the emit door in a debug build — tokora checks it with a `debug_assert!` — and
  /// reaches materialization, as `FinishError::OrphanFinish`, only in a release build, so a probe
  /// built on it would test a different thing in each profile.
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
