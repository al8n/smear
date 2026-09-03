//! The three declaration macros a lossless assembly is built out of.
//!
//! # Why a macro and not a generic item, in all three cases
//!
//! Each of these would be a function, a trait or an `impl` if it could be. None of them can:
//!
//! - `lossless_production!` declares a **signature**, not a value — six where-predicates carrying
//!   fifteen trait bounds, spelled over seven of the eight names a dialect owes it. A generic
//!   function cannot stand in for a signature its caller writes bodies against.
//! - `lossless_drivers!` declares a module of `fn(&str) -> Parse` entry points whose closure
//!   parameter types must be **spelled out**: a closure's parameter is not inferred through a
//!   `ParseInput` bound, only through an `Fn` bound.
//! - `lossless_error_impls!` declares four `From`/`FromUnclosed` impls onto a dialect's own error
//!   container. **Coherence forbids one impl covering both dialects**, so the choice is a macro or
//!   two hand-written copies; the macro is the one where the *bodies* — which span is zeroed,
//!   which delimiter names map to which constructor, which end-of-input constructor — are written
//!   once.
//!
//! # How a dialect is named, and why it is not a `:path`
//!
//! The two production macros take a `dialect = graphql::lossless;` header — **two `ident`s**,
//! rooted at `$crate` in the expansion — and the error macro takes seven plain `ident`s naming
//! items already in scope where it is invoked.
//!
//! A single `$dialect:path` fragment reads better and **does not compile**. Once a path is
//! captured as one fragment it is an opaque AST node, and in type position `$dialect::Input<…>`
//! parses as an *associated item* of the type `$dialect`, which rustc rejects with "missing angle
//! brackets in associated item path"; appending generic arguments directly (`$errors<S>`) is a
//! hard parse error. Plain `ident`s have neither problem, because each is still a token.
//!
//! Nor is it a **run** of idents, `$($d:ident)::+`. That parses, but the expansion has to sit
//! inside the `$( … )*` that walks the productions, and rustc refuses to nest two repetitions of
//! different lengths: *"meta-variable `inp` repeats 25 times, but `d` repeats 2 times"*. Two fixed
//! idents have no length to disagree about. The cost is that a dialect must live at exactly
//! `crate::<a>::<b>`, which both do and which the file layout already fixes.
//!
//! # Why not a macro that defines a macro
//!
//! The natural-looking design — a root `declare_lossless_production!` that generates a per-dialect
//! `lossless_production!` with the header baked in — needs `$$` (macro metavariable expressions),
//! which is unstable and therefore out of reach on the declared MSRV. The header-argument design
//! is the one that compiles today.
//!
//! # This file names no dialect
//!
//! Every concrete name arrives from the call site. That is the Lego rule, and gate 6 mechanises it.

/// Declares a lossless production: `fn(&mut Input, …) -> Result<(), Error>`, with the one
/// where-clause every production in a suite carries.
///
/// ```text
/// lossless_production! {
///   dialect = graphql::lossless;
///
///   /// `Variable` — `$name`.
///   fn variable<'inp, Src, Ctx>(inp) { … }
/// }
/// ```
///
/// # The eight names a dialect owes this macro
///
/// `Input`, `Error`, `Token`, `Lexer`, `LexerState`, `Brand`, `TokenKind` and `Keyword`, at
/// `crate::<dialect>::lossless`. They are aliases over whatever that dialect already calls those
/// things, so adopting the macro renames nothing.
///
/// `LexerState` is the eighth and the newest: the `State =` pin on the `Lexer` clause is what lets
/// a production read the resource budget the parse runs under — which is where the nesting
/// ceiling lives — through `inp.state()`. Without the equality the projection has nothing to
/// normalize against over a generic `Src`, exactly as `Kind =` does not for the token.
///
/// # Why the bundle is one block, and why it is here rather than per dialect
///
/// The bundle is **six where-predicates carrying fifteen trait bounds**, and not one of them is
/// optional once a production both opens a node and goes through the atom set — which is every
/// production in either suite. Written out per function it is ~150 lines of boilerplate across
/// `value.rs` alone, and the failure mode of drifting copies is a compile error a hundred lines
/// from its cause. Written out per *dialect* it is the same six predicates twice, and the failure
/// mode is worse: the two copies drift and the second dialect's productions accept a bound the
/// first rejects.
///
/// # The predicates, and what each bound buys
///
/// One bullet per predicate, in the order the expansion writes them, and every bound the
/// expansion spells appears under one of them. A bound in the macro body with no line here is the
/// drift this section exists to be read against — the count in the paragraph above is derived
/// from this list, so the two cannot disagree without one of them being visibly wrong.
///
/// - **`Src: Source<usize> + ?Sized`** — the source stays the caller's representation rather than
///   this crate's, and the `?Sized` relaxation is what lets `str` itself be the `Src` every
///   driver passes. Two entries, and only `Source<usize>` is a trait bound.
/// - **`Token<'inp, Src>: Token<'inp, Kind = TokenKind> + FromLogos<'inp> + Clone +
///   DowncastRef<Keyword>`** — four bounds.
///   - `Token<'inp, Kind = TokenKind>` — a dialect's lossless `Token` impl is macro-generated
///     **once per concrete slice type** (`smear-lexer`'s `token_impl!`), unlike the syntactic
///     token's generic impl, so over a generic `Src` the projection `<Token<…>>::Kind` has
///     nothing to normalize against. `Token<'inp>` alone makes the projection *nameable*; only
///     the `Kind =` equality makes a token-kind **literal** — which is what every
///     `expect(inp, Kind::LBracket)` passes — typecheck against it.
///   - `FromLogos<'inp>` — `LogosLexer<'inp, T>` carries it on its struct definition, so without
///     it the lexer alias is ill-formed rather than merely unbounded.
///   - `Clone` — `UnexpectedToken::with_found` takes the token by value, and a declined token is
///     only ever borrowed (it stays unconsumed, on purpose).
///   - `DowncastRef<Keyword>` — `true`/`false`/`null` and every other contextual keyword arrive
///     as identifier tokens; the value dispatcher tells them apart on their spelling.
/// - **`Lexer<'inp, Src>: Lexer<'inp, Token = …, Span = …, Offset = …, State = …>`** — one bound
///   and four associated-type pins. Without `Span`/`Offset` the two error `From` bounds cannot be
///   spelled at all; `Token =` is what ties the predicate above to *this* lexer; `State =` is the
///   pin described under the eight names.
/// - **`Ctx: ParseContext<'inp, Lexer, Brand>`** — the context the parse runs under. It is what
///   makes `Input<'inp, '_, Src, Ctx>`, the `inp` every body takes, a well-formed `InputRef` at
///   all, and what the next predicate projects `Emitter` out of.
/// - **`Ctx::Emitter: CstEmitter<'inp, Lexer, Brand>`** — the structural gate on the whole `node`
///   family.
/// - **`Error<'inp, Src, Ctx>: From<UnexpectedEot> + From<UnexpectedToken> + FromUnclosed +
///   From<RecursionLimitReached> + FromNestingLimit + FromTokenBudget + MaybeTerminal`** — seven
///   bounds. The first
///   three are the ordinary reports: `UnexpectedEot` for every peek, `UnexpectedToken` for every
///   declined `expect`, `FromUnclosed` for the unterminated-delimiter reports. The next two are
///   the parser-frame descent ([`crate::lossless::depth`]) —
///   [`InputRef::descend`](tokora::InputRef::descend) carries `From<RecursionLimitReached>` as its
///   own where-clause and it is the carrier a refusal *arrives* on, while `FromNestingLimit` is
///   how that refusal's **diagnostic** reaches the dialect's container. `FromTokenBudget` is the
///   same pairing at the other resource and it has only the second half: tokora refuses a
///   durable [`TokenBudget`](tokora::input::TokenBudget) **silently**, with no carrier at all, so
///   the diagnostic `drain_unless_stopped` mints through this trait is the only thing a refused
///   document reports (smear issue #193). The last, `MaybeTerminal`,
///   is what the drains above a root read to tell a resource refusal from a syntax error. All
///   seven
///   ride the whole bundle rather than only the productions that need them, because the bundle is
///   one block and a per-production subset is the drift this macro exists to prevent.
///
/// # The predicate that used to be here and is not
///
/// A seventh predicate, `<Lexer as Lexer<'inp>>::State: Clone`, rode along documented as
/// *"`InputRef::sync_balanced` lives in an impl block that requires it; nothing else in this suite
/// does"*. The first half is true (`tokora/src/input/input_ref/sync_balanced.rs:153-157`) and the
/// conclusion does not follow: `Lexer::State: State` and `trait State: Debug + Clone`
/// (`tokora/src/lexer/mod.rs:511`, `tokora/src/state/mod.rs:11`), so **`L: Lexer<'inp>` already
/// implies it**. Task 6's mutation proof found it: deleting the clause left the build green both
/// before and after this lift. It is gone rather than kept-just-in-case, because a bound nothing
/// can violate is a bound no reader can trust.
///
/// # Every generic parameter is threaded in from the call site
///
/// `'inp`, `Src` and `Ctx` are macro arguments rather than names minted inside the expansion, so a
/// body may name them: `macro_rules!` lifetimes are hygienic, and a `'inp` declared here would be
/// a *different* `'inp` than one written in the caller's body. Passing them makes the call site
/// read as the ordinary signature it stands for, and lets a `recover.rs` body spell a
/// `<Error<'inp, Src, Ctx> as FromUnclosed<…>>` turbofish.
macro_rules! lossless_production {
  (
    dialect = $dm:ident::$dl:ident;
    $(
      $(#[$meta:meta])*
      fn $name:ident<$lt:lifetime, $src:ident, $ctx:ident>(
        $inp:ident $(, $arg:ident : $argty:ty)* $(,)?
      ) $body:block
    )*
  ) => {$(
    $(#[$meta])*
    pub(crate) fn $name<$lt, $src, $ctx>(
      $inp: &mut $crate::$dm::$dl::Input<$lt, '_, $src, $ctx>,
      $($arg: $argty,)*
    ) -> ::core::result::Result<(), $crate::$dm::$dl::Error<$lt, $src, $ctx>>
    where
      $src: ::tokora::Source<usize> + ?Sized,
      $crate::$dm::$dl::Token<$lt, $src>: ::tokora::Token<
          $lt,
          Kind = $crate::$dm::$dl::TokenKind,
        > + ::tokora::lexer::FromLogos<$lt>
        + ::core::clone::Clone
        + ::tokora::utils::DowncastRef<$crate::$dm::$dl::Keyword>,
      $crate::$dm::$dl::Lexer<$lt, $src>: ::tokora::Lexer<
          $lt,
          Token = $crate::$dm::$dl::Token<$lt, $src>,
          Span = ::tokora::SimpleSpan,
          Offset = usize,
          State = $crate::$dm::$dl::LexerState,
        >,
      $ctx: ::tokora::ParseContext<
        $lt,
        $crate::$dm::$dl::Lexer<$lt, $src>,
        $crate::$dm::$dl::Brand,
      >,
      $ctx::Emitter: ::tokora::emitter::CstEmitter<
        $lt,
        $crate::$dm::$dl::Lexer<$lt, $src>,
        $crate::$dm::$dl::Brand,
      >,
      $crate::$dm::$dl::Error<$lt, $src, $ctx>:
        ::core::convert::From<
            ::tokora::error::UnexpectedEot<usize, $crate::$dm::$dl::Brand>,
          >
          + ::core::convert::From<
            ::tokora::error::token::UnexpectedToken<
              $lt,
              $crate::$dm::$dl::Token<$lt, $src>,
              $crate::$dm::$dl::TokenKind,
              ::tokora::SimpleSpan,
              $crate::$dm::$dl::Brand,
            >,
          >
          + ::tokora::emitter::FromUnclosed<
            $lt,
            $crate::$dm::$dl::Lexer<$lt, $src>,
            $crate::$dm::$dl::Brand,
          >
          + ::core::convert::From<
            ::tokora::error::RecursionLimitReached<usize, $crate::$dm::$dl::Brand>,
          >
          + $crate::lossless::depth::FromNestingLimit
          + $crate::lossless::depth::FromTokenBudget
          + ::tokora::error::MaybeTerminal,
    $body
  )*};
}

/// Declares a production module's `test_support` drivers: `fn(&str) -> Parse`, one per named
/// production, each building a [`Sink`](tokora::cst::Sink), running that one production, draining
/// whatever it left, and materializing.
///
/// # Why every production file needs drivers at all
///
/// A per-production assertion made through the dialect's `parse_document` is only as real as the
/// document dispatcher that reaches the production. While a dispatcher is a stub, such an
/// assertion does not fail — it compares two empty trees and passes, which is worse than failing.
/// These drivers make the assertions real from the first production onward, and they stay useful
/// afterwards: they isolate one production from the recovery behaviour of everything above it.
///
/// # Why a macro rather than a generic function
///
/// **This is where the productions stop being generic.** A driver must choose a concrete source,
/// emitter and context to build a `Sink` at all, exactly as a dialect's `parse_document` does,
/// and the closure it hands to `apply` must spell its parameter type in full — a closure's
/// parameter is **not** inferred through a `ParseInput` bound, only through an `Fn` bound, so
/// `|inp: &mut _|` leaves `L` and `Ctx` unresolved and the body's first method call becomes the
/// error site. A generic `fn(P) -> Parse` taking the production as a value would have to spell
/// that higher-ranked bound at every call; a macro spells the whole driver once.
///
/// # The drain is not optional
///
/// [`Sink::finish`](tokora::cst::Sink::finish) refuses any source byte that no committed token
/// covers and no lexer-error diagnostic explains (`FinishError::UncoveredGap`), and a
/// single-production driver stops at the end of its production by design. The drain also runs on
/// the error path: a production that returns `Err` has committed a prefix and left the rest, and
/// without the drain that would be a panic in the driver instead of a reportable parse.
///
/// # A retro-wrapping production takes a mark, and the driver mints it
///
/// A production whose node is discovered *after* its first child — every definition that may carry
/// a leading description, and every type-system extension, whose shape keyword follows the
/// `extend` its node must contain — takes an [`EventMark`](tokora::cst::event::EventMark) and
/// opens with [`node_at`](tokora::parser::node_at) rather than [`node`](tokora::parser::node). The
/// `(mark)` suffix on a driver declares that second argument.
///
/// **The two are equivalent at position zero**, which is what makes such a driver honest:
/// [`node`](tokora::parser::node) mints its own mark at entry and wraps on success, so a mark
/// minted immediately before the call and spent by `node_at` covers exactly the same region. The
/// difference only appears when the *caller* commits something between the two, which is the whole
/// reason the parameter exists.
///
/// # A const-parameterised production takes its flavour in the declaration
///
/// Every production that can reach a value carries a `Constness` argument (see a dialect's
/// `value.rs` module docs for why it is threaded rather than duplicated), and a driver is a
/// `fn(&str) -> Parse` with no such parameter — so the flavour is *baked into the driver*, in a
/// `[…]` suffix listing the extra arguments to append after `inp`. Two drivers over one production
/// is how both flavours get an entry point, which is what a test asserting the const rejection
/// needs below the document level.
///
/// `Constness` is imported into the generated module unconditionally and the import is
/// `allow(unused_imports)`, so a call site writes the short spelling and a driver set that names
/// no flavour still compiles clean. The import is spelled `super::super::value::Constness` — a
/// literal, dialect-free path, because every invocation of this macro sits in a direct child of
/// its dialect's `lossless` module.
///
/// ```text
/// lossless_drivers! {
///   dialect = graphql::lossless;
///
///   /// Module docs.
///   mod test_support;
///
///   /// Driver docs.
///   fn parse_value => value [Constness::NonConst];
///
///   /// A retro-wrapping production's driver.
///   fn parse_operation_definition => operation_definition (mark);
/// }
/// ```
///
/// # The generated module is behind `feature = "test-support"`
///
/// It was `#[doc(hidden)] pub` and nothing else, which hides a module from rustdoc and ships it
/// anyway: sixteen modules and sixty-eight `fn(&str) -> Parse` entry points, public, callable and
/// semver-relevant in every `rowan` build. The gate is written here, once, rather than at each
/// call site. Fourteen modules and sixty-three drivers remain. Smear issue #67 promoted the four
/// document roots to real entry points on each dialect's `lossless` module, which retired their
/// drivers and emptied GraphQL's `document::test_support` altogether; GraphQLx's own
/// `document::test_support` carried one more that no test ever called,
/// `parse_import_or_executable_definition`, and removing it emptied that module too.
///
/// **`pub` and not `pub(crate)`** because every consumer is a file under `tests/`, which cargo
/// compiles as its own crate and which therefore sees exactly the shipped public surface.
/// `#[cfg(test)]` cannot reach an integration test either. A feature is the only door that both
/// lets `tests/` in and keeps a consumer out.
///
/// # Three items the drivers are still keeping alive, down from fourteen
///
/// Gating the modules turned up fourteen productions whose **only** caller was a driver. Eleven of
/// them were the two alternate document roots in each dialect and the dispatchers and recovery head
/// lists that only those roots name — and smear issue #67 gave the roots real entry points
/// (`parse_type_system_document`, `parse_executable_document`, beside each dialect's
/// `parse_document`), which gave all eleven a caller that is not a test.
///
/// The three that remain are the ones a document root does not reach: GraphQLx's `extension` and
/// `path_or_recover`, and GraphQLx's description-reading `type_system_definition` wrapper, which
/// the three document dispatchers bypass by entering at `type_system_definition_at` with a mark
/// they already minted. They are real productions, deliberately not on any root's path (each says
/// so in its own docs), and they are `pub(crate)`, so with the drivers gone nothing in a shipped
/// build can reach them.
///
/// Each carries `#[cfg_attr(not(feature = "test-support"), allow(dead_code))]` rather than a bare
/// `allow`: the lint stays live in the configuration that has callers, so a driver that stops
/// calling one is still reported. The alternative — one module-wide `allow` — would blanket four
/// thousand lines and hide the next genuinely dead production.
macro_rules! lossless_drivers {
  (
    dialect = $dm:ident::$dl:ident;

    $(#[$modmeta:meta])*
    mod $modname:ident;
    $(
      $(#[$meta:meta])*
      fn $name:ident => $production:ident $(($mark:ident))? $([$($extra:expr),+ $(,)?])?;
    )*
  ) => {
    $(#[$modmeta])*
    // The fourteen driver modules the two suites declare, gated at the one place that writes them
    // all. `#[doc(hidden)]` alone left every one of them in the shipped `rowan` build — public,
    // callable and semver-relevant — for the benefit of `tests/`, which is a separate crate and
    // therefore cannot see anything less than `pub`. The feature removes them from the build
    // instead of merely from the docs; `doc(hidden)` stays because they are not API even when a
    // consumer opts in.
    #[cfg(feature = "test-support")]
    #[doc(hidden)]
    pub mod $modname {
      #[allow(unused_imports)]
      use super::super::value::Constness;

      /// The lexer every driver in this module pins, over `str`.
      type Lx<'inp> = $crate::$dm::$dl::Lexer<'inp, str>;

      /// The context pair and the input each driver's closure receives.
      ///
      /// The sink is held **by value**: `parse_lossless` mints it from the source itself and owns
      /// it for the parse, so there is no `&mut Sink` for a driver to hand around.
      type TestCtx<'inp> = (
        $crate::$dm::$dl::runner::LosslessSink<'inp>,
        ::tokora::cache::DefaultCache<'inp, Lx<'inp>>,
      );
      type TestInput<'inp, 'input> =
        ::tokora::InputRef<'inp, 'input, Lx<'inp>, TestCtx<'inp>, $crate::$dm::$dl::Brand>;

      $(
        $(#[$meta])*
        ///
        /// Test-only scaffolding; nothing in the crate calls it.
        pub fn $name<'inp>(src: &'inp str) -> $crate::$dm::$dl::Parse {
          // The `'inp` is **named**, threaded from `src`, for the reason the trivia driver
          // records: elided, it varies independently of the error type and the closure `E0521`s.
          //
          // `Lang` is `parse_lossless`'s SECOND parameter and is used only in bounds, so it is
          // turbofished alongside the lexer or inference settles it on `()`.
          // The same state the shipped doors seed, and the same two ceilings read off it. A
          // driver that left the context defaulted would run at tokora's `PARSE_DEFAULT_DEPTH`
          // and an unlimited token budget instead of this dialect's, which is a different budget
          // from the one every shipped entry point uses and therefore a driver that no longer
          // drives what ships.
          let state = <$crate::$dm::$dl::LexerState as ::core::default::Default>::default();
          // ONE CALL, exactly as a shipped door does it — smear issue #193, Codex round 4. The
          // context, the parse and the report are one function body in the substrate, so a driver
          // has no context to mint, no reporting authority to hold, and no way to hold one twice.
          //
          // `Lang` is the SECOND parameter and is used only in bounds, so it is turbofished
          // alongside the lexer or inference settles it on `()`.
          //
          // The root is handed in RAW. `root_turn` is what asks whether the one entry ended the
          // document — a driver's ONE production is the whole attempt, and nothing below a root
          // catches, so the baseline it takes immediately before that production is the correctly
          // scoped one. The verdict travels to the drain in the slot rather than being re-derived
          // there, which is the shape every shipped root has (smear PR #189). The drain itself,
          // and the report behind it, are the door's.
          //
          // `::<str, _>`: `Src` is not inferable from the input type, and `str` is the parameter
          // that matches `L::Source`.
          $crate::$dm::$dl::runner::parse_lossless_document(
              src,
              state,
              |inp: &mut TestInput<'inp, '_>,
               stop: &mut $crate::lossless::depth::RootStop| {
                use $crate::lossless::depth::{RootTurn, root_turn};

                // Minted at the top of the ROOT now rather than at the top of the parse closure,
                // because the parse closure is the door's. Same position either way: the door
                // calls the root before anything reads the input.
                $(let $mark = inp.cst_mark();)?
                match root_turn(inp, stop, |inp: &mut TestInput<'inp, '_>| {
                  super::$production::<str, _>(inp $(, $mark)? $($(, $extra)+)?)
                }) {
                  RootTurn::Parsed { parsed } => Ok(parsed),
                  RootTurn::EndsTheDocument { error } | RootTurn::Recoverable { error } => {
                    Err(error)
                  }
                }
              },
            )
        }
      )*
    }
  };
}

/// Generates a dialect's four lossless error conversions.
///
/// **A macro rather than a generic impl, and that is forced.** Each conversion targets the
/// dialect's own `Errors<S>` alias, and coherence forbids one impl covering both. What a macro
/// buys over two hand-written copies is that the *bodies* — which span is zeroed, which
/// unclosed-delimiter names map to which constructor, which end-of-input constructor — are written
/// once.
///
/// # Every argument is a bare `ident`, resolved where the macro is invoked
///
/// Not a `:path`, because three of them (`errors`, `value`, `token`) take generic arguments in the
/// expansion and a `:path` fragment cannot be given any. Each names an item already in scope in
/// the dialect's `lossless/mod.rs`, which is the only place this macro is invoked, so the `use`
/// that brings it there is the same one the surrounding aliases already need.
///
/// # The `unclosed` list
///
/// The delimiter pairs a dialect's grammar opens, each mapped to the error constructor that
/// reports it unterminated. A dialect with a fourth balanced pair (GraphQLx's `<>`) states it here
/// rather than by editing a shared body.
///
/// ```text
/// lossless_error_impls! {
///   errors       = GraphqlLosslessErrors;
///   value        = GraphqlLosslessErrorValue;
///   token        = LosslessToken;
///   kind         = LosslessTokenKind;
///   lexer_errors = LexerErrors;
///   error_data   = ErrorData;
///   expectation  = expectation_of;
///   unclosed     = { "[]" => unclosed_list, "()" => unclosed_parentheses, "{}" => unclosed_object };
/// }
/// ```
macro_rules! lossless_error_impls {
  (
    errors       = $errors:ident;
    value        = $value:ident;
    token        = $token:ident;
    kind         = $kind:ident;
    lexer_errors = $lexer_errors:ident;
    error_data   = $error_data:ident;
    expectation  = $expectation:ident;
    unclosed     = { $($pair:literal => $ctor:ident),+ $(,)? };
  ) => {
    /// The lossless lexer's error channel, landed in the dialect container.
    ///
    /// Unlike the syntactic twin, which flattens a lexer error to a bare `Other("lexer error")`
    /// note, the payload survives here: the container's `StateError` is the lexer's own
    /// `LimitExceeded`, so `ErrorData::Lexer` accepts it unchanged. The span is the one thing that
    /// cannot be recovered — the lexer error type is a *batch* and the container's error carries a
    /// single span — so it is zeroed exactly as the syntactic impl zeroes it.
    impl<S>
      ::core::convert::From<$lexer_errors<char, ::tokora::state::tracker::LimitExceeded>>
      for $errors<S>
    {
      #[inline]
      fn from(err: $lexer_errors<char, ::tokora::state::tracker::LimitExceeded>) -> Self {
        $value::new(
          ::tokora::SimpleSpan::new(0, 0),
          $error_data::Lexer(err),
        )
        .into()
      }
    }

    /// The end-of-input conversion, `Set`-generic exactly as the syntactic twin is, so the one
    /// impl covers both members tokora's `FromTokenErrors` bundle names: the default
    /// `&'static str` set the `_or_stop` family raises, and the `&'static [Kind]` classification
    /// table a committed dispatch driver feeds in.
    ///
    /// Every peek needs it: `InputRef::peek_kind` carries
    /// `Error: From<UnexpectedEot<L::Offset, Lang>>` as a where-clause, so the trivia atom set
    /// cannot be used without it. The container's own error has an `unexpected_end_of_input`
    /// constructor and the offset is the whole payload, so the conversion is total.
    impl<S, Lang: ?Sized, Set: ::core::clone::Clone + 'static>
      ::core::convert::From<::tokora::error::UnexpectedEot<usize, Lang, Set>> for $errors<S>
    {
      #[inline]
      fn from(err: ::tokora::error::UnexpectedEot<usize, Lang, Set>) -> Self {
        let off = err.offset();
        $value::unexpected_end_of_input(::tokora::SimpleSpan::new(off, off)).into()
      }
    }

    /// The declined-`expect` conversion.
    ///
    /// A `None` found token means the input ended where a kind was demanded, and is reported as an
    /// end of input rather than as a token mismatch: the two are different diagnostics and
    /// collapsing them costs the reader the distinction.
    impl<'a, S, Lang: ?Sized>
      ::core::convert::From<
        ::tokora::error::token::UnexpectedToken<'a, $token<S>, $kind, ::tokora::SimpleSpan, Lang>,
      > for $errors<S>
    {
      #[inline]
      fn from(
        err: ::tokora::error::token::UnexpectedToken<
          'a,
          $token<S>,
          $kind,
          ::tokora::SimpleSpan,
          Lang,
        >,
      ) -> Self {
        let (span, found, expected) = err.into_components();
        match found {
          ::core::option::Option::Some(token) => {
            $value::unexpected_token(token.kind(), $expectation(expected), span).into()
          }
          ::core::option::Option::None => $value::unexpected_end_of_input(span).into(),
        }
      }
    }

    /// The unclosed-delimiter conversion.
    ///
    /// **One impl absorbs every delimiter pair.** The dialect's `Unclosed` carries the pair's name
    /// as *data*, so the pair is discriminated at run time on `name_ref` rather than by a `From`
    /// impl per pair — and `Delimiter` stays generic, which is what makes the single bound
    /// `Error<…>: FromUnclosed<…>` cover every pair at once.
    ///
    /// This is a *trait* conversion rather than a `From` impl because `from_unclosed` is generic
    /// over the delimiter marker type. A `From` impl would have to be written per marker, and
    /// every production naming one would then carry one bound per delimiter it can fail to close.
    ///
    /// The catch-all arm produces an error rather than panicking. It is unreachable for a dialect
    /// whose `unclosed` list covers every pair its grammar opens — which is the point of listing
    /// them here rather than in a shared body — but "unreachable" is a claim about the grammar,
    /// and a conversion is the wrong place to enforce one.
    impl<'inp, S, L, Lang: ?Sized> ::tokora::emitter::FromUnclosed<'inp, L, Lang> for $errors<S>
    where
      L: ::tokora::Lexer<'inp, Span = ::tokora::SimpleSpan>,
    {
      #[inline]
      fn from_unclosed<Delimiter>(
        err: ::tokora::error::Unclosed<Delimiter, ::tokora::SimpleSpan, Lang>,
      ) -> Self {
        let span = err.span();
        match err.name_ref() {
          $($pair => $value::$ctor(span).into(),)+
          _ => $value::new(
            span,
            $error_data::Other(::std::borrow::Cow::Borrowed("unclosed delimiter")),
          )
          .into(),
        }
      }
    }

    /// The **parser-frame** nesting refusal, landed in the dialect container.
    ///
    /// Raised by `crate::lossless::depth::descend` when a production tries to enter one level
    /// more than the budget admits, and reported at an empty span on the parse's committed end:
    /// the refused frame has consumed nothing of its own, so there is no lexeme to point at.
    ///
    /// **The payload is a dedicated variant, and it did not used to be.** It was
    /// `Other("nesting limit exceeded")`, on the ruling that a depth trip reaches a consumer
    /// through [`Parse::diagnostics`](crate::lossless::runner::Parse::diagnostics) — which keeps
    /// the span and the severity and drops the typed payload to stay lifetime-free — so a variant
    /// would be observable nowhere the trip is actually read. What that missed is that the
    /// **parser** reads it: smear issue #169's repair asks
    /// [`MaybeTerminal::is_terminal`](::tokora::error::MaybeTerminal::is_terminal) at every
    /// document root that catches, and the dialect's arm for that answers off this variant.
    /// Against a `Cow` message the arm would be a string comparison that a reworded constructor
    /// turns into a permanent `false` — with no build failure and no test failure, only the
    /// amplification back on input that reaches the ceiling.
    ///
    /// **The amplification is no longer what a wrong arm costs, and the variant is still right.**
    /// smear issue #178 put the input's own trip witness beside the arm, in the substrate's
    /// crate-private `root_turn`, so a refusal ends the document whatever this arm answers:
    /// flipping both dialects' arms to `false` now leaves every end-to-end refusal cell in
    /// `nesting_depth.rs` green — three of them were red before — and reddens only the cells that
    /// read the arm at a value, which are the two `terminal.rs` censuses per dialect and
    /// `tokoras_own_descent_trip_lands_terminal_in_both_dialects`. What the variant still buys
    /// is the arm's *own* correctness, which those cells can then assert at the value — and a
    /// **scanner** stop, which rides `MaybeTerminal` alone because tokora's scanner witness is
    /// withdrawn for cause (al8n/tokora#311).
    ///
    /// The residual `lossless/runner.rs` records is untouched: a consumer still sees a positioned
    /// error rather than "this document is too deeply nested", because the projection still drops
    /// the payload. Closing that is a change to the *diagnostic surface*, not to this conversion.
    impl<S> $crate::lossless::depth::FromNestingLimit for $errors<S> {
      #[inline]
      fn nesting_limit_exceeded(
        span: ::tokora::SimpleSpan,
        _attempted: usize,
        _limit: usize,
      ) -> Self {
        $value::nesting_limit_exceeded(span).into()
      }
    }

    /// The **durable token-budget** refusal, landed in the dialect container — smear issue #193.
    ///
    /// Minted by `crate::lossless::depth::drain_unless_stopped` when the input's
    /// [`TokenBudget`](::tokora::input::TokenBudget) refused an item during this root, and
    /// reported at an empty span on the parse's committed end: tokora drops the refused item
    /// where it stands and publishes no span for it, so the committed end is the only position
    /// that describes anything real.
    ///
    /// **A dedicated variant, on the ruling the nesting refusal above already settled**, and here
    /// the reader is more than the parser: this is the *only* thing a refused document reports.
    /// tokora refuses silently, so without this conversion a parse that ran out of its ceiling
    /// comes back as a `Parse` with a truncated tree, a gap-tiled tail and
    /// [`has_errors`](crate::lossless::runner::Parse::has_errors) answering `false` — which a
    /// consumer cannot tell from a document that parsed.
    ///
    /// The numbers are dropped for the reason the nesting refusal's are: the projection behind
    /// [`Parse::diagnostics`](crate::lossless::runner::Parse::diagnostics) keeps the span and the
    /// severity and drops the typed payload, so a consumer sees a positioned error either way.
    /// The trait passes them so a container that *does* keep payloads is not forced to re-derive
    /// them.
    impl<S> $crate::lossless::depth::FromTokenBudget for $errors<S> {
      #[inline]
      fn token_budget_exhausted(
        span: ::tokora::SimpleSpan,
        _spent: usize,
        _limit: usize,
      ) -> Self {
        $value::token_budget_exhausted(span).into()
      }
    }

    /// tokora's own descent trip, landed on the **same variant** smear's own refusal lands on —
    /// and it is now the path **every** lossless refusal takes.
    ///
    /// [`InputRef::descend`](::tokora::InputRef::descend) carries this conversion as a
    /// where-clause, so it has to exist for a production to descend at all. It used to be a
    /// backstop as well: `depth::descend` pre-checked against tokora's `limitation()` and refused
    /// first, so this impl was reachable only if tokora tripped somewhere smear did not look.
    /// The pre-check is gone — the doors install the caller's clamped ceiling as *the*
    /// `RecursionLimiter`, so there is no second number left to check against — and what comes
    /// back out of a refused descent is built **here**. `depth::descend` still emits the
    /// diagnostic through [`FromNestingLimit`](crate::lossless::depth::FromNestingLimit) above,
    /// because a trip is returned and never emitted and the lossless door discards the `Result`;
    /// the two conversions therefore have to agree, and landing them on one variant is what makes
    /// that structural instead of remembered.
    ///
    /// # The backstop carried the discriminator #169 deleted, and that is the whole of this note
    ///
    /// It built `Other("nesting limit exceeded")` — the exact spelling the `FromNestingLimit`
    /// impl above was moved *off*, for the reason recorded there: a `Cow` message is a
    /// discriminator that a reword turns into a permanent `false` with nothing failing. That
    /// argument was applied to one of the two conversions and not to the other, and the half it
    /// missed is the one whose own doc calls it *the backstop if tokora ever trips somewhere
    /// smear did not look* — i.e. exactly the case where nothing else classifies the trip.
    ///
    /// [`MaybeTerminal`](::tokora::error::MaybeTerminal)'s arm for the dialect's `ErrorData::Other`
    /// answers `false`, so a trip arriving here was classified
    /// **recoverable** and a document root resynchronised past it: the pre-#169 amplification,
    /// reachable through the one door #169's repair did not close. tokora's own rule is the other
    /// half of the argument — a frame budget is never cleared by more input, so `false` is never
    /// the right answer for this value however it was carried.
    ///
    /// So it lands on the constructor a real path already uses, and the ask and the construction
    /// stay one enum apart. The offset is tokora's, not smear's: the trip is raised at the
    /// input's committed end, which is the same empty-span position `descend` reports at.
    ///
    /// That the two agree is no longer an observation about two paths that rarely both run — it
    /// is what one refusal now spends: `descend` emits off the span it reads back from the input
    /// and returns what this impl built off tokora's offset, for the same trip. A dialect that
    /// moved either one would put a diagnostic and an error value at different positions for a
    /// single refusal.
    impl<S, Lang: ?Sized>
      ::core::convert::From<::tokora::error::RecursionLimitReached<usize, Lang>> for $errors<S>
    {
      #[inline]
      fn from(err: ::tokora::error::RecursionLimitReached<usize, Lang>) -> Self {
        let off = err.offset();
        $value::nesting_limit_exceeded(::tokora::SimpleSpan::new(off, off)).into()
      }
    }
  };
}

/// Generates a dialect's `is_directive_location` from the one list of nineteen spellings.
///
/// **The nineteen spellings are a frozen, shared vocabulary** — the eight of
/// `ExecutableDirectiveLocation` and the eleven of `TypeSystemDirectiveLocation`, verified
/// byte-identical between the two dialects' `classify_location`. Two copies of a frozen list is
/// two things to get wrong; this is one.
///
/// **The count has been wrong in prose before.** It read "eighteen" for three tasks, from the task
/// that wrote the predicate until a gate report caught it; the *membership* was right the whole
/// time and identical to the syntactic layer's `is_location_keyword`, so it was a miscount rather
/// than a behaviour bug. That is the exact failure mode one list closes, and it is why the arms
/// below — not any sentence about them — are the authority.
///
/// A macro rather than a shared function because the two dialects' `ContextualKeyword` are
/// unrelated types with identical variant names. A trait would put the whole list back in each
/// impl, and a conversion is the measured dead end Task 0b recorded.
macro_rules! directive_location_predicate {
  ($kw:path) => {
    /// Whether `keyword` is one of the **nineteen** spellings `DirectiveLocation` admits: the
    /// eight of `ExecutableDirectiveLocation` and the eleven of `TypeSystemDirectiveLocation`.
    ///
    /// The lexer already tells `QUERY` from `query` — they are different `ContextualKeyword`
    /// variants — so this is a membership test over the projection and never a string comparison.
    #[inline]
    fn is_directive_location(keyword: $kw) -> bool {
      use $kw as DirectiveLocationKeyword;
      ::core::matches!(
        keyword,
        DirectiveLocationKeyword::QueryLocation
          | DirectiveLocationKeyword::MutationLocation
          | DirectiveLocationKeyword::SubscriptionLocation
          | DirectiveLocationKeyword::FieldLocation
          | DirectiveLocationKeyword::FragmentDefinitionLocation
          | DirectiveLocationKeyword::FragmentSpreadLocation
          | DirectiveLocationKeyword::InlineFragmentLocation
          | DirectiveLocationKeyword::VariableDefinitionLocation
          | DirectiveLocationKeyword::SchemaLocation
          | DirectiveLocationKeyword::ScalarLocation
          | DirectiveLocationKeyword::ObjectLocation
          | DirectiveLocationKeyword::FieldDefinitionLocation
          | DirectiveLocationKeyword::ArgumentDefinitionLocation
          | DirectiveLocationKeyword::InterfaceLocation
          | DirectiveLocationKeyword::UnionLocation
          | DirectiveLocationKeyword::EnumLocation
          | DirectiveLocationKeyword::EnumValueLocation
          | DirectiveLocationKeyword::InputObjectLocation
          | DirectiveLocationKeyword::InputFieldDefinitionLocation
      )
    }
  };
}

/// Generates a dialect's `starts_description` from the one two-kind test.
///
/// The two string kinds are the same two in both dialects, and the question — *does a description
/// open here?* — is asked by every described definition and by each document dispatcher. Sharing
/// it costs one macro and removes the second place a dialect could forget `BlockString`.
macro_rules! description_head_predicate {
  ($kind:path) => {
    /// Whether `head` opens a description — the one two-kind test a definition file makes
    /// repeatedly, and which the document dispatchers make once each.
    #[inline]
    pub(crate) fn starts_description(head: ::core::option::Option<$kind>) -> bool {
      use $kind as DescriptionHeadKind;
      ::core::matches!(
        head,
        ::core::option::Option::Some(
          DescriptionHeadKind::InlineString | DescriptionHeadKind::BlockString
        )
      )
    }
  };
}

pub(crate) use description_head_predicate;
pub(crate) use directive_location_predicate;
pub(crate) use lossless_drivers;
pub(crate) use lossless_error_impls;
pub(crate) use lossless_production;

/// Declares a dialect's **lossless door** — the one function that runs a lossless document parse,
/// and the only place a durable token-budget refusal is reported.
///
/// # Why this is a macro and not a generic function — smear issue #193, Codex rounds 4 to 6
///
/// Three rounds found the same defect wearing three costumes, and each repair moved the choice one
/// type to the left instead of removing it:
///
/// * **round 4, the token.** A `DocumentRoot` a `pub` factory minted — so an in-crate caller could
///   mint a second one with throwaway arguments and report twice over one input;
/// * **round 5, the emitter.** `E: Emitter + ValueKeyedEmitter` — a marker about checkpoint
///   semantics, not about ownership, so a value-keyed collector over shared state could hand one
///   handle to an outer parse and another to a nested one. Measured at **2** reports for one
///   refusal;
/// * **round 6, the error container.** `Errs` stayed caller-chosen, and `FromTokenBudget` for it is
///   in-crate code: a container backed by a shared bag can clone the same handle out of
///   `token_budget_exhausted` into both parses' stores.
///
/// The class is *an in-crate caller chooses a type through which state can be shared*, and no bound
/// ends it — `Default`, a sealed trait and a purity comment are all things this crate can satisfy
/// from inside. What ends it is the principle this macro exists to make true: **the report is made
/// by code that chose every type it runs over.** The expansion fixes the lexer, the grammar brand,
/// the error container, the cache and the profile to that dialect's own, reads both budgets off
/// `limits` itself, and keeps its emission private to the module it lands in. The one thing a
/// caller still supplies is the parser — and a parser can reach only
/// [`drain_unless_stopped`](crate::lossless::depth::drain_unless_stopped), which **stops and never
/// emits**.
///
/// # One door per dialect, and coherence is what says so
///
/// The expansion carries a marker impl —
/// `impl crate::lossless::depth::DoorOwner for <that dialect's brand>` — on a concrete type the
/// dialect owns. A second invocation for the same dialect, anywhere in this crate, is a second impl
/// of that trait for that type: **E0119**, at compile time, before any test runs. Without it this
/// macro would be the round-4 forgery again one level up — an in-crate module could invoke it
/// against the real dialect, get a private report function over the real `InputRef` type in its own
/// module, and call it from a composed root inside the real parse.
///
/// The trait has no methods and nothing reads it. Its only job is to be uninhabitable twice.
///
/// # Why the substrate does not hold the door itself
///
/// Because it may not name a dialect: `smear/tests/lossless_isolation.rs` forbids both dialect
/// crate roots and the lexer crate's own path anywhere under `smear-parser/src/lossless` — its
/// `SUBSTRATE_FORBIDDEN` table is the list — and a door that fixes every type has to spell all
/// three. Note that the gate matches on the LINE and not on whether it is code, so even this
/// paragraph has to describe those paths rather than write them. So the substrate holds this
/// **text**, which names
/// no dialect, and each dialect's `runner.rs` holds the expansion. That is also what keeps the
/// report site private: it is generated *into* the module that owns it, so neither the substrate
/// nor the other dialect can reach it, and nothing had to be widened to `pub(crate)` to make the
/// move.
///
/// # The residual, stated
///
/// The two dialect `runner.rs` modules, and only by editing them. Everything a forger would need —
/// the emission, the context, the profile — is private to the module the invocation lands in, and
/// the substrate below has no way to emit a budget refusal at all.
macro_rules! lossless_door {
  (
    dialect = $dm:ident::$dl:ident;
    errors  = $errors:ident;
  ) => {
    // ONE DOOR PER DIALECT, BY COHERENCE. See the macro's own note; a second invocation naming
    // this dialect is `E0119` on this line.
    impl $crate::lossless::depth::DoorOwner for $crate::$dm::$dl::Brand {}

    // AND THE DOOR READS ITS OWN MARKER. Without this the trait has an impl and no user, which is
    // `dead_code` on a `-Dwarnings` build; with it the assertion is also the true statement — the
    // function below is the door this dialect's brand owns.
    const _: () = {
      const fn door_is_owned<T: $crate::lossless::depth::DoorOwner>() {}
      door_is_owned::<$crate::$dm::$dl::Brand>();
    };

    /// **Builds** the durable token-budget refusal. It does not emit it, and after smear issue
    /// #193's round 8 nothing in this crate does.
    ///
    /// Rounds 4 to 7 each fenced who could obtain the capability to *emit*. Codex round 7 showed
    /// that fence has no last door: the grammar holds the same `InputRef` and `emit_error` is
    /// public, so a root can emit this very variant — twice, or with nothing refused. So the door
    /// stopped emitting. It builds the value here, hands it out of the parse, and
    /// `crate::lossless::runner::finish_parsed_root_with` decides what the finished log says. The
    /// grammar's emissions on this channel become inputs to that decision rather than peers of it.
    fn token_budget_report<'inp>(
      inp: &mut ::tokora::InputRef<
        'inp,
        '_,
        $crate::$dm::$dl::Lexer<'inp, str>,
        DoorCtx<'inp>,
        $crate::$dm::$dl::Brand,
      >,
    ) -> ($errors<&'inp str>, ::core::ops::Range<usize>) {
      // THE COMMITTED END. tokora drops the refused item where it stands and publishes no span
      // for it, so this is the only position that describes anything real: the last byte the parse
      // actually committed.
      let end = ::tokora::InputRef::span(inp).end();
      let span = ::tokora::SimpleSpan::new(end, end);
      let spent = inp.token_budget().spent();
      let limit = inp.token_budget().limitation();
      (
        <$errors<&'inp str> as $crate::lossless::depth::FromTokenBudget>::token_budget_exhausted(
          span, spent, limit,
        ),
        end..end,
      )
    }

    /// The context this door drives its parse under, spelled once.
    ///
    /// `parse_lossless_with_context` re-seats the caller's context around the `Sink` it mints from
    /// the source, so what an `InputRef` inside the parse is parameterised by is the pair.
    type DoorCtx<'inp> = (
      ::tokora::cst::Sink<'inp, $crate::$dm::$dl::Lexer<'inp, str>, LosslessEmitter<'inp>>,
      ::tokora::cache::DefaultCache<'inp, $crate::$dm::$dl::Lexer<'inp, str>>,
    );

    /// The **only** way to run a lossless document parse in this dialect, and the only place a
    /// budget refusal is reported.
    ///
    /// Builds the context, runs tokora's lossless driver over `root`, drains what an escape left
    /// behind, reports a refusal if the input's durable tally has one, and hands back the
    /// [`LosslessCst`] the door materialises plus the driver's own `Result`.
    ///
    /// # Every type is this function's choice, and both budgets are read here
    ///
    /// The lexer, the grammar brand, the error container, the emitter, the cache and the profile
    /// are all named above — a caller supplies the source, the limits and the parser, and none of
    /// those can carry a diagnostic anywhere. `limits.parse_ceiling()` and
    /// `limits.max_produce_events()` are read **here** rather than passed in, so no door can forget
    /// one or vary it; that is what makes "every door installs the same ceiling" a fact about one
    /// line instead of a check over six.
    ///
    /// # Nesting it is not a forgery
    ///
    /// A composed root inside `root` can call it, because it is `pub(crate)`. What that does is
    /// start a **separate parse** — its own `Input`, its own `Sink`, its own emitter, its own tally
    /// — whose report lands in that parse's own log. `smear-parser`'s
    /// `a_nested_door_reports_into_its_own_parse_and_not_the_enclosing_one` measures both halves.
    ///
    /// # The drain is not optional
    ///
    /// [`Sink::finish`](tokora::cst::Sink::finish) refuses any source byte that no committed token
    /// covers and no lexer-error diagnostic explains, and an `Err` escaping a document root leaves
    /// the rest of the source uncommitted. Draining here is what turns that into a reportable parse
    /// rather than a panic in materialisation — smear issue #57. It is
    /// [`drain_unless_stopped`](crate::lossless::depth::drain_unless_stopped) rather than a bare
    /// `skip_while` because a refusal must not read the tail.
    ///
    /// # The verdict never leaves this function — round 8
    ///
    /// The refusal is built, not emitted, and this function's own last statement is what decides
    /// whether the finished parse says so — it finishes its own `Cst` and returns a [`Parse`].
    /// Neither the `Cst`, the driver's `Result` nor the verdict is handed out, because each of the
    /// three is a way for an in-crate caller to say something about a parse it did not run: finish
    /// the tree without the verdict, or finish it with a forged one. Rounds 4 to 7
    /// each fenced who could obtain the capability to report and each fence had a next door;
    /// Codex round 7's case has none, because the grammar holds this door's own `InputRef` and
    /// `emit_error` is public — a root can emit this variant twice, or once with nothing refused.
    /// A decision taken over the finished log ends that: the grammar's emissions are **inputs** to
    /// it rather than peers of it.
    ///
    /// # The driver's `Result` is dropped here
    ///
    /// A lossless door keeps the tree and the diagnostics and throws the parser's `Result` away —
    /// which is the whole reason a refusal has to be *reported* rather than only returned. It used
    /// to be dropped at each door's `finish_root` line; the fold moved that inside, so it is
    /// dropped once. A cell that needs the stop VALUE runs the inner frame, which is where that
    /// value is decided — `the_value_a_frame_hands_up_after_a_drain_refusal_is_terminal` is the
    /// one that does.
    pub(crate) fn parse_lossless_document<'inp, Root>(
      src: &'inp str,
      limits: $crate::$dm::$dl::LexerState,
      root: Root,
    ) -> Parse
    where
      Root: for<'closure> ::core::ops::FnOnce(
          &mut ::tokora::InputRef<
            'inp,
            'closure,
            $crate::$dm::$dl::Lexer<'inp, str>,
            DoorCtx<'inp>,
            $crate::$dm::$dl::Brand,
          >,
          &mut $crate::lossless::depth::RootStop,
        ) -> ::core::result::Result<(), $errors<&'inp str>>,
    {
      // THE CONTEXT IS BUILT HERE, from tokora's own API and nothing of this crate's. There is no
      // smear mint left to call: round 5 deleted `lossless_context` rather than widen it, and
      // three chained tokora calls are what replaced it.
      let context = ::tokora::input::InputContext::new(
        <LosslessEmitter<'inp> as ::core::default::Default>::default(),
        ::tokora::cache::DefaultCache::<$crate::$dm::$dl::Lexer<'inp, str>>::default(),
      )
      .with_recursion_limiter(
        ::tokora::state::recursion_tracker::RecursionLimiter::with_limitation(
          limits.parse_ceiling(),
        ),
      )
      .with_token_budget(::tokora::input::TokenBudget::with_limitation(
        limits.max_produce_events(),
      ));

      // `Option` + `take`: `ParseInput` is implemented for `FnMut` and a closure that moves a
      // non-`Copy` capture out is only `FnOnce`. The root runs once per parse.
      let mut root = ::core::option::Option::Some(root);
      // THE DOOR'S VERDICT, carried out of the parse rather than written into it. See
      // `token_budget_report`.
      let mut door_report = ::core::option::Option::None;
      let (cst, out) = ::tokora::cst::parse_lossless_with_context::<
        $crate::$dm::$dl::Lexer<'inp, str>,
        $crate::$dm::$dl::Brand,
        _,
        _,
        _,
        _,
      >(
        src,
        limits,
        context,
        profile::<str>(),
        |inp: &mut ::tokora::InputRef<
          'inp,
          '_,
          $crate::$dm::$dl::Lexer<'inp, str>,
          DoorCtx<'inp>,
          $crate::$dm::$dl::Brand,
        >| {
          let out = $crate::lossless::depth::drain_unless_stopped(
            inp,
            root.take().expect("the root runs once per parse"),
          );
          // THE ONE VERDICT. Unconditional on what `out` is: a refusal is a refusal whether the
          // frame below ended on it, on a descent trip, on a syntax error or on nothing at all,
          // and the document is truncated in every one of those cases. Nothing is emitted — the
          // value goes into `door_report`, and this function's own last statement is what decides.
          if inp.token_budget().refused_an_item() {
            door_report = ::core::option::Option::Some(token_budget_report(inp));
          }
          out
        },
      );
      // `out` — the driver's `Result` — is dropped HERE, which is where every lossless door has
      // always dropped it, and the `Cst` never leaves this function. That is round 8's fold: a
      // door that handed back a `Cst` and a verdict handed an in-crate caller two ways to lie
      // about a parse it did not run — finish it without the verdict, or finish it with a forged
      // one. What leaves is a finished `Parse`, which is a value and not a capability.
      let _ = out;

      $crate::lossless::runner::finish_parsed_root_with(
        cst,
        K::Root.raw(),
        <K as $crate::lossless::KindSpace>::NAME,
        // THE DIALECT'S OWN VARIANT, recognised here because the substrate may not name it — and
        // asked at the MEMBER, which is round 8's own precision defect closed. The container is
        // the GRAMMAR's unit of emission and the variant is the DOOR's, so one `emit_error`
        // carrying `[TokenBudgetExhausted, FloatOverflow]` keeps its `FloatOverflow`: what this
        // answers is *is there anything here that is not mine*, and only a payload that is nothing
        // but the door's variant is dropped. Asking `any(is the variant)` instead threw away the
        // ordinary error with it and turned `has_errors()` false on a document that had one.
        // `is_empty() ||` is round 9's finding and it is not a guard against nothing: both dialect
        // containers implement `Default`, tokora records whatever payload it is handed, and
        // `any` over an empty `Vec` is `false` — so a root emitting `Errors::default()` produced a
        // record this classifier called "nothing but mine" and dropped. With no refusal to replace
        // it the finished parse had no diagnostics at all and `has_errors()` was false. An empty
        // container holds NO member of the door's, so there is nothing here for the door to take
        // away, and the record stays exactly as it arrived.
        |errs: &$errors<&'inp str>| {
          errs.is_empty()
            || errs
              .iter()
              .any(|e| !::core::matches!(e.data(), ErrorData::TokenBudgetExhausted))
        },
        // THE VERDICT, as a span. The substrate decides what it looks like in a `Parse`.
        door_report.map(|(_, span)| span),
      )
    }
  };
}

// AFTER the definition, because `macro_rules!` is textually scoped and a `use` above it does not
// resolve. The five re-exports higher up sit below their own definitions for the same reason.
pub(crate) use lossless_door;
