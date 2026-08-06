//! The three declaration macros a lossless assembly is built out of.
//!
//! # Why a macro and not a generic item, in all three cases
//!
//! Each of these would be a function, a trait or an `impl` if it could be. None of them can:
//!
//! - `lossless_production!` declares a **signature**, not a value — nine where-clauses over a
//!   dialect's seven types. A generic function cannot stand in for a signature its caller writes
//!   bodies against.
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
/// # The seven names a dialect owes this macro
///
/// `Input`, `Error`, `Token`, `Lexer`, `Brand`, `TokenKind` and `Keyword`, at
/// `crate::<dialect>::lossless`. They are aliases over whatever that dialect already calls those
/// things, so adopting the macro renames nothing.
///
/// # Why the bundle is one block, and why it is here rather than per dialect
///
/// The bundle is eight clauses long and **not one of them is optional** once a production both
/// opens a node and goes through the atom set — which is every production in either suite.
/// Written out per function it is ~150 lines of boilerplate across `value.rs` alone, and the
/// failure mode of drifting copies is a compile error a hundred lines from its cause. Written out
/// per *dialect* it is the same nine clauses twice, and the failure mode is worse: the two copies
/// drift and the second dialect's productions accept a bound the first rejects.
///
/// # The clauses, and what each one buys
///
/// - **`Token<'inp, Kind = TokenKind>`** — a dialect's lossless `Token` impl is macro-generated
///   **once per concrete slice type** (`smear-lexer`'s `token_impl!`), unlike the syntactic
///   token's generic impl, so over a generic `Src` the projection `<Token<…>>::Kind` has nothing
///   to normalize against. `Token<'inp>` alone makes the projection *nameable*; only the `Kind =`
///   equality makes a token-kind **literal** — which is what every `expect(inp, Kind::LBracket)`
///   passes — typecheck against it.
/// - **`FromLogos<'inp>`** — `LogosLexer<'inp, T>` carries it on its struct definition, so without
///   it the lexer alias is ill-formed rather than merely unbounded.
/// - **`Clone`** — `UnexpectedToken::with_found` takes the token by value, and a declined token is
///   only ever borrowed (it stays unconsumed, on purpose).
/// - **`DowncastRef<Keyword>`** — `true`/`false`/`null` and every other contextual keyword arrive
///   as identifier tokens; the value dispatcher tells them apart on their spelling.
/// - **the `Lexer` associated-type pins** — without `Span`/`Offset` the two error `From` bounds
///   cannot be spelled at all.
/// - **`Ctx::Emitter: CstEmitter`** — the structural gate on the whole `node` family.
/// - **the three error conversions** — `UnexpectedEot` for every peek, `UnexpectedToken` for every
///   declined `expect`, and `FromUnclosed` for the unterminated-delimiter reports.
///
/// # The clause that used to be here and is not
///
/// A ninth clause, `<Lexer as Lexer<'inp>>::State: Clone`, rode along documented as
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
          >,
    $body
  )*};
}

/// Declares a production module's `test_support` drivers: `fn(&str) -> Parse`, one per named
/// production, each building a [`Sink`](tokora::cst::Sink), running that one production, draining
/// whatever it left, and materializing.
///
/// # Why every production file needs drivers at all
///
/// A per-production assertion made through the dialect's `parse_str` is only as real as the
/// document dispatcher that reaches the production. While a dispatcher is a stub, such an
/// assertion does not fail — it compares two empty trees and passes, which is worse than failing.
/// These drivers make the assertions real from the first production onward, and they stay useful
/// afterwards: they isolate one production from the recovery behaviour of everything above it.
///
/// # Why a macro rather than a generic function
///
/// **This is where the productions stop being generic.** A driver must choose a concrete source,
/// emitter and context to build a `Sink` at all, exactly as a dialect's `parse_str` does, and the
/// closure it hands to `apply` must spell its parameter type in full — a closure's parameter is
/// **not** inferred through a `ParseInput` bound, only through an `Fn` bound, so `|inp: &mut _|`
/// leaves `L` and `Ctx` unresolved and the body's first method call becomes the error site. A
/// generic `fn(P) -> Parse` taking the production as a value would have to spell that
/// higher-ranked bound at every call; a macro spells the whole driver once.
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
/// call site. Fifteen modules and sixty-four drivers remain — smear issue #67 promoted the four
/// document roots to real entry points on each dialect's `lossless` module, which retired their
/// drivers and emptied GraphQL's `document::test_support` altogether.
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
/// (`parse_type_system_document`, `parse_executable_document`, beside each dialect's `parse_str`),
/// which gave all eleven a caller that is not a test.
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
    // The fifteen driver modules the two suites declare, gated at the one place that writes them
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
          let (cst, _out) =
            ::tokora::cst::parse_lossless::<Lx<'inp>, $crate::$dm::$dl::Brand, _, _, _, _>(
              src,
              ::core::default::Default::default(),
              ::core::default::Default::default(),
              $crate::$dm::$dl::runner::profile::<str>(),
              ::tokora::cache::DefaultCache::<Lx<'_>>::default(),
              |inp: &mut TestInput<'inp, '_>| {
                // Minted before the call, not inside the argument list: `inp` is already borrowed
                // mutably as the first argument, so a second `inp` method call in the second
                // would be a second mutable borrow of the same value.
                $(let $mark = inp.cst_mark();)?
                // `::<str, _>`: `Src` is not inferable from the input type, and `str` is the
                // parameter that matches `L::Source`.
                let out = super::$production::<str, _>(inp $(, $mark)? $($(, $extra)+)?);
                inp.skip_while(|_| true)?;
                out
              },
            );

          $crate::$dm::$dl::runner::finish_root(cst)
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
