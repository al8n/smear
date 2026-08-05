//! The GraphQL lossless parser suite: a `rowan` CST over the trivia-surfacing lexer.

use smear_lexer::graphql::{
  error::LexerErrors,
  lossless::{LosslessLexer, LosslessToken, LosslessTokenKind},
};
use tokora::{
  InputRef, Lexer, SimpleSpan, Source,
  emitter::FromUnclosed,
  error::{
    Unclosed as TokoraUnclosed, UnexpectedEot, token::UnexpectedToken as TokUnexpectedToken,
  },
  state::tracker::LimitExceeded,
  utils::Expected,
};

use crate::{
  combinator::ErrorOf,
  graphql::{
    GraphQL,
    error::{Error as DialectError, ErrorData, Errors as DialectErrors, Expectation},
  },
};

pub use crate::graphql::kinds::GraphQLLang;

/// A GraphQL lossless syntax node.
pub type SyntaxNode = rowan::SyntaxNode<GraphQLLang>;
/// A GraphQL lossless syntax token.
pub type SyntaxToken = rowan::SyntaxToken<GraphQLLang>;
/// A node-or-token in the GraphQL lossless CST.
pub type SyntaxElement = rowan::SyntaxElement<GraphQLLang>;

/// The source slice emitted by [`GraphqlLosslessLexer`] for `Src`.
///
/// Declared before the lexer alias, not after it as in `syntactic/`, because the lexer alias is
/// written in terms of this one.
#[allow(type_alias_bounds)]
pub type GraphqlLosslessSlice<'inp, Src: Source<usize> + ?Sized> =
  <Src as Source<usize>>::Slice<'inp>;

/// The concrete lexer used by GraphQL lossless productions over `Src`.
///
/// **Note the argument.** `LosslessLexer<'a, S = &'a str> = LogosLexer<'a, LosslessToken<S>>`
/// (`smear-lexer/src/graphql/lossless/mod.rs:16`) is parameterised by the **slice** type, not by
/// the source type — unlike `SyntacticLexer`, which takes the source. Writing
/// `LosslessLexer<'inp, Src>` here compiles into a lexer over the wrong token and then fails far
/// away, at the first `Lexer<'inp>` obligation.
#[allow(type_alias_bounds)]
pub type GraphqlLosslessLexer<'inp, Src: Source<usize> + ?Sized> =
  LosslessLexer<'inp, GraphqlLosslessSlice<'inp, Src>>;

/// The concrete token emitted by [`GraphqlLosslessLexer`] for `Src`.
#[allow(type_alias_bounds)]
pub type GraphqlLosslessToken<'inp, Src: Source<usize> + ?Sized> =
  LosslessToken<GraphqlLosslessSlice<'inp, Src>>;

/// The parser error emitted by a GraphQL lossless production.
#[allow(type_alias_bounds)]
pub type GraphqlLosslessError<'inp, Src: Source<usize> + ?Sized, Ctx>
where
  GraphqlLosslessLexer<'inp, Src>: Lexer<'inp>,
  Ctx: tokora::ParseContext<'inp, GraphqlLosslessLexer<'inp, Src>, GraphQL>,
= ErrorOf<'inp, GraphqlLosslessLexer<'inp, Src>, Ctx, GraphQL>;

/// A mutable GraphQL lossless parser input over `Src` and parser context `Ctx`.
#[allow(type_alias_bounds)]
pub type GraphqlLosslessInput<'inp, 'input, Src: Source<usize> + ?Sized, Ctx>
where
  GraphqlLosslessLexer<'inp, Src>: Lexer<'inp>,
  Ctx: tokora::ParseContext<'inp, GraphqlLosslessLexer<'inp, Src>, GraphQL>,
= InputRef<'inp, 'input, GraphqlLosslessLexer<'inp, Src>, Ctx, GraphQL>;

/// One error value a GraphQL lossless parse can record.
///
/// The dialect's own error, **re-keyed** to the lossless token kind and the lossless lexer's
/// state error. `GraphqlError` cannot serve: it pins `SyntacticTokenKind`, which has no image
/// for a trivia token, so a lossless "unexpected token" would have to invent a kind for the
/// comment or the newline it found.
pub type GraphqlLosslessErrorValue<S> =
  DialectError<S, LosslessTokenKind, char, Expectation, LimitExceeded>;

/// The error container the lossless driver pins for `Verbose`.
///
/// Productions never name this: they name [`GraphqlLosslessError`], which projects the error
/// out of the parse context, and constrain it with `From<…>` bounds. Only [`runner::parse_str`]
/// — the one place a concrete context is chosen — has to say which container that is.
pub type GraphqlLosslessErrors<S> =
  DialectErrors<S, LosslessTokenKind, char, Expectation, LimitExceeded>;

/// The lossless lexer's error channel, landed in the dialect container.
///
/// Unlike the syntactic twin (`error.rs:892`), which flattens a lexer error to a bare
/// `Other("lexer error")` note, the payload survives here: the container's `StateError` is the
/// lexer's own `LimitExceeded`, so `ErrorData::Lexer` accepts it unchanged. The span is the
/// one thing that cannot be recovered — `LexerErrors` is a batch, and the container's error
/// carries a single span — so it is zeroed exactly as the syntactic impl zeroes it.
impl<S> From<LexerErrors<char, LimitExceeded>> for GraphqlLosslessErrors<S> {
  #[inline]
  fn from(err: LexerErrors<char, LimitExceeded>) -> Self {
    GraphqlLosslessErrorValue::new(SimpleSpan::new(0, 0), ErrorData::Lexer(err)).into()
  }
}

/// The end-of-input conversion, `Set`-generic exactly as the syntactic twin
/// (`error.rs:857`) is, so the one impl covers both members tokora's `FromTokenErrors` bundle
/// names: the default `&'static str` set the `_or_stop` family raises, and the
/// `&'static [Kind]` classification table a committed dispatch driver feeds in.
///
/// Task 3 did not need this — `document`'s stub only calls `skip_while`, which raises no
/// end-of-input error — but every peek does: `InputRef::peek_kind` carries
/// `Error: From<UnexpectedEot<L::Offset, Lang>>` as a where-clause, so the trivia atom set
/// (Task 4) cannot be written without it. The container's own error has an
/// `unexpected_end_of_input` constructor and the offset is the whole payload, so the
/// conversion is total.
impl<S, Lang: ?Sized, Set: Clone + 'static> From<UnexpectedEot<usize, Lang, Set>>
  for GraphqlLosslessErrors<S>
{
  #[inline]
  fn from(err: UnexpectedEot<usize, Lang, Set>) -> Self {
    let off = err.offset();
    GraphqlLosslessErrorValue::unexpected_end_of_input(SimpleSpan::new(off, off)).into()
  }
}

impl<'a, S, Lang: ?Sized>
  From<TokUnexpectedToken<'a, LosslessToken<S>, LosslessTokenKind, SimpleSpan, Lang>>
  for GraphqlLosslessErrors<S>
{
  #[inline]
  fn from(
    err: TokUnexpectedToken<'a, LosslessToken<S>, LosslessTokenKind, SimpleSpan, Lang>,
  ) -> Self {
    let (span, found, expected) = err.into_components();
    match found {
      Some(token) => {
        GraphqlLosslessErrorValue::unexpected_token(token.kind(), expectation_of(expected), span)
          .into()
      }
      None => GraphqlLosslessErrorValue::unexpected_end_of_input(span).into(),
    }
  }
}

/// The unclosed-delimiter conversion, mirroring `error.rs:872`.
///
/// **One impl absorbs every delimiter pair.** The dialect's `Unclosed` carries the pair's
/// name as *data*, so the pair is discriminated at run time on `name_ref` rather than by a
/// `From` impl per pair — and `Delimiter` stays generic, which is what makes the single bound
/// `GraphqlLosslessError<…>: FromUnclosed<…>` cover `[]`, `{}` and `()` at once. GraphQL's
/// grammar opens no other pair, so the catch-all arm is unreachable in practice; it still has
/// to produce an error rather than panic.
///
/// This is a *trait* conversion rather than a `From` impl because `from_unclosed` is generic
/// over the delimiter marker type. A `From` impl would have to be written per marker, and
/// every production naming one would then carry one bound per delimiter it can fail to close.
impl<'inp, S, L, Lang: ?Sized> FromUnclosed<'inp, L, Lang> for GraphqlLosslessErrors<S>
where
  L: Lexer<'inp, Span = SimpleSpan>,
{
  #[inline]
  fn from_unclosed<Delimiter>(err: TokoraUnclosed<Delimiter, SimpleSpan, Lang>) -> Self {
    let span = err.span();
    match err.name_ref() {
      "[]" => GraphqlLosslessErrorValue::unclosed_list(span).into(),
      "()" => GraphqlLosslessErrorValue::unclosed_parentheses(span).into(),
      "{}" => GraphqlLosslessErrorValue::unclosed_object(span).into(),
      _ => GraphqlLosslessErrorValue::new(
        span,
        ErrorData::Other(std::borrow::Cow::Borrowed("unclosed delimiter")),
      )
      .into(),
    }
  }
}

/// The lossless twin of `error.rs`'s `expectation_from_token_kind`.
///
/// `ErrorData::UnexpectedToken` has one expectation slot, so a multi-kind or absent expectation
/// falls back to `Name` exactly as the syntactic mapping does. Every trivia kind falls back too:
/// no production expects a comment or a newline — trivia is skipped into the tree by the atom
/// set, never demanded — so those arms are unreachable in practice rather than merely lossy.
fn expectation_of(expected: Option<Expected<'_, LosslessTokenKind>>) -> Expectation {
  match expected {
    Some(Expected::One(kind)) => match kind {
      LosslessTokenKind::Identifier => Expectation::Name,
      LosslessTokenKind::Int => Expectation::IntValue,
      LosslessTokenKind::Float => Expectation::FloatValue,
      LosslessTokenKind::InlineString => Expectation::InlineString,
      LosslessTokenKind::BlockString => Expectation::BlockString,
      LosslessTokenKind::Dollar => Expectation::Dollar,
      LosslessTokenKind::LParen => Expectation::LParen,
      LosslessTokenKind::RParen => Expectation::RParen,
      LosslessTokenKind::Spread => Expectation::Spread,
      LosslessTokenKind::Colon => Expectation::Colon,
      LosslessTokenKind::Equal => Expectation::Equal,
      LosslessTokenKind::At => Expectation::At,
      LosslessTokenKind::LBracket => Expectation::LBracket,
      LosslessTokenKind::RBracket => Expectation::RBracket,
      LosslessTokenKind::LBrace => Expectation::LBrace,
      LosslessTokenKind::RBrace => Expectation::RBrace,
      LosslessTokenKind::Pipe => Expectation::Pipe,
      LosslessTokenKind::Bang => Expectation::Bang,
      LosslessTokenKind::Ampersand => Expectation::Ampersand,
      _ => Expectation::Name,
    },
    _ => Expectation::Name,
  }
}

/// Declares a lossless production: `fn(&mut GraphqlLosslessInput, …) -> Result<(),
/// GraphqlLosslessError>`, with the one where-clause every production in this suite carries.
///
/// # Why a macro, and why the bundle is one block
///
/// The bundle is nine clauses long and **not one of them is optional** once a production both
/// opens a node and goes through the atom set — which is every production here. Written out
/// per function it would be ~150 lines of boilerplate across `value.rs` alone, and the
/// failure mode of drifting copies is a compile error a hundred lines from its cause. The
/// house precedent is `syntactic/`'s `value_parser!` and `trivia.rs`'s `drive!`.
///
/// # The clauses, and what each one buys
///
/// - **`Token<'inp, Kind = LosslessTokenKind>`** — the correction Task 4 surfaced and this
///   task confirmed. `LosslessToken<S>`'s `Token` impl is macro-generated **once per concrete
///   slice type** (`smear-lexer` `lossless/token.rs`), unlike `SyntacticToken<S>`'s generic
///   impl, so over a generic `Src` the projection `<GraphqlLosslessToken<…>>::Kind` has
///   nothing to normalize against. `Token<'inp>` alone makes the projection *nameable*; only
///   the `Kind =` equality makes a `LosslessTokenKind` **literal** — which is what every
///   `expect(inp, Kind::LBracket)` passes — typecheck against it.
/// - **`FromLogos<'inp>`** — `LogosLexer<'inp, T>` carries it on its struct definition, so
///   without it the lexer alias is ill-formed rather than merely unbounded (`document.rs`).
/// - **`Clone`** — `UnexpectedToken::with_found` takes the token by value, and a declined
///   token is only ever borrowed (it stays unconsumed, on purpose).
/// - **`DowncastRef<ContextualKeyword>`** — `true`/`false`/`null` are `Identifier` tokens; the
///   value dispatcher tells them apart on their spelling, exactly as `syntactic/mod.rs:110`
///   does.
/// - **the `Lexer` associated-type pins** — without `Span`/`Offset` the two error `From`
///   bounds cannot be spelled at all.
/// - **`L::State: Clone`** — `InputRef::sync_balanced` lives in an impl block that requires
///   it. Nothing else in this suite does.
/// - **`Ctx::Emitter: CstEmitter`** — the structural gate on the whole `node` family.
/// - **the three error conversions** — `UnexpectedEot` for every peek, `UnexpectedToken` for
///   every declined `expect`, and `FromUnclosed` for the two unterminated-delimiter reports.
///
/// # Every generic parameter is threaded in from the call site
///
/// `'inp`, `Src` and `Ctx` are macro arguments rather than names minted inside the expansion,
/// so a body may name them: `macro_rules!` lifetimes are hygienic, and a `'inp` declared here
/// would be a *different* `'inp` than one written in the caller's body. Passing them makes the
/// call site read as the ordinary signature it stands for, and `recover.rs`'s
/// `<GraphqlLosslessError<'inp, Src, Ctx> as FromUnclosed<…>>` turbofish resolve. The same
/// choice, for the same reason, as `trivia.rs`'s `drive!`.
macro_rules! lossless_production {
  ($(
    $(#[$meta:meta])*
    fn $name:ident<$lt:lifetime, $src:ident, $ctx:ident>(
      $inp:ident $(, $arg:ident : $argty:ty)* $(,)?
    ) $body:block
  )*) => {$(
    $(#[$meta])*
    pub(crate) fn $name<$lt, $src, $ctx>(
      $inp: &mut $crate::graphql::lossless::GraphqlLosslessInput<$lt, '_, $src, $ctx>,
      $($arg: $argty,)*
    ) -> ::core::result::Result<
      (),
      $crate::graphql::lossless::GraphqlLosslessError<$lt, $src, $ctx>,
    >
    where
      $src: ::tokora::Source<usize> + ?Sized,
      $crate::graphql::lossless::GraphqlLosslessToken<$lt, $src>: ::tokora::Token<
          $lt,
          Kind = ::smear_lexer::graphql::lossless::LosslessTokenKind,
        > + ::tokora::lexer::FromLogos<$lt>
        + ::core::clone::Clone
        + ::tokora::utils::DowncastRef<::smear_lexer::graphql::ContextualKeyword>,
      $crate::graphql::lossless::GraphqlLosslessLexer<$lt, $src>: ::tokora::Lexer<
          $lt,
          Token = $crate::graphql::lossless::GraphqlLosslessToken<$lt, $src>,
          Span = ::tokora::SimpleSpan,
          Offset = usize,
        >,
      <$crate::graphql::lossless::GraphqlLosslessLexer<$lt, $src> as ::tokora::Lexer<$lt>>::State:
        ::core::clone::Clone,
      $ctx: ::tokora::ParseContext<
        $lt,
        $crate::graphql::lossless::GraphqlLosslessLexer<$lt, $src>,
        $crate::graphql::GraphQL,
      >,
      $ctx::Emitter: ::tokora::emitter::CstEmitter<
        $lt,
        $crate::graphql::lossless::GraphqlLosslessLexer<$lt, $src>,
        $crate::graphql::GraphQL,
      >,
      $crate::graphql::lossless::GraphqlLosslessError<$lt, $src, $ctx>:
        ::core::convert::From<
            ::tokora::error::UnexpectedEot<usize, $crate::graphql::GraphQL>,
          >
          + ::core::convert::From<
            ::tokora::error::token::UnexpectedToken<
              $lt,
              $crate::graphql::lossless::GraphqlLosslessToken<$lt, $src>,
              ::smear_lexer::graphql::lossless::LosslessTokenKind,
              ::tokora::SimpleSpan,
              $crate::graphql::GraphQL,
            >,
          >
          + ::tokora::emitter::FromUnclosed<
            $lt,
            $crate::graphql::lossless::GraphqlLosslessLexer<$lt, $src>,
            $crate::graphql::GraphQL,
          >,
    $body
  )*};
}

/// Declares a production module's `test_support` drivers: `fn(&str) -> Parse`, one per named
/// production, each building a [`Sink`](tokora::cst::Sink), running that one production,
/// draining whatever it left, and materializing.
///
/// # Why every production file needs drivers at all
///
/// [`document`] is Task 3's drain-everything stub until Task 8, so no production below the
/// document level is reachable from [`parse_str`]. Asserting through `parse_str` today would
/// not fail — it would compare two empty trees and pass, which is worse. These drivers make the
/// assertions real now.
///
/// # Why a macro rather than a generic function
///
/// **This is where the productions stop being generic.** A driver must choose a concrete
/// source, emitter and context to build a `Sink` at all, exactly as [`runner::parse_str`] does,
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
/// single-production driver stops at the end of its production by design. The drain also runs
/// on the error path: a production that returns `Err` has committed a prefix and left the rest,
/// and without the drain that would be a panic in the driver instead of a reportable parse.
///
/// # A retro-wrapping production takes a mark, and the driver mints it
///
/// A production whose node is discovered *after* its first child — every definition that may
/// carry a leading [`Description`](crate::graphql::kinds::SyntaxKind::Description), and every
/// type-system extension, whose shape keyword follows the `extend` its node must contain —
/// takes an [`EventMark`](tokora::cst::event::EventMark) and opens with
/// [`node_at`](tokora::parser::node_at) rather than [`node`](tokora::parser::node). The
/// `(mark)` suffix on a driver declares that second argument.
///
/// **The two are equivalent at position zero**, which is what makes such a driver honest:
/// [`node`](tokora::parser::node) mints its own mark at entry and wraps on success, so a mark
/// minted immediately before the call and spent by `node_at` covers exactly the same region.
/// The difference only appears when the *caller* commits something between the two, which is
/// the whole reason the parameter exists.
///
/// # A const-parameterised production takes its flavour in the declaration
///
/// Every production that can reach a value carries a
/// [`Constness`](value::Constness) argument (see `value.rs`'s module docs for why it is threaded
/// rather than duplicated), and a driver is a `fn(&str) -> Parse` with no such parameter — so the
/// flavour is *baked into the driver*, in a `[…]` suffix listing the extra arguments to append
/// after `inp`. Two drivers over one production is how both flavours get an entry point, which
/// is what a test asserting the const rejection needs below the document level.
///
/// `Constness` is imported into the generated module unconditionally and the import is
/// `allow(unused_imports)`, so a call site writes the short spelling and a driver set that names
/// no flavour still compiles clean.
///
/// ```text
/// lossless_drivers! {
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
macro_rules! lossless_drivers {
  (
    $(#[$modmeta:meta])*
    mod $modname:ident;
    $(
      $(#[$meta:meta])*
      fn $name:ident => $production:ident $(($mark:ident))? $([$($extra:expr),+ $(,)?])?;
    )*
  ) => {
    $(#[$modmeta])*
    #[doc(hidden)]
    pub mod $modname {
      use ::tokora::{InputRef, cache::DefaultCache, cst::parse_lossless};

      #[allow(unused_imports)]
      use $crate::graphql::lossless::value::Constness;

      use $crate::graphql::{
        GraphQL,
        lossless::{
          GraphqlLosslessLexer, Parse,
          runner::{LosslessEmitter, LosslessSink, finish_root, profile},
        },
      };

      /// The context pair and the input each driver's closure receives.
      ///
      /// The sink is held **by value**: `parse_lossless` mints it from the source itself and
      /// owns it for the parse, so there is no `&mut Sink` for a driver to hand around.
      type TestCtx<'inp> = (
        LosslessSink<'inp>,
        DefaultCache<'inp, GraphqlLosslessLexer<'inp, str>>,
      );
      type TestInput<'inp, 'input> =
        InputRef<'inp, 'input, GraphqlLosslessLexer<'inp, str>, TestCtx<'inp>, GraphQL>;

      $(
        $(#[$meta])*
        ///
        /// Test-only scaffolding; nothing in the crate calls it.
        pub fn $name<'inp>(src: &'inp str) -> Parse {
          // The `'inp` is **named**, threaded from `src`, for the reason `trivia`'s driver
          // records: elided, it varies independently of the error type and the closure `E0521`s.
          //
          // `Lang` is `parse_lossless`'s SECOND parameter and is used only in bounds, so it is
          // turbofished alongside the lexer or inference settles it on `()`.
          let (cst, _out) = parse_lossless::<GraphqlLosslessLexer<'inp, str>, GraphQL, _, _, _, _>(
            src,
            Default::default(),
            LosslessEmitter::default(),
            profile::<str>(),
            DefaultCache::<GraphqlLosslessLexer<'_, str>>::default(),
            |inp: &mut TestInput<'inp, '_>| {
              // Minted before the call, not inside the argument list: `inp` is already borrowed
              // mutably as the first argument, so a second `inp` method call in the second would
              // be a second mutable borrow of the same value.
              $(let $mark = inp.cst_mark();)?
              // `::<str, _>`: `Src` is not inferable from the input type, and `str` is the
              // parameter that matches `L::Source`.
              let out = super::$production::<str, _>(inp $(, $mark)? $($(, $extra)+)?);
              inp.skip_while(|_| true)?;
              out
            },
          );

          finish_root(cst)
        }
      )*
    }
  };
}

// `lossless_production!` and `lossless_drivers!` reach `value.rs`, `recover.rs`, `ty.rs` and
// `directive.rs` through **textual** macro scope, which is why their definitions sit immediately
// above these `mod` declarations and must stay there: a `macro_rules!` is in scope for a child
// module only if it is declared before that module's `mod` item in this file. A
// `pub(crate) use lossless_production;` re-export beside it is dead — the import would be
// shadowed by the textual binding and warns as unused.
pub mod ast;
pub mod coverage;
pub mod definition;
pub mod directive;
pub mod document;
pub mod executable;
pub mod kind_map;
pub mod recover;
pub mod runner;
pub mod selection;
pub mod trivia;
pub mod ty;
pub mod value;

pub use runner::{Parse, parse_str, profile};
