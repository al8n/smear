//! The GraphQL dialect's grammar productions.
//!
//! Productions in this module are specialized to the concrete
//! [`GraphqlLexer`] and [`GraphQL`] marker. The public aliases below keep their
//! source, token, input, and error signatures consistent while allowing the lexer
//! to run over `str`, `[u8]`, and the source wrappers it supports.
//! GraphQL AST types expose associated `graphql` entry points keyed by their source
//! slice. The lexer source remains a method generic inferred from [`GraphqlInput`].
//!
//! The module name reflects the driving lexer the syntactic suite pairs these
//! productions with; the productions themselves are purely syntactic — lossless/CST
//! structure is a separate `lossless` module's concern (a later wave).

use smear_lexer::graphql::{
  ContextualKeyword,
  syntactic::{SyntacticLexer, SyntacticToken},
};
use tokora::{
  ErrorOf, InputRef, Lexer, ParseContext, SimpleSpan, Slice, Source, try_parse_input::ParseAttempt,
  utils::DowncastRef,
};

use super::GraphQL;
use crate::{
  combinator::{ParseCtx, ident, try_ident},
  graphql::{
    ast,
    error::{Expectation, GraphqlError as DialectGraphqlError},
  },
};

/// The concrete lexer used by GraphQL syntactic productions over `Src`.
#[allow(type_alias_bounds)]
pub type GraphqlLexer<'inp, Src: ?Sized> = SyntacticLexer<'inp, Src>;

/// The source slice emitted by [`GraphqlLexer`] for `Src`.
#[allow(type_alias_bounds)]
pub type GraphqlSlice<'inp, Src: Source<usize> + ?Sized> = <Src as Source<usize>>::Slice<'inp>;

/// The concrete token emitted by [`GraphqlLexer`] for `Src`.
#[allow(type_alias_bounds)]
pub type GraphqlToken<'inp, Src: Source<usize> + ?Sized> = SyntacticToken<GraphqlSlice<'inp, Src>>;

/// The parser error emitted by a GraphQL syntactic production.
#[allow(type_alias_bounds)]
pub type GraphqlError<'inp, Src: ?Sized, Ctx>
where
  GraphqlLexer<'inp, Src>: Lexer<'inp>,
  Ctx: ParseContext<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
= ErrorOf<'inp, GraphqlLexer<'inp, Src>, Ctx, GraphQL>;

/// A mutable GraphQL syntactic parser input over `Src` and parser context `Ctx`.
#[allow(type_alias_bounds)]
pub type GraphqlInput<'inp, 'input, Src: ?Sized, Ctx>
where
  GraphqlLexer<'inp, Src>: Lexer<'inp>,
  Ctx: ParseContext<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
= InputRef<'inp, 'input, GraphqlLexer<'inp, Src>, Ctx, GraphQL>;

pub mod argument;
pub mod definition;
pub mod directive;
pub mod document;
pub mod executable;
pub mod selection;
pub mod ty;
pub mod value;

pub use document::{definition, definition_or_extension, described_definition, document};

// The suite's three document roots, side by side. Each was already `pub` where it is defined —
// [`document::document`], [`definition::type_system_document`],
// [`executable::executable_document`] — but only the mixed one was hoisted here, so the two
// alternates read as internals of the modules they happen to live in. They are the syntactic
// counterparts of `lossless`'s `parse_document`, `parse_type_system_document` and
// `parse_executable_document`, and a consumer choosing a root should find all three in one place
// (smear issue #67).
//
// These are productions, not `fn(&str) -> …` entry points: this layer has no runner of its own, so
// a consumer drives one through `tokora::Parser::with_parser(…).parse_str(src)`. That is why the
// names cannot match the lossless layer's — the shapes do not.
pub use definition::type_system_document;
pub use executable::executable_document;

fn name<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<ast::Name<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
{
  ident(inp).map(ast::Name::from)
}

fn try_name<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<ParseAttempt<ast::Name<GraphqlSlice<'inp, Src>>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
{
  try_ident(inp).map(|attempt| attempt.map(ast::Name::from))
}

/// Parses a GraphQL fragment name (`Name` but not `on`).
///
/// This is deliberately a dialect production instead of a generic atom: the
/// exclusion is a GraphQL grammar rule, and the rejection remains non-consuming
/// so a surrounding production can retain the token for recovery or dispatch.
pub fn fragment_name<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<ast::FragmentName<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let offset = *inp.offset();
  // `peek_head_map`, not a raw `peek`: a truncated window and a short document are the same
  // bytes, so a raw peek reports a scanner stop as an absent token — smear issue #177.
  match inp.peek_head_map(|token| {
    let rejected = matches!(token.data.downcast_ref(), Some(ContextualKeyword::On))
      || !matches!(token.data, GraphqlToken::<'inp, Src>::Identifier(_));
    (rejected, *token.span, token.data.kind())
  })? {
    Some((true, span, kind)) => {
      return Err(
        DialectGraphqlError::unexpected_token(kind, Expectation::FragmentName, span).into(),
      );
    }
    Some((false, ..)) => {}
    None => {
      return Err(
        DialectGraphqlError::maybe_unexpected_token(
          None,
          Expectation::FragmentName,
          SimpleSpan::new(offset, offset),
        )
        .into(),
      );
    }
  }

  // Rebranding, not rebuilding: taking the name apart and handing the pieces to
  // `FragmentName::new` would declare whatever came back valid, which is wrong for any name this
  // production recovered rather than read.
  name(inp).map(ast::FragmentName::from_name)
}

impl<S> ast::Name<S> {
  /// Parses one committed GraphQL name.
  ///
  /// The lexer source is inferred from `inp`.
  ///
  /// See the [GraphQL Names specification](https://spec.graphql.org/draft/#sec-Names).
  pub fn graphql<'inp, Src, Ctx>(
    inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  ) -> Result<Self, GraphqlError<'inp, Src, Ctx>>
  where
    Src: Source<usize, Slice<'inp> = S> + ?Sized,
    S: Slice<'inp> + Clone + 'inp,
    GraphqlLexer<'inp, Src>:
      Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  {
    name(inp)
  }

  /// Attempts one GraphQL name without consuming on a head mismatch.
  ///
  /// The lexer source is inferred from `inp`.
  ///
  /// See the [GraphQL Names specification](https://spec.graphql.org/draft/#sec-Names).
  pub fn try_graphql<'inp, Src, Ctx>(
    inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  ) -> Result<ParseAttempt<Self>, GraphqlError<'inp, Src, Ctx>>
  where
    Src: Source<usize, Slice<'inp> = S> + ?Sized,
    S: Slice<'inp> + Clone + 'inp,
    GraphqlLexer<'inp, Src>:
      Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  {
    try_name(inp)
  }
}

impl<S> ast::FragmentName<S> {
  /// Parses a GraphQL fragment name (`Name` but not `on`).
  ///
  /// The lexer source is inferred from `inp`.
  ///
  /// See the [GraphQL Fragment Name specification](https://spec.graphql.org/draft/#FragmentName).
  pub fn graphql<'inp, Src, Ctx>(
    inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  ) -> Result<Self, GraphqlError<'inp, Src, Ctx>>
  where
    Src: Source<usize, Slice<'inp> = S> + ?Sized,
    S: Slice<'inp> + Clone + 'inp,
    GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
    GraphqlLexer<'inp, Src>:
      Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
    GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<S>>,
  {
    fragment_name(inp)
  }
}

#[cfg(test)]
mod tests;

/// Peeks the next token without consuming it and reports whether it satisfies
/// `pred`. It returns `false` at end of input, and **raises** on a terminal scanner stop.
///
/// Selection and executable productions use this one-token dispatch primitive to
/// choose a committed arm while leaving the token available to that arm.
///
/// # It was an always-declining `try_expect`, and tokora names that hack
///
/// The body ran `try_expect` with a predicate that recorded the answer and then always returned
/// `false`, so the token stayed at the cache front. That reads the head without consuming it and
/// it also answers `false` for a **terminal stop**, because `try_expect`'s contract folds a
/// resource trip into the same `Ok(None)` a genuine end of input produces
/// (`tokora-0.10.0/src/input/input_ref/try_expect.rs:262`). A caller then reads a halted scanner
/// as "no `(` here" and commits to the arm for a construct that is absent — smear issue #177,
/// Codex round 2, on `variables_definition`, which returned a successful **empty**
/// `VariablesDefinition` over a refused input.
///
/// [`head_satisfies`](tokora::InputRef::head_satisfies) is the primitive for exactly this, and its
/// own doc says so: *"Replaces the consumer-side always-decline `try_expect` hack, which answered
/// `false` for both."* It is `peek_head_map` underneath, so `false` is still reserved for a real
/// end of input and the token is still left where the committed arm will find it.
fn peeks_where<'inp, Src, Ctx, F>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  pred: F,
) -> Result<bool, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  F: Fn(&GraphqlToken<'inp, Src>) -> bool,
{
  inp.head_satisfies(pred)
}
