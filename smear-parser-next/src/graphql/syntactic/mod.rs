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

use smear_lexer::graphql::syntactic::{SyntacticLexer, SyntacticToken};
use tokora::{
  ErrorOf, InputRef, Lexer, ParseContext, SimpleSpan, Slice, Source, Token,
  error::{UnexpectedEot, token::UnexpectedToken},
  try_parse_input::ParseAttempt,
};

use super::GraphQL;
use crate::{
  combinator::{ParseCtx, ident, try_ident},
  graphql::ast,
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
pub mod directive;
pub mod executable;
pub mod selection;
pub mod ty;
pub mod value;

fn name<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<ast::Name<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQL>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlToken<'inp, Src>,
        <GraphqlToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQL,
      >,
    >,
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
  GraphqlError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQL>>,
{
  try_ident(inp).map(|attempt| attempt.map(ast::Name::from))
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
    GraphqlError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQL>>
      + From<
        UnexpectedToken<
          'inp,
          GraphqlToken<'inp, Src>,
          <GraphqlToken<'inp, Src> as Token<'inp>>::Kind,
          SimpleSpan,
          GraphQL,
        >,
      >,
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
    GraphqlError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQL>>,
  {
    try_name(inp)
  }
}

#[cfg(test)]
mod tests;

/// Peeks the next token without consuming it and reports whether it satisfies
/// `pred`. It returns `false` at end of input.
///
/// Selection and executable productions use this one-token dispatch primitive to
/// choose a committed arm while leaving the token available to that arm.
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
  let mut found = false;
  inp.try_expect(|spanned| {
    found = pred(spanned.data);
    false
  })?;
  Ok(found)
}
