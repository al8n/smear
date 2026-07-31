//! The GraphQL lossless parser suite: a `rowan` CST over the trivia-surfacing lexer.

use smear_lexer::graphql::{
  error::LexerErrors,
  lossless::{LosslessLexer, LosslessToken, LosslessTokenKind},
};
use tokora::{
  InputRef, Lexer, SimpleSpan, Source, error::token::UnexpectedToken as TokUnexpectedToken,
  state::tracker::LimitExceeded, utils::Expected,
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
      LosslessTokenKind::Boolean => Expectation::BooleanValue,
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

// `trivia` arrives with Task 4; declaring it ahead of its file would leave the crate unable to
// compile.
pub mod document;
pub mod kind_map;
pub mod runner;

pub use runner::{Parse, parse_str, profile};
