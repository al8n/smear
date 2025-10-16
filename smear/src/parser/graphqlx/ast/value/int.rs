use derive_more::{From, Into};
use logosky::{
  Lexed, Logos, Parseable, Token,
  chumsky::{Parser, extra::ParserExtra, prelude::any},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::lexer::graphqlx::{LitInt, ast::AstLexerErrors};

use super::super::*;

type IntValueAlias<S> = crate::parser::value::IntValue<LitInt<S>>;

#[derive(Debug, Clone, Copy, From, Into)]
pub struct IntValue<S>(IntValueAlias<S>);

impl<S> AsSpan<Span> for IntValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S> IntoSpan<Span> for IntValue<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S> IntoComponents for IntValue<S> {
  type Components = (Span, LitInt<S>);

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<S> IntValue<S> {
  #[inline]
  pub(super) const fn new(span: Span, value: LitInt<S>) -> Self {
    Self(IntValueAlias::new(span, value))
  }

  /// Returns a reference to the span covering the entire integer value.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the literal integer value reference.
  #[inline]
  pub const fn value_ref(&self) -> &LitInt<S> {
    self.0.source_ref()
  }

  /// Returns the integer value.
  #[inline]
  pub const fn value(self) -> LitInt<S>
  where
    S: Copy,
  {
    self.0.source()
  }
}

impl<'a, S>
  Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for IntValue<S>
where
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, SyntacticTokenStream<'a, S>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, SyntacticTokenStream<'a, S>, Error = SyntacticTokenErrors<'a, S>> + 'a,
  {
    any().try_map(|res: Lexed<'_, SyntacticToken<_>>, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        match tok {
          SyntacticToken::LitInt(val) => Ok(Self::new(span, val)),
          tok => Err(Error::unexpected_token(tok, Expectation::IntValue, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
