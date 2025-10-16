use derive_more::{From, Into};
use logosky::{
  Lexed, Logos, Parseable, Token,
  chumsky::{Parser, extra::ParserExtra, prelude::any},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::lexer::graphqlx::{LitFloat, ast::AstLexerErrors};

use super::super::*;

type FloatValueAlias<S> = crate::parser::value::FloatValue<LitFloat<S>>;

#[derive(Debug, Clone, Copy, From, Into)]
pub struct FloatValue<S>(FloatValueAlias<S>);

impl<S> AsSpan<Span> for FloatValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S> IntoSpan<Span> for FloatValue<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S> IntoComponents for FloatValue<S> {
  type Components = (Span, LitFloat<S>);

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<S> FloatValue<S> {
  #[inline]
  pub(super) const fn new(span: Span, value: LitFloat<S>) -> Self {
    Self(FloatValueAlias::new(span, value))
  }

  /// Returns a reference to the span covering the entire float value.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the literal float value reference.
  #[inline]
  pub const fn value_ref(&self) -> &LitFloat<S> {
    self.0.source_ref()
  }

  /// Returns the float value.
  #[inline]
  pub const fn value(self) -> LitFloat<S>
  where
    S: Copy,
  {
    self.0.source()
  }
}

impl<'a, S>
  Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for FloatValue<S>
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
          SyntacticToken::LitFloat(val) => Ok(Self::new(span, val)),
          tok => Err(Error::unexpected_token(tok, Expectation::FloatValue, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
