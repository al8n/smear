use derive_more::{From, Into};
use smear_lexer::tokora::{
  Emitter, InputRef, Lexer, ParseContext, SimpleSpan as Span,
  lexer::FromLogos,
  span::{AsSpan, IntoSpan, Spanned},
  utils::IntoComponents,
};

use smear_lexer::graphqlx::{LitInt, syntactic::SyntacticLexer};

use super::super::*;

type IntValueAlias<S> = crate::value::IntValue<LitInt<S>>;

/// An integer value in GraphQLx.
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

/// Parses an integer value from the input.
pub fn parse_int_value<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<IntValue<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp>,
  SyntacticLexer<'inp, S>:
    Lexer<'inp, Token = SyntacticToken<S>, Span = smear_lexer::tokora::SimpleSpan>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>,
  Lang: ?Sized,
{
  let Spanned { span, data: token } = next_token(input)?;
  match token {
    SyntacticToken::LitInt(val) => Ok(IntValue::new(span, val)),
    tok => Err(Error::unexpected_token(tok, Expectation::IntValue, span).into()),
  }
}
