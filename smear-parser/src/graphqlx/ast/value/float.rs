use derive_more::{From, Into};
use smear_lexer::tokit::{
  lexer::FromLogos,
  Emitter, InputRef, Lexer, ParseContext, SimpleSpan as Span,
  span::{AsSpan, IntoSpan, Spanned},
  utils::IntoComponents,
};

use smear_lexer::graphqlx::{LitFloat, syntactic::SyntacticLexer};

use super::super::*;

type FloatValueAlias<S> = crate::value::FloatValue<LitFloat<S>>;

/// A floating-point value in GraphQLx.
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

/// Parses a float value from the input.
pub fn parse_float_value<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<FloatValue<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = smear_lexer::tokit::SimpleSpan>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>,
  Lang: ?Sized,
{
  let Spanned { span, data: token } = next_token(input)?;
  match token {
    SyntacticToken::LitFloat(val) => Ok(FloatValue::new(span, val)),
    tok => Err(Error::unexpected_token(tok, Expectation::FloatValue, span).into()),
  }
}
