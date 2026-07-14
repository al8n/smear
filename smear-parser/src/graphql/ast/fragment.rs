use smear_lexer::{
  graphql::syntactic::SyntacticLexerErrors,
  tokora::{
    Emitter, InputRef, Lexer, ParseContext, SimpleSpan as Span,
    lexer::FromLogos,
    span::Spanned,
    utils::{IntoComponents, cmp::Equivalent},
  },
};
use smear_scaffold::ast::{self as scaffold, FragmentName};

use super::{Expectation, Name, SyntacticTokenError, SyntacticTokenErrors, next_token};
use crate::lexer::graphql::syntactic::{SyntacticLexer, SyntacticToken};

/// A type condition for a fragment, specifying the type it applies to.
pub type TypeCondition<S> = scaffold::TypeCondition<Name<S>>;

/// Parses a fragment name from the input.
pub fn parse_fragment_name<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<FragmentName<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp>,
  SyntacticLexer<'inp, S>:
    Lexer<'inp, Token = SyntacticToken<S>, Span = smear_lexer::tokora::SimpleSpan>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let Spanned { span, data: token } = next_token(input)?;
  match token {
    SyntacticToken::Identifier(name) => {
      if "on".equivalent(&name) {
        Err(SyntacticTokenError::invalid_fragment_name(name, span).into())
      } else {
        Ok(FragmentName::new(span, name))
      }
    }
    tok => Err(SyntacticTokenError::unexpected_token(tok, Expectation::FragmentName, span).into()),
  }
}

impl<S> From<FragmentName<S>> for Name<S> {
  #[inline]
  fn from(value: FragmentName<S>) -> Self {
    let (span, value) = value.into_components();
    Self::new(span, value)
  }
}
