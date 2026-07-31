use smear_lexer::tokora::{
  Emitter, InputRef, Lexer, ParseContext, lexer::FromLogos, span::Spanned,
};
use tokora::{SimpleSpan, try_parse_input::ParseAttempt};

use super::{Expectation, SyntacticTokenError, SyntacticTokenErrors};
use crate::lexer::graphql::syntactic::{SyntacticLexer, SyntacticToken};

/// A GraphQL name identifier.
///
/// Represents a valid GraphQL name as defined by the specification. Names are
/// used throughout GraphQL for field names, type names, argument names, directive
/// names, and other identifiers.
///
/// ## Grammar
///
/// ```text
/// Name ::= [_A-Za-z][_0-9A-Za-z]*
/// ```
///
/// Spec: [Name](https://spec.graphql.org/draft/#sec-Names)
pub type Name<V, S = SimpleSpan> = crate::ident::Ident<V, S>;

/// Parses a GraphQL name from the input.
pub fn parse_name<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Name<S, <SyntacticLexer<'inp, S> as Lexer<'inp>>::Span>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = SimpleSpan>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>,
  Lang: ?Sized,
{
  match input.next()? {
    Some(Spanned { span, data: token }) => match token {
      SyntacticToken::Identifier(name) => Ok(Name::new(span, name)),
      tok => Err(SyntacticTokenError::unexpected_token(tok, Expectation::Name, span).into()),
    },
    None => {
      let span = input.span();
      Err(SyntacticTokenError::unexpected_end_of_input(*span).into())
    }
  }
}

/// Parses a GraphQL name from the input.
pub fn try_parse_name<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<
  ParseAttempt<Name<S, <SyntacticLexer<'inp, S> as Lexer<'inp>>::Span>>,
  SyntacticTokenErrors<S>,
>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = SimpleSpan>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>,
  Lang: ?Sized,
{
  input.try_expect(|t| t.data().is_identifier()).map(|val| {
    val
      .map(|Spanned { span, data: token }| Name::new(span, token.unwrap_identifier()))
      .into()
  })
}
