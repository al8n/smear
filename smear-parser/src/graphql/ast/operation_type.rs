use smear_lexer::{
  keywords::{Mutation, Query, Subscription},
  tokora::{
    Emitter, InputRef, Lexer, ParseContext, lexer::FromLogos, span::Spanned, utils::cmp::Equivalent,
  },
};
use smear_scaffold::ast::OperationType;

use super::{Expectation, SyntacticTokenError, SyntacticTokenErrors, next_token};
use crate::lexer::graphql::syntactic::{SyntacticLexer, SyntacticToken};

/// Parses an operation type from the input.
pub fn parse_operation_type<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<OperationType, SyntacticTokenErrors<S>>
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
      let op = match () {
        () if "query".equivalent(&name) => OperationType::Query(Query::new(span)),
        () if "mutation".equivalent(&name) => OperationType::Mutation(Mutation::new(span)),
        () if "subscription".equivalent(&name) => {
          OperationType::Subscription(Subscription::new(span))
        }
        _ => return Err(SyntacticTokenError::unknown_operation_type(name, span).into()),
      };
      Ok(op)
    }
    tok => Err(SyntacticTokenError::unexpected_token(tok, Expectation::OperationName, span).into()),
  }
}
