use smear_lexer::graphqlx::syntactic::SyntacticLexer;
use tokora::{FatalContext, Lexer, ParseCtx};

use super::*;

fn assert_parse_ctx<'inp, L, Ctx>()
where
  L: Lexer<'inp>,
  Ctx: ParseCtx<'inp, L, GraphQLX>,
{
}

#[test]
fn graphqlx_error_is_parse_ctx_over_str_and_slice() {
  assert_parse_ctx::<
    SyntacticLexer<'_, str>,
    FatalContext<'_, SyntacticLexer<'_, str>, GraphqlxErrors<&str>, GraphQLX>,
  >();
  assert_parse_ctx::<
    SyntacticLexer<'_, [u8]>,
    FatalContext<'_, SyntacticLexer<'_, [u8]>, GraphqlxErrors<&[u8]>, GraphQLX>,
  >();
}
