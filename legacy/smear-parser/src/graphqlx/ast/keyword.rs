use smear_lexer::{
  keywords::{self, *},
  tokora::{
    Emitter, InputRef, Lexer, ParseContext, SimpleSpan as Span, lexer::FromLogos, span::Spanned,
    utils::cmp::Equivalent,
  },
};

use super::{Expectation, SyntacticTokenError, SyntacticTokenErrors, next_token};
use crate::lexer::graphqlx::syntactic::{SyntacticLexer, SyntacticToken};

/// Generic keyword parser helper.
fn parse_keyword<'inp, S, Ctx, Lang, T>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
  kw: &'static str,
  make: impl FnOnce(Span) -> T,
) -> Result<T, SyntacticTokenErrors<S>>
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
      if kw.equivalent(&name) {
        Ok(make(span))
      } else {
        Err(SyntacticTokenError::unexpected_keyword(name, kw, span).into())
      }
    }
    tok => Err(SyntacticTokenError::unexpected_token(tok, Expectation::Keyword(kw), span).into()),
  }
}

macro_rules! keyword_parser_fn {
  ($($fn_name:ident -> $name:ty : $kw:literal),+$(,)?) => {
    $(
      paste::paste! {
        /// Parses the keyword from the input.
        pub fn $fn_name<'inp, S, Ctx, Lang>(
          input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
        ) -> Result<$name, SyntacticTokenErrors<S>>
        where
          S: Clone,
          SyntacticToken<S>: FromLogos<'inp>,
          SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = smear_lexer::tokora::SimpleSpan>,
          Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
          Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>,
          str: Equivalent<S>,
          Lang: ?Sized,
        {
          parse_keyword(input, $kw, <$name>::new)
        }
      }
    )*
  };
}

keyword_parser_fn! {
  parse_on -> On : "on",
  parse_fragment_kw -> Fragment : "fragment",
  parse_query_kw -> Query : "query",
  parse_mutation_kw -> Mutation : "mutation",
  parse_subscription_kw -> Subscription : "subscription",
  parse_type_kw -> keywords::Type : "type",
  parse_interface -> Interface : "interface",
  parse_union -> Union : "union",
  parse_enum -> keywords::Enum : "enum",
  parse_input_kw -> keywords::Input : "input",
  parse_implements -> Implements : "implements",
  parse_extend -> Extend : "extend",
  parse_directive_kw -> keywords::Directive : "directive",
  parse_schema -> Schema : "schema",
  parse_scalar -> Scalar : "scalar",
  parse_repeatable -> Repeatable : "repeatable",
  parse_import_kw -> Import : "import",
  parse_from_kw -> From : "from",
  parse_as_kw -> As : "as",
  parse_where_kw -> Where : "where",
  parse_set_kw -> keywords::Set : "set",
  parse_map_kw -> keywords::Map : "map",
}
