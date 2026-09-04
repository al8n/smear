//! GraphQL type-reference productions over the concrete syntactic lexer.
//!
//! Consumers can parse through the free [`ty`] parser or
//! [`Type::graphql`]. A fused named-type head avoids re-reading identifiers, while
//! the list branch retains its opener for the existing delimited parser; a trailing
//! `!` is folded into the selected node's `required` flag.
//!
//! See the [GraphQL Type References specification](https://spec.graphql.org/draft/#sec-Type-References).

use smear_lexer::graphql::syntactic::SyntacticTokenKind;
use tokora::{
  Lexer, ParseInput, ParseTokenChoice, SimpleSpan, Slice, Source, TryParseInput, span::Spanned,
  try_parse_input::ParseAttempt,
};

use super::{GraphqlError, GraphqlInput, GraphqlLexer, GraphqlSlice, GraphqlToken};
use crate::{
  combinator::{ParseCtx, TokenSpannedExt, try_bang},
  graphql::{
    GraphQL,
    ast::{ListType, Name, NamedType, Type},
    error::{Expectation, GraphqlError as DialectGraphqlError},
  },
};

enum TypeCore<S> {
  Name(Name<S>),
  List(Type<Name<S>>),
}

fn list_type_core<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<TypeCore<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  ty.delimited_by_brackets()
    .parse_input(inp)
    .map(|delimited| TypeCore::List(delimited.into_data()))
}

/// Parses a committed GraphQL type reference.
///
/// The parser accepts named and recursively nested list types, folding a trailing
/// `!` into the selected node's required flag. An absent or invalid head is an
/// error expecting a type reference.
///
/// See the [GraphQL Type References specification](https://spec.graphql.org/draft/#sec-Type-References).
pub fn ty<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<Type<Name<GraphqlSlice<'inp, Src>>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  (|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
    let named_head_arm =
      |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
       _: &mut GraphqlInput<'inp, '_, Src, Ctx>| match token {
        GraphqlToken::<'inp, Src>::Identifier(source) => {
          Ok(TypeCore::Name(Name::new(span, source)))
        }
        _ => unreachable!("fused type arm received a non-identifier token"),
      };

    match (named_head_arm,)
      .fused_dispatch_on_kind(&[SyntacticTokenKind::Identifier])
      .try_parse_input(inp)?
    {
      ParseAttempt::Accept(core) => Ok(core),
      ParseAttempt::Decline => {
        let offset = *inp.offset();
        // `peek_head_map`, not a raw `peek`: a truncated window and a short document are the
        // same bytes — smear issue #177.
        match inp.peek_head_map(|head| {
          let opens_list = matches!(head.data, GraphqlToken::<'inp, Src>::LBracket);
          (opens_list, head.data.kind(), *head.span)
        })? {
          Some((true, ..)) => {}
          Some((false, kind, span)) => {
            return Err(
              DialectGraphqlError::unexpected_token(kind, Expectation::Type, span).into(),
            );
          }
          None => {
            return Err(
              DialectGraphqlError::maybe_unexpected_token(
                None,
                Expectation::Type,
                SimpleSpan::new(offset, offset),
              )
              .into(),
            );
          }
        }
        list_type_core(inp)
      }
    }
  })
  .then(try_bang)
  .token_spanned()
  .map(
    |Spanned {
       span,
       data: (core, bang),
     }| {
      let required = matches!(bang, ParseAttempt::Accept(_));
      match core {
        TypeCore::Name(name) => Type::Name(NamedType::new(span, name, required)),
        TypeCore::List(inner) => ListType::new(span, inner, required).into(),
      }
    },
  )
  .parse_input(inp)
}

impl<S> Type<Name<S>> {
  /// Parses a committed GraphQL type reference.
  ///
  /// The lexer source is inferred from `inp`. This parser accepts named and
  /// recursively nested list types and folds a trailing `!` into the selected
  /// node's required flag.
  ///
  /// See the [GraphQL Type References specification](https://spec.graphql.org/draft/#sec-Type-References).
  pub fn graphql<'inp, Src, Ctx>(
    inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  ) -> Result<Self, GraphqlError<'inp, Src, Ctx>>
  where
    Src: Source<usize, Slice<'inp> = S> + ?Sized,
    S: Slice<'inp> + Clone + 'inp,
    GraphqlLexer<'inp, Src>:
      Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
    GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<S>>,
  {
    ty(inp)
  }
}

#[cfg(test)]
mod tests;
