//! GraphQL type-reference productions over the concrete syntactic lexer.
//!
//! The recursive dispatcher is private; consumers parse through
//! [`Type::graphql`]. A committed one-token choice selects a named or list type,
//! and a trailing `!` is folded into the selected node's `required` flag.

use std::boxed::Box;

use smear_scaffold::ast as scaffold;
use tokora::{
  Branch, Lexer, ParseChoice, ParseInput, SimpleSpan, Slice, Source, Token,
  cache::{Peeked, PeekedTokenExt},
  error::{UnexpectedEot, token::UnexpectedToken},
  punct::Bracket,
  span::Spanned,
  try_parse_input::ParseAttempt,
  utils::typenum::U1,
};

use super::{GraphqlError, GraphqlInput, GraphqlLexer, GraphqlSlice, GraphqlToken};
use crate::{
  combinator::{ParseCtx, ident, try_bang},
  graphql::{
    GraphQL,
    ast::{Name, Type},
    error::{Expectation, GraphqlError as DialectGraphqlError},
  },
};

enum TypeCore<S> {
  Name(Name<S>),
  List(Type<Name<S>>),
}

fn named_type_core<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<TypeCore<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQL>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlToken<'inp, Src>,
        <GraphqlToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQL,
      >,
    >,
{
  ident(inp).map(TypeCore::Name)
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
  GraphqlError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQL>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlToken<'inp, Src>,
        <GraphqlToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQL,
      >,
    > + From<tokora::error::Unclosed<Bracket, SimpleSpan, GraphQL>>
    + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  ty.delimited_by_brackets()
    .parse_input(inp)
    .map(|delimited| TypeCore::List(delimited.into_data()))
}

fn ty<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<Type<Name<GraphqlSlice<'inp, Src>>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQL>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlToken<'inp, Src>,
        <GraphqlToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQL,
      >,
    > + From<tokora::error::Unclosed<Bracket, SimpleSpan, GraphQL>>
    + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let offset = *inp.offset();
  (named_type_core, list_type_core)
    .peek_then_choice::<_, U1>(|peeked: Peeked<'_, 'inp, GraphqlLexer<'inp, Src>, U1>, _| {
      let Some(head) = peeked.front() else {
        return Err(
          DialectGraphqlError::maybe_unexpected_token(
            None,
            Expectation::Type,
            SimpleSpan::new(offset, offset),
          )
          .into(),
        );
      };

      match head.token() {
        GraphqlToken::<'inp, Src>::Identifier(_) => Ok(Branch::B0),
        GraphqlToken::<'inp, Src>::LBracket => Ok(Branch::B1),
        token => Err(
          DialectGraphqlError::unexpected_token(token.kind(), Expectation::Type, *head.span())
            .into(),
        ),
      }
    })
    .then(try_bang)
    .spanned()
    .map(
      |Spanned {
         span,
         data: (core, bang),
       }| {
        let required = matches!(bang, ParseAttempt::Accept(_));
        match core {
          TypeCore::Name(name) => Type::Name(scaffold::NamedType::new(span, name, required)),
          TypeCore::List(inner) => {
            Type::List(Box::new(scaffold::ListType::new(span, inner, required)))
          }
        }
      },
    )
    .parse_input(inp)
}

impl<S> Type<Name<S>> {
  /// Parses a GraphQL type reference from the concrete syntactic lexer.
  pub fn graphql<'inp, Src, Ctx>(
    inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  ) -> Result<Self, GraphqlError<'inp, Src, Ctx>>
  where
    Src: Source<usize, Slice<'inp> = S> + ?Sized,
    S: Slice<'inp> + Clone + 'inp,
    GraphqlLexer<'inp, Src>:
      Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
    GraphqlError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQL>>
      + From<
        UnexpectedToken<
          'inp,
          GraphqlToken<'inp, Src>,
          <GraphqlToken<'inp, Src> as Token<'inp>>::Kind,
          SimpleSpan,
          GraphQL,
        >,
      > + From<tokora::error::Unclosed<Bracket, SimpleSpan, GraphQL>>
      + From<DialectGraphqlError<S>>,
  {
    ty(inp)
  }
}

#[cfg(test)]
mod tests;
