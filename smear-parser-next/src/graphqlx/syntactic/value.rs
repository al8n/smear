//! GraphQLX value productions.
//!
//! Scalar heads are dispatched from one consumed token. `set` and `map` remain
//! ordinary path segments unless their immediately following token is `{`, at
//! which point they become collection constructors.

use std::vec::Vec;
use tokora::{
  Accumulator, Lexer, ParseInput, ParseTokenChoice, SimpleSpan, Slice, Source, Token,
  TryParseInput,
  cache::{Peeked, PeekedTokenExt},
  error::{Unclosed, UnexpectedEot, token::UnexpectedToken},
  parser::Action,
  punct::{Brace, Bracket},
  span::Spanned,
  try_parse_input::ParseAttempt,
  utils::{DowncastRef, typenum::U1},
};

use smear_lexer::graphqlx::{ContextualKeyword, syntactic::SyntacticTokenKind};

use super::{
  GraphqlxError, GraphqlxInput, GraphqlxLexer, GraphqlxSlice, GraphqlxToken, keyword_of, next_is,
  path, path_after_first, unexpected_here,
};
use crate::{
  combinator::{ParseCtx, colon, dollar, fat_arrow},
  graphqlx::{
    GraphQLX,
    ast::{
      BooleanValue, ConstInputValue, ConstList, ConstMap, ConstMapEntry, ConstObject,
      ConstObjectField, ConstSet, EnumValue, FloatValue, InlineStringValue, InputValue, IntValue,
      List, Map, MapEntry, NullValue, Object, ObjectField, Set, StringValue, VariableValue,
    },
    error::{Expectation, GraphqlxError as DialectGraphqlxError},
  },
};

macro_rules! value_parser {
  ($visibility:vis $name:ident, $input:ident, $output:ty, [], $body:block) => {
    value_parser!(@impl $visibility $name, $input, $output, [], $body);
  };
  ($visibility:vis $name:ident, $input:ident, $output:ty, [contextual], $body:block) => {
    value_parser!(
      @impl $visibility $name,
      $input,
      $output,
      [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
      $body
    );
  };
  ($visibility:vis $name:ident, $input:ident, $output:ty, [punctuated], $body:block) => {
    value_parser!(
      @impl $visibility $name,
      $input,
      $output,
      [
        GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
          + tokora::token::PunctuatorToken<'inp>,
        GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLX>>
          + From<
            UnexpectedToken<
              'inp,
              GraphqlxToken<'inp, Src>,
              <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
              SimpleSpan,
              GraphQLX,
            >,
          >,
      ],
      $body
    );
  };
  ($visibility:vis $name:ident, $input:ident, $output:ty, [path], $body:block) => {
    value_parser!(
      @impl $visibility $name,
      $input,
      $output,
      [
        GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
          + tokora::token::PunctuatorToken<'inp>
          + DowncastRef<ContextualKeyword>,
        GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLX>>
          + From<
            UnexpectedToken<
              'inp,
              GraphqlxToken<'inp, Src>,
              <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
              SimpleSpan,
              GraphQLX,
            >,
          >,
      ],
      $body
    );
  };
  ($visibility:vis $name:ident, $input:ident, $output:ty, [complex], $body:block) => {
    value_parser!(
      @impl $visibility $name,
      $input,
      $output,
      [
        GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
          + tokora::token::PunctuatorToken<'inp>
          + DowncastRef<ContextualKeyword>,
        GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLX>>
          + From<
            UnexpectedToken<
              'inp,
              GraphqlxToken<'inp, Src>,
              <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
              SimpleSpan,
              GraphQLX,
            >,
          > + From<Unclosed<Bracket, SimpleSpan, GraphQLX>>
          + From<Unclosed<Brace, SimpleSpan, GraphQLX>>,
      ],
      $body
    );
  };
  (
    @impl $visibility:vis $name:ident,
    $input:ident,
    $output:ty,
    [$($bounds:tt)*],
    $body:block
  ) => {
    #[doc = "Parses this committed GraphQLX value production."]
    $visibility fn $name<'inp, Src, Ctx>(
      $input: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
    ) -> Result<$output, GraphqlxError<'inp, Src, Ctx>>
    where
      Src: Source<usize> + ?Sized,
      GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
      GraphqlxLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlxToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
      >,
      $($bounds)*
      Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLX>,
      GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
    $body
  };
}

fn unexpected_after_consumed<'inp, Src, Ctx, Output>(
  token: GraphqlxToken<'inp, Src>,
  span: SimpleSpan,
  expected: Expectation,
) -> Result<Output, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLX>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  Err(DialectGraphqlxError::unexpected_token(token.kind(), expected, span).into())
}

value_parser!(int_value, inp, IntValue<GraphqlxSlice<'inp, Src>>, [], {
  match inp.next()? {
    Some(Spanned {
      span,
      data: GraphqlxToken::<'inp, Src>::LitInt(value),
    }) => Ok(IntValue::new(span, value)),
    Some(Spanned { span, data: token }) => {
      unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::IntValue)
    }
    None => {
      unexpected_here(inp, Expectation::IntValue)?;
      unreachable!("unexpected_here always returns an error")
    }
  }
});

value_parser!(
  float_value,
  inp,
  FloatValue<GraphqlxSlice<'inp, Src>>,
  [],
  {
    match inp.next()? {
      Some(Spanned {
        span,
        data: GraphqlxToken::<'inp, Src>::LitFloat(value),
      }) => Ok(FloatValue::new(span, value)),
      Some(Spanned { span, data: token }) => {
        unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::FloatValue)
      }
      None => {
        unexpected_here(inp, Expectation::FloatValue)?;
        unreachable!("unexpected_here always returns an error")
      }
    }
  }
);

value_parser!(pub string_value, inp, StringValue<GraphqlxSlice<'inp, Src>>, [], {
  match inp.next()? {
    Some(Spanned {
      span,
      data: GraphqlxToken::<'inp, Src>::LitInlineStr(value),
    }) => Ok(StringValue::new(span, value.into())),
    Some(Spanned {
      span,
      data: GraphqlxToken::<'inp, Src>::LitBlockStr(value),
    }) => Ok(StringValue::new(span, value.into())),
    Some(Spanned { span, data: token }) => {
      unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::StringValue)
    }
    None => {
      unexpected_here(inp, Expectation::StringValue)?;
      unreachable!("unexpected_here always returns an error")
    }
  }
});

pub(crate) fn inline_string_value<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<InlineStringValue<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLX>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match inp.next()? {
    Some(Spanned {
      span,
      data: GraphqlxToken::<'inp, Src>::LitInlineStr(value),
    }) => Ok(InlineStringValue::new(span, value)),
    Some(Spanned { span, data: token }) => {
      unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::InlineString)
    }
    None => {
      unexpected_here(inp, Expectation::InlineString)?;
      unreachable!("unexpected_here always returns an error")
    }
  }
}

value_parser!(
  boolean_value,
  inp,
  BooleanValue<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    match inp.next()? {
      Some(Spanned { span, data: token }) => match keyword_of(&token) {
        Some(ContextualKeyword::True) => Ok(BooleanValue::new(span, true)),
        Some(ContextualKeyword::False) => Ok(BooleanValue::new(span, false)),
        _ => unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::BooleanValue),
      },
      None => {
        unexpected_here(inp, Expectation::BooleanValue)?;
        unreachable!("unexpected_here always returns an error")
      }
    }
  }
);

value_parser!(
  null_value,
  inp,
  NullValue<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    match inp.next()? {
      Some(Spanned { span, data: token }) => match keyword_of(&token) {
        Some(ContextualKeyword::Null) => match token {
          GraphqlxToken::<'inp, Src>::Identifier(value) => Ok(NullValue::new(span, value)),
          _ => unreachable!("the contextual GraphQLX null keyword is always an identifier"),
        },
        _ => unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::NullValue),
      },
      None => {
        unexpected_here(inp, Expectation::NullValue)?;
        unreachable!("unexpected_here always returns an error")
      }
    }
  }
);

fn enum_after_first<'inp, Src, Ctx>(
  start: usize,
  first: super::ast::Name<GraphqlxSlice<'inp, Src>>,
  fully_qualified: bool,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<EnumValue<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>:
    Token<'inp, Kind = SyntacticTokenKind> + tokora::token::PunctuatorToken<'inp>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLX>,
  GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLX>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlxToken<'inp, Src>,
        <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQLX,
      >,
    >,
{
  path_after_first(start, first, fully_qualified, inp)
    .map(|path| EnumValue::new(*path.span(), path))
}

value_parser!(pub enum_value, inp, EnumValue<GraphqlxSlice<'inp, Src>>, [path], {
  let identifier_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
      let keyword = keyword_of(&token);
      match token {
        GraphqlxToken::<'inp, Src>::Identifier(source) => {
          if matches!(
            keyword,
            Some(ContextualKeyword::True | ContextualKeyword::False | ContextualKeyword::Null)
          ) {
            unexpected_after_consumed::<Src, Ctx, _>(
              GraphqlxToken::<'inp, Src>::Identifier(source),
              span,
              Expectation::EnumValue,
            )
          } else {
            enum_after_first(span.start(), super::ast::Name::new(span, source), false, inp)
          }
        }
        _ => unreachable!("fused GraphQLX enum dispatch received a non-identifier token"),
      }
    };

  match (identifier_head,)
    .fused_dispatch_on_kind(&[SyntacticTokenKind::Identifier])
    .try_parse_input(inp)?
  {
    ParseAttempt::Accept(value) => Ok(value),
    ParseAttempt::Decline => match super::peek_kind(inp)? {
      Some(SyntacticTokenKind::PathSeparator) => {
        path(inp).map(|path| EnumValue::new(*path.span(), path))
      }
      _ => {
        unexpected_here(inp, Expectation::EnumValue)?;
        unreachable!("unexpected_here always returns an error")
      }
    },
  }
});

value_parser!(
  variable_value,
  inp,
  VariableValue<GraphqlxSlice<'inp, Src>>,
  [punctuated],
  {
    dollar
      .then(|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
        let offset = *inp.offset();
        (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| super::name(inp))
          .peek_then::<_, U1>(
            |mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>, _| match peeked.pop_front()
            {
              Some(token) if matches!(token.token(), GraphqlxToken::<'inp, Src>::Identifier(_)) => {
                Ok(())
              }
              Some(token) => Err(
                DialectGraphqlxError::unexpected_token(
                  token.token().kind(),
                  Expectation::VariableValue,
                  *token.span(),
                )
                .into(),
              ),
              None => Err(
                DialectGraphqlxError::maybe_unexpected_token(
                  None,
                  Expectation::VariableValue,
                  SimpleSpan::new(offset, offset),
                )
                .into(),
              ),
            },
          )
          .parse_input(inp)
      })
      .spanned()
      .map(
        |Spanned {
           span,
           data: (_, name),
         }| VariableValue::new(span, name),
      )
      .parse_input(inp)
  }
);

fn variable_after_dollar<'inp, Src, Ctx>(
  start: usize,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<VariableValue<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>:
    Token<'inp, Kind = SyntacticTokenKind> + tokora::token::PunctuatorToken<'inp>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLX>,
  GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLX>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlxToken<'inp, Src>,
        <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQLX,
      >,
    >,
{
  let name = super::name(inp)?;
  Ok(VariableValue::new(
    SimpleSpan::new(start, name.span().end()),
    name,
  ))
}

#[derive(Copy, Clone)]
pub(crate) enum ValueHead {
  Int,
  Float,
  String,
  Identifier,
  Dollar,
  List,
  Object,
  Path,
}

/// Returns the deterministic FIRST-set category for a GraphQLX value token.
#[inline]
pub(crate) fn value_head<S>(
  token: &smear_lexer::graphqlx::syntactic::SyntacticToken<S>,
) -> Option<ValueHead> {
  Some(match token {
    smear_lexer::graphqlx::syntactic::SyntacticToken::LitInt(_) => ValueHead::Int,
    smear_lexer::graphqlx::syntactic::SyntacticToken::LitFloat(_) => ValueHead::Float,
    smear_lexer::graphqlx::syntactic::SyntacticToken::LitInlineStr(_)
    | smear_lexer::graphqlx::syntactic::SyntacticToken::LitBlockStr(_) => ValueHead::String,
    smear_lexer::graphqlx::syntactic::SyntacticToken::Identifier(_) => ValueHead::Identifier,
    smear_lexer::graphqlx::syntactic::SyntacticToken::Dollar => ValueHead::Dollar,
    smear_lexer::graphqlx::syntactic::SyntacticToken::LBracket => ValueHead::List,
    smear_lexer::graphqlx::syntactic::SyntacticToken::LBrace => ValueHead::Object,
    smear_lexer::graphqlx::syntactic::SyntacticToken::PathSeparator => ValueHead::Path,
    _ => return None,
  })
}

fn const_value_head<S>(token: &smear_lexer::graphqlx::syntactic::SyntacticToken<S>) -> bool {
  !matches!(value_head(token), Some(ValueHead::Dollar) | None)
}

fn decide_value_head<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>,
  _: &mut Ctx::Emitter,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLX>,
{
  Ok(match peeked.pop_front() {
    Some(token) if value_head(token.token()).is_some() => Action::Continue,
    _ => Action::Stop,
  })
}

fn decide_const_value_head<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>,
  _: &mut Ctx::Emitter,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLX>,
{
  Ok(match peeked.pop_front() {
    Some(token) if const_value_head(token.token()) => Action::Continue,
    _ => Action::Stop,
  })
}

fn decide_brace_member<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>,
  _: &mut Ctx::Emitter,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLX>,
{
  Ok(match peeked.pop_front() {
    Some(token) if matches!(token.token(), GraphqlxToken::<'inp, Src>::RBrace) => Action::Stop,
    Some(_) => Action::Continue,
    None => Action::Stop,
  })
}

fn guard_value_phase<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  expected: Expectation,
  mut accepts: impl FnMut(&GraphqlxToken<'inp, Src>) -> bool,
) -> Result<(), GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLX>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let offset = *inp.offset();
  let rejected = {
    let mut peeked = inp.peek::<U1>()?;
    match peeked.pop_front() {
      Some(token) if accepts(token.token()) => return Ok(()),
      Some(token) => Some((*token.span(), token.token().kind())),
      None => None,
    }
  };

  match rejected {
    Some((span, kind)) => Err(DialectGraphqlxError::unexpected_token(kind, expected, span).into()),
    None => Err(
      DialectGraphqlxError::maybe_unexpected_token(None, expected, SimpleSpan::new(offset, offset))
        .into(),
    ),
  }
}

value_parser!(pub list_value, inp, List<GraphqlxSlice<'inp, Src>>, [complex], {
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| value(inp))
    .repeated_while::<_, U1>(decide_value_head::<_, Ctx>)
    .delimited_by_brackets()
    .collect_with(Vec::new())
    .spanned()
    .parse_input(inp)
    .map(|Spanned { span, data }| List::new(span, data))
});

value_parser!(pub const_list_value, inp, ConstList<GraphqlxSlice<'inp, Src>>, [complex], {
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| const_value(inp))
    .repeated_while::<_, U1>(decide_const_value_head::<_, Ctx>)
    .delimited_by_brackets()
    .collect_with(Vec::new())
    .spanned()
    .parse_input(inp)
    .map(|Spanned { span, data }| ConstList::new(span, data))
});

value_parser!(pub object_field, inp, ObjectField<GraphqlxSlice<'inp, Src>>, [complex], {
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
    guard_value_phase(inp, Expectation::Name, |token| {
      matches!(token, GraphqlxToken::<'inp, Src>::Identifier(_))
    })?;
    super::name(inp)
  })
    .then_ignore(|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
      guard_value_phase(inp, Expectation::Colon, |token| {
        matches!(token, GraphqlxToken::<'inp, Src>::Colon)
      })?;
      colon(inp)
    })
    .then(|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
      guard_value_phase(inp, Expectation::InputValue, |token| value_head(token).is_some())?;
      value(inp)
    })
    .spanned()
    .map(|Spanned { span, data: (name, value) }| ObjectField::new(span, name, value))
    .parse_input(inp)
});

value_parser!(pub const_object_field, inp, ConstObjectField<GraphqlxSlice<'inp, Src>>, [complex], {
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
    guard_value_phase(inp, Expectation::Name, |token| {
      matches!(token, GraphqlxToken::<'inp, Src>::Identifier(_))
    })?;
    super::name(inp)
  })
    .then_ignore(|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
      guard_value_phase(inp, Expectation::Colon, |token| {
        matches!(token, GraphqlxToken::<'inp, Src>::Colon)
      })?;
      colon(inp)
    })
    .then(|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
      guard_value_phase(inp, Expectation::ConstInputValue, const_value_head)?;
      const_value(inp)
    })
    .spanned()
    .map(|Spanned { span, data: (name, value) }| ConstObjectField::new(span, name, value))
    .parse_input(inp)
});

value_parser!(pub object_value, inp, Object<GraphqlxSlice<'inp, Src>>, [complex], {
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| object_field(inp))
    .repeated_while::<_, U1>(decide_brace_member::<_, Ctx>)
    .delimited_by_braces()
    .collect_with(Vec::new())
    .spanned()
    .parse_input(inp)
    .map(|Spanned { span, data }| Object::new(span, data))
});

value_parser!(pub const_object_value, inp, ConstObject<GraphqlxSlice<'inp, Src>>, [complex], {
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| const_object_field(inp))
    .repeated_while::<_, U1>(decide_brace_member::<_, Ctx>)
    .delimited_by_braces()
    .collect_with(Vec::new())
    .spanned()
    .parse_input(inp)
    .map(|Spanned { span, data }| ConstObject::new(span, data))
});

value_parser!(pub map_entry, inp, MapEntry<GraphqlxSlice<'inp, Src>>, [complex], {
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
    guard_value_phase(inp, Expectation::InputValue, |token| value_head(token).is_some())?;
    value(inp)
  })
    .then_ignore(|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
      guard_value_phase(inp, Expectation::FatArrow, |token| {
        matches!(token, GraphqlxToken::<'inp, Src>::FatArrow)
      })?;
      fat_arrow(inp)
    })
    .then(|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
      guard_value_phase(inp, Expectation::InputValue, |token| value_head(token).is_some())?;
      value(inp)
    })
    .spanned()
    .map(
      |Spanned {
         span,
         data: (key, value),
       }| MapEntry::new(span, key, value),
    )
    .parse_input(inp)
});

value_parser!(
  pub const_map_entry,
  inp,
  ConstMapEntry<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
    (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
      guard_value_phase(inp, Expectation::ConstInputValue, const_value_head)?;
      const_value(inp)
    })
      .then_ignore(|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
        guard_value_phase(inp, Expectation::FatArrow, |token| {
          matches!(token, GraphqlxToken::<'inp, Src>::FatArrow)
        })?;
        fat_arrow(inp)
      })
      .then(|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
        guard_value_phase(inp, Expectation::ConstInputValue, const_value_head)?;
        const_value(inp)
      })
      .spanned()
      .map(
        |Spanned {
           span,
           data: (key, value),
         }| ConstMapEntry::new(span, key, value),
      )
      .parse_input(inp)
  }
);

fn set_after_keyword<'inp, Src, Ctx>(
  start: usize,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Set<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
    + tokora::token::PunctuatorToken<'inp>
    + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLX>,
  GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLX>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlxToken<'inp, Src>,
        <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQLX,
      >,
    > + From<Unclosed<Bracket, SimpleSpan, GraphQLX>>
    + From<Unclosed<Brace, SimpleSpan, GraphQLX>>
    + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| value(inp))
    .repeated_while::<_, U1>(decide_brace_member::<_, Ctx>)
    .delimited_by_braces()
    .collect_with(Vec::new())
    .spanned()
    .parse_input(inp)
    .map(|Spanned { span, data }| Set::new(SimpleSpan::new(start, span.end()), data))
}

fn const_set_after_keyword<'inp, Src, Ctx>(
  start: usize,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ConstSet<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
    + tokora::token::PunctuatorToken<'inp>
    + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLX>,
  GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLX>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlxToken<'inp, Src>,
        <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQLX,
      >,
    > + From<Unclosed<Bracket, SimpleSpan, GraphQLX>>
    + From<Unclosed<Brace, SimpleSpan, GraphQLX>>
    + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| const_value(inp))
    .repeated_while::<_, U1>(decide_brace_member::<_, Ctx>)
    .delimited_by_braces()
    .collect_with(Vec::new())
    .spanned()
    .parse_input(inp)
    .map(|Spanned { span, data }| ConstSet::new(SimpleSpan::new(start, span.end()), data))
}

fn map_after_keyword<'inp, Src, Ctx>(
  start: usize,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Map<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
    + tokora::token::PunctuatorToken<'inp>
    + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLX>,
  GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLX>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlxToken<'inp, Src>,
        <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQLX,
      >,
    > + From<Unclosed<Bracket, SimpleSpan, GraphQLX>>
    + From<Unclosed<Brace, SimpleSpan, GraphQLX>>
    + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| map_entry(inp))
    .repeated_while::<_, U1>(decide_brace_member::<_, Ctx>)
    .delimited_by_braces()
    .collect_with(Vec::new())
    .spanned()
    .parse_input(inp)
    .map(|Spanned { span, data }| Map::new(SimpleSpan::new(start, span.end()), data))
}

fn const_map_after_keyword<'inp, Src, Ctx>(
  start: usize,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ConstMap<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
    + tokora::token::PunctuatorToken<'inp>
    + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLX>,
  GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLX>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlxToken<'inp, Src>,
        <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQLX,
      >,
    > + From<Unclosed<Bracket, SimpleSpan, GraphQLX>>
    + From<Unclosed<Brace, SimpleSpan, GraphQLX>>
    + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| const_map_entry(inp))
    .repeated_while::<_, U1>(decide_brace_member::<_, Ctx>)
    .delimited_by_braces()
    .collect_with(Vec::new())
    .spanned()
    .parse_input(inp)
    .map(|Spanned { span, data }| ConstMap::new(SimpleSpan::new(start, span.end()), data))
}

value_parser!(pub value, inp, InputValue<GraphqlxSlice<'inp, Src>>, [complex], {
  let int_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     _: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::LitInt(value) => Ok(InputValue::Int(IntValue::new(span, value))),
      _ => unreachable!("fused GraphQLX value dispatch received a non-integer token"),
    };
  let float_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     _: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::LitFloat(value) => Ok(InputValue::Float(FloatValue::new(span, value))),
      _ => unreachable!("fused GraphQLX value dispatch received a non-float token"),
    };
  let inline_string_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     _: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::LitInlineStr(value) => Ok(InputValue::String(StringValue::new(span, value.into()))),
      _ => unreachable!("fused GraphQLX value dispatch received a non-inline-string token"),
    };
  let block_string_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     _: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::LitBlockStr(value) => Ok(InputValue::String(StringValue::new(span, value.into()))),
      _ => unreachable!("fused GraphQLX value dispatch received a non-block-string token"),
    };
  let identifier_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
      let keyword = keyword_of(&token);
      match (token, keyword) {
        (GraphqlxToken::<'inp, Src>::Identifier(_), Some(ContextualKeyword::True)) => {
          Ok(InputValue::Boolean(BooleanValue::new(span, true)))
        }
        (GraphqlxToken::<'inp, Src>::Identifier(_), Some(ContextualKeyword::False)) => {
          Ok(InputValue::Boolean(BooleanValue::new(span, false)))
        }
        (GraphqlxToken::<'inp, Src>::Identifier(source), Some(ContextualKeyword::Null)) => {
          Ok(InputValue::Null(NullValue::new(span, source)))
        }
        (GraphqlxToken::<'inp, Src>::Identifier(source), Some(ContextualKeyword::Set)) => {
          if next_is(inp, |token| matches!(token, GraphqlxToken::<'inp, Src>::LBrace))? {
            set_after_keyword(span.start(), inp).map(InputValue::Set)
          } else {
            enum_after_first(span.start(), super::ast::Name::new(span, source), false, inp)
              .map(InputValue::Enum)
          }
        }
        (GraphqlxToken::<'inp, Src>::Identifier(source), Some(ContextualKeyword::Map)) => {
          if next_is(inp, |token| matches!(token, GraphqlxToken::<'inp, Src>::LBrace))? {
            map_after_keyword(span.start(), inp).map(InputValue::Map)
          } else {
            enum_after_first(span.start(), super::ast::Name::new(span, source), false, inp)
              .map(InputValue::Enum)
          }
        }
        (GraphqlxToken::<'inp, Src>::Identifier(source), _) => {
          enum_after_first(span.start(), super::ast::Name::new(span, source), false, inp)
            .map(InputValue::Enum)
        }
        _ => unreachable!("fused GraphQLX value dispatch received a non-identifier token"),
      }
    };
  let dollar_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::Dollar => variable_after_dollar(span.start(), inp).map(InputValue::Variable),
      _ => unreachable!("fused GraphQLX value dispatch received a non-dollar token"),
    };

  match (
    int_head,
    float_head,
    inline_string_head,
    block_string_head,
    identifier_head,
    dollar_head,
  )
    .fused_dispatch_on_kind(&[
      SyntacticTokenKind::Int,
      SyntacticTokenKind::Float,
      SyntacticTokenKind::InlineString,
      SyntacticTokenKind::BlockString,
      SyntacticTokenKind::Identifier,
      SyntacticTokenKind::Dollar,
    ])
    .try_parse_input(inp)?
  {
    ParseAttempt::Accept(value) => Ok(value),
    ParseAttempt::Decline => match super::peek_kind(inp)? {
      Some(SyntacticTokenKind::LBracket) => list_value(inp).map(InputValue::List),
      Some(SyntacticTokenKind::LBrace) => object_value(inp).map(InputValue::Object),
      Some(SyntacticTokenKind::PathSeparator) => {
        path(inp).map(|path| InputValue::Enum(EnumValue::new(*path.span(), path)))
      }
      _ => {
        unexpected_here(inp, Expectation::InputValue)?;
        unreachable!("unexpected_here always returns an error")
      }
    },
  }
});

value_parser!(pub const_value, inp, ConstInputValue<GraphqlxSlice<'inp, Src>>, [complex], {
  let int_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     _: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::LitInt(value) => Ok(ConstInputValue::Int(IntValue::new(span, value))),
      _ => unreachable!("fused GraphQLX const-value dispatch received a non-integer token"),
    };
  let float_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     _: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::LitFloat(value) => Ok(ConstInputValue::Float(FloatValue::new(span, value))),
      _ => unreachable!("fused GraphQLX const-value dispatch received a non-float token"),
    };
  let inline_string_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     _: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::LitInlineStr(value) => Ok(ConstInputValue::String(StringValue::new(span, value.into()))),
      _ => unreachable!("fused GraphQLX const-value dispatch received a non-inline-string token"),
    };
  let block_string_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     _: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::LitBlockStr(value) => Ok(ConstInputValue::String(StringValue::new(span, value.into()))),
      _ => unreachable!("fused GraphQLX const-value dispatch received a non-block-string token"),
    };
  let identifier_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
      let keyword = keyword_of(&token);
      match (token, keyword) {
        (GraphqlxToken::<'inp, Src>::Identifier(_), Some(ContextualKeyword::True)) => {
          Ok(ConstInputValue::Boolean(BooleanValue::new(span, true)))
        }
        (GraphqlxToken::<'inp, Src>::Identifier(_), Some(ContextualKeyword::False)) => {
          Ok(ConstInputValue::Boolean(BooleanValue::new(span, false)))
        }
        (GraphqlxToken::<'inp, Src>::Identifier(source), Some(ContextualKeyword::Null)) => {
          Ok(ConstInputValue::Null(NullValue::new(span, source)))
        }
        (GraphqlxToken::<'inp, Src>::Identifier(source), Some(ContextualKeyword::Set)) => {
          if next_is(inp, |token| matches!(token, GraphqlxToken::<'inp, Src>::LBrace))? {
            const_set_after_keyword(span.start(), inp).map(ConstInputValue::Set)
          } else {
            enum_after_first(span.start(), super::ast::Name::new(span, source), false, inp)
              .map(ConstInputValue::Enum)
          }
        }
        (GraphqlxToken::<'inp, Src>::Identifier(source), Some(ContextualKeyword::Map)) => {
          if next_is(inp, |token| matches!(token, GraphqlxToken::<'inp, Src>::LBrace))? {
            const_map_after_keyword(span.start(), inp).map(ConstInputValue::Map)
          } else {
            enum_after_first(span.start(), super::ast::Name::new(span, source), false, inp)
              .map(ConstInputValue::Enum)
          }
        }
        (GraphqlxToken::<'inp, Src>::Identifier(source), _) => {
          enum_after_first(span.start(), super::ast::Name::new(span, source), false, inp)
            .map(ConstInputValue::Enum)
        }
        _ => unreachable!("fused GraphQLX const-value dispatch received a non-identifier token"),
      }
    };

  match (int_head, float_head, inline_string_head, block_string_head, identifier_head)
    .fused_dispatch_on_kind(&[
      SyntacticTokenKind::Int,
      SyntacticTokenKind::Float,
      SyntacticTokenKind::InlineString,
      SyntacticTokenKind::BlockString,
      SyntacticTokenKind::Identifier,
    ])
    .try_parse_input(inp)?
  {
    ParseAttempt::Accept(value) => Ok(value),
    ParseAttempt::Decline => match super::peek_kind(inp)? {
      Some(SyntacticTokenKind::LBracket) => const_list_value(inp).map(ConstInputValue::List),
      Some(SyntacticTokenKind::LBrace) => const_object_value(inp).map(ConstInputValue::Object),
      Some(SyntacticTokenKind::PathSeparator) => {
        path(inp).map(|path| ConstInputValue::Enum(EnumValue::new(*path.span(), path)))
      }
      _ => {
        unexpected_here(inp, Expectation::ConstInputValue)?;
        unreachable!("unexpected_here always returns an error")
      }
    },
  }
});

macro_rules! graphqlx_value_api {
  ($node:ty, $parse:ident, []) => {
    graphqlx_value_api!(@impl $node, $parse, []);
  };
  ($node:ty, $parse:ident, [contextual]) => {
    graphqlx_value_api!(
      @impl $node,
      $parse,
      [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
    );
  };
  ($node:ty, $parse:ident, [punctuated]) => {
    graphqlx_value_api!(
      @impl $node,
      $parse,
      [
        GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
          + tokora::token::PunctuatorToken<'inp>,
        GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLX>>
          + From<
            UnexpectedToken<
              'inp,
              GraphqlxToken<'inp, Src>,
              <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
              SimpleSpan,
              GraphQLX,
            >,
          >,
      ]
    );
  };
  ($node:ty, $parse:ident, [path]) => {
    graphqlx_value_api!(
      @impl $node,
      $parse,
      [
        GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
          + tokora::token::PunctuatorToken<'inp>
          + DowncastRef<ContextualKeyword>,
        GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLX>>
          + From<
            UnexpectedToken<
              'inp,
              GraphqlxToken<'inp, Src>,
              <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
              SimpleSpan,
              GraphQLX,
            >,
          >,
      ]
    );
  };
  ($node:ty, $parse:ident, [complex]) => {
    graphqlx_value_api!(
      @impl $node,
      $parse,
      [
        GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
          + tokora::token::PunctuatorToken<'inp>
          + DowncastRef<ContextualKeyword>,
        GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLX>>
          + From<
            UnexpectedToken<
              'inp,
              GraphqlxToken<'inp, Src>,
              <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
              SimpleSpan,
              GraphQLX,
            >,
          > + From<Unclosed<Bracket, SimpleSpan, GraphQLX>>
          + From<Unclosed<Brace, SimpleSpan, GraphQLX>>,
      ]
    );
  };
  (@impl $node:ty, $parse:ident, [$($bounds:tt)*]) => {
    impl<S> $node {
      /// Parses this committed GraphQLX value production.
      pub fn graphqlx<'inp, Src, Ctx>(
        inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
      ) -> Result<Self, GraphqlxError<'inp, Src, Ctx>>
      where
        Src: Source<usize, Slice<'inp> = S> + ?Sized,
        S: Slice<'inp> + Clone + 'inp,
        GraphqlxLexer<'inp, Src>: Lexer<
            'inp,
            Source = Src,
            Token = GraphqlxToken<'inp, Src>,
            Span = SimpleSpan,
            Offset = usize,
          >,
        $($bounds)*
        Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLX>,
        GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<S>>,
      {
        $parse(inp)
      }
    }
  };
}

graphqlx_value_api!(IntValue<S>, int_value, []);
graphqlx_value_api!(FloatValue<S>, float_value, []);
graphqlx_value_api!(StringValue<S>, string_value, []);
graphqlx_value_api!(BooleanValue<S>, boolean_value, [contextual]);
graphqlx_value_api!(NullValue<S>, null_value, [contextual]);
graphqlx_value_api!(EnumValue<S>, enum_value, [path]);
graphqlx_value_api!(VariableValue<S>, variable_value, [punctuated]);
graphqlx_value_api!(List<S>, list_value, [complex]);
graphqlx_value_api!(ConstList<S>, const_list_value, [complex]);
graphqlx_value_api!(Set<S>, set_value, [complex]);
graphqlx_value_api!(ConstSet<S>, const_set_value, [complex]);
graphqlx_value_api!(MapEntry<S>, map_entry, [complex]);
graphqlx_value_api!(ConstMapEntry<S>, const_map_entry, [complex]);
graphqlx_value_api!(Map<S>, map_value, [complex]);
graphqlx_value_api!(ConstMap<S>, const_map_value, [complex]);
graphqlx_value_api!(ObjectField<S>, object_field, [complex]);
graphqlx_value_api!(ConstObjectField<S>, const_object_field, [complex]);
graphqlx_value_api!(Object<S>, object_value, [complex]);
graphqlx_value_api!(ConstObject<S>, const_object_value, [complex]);
graphqlx_value_api!(InputValue<S>, value, [complex]);
graphqlx_value_api!(ConstInputValue<S>, const_value, [complex]);

value_parser!(pub set_value, inp, Set<GraphqlxSlice<'inp, Src>>, [complex], {
  match inp.next()? {
    Some(Spanned { span, data: token }) if keyword_of(&token) == Some(ContextualKeyword::Set) => {
      guard_value_phase(inp, Expectation::LBrace, |token| {
        matches!(token, GraphqlxToken::<'inp, Src>::LBrace)
      })?;
      set_after_keyword(span.start(), inp)
    }
    Some(Spanned { span, data: token }) => {
      unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::Keyword("set"))
    }
    None => {
      unexpected_here(inp, Expectation::Keyword("set"))?;
      unreachable!("unexpected_here always returns an error")
    }
  }
});

value_parser!(
  pub const_set_value,
  inp,
  ConstSet<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
    match inp.next()? {
      Some(Spanned { span, data: token }) if keyword_of(&token) == Some(ContextualKeyword::Set) => {
        guard_value_phase(inp, Expectation::LBrace, |token| {
          matches!(token, GraphqlxToken::<'inp, Src>::LBrace)
        })?;
        const_set_after_keyword(span.start(), inp)
      }
      Some(Spanned { span, data: token }) => {
        unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::Keyword("set"))
      }
      None => {
        unexpected_here(inp, Expectation::Keyword("set"))?;
        unreachable!("unexpected_here always returns an error")
      }
    }
  }
);

value_parser!(pub map_value, inp, Map<GraphqlxSlice<'inp, Src>>, [complex], {
  match inp.next()? {
    Some(Spanned { span, data: token }) if keyword_of(&token) == Some(ContextualKeyword::Map) => {
      guard_value_phase(inp, Expectation::LBrace, |token| {
        matches!(token, GraphqlxToken::<'inp, Src>::LBrace)
      })?;
      map_after_keyword(span.start(), inp)
    }
    Some(Spanned { span, data: token }) => {
      unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::Keyword("map"))
    }
    None => {
      unexpected_here(inp, Expectation::Keyword("map"))?;
      unreachable!("unexpected_here always returns an error")
    }
  }
});

value_parser!(
  pub const_map_value,
  inp,
  ConstMap<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
    match inp.next()? {
      Some(Spanned { span, data: token }) if keyword_of(&token) == Some(ContextualKeyword::Map) => {
        guard_value_phase(inp, Expectation::LBrace, |token| {
          matches!(token, GraphqlxToken::<'inp, Src>::LBrace)
        })?;
        const_map_after_keyword(span.start(), inp)
      }
      Some(Spanned { span, data: token }) => {
        unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::Keyword("map"))
      }
      None => {
        unexpected_here(inp, Expectation::Keyword("map"))?;
        unreachable!("unexpected_here always returns an error")
      }
    }
  }
);
