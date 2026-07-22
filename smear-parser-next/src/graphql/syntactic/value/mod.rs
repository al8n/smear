//! GraphQL value productions over the concrete syntactic lexer.
//!
//! Public parsers live on the local AST nodes as `Type::graphql` and
//! `Type::try_graphql`. They are fixed to [`GraphqlLexer`] and the
//! [`GraphQL`] marker, while retaining the lexer's supported source matrix through
//! the `Src` parameter. Results owned by `smear_scaffold` (lists, objects, object
//! fields, and defaults) remain specialized free helpers; private helpers assemble
//! their recursive productions with the same concrete input.
//!
//! Every committed parser reports an unexpected token or end of input after it has
//! committed. Where a `try_` counterpart is available, it declines without
//! consuming when its head does not match. The variable parser commits on `$` and
//! validates its name with a single-token lookahead. Object field lists stop only at
//! `}` or end of input; each field validates its name, colon, and value without
//! consuming a wrong token. Default values commit on `=` and validate the const-value
//! tail without consuming a wrong token.

use smear_scaffold::{ast as scaffold, hints::ObjectFieldValueHint};
use std::vec::Vec;
use tokora::{
  Accumulator, Branch, Lexer, ParseChoice, ParseInput, SimpleSpan, Slice, Source, Token,
  TryParseInput,
  cache::{Peeked, PeekedTokenExt},
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::Action,
  punct::{Brace, Bracket},
  span::Spanned,
  token::LitToken,
  try_parse_input::ParseAttempt,
  utils::typenum::{U1, U2},
};

use smear_lexer::graphql::syntactic::SyntacticTokenKind;

use super::{GraphqlError, GraphqlInput, GraphqlLexer, GraphqlSlice, GraphqlToken};
use crate::{
  combinator::{Equivalent, ParseCtx, colon, dollar, equal, ident},
  graphql::{
    GraphQL,
    ast::{
      BooleanValue, ConstInputValue, ConstObjectField, DefaultInputValue, EnumValue, FloatValue,
      InputValue, IntValue, Name, NullValue, ObjectField, StringValue, VariableValue,
    },
    error::{Expectation, GraphqlError as DialectGraphqlError},
  },
};

macro_rules! value_parser {
  (
    $visibility:vis $name:ident,
    $input:ident,
    $output:ty,
    [],
    $body:block
  ) => {
    value_parser!(@impl $visibility $name, $input, $output, [], $body);
  };
  (
    $visibility:vis $name:ident,
    $input:ident,
    $output:ty,
    [equivalent],
    $body:block
  ) => {
    value_parser!(
      @impl $visibility $name,
      $input,
      $output,
      [str: Equivalent<GraphqlSlice<'inp, Src>>,],
      $body
    );
  };
  (
    $visibility:vis $name:ident,
    $input:ident,
    $output:ty,
    [equivalent, delimited],
    $body:block
  ) => {
    value_parser!(
      @impl $visibility $name,
      $input,
      $output,
      [
        str: Equivalent<GraphqlSlice<'inp, Src>>,
        GraphqlError<'inp, Src, Ctx>: From<tokora::error::Unclosed<Bracket, SimpleSpan, GraphQL>>
          + From<tokora::error::Unclosed<Brace, SimpleSpan, GraphQL>>
          + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
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
    #[doc = "Parses or attempts this production with the concrete GraphQL syntactic lexer."]
    $visibility fn $name<'inp, Src, Ctx>(
      $input: &mut GraphqlInput<'inp, '_, Src, Ctx>,
    ) -> Result<$output, GraphqlError<'inp, Src, Ctx>>
    where
      Src: Source<usize> + ?Sized,
      GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
      GraphqlLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
      >,
      $($bounds)*
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
    $body
  };
}

macro_rules! value_try_parser {
  (
    $visibility:vis $name:ident,
    $input:ident,
    $output:ty,
    [],
    $body:block
  ) => {
    value_try_parser!(@impl $visibility $name, $input, $output, [], $body);
  };
  (
    $visibility:vis $name:ident,
    $input:ident,
    $output:ty,
    [equivalent],
    $body:block
  ) => {
    value_try_parser!(
      @impl $visibility $name,
      $input,
      $output,
      [str: Equivalent<GraphqlSlice<'inp, Src>>,],
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
    #[doc = "Parses or attempts this production with the concrete GraphQL syntactic lexer."]
    $visibility fn $name<'inp, Src, Ctx>(
      $input: &mut GraphqlInput<'inp, '_, Src, Ctx>,
    ) -> Result<$output, GraphqlError<'inp, Src, Ctx>>
    where
      Src: Source<usize> + ?Sized,
      GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
      GraphqlLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
      >,
      $($bounds)*
      Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
    $body
  };
}

macro_rules! value_eot_parser {
  (
    $visibility:vis $name:ident,
    $input:ident,
    $output:ty,
    [],
    $body:block
  ) => {
    value_eot_parser!(@impl $visibility $name, $input, $output, [], $body);
  };
  (
    $visibility:vis $name:ident,
    $input:ident,
    $output:ty,
    [equivalent],
    $body:block
  ) => {
    value_eot_parser!(
      @impl $visibility $name,
      $input,
      $output,
      [str: Equivalent<GraphqlSlice<'inp, Src>>,],
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
    #[doc = "Parses or attempts this production with the concrete GraphQL syntactic lexer."]
    $visibility fn $name<'inp, Src, Ctx>(
      $input: &mut GraphqlInput<'inp, '_, Src, Ctx>,
    ) -> Result<$output, GraphqlError<'inp, Src, Ctx>>
    where
      Src: Source<usize> + ?Sized,
      GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
      GraphqlLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
      >,
      $($bounds)*
      Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
      GraphqlError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQL>>,
    $body
  };
}

value_parser!(int_value, inp, IntValue<GraphqlSlice<'inp, Src>>, [], {
  match inp.next()? {
    Some(spanned) => {
      let (span, token) = spanned.into_components();
      match token {
        GraphqlToken::<'inp, Src>::LitInt(value) => Ok(IntValue::new(span, value)),
        other => Err(UnexpectedToken::of(span).with_found(other).into()),
      }
    }
    None => Err(UnexpectedEot::eot_of(*inp.offset()).into()),
  }
});

value_parser!(float_value, inp, FloatValue<GraphqlSlice<'inp, Src>>, [], {
  match inp.next()? {
    Some(spanned) => {
      let (span, token) = spanned.into_components();
      match token {
        GraphqlToken::<'inp, Src>::LitFloat(value) => Ok(FloatValue::new(span, value)),
        other => Err(UnexpectedToken::of(span).with_found(other).into()),
      }
    }
    None => Err(UnexpectedEot::eot_of(*inp.offset()).into()),
  }
});

value_parser!(
  string_value,
  inp,
  StringValue<GraphqlSlice<'inp, Src>>,
  [],
  {
    match inp.next()? {
      Some(spanned) => {
        let (span, token) = spanned.into_components();
        match token {
          GraphqlToken::<'inp, Src>::LitInlineStr(value) => {
            Ok(StringValue::new(span, value.into()))
          }
          GraphqlToken::<'inp, Src>::LitBlockStr(value) => Ok(StringValue::new(span, value.into())),
          other => Err(UnexpectedToken::of(span).with_found(other).into()),
        }
      }
      None => Err(UnexpectedEot::eot_of(*inp.offset()).into()),
    }
  }
);

value_parser!(
  inline_string_value,
  inp,
  StringValue<GraphqlSlice<'inp, Src>>,
  [],
  {
    match inp.next()? {
      Some(spanned) => {
        let (span, token) = spanned.into_components();
        match token {
          GraphqlToken::<'inp, Src>::LitInlineStr(value) => {
            Ok(StringValue::new(span, value.into()))
          }
          other => Err(UnexpectedToken::of(span).with_found(other).into()),
        }
      }
      None => Err(UnexpectedEot::eot_of(*inp.offset()).into()),
    }
  }
);

value_parser!(
  block_string_value,
  inp,
  StringValue<GraphqlSlice<'inp, Src>>,
  [],
  {
    match inp.next()? {
      Some(spanned) => {
        let (span, token) = spanned.into_components();
        match token {
          GraphqlToken::<'inp, Src>::LitBlockStr(value) => Ok(StringValue::new(span, value.into())),
          other => Err(UnexpectedToken::of(span).with_found(other).into()),
        }
      }
      None => Err(UnexpectedEot::eot_of(*inp.offset()).into()),
    }
  }
);

value_parser!(boolean_value, inp, BooleanValue, [equivalent], {
  match inp.next()? {
    Some(spanned) => {
      let (span, token) = spanned.into_components();

      if token.is_true_literal() {
        Ok(BooleanValue::new(span, true))
      } else if token.is_false_literal() {
        Ok(BooleanValue::new(span, false))
      } else {
        Err(UnexpectedToken::of(span).with_found(token).into())
      }
    }
    None => Err(UnexpectedEot::eot_of(*inp.offset()).into()),
  }
});

value_parser!(
  null_value,
  inp,
  NullValue<GraphqlSlice<'inp, Src>>,
  [equivalent],
  {
    match inp.next()? {
      Some(spanned) => {
        let (span, token) = spanned.into_components();
        match token {
          GraphqlToken::<'inp, Src>::Identifier(value) if "null".equivalent(&value) => {
            Ok(NullValue::new(span, value))
          }
          other => Err(UnexpectedToken::of(span).with_found(other).into()),
        }
      }
      None => Err(UnexpectedEot::eot_of(*inp.offset()).into()),
    }
  }
);

value_parser!(
  enum_value,
  inp,
  EnumValue<GraphqlSlice<'inp, Src>>,
  [equivalent],
  {
    match inp.next()? {
      Some(spanned) => {
        let (span, token) = spanned.into_components();
        match token {
          GraphqlToken::<'inp, Src>::Identifier(value)
            if !("true".equivalent(&value)
              || "false".equivalent(&value)
              || "null".equivalent(&value)) =>
          {
            Ok(EnumValue::new(span, value))
          }
          other => Err(UnexpectedToken::of(span).with_found(other).into()),
        }
      }
      None => Err(UnexpectedEot::eot_of(*inp.offset()).into()),
    }
  }
);

value_parser!(
  variable_value,
  inp,
  VariableValue<GraphqlSlice<'inp, Src>>,
  [],
  {
    dollar
      .ignore_then(ident.peek_then::<_, U1>(
        |mut peeked: Peeked<'_, 'inp, GraphqlLexer<'inp, Src>, U1>, _| {
          match peeked.pop_front() {
            Some(token) if token.token().is_identifier() => Ok(()),
            Some(token) => Err(
              UnexpectedToken::expected_one_with_found(
                *token.span(),
                token.token().clone(),
                SyntacticTokenKind::Identifier,
              )
              .into(),
            ),
            None => Ok(()),
          }
        },
      ))
      .spanned()
      .map(|Spanned { span, data: ident }| VariableValue::new(span, ident))
      .parse_input(inp)
  }
);

value_try_parser!(
  try_int_value,
  inp,
  ParseAttempt<IntValue<GraphqlSlice<'inp, Src>>>,
  [],
  {
    inp.try_expect(|t| t.data().is_lit_int()).map(|opt| {
      opt
        .map(|spanned| {
          let (span, token) = spanned.into_components();
          match token {
            GraphqlToken::<'inp, Src>::LitInt(value) => IntValue::new(span, value),
            other => unreachable!("unexpected token in try_int_value: {:?}", other),
          }
        })
        .into()
    })
  }
);

value_try_parser!(
  try_float_value,
  inp,
  ParseAttempt<FloatValue<GraphqlSlice<'inp, Src>>>,
  [],
  {
    inp.try_expect(|t| t.data().is_lit_float()).map(|opt| {
      opt
        .map(|Spanned { span, data: token }| match token {
          GraphqlToken::<'inp, Src>::LitFloat(value) => FloatValue::new(span, value),
          other => unreachable!("unexpected token in try_float_value: {:?}", other),
        })
        .into()
    })
  }
);

value_try_parser!(
  try_string_value,
  inp,
  ParseAttempt<StringValue<GraphqlSlice<'inp, Src>>>,
  [],
  {
    inp
      .try_expect(|t| {
        matches!(
          t.data(),
          GraphqlToken::<'inp, Src>::LitInlineStr(_) | GraphqlToken::<'inp, Src>::LitBlockStr(_)
        )
      })
      .map(|opt| {
        opt
          .map(|Spanned { span, data: token }| match token {
            GraphqlToken::<'inp, Src>::LitInlineStr(value) => StringValue::new(span, value.into()),
            GraphqlToken::<'inp, Src>::LitBlockStr(value) => StringValue::new(span, value.into()),
            other => unreachable!("unexpected token in try_string_value: {:?}", other),
          })
          .into()
      })
  }
);

value_eot_parser!(
  try_boolean_value,
  inp,
  ParseAttempt<BooleanValue>,
  [equivalent],
  {
    inp
      .try_expect_map(|t| {
        let token = t.into_data();
        if token.is_true_literal() {
          Some(true)
        } else if token.is_false_literal() {
          Some(false)
        } else {
          None
        }
      })
      .map(|opt| {
        opt
          .map(|(val, Spanned { span, data: _ })| BooleanValue::new(span, val))
          .into()
      })
  }
);

value_eot_parser!(
  try_null_value,
  inp,
  ParseAttempt<NullValue<GraphqlSlice<'inp, Src>>>,
  [equivalent],
  {
    inp
      .try_expect(|t| t.into_data().is_null_literal())
      .map(|opt| {
        opt
          .map(|Spanned { span, data: token }| match token {
            GraphqlToken::<'inp, Src>::Identifier(s) => NullValue::new(span, s),
            other => unreachable!("unexpected token in try_null_value: {:?}", other),
          })
          .into()
      })
  }
);

value_eot_parser!(
  try_enum_value,
  inp,
  ParseAttempt<EnumValue<GraphqlSlice<'inp, Src>>>,
  [equivalent],
  {
    inp
      .try_expect(|t| t.into_data().is_enum_value_literal())
      .map(|opt| {
        opt
          .map(|Spanned { span, data: token }| match token {
            GraphqlToken::<'inp, Src>::Identifier(s) => EnumValue::new(span, s),
            other => unreachable!("unexpected token in try_enum_value: {:?}", other),
          })
          .into()
      })
  }
);

value_parser!(
  try_variable_value,
  inp,
  ParseAttempt<VariableValue<GraphqlSlice<'inp, Src>>>,
  [],
  {
    (variable_value::<Src, Ctx>,)
      .peek_then_try_choice::<_, U1>(
        |mut peeked: Peeked<'_, 'inp, GraphqlLexer<'inp, Src>, U1>, _| {
          let Some(dollar) = peeked.pop_front() else {
            return Ok(None);
          };
          if !dollar.token().is_dollar() {
            return Ok(None);
          }
          Ok(Some(Branch::B0))
        },
      )
      .try_parse_input(inp)
  }
);

#[derive(Clone, Copy)]
pub(crate) enum HeadKind {
  Int,
  Float,
  InlineStr,
  BlockStr,
  True,
  False,
  Null,
  Enum,
  Dollar,
  List,
  Object,
}

/// Classifies a single token into the [`HeadKind`] it begins, or `None` when it
/// begins no value. The sole source of truth for a value's FIRST set — reused by
/// the sibling `Argument`/`ObjectField` list decisions (hence `pub(crate)`).
#[inline]
pub(crate) fn value_head_kind<'inp, Src>(t: &GraphqlToken<'inp, Src>) -> Option<HeadKind>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
{
  Some(match t {
    GraphqlToken::<'inp, Src>::LitInt(_) => HeadKind::Int,
    GraphqlToken::<'inp, Src>::LitFloat(_) => HeadKind::Float,
    GraphqlToken::<'inp, Src>::LitInlineStr(_) => HeadKind::InlineStr,
    GraphqlToken::<'inp, Src>::LitBlockStr(_) => HeadKind::BlockStr,
    GraphqlToken::<'inp, Src>::Identifier(s) => {
      if "true".equivalent(s) {
        HeadKind::True
      } else if "false".equivalent(s) {
        HeadKind::False
      } else if "null".equivalent(s) {
        HeadKind::Null
      } else {
        HeadKind::Enum
      }
    }
    GraphqlToken::<'inp, Src>::Dollar => HeadKind::Dollar,
    GraphqlToken::<'inp, Src>::LBracket => HeadKind::List,
    GraphqlToken::<'inp, Src>::LBrace => HeadKind::Object,
    _ => return None,
  })
}

#[inline]
fn is_const_value_head<'inp, Src>(token: &GraphqlToken<'inp, Src>) -> bool
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
{
  !matches!(value_head_kind::<Src>(token), Some(HeadKind::Dollar) | None)
}

pub(crate) fn decide_value_head<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlLexer<'inp, Src>, U1>,
  _: &mut Ctx::Emitter,
) -> Result<Action, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
{
  Ok(match peeked.pop_front() {
    Some(token) if value_head_kind::<Src>(token.token()).is_some() => Action::Continue,
    _ => Action::Stop,
  })
}

pub(crate) fn decide_object_field_head<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlLexer<'inp, Src>, U1>,
  _: &mut Ctx::Emitter,
) -> Result<Action, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
{
  Ok(match peeked.pop_front() {
    Some(token) if matches!(token.token(), GraphqlToken::<'inp, Src>::RBrace) => Action::Stop,
    Some(_) => Action::Continue,
    None => Action::Stop,
  })
}

/// Consumes the scalar token selected by [`value_head_kind`]. The dispatcher has
/// already established the token's meaning, so this helper only handles token
/// availability and preserves the payload for the selected branch to move.
#[inline]
fn consume_dispatched_token<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<Spanned<GraphqlToken<'inp, Src>, SimpleSpan>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQL>>,
{
  match inp.next()? {
    Some(spanned) => Ok(spanned),
    None => Err(UnexpectedEot::eot_of(*inp.offset()).into()),
  }
}

/// Moves the identifier payload from a scalar branch that was already selected
/// by [`value_head_kind`], preserving a typed parser error if that invariant is
/// ever violated.
#[inline]
fn consume_dispatched_identifier<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<(SimpleSpan, GraphqlSlice<'inp, Src>), GraphqlError<'inp, Src, Ctx>>
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
  let (span, token) = consume_dispatched_token(inp)?.into_components();
  match token {
    GraphqlToken::<'inp, Src>::Identifier(value) => Ok((span, value)),
    other => Err(UnexpectedToken::of(span).with_found(other).into()),
  }
}

fn guard_object_field_phase<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  expected: Expectation,
  eot_hint: ObjectFieldValueHint,
  mut accepts: impl FnMut(&GraphqlToken<'inp, Src>) -> bool,
) -> Result<(), GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let off = *inp.offset();
  let rejected = {
    let mut peeked = inp.peek::<U1>()?;
    match peeked.pop_front() {
      Some(token) if accepts(token.token()) => return Ok(()),
      Some(token) => Some((*token.span(), token.token().kind())),
      None => None,
    }
  };

  match rejected {
    Some((span, kind)) => Err(DialectGraphqlError::unexpected_token(kind, expected, span).into()),
    None => Err(
      DialectGraphqlError::unexpected_end_of_object_field_value(
        eot_hint,
        SimpleSpan::new(off, off),
      )
      .into(),
    ),
  }
}

value_parser!(
  pub list_value,
  inp,
  scaffold::List<InputValue<GraphqlSlice<'inp, Src>>>,
  [equivalent, delimited],
  {
    value
      .repeated_while::<_, U1>(decide_value_head::<_, Ctx>)
      .delimited_by_brackets()
      .collect_with(Vec::new())
      .spanned()
      .parse_input(inp)
      .map(|Spanned { span, data: values }| scaffold::List::new(span, values))
  }
);

value_parser!(
  pub const_list_value,
  inp,
  scaffold::List<ConstInputValue<GraphqlSlice<'inp, Src>>>,
  [equivalent, delimited],
  {
    const_value
      .repeated_while::<_, U1>(decide_value_head::<_, Ctx>)
      .delimited_by_brackets()
      .collect_with(Vec::new())
      .spanned()
      .parse_input(inp)
      .map(|Spanned { span, data: values }| scaffold::List::new(span, values))
  }
);

value_parser!(
  pub object_field,
  inp,
  ObjectField<GraphqlSlice<'inp, Src>>,
  [equivalent, delimited],
  {
    (|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
      guard_object_field_phase(
        inp,
        Expectation::Name,
        ObjectFieldValueHint::Name,
        |token| token.is_identifier(),
      )?;
      ident(inp)
    })
      .then_ignore(|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        guard_object_field_phase(
          inp,
          Expectation::Colon,
          ObjectFieldValueHint::Colon,
          |token| token.is_colon(),
        )?;
        colon(inp)
      })
      .then(|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        guard_object_field_phase(
          inp,
          Expectation::InputValue,
          ObjectFieldValueHint::Value,
          |token| value_head_kind::<Src>(token).is_some(),
        )?;
        value(inp)
      })
      .spanned()
      .map(|Spanned { span, data: (name, value) }| scaffold::ObjectField::new(span, name, value))
      .parse_input(inp)
  }
);

value_parser!(
  pub const_object_field,
  inp,
  ConstObjectField<GraphqlSlice<'inp, Src>>,
  [equivalent, delimited],
  {
    (|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
      guard_object_field_phase(
        inp,
        Expectation::Name,
        ObjectFieldValueHint::Name,
        |token| token.is_identifier(),
      )?;
      ident(inp)
    })
      .then_ignore(|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        guard_object_field_phase(
          inp,
          Expectation::Colon,
          ObjectFieldValueHint::Colon,
          |token| token.is_colon(),
        )?;
        colon(inp)
      })
      .then(|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        guard_object_field_phase(
          inp,
          Expectation::ConstInputValue,
          ObjectFieldValueHint::Value,
          is_const_value_head::<Src>,
        )?;
        const_value(inp)
      })
      .spanned()
      .map(|Spanned { span, data: (name, value) }| scaffold::ObjectField::new(span, name, value))
      .parse_input(inp)
  }
);

value_parser!(
  pub object_value,
  inp,
  scaffold::Object<Name<GraphqlSlice<'inp, Src>>, InputValue<GraphqlSlice<'inp, Src>>>,
  [equivalent, delimited],
  {
    object_field
      .repeated_while::<_, U1>(decide_object_field_head::<_, Ctx>)
      .delimited_by_braces()
      .collect_with(Vec::new())
      .spanned()
      .parse_input(inp)
      .map(|Spanned { span, data: fields }| scaffold::Object::new(span, fields))
  }
);

value_parser!(
  pub const_object_value,
  inp,
  scaffold::Object<Name<GraphqlSlice<'inp, Src>>, ConstInputValue<GraphqlSlice<'inp, Src>>>,
  [equivalent, delimited],
  {
    const_object_field
      .repeated_while::<_, U1>(decide_object_field_head::<_, Ctx>)
      .delimited_by_braces()
      .collect_with(Vec::new())
      .spanned()
      .parse_input(inp)
      .map(|Spanned { span, data: fields }| scaffold::Object::new(span, fields))
  }
);

value_parser!(
  value,
  inp,
  InputValue<GraphqlSlice<'inp, Src>>,
  [equivalent, delimited],
  {
    let off = *inp.offset();
    (
      int_value.map(InputValue::Int),
      float_value.map(InputValue::Float),
      inline_string_value.map(InputValue::String),
      block_string_value.map(InputValue::String),
      (|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        let span = consume_dispatched_token(inp)?.into_span();
        Ok(InputValue::Boolean(BooleanValue::new(span, true)))
      }),
      (|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        let span = consume_dispatched_token(inp)?.into_span();
        Ok(InputValue::Boolean(BooleanValue::new(span, false)))
      }),
      (|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        let (span, value) = consume_dispatched_identifier(inp)?;
        Ok(InputValue::Null(NullValue::new(span, value)))
      }),
      (|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        let (span, value) = consume_dispatched_identifier(inp)?;
        Ok(InputValue::Enum(EnumValue::new(span, value)))
      }),
      variable_value.map(InputValue::Variable),
      list_value.map(InputValue::List),
      object_value.map(InputValue::Object),
    )
      .peek_then_choice::<_, U2>(|peeked, _| {
        let Some(head) = peeked.front() else {
          return Err(UnexpectedEot::eot_of(off).into());
        };

        Ok(match value_head_kind::<Src>(head.token()) {
          Some(knd) => match knd {
            HeadKind::Int => Branch::B0,
            HeadKind::Float => Branch::B1,
            HeadKind::InlineStr => Branch::B2,
            HeadKind::BlockStr => Branch::B3,
            HeadKind::True => Branch::B4,
            HeadKind::False => Branch::B5,
            HeadKind::Null => Branch::B6,
            HeadKind::Enum => Branch::B7,
            HeadKind::Dollar => Branch::B8,
            HeadKind::List => Branch::B9,
            HeadKind::Object => Branch::B10,
          },
          _ => {
            return Err(
              DialectGraphqlError::unexpected_token(
                head.token().kind(),
                Expectation::InputValue,
                *head.span(),
              )
              .into(),
            );
          }
        })
      })
      .parse_input(inp)
  }
);

value_parser!(
  const_value,
  inp,
  ConstInputValue<GraphqlSlice<'inp, Src>>,
  [equivalent, delimited],
  {
    let off = *inp.offset();
    (
      int_value.map(ConstInputValue::Int),
      float_value.map(ConstInputValue::Float),
      inline_string_value.map(ConstInputValue::String),
      block_string_value.map(ConstInputValue::String),
      (|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        let span = consume_dispatched_token(inp)?.into_span();
        Ok(ConstInputValue::Boolean(BooleanValue::new(span, true)))
      }),
      (|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        let span = consume_dispatched_token(inp)?.into_span();
        Ok(ConstInputValue::Boolean(BooleanValue::new(span, false)))
      }),
      (|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        let (span, value) = consume_dispatched_identifier(inp)?;
        Ok(ConstInputValue::Null(NullValue::new(span, value)))
      }),
      (|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        let (span, value) = consume_dispatched_identifier(inp)?;
        Ok(ConstInputValue::Enum(EnumValue::new(span, value)))
      }),
      const_list_value.map(ConstInputValue::List),
      const_object_value.map(ConstInputValue::Object),
    )
      .peek_then_choice::<_, U2>(|peeked, _| {
        let Some(head) = peeked.front() else {
          return Err(UnexpectedEot::eot_of(off).into());
        };

        Ok(match value_head_kind::<Src>(head.token()) {
          Some(knd) => match knd {
            HeadKind::Int => Branch::B0,
            HeadKind::Float => Branch::B1,
            HeadKind::InlineStr => Branch::B2,
            HeadKind::BlockStr => Branch::B3,
            HeadKind::True => Branch::B4,
            HeadKind::False => Branch::B5,
            HeadKind::Null => Branch::B6,
            HeadKind::Enum => Branch::B7,
            HeadKind::List => Branch::B8,
            HeadKind::Object => Branch::B9,
            HeadKind::Dollar => {
              return Err(
                DialectGraphqlError::unexpected_token(
                  head.token().kind(),
                  Expectation::ConstInputValue,
                  *head.span(),
                )
                .into(),
              );
            }
          },
          _ => {
            return Err(
              DialectGraphqlError::unexpected_token(
                head.token().kind(),
                Expectation::ConstInputValue,
                *head.span(),
              )
              .into(),
            );
          }
        })
      })
      .parse_input(inp)
  }
);

value_parser!(
  committed_default_value,
  inp,
  DefaultInputValue<GraphqlSlice<'inp, Src>>,
  [equivalent, delimited],
  {
    let validated_const_tail = |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
      let off = *inp.offset();
      const_value
        .peek_then::<_, U1>(
          move |mut peeked: Peeked<'_, 'inp, GraphqlLexer<'inp, Src>, U1>, _| match peeked
            .pop_front()
          {
            Some(token) if is_const_value_head::<Src>(token.token()) => Ok(()),
            Some(token) => Err(
              DialectGraphqlError::unexpected_token(
                token.token().kind(),
                Expectation::ConstInputValue,
                *token.span(),
              )
              .into(),
            ),
            None => Err(
              DialectGraphqlError::maybe_unexpected_token(
                None,
                Expectation::ConstInputValue,
                SimpleSpan::new(off, off),
              )
              .into(),
            ),
          },
        )
        .parse_input(inp)
    };

    equal
      .ignore_then(validated_const_tail)
      .spanned()
      .map(|Spanned { span, data: value }| DefaultInputValue::new(span, value))
      .parse_input(inp)
  }
);

value_parser!(
  pub try_default_value,
  inp,
  ParseAttempt<DefaultInputValue<GraphqlSlice<'inp, Src>>>,
  [equivalent, delimited],
  {
    committed_default_value
      .peek_then_try::<_, U1>(
        |mut peeked: Peeked<'_, 'inp, GraphqlLexer<'inp, Src>, U1>, _: &mut Ctx::Emitter| -> Result<
          Action,
          GraphqlError<'inp, Src, Ctx>,
        > {
          Ok(match peeked.pop_front() {
            Some(equal) if equal.token().is_equal() => Action::Continue,
            _ => Action::Stop,
          })
        },
      )
      .try_parse_input(inp)
  }
);

value_parser!(
  pub default_value,
  inp,
  Option<DefaultInputValue<GraphqlSlice<'inp, Src>>>,
  [equivalent, delimited],
  {
    try_default_value(inp).map(Into::into)
  }
);

macro_rules! graphql_slice_api {
  ($slice:ident, $node:ty, $parse:ident, []) => {
    graphql_slice_api!(@graphql $slice, $node, $parse, []);
  };
  ($slice:ident, $node:ty, $parse:ident, $try_parse:ident, []) => {
    graphql_slice_api!(@graphql $slice, $node, $parse, []);
    graphql_slice_api!(@try $slice, $node, $try_parse, []);
  };
  ($slice:ident, $node:ty, $parse:ident, [equivalent]) => {
    graphql_slice_api!(@graphql $slice, $node, $parse, [str: Equivalent<$slice>,]);
  };
  ($slice:ident, $node:ty, $parse:ident, [equivalent, delimited]) => {
    graphql_slice_api!(
      @graphql $slice,
      $node,
      $parse,
      [
        str: Equivalent<$slice>,
        GraphqlError<'inp, Src, Ctx>: From<tokora::error::Unclosed<Bracket, SimpleSpan, GraphQL>>
          + From<tokora::error::Unclosed<Brace, SimpleSpan, GraphQL>>
          + From<DialectGraphqlError<$slice>>,
      ]
    );
  };
  ($slice:ident, $node:ty, $parse:ident, $try_parse:ident, [equivalent]) => {
    graphql_slice_api!(@graphql $slice, $node, $parse, [str: Equivalent<$slice>,]);
    graphql_slice_api!(@try $slice, $node, $try_parse, [str: Equivalent<$slice>,]);
  };
  (
    @graphql $slice:ident,
    $node:ty,
    $parse:ident,
    [$($bounds:tt)*]
  ) => {
    impl<$slice> $node {
      #[doc = "Parses this GraphQL value from the concrete syntactic lexer."]
      pub fn graphql<'inp, Src, Ctx>(
        inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
      ) -> Result<Self, GraphqlError<'inp, Src, Ctx>>
      where
        Src: Source<usize, Slice<'inp> = $slice> + ?Sized,
        $slice: Slice<'inp> + Clone + 'inp,
        GraphqlLexer<'inp, Src>: Lexer<
            'inp,
            Source = Src,
            Token = GraphqlToken<'inp, Src>,
            Span = SimpleSpan,
            Offset = usize,
          >,
        $($bounds)*
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
        $parse(inp)
      }
    }
  };

  (
    @try $slice:ident,
    $node:ty,
    $try_parse:ident,
    [$($bounds:tt)*]
  ) => {
    impl<$slice> $node {
      #[doc = "Attempts this GraphQL value without consuming on a head mismatch."]
      pub fn try_graphql<'inp, Src, Ctx>(
        inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
      ) -> Result<ParseAttempt<Self>, GraphqlError<'inp, Src, Ctx>>
      where
        Src: Source<usize, Slice<'inp> = $slice> + ?Sized,
        $slice: Slice<'inp> + Clone + 'inp,
        GraphqlLexer<'inp, Src>: Lexer<
            'inp,
            Source = Src,
            Token = GraphqlToken<'inp, Src>,
            Span = SimpleSpan,
            Offset = usize,
          >,
        $($bounds)*
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
        $try_parse(inp)
      }
    }
  };
}

graphql_slice_api!(S, IntValue<S>, int_value, try_int_value, []);
graphql_slice_api!(S, FloatValue<S>, float_value, try_float_value, []);
graphql_slice_api!(S, StringValue<S>, string_value, try_string_value, []);
graphql_slice_api!(S, NullValue<S>, null_value, try_null_value, [equivalent]);
graphql_slice_api!(S, EnumValue<S>, enum_value, try_enum_value, [equivalent]);
graphql_slice_api!(
  S,
  crate::value::VariableValue<Name<S>>,
  variable_value,
  try_variable_value,
  []
);
graphql_slice_api!(S, InputValue<S>, value, [equivalent, delimited]);
graphql_slice_api!(S, ConstInputValue<S>, const_value, [equivalent, delimited]);

impl BooleanValue {
  /// Parses a GraphQL boolean value from the concrete syntactic lexer.
  pub fn graphql<'inp, Src, Ctx>(
    inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  ) -> Result<Self, GraphqlError<'inp, Src, Ctx>>
  where
    Src: Source<usize> + ?Sized,
    GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
    str: Equivalent<GraphqlSlice<'inp, Src>>,
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
    boolean_value(inp)
  }

  /// Attempts a GraphQL boolean value without consuming on a head mismatch.
  pub fn try_graphql<'inp, Src, Ctx>(
    inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  ) -> Result<ParseAttempt<Self>, GraphqlError<'inp, Src, Ctx>>
  where
    Src: Source<usize> + ?Sized,
    GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
    str: Equivalent<GraphqlSlice<'inp, Src>>,
    GraphqlLexer<'inp, Src>:
      Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
    GraphqlError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQL>>,
  {
    try_boolean_value(inp)
  }
}

#[cfg(test)]
mod tests;
