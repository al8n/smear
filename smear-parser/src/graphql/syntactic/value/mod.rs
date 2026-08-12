//! GraphQL value productions over the concrete syntactic lexer.
//!
//! Public associated parsers live directly on the GraphQL AST types, such as
//! [`crate::graphql::ast::IntValue`], [`crate::graphql::ast::VariableValue`], and
//! [`crate::graphql::ast::Object`]. Their slice generic is fixed by the AST result
//! while the [`GraphqlLexer`] source is inferred from the parser input. Public
//! free production functions remain available for direct parser composition.
//!
//! Every committed parser reports an unexpected token or end of input after it has
//! committed. Where a `try_` counterpart is available, it declines without
//! consuming when its head does not match. Scalar input-value heads are fused so
//! their token is lexed and classified once; list and object heads retain their
//! delimiter for the existing delimited parsers. The variable parser commits on `$`
//! and validates its name with a single-token lookahead. Object field lists stop only
//! at `}` or end of input; each field validates its name, colon, and value without
//! consuming a wrong token. Default values commit on `=` and validate the const-value
//! tail without consuming a wrong token.

use std::vec::Vec;
use tokora::{
  Accumulator, EmitterView, Lexer, ParseInput, ParseTokenChoice, SimpleSpan, Slice, Source,
  TryParseInput,
  cache::{Peeked, PeekedTokenExt},
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::Action,
  span::Spanned,
  try_parse_input::ParseAttempt,
  utils::{DowncastRef, typenum::U1},
};

use smear_lexer::graphql::{ContextualKeyword, syntactic::SyntacticTokenKind};

use super::{GraphqlError, GraphqlInput, GraphqlLexer, GraphqlSlice, GraphqlToken, name};
use crate::{
  combinator::{ParseCtx, TokenSpannedExt, colon, dollar, equal},
  graphql::{
    GraphQL,
    ast::{
      BooleanValue, ConstInputValue, ConstList as AstConstList, ConstObject as AstConstObject,
      ConstObjectField, DefaultInputValue, EnumValue, FloatValue, InputValue, IntValue,
      List as AstList, NullValue, Object as AstObject, ObjectField, StringValue, VariableValue,
    },
    error::{Expectation, GraphqlError as DialectGraphqlError, ObjectFieldValueHint},
  },
};

use numbers::{
  ConstValueList, ConstValueObject, ConstValueObjectField, Numbers, SliceNumbers, ValueDefault,
  ValueList, ValueObject, ValueObjectField,
};

use crate::value::{
  DefaultInputValue as DefaultInputValueNode, List as ListNode, Object as ObjectNode,
  ObjectField as ObjectFieldNode,
};

/// The composite value productions, once, generic over what their numeric leaves carry.
///
/// Identical to `value_parser!` but for the extra `N` parameter and its bound: the body writes
/// `N::int` / `N::float` where a payload is built and `N::report` where one fails, and the
/// instantiations — [`SliceNumbers`] here, `MaterializedNumbers` in [`materialized`],
/// `MaterializedNumbers32` in [`materialized32`] — are then the same parser at three payloads
/// rather than three parsers that agree today.
///
/// Only the `[contextual, delimited]` bound set exists, and that is not an omission: every
/// production this macro generates already carried it, so making the family generic added no
/// obligation to any public signature. The four *leaf* productions are hand-written on both
/// sides instead, precisely because sharing them WOULD have widened
/// [`int_value`]'s and [`float_value`]'s public where-clauses with a `From<GraphqlError<…>>`
/// bound the slice parser can never need.
macro_rules! numeric_value_parser {
  (
    $visibility:vis $name:ident,
    $input:ident,
    $output:ty,
    [contextual, delimited],
    $body:block
  ) => {
    #[doc = "Parses or attempts this production with the concrete GraphQL syntactic lexer, over the numeric payloads `N` selects."]
    $visibility fn $name<'inp, Src, Ctx, N>(
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
      GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
      GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
      N: Numbers<GraphqlSlice<'inp, Src>>,
      // One conversion per leaf the shared bodies read, which is how a body that serves two
      // trees names neither. `derive_more::From` on both enums is what satisfies these, so a
      // variant renamed on one side and not the other stops compiling here.
      N::Value: From<VariableValue<GraphqlSlice<'inp, Src>>>
        + From<BooleanValue<GraphqlSlice<'inp, Src>>>
        + From<StringValue<GraphqlSlice<'inp, Src>>>
        + From<FloatValue<N::Float>>
        + From<IntValue<N::Int>>
        + From<EnumValue<GraphqlSlice<'inp, Src>>>
        + From<NullValue<GraphqlSlice<'inp, Src>>>
        + From<ValueList<GraphqlSlice<'inp, Src>, N>>
        + From<ValueObject<GraphqlSlice<'inp, Src>, N>>,
      N::ConstValue: From<BooleanValue<GraphqlSlice<'inp, Src>>>
        + From<StringValue<GraphqlSlice<'inp, Src>>>
        + From<FloatValue<N::Float>>
        + From<IntValue<N::Int>>
        + From<EnumValue<GraphqlSlice<'inp, Src>>>
        + From<NullValue<GraphqlSlice<'inp, Src>>>
        + From<ConstValueList<GraphqlSlice<'inp, Src>, N>>
        + From<ConstValueObject<GraphqlSlice<'inp, Src>, N>>,
      Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
    $body
  };
}

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
    [contextual],
    $body:block
  ) => {
    value_parser!(
      @impl $visibility $name,
      $input,
      $output,
      [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
      $body
    );
  };
  (
    $visibility:vis $name:ident,
    $input:ident,
    $output:ty,
    [contextual, delimited],
    $body:block
  ) => {
    value_parser!(
      @impl $visibility $name,
      $input,
      $output,
      [
        GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
        GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
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
    [contextual],
    $body:block
  ) => {
    value_try_parser!(
      @impl $visibility $name,
      $input,
      $output,
      [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
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
    [contextual],
    $body:block
  ) => {
    value_eot_parser!(
      @impl $visibility $name,
      $input,
      $output,
      [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
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
  boolean_value,
  inp,
  BooleanValue<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    match inp.next()? {
      Some(spanned) => {
        let (span, token) = spanned.into_components();
        match token.downcast_ref() {
          Some(ContextualKeyword::True) => Ok(BooleanValue::new(span, true)),
          Some(ContextualKeyword::False) => Ok(BooleanValue::new(span, false)),
          _ => Err(UnexpectedToken::of(span).with_found(token).into()),
        }
      }
      None => Err(UnexpectedEot::eot_of(*inp.offset()).into()),
    }
  }
);

value_parser!(
  null_value,
  inp,
  NullValue<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    match inp.next()? {
      Some(spanned) => {
        let (span, token) = spanned.into_components();
        match token.downcast_ref() {
          Some(ContextualKeyword::Null) => match token {
            GraphqlToken::<'inp, Src>::Identifier(value) => Ok(NullValue::new(span, value)),
            other => unreachable!("contextual null token was not an identifier: {:?}", other),
          },
          _ => Err(UnexpectedToken::of(span).with_found(token).into()),
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
  [contextual],
  {
    match inp.next()? {
      Some(spanned) => {
        let (span, token) = spanned.into_components();
        match token.downcast_ref() {
          Some(ContextualKeyword::True | ContextualKeyword::False | ContextualKeyword::Null) => {
            Err(UnexpectedToken::of(span).with_found(token).into())
          }
          _ => match token {
            GraphqlToken::<'inp, Src>::Identifier(value) => Ok(EnumValue::new(span, value)),
            other => Err(UnexpectedToken::of(span).with_found(other).into()),
          },
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
    let start = dollar(inp)?.span().start();
    variable_after_dollar(start, inp)
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
  ParseAttempt<BooleanValue<GraphqlSlice<'inp, Src>>>,
  [contextual],
  {
    inp
      .try_expect_map(|t| {
        let token = t.into_data();
        match token.downcast_ref() {
          Some(ContextualKeyword::True) => Some(true),
          Some(ContextualKeyword::False) => Some(false),
          _ => None,
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
  [contextual],
  {
    inp
      .try_expect(|t| t.into_data().downcast_ref() == Some(ContextualKeyword::Null))
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
  [contextual],
  {
    inp
      .try_expect(|t| {
        let token = t.into_data();
        !matches!(
          token.downcast_ref(),
          Some(ContextualKeyword::True | ContextualKeyword::False | ContextualKeyword::Null)
        ) && token.is_identifier()
      })
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
    (
      |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
       inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| match token {
        GraphqlToken::<'inp, Src>::Dollar => variable_after_dollar(span.start(), inp),
        _ => unreachable!("fused variable arm received a non-dollar token"),
      },
    )
      .fused_dispatch_on_kind(&[SyntacticTokenKind::Dollar])
      .try_parse_input(inp)
  }
);

/// Parses a variable name after its `$` head has already committed. A bad name
/// remains available to its parent, matching the committed variable parser.
fn variable_after_dollar<'inp, Src, Ctx>(
  start: usize,
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<VariableValue<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
{
  let name = name
    .peek_then::<_, U1>(
      |mut peeked: Peeked<'_, 'inp, GraphqlLexer<'inp, Src>, U1>, _| match peeked.pop_front() {
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
      },
    )
    .parse_input(inp)?;
  Ok(VariableValue::new(
    SimpleSpan::new(start, name.span().end()),
    name,
  ))
}

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
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
{
  Some(match t.downcast_ref() {
    Some(ContextualKeyword::True) => HeadKind::True,
    Some(ContextualKeyword::False) => HeadKind::False,
    Some(ContextualKeyword::Null) => HeadKind::Null,
    _ => match t {
      GraphqlToken::<'inp, Src>::LitInt(_) => HeadKind::Int,
      GraphqlToken::<'inp, Src>::LitFloat(_) => HeadKind::Float,
      GraphqlToken::<'inp, Src>::LitInlineStr(_) => HeadKind::InlineStr,
      GraphqlToken::<'inp, Src>::LitBlockStr(_) => HeadKind::BlockStr,
      GraphqlToken::<'inp, Src>::Identifier(_) => HeadKind::Enum,
      GraphqlToken::<'inp, Src>::Dollar => HeadKind::Dollar,
      GraphqlToken::<'inp, Src>::LBracket => HeadKind::List,
      GraphqlToken::<'inp, Src>::LBrace => HeadKind::Object,
      _ => return None,
    },
  })
}

#[inline]
fn is_const_value_head<'inp, Src>(token: &GraphqlToken<'inp, Src>) -> bool
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
{
  !matches!(value_head_kind::<Src>(token), Some(HeadKind::Dollar) | None)
}

pub(crate) fn decide_value_head<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlLexer<'inp, Src>, U1>,
  _: EmitterView<'_, 'inp, GraphqlLexer<'inp, Src>, Ctx::Emitter, GraphQL>,
) -> Result<Action, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
{
  Ok(match peeked.pop_front() {
    Some(token) if value_head_kind::<Src>(token.token()).is_some() => Action::Continue,
    _ => Action::Stop,
  })
}

pub(crate) fn decide_object_field_head<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlLexer<'inp, Src>, U1>,
  _: EmitterView<'_, 'inp, GraphqlLexer<'inp, Src>, Ctx::Emitter, GraphQL>,
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

numeric_value_parser!(
  list_value_with,
  inp,
  ValueList<GraphqlSlice<'inp, Src>, N>,
  [contextual, delimited],
  {
    value_with::<Src, Ctx, N>
      .repeated_while::<_, U1>(decide_value_head::<_, Ctx>)
      .delimited_by_brackets()
      .collect_with(Vec::new())
      .token_spanned()
      .parse_input(inp)
      .map(|Spanned { span, data: values }| ListNode::new(span, values))
  }
);

numeric_value_parser!(
  const_list_value_with,
  inp,
  ConstValueList<GraphqlSlice<'inp, Src>, N>,
  [contextual, delimited],
  {
    const_value_with::<Src, Ctx, N>
      .repeated_while::<_, U1>(decide_value_head::<_, Ctx>)
      .delimited_by_brackets()
      .collect_with(Vec::new())
      .token_spanned()
      .parse_input(inp)
      .map(|Spanned { span, data: values }| ListNode::new(span, values))
  }
);

numeric_value_parser!(
  object_field_with,
  inp,
  ValueObjectField<GraphqlSlice<'inp, Src>, N>,
  [contextual, delimited],
  {
    (|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
      guard_object_field_phase(
        inp,
        Expectation::Name,
        ObjectFieldValueHint::Name,
        |token| token.is_identifier(),
      )?;
      name(inp)
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
      value_with::<Src, Ctx, N>(inp)
    })
    .token_spanned()
    .map(
      |Spanned {
         span,
         data: (name, value),
       }| ObjectFieldNode::new(span, name, value),
    )
    .parse_input(inp)
  }
);

numeric_value_parser!(
  const_object_field_with,
  inp,
  ConstValueObjectField<GraphqlSlice<'inp, Src>, N>,
  [contextual, delimited],
  {
    (|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
      guard_object_field_phase(
        inp,
        Expectation::Name,
        ObjectFieldValueHint::Name,
        |token| token.is_identifier(),
      )?;
      name(inp)
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
      const_value_with::<Src, Ctx, N>(inp)
    })
    .token_spanned()
    .map(
      |Spanned {
         span,
         data: (name, value),
       }| ObjectFieldNode::new(span, name, value),
    )
    .parse_input(inp)
  }
);

numeric_value_parser!(
  object_value_with,
  inp,
  ValueObject<GraphqlSlice<'inp, Src>, N>,
  [contextual, delimited],
  {
    object_field_with::<Src, Ctx, N>
      .repeated_while::<_, U1>(decide_object_field_head::<_, Ctx>)
      .delimited_by_braces()
      .collect_with(Vec::new())
      .token_spanned()
      .parse_input(inp)
      .map(|Spanned { span, data: fields }| ObjectNode::new(span, fields))
  }
);

numeric_value_parser!(
  const_object_value_with,
  inp,
  ConstValueObject<GraphqlSlice<'inp, Src>, N>,
  [contextual, delimited],
  {
    const_object_field_with::<Src, Ctx, N>
      .repeated_while::<_, U1>(decide_object_field_head::<_, Ctx>)
      .delimited_by_braces()
      .collect_with(Vec::new())
      .token_spanned()
      .parse_input(inp)
      .map(|Spanned { span, data: fields }| ObjectNode::new(span, fields))
  }
);

value_parser!(
  pub list_value,
  inp,
  AstList<GraphqlSlice<'inp, Src>>,
  [contextual, delimited],
  { list_value_with::<Src, Ctx, SliceNumbers>(inp) }
);

value_parser!(
  pub const_list_value,
  inp,
  AstConstList<GraphqlSlice<'inp, Src>>,
  [contextual, delimited],
  { const_list_value_with::<Src, Ctx, SliceNumbers>(inp) }
);

value_parser!(
  pub object_field,
  inp,
  ObjectField<GraphqlSlice<'inp, Src>>,
  [contextual, delimited],
  { object_field_with::<Src, Ctx, SliceNumbers>(inp) }
);

value_parser!(
  pub const_object_field,
  inp,
  ConstObjectField<GraphqlSlice<'inp, Src>>,
  [contextual, delimited],
  { const_object_field_with::<Src, Ctx, SliceNumbers>(inp) }
);

value_parser!(
  pub object_value,
  inp,
  AstObject<GraphqlSlice<'inp, Src>>,
  [contextual, delimited],
  { object_value_with::<Src, Ctx, SliceNumbers>(inp) }
);

value_parser!(
  pub const_object_value,
  inp,
  AstConstObject<GraphqlSlice<'inp, Src>>,
  [contextual, delimited],
  { const_object_value_with::<Src, Ctx, SliceNumbers>(inp) }
);

numeric_value_parser!(value_with, inp, N::Value, [contextual, delimited], {
  let int_head_arm =
    |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
     _: &mut GraphqlInput<'inp, '_, Src, Ctx>| match token {
      GraphqlToken::<'inp, Src>::LitInt(value) => match N::int(value) {
        Ok(payload) => Ok(IntValue::new(span, payload).into()),
        Err(err) => Err(N::report(err, span).into()),
      },
      _ => unreachable!("fused input-value arm received a non-int token"),
    };
  let float_head_arm =
    |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
     _: &mut GraphqlInput<'inp, '_, Src, Ctx>| match token {
      GraphqlToken::<'inp, Src>::LitFloat(value) => match N::float(value) {
        Ok(payload) => Ok(FloatValue::new(span, payload).into()),
        Err(err) => Err(N::report(err, span).into()),
      },
      _ => unreachable!("fused input-value arm received a non-float token"),
    };
  let inline_string_head_arm =
    |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
     _: &mut GraphqlInput<'inp, '_, Src, Ctx>| match token {
      GraphqlToken::<'inp, Src>::LitInlineStr(value) => {
        Ok(StringValue::new(span, value.into()).into())
      }
      _ => unreachable!("fused input-value arm received a non-inline-string token"),
    };
  let block_string_head_arm =
    |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
     _: &mut GraphqlInput<'inp, '_, Src, Ctx>| match token {
      GraphqlToken::<'inp, Src>::LitBlockStr(value) => {
        Ok(StringValue::new(span, value.into()).into())
      }
      _ => unreachable!("fused input-value arm received a non-block-string token"),
    };
  let identifier_head_arm =
    |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
     _: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
      let keyword = token.downcast_ref();
      match token {
        GraphqlToken::<'inp, Src>::Identifier(value) => Ok(match keyword {
          Some(ContextualKeyword::True) => BooleanValue::new(span, true).into(),
          Some(ContextualKeyword::False) => BooleanValue::new(span, false).into(),
          Some(ContextualKeyword::Null) => NullValue::new(span, value).into(),
          _ => EnumValue::new(span, value).into(),
        }),
        _ => unreachable!("fused input-value arm received a non-identifier token"),
      }
    };
  let dollar_head_arm =
    |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| match token {
      GraphqlToken::<'inp, Src>::Dollar => variable_after_dollar(span.start(), inp).map(Into::into),
      _ => unreachable!("fused input-value arm received a non-dollar token"),
    };

  match (
    int_head_arm,
    float_head_arm,
    inline_string_head_arm,
    block_string_head_arm,
    identifier_head_arm,
    dollar_head_arm,
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
    ParseAttempt::Decline => {
      let off = *inp.offset();
      let list = {
        let mut peeked = inp.peek::<U1>()?;
        match peeked.pop_front() {
          Some(head) if matches!(head.token(), GraphqlToken::<'inp, Src>::LBracket) => true,
          Some(head) if matches!(head.token(), GraphqlToken::<'inp, Src>::LBrace) => false,
          Some(head) => {
            return Err(
              DialectGraphqlError::unexpected_token(
                head.token().kind(),
                Expectation::InputValue,
                *head.span(),
              )
              .into(),
            );
          }
          None => return Err(UnexpectedEot::eot_of(off).into()),
        }
      };

      if list {
        list_value_with::<Src, Ctx, N>(inp).map(Into::into)
      } else {
        object_value_with::<Src, Ctx, N>(inp).map(Into::into)
      }
    }
  }
});

numeric_value_parser!(
  const_value_with,
  inp,
  N::ConstValue,
  [contextual, delimited],
  {
    let int_head_arm =
      |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
       _: &mut GraphqlInput<'inp, '_, Src, Ctx>| match token {
        GraphqlToken::<'inp, Src>::LitInt(value) => match N::int(value) {
          Ok(payload) => Ok(IntValue::new(span, payload).into()),
          Err(err) => Err(N::report(err, span).into()),
        },
        _ => unreachable!("fused const-input-value arm received a non-int token"),
      };
    let float_head_arm =
      |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
       _: &mut GraphqlInput<'inp, '_, Src, Ctx>| match token {
        GraphqlToken::<'inp, Src>::LitFloat(value) => match N::float(value) {
          Ok(payload) => Ok(FloatValue::new(span, payload).into()),
          Err(err) => Err(N::report(err, span).into()),
        },
        _ => unreachable!("fused const-input-value arm received a non-float token"),
      };
    let inline_string_head_arm =
      |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
       _: &mut GraphqlInput<'inp, '_, Src, Ctx>| match token {
        GraphqlToken::<'inp, Src>::LitInlineStr(value) => {
          Ok(Into::into(StringValue::new(span, value.into())))
        }
        _ => unreachable!("fused const-input-value arm received a non-inline-string token"),
      };
    let block_string_head_arm =
      |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
       _: &mut GraphqlInput<'inp, '_, Src, Ctx>| match token {
        GraphqlToken::<'inp, Src>::LitBlockStr(value) => {
          Ok(Into::into(StringValue::new(span, value.into())))
        }
        _ => unreachable!("fused const-input-value arm received a non-block-string token"),
      };
    let identifier_head_arm =
      |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
       _: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        let keyword = token.downcast_ref();
        match token {
          GraphqlToken::<'inp, Src>::Identifier(value) => Ok(match keyword {
            Some(ContextualKeyword::True) => BooleanValue::new(span, true).into(),
            Some(ContextualKeyword::False) => BooleanValue::new(span, false).into(),
            Some(ContextualKeyword::Null) => NullValue::new(span, value).into(),
            _ => EnumValue::new(span, value).into(),
          }),
          _ => unreachable!("fused const-input-value arm received a non-identifier token"),
        }
      };

    match (
      int_head_arm,
      float_head_arm,
      inline_string_head_arm,
      block_string_head_arm,
      identifier_head_arm,
    )
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
      ParseAttempt::Decline => {
        let off = *inp.offset();
        let list = {
          let mut peeked = inp.peek::<U1>()?;
          match peeked.pop_front() {
            Some(head) if matches!(head.token(), GraphqlToken::<'inp, Src>::LBracket) => true,
            Some(head) if matches!(head.token(), GraphqlToken::<'inp, Src>::LBrace) => false,
            Some(head) => {
              return Err(
                DialectGraphqlError::unexpected_token(
                  head.token().kind(),
                  Expectation::ConstInputValue,
                  *head.span(),
                )
                .into(),
              );
            }
            None => return Err(UnexpectedEot::eot_of(off).into()),
          }
        };

        if list {
          const_list_value_with::<Src, Ctx, N>(inp).map(Into::into)
        } else {
          const_object_value_with::<Src, Ctx, N>(inp).map(Into::into)
        }
      }
    }
  }
);

value_parser!(
  pub(super) value,
  inp,
  InputValue<GraphqlSlice<'inp, Src>>,
  [contextual, delimited],
  { value_with::<Src, Ctx, SliceNumbers>(inp) }
);

value_parser!(
  pub(super) const_value,
  inp,
  ConstInputValue<GraphqlSlice<'inp, Src>>,
  [contextual, delimited],
  { const_value_with::<Src, Ctx, SliceNumbers>(inp) }
);

numeric_value_parser!(
  committed_default_value_with,
  inp,
  ValueDefault<GraphqlSlice<'inp, Src>, N>,
  [contextual, delimited],
  {
    let validated_const_tail = |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
      let off = *inp.offset();
      const_value_with::<Src, Ctx, N>
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
      .token_spanned()
      .map(|Spanned { span, data: value }| DefaultInputValueNode::new(span, value))
      .parse_input(inp)
  }
);

numeric_value_parser!(
  try_default_value_with,
  inp,
  ParseAttempt<ValueDefault<GraphqlSlice<'inp, Src>, N>>,
  [contextual, delimited],
  {
    committed_default_value_with::<Src, Ctx, N>
      .peek_then_try::<_, U1>(
        |mut peeked: Peeked<'_, 'inp, GraphqlLexer<'inp, Src>, U1>,
         _: EmitterView<'_, 'inp, GraphqlLexer<'inp, Src>, Ctx::Emitter, GraphQL>|
         -> Result<Action, GraphqlError<'inp, Src, Ctx>> {
          Ok(match peeked.pop_front() {
            Some(equal) if equal.token().is_equal() => Action::Continue,
            _ => Action::Stop,
          })
        },
      )
      .try_parse_input(inp)
  }
);

numeric_value_parser!(
  default_value_with,
  inp,
  Option<ValueDefault<GraphqlSlice<'inp, Src>, N>>,
  [contextual, delimited],
  { try_default_value_with::<Src, Ctx, N>(inp).map(Into::into) }
);

// No slice-payload wrapper for `committed_default_value_with`: it was private and reached only
// from `try_default_value`, which now reaches the generic form directly. A wrapper kept for
// symmetry would be dead code in every configuration.

value_parser!(
  pub try_default_value,
  inp,
  ParseAttempt<DefaultInputValue<GraphqlSlice<'inp, Src>>>,
  [contextual, delimited],
  { try_default_value_with::<Src, Ctx, SliceNumbers>(inp) }
);

value_parser!(
  pub default_value,
  inp,
  Option<DefaultInputValue<GraphqlSlice<'inp, Src>>>,
  [contextual, delimited],
  { default_value_with::<Src, Ctx, SliceNumbers>(inp) }
);

macro_rules! graphql_slice_api {
  ($slice:ident, $node:ty, $parse:ident, [], $spec:literal) => {
    graphql_slice_api!(@graphql $slice, $node, $parse, [], $spec);
  };
  ($slice:ident, $node:ty, $parse:ident, $try_parse:ident, [], $spec:literal) => {
    graphql_slice_api!(@graphql $slice, $node, $parse, [], $spec);
    graphql_slice_api!(@try $slice, $node, $try_parse, [], $spec);
  };
  ($slice:ident, $node:ty, $parse:ident, $try_parse:ident, [eot], $spec:literal) => {
    graphql_slice_api!(@graphql $slice, $node, $parse, [], $spec);
    graphql_slice_api!(
      @try $slice,
      $node,
      $try_parse,
      [
      ],
      $spec
    );
  };
  (
    $slice:ident,
    $node:ty,
    $parse:ident,
    $try_parse:ident,
    [contextual],
    $spec:literal
  ) => {
    graphql_slice_api!(
      @graphql $slice,
      $node,
      $parse,
      [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
      $spec
    );
    graphql_slice_api!(
      @try $slice,
      $node,
      $try_parse,
      [
        GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
      ],
      $spec
    );
  };
  (
    $slice:ident,
    $node:ty,
    $parse:ident,
    [contextual, delimited],
    $spec:literal
  ) => {
    graphql_slice_api!(
      @graphql $slice,
      $node,
      $parse,
      [
        GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
        GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<$slice>>,
      ],
      $spec
    );
  };
  (
    @graphql $slice:ident,
    $node:ty,
    $parse:ident,
    [$($bounds:tt)*],
    $spec:literal
  ) => {
    impl<$slice> $node {
      /// Parses this committed GraphQL production.
      ///
      /// The lexer source is inferred from `inp`.
      #[doc = $spec]
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
      {
        $parse(inp)
      }
    }
  };
  (
    @try $slice:ident,
    $node:ty,
    $try_parse:ident,
    [$($bounds:tt)*],
    $spec:literal
  ) => {
    impl<$slice> $node {
      /// Attempts this GraphQL production without consuming on a head mismatch.
      ///
      /// The lexer source is inferred from `inp`.
      #[doc = $spec]
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
      {
        $try_parse(inp)
      }
    }
  };
}

graphql_slice_api!(
  S,
  IntValue<S>,
  int_value,
  try_int_value,
  [],
  "See the [GraphQL Int Value specification](https://spec.graphql.org/draft/#sec-Int-Value)."
);
graphql_slice_api!(
  S,
  FloatValue<S>,
  float_value,
  try_float_value,
  [],
  "See the [GraphQL Float Value specification](https://spec.graphql.org/draft/#sec-Float-Value)."
);
graphql_slice_api!(
  S,
  StringValue<S>,
  string_value,
  try_string_value,
  [],
  "See the [GraphQL String Value specification](https://spec.graphql.org/draft/#sec-String-Value)."
);
graphql_slice_api!(
  S,
  BooleanValue<S>,
  boolean_value,
  try_boolean_value,
  [contextual],
  "See the [GraphQL Boolean Value specification](https://spec.graphql.org/draft/#sec-Boolean-Value)."
);
graphql_slice_api!(
  S,
  NullValue<S>,
  null_value,
  try_null_value,
  [contextual],
  "See the [GraphQL Null Value specification](https://spec.graphql.org/draft/#sec-Null-Value)."
);
graphql_slice_api!(
  S,
  EnumValue<S>,
  enum_value,
  try_enum_value,
  [contextual],
  "See the [GraphQL Enum Value specification](https://spec.graphql.org/draft/#sec-Enum-Value)."
);
graphql_slice_api!(
  S,
  VariableValue<S>,
  variable_value,
  try_variable_value,
  [eot],
  "See the [GraphQL Variables specification](https://spec.graphql.org/draft/#sec-Language.Variables)."
);
graphql_slice_api!(
  S,
  AstList<S>,
  list_value,
  [contextual, delimited],
  "See the [GraphQL List Value specification](https://spec.graphql.org/draft/#sec-List-Value)."
);
graphql_slice_api!(
  S,
  AstConstList<S>,
  const_list_value,
  [contextual, delimited],
  "See the [GraphQL List Value specification](https://spec.graphql.org/draft/#sec-List-Value)."
);
graphql_slice_api!(
  S,
  ObjectField<S>,
  object_field,
  [contextual, delimited],
  "See the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values)."
);
graphql_slice_api!(
  S,
  ConstObjectField<S>,
  const_object_field,
  [contextual, delimited],
  "See the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values)."
);
graphql_slice_api!(
  S,
  AstObject<S>,
  object_value,
  [contextual, delimited],
  "See the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values)."
);
graphql_slice_api!(
  S,
  AstConstObject<S>,
  const_object_value,
  [contextual, delimited],
  "See the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values)."
);
graphql_slice_api!(
  S,
  InputValue<S>,
  value,
  [contextual, delimited],
  "See the [GraphQL Input Values specification](https://spec.graphql.org/draft/#sec-Input-Values)."
);
graphql_slice_api!(
  S,
  ConstInputValue<S>,
  const_value,
  [contextual, delimited],
  "See the [GraphQL Input Values specification](https://spec.graphql.org/draft/#sec-Input-Values)."
);

impl<S> DefaultInputValue<S> {
  /// Parses an optional GraphQL default value.
  ///
  /// The lexer source is inferred from `inp`.
  ///
  /// See the [GraphQL Input Value Definitions specification](https://spec.graphql.org/draft/#sec-Input-Value-Definitions).
  pub fn graphql<'inp, Src, Ctx>(
    inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  ) -> Result<Option<Self>, GraphqlError<'inp, Src, Ctx>>
  where
    Src: Source<usize, Slice<'inp> = S> + ?Sized,
    S: Slice<'inp> + Clone + 'inp,
    GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
    GraphqlLexer<'inp, Src>:
      Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
    GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<S>>,
  {
    default_value(inp)
  }

  /// Attempts a GraphQL default value without consuming when `=` is absent.
  ///
  /// The lexer source is inferred from `inp`.
  ///
  /// See the [GraphQL Input Value Definitions specification](https://spec.graphql.org/draft/#sec-Input-Value-Definitions).
  pub fn try_graphql<'inp, Src, Ctx>(
    inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  ) -> Result<ParseAttempt<Self>, GraphqlError<'inp, Src, Ctx>>
  where
    Src: Source<usize, Slice<'inp> = S> + ?Sized,
    S: Slice<'inp> + Clone + 'inp,
    GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
    GraphqlLexer<'inp, Src>:
      Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
    GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<S>>,
  {
    try_default_value(inp)
  }
}

/// The materialised-number instantiation of every production in this module, at [`i64`] — the
/// reading that accepts every literal the grammar admits.
#[cfg(feature = "materialized-numbers")]
#[cfg_attr(docsrs, doc(cfg(feature = "materialized-numbers")))]
pub mod materialized;

/// The same instantiation at [`i32`], the width draft §3.5.1 specifies `Int` to be.
#[cfg(feature = "materialized-numbers")]
#[cfg_attr(docsrs, doc(cfg(feature = "materialized-numbers")))]
pub mod materialized32;

// `pub(crate)` rather than private for one reason, and it is a correctness one:
// `IntOverflow::checked` in [`graphql::error`](crate::graphql::error) has to decide whether a
// literal really is out of range at the width a caller named, and the only honest decider is the
// reader the productions here already use. A second reader written in `error.rs` could disagree
// with this one, and the constructor's promise would then be about a function nobody calls.
pub(crate) mod numbers;

#[cfg(test)]
mod tests;
