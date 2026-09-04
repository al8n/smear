//! GraphQLx value productions.
//!
//! Scalar heads are dispatched from one consumed token. `set` and `map` remain
//! ordinary path segments unless their immediately following token is `{`, at
//! which point they become collection constructors.
//!
//! GraphQLx retains GraphQL input values while extending enum values to paths
//! and adding `set` and `map` collection values.
//!
//! See the [GraphQL Input Values specification](https://spec.graphql.org/draft/#sec-Input-Values).

use tokora::{
  Accumulator, EmitterView, Lexer, ParseInput, ParseTokenChoice, SimpleSpan, Slice, Source, Token,
  TryParseInput,
  cache::{Peeked, PeekedTokenExt},
  parser::Action,
  span::Spanned,
  try_parse_input::ParseAttempt,
  utils::{DowncastRef, typenum::U1},
};

use smear_lexer::graphqlx::{ContextualKeyword, syntactic::SyntacticTokenKind};

use super::{
  GraphqlxError, GraphqlxInput, GraphqlxLexer, GraphqlxSlice, GraphqlxToken, keyword_of, path,
  path_after_first, unexpected_here,
};
use crate::{
  combinator::{
    ParseCtx, TokenSpannedExt, colon, dollar, extent_since, extent_start, fat_arrow, try_equal,
  },
  graphqlx::{
    GraphQLx,
    ast::{
      BooleanValue, ConstInputValue, ConstList, ConstMap, ConstMapEntry, ConstObject,
      ConstObjectField, ConstSet, DefaultInputValue, EnumValue, FloatValue, InlineStringValue,
      InputValue, IntValue, List, Map, MapEntry, NullValue, Object, ObjectField, Set, StringValue,
      VariableValue,
    },
    error::{Expectation, GraphqlxError as DialectGraphqlxError},
  },
};

macro_rules! value_parser {
  ($(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty, [], $body:block) => {
    value_parser!(@impl $(#[$meta])* $visibility $name, $input, $output, [], $body);
  };
  ($(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty, [contextual], $body:block) => {
    value_parser!(
      @impl $(#[$meta])* $visibility $name,
      $input,
      $output,
      [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
      $body
    );
  };
  ($(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty, [punctuated], $body:block) => {
    value_parser!(
      @impl $(#[$meta])* $visibility $name,
      $input,
      $output,
      [
      ],
      $body
    );
  };
  ($(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty, [path], $body:block) => {
    value_parser!(
      @impl $(#[$meta])* $visibility $name,
      $input,
      $output,
      [
        GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
      ],
      $body
    );
  };
  ($(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty, [complex], $body:block) => {
    value_parser!(
      @impl $(#[$meta])* $visibility $name,
      $input,
      $output,
      [
        GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
      ],
      $body
    );
  };
  (
    @impl $(#[$meta:meta])* $visibility:vis $name:ident,
    $input:ident,
    $output:ty,
    [$($bounds:tt)*],
    $body:block
  ) => {
    $(#[$meta])*
    #[doc = "Parses this committed GraphQLx value production."]
    $visibility fn $name<'inp, Src, Ctx>(
      $input: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
    ) -> Result<$output, GraphqlxError<'inp, Src, Ctx>>
    where
      Src: Source<usize> + ?Sized,
      GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
      GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
      GraphqlxLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlxToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
      >,
      $($bounds)*
      Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
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
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  Err(DialectGraphqlxError::unexpected_token(token.kind(), expected, span).into())
}

value_parser!(int_value, inp, IntValue<GraphqlxSlice<'inp, Src>>, [], {
  match inp.next_or_stop()? {
    Some(Spanned {
      span,
      data: GraphqlxToken::<'inp, Src>::LitInt(value),
    }) => Ok(IntValue::new(span, value)),
    Some(Spanned { span, data: token }) => {
      unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::IntValue)
    }
    None => unexpected_here(inp, Expectation::IntValue),
  }
});

value_parser!(
  float_value,
  inp,
  FloatValue<GraphqlxSlice<'inp, Src>>,
  [],
  {
    match inp.next_or_stop()? {
      Some(Spanned {
        span,
        data: GraphqlxToken::<'inp, Src>::LitFloat(value),
      }) => Ok(FloatValue::new(span, value)),
      Some(Spanned { span, data: token }) => {
        unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::FloatValue)
      }
      None => unexpected_here(inp, Expectation::FloatValue),
    }
  }
);

value_parser!(
  /// Parses a committed GraphQLx string value.
  ///
  /// GraphQLx retains GraphQL inline and block string forms.
  ///
  /// See the [GraphQL String Value specification](https://spec.graphql.org/draft/#sec-String-Value).
  pub string_value,
  inp,
  StringValue<GraphqlxSlice<'inp, Src>>,
  [],
  {
    match inp.next_or_stop()? {
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
      None => unexpected_here(inp, Expectation::StringValue),
    }
  }
);

pub(crate) fn inline_string_value<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<InlineStringValue<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match inp.next_or_stop()? {
    Some(Spanned {
      span,
      data: GraphqlxToken::<'inp, Src>::LitInlineStr(value),
    }) => Ok(InlineStringValue::new(span, value)),
    Some(Spanned { span, data: token }) => {
      unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::InlineString)
    }
    None => unexpected_here(inp, Expectation::InlineString),
  }
}

value_parser!(
  boolean_value,
  inp,
  BooleanValue<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    match inp.next_or_stop()? {
      Some(Spanned { span, data: token }) => match keyword_of(&token) {
        Some(ContextualKeyword::True) => Ok(BooleanValue::new(span, true)),
        Some(ContextualKeyword::False) => Ok(BooleanValue::new(span, false)),
        _ => unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::BooleanValue),
      },
      None => unexpected_here(inp, Expectation::BooleanValue),
    }
  }
);

value_parser!(
  null_value,
  inp,
  NullValue<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    match inp.next_or_stop()? {
      Some(Spanned { span, data: token }) => match keyword_of(&token) {
        Some(ContextualKeyword::Null) => match token {
          GraphqlxToken::<'inp, Src>::Identifier(value) => Ok(NullValue::new(span, value)),
          _ => unreachable!("the contextual GraphQLx null keyword is always an identifier"),
        },
        _ => unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::NullValue),
      },
      None => unexpected_here(inp, Expectation::NullValue),
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
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  path_after_first(start, first, fully_qualified, inp)
    .map(|path| EnumValue::new(*path.span(), path))
}

value_parser!(
  /// Parses a committed GraphQLx enum/path value.
  ///
  /// GraphQLx extends GraphQL's `EnumValue` from a single name to an optional
  /// qualified path, while retaining GraphQL's reserved `true`, `false`, and
  /// `null` exclusions.
  ///
  /// See the [GraphQL Enum Value specification](https://spec.graphql.org/draft/#sec-Enum-Value).
  pub enum_value,
  inp,
  EnumValue<GraphqlxSlice<'inp, Src>>,
  [path],
  {
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
        _ => unreachable!("fused GraphQLx enum dispatch received a non-identifier token"),
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
      _ => unexpected_here(inp, Expectation::EnumValue),
    },
  }
  }
);

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
      .token_spanned()
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
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
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

/// Returns the deterministic FIRST-set category for a GraphQLx value token.
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
  _: EmitterView<'_, 'inp, GraphqlxLexer<'inp, Src>, Ctx::Emitter, GraphQLx>,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  Ok(match peeked.pop_front() {
    Some(token) if value_head(token.token()).is_some() => Action::Continue,
    _ => Action::Stop,
  })
}

fn decide_const_value_head<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>,
  _: EmitterView<'_, 'inp, GraphqlxLexer<'inp, Src>, Ctx::Emitter, GraphQLx>,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  Ok(match peeked.pop_front() {
    Some(token) if const_value_head(token.token()) => Action::Continue,
    _ => Action::Stop,
  })
}

fn decide_brace_member<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>,
  _: EmitterView<'_, 'inp, GraphqlxLexer<'inp, Src>, Ctx::Emitter, GraphQLx>,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  Ok(match peeked.pop_front() {
    Some(token) if matches!(token.token(), GraphqlxToken::<'inp, Src>::RBrace) => Action::Stop,
    Some(_) => Action::Continue,
    None => Action::Stop,
  })
}

value_parser!(
  /// Parses a committed executable GraphQLx list value.
  ///
  /// The list delimiters follow GraphQL; each element may use GraphQLx input
  /// values.
  ///
  /// See the [GraphQL List Value specification](https://spec.graphql.org/draft/#sec-List-Value).
  pub list_value,
  inp,
  List<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| value(inp))
    .repeated_while::<_, U1>(decide_value_head::<_, Ctx>)
    .delimited_by_brackets()
    .collect_with(Default::default())
    .token_spanned()
    .parse_input(inp)
    .map(|Spanned { span, data }| List::new(span, data))
  }
);

value_parser!(
  /// Parses a committed constant GraphQLx list value.
  ///
  /// The list delimiters follow GraphQL; elements use GraphQLx constant input
  /// values and therefore reject variables.
  ///
  /// See the [GraphQL List Value specification](https://spec.graphql.org/draft/#sec-List-Value).
  pub const_list_value,
  inp,
  ConstList<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| const_value(inp))
    .repeated_while::<_, U1>(decide_const_value_head::<_, Ctx>)
    .delimited_by_brackets()
    .collect_with(Default::default())
    .token_spanned()
    .parse_input(inp)
    .map(|Spanned { span, data }| ConstList::new(span, data))
  }
);

value_parser!(
  /// Parses a committed executable GraphQLx input-object field.
  ///
  /// The `Name ':' Value` field shape follows GraphQL; values may use GraphQLx
  /// input-value extensions.
  ///
  /// See the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values).
  pub object_field,
  inp,
  ObjectField<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
  super::name
    .then_ignore(colon)
    .then(value)
    .token_spanned()
    .map(|Spanned { span, data: (name, value) }| ObjectField::new(span, name, value))
    .parse_input(inp)
  }
);

value_parser!(
  /// Parses a committed constant GraphQLx input-object field.
  ///
  /// The field shape follows GraphQL; its GraphQLx constant input value rejects
  /// variables.
  ///
  /// See the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values).
  pub const_object_field,
  inp,
  ConstObjectField<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
  super::name
    .then_ignore(colon)
    .then(const_value)
    .token_spanned()
    .map(|Spanned { span, data: (name, value) }| ConstObjectField::new(span, name, value))
    .parse_input(inp)
  }
);

value_parser!(
  /// Parses a committed executable GraphQLx input-object value.
  ///
  /// The object delimiters and fields follow GraphQL; field values may use
  /// GraphQLx input-value extensions.
  ///
  /// See the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values).
  pub object_value,
  inp,
  Object<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| object_field(inp))
    .repeated_while::<_, U1>(decide_brace_member::<_, Ctx>)
    .delimited_by_braces()
    .collect_with(Default::default())
    .token_spanned()
    .parse_input(inp)
    .map(|Spanned { span, data }| Object::new(span, data))
  }
);

value_parser!(
  /// Parses a committed constant GraphQLx input-object value.
  ///
  /// The object delimiters and fields follow GraphQL; field values use GraphQLx
  /// constant input values and reject variables.
  ///
  /// See the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values).
  pub const_object_value,
  inp,
  ConstObject<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| const_object_field(inp))
    .repeated_while::<_, U1>(decide_brace_member::<_, Ctx>)
    .delimited_by_braces()
    .collect_with(Default::default())
    .token_spanned()
    .parse_input(inp)
    .map(|Spanned { span, data }| ConstObject::new(span, data))
  }
);

value_parser!(
  /// Parses a committed executable GraphQLx map entry (`Value => Value`).
  ///
  /// Map entries are a GraphQLx dialect extension with no corresponding
  /// GraphQL draft production.
  pub map_entry,
  inp,
  MapEntry<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
    value
      .then_ignore(fat_arrow)
      .then(value)
      .token_spanned()
      .map(
        |Spanned {
           span,
           data: (key, value),
         }| MapEntry::new(span, key, value),
      )
      .parse_input(inp)
  }
);

value_parser!(
  /// Parses a committed constant GraphQLx map entry (`ConstValue => ConstValue`).
  ///
  /// Map entries are a GraphQLx dialect extension with no corresponding
  /// GraphQL draft production.
  pub const_map_entry,
  inp,
  ConstMapEntry<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
    const_value
      .then_ignore(fat_arrow)
      .then(const_value)
      .token_spanned()
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
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| value(inp))
    .repeated_while::<_, U1>(decide_brace_member::<_, Ctx>)
    .delimited_by_braces()
    .collect_with(Default::default())
    .token_spanned()
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
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| const_value(inp))
    .repeated_while::<_, U1>(decide_brace_member::<_, Ctx>)
    .delimited_by_braces()
    .collect_with(Default::default())
    .token_spanned()
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
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| map_entry(inp))
    .repeated_while::<_, U1>(decide_brace_member::<_, Ctx>)
    .delimited_by_braces()
    .collect_with(Default::default())
    .token_spanned()
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
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| const_map_entry(inp))
    .repeated_while::<_, U1>(decide_brace_member::<_, Ctx>)
    .delimited_by_braces()
    .collect_with(Default::default())
    .token_spanned()
    .parse_input(inp)
    .map(|Spanned { span, data }| ConstMap::new(SimpleSpan::new(start, span.end()), data))
}

value_parser!(
  /// Parses a committed executable GraphQLx input value.
  ///
  /// This retains GraphQL input values while extending enum values to paths and
  /// adding `set` and `map` collection values.
  ///
  /// See the [GraphQL Input Values specification](https://spec.graphql.org/draft/#sec-Input-Values).
  pub value,
  inp,
  InputValue<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
  let int_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     _: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::LitInt(value) => Ok(InputValue::Int(IntValue::new(span, value))),
      _ => unreachable!("fused GraphQLx value dispatch received a non-integer token"),
    };
  let float_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     _: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::LitFloat(value) => Ok(InputValue::Float(FloatValue::new(span, value))),
      _ => unreachable!("fused GraphQLx value dispatch received a non-float token"),
    };
  let inline_string_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     _: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::LitInlineStr(value) => Ok(InputValue::String(StringValue::new(span, value.into()))),
      _ => unreachable!("fused GraphQLx value dispatch received a non-inline-string token"),
    };
  let block_string_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     _: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::LitBlockStr(value) => Ok(InputValue::String(StringValue::new(span, value.into()))),
      _ => unreachable!("fused GraphQLx value dispatch received a non-block-string token"),
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
          if matches!(super::peek_kind(inp)?, Some(SyntacticTokenKind::LBrace)) {
            set_after_keyword(span.start(), inp).map(InputValue::Set)
          } else {
            enum_after_first(span.start(), super::ast::Name::new(span, source), false, inp)
              .map(InputValue::Enum)
          }
        }
        (GraphqlxToken::<'inp, Src>::Identifier(source), Some(ContextualKeyword::Map)) => {
          if matches!(super::peek_kind(inp)?, Some(SyntacticTokenKind::LBrace)) {
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
        _ => unreachable!("fused GraphQLx value dispatch received a non-identifier token"),
      }
    };
  let dollar_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::Dollar => variable_after_dollar(span.start(), inp).map(InputValue::Variable),
      _ => unreachable!("fused GraphQLx value dispatch received a non-dollar token"),
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
      _ => unexpected_here(inp, Expectation::InputValue),
    },
  }
  }
);

value_parser!(
  /// Parses a committed constant GraphQLx input value.
  ///
  /// This retains GraphQL constant input values while extending enum values to
  /// paths and adding constant `set` and `map` collection values.
  ///
  /// See the [GraphQL Input Values specification](https://spec.graphql.org/draft/#sec-Input-Values).
  pub const_value,
  inp,
  ConstInputValue<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
  let int_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     _: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::LitInt(value) => Ok(ConstInputValue::Int(IntValue::new(span, value))),
      _ => unreachable!("fused GraphQLx const-value dispatch received a non-integer token"),
    };
  let float_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     _: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::LitFloat(value) => Ok(ConstInputValue::Float(FloatValue::new(span, value))),
      _ => unreachable!("fused GraphQLx const-value dispatch received a non-float token"),
    };
  let inline_string_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     _: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::LitInlineStr(value) => Ok(ConstInputValue::String(StringValue::new(span, value.into()))),
      _ => unreachable!("fused GraphQLx const-value dispatch received a non-inline-string token"),
    };
  let block_string_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     _: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::LitBlockStr(value) => Ok(ConstInputValue::String(StringValue::new(span, value.into()))),
      _ => unreachable!("fused GraphQLx const-value dispatch received a non-block-string token"),
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
          if matches!(super::peek_kind(inp)?, Some(SyntacticTokenKind::LBrace)) {
            const_set_after_keyword(span.start(), inp).map(ConstInputValue::Set)
          } else {
            enum_after_first(span.start(), super::ast::Name::new(span, source), false, inp)
              .map(ConstInputValue::Enum)
          }
        }
        (GraphqlxToken::<'inp, Src>::Identifier(source), Some(ContextualKeyword::Map)) => {
          if matches!(super::peek_kind(inp)?, Some(SyntacticTokenKind::LBrace)) {
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
        _ => unreachable!("fused GraphQLx const-value dispatch received a non-identifier token"),
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
      _ => unexpected_here(inp, Expectation::ConstInputValue),
    },
  }
  }
);

/// Attempts an optional GraphQLx default assignment without consuming when
/// `=` is absent. Once the assignment opener is present, its constant value is
/// committed and receives the normal constant-value diagnostic.
///
/// GraphQLx follows GraphQL's default-value assignment shape while admitting
/// GraphQLx constant input values.
///
/// See the [GraphQL Input Value Definitions specification](https://spec.graphql.org/draft/#sec-Input-Value-Definitions).
pub fn try_default_value<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ParseAttempt<DefaultInputValue<GraphqlxSlice<'inp, Src>>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let node_start = extent_start(inp)?;
  match try_equal(inp)? {
    ParseAttempt::Accept(_) => {
      let value = const_value(inp)?;
      Ok(ParseAttempt::Accept(DefaultInputValue::new(
        extent_since(inp, node_start),
        value,
      )))
    }
    ParseAttempt::Decline => Ok(ParseAttempt::Decline),
  }
}

/// Parses an optional GraphQLx default assignment.
///
/// GraphQLx follows GraphQL's default-value assignment shape while admitting
/// GraphQLx constant input values.
///
/// See the [GraphQL Input Value Definitions specification](https://spec.graphql.org/draft/#sec-Input-Value-Definitions).
pub fn default_value<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Option<DefaultInputValue<GraphqlxSlice<'inp, Src>>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  try_default_value(inp).map(Into::into)
}

macro_rules! graphqlx_value_api {
  ($(#[$meta:meta])* $node:ty, $parse:ident, []) => {
    graphqlx_value_api!(@impl $(#[$meta])* $node, $parse, []);
  };
  ($(#[$meta:meta])* $node:ty, $parse:ident, [contextual]) => {
    graphqlx_value_api!(
      @impl $(#[$meta])* $node,
      $parse,
      [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
    );
  };
  ($(#[$meta:meta])* $node:ty, $parse:ident, [punctuated]) => {
    graphqlx_value_api!(
      @impl $(#[$meta])* $node,
      $parse,
      [
      ]
    );
  };
  ($(#[$meta:meta])* $node:ty, $parse:ident, [path]) => {
    graphqlx_value_api!(
      @impl $(#[$meta])* $node,
      $parse,
      [
        GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
      ]
    );
  };
  ($(#[$meta:meta])* $node:ty, $parse:ident, [complex]) => {
    graphqlx_value_api!(
      @impl $(#[$meta])* $node,
      $parse,
      [
        GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
      ]
    );
  };
  (@impl $(#[$meta:meta])* $node:ty, $parse:ident, [$($bounds:tt)*]) => {
    impl<S> $node {
      $(#[$meta])*
      /// Parses this committed GraphQLx value production.
      pub fn graphqlx<'inp, Src, Ctx>(
        inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
      ) -> Result<Self, GraphqlxError<'inp, Src, Ctx>>
      where
        Src: Source<usize, Slice<'inp> = S> + ?Sized,
        S: Slice<'inp> + Clone + 'inp,
        GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
        GraphqlxLexer<'inp, Src>: Lexer<
            'inp,
            Source = Src,
            Token = GraphqlxToken<'inp, Src>,
            Span = SimpleSpan,
            Offset = usize,
          >,
        $($bounds)*
        Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
        GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<S>>,
      {
        $parse(inp)
      }
    }
  };
}

graphqlx_value_api!(
  /// Parses a committed GraphQLx integer value.
  ///
  /// See the [GraphQL Int Value specification](https://spec.graphql.org/draft/#sec-Int-Value).
  IntValue<S>,
  int_value,
  []
);
graphqlx_value_api!(
  /// Parses a committed GraphQLx floating-point value.
  ///
  /// See the [GraphQL Float Value specification](https://spec.graphql.org/draft/#sec-Float-Value).
  FloatValue<S>,
  float_value,
  []
);
graphqlx_value_api!(
  /// Parses a committed GraphQLx string value.
  ///
  /// See [`string_value`] and the [GraphQL String Value specification](https://spec.graphql.org/draft/#sec-String-Value).
  StringValue<S>,
  string_value,
  []
);
graphqlx_value_api!(
  /// Parses a committed GraphQLx boolean value.
  ///
  /// See the [GraphQL Boolean Value specification](https://spec.graphql.org/draft/#sec-Boolean-Value).
  BooleanValue<S>,
  boolean_value,
  [contextual]
);
graphqlx_value_api!(
  /// Parses a committed GraphQLx null value.
  ///
  /// See the [GraphQL Null Value specification](https://spec.graphql.org/draft/#sec-Null-Value).
  NullValue<S>,
  null_value,
  [contextual]
);
graphqlx_value_api!(
  /// Parses a committed GraphQLx enum/path value.
  ///
  /// GraphQLx extends GraphQL enum values with qualified paths.
  ///
  /// See [`enum_value`] and the [GraphQL Enum Value specification](https://spec.graphql.org/draft/#sec-Enum-Value).
  EnumValue<S>,
  enum_value,
  [path]
);
graphqlx_value_api!(
  /// Parses a committed GraphQLx variable value.
  ///
  /// See the [GraphQL Variables specification](https://spec.graphql.org/draft/#sec-Language.Variables).
  VariableValue<S>,
  variable_value,
  [punctuated]
);
graphqlx_value_api!(
  /// Parses a committed executable GraphQLx list value.
  ///
  /// See [`list_value`] and the [GraphQL List Value specification](https://spec.graphql.org/draft/#sec-List-Value).
  List<S>,
  list_value,
  [complex]
);
graphqlx_value_api!(
  /// Parses a committed constant GraphQLx list value.
  ///
  /// See [`const_list_value`] and the [GraphQL List Value specification](https://spec.graphql.org/draft/#sec-List-Value).
  ConstList<S>,
  const_list_value,
  [complex]
);
graphqlx_value_api!(
  /// Parses a committed executable GraphQLx set value.
  ///
  /// `set { ... }` is a GraphQLx dialect extension with no corresponding
  /// GraphQL draft production.
  Set<S>,
  set_value,
  [complex]
);
graphqlx_value_api!(
  /// Parses a committed constant GraphQLx set value.
  ///
  /// `set { ... }` is a GraphQLx dialect extension with no corresponding
  /// GraphQL draft production.
  ConstSet<S>,
  const_set_value,
  [complex]
);
graphqlx_value_api!(
  /// Parses a committed executable GraphQLx map entry.
  ///
  /// `Value => Value` is a GraphQLx dialect extension with no corresponding
  /// GraphQL draft production.
  MapEntry<S>,
  map_entry,
  [complex]
);
graphqlx_value_api!(
  /// Parses a committed constant GraphQLx map entry.
  ///
  /// `ConstValue => ConstValue` is a GraphQLx dialect extension with no
  /// corresponding GraphQL draft production.
  ConstMapEntry<S>,
  const_map_entry,
  [complex]
);
graphqlx_value_api!(
  /// Parses a committed executable GraphQLx map value.
  ///
  /// `map { ... }` is a GraphQLx dialect extension with no corresponding
  /// GraphQL draft production.
  Map<S>,
  map_value,
  [complex]
);
graphqlx_value_api!(
  /// Parses a committed constant GraphQLx map value.
  ///
  /// `map { ... }` is a GraphQLx dialect extension with no corresponding
  /// GraphQL draft production.
  ConstMap<S>,
  const_map_value,
  [complex]
);
graphqlx_value_api!(
  /// Parses a committed executable GraphQLx input-object field.
  ///
  /// See [`object_field`] and the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values).
  ObjectField<S>,
  object_field,
  [complex]
);
graphqlx_value_api!(
  /// Parses a committed constant GraphQLx input-object field.
  ///
  /// See [`const_object_field`] and the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values).
  ConstObjectField<S>,
  const_object_field,
  [complex]
);
graphqlx_value_api!(
  /// Parses a committed executable GraphQLx input-object value.
  ///
  /// See [`object_value`] and the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values).
  Object<S>,
  object_value,
  [complex]
);
graphqlx_value_api!(
  /// Parses a committed constant GraphQLx input-object value.
  ///
  /// See [`const_object_value`] and the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values).
  ConstObject<S>,
  const_object_value,
  [complex]
);
graphqlx_value_api!(
  /// Parses a committed executable GraphQLx input value.
  ///
  /// GraphQLx extends GraphQL input values with enum paths and `set`/`map`
  /// collection values.
  ///
  /// See [`value`] and the [GraphQL Input Values specification](https://spec.graphql.org/draft/#sec-Input-Values).
  InputValue<S>,
  value,
  [complex]
);
graphqlx_value_api!(
  /// Parses a committed constant GraphQLx input value.
  ///
  /// GraphQLx extends GraphQL constant input values with enum paths and
  /// `set`/`map` collection values.
  ///
  /// See [`const_value`] and the [GraphQL Input Values specification](https://spec.graphql.org/draft/#sec-Input-Values).
  ConstInputValue<S>,
  const_value,
  [complex]
);

impl<S> DefaultInputValue<S> {
  /// Parses an optional GraphQLx default assignment.
  ///
  /// The lexer source is inferred from `inp`.
  ///
  /// GraphQLx admits extended constant input values after the `=` opener.
  ///
  /// See [`default_value`] and the [GraphQL Input Value Definitions specification](https://spec.graphql.org/draft/#sec-Input-Value-Definitions).
  pub fn graphqlx<'inp, Src, Ctx>(
    inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  ) -> Result<Option<Self>, GraphqlxError<'inp, Src, Ctx>>
  where
    Src: Source<usize, Slice<'inp> = S> + ?Sized,
    S: Slice<'inp> + Clone + 'inp,
    GraphqlxToken<'inp, Src>:
      Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
    GraphqlxLexer<'inp, Src>: Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
    GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<S>>,
  {
    default_value(inp)
  }

  /// Attempts a GraphQLx default assignment without consuming when `=` is absent.
  ///
  /// GraphQLx admits extended constant input values after the `=` opener.
  ///
  /// See [`try_default_value`] and the [GraphQL Input Value Definitions specification](https://spec.graphql.org/draft/#sec-Input-Value-Definitions).
  pub fn try_graphqlx<'inp, Src, Ctx>(
    inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  ) -> Result<ParseAttempt<Self>, GraphqlxError<'inp, Src, Ctx>>
  where
    Src: Source<usize, Slice<'inp> = S> + ?Sized,
    S: Slice<'inp> + Clone + 'inp,
    GraphqlxToken<'inp, Src>:
      Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
    GraphqlxLexer<'inp, Src>: Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
    GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<S>>,
  {
    try_default_value(inp)
  }
}

value_parser!(
  /// Parses a committed executable GraphQLx set value (`set { Value* }`).
  ///
  /// Set values are a GraphQLx dialect extension with no corresponding GraphQL
  /// draft production.
  pub set_value,
  inp,
  Set<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
    match inp.next_or_stop()? {
      Some(Spanned { span, data: token }) if keyword_of(&token) == Some(ContextualKeyword::Set) => {
        if super::peek_kind(inp)?.is_none() {
          return unexpected_here(inp, Expectation::LBrace);
        }
        set_after_keyword(span.start(), inp)
      }
      Some(Spanned { span, data: token }) => {
        unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::Keyword("set"))
      }
      None => unexpected_here(inp, Expectation::Keyword("set")),
    }
  }
);

value_parser!(
  /// Parses a committed constant GraphQLx set value (`set { ConstValue* }`).
  ///
  /// Set values are a GraphQLx dialect extension with no corresponding GraphQL
  /// draft production.
  pub const_set_value,
  inp,
  ConstSet<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
    match inp.next_or_stop()? {
      Some(Spanned { span, data: token }) if keyword_of(&token) == Some(ContextualKeyword::Set) => {
        if super::peek_kind(inp)?.is_none() {
          return unexpected_here(inp, Expectation::LBrace);
        }
        const_set_after_keyword(span.start(), inp)
      }
      Some(Spanned { span, data: token }) => {
        unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::Keyword("set"))
      }
      None => unexpected_here(inp, Expectation::Keyword("set")),
    }
  }
);

value_parser!(
  /// Parses a committed executable GraphQLx map value (`map { Value => Value }`).
  ///
  /// Map values are a GraphQLx dialect extension with no corresponding GraphQL
  /// draft production.
  pub map_value,
  inp,
  Map<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
    match inp.next_or_stop()? {
      Some(Spanned { span, data: token }) if keyword_of(&token) == Some(ContextualKeyword::Map) => {
        if super::peek_kind(inp)?.is_none() {
          return unexpected_here(inp, Expectation::LBrace);
        }
        map_after_keyword(span.start(), inp)
      }
      Some(Spanned { span, data: token }) => {
        unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::Keyword("map"))
      }
      None => unexpected_here(inp, Expectation::Keyword("map")),
    }
  }
);

value_parser!(
  /// Parses a committed constant GraphQLx map value (`map { ConstValue => ConstValue }`).
  ///
  /// Map values are a GraphQLx dialect extension with no corresponding GraphQL
  /// draft production.
  pub const_map_value,
  inp,
  ConstMap<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
    match inp.next_or_stop()? {
      Some(Spanned { span, data: token }) if keyword_of(&token) == Some(ContextualKeyword::Map) => {
        if super::peek_kind(inp)?.is_none() {
          return unexpected_here(inp, Expectation::LBrace);
        }
        const_map_after_keyword(span.start(), inp)
      }
      Some(Spanned { span, data: token }) => {
        unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::Keyword("map"))
      }
      None => unexpected_here(inp, Expectation::Keyword("map")),
    }
  }
);
