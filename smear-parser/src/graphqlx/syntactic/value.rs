//! GraphQLx value productions.
//!
//! Scalar heads are dispatched from one consumed token. `set` and `map` remain
//! ordinary path segments unless their immediately following token is `!`, at
//! which point they become collection constructors and a `{` must follow the
//! `!`.
//!
//! GraphQLx retains GraphQL input values while extending enum values to paths
//! and adding `set` and `map` collection values and a `char` literal, `'a'`,
//! which every value grammar reads where GraphQL reads a scalar.
//!
//! A schema position, a constant position of a type-system definition or extension, reads two
//! forms more. A path followed by `{` is a typed object, `Point { x: 1 }`, and a path followed by
//! `(` is a typed value, `Url("x")`, each with type arguments between when `<` follows the path.
//! So in a schema position `set { a: 1 }` is a typed object whose type is named `set`, and
//! `[RED {a: 1}]` is a list of one element. An executable document reads neither form, and there
//! the same texts are a path and then an object.
//!
//! A schema position reads a `const { … }` block too, when `const` is followed by `{`. Its
//! production is the one that switches the lexer into the block's mode, at the `{`, and back, at
//! the `}`; inside, a block holds one expression, whose operand may be a parenthesis, a prefixed
//! string `b"…"` or `i"…"`, a block inside the block, which switches nothing, or a list, a set, a
//! map, an object or a typed form, whose members a `,` separates and each of which is an
//! expression. Outside a block an expression is refused at its first token, against the block it
//! offers: an operator's token or a `(` at a value's head, and an operator's token, a `>` or a `^`
//! after a value where no second value may follow.
//!
//! See the [GraphQL Input Values specification](https://spec.graphql.org/draft/#sec-Input-Values).

use std::boxed::Box;

use tokora::{
  Accumulator, Action, EmitterView, Lexer, ParseAttempt, ParseInput, ParseTokenChoice, SimpleSpan,
  Slice, Source, Token, TryParseInput,
  cache::{Peeked, PeekedTokenExt},
  combinator::parens,
  span::Spanned,
  utils::{DowncastRef, IntoComponents, typenum::U1},
};

use smear_lexer::graphqlx::{ContextualKeyword, StrPrefix, syntactic::SyntacticTokenKind};

use super::{
  GraphqlxError, GraphqlxInput, GraphqlxLexer, GraphqlxSlice, GraphqlxToken, keyword_of,
  leading_path_separator, path, path_after_first, unexpected_here,
};
use crate::{
  combinator::{
    ParseCtx, bang, colon, dollar, fat_arrow, lbrace, lbracket, lparen, try_comma, try_equal,
    try_rbrace, try_rparen,
  },
  graphqlx::{
    GraphQLx,
    ast::{
      BooleanValue, BytesValue, CharValue, ConstBlock, ConstInputValue, ConstList, ConstMap,
      ConstMapEntry, ConstObject, ConstObjectField, ConstSet, DefaultInputValue, EnumValue,
      FloatValue, IdValue, InlineStringValue, InputValue, IntValue, List, Map, MapEntry, Nest,
      Nested, NullValue, Object, ObjectField, ParenValue, Set, StringValue, TypePath,
      TypedObjectValue, TypedValue, VariableValue,
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
  // A body written once for an executable document's constant positions and a schema position's,
  // generic over the constant that says which: `false` for the first, `true` for the second.
  (@schema_generic $(#[$meta:meta])* $name:ident, $input:ident, $output:ty, $body:block) => {
    $(#[$meta])*
    fn $name<'inp, Src, Ctx, const SCHEMA: bool>(
      $input: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
    ) -> Result<$output, GraphqlxError<'inp, Src, Ctx>>
    where
      Src: Source<usize> + ?Sized,
      GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
      <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
      GraphqlxToken<'inp, Src>:
        Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
      GraphqlxLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlxToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
        State = smear_lexer::graphqlx::syntactic::SyntacticState,
      >,
      Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
      GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
    $body
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
      GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
      <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
      GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
      GraphqlxLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlxToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
        State = smear_lexer::graphqlx::syntactic::SyntacticState,
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
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
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

value_parser!(
  /// Parses a committed GraphQLx `char` value: `'a'`, one character or one escape between single
  /// quotes.
  ///
  /// A `char` literal is a GraphQLx dialect extension with no corresponding GraphQL draft
  /// production.
  pub char_value,
  inp,
  CharValue<GraphqlxSlice<'inp, Src>>,
  [],
  {
    match inp.next_or_stop()? {
      Some(Spanned {
        span,
        data: GraphqlxToken::<'inp, Src>::LitChar(value),
      }) => Ok(CharValue::new(span, value)),
      Some(Spanned { span, data: token }) => unexpected_after_consumed::<Src, Ctx, _>(
        token,
        span,
        Expectation::Production("a character"),
      ),
      None => unexpected_here(inp, Expectation::Production("a character")),
    }
  }
);

pub(crate) fn inline_string_value<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<InlineStringValue<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
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

/// An enum value whose first name is consumed: a relative path, the only kind a name can start.
fn enum_after_first<'inp, Src, Ctx>(
  start: usize,
  first: super::ast::Name<GraphqlxSlice<'inp, Src>>,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<EnumValue<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  path_after_first(start, first, super::ast::PathRoot::Relative, inp)
    .map(|path| EnumValue::new(*path.span(), path))
}

value_parser!(
  /// Parses a committed GraphQLx enum/path value.
  ///
  /// GraphQLx extends GraphQL's `EnumValue` from a single name to a path under an
  /// optional root, while retaining GraphQL's reserved `true`, `false`, and `null`
  /// exclusions for a relative path's first segment; `^::null` is a path.
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
            enum_after_first(span.start(), super::ast::Name::new(span, source), inp)
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
      Some(
        SyntacticTokenKind::Tilde | SyntacticTokenKind::DotDot | SyntacticTokenKind::Caret,
      ) => path(inp).map(|path| EnumValue::new(*path.span(), path)),
      Some(SyntacticTokenKind::PathSeparator) => leading_path_separator(inp),
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
      .spanned_trimmed()
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
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
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
  Char,
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
    smear_lexer::graphqlx::syntactic::SyntacticToken::LitChar(_) => ValueHead::Char,
    smear_lexer::graphqlx::syntactic::SyntacticToken::Identifier(_) => ValueHead::Identifier,
    smear_lexer::graphqlx::syntactic::SyntacticToken::Dollar => ValueHead::Dollar,
    smear_lexer::graphqlx::syntactic::SyntacticToken::LBracket => ValueHead::List,
    smear_lexer::graphqlx::syntactic::SyntacticToken::LBrace => ValueHead::Object,
    smear_lexer::graphqlx::syntactic::SyntacticToken::Tilde
    | smear_lexer::graphqlx::syntactic::SyntacticToken::DotDot
    | smear_lexer::graphqlx::syntactic::SyntacticToken::Caret => ValueHead::Path,
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
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  Ok(match peeked.pop_front() {
    Some(token) if value_head(token.token()).is_some() => Action::Continue,
    _ => Action::Stop,
  })
}

/// The member test of a constant list: a value's head goes on, and in a schema position, where
/// `SCHEMA` holds, so does an operator's token or a `(`, which the value's own dispatch then refuses
/// for the block it offers.
fn decide_const_value_head<'inp, Src, Ctx, const SCHEMA: bool>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>,
  _: EmitterView<'_, 'inp, GraphqlxLexer<'inp, Src>, Ctx::Emitter, GraphQLx>,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  Ok(match peeked.pop_front() {
    Some(token) if const_value_head(token.token()) => Action::Continue,
    Some(token) if SCHEMA && begins_an_expression(token.token().kind()) => Action::Continue,
    _ => Action::Stop,
  })
}

fn decide_brace_member<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>,
  _: EmitterView<'_, 'inp, GraphqlxLexer<'inp, Src>, Ctx::Emitter, GraphQLx>,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  Ok(match peeked.pop_front() {
    Some(token) if matches!(token.token(), GraphqlxToken::<'inp, Src>::RBrace) => Action::Stop,
    Some(_) => Action::Continue,
    None => Action::Stop,
  })
}

/// The member test of a constant object and of a typed object: a `}` ends the fields, and in a
/// schema position, where `SCHEMA` holds, what goes on with an expression is refused at the token
/// the test peeked, against the block it offers.
fn decide_field_member<'inp, Src, Ctx, const SCHEMA: bool>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>,
  _: EmitterView<'_, 'inp, GraphqlxLexer<'inp, Src>, Ctx::Emitter, GraphQLx>,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match peeked.pop_front() {
    Some(token) if matches!(token.token(), GraphqlxToken::<'inp, Src>::RBrace) => Ok(Action::Stop),
    Some(token)
      if SCHEMA && goes_on_with_an_expression::<Src>(token.token().kind(), token.token()) =>
    {
      Err(expression_refused::<Src, Ctx>(
        token.token().kind(),
        *token.span(),
      ))
    }
    Some(_) => Ok(Action::Continue),
    None => Ok(Action::Stop),
  }
}

/// Whether `kind` is one of the nine tokens an expression's operator is written with outside a
/// block: `+`, `-`, `*`, `/`, `%`, `<<`, `&`, `|` and `!`.
pub(super) const fn is_operator(kind: SyntacticTokenKind) -> bool {
  matches!(
    kind,
    SyntacticTokenKind::Plus
      | SyntacticTokenKind::Minus
      | SyntacticTokenKind::Asterisk
      | SyntacticTokenKind::Slash
      | SyntacticTokenKind::Percent
      | SyntacticTokenKind::Shl
      | SyntacticTokenKind::Ampersand
      | SyntacticTokenKind::Pipe
      | SyntacticTokenKind::Bang
  )
}

/// Whether `kind`, at a value's head in a schema position, begins an expression: an operator's
/// token, or a `(`.
const fn begins_an_expression(kind: SyntacticTokenKind) -> bool {
  is_operator(kind) || matches!(kind, SyntacticTokenKind::LParen)
}

/// Whether `kind`, directly after a value where no second value may follow, can only go on with an
/// expression: an operator's token, a `>`, which closes nothing there, a `^`, or a `(`.
const fn continues_an_expression(kind: SyntacticTokenKind) -> bool {
  is_operator(kind)
    || matches!(
      kind,
      SyntacticTokenKind::RAngle | SyntacticTokenKind::Caret | SyntacticTokenKind::LParen
    )
}

/// Whether `token`, of kind `kind`, directly after a value where no second value may follow, can
/// only go on with an expression: one of `continues_an_expression`'s kinds, or a number whose
/// token begins with `-`, as in `HOUR-1`. The number's first character is read only where the token
/// is a number.
#[inline]
pub(super) fn goes_on_with_an_expression<'inp, Src>(
  kind: SyntacticTokenKind,
  token: &GraphqlxToken<'inp, Src>,
) -> bool
where
  Src: Source<usize> + ?Sized,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
{
  continues_an_expression(kind)
    || match token {
      GraphqlxToken::<'inp, Src>::LitInt(lit) => {
        crate::graphqlx::begins_with_minus(lit.source_ref())
      }
      GraphqlxToken::<'inp, Src>::LitFloat(lit) => {
        crate::graphqlx::begins_with_minus(lit.source_ref())
      }
      _ => false,
    }
}

/// The refusal of an expression's token in a schema position: what was found, where it was found,
/// against a `const { … }` block around the expression.
pub(super) fn expression_refused<'inp, Src, Ctx>(
  kind: SyntacticTokenKind,
  span: SimpleSpan,
) -> GraphqlxError<'inp, Src, Ctx>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  DialectGraphqlxError::unexpected_token(kind, Expectation::ConstBlock, span).into()
}

/// After a value where no second value may follow, in a schema position: what goes on with an
/// expression is refused at the token found, against the block it offers, and anything else is
/// left for the reader that comes next. One read of the staged head, and no lex.
pub(super) fn refuse_expression_after<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<(), GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match inp
    .peek_head_map(|token| goes_on_with_an_expression::<Src>(token.data.kind(), token.data))?
  {
    Some(true) => unexpected_here(inp, Expectation::ConstBlock),
    _ => Ok(()),
  }
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
    .spanned_trimmed()
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
  { const_list_body::<Src, Ctx, false>(inp) }
);

value_parser!(
  @schema_generic
  /// The list both constant grammars read, its elements read by the grammar `SCHEMA` names.
  #[inline(always)]
  const_list_body,
  inp,
  ConstList<GraphqlxSlice<'inp, Src>>,
  {
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| const_member::<Src, Ctx, SCHEMA>(inp))
    .repeated_while::<_, U1>(decide_const_value_head::<_, Ctx, SCHEMA>)
    .delimited_by_brackets()
    .collect_with(Default::default())
    .spanned_trimmed()
    .parse_input(inp)
    .map(|Spanned { span, data }| ConstList::new(span, data))
  }
);

value_parser!(
  /// A list in a schema position, whose elements are schema values.
  schema_list_value,
  inp,
  ConstList<GraphqlxSlice<'inp, Src>>,
  [complex],
  { const_list_body::<Src, Ctx, true>(inp) }
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
    .spanned_trimmed()
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
  { const_object_field_body::<Src, Ctx, false>(inp) }
);

value_parser!(
  @schema_generic
  /// The object field both constant grammars read, its value read by the grammar `SCHEMA` names.
  #[inline(always)]
  const_object_field_body,
  inp,
  ConstObjectField<GraphqlxSlice<'inp, Src>>,
  {
  super::name
    .then_ignore(colon)
    .then(const_member::<Src, Ctx, SCHEMA>)
    .spanned_trimmed()
    .map(|Spanned { span, data: (name, value) }| ConstObjectField::new(span, name, value))
    .parse_input(inp)
  }
);

value_parser!(
  /// An object field in a schema position, whose value is a schema value.
  schema_object_field,
  inp,
  ConstObjectField<GraphqlxSlice<'inp, Src>>,
  [complex],
  { const_object_field_body::<Src, Ctx, true>(inp) }
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
    .spanned_trimmed()
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
  { const_object_body::<Src, Ctx, false>(inp) }
);

value_parser!(
  @schema_generic
  /// The object both constant grammars read, its fields read by the grammar `SCHEMA` names.
  #[inline(always)]
  const_object_body,
  inp,
  ConstObject<GraphqlxSlice<'inp, Src>>,
  {
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| const_field_member::<Src, Ctx, SCHEMA>(inp))
    .repeated_while::<_, U1>(decide_field_member::<_, Ctx, SCHEMA>)
    .delimited_by_braces()
    .collect_with(Default::default())
    .spanned_trimmed()
    .parse_input(inp)
    .map(|Spanned { span, data }| ConstObject::new(span, data))
  }
);

value_parser!(
  /// An object in a schema position, whose fields hold schema values.
  schema_object_value,
  inp,
  ConstObject<GraphqlxSlice<'inp, Src>>,
  [complex],
  { const_object_body::<Src, Ctx, true>(inp) }
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
      .spanned_trimmed()
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
  { const_map_entry_body::<Src, Ctx, false>(inp) }
);

value_parser!(
  @schema_generic
  /// The map entry both constant grammars read, its halves read by the grammar `SCHEMA` names.
  #[inline(always)]
  const_map_entry_body,
  inp,
  ConstMapEntry<GraphqlxSlice<'inp, Src>>,
  {
    // In a schema position a key that an expression's token follows is refused there, against
    // the block it offers, rather than at the missing `=>`.
    (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| -> Result<
      ConstInputValue<GraphqlxSlice<'inp, Src>>,
      GraphqlxError<'inp, Src, Ctx>,
    > {
      let key = const_member::<Src, Ctx, SCHEMA>(inp)?;
      if SCHEMA {
        refuse_expression_after(inp)?;
      }
      Ok(key)
    })
      .then_ignore(fat_arrow)
      .then(const_member::<Src, Ctx, SCHEMA>)
      .spanned_trimmed()
      .map(
        |Spanned {
           span,
           data: (key, value),
         }| ConstMapEntry::new(span, key, value),
      )
      .parse_input(inp)
  }
);

value_parser!(
  /// A map entry in a schema position, whose halves are schema values.
  schema_map_entry,
  inp,
  ConstMapEntry<GraphqlxSlice<'inp, Src>>,
  [complex],
  { const_map_entry_body::<Src, Ctx, true>(inp) }
);

value_parser!(
  @schema_generic
  /// A member of a constant aggregate: a value of the executable constant grammar, or of a schema
  /// position's when `SCHEMA` holds. Always inlined, so a member costs the frames it did when the
  /// aggregates read the executable constant grammar alone.
  #[inline(always)]
  const_member,
  inp,
  ConstInputValue<GraphqlxSlice<'inp, Src>>,
  {
    if SCHEMA { schema_value(inp) } else { const_value(inp) }
  }
);

value_parser!(
  @schema_generic
  /// A field of a constant object, in the grammar `SCHEMA` names, under the terms of
  /// [`const_member`].
  #[inline(always)]
  const_field_member,
  inp,
  ConstObjectField<GraphqlxSlice<'inp, Src>>,
  {
    if SCHEMA { schema_object_field(inp) } else { const_object_field(inp) }
  }
);

value_parser!(
  @schema_generic
  /// An entry of a constant map, in the grammar `SCHEMA` names, under the terms of
  /// [`const_member`].
  #[inline(always)]
  const_entry_member,
  inp,
  ConstMapEntry<GraphqlxSlice<'inp, Src>>,
  {
    if SCHEMA { schema_map_entry(inp) } else { const_map_entry(inp) }
  }
);

/// The `!` after a `set` or `map` keyword, and the `{` it must be followed by.
///
/// The `!` is committed: the two dispatchers come here only once they have peeked it, and the
/// standalone productions name it as their expectation when it is missing. The `{` is required
/// before any member is read, so anything else in its place, end of input included, is refused
/// here and left unconsumed rather than handed to the brace delimiter. Under a recovering emitter
/// that delimiter reports a missing opener and reads members anyway, and every member begins with
/// a value, so `set! set! set! …` would enter one collection per prefix with no brace opened for
/// the nesting limit to count.
fn collection_bang<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<(), GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  bang(inp)?;
  if !matches!(super::peek_kind(inp)?, Some(SyntacticTokenKind::LBrace)) {
    return unexpected_here(inp, Expectation::LBrace);
  }
  Ok(())
}

fn set_after_keyword<'inp, Src, Ctx>(
  start: usize,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Set<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  collection_bang(inp)?;
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| value(inp))
    .repeated_while::<_, U1>(decide_brace_member::<_, Ctx>)
    .delimited_by_braces()
    .collect_with(Default::default())
    .spanned_trimmed()
    .parse_input(inp)
    .map(|Spanned { span, data }| Set::new(SimpleSpan::new(start, span.end()), data))
}

/// The `! { … }` of a constant set, its elements read by the grammar `SCHEMA` names.
fn const_set_after_keyword<'inp, Src, Ctx, const SCHEMA: bool>(
  start: usize,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ConstSet<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  collection_bang(inp)?;
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| const_member::<Src, Ctx, SCHEMA>(inp))
    .repeated_while::<_, U1>(decide_brace_member::<_, Ctx>)
    .delimited_by_braces()
    .collect_with(Default::default())
    .spanned_trimmed()
    .parse_input(inp)
    .map(|Spanned { span, data }| ConstSet::new(SimpleSpan::new(start, span.end()), data))
}

fn map_after_keyword<'inp, Src, Ctx>(
  start: usize,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Map<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  collection_bang(inp)?;
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| map_entry(inp))
    .repeated_while::<_, U1>(decide_brace_member::<_, Ctx>)
    .delimited_by_braces()
    .collect_with(Default::default())
    .spanned_trimmed()
    .parse_input(inp)
    .map(|Spanned { span, data }| Map::new(SimpleSpan::new(start, span.end()), data))
}

/// The `! { … }` of a constant map, its entries read by the grammar `SCHEMA` names.
fn const_map_after_keyword<'inp, Src, Ctx, const SCHEMA: bool>(
  start: usize,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ConstMap<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  collection_bang(inp)?;
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| const_entry_member::<Src, Ctx, SCHEMA>(inp))
    .repeated_while::<_, U1>(decide_brace_member::<_, Ctx>)
    .delimited_by_braces()
    .collect_with(Default::default())
    .spanned_trimmed()
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
          if matches!(super::peek_kind(inp)?, Some(SyntacticTokenKind::Bang)) {
            set_after_keyword(span.start(), inp).map(InputValue::Set)
          } else {
            enum_after_first(span.start(), super::ast::Name::new(span, source), inp)
              .map(InputValue::Enum)
          }
        }
        (GraphqlxToken::<'inp, Src>::Identifier(source), Some(ContextualKeyword::Map)) => {
          if matches!(super::peek_kind(inp)?, Some(SyntacticTokenKind::Bang)) {
            map_after_keyword(span.start(), inp).map(InputValue::Map)
          } else {
            enum_after_first(span.start(), super::ast::Name::new(span, source), inp)
              .map(InputValue::Enum)
          }
        }
        (GraphqlxToken::<'inp, Src>::Identifier(source), _) => {
          enum_after_first(span.start(), super::ast::Name::new(span, source), inp)
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
      Some(SyntacticTokenKind::Char) => char_value(inp).map(InputValue::Char),
      Some(
        SyntacticTokenKind::Tilde | SyntacticTokenKind::DotDot | SyntacticTokenKind::Caret,
      ) => path(inp).map(|path| InputValue::Enum(EnumValue::new(*path.span(), path))),
      Some(SyntacticTokenKind::PathSeparator) => leading_path_separator(inp),
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
  { const_value_body::<Src, Ctx, false>(inp) }
);

value_parser!(
  @schema_generic
  /// The dispatch both constant grammars read, written once: [`const_value`] is its instance with
  /// `SCHEMA` false, and [`schema_value`] its instance with `SCHEMA` true.
  ///
  /// The schema instance has three outcomes more, each decided after a path by the one token that
  /// follows it: a typed object, a typed value, and the refusal of type arguments that open
  /// neither. Always inlined, so each instance is one function and a level of nesting costs the
  /// frames it did when there was one instance.
  #[inline(always)]
  const_value_body,
  inp,
  ConstInputValue<GraphqlxSlice<'inp, Src>>,
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
          if matches!(super::peek_kind(inp)?, Some(SyntacticTokenKind::Bang)) {
            const_set_after_keyword::<Src, Ctx, SCHEMA>(span.start(), inp).map(ConstInputValue::Set)
          } else {
            enum_after_first(span.start(), super::ast::Name::new(span, source), inp)
              .map(ConstInputValue::Enum)
          }
        }
        (GraphqlxToken::<'inp, Src>::Identifier(source), Some(ContextualKeyword::Map)) => {
          if matches!(super::peek_kind(inp)?, Some(SyntacticTokenKind::Bang)) {
            const_map_after_keyword::<Src, Ctx, SCHEMA>(span.start(), inp).map(ConstInputValue::Map)
          } else {
            enum_after_first(span.start(), super::ast::Name::new(span, source), inp)
              .map(ConstInputValue::Enum)
          }
        }
        // A schema position's block: `const` before `{`. Anywhere else, and before anything else,
        // `const` is a name.
        (GraphqlxToken::<'inp, Src>::Identifier(source), Some(ContextualKeyword::Const))
          if SCHEMA =>
        {
          if matches!(super::peek_kind(inp)?, Some(SyntacticTokenKind::LBrace)) {
            const_block_after_keyword(span.start(), inp).map(ConstInputValue::Block)
          } else {
            enum_after_first(span.start(), super::ast::Name::new(span, source), inp)
              .map(ConstInputValue::Enum)
          }
        }
        (GraphqlxToken::<'inp, Src>::Identifier(source), _) => {
          enum_after_first(span.start(), super::ast::Name::new(span, source), inp)
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
    // A path a name began is decided in a schema position by the token after it. Every other value
    // the dispatch built is the value.
    ParseAttempt::Accept(ConstInputValue::Enum(value)) if SCHEMA => {
      schema_path_value::<Src, Ctx, false>(value, inp)
    }
    ParseAttempt::Accept(value) => Ok(value),
    ParseAttempt::Decline => match super::peek_kind(inp)? {
      Some(SyntacticTokenKind::LBracket) => {
        (if SCHEMA { schema_list_value(inp) } else { const_list_value(inp) }).map(ConstInputValue::List)
      }
      Some(SyntacticTokenKind::LBrace) => {
        (if SCHEMA { schema_object_value(inp) } else { const_object_value(inp) })
          .map(ConstInputValue::Object)
      }
      Some(SyntacticTokenKind::Char) => char_value(inp).map(ConstInputValue::Char),
      Some(
        SyntacticTokenKind::Tilde | SyntacticTokenKind::DotDot | SyntacticTokenKind::Caret,
      ) => {
        let path = path(inp)?;
        let value = EnumValue::new(*path.span(), path);
        if SCHEMA {
          schema_path_value::<Src, Ctx, false>(value, inp)
        } else {
          Ok(ConstInputValue::Enum(value))
        }
      }
      Some(SyntacticTokenKind::PathSeparator) => leading_path_separator(inp),
      // In a schema position an operator's token or a `(` at a value's head begins an expression,
      // which only a `const { … }` block reads: the block is offered.
      Some(kind) if SCHEMA && begins_an_expression(kind) => {
        unexpected_here(inp, Expectation::ConstBlock)
      }
      _ => unexpected_here(inp, Expectation::ConstInputValue),
    },
  }
  }
);

/// A path at the head of a schema value, or of an operand inside a block when `IN_BLOCK` holds,
/// decided by the one token after it: `<`, `{` and `(` open a typed form, and anything else leaves
/// the path the value.
///
/// The path's own loop ended on a declined `::`, which left that token staged, so this reads the
/// staged token and lexes nothing. Inlined into the schema dispatch, and the typed forms are out of
/// line, so a schema value that is no typed form carries none of their frame.
#[inline(always)]
fn schema_path_value<'inp, Src, Ctx, const IN_BLOCK: bool>(
  value: EnumValue<GraphqlxSlice<'inp, Src>>,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ConstInputValue<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match super::peek_kind(inp)? {
    Some(SyntacticTokenKind::LAngle | SyntacticTokenKind::LBrace | SyntacticTokenKind::LParen) => {
      typed_form::<Src, Ctx, IN_BLOCK>(value.into_components().source, inp)
    }
    _ => Ok(ConstInputValue::Enum(value)),
  }
}

/// A typed form, from the path that names its type: `Point { x: 1 }`, `Box<i32> {}` or `Url("x")`.
///
/// Type arguments come first when `<` follows the path, and then `{` or `(` must: anything else is
/// refused at the token found. A typed object's fields and a typed value's value are a schema
/// position's, and inside a block, when `IN_BLOCK` holds, the block's own: each field holds an
/// expression and a `,` separates the fields, and a typed value holds one expression.
///
/// Never inlined, and not cold: a schema value that is no typed form carries none of this frame,
/// and a list of lists costs the native stack what it costs without the typed forms.
#[inline(never)]
fn typed_form<'inp, Src, Ctx, const IN_BLOCK: bool>(
  path: super::ast::Path<GraphqlxSlice<'inp, Src>>,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ConstInputValue<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let start = path.span().start();
  let path_end = path.span().end();
  let generics = super::ty::try_type_generics(inp)?;
  let end = generics
    .as_ref()
    .map_or(path_end, |generics| generics.span().end());
  let ty = TypePath::new(SimpleSpan::new(start, end), path, generics);
  match super::peek_kind(inp)? {
    Some(SyntacticTokenKind::LBrace) if IN_BLOCK => {
      let (fields, braces) = block_object_fields(inp)?;
      Ok(ConstInputValue::TypedObject(Box::new(
        TypedObjectValue::new(SimpleSpan::new(start, braces.end()), ty, fields),
      )))
    }
    Some(SyntacticTokenKind::LParen) if IN_BLOCK => {
      let open = *lparen(inp)?.span();
      let value = block_expression(inp)?;
      match try_rparen(inp)? {
        ParseAttempt::Accept(close) => Ok(ConstInputValue::Typed(Nest::new(TypedValue::new(
          SimpleSpan::new(start, close.span().end()),
          ty,
          value,
        )))),
        ParseAttempt::Decline => unclosed_or_unexpected(inp, open, Expectation::RParen),
      }
    }
    Some(SyntacticTokenKind::LBrace) => {
      (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| schema_object_field(inp))
        .repeated_while::<_, U1>(decide_field_member::<_, Ctx, true>)
        .delimited_by_braces()
        .collect_with(Default::default())
        .spanned_trimmed()
        .parse_input(inp)
        .map(|Spanned { span, data }| {
          ConstInputValue::TypedObject(Box::new(TypedObjectValue::new(
            SimpleSpan::new(start, span.end()),
            ty,
            data,
          )))
        })
    }
    Some(SyntacticTokenKind::LParen) => {
      // Not the `Delimited`'s own span, as `committed_arguments` explains: the trimmed cursor is
      // the `(`'s own start, and the span since it ends at the `)`.
      let node_start = inp.cursor_trimmed()?;
      // The one value, and then what goes on with an expression is refused where it stands,
      // against the block it offers, rather than at the missing `)`.
      let parsed = parens::<_, _, _, _, ConstInputValue<GraphqlxSlice<'inp, Src>>, _>(
        |inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| -> Result<
          ConstInputValue<GraphqlxSlice<'inp, Src>>,
          GraphqlxError<'inp, Src, Ctx>,
        > {
          let value = schema_value(inp)?;
          refuse_expression_after(inp)?;
          Ok(value)
        },
      )(inp)?;
      let (_, _, _, value) = parsed.into_components();
      let end = inp.span_since_trimmed(&node_start).end();
      Ok(ConstInputValue::Typed(Nest::new(TypedValue::new(
        SimpleSpan::new(start, end),
        ty,
        value,
      ))))
    }
    None => at_the_end(
      inp,
      Expectation::Production("`{` or `(` after the type arguments"),
    ),
    _ => unexpected_here(
      inp,
      Expectation::Production("`{` or `(` after the type arguments"),
    ),
  }
}

/// A schema position's `const { … }` block, its `const` consumed and its `{` staged: the one place
/// the parser switches the lexer into the block's mode, and back.
///
/// Seven steps, written out so that no wrapper reads across a switch: the `{` the caller's read
/// staged is taken with nothing read behind it; the lexer enters the block; the expression is read,
/// every token of it lexed in the mode; the `}` is taken; the lexer leaves the block; and the span
/// runs from the `const` to the `}`. On an error the parse may go on from, the lexer leaves the
/// block before the error is returned, so nothing after it is lexed in the mode. On a scanner's
/// stop it writes no state: the state the stop latched stays as it is.
///
/// Never inlined, and not cold, so a schema value that is no block carries none of this frame.
#[inline(never)]
fn const_block_after_keyword<'inp, Src, Ctx>(
  start: usize,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ConstBlock<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let open = *lbrace(inp)?.span();
  inp.state_mut().enter_const_block();
  match block_inside(inp, open) {
    Ok((value, end)) => {
      inp.state_mut().leave_const_block();
      Ok(ConstBlock::new(SimpleSpan::new(start, end), value))
    }
    Err(error) => {
      if !inp.at_scanner_stop() {
        inp.state_mut().leave_const_block();
      }
      Err(error)
    }
  }
}

/// A block's expression and its `}`, inside the block: the value, and where the `}` ends.
#[inline(always)]
fn block_inside<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  open: SimpleSpan,
) -> Result<(ConstInputValue<GraphqlxSlice<'inp, Src>>, usize), GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let value = block_expression(inp)?;
  match try_rbrace(inp)? {
    ParseAttempt::Accept(close) => Ok((value, close.span().end())),
    ParseAttempt::Decline => unclosed_or_unexpected(inp, open, Expectation::RBrace),
  }
}

/// A block inside a block, its `const` consumed and its `{` staged. The lexer is in the mode
/// already, so this switches nothing: it reads the `{`, the expression and the `}`, as a
/// parenthesis would.
#[inline(never)]
fn nested_block_after_keyword<'inp, Src, Ctx>(
  start: usize,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ConstBlock<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let open = *lbrace(inp)?.span();
  let (value, end) = block_inside(inp, open)?;
  Ok(ConstBlock::new(SimpleSpan::new(start, end), value))
}

value_parser!(
  /// Parses a committed `const { … }` block in a schema position, switching the lexer into the
  /// block's mode at its `{` and back at its `}`.
  ///
  /// **Called outside a block alone.** Inside one it would leave the mode at its own `}` while the
  /// block around it still ran; a block inside a block is [`block_operand`]'s.
  pub const_block,
  inp,
  ConstBlock<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
    match inp.next_or_stop()? {
      Some(Spanned { span, data: token })
        if keyword_of(&token) == Some(ContextualKeyword::Const) =>
      {
        if !matches!(super::peek_kind(inp)?, Some(SyntacticTokenKind::LBrace)) {
          return unexpected_here(inp, Expectation::LBrace);
        }
        const_block_after_keyword(span.start(), inp)
      }
      Some(Spanned { span, data: token }) => {
        unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::Keyword("const"))
      }
      None => unexpected_here(inp, Expectation::Keyword("const")),
    }
  }
);

value_parser!(
  /// Parses the expression a `const { … }` block holds, or a parenthesis, a typed value or a member
  /// of a list, a set, a map or an object inside one: an operand, and then what ends it.
  ///
  /// A `,`, a closer, a `=>` or the end of input ends the expression. Anything else is refused at
  /// the token found, against an operator or a `,`: no operator is read yet, so a second operand
  /// written beside the first is refused there too.
  ///
  /// **Called inside a block alone:** outside one the lexer hands it another token stream.
  pub block_expression,
  inp,
  ConstInputValue<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
    let operand = block_operand(inp)?;
    match super::peek_kind(inp)? {
      None
      | Some(
        SyntacticTokenKind::Separator
        | SyntacticTokenKind::RBrace
        | SyntacticTokenKind::RBracket
        | SyntacticTokenKind::RParen
        | SyntacticTokenKind::FatArrow,
      ) => Ok(operand),
      Some(_) => unexpected_here(inp, Expectation::OperatorOrSeparator),
    }
  }
);

value_parser!(
  /// Parses one operand inside a `const { … }` block: a number, which carries no sign there; a
  /// string; a prefixed string, `b"…"` bytes and `i"…"` an id; a `char` literal; `true`, `false`
  /// or `null`; a block, when `const` is followed by `{`; a parenthesis; a list, a set, a map or an
  /// object, whose members a `,` separates and each of which is an expression; or a path, from a
  /// name or a root's sigil, which `<`, `{` or `(` after it makes the type of a typed form.
  ///
  /// A head that begins none of them is refused at the token found, against a constant value.
  ///
  /// **Called inside a block alone:** outside one the lexer hands it another token stream.
  pub block_operand,
  inp,
  ConstInputValue<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
    match super::peek_kind(inp)? {
      Some(SyntacticTokenKind::Int) => int_value(inp).map(ConstInputValue::Int),
      Some(SyntacticTokenKind::Float) => float_value(inp).map(ConstInputValue::Float),
      Some(SyntacticTokenKind::InlineString | SyntacticTokenKind::BlockString) => {
        string_value(inp).map(ConstInputValue::String)
      }
      Some(SyntacticTokenKind::Char) => char_value(inp).map(ConstInputValue::Char),
      Some(SyntacticTokenKind::PrefixedString) => prefixed_string_value(inp),
      Some(SyntacticTokenKind::Identifier) => block_name(inp),
      Some(
        SyntacticTokenKind::Tilde | SyntacticTokenKind::DotDot | SyntacticTokenKind::Caret,
      ) => {
        let rooted = path(inp)?;
        schema_path_value::<Src, Ctx, true>(EnumValue::new(*rooted.span(), rooted), inp)
      }
      Some(SyntacticTokenKind::LParen) => paren_value(inp).map(ConstInputValue::Paren),
      Some(SyntacticTokenKind::LBracket) => block_list_value(inp).map(ConstInputValue::List),
      Some(SyntacticTokenKind::LBrace) => block_object_value(inp).map(ConstInputValue::Object),
      // A missing operand at the end of input is refused where the input ends.
      None => at_the_end(inp, Expectation::ConstInputValue),
      // A `::` begins no operand, and is refused as every head that begins none is, so the code
      // is the one the lossless layer reports there.
      _ => unexpected_here(inp, Expectation::ConstInputValue),
    }
  }
);

value_parser!(
  /// A name at an operand's head inside a block: `true`, `false` and `null`; a block inside the
  /// block, when `const` is followed by `{`; a set or a map, when `set` or `map` is followed by
  /// `!`; and otherwise a path, which `<`, `{` or `(` after it makes the type of a typed form.
  block_name,
  inp,
  ConstInputValue<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
    match inp.next_or_stop()? {
      Some(Spanned { span, data: token }) => {
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
          (GraphqlxToken::<'inp, Src>::Identifier(source), keyword) => {
            match (keyword, super::peek_kind(inp)?) {
              (Some(ContextualKeyword::Const), Some(SyntacticTokenKind::LBrace)) => {
                nested_block_after_keyword(span.start(), inp).map(ConstInputValue::Block)
              }
              (Some(ContextualKeyword::Set), Some(SyntacticTokenKind::Bang)) => {
                block_set_after_keyword(span.start(), inp).map(ConstInputValue::Set)
              }
              (Some(ContextualKeyword::Map), Some(SyntacticTokenKind::Bang)) => {
                block_map_after_keyword(span.start(), inp).map(ConstInputValue::Map)
              }
              _ => {
                let value =
                  enum_after_first(span.start(), super::ast::Name::new(span, source), inp)?;
                schema_path_value::<Src, Ctx, true>(value, inp)
              }
            }
          }
          (token, _) => {
            unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::ConstInputValue)
          }
        }
      }
      None => unexpected_here(inp, Expectation::ConstInputValue),
    }
  }
);

value_parser!(
  /// A prefixed string inside a block: `b"…"` is bytes and `i"…"` an id, by the literal's prefix.
  prefixed_string_value,
  inp,
  ConstInputValue<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
    match inp.next_or_stop()? {
      Some(Spanned {
        span,
        data: GraphqlxToken::<'inp, Src>::LitPrefixedStr(literal),
      }) => match literal.prefix() {
        StrPrefix::Bytes => Ok(ConstInputValue::Bytes(BytesValue::new(span, literal))),
        StrPrefix::Id => Ok(ConstInputValue::Id(IdValue::new(span, literal))),
        // The lexer makes no other prefix: every other letter before a string is refused as a
        // reserved one before a token is made.
        _ => unexpected_after_consumed::<Src, Ctx, _>(
          GraphqlxToken::<'inp, Src>::LitPrefixedStr(literal),
          span,
          Expectation::ConstInputValue,
        ),
      },
      Some(Spanned { span, data: token }) => {
        unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::ConstInputValue)
      }
      None => unexpected_here(inp, Expectation::ConstInputValue),
    }
  }
);

value_parser!(
  /// A parenthesis inside a block: `(`, one expression, and `)`. A separator is not its own, so
  /// `(1,)` is refused at the comma, against the `)`.
  paren_value,
  inp,
  ParenValue<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
    let open = lparen(inp)?;
    let value = block_expression(inp)?;
    match try_rparen(inp)? {
      ParseAttempt::Accept(close) => Ok(ParenValue::new(
        SimpleSpan::new(open.span().start(), close.span().end()),
        value,
      )),
      ParseAttempt::Decline => unclosed_or_unexpected(inp, *open.span(), Expectation::RParen),
    }
  }
);

/// The members of an aggregate inside a block, after its opener and up to its `closer`, each read
/// by `member`. Answers where the closer ends.
///
/// After a member a `,` goes on, and the closer may follow it, so one trailing separator is
/// allowed; the closer ends the members. Anything else ended the member's expression without being
/// the aggregate's own, and is refused at the token found, against `expected`, the closer. A member
/// that no separator follows was refused before this sees it, by the expression's own test of what
/// ends it.
#[inline(always)]
fn block_members<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  closer: SyntacticTokenKind,
  expected: Expectation,
  open: SimpleSpan,
  mut member: impl FnMut(
    &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  ) -> Result<(), GraphqlxError<'inp, Src, Ctx>>,
) -> Result<usize, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  loop {
    match super::peek_kind(inp)? {
      Some(kind) if kind == closer => break,
      None => return unclosed_or_unexpected(inp, open, expected),
      Some(_) => {}
    }
    member(inp)?;
    if let ParseAttempt::Accept(_) = try_comma(inp)? {
      continue;
    }
    match super::peek_kind(inp)? {
      Some(kind) if kind == closer => break,
      _ => return unclosed_or_unexpected(inp, open, expected),
    }
  }
  match inp.next_or_stop()? {
    Some(Spanned { span, .. }) => Ok(span.end()),
    None => unexpected_here(inp, expected),
  }
}

/// A closer that is missing inside a block: at the end of input the aggregate `open` begins is
/// unclosed there, its kind by the closer `expected` names, as the lossless layer reports it; at any
/// other token that token is refused, against `expected`. A scanner's stop stays terminal, through
/// the peek.
fn unclosed_or_unexpected<'inp, Src, Ctx, T>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  open: SimpleSpan,
  expected: Expectation,
) -> Result<T, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match super::peek_kind(inp)? {
    None => Err(
      match expected {
        Expectation::RBracket => DialectGraphqlxError::unclosed_list(open),
        Expectation::RParen => DialectGraphqlxError::unclosed_parentheses(open),
        _ => DialectGraphqlxError::unclosed_object(open),
      }
      .into(),
    ),
    Some(_) => unexpected_here(inp, expected),
  }
}

/// A part of a schema position's form missing at the end of input, refused where the input ends:
/// past its last trivia, which is where the lossless layer, having read across that trivia, meets
/// the end. Reached only from a peek that answered no token, which a scanner's stop never does.
fn at_the_end<'inp, Src, Ctx, T>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  expected: Expectation,
) -> Result<T, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let end = inp.source().len();
  Err(
    DialectGraphqlxError::maybe_unexpected_token(None, expected, SimpleSpan::new(end, end)).into(),
  )
}

value_parser!(
  /// A list inside a block: `[`, its elements, each an expression, and `]`.
  block_list_value,
  inp,
  ConstList<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
    let open = *lbracket(inp)?.span();
    let start = open.start();
    let mut values: Nested<ConstInputValue<GraphqlxSlice<'inp, Src>>> = Nested::empty();
    let end = block_members(
      inp,
      SyntacticTokenKind::RBracket,
      Expectation::RBracket,
      open,
      |inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
        values.push(block_expression(inp)?);
        Ok(())
      },
    )?;
    Ok(ConstList::new(SimpleSpan::new(start, end), values))
  }
);

value_parser!(
  /// An object inside a block: `{`, its fields, each holding an expression, and `}`.
  block_object_value,
  inp,
  ConstObject<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
    let (fields, span) = block_object_fields(inp)?;
    Ok(ConstObject::new(span, fields))
  }
);

value_parser!(
  /// The fields of an object or a typed object inside a block: the `{`, each field a name, `:` and
  /// an expression, and then the `}`. Answers the fields and the span from the `{` to the `}`.
  #[inline(always)]
  block_object_fields,
  inp,
  (
    Nested<ConstObjectField<GraphqlxSlice<'inp, Src>>>,
    SimpleSpan
  ),
  [complex],
  {
    let open = *lbrace(inp)?.span();
    let mut fields: Nested<ConstObjectField<GraphqlxSlice<'inp, Src>>> = Nested::empty();
    let end = block_members(
      inp,
      SyntacticTokenKind::RBrace,
      Expectation::RBrace,
      open,
      |inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
        fields.push(block_object_field(inp)?);
        Ok(())
      },
    )?;
    Ok((fields, SimpleSpan::new(open.start(), end)))
  }
);

value_parser!(
  /// A field of an object or a typed object inside a block: a name, `:`, and an expression.
  block_object_field,
  inp,
  ConstObjectField<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
    super::name
      .then_ignore(colon)
      .then(block_expression)
      .spanned_trimmed()
      .map(
        |Spanned {
           span,
           data: (name, value),
         }| ConstObjectField::new(span, name, value),
      )
      .parse_input(inp)
  }
);

/// The `! { … }` of a set inside a block, its elements each an expression.
fn block_set_after_keyword<'inp, Src, Ctx>(
  start: usize,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ConstSet<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  collection_bang(inp)?;
  let open = *lbrace(inp)?.span();
  let mut values: Nested<ConstInputValue<GraphqlxSlice<'inp, Src>>> = Nested::empty();
  let end = block_members(
    inp,
    SyntacticTokenKind::RBrace,
    Expectation::RBrace,
    open,
    |inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
      values.push(block_expression(inp)?);
      Ok(())
    },
  )?;
  Ok(ConstSet::new(SimpleSpan::new(start, end), values))
}

/// The `! { … }` of a map inside a block, its entries each two expressions.
fn block_map_after_keyword<'inp, Src, Ctx>(
  start: usize,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ConstMap<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  collection_bang(inp)?;
  let open = *lbrace(inp)?.span();
  let mut entries: Nested<ConstMapEntry<GraphqlxSlice<'inp, Src>>> = Nested::empty();
  let end = block_members(
    inp,
    SyntacticTokenKind::RBrace,
    Expectation::RBrace,
    open,
    |inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
      entries.push(block_map_entry(inp)?);
      Ok(())
    },
  )?;
  Ok(ConstMap::new(SimpleSpan::new(start, end), entries))
}

value_parser!(
  /// A map entry inside a block: a key, `=>`, and a value, each an expression.
  block_map_entry,
  inp,
  ConstMapEntry<GraphqlxSlice<'inp, Src>>,
  [complex],
  {
    block_expression
      .then_ignore(fat_arrow)
      .then(block_expression)
      .spanned_trimmed()
      .map(
        |Spanned {
           span,
           data: (key, value),
         }| ConstMapEntry::new(span, key, value),
      )
      .parse_input(inp)
  }
);

/// Parses a committed GraphQLx value in a schema position: a constant position of a type-system
/// definition or extension, which is a member's default and an argument of each directive such a
/// definition applies.
///
/// Every form [`const_value`] reads is a value here, and two more. A path followed by `{` is a typed
/// object, `Point { x: 1 }`, and a path followed by `(` is a typed value, `Url("x")`, each with type
/// arguments between when `<` follows the path, as in `Box<i32> {}`. So `[RED {a: 1}]` is one
/// typed object here, where an executable document reads an enum value and an object. An
/// executable document's constant positions read [`const_value`] alone.
pub fn schema_value<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ConstInputValue<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  const_value_body::<Src, Ctx, true>(inp)
}

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
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  try_default_body::<Src, Ctx, false>(inp)
}

value_parser!(
  @schema_generic
  /// The default assignment both constant grammars read, its value read by the grammar `SCHEMA`
  /// names.
  #[inline(always)]
  try_default_body,
  inp,
  ParseAttempt<DefaultInputValue<GraphqlxSlice<'inp, Src>>>,
  {
    let node_start = inp.cursor_trimmed()?;
    match try_equal(inp)? {
      ParseAttempt::Accept(_) => {
        let value = const_member::<Src, Ctx, SCHEMA>(inp)?;
        Ok(ParseAttempt::Accept(DefaultInputValue::new(
          inp.span_since_trimmed(&node_start),
          value,
        )))
      }
      ParseAttempt::Decline => Ok(ParseAttempt::Decline),
    }
  }
);

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
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  try_default_value(inp).map(Into::into)
}

/// Attempts an optional GraphQLx default assignment in a schema position, a member's default,
/// without consuming when `=` is absent. Once the `=` is present its value is committed, and read
/// by [`schema_value`].
///
/// See [`try_default_value`] and the [GraphQL Input Value Definitions specification](https://spec.graphql.org/draft/#sec-Input-Value-Definitions).
pub fn try_schema_default_value<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ParseAttempt<DefaultInputValue<GraphqlxSlice<'inp, Src>>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  try_default_body::<Src, Ctx, true>(inp)
}

/// Parses an optional GraphQLx default assignment in a schema position, a member's default.
///
/// See [`try_schema_default_value`] and the [GraphQL Input Value Definitions specification](https://spec.graphql.org/draft/#sec-Input-Value-Definitions).
pub fn schema_default_value<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Option<DefaultInputValue<GraphqlxSlice<'inp, Src>>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  try_schema_default_value(inp).map(Into::into)
}

macro_rules! graphqlx_value_api {
  ($(#[$meta:meta])* $node:ty, $parse:ident, []) => {
    graphqlx_value_api!(@impl graphqlx; $(#[$meta])* $node, $parse, []);
  };
  ($(#[$meta:meta])* $node:ty, $parse:ident, [contextual]) => {
    graphqlx_value_api!(
      @impl graphqlx; $(#[$meta])* $node,
      $parse,
      [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
    );
  };
  ($(#[$meta:meta])* $node:ty, $parse:ident, [punctuated]) => {
    graphqlx_value_api!(
      @impl graphqlx; $(#[$meta])* $node,
      $parse,
      [
      ]
    );
  };
  ($(#[$meta:meta])* $node:ty, $parse:ident, [path]) => {
    graphqlx_value_api!(
      @impl graphqlx; $(#[$meta])* $node,
      $parse,
      [
        GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
      ]
    );
  };
  ($(#[$meta:meta])* $node:ty, $parse:ident, [complex]) => {
    graphqlx_value_api!(
      @impl graphqlx; $(#[$meta])* $node,
      $parse,
      [
        GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
      ]
    );
  };
  // A schema position's twin of a `[complex]` production, under the method name `graphqlx_schema`.
  (@schema $(#[$meta:meta])* $node:ty, $parse:ident) => {
    graphqlx_value_api!(
      @impl graphqlx_schema; $(#[$meta])* $node,
      $parse,
      [
        GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
      ]
    );
  };
  (@impl $method:ident; $(#[$meta:meta])* $node:ty, $parse:ident, [$($bounds:tt)*]) => {
    impl<S> $node {
      $(#[$meta])*
      /// Parses this committed GraphQLx value production.
      pub fn $method<'inp, Src, Ctx>(
        inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
      ) -> Result<Self, GraphqlxError<'inp, Src, Ctx>>
      where
        Src: Source<usize, Slice<'inp> = S> + ?Sized,
        S: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
        <S as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
        GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
        GraphqlxLexer<'inp, Src>: Lexer<
            'inp,
            Source = Src,
            Token = GraphqlxToken<'inp, Src>,
            Span = SimpleSpan,
            Offset = usize,
            State = smear_lexer::graphqlx::syntactic::SyntacticState,
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
  /// Parses a committed GraphQLx `char` value.
  ///
  /// See [`char_value`].
  CharValue<S>,
  char_value,
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
  /// `set!{ ... }` is a GraphQLx dialect extension with no corresponding
  /// GraphQL draft production.
  Set<S>,
  set_value,
  [complex]
);
graphqlx_value_api!(
  /// Parses a committed constant GraphQLx set value.
  ///
  /// `set!{ ... }` is a GraphQLx dialect extension with no corresponding
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
  /// `map!{ ... }` is a GraphQLx dialect extension with no corresponding
  /// GraphQL draft production.
  Map<S>,
  map_value,
  [complex]
);
graphqlx_value_api!(
  /// Parses a committed constant GraphQLx map value.
  ///
  /// `map!{ ... }` is a GraphQLx dialect extension with no corresponding
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
graphqlx_value_api!(
  /// Parses a committed `const { … }` block in a schema position, outside any block.
  ///
  /// See [`const_block`].
  ConstBlock<S>,
  const_block,
  [complex]
);
graphqlx_value_api!(
  @schema
  /// Parses a committed GraphQLx value in a schema position: a constant position of a type-system
  /// definition or extension.
  ///
  /// See [`schema_value`] and the [GraphQL Input Values specification](https://spec.graphql.org/draft/#sec-Input-Values).
  ConstInputValue<S>,
  schema_value
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
    S: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
    <S as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
    GraphqlxToken<'inp, Src>:
      Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
    GraphqlxLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlxToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
        State = smear_lexer::graphqlx::syntactic::SyntacticState,
      >,
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
    S: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
    <S as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
    GraphqlxToken<'inp, Src>:
      Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
    GraphqlxLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlxToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
        State = smear_lexer::graphqlx::syntactic::SyntacticState,
      >,
    Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
    GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<S>>,
  {
    try_default_value(inp)
  }

  /// Parses an optional GraphQLx default assignment in a schema position, a member's default.
  ///
  /// The lexer source is inferred from `inp`.
  ///
  /// See [`schema_default_value`] and the [GraphQL Input Value Definitions specification](https://spec.graphql.org/draft/#sec-Input-Value-Definitions).
  pub fn graphqlx_schema<'inp, Src, Ctx>(
    inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  ) -> Result<Option<Self>, GraphqlxError<'inp, Src, Ctx>>
  where
    Src: Source<usize, Slice<'inp> = S> + ?Sized,
    S: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
    <S as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
    GraphqlxToken<'inp, Src>:
      Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
    GraphqlxLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlxToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
        State = smear_lexer::graphqlx::syntactic::SyntacticState,
      >,
    Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
    GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<S>>,
  {
    schema_default_value(inp)
  }

  /// Attempts a GraphQLx default assignment in a schema position without consuming when `=` is
  /// absent.
  ///
  /// See [`try_schema_default_value`] and the [GraphQL Input Value Definitions specification](https://spec.graphql.org/draft/#sec-Input-Value-Definitions).
  pub fn try_graphqlx_schema<'inp, Src, Ctx>(
    inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  ) -> Result<ParseAttempt<Self>, GraphqlxError<'inp, Src, Ctx>>
  where
    Src: Source<usize, Slice<'inp> = S> + ?Sized,
    S: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
    <S as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
    GraphqlxToken<'inp, Src>:
      Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
    GraphqlxLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlxToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
        State = smear_lexer::graphqlx::syntactic::SyntacticState,
      >,
    Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
    GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<S>>,
  {
    try_schema_default_value(inp)
  }
}

value_parser!(
  /// Parses a committed executable GraphQLx set value (`set!{ Value* }`).
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
          return unexpected_here(inp, Expectation::Bang);
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
  /// Parses a committed constant GraphQLx set value (`set!{ ConstValue* }`).
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
          return unexpected_here(inp, Expectation::Bang);
        }
        const_set_after_keyword::<Src, Ctx, false>(span.start(), inp)
      }
      Some(Spanned { span, data: token }) => {
        unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::Keyword("set"))
      }
      None => unexpected_here(inp, Expectation::Keyword("set")),
    }
  }
);

value_parser!(
  /// Parses a committed executable GraphQLx map value (`map!{ Value => Value }`).
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
          return unexpected_here(inp, Expectation::Bang);
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
  /// Parses a committed constant GraphQLx map value (`map!{ ConstValue => ConstValue }`).
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
          return unexpected_here(inp, Expectation::Bang);
        }
        const_map_after_keyword::<Src, Ctx, false>(span.start(), inp)
      }
      Some(Spanned { span, data: token }) => {
        unexpected_after_consumed::<Src, Ctx, _>(token, span, Expectation::Keyword("map"))
      }
      None => unexpected_here(inp, Expectation::Keyword("map")),
    }
  }
);
