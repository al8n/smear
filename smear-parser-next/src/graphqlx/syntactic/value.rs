//! GraphQLx `Value` productions — the value family with the GraphQLx composites.
//!
//! Every production mirrors the GraphQL value template
//! ([`graphql::syntactic::value`](crate::graphql::syntactic::value)): free fns
//! generic over `L: Lexer<'inp>` and the language marker `Lang`, bounded only by
//! tokora capability traits and the [`ParseCtx`] bundle, with `Span = SimpleSpan`.
//!
//! GraphQLx adds four things to the shared value core:
//!
//! - int / float literals carry the **radix-preserving** payloads
//!   ([`LitInt`] / [`LitFloat`]) rather than a bare slice, so decimal / hex / octal /
//!   binary ints and decimal / hex floats survive into the AST;
//! - the enum value is a `::`-**path composite** ([`EnumValue`]), not a bare token:
//!   a bare identifier is a single-segment relative path, a leading `::` opens a
//!   fully-qualified multi-segment path (frozen `graphqlx/ast/value.rs:157-265`);
//! - `set { … }` and `map { key => value, … }` composites, dispatched off the `set`
//!   / `map` soft keyword by a slice compare, committed on the following `{`;
//! - the `{ … }` object and `[ … ]` list forms, shared with GraphQL.
//!
//! # Dispatch discipline
//!
//! [`value`] / [`const_value`] peek the next token's kind **once** and match into
//! committed arms — jump-table style. On the identifier arm a slice compare resolves
//! `true` / `false` / `null` **before** `set` / `map`, and only then the enum
//! fallthrough (frozen ordering, spec-correct): those spellings are the sole
//! exclusions the enum-value path carves out of a bare `Name`.

use smear_lexer::{
  LitBlockStr, LitInlineStr,
  graphqlx::{LitFloat, LitInt},
};
use smear_scaffold::ast as scaffold;
use tokora::{
  InputRef, Lexer, SimpleSpan, Token,
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::{braces, brackets, list_of},
  span::AsSpan,
  token::{IdentifierToken, LitToken, PunctuatorToken, PunctuatorTokenExt},
  try_parse_input::ParseAttempt,
  utils::IntoComponents,
};

use super::peeks_where;
use crate::{
  combinator::{
    Equivalent, ErrorOf, LiteralValueToken, ParseCtx, SliceOf, colon, dollar, fat_arrow, ident,
    path_sep, try_dollar, try_equal, try_path_sep,
  },
  graphqlx::ast::{
    BooleanValue, ConstInputValue, ConstMapEntry, ConstObjectField, DefaultInputValue, EnumValue,
    FloatValue, InputValue, IntValue, MapEntry, Name, NullValue, ObjectField, Path, StringValue,
    VariableValue,
  },
};

// ─── Committed leaf builders ─────────────────────────────────────────────────

/// Consumes the next token as an [`IntValue`], extracting its radix-preserving
/// [`LitInt`] payload.
///
/// Committed: it consumes whatever token is next and errors on anything but an
/// integer literal. The dispatchers only reach it once a peek has classified the
/// next token as an int, so the error paths are unreachable through them; they are
/// spelled out so the leaf is a sound committed parser on its own.
///
/// Spec: [IntValue](https://spec.graphql.org/draft/#IntValue) (GraphQLx: any radix).
pub fn int_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<IntValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: LiteralValueToken<'inp, Int = LitInt<SliceOf<'inp, L>>>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  match inp.next()? {
    Some(spanned) => {
      let (span, token) = spanned.into_components();
      match <L::Token as LiteralValueToken>::into_int(token) {
        Ok(raw) => Ok(IntValue::new(span, raw)),
        Err(token) => Err(UnexpectedToken::of(span).with_found(token).into()),
      }
    }
    None => Err(UnexpectedEot::eot_of(inp.offset().clone()).into()),
  }
}

/// Consumes the next token as a [`FloatValue`], extracting its radix-preserving
/// [`LitFloat`] payload.
///
/// Committed, with the same discipline as [`int_value`].
///
/// Spec: [FloatValue](https://spec.graphql.org/draft/#FloatValue) (GraphQLx: decimal
/// or hex).
pub fn float_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<FloatValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: LiteralValueToken<'inp, Float = LitFloat<SliceOf<'inp, L>>>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  match inp.next()? {
    Some(spanned) => {
      let (span, token) = spanned.into_components();
      match <L::Token as LiteralValueToken>::into_float(token) {
        Ok(raw) => Ok(FloatValue::new(span, raw)),
        Err(token) => Err(UnexpectedToken::of(span).with_found(token).into()),
      }
    }
    None => Err(UnexpectedEot::eot_of(inp.offset().clone()).into()),
  }
}

/// Consumes the next token as a [`StringValue`] — inline or block — wrapping the
/// carrier into the node's `LitStr` via `into`.
///
/// Committed, with the same discipline as [`int_value`].
///
/// Spec: [StringValue](https://spec.graphql.org/draft/#StringValue).
pub fn string_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<StringValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: LiteralValueToken<
      'inp,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  match inp.next()? {
    Some(spanned) => {
      let (span, token) = spanned.into_components();
      match <L::Token as LiteralValueToken>::into_inline_str(token) {
        Ok(inline) => Ok(StringValue::new(span, inline.into())),
        Err(token) => match <L::Token as LiteralValueToken>::into_block_str(token) {
          Ok(block) => Ok(StringValue::new(span, block.into())),
          Err(token) => Err(UnexpectedToken::of(span).with_found(token).into()),
        },
      }
    }
    None => Err(UnexpectedEot::eot_of(inp.offset().clone()).into()),
  }
}

// ─── Variable ────────────────────────────────────────────────────────────────

/// Parses a [`VariableValue`] (`$name`), committed on the leading `$`.
///
/// Spec: [Variable](https://spec.graphql.org/draft/#Variable).
pub fn variable_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<VariableValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let dollar = dollar(inp)?;
  let (name_span, name_src) = ident(inp)?.into_components();
  let name = Name::new(name_span, name_src);
  let span = SimpleSpan::new(dollar.span().start(), name.span().end());
  Ok(VariableValue::new(span, name))
}

/// Declines (no tokens consumed) unless the next token is a `$`, in which case it
/// commits to the following name and yields a [`VariableValue`].
///
/// The attempt boundary is the `$` alone: once consumed, a missing name is an
/// error, never a decline.
///
/// Spec: [Variable](https://spec.graphql.org/draft/#Variable).
// The `Result<ParseAttempt<…>, …>` return is inherent to a declining generic
// production; factoring it into an alias would only move the same generics.
#[allow(clippy::type_complexity)]
pub fn try_variable_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ParseAttempt<VariableValue<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  match try_dollar(inp)? {
    ParseAttempt::Accept(dollar) => {
      let (name_span, name_src) = ident(inp)?.into_components();
      let name = Name::new(name_span, name_src);
      let span = SimpleSpan::new(dollar.span().start(), name.span().end());
      Ok(ParseAttempt::Accept(VariableValue::new(span, name)))
    }
    ParseAttempt::Decline => Ok(ParseAttempt::Decline),
  }
}

// ─── Dispatch ────────────────────────────────────────────────────────────────

/// The classified head of a value: the token kind the one-token peek resolves to.
///
/// A superset of the GraphQL heads: it adds [`PathSep`](ValueHead::PathSep) for a
/// leading `::` (a fully-qualified enum path).
#[derive(Clone, Copy)]
enum ValueHead {
  Int,
  Float,
  Str,
  Ident,
  Dollar,
  PathSep,
  List,
  Object,
}

/// Peeks the next token (without consuming it) and classifies it into a
/// [`ValueHead`]. `Ok(None)` is end of input; `Ok(Some(None))` is a token that
/// begins no value; `Ok(Some(Some(head)))` is a recognised head, and the token is
/// still in place for the committed arm.
fn classify_value_head<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<Option<ValueHead>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp>,
  L::Token: LitToken<'inp> + PunctuatorToken<'inp> + IdentifierToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
{
  let mut outcome = None;
  inp.try_expect(|spanned| {
    let t: &L::Token = spanned.data;
    let head = if <L::Token as LitToken>::is_integer_literal(t) {
      Some(ValueHead::Int)
    } else if <L::Token as LitToken>::is_float_literal(t)
      || <L::Token as LitToken>::is_hex_float_literal(t)
    {
      Some(ValueHead::Float)
    } else if <L::Token as LitToken>::is_string_literal(t) {
      Some(ValueHead::Str)
    } else if <L::Token as PunctuatorTokenExt>::is_dollar(t) {
      Some(ValueHead::Dollar)
    } else if <L::Token as PunctuatorTokenExt>::is_double_colon(t) {
      Some(ValueHead::PathSep)
    } else if <L::Token as PunctuatorTokenExt>::is_open_bracket(t) {
      Some(ValueHead::List)
    } else if <L::Token as PunctuatorTokenExt>::is_open_brace(t) {
      Some(ValueHead::Object)
    } else if <L::Token as IdentifierToken>::is_identifier(t) {
      Some(ValueHead::Ident)
    } else {
      None
    };
    outcome = Some(head);
    false
  })?;
  Ok(outcome)
}

/// Consumes an identifier already peeked as a value head and resolves it, in order:
/// `true` / `false` → [`BooleanValue`], `null` → [`NullValue`], `set` / `map`
/// followed by `{` → the [set](InputValue::Set) / [map](InputValue::Map) composite,
/// and otherwise a single-segment enum [`Path`] ([`EnumValue`]).
fn identifier_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<InputValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = LitInt<SliceOf<'inp, L>>,
      Float = LitFloat<SliceOf<'inp, L>>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let spanned = match inp.next()? {
    Some(spanned) => spanned,
    None => return Err(UnexpectedEot::eot_of(inp.offset().clone()).into()),
  };
  let slice = inp.slice();
  let (span, _token) = spanned.into_components();
  if slice.equivalent("true") || slice.equivalent("false") {
    let value = InputValue::Boolean(BooleanValue::new(span, slice.equivalent("true")));
    return Ok(value);
  }
  if slice.equivalent("null") {
    let value = InputValue::Null(NullValue::new(span, slice));
    return Ok(value);
  }
  if slice.equivalent("set") && peeks_where(inp, <L::Token as PunctuatorTokenExt>::is_open_brace)? {
    let delimited = braces(list_of(
      value,
      <L::Token as PunctuatorTokenExt>::is_close_brace,
    ))(inp)?;
    let (body_span, _open, _close, items) = delimited.into_components();
    let full = SimpleSpan::new(span.start(), body_span.end());
    let out = InputValue::Set(scaffold::Set::new(full, items));
    return Ok(out);
  }
  if slice.equivalent("map") && peeks_where(inp, <L::Token as PunctuatorTokenExt>::is_open_brace)? {
    let delimited = braces(list_of(
      map_entry,
      <L::Token as PunctuatorTokenExt>::is_close_brace,
    ))(inp)?;
    let (body_span, _open, _close, entries) = delimited.into_components();
    let full = SimpleSpan::new(span.start(), body_span.end());
    let out = InputValue::Map(scaffold::Map::new(full, entries));
    return Ok(out);
  }
  // Bare identifier (including `set` / `map` not followed by `{`): a single-segment
  // relative enum path (frozen `graphqlx/ast/value.rs` — no `::` continuation here).
  let out = InputValue::Enum(EnumValue::new(Path::from(Name::new(span, slice))));
  Ok(out)
}

/// The const twin of [`identifier_value`], yielding a [`ConstInputValue`].
fn const_identifier_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ConstInputValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = LitInt<SliceOf<'inp, L>>,
      Float = LitFloat<SliceOf<'inp, L>>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let spanned = match inp.next()? {
    Some(spanned) => spanned,
    None => return Err(UnexpectedEot::eot_of(inp.offset().clone()).into()),
  };
  let slice = inp.slice();
  let (span, _token) = spanned.into_components();
  if slice.equivalent("true") || slice.equivalent("false") {
    let value = ConstInputValue::Boolean(BooleanValue::new(span, slice.equivalent("true")));
    return Ok(value);
  }
  if slice.equivalent("null") {
    let value = ConstInputValue::Null(NullValue::new(span, slice));
    return Ok(value);
  }
  if slice.equivalent("set") && peeks_where(inp, <L::Token as PunctuatorTokenExt>::is_open_brace)? {
    let delimited = braces(list_of(
      const_value,
      <L::Token as PunctuatorTokenExt>::is_close_brace,
    ))(inp)?;
    let (body_span, _open, _close, items) = delimited.into_components();
    let full = SimpleSpan::new(span.start(), body_span.end());
    let out = ConstInputValue::Set(scaffold::Set::new(full, items));
    return Ok(out);
  }
  if slice.equivalent("map") && peeks_where(inp, <L::Token as PunctuatorTokenExt>::is_open_brace)? {
    let delimited = braces(list_of(
      const_map_entry,
      <L::Token as PunctuatorTokenExt>::is_close_brace,
    ))(inp)?;
    let (body_span, _open, _close, entries) = delimited.into_components();
    let full = SimpleSpan::new(span.start(), body_span.end());
    let out = ConstInputValue::Map(scaffold::Map::new(full, entries));
    return Ok(out);
  }
  let out = ConstInputValue::Enum(EnumValue::new(Path::from(Name::new(span, slice))));
  Ok(out)
}

/// Parses a fully-qualified enum path opened by a leading `::` (`::a::b::C`).
///
/// Committed on the `::`; the segments after it are gathered by a commit-first
/// `try_path_sep` loop (never a separated1 with an identifier follow-set — the
/// W5 bug class).
fn leading_enum_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Path<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  path_sep(inp)?;
  let mut segments = alloc_segments(inp)?;
  while let ParseAttempt::Accept(_sep) = try_path_sep(inp)? {
    let (seg_span, seg_src) = ident(inp)?.into_components();
    segments.push(Name::new(seg_span, seg_src));
  }
  let span = inp.span_since(&cursor);
  let path = Path::new(span, segments, true);
  Ok(path)
}

/// Parses the first path segment (a required `Name`) into a fresh segment vector.
///
/// The `Vec<Name<…>>` return is inherent to the path segment container; factoring it
/// into an alias would only move the same generics.
#[allow(clippy::type_complexity)]
fn alloc_segments<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<std::vec::Vec<Name<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let (seg_span, seg_src) = ident(inp)?.into_components();
  Ok(std::vec::Vec::from_iter([Name::new(seg_span, seg_src)]))
}

/// The shared error tail of the value dispatchers: reports the offending token as
/// an unexpected token, or end of input.
fn unexpected_value<'inp, L, Ctx, T, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<T, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  match inp.next()? {
    Some(spanned) => {
      let (span, token) = spanned.into_components();
      Err(UnexpectedToken::of(span).with_found(token).into())
    }
    None => Err(UnexpectedEot::eot_of(inp.offset().clone()).into()),
  }
}

/// Parses a (non-const) `Value`.
///
/// One peek, committed arms: `$` → [`VariableValue`], int/float/string literals →
/// their leaf nodes, `::` → a fully-qualified enum path, an identifier →
/// `true`/`false`/`null` / `set`/`map` composite / relative enum path, `[` → a
/// [`ListValue`](InputValue::List), `{` → an [`ObjectValue`](InputValue::Object).
///
/// Spec: [Value](https://spec.graphql.org/draft/#Value) (GraphQLx).
pub fn value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<InputValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = LitInt<SliceOf<'inp, L>>,
      Float = LitFloat<SliceOf<'inp, L>>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  match classify_value_head(inp)? {
    Some(Some(ValueHead::Int)) => int_value(inp).map(InputValue::Int),
    Some(Some(ValueHead::Float)) => float_value(inp).map(InputValue::Float),
    Some(Some(ValueHead::Str)) => string_value(inp).map(InputValue::String),
    Some(Some(ValueHead::Dollar)) => variable_value(inp).map(InputValue::Variable),
    Some(Some(ValueHead::PathSep)) => {
      leading_enum_value(inp).map(|p| InputValue::Enum(EnumValue::new(p)))
    }
    Some(Some(ValueHead::List)) => brackets(list_of(
      value,
      <L::Token as PunctuatorTokenExt>::is_close_bracket,
    ))(inp)
    .map(|delimited| {
      let (span, _open, _close, items) = delimited.into_components();
      InputValue::List(scaffold::List::new(span, items))
    }),
    Some(Some(ValueHead::Object)) => braces(list_of(
      object_field,
      <L::Token as PunctuatorTokenExt>::is_close_brace,
    ))(inp)
    .map(|delimited| {
      let (span, _open, _close, fields) = delimited.into_components();
      InputValue::Object(scaffold::Object::new(span, fields))
    }),
    Some(Some(ValueHead::Ident)) => identifier_value(inp),
    _ => unexpected_value(inp),
  }
}

/// Parses a `Value` in a constant context — the non-variable alternatives.
///
/// Identical dispatch to [`value`] minus the `$` arm: a leading `$` is reported as
/// an unexpected token (a variable is not a const value).
///
/// Spec: [Value](https://spec.graphql.org/draft/#Value) (GraphQLx, const context).
pub fn const_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ConstInputValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = LitInt<SliceOf<'inp, L>>,
      Float = LitFloat<SliceOf<'inp, L>>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  match classify_value_head(inp)? {
    Some(Some(ValueHead::Int)) => int_value(inp).map(ConstInputValue::Int),
    Some(Some(ValueHead::Float)) => float_value(inp).map(ConstInputValue::Float),
    Some(Some(ValueHead::Str)) => string_value(inp).map(ConstInputValue::String),
    Some(Some(ValueHead::PathSep)) => {
      leading_enum_value(inp).map(|p| ConstInputValue::Enum(EnumValue::new(p)))
    }
    Some(Some(ValueHead::List)) => brackets(list_of(
      const_value,
      <L::Token as PunctuatorTokenExt>::is_close_bracket,
    ))(inp)
    .map(|delimited| {
      let (span, _open, _close, items) = delimited.into_components();
      ConstInputValue::List(scaffold::List::new(span, items))
    }),
    Some(Some(ValueHead::Object)) => braces(list_of(
      const_object_field,
      <L::Token as PunctuatorTokenExt>::is_close_brace,
    ))(inp)
    .map(|delimited| {
      let (span, _open, _close, fields) = delimited.into_components();
      ConstInputValue::Object(scaffold::Object::new(span, fields))
    }),
    Some(Some(ValueHead::Ident)) => const_identifier_value(inp),
    _ => unexpected_value(inp),
  }
}

// ─── Object fields ───────────────────────────────────────────────────────────

/// Parses a (non-const) `ObjectField` (`name : Value`).
///
/// Spec: [ObjectField](https://spec.graphql.org/draft/#ObjectField).
pub fn object_field<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ObjectField<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = LitInt<SliceOf<'inp, L>>,
      Float = LitFloat<SliceOf<'inp, L>>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let (name_span, name_src) = ident(inp)?.into_components();
  let name = Name::new(name_span, name_src);
  colon(inp)?;
  let value = value(inp)?;
  let span = SimpleSpan::new(name.span().start(), value.as_span().end());
  let field = scaffold::ObjectField::new(span, name, value);
  Ok(field)
}

/// Parses a constant `ObjectField` (`name : ConstValue`).
///
/// Spec: [ObjectField](https://spec.graphql.org/draft/#ObjectField) (const context).
pub fn const_object_field<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ConstObjectField<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = LitInt<SliceOf<'inp, L>>,
      Float = LitFloat<SliceOf<'inp, L>>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let (name_span, name_src) = ident(inp)?.into_components();
  let name = Name::new(name_span, name_src);
  colon(inp)?;
  let value = const_value(inp)?;
  let span = SimpleSpan::new(name.span().start(), value.as_span().end());
  let field = scaffold::ObjectField::new(span, name, value);
  Ok(field)
}

// ─── Map entries ───────────────────────────────────────────────────────────

/// Parses a (non-const) `MapEntry` (`key => value`).
///
/// A literal `=>` is required between the key and value (the fixture grammar and the
/// scaffold `MapEntry` shape). This is stricter than the frozen
/// `parse_input_value`, which consumes the arrow position *unchecked*; W7 enforces
/// the arrow with a rejection regression (Deviations Register).
///
/// Grammar: `MapEntry : Value => Value`.
pub fn map_entry<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<MapEntry<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = LitInt<SliceOf<'inp, L>>,
      Float = LitFloat<SliceOf<'inp, L>>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let key = value(inp)?;
  fat_arrow(inp)?;
  let val = value(inp)?;
  let span = SimpleSpan::new(key.as_span().start(), val.as_span().end());
  let entry = scaffold::MapEntry::new(span, key, val);
  Ok(entry)
}

/// Parses a constant `MapEntry` (`key => value`), keys and values both const.
///
/// Grammar: `MapEntry : ConstValue => ConstValue`.
pub fn const_map_entry<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ConstMapEntry<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = LitInt<SliceOf<'inp, L>>,
      Float = LitFloat<SliceOf<'inp, L>>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let key = const_value(inp)?;
  fat_arrow(inp)?;
  let val = const_value(inp)?;
  let span = SimpleSpan::new(key.as_span().start(), val.as_span().end());
  let entry = scaffold::MapEntry::new(span, key, val);
  Ok(entry)
}

// ─── Default value ───────────────────────────────────────────────────────────

/// Parses an optional `DefaultValue` (`= ConstValue`).
///
/// Declines to `None` (no tokens consumed) unless the next token is `=`, in which
/// case it commits to the following const value.
///
/// Spec: [DefaultValue](https://spec.graphql.org/draft/#DefaultValue).
// The `Result<Option<…>, …>` return is inherent to an optional generic production.
#[allow(clippy::type_complexity)]
pub fn default_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<DefaultInputValue<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = LitInt<SliceOf<'inp, L>>,
      Float = LitFloat<SliceOf<'inp, L>>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  match try_equal(inp)? {
    ParseAttempt::Accept(_equal) => {
      let value = const_value(inp)?;
      let span = inp.span_since(&cursor);
      let default = DefaultInputValue::new(span, value);
      Ok(Some(default))
    }
    ParseAttempt::Decline => Ok(None),
  }
}

#[cfg(test)]
mod tests;
