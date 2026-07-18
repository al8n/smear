//! GraphQL `Value` productions — the worked template of Phase 2S.
//!
//! Every production is a free fn generic over `L: Lexer<'inp>` and the language
//! marker `Lang`, bounded only by tokora capability traits and the [`ParseCtx`]
//! bundle, with `Span = SimpleSpan`. The
//! productions stay `Lang`-generic like the atom layer and the frozen `smear-parser`
//! crate — the dialect is fixed by the AST types, the
//! [`SyntaxKind`](crate::graphql::kinds::SyntaxKind) space, and the error they name,
//! not by the marker — so the entry runner pins the dialect
//! marker when it drives them. Each materialising node is bracketed with a [`node`]
//! event that is a no-op on the syntactic (`Fatal`/`Verbose`) emitters and records a
//! lossless CST node on a recording sink — the same production set, never a twin.
//!
//! # Dispatch discipline
//!
//! [`value`] and [`const_value`] peek the next token's kind **once** and match into
//! committed arms — jump-table style — never a chain of declining attempts.
//! The committed arm consumes the peeked token and extracts its payload directly
//! (`inp.next()` + the [`LiteralValueToken`] extractors), exactly like the
//! `enum_value` atom's body.
//!
//! On the identifier arm a slice compare resolves `true` / `false` / `null`
//! **before** the enum fallthrough (frozen `graphql/ast/value.rs` ordering, which
//! the spec blesses): those three spellings are the sole exclusion the `enum_value`
//! atom carves out of `Name`, so ruling them out first leaves the `EnumValue` arm
//! with exactly the atom's admissible set.

use smear_lexer::{LitBlockStr, LitInlineStr};
use smear_scaffold::ast as scaffold;
use tokora::{
  InputRef, Lexer, ParseInput, SimpleSpan, Token,
  emitter::CstEmitter,
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::{braces, brackets, list_of, node},
  span::AsSpan,
  token::{IdentifierToken, LitToken, PunctuatorToken, PunctuatorTokenExt},
  try_parse_input::ParseAttempt,
  utils::IntoComponents,
};

use crate::{
  combinator::{
    ErrorOf, LiteralValueToken, ParseCtx, SliceOf, colon, dollar, ident, try_dollar, try_equal,
  },
  graphql::{
    ast::{
      BooleanValue, ConstInputValue, ConstObjectField, DefaultInputValue, EnumValue, FloatValue,
      InputValue, IntValue, Name, NullValue, ObjectField, StringValue, VariableValue,
    },
    kinds::SyntaxKind as K,
  },
};

// ─── Committed leaf builders ─────────────────────────────────────────────────

/// Consumes the next token as an [`IntValue`], extracting its raw slice payload.
///
/// Committed: it consumes whatever token is next and errors on anything but an
/// integer literal. The dispatchers only reach it once a peek has classified the
/// next token as an int, so the error paths are unreachable through them; they are
/// spelled out so the leaf is a sound committed parser on its own.
///
/// Spec: [IntValue](https://spec.graphql.org/draft/#IntValue).
pub fn int_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<IntValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: LiteralValueToken<'inp, Int = SliceOf<'inp, L>>,
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

/// Consumes the next token as a [`FloatValue`], extracting its raw slice payload.
///
/// Committed, with the same discipline as [`int_value`].
///
/// Spec: [FloatValue](https://spec.graphql.org/draft/#FloatValue).
pub fn float_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<FloatValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: LiteralValueToken<'inp, Float = SliceOf<'inp, L>>,
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
#[derive(Clone, Copy)]
enum ValueHead {
  Int,
  Float,
  Str,
  Ident,
  Dollar,
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

/// Consumes an identifier already peeked as a value head and resolves it, in order,
/// to `true` / `false` → [`BooleanValue`], `null` → [`NullValue`], otherwise
/// [`EnumValue`]. Retro-wraps the consumed token in the resolved node kind.
fn identifier_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<InputValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  SliceOf<'inp, L>: AsRef<[u8]>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  let spanned = match inp.next()? {
    Some(spanned) => spanned,
    None => return Err(UnexpectedEot::eot_of(inp.offset().clone()).into()),
  };
  let slice = inp.slice();
  let (span, _token) = spanned.into_components();
  let bytes = slice.as_ref();
  let is_true = matches!(bytes, b"true");
  let is_false = matches!(bytes, b"false");
  let is_null = matches!(bytes, b"null");
  let (value, kind) = if is_true {
    (
      InputValue::Boolean(BooleanValue::new(span, true)),
      K::BooleanValue,
    )
  } else if is_false {
    (
      InputValue::Boolean(BooleanValue::new(span, false)),
      K::BooleanValue,
    )
  } else if is_null {
    (InputValue::Null(NullValue::new(span, slice)), K::NullValue)
  } else {
    (InputValue::Enum(EnumValue::new(span, slice)), K::EnumValue)
  };
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, kind.raw());
  emitter.cst_finish();
  Ok(value)
}

/// The const twin of [`identifier_value`], yielding a [`ConstInputValue`].
fn const_identifier_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ConstInputValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  SliceOf<'inp, L>: AsRef<[u8]>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  let spanned = match inp.next()? {
    Some(spanned) => spanned,
    None => return Err(UnexpectedEot::eot_of(inp.offset().clone()).into()),
  };
  let slice = inp.slice();
  let (span, _token) = spanned.into_components();
  let bytes = slice.as_ref();
  let is_true = matches!(bytes, b"true");
  let is_false = matches!(bytes, b"false");
  let is_null = matches!(bytes, b"null");
  let (value, kind) = if is_true {
    (
      ConstInputValue::Boolean(BooleanValue::new(span, true)),
      K::BooleanValue,
    )
  } else if is_false {
    (
      ConstInputValue::Boolean(BooleanValue::new(span, false)),
      K::BooleanValue,
    )
  } else if is_null {
    (
      ConstInputValue::Null(NullValue::new(span, slice)),
      K::NullValue,
    )
  } else {
    (
      ConstInputValue::Enum(EnumValue::new(span, slice)),
      K::EnumValue,
    )
  };
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, kind.raw());
  emitter.cst_finish();
  Ok(value)
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
/// their leaf nodes, an identifier → `true`/`false`/`null`/enum, `[` → a
/// [`ListValue`](InputValue::List), `{` → an [`ObjectValue`](InputValue::Object).
///
/// Spec: [Value](https://spec.graphql.org/draft/#Value).
pub fn value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<InputValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  SliceOf<'inp, L>: AsRef<[u8]> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  match classify_value_head(inp)? {
    Some(Some(ValueHead::Int)) => node(K::IntValue.raw(), int_value)
      .parse_input(inp)
      .map(InputValue::Int),
    Some(Some(ValueHead::Float)) => node(K::FloatValue.raw(), float_value)
      .parse_input(inp)
      .map(InputValue::Float),
    Some(Some(ValueHead::Str)) => node(K::StringValue.raw(), string_value)
      .parse_input(inp)
      .map(InputValue::String),
    Some(Some(ValueHead::Dollar)) => node(K::Variable.raw(), variable_value)
      .parse_input(inp)
      .map(InputValue::Variable),
    Some(Some(ValueHead::List)) => node(
      K::ListValue.raw(),
      brackets(list_of(
        value,
        <L::Token as PunctuatorTokenExt>::is_close_bracket,
      )),
    )
    .parse_input(inp)
    .map(|delimited| {
      let (span, _open, _close, items) = delimited.into_components();
      InputValue::List(scaffold::List::new(span, items))
    }),
    Some(Some(ValueHead::Object)) => node(
      K::ObjectValue.raw(),
      braces(list_of(
        object_field,
        <L::Token as PunctuatorTokenExt>::is_close_brace,
      )),
    )
    .parse_input(inp)
    .map(|delimited| {
      let (span, _open, _close, fields) = delimited.into_components();
      InputValue::Object(scaffold::Object::new(span, fields))
    }),
    Some(Some(ValueHead::Ident)) => identifier_value(inp),
    _ => unexpected_value(inp),
  }
}

/// Parses a `Value` in a constant context — the eight non-variable alternatives.
///
/// Identical dispatch to [`value`] minus the `$` arm: a leading `$` is reported as
/// an unexpected token (a variable is not a const value).
///
/// Spec: [Value](https://spec.graphql.org/draft/#Value) (const context).
pub fn const_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ConstInputValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  SliceOf<'inp, L>: AsRef<[u8]> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  match classify_value_head(inp)? {
    Some(Some(ValueHead::Int)) => node(K::IntValue.raw(), int_value)
      .parse_input(inp)
      .map(ConstInputValue::Int),
    Some(Some(ValueHead::Float)) => node(K::FloatValue.raw(), float_value)
      .parse_input(inp)
      .map(ConstInputValue::Float),
    Some(Some(ValueHead::Str)) => node(K::StringValue.raw(), string_value)
      .parse_input(inp)
      .map(ConstInputValue::String),
    Some(Some(ValueHead::List)) => node(
      K::ListValue.raw(),
      brackets(list_of(
        const_value,
        <L::Token as PunctuatorTokenExt>::is_close_bracket,
      )),
    )
    .parse_input(inp)
    .map(|delimited| {
      let (span, _open, _close, items) = delimited.into_components();
      ConstInputValue::List(scaffold::List::new(span, items))
    }),
    Some(Some(ValueHead::Object)) => node(
      K::ObjectValue.raw(),
      braces(list_of(
        const_object_field,
        <L::Token as PunctuatorTokenExt>::is_close_brace,
      )),
    )
    .parse_input(inp)
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
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  SliceOf<'inp, L>: AsRef<[u8]> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  let (name_span, name_src) = ident(inp)?.into_components();
  let name = Name::new(name_span, name_src);
  colon(inp)?;
  let value = value(inp)?;
  let span = SimpleSpan::new(name.span().start(), value.as_span().end());
  let field = scaffold::ObjectField::new(span, name, value);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::ObjectField.raw());
  emitter.cst_finish();
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
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  SliceOf<'inp, L>: AsRef<[u8]> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  let (name_span, name_src) = ident(inp)?.into_components();
  let name = Name::new(name_span, name_src);
  colon(inp)?;
  let value = const_value(inp)?;
  let span = SimpleSpan::new(name.span().start(), value.as_span().end());
  let field = scaffold::ObjectField::new(span, name, value);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::ObjectField.raw());
  emitter.cst_finish();
  Ok(field)
}

// ─── Default value ───────────────────────────────────────────────────────────

/// Parses an optional `DefaultValue` (`= ConstValue`).
///
/// Declines to `None` (no tokens consumed) unless the next token is `=`, in which
/// case it commits to the following const value and wraps the whole `= value` in a
/// [`DefaultValue`](crate::graphql::kinds::SyntaxKind::DefaultValue) node.
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
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  SliceOf<'inp, L>: AsRef<[u8]> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  // Retro-wrap: mark before the `=`, so a present default wraps `= ConstValue`; a
  // decline (no `=`) leaves the tombstone unspent — no node — and consumes nothing.
  let mark = inp.emitter().cst_mark();
  let cursor = inp.cursor().clone();
  match try_equal(inp)? {
    ParseAttempt::Accept(_equal) => {
      let value = const_value(inp)?;
      let span = inp.span_since(&cursor);
      let default = DefaultInputValue::new(span, value);
      let emitter = inp.emitter();
      emitter.cst_start_at(mark, K::DefaultValue.raw());
      emitter.cst_finish();
      Ok(Some(default))
    }
    ParseAttempt::Decline => Ok(None),
  }
}
