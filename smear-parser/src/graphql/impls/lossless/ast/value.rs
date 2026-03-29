use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use smear_lexer::tokit::{
  lexer::FromLogos,
  Emitter, InputRef, Lexer, ParseContext, SimpleSpan as Span,
  span::Spanned,
  utils::cmp::Equivalent,
};

use crate::lexer::graphql::lossless::{LosslessLexer, LosslessToken};
use crate::graphql::ast::Name;
use crate::graphql::Expectation;
use crate::value::{
  BooleanValue, EnumValue, FloatValue, IntValue, NullValue, StringValue, VariableValue,
};
use smear_scaffold::ast as scaffold;
use super::{
  LosslessTokenError, LosslessTokenErrors, next_token,
  list::parse_list,
  name::parse_name,
  object::{self, ObjectField, parse_object},
  padded::Padded,
};

/// GraphQL input value for CST (preserves trivia, supports variables).
#[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum InputValue<S> {
  /// Variable reference (e.g., `$userId`).
  Variable(VariableValue<Name<S>>),
  /// Boolean value (`true` or `false`).
  Boolean(BooleanValue),
  /// String value (inline or block string).
  String(StringValue<S>),
  /// Floating-point number.
  Float(FloatValue<S>),
  /// Integer number.
  Int(IntValue<S>),
  /// Enum value name.
  Enum(EnumValue<S>),
  /// The `null` literal.
  Null(NullValue<S>),
  /// List of values.
  List(scaffold::List<Padded<InputValue<S>, S>>),
  /// Object value with named fields.
  Object(object::CstObject<InputValue<S>, S>),
}

/// Parses an InputValue from the lossless input (recursive, supports variables).
pub fn parse_input_value<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
) -> Result<InputValue<S>, LosslessTokenErrors<S>>
where
  S: Clone,
  LosslessToken<S>: FromLogos<'inp>,
  LosslessLexer<'inp, S>: Lexer<'inp, Token = LosslessToken<S>, Span = smear_lexer::tokit::SimpleSpan>,
  Ctx: ParseContext<'inp, LosslessLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, LosslessLexer<'inp, S>, Lang, Error = LosslessTokenErrors<S>>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let saved = input.save();
  let Spanned { span, data: token } = next_token(input)?;

  match token {
    LosslessToken::LitFloat(raw) => Ok(InputValue::Float(FloatValue::new(span, raw))),
    LosslessToken::LitInt(raw) => Ok(InputValue::Int(IntValue::new(span, raw))),
    LosslessToken::LitInlineStr(raw) => {
      Ok(InputValue::String(StringValue::new(span, raw.into())))
    }
    LosslessToken::LitBlockStr(raw) => {
      Ok(InputValue::String(StringValue::new(span, raw.into())))
    }
    LosslessToken::Identifier(name) => match () {
      () if "true".equivalent(&name) => Ok(InputValue::Boolean(BooleanValue::new(span, true))),
      () if "false".equivalent(&name) => Ok(InputValue::Boolean(BooleanValue::new(span, false))),
      () if "null".equivalent(&name) => Ok(InputValue::Null(NullValue::new(span, name))),
      _ => Ok(InputValue::Enum(EnumValue::new(span, name))),
    },
    LosslessToken::Dollar => {
      let name = parse_name(input)?;
      let full_span = Span::new(span.start(), name.span().end());
      Ok(InputValue::Variable(VariableValue::new(full_span, name)))
    }
    LosslessToken::LBracket => {
      // Restore and re-parse as list (parse_list expects the bracket)
      input.restore(saved);
      let list = parse_list(input, parse_input_value)?;
      Ok(InputValue::List(list))
    }
    LosslessToken::LBrace => {
      // Restore and re-parse as object (parse_object expects the brace)
      input.restore(saved);
      let obj = parse_object(input, parse_input_value)?;
      Ok(InputValue::Object(obj))
    }
    tok => Err(LosslessTokenError::unexpected_token(tok, Expectation::InputValue, span).into()),
  }
}

/// GraphQL constant input value for CST (preserves trivia, no variables).
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ConstInputValue<S> {
  /// Boolean value (`true` or `false`).
  Boolean(BooleanValue),
  /// String value (inline or block string).
  String(StringValue<S>),
  /// Floating-point number.
  Float(FloatValue<S>),
  /// Integer number.
  Int(IntValue<S>),
  /// Enum value name.
  Enum(EnumValue<S>),
  /// The `null` literal.
  Null(NullValue<S>),
  /// List of constant values.
  List(scaffold::List<Padded<ConstInputValue<S>, S>>),
  /// Object value with named fields (all values must be constant).
  Object(object::CstObject<ConstInputValue<S>, S>),
}

/// Parses a ConstInputValue from the lossless input (recursive, no variables).
pub fn parse_const_input_value<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
) -> Result<ConstInputValue<S>, LosslessTokenErrors<S>>
where
  S: Clone,
  LosslessToken<S>: FromLogos<'inp>,
  LosslessLexer<'inp, S>: Lexer<'inp, Token = LosslessToken<S>, Span = smear_lexer::tokit::SimpleSpan>,
  Ctx: ParseContext<'inp, LosslessLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, LosslessLexer<'inp, S>, Lang, Error = LosslessTokenErrors<S>>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let saved = input.save();
  let Spanned { span, data: token } = next_token(input)?;

  match token {
    LosslessToken::LitFloat(raw) => Ok(ConstInputValue::Float(FloatValue::new(span, raw))),
    LosslessToken::LitInt(raw) => Ok(ConstInputValue::Int(IntValue::new(span, raw))),
    LosslessToken::LitInlineStr(raw) => {
      Ok(ConstInputValue::String(StringValue::new(span, raw.into())))
    }
    LosslessToken::LitBlockStr(raw) => {
      Ok(ConstInputValue::String(StringValue::new(span, raw.into())))
    }
    LosslessToken::Identifier(name) => match () {
      () if "true".equivalent(&name) => Ok(ConstInputValue::Boolean(BooleanValue::new(span, true))),
      () if "false".equivalent(&name) => Ok(ConstInputValue::Boolean(BooleanValue::new(span, false))),
      () if "null".equivalent(&name) => Ok(ConstInputValue::Null(NullValue::new(span, name))),
      _ => Ok(ConstInputValue::Enum(EnumValue::new(span, name))),
    },
    LosslessToken::LBracket => {
      input.restore(saved);
      let list = parse_list(input, parse_const_input_value)?;
      Ok(ConstInputValue::List(list))
    }
    LosslessToken::LBrace => {
      input.restore(saved);
      let obj = parse_object(input, parse_const_input_value)?;
      Ok(ConstInputValue::Object(obj))
    }
    tok => Err(LosslessTokenError::unexpected_token(tok, Expectation::ConstInputValue, span).into()),
  }
}
