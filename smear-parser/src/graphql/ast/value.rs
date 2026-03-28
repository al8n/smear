use crate::lexer::graphql::syntactic::{SyntacticLexer, SyntacticToken};

use super::{
  DefaultVec, Expectation, Name, SyntacticTokenError, SyntacticTokenErrors,
  next_token, name::parse_name,
};
use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use smear_lexer::tokit::{
  lexer::FromLogos,
  Emitter, InputRef, Lexer, ParseContext, SimpleSpan as Span,
  span::{self, AsSpan, IntoSpan, Spanned},
  utils::cmp::Equivalent,
};
use std::vec::Vec;
use smear_scaffold::ast as scaffold;

pub use boolean_value::*;
pub use enum_value::*;
pub use float::*;
pub use int::*;
pub use null_value::*;
pub use string::*;

/// A GraphQL value that can appear in queries and mutations.
pub type VariableValue<S> = crate::value::VariableValue<super::Name<S>>;

mod boolean_value;
mod enum_value;
mod float;
mod int;
mod null_value;
mod string;

/// List value in GraphQL (can contain variables).
pub type List<S, Container = DefaultVec<InputValue<S>>> = scaffold::List<InputValue<S>, Container>;

/// Object value in GraphQL (can contain variables).
pub type Object<S, Container = DefaultVec<InputValue<S>>> =
  scaffold::Object<Name<S>, InputValue<S>, Container>;

/// Object field in GraphQL (can contain variables).
pub type ObjectField<S> = scaffold::ObjectField<Name<S>, InputValue<S>>;

/// Constant list value in GraphQL (no variables).
pub type ConstList<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::List<ConstInputValue<S>, Container>;

/// Constant object value in GraphQL (no variables).
pub type ConstObject<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::Object<Name<S>, ConstInputValue<S>, Container>;

/// Constant object field in GraphQL (no variables).
pub type ConstObjectField<S> = scaffold::ObjectField<Name<S>, ConstInputValue<S>>;

/// GraphQL input value (executable context).
#[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum InputValue<S> {
  /// Variable reference (e.g., `$userId`).
  Variable(VariableValue<S>),
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
  List(scaffold::List<InputValue<S>>),
  /// Object value with named fields.
  Object(scaffold::Object<Name<S>, InputValue<S>>),
}

impl<S> AsSpan<Span> for InputValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Variable(v) => v.as_span(),
      Self::Boolean(v) => v.as_span(),
      Self::String(v) => v.as_span(),
      Self::Float(v) => v.as_span(),
      Self::Int(v) => v.as_span(),
      Self::Enum(v) => v.as_span(),
      Self::Null(v) => v.as_span(),
      Self::List(v) => v.as_span(),
      Self::Object(v) => v.as_span(),
    }
  }
}

impl<S> IntoSpan<Span> for InputValue<S> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Variable(v) => v.into_span(),
      Self::Boolean(v) => v.into_span(),
      Self::String(v) => v.into_span(),
      Self::Float(v) => v.into_span(),
      Self::Int(v) => v.into_span(),
      Self::Enum(v) => v.into_span(),
      Self::Null(v) => v.into_span(),
      Self::List(v) => v.into_span(),
      Self::Object(v) => v.into_span(),
    }
  }
}

/// Parses an InputValue from the input (recursive, supports variables).
pub fn parse_input_value<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<InputValue<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = smear_lexer::tokit::SimpleSpan>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let Spanned { span, data: token } = next_token(input)?;

  match token {
    SyntacticToken::LitFloat(raw) => Ok(InputValue::Float(FloatValue::new(span, raw))),
    SyntacticToken::LitInt(raw) => Ok(InputValue::Int(IntValue::new(span, raw))),
    SyntacticToken::LitInlineStr(raw) => {
      Ok(InputValue::String(StringValue::new(span, raw.into())))
    }
    SyntacticToken::LitBlockStr(raw) => {
      Ok(InputValue::String(StringValue::new(span, raw.into())))
    }
    SyntacticToken::Identifier(name) => match () {
      () if "true".equivalent(&name) => Ok(InputValue::Boolean(BooleanValue::new(span, true))),
      () if "false".equivalent(&name) => Ok(InputValue::Boolean(BooleanValue::new(span, false))),
      () if "null".equivalent(&name) => Ok(InputValue::Null(NullValue::new(span, name))),
      _ => Ok(InputValue::Enum(EnumValue::new(span, name))),
    },
    SyntacticToken::Dollar => {
      let name = parse_name(input)?;
      let full_span = Span::new(span.start(), name.span().end());
      Ok(InputValue::Variable(VariableValue::new(full_span, name)))
    }
    SyntacticToken::LBrace => {
      // Parse object fields
      let mut fields = Vec::new();
      loop {
        // Check for closing brace
        let saved = input.save();
        match next_token(input) {
          Ok(Spanned { data: SyntacticToken::RBrace, .. }) => {
            let full_span = Span::new(span.start(), input.span_since(&saved.cursor()).end());
            return Ok(InputValue::Object(scaffold::Object::new(
              Span::new(span.start(), full_span.end()),
              fields,
            )));
          }
          Ok(tok) => {
            input.restore(saved);
            // Parse field: name : value
            let field_name = parse_name(input)?;
            let _colon = next_token(input)?; // expect colon
            let value = parse_input_value(input)?;
            let field_span = Span::new(field_name.span().start(), value.as_span().end());
            fields.push(scaffold::ObjectField::new(field_span, field_name, value));
          }
          Err(_) => {
            return Err(SyntacticTokenError::unclosed_object(span).into());
          }
        }
      }
    }
    SyntacticToken::LBracket => {
      // Parse list elements
      let mut elements = Vec::new();
      loop {
        let saved = input.save();
        match next_token(input) {
          Ok(Spanned { data: SyntacticToken::RBracket, .. }) => {
            let full_span = Span::new(span.start(), input.span_since(&saved.cursor()).end());
            return Ok(InputValue::List(scaffold::List::new(
              Span::new(span.start(), full_span.end()),
              elements,
            )));
          }
          Ok(_) => {
            input.restore(saved);
            elements.push(parse_input_value(input)?);
          }
          Err(_) => {
            return Err(SyntacticTokenError::unclosed_list(span).into());
          }
        }
      }
    }
    tok => Err(SyntacticTokenError::unexpected_token(tok, Expectation::InputValue, span).into()),
  }
}

/// GraphQL constant input value (schema context).
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
  List(scaffold::List<ConstInputValue<S>>),
  /// Object value with named fields (all values must be constant).
  Object(scaffold::Object<Name<S>, ConstInputValue<S>>),
}

impl<S> AsSpan<Span> for ConstInputValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Boolean(v) => v.as_span(),
      Self::String(v) => v.as_span(),
      Self::Float(v) => v.as_span(),
      Self::Int(v) => v.as_span(),
      Self::Enum(v) => v.as_span(),
      Self::Null(v) => v.as_span(),
      Self::List(v) => v.as_span(),
      Self::Object(v) => v.as_span(),
    }
  }
}

impl<S> IntoSpan<Span> for ConstInputValue<S> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Boolean(v) => v.into_span(),
      Self::String(v) => v.into_span(),
      Self::Float(v) => v.into_span(),
      Self::Int(v) => v.into_span(),
      Self::Enum(v) => v.into_span(),
      Self::Null(v) => v.into_span(),
      Self::List(v) => v.into_span(),
      Self::Object(v) => v.into_span(),
    }
  }
}

/// Parses a ConstInputValue from the input (recursive, no variables).
pub fn parse_const_input_value<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<ConstInputValue<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = smear_lexer::tokit::SimpleSpan>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let Spanned { span, data: token } = next_token(input)?;

  match token {
    SyntacticToken::LitFloat(raw) => Ok(ConstInputValue::Float(FloatValue::new(span, raw))),
    SyntacticToken::LitInt(raw) => Ok(ConstInputValue::Int(IntValue::new(span, raw))),
    SyntacticToken::LitInlineStr(raw) => {
      Ok(ConstInputValue::String(StringValue::new(span, raw.into())))
    }
    SyntacticToken::LitBlockStr(raw) => {
      Ok(ConstInputValue::String(StringValue::new(span, raw.into())))
    }
    SyntacticToken::Identifier(name) => match () {
      () if "true".equivalent(&name) => Ok(ConstInputValue::Boolean(BooleanValue::new(span, true))),
      () if "false".equivalent(&name) => Ok(ConstInputValue::Boolean(BooleanValue::new(span, false))),
      () if "null".equivalent(&name) => Ok(ConstInputValue::Null(NullValue::new(span, name))),
      _ => Ok(ConstInputValue::Enum(EnumValue::new(span, name))),
    },
    SyntacticToken::LBrace => {
      let mut fields = Vec::new();
      loop {
        let saved = input.save();
        match next_token(input) {
          Ok(Spanned { data: SyntacticToken::RBrace, .. }) => {
            let full_span = Span::new(span.start(), input.span_since(&saved.cursor()).end());
            return Ok(ConstInputValue::Object(scaffold::Object::new(
              Span::new(span.start(), full_span.end()),
              fields,
            )));
          }
          Ok(_) => {
            input.restore(saved);
            let field_name = parse_name(input)?;
            let _colon = next_token(input)?;
            let value = parse_const_input_value(input)?;
            let field_span = Span::new(field_name.span().start(), value.as_span().end());
            fields.push(scaffold::ObjectField::new(field_span, field_name, value));
          }
          Err(_) => {
            return Err(SyntacticTokenError::unclosed_object(span).into());
          }
        }
      }
    }
    SyntacticToken::LBracket => {
      let mut elements = Vec::new();
      loop {
        let saved = input.save();
        match next_token(input) {
          Ok(Spanned { data: SyntacticToken::RBracket, .. }) => {
            let full_span = Span::new(span.start(), input.span_since(&saved.cursor()).end());
            return Ok(ConstInputValue::List(scaffold::List::new(
              Span::new(span.start(), full_span.end()),
              elements,
            )));
          }
          Ok(_) => {
            input.restore(saved);
            elements.push(parse_const_input_value(input)?);
          }
          Err(_) => {
            return Err(SyntacticTokenError::unclosed_list(span).into());
          }
        }
      }
    }
    tok => Err(SyntacticTokenError::unexpected_token(tok, Expectation::ConstInputValue, span).into()),
  }
}
