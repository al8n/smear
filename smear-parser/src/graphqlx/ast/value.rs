use crate::{hints::VariableValueHint, ident::Ident};

use super::{DefaultVec, Expectation, SyntacticTokenError, SyntacticTokenErrors, next_token};
use crate::lexer::graphqlx::syntactic::{SyntacticLexer, SyntacticToken};

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use smear_lexer::tokora::{
  Emitter, InputRef, Lexer, ParseContext, SimpleSpan as Span,
  lexer::FromLogos,
  span::{AsSpan, IntoSpan, Spanned},
  utils::cmp::Equivalent,
};
use smear_scaffold::{
  ast::{self as scaffold, Path},
  error::{UnclosedBraceError, UnclosedBracketError},
};
use std::vec::Vec;

pub use boolean_value::*;
pub use enum_value::*;
pub use float::*;
pub use int::*;
pub use null_value::*;
pub use string::*;

/// A variable value reference in GraphQLx.
pub type VariableValue<S> = crate::value::VariableValue<Ident<S>>;

mod boolean_value;
mod enum_value;
mod float;
mod int;
mod null_value;
mod string;

/// A list value in GraphQLx that can contain any input value.
pub type List<S, Container = DefaultVec<InputValue<S>>> = scaffold::List<InputValue<S>, Container>;
/// A set value in GraphQLx containing unique input values.
pub type Set<S, Container = DefaultVec<InputValue<S>>> = scaffold::Set<InputValue<S>, Container>;
/// A key-value entry in a map.
pub type MapEntry<S> = scaffold::MapEntry<InputValue<S>, InputValue<S>>;
/// A map value in GraphQLx containing key-value pairs.
pub type Map<S, Container = DefaultVec<(InputValue<S>, InputValue<S>)>> =
  scaffold::Map<InputValue<S>, InputValue<S>, Container>;
/// An object value in GraphQLx containing named fields.
pub type Object<S, Container = DefaultVec<InputValue<S>>> =
  scaffold::Object<Ident<S>, InputValue<S>, Container>;
/// A field in an object value.
pub type ObjectField<S> = scaffold::ObjectField<Ident<S>, InputValue<S>>;

/// A constant list value in GraphQLx (cannot contain variables).
pub type ConstList<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::List<ConstInputValue<S>, Container>;
/// A constant set value in GraphQLx (cannot contain variables).
pub type ConstSet<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::Set<ConstInputValue<S>, Container>;
/// A constant key-value entry in a map.
pub type ConstMapEntry<S> = scaffold::MapEntry<ConstInputValue<S>, ConstInputValue<S>>;
/// A constant map value in GraphQLx (cannot contain variables).
pub type ConstMap<S, Container = DefaultVec<(ConstInputValue<S>, ConstInputValue<S>)>> =
  scaffold::Map<ConstInputValue<S>, ConstInputValue<S>, Container>;
/// A constant object value in GraphQLx (cannot contain variables).
pub type ConstObject<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::Object<Ident<S>, ConstInputValue<S>, Container>;
/// A field in a constant object value.
pub type ConstObjectField<S> = scaffold::ObjectField<Ident<S>, ConstInputValue<S>>;

/// GraphQLx Input Value
#[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum InputValue<S> {
  /// GraphQLx Variable
  Variable(VariableValue<S>),
  /// GraphQLx Boolean
  Boolean(BooleanValue),
  /// GraphQLx String
  String(StringValue<S>),
  /// GraphQLx Float
  Float(FloatValue<S>),
  /// GraphQLx Int
  Int(IntValue<S>),
  /// GraphQLx Enum
  Enum(EnumValue<S>),
  /// GraphQLx Null
  Null(NullValue<S>),
  /// GraphQLx List
  List(scaffold::List<InputValue<S>>),
  /// GraphQLx Set
  Set(scaffold::Set<InputValue<S>>),
  /// GraphQLx Map
  Map(scaffold::Map<InputValue<S>, InputValue<S>>),
  /// GraphQLx Object
  Object(scaffold::Object<Ident<S>, InputValue<S>>),
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
      Self::Set(v) => v.as_span(),
      Self::Map(v) => v.as_span(),
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
      Self::Set(v) => v.into_span(),
      Self::Map(v) => v.into_span(),
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
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  use super::ident::parse_name;

  let Spanned { span, data: token } = next_token(input)?;

  match token {
    SyntacticToken::LitFloat(raw) => Ok(InputValue::Float(FloatValue::new(span, raw))),
    SyntacticToken::LitInt(raw) => Ok(InputValue::Int(IntValue::new(span, raw))),
    SyntacticToken::LitInlineStr(raw) => Ok(InputValue::String(StringValue::new(span, raw.into()))),
    SyntacticToken::LitBlockStr(raw) => Ok(InputValue::String(StringValue::new(span, raw.into()))),
    SyntacticToken::PathSeparator => {
      // Parse path segments after ::
      let mut segments = Vec::new();
      segments.push(parse_name(input)?);
      loop {
        let saved = input.save();
        match next_token(input) {
          Ok(Spanned {
            data: SyntacticToken::PathSeparator,
            ..
          }) => {
            segments.push(parse_name(input)?);
          }
          _ => {
            input.restore(saved);
            break;
          }
        }
      }
      let full_span = Span::new(span.start(), input.span_since(&input.cursor()).end());
      Ok(InputValue::Enum(EnumValue::new(Path::new(
        full_span, segments, true,
      ))))
    }
    SyntacticToken::Identifier(name) => match () {
      () if "true".equivalent(&name) => Ok(InputValue::Boolean(BooleanValue::new(span, true))),
      () if "false".equivalent(&name) => Ok(InputValue::Boolean(BooleanValue::new(span, false))),
      () if "null".equivalent(&name) => Ok(InputValue::Null(NullValue::new(span, name))),
      () if "set".equivalent(&name) => {
        let saved = input.save();
        match next_token(input) {
          Ok(Spanned {
            data: SyntacticToken::LBrace,
            ..
          }) => {
            let mut values = Vec::new();
            loop {
              let saved2 = input.save();
              match next_token(input) {
                Ok(Spanned {
                  data: SyntacticToken::RBrace,
                  ..
                }) => {
                  let full_span = Span::new(span.start(), input.span_since(&saved2.cursor()).end());
                  return Ok(InputValue::Set(Set::new(full_span, values)));
                }
                Ok(_) => {
                  input.restore(saved2);
                  values.push(parse_input_value(input)?);
                }
                Err(_) => {
                  return Err(SyntacticTokenError::unclosed_brace(span).into());
                }
              }
            }
          }
          _ => {
            input.restore(saved);
            Ok(InputValue::Enum(EnumValue::new(Path::from(Ident::new(
              span, name,
            )))))
          }
        }
      }
      () if "map".equivalent(&name) => {
        let saved = input.save();
        match next_token(input) {
          Ok(Spanned {
            data: SyntacticToken::LBrace,
            ..
          }) => {
            let mut entries = Vec::new();
            loop {
              let saved2 = input.save();
              match next_token(input) {
                Ok(Spanned {
                  data: SyntacticToken::RBrace,
                  ..
                }) => {
                  let full_span = Span::new(span.start(), input.span_since(&saved2.cursor()).end());
                  return Ok(InputValue::Map(Map::new(full_span, entries)));
                }
                Ok(_) => {
                  input.restore(saved2);
                  let key = parse_input_value(input)?;
                  // expect fat arrow
                  let _fat_arrow = next_token(input)?;
                  let value = parse_input_value(input)?;
                  let entry_span = Span::new(key.as_span().start(), value.as_span().end());
                  entries.push(scaffold::MapEntry::new(entry_span, key, value));
                }
                Err(_) => {
                  return Err(SyntacticTokenError::unclosed_brace(span).into());
                }
              }
            }
          }
          _ => {
            input.restore(saved);
            Ok(InputValue::Enum(EnumValue::new(Path::from(Ident::new(
              span, name,
            )))))
          }
        }
      }
      _ => Ok(InputValue::Enum(EnumValue::new(Path::from(Ident::new(
        span, name,
      ))))),
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
        let saved = input.save();
        match next_token(input) {
          Ok(Spanned {
            data: SyntacticToken::RBrace,
            ..
          }) => {
            let full_span = Span::new(span.start(), input.span_since(&saved.cursor()).end());
            return Ok(InputValue::Object(scaffold::Object::new(
              Span::new(span.start(), full_span.end()),
              fields,
            )));
          }
          Ok(_) => {
            input.restore(saved);
            let field_name = parse_name(input)?;
            let _colon = next_token(input)?;
            let value = parse_input_value(input)?;
            let field_span = Span::new(field_name.span().start(), value.as_span().end());
            fields.push(scaffold::ObjectField::new(field_span, field_name, value));
          }
          Err(_) => {
            return Err(SyntacticTokenErrors::unclosed_brace(Span::new(
              span.start(),
              input.span_since(&input.cursor()).end(),
            )));
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
          Ok(Spanned {
            data: SyntacticToken::RBracket,
            ..
          }) => {
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
            return Err(SyntacticTokenErrors::unclosed_bracket(Span::new(
              span.start(),
              input.span_since(&input.cursor()).end(),
            )));
          }
        }
      }
    }
    tok => Err(SyntacticTokenError::unexpected_token(tok, Expectation::InputValue, span).into()),
  }
}

/// GraphQLx Const Input Value
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ConstInputValue<S> {
  /// GraphQLx Boolean value
  Boolean(BooleanValue),
  /// GraphQLx String value
  String(StringValue<S>),
  /// GraphQLx Float value
  Float(FloatValue<S>),
  /// GraphQLx Int value
  Int(IntValue<S>),
  /// GraphQLx Enum value
  Enum(EnumValue<S>),
  /// GraphQLx Null value
  Null(NullValue<S>),
  /// GraphQLx List value
  List(scaffold::List<ConstInputValue<S>>),
  /// GraphQLx Set value
  Set(scaffold::Set<ConstInputValue<S>>),
  /// GraphQLx Map value
  Map(scaffold::Map<ConstInputValue<S>, ConstInputValue<S>>),
  /// GraphQLx Object value
  Object(scaffold::Object<Ident<S>, ConstInputValue<S>>),
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
      Self::Set(v) => v.as_span(),
      Self::Map(v) => v.as_span(),
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
      Self::Set(v) => v.into_span(),
      Self::Map(v) => v.into_span(),
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
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  use super::ident::parse_name;

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
    SyntacticToken::PathSeparator => {
      let mut segments = Vec::new();
      segments.push(parse_name(input)?);
      loop {
        let saved = input.save();
        match next_token(input) {
          Ok(Spanned {
            data: SyntacticToken::PathSeparator,
            ..
          }) => {
            segments.push(parse_name(input)?);
          }
          _ => {
            input.restore(saved);
            break;
          }
        }
      }
      let full_span = Span::new(span.start(), input.span_since(&input.cursor()).end());
      Ok(ConstInputValue::Enum(EnumValue::new(Path::new(
        full_span, segments, true,
      ))))
    }
    SyntacticToken::Identifier(name) => match () {
      () if "true".equivalent(&name) => Ok(ConstInputValue::Boolean(BooleanValue::new(span, true))),
      () if "false".equivalent(&name) => {
        Ok(ConstInputValue::Boolean(BooleanValue::new(span, false)))
      }
      () if "null".equivalent(&name) => Ok(ConstInputValue::Null(NullValue::new(span, name))),
      () if "set".equivalent(&name) => {
        let saved = input.save();
        match next_token(input) {
          Ok(Spanned {
            data: SyntacticToken::LBrace,
            ..
          }) => {
            let mut values = Vec::new();
            loop {
              let saved2 = input.save();
              match next_token(input) {
                Ok(Spanned {
                  data: SyntacticToken::RBrace,
                  ..
                }) => {
                  let full_span = Span::new(span.start(), input.span_since(&saved2.cursor()).end());
                  return Ok(ConstInputValue::Set(ConstSet::new(full_span, values)));
                }
                Ok(_) => {
                  input.restore(saved2);
                  values.push(parse_const_input_value(input)?);
                }
                Err(_) => {
                  return Err(SyntacticTokenError::unclosed_brace(span).into());
                }
              }
            }
          }
          _ => {
            input.restore(saved);
            Ok(ConstInputValue::Enum(EnumValue::new(Path::from(
              Ident::new(span, name),
            ))))
          }
        }
      }
      () if "map".equivalent(&name) => {
        let saved = input.save();
        match next_token(input) {
          Ok(Spanned {
            data: SyntacticToken::LBrace,
            ..
          }) => {
            let mut entries = Vec::new();
            loop {
              let saved2 = input.save();
              match next_token(input) {
                Ok(Spanned {
                  data: SyntacticToken::RBrace,
                  ..
                }) => {
                  let full_span = Span::new(span.start(), input.span_since(&saved2.cursor()).end());
                  return Ok(ConstInputValue::Map(ConstMap::new(full_span, entries)));
                }
                Ok(_) => {
                  input.restore(saved2);
                  let key = parse_const_input_value(input)?;
                  let _fat_arrow = next_token(input)?;
                  let value = parse_const_input_value(input)?;
                  let entry_span = Span::new(key.as_span().start(), value.as_span().end());
                  entries.push(scaffold::MapEntry::new(entry_span, key, value));
                }
                Err(_) => {
                  return Err(SyntacticTokenError::unclosed_brace(span).into());
                }
              }
            }
          }
          _ => {
            input.restore(saved);
            Ok(ConstInputValue::Enum(EnumValue::new(Path::from(
              Ident::new(span, name),
            ))))
          }
        }
      }
      _ => Ok(ConstInputValue::Enum(EnumValue::new(Path::from(
        Ident::new(span, name),
      )))),
    },
    SyntacticToken::LBrace => {
      let mut fields = Vec::new();
      loop {
        let saved = input.save();
        match next_token(input) {
          Ok(Spanned {
            data: SyntacticToken::RBrace,
            ..
          }) => {
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
            return Err(SyntacticTokenErrors::unclosed_brace(Span::new(
              span.start(),
              input.span_since(&input.cursor()).end(),
            )));
          }
        }
      }
    }
    SyntacticToken::LBracket => {
      let mut elements = Vec::new();
      loop {
        let saved = input.save();
        match next_token(input) {
          Ok(Spanned {
            data: SyntacticToken::RBracket,
            ..
          }) => {
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
            return Err(SyntacticTokenErrors::unclosed_bracket(Span::new(
              span.start(),
              input.span_since(&input.cursor()).end(),
            )));
          }
        }
      }
    }
    tok => {
      Err(SyntacticTokenError::unexpected_token(tok, Expectation::ConstInputValue, span).into())
    }
  }
}
