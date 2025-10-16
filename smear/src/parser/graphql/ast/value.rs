use crate::{
  error::{UnclosedBraceError as _, UnclosedBracketError},
  hints::VariableValueHint,
  lexer::graphql::ast::AstLexerErrors,
  punctuator::{RBrace, RBracket},
  scaffold,
};

use super::{
  DefaultVec, Expectation, Name, SyntacticToken, SyntacticTokenError, SyntacticTokenErrors,
  SyntacticTokenStream,
};
use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Lexed, Parseable, Source, Token, Tokenizer,
  chumsky::{Parser, extra::ParserExtra, prelude::*},
  logos::Logos,
  utils::{AsSpan, IntoSpan, Span, Spanned, cmp::Equivalent},
};

pub use boolean_value::*;
pub use enum_value::*;
pub use float::*;
pub use int::*;
pub use null_value::*;
pub use string::*;

pub type VariableValue<S> = crate::parser::value::VariableValue<super::Name<S>>;

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
///
/// Represents a value that can be provided as an argument or input field value in
/// GraphQL operations. Input values in executable contexts **can contain variables**
/// (prefixed with `$`).
///
/// This enum covers all valid GraphQL value literals plus variable references:
/// - Scalar values: boolean, string, float, int, enum, null
/// - Complex values: list and object
/// - Variables: `$variableName`
///
/// # Variants
///
/// - `Variable`: A variable reference (e.g., `$userId`)
/// - `Boolean`: `true` or `false`
/// - `String`: String literals (inline or block)
/// - `Float`: Floating-point numbers
/// - `Int`: Integer numbers
/// - `Enum`: Enum value names
/// - `Null`: The `null` literal
/// - `List`: Array of values `[value1, value2, ...]`
/// - `Object`: Object with field-value pairs `{ field1: value1, field2: value2 }`
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

impl<'a, S>
  Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for InputValue<S>
where
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  RBrace:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>,
  RBracket:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>,
  str: Equivalent<S>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, SyntacticTokenStream<'a, S>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, SyntacticTokenStream<'a, S>, Error = SyntacticTokenErrors<'a, S>> + 'a,
    SyntacticTokenStream<'a, S>: Tokenizer<
        'a,
        SyntacticToken<S>,
        Slice = <<<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<
          'a,
        >,
      >,
    SyntacticTokenErrors<'a, S>: 'a,
  {
    recursive(|parser| {
      custom::<_, SyntacticTokenStream<'_, S>, Self, E>(move |inp| {
        let before = inp.cursor();

        match inp.next() {
          None => Err(SyntacticTokenError::unexpected_end_of_input(inp.span_since(&before)).into()),
          Some(tok) => match tok {
            Lexed::Error(err) => {
              Err(SyntacticTokenError::from_lexer_errors(err, inp.span_since(&before)).into())
            }
            Lexed::Token(Spanned { span, data: token }) => {
              let output = match token {
                SyntacticToken::LitFloat(raw) => Self::Float(FloatValue::new(span, raw)),
                SyntacticToken::LitInt(raw) => Self::Int(IntValue::new(span, raw)),
                SyntacticToken::LitInlineStr(raw) => {
                  Self::String(StringValue::new(span, raw.into()))
                }
                SyntacticToken::LitBlockStr(raw) => {
                  Self::String(StringValue::new(span, raw.into()))
                }
                SyntacticToken::Identifier(name) => match () {
                  () if "true".equivalent(&name) => Self::Boolean(BooleanValue::new(span, true)),
                  () if "false".equivalent(&name) => Self::Boolean(BooleanValue::new(span, false)),
                  () if "null".equivalent(&name) => Self::Null(NullValue::new(span, name)),
                  _ => Self::Enum(EnumValue::new(span, name)),
                },
                SyntacticToken::Dollar => {
                  let current_cursor = inp.cursor();
                  let name = match inp.next() {
                    Some(Lexed::Token(Spanned {
                      span,
                      data: SyntacticToken::Identifier(name),
                    })) => Name::new(span, name),
                    Some(Lexed::Token(Spanned { span, data })) => {
                      return Err(
                        SyntacticTokenError::unexpected_token(data, Expectation::Name, span).into(),
                      );
                    }
                    Some(Lexed::Error(err)) => {
                      return Err(
                        SyntacticTokenError::from_lexer_errors(
                          err,
                          inp.span_since(&current_cursor),
                        )
                        .into(),
                      );
                    }
                    None => {
                      return Err(
                        SyntacticTokenError::unexpected_end_of_variable_value(
                          VariableValueHint::Name,
                          inp.span_since(&before),
                        )
                        .into(),
                      );
                    }
                  };
                  Self::Variable(VariableValue::new(inp.span_since(&before), name))
                }
                SyntacticToken::LBrace => {
                  let (fields, rbrace) = inp.parse(
                    ObjectField::<S>::parser_with(Name::<S>::parser(), parser.clone())
                      .repeated()
                      .collect()
                      .then(RBrace::parser().or_not()),
                  )?;
                  return match rbrace {
                    Some(_) => Ok(Self::Object(scaffold::Object::new(
                      inp.span_since(&before),
                      fields,
                    ))),
                    None => Err(SyntacticTokenErrors::unclosed_brace(
                      inp.span_since(&before),
                    )),
                  };
                }
                SyntacticToken::LBracket => {
                  let (elements, rbracket) = inp.parse(
                    parser
                      .clone()
                      .repeated()
                      .collect()
                      .then(RBracket::parser().or_not()),
                  )?;

                  return match rbracket {
                    Some(_) => Ok(Self::List(scaffold::List::new(
                      inp.span_since(&before),
                      elements,
                    ))),
                    None => Err(SyntacticTokenErrors::unclosed_bracket(
                      inp.span_since(&before),
                    )),
                  };
                }
                tok => {
                  return Err(
                    SyntacticTokenError::unexpected_token(tok, Expectation::InputValue, span)
                      .into(),
                  );
                }
              };

              Ok(output)
            }
          },
        }
      })
    })
  }
}

/// GraphQL constant input value (schema context).
///
/// Represents a value that can be used in schema definitions (type system). Constant values
/// **cannot contain variables** - they must be literal values known at schema definition time.
///
/// This is used for:
/// - Default values for input fields and arguments
/// - Directive arguments in schema definitions
/// - Any value in type system definitions
///
/// The set of allowed values is the same as `InputValue`, except variables are not permitted.
///
/// # Variants
///
/// - `Boolean`: `true` or `false`
/// - `String`: String literals (inline or block)
/// - `Float`: Floating-point numbers
/// - `Int`: Integer numbers
/// - `Enum`: Enum value names
/// - `Null`: The `null` literal
/// - `List`: Array of constant values
/// - `Object`: Object with field-value pairs (all values must be constant)
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

impl<'a, S>
  Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for ConstInputValue<S>
where
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  RBrace:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>,
  RBracket:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>,
  str: Equivalent<S>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, SyntacticTokenStream<'a, S>, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, SyntacticTokenStream<'a, S>, Error = SyntacticTokenErrors<'a, S>> + 'a,
    SyntacticTokenStream<'a, S>: Tokenizer<
        'a,
        SyntacticToken<S>,
        Slice = <<<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<
          'a,
        >,
      >,
    SyntacticTokenErrors<'a, S>: 'a,
  {
    recursive(|parser| {
      custom::<_, SyntacticTokenStream<'_, S>, Self, E>(move |inp| {
        let before = inp.cursor();

        match inp.next() {
          None => Err(SyntacticTokenError::unexpected_end_of_input(inp.span_since(&before)).into()),
          Some(tok) => match tok {
            Lexed::Error(err) => {
              Err(SyntacticTokenError::from_lexer_errors(err, inp.span_since(&before)).into())
            }
            Lexed::Token(Spanned { span, data: token }) => {
              let output = match token {
                SyntacticToken::LitFloat(raw) => Self::Float(FloatValue::new(span, raw)),
                SyntacticToken::LitInt(raw) => Self::Int(IntValue::new(span, raw)),
                SyntacticToken::LitInlineStr(raw) => {
                  Self::String(StringValue::new(span, raw.into()))
                }
                SyntacticToken::LitBlockStr(raw) => {
                  Self::String(StringValue::new(span, raw.into()))
                }
                SyntacticToken::Identifier(name) => match () {
                  () if "true".equivalent(&name) => Self::Boolean(BooleanValue::new(span, true)),
                  () if "false".equivalent(&name) => Self::Boolean(BooleanValue::new(span, false)),
                  () if "null".equivalent(&name) => Self::Null(NullValue::new(span, name)),
                  _ => Self::Enum(EnumValue::new(span, name)),
                },
                SyntacticToken::LBrace => {
                  let (fields, rbrace) = inp.parse(
                    ConstObjectField::<S>::parser_with(Name::<S>::parser(), parser.clone())
                      .repeated()
                      .collect()
                      .then(RBrace::parser().or_not()),
                  )?;
                  return match rbrace {
                    Some(_) => Ok(Self::Object(scaffold::Object::new(
                      inp.span_since(&before),
                      fields,
                    ))),
                    None => Err(SyntacticTokenErrors::unclosed_brace(
                      inp.span_since(&before),
                    )),
                  };
                }
                SyntacticToken::LBracket => {
                  let (elements, rbracket) = inp.parse(
                    parser
                      .clone()
                      .repeated()
                      .collect()
                      .then(RBracket::parser().or_not()),
                  )?;

                  return match rbracket {
                    Some(_) => Ok(Self::List(scaffold::List::new(
                      inp.span_since(&before),
                      elements,
                    ))),
                    None => Err(SyntacticTokenErrors::unclosed_bracket(
                      inp.span_since(&before),
                    )),
                  };
                }
                tok => {
                  return Err(
                    SyntacticTokenError::unexpected_token(tok, Expectation::ConstInputValue, span)
                      .into(),
                  );
                }
              };

              Ok(output)
            }
          },
        }
      })
    })
  }
}
