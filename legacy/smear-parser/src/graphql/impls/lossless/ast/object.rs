use smear_lexer::tokora::{
  Emitter, InputRef, Lexer, ParseContext, SimpleSpan as Span,
  lexer::FromLogos,
  span::{AsSpan, IntoSpan, Spanned},
  utils::IntoComponents,
};

use crate::{
  graphql::{Expectation, ast::Name},
  lexer::graphql::lossless::{LosslessLexer, LosslessToken},
};
use smear_lexer::punctuator::Colon;
use smear_scaffold::ast as scaffold;

use super::{
  LosslessTokenError, LosslessTokenErrors,
  name::parse_name,
  next_token,
  padded::{Padded, PaddedLeft, PaddedRight, parse_padded, parse_padded_left, parse_padded_right},
  punctuator::parse_colon,
};

/// Object value in GraphQL CST (preserves trivia).
///
/// Uses a custom container type to store `Padded<ObjectField<V, S>, S>` elements
/// rather than the default `scaffold::ObjectField`.
pub type CstObject<V, S> =
  scaffold::Object<Name<S>, V, std::vec::Vec<Padded<ObjectField<V, S>, S>>>;

/// A single field within a GraphQL input object literal (CST variant with trivia).
///
/// Represents a name-value pair within an object literal, preserving all whitespace,
/// comments, and formatting around the components.
///
/// ## Grammar
///
/// ```text
/// ObjectField ::= Name ':' Value
/// ```
#[derive(Debug, Clone)]
pub struct ObjectField<InputValue, S> {
  span: Span,
  name: PaddedRight<Name<S>, S>,
  colon: Colon,
  value: PaddedLeft<InputValue, S>,
}

impl<InputValue, S> AsSpan<Span> for ObjectField<InputValue, S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<InputValue, S> IntoSpan<Span> for ObjectField<InputValue, S> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<InputValue, S> IntoComponents for ObjectField<InputValue, S> {
  type Components = (
    Span,
    PaddedRight<Name<S>, S>,
    Colon,
    PaddedLeft<InputValue, S>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.colon, self.value)
  }
}

impl<InputValue, S> ObjectField<InputValue, S> {
  #[inline]
  pub(crate) const fn new(
    span: Span,
    name: PaddedRight<Name<S>, S>,
    colon: Colon,
    value: PaddedLeft<InputValue, S>,
  ) -> Self {
    Self {
      span,
      name,
      colon,
      value,
    }
  }

  /// Returns the source span of the entire field.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the colon separator token.
  #[inline]
  pub const fn colon(&self) -> &Colon {
    &self.colon
  }

  /// Returns the field name.
  #[inline]
  pub const fn name(&self) -> &PaddedRight<Name<S>, S> {
    &self.name
  }

  /// Returns the field value.
  #[inline]
  pub const fn value(&self) -> &PaddedLeft<InputValue, S> {
    &self.value
  }
}

/// Parses a single object field from the lossless input.
pub fn parse_object_field<'inp, S, Ctx, Lang, V>(
  input: &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
  parse_value: impl FnOnce(
    &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
  ) -> Result<V, LosslessTokenErrors<S>>,
) -> Result<ObjectField<V, S>, LosslessTokenErrors<S>>
where
  S: Clone,
  LosslessToken<S>: FromLogos<'inp>,
  LosslessLexer<'inp, S>: Lexer<'inp, Token = LosslessToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, LosslessLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, LosslessLexer<'inp, S>, Lang, Error = LosslessTokenErrors<S>>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  let name = parse_padded_right(input, parse_name)?;
  let colon = parse_colon(input)?;
  let value = parse_padded_left(input, parse_value)?;
  let span = input.span_since(&cursor);
  Ok(ObjectField::new(span, name, colon, value))
}

/// Parses an object value from the lossless input.
///
/// Uses the provided `parse_value` function to parse each field's value,
/// with trivia preserved around each field.
pub fn parse_object<'inp, S, Ctx, Lang, V>(
  input: &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
  parse_value: impl Fn(
    &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
  ) -> Result<V, LosslessTokenErrors<S>>,
) -> Result<CstObject<V, S>, LosslessTokenErrors<S>>
where
  S: Clone,
  LosslessToken<S>: FromLogos<'inp>,
  LosslessLexer<'inp, S>: Lexer<'inp, Token = LosslessToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, LosslessLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, LosslessLexer<'inp, S>, Lang, Error = LosslessTokenErrors<S>>,
  Lang: ?Sized,
{
  // Parse opening brace
  let Spanned {
    span: open_span,
    data: token,
  } = next_token(input)?;
  match token {
    LosslessToken::LBrace => {}
    tok => {
      return Err(LosslessTokenError::unexpected_token(tok, Expectation::LBrace, open_span).into());
    }
  }

  let mut fields = std::vec::Vec::new();

  loop {
    // Check for closing brace
    let saved = input.save();
    match next_token(input) {
      Ok(Spanned {
        data: LosslessToken::RBrace,
        ..
      }) => {
        let full_span = Span::new(open_span.start(), input.span_since(&saved.cursor()).end());
        return Ok(scaffold::Object::new(full_span, fields));
      }
      Ok(_) => {
        input.restore(saved);
        let padded = parse_padded(input, |inp| parse_object_field(inp, &parse_value))?;
        fields.push(padded);
      }
      Err(_) => {
        return Err(LosslessTokenError::unclosed_object(open_span).into());
      }
    }
  }
}
