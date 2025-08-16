use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{
  punct::{LBracket, RBracket},
  SmearChar, Spanned,
};

use super::{ConstInputValue, InputValue};

/// Represents an list value parsed from input
///
/// Spec: [List Value](https://spec.graphql.org/draft/#sec-List-Value)
#[derive(Debug, Clone)]
pub struct ListValue<Src, Span> {
  /// The original span of the list value
  span: Spanned<Src, Span>,
  /// The left `[` token.
  l_bracket: LBracket<Spanned<Src, Span>>,
  /// The right `]` token.
  r_bracket: RBracket<Spanned<Src, Span>>,
  /// The content between the brackets.
  ///
  /// Instead of parsing the list entirely, we keep the raw string representation.
  /// This allows for more efficient parsing and easier error reporting.
  /// The upper layers can perform the necessary parsing and validation.
  values: Vec<InputValue<Src, Span>>,
}

impl<Src, Span> ListValue<Src, Span> {
  /// Returns the span of the list value.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the left bracket of the list value.
  #[inline]
  pub const fn l_bracket(&self) -> &LBracket<Spanned<Src, Span>> {
    &self.l_bracket
  }

  /// Returns the right bracket of the list value.
  #[inline]
  pub const fn r_bracket(&self) -> &RBracket<Spanned<Src, Span>> {
    &self.r_bracket
  }

  /// Returns the values of the list.
  #[inline]
  pub const fn values(&self) -> &[InputValue<Src, Span>] {
    self.values.as_slice()
  }
}

/// Represents an list value parsed from input
///
/// Spec: [List Value](https://spec.graphql.org/draft/#sec-List-Value)
#[derive(Debug, Clone)]
pub struct ConstListValue<Src, Span> {
  /// The original span of the list value
  span: Spanned<Src, Span>,
  /// The left `[` token.
  l_bracket: LBracket<Spanned<Src, Span>>,
  /// The right `]` token.
  r_bracket: RBracket<Spanned<Src, Span>>,
  /// The content between the brackets.
  ///
  /// Instead of parsing the list entirely, we keep the raw string representation.
  /// This allows for more efficient parsing and easier error reporting.
  /// The upper layers can perform the necessary parsing and validation.
  values: Vec<ConstInputValue<Src, Span>>,
}

impl<Src, Span> ConstListValue<Src, Span> {
  /// Returns the span of the list value.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the left bracket of the list value.
  #[inline]
  pub const fn l_bracket(&self) -> &LBracket<Spanned<Src, Span>> {
    &self.l_bracket
  }

  /// Returns the right bracket of the list value.
  #[inline]
  pub const fn r_bracket(&self) -> &RBracket<Spanned<Src, Span>> {
    &self.r_bracket
  }

  /// Returns the values of the list.
  #[inline]
  pub const fn values(&self) -> &[ConstInputValue<Src, Span>] {
    self.values.as_slice()
  }
}
