use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{
  punct::{LBrace, RBrace},
  Name, SmearChar, Spanned,
};

use super::{ConstInputValue, InputValue};

/// Represents an input object value parsed from input
///
/// Spec: [Input Object Value](https://spec.graphql.org/draft/#sec-Input-Object-Value)
#[derive(Debug, Clone)]
pub struct ObjectValue<Src, Span> {
  /// The original span of the object value
  span: Spanned<Src, Span>,
  /// The left `{` token.
  l_brace: LBrace<Spanned<Src, Span>>,
  /// The right `}` token.
  r_brace: RBrace<Spanned<Src, Span>>,
  /// The content between the brackets.
  ///
  /// Instead of parsing the list entirely, we keep the raw string representation.
  /// This allows for more efficient parsing and easier error reporting.
  /// The upper layers can perform the necessary parsing and validation.
  fields: Vec<(Name<Src, Span>, InputValue<Src, Span>)>,
}

impl<Src, Span> ObjectValue<Src, Span> {
  /// Returns the span of the object value.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the left brace of the object value.
  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Spanned<Src, Span>> {
    &self.l_brace
  }

  /// Returns the right brace of the object value.
  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Spanned<Src, Span>> {
    &self.r_brace
  }

  /// Returns the fields of the object value.
  #[inline]
  pub const fn fields(&self) -> &[(Name<Src, Span>, InputValue<Src, Span>)] {
    self.fields.as_slice()
  }
}

/// Represents an input object value parsed from input
///
/// Spec: [Input Object Value](https://spec.graphql.org/draft/#sec-Input-Object-Value)
#[derive(Debug, Clone)]
pub struct ConstObjectValue<Src, Span> {
  /// The original span of the object value
  span: Spanned<Src, Span>,
  /// The left `{` token.
  l_brace: LBrace<Spanned<Src, Span>>,
  /// The right `}` token.
  r_brace: RBrace<Spanned<Src, Span>>,
  /// The content between the brackets.
  ///
  /// Instead of parsing the list entirely, we keep the raw string representation.
  /// This allows for more efficient parsing and easier error reporting.
  /// The upper layers can perform the necessary parsing and validation.
  fields: Vec<(Name<Src, Span>, ConstInputValue<Src, Span>)>,
}

impl<Src, Span> ConstObjectValue<Src, Span> {
  /// Returns the span of the object value.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the left brace of the object value.
  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Spanned<Src, Span>> {
    &self.l_brace
  }

  /// Returns the right brace of the object value.
  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Spanned<Src, Span>> {
    &self.r_brace
  }

  /// Returns the fields of the object value.
  #[inline]
  pub const fn fields(&self) -> &[(Name<Src, Span>, ConstInputValue<Src, Span>)] {
    self.fields.as_slice()
  }
}
