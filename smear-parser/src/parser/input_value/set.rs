use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{
  punct::{LAngle, RAngle},
  SmearChar, Spanned,
};

/// Represents an set value parsed from input
///
/// Spec: [Set Value](https://spec.graphql.org/draft/#sec-Set-Value)
#[derive(Debug, Clone)]
pub struct Set<Value, Src, Span> {
  /// The original span of the set value
  span: Spanned<Src, Span>,
  /// The left `[` token.
  l_angle: LAngle<Spanned<Src, Span>>,
  /// The right `]` token.
  r_angle: RAngle<Spanned<Src, Span>>,
  /// The content between the brackets.
  ///
  /// Instead of parsing the set entirely, we keep the raw string representation.
  /// This allows for more efficient parsing and easier error reporting.
  /// The upper layers can perform the necessary parsing and validation.
  values: Vec<Value>,
}

impl<Value, Src, Span> Set<Value, Src, Span> {
  /// Returns the span of the set value.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the left bracket of the set value.
  #[inline]
  pub const fn l_angle(&self) -> &LAngle<Spanned<Src, Span>> {
    &self.l_angle
  }

  /// Returns the right bracket of the set value.
  #[inline]
  pub const fn r_angle(&self) -> &RAngle<Spanned<Src, Span>> {
    &self.r_angle
  }

  /// Returns the values of the set.
  #[inline]
  pub const fn values(&self) -> &[Value] {
    self.values.as_slice()
  }

  /// Returns a parser for the set value.
  /// Returns a parser for the set value.
  pub fn parser_with<'src, I, E, P>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    Src: 'src,
    Span: 'src,
    I::Token: SmearChar + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    P: Parser<'src, I, Value, E> + Clone + 'src,
  {
    just(I::Token::BRACKET_OPEN)
      .map_with(|_, sp| LAngle::new(Spanned::from(sp)))
      .then(
        value
          .separated_by(just(I::Token::COMMA).padded_by(super::ignored::padded()))
          .allow_trailing()
          .collect()
          .padded_by(super::ignored::padded()),
      )
      .then(just(I::Token::BRACKET_CLOSE).map_with(|_, sp| RAngle::new(Spanned::from(sp))))
      .map_with(|((l_angle, values), r_angle), sp| Self {
        span: Spanned::from(sp),
        l_angle,
        r_angle,
        values,
      })
  }
}

// #[test]
// fn test_set_value_parser() {
//   let input = "<1, 2.0, true, [\"a\", \"b\", \"c\"]>";
//   let parser = ConstInputValue::parser::<&str, super::super::Error>();
//   let ConstInputValue::Set(result) = parser.parse(input).into_result().unwrap() else {
//     panic!("Expected a set value");
//   };
//   assert!(result.values().len() == 4);

//   let first = &result.values()[0];
//   let second = &result.values()[1];
//   let third = &result.values()[2];
//   let fourth = &result.values()[3];

//   assert_eq!(first.unwrap_int_ref().digits().source(), &"1");
//   assert_eq!(second.unwrap_float_ref().span().source(), &"2.0");
//   assert!(third.is_boolean());
//   assert!(fourth.is_set());
// }

