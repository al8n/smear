use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{
  language::punct::{LBracket, RBracket},
  Char, Spanned,
};

/// Represents an list value parsed from input
///
/// Spec: [List Value](https://spec.graphql.org/draft/#sec-List-Value)
#[derive(Debug, Clone)]
pub struct ListValue<Value, Src, Span, Container = std::vec::Vec<Value>> {
  /// The original span of the list value
  span: Spanned<Src, Span>,
  /// The left `[` token.
  l_bracket: LBracket<Src, Span>,
  /// The right `]` token.
  r_bracket: RBracket<Src, Span>,
  /// The content between the brackets.
  ///
  /// Instead of parsing the list entirely, we keep the raw string representation.
  /// This allows for more efficient parsing and easier error reporting.
  /// The upper layers can perform the necessary parsing and validation.
  values: Container,
  _value: core::marker::PhantomData<Value>,
}

impl<Value, Src, Span, Container> ListValue<Value, Src, Span, Container> {
  /// Returns the span of the list value.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the left bracket of the list value.
  #[inline]
  pub const fn l_bracket(&self) -> &LBracket<Src, Span> {
    &self.l_bracket
  }

  /// Returns the right bracket of the list value.
  #[inline]
  pub const fn r_bracket(&self) -> &RBracket<Src, Span> {
    &self.r_bracket
  }

  /// Returns the values of the list.
  #[inline]
  pub const fn values(&self) -> &Container {
    &self.values
  }

  pub fn parser_with<'src, I, E, P>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    Src: 'src,
    Span: 'src,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    P: Parser<'src, I, Value, E> + Clone + 'src,
    Container: chumsky::container::Container<Value>,
  {
    let ws = super::ignored::ignored();

    // Keep bracket token spans precise; don't pad the tokens themselves.
    let open = LBracket::parser();
    let close = RBracket::parser();

    // Each element owns only *trailing* ignored (incl. commas).
    let elem = value.clone().then_ignore(ws.clone());

    // '[' ws? ( ']' | elem+ ']' )
    open
    .then_ignore(ws.clone())                              // allow ignored right after '['
    .then(
      choice((
        // Empty fast path: immediately see ']'
        close.clone().map(|r| (Container::default(), r)),

        // Non-empty: one-or-more elements; trailing commas handled by elem’s trailing ws
        elem
          .repeated()
          .at_least(1)
          .collect::<Container>()
          .then(close.clone()),
      ))
    )
    .map_with(|(l_bracket, (values, r_bracket)), sp| Self {
      span: Spanned::from(sp),
      l_bracket,
      r_bracket,
      values,
      _value: core::marker::PhantomData,
    })
  }
}
