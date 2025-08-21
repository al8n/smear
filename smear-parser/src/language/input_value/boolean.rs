use chumsky::{extra::ParserExtra, label::LabelError, prelude::*};

use crate::{char::Char, source::Source, spanned::Spanned};

/// A parsed GraphQL **Boolean Value** (`true` or `false`).
///
/// Per the GraphQL spec, boolean literals are lowercase and case-sensitive.
/// This parser **only** accepts the exact tokens `true` and `false`.
///
/// ## Note
///
/// This parser will not handle any surrounding whitespace or comments,
/// and only focus on the boolean literals themselves.
/// Invoker should handle any necessary whitespace or comment skipping.
#[derive(Debug, Clone, Copy)]
pub struct BooleanValue<Src, Span> {
  /// The original span of the boolean value
  span: Spanned<Src, Span>,
  /// The value of the boolean
  value: bool,
}

impl<Src, Span> BooleanValue<Src, Span> {
  /// Returns the parsed boolean.
  #[inline]
  pub const fn value(&self) -> bool {
    self.value
  }

  /// Returns the source span for this literal.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Parser for a GraphQL **Boolean Value**.
  ///
  /// Accepts only the exact tokens `true` and `false` (lowercase).
  ///
  /// Spec: <https://spec.graphql.org/draft/#sec-Boolean-Value>
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
  {
    just([I::Token::t, I::Token::r, I::Token::u, I::Token::e])
      .to(true)
      .or(
        just([
          I::Token::f,
          I::Token::a,
          I::Token::l,
          I::Token::s,
          I::Token::e,
        ])
        .to(false),
      )
      .map_with(|data, span| Self {
        span: Spanned::from(span),
        value: data,
      })
      .labelled("boolean value")
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use chumsky::{error::Simple, extra};

  fn boolean_parser<'a>(
  ) -> impl Parser<'a, &'a str, BooleanValue<&'a str, SimpleSpan>, extra::Err<Simple<'a, char>>> + Clone
  {
    BooleanValue::<&str, SimpleSpan>::parser::<&str, extra::Err<Simple<char>>>().then_ignore(end())
  }

  #[test]
  fn parses_true() {
    let parsed = boolean_parser()
      .parse("true")
      .into_result()
      .expect("should parse `true`");
    assert!(parsed.value());
  }

  #[test]
  fn parses_false() {
    let parsed = boolean_parser()
      .parse("false")
      .into_result()
      .expect("should parse `false`");
    assert!(!parsed.value());
  }

  #[test]
  fn rejects_mixed_or_uppercase() {
    for bad in ["True", "FALSE", "False", "tRuE"] {
      assert!(
        boolean_parser().parse(bad).into_result().is_err(),
        "should reject `{bad}`"
      );
    }
  }

  #[test]
  fn rejects_prefix_suffix_junk() {
    // Because we add `end()`, leftover input causes failure.
    for bad in ["tru", "fals", "truex", "xfalse", "truefalse"] {
      assert!(
        boolean_parser().parse(bad).into_result().is_err(),
        "should reject `{bad}`"
      );
    }
  }

  #[test]
  fn span_covers_exact_literal() {
    let parsed = boolean_parser().parse("false").unwrap();
    let span = parsed.span().source(); // At least ensure it’s set and accessible.
    assert_eq!(span, &"false");
    assert!(!parsed.value());
  }
}
