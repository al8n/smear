use chumsky::{extra::ParserExtra, prelude::*};

use crate::{char::Char, convert::*, source::Source, spanned::Spanned};

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
  /// Returns the parsed boolean value.
  ///
  /// This is the actual boolean value (`true` or `false`) that was parsed
  /// from the source text, extracted from the literal tokens.
  #[inline]
  pub const fn value(&self) -> bool {
    self.value
  }

  /// Returns the source span of the boolean literal.
  ///
  /// This span covers the entire boolean token (`true` or `false`) in the
  /// original source, useful for error reporting, source mapping, and
  /// syntax highlighting.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Creates a parser for GraphQL boolean literals.
  ///
  /// This parser implements the GraphQL boolean literal specification by
  /// matching the exact character sequences for `true` and `false`. The
  /// parser is strictly case-sensitive and will reject any variations
  /// in capitalization.
  ///
  /// The parser will not handle any surrounding whitespace or comments.
  ///
  /// Spec: [Boolean Value](https://spec.graphql.org/draft/#sec-Boolean-Value)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
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
  }
}

impl<Src, Span> AsSpanned<Src, Span> for BooleanValue<Src, Span> {
  #[inline]
  fn as_spanned(&self) -> &Spanned<Src, Span> {
    self.span()
  }
}

impl<Src, Span> IntoSpanned<Src, Span> for BooleanValue<Src, Span> {
  #[inline]
  fn into_spanned(self) -> Spanned<Src, Span> {
    self.span
  }
}

impl<Src, Span> IntoComponents for BooleanValue<Src, Span> {
  type Components = (Spanned<Src, Span>, bool);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.value)
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
