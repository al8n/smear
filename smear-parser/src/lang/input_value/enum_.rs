use chumsky::{extra::ParserExtra, prelude::*};

use crate::{lang::Name, source::*};

/// A GraphQL enum value identifier.
///
/// Represents a valid enum value as defined by the GraphQL specification. Enum
/// values are special identifiers that follow GraphQL name rules but exclude
/// certain reserved keywords that have special meaning in the GraphQL language.
///
/// ## Specification Rules
///
/// An enum value must be:
/// - A valid GraphQL [`Name`] (letters, digits, underscores; starts with letter/underscore)
/// - **Not** one of the reserved keywords: `true`, `false`, or `null` (exact matches only)
/// - Case-sensitive (so `True`, `FALSE`, `NULL` are valid enum values)
///
/// ## Reserved Keywords
///
/// The following exact strings are **forbidden** as enum values:
/// - `true` - Reserved for boolean literals
/// - `false` - Reserved for boolean literals  
/// - `null` - Reserved for null literals
///
/// ## Grammar
///
/// ```text
/// EnumValue ::= Name but not (`true` | `false` | `null`)
/// ```
///
/// Spec: [Enum Value](https://spec.graphql.org/draft/#sec-Enum-Value)
#[derive(Debug, Clone, Copy)]
pub struct EnumValue<Span>(Span);

impl<Span> EnumValue<Span> {
  /// Returns the source span of the enum value.
  ///
  /// This provides access to the original source location and text of the
  /// enum value, useful for error reporting, source mapping, syntax highlighting,
  /// and extracting the actual string content of the enum value name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.0
  }

  /// Creates a parser for GraphQL enum values.
  ///
  /// This parser implements the complete GraphQL enum value specification by
  /// first parsing a valid name, then filtering out the reserved keywords.
  /// The filtering ensures compliance with GraphQL's lexical rules while
  /// maintaining good error messages.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the enum value.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
  {
    Name::<Span>::parser()
      .to_slice()
      .filter(|slice| {
        !(slice.equivalent(null_tokens::<I::Token>().into_iter())
          || slice.equivalent(true_tokens::<I::Token>().into_iter())
          || slice.equivalent(false_tokens::<I::Token>().into_iter()))
      })
      .map_with(|_, sp| Self(Span::from_map_extra(sp)))
  }
}

impl<Span> From<EnumValue<Span>> for Name<Span> {
  #[inline]
  fn from(enum_value: EnumValue<Span>) -> Self {
    Self(enum_value.into_span())
  }
}

impl<Span> AsRef<Span> for EnumValue<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for EnumValue<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0
  }
}

impl<Span> IntoComponents for EnumValue<Span> {
  type Components = Name<Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.into()
  }
}

#[inline]
const fn true_tokens<T>() -> [T; 4]
where
  T: Char,
{
  [T::t, T::r, T::u, T::e]
}

#[inline]
const fn false_tokens<T>() -> [T; 5]
where
  T: Char,
{
  [T::f, T::a, T::l, T::s, T::e]
}

#[inline]
const fn null_tokens<T>() -> [T; 4]
where
  T: Char,
{
  [T::n, T::u, T::l, T::l]
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::source::WithSource;
  use chumsky::{error::Simple, extra};

  fn enum_parser<'a>()
  -> impl Parser<'a, &'a str, EnumValue<WithSource<&'a str, SimpleSpan>>, extra::Err<Simple<'a, char>>>
  + Clone {
    EnumValue::<WithSource<&str, SimpleSpan>>::parser::<&str, extra::Err<Simple<char>>>()
  }

  #[test]
  fn accepts_regular_names() {
    // Valid GraphQL Names that are not the exact reserved words.
    let ok = [
      "RED",
      "Blue",
      "_internal",
      "__meta",
      "A1",
      "_1",
      "trueValue",
      "nullish",
      "FALSE",
      "True",
      "Null",
      "HTTP_200",
      "myEnum123",
    ];
    for s in ok {
      let ev = enum_parser().parse(s).into_result().unwrap();
      assert_eq!(ev.span().source(), &s, "name source mismatch for `{s}`");
      assert_eq!(
        ev.span().source(),
        &s,
        "outer span should match full input `{s}`"
      );
    }
  }

  #[test]
  fn rejects_reserved_keywords_exactly() {
    for s in ["true", "false", "null"] {
      assert!(
        enum_parser().parse(s).into_result().is_err(),
        "should reject reserved keyword `{s}`"
      );
    }
  }

  #[test]
  fn case_variants_of_reserved_are_allowed() {
    for s in ["TRUE", "False", "Null"] {
      assert!(
        enum_parser().parse(s).into_result().is_ok(),
        "should accept case-variant `{s}`"
      );
    }
  }

  #[test]
  fn rejects_non_name_starters_and_illegal_chars() {
    for s in [
      "1",
      "1a",
      "-",
      "my-enum",
      "my.enum",
      "my enum",
      "@directive",
      "",
    ] {
      assert!(
        enum_parser().parse(s).into_result().is_err(),
        "should reject non-name `{s}`"
      );
    }
  }

  #[test]
  fn requires_full_input() {
    for s in ["FOO,", "Bar ", "trueValue!", "RED\n"] {
      assert!(
        enum_parser().parse(s).into_result().is_err(),
        "should reject with trailing input `{s}`"
      );
    }
  }

  #[test]
  fn into_span_returns_owned_span_with_same_source() {
    let s = "MY_ENUM";
    let ev1 = enum_parser().parse(s).into_result().unwrap();
    assert_eq!(ev1.span().source(), &s);

    let ev2 = enum_parser().parse(s).into_result().unwrap();
    let owned_span = ev2.into_span();
    assert_eq!(owned_span.source(), &s);
  }

  #[test]
  fn labelled_error_exists_for_reserved() {
    let errs = enum_parser().parse("true").into_result().unwrap_err();
    assert!(!errs.is_empty());
    let errs = enum_parser().parse("false").into_result().unwrap_err();
    assert!(!errs.is_empty());
    let errs = enum_parser().parse("null").into_result().unwrap_err();
    assert!(!errs.is_empty());
  }
}
