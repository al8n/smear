use chumsky::{extra::ParserExtra, label::LabelError, prelude::*};

use crate::{char::Char, name::Name, source::Source, spanned::Spanned, convert::*};

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
/// ## Format
///
/// ```text
/// EnumValue ::= Name but not (`true` | `false` | `null`)
/// ```
///
/// ## Examples
///
/// **Valid enum values:**
/// ```text
/// RED              // Simple enum value
/// BLUE             // Simple enum value
/// LARGE_SIZE       // Underscore allowed
/// HTTP_200         // Contains digits
/// _internal        // Starting underscore allowed
/// True             // Different case than reserved 'true'
/// FALSE            // Different case than reserved 'false'
/// NULL             // Different case than reserved 'null'
/// trueValue        // Contains but not exactly 'true'
/// nullish          // Contains but not exactly 'null'
/// myEnum123        // Mixed letters and digits
/// ```
///
/// **Invalid enum values:**
/// ```text
/// true             // Reserved keyword
/// false            // Reserved keyword
/// null             // Reserved keyword
/// 123ABC           // Cannot start with digit
/// my-enum          // Hyphens not allowed
/// my.enum          // Dots not allowed
/// my enum          // Spaces not allowed
/// @directive       // Special characters not allowed
/// ```
///
/// ## Usage in GraphQL
///
/// Enum values appear in various GraphQL contexts:
/// - **Enum definitions**: `enum Color { RED GREEN BLUE }`
/// - **Query arguments**: `user(status: ACTIVE)`
/// - **Variable values**: `{ "status": "PENDING" }`
/// - **Default values**: `field(sort: DESC = ASC)`
/// - **Input object fields**: `{ filter: { type: PREMIUM } }`
///
/// ## Lexical vs Semantic Validation
///
/// This parser performs **lexical validation only**:
/// - ✅ Checks if the identifier is a valid name
/// - ✅ Checks if it avoids reserved keywords
/// - ❌ Does **not** validate against specific enum type definitions
/// - ❌ Does **not** check if the value exists in any particular enum
///
/// Semantic validation (checking against actual enum types) should be performed
/// at a higher level during schema validation or query execution.
///
/// ## Case Sensitivity
///
/// GraphQL enum values are case-sensitive:
/// - `RED` and `red` are different enum values
/// - Only lowercase `true`, `false`, `null` are reserved
/// - `True`, `FALSE`, `NULL` are valid enum value names
///
/// ## Design Notes
///
/// This parser does not handle whitespace, comments, or other GraphQL syntax
/// elements. The calling parser is responsible for handling any necessary
/// whitespace skipping or comment processing around the enum value.
///
/// Spec: [Enum Value](https://spec.graphql.org/draft/#sec-Enum-Value)
#[derive(Debug, Clone, Copy)]
pub struct EnumValue<Src, Span> {
  /// The name of the enum value
  name: Name<Src, Span>,
}

impl<Src, Span> EnumValue<Src, Span> {
  /// Returns the source span of the enum value.
  /// 
  /// This provides access to the original source location and text of the
  /// enum value, useful for error reporting, source mapping, syntax highlighting,
  /// and extracting the actual string content of the enum value name.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    self.name.span()
  }

  /// Returns the name component of the enum value.
  /// 
  /// This provides access to the underlying [`Name`] that represents the enum
  /// value identifier. The name contains the parsed identifier with its source
  /// location information.
  #[inline]
  pub const fn name(&self) -> &Name<Src, Span> {
    &self.name
  }

  /// Creates a parser for GraphQL enum values.
  ///
  /// This parser implements the complete GraphQL enum value specification by
  /// first parsing a valid name, then filtering out the reserved keywords.
  /// The filtering ensures compliance with GraphQL's lexical rules while
  /// maintaining good error messages.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
  {
    Name::<Src, Span>::parser()
      .filter(|name| {
        let src = name.span().source();
        !(I::is_true_slice(src) || I::is_false_slice(src) || I::is_null_slice(src))
      })
      .map(|name| Self { name })
      .labelled("enum value")
  }
}

impl<Src, Span> AsSpanned<Src, Span> for EnumValue<Src, Span> {
  #[inline]
  fn as_spanned(&self) -> &Spanned<Src, Span> {
    self.span()
  }
}

impl<Src, Span> IntoSpanned<Src, Span> for EnumValue<Src, Span> {
  #[inline]
  fn into_spanned(self) -> Spanned<Src, Span> {
    self.name.into_spanned()
  }
}

impl<Src, Span> IntoComponents for EnumValue<Src, Span> {
  type Components = Name<Src, Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.name
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use chumsky::{error::Simple, extra};

  fn enum_parser<'a>(
  ) -> impl Parser<'a, &'a str, EnumValue<&'a str, SimpleSpan>, extra::Err<Simple<'a, char>>> + Clone
  {
    EnumValue::<&str, SimpleSpan>::parser::<&str, extra::Err<Simple<char>>>().then_ignore(end())
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
      assert_eq!(ev.name().span().source(), &s, "name source mismatch for `{s}`");
      assert_eq!(ev.span().source(), &s, "outer span should match full input `{s}`");
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
    for s in ["1", "1a", "-", "my-enum", "my.enum", "my enum", "@directive", ""] {
      assert!(
        enum_parser().parse(s).into_result().is_err(),
        "should reject non-name `{s}`"
      );
    }
  }

  #[test]
  fn requires_full_input() {
    // Because we add `end()`, trailing input must fail.
    for s in ["FOO,", "Bar ", "trueValue!", "RED\n"] {
      assert!(
        enum_parser().parse(s).into_result().is_err(),
        "should reject with trailing input `{s}`"
      );
    }
  }

  #[test]
  fn into_span_returns_owned_span_with_same_source() {
    // Parse once to inspect, once to move.
    let s = "MY_ENUM";
    let ev1 = enum_parser().parse(s).into_result().unwrap();
    assert_eq!(ev1.name().span().source(), &s);

    // Parse again to test into_span (consumes the value).
    let ev2 = enum_parser().parse(s).into_result().unwrap();
    let owned_span = ev2.into_spanned();
    assert_eq!(owned_span.source(), &s);
  }

  #[test]
  fn labelled_error_exists_for_reserved() {
    // Ensure we actually get an error object (label text is impl-dependent).
    let errs = enum_parser().parse("true").into_result().unwrap_err();
    assert!(!errs.is_empty());
    // Ensure we actually get an error object (label text is impl-dependent).
    let errs = enum_parser().parse("false").into_result().unwrap_err();
    assert!(!errs.is_empty());
    // Ensure we actually get an error object (label text is impl-dependent).
    let errs = enum_parser().parse("null").into_result().unwrap_err();
    assert!(!errs.is_empty());
  }
}
