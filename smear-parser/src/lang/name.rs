use chumsky::{extra::ParserExtra, prelude::*};

use crate::source::*;

/// A GraphQL name identifier.
///
/// Represents a valid GraphQL name as defined by the specification. Names are
/// used throughout GraphQL for field names, type names, argument names, directive
/// names, and other identifiers. They follow strict lexical rules to ensure
/// consistent parsing across different GraphQL implementations.
///
/// ## Specification Rules
///
/// A GraphQL name must:
/// - Start with a letter (`A-Z`, `a-z`) or underscore (`_`)
/// - Contain only letters, digits (`0-9`), and underscores in subsequent positions
/// - Be at least one character long
/// - Be case-sensitive (`myField` and `MyField` are different names)
///
/// ## Grammar
///
/// ```text
/// Name ::= [_A-Za-z][_0-9A-Za-z]*
/// ```
///
/// ## Examples
///
/// **Valid names:**
/// ```text
/// user           // Simple lowercase name
/// User           // Capitalized name
/// _private       // Starting with underscore
/// field123       // Contains digits
/// __typename     // GraphQL introspection field
/// MyCustomType   // PascalCase type name
/// _id            // Underscore prefix
/// a              // Single character
/// ```
///
/// **Invalid names:**
/// ```text
/// 123field       // Cannot start with digit
/// my-field       // Hyphens not allowed
/// my.field       // Dots not allowed
/// my field       // Spaces not allowed
/// my@field       // Special characters not allowed
/// ""             // Empty string not allowed
/// ```
///
/// ## Implementation Notes
///
/// This parser only handles the lexical structure of names and does not validate
/// GraphQL-specific naming conventions (e.g., type names should be PascalCase).
/// Such semantic validation should be performed at a higher level.
///
/// Spec: [Name](https://spec.graphql.org/draft/#sec-Names)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Name<Span>(pub(crate) Span);

impl<Span> Name<Span> {
  /// Returns the source span of the name.
  ///
  /// This provides access to the original source location and text of the name,
  /// useful for error reporting, source mapping, syntax highlighting, and
  /// extracting the actual string content of the name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.0
  }

  /// Creates a parser for GraphQL names.
  ///
  /// This parser implements the complete GraphQL name specification, enforcing
  /// the lexical rules for valid identifiers. It uses a two-part strategy:
  /// first matching the initial character (which has stricter rules), then
  /// matching any number of continuation characters.
  ///
  /// ## Parsing Strategy
  ///
  /// 1. **Start character**: Must be a letter (`A-Z`, `a-z`) or underscore (`_`)
  /// 2. **Continuation characters**: Zero or more letters, digits (`0-9`), or underscores
  ///
  /// The parser builds the complete character sequence and validates it conforms
  /// to GraphQL naming rules during parsing, ensuring only valid names are accepted.
  ///
  /// ## Examples
  ///
  /// ```text
  /// // Successful parses:
  /// "user"         -> Name { span: "user" }
  /// "User"         -> Name { span: "User" }
  /// "_private"     -> Name { span: "_private" }
  /// "field123"     -> Name { span: "field123" }
  /// "__typename"   -> Name { span: "__typename" }
  /// "a"            -> Name { span: "a" }
  ///
  /// // Parse failures:
  /// "123field"     -> Error: cannot start with digit
  /// "my-field"     -> Error: hyphen not allowed
  /// "my.field"     -> Error: dot not allowed
  /// "my field"     -> Error: space not allowed
  /// ""             -> Error: empty string
  /// "@directive"   -> Error: @ symbol not allowed at start
  /// ```
  ///
  /// ## Character Set Details
  ///
  /// The parser explicitly handles the following character classes:
  /// - **Letters**: `A-Z` (uppercase) and `a-z` (lowercase)
  /// - **Digits**: `0-9` (only allowed after the first character)
  /// - **Underscore**: `_` (allowed anywhere)
  ///
  /// ## Error Handling
  ///
  /// The parser will fail if:
  /// - The input is empty
  /// - The first character is not a letter or underscore
  /// - Any subsequent character is not a letter, digit, or underscore
  /// - The input contains any other characters (spaces, punctuation, etc.)
  ///
  /// Spec: [Name](https://spec.graphql.org/draft/#sec-Names)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
  {
    // [_A-Za-z]
    let start = one_of([
      I::Token::UNDERSCORE,
      I::Token::A,
      I::Token::B,
      I::Token::C,
      I::Token::D,
      I::Token::E,
      I::Token::F,
      I::Token::G,
      I::Token::H,
      I::Token::I,
      I::Token::J,
      I::Token::K,
      I::Token::L,
      I::Token::M,
      I::Token::N,
      I::Token::O,
      I::Token::P,
      I::Token::Q,
      I::Token::R,
      I::Token::S,
      I::Token::T,
      I::Token::U,
      I::Token::V,
      I::Token::W,
      I::Token::X,
      I::Token::Y,
      I::Token::Z,
      I::Token::a,
      I::Token::b,
      I::Token::c,
      I::Token::d,
      I::Token::e,
      I::Token::f,
      I::Token::g,
      I::Token::h,
      I::Token::i,
      I::Token::j,
      I::Token::k,
      I::Token::l,
      I::Token::m,
      I::Token::n,
      I::Token::o,
      I::Token::p,
      I::Token::q,
      I::Token::r,
      I::Token::s,
      I::Token::t,
      I::Token::u,
      I::Token::v,
      I::Token::w,
      I::Token::x,
      I::Token::y,
      I::Token::z,
    ])
    .ignored();

    // [_0-9A-Za-z]*
    let cont = one_of([
      I::Token::UNDERSCORE,
      I::Token::ZERO,
      I::Token::ONE,
      I::Token::TWO,
      I::Token::THREE,
      I::Token::FOUR,
      I::Token::FIVE,
      I::Token::SIX,
      I::Token::SEVEN,
      I::Token::EIGHT,
      I::Token::NINE,
      I::Token::A,
      I::Token::B,
      I::Token::C,
      I::Token::D,
      I::Token::E,
      I::Token::F,
      I::Token::G,
      I::Token::H,
      I::Token::I,
      I::Token::J,
      I::Token::K,
      I::Token::L,
      I::Token::M,
      I::Token::N,
      I::Token::O,
      I::Token::P,
      I::Token::Q,
      I::Token::R,
      I::Token::S,
      I::Token::T,
      I::Token::U,
      I::Token::V,
      I::Token::W,
      I::Token::X,
      I::Token::Y,
      I::Token::Z,
      I::Token::a,
      I::Token::b,
      I::Token::c,
      I::Token::d,
      I::Token::e,
      I::Token::f,
      I::Token::g,
      I::Token::h,
      I::Token::i,
      I::Token::j,
      I::Token::k,
      I::Token::l,
      I::Token::m,
      I::Token::n,
      I::Token::o,
      I::Token::p,
      I::Token::q,
      I::Token::r,
      I::Token::s,
      I::Token::t,
      I::Token::u,
      I::Token::v,
      I::Token::w,
      I::Token::x,
      I::Token::y,
      I::Token::z,
    ])
    .ignored()
    .repeated();

    start
      .then(cont)
      .map_with(|_, sp| Name(Span::from_map_extra(sp)))
  }
}

impl<Span> AsRef<Span> for Name<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for Name<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0
  }
}

impl<Span> IntoComponents for Name<Span> {
  type Components = Span;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.into_span()
  }
}
