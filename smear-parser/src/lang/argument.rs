use logosky::utils::Span;
use smear_utils::{IntoComponents, IntoSpan};

/// A single named argument in a GraphQL operation or directive.
///
/// Represents a name-value pair used to pass parameters to GraphQL fields,
/// directives, or other language constructs. Arguments follow the standard
/// GraphQL syntax of a name identifier followed by a colon and a value.
///
/// ## Grammar
///
/// ```text
/// Argument ::= Name ':' Value
/// ```
///
/// Spec: [Argument](https://spec.graphql.org/draft/#Argument)
#[derive(Debug, Clone, Copy)]
pub struct Argument<Name, Colon, Value> {
  span: Span,
  name: Name,
  colon: Colon,
  value: Value,
}

impl<Name, Colon, Value> AsRef<Span> for Argument<Name, Colon, Value> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name, Colon, Value> IntoSpan<Span> for Argument<Name, Colon, Value> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Colon, Value> IntoComponents for Argument<Name, Colon, Value> {
  type Components = (Span, Name, Colon, Value);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.colon, self.value)
  }
}

impl<Name, Colon, Value> Argument<Name, Colon, Value> {
  /// Returns the source span of the entire argument.
  ///
  /// This span covers from the first character of the argument name through
  /// the last character of the argument value, providing the complete source
  /// location for error reporting and source mapping.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the colon separator token.
  ///
  /// This provides access to the `:` character that separates the argument
  /// name from its value, including its exact source position. Useful for
  /// syntax highlighting and precise error reporting.
  #[inline]
  pub const fn colon(&self) -> &Colon {
    &self.colon
  }

  /// Returns the argument name identifier.
  ///
  /// This provides access to the GraphQL name that identifies this argument.
  /// The name follows standard GraphQL identifier rules and is used to match
  /// the argument with its expected parameter in the schema.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the argument value.
  ///
  /// This provides access to the value assigned to this argument. The value
  /// can be any valid GraphQL input value including scalars, enums, objects,
  /// lists, variables, or null depending on the argument's expected type.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }

  // /// Creates a parser for GraphQL arguments with a custom value parser.
  // ///
  // /// This parser handles the complete argument syntax including the argument
  // /// name, colon separator, and argument value. It manages whitespace around
  // /// the colon according to GraphQL's flexible whitespace rules.
  // ///
  // /// ## Notes
  // ///
  // /// This parser does not handle surrounding [ignored tokens].
  // /// The calling parser is responsible for handling any necessary
  // /// whitespace skipping or comment processing around the argument.
  // ///
  // /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  // pub fn parser_with<'src, I, E, P>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  // where
  //   I: Source<'src>,
  //   I::Token: Char + 'src,
  //   I::Slice: Slice<Token = I::Token>,
  //   E: ParserExtra<'src, I>,
  //   Span: crate::source::FromMapExtra<'src, I, E>,
  //   P: Parser<'src, I, Value, E> + Clone,
  // {
  //   Name::parser()
  //     .then(Colon::parser().padded_by(ignored()))
  //     .then(value)
  //     .map_with(|((name, colon), value), sp| Self {
  //       span: Span::from_map_extra(sp),
  //       name,
  //       colon,
  //       value,
  //     })
  // }
}
