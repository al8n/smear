use chumsky::{extra::ParserExtra, prelude::*};

use super::{super::source::*, Name, ignored, punct::Colon};

/// Represents a field alias in GraphQL syntax.
///
/// An alias allows you to rename the result of a field to avoid conflicts or
/// provide more descriptive names in the response. The alias consists of a name
/// followed by a colon, which precedes the actual field name.
///
/// # Examples
///
/// ```text
/// user: profile        # "user" is the alias for field "profile"
/// primaryEmail: email  # "primaryEmail" is the alias for field "email"
/// count: totalUsers    # "count" is the alias for field "totalUsers"
/// ```
///
/// ## Grammar
///
/// ```text
/// Alias : Name
/// ```
///
/// Spec: [Field Alias](https://spec.graphql.org/draft/#sec-Field-Alias)
#[derive(Debug, Clone, Copy)]
pub struct Alias<Span> {
  span: Span,
  name: Name<Span>,
  colon: Colon<Span>,
}

impl<Span> AsRef<Span> for Alias<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for Alias<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Span> IntoComponents for Alias<Span> {
  type Components = (Span, Name<Span>, Colon<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.colon)
  }
}

impl<Span> Alias<Span> {
  /// Returns a reference to the span covering the entire alias.
  ///
  /// The span includes both the alias name and the colon separator.
  /// This is useful for error reporting, syntax highlighting, and source mapping.
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the colon separator.
  ///
  /// The colon separates the alias name from the field name that follows.
  /// This provides access to the exact location and span information of the separator.
  pub const fn colon(&self) -> &Colon<Span> {
    &self.colon
  }

  /// Returns a reference to the name part of the alias.
  ///
  /// This is the identifier that will be used as the key in the response
  /// object instead of the actual field name.
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// Creates a parser that can parse a field alias.
  ///
  /// The parser expects an identifier (name) followed by optional ignored tokens
  /// and then a colon. This parser is typically used as part of a larger field parser.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the alias.
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
      .then_ignore(ignored())
      .then(Colon::parser())
      .map_with(|(name, colon), sp| Self {
        span: Span::from_map_extra(sp),
        name,
        colon,
      })
  }
}

/// Represents a field in a GraphQL selection set.
///
/// A field is the basic unit of data that can be requested in GraphQL. Fields can have
/// aliases, arguments, directives, and nested selection sets. This structure represents
/// the complete syntax for a field including all its optional components.
///
/// ## Examples
///
/// ```text
/// # Simple field
/// name
///
/// ### Field with alias
/// userName: name
///
/// ### Field with arguments
/// user(id: "123")
///
/// ### Field with directives
/// name @deprecated
///
/// ### Field with selection set (for object types)
/// user {
///   id
///   name
/// }
///
/// # Complex field with all components
/// primaryUser: user(id: "123") @include(if: true) {
///   id
///   profile {
///     name
///     email
///   }
/// }
/// ```
///
/// ## Type Parameters
///
/// * `Args` - The type representing field arguments
/// * `Directives` - The type representing field directives
/// * `SelectionSet` - The type representing nested field selections
/// * `Span` - The type representing source location information
///
/// ## Grammar
///
/// ```text
/// Field : Alias? Name Arguments? Directives? SelectionSet?
/// ```
///
/// Spec: [Fields](https://spec.graphql.org/draft/#sec-Language.Fields)
#[derive(Debug, Clone, Copy)]
pub struct Field<Args, Directives, SelectionSet, Span> {
  span: Span,
  alias: Option<Alias<Span>>,
  name: Name<Span>,
  arguments: Option<Args>,
  directives: Option<Directives>,
  selection_set: Option<SelectionSet>,
}

impl<Args, Directives, SelectionSet, Span> AsRef<Span>
  for Field<Args, Directives, SelectionSet, Span>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Args, Directives, SelectionSet, Span> IntoSpan<Span>
  for Field<Args, Directives, SelectionSet, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Args, Directives, SelectionSet, Span> IntoComponents
  for Field<Args, Directives, SelectionSet, Span>
{
  type Components = (
    Span,
    Option<Alias<Span>>,
    Name<Span>,
    Option<Args>,
    Option<Directives>,
    Option<SelectionSet>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.alias,
      self.name,
      self.arguments,
      self.directives,
      self.selection_set,
    )
  }
}

impl<Args, Directives, SelectionSet, Span> Field<Args, Directives, SelectionSet, Span> {
  /// Returns a reference to the span covering the entire field.
  ///
  /// The span includes the alias (if present), field name, arguments, directives,
  /// and selection set. This is useful for error reporting, syntax highlighting,
  /// and source mapping.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the field's alias, if present.
  ///
  /// An alias allows the field to be returned under a different name in the response.
  /// This is useful for requesting the same field multiple times with different arguments
  /// or for providing more descriptive names.
  #[inline]
  pub const fn alias(&self) -> Option<&Alias<Span>> {
    self.alias.as_ref()
  }

  /// Returns a reference to the field's name.
  ///
  /// This is the actual field name that will be resolved against the schema.
  /// If an alias is present, the alias name will be used as the key in the response,
  /// but this name determines which field is actually queried.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// Returns a reference to the field's arguments, if present.
  ///
  /// Arguments allow you to pass parameters to fields, enabling filtering,
  /// pagination, or other field-specific behavior.
  #[inline]
  pub const fn arguments(&self) -> Option<&Args> {
    self.arguments.as_ref()
  }

  /// Returns a reference to the field's directives, if present.
  ///
  /// Directives provide metadata or instructions for processing the field,
  /// such as conditional inclusion, deprecation warnings, or custom behaviors.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns a reference to the field's selection set, if present.
  ///
  /// A selection set defines what subfields to request for object or interface types.
  /// Scalar fields typically don't have selection sets, while object fields require them
  /// to specify which nested fields to include in the response.
  #[inline]
  pub const fn selection_set(&self) -> Option<&SelectionSet> {
    self.selection_set.as_ref()
  }

  /// Creates a parser that can parse a complete field with custom component parsers.
  ///
  /// This parser handles the complete field syntax including optional alias, required name,
  /// and optional arguments, directives, and selection set. The parsing of each component
  /// is delegated to the provided parsers.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the field.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, AP, DP, SP>(
    args: AP,
    directives: DP,
    selection_set: SP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
    AP: Parser<'src, I, Args, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    SP: Parser<'src, I, SelectionSet, E> + Clone,
  {
    Alias::parser()
      .or_not()
      .then(Name::parser().padded_by(ignored()))
      .then(args.or_not())
      .then(ignored().ignore_then(directives).or_not())
      .then(ignored().ignore_then(selection_set).or_not())
      .map_with(
        |((((alias, name), arguments), directives), selection_set), sp| Self {
          span: Span::from_map_extra(sp),
          alias,
          name,
          arguments,
          directives,
          selection_set,
        },
      )
  }
}
