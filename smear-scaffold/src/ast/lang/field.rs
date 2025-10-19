use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{extra::ParserExtra, prelude::*},
  utils::{
    AsSpan, IntoComponents, IntoSpan, Span,
    human_display::DisplayHuman,
    sdl_display::{DisplayCompact, DisplayPretty},
    syntax_tree_display::DisplaySyntaxTree,
  },
};

use smear_lexer::punctuator::Colon;

pub use standard::*;

mod standard;

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
pub struct Alias<Name> {
  span: Span,
  name: Name,
}

impl<Name> AsSpan<Span> for Alias<Name> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name> IntoSpan<Span> for Alias<Name> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name> IntoComponents for Alias<Name> {
  type Components = (Span, Name);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name)
  }
}

impl<Name> Alias<Name> {
  /// Returns a reference to the span covering the entire alias.
  ///
  /// The span includes both the alias name and the colon separator.
  /// This is useful for error reporting, syntax highlighting, and source mapping.
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the name part of the alias.
  ///
  /// This is the identifier that will be used as the key in the response
  /// object instead of the actual field name.
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Creates a parser for an alias using the provided name parser.
  ///
  /// The parser recognizes a name followed by a colon, constructing an `Alias`
  /// instance with the
  /// parsed name and the span covering the entire alias.
  pub fn parser_with<'a, I, T, Error, E, P>(name_parser: P) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    P: Parser<'a, I, Name, E> + Clone,
    Colon: Parseable<'a, I, T, Error>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    name_parser
      .then_ignore(Colon::parser())
      .map_with(|name, exa| Self {
        span: exa.span(),
        name,
      })
  }
}

impl<Name> core::fmt::Display for Alias<Name>
where
  Name: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "{}:", self.name.display())
  }
}

impl<Name> core::ops::Deref for Alias<Name> {
  type Target = Name;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.name()
  }
}

impl<Name> DisplayHuman for Alias<Name>
where
  Name: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Display::fmt(self, f)
  }
}

impl<Name> DisplayCompact for Alias<Name>
where
  Name: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    DisplayHuman::fmt(self, f)
  }
}

impl<Name> DisplayPretty for Alias<Name>
where
  Name: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    DisplayCompact::fmt(self, f, &())
  }
}

impl<Name> DisplaySyntaxTree for Alias<Name>
where
  Name: DisplaySyntaxTree,
{
  #[inline]
  fn fmt(
    &self,
    level: usize,
    indent: usize,
    f: &mut core::fmt::Formatter<'_>,
  ) -> core::fmt::Result {
    let padding = level * indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    writeln!(f, "- ALIAS@{}..{}", self.span.start(), self.span.end())?;
    self.name.fmt(level + 1, indent, f)
  }
}

impl<'a, Name, I, T, Error> Parseable<'a, I, T, Error> for Alias<Name>
where
  Name: Parseable<'a, I, T, Error>,
  Colon: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Self::parser_with(Name::parser())
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
#[derive(Debug, Clone)]
pub struct Field<Alias, Name, Arguments, Directives, SelectionSet> {
  span: Span,
  alias: Option<Alias>,
  name: Name,
  arguments: Option<Arguments>,
  directives: Option<Directives>,
  selection_set: Option<SelectionSet>,
}

impl<Alias, Name, Arguments, Directives, SelectionSet> AsSpan<Span>
  for Field<Alias, Name, Arguments, Directives, SelectionSet>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Alias, Name, Arguments, Directives, SelectionSet> IntoSpan<Span>
  for Field<Alias, Name, Arguments, Directives, SelectionSet>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Alias, Name, Arguments, Directives, SelectionSet> IntoComponents
  for Field<Alias, Name, Arguments, Directives, SelectionSet>
{
  type Components = (
    Span,
    Option<Alias>,
    Name,
    Option<Arguments>,
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

impl<Alias, Name, Arguments, Directives, SelectionSet>
  Field<Alias, Name, Arguments, Directives, SelectionSet>
{
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
  pub const fn alias(&self) -> Option<&Alias> {
    self.alias.as_ref()
  }

  /// Returns a reference to the field's name.
  ///
  /// This is the actual field name that will be resolved against the schema.
  /// If an alias is present, the alias name will be used as the key in the response,
  /// but this name determines which field is actually queried.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns a reference to the field's arguments, if present.
  ///
  /// Arguments allow you to pass parameters to fields, enabling filtering,
  /// pagination, or other field-specific behavior.
  #[inline]
  pub const fn arguments(&self) -> Option<&Arguments> {
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
  pub fn parser_with<'a, I, T, Error, E, AP, DP, SP>(
    args: AP,
    directives: DP,
    selection_set: SP,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    Error: 'a,
    Name: Parseable<'a, I, T, Error> + 'a,
    Alias: Parseable<'a, I, T, Error> + 'a,
    AP: Parser<'a, I, Arguments, E> + Clone,
    DP: Parser<'a, I, Directives, E> + Clone,
    SP: Parser<'a, I, SelectionSet, E> + Clone,
  {
    Alias::parser()
      .or_not()
      .then(Name::parser())
      .then(args.or_not())
      .then(directives.or_not())
      .then(selection_set.or_not())
      .map_with(
        |((((alias, name), arguments), directives), selection_set), exa| Self {
          span: exa.span(),
          alias,
          name,
          arguments,
          directives,
          selection_set,
        },
      )
  }
}

// impl<'a, Alias: 'a, Name: 'a, FragmentName: 'a, TypeCondition: 'a, Arguments: 'a, Directives: 'a, Container, I, T, Error> Parseable<I, T, Error> for Field<Alias, Name, Arguments, Directives, SelectionSet>
// where
//   T: Token<'a>,
//   I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
//   On: Parseable<I, T, Error>,
//   Spread: Parseable<I, T, Error>,
//   LBrace: Parseable<I, T, Error>,
//   RBrace: Parseable<I, T, Error>,
//   TypeCondition: Parseable<I, T, Error>,
//   Alias: Parseable<I, T, Error>,
//   Name: Parseable<I, T, Error>,
//   FragmentName: Parseable<I, T, Error>,
//   Arguments: Parseable<I, T, Error>,
//   Directives: Parseable<I, T, Error>,
//   Error: 'a,
//   Container: chumsky::container::Container<Selection<Alias, Name, Arguments, Directives, SelectionSet>> + 'a,
// {
//   #[inline]
//   fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
//   where
//     Self: Sized,
//     E: ParserExtra<'a, I, Error = Error> + 'a
//   {
//     recursive(|field_parser| {
//       // Inner fixpoint: build a `Selection` parser by using the recursive `field_parser`.
//       let selection = recursive(|selection| {
//         // SelectionSet needs a `Selection` parser
//         let selection_set =
//           SelectionSet::parser_with(selection.clone());

//         let spread = FragmentSpread::parser()
//           .map(|fs| Selection::FragmentSpread(fs));

//         let inline = InlineFragment::parser_with(
//           TypeCondition::parser(),
//           Directives::parser(),
//           selection_set,
//         )
//           .map(|f| Selection::InlineFragment(f));

//         choice((field_parser.map(Selection::Field), spread, inline))
//       });

//       // Pass the selection parser to the selection set
//       Self::parser_with(Arguments::parser(), Directives::parser(), SelectionSet::parser_with(selection))
//     })
//   }
// }
