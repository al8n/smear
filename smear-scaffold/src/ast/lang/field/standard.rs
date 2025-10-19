// use crate::{
//   smear_lexer::keywords::On,
//   smear_lexer::punctuator::{LBrace, RBrace, Spread},
//   scaffold::{self, FragmentSpread},
// };
use derive_more::{From, Into};
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{Parser, extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use smear_lexer::{
  keywords::On,
  punctuator::{LBrace, RBrace, Spread},
};

use crate::ast::{self, FragmentSpread};

use super::super::{StandardSelection, StandardSelectionSet};

/// Represents a field in a GraphQL selection set.
///
/// A field is the basic unit of data that can be requested in GraphQL. StandardFields can have
/// aliases, arguments, directives, and nested selection sets. This structure represents
/// the complete syntax for a field including all its optional components.
///
/// ## Examples
///
/// ```text
/// # Simple field
/// name
///
/// ### StandardField with alias
/// userName: name
///
/// ### StandardField with arguments
/// user(id: "123")
///
/// ### StandardField with directives
/// name @deprecated
///
/// ### StandardField with selection set (for object types)
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
/// * `StandardSelectionSet` - The type representing nested field selections
/// * `Span` - The type representing source location information
///
/// ## Grammar
///
/// ```text
/// StandardField : Alias? Name Arguments? Directives? StandardSelectionSet?
/// ```
///
/// Spec: [StandardFields](https://spec.graphql.org/draft/#sec-Language.StandardFields)
#[derive(Debug, Clone, From, Into)]
#[allow(clippy::type_complexity)]
pub struct StandardField<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>(
  ast::Field<
    Alias,
    Name,
    Arguments,
    Directives,
    StandardSelectionSet<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>,
  >,
);

impl<Alias, Name, FragmentName, TypeCondition, Arguments, Directives> AsSpan<Span>
  for StandardField<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.span()
  }
}

impl<Alias, Name, FragmentName, TypeCondition, Arguments, Directives> IntoSpan<Span>
  for StandardField<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>
{
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Alias, Name, FragmentName, TypeCondition, Arguments, Directives> IntoComponents
  for StandardField<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>
{
  type Components = (
    Span,
    Option<Alias>,
    Name,
    Option<Arguments>,
    Option<Directives>,
    Option<StandardSelectionSet<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>
  StandardField<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>
{
  /// Returns a reference to the span covering the entire field.
  ///
  /// The span includes the alias (if present), field name, arguments, directives,
  /// and selection set. This is useful for error reporting, syntax highlighting,
  /// and source mapping.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns a reference to the field's alias, if present.
  ///
  /// An alias allows the field to be returned under a different name in the response.
  /// This is useful for requesting the same field multiple times with different arguments
  /// or for providing more descriptive names.
  #[inline]
  pub const fn alias(&self) -> Option<&Alias> {
    self.0.alias()
  }

  /// Returns a reference to the field's name.
  ///
  /// This is the actual field name that will be resolved against the schema.
  /// If an alias is present, the alias name will be used as the key in the response,
  /// but this name determines which field is actually queried.
  #[inline]
  pub const fn name(&self) -> &Name {
    self.0.name()
  }

  /// Returns a reference to the field's arguments, if present.
  ///
  /// Arguments allow you to pass parameters to fields, enabling filtering,
  /// pagination, or other field-specific behavior.
  #[inline]
  pub const fn arguments(&self) -> Option<&Arguments> {
    self.0.arguments()
  }

  /// Returns a reference to the field's directives, if present.
  ///
  /// Directives provide metadata or instructions for processing the field,
  /// such as conditional inclusion, deprecation warnings, or custom behaviors.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.0.directives()
  }

  /// Returns a reference to the field's selection set, if present.
  ///
  /// A selection set defines what subfields to request for object or interface types.
  /// Scalar fields typically don't have selection sets, while object fields require them
  /// to specify which nested fields to include in the response.
  #[inline]
  pub const fn selection_set(
    &self,
  ) -> Option<&StandardSelectionSet<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>>
  {
    self.0.selection_set()
  }
}

impl<
  'a,
  Alias: 'a,
  Name: 'a,
  FragmentName: 'a,
  TypeCondition: 'a,
  Arguments: 'a,
  Directives: 'a,
  I,
  T,
  Error,
> Parseable<'a, I, T, Error>
  for StandardField<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>
where
  On: Parseable<'a, I, T, Error> + 'a,
  Spread: Parseable<'a, I, T, Error> + 'a,
  LBrace: Parseable<'a, I, T, Error> + 'a,
  RBrace: Parseable<'a, I, T, Error> + 'a,
  TypeCondition: Parseable<'a, I, T, Error> + 'a,
  Alias: Parseable<'a, I, T, Error> + 'a,
  Name: Parseable<'a, I, T, Error> + 'a,
  FragmentName: Parseable<'a, I, T, Error> + 'a,
  Arguments: Parseable<'a, I, T, Error> + 'a,
  Directives: Parseable<'a, I, T, Error> + 'a,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>> + 'a,
    T: Token<'a>,
    Error: 'a,
  {
    recursive(|field_parser| {
      // Inner fixpoint: build a `StandardSelection<Span>` parser by using the recursive `field_parser`.
      let selection = recursive(|selection| {
        // StandardSelectionSet needs a `StandardSelection` parser
        let selection_set = ast::StandardSelectionSet::parser_with(selection.clone());

        let spread = FragmentSpread::parser().map(|fs| StandardSelection::FragmentSpread(fs));

        let inline = ast::InlineFragment::parser_with(
          TypeCondition::parser(),
          Directives::parser(),
          selection_set.clone(),
        )
        .map(|f| StandardSelection::InlineFragment(f));

        choice((field_parser.map(StandardSelection::Field), spread, inline))
      });

      // Pass the selection parser to the selection set
      let selection_set = ast::StandardSelectionSet::parser_with(selection);

      ast::Field::parser_with(Arguments::parser(), Directives::parser(), selection_set).map(Self)
    })
  }
}
