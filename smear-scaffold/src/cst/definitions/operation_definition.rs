use logosky::{
  Logos, LosslessToken, Source, LogoStream,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, TextRange};

use core::fmt::Debug;

/// Represents a named GraphQL operation definition.
///
/// ## Grammar
///
/// ```text
/// OperationDefinition:
///   OperationType Name? VariablesDefinition? Directives? SelectionSet
/// ```
///
/// Spec: [Operation Definition](https://spec.graphql.org/draft/#sec-Language.Operations)
#[derive(Debug, Clone)]
pub struct NamedOperationDefinition<
  Name,
  OperationType,
  VariablesDefinition,
  Directives,
  SelectionSet,
  Lang,
> where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  operation_type: OperationType,
  name: Option<Name>,
  variables_definition: Option<VariablesDefinition>,
  directives: Option<Directives>,
  selection_set: SelectionSet,
}

impl<Name, OperationType, VariablesDefinition, Directives, SelectionSet, Lang>
  NamedOperationDefinition<Name, OperationType, VariablesDefinition, Directives, SelectionSet, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    operation_type: OperationType,
    name: Option<Name>,
    variables_definition: Option<VariablesDefinition>,
    directives: Option<Directives>,
    selection_set: SelectionSet,
  ) -> Self {
    Self {
      syntax,
      operation_type,
      name,
      variables_definition,
      directives,
      selection_set,
    }
  }

  /// Tries to create a `NamedOperationDefinition` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the span of the named operation definition.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node of the named operation definition.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the operation type of the named operation definition.
  #[inline]
  pub const fn operation_type(&self) -> &OperationType {
    &self.operation_type
  }

  /// Returns the name of the named operation definition.
  #[inline]
  pub const fn name(&self) -> Option<&Name> {
    self.name.as_ref()
  }

  /// Returns the variable definitions of the named operation definition.
  #[inline]
  pub const fn variable_definitions(&self) -> Option<&VariablesDefinition> {
    self.variables_definition.as_ref()
  }

  /// Returns the directives of the named operation definition.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns the selection set of the named operation definition.
  #[inline]
  pub const fn selection_set(&self) -> &SelectionSet {
    &self.selection_set
  }
}

impl<'a, Name, OperationType, VariablesDefinition, Directives, SelectionSet, Lang, I, T, Error>
  Parseable<'a, I, T, Error>
  for NamedOperationDefinition<
    Name,
    OperationType,
    VariablesDefinition,
    Directives,
    SelectionSet,
    Lang,
  >
where
  Name: Parseable<'a, I, T, Error, Language = Lang>,
  OperationType: Parseable<'a, I, T, Error, Language = Lang>,
  VariablesDefinition: Parseable<'a, I, T, Error, Language = Lang>,
  Directives: Parseable<'a, I, T, Error, Language = Lang>,
  SelectionSet: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
{
  type Language = Lang;

  #[inline]
  fn parser<E>(builder: &'a SyntaxTreeBuilder<Self::Language>) -> impl Parser<'a, I, (), E> + Clone
  where
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    builder.start_node(Self::KIND);
    OperationType::parser(builder)
      .ignore_then(Name::parser(builder).or_not())
      .ignore_then(VariablesDefinition::parser(builder).or_not())
      .ignore_then(Directives::parser(builder).or_not())
      .ignore_then(SelectionSet::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}
