use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{Node, Parseable, SyntaxTreeBuilder, cast::child, error::SyntaxNodeMismatch},
};
use rowan::{Language, SyntaxNode, TextRange};

use core::marker::PhantomData;

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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
  _name: PhantomData<Name>,
  _operation_type: PhantomData<OperationType>,
  _variables_definition: PhantomData<VariablesDefinition>,
  _directives: PhantomData<Directives>,
  _selection_set: PhantomData<SelectionSet>,
}

impl<Name, OperationType, VariablesDefinition, Directives, SelectionSet, Lang>
  NamedOperationDefinition<Name, OperationType, VariablesDefinition, Directives, SelectionSet, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: Node<Language = Lang>,
{
  /// Tries to create a `NamedOperationDefinition` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxNodeMismatch<Self>> {
    Self::try_cast(syntax)
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
  pub fn operation_type(&self) -> OperationType
  where
    OperationType: Node<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }

  /// Returns the name of the named operation definition.
  #[inline]
  pub fn name(&self) -> Option<Name>
  where
    Name: Node<Language = Lang>,
  {
    child(self.syntax())
  }

  /// Returns the variable definitions of the named operation definition.
  #[inline]
  pub fn variable_definitions(&self) -> Option<VariablesDefinition>
  where
    VariablesDefinition: Node<Language = Lang>,
  {
    child(self.syntax())
  }

  /// Returns the directives of the named operation definition.
  #[inline]
  pub fn directives(&self) -> Option<Directives>
  where
    Directives: Node<Language = Lang>,
  {
    child(self.syntax())
  }

  /// Returns the selection set of the named operation definition.
  #[inline]
  pub fn selection_set(&self) -> SelectionSet
  where
    SelectionSet: Node<Language = Lang>,
  {
    child(self.syntax()).unwrap()
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
  Self: Node<Language = Lang>,
{
  type Language = Lang;

  #[inline]
  fn parser<E>(builder: &'a SyntaxTreeBuilder<Self::Language>) -> impl Parser<'a, I, (), E> + Clone
  where
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
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
