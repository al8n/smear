use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{
    CstElement, CstNode, Parseable, SyntaxTreeBuilder, error::SyntaxError,
  },
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::fmt::Debug;
use smear_lexer::keywords::Fragment;

/// Represents a named fragment definition in GraphQL.
///
/// ## Grammar
///
/// ```text
/// FragmentDefinition : fragment FragmentName TypeCondition Directives? SelectionSet
/// ```
///
/// Spec: [Fragment Definition](https://spec.graphql.org/draft/#sec-Language.Fragments.Fragment-Definitions)
#[derive(Debug, Clone)]
pub struct FragmentDefinition<FragmentName, TypeCondition, Directives, SelectionSet, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  fragment_kw: Fragment<TextRange, SyntaxToken<Lang>>,
  name: FragmentName,
  type_condition: TypeCondition,
  directives: Option<Directives>,
  selection_set: SelectionSet,
}

impl<FragmentName, TypeCondition, Directives, SelectionSet, Lang>
  FragmentDefinition<FragmentName, TypeCondition, Directives, SelectionSet, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    fragment_kw: Fragment<TextRange, SyntaxToken<Lang>>,
    name: FragmentName,
    type_condition: TypeCondition,
    directives: Option<Directives>,
    selection_set: SelectionSet,
  ) -> Self {
    Self {
      syntax,
      fragment_kw,
      name,
      type_condition,
      directives,
      selection_set,
    }
  }

  /// Tries to create a `FragmentDefinition` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering this fragment definition.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the fragment keyword token.
  #[inline]
  pub const fn fragment_keyword(&self) -> &Fragment<TextRange, SyntaxToken<Lang>>
  {
    &self.fragment_kw
  }

  /// Returns the fragment name.
  #[inline]
  pub const fn name(&self) -> &FragmentName {
    &self.name
  }

  /// Returns the type condition.
  #[inline]
  pub const fn type_condition(&self) -> &TypeCondition
  {
    &self.type_condition
  }

  /// Returns the optional directives.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives>
  {
    self.directives.as_ref()
  }

  /// Returns the selection set.
  #[inline]
  pub const fn selection_set(&self) -> &SelectionSet
  {
    &self.selection_set
  }
}

impl<'a, FragmentName, TypeCondition, Directives, SelectionSet, Lang, I, T, Error>
  Parseable<'a, I, T, Error>
  for FragmentDefinition<FragmentName, TypeCondition, Directives, SelectionSet, Lang>
where
  Fragment<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  FragmentName: Parseable<'a, I, T, Error, Language = Lang>,
  TypeCondition: Parseable<'a, I, T, Error, Language = Lang>,
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
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    builder.start_node(Self::KIND);
    Fragment::parser(builder)
      .ignore_then(FragmentName::parser(builder))
      .ignore_then(TypeCondition::parser(builder))
      .ignore_then(Directives::parser(builder).or_not())
      .ignore_then(SelectionSet::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}
