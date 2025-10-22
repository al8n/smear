use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstNode, CstToken, CstElement, Parseable, SyntaxTreeBuilder, cast::child, error::CastNodeError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::marker::PhantomData;
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FragmentDefinition<FragmentName, TypeCondition, Directives, SelectionSet, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _fragment_name: PhantomData<FragmentName>,
  _type_condition: PhantomData<TypeCondition>,
  _directives: PhantomData<Directives>,
  _selection_set: PhantomData<SelectionSet>,
}

impl<FragmentName, TypeCondition, Directives, SelectionSet, Lang>
  FragmentDefinition<FragmentName, TypeCondition, Directives, SelectionSet, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _fragment_name: PhantomData,
      _type_condition: PhantomData,
      _directives: PhantomData,
      _selection_set: PhantomData,
    }
  }

  /// Tries to create a `FragmentDefinition` from the given syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, CastNodeError<Self>> {
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
  pub fn fragment_keyword(&self) -> Fragment<TextRange, SyntaxToken<Lang>>
  where
    Fragment<TextRange, SyntaxToken<Lang>>: CstToken<Language = Lang>,
  {
    logosky::cst::cast::token(self.syntax(), &Fragment::KIND)
      .map(|t| Fragment::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the fragment name.
  #[inline]
  pub fn name(&self) -> FragmentName
  where
    FragmentName: CstNode<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }

  /// Returns the type condition.
  #[inline]
  pub fn type_condition(&self) -> TypeCondition
  where
    TypeCondition: CstNode<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }

  /// Returns the optional directives.
  #[inline]
  pub fn directives(&self) -> Option<Directives>
  where
    Directives: CstNode<Language = Lang>,
  {
    child(self.syntax())
  }

  /// Returns the selection set.
  #[inline]
  pub fn selection_set(&self) -> SelectionSet
  where
    SelectionSet: CstNode<Language = Lang>,
  {
    child(self.syntax()).unwrap()
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
  Self: CstNode<Language = Lang>,
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
