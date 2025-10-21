use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser, extra::ParserExtra},
  cst::{
    Node, Parseable, SyntaxTreeBuilder,
    cast::{child, token},
  },
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::marker::PhantomData;

use smear_lexer::{keywords::On, punctuator::Spread};

/// Represents a fragment name with the special restriction that it cannot be "on".
///
/// In GraphQL, fragment names are regular identifiers with one exception: they cannot
/// be the keyword "on" since this would create ambiguity with type conditions.
/// This type ensures this constraint is enforced at the parser level.
///
/// ## Grammar
///
/// ```text
/// FragmentName : Name but not `on`
/// ```
///
/// Spec: [Fragment Name](https://spec.graphql.org/draft/#FragmentName)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FragmentName<Name, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _name: PhantomData<Name>,
}

impl<Name, Lang> FragmentName<Name, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: Node<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _name: PhantomData,
    }
  }

  /// Tries to create a `FragmentName` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, super::error::SyntaxNodeMismatch<Self>> {
    Self::try_cast(syntax)
  }

  /// Returns the source span of the fragment name.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node representing the fragment name.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the underlying name value.
  #[inline]
  pub fn name(&self) -> Name
  where
    Name: Node<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }
}

/// Represents a type condition used in GraphQL fragments.
///
/// A type condition specifies which type a fragment applies to, using the `on` keyword
/// followed by a type name. This is essential for fragments to work with union and
/// interface types, where different fields may be available depending on the concrete type.
///
/// ## Grammar
///
/// ```text
/// TypeCondition : on NamedType
/// ```
///
/// Spec: [Type Conditions](https://spec.graphql.org/draft/#sec-Type-Conditions)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeCondition<Name, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _name: PhantomData<Name>,
}

impl<Name, Lang> TypeCondition<Name, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: Node<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _name: PhantomData,
    }
  }

  /// Tries to create a `TypeCondition` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, super::error::SyntaxNodeMismatch<Self>> {
    Self::try_cast(syntax)
  }

  /// Returns the source span of the entire type condition.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node representing the type condition.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the type condition that follows the `on` keyword.
  #[inline]
  pub fn on_keyword(&self) -> On<TextRange, SyntaxToken<Lang>>
  where
    On<TextRange, SyntaxToken<Lang>>: Node<Language = Lang>,
  {
    token(self.syntax(), &On::KIND)
      .map(|t| On::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the type name that follows the `on` keyword.
  #[inline]
  pub fn name(&self) -> Name
  where
    Name: Node<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }

  /// Creates a parser that can parse a type condition with a custom name parser.
  pub fn parser_with<'a, I, T, Error, E, NP>(
    builder: &'a SyntaxTreeBuilder<Lang>,
    name_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> NP,
  ) -> impl Parser<'a, I, (), E> + Clone
  where
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    NP: Parser<'a, I, (), E> + Clone,
    On<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  {
    builder.start_node(Self::KIND);
    On::parser(builder)
      .ignore_then(name_parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}

impl<'a, Name, Lang, I, T, Error> Parseable<'a, I, T, Error> for TypeCondition<Name, Lang>
where
  Name: Parseable<'a, I, T, Error, Language = Lang> + 'a,
  On<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: Node<Language = Lang> + 'a,
{
  type Language = Lang;

  #[inline]
  fn parser<E>(
    builder: &'a SyntaxTreeBuilder<Self::Language>,
  ) -> impl chumsky::Parser<'a, I, (), E> + Clone
  where
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    Error: 'a,
    E: chumsky::extra::ParserExtra<'a, I, Error = Error> + 'a,
  {
    Self::parser_with(builder, Name::parser)
  }
}

/// Represents a fragment spread in GraphQL selection sets.
///
/// A fragment spread allows you to include a named fragment's selection set
/// at the current location. It consists of three dots (`...`) followed by
/// the fragment name and optional directives.
///
/// ## Grammar
///
/// ```text
/// FragmentSpread : ... FragmentName Directives?
/// ```
///
/// Spec: [Fragment Spreads](https://spec.graphql.org/draft/#sec-Language.Fragments.Fragment-Spreads)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FragmentSpread<FragmentName, Directives, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _fragment_name: PhantomData<FragmentName>,
  _directives: PhantomData<Directives>,
}

impl<FragmentName, Directives, Lang> FragmentSpread<FragmentName, Directives, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
{
  /// Returns the syntax node representing the fragment spread.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }
}

impl<FragmentName, Directives, Lang> FragmentSpread<FragmentName, Directives, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: Node<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _fragment_name: PhantomData,
      _directives: PhantomData,
    }
  }

  /// Tries to create a `FragmentSpread` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, super::error::SyntaxNodeMismatch<Self>> {
    Self::try_cast(syntax)
  }

  /// Returns the source span of the entire fragment spread.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the spread operator (`...`) token.
  #[inline]
  pub fn spread_token(&self) -> Spread<TextRange, SyntaxToken<Lang>>
  where
    Spread<TextRange, SyntaxToken<Lang>>: Node<Language = Lang>,
  {
    token(self.syntax(), &Spread::KIND)
      .map(|t| Spread::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the fragment name being spread.
  #[inline]
  pub fn name(&self) -> FragmentName
  where
    FragmentName: Node<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }

  /// Returns the directives applied to this fragment spread, if any.
  #[inline]
  pub fn directives(&self) -> Directives
  where
    Directives: Node<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }

  /// Creates a parser that can parse a fragment spread with custom parsers.
  pub fn parser_with<'a, I, T, Error, E, FP, DP>(
    builder: &'a SyntaxTreeBuilder<Lang>,
    fragment_name_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> FP,
    directives_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> DP,
  ) -> impl Parser<'a, I, (), E> + Clone
  where
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    Spread<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    FP: Parser<'a, I, (), E> + Clone,
    DP: Parser<'a, I, (), E> + Clone,
  {
    builder.start_node(Self::KIND);
    Spread::parser(builder)
      .ignore_then(fragment_name_parser(builder))
      .then(directives_parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}

impl<'a, FragmentName, Directives, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for FragmentSpread<FragmentName, Directives, Lang>
where
  FragmentName: Parseable<'a, I, T, Error, Language = Lang> + 'a,
  Directives: Parseable<'a, I, T, Error, Language = Lang> + 'a,
  Spread<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: Node<Language = Lang> + 'a,
{
  type Language = Lang;

  #[inline]
  fn parser<E>(
    builder: &'a SyntaxTreeBuilder<Self::Language>,
  ) -> impl chumsky::Parser<'a, I, (), E> + Clone
  where
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    Error: 'a,
    E: chumsky::extra::ParserExtra<'a, I, Error = Error> + 'a,
  {
    Self::parser_with(builder, FragmentName::parser, Directives::parser)
  }
}

/// Represents an inline fragment in GraphQL selection sets.
///
/// An inline fragment allows you to conditionally include fields based on the
/// concrete type of the object. Unlike fragment spreads, inline fragments don't
/// reference named fragments but instead include their selection set directly.
///
/// ## Grammar
///
/// ```text
/// InlineFragment : ... TypeCondition? Directives? SelectionSet
/// ```
///
/// Spec: [Inline Fragments](https://spec.graphql.org/draft/#sec-Inline-Fragments)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InlineFragment<TypeCondition, Directives, SelectionSet, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _type_condition: PhantomData<TypeCondition>,
  _directives: PhantomData<Directives>,
  _selection_set: PhantomData<SelectionSet>,
}

impl<TypeCondition, Directives, SelectionSet, Lang>
  InlineFragment<TypeCondition, Directives, SelectionSet, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
{
  /// Returns the syntax node representing the inline fragment.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }
}

impl<TypeCondition, Directives, SelectionSet, Lang>
  InlineFragment<TypeCondition, Directives, SelectionSet, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: Node<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _type_condition: PhantomData,
      _directives: PhantomData,
      _selection_set: PhantomData,
    }
  }

  /// Tries to create an `InlineFragment` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, super::error::SyntaxNodeMismatch<Self>> {
    Self::try_cast(syntax)
  }

  /// Returns the source span of the entire inline fragment.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the spread operator (`...`) token.
  #[inline]
  pub fn spread_token(&self) -> Spread<TextRange, SyntaxToken<Lang>>
  where
    Spread<TextRange, SyntaxToken<Lang>>: Node<Language = Lang>,
  {
    token(self.syntax(), &Spread::KIND)
      .map(|t| Spread::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the type condition, if present.
  #[inline]
  pub fn type_condition(&self) -> Option<TypeCondition>
  where
    TypeCondition: Node<Language = Lang>,
  {
    child(self.syntax())
  }

  /// Returns the directives applied to this inline fragment, if any.
  #[inline]
  pub fn directives(&self) -> Option<Directives>
  where
    Directives: Node<Language = Lang>,
  {
    child(self.syntax())
  }

  /// Returns the selection set containing the fields to be selected.
  #[inline]
  pub fn selection_set(&self) -> SelectionSet
  where
    SelectionSet: Node<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }

  /// Creates a parser that can parse an inline fragment with custom component parsers.
  pub fn parser_with<'a, I, T, Error, E, TP, DP, SP>(
    builder: &'a SyntaxTreeBuilder<Lang>,
    type_condition_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> TP,
    directives_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> DP,
    selection_set_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> SP,
  ) -> impl Parser<'a, I, (), E> + Clone
  where
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    Spread<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    TP: Parser<'a, I, (), E> + Clone,
    DP: Parser<'a, I, (), E> + Clone,
    SP: Parser<'a, I, (), E> + Clone,
  {
    builder.start_node(Self::KIND);
    Spread::parser(builder)
      .ignore_then(type_condition_parser(builder).or_not())
      .then(directives_parser(builder).or_not())
      .then(selection_set_parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}

impl<'a, TypeCondition, Directives, SelectionSet, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for InlineFragment<TypeCondition, Directives, SelectionSet, Lang>
where
  TypeCondition: Parseable<'a, I, T, Error, Language = Lang> + 'a,
  Directives: Parseable<'a, I, T, Error, Language = Lang> + 'a,
  SelectionSet: Parseable<'a, I, T, Error, Language = Lang> + 'a,
  Spread<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: Node<Language = Lang> + 'a,
{
  type Language = Lang;

  #[inline]
  fn parser<E>(
    builder: &'a SyntaxTreeBuilder<Self::Language>,
  ) -> impl chumsky::Parser<'a, I, (), E> + Clone
  where
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    Error: 'a,
    E: chumsky::extra::ParserExtra<'a, I, Error = Error> + 'a,
  {
    Self::parser_with(
      builder,
      TypeCondition::parser,
      Directives::parser,
      SelectionSet::parser,
    )
  }
}
