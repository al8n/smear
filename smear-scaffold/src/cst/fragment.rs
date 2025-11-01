use logosky::{
  Logos, LosslessToken, Source, LogoStream,
  chumsky::{self, Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::fmt::Debug;

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
#[derive(Debug, Clone)]
pub struct FragmentName<Name, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  name: Name,
}

impl<Name, Lang> FragmentName<Name, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn new(syntax: SyntaxNode<Lang>, name: Name) -> Self {
    Self { syntax, name }
  }

  /// Tries to create a `FragmentName` from the given syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the source span of the fragment name.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node representing the fragment name.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the underlying name value.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn name(&self) -> &Name {
    &self.name
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
#[derive(Debug, Clone)]
pub struct TypeCondition<Name, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  on: On<TextRange, SyntaxToken<Lang>>,
  name: Name,
}

impl<Name, Lang> TypeCondition<Name, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn new(
    syntax: SyntaxNode<Lang>,
    on: On<TextRange, SyntaxToken<Lang>>,
    name: Name,
  ) -> Self {
    Self { syntax, on, name }
  }

  /// Tries to create a `TypeCondition` from the given syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the source span of the entire type condition.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node representing the type condition.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the type condition that follows the `on` keyword.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn on_keyword(&self) -> &On<TextRange, SyntaxToken<Lang>> {
    &self.on
  }

  /// Returns the type name that follows the `on` keyword.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Creates a parser that can parse a type condition with a custom name parser.
  pub fn parser_with<'a, I, T, Error, E, NP>(
    builder: &'a SyntaxTreeBuilder<Lang>,
    name_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> NP,
  ) -> impl Parser<'a, I, (), E> + Clone
  where
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    NP: Parser<'a, I, (), E> + Clone,
    On<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    Lang::Kind: Into<rowan::SyntaxKind>,
    Self: CstNode<Lang>,
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
  Self: CstNode<Lang> + 'a,
{
  type Language = Lang;

  #[inline]
  fn parser<E>(
    builder: &'a SyntaxTreeBuilder<Self::Language>,
  ) -> impl chumsky::Parser<'a, I, (), E> + Clone
  where
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
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
#[derive(Debug, Clone)]
pub struct FragmentSpread<FragmentName, Directives, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  spread: Spread<TextRange, SyntaxToken<Lang>>,
  name: FragmentName,
  directives: Option<Directives>,
}

impl<FragmentName, Directives, Lang> FragmentSpread<FragmentName, Directives, Lang>
where
  Lang: Language,
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
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn new(
    syntax: SyntaxNode<Lang>,
    spread: Spread<TextRange, SyntaxToken<Lang>>,
    name: FragmentName,
    directives: Option<Directives>,
  ) -> Self {
    Self {
      syntax,
      spread,
      name,
      directives,
    }
  }

  /// Tries to create a `FragmentSpread` from the given syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the source span of the entire fragment spread.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the spread operator (`...`) token.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn spread_token(&self) -> Spread<TextRange, SyntaxToken<Lang>>
  where
    Spread<TextRange, SyntaxToken<Lang>>: Clone,
  {
    self.spread.clone()
  }

  /// Returns the fragment name being spread.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn name(&self) -> FragmentName
  where
    FragmentName: Clone,
  {
    self.name.clone()
  }

  /// Returns the directives applied to this fragment spread, if any.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn directives(&self) -> Option<Directives>
  where
    Directives: Clone,
  {
    self.directives.clone()
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
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    Spread<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    FP: Parser<'a, I, (), E> + Clone,
    DP: Parser<'a, I, (), E> + Clone,
    Self: CstNode<Lang>,
    Lang::Kind: Into<rowan::SyntaxKind>,
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
  Self: CstNode<Lang> + 'a,
{
  type Language = Lang;

  #[inline]
  fn parser<E>(
    builder: &'a SyntaxTreeBuilder<Self::Language>,
  ) -> impl chumsky::Parser<'a, I, (), E> + Clone
  where
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
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
#[derive(Debug, Clone)]
pub struct InlineFragment<TypeCondition, Directives, SelectionSet, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  spread: Spread<TextRange, SyntaxToken<Lang>>,
  type_condition: Option<TypeCondition>,
  directives: Option<Directives>,
  selection_set: SelectionSet,
}

impl<TypeCondition, Directives, SelectionSet, Lang>
  InlineFragment<TypeCondition, Directives, SelectionSet, Lang>
where
  Lang: Language,
{
  /// Returns the syntax node representing the inline fragment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }
}

impl<TypeCondition, Directives, SelectionSet, Lang>
  InlineFragment<TypeCondition, Directives, SelectionSet, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn new(
    syntax: SyntaxNode<Lang>,
    spread: Spread<TextRange, SyntaxToken<Lang>>,
    type_condition: Option<TypeCondition>,
    directives: Option<Directives>,
    selection_set: SelectionSet,
  ) -> Self {
    Self {
      syntax,
      spread,
      type_condition,
      directives,
      selection_set,
    }
  }

  /// Tries to create an `InlineFragment` from the given syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the source span of the entire inline fragment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the spread operator (`...`) token.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn spread_token(&self) -> &Spread<TextRange, SyntaxToken<Lang>> {
    &self.spread
  }

  /// Returns the optional type condition.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn type_condition(&self) -> Option<&TypeCondition> {
    self.type_condition.as_ref()
  }

  /// Returns the directives applied to this inline fragment, if any.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns the selection set containing the fields to be selected.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn selection_set(&self) -> &SelectionSet {
    &self.selection_set
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
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    Spread<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    TP: Parser<'a, I, (), E> + Clone,
    DP: Parser<'a, I, (), E> + Clone,
    SP: Parser<'a, I, (), E> + Clone,
    Self: CstNode<Lang>,
    Lang::Kind: Into<rowan::SyntaxKind>,
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
  TypeCondition: Parseable<'a, I, T, Error, Language = Lang> + CstNode<Lang> + 'a,
  Directives: Parseable<'a, I, T, Error, Language = Lang> + 'a,
  SelectionSet: Parseable<'a, I, T, Error, Language = Lang> + 'a,
  Spread<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang> + 'a,
{
  type Language = Lang;

  #[inline]
  fn parser<E>(
    builder: &'a SyntaxTreeBuilder<Self::Language>,
  ) -> impl chumsky::Parser<'a, I, (), E> + Clone
  where
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
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
