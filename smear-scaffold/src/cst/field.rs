use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use smear_lexer::punctuator::Colon;

/// The standard field implementations.
pub mod standard;

/// Represents a field alias in GraphQL syntax.
///
/// An alias allows you to rename the result of a field to avoid conflicts or
/// provide more descriptive names in the response. The alias consists of a name
/// followed by a colon, which precedes the actual field name.
///
/// ## Grammar
///
/// ```text
/// Alias : Name
/// ```
///
/// Spec: [Field Alias](https://spec.graphql.org/draft/#sec-Field-Alias)
#[derive(Debug, Clone)]
pub struct Alias<Name, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  name: Name,
  colon: Colon<TextRange, SyntaxToken<Lang>>,
}

impl<Name, Lang> Alias<Name, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn new(
    syntax: SyntaxNode<Lang>,
    name: Name,
    colon: Colon<TextRange, SyntaxToken<Lang>>,
  ) -> Self {
    Self {
      syntax,
      name,
      colon,
    }
  }

  /// Tries to create an `Alias` from the given syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self>>
  where
    Lang::Kind: Into<rowan::SyntaxKind>,
    Self: CstNode<Language = Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the source span of the entire alias.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node representing the entire alias.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Attempts to cast a syntax node to a `Name`.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the colon token separating the alias and the field name.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn colon_token(&self) -> &Colon<TextRange, SyntaxToken<Lang>> {
    &self.colon
  }

  /// Creates a parser for an alias using the provided name parser.
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
    Colon<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    NP: Parser<'a, I, (), E> + Clone,
    Lang::Kind: Into<rowan::SyntaxKind>,
    Self: CstNode<Language = Lang>,
  {
    builder.start_node(Self::KIND);
    name_parser(builder)
      .then_ignore(Colon::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}

impl<'a, Name, Lang, I, T, Error> Parseable<'a, I, T, Error> for Alias<Name, Lang>
where
  Name: Parseable<'a, I, T, Error, Language = Lang>,
  Colon<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  type Language = Lang;

  #[cfg_attr(not(tarpaulin), inline(always))]
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

/// Represents a field in a GraphQL selection set.
///
/// A field is the basic unit of data that can be requested in GraphQL. Fields can have
/// aliases, arguments, directives, and nested selection sets. This structure represents
/// the complete syntax for a field including all its optional components.
///
/// ## Grammar
///
/// ```text
/// Field : Alias? Name Arguments? Directives? SelectionSet?
/// ```
///
/// Spec: [Fields](https://spec.graphql.org/draft/#sec-Language.Fields)
#[derive(Debug, Clone)]
pub struct Field<Alias, Name, Arguments, Directives, SelectionSet, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  alias: Option<Alias>,
  name: Name,
  arguments: Option<Arguments>,
  directives: Option<Directives>,
  selection_set: Option<SelectionSet>,
}

impl<Alias, Name, Arguments, Directives, SelectionSet, Lang>
  Field<Alias, Name, Arguments, Directives, SelectionSet, Lang>
where
  Lang: Language,
{
  /// Returns the syntax node representing the entire field.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }
}

impl<Alias, Name, Arguments, Directives, SelectionSet, Lang>
  Field<Alias, Name, Arguments, Directives, SelectionSet, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn new(
    syntax: SyntaxNode<Lang>,
    alias: Option<Alias>,
    name: Name,
    arguments: Option<Arguments>,
    directives: Option<Directives>,
    selection_set: Option<SelectionSet>,
  ) -> Self {
    Self {
      syntax,
      alias,
      name,
      arguments,
      directives,
      selection_set,
    }
  }

  /// Tries to create a `Field` from the given syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the source span of the entire field.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the field's alias, if present.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn alias(&self) -> Option<&Alias> {
    self.alias.as_ref()
  }

  /// Returns the field's name.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the field's arguments, if present.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn arguments(&self) -> Option<&Arguments> {
    self.arguments.as_ref()
  }

  /// Returns the field's directives, if present.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns the field's selection set, if present.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn selection_set(&self) -> Option<&SelectionSet> {
    self.selection_set.as_ref()
  }

  /// Creates a parser that can parse a complete field with custom component parsers.
  pub fn parser_with<'a, I, T, Error, E, AP, NP, AliasP, DP, SP>(
    builder: &'a SyntaxTreeBuilder<Lang>,
    alias_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> AliasP,
    name_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> NP,
    args_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> AP,
    directives_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> DP,
    selection_set_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> SP,
  ) -> impl Parser<'a, I, (), E> + Clone
  where
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    AliasP: Parser<'a, I, (), E> + Clone,
    NP: Parser<'a, I, (), E> + Clone,
    AP: Parser<'a, I, (), E> + Clone,
    DP: Parser<'a, I, (), E> + Clone,
    SP: Parser<'a, I, (), E> + Clone,
  {
    builder.start_node(Self::KIND);
    alias_parser(builder)
      .or_not()
      .then(name_parser(builder))
      .then(args_parser(builder).or_not())
      .then(directives_parser(builder).or_not())
      .then(selection_set_parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}

impl<'a, Alias, Name, Arguments, Directives, SelectionSet, Lang, I, T, Error>
  Parseable<'a, I, T, Error> for Field<Alias, Name, Arguments, Directives, SelectionSet, Lang>
where
  Alias: Parseable<'a, I, T, Error, Language = Lang> + 'a,
  Name: Parseable<'a, I, T, Error, Language = Lang> + 'a,
  Arguments: Parseable<'a, I, T, Error, Language = Lang> + 'a,
  Directives: Parseable<'a, I, T, Error, Language = Lang> + 'a,
  SelectionSet: Parseable<'a, I, T, Error, Language = Lang> + 'a,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang> + 'a,
{
  type Language = Lang;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn parser<E>(
    builder: &'a SyntaxTreeBuilder<Self::Language>,
  ) -> impl chumsky::Parser<'a, I, (), E> + Clone
  where
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: LosslessToken<'a>,
    <<T>::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    Error: 'a,
    E: chumsky::extra::ParserExtra<'a, I, Error = Error> + 'a,
  {
    Self::parser_with(
      builder,
      Alias::parser,
      Name::parser,
      Arguments::parser,
      Directives::parser,
      SelectionSet::parser,
    )
  }
}
