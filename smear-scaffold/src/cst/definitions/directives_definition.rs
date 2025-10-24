use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, CstNodeChildren, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};
use smear_lexer::{keywords::{self, On}, punctuator::{At, Pipe}};

use core::fmt::Debug;

/// Represents a directive location in GraphQL schema.
#[derive(Debug, Clone)]
pub struct DirectiveLocation<Location, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  pipe: Option<Pipe<TextRange, SyntaxToken<Lang>>>,
  location: Location,
}

impl<Location, Lang> DirectiveLocation<Location, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    pipe: Option<Pipe<TextRange, SyntaxToken<Lang>>>,
    location: Location,
  ) -> Self {
    Self { syntax, pipe, location }
  }

  /// Tries to create a `DirectiveLocation` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the directive location.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the pipe token if present.
  #[inline]
  pub const fn pipe(&self) -> Option<&Pipe<TextRange, SyntaxToken<Lang>>> {
    self.pipe.as_ref()
  }

  /// Returns the directive location.
  #[inline]
  pub const fn location(&self) -> &Location {
    &self.location
  }

  fn parse_leading<'a, I, T, Error, E, P>(
    builder: &'a SyntaxTreeBuilder<Lang>,
    location_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> P,
  ) -> impl Parser<'a, I, (), E> + Clone
  where
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    P: Parser<'a, I, (), E> + Clone,
    Pipe<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    Lang::Kind: Into<rowan::SyntaxKind>,
    Self: CstNode<Lang>,
  {
    builder.start_node(Self::KIND);
    Pipe::parser(builder)
      .or_not()
      .ignore_then(location_parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }

  fn parse_following<'a, I, T, Error, E, P>(
    builder: &'a SyntaxTreeBuilder<Lang>,
    location_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> P,
  ) -> impl Parser<'a, I, (), E> + Clone
  where
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    P: Parser<'a, I, (), E> + Clone,
    Pipe<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    Lang::Kind: Into<rowan::SyntaxKind>,
    Self: CstNode<Lang>,
  {
    builder.start_node(Self::KIND);
    Pipe::parser(builder).ignore_then(location_parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}

#[derive(Debug, Clone)]
pub struct DirectiveLocations<Location, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  locations: CstNodeChildren<DirectiveLocation<Location, Lang>, Lang>,
}

impl<Location, Lang> DirectiveLocations<Location, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    locations: CstNodeChildren<DirectiveLocation<Location, Lang>, Lang>,
  ) -> Self {
    Self { syntax, locations }
  }

  /// Tries to create a `DirectiveLocations` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the directive locations.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the collection of directive locations.
  #[inline]
  pub const fn locations(&self) -> &CstNodeChildren<DirectiveLocation<Location, Lang>, Lang> {
    &self.locations
  }
}

impl<'a, Location, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for DirectiveLocations<Location, Lang>
where
  Location: Parseable<'a, I, T, Error, Language = Lang>,
  DirectiveLocation<Location, Lang>: CstNode<Lang>,
  Pipe<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
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
    DirectiveLocation::parse_leading(builder, Location::parser)
      .ignore_then(DirectiveLocation::parse_following(builder, Location::parser).repeated().ignored())
      .map(|_| {
        builder.finish_node();
      })
  }
}

/// Represents a directives definition in GraphQL schema.
#[derive(Debug, Clone)]
pub struct DirectiveDefinition<Name, Args, Locations, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  directive_keyword: keywords::Directive<TextRange, SyntaxToken<Lang>>,
  at_token: At<TextRange, SyntaxToken<Lang>>,
  name: Name,
  arguments_definition: Option<Args>,
  repeateable: Option<keywords::Repeatable<TextRange, SyntaxToken<Lang>>>,
  on_keyword: Option<keywords::On<TextRange, SyntaxToken<Lang>>>,
  directive_locations: Locations,
}

impl<Name, Args, Locations, Lang> DirectiveDefinition<Name, Args, Locations, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    directive_keyword: keywords::Directive<TextRange, SyntaxToken<Lang>>,
    at_token: At<TextRange, SyntaxToken<Lang>>,
    name: Name,
    arguments_definition: Option<Args>,
    repeateable: Option<keywords::Repeatable<TextRange, SyntaxToken<Lang>>>,
    on_keyword: Option<keywords::On<TextRange, SyntaxToken<Lang>>>,
    directive_locations: Locations,
  ) -> Self {
    Self { syntax, directive_keyword, at_token, name, arguments_definition, repeateable, on_keyword, directive_locations }
  }

  /// Tries to create a `DirectiveDefinition` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering this directives definition.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the directive keyword token.
  #[inline]
  pub const fn directive_keyword(&self) -> &keywords::Directive<TextRange, SyntaxToken<Lang>> {
    &self.directive_keyword
  }

  /// Returns the at symbol token.
  #[inline]
  pub const fn at_token(&self) -> &At<TextRange, SyntaxToken<Lang>> {
    &self.at_token
  }

  /// Returns the name of the directive.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the optional arguments definition.
  #[inline]
  pub const fn arguments_definition(&self) -> Option<&Args> {
    self.arguments_definition.as_ref()
  }

  /// Returns the repeatable keyword token.
  #[inline]
  pub const fn repeatable(&self) -> Option<&keywords::Repeatable<TextRange, SyntaxToken<Lang>>> {
    self.repeateable.as_ref()
  }

  /// Returns the on keyword token.
  #[inline]
  pub const fn on_keyword(&self) -> Option<&keywords::On<TextRange, SyntaxToken<Lang>>> {
    self.on_keyword.as_ref()
  }

  /// Returns the directive locations.
  #[inline]
  pub const fn directive_locations(&self) -> &Locations {
    &self.directive_locations
  }
}

impl<'a, Name, Args, Locations, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for DirectiveDefinition<Name, Args, Locations, Lang>
where
  keywords::Directive<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  keywords::Repeatable<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  At<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  On<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Name: Parseable<'a, I, T, Error, Language = Lang>,
  Args: Parseable<'a, I, T, Error, Language = Lang>,
  Locations: Parseable<'a, I, T, Error, Language = Lang>,
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
    keywords::Directive::parser(builder)
      .then(At::parser(builder))
      .then(Name::parser(builder))
      .then(Args::parser(builder).or_not())
      .then(keywords::Repeatable::parser(builder).or_not())
      .then(keywords::On::parser(builder))
      .then(Locations::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}
