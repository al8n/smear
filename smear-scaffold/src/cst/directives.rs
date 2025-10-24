use core::fmt::Debug;
use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser, extra::ParserExtra},
  cst::CstNodeChildren,
};

use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};
use smear_lexer::punctuator::At;

use super::{CstElement, CstNode, Parseable, SyntaxTreeBuilder, error::SyntaxError};

/// Represents a single directive in a GraphQL-style syntax.
///
/// A directive consists of an `@` symbol followed by a name and optional arguments.
/// For example: `@deprecated`, `@include(if: true)`, `@customDirective(arg1: "value", arg2: 42)`
///
/// Spec: [Directive](https://spec.graphql.org/draft/#Directive)
#[derive(Debug, Clone)]
pub struct Directive<Name, Args, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  at: At<TextRange, SyntaxToken<Lang>>,
  name: Name,
  arguments: Option<Args>,
}

impl<Name, Args, Lang> Directive<Name, Args, Lang>
where
  Lang: Language,
{
  /// Returns the syntax node of this directive.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }
}

impl<Name, Args, Lang> Directive<Name, Args, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn new(
    syntax: SyntaxNode<Lang>,
    at: At<TextRange, SyntaxToken<Lang>>,
    name: Name,
    arguments: Option<Args>,
  ) -> Self {
    Self {
      syntax,
      at,
      name,
      arguments,
    }
  }

  /// Tries to create a `Directive` from the given syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the at symbol token, if present.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn at(&self) -> &At<TextRange, SyntaxToken<Lang>> {
    &self.at
  }

  /// Returns the directive name.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the optional arguments.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn arguments(&self) -> Option<&Args> {
    self.arguments.as_ref()
  }

  /// Creates a parser for a directive using the provided name and arguments parsers.
  ///
  /// The `name_parser` is used to parse the directive's name, while the `args_parser`
  /// is used to parse the optional arguments.
  ///
  /// The resulting parser will recognize the `@` symbol, followed by the name and optional arguments,
  /// constructing a `Directive` instance with the parsed components.
  pub fn parse<'a, I, T, Error, E, NP, AP>(
    builder: &'a SyntaxTreeBuilder<Lang>,
    name_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> NP,
    args_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> AP,
  ) -> impl Parser<'a, I, (), E> + Clone
  where
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    At<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    NP: Parser<'a, I, (), E> + Clone,
    AP: Parser<'a, I, (), E> + Clone,
    Self: CstNode<Language = Lang>,
  {
    builder.start_node(Self::KIND);
    At::parser(builder)
      .then(name_parser(builder))
      .then(args_parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}

impl<'a, Name, Args, Lang, I, T, Error> Parseable<'a, I, T, Error> for Directive<Name, Args, Lang>
where
  Args: Parseable<'a, I, T, Error, Language = Lang> + 'a,
  At<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Name: Parseable<'a, I, T, Error, Language = Lang> + 'a,
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
    Self::parse(builder, Name::parser, Args::parser)
  }
}

/// Represents a collection of one or more directives.
///
/// In GraphQL and similar languages, multiple directives can be applied to a single element.
/// This structure represents such a collection, maintaining the source span information
/// and providing access to all individual directives.
///
/// Spec: [Directives](https://spec.graphql.org/draft/#Directives)
#[derive(Debug, Clone)]
pub struct Directives<Directive, Lang>
where
  Lang: Language,
  Directive: CstNode<Language = Lang>,
{
  syntax: SyntaxNode<Lang>,
  directives: CstNodeChildren<Directive>,
}

impl<Directive, Lang> Directives<Directive, Lang>
where
  Lang: Language,
  Directive: CstNode<Language = Lang>,
{
  /// Returns the syntax node of this directives collection.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }
}

impl<Directive, Lang> Directives<Directive, Lang>
where
  Lang: Language,
  Directive: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn new(
    syntax: SyntaxNode<Lang>,
    directives: CstNodeChildren<Directive>,
  ) -> Self {
    Self { syntax, directives }
  }

  /// Tries to create a `Directives` from the given syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self>>
  where
    Self: CstNode<Language = Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the collection of directives.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn directives(&self) -> &CstNodeChildren<Directive> {
    &self.directives
  }

  /// Creates a parser for a collection of directives using the provided directive parser.
  pub fn parse<'a, I, T, Error, E, DP>(
    builder: &'a SyntaxTreeBuilder<Lang>,
    directive_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> DP,
  ) -> impl Parser<'a, I, (), E> + Clone
  where
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    DP: Parser<'a, I, (), E> + Clone,
    Self: CstNode<Language = Lang>,
    Lang::Kind: Into<rowan::SyntaxKind>,
  {
    builder.start_node(Self::KIND);
    directive_parser(builder).repeated().at_least(1).map(|_| {
      builder.finish_node();
    })
  }
}

impl<'a, Directive, Lang, I, T, Error> Parseable<'a, I, T, Error> for Directives<Directive, Lang>
where
  Directive: Parseable<'a, I, T, Error, Language = Lang> + CstNode<Language = Lang> + 'a,
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
    Self::parse(builder, Directive::parser)
  }
}
