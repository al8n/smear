use core::marker::PhantomData;
use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser, extra::ParserExtra},
  cst::cast::children,
};

use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};
use smear_lexer::punctuator::At;

use super::{
  CstNode, CstToken, CstElement, Parseable, SyntaxTreeBuilder, error::CastNodeError,
  cast::{child, token},
};

/// Represents a single directive in a GraphQL-style syntax.
///
/// A directive consists of an `@` symbol followed by a name and optional arguments.
/// For example: `@deprecated`, `@include(if: true)`, `@customDirective(arg1: "value", arg2: 42)`
///
/// Spec: [Directive](https://spec.graphql.org/draft/#Directive)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Directive<Name, Args, Lang>
where
  Lang: Language,
{
  node: SyntaxNode<Lang>,
  _name: PhantomData<Name>,
  _args: PhantomData<Args>,
}

impl<Name, Args, Lang> Directive<Name, Args, Lang>
where
  Lang: Language,
{
  /// Returns the syntax node of this directive.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.node
  }
}

impl<Name, Args, Lang> Directive<Name, Args, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      node: syntax,
      _name: PhantomData,
      _args: PhantomData,
    }
  }

  /// Tries to create a `Directive` from the given syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, CastNodeError<Self>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the at symbol token, if present.
  #[inline]
  pub fn at(&self) -> Option<At<TextRange, SyntaxToken<Lang>>>
  where
    At<TextRange, SyntaxToken<Lang>>: CstToken<Language = Lang>,
  {
    token(self.syntax(), &At::KIND).map(|t| At::with_content(t.text_range(), t))
  }

  /// Returns the directive name.
  #[inline]
  pub fn name(&self) -> Name
  where
    Name: CstNode<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }

  /// Returns the optional arguments.
  #[inline]
  pub fn arguments(&self) -> Option<Args>
  where
    Args: CstNode<Language = Lang>,
  {
    child(self.syntax())
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

  #[inline]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Directives<Directive, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _directive: PhantomData<Directive>,
}

impl<Directive, Lang> Directives<Directive, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
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
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _directive: PhantomData,
    }
  }

  /// Tries to create a `Directives` from the given syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, CastNodeError<Self>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the collection of directives.
  #[inline]
  pub fn directives(&self) -> logosky::cst::SyntaxNodeChildren<Directive>
  where
    Directive: CstNode<Language = Lang>,
  {
    children::<Directive>(self.syntax())
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
  {
    builder.start_node(Self::KIND);
    directive_parser(builder).repeated().at_least(1).map(|_| {
      builder.finish_node();
    })
  }
}

impl<'a, Directive, Lang, I, T, Error> Parseable<'a, I, T, Error> for Directives<Directive, Lang>
where
  Directive: Parseable<'a, I, T, Error, Language = Lang> + 'a,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang> + 'a,
{
  type Language = Lang;

  #[inline]
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
