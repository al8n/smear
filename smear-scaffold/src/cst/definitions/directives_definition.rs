use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, CstNodeChildren, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, TextRange};

use core::fmt::Debug;

/// Represents a directives definition in GraphQL schema.
#[derive(Debug, Clone)]
pub struct DirectivesDefinition<Directive, Lang>
where
  Lang: Language,
  Directive: CstNode<Language = Lang>,
{
  syntax: SyntaxNode<Lang>,
  directives: CstNodeChildren<Directive>,
}

impl<Directive, Lang> DirectivesDefinition<Directive, Lang>
where
  Lang: Language,
  Directive: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    directives: CstNodeChildren<Directive>,
  ) -> Self {
    Self { syntax, directives }
  }

  /// Tries to create a `DirectivesDefinition` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self>>
  where
    Self: CstNode<Language = Lang>,
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

  /// Returns the collection of directives.
  #[inline]
  pub const fn directives(&self) -> &CstNodeChildren<Directive>
  {
    &self.directives
  }
}

impl<'a, Directive, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for DirectivesDefinition<Directive, Lang>
where
  Directive: Parseable<'a, I, T, Error, Language = Lang> + CstNode<Language = Lang>,
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
    Directive::parser(builder)
      .repeated()
      .at_least(1)
      .ignored()
      .map(|_| {
        builder.finish_node();
      })
  }
}
