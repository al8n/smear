use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser},
  cst::{CstElement, CstNode, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, TextRange};

use super::ExecutableDefinitionTypeGenerics;

/// An executable definition name in CST (e.g., operation or fragment name with generics).
///
/// ## Example
/// ```text
/// GetUser<ID, Name>
/// ```
#[derive(Debug, Clone)]
pub struct ExecutableDefinitionName<Ident, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  name: Ident,
  generics: Option<ExecutableDefinitionTypeGenerics<Ident, Lang>>,
}

impl<Ident, Lang> ExecutableDefinitionName<Ident, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    name: Ident,
    generics: Option<ExecutableDefinitionTypeGenerics<Ident, Lang>>,
  ) -> Self {
    Self { syntax, name, generics }
  }

  /// Tries to create an `ExecutableDefinitionName` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the entire executable definition name.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the name identifier.
  #[inline]
  pub const fn name(&self) -> &Ident {
    &self.name
  }

  /// Returns the optional type generics.
  #[inline]
  pub const fn generics(
    &self,
  ) -> Option<&ExecutableDefinitionTypeGenerics<Ident, Lang>> {
    self.generics.as_ref()
  }
}

impl<'a, Ident, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for ExecutableDefinitionName<Ident, Lang>
where
  Ident: Parseable<'a, I, T, Error, Language = Lang>,
  ExecutableDefinitionTypeGenerics<Ident, Lang>: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
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
    builder.start_node(Self::KIND);
    Ident::parser(builder)
      .ignore_then(ExecutableDefinitionTypeGenerics::parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}
