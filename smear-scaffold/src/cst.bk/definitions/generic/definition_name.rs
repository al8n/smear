use logosky::{
  Logos, LosslessToken, Source, LogoStream,
  chumsky::{self, Parser},
  cst::{CstElement, CstNode, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, TextRange};

use super::DefinitionTypeGenerics;

/// The CST for a definition name (e.g., `User<ID, Name = String>`).
///
/// In the example `User<ID, Name = String>`, `User` is the identifier,
/// and `<ID, Name = String>` are the type generics.
#[derive(Debug, Clone)]
pub struct DefinitionName<Ident, Type, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  name: Ident,
  generics: Option<DefinitionTypeGenerics<Ident, Type, Lang>>,
}

impl<Ident, Type, Lang> DefinitionName<Ident, Type, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    name: Ident,
    generics: Option<DefinitionTypeGenerics<Ident, Type, Lang>>,
  ) -> Self {
    Self { syntax, name, generics }
  }

  /// Tries to create a `DefinitionName` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the entire definition name.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the name identifier of the definition.
  #[inline]
  pub const fn name(&self) -> &Ident {
    &self.name
  }

  /// Returns the optional type generics of the definition name.
  #[inline]
  pub const fn generics(
    &self,
  ) -> Option<&DefinitionTypeGenerics<Ident, Type, Lang>> {
    self.generics.as_ref()
  }
}

impl<'a, Ident, Type, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for DefinitionName<Ident, Type, Lang>
where
  Ident: Parseable<'a, I, T, Error, Language = Lang>,
  DefinitionTypeGenerics<Ident, Type, Lang>: Parseable<'a, I, T, Error, Language = Lang>,
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
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    Error: 'a,
    E: chumsky::extra::ParserExtra<'a, I, Error = Error> + 'a,
  {
    builder.start_node(Self::KIND);
    Ident::parser(builder)
      .ignore_then(DefinitionTypeGenerics::parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}
