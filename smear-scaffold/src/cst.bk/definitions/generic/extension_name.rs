use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser},
  cst::{CstElement, CstNode, Parseable, SyntaxTreeBuilder},
};
use rowan::{Language, SyntaxNode, TextRange};

use crate::cst::Path;

use super::ExtensionTypeGenerics;

/// An extension name in CST (e.g., for type extensions with generics).
///
/// ## Example
/// ```text
/// extend type User<ID>
/// ```
#[derive(Debug, Clone)]
pub struct ExtensionName<Ident, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  path: Path<Ident, Lang>,
  generics: Option<ExtensionTypeGenerics<Ident, Lang>>,
}

impl<Ident, Lang> ExtensionName<Ident, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    path: Path<Ident, Lang>,
    generics: Option<ExtensionTypeGenerics<Ident, Lang>>,
  ) -> Self {
    Self { syntax, path, generics }
  }

  /// Tries to create an `ExtensionName` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, logosky::cst::error::SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the entire extension name.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the path.
  #[inline]
  pub const fn path(&self) -> &Path<Ident, Lang> {
    &self.path
  }

  /// Returns the optional type generics.
  #[inline]
  pub const fn generics(
    &self,
  ) -> Option<&ExtensionTypeGenerics<Ident, Lang>> {
    self.generics.as_ref()
  }
}

impl<'a, Ident, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for ExtensionName<Ident, Lang>
where
  Ident: Parseable<'a, I, T, Error, Language = Lang>,
  ExtensionTypeGenerics<Ident, Lang>: Parseable<'a, I, T, Error, Language = Lang>,
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
      .ignore_then(ExtensionTypeGenerics::parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}
