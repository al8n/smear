use logosky::{
  Logos, LosslessToken, Source, LogoStream,
  chumsky::{self, Parser},
  cst::{CstElement, CstNode, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, TextRange};

use super::TypeGenerics;
use crate::cst::Path;

/// A GraphQLx fragment type path in CST (e.g., `User<ID, Name>` or `v1::Comment<ID>`).
///
/// Combines a path with optional type generics for fragment definitions.
///
/// ## Example
/// ```text
/// User<ID, Name>
/// v1::Comment<ID>
/// ```
#[derive(Debug, Clone)]
pub struct FragmentTypePath<Ident, Type, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  path: Path<Ident, Lang>,
  generics: Option<TypeGenerics<Type, Lang>>,
}

impl<Ident, Type, Lang> FragmentTypePath<Ident, Type, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    path: Path<Ident, Lang>,
    generics: Option<TypeGenerics<Type, Lang>>,
  ) -> Self {
    Self { syntax, path, generics }
  }

  /// Tries to create a `FragmentTypePath` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the entire fragment type path.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the path component.
  #[inline]
  pub const fn path(&self) -> &Path<Ident, Lang> {
    &self.path
  }

  /// Returns the optional type generics.
  #[inline]
  pub const fn type_generics(&self) -> Option<&TypeGenerics<Type, Lang>> {
    self.generics.as_ref()
  }
}

impl<'a, Ident, Type, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for FragmentTypePath<Ident, Type, Lang>
where
  Path<Ident, Lang>: Parseable<'a, I, T, Error, Language = Lang>,
  TypeGenerics<Type, Lang>: Parseable<'a, I, T, Error, Language = Lang>,
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
    Path::parser(builder)
      .ignore_then(TypeGenerics::parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}
