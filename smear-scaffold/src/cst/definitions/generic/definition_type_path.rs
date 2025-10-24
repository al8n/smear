use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser},
  cst::{CstElement, CstNode, Parseable, SyntaxTreeBuilder},
};
use rowan::{Language, SyntaxNode, TextRange};

use super::DefinitionName;
use crate::cst::{Path, PathSegment, generic::TypeGenerics};

/// A GraphQLx definition type path in CST.
///
/// Combines a path with a definition name (which may have type generics).
///
/// ## Example
/// ```text
/// User<ID, Name>
/// ```
#[derive(Debug, Clone)]
pub struct DefinitionTypePath<Ident, Type, Lang>
where
  Lang: Language,
  PathSegment<Ident, Lang>: CstNode<Lang>,
{
  syntax: SyntaxNode<Lang>,
  path: Path<Ident, Lang>,
  generics: Option<TypeGenerics<Type, Lang>>,
  required: bool,
}

impl<Ident, Type, Lang> DefinitionTypePath<Ident, Type, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    path: Option<Path<Ident, Lang>>,
    name: DefinitionName<Ident, Type, Lang>,
  ) -> Self {
    Self { syntax, path, name }
  }

  /// Tries to create a `DefinitionTypePath` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, logosky::cst::error::SyntaxError<Self, Lang>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the entire definition type path.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the optional path prefix.
  #[inline]
  pub const fn path(&self) -> Option<&Path<Ident, Lang>> {
    self.path.as_ref()
  }

  /// Returns the definition name (with optional generics).
  #[inline]
  pub const fn name(&self) -> &DefinitionName<Ident, Type, Lang> {
    &self.name
  }
}

impl<'a, Ident, Type, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for DefinitionTypePath<Ident, Type, Lang>
where
  Path<Ident, Lang>: Parseable<'a, I, T, Error, Language = Lang>,
  DefinitionName<Ident, Type, Lang>: Parseable<'a, I, T, Error, Language = Lang>,
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
    Path::parser(builder)
      .or_not()
      .ignore_then(DefinitionName::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}
