use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser},
  cst::{CstNode, CstElement, Parseable, SyntaxTreeBuilder, cast::child},
};
use rowan::{Language, SyntaxNode, TextRange};

use super::DefinitionName;
use crate::cst::Path;
use core::marker::PhantomData;

/// A GraphQLx definition type path in CST.
///
/// Combines a path with a definition name (which may have type generics).
///
/// ## Example
/// ```text
/// v1::User<ID, Name>
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefinitionTypePath<Ident, Type, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _ident: PhantomData<Ident>,
  _type: PhantomData<Type>,
}

impl<Ident, Type, Lang> DefinitionTypePath<Ident, Type, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _ident: PhantomData,
      _type: PhantomData,
    }
  }

  /// Tries to create a `DefinitionTypePath` from the given syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, logosky::cst::error::CstNodeMismatch<Self>> {
    Self::try_cast(syntax)
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
  pub fn path(&self) -> Option<Path<Ident, Lang>>
  where
    Path<Ident, Lang>: CstNode<Language = Lang>,
  {
    child(self.syntax())
  }

  /// Returns the definition name (with optional generics).
  #[inline]
  pub fn name(&self) -> DefinitionName<Ident, Type, Lang>
  where
    DefinitionName<Ident, Type, Lang>: CstNode<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }
}

impl<'a, Ident, Type, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for DefinitionTypePath<Ident, Type, Lang>
where
  Path<Ident, Lang>: Parseable<'a, I, T, Error, Language = Lang>,
  DefinitionName<Ident, Type, Lang>: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
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
