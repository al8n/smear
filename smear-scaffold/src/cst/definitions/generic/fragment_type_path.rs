use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser},
  cst::{CstNode, CstElement, Parseable, SyntaxTreeBuilder, cast::child},
};
use rowan::{Language, SyntaxNode, TextRange};

use super::TypeGenerics;
use crate::cst::Path;
use core::marker::PhantomData;

/// A GraphQLx fragment type path in CST (e.g., `User<ID, Name>` or `v1::Comment<ID>`).
///
/// Combines a path with optional type generics for fragment definitions.
///
/// ## Example
/// ```text
/// User<ID, Name>
/// v1::Comment<ID>
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FragmentTypePath<Ident, Type, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _ident: PhantomData<Ident>,
  _type: PhantomData<Type>,
}

impl<Ident, Type, Lang> FragmentTypePath<Ident, Type, Lang>
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

  /// Tries to create a `FragmentTypePath` from the given syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, logosky::cst::error::CstNodeMismatch<Self>> {
    Self::try_cast(syntax)
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
  pub fn path(&self) -> Path<Ident, Lang>
  where
    Path<Ident, Lang>: CstNode<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }

  /// Returns the optional type generics.
  #[inline]
  pub fn type_generics(&self) -> Option<TypeGenerics<Type, Lang>>
  where
    TypeGenerics<Type, Lang>: CstNode<Language = Lang>,
  {
    child(self.syntax())
  }
}

impl<'a, Ident, Type, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for FragmentTypePath<Ident, Type, Lang>
where
  Path<Ident, Lang>: Parseable<'a, I, T, Error, Language = Lang>,
  TypeGenerics<Type, Lang>: Parseable<'a, I, T, Error, Language = Lang>,
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
      .ignore_then(TypeGenerics::parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}
