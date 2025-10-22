use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser},
  cst::{CstElement, CstNode, Parseable, SyntaxTreeBuilder, cast::child, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, TextRange};

use super::TypeGenerics;
use core::marker::PhantomData;

/// A GraphQLx type path in CST (e.g., `User<ID, Name>` or `v1::Comment<ID>`).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypePath<Path, Type, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _path: PhantomData<Path>,
  _type: PhantomData<Type>,
}

impl<Path, Type, Lang> TypePath<Path, Type, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _path: PhantomData,
      _type: PhantomData,
    }
  }

  /// Tries to create a new `TypePath` from a syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the entire type path.
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
  pub fn path(&self) -> Path
  where
    Path: CstNode<Language = Lang>,
  {
    logosky::cst::cast::child(self.syntax()).unwrap()
  }

  /// Returns the type generics, if present.
  #[inline]
  pub fn type_generics(&self) -> Option<TypeGenerics<Type, Lang>>
  where
    TypeGenerics<Type, Lang>: CstNode<Language = Lang>,
  {
    child(self.syntax())
  }
}

impl<'a, Path, Type, Lang, I, T, Error> Parseable<'a, I, T, Error> for TypePath<Path, Type, Lang>
where
  Path: Parseable<'a, I, T, Error, Language = Lang>,
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
