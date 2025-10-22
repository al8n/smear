use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser},
  cst::{CstNode, CstElement, Parseable, SyntaxTreeBuilder, cast::child},
};
use rowan::{Language, SyntaxNode, TextRange};

use super::DefinitionTypeGenerics;
use core::marker::PhantomData;

/// The CST for a definition name (e.g., `User<ID, Name = String>`).
///
/// In the example `User<ID, Name = String>`, `User` is the identifier,
/// and `<ID, Name = String>` are the type generics.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefinitionName<Ident, Type, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _ident: PhantomData<Ident>,
  _type: PhantomData<Type>,
}

impl<Ident, Type, Lang> DefinitionName<Ident, Type, Lang>
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

  /// Tries to create a `DefinitionName` from the given syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, logosky::cst::error::CastNodeError<Self>> {
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
  pub fn name(&self) -> Ident
  where
    Ident: CstNode<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }

  /// Returns the optional type generics of the definition name.
  #[inline]
  pub fn generics(&self) -> Option<DefinitionTypeGenerics<Ident, Type, Lang>>
  where
    DefinitionTypeGenerics<Ident, Type, Lang>: CstNode<Language = Lang>,
  {
    child(self.syntax())
  }
}

impl<'a, Ident, Type, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for DefinitionName<Ident, Type, Lang>
where
  Ident: Parseable<'a, I, T, Error, Language = Lang>,
  DefinitionTypeGenerics<Ident, Type, Lang>: Parseable<'a, I, T, Error, Language = Lang>,
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
    Ident::parser(builder)
      .ignore_then(DefinitionTypeGenerics::parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}
