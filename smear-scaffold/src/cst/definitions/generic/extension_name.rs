use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser},
  cst::{Node, Parseable, SyntaxTreeBuilder, cast::child},
};
use rowan::{Language, SyntaxNode, TextRange};

use super::ExtensionTypeGenerics;
use core::marker::PhantomData;

/// An extension name in CST (e.g., for type extensions with generics).
///
/// ## Example
/// ```text
/// extend type User<ID>
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtensionName<Ident, Type, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _ident: PhantomData<Ident>,
  _type: PhantomData<Type>,
}

impl<Ident, Type, Lang> ExtensionName<Ident, Type, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: Node<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _ident: PhantomData,
      _type: PhantomData,
    }
  }

  /// Tries to create an `ExtensionName` from the given syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, logosky::cst::error::SyntaxNodeMismatch<Self>> {
    Self::try_cast(syntax)
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

  /// Returns the name identifier.
  #[inline]
  pub fn name(&self) -> Ident
  where
    Ident: Node<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }

  /// Returns the optional type generics.
  #[inline]
  pub fn generics(&self) -> Option<ExtensionTypeGenerics<Ident, Type, Lang>>
  where
    ExtensionTypeGenerics<Ident, Type, Lang>: Node<Language = Lang>,
  {
    child(self.syntax())
  }
}

impl<'a, Ident, Type, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for ExtensionName<Ident, Type, Lang>
where
  Ident: Parseable<'a, I, T, Error, Language = Lang>,
  ExtensionTypeGenerics<Ident, Type, Lang>: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: Node<Language = Lang>,
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
