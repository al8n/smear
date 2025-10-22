use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, Parseable, SyntaxTreeBuilder, cast::children, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, TextRange};

use core::marker::PhantomData;

/// A document consisting of a series of definitions.
///
/// This is the top-level container for GraphQL documents. It holds a collection of definitions
/// (type system definitions, executable definitions, or both) and tracks the source span.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Document<Definition, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _definition: PhantomData<Definition>,
}

impl<Definition, Lang> Document<Definition, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _definition: PhantomData,
    }
  }

  /// Tries to create a `Document` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self>> {
    Self::try_cast_node(syntax)
  }

  /// Returns a reference to the span covering the entire document.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the definitions in the document.
  #[inline]
  pub fn definitions(&self) -> logosky::cst::CstNodeChildren<Definition>
  where
    Definition: CstNode<Language = Lang>,
  {
    children(self.syntax())
  }
}

impl<'a, Definition, Lang, I, Token, Error> Parseable<'a, I, Token, Error>
  for Document<Definition, Lang>
where
  Definition: Parseable<'a, I, Token, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  type Language = Lang;

  #[inline]
  fn parser<E>(builder: &'a SyntaxTreeBuilder<Self::Language>) -> impl Parser<'a, I, (), E> + Clone
  where
    I: Tokenizer<'a, Token, Slice = <<<Token>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Token: LosslessToken<'a>,
    <<Token>::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    builder.start_node(Self::KIND);
    Definition::parser(builder)
      .repeated()
      .at_least(1)
      .ignored()
      .map(|_| {
        builder.finish_node();
      })
  }
}
