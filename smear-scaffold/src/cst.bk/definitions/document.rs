use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, CstNodeChildren, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, TextRange};

/// A document consisting of a series of definitions.
///
/// This is the top-level container for GraphQL documents. It holds a collection of definitions
/// (type system definitions, executable definitions, or both) and tracks the source span.
#[derive(Debug, Clone)]
pub struct Document<Definition, Lang>
where
  Lang: Language,
  Definition: CstNode<Lang>,
{
  syntax: SyntaxNode<Lang>,
  definitions: CstNodeChildren<Definition, Lang>,
}

impl<Definition, Lang> Document<Definition, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
  Definition: CstNode<Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    definitions: CstNodeChildren<Definition, Lang>,
  ) -> Self {
    Self {
      syntax,
      definitions,
    }
  }

  /// Tries to create a `Document` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>> {
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
  pub const fn definitions(&self) -> &CstNodeChildren<Definition, Lang> {
    &self.definitions
  }
}

impl<'a, Definition, Lang, I, Token, Error> Parseable<'a, I, Token, Error>
  for Document<Definition, Lang>
where
  Definition: Parseable<'a, I, Token, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Definition: CstNode<Lang>,
  Self: CstNode<Lang>,
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
