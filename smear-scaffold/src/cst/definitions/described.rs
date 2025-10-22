use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstNode, CstElement, Parseable, SyntaxTreeBuilder, cast::child, error::CastNodeError},
};
use rowan::{Language, SyntaxNode, TextRange};

use core::marker::PhantomData;

/// A node with an optional description.
///
/// In GraphQL, many definitions can have an optional description string that provides
/// documentation. This wrapper type represents any node that may be preceded by a description.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Described<T, Description, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _node: PhantomData<T>,
  _description: PhantomData<Description>,
}

impl<T, Description, Lang> Described<T, Description, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _node: PhantomData,
      _description: PhantomData,
    }
  }

  /// Tries to create a `Described` from the given syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, CastNodeError<Self>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the span of the described node.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the description of the described node, if any.
  #[inline]
  pub fn description(&self) -> Option<Description>
  where
    Description: CstNode<Language = Lang>,
  {
    child(self.syntax())
  }

  /// Returns the inner node.
  #[inline]
  pub fn node(&self) -> T
  where
    T: CstNode<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }
}

impl<'a, T, Description, Lang, I, Token, Error> Parseable<'a, I, Token, Error>
  for Described<T, Description, Lang>
where
  T: Parseable<'a, I, Token, Error, Language = Lang>,
  Description: Parseable<'a, I, Token, Error, Language = Lang>,
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
    Description::parser(builder)
      .or_not()
      .then(T::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}
