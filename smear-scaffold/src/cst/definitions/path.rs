use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstNode, CstElement, Parseable, SyntaxTreeBuilder, error::CstNodeMismatch, cast::children},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::marker::PhantomData;
use smear_lexer::punctuator::PathSeparator;

/// A GraphQLx path in CST, which is a sequence of identifiers separated by `::`.
///
/// ## Example
/// ```text
/// user::profile
/// ::std::string
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path<Ident, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _ident: PhantomData<Ident>,
}

impl<Ident, Lang> Path<Ident, Lang>
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
    }
  }

  /// Tries to create a `Path` from the given syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, CstNodeMismatch<Self>> {
    Self::try_cast(syntax)
  }

  /// Returns the span covering the entire path.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns whether the path is fully qualified (starts with `::` path separator).
  #[inline]
  pub fn is_fully_qualified(&self) -> bool
  where
    PathSeparator<TextRange, SyntaxToken<Lang>>: CstNode<Language = Lang>,
  {
    // Check if first child is a PathSeparator token
    self
      .syntax()
      .children_with_tokens()
      .find_map(|element| element.into_token())
      .and_then(|token| {
        if PathSeparator::<TextRange, SyntaxToken<Lang>>::can_cast(token.kind()) {
          Some(())
        } else {
          None
        }
      })
      .is_some()
  }

  /// Returns the path segments as children.
  #[inline]
  pub fn segments(&self) -> logosky::cst::SyntaxNodeChildren<Ident>
  where
    Ident: CstNode<Language = Lang>,
  {
    children(self.syntax())
  }
}

impl<'a, Ident, Lang, I, T, Error> Parseable<'a, I, T, Error> for Path<Ident, Lang>
where
  Ident: Parseable<'a, I, T, Error, Language = Lang>,
  PathSeparator<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  type Language = Lang;

  #[inline]
  fn parser<E>(builder: &'a SyntaxTreeBuilder<Self::Language>) -> impl Parser<'a, I, (), E> + Clone
  where
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    builder.start_node(Self::KIND);
    PathSeparator::parser(builder)
      .or_not()
      .ignore_then(
        Ident::parser(builder)
          .separated_by(PathSeparator::parser(builder))
          .at_least(1)
          .ignored(),
      )
      .map(|_| {
        builder.finish_node();
      })
  }
}
