use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser},
  cst::{CstElement, CstNode, CstToken, Parseable, SyntaxTreeBuilder, cast::child},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::marker::PhantomData;
use smear_lexer::punctuator::{Bang, LBracket, RBracket};

/// Represents a GraphQL list type with optional non-null modifier in CST.
///
/// ## Grammar
/// ```text
/// ListType : [ Type ] !?
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ListType<Type, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _ty: PhantomData<Type>,
}

impl<Type, Lang> ListType<Type, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _ty: PhantomData,
    }
  }

  /// Tries to create a new `ListType` from a syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, logosky::cst::error::SyntaxError<Self>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the entire list type.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the left bracket token.
  #[inline]
  pub fn l_bracket_token(&self) -> LBracket<TextRange, SyntaxToken<Lang>>
  where
    LBracket<TextRange, SyntaxToken<Lang>>: CstToken<Language = Lang>,
  {
    logosky::cst::cast::token(self.syntax(), &LBracket::KIND)
      .map(|t| LBracket::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the element type.
  #[inline]
  pub fn ty(&self) -> Type
  where
    Type: CstNode<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }

  /// Returns the right bracket token.
  #[inline]
  pub fn r_bracket_token(&self) -> RBracket<TextRange, SyntaxToken<Lang>>
  where
    RBracket<TextRange, SyntaxToken<Lang>>: CstToken<Language = Lang>,
  {
    logosky::cst::cast::token(self.syntax(), &RBracket::KIND)
      .map(|t| RBracket::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the bang token if present.
  #[inline]
  pub fn bang_token(&self) -> Option<Bang<TextRange, SyntaxToken<Lang>>>
  where
    Bang<TextRange, SyntaxToken<Lang>>: CstToken<Language = Lang>,
  {
    logosky::cst::cast::token(self.syntax(), &Bang::KIND)
      .map(|t| Bang::with_content(t.text_range(), t))
  }

  /// Returns whether this type is required (non-null).
  #[inline]
  pub fn required(&self) -> bool
  where
    Bang<TextRange, SyntaxToken<Lang>>: CstToken<Language = Lang>,
  {
    self.bang_token().is_some()
  }
}

impl<'a, Type, Lang, I, T, Error> Parseable<'a, I, T, Error> for ListType<Type, Lang>
where
  Type: Parseable<'a, I, T, Error, Language = Lang>,
  LBracket<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RBracket<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Bang<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
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
    LBracket::parser(builder)
      .ignore_then(Type::parser(builder))
      .then_ignore(RBracket::parser(builder))
      .ignore_then(Bang::parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}
