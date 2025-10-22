use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser},
  cst::{
    CstElement, CstNode, CstToken, Parseable, SyntaxTreeBuilder, cast::child, error::SyntaxError,
  },
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::marker::PhantomData;
use smear_lexer::{
  keywords::Set,
  punctuator::{Bang, LBrace, RBrace},
};

/// Represents a GraphQLx set type with optional non-null modifier in CST.
///
/// ## Grammar
/// ```text
/// SetType : set { Type } !?
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SetType<Type, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _ty: PhantomData<Type>,
}

impl<Type, Lang> SetType<Type, Lang>
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

  /// Tries to create a new `SetType` from a syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the entire set type.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the set keyword token.
  #[inline]
  pub fn set_keyword(&self) -> Set<TextRange, SyntaxToken<Lang>>
  where
    Set<TextRange, SyntaxToken<Lang>>: CstToken<Language = Lang>,
  {
    logosky::cst::cast::token(self.syntax(), &Set::KIND)
      .map(|t| Set::with_content(t.text_range(), t))
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

impl<'a, Type, Lang, I, T, Error> Parseable<'a, I, T, Error> for SetType<Type, Lang>
where
  Type: Parseable<'a, I, T, Error, Language = Lang>,
  Set<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  LBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
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
    Set::parser(builder)
      .ignore_then(LBrace::parser(builder))
      .ignore_then(Type::parser(builder))
      .then_ignore(RBrace::parser(builder))
      .ignore_then(Bang::parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}
