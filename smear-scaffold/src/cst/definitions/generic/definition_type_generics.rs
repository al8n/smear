use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstNode, CstToken, CstElement, Parseable, SyntaxTreeBuilder, cast::children},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::marker::PhantomData;
use smear_lexer::punctuator::{Equal, LAngle, RAngle};

/// A definition type parameter with an optional default type in CST.
///
/// ## Examples
/// ```text
/// T = String  # Type parameter `T` with default type `String`
/// T           # Type parameter `T` without default type
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefinitionTypeParam<Ident, Type, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _ident: PhantomData<Ident>,
  _type: PhantomData<Type>,
}

impl<Ident, Type, Lang> DefinitionTypeParam<Ident, Type, Lang>
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

  /// Tries to create a `DefinitionTypeParam` from the given syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, logosky::cst::error::CastNodeError<Self>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the entire type parameter.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the identifier of the type parameter.
  #[inline]
  pub fn ident(&self) -> Ident
  where
    Ident: CstNode<Language = Lang>,
  {
    logosky::cst::cast::child(self.syntax()).unwrap()
  }

  /// Returns the optional default type of the type parameter.
  #[inline]
  pub fn default(&self) -> Option<Type>
  where
    Type: CstNode<Language = Lang>,
  {
    logosky::cst::cast::child(self.syntax())
  }

  /// Returns the optional equals token.
  #[inline]
  pub fn equal_token(&self) -> Option<Equal<TextRange, SyntaxToken<Lang>>>
  where
    Equal<TextRange, SyntaxToken<Lang>>: CstToken<Language = Lang>,
  {
    logosky::cst::cast::token(self.syntax(), &Equal::KIND)
      .map(|t| Equal::with_content(t.text_range(), t))
  }
}

impl<'a, Ident, Type, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for DefinitionTypeParam<Ident, Type, Lang>
where
  Ident: Parseable<'a, I, T, Error, Language = Lang>,
  Type: Parseable<'a, I, T, Error, Language = Lang>,
  Equal<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
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
    Ident::parser(builder)
      .ignore_then(
        Equal::parser(builder)
          .ignore_then(Type::parser(builder))
          .or_not(),
      )
      .map(|_| {
        builder.finish_node();
      })
  }
}

/// A definition type generics with a list of type parameters in CST.
///
/// ## Examples
/// ```text
/// <T, U = String>  # Type generics with two parameters
/// <T, U>           # Type generics without defaults
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefinitionTypeGenerics<Ident, Type, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _ident: PhantomData<Ident>,
  _type: PhantomData<Type>,
}

impl<Ident, Type, Lang> DefinitionTypeGenerics<Ident, Type, Lang>
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

  /// Tries to create a `DefinitionTypeGenerics` from the given syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, logosky::cst::error::CastNodeError<Self>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the entire type generics.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the left angle bracket token.
  #[inline]
  pub fn l_angle_token(&self) -> LAngle<TextRange, SyntaxToken<Lang>>
  where
    LAngle<TextRange, SyntaxToken<Lang>>: CstToken<Language = Lang>,
  {
    logosky::cst::cast::token(self.syntax(), &LAngle::KIND)
      .map(|t| LAngle::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the type parameters.
  #[inline]
  pub fn params(&self) -> logosky::cst::SyntaxNodeChildren<DefinitionTypeParam<Ident, Type, Lang>>
  where
    DefinitionTypeParam<Ident, Type, Lang>: CstNode<Language = Lang>,
  {
    children(self.syntax())
  }

  /// Returns the right angle bracket token.
  #[inline]
  pub fn r_angle_token(&self) -> RAngle<TextRange, SyntaxToken<Lang>>
  where
    RAngle<TextRange, SyntaxToken<Lang>>: CstToken<Language = Lang>,
  {
    logosky::cst::cast::token(self.syntax(), &RAngle::KIND)
      .map(|t| RAngle::with_content(t.text_range(), t))
      .unwrap()
  }
}

impl<'a, Ident, Type, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for DefinitionTypeGenerics<Ident, Type, Lang>
where
  DefinitionTypeParam<Ident, Type, Lang>: Parseable<'a, I, T, Error, Language = Lang>,
  LAngle<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RAngle<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
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
    LAngle::parser(builder)
      .ignore_then(
        DefinitionTypeParam::parser(builder)
          .repeated()
          .at_least(1)
          .ignored(),
      )
      .then_ignore(RAngle::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}
