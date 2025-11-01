use logosky::{
  Logos, LosslessToken, Source, LogoStream,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, CstNodeChildren, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

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
  ident: Ident,
  equal: Option<Equal<TextRange, SyntaxToken<Lang>>>,
  default_ty: Option<Type>,
}

impl<Ident, Type, Lang> DefinitionTypeParam<Ident, Type, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    ident: Ident,
    equal: Option<Equal<TextRange, SyntaxToken<Lang>>>,
    default_ty: Option<Type>,
  ) -> Self {
    Self {
      syntax,
      ident,
      equal,
      default_ty,
    }
  }

  /// Tries to create a `DefinitionTypeParam` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
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
  pub const fn ident(&self) -> &Ident {
    &self.ident
  }

  /// Returns the optional default type of the type parameter.
  #[inline]
  pub const fn default(&self) -> Option<&Type> {
    self.default_ty.as_ref()
  }

  /// Returns the optional equals token.
  #[inline]
  pub const fn equal_token(&self) -> Option<&Equal<TextRange, SyntaxToken<Lang>>> {
    self.equal.as_ref()
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
  Self: CstNode<Lang>,
{
  type Language = Lang;

  #[inline]
  fn parser<E>(builder: &'a SyntaxTreeBuilder<Self::Language>) -> impl Parser<'a, I, (), E> + Clone
  where
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
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
#[derive(Debug, Clone)]
pub struct DefinitionTypeGenerics<Ident, Type, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  l_angle: LAngle<TextRange, SyntaxToken<Lang>>,
  params: CstNodeChildren<DefinitionTypeParam<Ident, Type, Lang>, Lang>,
  r_angle: RAngle<TextRange, SyntaxToken<Lang>>,
}

impl<Ident, Type, Lang> DefinitionTypeGenerics<Ident, Type, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    l_angle: LAngle<TextRange, SyntaxToken<Lang>>,
    params: CstNodeChildren<DefinitionTypeParam<Ident, Type, Lang>, Lang>,
    r_angle: RAngle<TextRange, SyntaxToken<Lang>>,
  ) -> Self {
    Self {
      syntax,
      l_angle,
      params,
      r_angle,
    }
  }

  /// Tries to create a `DefinitionTypeGenerics` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
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
  pub const fn l_angle_token(&self) -> &LAngle<TextRange, SyntaxToken<Lang>> {
    &self.l_angle
  }

  /// Returns the type parameters.
  #[inline]
  pub const fn params(
    &self,
  ) -> &CstNodeChildren<DefinitionTypeParam<Ident, Type, Lang>, Lang> {
    &self.params
  }

  /// Returns the right angle bracket token.
  #[inline]
  pub const fn r_angle_token(&self) -> &RAngle<TextRange, SyntaxToken<Lang>> {
    &self.r_angle
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
  Self: CstNode<Lang>,
{
  type Language = Lang;

  #[inline]
  fn parser<E>(builder: &'a SyntaxTreeBuilder<Self::Language>) -> impl Parser<'a, I, (), E> + Clone
  where
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
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
