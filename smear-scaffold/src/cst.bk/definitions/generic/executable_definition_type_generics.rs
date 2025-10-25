use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, CstNodeChildren, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use smear_lexer::punctuator::{LAngle, RAngle};

/// An executable definition type parameter in CST (without default types).
///
/// ## Example
/// ```text
/// T  # Type parameter without default
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExecutableDefinitionTypeParam<Ident, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  ident: Ident,
}

impl<Ident, Lang> ExecutableDefinitionTypeParam<Ident, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(syntax: SyntaxNode<Lang>, ident: Ident) -> Self {
    Self { syntax, ident }
  }

  /// Tries to create an `ExecutableDefinitionTypeParam` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the type parameter.
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
}

impl<'a, Ident, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for ExecutableDefinitionTypeParam<Ident, Lang>
where
  Ident: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
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
    Ident::parser(builder).map(|_| {
      builder.finish_node();
    })
  }
}

/// Executable definition type generics in CST (e.g., `<T, U>`).
///
/// Similar to definition type generics but for executable definitions (operations/fragments).
#[derive(Debug, Clone)]
pub struct ExecutableDefinitionTypeGenerics<Ident, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  l_angle: LAngle<TextRange, SyntaxToken<Lang>>,
  params: CstNodeChildren<ExecutableDefinitionTypeParam<Ident, Lang>, Lang>,
  r_angle: RAngle<TextRange, SyntaxToken<Lang>>,
}

impl<Ident, Lang> ExecutableDefinitionTypeGenerics<Ident, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    l_angle: LAngle<TextRange, SyntaxToken<Lang>>,
    params: CstNodeChildren<ExecutableDefinitionTypeParam<Ident, Lang>, Lang>,
    r_angle: RAngle<TextRange, SyntaxToken<Lang>>,
  ) -> Self {
    Self {
      syntax,
      l_angle,
      params,
      r_angle,
    }
  }

  /// Tries to create `ExecutableDefinitionTypeGenerics` from the given syntax node.
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
  ) -> &CstNodeChildren<ExecutableDefinitionTypeParam<Ident, Lang>, Lang> {
    &self.params
  }

  /// Returns the right angle bracket token.
  #[inline]
  pub const fn r_angle_token(&self) -> &RAngle<TextRange, SyntaxToken<Lang>> {
    &self.r_angle
  }
}

impl<'a, Ident, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for ExecutableDefinitionTypeGenerics<Ident, Lang>
where
  ExecutableDefinitionTypeParam<Ident, Lang>: Parseable<'a, I, T, Error, Language = Lang>,
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
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    builder.start_node(Self::KIND);
    LAngle::parser(builder)
      .ignore_then(
        ExecutableDefinitionTypeParam::parser(builder)
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
