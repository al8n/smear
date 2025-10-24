use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, CstNodeChildren, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use smear_lexer::punctuator::{LAngle, RAngle};

/// Type generics in CST (e.g., `<ID, Username>`).
#[derive(Debug, Clone)]
pub struct TypeGenerics<Type, Lang>
where
  Lang: Language,
  Type: CstNode<Lang>,
{
  syntax: SyntaxNode<Lang>,
  l_angle: LAngle<TextRange, SyntaxToken<Lang>>,
  params: CstNodeChildren<Type>,
  r_angle: RAngle<TextRange, SyntaxToken<Lang>>,
}

impl<Type, Lang> TypeGenerics<Type, Lang>
where
  Lang: Language,
  Type: CstNode<Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    l_angle: LAngle<TextRange, SyntaxToken<Lang>>,
    params: CstNodeChildren<Type>,
    r_angle: RAngle<TextRange, SyntaxToken<Lang>>,
  ) -> Self {
    Self {
      syntax,
      l_angle,
      params,
      r_angle,
    }
  }

  /// Tries to create a new `TypeGenerics` from a syntax node.
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

  /// Returns the type parameters.
  #[inline]
  pub const fn params(&self) -> &CstNodeChildren<Type> {
    &self.params
  }

  /// Returns the left angle bracket token.
  #[inline]
  pub const fn l_angle_token(&self) -> &LAngle<TextRange, SyntaxToken<Lang>> {
    &self.l_angle
  }

  /// Returns the right angle bracket token.
  #[inline]
  pub const fn r_angle_token(&self) -> &RAngle<TextRange, SyntaxToken<Lang>> {
    &self.r_angle
  }
}

impl<'a, Type, Lang, I, T, Error> Parseable<'a, I, T, Error> for TypeGenerics<Type, Lang>
where
  Type: Parseable<'a, I, T, Error, Language = Lang> + CstNode<Lang>,
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
      .ignore_then(Type::parser(builder).repeated().at_least(1).ignored())
      .then_ignore(RAngle::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}
