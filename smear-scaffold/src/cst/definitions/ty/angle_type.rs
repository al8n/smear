use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser},
  cst::{Node, Parseable, SyntaxTreeBuilder, cast::child},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::marker::PhantomData;
use smear_lexer::punctuator::{Bang, LAngle, RAngle};

/// Represents a GraphQLx angle-bracketed type with optional non-null modifier in CST.
///
/// ## Grammar
/// ```text
/// AngleType : < Type > !?
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AngleType<Type, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _ty: PhantomData<Type>,
}

impl<Type, Lang> AngleType<Type, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: Node<Language = Lang>,
{
  /// Tries to create a new `AngleType` from a syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, logosky::cst::error::SyntaxNodeMismatch<Self>> {
    Self::try_cast(syntax)
  }

  /// Returns the span covering the entire angle type.
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
    LAngle<TextRange, SyntaxToken<Lang>>: Node<Language = Lang>,
  {
    logosky::cst::cast::token(self.syntax(), &LAngle::KIND)
      .map(|t| LAngle::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the inner type.
  #[inline]
  pub fn ty(&self) -> Type
  where
    Type: Node<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }

  /// Returns the right angle bracket token.
  #[inline]
  pub fn r_angle_token(&self) -> RAngle<TextRange, SyntaxToken<Lang>>
  where
    RAngle<TextRange, SyntaxToken<Lang>>: Node<Language = Lang>,
  {
    logosky::cst::cast::token(self.syntax(), &RAngle::KIND)
      .map(|t| RAngle::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the bang token if present.
  #[inline]
  pub fn bang_token(&self) -> Option<Bang<TextRange, SyntaxToken<Lang>>>
  where
    Bang<TextRange, SyntaxToken<Lang>>: Node<Language = Lang>,
  {
    logosky::cst::cast::token(self.syntax(), &Bang::KIND)
      .map(|t| Bang::with_content(t.text_range(), t))
  }

  /// Returns whether this type is required (non-null).
  #[inline]
  pub fn required(&self) -> bool
  where
    Bang<TextRange, SyntaxToken<Lang>>: Node<Language = Lang>,
  {
    self.bang_token().is_some()
  }
}

impl<'a, Type, Lang, I, T, Error> Parseable<'a, I, T, Error> for AngleType<Type, Lang>
where
  Type: Parseable<'a, I, T, Error, Language = Lang>,
  LAngle<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RAngle<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Bang<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: Node<Language = Lang>,
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
    LAngle::parser(builder)
      .ignore_then(Type::parser(builder))
      .then_ignore(RAngle::parser(builder))
      .ignore_then(Bang::parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}
