use logosky::{
  Logos, LosslessToken, Source, LogoStream,
  chumsky::{self, Parser},
  cst::{CstElement, CstNode, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

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
  l_angle: LAngle<TextRange, SyntaxToken<Lang>>,
  ty: Type,
  r_angle: RAngle<TextRange, SyntaxToken<Lang>>,
  bang: Option<Bang<TextRange, SyntaxToken<Lang>>>,
}

impl<Type, Lang> AngleType<Type, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    l_angle: LAngle<TextRange, SyntaxToken<Lang>>,
    ty: Type,
    r_angle: RAngle<TextRange, SyntaxToken<Lang>>,
    bang: Option<Bang<TextRange, SyntaxToken<Lang>>>,
  ) -> Self {
    Self {
      syntax,
      l_angle,
      ty,
      r_angle,
      bang,
    }
  }

  /// Tries to create a new `AngleType` from a syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>> {
    Self::try_cast_node(syntax)
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
  pub const fn l_angle_token(&self) -> &LAngle<TextRange, SyntaxToken<Lang>> {
    &self.l_angle
  }

  /// Returns the inner type.
  #[inline]
  pub const fn ty(&self) -> &Type {
    &self.ty
  }

  /// Returns the right angle bracket token.
  #[inline]
  pub const fn r_angle_token(&self) -> &RAngle<TextRange, SyntaxToken<Lang>> {
    &self.r_angle
  }

  /// Returns the bang token if present.
  #[inline]
  pub const fn bang_token(&self) -> Option<&Bang<TextRange, SyntaxToken<Lang>>> {
    self.bang.as_ref()
  }

  /// Returns whether this type is required (non-null).
  #[inline]
  pub const fn required(&self) -> bool {
    self.bang.is_some()
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
  Self: CstNode<Lang>,
{
  type Language = Lang;

  #[inline]
  fn parser<E>(
    builder: &'a SyntaxTreeBuilder<Self::Language>,
  ) -> impl chumsky::Parser<'a, I, (), E> + Clone
  where
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
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
