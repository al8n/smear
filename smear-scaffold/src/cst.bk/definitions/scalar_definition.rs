use logosky::{
  Logos, LosslessToken, Source, LogoStream,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use smear_lexer::keywords::Scalar;

/// Represents a scalar type definition in the CST.
///
/// ## Grammar
///
/// ```text
/// ScalarTypeDefinition : scalar Name Directives?
/// ```
#[derive(Debug, Clone)]
pub struct ScalarTypeDefinition<Name, Directives, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  scalar_kw: Scalar<TextRange, SyntaxToken<Lang>>,
  name: Name,
  directives: Option<Directives>,
}

impl<Name, Directives, Lang> ScalarTypeDefinition<Name, Directives, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    scalar_kw: Scalar<TextRange, SyntaxToken<Lang>>,
    name: Name,
    directives: Option<Directives>,
  ) -> Self {
    Self {
      syntax,
      scalar_kw,
      name,
      directives,
    }
  }

  /// Tries to create a `ScalarTypeDefinition` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering this scalar definition.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the underlying syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the `scalar` keyword token.
  #[inline]
  pub const fn scalar_keyword(&self) -> &Scalar<TextRange, SyntaxToken<Lang>> {
    &self.scalar_kw
  }

  /// Returns the name of the scalar definition.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the optional directives applied to the scalar definition.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }
}

impl<'a, Name, Directives, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for ScalarTypeDefinition<Name, Directives, Lang>
where
  Scalar<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Name: Parseable<'a, I, T, Error, Language = Lang>,
  Directives: Parseable<'a, I, T, Error, Language = Lang>,
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
    Scalar::parser(builder)
      .ignore_then(Name::parser(builder))
      .ignore_then(Directives::parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}
