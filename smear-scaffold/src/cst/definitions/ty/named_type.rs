use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser},
  cst::{CstElement, CstNode, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use smear_lexer::punctuator::Bang;

/// Represents a named GraphQL type with optional non-null modifier in CST.
///
/// ## Grammar
/// ```text
/// NamedType : Name !?
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NamedType<Name, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  name: Name,
  bang: Option<Bang<TextRange, SyntaxToken<Lang>>>,
}

impl<Name, Lang> NamedType<Name, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    name: Name,
    bang: Option<Bang<TextRange, SyntaxToken<Lang>>>,
  ) -> Self {
    Self { syntax, name, bang }
  }

  /// Tries to create a new `NamedType` from a syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the entire named type.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the type name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
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

impl<'a, Name, Lang, I, T, Error> Parseable<'a, I, T, Error> for NamedType<Name, Lang>
where
  Name: Parseable<'a, I, T, Error, Language = Lang>,
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
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    Error: 'a,
    E: chumsky::extra::ParserExtra<'a, I, Error = Error> + 'a,
  {
    builder.start_node(Self::KIND);
    Name::parser(builder)
      .ignore_then(Bang::parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}
