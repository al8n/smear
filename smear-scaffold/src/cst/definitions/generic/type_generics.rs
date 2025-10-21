use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{Node, Parseable, SyntaxTreeBuilder, cast::children},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::marker::PhantomData;
use smear_lexer::punctuator::{LAngle, RAngle};

/// Type generics in CST (e.g., `<ID, Username>`).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeGenerics<Type, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _type: PhantomData<Type>,
}

impl<Type, Lang> TypeGenerics<Type, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: Node<Language = Lang>,
{
  /// Tries to create a new `TypeGenerics` from a syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, logosky::cst::error::SyntaxNodeMismatch<Self>> {
    Self::try_cast(syntax)
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
  pub fn params(&self) -> logosky::cst::SyntaxNodeChildren<Type>
  where
    Type: Node<Language = Lang>,
  {
    children(self.syntax())
  }
}

impl<'a, Type, Lang, I, T, Error> Parseable<'a, I, T, Error> for TypeGenerics<Type, Lang>
where
  Type: Parseable<'a, I, T, Error, Language = Lang>,
  LAngle<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RAngle<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: Node<Language = Lang>,
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
