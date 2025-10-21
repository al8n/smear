use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{
    Node, Parseable, SyntaxTreeBuilder,
    cast::{children, token},
  },
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::marker::PhantomData;
use smear_lexer::punctuator::{LBrace, RBrace};

/// Represents an input fields definition in GraphQL schema.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InputFieldsDefinition<InputValueDefinition, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _input_value_definition: PhantomData<InputValueDefinition>,
}

impl<InputValueDefinition, Lang> InputFieldsDefinition<InputValueDefinition, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: Node<Language = Lang>,
{
  /// Tries to create an `InputFieldsDefinition` from the given syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, super::super::error::SyntaxNodeMismatch<Self>> {
    Self::try_cast(syntax)
  }

  /// Returns the span covering this input fields definition.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the left brace token.
  #[inline]
  pub fn l_brace_token(&self) -> LBrace<TextRange, SyntaxToken<Lang>>
  where
    LBrace<TextRange, SyntaxToken<Lang>>: Node<Language = Lang>,
  {
    token(self.syntax(), &LBrace::KIND)
      .map(|t| LBrace::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the right brace token.
  #[inline]
  pub fn r_brace_token(&self) -> RBrace<TextRange, SyntaxToken<Lang>>
  where
    RBrace<TextRange, SyntaxToken<Lang>>: Node<Language = Lang>,
  {
    token(self.syntax(), &RBrace::KIND)
      .map(|t| RBrace::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the collection of input value definitions.
  #[inline]
  pub fn input_value_definitions(&self) -> logosky::cst::SyntaxNodeChildren<InputValueDefinition>
  where
    InputValueDefinition: Node<Language = Lang>,
  {
    children(self.syntax())
  }
}

impl<'a, InputValueDefinition, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for InputFieldsDefinition<InputValueDefinition, Lang>
where
  InputValueDefinition: Parseable<'a, I, T, Error, Language = Lang>,
  LBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
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
    LBrace::parser(builder)
      .ignore_then(
        InputValueDefinition::parser(builder)
          .repeated()
          .at_least(1)
          .ignored(),
      )
      .then_ignore(RBrace::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}
