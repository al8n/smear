use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{
    CstElement, CstNode, CstToken, Parseable, SyntaxTreeBuilder,
    cast::{children, token},
    error::SyntaxError,
  },
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::marker::PhantomData;
use smear_lexer::punctuator::{LParen, RParen};

/// Represents an arguments definition in GraphQL schema syntax.
///
/// An arguments definition specifies the input parameters that can be provided
/// to a field, directive, or other GraphQL construct.
///
/// ## Grammar
///
/// ```text
/// ArgumentsDefinition : ( InputValueDefinition+ )
/// ```
///
/// Spec: [ArgumentsDefinition](https://spec.graphql.org/draft/#ArgumentsDefinition)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArgumentsDefinition<InputValueDefinition, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _input_value_definition: PhantomData<InputValueDefinition>,
}

impl<InputValueDefinition, Lang> ArgumentsDefinition<InputValueDefinition, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _input_value_definition: PhantomData,
    }
  }

  /// Tries to create an `ArgumentsDefinition` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the entire arguments definition.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the left parenthesis token.
  #[inline]
  pub fn l_paren_token(&self) -> LParen<TextRange, SyntaxToken<Lang>>
  where
    LParen<TextRange, SyntaxToken<Lang>>: CstToken<Language = Lang>,
  {
    token(self.syntax(), &LParen::KIND)
      .map(|t| LParen::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the right parenthesis token.
  #[inline]
  pub fn r_paren_token(&self) -> RParen<TextRange, SyntaxToken<Lang>>
  where
    RParen<TextRange, SyntaxToken<Lang>>: CstToken<Language = Lang>,
  {
    token(self.syntax(), &RParen::KIND)
      .map(|t| RParen::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns all input value definitions.
  #[inline]
  pub fn input_value_definitions(&self) -> logosky::cst::CstNodeChildren<InputValueDefinition>
  where
    InputValueDefinition: CstNode<Language = Lang>,
  {
    children(self.syntax())
  }
}

impl<'a, InputValueDefinition, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for ArgumentsDefinition<InputValueDefinition, Lang>
where
  InputValueDefinition: Parseable<'a, I, T, Error, Language = Lang>,
  LParen<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RParen<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
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
    LParen::parser(builder)
      .ignore_then(
        InputValueDefinition::parser(builder)
          .repeated()
          .at_least(1)
          .ignored(),
      )
      .then_ignore(RParen::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}
