use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, CstNodeChildren, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::fmt::Debug;
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
#[derive(Debug, Clone)]
pub struct ArgumentsDefinition<InputValueDefinition, Lang>
where
  Lang: Language,
  InputValueDefinition: CstNode<Lang>,
{
  syntax: SyntaxNode<Lang>,
  l_paren: LParen<TextRange, SyntaxToken<Lang>>,
  arguments: CstNodeChildren<InputValueDefinition, Lang>,
  r_paren: RParen<TextRange, SyntaxToken<Lang>>,
}

impl<InputValueDefinition, Lang> ArgumentsDefinition<InputValueDefinition, Lang>
where
  Lang: Language,
  InputValueDefinition: CstNode<Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    l_paren: LParen<TextRange, SyntaxToken<Lang>>,
    arguments: CstNodeChildren<InputValueDefinition, Lang>,
    r_paren: RParen<TextRange, SyntaxToken<Lang>>,
  ) -> Self {
    Self {
      syntax,
      l_paren,
      arguments,
      r_paren,
    }
  }

  /// Tries to create an `ArgumentsDefinition` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
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
  pub const fn l_paren_token(&self) -> &LParen<TextRange, SyntaxToken<Lang>> {
    &self.l_paren
  }

  /// Returns the right parenthesis token.
  #[inline]
  pub const fn r_paren_token(&self) -> &RParen<TextRange, SyntaxToken<Lang>> {
    &self.r_paren
  }

  /// Returns all input value definitions.
  #[inline]
  pub const fn input_value_definitions(&self) -> &CstNodeChildren<InputValueDefinition, Lang> {
    &self.arguments
  }
}

impl<'a, InputValueDefinition, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for ArgumentsDefinition<InputValueDefinition, Lang>
where
  InputValueDefinition: Parseable<'a, I, T, Error, Language = Lang> + CstNode<Lang>,
  LParen<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RParen<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
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
