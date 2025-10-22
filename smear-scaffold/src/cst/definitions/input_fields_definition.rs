use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, CstNodeChildren, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::fmt::Debug;
use smear_lexer::punctuator::{LBrace, RBrace};

/// Represents an input fields definition in GraphQL schema.
#[derive(Debug, Clone)]
pub struct InputFieldsDefinition<InputValueDefinition, Lang>
where
  Lang: Language,
  InputValueDefinition: CstNode<Language = Lang>,
{
  syntax: SyntaxNode<Lang>,
  l_brace: LBrace<TextRange, SyntaxToken<Lang>>,
  fields: CstNodeChildren<InputValueDefinition>,
  r_brace: RBrace<TextRange, SyntaxToken<Lang>>,
}

impl<InputValueDefinition, Lang> InputFieldsDefinition<InputValueDefinition, Lang>
where
  Lang: Language,
  InputValueDefinition: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    l_brace: LBrace<TextRange, SyntaxToken<Lang>>,
    fields: CstNodeChildren<InputValueDefinition>,
    r_brace: RBrace<TextRange, SyntaxToken<Lang>>,
  ) -> Self {
    Self {
      syntax,
      l_brace,
      fields,
      r_brace,
    }
  }

  /// Tries to create an `InputFieldsDefinition` from the given syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self>>
  where
    Self: CstNode<Language = Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering this input fields definition.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the left brace token.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn l_brace_token(&self) -> &LBrace<TextRange, SyntaxToken<Lang>> {
    &self.l_brace
  }

  /// Returns the right brace token.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn r_brace_token(&self) -> &RBrace<TextRange, SyntaxToken<Lang>> {
    &self.r_brace
  }

  /// Returns the collection of input value definitions.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn input_value_definitions(&self) -> &CstNodeChildren<InputValueDefinition> {
    &self.fields
  }
}

impl<'a, InputValueDefinition, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for InputFieldsDefinition<InputValueDefinition, Lang>
where
  InputValueDefinition: Parseable<'a, I, T, Error, Language = Lang> + CstNode<Language = Lang>,
  LBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
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
