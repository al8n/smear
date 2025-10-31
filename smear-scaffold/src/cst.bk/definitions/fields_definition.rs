use logosky::{
  Logos, LosslessToken, Source, LogoStream,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, CstNodeChildren, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::fmt::Debug;
use smear_lexer::punctuator::{LBrace, RBrace};

/// Represents a fields definition in GraphQL schema.
#[derive(Debug, Clone)]
pub struct FieldsDefinition<FieldDefinition, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  l_brace: LBrace<TextRange, SyntaxToken<Lang>>,
  fields: CstNodeChildren<FieldDefinition, Lang>,
  r_brace: RBrace<TextRange, SyntaxToken<Lang>>,
}

impl<FieldDefinition, Lang> FieldsDefinition<FieldDefinition, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    l_brace: LBrace<TextRange, SyntaxToken<Lang>>,
    fields: CstNodeChildren<FieldDefinition, Lang>,
    r_brace: RBrace<TextRange, SyntaxToken<Lang>>,
  ) -> Self {
    Self {
      syntax,
      l_brace,
      fields,
      r_brace,
    }
  }

  /// Tries to create a `FieldsDefinition` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering this fields definition.
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
  pub const fn l_brace_token(&self) -> &LBrace<TextRange, SyntaxToken<Lang>> {
    &self.l_brace
  }

  /// Returns the right brace token.
  #[inline]
  pub const fn r_brace_token(&self) -> &RBrace<TextRange, SyntaxToken<Lang>> {
    &self.r_brace
  }

  /// Returns the collection of field definitions.
  #[inline]
  pub const fn field_definitions(&self) -> &CstNodeChildren<FieldDefinition, Lang> {
    &self.fields
  }
}

impl<'a, FieldDefinition, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for FieldsDefinition<FieldDefinition, Lang>
where
  FieldDefinition: Parseable<'a, I, T, Error, Language = Lang> + CstNode<Lang>,
  LBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
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
    LBrace::parser(builder)
      .ignore_then(
        FieldDefinition::parser(builder)
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
