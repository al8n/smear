use logosky::{
  Logos, LosslessToken, Source, LogoStream,
  chumsky::{self, Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, CstNodeChildren, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use smear_lexer::punctuator::{LBracket, RBracket};

/// A GraphQL list literal value in CST form.
///
/// Represents a complete list literal as defined by the GraphQL specification.
/// List literals are ordered collections of values enclosed in square brackets.
///
/// ## Grammar
///
/// ```text
/// List ::= '[' Values? ']'
/// Values ::= Value+
/// ```
///
/// Spec: [List Value](https://spec.graphql.org/draft/#sec-List-Value)
#[derive(Debug, Clone)]
pub struct List<Value, Lang>
where
  Lang: Language,
  Value: CstNode<Lang>,
{
  syntax: SyntaxNode<Lang>,
  l_bracket: LBracket<TextRange, SyntaxToken<Lang>>,
  values: CstNodeChildren<Value, Lang>,
  r_bracket: RBracket<TextRange, SyntaxToken<Lang>>,
}

impl<Value, Lang> List<Value, Lang>
where
  Lang: Language,
  Value: CstNode<Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    l_bracket: LBracket<TextRange, SyntaxToken<Lang>>,
    values: CstNodeChildren<Value, Lang>,
    r_bracket: RBracket<TextRange, SyntaxToken<Lang>>,
  ) -> Self {
    Self {
      syntax,
      l_bracket,
      values,
      r_bracket,
    }
  }

  /// Tries to create a `List` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the source span of the entire list literal.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node representing the list.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the left bracket token.
  #[inline]
  pub const fn l_bracket_token(&self) -> &LBracket<TextRange, SyntaxToken<Lang>> {
    &self.l_bracket
  }

  /// Returns the right bracket token.
  #[inline]
  pub const fn r_bracket_token(&self) -> &RBracket<TextRange, SyntaxToken<Lang>> {
    &self.r_bracket
  }

  /// Returns the values contained in the list.
  #[inline]
  pub const fn values(&self) -> &CstNodeChildren<Value, Lang> {
    &self.values
  }

  /// Creates a parser for GraphQL list literals with customizable value parsing.
  pub fn parser_with<'a, I, T, Error, E, VP>(
    builder: &'a SyntaxTreeBuilder<Lang>,
    value_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> VP,
  ) -> impl Parser<'a, I, (), E> + Clone
  where
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    LBracket<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    RBracket<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    VP: Parser<'a, I, (), E> + Clone,
    Lang::Kind: Into<rowan::SyntaxKind>,
    Self: CstNode<Lang>,
  {
    builder.start_node(Self::KIND);
    LBracket::parser(builder)
      .ignore_then(value_parser(builder).repeated().ignored())
      .then_ignore(RBracket::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}

impl<'a, Value, Lang, I, T, Error> Parseable<'a, I, T, Error> for List<Value, Lang>
where
  Value: Parseable<'a, I, T, Error, Language = Lang> + CstNode<Lang>,
  LBracket<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RBracket<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
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
    Self::parser_with(builder, Value::parser)
  }
}
