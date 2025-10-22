use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser, extra::ParserExtra},
  cst::{
    CstNode, CstToken, CstElement, Parseable, SyntaxTreeBuilder, error::CastNodeError,
    cast::{children, token},
  },
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::marker::PhantomData;

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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct List<Value, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _value: PhantomData<Value>,
}

impl<Value, Lang> List<Value, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _value: PhantomData,
    }
  }

  /// Tries to create a `List` from the given syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, CastNodeError<Self>> {
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
  pub fn l_bracket_token(&self) -> LBracket<TextRange, SyntaxToken<Lang>>
  where
    LBracket<TextRange, SyntaxToken<Lang>>: CstToken<Language = Lang>,
  {
    token(self.syntax(), &LBracket::KIND)
      .map(|t| LBracket::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the right bracket token.
  #[inline]
  pub fn r_bracket_token(&self) -> RBracket<TextRange, SyntaxToken<Lang>>
  where
    RBracket<TextRange, SyntaxToken<Lang>>: CstToken<Language = Lang>,
  {
    token(self.syntax(), &RBracket::KIND)
      .map(|t| RBracket::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the values contained in the list.
  #[inline]
  pub fn values(&self) -> logosky::cst::SyntaxNodeChildren<Value>
  where
    Value: CstNode<Language = Lang>,
  {
    children(self.syntax())
  }

  /// Creates a parser for GraphQL list literals with customizable value parsing.
  pub fn parser_with<'a, I, T, Error, E, VP>(
    builder: &'a SyntaxTreeBuilder<Lang>,
    value_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> VP,
  ) -> impl Parser<'a, I, (), E> + Clone
  where
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    LBracket<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    RBracket<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    VP: Parser<'a, I, (), E> + Clone,
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
  Value: Parseable<'a, I, T, Error, Language = Lang>,
  LBracket<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RBracket<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
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
    Self::parser_with(builder, Value::parser)
  }
}
