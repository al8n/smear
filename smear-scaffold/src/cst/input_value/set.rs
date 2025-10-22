use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser, extra::ParserExtra},
  cst::{
    CstElement, CstNode, CstNodeChildren, CstToken, Parseable, SyntaxTreeBuilder,
    cast::{children, token},
    error::SyntaxError,
  },
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use smear_lexer::{
  keywords,
  punctuator::{LBrace, RBrace},
};

/// A GraphQLx set literal value in CST form.
///
/// Represents a complete set literal as defined by the GraphQLx specification.
/// Set literals are collections of unique values enclosed in braces with
/// the `set` keyword prefix.
///
/// ## Grammar
///
/// ```text
/// Set ::= 'set' '{' Values? '}'
/// Values ::= Value+
/// ```
#[derive(Debug, Clone)]
pub struct Set<Value, Lang>
where
  Lang: Language,
  Value: CstNode<Language = Lang>,
{
  syntax: SyntaxNode<Lang>,
  set_keyword: keywords::Set<TextRange, SyntaxToken<Lang>>,
  l_brace: LBrace<TextRange, SyntaxToken<Lang>>,
  r_brace: RBrace<TextRange, SyntaxToken<Lang>>,
  values: CstNodeChildren<Value>,
}

impl<Value, Lang> Set<Value, Lang>
where
  Lang: Language,
  Value: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    set_keyword: keywords::Set<TextRange, SyntaxToken<Lang>>,
    l_brace: LBrace<TextRange, SyntaxToken<Lang>>,
    values: CstNodeChildren<Value>,
    r_brace: RBrace<TextRange, SyntaxToken<Lang>>,
  ) -> Self {
    Self {
      syntax,
      set_keyword,
      l_brace,
      values,
      r_brace,
    }
  }

  /// Tries to create a `Set` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self>>
  where
    Self: CstNode<Language = Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the source span of the entire set literal.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node representing the set.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the set keyword token.
  #[inline]
  pub fn set_keyword_token(&self) -> keywords::Set<TextRange, SyntaxToken<Lang>>
  where
    keywords::Set<TextRange, SyntaxToken<Lang>>: CstToken<Language = Lang>,
  {
    token(self.syntax(), &keywords::Set::KIND)
      .map(|t| keywords::Set::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the left brace token.
  #[inline]
  pub fn l_brace_token(&self) -> LBrace<TextRange, SyntaxToken<Lang>>
  where
    LBrace<TextRange, SyntaxToken<Lang>>: CstToken<Language = Lang>,
  {
    token(self.syntax(), &LBrace::KIND)
      .map(|t| LBrace::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the right brace token.
  #[inline]
  pub fn r_brace_token(&self) -> RBrace<TextRange, SyntaxToken<Lang>>
  where
    RBrace<TextRange, SyntaxToken<Lang>>: CstToken<Language = Lang>,
  {
    token(self.syntax(), &RBrace::KIND)
      .map(|t| RBrace::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the values contained in the set.
  #[inline]
  pub fn values(&self) -> logosky::cst::CstNodeChildren<Value>
  where
    Value: CstNode<Language = Lang>,
  {
    children(self.syntax())
  }

  /// Creates a parser for GraphQLx set literals with customizable value parsing.
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
    keywords::Set<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    LBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    RBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    VP: Parser<'a, I, (), E> + Clone,
    Lang::Kind: Into<rowan::SyntaxKind>,
    Self: CstNode<Language = Lang>,
  {
    builder.start_node(Self::KIND);
    keywords::Set::parser(builder)
      .ignore_then(LBrace::parser(builder))
      .ignore_then(value_parser(builder).repeated().ignored())
      .then_ignore(RBrace::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}

impl<'a, Value, Lang, I, T, Error> Parseable<'a, I, T, Error> for Set<Value, Lang>
where
  Value: Parseable<'a, I, T, Error, Language = Lang> + CstNode<Language = Lang>,
  keywords::Set<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  LBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
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
