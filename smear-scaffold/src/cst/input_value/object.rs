use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser, extra::ParserExtra},
  cst::{
    CstNode, CstToken, CstElement, Parseable, SyntaxTreeBuilder, error::CastNodeError,
    cast::{child, children, token},
  },
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::marker::PhantomData;

use smear_lexer::punctuator::{Colon, LBrace, RBrace};

/// A single field within a GraphQL input object literal.
///
/// Represents a name-value pair within an object literal, following the
/// GraphQL specification for input object fields.
///
/// ## Grammar
///
/// ```text
/// ObjectField ::= Name ':' Value
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjectField<Name, Value, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _name: PhantomData<Name>,
  _value: PhantomData<Value>,
}

impl<Name, Value, Lang> ObjectField<Name, Value, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _name: PhantomData,
      _value: PhantomData,
    }
  }

  /// Tries to create an `ObjectField` from the given syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, CastNodeError<Self>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the source span of the entire field.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node representing the field.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the field name.
  #[inline]
  pub fn name(&self) -> Name
  where
    Name: CstNode<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }

  /// Returns the colon token.
  #[inline]
  pub fn colon_token(&self) -> Colon<TextRange, SyntaxToken<Lang>>
  where
    Colon<TextRange, SyntaxToken<Lang>>: CstToken<Language = Lang>,
  {
    token(self.syntax(), &Colon::KIND)
      .map(|t| Colon::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the field value.
  #[inline]
  pub fn value(&self) -> Value
  where
    Value: CstNode<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }

  /// Creates a parser for object fields.
  pub fn parser_with<'a, I, T, Error, E, NP, VP>(
    builder: &'a SyntaxTreeBuilder<Lang>,
    name_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> NP,
    value_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> VP,
  ) -> impl Parser<'a, I, (), E> + Clone
  where
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    Colon<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    NP: Parser<'a, I, (), E> + Clone,
    VP: Parser<'a, I, (), E> + Clone,
  {
    builder.start_node(Self::KIND);
    name_parser(builder)
      .then_ignore(Colon::parser(builder))
      .then(value_parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}

impl<'a, Name, Value, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for ObjectField<Name, Value, Lang>
where
  Name: Parseable<'a, I, T, Error, Language = Lang>,
  Value: Parseable<'a, I, T, Error, Language = Lang>,
  Colon<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
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
    Self::parser_with(builder, Name::parser, Value::parser)
  }
}

/// A GraphQL input object literal value.
///
/// Represents a complete input object literal as defined by the GraphQL
/// specification. Input objects are unordered collections of name-value pairs
/// enclosed in curly braces.
///
/// ## Grammar
///
/// ```text
/// Object ::= '{' ObjectFields? '}'
/// ObjectFields ::= ObjectField+
/// ObjectField ::= Name ':' Value
/// ```
///
/// Spec: [Input Object Values](https://spec.graphql.org/draft/#sec-Input-Object-Values)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Object<Name, Value, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _name: PhantomData<Name>,
  _value: PhantomData<Value>,
}

impl<Name, Value, Lang> Object<Name, Value, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _name: PhantomData,
      _value: PhantomData,
    }
  }

  /// Tries to create an `Object` from the given syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, super::CastNodeError<Self>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the source span of the entire object literal.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node representing the object.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
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

  /// Returns the fields contained in the object.
  #[inline]
  pub fn fields(&self) -> logosky::cst::SyntaxNodeChildren<ObjectField<Name, Value, Lang>>
  where
    ObjectField<Name, Value, Lang>: CstNode<Language = Lang>,
  {
    children(self.syntax())
  }

  /// Creates a parser for GraphQL object literals.
  pub fn parser_with<'a, I, T, Error, E, NP, VP>(
    builder: &'a SyntaxTreeBuilder<Lang>,
    name_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> NP,
    value_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> VP,
  ) -> impl Parser<'a, I, (), E> + Clone
  where
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    LBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    RBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    Colon<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    NP: Parser<'a, I, (), E> + Clone,
    VP: Parser<'a, I, (), E> + Clone,
    ObjectField<Name, Value, Lang>: CstNode<Language = Lang>,
  {
    builder.start_node(Self::KIND);
    LBrace::parser(builder)
      .ignore_then(
        ObjectField::parser_with(builder, name_parser, value_parser)
          .repeated()
          .ignored(),
      )
      .then_ignore(RBrace::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}

impl<'a, Name, Value, Lang, I, T, Error> Parseable<'a, I, T, Error> for Object<Name, Value, Lang>
where
  Name: Parseable<'a, I, T, Error, Language = Lang>,
  Value: Parseable<'a, I, T, Error, Language = Lang>,
  LBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Colon<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  ObjectField<Name, Value, Lang>:
    Parseable<'a, I, T, Error, Language = Lang> + CstNode<Language = Lang>,
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
    Self::parser_with(builder, Name::parser, Value::parser)
  }
}
