use logosky::{
  Logos, LosslessToken, Source, LogoStream,
  chumsky::{self, Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, CstNodeChildren, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

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
  name: Name,
  colon: Colon<TextRange, SyntaxToken<Lang>>,
  value: Value,
}

impl<Name, Value, Lang> ObjectField<Name, Value, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    name: Name,
    colon: Colon<TextRange, SyntaxToken<Lang>>,
    value: Value,
  ) -> Self {
    Self {
      syntax,
      name,
      colon,
      value,
    }
  }

  /// Tries to create an `ObjectField` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
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
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the colon token.
  #[inline]
  pub const fn colon_token(&self) -> &Colon<TextRange, SyntaxToken<Lang>> {
    &self.colon
  }

  /// Returns the field value.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
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
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    Colon<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    NP: Parser<'a, I, (), E> + Clone,
    VP: Parser<'a, I, (), E> + Clone,
    Lang::Kind: Into<rowan::SyntaxKind>,
    Self: CstNode<Lang>,
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
#[derive(Debug, Clone)]
pub struct Object<Name, Value, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  l_brace: LBrace<TextRange, SyntaxToken<Lang>>,
  fields: CstNodeChildren<ObjectField<Name, Value, Lang>, Lang>,
  r_brace: RBrace<TextRange, SyntaxToken<Lang>>,
}

impl<Name, Value, Lang> Object<Name, Value, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    l_brace: LBrace<TextRange, SyntaxToken<Lang>>,
    fields: CstNodeChildren<ObjectField<Name, Value, Lang>, Lang>,
    r_brace: RBrace<TextRange, SyntaxToken<Lang>>,
  ) -> Self {
    Self {
      syntax,
      l_brace,
      fields,
      r_brace,
    }
  }

  /// Tries to create an `Object` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
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
  pub const fn l_brace_token(&self) -> &LBrace<TextRange, SyntaxToken<Lang>> {
    &self.l_brace
  }

  /// Returns the right brace token.
  #[inline]
  pub const fn r_brace_token(&self) -> &RBrace<TextRange, SyntaxToken<Lang>> {
    &self.r_brace
  }

  /// Returns the fields contained in the object.
  #[inline]
  pub const fn fields(&self) -> &CstNodeChildren<ObjectField<Name, Value, Lang>, Lang> {
    &self.fields
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
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    LBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    RBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    Colon<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    NP: Parser<'a, I, (), E> + Clone,
    VP: Parser<'a, I, (), E> + Clone,
    ObjectField<Name, Value, Lang>: CstNode<Lang>,
    Lang::Kind: Into<rowan::SyntaxKind>,
    Self: CstNode<Lang>,
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
    Parseable<'a, I, T, Error, Language = Lang> + CstNode<Lang>,
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
    Self::parser_with(builder, Name::parser, Value::parser)
  }
}
