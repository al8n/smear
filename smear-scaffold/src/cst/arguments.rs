use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser, extra::ParserExtra},
  cst::{
    Node, Parseable, SyntaxTreeBuilder,
    cast::{child, children, token},
  },
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::marker::PhantomData;

use smear_lexer::punctuator::{Colon, LParen, RParen};

/// A single named argument in a GraphQL operation or directive.
///
/// Represents a name-value pair used to pass parameters to GraphQL fields,
/// directives, or other language constructs. Arguments follow the standard
/// GraphQL syntax of a name identifier followed by a colon and a value.
///
/// ## Grammar
///
/// ```text
/// Argument ::= Name ':' Value
/// ```
///
/// Spec: [Argument](https://spec.graphql.org/draft/#Argument)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Argument<Name, Value, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _name: PhantomData<Name>,
  _value: PhantomData<Value>,
}

impl<Name, Value, Lang> Argument<Name, Value, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: Node<Language = Lang>,
{
  /// Tries to create an `Argument` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, super::error::SyntaxNodeMismatch<Self>>
  where
    Lang::Kind: Into<rowan::SyntaxKind>,
    Self: crate::cst::Node<Language = Lang>,
  {
    Self::try_cast(syntax)
  }

  /// Returns the source span of the entire argument.
  ///
  /// This span covers from the first character of the argument name through
  /// the last character of the argument value, providing the complete source
  /// location for error reporting and source mapping.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node representing the entire argument.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the argument name.
  #[inline]
  pub fn name(&self) -> Name
  where
    Name: Node<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }

  /// Returns the colon token separating the name and value.
  #[inline]
  pub fn colon_token(&self) -> Colon<TextRange, SyntaxToken<Lang>>
  where
    Colon<TextRange, SyntaxToken<Lang>>: Node<Language = Lang>,
  {
    token(self.syntax(), &Colon::KIND)
      .map(|t| Colon::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the argument value.
  ///
  /// This provides access to the value assigned to this argument. The value
  /// can be any valid GraphQL input value including scalars, enums, objects,
  /// lists, variables, or null depending on the argument's expected type.
  #[inline]
  pub fn value(&self) -> Value
  where
    Value: Node<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }

  /// Creates a parser for arguments with custom name and value parsers.
  ///
  /// This parser handles the complete argument syntax including the name,
  /// colon, and value. It allows for flexible parsing of both the name and
  /// value components by accepting custom parsers for each. This enables
  /// integration with different name and value parsing strategies as needed.
  #[inline]
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

impl<'a, Name, Value, Lang, I, T, Error> Parseable<'a, I, T, Error> for Argument<Name, Value, Lang>
where
  Name: Parseable<'a, I, T, Error, Language = Lang>,
  Colon<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Value: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: Node<Language = Lang>,
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

/// A collection of arguments enclosed in parentheses.
///
/// Represents an argument list as used in GraphQL fields, directives, and other
/// language constructs. Arguments are enclosed in parentheses and separated by
/// whitespace, providing a structured way to pass parameters in GraphQL operations.
///
/// ## Specification Rules
///
/// GraphQL argument lists follow these formatting rules:
/// - **Parenthesis delimiters**: Must be enclosed in `(` and `)`
/// - **Argument format**: Each argument follows `name: value` syntax
/// - **Argument separation**: Arguments separated by whitespace (commas optional)
/// - **Non-empty requirement**: Argument lists must contain at least one argument
/// - **Flexible whitespace**: Whitespace and comments allowed throughout
/// - **Unique names**: Argument names should be unique within the list (semantic validation)
///
/// ## Grammar
///
/// ```text
/// Arguments ::= '(' Argument+ ')'
/// Argument ::= Name ':' Value
/// ```
///
/// ## Generic Parameters
///
/// - `Arg`: The type representing individual arguments
/// - `Span`: The span type for position information
/// - `Container`: The collection type for arguments (defaults to `Vec`, can be customized)
///
/// ## Container Flexibility
///
/// The `Container` parameter allows using different collection types:
/// - `Vec<Argument<Value, Span>>` (default): Standard dynamic array
/// - Any type implementing `chumsky::container::Container<Arg>`
///
/// ## Component Structure
///
/// Each argument list contains:
/// - **Overall span**: Covers the entire argument list including parentheses
/// - **Left parenthesis**: The opening `(` token with its position
/// - **Right parenthesis**: The closing `)` token with its position
/// - **Arguments**: The collection of parsed arguments
///
/// Spec: [Arguments](https://spec.graphql.org/draft/#Arguments)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Arguments<Arg, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _arg: PhantomData<Arg>,
}

impl<Arg, Lang> Arguments<Arg, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: Node<Language = Lang>,
{
  /// Tries to create a `Arguments` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, super::error::SyntaxNodeMismatch<Self>> {
    Self::try_cast(syntax)
  }

  /// Returns the source span of the entire argument list.
  ///
  /// This span covers from the opening parenthesis through the closing
  /// parenthesis, including all arguments and whitespace within. Useful for
  /// error reporting, source mapping, and extracting the complete argument text.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node representing the entire argument list.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the opening parenthesis token.
  ///
  /// This provides access to the `(` character that begins the argument list,
  /// including its exact source position. Useful for syntax highlighting,
  /// parenthesis matching, and precise error reporting at argument boundaries.
  #[inline]
  pub fn l_paren_token(&self) -> LParen<TextRange, SyntaxToken<Lang>>
  where
    LParen<TextRange, SyntaxToken<Lang>>: Node<Language = Lang>,
  {
    token(self.syntax(), &LParen::KIND)
      .map(|t| LParen::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the closing parenthesis token.
  ///
  /// This provides access to the `)` character that ends the argument list,
  /// including its exact source position. Useful for syntax highlighting,
  /// parenthesis matching, and detecting incomplete argument lists.
  #[inline]
  pub fn r_paren_token(&self) -> RParen<TextRange, SyntaxToken<Lang>>
  where
    RParen<TextRange, SyntaxToken<Lang>>: Node<Language = Lang>,
  {
    token(self.syntax(), &RParen::KIND)
      .map(|t| RParen::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the collection of arguments.
  ///
  /// This provides access to all arguments that were successfully parsed
  /// from the argument list.
  #[inline]
  pub fn arguments(&self) -> logosky::cst::SyntaxNodeChildren<Arg>
  where
    Arg: Node<Language = Lang>,
  {
    children(self.syntax())
  }

  /// Creates a parser for arguments using the provided argument parser.
  pub fn parser_with<'a, I, T, Error, E, AP>(
    builder: &'a SyntaxTreeBuilder<Lang>,
    arg_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> AP,
  ) -> impl Parser<'a, I, (), E> + Clone
  where
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    Arg: Parseable<'a, I, T, Error, Language = Lang>,
    LParen<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    RParen<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    AP: Parser<'a, I, (), E> + Clone,
  {
    builder.start_node(Self::KIND);
    LParen::parser(builder)
      .ignore_then(arg_parser(builder).repeated().at_least(1).ignored())
      .then_ignore(RParen::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}

impl<'a, Arg, Lang, I, T, Error> Parseable<'a, I, T, Error> for Arguments<Arg, Lang>
where
  Arg: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  LParen<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RParen<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Self: Node<Language = Lang>,
{
  type Language = Lang;

  fn parser<E>(
    builder: &'a SyntaxTreeBuilder<Self::Language>,
  ) -> impl chumsky::Parser<'a, I, (), E> + Clone
  where
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: LosslessToken<'a>,
    <<T>::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    Error: 'a,
    E: chumsky::extra::ParserExtra<'a, I, Error = Error> + 'a,
  {
    Self::parser_with(builder, Arg::parser)
  }
}
