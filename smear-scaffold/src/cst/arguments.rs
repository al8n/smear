use logosky::{
  Logos, LosslessToken, Source, LogoStream,
  chumsky::{self, Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, CstNodeChildren, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::fmt::Debug;

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
#[derive(Debug, Clone)]
pub struct Argument<Name, Value, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  name: Name,
  colon: Colon<TextRange, SyntaxToken<Lang>>,
  value: Value,
}

impl<Name, Value, Lang> Argument<Name, Value, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn new(
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

  /// Tries to create an `Argument` from the given syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the source span of the entire argument.
  ///
  /// This span covers from the first character of the argument name through
  /// the last character of the argument value, providing the complete source
  /// location for error reporting and source mapping.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node representing the entire argument.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the argument name.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn name(&self) -> Name
  where
    Name: Clone,
  {
    self.name.clone()
  }

  /// Returns the colon token separating the name and value.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn colon_token(&self) -> &Colon<TextRange, SyntaxToken<Lang>>
  {
    &self.colon
  }

  /// Returns the argument value.
  ///
  /// This provides access to the value assigned to this argument. The value
  /// can be any valid GraphQL input value including scalars, enums, objects,
  /// lists, variables, or null depending on the argument's expected type.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn value(&self) -> &Value
  {
    &self.value
  }

  /// Creates a parser for arguments with custom name and value parsers.
  ///
  /// This parser handles the complete argument syntax including the name,
  /// colon, and value. It allows for flexible parsing of both the name and
  /// value components by accepting custom parsers for each. This enables
  /// integration with different name and value parsing strategies as needed.
  #[cfg_attr(not(tarpaulin), inline(always))]
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
    Self: CstNode<Lang>,
    Lang::Kind: Into<rowan::SyntaxKind>,
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
  Name: Parseable<'a, I, T, Error, Language = Lang> + CstNode<Lang> + Clone,
  Colon<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Value: Parseable<'a, I, T, Error, Language = Lang> + CstNode<Lang> + Clone,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
{
  type Language = Lang;

  #[cfg_attr(not(tarpaulin), inline(always))]
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
#[derive(Debug, Clone)]
pub struct Arguments<Arg, Lang>
where
  Lang: Language,
  Arg: CstNode<Lang>,
{
  syntax: SyntaxNode<Lang>,
  arguments: CstNodeChildren<Arg, Lang>,
  l_paren: LParen<TextRange, SyntaxToken<Lang>>,
  r_paren: RParen<TextRange, SyntaxToken<Lang>>,
}

impl<Arg, Lang> Arguments<Arg, Lang>
where
  Lang: Language,
  Arg: CstNode<Lang>,
  Self: CstNode<Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn new(
    syntax: SyntaxNode<Lang>,
    l_paren: LParen<TextRange, SyntaxToken<Lang>>,
    arguments: CstNodeChildren<Arg, Lang>,
    r_paren: RParen<TextRange, SyntaxToken<Lang>>,
  ) -> Self {
    Self {
      syntax,
      arguments,
      l_paren,
      r_paren,
    }
  }

  /// Tries to create a `Arguments` from the given syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the source span of the entire argument list.
  ///
  /// This span covers from the opening parenthesis through the closing
  /// parenthesis, including all arguments and whitespace within. Useful for
  /// error reporting, source mapping, and extracting the complete argument text.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node representing the entire argument list.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the opening parenthesis token.
  ///
  /// This provides access to the `(` character that begins the argument list,
  /// including its exact source position. Useful for syntax highlighting,
  /// parenthesis matching, and precise error reporting at argument boundaries.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn l_paren_token(&self) -> &LParen<TextRange, SyntaxToken<Lang>> {
    &self.l_paren
  }

  /// Returns the closing parenthesis token.
  ///
  /// This provides access to the `)` character that ends the argument list,
  /// including its exact source position. Useful for syntax highlighting,
  /// parenthesis matching, and detecting incomplete argument lists.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn r_paren_token(&self) -> &RParen<TextRange, SyntaxToken<Lang>> {
    &self.r_paren
  }

  /// Returns the collection of arguments.
  ///
  /// This provides access to all arguments that were successfully parsed
  /// from the argument list.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn arguments(&self) -> &CstNodeChildren<Arg, Lang>
  {
    &self.arguments
  }

  /// Creates a parser for arguments using the provided argument parser.
  pub fn parser_with<'a, I, T, Error, E, AP>(
    builder: &'a SyntaxTreeBuilder<Lang>,
    arg_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> AP,
  ) -> impl Parser<'a, I, (), E> + Clone
  where
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    Arg: Parseable<'a, I, T, Error, Language = Lang>,
    LParen<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    RParen<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    AP: Parser<'a, I, (), E> + Clone,
    Lang::Kind: Into<rowan::SyntaxKind>,
    Self: CstNode<Lang>,
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
  Arg: Parseable<'a, I, T, Error, Language = Lang> + CstNode<Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  LParen<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RParen<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Self: CstNode<Lang>,
{
  type Language = Lang;

  fn parser<E>(
    builder: &'a SyntaxTreeBuilder<Self::Language>,
  ) -> impl chumsky::Parser<'a, I, (), E> + Clone
  where
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: LosslessToken<'a>,
    <<T>::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    Error: 'a,
    E: chumsky::extra::ParserExtra<'a, I, Error = Error> + 'a,
  {
    Self::parser_with(builder, Arg::parser)
  }
}
