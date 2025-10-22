use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use smear_lexer::punctuator::Equal;

pub use list::List;
pub use map::{Map, MapEntry};
pub use object::{Object, ObjectField};
pub use set::Set;

mod list;
mod map;
mod object;
mod set;

/// A GraphQL default value assignment for input parameters.
///
/// Represents the default value assignment syntax used in GraphQL variable
/// declarations, field arguments, and input type definitions. Default values
/// provide fallback values when no explicit value is provided, following
/// GraphQL's default value semantics and constant expression requirements.
///
/// ## Grammar
///
/// ```text
/// DefaultValue ::= '=' Value
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefaultInputValue<Value, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  equal: Equal<TextRange, SyntaxToken<Lang>>,
  value: Value,
}

impl<Value, Lang> DefaultInputValue<Value, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn new(
    syntax: SyntaxNode<Lang>,
    equal: Equal<TextRange, SyntaxToken<Lang>>,
    value: Value,
  ) -> Self {
    Self {
      syntax,
      equal,
      value,
    }
  }

  /// Tries to create a `DefaultInputValue` from the given syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self>>
  where
    Self: CstNode<Language = Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the source span of the entire default value assignment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node representing the default value assignment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the equal sign token.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn equal_token(&self) -> &Equal<TextRange, SyntaxToken<Lang>> {
    &self.equal
  }

  /// Returns the default value expression.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn value(&self) -> &Value {
    &self.value
  }

  /// Creates a parser for default value assignments with constant validation.
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
    Equal<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    VP: Parser<'a, I, (), E> + Clone,
    Lang::Kind: Into<rowan::SyntaxKind>,
    Self: CstNode<Language = Lang>,
  {
    builder.start_node(Self::KIND);
    Equal::parser(builder)
      .ignore_then(value_parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}

impl<'a, Value, Lang, I, T, Error> Parseable<'a, I, T, Error> for DefaultInputValue<Value, Lang>
where
  Value: Parseable<'a, I, T, Error, Language = Lang>,
  Equal<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
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
