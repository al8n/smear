use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser, extra::ParserExtra},
  cst::{
    CstElement, CstNode, CstNodeChildren, CstToken, Parseable, SyntaxTreeBuilder,
    error::SyntaxError,
  },
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use smear_lexer::punctuator::{LBrace, RBrace};

/// The standard selection set implementations.
pub mod standard;

/// The components of a [`SelectionSet`] syntax.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum SelectionSetSyntax {
  /// The left brace token.
  #[display("'{{'")]
  LBrace,
  /// The selections node.
  #[display("selections")]
  Selections,
  /// The right brace token.
  #[display("'}}'")]
  RBrace,
}

/// Represents a selection set in GraphQL syntax.
///
/// A selection set is a collection of fields, fragment spreads, and inline fragments
/// enclosed in curly braces. It defines what data should be fetched from a GraphQL
/// object or interface type. Selection sets are fundamental to GraphQL queries,
/// mutations, and subscriptions.
///
/// ## Grammar
///
/// ```text
/// SelectionSet : { Selection+ }
/// ```
///
/// Note: The grammar requires at least one selection (the `+` indicates one-or-more).
/// Empty selection sets `{}` are not valid in GraphQL.
///
/// Spec: [Selection Sets](https://spec.graphql.org/draft/#sec-Selection-Sets)
#[derive(Debug, Clone)]
pub struct SelectionSet<Selection, Lang>
where
  Lang: Language,
  Selection: CstNode<Language = Lang>,
{
  syntax: SyntaxNode<Lang>,
  selections: CstNodeChildren<Selection>,
  lbrace: LBrace<TextRange, SyntaxToken<Lang>>,
  rbrace: RBrace<TextRange, SyntaxToken<Lang>>,
}

impl<Selection, Lang> SelectionSet<Selection, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Selection: CstNode<Language = Lang>,
{
  /// Returns the syntax node representing the selection set.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }
}

impl<Selection, Lang> SelectionSet<Selection, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Selection: CstNode<Language = Lang>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn new(
    syntax: SyntaxNode<Lang>,
    selections: CstNodeChildren<Selection>,
    lbrace: LBrace<TextRange, SyntaxToken<Lang>>,
    rbrace: RBrace<TextRange, SyntaxToken<Lang>>,
  ) -> Self {
    Self {
      syntax,
      selections,
      lbrace,
      rbrace,
    }
  }

  /// Tries to create a `SelectionSet` from the given syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the source span of the entire selection set.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the left brace token.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn l_brace_token(&self) -> &LBrace<TextRange, SyntaxToken<Lang>>
  {
    &self.lbrace
  }

  /// Returns the right brace token.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn r_brace_token(&self) -> &RBrace<TextRange, SyntaxToken<Lang>>
  {
    &self.rbrace
  }

  /// Returns the container holding all selections.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn selections(&self) -> &CstNodeChildren<Selection> {
    &self.selections
  }

  /// Creates a parser that can parse a selection set with a custom selection parser.
  pub fn parser_with<'a, I, T, Error, E, SP>(
    builder: &'a SyntaxTreeBuilder<Lang>,
    selection_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> SP,
  ) -> impl Parser<'a, I, (), E> + Clone
  where
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    LBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    RBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    SP: Parser<'a, I, (), E> + Clone,
  {
    builder.start_node(Self::KIND);
    LBrace::parser(builder)
      .ignore_then(selection_parser(builder).repeated().at_least(1).ignored())
      .then_ignore(RBrace::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}

impl<'a, Selection, Lang, I, T, Error> Parseable<'a, I, T, Error> for SelectionSet<Selection, Lang>
where
  Selection: Parseable<'a, I, T, Error, Language = Lang> + CstNode<Language = Lang>,
  LBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  type Language = Lang;

  #[cfg_attr(not(tarpaulin), inline(always))]
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
    Self::parser_with(builder, Selection::parser)
  }
}
