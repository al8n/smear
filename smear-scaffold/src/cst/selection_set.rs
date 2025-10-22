use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser, extra::ParserExtra},
  cst::{
    CstElement, CstNode, CstToken, Parseable, SyntaxTreeBuilder, cast::{children, token}, error::CastNodeError
  },
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::marker::PhantomData;

use smear_lexer::punctuator::{LBrace, RBrace};

/// The standard selection set implementations.
pub mod standard;

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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SelectionSet<Selection, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _selection: PhantomData<Selection>,
}

impl<Selection, Lang> SelectionSet<Selection, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
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
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _selection: PhantomData,
    }
  }

  /// Tries to create a `SelectionSet` from the given syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, CastNodeError<Self>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the source span of the entire selection set.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
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

  /// Returns the container holding all selections.
  #[inline]
  pub fn selections(&self) -> logosky::cst::SyntaxNodeChildren<Selection>
  where
    Selection: CstNode<Language = Lang>,
  {
    children(self.syntax())
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
  Selection: Parseable<'a, I, T, Error, Language = Lang>,
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
    Self::parser_with(builder, Selection::parser)
  }
}
