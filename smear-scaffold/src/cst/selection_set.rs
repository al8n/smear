use core::cell::OnceCell;

use logosky::{
  LogoStream, Logos, LosslessToken, Source, chumsky::{self, Parser, extra::ParserExtra}, cst::{
    CstElement, CstNode, CstNodeChildren, CstToken, SyntaxTreeBuilder, cast::{children, token}, error::{CstNodeMismatch, SyntaxError}
  }, utils::syntax::IncompleteSyntax
};
use rowan::{Language, SyntaxNode, SyntaxText, SyntaxToken, TextRange};


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
{
  syntax: SyntaxNode<Lang>,
  selections: OnceCell<Option<CstNodeChildren<Selection, Lang>>>,
  lbrace: OnceCell<Option<LBrace<TextRange, SyntaxToken<Lang>>>>,
  rbrace: OnceCell<Option<RBrace<TextRange, SyntaxToken<Lang>>>>,
}

impl<Selection, Lang> SelectionSet<Selection, Lang>
where
  Lang: Language,
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
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  const fn new(
    syntax: SyntaxNode<Lang>,
  ) -> Self {
    Self {
      syntax,
      selections: OnceCell::new(),
      lbrace: OnceCell::new(),
      rbrace: OnceCell::new(),
    }
  }

  /// Tries to create a `SelectionSet` from the given syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, CstNodeMismatch<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    if syntax.kind() == Self::KIND {
      Ok(Self::new(syntax))
    } else {
      Err(CstNodeMismatch::new(syntax))
    }
  }

  /// Returns the source span of the entire selection set.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax text
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn text(&self) -> SyntaxText {
    self.syntax.text()
  }

  /// Returns the left brace token.
  /// 
  /// If the left brace is missing, returns a `SelectionSetSyntax::LBrace` error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn l_brace_token(&self) -> Result<&LBrace<TextRange, SyntaxToken<Lang>>, SelectionSetSyntax>
  where
    LBrace<TextRange, SyntaxToken<Lang>>: CstToken<Lang>,
    Self: CstNode<Lang, Component = SelectionSetSyntax>,
  {
    match self.lbrace.get_or_init(|| {
      token(&self.syntax, &LBrace::KIND).map(|t| LBrace::with_content(t.text_range(), t))
    }) {
      Some(token) => Ok(token),
      None => Err(SelectionSetSyntax::LBrace),
    }
  }

  /// Returns the right brace token.
  /// 
  /// If the right brace is missing, returns a `SelectionSetSyntax::RBrace` error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn r_brace_token(&self) -> Result<&RBrace<TextRange, SyntaxToken<Lang>>, SelectionSetSyntax>
  where
    RBrace<TextRange, SyntaxToken<Lang>>: CstToken<Lang>,
    Self: CstNode<Lang, Component = SelectionSetSyntax>,
  {
    match self.rbrace.get_or_init(|| {
      token(&self.syntax, &RBrace::KIND).map(|t| RBrace::with_content(t.text_range(), t))
    }) {
      Some(token) => Ok(token),
      None => Err(SelectionSetSyntax::RBrace),
    }
  }

  /// Returns the container holding all selections.
  /// 
  /// If there are no selections, returns a `SelectionSetSyntax::Selections` error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn selections(&self) -> Result<&CstNodeChildren<Selection, Lang>, SelectionSetSyntax>
  where
    Selection: CstNode<Lang>,
    Self: CstNode<Lang, Component = SelectionSetSyntax>,
  {
    match self.selections.get_or_init(|| {
      let children = children::<Selection, Lang>(&self.syntax);
      if children.clone().next().is_some() {
        Some(children)
      } else {
        None
      }
    }) {
      Some(selections) => Ok(selections),
      None => Err(SelectionSetSyntax::Selections),
    }
  }

  /// Validates the syntax of the selection set, returning an `IncompleteSyntax` error
  /// if any required components are missing.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn validate_syntax(&self) -> Result<(), IncompleteSyntax<Self>>
  where
    LBrace<TextRange, SyntaxToken<Lang>>: CstToken<Lang>,
    RBrace<TextRange, SyntaxToken<Lang>>: CstToken<Lang>,
    Selection: CstNode<Lang>,
    Self: CstNode<Lang, Component = SelectionSetSyntax>,
  {
    match (
      self.l_brace_token(),
      self.selections(),
      self.r_brace_token(),
    ) {
      (Ok(_), Ok(_), Ok(_)) => Ok(()),
      (l, s, r) => {
        Err(IncompleteSyntax::from_iter([
          l.err(),
          s.err(),
          r.err(),
        ].into_iter().flatten()).unwrap())
      },
    }
  }

  // /// Creates a parser that can parse a selection set with a custom selection parser.
  // pub fn parser_with<'a, I, T, Error, E, SP>(
  //   builder: &'a SyntaxTreeBuilder<Lang>,
  //   selection_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> SP,
  // ) -> impl Parser<'a, I, (), E> + Clone
  // where
  //   T: LosslessToken<'a>,
  //   <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
  //   I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
  //   Error: 'a,
  //   E: ParserExtra<'a, I, Error = Error> + 'a,
  //   LBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  //   RBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  //   SP: Parser<'a, I, (), E> + Clone,
  // {
  //   builder.start_node(Self::KIND);
  //   LBrace::parser(builder)
  //     .ignore_then(selection_parser(builder).repeated().at_least(1).ignored())
  //     .then_ignore(RBrace::parser(builder))
  //     .map(|_| {
  //       builder.finish_node();
  //     })
  // }
}

// impl<'a, Selection, Lang, I, T, Error> Parseable<'a, I, T, Error> for SelectionSet<Selection, Lang>
// where
//   Selection: Parseable<'a, I, T, Error, Language = Lang> + CstNode<Lang>,
//   LBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
//   RBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
//   Lang: Language,
//   Lang::Kind: Into<rowan::SyntaxKind>,
//   Self: CstNode<Lang>,
// {
//   type Language = Lang;

//   #[cfg_attr(not(tarpaulin), inline(always))]
//   fn parser<E>(
//     builder: &'a SyntaxTreeBuilder<Self::Language>,
//   ) -> impl chumsky::Parser<'a, I, (), E> + Clone
//   where
//     I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
//     T: LosslessToken<'a>,
//     <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
//     Error: 'a,
//     E: chumsky::extra::ParserExtra<'a, I, Error = Error> + 'a,
//   {
//     Self::parser_with(builder, Selection::parser)
//   }
// }
