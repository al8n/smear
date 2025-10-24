use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, CstNodeChildren, CstToken, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use smear_lexer::punctuator::PathSeparator;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PathSegment<Ident, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  separator: Option<PathSeparator<TextRange, SyntaxToken<Lang>>>,
  ident: Ident,
}

impl<Ident, Lang> PathSegment<Ident, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    separator: Option<PathSeparator<TextRange, SyntaxToken<Lang>>>,
    ident: Ident,
  ) -> Self {
    Self {
      syntax,
      separator,
      ident,
    }
  }

  /// Tries to create a `PathSegment` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the entire path segment.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the optional path separator.
  #[inline]
  pub const fn separator(&self) -> &Option<PathSeparator<TextRange, SyntaxToken<Lang>>> {
    &self.separator
  }

  /// Returns the identifier of the path segment.
  #[inline]
  pub const fn ident(&self) -> &Ident {
    &self.ident
  }

  fn parser_leading<'a, I, T, Error, E, IP>(
    builder: &'a SyntaxTreeBuilder<Lang>,
    ident_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> IP,
  ) -> impl Parser<'a, I, (), E> + Clone
  where
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    PathSeparator<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    IP: Parser<'a, I, (), E> + Clone,
    Lang::Kind: Into<rowan::SyntaxKind>,
    Self: CstNode<Lang>,
  {
    builder.start_node(Self::KIND);
    PathSeparator::parser(builder)
      .or_not()
      .ignore_then(ident_parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }

  fn parser_following<'a, I, T, Error, E, IP>(
    builder: &'a SyntaxTreeBuilder<Lang>,
    ident_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> IP,
  ) -> impl Parser<'a, I, (), E> + Clone
  where
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    PathSeparator<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    IP: Parser<'a, I, (), E> + Clone,
    Lang::Kind: Into<rowan::SyntaxKind>,
    Self: CstNode<Lang>,
  {
    builder.start_node(Self::KIND);
    PathSeparator::parser(builder)
      .ignore_then(ident_parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}

/// A GraphQLx path in CST, which is a sequence of identifiers separated by `::`.
///
/// ## Example
/// ```text
/// user::profile
/// ::std::string
/// ```
#[derive(Debug, Clone)]
pub struct Path<Ident, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  segments: CstNodeChildren<PathSegment<Ident, Lang>, Lang>,
  fqdp: bool,
}

impl<Ident, Lang> Path<Ident, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) fn new(
    syntax: SyntaxNode<Lang>,
    segments: CstNodeChildren<PathSegment<Ident, Lang>, Lang>,
    fqdp: bool,
  ) -> Self {
    Self {
      syntax,
      segments,
      fqdp,
    }
  }

  /// Tries to create a `Path` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the entire path.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns all path segments.
  #[inline]
  pub const fn segments(&self) -> &CstNodeChildren<PathSegment<Ident, Lang>, Lang>
  {
    &self.segments
  }

  /// Returns whether the path is fully qualified (starts with `::` path separator).
  #[inline]
  pub fn is_fully_qualified(&self) -> bool
  where
    PathSeparator<TextRange, SyntaxToken<Lang>>: CstToken<Lang>,
  {
    self.fqdp
  }
}

impl<'a, Ident, Lang, I, T, Error> Parseable<'a, I, T, Error> for Path<Ident, Lang>
where
  Ident: Parseable<'a, I, T, Error, Language = Lang>,
  PathSeparator<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
  PathSegment<Ident, Lang>: CstNode<Lang>,
{
  type Language = Lang;

  #[inline]
  fn parser<E>(builder: &'a SyntaxTreeBuilder<Self::Language>) -> impl Parser<'a, I, (), E> + Clone
  where
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    builder.start_node(Self::KIND);
    PathSegment::parser_leading(builder, Ident::parser)
      .ignore_then(
        PathSegment::parser_following(builder, Ident::parser)
          .repeated()
          .ignored(),
      )
      .map(|_| {
        builder.finish_node();
      })
  }
}
