use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, CstNodeChildren, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use smear_lexer::{
  keywords,
  punctuator::{FatArrow, LBrace, RBrace},
};

/// A single entry in a GraphQLx map literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MapEntry<Key, Value, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  key: Key,
  fat_arrow: FatArrow<TextRange, SyntaxToken<Lang>>,
  value: Value,
}

impl<Key, Value, Lang> MapEntry<Key, Value, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    key: Key,
    fat_arrow: FatArrow<TextRange, SyntaxToken<Lang>>,
    value: Value,
  ) -> Self {
    Self {
      syntax,
      key,
      fat_arrow,
      value,
    }
  }

  /// Tries to create a `MapEntry` from the given syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the span of the map entry.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node representing the map entry.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the key of the map entry.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn key(&self) -> &Key {
    &self.key
  }

  /// Returns the fat arrow token.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn fat_arrow_token(&self) -> &FatArrow<TextRange, SyntaxToken<Lang>> {
    &self.fat_arrow
  }

  /// Returns the value of the map entry.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn value(&self) -> &Value {
    &self.value
  }

  /// Creates a parser for a single map entry.
  pub fn parser_with<'a, I, T, Error, E, KP, VP>(
    builder: &'a SyntaxTreeBuilder<Lang>,
    key_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> KP,
    value_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> VP,
  ) -> impl Parser<'a, I, (), E> + Clone
  where
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    FatArrow<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    KP: Parser<'a, I, (), E> + Clone,
    VP: Parser<'a, I, (), E> + Clone,
  {
    builder.start_node(Self::KIND);
    key_parser(builder)
      .then_ignore(FatArrow::parser(builder))
      .then(value_parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}

impl<'a, Key, Value, Lang, I, T, Error> Parseable<'a, I, T, Error> for MapEntry<Key, Value, Lang>
where
  Key: Parseable<'a, I, T, Error, Language = Lang>,
  Value: Parseable<'a, I, T, Error, Language = Lang>,
  FatArrow<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
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
    Self::parser_with(builder, Key::parser, Value::parser)
  }
}

/// A GraphQLx map literal value.
///
/// Represents a complete map literal as defined by the GraphQLx specification.
/// Map literals are collections of key-value pairs enclosed in braces with
/// the `map` keyword prefix.
///
/// ## Grammar
///
/// ```text
/// Map ::= 'map' '{' MapEntries? '}'
/// MapEntries ::= MapEntry+
/// MapEntry ::= Key '=>' Value
/// ```
#[derive(Debug, Clone)]
pub struct Map<Key, Value, Lang>
where
  Lang: Language,
  MapEntry<Key, Value, Lang>: CstNode<Language = Lang>,
{
  syntax: SyntaxNode<Lang>,
  map_keyword: keywords::Map<TextRange, SyntaxToken<Lang>>,
  l_brace: LBrace<TextRange, SyntaxToken<Lang>>,
  entries: CstNodeChildren<MapEntry<Key, Value, Lang>>,
  r_brace: RBrace<TextRange, SyntaxToken<Lang>>,
}

impl<Key, Value, Lang> Map<Key, Value, Lang>
where
  Lang: Language,
  MapEntry<Key, Value, Lang>: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    map_keyword: keywords::Map<TextRange, SyntaxToken<Lang>>,
    l_brace: LBrace<TextRange, SyntaxToken<Lang>>,
    entries: CstNodeChildren<MapEntry<Key, Value, Lang>>,
    r_brace: RBrace<TextRange, SyntaxToken<Lang>>,
  ) -> Self {
    Self {
      syntax,
      map_keyword,
      l_brace,
      entries,
      r_brace,
    }
  }

  /// Tries to create a `Map` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self>>
  where
    Self: CstNode<Language = Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the source span of the entire map literal.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node representing the map.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the map keyword.
  #[inline]
  pub const fn map_keyword(&self) -> &keywords::Map<TextRange, SyntaxToken<Lang>> {
    &self.map_keyword
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

  /// Returns the entries contained in the map.
  #[inline]
  pub const fn entries(&self) -> &CstNodeChildren<MapEntry<Key, Value, Lang>> {
    &self.entries
  }

  /// Creates a parser for GraphQLx map literals.
  pub fn parser_with<'a, I, T, Error, E, KP, VP>(
    builder: &'a SyntaxTreeBuilder<Lang>,
    key_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> KP,
    value_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> VP,
  ) -> impl Parser<'a, I, (), E> + Clone
  where
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    keywords::Map<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    LBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    RBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    FatArrow<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    KP: Parser<'a, I, (), E> + Clone,
    VP: Parser<'a, I, (), E> + Clone,
    MapEntry<Key, Value, Lang>: CstNode<Language = Lang>,
    Lang::Kind: Into<rowan::SyntaxKind>,
    Self: CstNode<Language = Lang>,
  {
    builder.start_node(Self::KIND);
    keywords::Map::parser(builder)
      .ignore_then(LBrace::parser(builder))
      .ignore_then(
        MapEntry::parser_with(builder, key_parser, value_parser)
          .repeated()
          .ignored(),
      )
      .then_ignore(RBrace::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}

impl<'a, Key, Value, Lang, I, T, Error> Parseable<'a, I, T, Error> for Map<Key, Value, Lang>
where
  Key: Parseable<'a, I, T, Error, Language = Lang>,
  Value: Parseable<'a, I, T, Error, Language = Lang>,
  keywords::Map<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  LBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  FatArrow<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  MapEntry<Key, Value, Lang>:
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
    Self::parser_with(builder, Key::parser, Value::parser)
  }
}
