use logosky::{
  Logos, LosslessToken, Source, LogoStream,
  chumsky::{self, Parser},
  cst::{CstElement, CstNode, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use smear_lexer::{
  keywords::Map,
  punctuator::{Bang, FatArrow, LBrace, RBrace},
};

/// Represents a GraphQLx map type with optional non-null modifier in CST.
///
/// ## Grammar
/// ```text
/// MapType : map { KeyType : ValueType } !?
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MapType<KeyType, ValueType, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  map_kw: Map<TextRange, SyntaxToken<Lang>>,
  l_brace: LBrace<TextRange, SyntaxToken<Lang>>,
  key_type: KeyType,
  fat_arrow: FatArrow<TextRange, SyntaxToken<Lang>>,
  value_type: ValueType,
  r_brace: RBrace<TextRange, SyntaxToken<Lang>>,
  bang: Option<Bang<TextRange, SyntaxToken<Lang>>>,
}

impl<KeyType, ValueType, Lang> MapType<KeyType, ValueType, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    map_kw: Map<TextRange, SyntaxToken<Lang>>,
    l_brace: LBrace<TextRange, SyntaxToken<Lang>>,
    key_type: KeyType,
    fat_arrow: FatArrow<TextRange, SyntaxToken<Lang>>,
    value_type: ValueType,
    r_brace: RBrace<TextRange, SyntaxToken<Lang>>,
    bang: Option<Bang<TextRange, SyntaxToken<Lang>>>,
  ) -> Self {
    Self {
      syntax,
      map_kw,
      l_brace,
      key_type,
      fat_arrow,
      value_type,
      r_brace,
      bang,
    }
  }

  /// Tries to create a new `MapType` from a syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the entire map type.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the map keyword token.
  #[inline]
  pub const fn map_keyword(&self) -> &Map<TextRange, SyntaxToken<Lang>> {
    &self.map_kw
  }

  /// Returns the left brace token.
  #[inline]
  pub const fn l_brace_token(&self) -> &LBrace<TextRange, SyntaxToken<Lang>> {
    &self.l_brace
  }

  /// Returns the key type.
  #[inline]
  pub const fn key_type(&self) -> &KeyType {
    &self.key_type
  }

  /// Returns the value type.
  #[inline]
  pub const fn value_type(&self) -> &ValueType {
    &self.value_type
  }

  /// Returns the colon token separating key and value types.
  #[inline]
  pub const fn fat_arrow_token(&self) -> &FatArrow<TextRange, SyntaxToken<Lang>> {
    &self.fat_arrow
  }

  /// Returns the right brace token.
  #[inline]
  pub const fn r_brace_token(&self) -> &RBrace<TextRange, SyntaxToken<Lang>> {
    &self.r_brace
  }

  /// Returns the bang token if present.
  #[inline]
  pub const fn bang_token(&self) -> Option<&Bang<TextRange, SyntaxToken<Lang>>> {
    self.bang.as_ref()
  }

  /// Returns whether this type is required (non-null).
  #[inline]
  pub const fn required(&self) -> bool {
    self.bang.is_some()
  }
}

impl<'a, KeyType, ValueType, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for MapType<KeyType, ValueType, Lang>
where
  KeyType: Parseable<'a, I, T, Error, Language = Lang>,
  ValueType: Parseable<'a, I, T, Error, Language = Lang>,
  Map<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  LBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  FatArrow<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Bang<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
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
    builder.start_node(Self::KIND);
    Map::parser(builder)
      .ignore_then(LBrace::parser(builder))
      .ignore_then(KeyType::parser(builder))
      .then_ignore(FatArrow::parser(builder))
      .ignore_then(ValueType::parser(builder))
      .then_ignore(RBrace::parser(builder))
      .ignore_then(Bang::parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}
