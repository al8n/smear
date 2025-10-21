use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser},
  cst::{CstNode, CstElement, Parseable, SyntaxTreeBuilder, cast::child},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::marker::PhantomData;
use smear_lexer::{
  keywords::Map,
  punctuator::{Bang, Colon, LBrace, RBrace},
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
  _key_ty: PhantomData<KeyType>,
  _value_ty: PhantomData<ValueType>,
}

impl<KeyType, ValueType, Lang> MapType<KeyType, ValueType, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _key_ty: PhantomData,
      _value_ty: PhantomData,
    }
  }

  /// Tries to create a new `MapType` from a syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, logosky::cst::error::CstNodeMismatch<Self>> {
    Self::try_cast(syntax)
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
  pub fn map_keyword(&self) -> Map<TextRange, SyntaxToken<Lang>>
  where
    Map<TextRange, SyntaxToken<Lang>>: CstNode<Language = Lang>,
  {
    logosky::cst::cast::token(self.syntax(), &Map::KIND)
      .map(|t| Map::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the key type.
  #[inline]
  pub fn key_type(&self) -> KeyType
  where
    KeyType: CstNode<Language = Lang>,
  {
    // First child after map keyword
    child(self.syntax()).unwrap()
  }

  /// Returns the value type.
  #[inline]
  pub fn value_type(&self) -> ValueType
  where
    ValueType: CstNode<Language = Lang>,
  {
    // Second child after key type
    let mut children = self.syntax().children();
    children.find_map(|n| ValueType::try_cast(n).ok()).unwrap()
  }

  /// Returns the bang token if present.
  #[inline]
  pub fn bang_token(&self) -> Option<Bang<TextRange, SyntaxToken<Lang>>>
  where
    Bang<TextRange, SyntaxToken<Lang>>: CstNode<Language = Lang>,
  {
    logosky::cst::cast::token(self.syntax(), &Bang::KIND)
      .map(|t| Bang::with_content(t.text_range(), t))
  }

  /// Returns whether this type is required (non-null).
  #[inline]
  pub fn required(&self) -> bool
  where
    Bang<TextRange, SyntaxToken<Lang>>: CstNode<Language = Lang>,
  {
    self.bang_token().is_some()
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
  Colon<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Bang<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
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
    builder.start_node(Self::KIND);
    Map::parser(builder)
      .ignore_then(LBrace::parser(builder))
      .ignore_then(KeyType::parser(builder))
      .then_ignore(Colon::parser(builder))
      .ignore_then(ValueType::parser(builder))
      .then_ignore(RBrace::parser(builder))
      .ignore_then(Bang::parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}
