use logosky::{
  Logos, LosslessToken, Source, LogoStream,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, CstNodeChildren, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use smear_lexer::{
  keywords::Enum,
  punctuator::{LBrace, RBrace},
};

/// Represents a single enum value definition within an enum type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumValueDefinition<Value, Directives, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  value: Value,
  directives: Option<Directives>,
}

impl<Value, Directives, Lang> EnumValueDefinition<Value, Directives, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    value: Value,
    directives: Option<Directives>,
  ) -> Self {
    Self {
      syntax,
      value,
      directives,
    }
  }

  /// Tries to create an `EnumValueDefinition` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the entire enum value definition.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the enum value.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }

  /// Returns the optional directives.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }
}

impl<'a, Value, Directives, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for EnumValueDefinition<Value, Directives, Lang>
where
  Value: Parseable<'a, I, T, Error, Language = Lang>,
  Directives: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
{
  type Language = Lang;

  #[inline]
  fn parser<E>(builder: &'a SyntaxTreeBuilder<Self::Language>) -> impl Parser<'a, I, (), E> + Clone
  where
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    builder.start_node(Self::KIND);
    Value::parser(builder)
      .ignore_then(Directives::parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}

/// Represents a collection of enum value definitions.
#[derive(Debug, Clone)]
pub struct EnumValuesDefinition<ValueDefinition, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  l_brace: LBrace<TextRange, SyntaxToken<Lang>>,
  values: CstNodeChildren<ValueDefinition, Lang>,
  r_brace: RBrace<TextRange, SyntaxToken<Lang>>,
}

impl<ValueDefinition, Lang> EnumValuesDefinition<ValueDefinition, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    l_brace: LBrace<TextRange, SyntaxToken<Lang>>,
    values: CstNodeChildren<ValueDefinition, Lang>,
    r_brace: RBrace<TextRange, SyntaxToken<Lang>>,
  ) -> Self {
    Self {
      syntax,
      l_brace,
      values,
      r_brace,
    }
  }

  /// Tries to create an `EnumValuesDefinition` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the entire enum values definition.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the left brace token.
  #[inline]
  pub const fn l_brace_token(&self) -> &LBrace<TextRange, SyntaxToken<Lang>> {
    &self.l_brace
  }

  /// Returns the enum value definitions.
  #[inline]
  pub const fn values(&self) -> &CstNodeChildren<ValueDefinition, Lang> {
    &self.values
  }

  /// Returns the right brace token.
  #[inline]
  pub const fn r_brace_token(&self) -> &RBrace<TextRange, SyntaxToken<Lang>> {
    &self.r_brace
  }
}

impl<'a, ValueDefinition, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for EnumValuesDefinition<ValueDefinition, Lang>
where
  ValueDefinition: Parseable<'a, I, T, Error, Language = Lang> + CstNode<Lang>,
  LBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
{
  type Language = Lang;

  #[inline]
  fn parser<E>(builder: &'a SyntaxTreeBuilder<Self::Language>) -> impl Parser<'a, I, (), E> + Clone
  where
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    builder.start_node(Self::KIND);
    LBrace::parser(builder)
      .ignore_then(
        ValueDefinition::parser(builder)
          .repeated()
          .at_least(1)
          .ignored(),
      )
      .then_ignore(RBrace::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}

/// Represents an enum type definition in the CST.
#[derive(Debug, Clone)]
pub struct EnumTypeDefinition<Name, Directives, ValuesDefinition, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  enum_kw: Enum<TextRange, SyntaxToken<Lang>>,
  name: Name,
  directives: Option<Directives>,
  values_definition: Option<ValuesDefinition>,
}

impl<Name, Directives, ValuesDefinition, Lang>
  EnumTypeDefinition<Name, Directives, ValuesDefinition, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    enum_kw: Enum<TextRange, SyntaxToken<Lang>>,
    name: Name,
    directives: Option<Directives>,
    values_definition: Option<ValuesDefinition>,
  ) -> Self {
    Self {
      syntax,
      enum_kw,
      name,
      directives,
      values_definition,
    }
  }

  /// Tries to create an `EnumTypeDefinition` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the entire enum type definition.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the enum keyword token.
  #[inline]
  pub const fn enum_keyword(&self) -> &Enum<TextRange, SyntaxToken<Lang>> {
    &self.enum_kw
  }

  /// Returns the name of the enum type.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the directives of the enum type.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns the optional values definition of the enum type.
  #[inline]
  pub const fn values_definition(&self) -> Option<&ValuesDefinition> {
    self.values_definition.as_ref()
  }
}

impl<'a, Name, Directives, ValuesDefinition, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for EnumTypeDefinition<Name, Directives, ValuesDefinition, Lang>
where
  Enum<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Name: Parseable<'a, I, T, Error, Language = Lang>,
  Directives: Parseable<'a, I, T, Error, Language = Lang>,
  ValuesDefinition: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
{
  type Language = Lang;

  #[inline]
  fn parser<E>(builder: &'a SyntaxTreeBuilder<Self::Language>) -> impl Parser<'a, I, (), E> + Clone
  where
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    builder.start_node(Self::KIND);
    Enum::parser(builder)
      .ignore_then(Name::parser(builder))
      .ignore_then(Directives::parser(builder).or_not())
      .ignore_then(ValuesDefinition::parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}

