use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{
    CstElement, CstNode, CstNodeChildren, Parseable, SyntaxTreeBuilder,
    error::SyntaxError,
  },
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::fmt::Debug;
use smear_lexer::punctuator::{Colon, LBrace, RBrace};

/// Represents a single root operation type definition.
///
/// ## Grammar
/// ```text
/// RootOperationTypeDefinition : OperationType : NamedType
/// ```
///
/// Spec: [Root Operation Types Definition](https://spec.graphql.org/draft/#sec-Root-Operation-Types)
#[derive(Debug, Clone)]
pub struct RootOperationTypeDefinition<OperationType, Name, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  operation_type: OperationType,
  colon: Colon<TextRange, SyntaxToken<Lang>>,
  name: Name,
}

impl<OperationType, Name, Lang> RootOperationTypeDefinition<OperationType, Name, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    operation_type: OperationType,
    colon: Colon<TextRange, SyntaxToken<Lang>>,
    name: Name,
  ) -> Self {
    Self {
      syntax,
      operation_type,
      colon,
      name,
    }
  }

  /// Tries to create a `RootOperationTypeDefinition` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering this root operation type definition.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the operation type.
  #[inline]
  pub const fn operation_type(&self) -> &OperationType {
    &self.operation_type
  }

  /// Returns the colon token.
  #[inline]
  pub const fn colon_token(&self) -> &Colon<TextRange, SyntaxToken<Lang>> {
    &self.colon
  }

  /// Returns the named type.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }
}

impl<'a, OperationType, Name, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for RootOperationTypeDefinition<OperationType, Name, Lang>
where
  OperationType: Parseable<'a, I, T, Error, Language = Lang>,
  Colon<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Name: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
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
    OperationType::parser(builder)
      .ignore_then(Colon::parser(builder))
      .ignore_then(Name::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}

/// Represents a collection of root operation type definitions.
///
/// ## Grammar
/// ```text
/// RootOperationTypesDefinition : { RootOperationTypeDefinition+ }
/// ```
#[derive(Debug, Clone)]
pub struct RootOperationTypesDefinition<RootOperationTypeDefinition, Lang>
where
  Lang: Language,
  RootOperationTypeDefinition: CstNode<Language = Lang>,
{
  syntax: SyntaxNode<Lang>,
  l_brace: LBrace<TextRange, SyntaxToken<Lang>>,
  definitions: CstNodeChildren<RootOperationTypeDefinition>,
  r_brace: RBrace<TextRange, SyntaxToken<Lang>>,
}

impl<RootOperationTypeDefinition, Lang>
  RootOperationTypesDefinition<RootOperationTypeDefinition, Lang>
where
  Lang: Language,
  RootOperationTypeDefinition: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    l_brace: LBrace<TextRange, SyntaxToken<Lang>>,
    definitions: CstNodeChildren<RootOperationTypeDefinition>,
    r_brace: RBrace<TextRange, SyntaxToken<Lang>>,
  ) -> Self {
    Self {
      syntax,
      l_brace,
      definitions,
      r_brace,
    }
  }

  /// Tries to create a `RootOperationTypesDefinition` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self>>
  where
    Self: CstNode<Language = Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering this root operation types definition.
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
  pub const fn l_brace_token(&self) -> &LBrace<TextRange, SyntaxToken<Lang>>
  {
    &self.l_brace
  }

  /// Returns the right brace token.
  #[inline]
  pub const fn r_brace_token(&self) -> &RBrace<TextRange, SyntaxToken<Lang>>
  {
    &self.r_brace
  }

  /// Returns the collection of root operation type definitions.
  #[inline]
  pub const fn root_operation_type_definitions(
    &self,
  ) -> &CstNodeChildren<RootOperationTypeDefinition>
  {
    &self.definitions
  }
}

impl<'a, RootOperationTypeDefinition, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for RootOperationTypesDefinition<RootOperationTypeDefinition, Lang>
where
  RootOperationTypeDefinition: Parseable<'a, I, T, Error, Language = Lang> + CstNode<Language = Lang>,
  LBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
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
    LBrace::parser(builder)
      .ignore_then(
        RootOperationTypeDefinition::parser(builder)
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
