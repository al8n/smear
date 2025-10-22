use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{
    CstNode, CstToken, CstElement, Parseable, SyntaxTreeBuilder, error::CastNodeError,
    cast::{child, children, token},
  },
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::marker::PhantomData;
use smear_lexer::punctuator::{Colon, LBrace, RBrace};

/// Represents a single root operation type definition.
///
/// ## Grammar
/// ```text
/// RootOperationTypeDefinition : OperationType : NamedType
/// ```
///
/// Spec: [Root Operation Types Definition](https://spec.graphql.org/draft/#sec-Root-Operation-Types)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RootOperationTypeDefinition<OperationType, Name, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _operation_type: PhantomData<OperationType>,
  _name: PhantomData<Name>,
}

impl<OperationType, Name, Lang> RootOperationTypeDefinition<OperationType, Name, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _operation_type: PhantomData,
      _name: PhantomData,
    }
  }

  /// Tries to create a `RootOperationTypeDefinition` from the given syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, CastNodeError<Self>> {
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
  pub fn operation_type(&self) -> OperationType
  where
    OperationType: CstNode<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }

  /// Returns the colon token.
  #[inline]
  pub fn colon_token(&self) -> Colon<TextRange, SyntaxToken<Lang>>
  where
    Colon<TextRange, SyntaxToken<Lang>>: CstToken<Language = Lang>,
  {
    token(self.syntax(), &Colon::KIND)
      .map(|t| Colon::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the named type.
  #[inline]
  pub fn name(&self) -> Name
  where
    Name: CstNode<Language = Lang>,
  {
    child(self.syntax()).unwrap()
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RootOperationTypesDefinition<RootOperationTypeDefinition, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _root_operation_type_definition: PhantomData<RootOperationTypeDefinition>,
}

impl<RootOperationTypeDefinition, Lang>
  RootOperationTypesDefinition<RootOperationTypeDefinition, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _root_operation_type_definition: PhantomData,
    }
  }

  /// Tries to create a `RootOperationTypesDefinition` from the given syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, CastNodeError<Self>> {
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

  /// Returns the collection of root operation type definitions.
  #[inline]
  pub fn root_operation_type_definitions(
    &self,
  ) -> logosky::cst::SyntaxNodeChildren<RootOperationTypeDefinition>
  where
    RootOperationTypeDefinition: CstNode<Language = Lang>,
  {
    children(self.syntax())
  }
}

impl<'a, RootOperationTypeDefinition, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for RootOperationTypesDefinition<RootOperationTypeDefinition, Lang>
where
  RootOperationTypeDefinition: Parseable<'a, I, T, Error, Language = Lang>,
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
