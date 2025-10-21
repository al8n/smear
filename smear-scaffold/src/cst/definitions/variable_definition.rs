use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser, extra::ParserExtra},
  cst::{
    CstNode, CstElement, Parseable, SyntaxTreeBuilder, error::CstNodeMismatch,
    cast::{child, children, token},
  },
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::marker::PhantomData;
use smear_lexer::punctuator::{Colon, LParen, RParen};

/// Represents a variable definition in a GraphQL operation.
///
/// ## Grammar
///
/// ```text
/// VariableDefinition : Variable : Type DefaultValue? Directives?
/// ```
///
/// Spec: [Variable Definition](https://spec.graphql.org/draft/#sec-Variable-Definition)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariableDefinition<Variable, Type, DefaultValue, Directives, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _variable: PhantomData<Variable>,
  _ty: PhantomData<Type>,
  _default_value: PhantomData<DefaultValue>,
  _directives: PhantomData<Directives>,
}

impl<Variable, Type, DefaultValue, Directives, Lang>
  VariableDefinition<Variable, Type, DefaultValue, Directives, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _variable: PhantomData,
      _ty: PhantomData,
      _default_value: PhantomData,
      _directives: PhantomData,
    }
  }

  /// Tries to create a `VariableDefinition` from the given syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, CstNodeMismatch<Self>> {
    Self::try_cast(syntax)
  }

  /// Returns the span covering this variable definition.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the variable.
  #[inline]
  pub fn variable(&self) -> Variable
  where
    Variable: CstNode<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }

  /// Returns the colon token.
  #[inline]
  pub fn colon_token(&self) -> Colon<TextRange, SyntaxToken<Lang>>
  where
    Colon<TextRange, SyntaxToken<Lang>>: CstNode<Language = Lang>,
  {
    token(self.syntax(), &Colon::KIND)
      .map(|t| Colon::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the type.
  #[inline]
  pub fn ty(&self) -> Type
  where
    Type: CstNode<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }

  /// Returns the optional default value.
  #[inline]
  pub fn default_value(&self) -> Option<DefaultValue>
  where
    DefaultValue: CstNode<Language = Lang>,
  {
    child(self.syntax())
  }

  /// Returns the optional directives.
  #[inline]
  pub fn directives(&self) -> Option<Directives>
  where
    Directives: CstNode<Language = Lang>,
  {
    child(self.syntax())
  }
}

impl<'a, Variable, Type, DefaultValue, Directives, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for VariableDefinition<Variable, Type, DefaultValue, Directives, Lang>
where
  Variable: Parseable<'a, I, T, Error, Language = Lang>,
  Colon<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Type: Parseable<'a, I, T, Error, Language = Lang>,
  DefaultValue: Parseable<'a, I, T, Error, Language = Lang>,
  Directives: Parseable<'a, I, T, Error, Language = Lang>,
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
    Variable::parser(builder)
      .ignore_then(Colon::parser(builder))
      .ignore_then(Type::parser(builder))
      .ignore_then(DefaultValue::parser(builder).or_not())
      .ignore_then(Directives::parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}

/// Represents a collection of variable definitions for a GraphQL operation.
///
/// ## Grammar
///
/// ```text
/// VariablesDefinition : ( VariableDefinition+ )
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariablesDefinition<VariableDefinition, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _variable_definition: PhantomData<VariableDefinition>,
}

impl<VariableDefinition, Lang> VariablesDefinition<VariableDefinition, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _variable_definition: PhantomData,
    }
  }

  /// Tries to create a `VariablesDefinition` from the given syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, super::super::error::CstNodeMismatch<Self>> {
    Self::try_cast(syntax)
  }

  /// Returns the span covering this variables definition.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the left parenthesis token.
  #[inline]
  pub fn l_paren_token(&self) -> LParen<TextRange, SyntaxToken<Lang>>
  where
    LParen<TextRange, SyntaxToken<Lang>>: CstNode<Language = Lang>,
  {
    token(self.syntax(), &LParen::KIND)
      .map(|t| LParen::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the right parenthesis token.
  #[inline]
  pub fn r_paren_token(&self) -> RParen<TextRange, SyntaxToken<Lang>>
  where
    RParen<TextRange, SyntaxToken<Lang>>: CstNode<Language = Lang>,
  {
    token(self.syntax(), &RParen::KIND)
      .map(|t| RParen::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the collection of variable definitions.
  #[inline]
  pub fn variable_definitions(&self) -> logosky::cst::SyntaxNodeChildren<VariableDefinition>
  where
    VariableDefinition: CstNode<Language = Lang>,
  {
    children(self.syntax())
  }
}

impl<'a, VariableDefinition, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for VariablesDefinition<VariableDefinition, Lang>
where
  VariableDefinition: Parseable<'a, I, T, Error, Language = Lang>,
  LParen<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RParen<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
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
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    builder.start_node(Self::KIND);
    LParen::parser(builder)
      .ignore_then(
        VariableDefinition::parser(builder)
          .repeated()
          .at_least(1)
          .ignored(),
      )
      .then_ignore(RParen::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}
