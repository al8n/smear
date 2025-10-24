use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{self, Parser, extra::ParserExtra},
  cst::{
    CstElement, CstNode, CstNodeChildren, Parseable, SyntaxTreeBuilder,
    error::SyntaxError,
  },
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

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
#[derive(Debug, Clone)]
pub struct VariableDefinition<Variable, Type, DefaultValue, Directives, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  colon: Colon<TextRange, SyntaxToken<Lang>>,
  variable: Variable,
  ty: Type,
  default_value: Option<DefaultValue>,
  directives: Option<Directives>,
}

impl<Variable, Type, DefaultValue, Directives, Lang>
  VariableDefinition<Variable, Type, DefaultValue, Directives, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    variable: Variable,
    colon: Colon<TextRange, SyntaxToken<Lang>>,
    ty: Type,
    default_value: Option<DefaultValue>,
    directives: Option<Directives>,
  ) -> Self {
    Self {
      syntax,
      variable,
      colon,
      ty,
      default_value,
      directives,
    }
  }

  /// Tries to create a `VariableDefinition` from the given syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering this variable definition.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the variable.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn variable(&self) -> &Variable {
    &self.variable
  }

  /// Returns the colon token.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn colon_token(&self) -> &Colon<TextRange, SyntaxToken<Lang>>
  {
    &self.colon
  }

  /// Returns the type.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn ty(&self) -> &Type {
    &self.ty
  }

  /// Returns the optional default value.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn default_value(&self) -> Option<&DefaultValue> {
    self.default_value.as_ref()
  }

  /// Returns the optional directives.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
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
  Self: CstNode<Lang>,
{
  type Language = Lang;

  #[cfg_attr(not(tarpaulin), inline(always))]
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
#[derive(Debug, Clone)]
pub struct VariablesDefinition<VariableDefinition, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  l_paren: LParen<TextRange, SyntaxToken<Lang>>,
  variables: CstNodeChildren<VariableDefinition, Lang>,
  r_paren: RParen<TextRange, SyntaxToken<Lang>>,
}

impl<VariableDefinition, Lang> VariablesDefinition<VariableDefinition, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    l_paren: LParen<TextRange, SyntaxToken<Lang>>,
    variables: CstNodeChildren<VariableDefinition, Lang>,
    r_paren: RParen<TextRange, SyntaxToken<Lang>>,
  ) -> Self {
    Self {
      syntax,
      l_paren,
      variables,
      r_paren,
    }
  }

  /// Tries to create a `VariablesDefinition` from the given syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering this variables definition.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the left parenthesis token.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn l_paren_token(&self) -> &LParen<TextRange, SyntaxToken<Lang>> {
    &self.l_paren
  }

  /// Returns the right parenthesis token.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn r_paren_token(&self) -> &RParen<TextRange, SyntaxToken<Lang>> {
    &self.r_paren
  }

  /// Returns the collection of variable definitions.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn variable_definitions(&self) -> &CstNodeChildren<VariableDefinition, Lang> {
    &self.variables
  }
}

impl<'a, VariableDefinition, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for VariablesDefinition<VariableDefinition, Lang>
where
  VariableDefinition: Parseable<'a, I, T, Error, Language = Lang> + CstNode<Lang>,
  LParen<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RParen<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
{
  type Language = Lang;

  #[cfg_attr(not(tarpaulin), inline(always))]
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
