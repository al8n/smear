use logosky::{
  Logos, LosslessToken, Source, LogoStream,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::fmt::Debug;
use smear_lexer::punctuator::Colon;

/// Represents a GraphQL input value definition in CST.
///
/// An input value definition specifies a parameter that can be provided to a field,
/// directive, or input object.
///
/// ## Grammar
///
/// ```text
/// InputValueDefinition : Name : Type DefaultValue? Directives?
/// ```
///
/// Spec: [InputValueDefinition](https://spec.graphql.org/draft/#InputValueDefinition)
#[derive(Debug, Clone)]
pub struct InputValueDefinition<Name, Type, DefaultValue, Directives, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  name: Name,
  colon: Colon<TextRange, SyntaxToken<Lang>>,
  ty: Type,
  default_value: Option<DefaultValue>,
  directives: Option<Directives>,
}

impl<Name, Type, DefaultValue, Directives, Lang>
  InputValueDefinition<Name, Type, DefaultValue, Directives, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    name: Name,
    colon: Colon<TextRange, SyntaxToken<Lang>>,
    ty: Type,
    default_value: Option<DefaultValue>,
    directives: Option<Directives>,
  ) -> Self {
    Self {
      syntax,
      name,
      colon,
      ty,
      default_value,
      directives,
    }
  }

  /// Tries to create an `InputValueDefinition` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering this input value definition.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the colon token.
  #[inline]
  pub const fn colon_token(&self) -> &Colon<TextRange, SyntaxToken<Lang>>
  {
    &self.colon
  }

  /// Returns the type.
  #[inline]
  pub const fn ty(&self) -> &Type {
    &self.ty
  }

  /// Returns the optional default value.
  #[inline]
  pub const fn default_value(&self) -> Option<&DefaultValue> {
    self.default_value.as_ref()
  }

  /// Returns the optional directives.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }
}

impl<'a, Name, Type, DefaultValue, Directives, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for InputValueDefinition<Name, Type, DefaultValue, Directives, Lang>
where
  Name: Parseable<'a, I, T, Error, Language = Lang>,
  Colon<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Type: Parseable<'a, I, T, Error, Language = Lang>,
  DefaultValue: Parseable<'a, I, T, Error, Language = Lang>,
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
    Name::parser(builder)
      .ignore_then(Colon::parser(builder))
      .ignore_then(Type::parser(builder))
      .ignore_then(DefaultValue::parser(builder).or_not())
      .ignore_then(Directives::parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}
