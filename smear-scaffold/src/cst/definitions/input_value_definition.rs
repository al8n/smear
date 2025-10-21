use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{Node, Parseable, SyntaxTreeBuilder, cast::child},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use core::marker::PhantomData;
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InputValueDefinition<Name, Type, DefaultValue, Directives, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  _name: PhantomData<Name>,
  _ty: PhantomData<Type>,
  _default_value: PhantomData<DefaultValue>,
  _directives: PhantomData<Directives>,
}

impl<Name, Type, DefaultValue, Directives, Lang>
  InputValueDefinition<Name, Type, DefaultValue, Directives, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: Node<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _name: PhantomData,
      _ty: PhantomData,
      _default_value: PhantomData,
      _directives: PhantomData,
    }
  }

  /// Tries to create an `InputValueDefinition` from the given syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, super::super::error::SyntaxNodeMismatch<Self>> {
    Self::try_cast(syntax)
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
  pub fn name(&self) -> Name
  where
    Name: Node<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }

  /// Returns the colon token.
  #[inline]
  pub fn colon_token(&self) -> Colon<TextRange, SyntaxToken<Lang>>
  where
    Colon<TextRange, SyntaxToken<Lang>>: Node<Language = Lang>,
  {
    logosky::cst::cast::token(self.syntax(), &Colon::KIND)
      .map(|t| Colon::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the type.
  #[inline]
  pub fn ty(&self) -> Type
  where
    Type: Node<Language = Lang>,
  {
    child(self.syntax()).unwrap()
  }

  /// Returns the optional default value.
  #[inline]
  pub fn default_value(&self) -> Option<DefaultValue>
  where
    DefaultValue: Node<Language = Lang>,
  {
    child(self.syntax())
  }

  /// Returns the optional directives.
  #[inline]
  pub fn directives(&self) -> Option<Directives>
  where
    Directives: Node<Language = Lang>,
  {
    child(self.syntax())
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
  Self: Node<Language = Lang>,
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
