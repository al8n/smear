use logosky::{
  Logos, LosslessToken, Source, LogoStream,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use smear_lexer::keywords::Type;

/// Represents an object type definition in the CST.
#[derive(Debug, Clone)]
pub struct ObjectTypeDefinition<Name, ImplementsInterfaces, Directives, FieldsDefinition, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  type_kw: Type<TextRange, SyntaxToken<Lang>>,
  name: Name,
  implements: Option<ImplementsInterfaces>,
  directives: Option<Directives>,
  fields_definition: Option<FieldsDefinition>,
}

impl<Name, ImplementsInterfaces, Directives, FieldsDefinition, Lang>
  ObjectTypeDefinition<Name, ImplementsInterfaces, Directives, FieldsDefinition, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    type_kw: Type<TextRange, SyntaxToken<Lang>>,
    name: Name,
    implements: Option<ImplementsInterfaces>,
    directives: Option<Directives>,
    fields_definition: Option<FieldsDefinition>,
  ) -> Self {
    Self {
      syntax,
      type_kw,
      name,
      implements,
      directives,
      fields_definition,
    }
  }

  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>> {
    Self::try_cast_node(syntax)
  }

  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  #[inline]
  pub const fn type_keyword(&self) -> &Type<TextRange, SyntaxToken<Lang>> {
    &self.type_kw
  }

  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  #[inline]
  pub const fn implements(&self) -> Option<&ImplementsInterfaces> {
    self.implements.as_ref()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  #[inline]
  pub const fn fields_definition(&self) -> Option<&FieldsDefinition> {
    self.fields_definition.as_ref()
  }
}

impl<
    'a,
    Name,
    ImplementsInterfaces,
    Directives,
    FieldsDefinition,
    Lang,
    I,
    T,
    Error,
  > Parseable<'a, I, T, Error>
  for ObjectTypeDefinition<Name, ImplementsInterfaces, Directives, FieldsDefinition, Lang>
where
  Type<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Name: Parseable<'a, I, T, Error, Language = Lang>,
  ImplementsInterfaces: Parseable<'a, I, T, Error, Language = Lang>,
  Directives: Parseable<'a, I, T, Error, Language = Lang>,
  FieldsDefinition: Parseable<'a, I, T, Error, Language = Lang>,
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
    Type::parser(builder)
      .ignore_then(Name::parser(builder))
      .ignore_then(ImplementsInterfaces::parser(builder).or_not())
      .ignore_then(Directives::parser(builder).or_not())
      .ignore_then(FieldsDefinition::parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}

