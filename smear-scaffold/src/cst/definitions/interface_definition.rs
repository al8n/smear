use logosky::{
  Logos, LosslessToken, Source, LogoStream,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, CstNodeChildren, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use smear_lexer::{keywords::Interface, punctuator::Ampersand};

#[derive(Debug, Clone)]
pub struct ImplementInterfaceMember<TypePath, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  ampersand: Option<Ampersand<TextRange, SyntaxToken<Lang>>>,
  path: TypePath,
}

impl<TypePath, Lang> ImplementInterfaceMember<TypePath, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    ampersand: Option<Ampersand<TextRange, SyntaxToken<Lang>>>,
    path: TypePath,
  ) -> Self {
    Self { syntax, ampersand, path }
  }

  /// Tries to create an `ImplementInterfaceMember` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the implement interface member.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the ampersand token if present.
  #[inline]
  pub const fn ampersand(&self) -> Option<&Ampersand<TextRange, SyntaxToken<Lang>>> {
    self.ampersand.as_ref()
  }

  /// Returns the type path of the implemented interface.
  #[inline]
  pub const fn type_path(&self) -> &TypePath {
    &self.path
  }

  fn parse_leading<'a, I, T, Error, E, P>(
    builder: &'a SyntaxTreeBuilder<Lang>,
    path_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> P,
  ) -> impl Parser<'a, I, (), E> + Clone
  where
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    Ampersand<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    P: Parser<'a, I, (), E> + Clone,
    Lang::Kind: Into<rowan::SyntaxKind>,
    Self: CstNode<Lang>,
  {
    builder.start_node(Self::KIND);
    Ampersand::parser(builder)
      .or_not()
      .ignore_then(path_parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }

  fn parse_following<'a, I, T, Error, E, P>(
    builder: &'a SyntaxTreeBuilder<Lang>,
    path_parser: impl FnOnce(&'a SyntaxTreeBuilder<Lang>) -> P,
  ) -> impl Parser<'a, I, (), E> + Clone
  where
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    Ampersand<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    P: Parser<'a, I, (), E> + Clone,
    Lang::Kind: Into<rowan::SyntaxKind>,
    Self: CstNode<Lang>,
  {
    builder.start_node(Self::KIND);
    Ampersand::parser(builder)
      .ignore_then(path_parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}


#[derive(Debug, Clone)]
pub struct ImplementsInterfaces<TypePath, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  members: CstNodeChildren<ImplementInterfaceMember<TypePath, Lang>, Lang>,
}

impl<TypePath, Lang> ImplementsInterfaces<TypePath, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    members: CstNodeChildren<ImplementInterfaceMember<TypePath, Lang>, Lang>,
  ) -> Self {
    Self { syntax, members }
  }

  /// Tries to create an `ImplementsInterfaces` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the implemented interfaces.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the children representing the implemented interfaces.
  #[inline]
  pub const fn members(&self) -> &CstNodeChildren<ImplementInterfaceMember<TypePath, Lang>, Lang> {
    &self.members
  }
}

impl<'a, TypePath, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for ImplementsInterfaces<TypePath, Lang>
where
  ImplementInterfaceMember<TypePath, Lang>: CstNode<Lang>,
  Ampersand<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  TypePath: Parseable<'a, I, T, Error, Language = Lang>,
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
    ImplementInterfaceMember::parse_leading(builder, TypePath::parser)
      .ignore_then(
        ImplementInterfaceMember::parse_following(builder, TypePath::parser)
          .repeated()
          .ignored(),
      )
      .map(|_| {
        builder.finish_node();
      })
  }
}

/// Represents an interface type definition in the CST.
#[derive(Debug, Clone)]
pub struct InterfaceTypeDefinition<Name, ImplementsInterfaces, Directives, FieldsDefinition, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  interface_kw: Interface<TextRange, SyntaxToken<Lang>>,
  name: Name,
  implements: Option<ImplementsInterfaces>,
  directives: Option<Directives>,
  fields_definition: Option<FieldsDefinition>,
}

impl<Name, ImplementsInterfaces, Directives, FieldsDefinition, Lang>
  InterfaceTypeDefinition<Name, ImplementsInterfaces, Directives, FieldsDefinition, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    interface_kw: Interface<TextRange, SyntaxToken<Lang>>,
    name: Name,
    implements: Option<ImplementsInterfaces>,
    directives: Option<Directives>,
    fields_definition: Option<FieldsDefinition>,
  ) -> Self {
    Self {
      syntax,
      interface_kw,
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
  pub const fn interface_keyword(&self) -> &Interface<TextRange, SyntaxToken<Lang>> {
    &self.interface_kw
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
  for InterfaceTypeDefinition<Name, ImplementsInterfaces, Directives, FieldsDefinition, Lang>
where
  Interface<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
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
    Interface::parser(builder)
      .ignore_then(Name::parser(builder))
      .ignore_then(ImplementsInterfaces::parser(builder).or_not())
      .ignore_then(Directives::parser(builder).or_not())
      .ignore_then(FieldsDefinition::parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}

