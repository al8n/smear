use logosky::{
  Logos, LosslessToken, Source, LogoStream,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, CstNodeChildren, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use smear_lexer::{
  keywords::Union,
  punctuator::{Equal, Pipe},
};

#[derive(Debug, Clone)]
pub struct UnionMember<Path, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  pipe: Option<Pipe<TextRange, SyntaxToken<Lang>>>,
  path: Path,
}

impl<Path, Lang> UnionMember<Path, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    pipe: Option<Pipe<TextRange, SyntaxToken<Lang>>>,
    path: Path,
  ) -> Self {
    Self { syntax, pipe, path }
  }

  /// Tries to create a `UnionMember` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the union member.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the pipe token if present.
  #[inline]
  pub const fn pipe(&self) -> Option<&Pipe<TextRange, SyntaxToken<Lang>>> {
    self.pipe.as_ref()
  }

  /// Returns the path of the union member.
  #[inline]
  pub const fn path(&self) -> &Path {
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
    Pipe<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    P: Parser<'a, I, (), E> + Clone,
    Lang::Kind: Into<rowan::SyntaxKind>,
    Self: CstNode<Lang>,
  {
    builder.start_node(Self::KIND);
    Pipe::parser(builder)
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
    Pipe<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
    P: Parser<'a, I, (), E> + Clone,
    Lang::Kind: Into<rowan::SyntaxKind>,
    Self: CstNode<Lang>,
  {
    builder.start_node(Self::KIND);
    Pipe::parser(builder)
      .ignore_then(path_parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}

/// Represents the member types of a union definition.
#[derive(Debug, Clone)]
pub struct UnionMembers<Path, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  members: CstNodeChildren<UnionMember<Path, Lang>, Lang>,
}

impl<Path, Lang> UnionMembers<Path, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) fn new(
    syntax: SyntaxNode<Lang>,
    members: CstNodeChildren<UnionMember<Path, Lang>, Lang>,
  ) -> Self {
    Self {
      syntax,
      members,
    }
  }

  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self, Lang>>
  where
    Self: CstNode<Lang>,
  {
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
  pub const fn members(&self) -> &CstNodeChildren<UnionMember<Path, Lang>, Lang> {
    &self.members
  }
}

impl<'a, Path, Lang, I, T, Error> Parseable<'a, I, T, Error> for UnionMembers<Path, Lang>
where
  Pipe<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  UnionMember<Path, Lang>: CstNode<Lang>,
  Path: Parseable<'a, I, T, Error, Language = Lang>,
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
    UnionMember::parse_leading(builder, Path::parser)
      .ignore_then(UnionMember::parse_following(builder, Path::parser).repeated().ignored())
      .map(|_| {
        builder.finish_node();
      })
  }
}

/// Represents a union type definition in the CST.
#[derive(Debug, Clone)]
pub struct UnionTypeDefinition<Name, Directives, MemberTypes, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  union_kw: Union<TextRange, SyntaxToken<Lang>>,
  name: Name,
  directives: Option<Directives>,
  equal: Equal<TextRange, SyntaxToken<Lang>>,
  members: MemberTypes,
}

impl<Name, Directives, MemberTypes, Lang>
  UnionTypeDefinition<Name, Directives, MemberTypes, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    union_kw: Union<TextRange, SyntaxToken<Lang>>,
    name: Name,
    directives: Option<Directives>,
    equal: Equal<TextRange, SyntaxToken<Lang>>,
    members: MemberTypes,
  ) -> Self {
    Self {
      syntax,
      union_kw,
      name,
      directives,
      equal,
      members,
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
  pub const fn union_keyword(&self) -> &Union<TextRange, SyntaxToken<Lang>> {
    &self.union_kw
  }

  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  #[inline]
  pub const fn equal_token(&self) -> &Equal<TextRange, SyntaxToken<Lang>> {
    &self.equal
  }

  #[inline]
  pub const fn member_types(&self) -> &MemberTypes {
    &self.members
  }
}

impl<'a, Name, Directives, MemberTypes, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for UnionTypeDefinition<Name, Directives, MemberTypes, Lang>
where
  Union<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Name: Parseable<'a, I, T, Error, Language = Lang>,
  Directives: Parseable<'a, I, T, Error, Language = Lang>,
  Equal<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  MemberTypes: Parseable<'a, I, T, Error, Language = Lang>,
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
    Union::parser(builder)
      .ignore_then(Name::parser(builder))
      .ignore_then(Directives::parser(builder).or_not())
      .ignore_then(Equal::parser(builder))
      .ignore_then(MemberTypes::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}

