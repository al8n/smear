use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, CstNodeChildren, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use smear_lexer::{
  keywords::Union,
  punctuator::{Equal, Pipe},
};

use std::vec::Vec;

/// Represents the member types of a union definition.
#[derive(Debug, Clone)]
pub struct UnionMemberTypes<Member, Lang>
where
  Lang: Language,
  Member: CstNode<Lang>,
{
  syntax: SyntaxNode<Lang>,
  members: CstNodeChildren<Member, Lang>,
  separators: Vec<Pipe<TextRange, SyntaxToken<Lang>>>,
}

impl<Member, Lang> UnionMemberTypes<Member, Lang>
where
  Lang: Language,
  Member: CstNode<Lang>,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) fn new(
    syntax: SyntaxNode<Lang>,
    members: CstNodeChildren<Member, Lang>,
    separators: Vec<Pipe<TextRange, SyntaxToken<Lang>>>,
  ) -> Self {
    Self {
      syntax,
      members,
      separators,
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
  pub const fn members(&self) -> &CstNodeChildren<Member, Lang> {
    &self.members
  }

  #[inline]
  pub fn separators(&self) -> impl ExactSizeIterator<Item = &Pipe<TextRange, SyntaxToken<Lang>>> {
    self.separators.iter()
  }
}

impl<'a, Member, Lang, I, T, Error> Parseable<'a, I, T, Error> for UnionMemberTypes<Member, Lang>
where
  Member: Parseable<'a, I, T, Error, Language = Lang> + CstNode<Lang>,
  Pipe<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
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
    Member::parser(builder)
      .separated_by(Pipe::parser(builder))
      .at_least(1)
      .ignored()
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
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
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

