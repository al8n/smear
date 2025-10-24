use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, CstNodeChildren, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use smear_lexer::{
  keywords::{As, From, Import},
  punctuator::{Asterisk, LBrace, RBrace},
};

use crate::cst::{Path, PathSegment};

/// Represents a list of import members enclosed in braces.
#[derive(Debug, Clone)]
pub struct ImportList<Ident, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  l_brace: LBrace<TextRange, SyntaxToken<Lang>>,
  members: CstNodeChildren<ImportMember<Ident, Lang>, Lang>,
  r_brace: RBrace<TextRange, SyntaxToken<Lang>>,
}

impl<Ident, Lang> ImportList<Ident, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    l_brace: LBrace<TextRange, SyntaxToken<Lang>>,
    members: CstNodeChildren<ImportMember<Ident, Lang>, Lang>,
    r_brace: RBrace<TextRange, SyntaxToken<Lang>>,
  ) -> Self {
    Self {
      syntax,
      l_brace,
      members,
      r_brace,
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
  pub const fn l_brace_token(&self) -> &LBrace<TextRange, SyntaxToken<Lang>> {
    &self.l_brace
  }

  #[inline]
  pub const fn import_members(&self) -> &CstNodeChildren<ImportMember<Ident, Lang>, Lang> {
    &self.members
  }

  #[inline]
  pub const fn r_brace_token(&self) -> &RBrace<TextRange, SyntaxToken<Lang>> {
    &self.r_brace
  }
}

impl<'a, Ident, Lang, I, T, Error> Parseable<'a, I, T, Error> for ImportList<Ident, Lang>
where
  ImportMember<Ident, Lang>: Parseable<'a, I, T, Error, Language = Lang> + CstNode<Lang>,
  LBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  RBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
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
    LBrace::parser(builder)
      .ignore_then(
        ImportMember::<Ident, Lang>::parser(builder)
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

/// Represents the clause of an import (either a list or a single member).
#[derive(Debug, Clone)]
pub struct ImportClause<Ident, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  list: Option<ImportList<Ident, Lang>>,
  member: Option<ImportMember<Ident, Lang>>,
}

impl<Ident, Lang> ImportClause<Ident, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    list: Option<ImportList<Ident, Lang>>,
    member: Option<ImportMember<Ident, Lang>>,
  ) -> Self {
    Self { syntax, list, member }
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
  pub const fn list(&self) -> Option<&ImportList<Ident, Lang>> {
    self.list.as_ref()
  }

  #[inline]
  pub const fn member(&self) -> Option<&ImportMember<Ident, Lang>> {
    self.member.as_ref()
  }
}

impl<'a, Ident, Lang, I, T, Error> Parseable<'a, I, T, Error> for ImportClause<Ident, Lang>
where
  ImportList<Ident, Lang>: Parseable<'a, I, T, Error, Language = Lang>,
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
    ImportList::<Ident, Lang>::parser(builder)
      .or_not()
      .then(ImportMember::<Ident, Lang>::parser(builder).or_not())
      .map(|_| {
        builder.finish_node();
      })
  }
}

/// Represents a complete import definition.
#[derive(Debug, Clone)]
pub struct ImportDefinition<Clause, PathNode, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  import_kw: Import<TextRange, SyntaxToken<Lang>>,
  clause: Clause,
  from_kw: From<TextRange, SyntaxToken<Lang>>,
  path: PathNode,
}

impl<Clause, PathNode, Lang> ImportDefinition<Clause, PathNode, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    import_kw: Import<TextRange, SyntaxToken<Lang>>,
    clause: Clause,
    from_kw: From<TextRange, SyntaxToken<Lang>>,
    path: PathNode,
  ) -> Self {
    Self {
      syntax,
      import_kw,
      clause,
      from_kw,
      path,
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
  pub const fn import_keyword(&self) -> &Import<TextRange, SyntaxToken<Lang>> {
    &self.import_kw
  }

  #[inline]
  pub const fn clause(&self) -> &Clause {
    &self.clause
  }

  #[inline]
  pub const fn from_keyword(&self) -> &From<TextRange, SyntaxToken<Lang>> {
    &self.from_kw
  }

  #[inline]
  pub const fn path(&self) -> &PathNode {
    &self.path
  }
}

impl<'a, Clause, PathNode, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for ImportDefinition<Clause, PathNode, Lang>
where
  Import<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Clause: Parseable<'a, I, T, Error, Language = Lang>,
  From<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  PathNode: Parseable<'a, I, T, Error, Language = Lang>,
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
    Import::parser(builder)
      .ignore_then(Clause::parser(builder))
      .ignore_then(From::parser(builder))
      .ignore_then(PathNode::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}


/// Represents a single import member (named or wildcard).
#[derive(Debug, Clone)]
pub struct ImportMember<Name, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  star: Option<Asterisk<TextRange, SyntaxToken<Lang>>>,
  name: Option<Name>,
  as_kw: Option<As<TextRange, SyntaxToken<Lang>>>,
  alias: Option<Path<Name, Lang>>,
}

impl<Name, Lang> ImportMember<Name, Lang>
where
  Lang: Language,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    star: Option<Asterisk<TextRange, SyntaxToken<Lang>>>,
    name: Option<Name>,
    as_kw: Option<As<TextRange, SyntaxToken<Lang>>>,
    alias: Option<Path<Name, Lang>>,
  ) -> Self {
    Self {
      syntax,
      star,
      name,
      as_kw,
      alias,
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
  pub const fn star_token(&self) -> Option<&Asterisk<TextRange, SyntaxToken<Lang>>> {
    self.star.as_ref()
  }

  #[inline]
  pub const fn name(&self) -> Option<&Name> {
    self.name.as_ref()
  }

  #[inline]
  pub const fn as_keyword(&self) -> Option<&As<TextRange, SyntaxToken<Lang>>> {
    self.as_kw.as_ref()
  }

  #[inline]
  pub const fn alias(&self) -> Option<&Path<Name, Lang>> {
    self.alias.as_ref()
  }
}

impl<'a, Name, Lang, I, T, Error> Parseable<'a, I, T, Error> for ImportMember<Name, Lang>
where
  Name: Parseable<'a, I, T, Error, Language = Lang> + CstNode<Lang>,
  Asterisk<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  As<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Path<Name, Lang>: Parseable<'a, I, T, Error, Language = Lang>,
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
    Asterisk::parser(builder)
      .or_not()
      .ignore_then(Name::parser(builder).or_not())
      .ignore_then(
        As::parser(builder)
          .ignore_then(Path::<Name, Lang>::parser(builder))
          .or_not(),
      )
      .map(|_| {
        builder.finish_node();
      })
  }
}
