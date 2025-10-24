use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{
    CstElement, CstNode, CstNodeChildren, CstToken, Parseable, SyntaxTreeBuilder, error::SyntaxError,
  },
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use super::TypePath;
use smear_lexer::{
  keywords::Where,
  punctuator::{Ampersand, Colon},
};
use std::vec::Vec;

/// A where predicate in CST, which constrains a type to implement certain interfaces.
///
/// ## Example
/// ```text
/// T: User & Admin
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WherePredicate<Ident, Type, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  bounded_type: TypePath<Ident, Type, Lang>,
  colon: Colon<TextRange, SyntaxToken<Lang>>,
  type_paths: CstNodeChildren<TypePath<Ident, Type, Lang>>,
  separators: Vec<Ampersand<TextRange, SyntaxToken<Lang>>>,
}

impl<Ident, Type, Lang> WherePredicate<Ident, Type, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) fn new(
    syntax: SyntaxNode<Lang>,
    bounded_type: TypePath<Ident, Type, Lang>,
    colon: Colon<TextRange, SyntaxToken<Lang>>,
    type_paths: CstNodeChildren<TypePath<Ident, Type, Lang>>,
    separators: Vec<Ampersand<TextRange, SyntaxToken<Lang>>>,
  ) -> Self {
    Self {
      syntax,
      bounded_type,
      colon,
      type_paths,
      separators,
    }
  }

  /// Tries to create a `WherePredicate` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the entire where predicate.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the bounded type (the type being constrained).
  #[inline]
  pub const fn bounded_type(&self) -> &TypePath<Ident, Type, Lang> {
    &self.bounded_type
  }

  /// Returns the colon token separating the bounded type from the bounds.
  #[inline]
  pub const fn colon_token(&self) -> &Colon<TextRange, SyntaxToken<Lang>> {
    &self.colon
  }

  /// Returns the bounds (the interfaces/types the bounded type must implement).
  #[inline]
  pub fn bounds(
    &self,
  ) -> impl Iterator<Item = TypePath<Ident, Type, Lang>> + '_
  where
    TypePath<Ident, Type, Lang>: Clone,
  {
    let mut iter = self.type_paths.clone();
    // Skip the bounded type
    iter.next();
    iter
  }

  /// Returns the ampersand tokens separating bounds.
  #[inline]
  pub fn ampersand_tokens(
    &self,
  ) -> impl ExactSizeIterator<Item = &Ampersand<TextRange, SyntaxToken<Lang>>> {
    self.separators.iter()
  }
}

impl<'a, Ident, Type, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for WherePredicate<Ident, Type, Lang>
where
  TypePath<Ident, Type, Lang>: Parseable<'a, I, T, Error, Language = Lang>,
  Colon<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Ampersand<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
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
    TypePath::parser(builder)
      .then_ignore(Colon::parser(builder))
      .then(
        Ampersand::parser(builder)
          .ignore_then(TypePath::parser(builder))
          .repeated()
          .at_least(1)
          .ignored(),
      )
      .map(|_| {
        builder.finish_node();
      })
  }
}

/// A where clause in CST, which contains a list of where predicates.
///
/// ## Example
/// ```text
/// where T: User & Admin, U: Post
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WhereClause<Ident, Type, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  where_kw: Where<TextRange, SyntaxToken<Lang>>,
  predicates: CstNodeChildren<WherePredicate<Ident, Type, Lang>>,
}

impl<Ident, Type, Lang> WhereClause<Ident, Type, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    where_kw: Where<TextRange, SyntaxToken<Lang>>,
    predicates: CstNodeChildren<WherePredicate<Ident, Type, Lang>>,
  ) -> Self {
    Self {
      syntax,
      where_kw,
      predicates,
    }
  }

  /// Tries to create a `WhereClause` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the entire where clause.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the `where` keyword token.
  #[inline]
  pub const fn where_token(&self) -> &Where<TextRange, SyntaxToken<Lang>> {
    &self.where_kw
  }

  /// Returns the where predicates.
  #[inline]
  pub const fn predicates(
    &self,
  ) -> &CstNodeChildren<WherePredicate<Ident, Type, Lang>> {
    &self.predicates
  }
}

impl<'a, Ident, Type, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for WhereClause<Ident, Type, Lang>
where
  WherePredicate<Ident, Type, Lang>: Parseable<'a, I, T, Error, Language = Lang>,
  Where<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
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
    Where::parser(builder)
      .ignore_then(
        WherePredicate::parser(builder)
          .repeated()
          .at_least(1)
          .ignored(),
      )
      .map(|_| {
        builder.finish_node();
      })
  }
}

/// A constrained target in CST, which combines an optional where clause with a target.
///
/// This is used to represent definitions that can have type constraints via where clauses.
///
/// ## Example
/// ```text
/// where T: User & Admin { ... }
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constrained<Ident, Type, Target, Lang>
where
  Lang: Language,
{
  syntax: SyntaxNode<Lang>,
  where_clause: Option<WhereClause<Ident, Type, Lang>>,
  target: Target,
}

impl<Ident, Type, Target, Lang> Constrained<Ident, Type, Target, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    where_clause: Option<WhereClause<Ident, Type, Lang>>,
    target: Target,
  ) -> Self {
    Self {
      syntax,
      where_clause,
      target,
    }
  }

  /// Tries to create a `Constrained` from the given syntax node.
  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the span covering the entire constrained definition.
  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  /// Returns the syntax node.
  #[inline]
  pub const fn syntax(&self) -> &SyntaxNode<Lang> {
    &self.syntax
  }

  /// Returns the optional where clause.
  #[inline]
  pub const fn where_clause(
    &self,
  ) -> Option<&WhereClause<Ident, Type, Lang>> {
    self.where_clause.as_ref()
  }

  /// Returns the target being constrained.
  #[inline]
  pub const fn target(&self) -> &Target {
    &self.target
  }
}

impl<'a, Ident, Type, Target, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for Constrained<Ident, Type, Target, Lang>
where
  WhereClause<Ident, Type, Lang>: Parseable<'a, I, T, Error, Language = Lang>,
  Target: Parseable<'a, I, T, Error, Language = Lang>,
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
    WhereClause::parser(builder)
      .or_not()
      .ignore_then(Target::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}
