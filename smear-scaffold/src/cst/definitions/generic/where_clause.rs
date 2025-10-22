use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{
    CstNode, CstToken, CstElement, Parseable, SyntaxTreeBuilder,
    cast::{child, children},
    error::CastNodeError,
  },
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use super::TypePath;
use core::marker::PhantomData;
use smear_lexer::{
  keywords::Where,
  punctuator::{Ampersand, Colon},
};

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
  _ident: PhantomData<Ident>,
  _type: PhantomData<Type>,
}

impl<Ident, Type, Lang> WherePredicate<Ident, Type, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _ident: PhantomData,
      _type: PhantomData,
    }
  }

  /// Tries to create a `WherePredicate` from the given syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, CastNodeError<Self>> {
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
  pub fn bounded_type(&self) -> TypePath<Ident, Type, Lang>
  where
    TypePath<Ident, Type, Lang>: CstNode<Language = Lang>,
  {
    // First TypePath child is the bounded type
    children::<TypePath<Ident, Type, Lang>>(self.syntax())
      .next()
      .unwrap()
  }

  /// Returns the colon token separating the bounded type from the bounds.
  #[inline]
  pub fn colon_token(&self) -> Colon<TextRange, SyntaxToken<Lang>>
  where
    Colon<TextRange, SyntaxToken<Lang>>: CstToken<Language = Lang>,
  {
    logosky::cst::cast::token(self.syntax(), &Colon::KIND)
      .map(|t| Colon::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the bounds (the interfaces/types the bounded type must implement).
  #[inline]
  pub fn bounds(&self) -> impl Iterator<Item = TypePath<Ident, Type, Lang>>
  where
    TypePath<Ident, Type, Lang>: CstNode<Language = Lang>,
  {
    // Skip the first TypePath (bounded_type), rest are bounds
    children::<TypePath<Ident, Type, Lang>>(self.syntax()).skip(1)
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
  _ident: PhantomData<Ident>,
  _type: PhantomData<Type>,
}

impl<Ident, Type, Lang> WhereClause<Ident, Type, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _ident: PhantomData,
      _type: PhantomData,
    }
  }

  /// Tries to create a `WhereClause` from the given syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, CastNodeError<Self>> {
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
  pub fn where_token(&self) -> Where<TextRange, SyntaxToken<Lang>>
  where
    Where<TextRange, SyntaxToken<Lang>>: CstToken<Language = Lang>,
  {
    logosky::cst::cast::token(self.syntax(), &Where::KIND)
      .map(|t| Where::with_content(t.text_range(), t))
      .unwrap()
  }

  /// Returns the where predicates.
  #[inline]
  pub fn predicates(&self) -> logosky::cst::SyntaxNodeChildren<WherePredicate<Ident, Type, Lang>>
  where
    WherePredicate<Ident, Type, Lang>: CstNode<Language = Lang>,
  {
    children(self.syntax())
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
  _ident: PhantomData<Ident>,
  _type: PhantomData<Type>,
  _target: PhantomData<Target>,
}

impl<Ident, Type, Target, Lang> Constrained<Ident, Type, Target, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(syntax: SyntaxNode<Lang>) -> Self {
    Self {
      syntax,
      _ident: PhantomData,
      _type: PhantomData,
      _target: PhantomData,
    }
  }

  /// Tries to create a `Constrained` from the given syntax node.
  #[inline]
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, CastNodeError<Self>> {
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
  pub fn where_clause(&self) -> Option<WhereClause<Ident, Type, Lang>>
  where
    WhereClause<Ident, Type, Lang>: CstNode<Language = Lang>,
  {
    child(self.syntax())
  }

  /// Returns the target being constrained.
  #[inline]
  pub fn target(&self) -> Target
  where
    Target: CstNode<Language = Lang>,
  {
    child(self.syntax()).unwrap()
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
