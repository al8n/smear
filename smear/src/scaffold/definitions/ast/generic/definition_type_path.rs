use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{Parser, container::Container as ChumskyContainer, extra::ParserExtra},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::punctuator::{Bang, LAngle, PathSeparator, RAngle};

use super::{super::Path, TypeGenerics};

/// A GraphQLx type path.
///
/// ## Example
///
/// ```graphqlx
/// User<ID, Name>
/// v1::Comment<ID, Name>
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefinitionTypePath<
  Ident,
  Type,
  PathSegmentContainer = Vec<Ident>,
  TypeContainer = Vec<Type>,
> {
  span: Span,
  path: Path<Ident, PathSegmentContainer>,
  generics: Option<TypeGenerics<Type, TypeContainer>>,
  required: bool,
}

impl<Ident, Type, PathSegmentContainer, TypeContainer> AsSpan<Span>
  for DefinitionTypePath<Ident, Type, PathSegmentContainer, TypeContainer>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Type, PathSegmentContainer, TypeContainer> IntoSpan<Span>
  for DefinitionTypePath<Ident, Type, PathSegmentContainer, TypeContainer>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Type, PathSegmentContainer, TypeContainer> IntoComponents
  for DefinitionTypePath<Ident, Type, PathSegmentContainer, TypeContainer>
{
  type Components = (
    Span,
    bool,
    Path<Ident, PathSegmentContainer>,
    Option<TypeGenerics<Type, TypeContainer>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.required, self.path, self.generics)
  }
}

impl<Ident, Type, PathSegmentContainer, TypeContainer>
  DefinitionTypePath<Ident, Type, PathSegmentContainer, TypeContainer>
{
  /// Creates a new path from the given segments.
  #[inline]
  const fn new(
    span: Span,
    required: bool,
    path: Path<Ident, PathSegmentContainer>,
    generics: Option<TypeGenerics<Type, TypeContainer>>,
  ) -> Self {
    Self {
      span,
      required,
      path,
      generics,
    }
  }

  /// Returns the path.
  #[inline]
  pub const fn path(&self) -> &Path<Ident, PathSegmentContainer> {
    &self.path
  }

  /// Returns `true` if the type is required (non-nullable).
  #[inline]
  pub const fn is_required(&self) -> bool {
    self.required
  }

  /// Returns the type generics.
  #[inline]
  pub const fn type_generics(&self) -> Option<&TypeGenerics<Type, TypeContainer>> {
    self.generics.as_ref()
  }

  /// Returns the span of the path.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Creates a parser for the path.
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, IP, TP>(
    ident_parser: IP,
    type_parser: TP,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    PathSeparator: Parseable<'a, I, T, Error>,
    LAngle: Parseable<'a, I, T, Error> + 'a,
    RAngle: Parseable<'a, I, T, Error> + 'a,
    Bang: Parseable<'a, I, T, Error> + 'a,
    IP: Parser<'a, I, Ident, E> + Clone + 'a,
    TP: Parser<'a, I, Type, E> + Clone + 'a,
    PathSegmentContainer: ChumskyContainer<Ident>,
    TypeContainer: ChumskyContainer<Type>,
  {
    Path::parser_with(ident_parser)
      .then(TypeGenerics::parser_with(type_parser).or_not())
      .then(Bang::parser().or_not())
      .map_with(|((path, generics), bang), exa| {
        Self::new(exa.span(), bang.is_some(), path, generics)
      })
  }
}

impl<'a, Ident, Type, PathSegmentContainer, TypeContainer, I, T, Error> Parseable<'a, I, T, Error>
  for DefinitionTypePath<Ident, Type, PathSegmentContainer, TypeContainer>
where
  PathSegmentContainer: ChumskyContainer<Ident>,
  TypeContainer: ChumskyContainer<Type>,
  Ident: Parseable<'a, I, T, Error>,
  PathSeparator: Parseable<'a, I, T, Error>,
  LAngle: Parseable<'a, I, T, Error> + 'a,
  RAngle: Parseable<'a, I, T, Error> + 'a,
  Bang: Parseable<'a, I, T, Error> + 'a,
  Type: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    T: Token<'a>,
  {
    Self::parser_with(Ident::parser(), Type::parser())
  }
}
