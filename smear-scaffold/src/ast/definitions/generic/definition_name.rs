use logosky::{
  Logos, Source, Token, Tokenizer,
  chumsky::{Parseable, Parser, container::Container as ChumskyContainer, extra::ParserExtra},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use smear_lexer::punctuator::{Equal, LAngle, RAngle};

use super::{DefinitionTypeGenerics, DefinitionTypeParam};

use std::vec::Vec;

/// The AST for a definition name.
///
/// In the below example, `User<ID, Name = String>` is a definition name, where `User` is the identifier,
/// and `<ID, Name = String>` are the [`DefinitionTypeGenerics`].
///
/// ```graphqlx
/// type User<ID, Name = String> {
///   id: ID!
///   name: Name!
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefinitionName<Ident, Type, Container = Vec<DefinitionTypeParam<Ident, Type>>> {
  span: Span,
  ident: Ident,
  generics: Option<DefinitionTypeGenerics<Ident, Type, Container>>,
}

impl<Ident, Type, Container> AsSpan<Span> for DefinitionName<Ident, Type, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Type, Container> IntoSpan<Span> for DefinitionName<Ident, Type, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Type, Container> IntoComponents for DefinitionName<Ident, Type, Container> {
  type Components = (
    Span,
    Ident,
    Option<DefinitionTypeGenerics<Ident, Type, Container>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.ident, self.generics)
  }
}

impl<Ident, Type, Container> DefinitionName<Ident, Type, Container> {
  /// Creates a new `DefinitionName` with the given identifier and optional generics.
  #[inline]
  const fn new(
    span: Span,
    ident: Ident,
    generics: Option<DefinitionTypeGenerics<Ident, Type, Container>>,
  ) -> Self {
    Self {
      span,
      ident,
      generics,
    }
  }

  /// Returns the span of the definition name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the name of the definition.
  #[inline]
  pub const fn name(&self) -> &Ident {
    &self.ident
  }

  /// Returns the optional generics of the definition name.
  #[inline]
  pub const fn generics(&self) -> Option<&DefinitionTypeGenerics<Ident, Type, Container>> {
    self.generics.as_ref()
  }

  /// Returns a parser for the definition name.
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, IP, TP>(
    ident_parser: IP,
    type_parser: TP,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    IP: Parser<'a, I, Ident, E> + Clone + 'a,
    TP: Parser<'a, I, Type, E> + Clone + 'a,
    Equal: Parseable<'a, I, T, Error> + 'a,
    LAngle: Parseable<'a, I, T, Error> + 'a,
    RAngle: Parseable<'a, I, T, Error> + 'a,
    Container: ChumskyContainer<DefinitionTypeParam<Ident, Type>>,
  {
    ident_parser
      .clone()
      .then(DefinitionTypeGenerics::parser_with(ident_parser, type_parser).or_not())
      .map_with(|(ident, generics), exa| Self::new(exa.span(), ident, generics))
  }
}

impl<'a, Ident, Type, Container, I, T, Error> Parseable<'a, I, T, Error>
  for DefinitionName<Ident, Type, Container>
where
  Equal: Parseable<'a, I, T, Error> + 'a,
  Ident: Parseable<'a, I, T, Error> + 'a,
  Type: Parseable<'a, I, T, Error> + 'a,
  LAngle: Parseable<'a, I, T, Error> + 'a,
  RAngle: Parseable<'a, I, T, Error> + 'a,
  Container: ChumskyContainer<DefinitionTypeParam<Ident, Type>> + 'a,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Self::parser_with(Ident::parser(), Type::parser())
  }
}
