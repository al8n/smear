use logosky::{
  LogoStream, Logos, Source, Token,
  chumsky::{Parseable, Parser, container::Container as ChumskyContainer, extra::ParserExtra},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use smear_lexer::punctuator::{LAngle, RAngle};

use super::ExecutableDefinitionTypeGenerics;

use std::vec::Vec;

/// The AST for a definition name.
///
/// In the below example, `User<ID, Name = String>` is a definition name, where `User` is the identifier,
/// and `<ID, Name = String>` are the [`DefinitionTypeGenerics`].
///
/// ```graphqlx
/// fragment<ID, Name = String> userFragment<Name> on User<ID, Name> {
///   id,
///   name,
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExecutableDefinitionName<Ident, Container = Vec<Ident>> {
  span: Span,
  ident: Ident,
  generics: Option<ExecutableDefinitionTypeGenerics<Ident, Container>>,
}

impl<Ident, Container> AsSpan<Span> for ExecutableDefinitionName<Ident, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Container> IntoSpan<Span> for ExecutableDefinitionName<Ident, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Container> IntoComponents for ExecutableDefinitionName<Ident, Container> {
  type Components = (
    Span,
    Ident,
    Option<ExecutableDefinitionTypeGenerics<Ident, Container>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.ident, self.generics)
  }
}

impl<Ident, Container> ExecutableDefinitionName<Ident, Container> {
  /// Creates a new `ExecutableDefinitionName` with the given identifier and optional generics.
  #[inline]
  const fn new(
    span: Span,
    ident: Ident,
    generics: Option<ExecutableDefinitionTypeGenerics<Ident, Container>>,
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

  /// Returns the identifier of the definition name.
  #[inline]
  pub const fn ident(&self) -> &Ident {
    &self.ident
  }

  /// Returns the optional generics of the definition name.
  #[inline]
  pub const fn generics(&self) -> Option<&ExecutableDefinitionTypeGenerics<Ident, Container>> {
    self.generics.as_ref()
  }

  /// Returns a parser for the definition name.
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, IP>(
    ident_parser: IP,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    IP: Parser<'a, I, Ident, E> + Clone + 'a,
    LAngle: Parseable<'a, I, T, Error> + 'a,
    RAngle: Parseable<'a, I, T, Error> + 'a,
    Container: ChumskyContainer<Ident> + 'a,
  {
    ident_parser
      .clone()
      .then(ExecutableDefinitionTypeGenerics::parser_with(ident_parser).or_not())
      .map_with(|(ident, generics), exa| Self::new(exa.span(), ident, generics))
  }
}

impl<'a, Ident, Container, I, T, Error> Parseable<'a, I, T, Error>
  for ExecutableDefinitionName<Ident, Container>
where
  Ident: Parseable<'a, I, T, Error> + 'a,
  LAngle: Parseable<'a, I, T, Error> + 'a,
  RAngle: Parseable<'a, I, T, Error> + 'a,
  Container: ChumskyContainer<Ident> + 'a,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Self::parser_with(Ident::parser())
  }
}
