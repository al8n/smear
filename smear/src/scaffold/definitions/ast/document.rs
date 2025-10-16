use core::marker::PhantomData;

use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{self, IterParser, Parser, extra::ParserExtra},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

/// A document consisting of a series of definitions.
///
/// This is the top-level container for GraphQL documents. It holds a collection of definitions
/// (type system definitions, executable definitions, or both) and tracks the source span.
///
/// # Type Parameters
///
/// - `Definition`: The type of definitions contained in the document
/// - `Container`: The container type for definitions (defaults to `Vec<Definition>`)
///
/// # Examples
///
/// ```rust,ignore
/// use smear::scaffold::Document;
///
/// // GraphQL type system document
/// type TypeSystemDocument<S> = Document<TypeSystemDefinitionOrExtension<S>>;
///
/// // GraphQL executable document (queries/mutations)
/// type ExecutableDocument<S> = Document<ExecutableDefinition<S>>;
///
/// // Full document with both type system and executable definitions
/// type FullDocument<S> = Document<DefinitionOrExtension<S>>;
/// ```
#[derive(Debug, Clone)]
pub struct Document<Definition, Container = std::vec::Vec<Definition>> {
  span: Span,
  definitions: Container,
  _m: PhantomData<Definition>,
}

impl<Definition, Container> AsSpan<Span> for Document<Definition, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Definition, Container> IntoSpan<Span> for Document<Definition, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Definition, Container> IntoComponents for Document<Definition, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.definitions)
  }
}

impl<Definition, Container> Document<Definition, Container> {
  /// Returns a reference to the span covering the entire document.
  ///
  /// The span represents the source location from the first character of the first definition
  /// to the last character of the last definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the container holding all definitions in the document.
  ///
  /// This allows access to the underlying container (typically `Vec<Definition>`) to iterate
  /// over or access individual definitions.
  #[inline]
  pub const fn definitions(&self) -> &Container {
    &self.definitions
  }

  /// Returns a slice of definitions in the document.
  ///
  /// This is a convenience method that works when the container type can be converted to a slice.
  /// For `Vec<Definition>`, this provides direct access to the definitions as a slice.
  #[inline]
  pub fn definitions_slice(&self) -> &[Definition]
  where
    Container: AsRef<[Definition]>,
  {
    self.definitions().as_ref()
  }
}

impl<'a, Definition, Container, I, T, Error> Parseable<'a, I, T, Error>
  for Document<Definition, Container>
where
  Container: chumsky::container::Container<Definition>,
  Definition: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Definition::parser::<E>()
      .repeated()
      .at_least(1)
      .collect()
      .map_with(|definitions, exa| Self {
        span: exa.span(),
        definitions,
        _m: PhantomData,
      })
  }
}
