use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

/// A node with an optional description.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Described<T, Description> {
  span: Span,
  description: Option<Description>,
  node: T,
}

impl<T, Description> core::ops::Deref for Described<T, Description> {
  type Target = T;

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.node
  }
}

impl<T, Description> core::ops::DerefMut for Described<T, Description> {
  #[inline]
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.node
  }
}

impl<T, Description> AsSpan<Span> for Described<T, Description> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<T, Description> IntoSpan<Span> for Described<T, Description> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<T, Description> IntoComponents for Described<T, Description> {
  type Components = (Span, Option<Description>, T);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.description, self.node)
  }
}

impl<T, Description> Described<T, Description> {
  /// Returns the span of the described node.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the description of the described node, if any.
  #[inline]
  pub const fn description(&self) -> Option<&Description> {
    self.description.as_ref()
  }

  /// Returns the inner node.
  #[inline]
  pub const fn node(&self) -> &T {
    &self.node
  }
}

impl<'a, Description, Node, I, T, Error> Parseable<'a, I, T, Error> for Described<Node, Description>
where
  Description: Parseable<'a, I, T, Error>,
  Node: Parseable<'a, I, T, Error>,
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
    Description::parser()
      .or_not()
      .then(Node::parser())
      .map_with(|(description, node), exa| Self {
        span: exa.span(),
        description,
        node,
      })
  }
}
