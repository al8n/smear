use core::marker::PhantomData;

use logosky::{
  LogoStream, Logos, Source, Token,
  chumsky::{Parseable, extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

/// A node with an optional description.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Described<T, Description, Lang = ()> {
  span: Span,
  description: Option<Description>,
  node: T,
  _lang: PhantomData<Lang>,
}

impl<T, Description, Lang> core::ops::Deref for Described<T, Description, Lang> {
  type Target = T;

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.node
  }
}

impl<T, Description, Lang> core::ops::DerefMut for Described<T, Description, Lang> {
  #[inline]
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.node
  }
}

impl<T, Description, Lang> AsSpan<Span> for Described<T, Description, Lang> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<T, Description, Lang> IntoSpan<Span> for Described<T, Description, Lang> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<T, Description, Lang> IntoComponents for Described<T, Description, Lang> {
  type Components = (Span, Option<Description>, T, PhantomData<Lang>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.description, self.node, self._lang)
  }
}

impl<T, Description, Lang> Described<T, Description, Lang> {
  /// Creates a new `Described` node with the given description and inner node.
  pub const fn new(span: Span, description: Option<Description>, node: T) -> Self {
    Self {
      span,
      description,
      node,
      _lang: PhantomData,
    }
  }

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

impl<'a, Description, Node, Lang, I, T, Error> Parseable<'a, I, T, Error> for Described<Node, Description, Lang>
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
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Description::parser()
      .or_not()
      .then(Node::parser())
      .map_with(|(description, node), exa| Self {
        span: exa.span(),
        description,
        node,
        _lang: PhantomData,
      })
  }
}
