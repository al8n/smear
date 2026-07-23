//! Argument-node carriers shared by the GraphQL-family dialect ASTs.
//!
//! These copied structures contain only source-independent grammar data. Dialect
//! assemblies bind their own name and value node types through public aliases.

use core::marker::PhantomData;

use std::vec::Vec;

use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

/// A GraphQL-family argument.
///
/// An argument commits to the `Name : Value` production. Its span covers the
/// argument name through the end of its value.
///
/// See the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
#[derive(Debug, Clone, Copy)]
pub struct Argument<Name, Value, Span = SimpleSpan> {
  span: Span,
  name: Name,
  value: Value,
}

impl<Name, Value, Span> AsSpan<Span> for Argument<Name, Value, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Value, Span> IntoSpan<Span> for Argument<Name, Value, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Value, Span> IntoComponents for Argument<Name, Value, Span> {
  type Components = (Span, Name, Value);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.value)
  }
}

impl<Name, Value, Span> Argument<Name, Value, Span> {
  /// Creates a GraphQL-family argument.
  #[inline]
  pub const fn new(span: Span, name: Name, value: Value) -> Self {
    Self { span, name, value }
  }

  /// Returns the span from the argument name through its value.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the argument name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the argument value.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }
}

/// A GraphQL-family argument collection.
///
/// This delimiter-free carrier stores no `(`/`)` tokens. A present list's
/// [`span`](Self::span) bounds its parentheses; an absent list is empty with a
/// zero-width span at the parser's starting offset.
///
/// See the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
///
/// ## Generic Parameters
///
/// - `Arg`: the type representing individual arguments
/// - `Container`: the collection holding the arguments (defaults to `Vec`)
/// - `Span`: the span type (defaults to [`SimpleSpan`])
#[derive(Debug, Clone, Copy)]
pub struct ArgumentList<Arg, Container = Vec<Arg>, Span = SimpleSpan> {
  span: Span,
  arguments: Container,
  _arg: PhantomData<Arg>,
}

impl<Arg, Container, Span> AsSpan<Span> for ArgumentList<Arg, Container, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Arg, Container, Span> IntoSpan<Span> for ArgumentList<Arg, Container, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Arg, Container, Span> IntoComponents for ArgumentList<Arg, Container, Span> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.arguments)
  }
}

impl<Arg, Container, Span> ArgumentList<Arg, Container, Span> {
  /// Creates an argument list from its span and parsed arguments.
  #[inline]
  pub const fn new(span: Span, arguments: Container) -> Self {
    Self {
      span,
      arguments,
      _arg: PhantomData,
    }
  }

  /// Returns the collection span.
  ///
  /// Present lists span their parentheses. Absent lists use a zero-width span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the container holding the arguments.
  #[inline]
  pub const fn arguments(&self) -> &Container {
    &self.arguments
  }
}

#[cfg(test)]
mod tests {
  use tokora::{
    span::{AsSpan, IntoSpan},
    utils::IntoComponents,
  };

  use super::{Argument, ArgumentList};
  use crate::directive::{Directive, Directives};

  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  struct CustomSpan(u8);

  #[test]
  fn carriers_support_custom_spans() {
    let argument = Argument::<_, _, CustomSpan>::new(CustomSpan(1), "name", 7_u8);
    assert_eq!(argument.as_span(), &CustomSpan(1));
    assert_eq!(argument.into_span(), CustomSpan(1));
    assert_eq!(argument.into_components(), (CustomSpan(1), "name", 7));

    let arguments =
      ArgumentList::<Argument<&str, u8, CustomSpan>, _, CustomSpan>::new(CustomSpan(2), [argument]);
    assert_eq!(arguments.as_span(), &CustomSpan(2));
    assert_eq!(arguments.into_span(), CustomSpan(2));
    let (span, [argument]) = arguments.into_components();
    assert_eq!(span, CustomSpan(2));
    assert_eq!(argument.span(), &CustomSpan(1));
    assert_eq!(argument.name(), &"name");
    assert_eq!(argument.value(), &7);

    let directive = Directive::<_, _, CustomSpan>::new(CustomSpan(3), "name", Some(7_u8));
    assert_eq!(directive.as_span(), &CustomSpan(3));
    assert_eq!(directive.into_span(), CustomSpan(3));
    assert_eq!(
      directive.into_components(),
      (CustomSpan(3), "name", Some(7))
    );

    let directives =
      Directives::<Directive<&str, u8, CustomSpan>, _, CustomSpan>::new(CustomSpan(4), [directive]);
    assert_eq!(directives.as_span(), &CustomSpan(4));
    assert_eq!(directives.into_span(), CustomSpan(4));
    let (span, [directive]) = directives.into_components();
    assert_eq!(span, CustomSpan(4));
    assert_eq!(directive.span(), &CustomSpan(3));
    assert_eq!(directive.name(), &"name");
    assert_eq!(directive.arguments(), Some(&7));
  }
}
