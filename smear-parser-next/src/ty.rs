//! Type-reference carriers shared by the GraphQL-family dialect ASTs.
//!
//! These source-independent nodes retain the referenced name or element type,
//! the complete source span, and whether the reference is non-null.

use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

/// A named type reference with an optional non-null modifier.
#[derive(Debug, Clone, Copy)]
pub struct NamedType<Name, Span = SimpleSpan> {
  span: Span,
  name: Name,
  required: bool,
}

impl<Name, Span> AsSpan<Span> for NamedType<Name, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Span> IntoSpan<Span> for NamedType<Name, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Span> IntoComponents for NamedType<Name, Span> {
  type Components = (Span, Name, bool);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.required)
  }
}

impl<Name, Span> NamedType<Name, Span> {
  /// Creates a named type reference.
  #[inline]
  pub const fn new(span: Span, name: Name, required: bool) -> Self {
    Self {
      span,
      name,
      required,
    }
  }

  /// Returns the span covering the complete type reference.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the referenced type name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns whether this type reference is non-null.
  #[inline]
  pub const fn required(&self) -> bool {
    self.required
  }
}

/// A list type reference with an optional non-null modifier.
#[derive(Debug, Clone, Copy)]
pub struct ListType<Type, Span = SimpleSpan> {
  span: Span,
  ty: Type,
  required: bool,
}

impl<Type, Span> AsSpan<Span> for ListType<Type, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Type, Span> IntoSpan<Span> for ListType<Type, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Type, Span> IntoComponents for ListType<Type, Span> {
  type Components = (Span, Type, bool);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.ty, self.required)
  }
}

impl<Type, Span> ListType<Type, Span> {
  /// Creates a list type reference.
  #[inline]
  pub const fn new(span: Span, ty: Type, required: bool) -> Self {
    Self { span, ty, required }
  }

  /// Returns the span covering the complete type reference.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the element type.
  #[inline]
  pub const fn ty(&self) -> &Type {
    &self.ty
  }

  /// Returns whether this type reference is non-null.
  #[inline]
  pub const fn required(&self) -> bool {
    self.required
  }
}

#[cfg(test)]
mod tests {
  use tokora::{
    span::{AsSpan, IntoSpan},
    utils::IntoComponents,
  };

  use super::{ListType, NamedType};

  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  struct CustomSpan(u8);

  #[test]
  fn carriers_support_custom_spans() {
    let named = NamedType::<_, CustomSpan>::new(CustomSpan(1), "Name", true);
    assert_eq!(named.as_span(), &CustomSpan(1));
    assert_eq!(
      NamedType::<_, CustomSpan>::new(CustomSpan(1), "Name", true).into_span(),
      CustomSpan(1)
    );
    assert_eq!(named.into_components(), (CustomSpan(1), "Name", true));

    let list = ListType::<_, CustomSpan>::new(CustomSpan(2), "Element", false);
    assert_eq!(list.as_span(), &CustomSpan(2));
    assert_eq!(
      ListType::<_, CustomSpan>::new(CustomSpan(2), "Element", false).into_span(),
      CustomSpan(2)
    );
    assert_eq!(list.into_components(), (CustomSpan(2), "Element", false));
  }
}
