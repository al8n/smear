//! GraphQLx value nodes of the dialect's own: the forms whose payload is a literal this dialect's
//! lexer makes, or this dialect's type path, which the value carriers GraphQL shares cannot name.

use smear_lexer::graphqlx::LitChar;
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::{ConstInputValue, ConstObjectField, Leaf, Nested, TypePath};

/// A `char` value: `'a'`, one character or one escape between single quotes.
///
/// The literal is the lexer's own, so [`value`](Self::value) answers both its slice, the quotes
/// included, and the one character it spells.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CharValue<S, Span = SimpleSpan> {
  span: Span,
  value: LitChar<S>,
}

impl<S, Span> CharValue<S, Span> {
  /// Creates a `char` value from its span and the literal the lexer read there.
  #[inline]
  pub(crate) const fn new(span: Span, value: LitChar<S>) -> Self
  where
    S: Leaf,
    Span: Leaf,
  {
    Self { span, value }
  }

  /// Returns the span covering the literal, its quotes included.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the literal: its slice and the character it spells.
  #[inline]
  pub const fn value(&self) -> &LitChar<S> {
    &self.value
  }
}

impl<S, Span> AsSpan<Span> for CharValue<S, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span> IntoSpan<Span> for CharValue<S, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

/// `CharValue` taken apart: every field, in the order it is written.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CharValueComponents<S, Span> {
  /// The span covering the literal, its quotes included.
  pub span: Span,
  /// The literal, as the lexer read it.
  pub value: LitChar<S>,
}

impl<S, Span> IntoComponents for CharValue<S, Span> {
  type Components = CharValueComponents<S, Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    CharValueComponents {
      span: self.span,
      value: self.value,
    }
  }
}

/// A typed object: a type path and the fields of an object of that type, `Point { x: 1 }` or
/// `Box<i32> {}`. A schema position's form alone.
///
/// The type ascribes the object, and converts nothing: what the fields mean is the type's.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedObjectValue<S, Span = SimpleSpan, Container = Nested<ConstObjectField<S, Span>>> {
  span: Span,
  ty: TypePath<S, Span>,
  fields: Container,
}

impl<S, Span, Container> TypedObjectValue<S, Span, Container> {
  /// Creates a typed object from its span, the type path it names and its fields.
  ///
  /// The type path holds spans of this `Span` too, so the bound covers them as well.
  #[inline]
  pub const fn new(span: Span, ty: TypePath<S, Span>, fields: Container) -> Self
  where
    Span: Leaf,
  {
    Self { span, ty, fields }
  }

  /// Returns the span covering the complete typed object, its type path included.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the type path the object is of, with its type arguments.
  #[inline]
  pub const fn ty(&self) -> &TypePath<S, Span> {
    &self.ty
  }

  /// Returns the object's fields.
  #[inline]
  pub fn fields(&self) -> &[ConstObjectField<S, Span>]
  where
    Container: AsRef<[ConstObjectField<S, Span>]>,
  {
    self.fields.as_ref()
  }

  /// Consumes this typed object and returns its fields.
  #[inline]
  pub fn into_fields(self) -> Container {
    self.fields
  }
}

impl<S, Span, Container> AsSpan<Span> for TypedObjectValue<S, Span, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, Container> IntoSpan<Span> for TypedObjectValue<S, Span, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

/// `TypedObjectValue` taken apart: every field, in the order it is written.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedObjectValueComponents<S, Span, Container> {
  /// The span covering the complete typed object.
  pub span: Span,
  /// The type path the object is of.
  pub ty: TypePath<S, Span>,
  /// The object's fields.
  pub fields: Container,
}

impl<S, Span, Container> IntoComponents for TypedObjectValue<S, Span, Container> {
  type Components = TypedObjectValueComponents<S, Span, Container>;

  #[inline]
  fn into_components(self) -> Self::Components {
    TypedObjectValueComponents {
      span: self.span,
      ty: self.ty,
      fields: self.fields,
    }
  }
}

/// A typed value: a type path and the one value it ascribes, `Url("x")`. A schema position's form
/// alone.
///
/// The value is held in place, with no pointer of its own: the variant that holds this node holds
/// it behind one already, and that one pointer is what releases the value without a frame for each
/// level. So `Url("x")` costs one allocation for its two nodes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedValue<S, Span = SimpleSpan> {
  span: Span,
  ty: TypePath<S, Span>,
  value: ConstInputValue<S, Span>,
}

impl<S, Span> TypedValue<S, Span> {
  /// Creates a typed value from its span, the type path it names and the value it ascribes.
  ///
  /// The type path holds spans of this `Span` too, so the bound covers them as well.
  #[inline]
  pub const fn new(span: Span, ty: TypePath<S, Span>, value: ConstInputValue<S, Span>) -> Self
  where
    Span: Leaf,
  {
    Self { span, ty, value }
  }

  /// Returns the span covering the complete typed value, its type path included.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the type path the value is of, with its type arguments.
  #[inline]
  pub const fn ty(&self) -> &TypePath<S, Span> {
    &self.ty
  }

  /// Returns the value the type ascribes.
  #[inline]
  pub const fn value(&self) -> &ConstInputValue<S, Span> {
    &self.value
  }

  /// Consumes this typed value and returns the value the type ascribes.
  #[inline]
  pub fn into_value(self) -> ConstInputValue<S, Span> {
    self.value
  }
}

impl<S, Span> AsSpan<Span> for TypedValue<S, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span> IntoSpan<Span> for TypedValue<S, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

/// `TypedValue` taken apart: every field, in the order it is written.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedValueComponents<S, Span> {
  /// The span covering the complete typed value.
  pub span: Span,
  /// The type path the value is of.
  pub ty: TypePath<S, Span>,
  /// The value the type ascribes.
  pub value: ConstInputValue<S, Span>,
}

impl<S, Span> IntoComponents for TypedValue<S, Span> {
  type Components = TypedValueComponents<S, Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    TypedValueComponents {
      span: self.span,
      ty: self.ty,
      value: self.value,
    }
  }
}
