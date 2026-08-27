//! Name-node carrier shared by the dialect ASTs.
//!
//! This wrapper distinguishes a dialect's nominal name node from tokora's
//! general-purpose [`Ident`](tokora::types::Ident), while preserving the
//! dialect marker in its type.

use core::ops::{Deref, DerefMut};

use tokora::{
  SimpleSpan,
  error::ErrorNode,
  span::{AsSpan, IntoSpan},
  types::{
    Ident,
    recovery::{Components, FromComponents},
  },
  utils::IntoComponents,
};

/// A dialect-branded name identifier.
///
/// Dialect AST assemblies specialize `Lang` to their own marker type. The
/// wrapper otherwise preserves the identifier's source and span types.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Name<S: ?Sized, Span = SimpleSpan, Lang: ?Sized = ()>(Ident<S, Span, Lang>);

/// A dialect-branded fragment name.
///
/// GraphQL-family dialects use this nominal wrapper for the `Name but not on`
/// grammar position. Its constructor is kept within this crate so syntactic
/// productions are the single place that establish that exclusion.
///
/// Vanilla GraphQL fragments use this wrapper; GraphQLx has no fragment
/// grammar.
#[cfg(feature = "graphql")]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct FragmentName<S: ?Sized, Span = SimpleSpan, Lang: ?Sized = ()>(Name<S, Span, Lang>);

#[cfg(feature = "graphql")]
impl<S, Span, Lang: ?Sized> FragmentName<S, Span, Lang> {
  #[inline]
  pub(crate) const fn new(span: Span, source: S) -> Self {
    Self(Name::new(span, source))
  }

  /// Rebrands a parsed [`Name`] as a fragment name, carrying its recovery status across
  /// unchanged.
  ///
  /// This exists so a production that already holds a `Name` never has to take it apart and put
  /// it back together through [`new`](Self::new), which declares whatever it builds valid. It is
  /// the same reason tokora pairs its decomposition with an inverse — except that the public
  /// inverse cannot be offered *here*: [`FromComponents`] is a public trait, and an impl of it
  /// for this type would hand any caller a constructor for the one name node whose grammar rule
  /// (`Name` but not `on`) only the syntactic productions establish. That exclusion is why `new`
  /// is crate-private, and it outranks the convenience of a public rebuild, so the status-carrying
  /// route stays crate-private beside it.
  #[inline]
  pub(crate) const fn from_name(name: Name<S, Span, Lang>) -> Self {
    Self(name)
  }
}

#[cfg(feature = "graphql")]
impl<S: ?Sized, Span, Lang: ?Sized> Deref for FragmentName<S, Span, Lang> {
  type Target = Name<S, Span, Lang>;

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

#[cfg(feature = "graphql")]
impl<S: ?Sized, Span, Lang: ?Sized> DerefMut for FragmentName<S, Span, Lang> {
  #[inline]
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

#[cfg(feature = "graphql")]
impl<S: ?Sized, Span, Lang: ?Sized> AsRef<Name<S, Span, Lang>> for FragmentName<S, Span, Lang> {
  #[inline]
  fn as_ref(&self) -> &Name<S, Span, Lang> {
    &self.0
  }
}

#[cfg(feature = "graphql")]
impl<S, Span, Lang: ?Sized> PartialEq<S> for FragmentName<S, Span, Lang>
where
  S: PartialEq,
{
  #[inline]
  fn eq(&self, other: &S) -> bool {
    self.source().eq(other)
  }
}

#[cfg(feature = "graphql")]
impl<S: ?Sized, Span, Lang: ?Sized> AsSpan<Span> for FragmentName<S, Span, Lang> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

#[cfg(feature = "graphql")]
impl<S, Span, Lang: ?Sized> IntoSpan<Span> for FragmentName<S, Span, Lang> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

#[cfg(feature = "graphql")]
impl<S, Span, Lang: ?Sized> IntoComponents for FragmentName<S, Span, Lang> {
  /// The wrapped [`Name`]'s decomposition, which is the inner
  /// [`Ident`](tokora::types::Ident)'s: span, spelling, and recovery status.
  ///
  /// Reading the status out is unconditional; putting one back is not — see
  /// [`from_name`](Self::from_name) for why this type has no public inverse.
  type Components = Components<Span, S>;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<S, Span, Lang: ?Sized> Name<S, Span, Lang> {
  /// Creates a dialect name from a span and a spelling.
  ///
  /// It does not check the spelling, and the name says "valid" only of what the *productions*
  /// build: a name that was lexed is a name, because the lexer's token production is draft
  /// §2.1.9's. Nothing establishes that for a name assembled here, and a consumer that reads one
  /// as text — `graphql_proto`'s response keys, its variable lookups — has to answer that question
  /// itself rather than assume this constructor answered it.
  #[inline]
  pub const fn new(span: Span, source: S) -> Self {
    Self(Ident::new(span, source))
  }

  /// Unwraps this name into the underlying identifier.
  #[inline]
  pub fn into_ident(self) -> Ident<S, Span, Lang> {
    self.0
  }
}

impl<S: ?Sized, Span, Lang: ?Sized> Name<S, Span, Lang> {
  /// Returns the name's source spelling.
  #[inline]
  pub const fn source(&self) -> &S {
    self.0.source_ref()
  }
}

impl<S: ?Sized, Span, Lang: ?Sized> Deref for Name<S, Span, Lang> {
  type Target = Ident<S, Span, Lang>;

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl<S: ?Sized, Span, Lang: ?Sized> DerefMut for Name<S, Span, Lang> {
  #[inline]
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

impl<S: ?Sized, Span, Lang: ?Sized> AsRef<Ident<S, Span, Lang>> for Name<S, Span, Lang> {
  #[inline]
  fn as_ref(&self) -> &Ident<S, Span, Lang> {
    &self.0
  }
}

impl<S, Span, Lang: ?Sized> From<Ident<S, Span, Lang>> for Name<S, Span, Lang> {
  #[inline]
  fn from(ident: Ident<S, Span, Lang>) -> Self {
    Self(ident)
  }
}

impl<S, Span, Lang: ?Sized> From<Name<S, Span, Lang>> for Ident<S, Span, Lang> {
  #[inline]
  fn from(name: Name<S, Span, Lang>) -> Self {
    name.0
  }
}

impl<S: ?Sized, Span, Lang: ?Sized> AsSpan<Span> for Name<S, Span, Lang> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, Span, Lang: ?Sized> IntoSpan<Span> for Name<S, Span, Lang> {
  #[inline]
  fn into_span(self) -> Span {
    self.into_components().span
  }
}

impl<S, Span, Lang: ?Sized> IntoComponents for Name<S, Span, Lang> {
  /// The span, the spelling, **and the recovery status** — the inner
  /// [`Ident`](tokora::types::Ident)'s own decomposition, passed through unchanged.
  ///
  /// This carrier is `repr(transparent)` over an `Ident`, so the status is not a part it could
  /// choose to keep or drop: it is a field of the value being taken apart. Returning the pair
  /// this used to return dropped it, which is the one thing a decomposition must not do — a
  /// caller who rebuilt through [`Name::new`] got a placeholder relabelled as valid syntax.
  /// [`FromComponents`] is the inverse over this same type, so the round trip is an identity in
  /// all three states.
  type Components = Components<Span, S>;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<S, Span, Lang: ?Sized> FromComponents for Name<S, Span, Lang> {
  #[inline]
  fn from_components(components: Self::Components) -> Self {
    Self(Ident::from_components(components))
  }
}

impl<S, Span, Lang: ?Sized> ErrorNode<Span> for Name<S, Span, Lang>
where
  Ident<S, Span, Lang>: ErrorNode<Span>,
{
  #[inline]
  fn error(span: Span) -> Self {
    Self(Ident::error(span))
  }

  #[inline]
  fn missing(span: Span) -> Self {
    Self(Ident::missing(span))
  }
}

#[cfg(test)]
mod tests {
  use std::string::String;

  use tokora::{
    SimpleSpan,
    error::ErrorNode,
    span::AsSpan,
    types::{
      Ident, Status,
      recovery::{Components, FromComponents, RecoveryState},
    },
    utils::IntoComponents,
  };

  use super::Name;

  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  struct CustomSpan(u8);

  trait OtherLanguage {}

  #[test]
  fn carrier_preserves_an_arbitrary_unsized_language_marker() {
    let name = Name::<_, CustomSpan, dyn OtherLanguage>::new(CustomSpan(1), "field");
    assert_eq!(name.as_span(), &CustomSpan(1));
    assert_eq!(name.source(), &"field");

    let ident: Ident<_, CustomSpan, dyn OtherLanguage> = name.into();
    assert_eq!(ident.as_span(), &CustomSpan(1));

    let name = Name::<_, CustomSpan, dyn OtherLanguage>::new(CustomSpan(2), "field");
    let Components {
      span,
      payload,
      status,
    } = name.into_components();
    assert_eq!(span, CustomSpan(2));
    assert_eq!(payload, "field");
    assert_eq!(status, Status::Valid);
  }

  #[test]
  fn decomposing_a_placeholder_and_rebuilding_it_keeps_the_placeholder() {
    // The pair this used to decompose into had no room for the status, so the only rebuild
    // available was `Name::new`, which declares its result valid. That turned a recovery
    // placeholder into ordinary syntax on the way through.
    for (name, expected) in [
      (Name::<&str>::error(SimpleSpan::new(0, 5)), Status::Error),
      (
        Name::<&str>::missing(SimpleSpan::new(5, 5)),
        Status::Missing,
      ),
      (
        Name::<&str>::new(SimpleSpan::new(0, 5), "field"),
        Status::Valid,
      ),
    ] {
      assert_eq!(name.status(), expected);
      assert_eq!(Name::from_components(name.into_components()), name);
    }
  }

  #[test]
  fn source_borrows_non_copy_source() {
    let name = Name::<_, CustomSpan, dyn OtherLanguage>::new(CustomSpan(1), String::from("field"));
    let source: &String = name.source();
    assert_eq!(source, "field");
  }
}
