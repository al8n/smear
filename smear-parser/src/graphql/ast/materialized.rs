//! The materialised-number view of the GraphQL value AST: **the same productions, a second tree,
//! at every width the axis reads.**
//!
//! Two enums and seven aliases. The enums are [`InputValue`] and [`ConstInputValue`], variant for
//! variant the shape of their slice twins in [`ast`](crate::graphql::ast) with `Float` carrying
//! [`f64`] and `Int` carrying `I` — [`i32`], which is the width draft §3.5.1 specifies, or
//! [`i64`], the grammar-permissive reading. The aliases bind the shared list, object and
//! default-value carriers to them, at the arity and argument positions their slice twins publish.
//! Every leaf type is the *same* type on both sides, and every production is the same production —
//! see [`syntactic::materialized`](crate::graphql::syntactic::value::materialized), whose entries
//! are [`super`]'s bodies at this tree.
//!
//! # Why a second tree and not a second instantiation of one
//!
//! Because a Rust type alias is not a module. Making the slice `InputValue<S>` an alias of a
//! wider carrier — one tree, two instantiations, no second enum — costs
//! `use ast::InputValue::{Int, String}`, which is `E0432` against an alias and compiles against an
//! enum. It also has to be got exactly right in three other ways at once: a parameter default
//! does not constrain an unmentioned parameter during variant construction, and inserting the
//! payloads ahead of `List`'s `Container` argument silently reinterprets `List<S, MyContainer>`.
//!
//! Two nominal enums have none of those problems and one cost — the variant lists are written
//! twice. `every_value_tree_declares_the_same_variants` pays for that cost with a wildcard-free
//! census on every tree, rather than with a comment asking the next reader to keep them in step.
//! `graphql/ast/value.rs` **was byte-identical to the revision before this axis existed**, which
//! was a stronger compatibility statement than any test. It no longer is, and the edit that ended
//! it is not this axis reaching across: it is the iterative release in
//! [`nesting`](crate::value::nesting), applied to both trees at once because the process abort it
//! removes belongs to the *shape* — a value holding a container of values — and not to the numeric
//! payload that tells these two files apart.
//!
//! # Why the *width* is a parameter here and not a third and fourth enum
//!
//! Every clause of the paragraph above is about `ast::InputValue<S>`, and not one of them is true
//! of this tree. `E0432` is a fact about a type **alias**, and `I` is a parameter on an `enum`, so
//! `use materialized::InputValue::{Int, String}` compiles exactly as it did. The other two are
//! facts about an **established, publicly constructed** type: this one arrived with the
//! `materialized-numbers` feature and has no out-of-crate constructor to break, and `Container`
//! cannot be silently rebound because there is no default on `I` for a positional argument to slip
//! past — every site names the width.
//!
//! A shape that shipped first got this wrong and paid twice: a `materialized32` module was these
//! two enums and these seven aliases again with one type substituted, and a `MaterializedNumbers32`
//! marker was the same production module again beside it. What the second copy bought was a
//! `git diff` in which the interesting line and the transcribed one look alike.
//!
//! # No default on `I`, deliberately
//!
//! A default would make `materialized::InputValue<S>` mean *some* width, and the two candidates
//! are both wrong: `i64` writes the permissive reading into every unannotated spelling, and `i32`
//! silently changes what an existing `i64` spelling means. It would also re-open the positional
//! hazard the section above rules out, because `List<S, Own<…>>` binds its second argument to `I`
//! the moment `I` is defaultable. The width is one word at each site, and the site is where the
//! reading is chosen.
//!
//! # What is materialised, and what is not
//!
//! **`Int` and `Float`. Nothing else.** An [`ast::StringValue`](crate::graphql::ast::StringValue)
//! still holds the source slice, escapes and all; an
//! [`ast::EnumValue`](crate::graphql::ast::EnumValue), an
//! [`ast::NullValue`](crate::graphql::ast::NullValue) and every
//! [`ast::Name`](crate::graphql::ast::Name) do too. Unescaping a string
//! would mean an owned buffer per node, and the property this view exists to keep is that
//! materialisation allocates **nothing** the slice parser did not already allocate — the two
//! parsers make the same allocations, node for node, and `materialization_allocates_nothing`
//! in the production module's tests is the measurement rather than the claim.
//!
//! There is no `OnceCell` either, and that is the same decision seen from the other side.
//! [`IntValue`] and [`FloatValue`] are `Copy`, so a lazily-filled cell would cost 8–16 bytes on
//! every numeric node whether or not anything ever read it, and it would make the tree `!Sync` —
//! which a server sharing one parsed persisted-query document across request threads cannot
//! accept.
//!
//! # The two widths, and why `Float` is not one of them
//!
//! `I` is signed at both widths because GraphQL's `IntValue` grammar carries an optional leading
//! `-` (draft §2.9.1), so no unsigned type can hold `-5`. [`i32`] is what draft §3.5.1 says an
//! `Int` **is**, and [`i64`] is the reading that takes §2.9.1's grammar at its word — it bounds
//! the digits not at all, so `2147483648` is a well-formed `IntValue` that no conformant `Int` can
//! hold. Neither is a subset of "correct": one answers *what does this document mean under the
//! specification*, the other *what did the author write*, and a refusal names its
//! [`IntWidth`](crate::graphql::error::IntWidth) so a report can say which was asked.
//!
//! `Float` is [`f64`] at both, and `f32` never appears, because GraphQL's `Float` **is** IEEE 754
//! double precision (draft §3.5.2) — a narrower float would be non-conformant rather than
//! narrower. That is why the parameter is on the `Int` leaf alone.
//!
//! # The bound this view accepts, stated rather than engineered around
//!
//! A 26-digit integer literal is *syntactically valid GraphQL*. In this view it becomes a
//! **parse** error where the specification would make it a **coercion** error, because the
//! conversion happens where the literal is read. At `i32` the same is true of `2147483648`. The
//! slice parser remains available for the full grammar and is unchanged. See
//! [`syntactic::materialized`](crate::graphql::syntactic::value::materialized) for the
//! productions and the exact error.

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::{
  BooleanValue as AstBooleanValue, DefaultVec, EnumValue as AstEnumValue, Name,
  NullValue as AstNullValue, StringValue as AstStringValue, VariableValue as AstVariableValue,
};
use crate::value::{Unnest, push_nesting, release};

/// A GraphQL integer literal, materialised as `I`.
pub type IntValue<I, Span = SimpleSpan> = super::IntValue<I, Span>;

/// A GraphQL floating-point literal, materialised as [`f64`].
///
/// It takes no width parameter and must not gain one: GraphQL's `Float` *is* IEEE 754 double
/// precision (draft §3.5.2).
pub type FloatValue<Span = SimpleSpan> = super::FloatValue<f64, Span>;

/// A GraphQL input value (executable context) whose numeric leaves are materialised.
///
/// Variant for variant [`ast::InputValue`](crate::graphql::ast::InputValue), with `Float`
/// carrying [`f64`] and `Int` carrying `I` — see the module header for the two widths. Every
/// other leaf is the *same type* the slice tree holds, not a copy of it.
/// # The by-value `unwrap_*` forms are gone, and a nested value is why
///
/// `#[unwrap(ref, ref_mut)]` and `#[try_unwrap(ref, ref_mut)]` are repeated on **every variant**
/// rather than only on the enum, which is how `derive_more` is told to stop generating the owned
/// `unwrap_list(self) -> …` and `try_unwrap_list(self) -> Result<…, Self>` pairs. Nothing else
/// changed about the variant set or the shape.
///
/// The reason is not taste. This enum has a hand-written [`Drop`] — see
/// [`nesting`](crate::value::nesting) for the abort it removes — and Rust does not let a payload be
/// moved out of a type that implements `Drop` (`E0509`). *Some* by-value accessor had to go; what
/// went is the one whose replacement is a two-character edit, since `unwrap_list_ref` and
/// `try_unwrap_list_ref` are generated exactly as before and `is_list` with them.
#[derive(Debug, Clone, PartialEq, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum InputValue<S, I> {
  /// Variable reference (e.g., `$userId`).
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Variable(AstVariableValue<S>),
  /// Boolean value (`true` or `false`).
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Boolean(AstBooleanValue<S>),
  /// String value (inline or block string).
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  String(AstStringValue<S>),
  /// Floating-point number, materialised.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Float(FloatValue),
  /// Integer number, materialised at `I`.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Int(IntValue<I>),
  /// Enum value name.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Enum(AstEnumValue<S>),
  /// The `null` literal.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Null(AstNullValue<S>),
  /// List of values.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  List(List<S, I>),
  /// Object value with named fields.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Object(Object<S, I>),
}

impl<S, I> Unnest for InputValue<S, I> {
  #[inline]
  fn nests(&self) -> bool {
    matches!(self, Self::List(_) | Self::Object(_))
  }

  fn unnest(&mut self, pending: &mut std::vec::Vec<Self>) {
    match self {
      Self::Variable(_)
      | Self::Boolean(_)
      | Self::String(_)
      | Self::Float(_)
      | Self::Int(_)
      | Self::Enum(_)
      | Self::Null(_) => {}
      Self::List(list) => push_nesting(pending, list.values_mut().drain(..)),
      Self::Object(object) => push_nesting(
        pending,
        // A field is `(span, name, value)` and only the value can nest; the name is a leaf and is
        // released here. `ObjectField` has no `Drop`, so it can still be taken apart by move.
        object
          .fields_mut()
          .drain(..)
          .map(|field| field.into_components().2),
      ),
    }
  }
}

impl<S, I> Drop for InputValue<S, I> {
  #[inline]
  fn drop(&mut self) {
    release(self);
  }
}

impl<S, I> AsSpan<SimpleSpan> for InputValue<S, I> {
  #[inline]
  fn as_span(&self) -> &SimpleSpan {
    match self {
      Self::Variable(v) => v.as_span(),
      Self::Boolean(v) => v.as_span(),
      Self::String(v) => v.as_span(),
      Self::Float(v) => v.as_span(),
      Self::Int(v) => v.as_span(),
      Self::Enum(v) => v.as_span(),
      Self::Null(v) => v.as_span(),
      Self::List(v) => v.as_span(),
      Self::Object(v) => v.as_span(),
    }
  }
}

impl<S, I> IntoSpan<SimpleSpan> for InputValue<S, I> {
  /// The same span the borrowing accessor answers, copied out.
  ///
  /// It used to reach the span by matching `self` by value and moving each payload out, which
  /// `E0509` forbids now that this enum has a [`Drop`]. [`SimpleSpan`] is [`Copy`], so the answer
  /// is identical and the value is released on the way out exactly as it was before.
  #[inline]
  fn into_span(self) -> SimpleSpan {
    *self.as_span()
  }
}

/// A GraphQL constant input value (schema context) whose numeric leaves are materialised.
///
/// Variant for variant [`ast::ConstInputValue`](crate::graphql::ast::ConstInputValue).
///
/// The per-variant `unwrap` attributes and the [`Drop`] below are [`InputValue`]'s, for the reason
/// stated there.
#[derive(Debug, Clone, PartialEq, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ConstInputValue<S, I> {
  /// Boolean value (`true` or `false`).
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Boolean(AstBooleanValue<S>),
  /// String value (inline or block string).
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  String(AstStringValue<S>),
  /// Floating-point number, materialised.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Float(FloatValue),
  /// Integer number, materialised at `I`.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Int(IntValue<I>),
  /// Enum value name.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Enum(AstEnumValue<S>),
  /// The `null` literal.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Null(AstNullValue<S>),
  /// List of constant values.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  List(ConstList<S, I>),
  /// Object value with named fields (all values must be constant).
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Object(ConstObject<S, I>),
}

impl<S, I> Unnest for ConstInputValue<S, I> {
  #[inline]
  fn nests(&self) -> bool {
    matches!(self, Self::List(_) | Self::Object(_))
  }

  fn unnest(&mut self, pending: &mut std::vec::Vec<Self>) {
    match self {
      Self::Boolean(_)
      | Self::String(_)
      | Self::Float(_)
      | Self::Int(_)
      | Self::Enum(_)
      | Self::Null(_) => {}
      Self::List(list) => push_nesting(pending, list.values_mut().drain(..)),
      Self::Object(object) => push_nesting(
        pending,
        object
          .fields_mut()
          .drain(..)
          .map(|field| field.into_components().2),
      ),
    }
  }
}

impl<S, I> Drop for ConstInputValue<S, I> {
  #[inline]
  fn drop(&mut self) {
    release(self);
  }
}

impl<S, I> AsSpan<SimpleSpan> for ConstInputValue<S, I> {
  #[inline]
  fn as_span(&self) -> &SimpleSpan {
    match self {
      Self::Boolean(v) => v.as_span(),
      Self::String(v) => v.as_span(),
      Self::Float(v) => v.as_span(),
      Self::Int(v) => v.as_span(),
      Self::Enum(v) => v.as_span(),
      Self::Null(v) => v.as_span(),
      Self::List(v) => v.as_span(),
      Self::Object(v) => v.as_span(),
    }
  }
}

impl<S, I> IntoSpan<SimpleSpan> for ConstInputValue<S, I> {
  /// [`InputValue::into_span`](InputValue#impl-IntoSpan<SimpleSpan>-for-InputValue<S,+I>)'s body,
  /// for the same reason.
  #[inline]
  fn into_span(self) -> SimpleSpan {
    *self.as_span()
  }
}

/// A list value whose numeric leaves are materialised.
///
/// `Container` stays the **last** argument, as it is on the slice twin, so the two sets differ in
/// one inserted parameter and not in what an existing argument means.
pub type List<S, I, Container = DefaultVec<InputValue<S, I>>> =
  crate::value::List<InputValue<S, I>, SimpleSpan, Container>;

/// An object value whose numeric leaves are materialised.
pub type Object<S, I, Container = DefaultVec<ObjectField<S, I>>> =
  crate::value::Object<Name<S>, InputValue<S, I>, SimpleSpan, Container>;

/// An object field whose value's numeric leaves are materialised.
pub type ObjectField<S, I> = crate::value::ObjectField<Name<S>, InputValue<S, I>>;

/// A constant list value whose numeric leaves are materialised.
pub type ConstList<S, I, Container = DefaultVec<ConstInputValue<S, I>>> =
  crate::value::List<ConstInputValue<S, I>, SimpleSpan, Container>;

/// A constant object value whose numeric leaves are materialised.
pub type ConstObject<S, I, Container = DefaultVec<ConstObjectField<S, I>>> =
  crate::value::Object<Name<S>, ConstInputValue<S, I>, SimpleSpan, Container>;

/// A constant object field whose value's numeric leaves are materialised.
pub type ConstObjectField<S, I> = crate::value::ObjectField<Name<S>, ConstInputValue<S, I>>;

/// A default value whose numeric leaves are materialised.
pub type DefaultInputValue<S, I> = crate::value::DefaultInputValue<ConstInputValue<S, I>>;

#[cfg(test)]
mod tests;
