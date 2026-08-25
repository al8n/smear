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
//! [`Nested`], applied to both trees at once because the process abort it
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
};

use super::{
  BooleanValue as AstBooleanValue, EnumValue as AstEnumValue, Name, NullValue as AstNullValue,
  StringValue as AstStringValue, VariableValue as AstVariableValue,
};
use crate::value::{Absent, NestNode, Nestable, Nested, Sealed, Worklist};

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
/// # This enum declares no `Drop`, and that is load-bearing
///
/// Releasing a deeply nested one used to abort the process, one native frame per level
/// (al8n/smear#165). The repair is [`Nested`], the container the `List` and
/// `Object` arms hold their children in — **not** a `Drop` on this enum, which would have cost
/// every by-value `unwrap_*` and `try_unwrap_*` to `E0509`. Everything `derive_more` generated
/// before is generated now.
///
/// That repair covers every recursive position the *grammar* forms. It does not cover a node a
/// caller stored in `S` — see [`Nested`]'s own documentation and `al8n/smear#176`.
#[derive(Debug, Clone, PartialEq, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum InputValue<S, I> {
  /// Variable reference (e.g., `$userId`).
  Variable(AstVariableValue<S>),
  /// Boolean value (`true` or `false`).
  Boolean(AstBooleanValue<S>),
  /// String value (inline or block string).
  String(AstStringValue<S>),
  /// Floating-point number, materialised.
  Float(FloatValue),
  /// Integer number, materialised at `I`.
  Int(IntValue<I>),
  /// Enum value name.
  Enum(AstEnumValue<S>),
  /// The `null` literal.
  Null(AstNullValue<S>),
  /// List of values.
  List(List<S, I>),
  /// Object value with named fields.
  Object(Object<S, I>),
}

impl<S, I> Sealed for InputValue<S, I> {}

impl<S, I> NestNode for InputValue<S, I> {
  type Field = ObjectField<S, I>;
  /// GraphQL has no map literal; GraphQLx's [`InputValue`](crate::graphqlx::ast::InputValue) is
  /// the enum that fills this lane.
  type Entry = Absent<Self>;
}

impl<S, I> Nestable for InputValue<S, I> {
  type Node = Self;

  #[inline]
  fn into_children(self, worklist: &mut Worklist<Self>) {
    match self {
      // These arms hold no `InputValue`, so they are released here rather than reaching the walk.
      // What they do hold is `S` and `I` — at the crate's own arguments a source slice and an
      // integer, at a caller's whatever the caller chose, including a node this loop cannot reach
      // (al8n/smear#176; `I` has no public constructor to reach it through today).
      Self::Variable(_)
      | Self::Boolean(_)
      | Self::String(_)
      | Self::Float(_)
      | Self::Int(_)
      | Self::Enum(_)
      | Self::Null(_) => {}
      Self::List(list) => worklist.adopt(list.into_values().into_vec()),
      // A field is `(span, name, value)` and only the value holds an `InputValue`. The container
      // is handed over as it stands rather than projected onto its values: projecting is a copy of
      // every element, which is what adoption exists to remove.
      Self::Object(object) => worklist.adopt_fields(object.into_fields().into_vec()),
    }
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
  #[inline]
  fn into_span(self) -> SimpleSpan {
    match self {
      Self::Variable(v) => v.into_span(),
      Self::Boolean(v) => v.into_span(),
      Self::String(v) => v.into_span(),
      Self::Float(v) => v.into_span(),
      Self::Int(v) => v.into_span(),
      Self::Enum(v) => v.into_span(),
      Self::Null(v) => v.into_span(),
      Self::List(v) => v.into_span(),
      Self::Object(v) => v.into_span(),
    }
  }
}

/// A GraphQL constant input value (schema context) whose numeric leaves are materialised.
///
/// Variant for variant [`ast::ConstInputValue`](crate::graphql::ast::ConstInputValue).
///
/// Like [`InputValue`], it declares no `Drop`; the release is [`Nested`]'s.
#[derive(Debug, Clone, PartialEq, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ConstInputValue<S, I> {
  /// Boolean value (`true` or `false`).
  Boolean(AstBooleanValue<S>),
  /// String value (inline or block string).
  String(AstStringValue<S>),
  /// Floating-point number, materialised.
  Float(FloatValue),
  /// Integer number, materialised at `I`.
  Int(IntValue<I>),
  /// Enum value name.
  Enum(AstEnumValue<S>),
  /// The `null` literal.
  Null(AstNullValue<S>),
  /// List of constant values.
  List(ConstList<S, I>),
  /// Object value with named fields (all values must be constant).
  Object(ConstObject<S, I>),
}

impl<S, I> Sealed for ConstInputValue<S, I> {}

impl<S, I> NestNode for ConstInputValue<S, I> {
  type Field = ConstObjectField<S, I>;
  type Entry = Absent<Self>;
}

impl<S, I> Nestable for ConstInputValue<S, I> {
  type Node = Self;

  #[inline]
  fn into_children(self, worklist: &mut Worklist<Self>) {
    match self {
      Self::Boolean(_)
      | Self::String(_)
      | Self::Float(_)
      | Self::Int(_)
      | Self::Enum(_)
      | Self::Null(_) => {}
      Self::List(list) => worklist.adopt(list.into_values().into_vec()),
      Self::Object(object) => worklist.adopt_fields(object.into_fields().into_vec()),
    }
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
  #[inline]
  fn into_span(self) -> SimpleSpan {
    match self {
      Self::Boolean(v) => v.into_span(),
      Self::String(v) => v.into_span(),
      Self::Float(v) => v.into_span(),
      Self::Int(v) => v.into_span(),
      Self::Enum(v) => v.into_span(),
      Self::Null(v) => v.into_span(),
      Self::List(v) => v.into_span(),
      Self::Object(v) => v.into_span(),
    }
  }
}

/// A list value whose numeric leaves are materialised.
///
/// `Container` stays the **last** argument, as it is on the slice twin, so the two sets differ in
/// one inserted parameter and not in what an existing argument means.
pub type List<S, I, Container = Nested<InputValue<S, I>>> =
  crate::value::List<InputValue<S, I>, SimpleSpan, Container>;

/// An object value whose numeric leaves are materialised.
pub type Object<S, I, Container = Nested<ObjectField<S, I>>> =
  crate::value::Object<Name<S>, InputValue<S, I>, SimpleSpan, Container>;

/// An object field whose value's numeric leaves are materialised.
pub type ObjectField<S, I> = crate::value::ObjectField<Name<S>, InputValue<S, I>>;

/// A constant list value whose numeric leaves are materialised.
pub type ConstList<S, I, Container = Nested<ConstInputValue<S, I>>> =
  crate::value::List<ConstInputValue<S, I>, SimpleSpan, Container>;

/// A constant object value whose numeric leaves are materialised.
pub type ConstObject<S, I, Container = Nested<ConstObjectField<S, I>>> =
  crate::value::Object<Name<S>, ConstInputValue<S, I>, SimpleSpan, Container>;

/// A constant object field whose value's numeric leaves are materialised.
pub type ConstObjectField<S, I> = crate::value::ObjectField<Name<S>, ConstInputValue<S, I>>;

/// A default value whose numeric leaves are materialised.
pub type DefaultInputValue<S, I> = crate::value::DefaultInputValue<ConstInputValue<S, I>>;

#[cfg(test)]
mod tests;
