//! The materialised-number view of the GraphQL value AST: **a second set of aliases, and no new
//! types at all.**
//!
//! Every alias here binds [`ast::InputValue`](crate::graphql::ast::InputValue)'s two numeric
//! payload parameters to `i64` and `f64` and leaves everything else alone. Nothing in this module
//! declares a `struct` or an `enum`; the nodes are the same nodes the slice parser builds, at a
//! different instantiation. That is what "materialisation varies the payload" means concretely,
//! and it is why a second materialised dialect would cost nothing here either.
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
//! # Why `i64` and `f64`
//!
//! `i64` because GraphQL's `IntValue` grammar carries an optional leading `-`
//! (draft §2.9.1), so no unsigned type can hold `-5`. `f64` because GraphQL's `Float` **is** IEEE
//! 754 double precision (draft §3.5.2). Were a narrower integer ever wanted the pair would be
//! `i32` + `f64`; `f32` is non-conformant and must not follow `i32` down.
//!
//! # The bound this view accepts, stated rather than engineered around
//!
//! A 26-digit integer literal is *syntactically valid GraphQL*. In this view it becomes a
//! **parse** error where the specification would make it a **coercion** error, because the
//! conversion happens where the literal is read. The slice parser remains available for the full
//! grammar and is unchanged. See
//! [`syntactic::materialized`](crate::graphql::syntactic::value::materialized) for the
//! productions and the exact error.

use tokora::SimpleSpan;

use super::DefaultVec;

/// A GraphQL integer literal, materialised as [`i64`].
pub type IntValue<Span = SimpleSpan> = super::IntValue<i64, Span>;

/// A GraphQL floating-point literal, materialised as [`f64`].
pub type FloatValue<Span = SimpleSpan> = super::FloatValue<f64, Span>;

/// A GraphQL input value whose numeric leaves are materialised.
pub type InputValue<S> = super::InputValue<S, i64, f64>;

/// A GraphQL constant input value whose numeric leaves are materialised.
pub type ConstInputValue<S> = super::ConstInputValue<S, i64, f64>;

/// A list value whose numeric leaves are materialised.
pub type List<S, Container = DefaultVec<InputValue<S>>> = super::List<S, i64, f64, Container>;

/// An object value whose numeric leaves are materialised.
pub type Object<S, Container = DefaultVec<ObjectField<S>>> = super::Object<S, i64, f64, Container>;

/// An object field whose value's numeric leaves are materialised.
pub type ObjectField<S> = super::ObjectField<S, i64, f64>;

/// A constant list value whose numeric leaves are materialised.
pub type ConstList<S, Container = DefaultVec<ConstInputValue<S>>> =
  super::ConstList<S, i64, f64, Container>;

/// A constant object value whose numeric leaves are materialised.
pub type ConstObject<S, Container = DefaultVec<ConstObjectField<S>>> =
  super::ConstObject<S, i64, f64, Container>;

/// A constant object field whose value's numeric leaves are materialised.
pub type ConstObjectField<S> = super::ConstObjectField<S, i64, f64>;

/// A default value whose numeric leaves are materialised.
pub type DefaultInputValue<S> = super::DefaultInputValue<S, i64, f64>;
