//! The driver's value representation, and the seven questions execution asks about it.
//!
//! # Why there is no `smear::proto::Value`
//!
//! An owned `Value` enum is the obvious design and it is the wrong one, for the same reason the
//! parser is generic over its source rather than owning a `String`. Draft §6 never inspects a
//! value's *content*: it asks whether a value is null, whether it is a list, how long that list
//! is, which object type it is, and it asks the service to serialise a leaf. Every one of those is
//! a question the service can already answer about the representation it has. Defining an enum
//! would force a conversion at the boundary — an allocation per leaf on a response of ten thousand
//! leaves — to answer questions that could have been asked in place.
//!
//! It would also make foreign values second-class. A wasm or FFI driver's value is a `u32` handle
//! into a table it owns; a `serde_json` driver's is a `serde_json::Value`; a database driver's may
//! be a row-and-column pair. None of those can be turned into a `smear` enum without copying, and
//! the first cannot answer any structural question *about itself at all* — `is_null` on a bare
//! `u32` is meaningless without the table.
//!
//! That last observation is why this is a trait on the **space**, not on the value. [`Values`]
//! carries `Self::Value` as an associated type and takes `&self` alongside every value it is asked
//! about, so the handle table is in scope for every question. A self-describing representation
//! implements it on a unit struct and pays nothing; a handle table implements it on the table.
//!
//! # The division of labour, stated once
//!
//! **`proto` owns structure and control flow; the driver owns representation.** Draft §6 is a
//! control-flow specification — which fields are collected, in what order they complete, where a
//! null propagates to, which path an error carries. None of that depends on how an `Int` is
//! stored. Everything in this trait is a representation question, and nothing in it is a
//! conformance decision.
//!
//! [`Values::coerce_leaf`] is the one that looks like an exception and is not. Draft §6.4.3's
//! `CoerceResult` is defined as "the result of calling the internal function provided by the type
//! system for determining the 'serialized' value" — the specification hands leaf serialisation to
//! the service by name. A `Float` may be an `f64`, a `rust_decimal::Decimal` or a string in a
//! column buffer, and only the service knows which.

/// The leaf type a value is being serialised against, for [`Values::coerce_leaf`].
///
/// The five built-in scalars get their own variants rather than arriving as
/// [`Scalar`](Leaf::Scalar) with a name, because a driver's `match` over them should be exhaustive
/// and checked, not a chain of string comparisons on a hot path. Draft §3.5.5 makes built-in-ness
/// a property of the *name*, so a schema that spells `scalar String` out still arrives here as
/// [`Leaf::String`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Leaf<'a> {
  /// The built-in `Int`: a signed 32-bit integer (draft §3.5.1).
  Int,
  /// The built-in `Float`: a finite double-precision value (draft §3.5.2).
  Float,
  /// The built-in `String` (draft §3.5.3).
  String,
  /// The built-in `Boolean` (draft §3.5.4).
  Boolean,
  /// The built-in `ID`, which serialises as a string (draft §3.5.5).
  Id,
  /// A scalar the schema declared, by name. Only the service knows how to serialise one.
  Scalar(&'a str),
  /// An enum, by name. The result must be one of its members (draft §6.4.3).
  Enum(&'a str),
}

impl Leaf<'_> {
  /// Returns the type's name as the schema spells it.
  #[inline]
  pub const fn name(&self) -> &str {
    match self {
      Self::Int => "Int",
      Self::Float => "Float",
      Self::String => "String",
      Self::Boolean => "Boolean",
      Self::Id => "ID",
      Self::Scalar(name) | Self::Enum(name) => name,
    }
  }

  /// Returns whether the leaf is an enum rather than a scalar.
  #[inline]
  pub const fn is_enum(&self) -> bool {
    matches!(self, Self::Enum(_))
  }
}

impl core::fmt::Display for Leaf<'_> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.write_str(self.name())
  }
}

/// The driver's values, and the vocabulary draft §6 uses to ask about them.
///
/// One instance is one operation's value space: it holds whatever the driver needs to interpret
/// its own handles, and it holds that operation's coerced variable values. It is passed by
/// `&mut` to every executor input that can reach a value, so a handle table may allocate, refcount
/// or intern without interior mutability and without `Arc`.
///
/// # Why each method is here
///
/// Each one discharges a numbered step of draft §6 that has no other implementation. A method that
/// could be answered from the schema is not here.
///
/// | method | draft step |
/// |---|---|
/// | [`is_null`](Values::is_null) | §6.4.3 `CompleteValue` 1–2: the non-null check, and the whole of null propagation — step 4, on what [`coerce_leaf`](Values::coerce_leaf) returned — and the null half of §6.4.1 step 5.i.i and of §6.3's directive conditions |
/// | [`as_bool`](Values::as_bool) | §6.3 `CollectFields` 3.a–3.b: `@skip(if:)` and `@include(if:)` when the condition is a variable |
/// | [`list_len`](Values::list_len) | §6.4.3 `CompleteValue` 3: "if result is not a collection of values, raise a field error" |
/// | [`list_item`](Values::list_item) | §6.4.3 `CompleteValue` 3: completing each element |
/// | [`type_name`](Values::type_name) | §6.4.3 `CompleteValue` 5 / `ResolveAbstractType` |
/// | [`coerce_leaf`](Values::coerce_leaf) | §6.4.3 `CompleteValue` 4: `CoerceResult` |
/// | [`variable`](Values::variable) | §6.4.1 `CoerceArgumentValues` 5.c–5.f, and §6.3's directive conditions |
///
/// # The meta-fields
///
/// **A driver is never asked for `__typename`.** Draft §6.4.3's `ResolveAbstractType` settles the
/// concrete object type before draft §6.3 collects a single field on it, and the executor answers
/// draft §4.4's `__typename` from that same conclusion: it lands in the response as
/// [`Node::TypeName`](crate::proto::Node::TypeName), and no
/// [`FieldRequest`](crate::proto::FieldRequest) is issued for it. That is a guarantee and not a
/// convenience. The executor has already spent the object type deciding which fragment type
/// conditions apply, so a driver free to name that type could return one the surrounding selections
/// contradict — `... on Cat` fields sitting under a `"Dog"` — and nothing downstream could tell.
///
/// `__schema` and `__type` are **not** answered by the executor. Draft §4.5 introspection is
/// outside this phase, so they arrive as ordinary fields of the query root and a driver may resolve
/// them or not: `__type` is nullable and answering `null` is a complete response, while `__schema`
/// is `__Schema!` and a null there is draft §6.4.3 step 1's field error. When introspection does
/// land, both move to the executor's side of the line and out of the driver's reach, the way
/// `__typename` already is.
///
/// # What is deliberately absent
///
/// There are **no constructors**. `proto` never builds a value, so there is no `make_int`,
/// no `make_list` and no builder — which is what keeps this trait from being a `Value` enum
/// wearing a different hat. The obvious place for the first one would have been `__typename`: a
/// hook handing the driver a `&str` to wrap. It is answered as
/// [`Node::TypeName`](crate::proto::Node::TypeName) instead, so a driver has no way to answer it
/// wrongly rather than merely no reason to. Argument values reach the driver as
/// [`ArgumentSource`](crate::proto::ArgumentSource), which is a decision and not a value `proto`
/// built: it resolves draft §6.4.1's control flow, says *which* of the sources supplies each
/// argument, and hands back either the literal the document already holds or the driver's own
/// value for a variable — read once by §6.4.1 and moved through, never minted.
pub trait Values {
  /// A resolved value: whatever the driver's representation is.
  ///
  /// The executor stores these — a completed response is a tree whose *shape* belongs to `proto`
  /// and whose *leaves* belong to the driver — so the type must be owned. A handle is owned; so is
  /// an `Rc<Json>`. Nothing here requires it to be cheap to produce, only possible.
  type Value;

  /// Returns whether the value is GraphQL `null`.
  ///
  /// Draft §6.4.3 steps 1 and 2 turn on this and nothing else, which makes it the single method
  /// the whole of §6.4.4's null propagation rests on. A driver that reports a missing key as
  /// non-null will produce a subtly wrong response and no exception anywhere; that is the failure
  /// mode this crate's oracle exists to catch, and it is worth being exact here.
  ///
  /// On a leaf it is asked about *two different values*. Steps 1 and 2 ask about the value the
  /// resolver produced; step 4 asks again about what [`coerce_leaf`](Values::coerce_leaf) returned,
  /// because that second one is what reaches the response. An implementation that disagrees with
  /// itself between the two can still put a null in a non-null position, which is why this must be
  /// a question about the representation and not a decision taken per call site.
  fn is_null(&self, value: &Self::Value) -> bool;

  /// Returns the value as a boolean, or `None` when it is not one.
  ///
  /// Only reached for `@skip(if:)` and `@include(if:)` with a variable condition (draft §6.3), and
  /// only for a value [`is_null`](Values::is_null) already reported as non-null. This says what
  /// the value *is*; what it means is §6.3's.
  ///
  /// # `None` is a failure, not a `false`
  ///
  /// `if` is declared `Boolean!`, so a value that is not a boolean does not satisfy the position,
  /// and the executor raises [`Kind::DirectiveCondition`](crate::proto::Kind::DirectiveCondition)
  /// at the object whose selection set was being collected rather than choosing a boolean for it.
  /// There is no boolean it could choose: `@skip` removes the selection when the condition is
  /// `true` and `@include` removes it when it is not, so `false` would keep a `@skip`ped selection
  /// and `true` would keep an `@include`d one. A guard that cannot be evaluated has to fail
  /// closed, and failing closed here means the enclosing position is nulled and the response says
  /// why.
  ///
  /// A conforming driver never reaches that branch, and the obligation that keeps it so is draft
  /// §6.1's, which is this trait's caller's: 5.8.5 admits only a `Boolean` or `Boolean!` variable
  /// at the condition, and `CoerceVariableValues` rejects a runtime value for one that is neither
  /// a boolean nor null. What *is* reachable from a conforming request is a **null** — 5.8.5 lets
  /// `$flag: Boolean = false` stand at the `Boolean!` location, and §6.1 step 5.e.i coerces an
  /// explicitly supplied `null` for it to `null` — and that one is settled by
  /// [`is_null`](Values::is_null) before this method is asked.
  fn as_bool(&self, value: &Self::Value) -> Option<bool>;

  /// Returns the number of elements when the value is a list, and `None` when it is not.
  ///
  /// One call answers both of draft §6.4.3 step 3's questions — "is this a collection" and "how
  /// many" — so the field-error branch is total rather than a second call that might disagree with
  /// the first.
  ///
  /// A *length* rather than an iterator because this is a Sans-I/O machine: completing an element
  /// may hand a field request back to the driver, which may call in again before the list is
  /// finished, so nothing may hold a borrow of the list across a poll.
  fn list_len(&self, value: &Self::Value) -> Option<usize>;

  /// Returns the element at `index`, which is always less than the length
  /// [`list_len`](Values::list_len) reported for the same value.
  ///
  /// `&mut self` because a handle table must be able to issue a handle for the element. That
  /// mutability is the whole of the obligation this method carries: the length is read once and
  /// the executor may then ask for any index below it, so an implementation must not shorten or
  /// discard a list it has already measured. It may also ask for fewer than all of them — draft
  /// §6.4.4 nulling a `[T!]` stops the walk at the element that failed. Completing an element can
  /// hand a field request back to the driver, so the two calls are not adjacent in time.
  fn list_item(&mut self, value: &Self::Value, index: usize) -> Self::Value;

  /// Returns the name of the value's concrete object type, for draft §6.4.3's `ResolveAbstractType`.
  ///
  /// Asked only when a field's type is an interface or a union — an object-typed field's runtime
  /// type is the declared one, and asking would be a call per field for an answer already known.
  ///
  /// `None` raises the specified field error rather than panicking, because "this value does not
  /// belong to any type in the schema" is a service condition and draft §6.4.3 says a field error
  /// is how a service reports one.
  fn type_name<'a>(&'a self, value: &'a Self::Value) -> Option<&'a str>;

  /// Serialises a leaf, for draft §6.4.3 step 4's `CoerceResult`.
  ///
  /// A returned value that is not null is what lands in the response, so a driver may normalise
  /// here: an `ID` held as an integer becomes a string, an enum held as a discriminant becomes its
  /// member name.
  ///
  /// # Failure has two spellings and one meaning
  ///
  /// A field error is raised both by returning `None` **and** by returning a value
  /// [`is_null`](Values::is_null) reports as null. Draft §6.4.3 raises one when the internal
  /// function "raises an exception or returns a value other than a legal GraphQL value", and null
  /// is not a legal serialised leaf, so the two are one outcome and a driver may report it either
  /// way. The executor asks `is_null` about whatever this returns and takes the same branch for
  /// both, *including where the position is nullable* — a null there would have been legal from a
  /// resolver, but coming from a serialiser it is the specified error, and a response carrying it
  /// silently would be a null no `errors` entry accounts for. The executor supplies the message and
  /// the response path; the driver only has to say that it could not.
  ///
  /// # A null field is not this method's to report
  ///
  /// `value` is never null: draft §6.4.3 step 2 settles a null value before step 4 is reached, so
  /// a driver expressing "this field has no value" does it by resolving null, not by serialising to
  /// one. That is what keeps the paragraph above from being a trap — the only way to reach it is to
  /// have resolved a value and then failed to serialise it.
  fn coerce_leaf(&mut self, value: Self::Value, leaf: Leaf<'_>) -> Option<Self::Value>;

  /// Returns the operation's coerced value for a variable, or `None` when the request supplied
  /// none.
  ///
  /// `None` means the variable was *not provided* in draft §6.4.1's sense — which is a different
  /// state from a variable provided the value `null`, and the two take different branches of steps
  /// 5.d through 5.f. A driver that flattens them will silently accept a query the specification
  /// requires it to reject.
  ///
  /// Draft §6.1's `CoerceVariableValues` is the driver's, not `proto`'s: the values arriving here
  /// are already coerced against their declared types.
  ///
  /// # Called once per use, and the answer is carried
  ///
  /// The two callers — draft §6.4.1's `CoerceArgumentValues` and draft §6.3's directive conditions
  /// — each read a variable once and spend that one value. A field argument's reaches the driver
  /// as [`ArgumentSource::Variable`](crate::proto::ArgumentSource::Variable) rather than as a name
  /// to look up again, so the value that passed §6.4.1's non-null check is the value the resolver
  /// receives; a condition's is read, tested and dropped inside §6.3.
  ///
  /// Neither outlives the position that read it. A condition's value is gone before collection
  /// returns, and an argument's is released as soon as the request carrying it stops being the one
  /// offered, so a value moved out of a table here is given back within the operation and not at
  /// the end of it.
  ///
  /// So this may be called at most once per position, and an implementation that consumes,
  /// invalidates or recycles on read cannot put a value past a check it did not pass. It may still
  /// be called more than once for the same *name*: a variable used at two arguments is two
  /// positions, and one nested inside an
  /// [`ArgumentSource::Literal`](crate::proto::ArgumentSource::Literal) is read by the driver
  /// itself.
  fn variable(&mut self, name: &str) -> Option<Self::Value>;
}
