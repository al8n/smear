//! Field errors, and the draft §7.1.2 entries they become.
//!
//! # Every one of these is a *field* error
//!
//! Draft §7.1.2 distinguishes request errors — raised before execution begins, producing a
//! response with no `data` entry at all — from field errors, which are raised during execution,
//! null the position they happened at and are collected into `errors`. Nothing in this module is a
//! request error: `proto` is entered with a document the validator has already accepted against a
//! schema `Schema::build` has already accepted, so the request-error phase is over before the
//! executor exists.
//!
//! *Position* rather than *field*, because one of these can happen where there is no field.
//! [`Kind::DirectiveCondition`] is raised while a selection set is being collected, and over the
//! root selection set the position is the root: `data` is present and null, and §7.1.2's `path` is
//! absent because the error cannot be associated with a particular field. That is still the
//! during-execution shape §7.1.1 describes and not the request-error one, which omits `data`
//! entirely.
//!
//! That is also why an error here is never returned from a method. A field error is an ordinary
//! outcome of executing a field, and pulling it out of the response would break the one property
//! draft §6.4.4 depends on: that the error and the null it caused are produced together.
//!
//! # Messages are rendered, not stored
//!
//! Only [`Kind::Resolver`] carries text at all, because only the driver's message is
//! unpredictable, and even that is interned rather than owned per error. Every specified message
//! is a function of the schema and the field, so the error row keeps the symbols and [`Error`]'s
//! `Display` writes the sentence. A response with four hundred
//! `Cannot return null for non-nullable field` errors allocates four hundred times nothing.
//!
//! The wording follows the reference implementation's, not because `graphql-js` is normative — the
//! specification does not prescribe message text — but because the oracle in
//! `tests/proto_nonnull_oracle.rs` compares against it, and a differential that ignored the
//! message would agree about strictly less.

use core::fmt;

use tokora::SimpleSpan;

use crate::validator::schema::{PackedType, Schema, Sym, TypeId};

use super::response::{Path, Slot};

/// What went wrong, with everything the message needs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Raw {
  /// The driver reported a field error through
  /// [`handle_field_error`](super::Executor::handle_field_error). The message is held out of line.
  Resolver { message: u32 },
  /// Draft §6.4.3 step 1: a non-null position completed to null.
  NullInNonNull { parent: TypeId, field: Sym },
  /// Draft §6.4.3 step 3: the value of a list-typed position is not a collection.
  NotAList { parent: TypeId, field: Sym },
  /// Draft §6.4.3 step 4: `CoerceResult` could not serialise the value.
  LeafCoercion {
    leaf: Sym,
    parent: TypeId,
    field: Sym,
  },
  /// Draft §6.4.3 step 5: the driver could not name the value's concrete type.
  AbstractUnresolved {
    abstract_ty: TypeId,
    parent: TypeId,
    field: Sym,
  },
  /// Draft §6.4.3 step 5: the driver named a type that is not a possible type of the position's
  /// abstract type — including a name the schema does not know at all, and a name that is known
  /// but is not an object type.
  AbstractNotPossible { abstract_ty: TypeId, runtime: u32 },
  /// Draft §6.4.1 step 5.b: a required argument was not provided.
  ArgumentMissing {
    field: Sym,
    argument: Sym,
    ty: PackedType,
  },
  /// Draft §6.4.1 step 5.f: a non-null argument was given `null`.
  ArgumentNull {
    field: Sym,
    argument: Sym,
    ty: PackedType,
  },
  /// Draft §6.4.1 step 5.d: a required argument names a variable the request did not supply.
  ArgumentVariableMissing {
    field: Sym,
    argument: Sym,
    ty: PackedType,
    variable: u32,
  },
  /// Draft §6.3 steps 3.a and 3.b: a `@skip`/`@include` condition that is not a readable boolean.
  DirectiveCondition { fault: ConditionFault },
  /// Not a specification failure: completing this position would have taken the response past
  /// [`Limits::max_response_slots`](super::Limits::max_response_slots).
  ResponseBudget {
    parent: TypeId,
    field: Sym,
    limit: u32,
  },
}

/// Which way a `@skip`/`@include` condition failed to be a boolean.
///
/// The four are draft §6.4.1's own failures, read at a directive argument instead of a field one:
/// `if` is declared `Boolean!`, so an absent argument is step 5.b, an unsupplied variable is 5.d
/// and a null is 5.i.i. [`NotABoolean`](ConditionFault::NotABoolean) is the one with no §6.4.1
/// number, because §6.4.1 reaches it through step 5.j's input coercion — which `proto` leaves to
/// the driver everywhere else, and can perform here only because
/// [`Values::as_bool`](super::Values::as_bool) is the one coercion the trait already exposes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ConditionFault {
  /// The directive carries no `if` argument at all.
  Missing,
  /// `if` names a variable the request did not supply. The name is held out of line.
  VariableMissing { variable: u32 },
  /// `if`'s value is `null`.
  Null,
  /// `if`'s value is neither null nor a boolean.
  NotABoolean,
}

/// One row of the executor's error table.
#[derive(Debug)]
pub(super) struct Row {
  pub(super) raw: Raw,
  /// The slot the error happened at, which is what the response path is derived from.
  pub(super) slot: u32,
  /// Draft §7.1.2's `locations`, as a range into the executor's flat span table.
  ///
  /// A range and not a `Vec`, for the reason the module header gives about messages: a response
  /// with four hundred field errors must not be four hundred allocations, and the spans it points
  /// at were collected once per response key whatever happens afterwards. It is also what lets a
  /// list of a thousand elements share one entry per merged field rather than one per element.
  pub(super) locations: (u32, u32),
}

/// The classification of a field error, for a driver that wants to branch on it.
///
/// Separate from the message on purpose: the message is prose that may be improved, and a driver
/// that matched on it would break when it was.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Kind {
  /// The driver reported it.
  Resolver,
  /// A non-null position completed to null (draft §6.4.3 step 1).
  NullInNonNull,
  /// A list-typed position did not receive a collection (draft §6.4.3 step 3).
  NotAList,
  /// A leaf could not be serialised (draft §6.4.3 step 4).
  LeafCoercion,
  /// An abstract position's concrete type could not be determined (draft §6.4.3 step 5).
  AbstractUnresolved,
  /// An abstract position's concrete type is not one of its possible types (draft §6.4.3 step 5).
  AbstractNotPossible,
  /// A required argument was not provided (draft §6.4.1 step 5.b).
  ArgumentMissing,
  /// A non-null argument was given `null` (draft §6.4.1 step 5.f).
  ArgumentNull,
  /// A required argument names a variable the request did not supply (draft §6.4.1 step 5.d).
  ArgumentVariableMissing,
  /// A `@skip`/`@include` condition was not a readable boolean (draft §6.3 steps 3.a and 3.b).
  ///
  /// Separate from [`ArgumentNull`](Kind::ArgumentNull) and its two siblings even though it
  /// borrows their messages, because the *position* differs: a field-argument error is raised at
  /// the field whose argument it is, and a condition is read while collecting a selection set, so
  /// this one is raised at the enclosing object and nulls that. At the root selection set there is
  /// no enclosing field at all and the error's path is empty.
  DirectiveCondition,
  /// The response reached [`Limits::max_response_slots`](super::Limits::max_response_slots)
  /// (not a draft §6 failure).
  ///
  /// The only kind here that is the *executor's* refusal rather than a fault in the document or in
  /// the driver's answer, and the only one whose path names where execution stopped rather than
  /// what went wrong. A driver matching on it is being told to ask for less or to raise the
  /// ceiling, not that the position it names is bad.
  ///
  /// It is a field error and not a refusal of the whole request because draft §7.1.2 reserves the
  /// request-error shape — a response with no `data` key — for a failure raised before execution
  /// begins. By the time a list is being completed, resolvers have already answered.
  ResponseBudget,
}

impl Raw {
  #[inline]
  pub(super) const fn kind(&self) -> Kind {
    match self {
      Self::Resolver { .. } => Kind::Resolver,
      Self::NullInNonNull { .. } => Kind::NullInNonNull,
      Self::NotAList { .. } => Kind::NotAList,
      Self::LeafCoercion { .. } => Kind::LeafCoercion,
      Self::AbstractUnresolved { .. } => Kind::AbstractUnresolved,
      Self::AbstractNotPossible { .. } => Kind::AbstractNotPossible,
      Self::ArgumentMissing { .. } => Kind::ArgumentMissing,
      Self::ArgumentNull { .. } => Kind::ArgumentNull,
      Self::ArgumentVariableMissing { .. } => Kind::ArgumentVariableMissing,
      Self::DirectiveCondition { .. } => Kind::DirectiveCondition,
      Self::ResponseBudget { .. } => Kind::ResponseBudget,
    }
  }
}

/// One entry of a response's `errors` (draft §7.1.2).
pub struct Error<'r, V> {
  pub(super) schema: &'r Schema,
  pub(super) slots: &'r [Slot<V>],
  pub(super) names: &'r [u8],
  pub(super) name_spans: &'r [(u32, u32)],
  pub(super) locations: &'r [SimpleSpan],
  pub(super) row: &'r Row,
}

impl<'r, V> Error<'r, V> {
  /// Returns what went wrong.
  #[inline]
  pub const fn kind(&self) -> Kind {
    self.row.raw.kind()
  }

  /// Returns the draft §7.1.2 response path of the field the error happened at.
  #[inline]
  pub fn path(&self) -> Path<'r, V> {
    Path {
      slots: self.slots,
      names: self.names,
      name_spans: self.name_spans,
      slot: self.row.slot,
    }
  }

  /// Returns the spans in the source document this error is attributed to — draft §7.1.2's
  /// `locations`.
  ///
  /// A list, not a span, because §7.1.2 makes it one. Draft §6.3 groups every selection that
  /// shares a response key into a single field of the response, so `{ a { x } a { x } }` resolves
  /// `a` once and one error about it belongs to *two* places in the document. Returning the first
  /// would silently drop the rest, which is the shape a scalar accessor cannot avoid, so there is
  /// no scalar accessor; a caller that wants one position takes `first()`.
  ///
  /// The reference implementation reports the same list, in the same order, and draws the same
  /// exception: an error draft §6.4.1 raised while coercing arguments carries the one node it can
  /// name, so this is a single span for those.
  ///
  /// Spans rather than lines and columns, because that is the currency of the rest of this crate
  /// and turning one into the other needs the source text, which the executor deliberately does
  /// not hold.
  ///
  /// Always at least one entry for any error a driver can observe.
  #[inline]
  pub fn locations(&self) -> &'r [SimpleSpan] {
    let (start, len) = self.row.locations;
    &self.locations[start as usize..(start + len) as usize]
  }

  fn name(&self, id: u32) -> &'r str {
    let (start, len) = self.name_spans[id as usize];
    core::str::from_utf8(&self.names[start as usize..(start + len) as usize]).unwrap_or("")
  }
}

impl<V> fmt::Debug for Error<'_, V> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("Error")
      .field("kind", &self.kind())
      .field("path", &self.path())
      .field("locations", &self.locations())
      .finish()
  }
}

impl<V> fmt::Display for Error<'_, V> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let schema = self.schema;
    // A field's owner is rendered as `Type.field`, which is how every implementation spells it and
    // what draft §6.4.3's prose calls the field.
    let owner = |f: &mut fmt::Formatter<'_>, parent: TypeId, field: Sym| -> fmt::Result {
      write!(
        f,
        "{}.{}",
        schema.name(schema.type_def(parent).name()),
        schema.name(field)
      )
    };
    let ty = |f: &mut fmt::Formatter<'_>, packed: PackedType| -> fmt::Result {
      packed.write(f, schema.name(packed.base()))
    };
    match self.row.raw {
      Raw::Resolver { message } => f.write_str(self.name(message)),
      Raw::NullInNonNull { parent, field } => {
        f.write_str("Cannot return null for non-nullable field ")?;
        owner(f, parent, field)?;
        f.write_str(".")
      }
      Raw::NotAList { parent, field } => {
        f.write_str("Expected Iterable, but did not find one for field \"")?;
        owner(f, parent, field)?;
        f.write_str("\".")
      }
      Raw::LeafCoercion {
        leaf,
        parent,
        field,
      } => {
        write!(f, "Expected a value of type \"{}\" but ", schema.name(leaf))?;
        f.write_str("received an incompatible value for field \"")?;
        owner(f, parent, field)?;
        f.write_str("\".")
      }
      Raw::AbstractUnresolved {
        abstract_ty,
        parent,
        field,
      } => {
        write!(
          f,
          "Abstract type \"{}\" must resolve to an Object type at runtime for field \"",
          schema.name(schema.type_def(abstract_ty).name())
        )?;
        owner(f, parent, field)?;
        f.write_str("\".")
      }
      Raw::AbstractNotPossible {
        abstract_ty,
        runtime,
      } => write!(
        f,
        "Runtime Object type \"{}\" is not a possible type for \"{}\".",
        self.name(runtime),
        schema.name(schema.type_def(abstract_ty).name())
      ),
      Raw::ArgumentMissing {
        field: _,
        argument,
        ty: packed,
      } => {
        write!(
          f,
          "Argument \"{}\" of required type \"",
          schema.name(argument)
        )?;
        ty(f, packed)?;
        f.write_str("\" was not provided.")
      }
      Raw::ArgumentNull {
        field: _,
        argument,
        ty: packed,
      } => {
        write!(
          f,
          "Argument \"{}\" of non-null type \"",
          schema.name(argument)
        )?;
        ty(f, packed)?;
        f.write_str("\" must not be null.")
      }
      Raw::ArgumentVariableMissing {
        field: _,
        argument,
        ty: packed,
        variable,
      } => {
        write!(
          f,
          "Argument \"{}\" of required type \"",
          schema.name(argument)
        )?;
        ty(f, packed)?;
        write!(
          f,
          "\" was provided the variable \"${}\" which was not provided a runtime value.",
          self.name(variable)
        )
      }
      // Draft §6.3 reads the condition from an argument it names `if`, and draft §3.13 declares
      // both directives with that one argument at type `Boolean!`. Both names are therefore the
      // specification's rather than the schema's, and are written out rather than looked up: a
      // schema whose `@skip` disagrees with §3.13 is already non-conforming, and reading its
      // spelling back would make the message describe the wrong contract.
      Raw::DirectiveCondition { fault } => match fault {
        ConditionFault::Missing => {
          f.write_str("Argument \"if\" of required type \"Boolean!\" was not provided.")
        }
        ConditionFault::VariableMissing { variable } => write!(
          f,
          "Argument \"if\" of required type \"Boolean!\" was provided the variable \"${}\" \
           which was not provided a runtime value.",
          self.name(variable)
        ),
        ConditionFault::Null => {
          f.write_str("Argument \"if\" of non-null type \"Boolean!\" must not be null.")
        }
        ConditionFault::NotABoolean => {
          f.write_str("Argument \"if\" of type \"Boolean!\" must be a Boolean.")
        }
      },
      // No reference wording to follow: `graphql-js` has no such limit, so this message is this
      // crate's own. It names the ceiling because that is the one thing a reader can act on — the
      // number is static configuration rather than anything about the request, so saying it
      // discloses nothing the operator did not choose to impose.
      Raw::ResponseBudget {
        parent,
        field,
        limit,
      } => {
        f.write_str("Cannot complete field ")?;
        owner(f, parent, field)?;
        write!(
          f,
          ": the response would exceed the executor's limit of {limit} positions."
        )
      }
    }
  }
}
