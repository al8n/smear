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

use smear_schema::{PackedType, Schema, Sym, TypeId};

use super::{
  collect::Unstored,
  response::{Path, Slot},
};

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
  /// [`AbstractNotPossible`](Raw::AbstractNotPossible) with the offending name unquotable.
  ///
  /// The same failure, and deliberately not a different one: the driver still named a type the
  /// position cannot hold, and that is what the reader needs to know. What is missing is the
  /// *spelling*, because recording it would have taken the executor past one of two ceilings —
  /// which one is [`Unstored`], and it is carried rather than assumed. This variant used to render
  /// [`Limits::max_interned_bytes`](super::Limits::max_interned_bytes) whichever ceiling had
  /// refused, so an operator whose visit budget ran out was sent to resize the arena.
  ///
  /// It exists rather than a placeholder because there is no honest placeholder. An empty string
  /// renders as `Runtime Object type "" is not a possible type`, and `<unknown>` renders as a type
  /// that could plausibly be in someone's schema — both invite a reader to go looking for a type
  /// that was never named. Saying the name could not be recorded, and which ceiling stopped it, is
  /// the only version that does not lie about what the driver said.
  AbstractNotPossibleUnnamed {
    abstract_ty: TypeId,
    unstored: Unstored,
  },
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
    /// The variable's spelling, or `None` when it could not be interned — for either reason.
    ///
    /// The finding does not depend on the spelling: the request did not supply the variable, and
    /// that is true whether or not its name can be quoted. So an exhausted arena *or* an exhausted
    /// visit budget shortens this message and leaves the error saying the same thing, rather than
    /// swapping it for one about a resource — which would report a resource problem where the
    /// caller has an argument problem. This is why [`Unstored`] is discarded here and carried at
    /// the two sites whose message names a ceiling.
    variable: Option<u32>,
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
  /// Not a specification failure: recording this object's response metadata would have taken it
  /// past [`Limits::max_response_metadata`](super::Limits::max_response_metadata).
  SelectionBudget {
    parent: TypeId,
    field: Sym,
    limit: u32,
  },
  /// Not a specification failure: collecting this selection set would take the response metadata
  /// past [`Limits::max_response_metadata`](super::Limits::max_response_metadata).
  ///
  /// Raised one step earlier than [`SelectionBudget`](Raw::SelectionBudget), at the staging buffer
  /// rather than at the commit, and `expand` rewrites it into that variant so the message can name
  /// the position. The rewrite is an improvement to the message and not a correctness requirement:
  /// if a future path forgets it, this renders a true sentence that is merely less specific, which
  /// is the direction a forgotten step should fail in.
  MetadataBudget { limit: u32 },
  /// Not a specification failure: draft §6.3's collection ran past
  /// [`Limits::max_selection_visits`](super::Limits::max_selection_visits).
  ///
  /// No field is named, because collection is what *discovers* the fields — this is raised inside
  /// the walk, before the position that would have been blamed exists.
  CollectionBudget { limit: u32 },
  /// Not a specification failure: looking up this position's runtime type name would have taken
  /// the operation past [`Limits::max_selection_visits`](super::Limits::max_selection_visits).
  ///
  /// Its own variant, and deliberately not [`AbstractNotPossible`](Raw::AbstractNotPossible) with
  /// a shortened message. That one says the driver named a type this position cannot hold; this
  /// one says nothing about the type at all, because the lookup that would have decided never ran.
  /// Folding them would tell an operator their schema is missing a type when what happened is that
  /// the executor stopped looking — a fact about a ceiling, reported as a fact about their schema,
  /// and the one thing a caller acting on it cannot recover from.
  ///
  /// # The name here is the *driver's*
  ///
  /// [`Values::type_name`](super::Values::type_name) returns whatever the backend says, so its
  /// length is the resolver's choice and not the request's. That changes who has to misbehave for
  /// the work to be large; it does not change whether the work is metered. `Schema::sym` hashes
  /// the whole string before it compares anything, and an abstract position inside a list asks it
  /// once per element — so one constant-size query over a driver returning the same long
  /// discriminator for every element performs `positions × length` byte work off a ceiling
  /// (`max_response_slots`) that counts positions and never looked at the string. Worse, that
  /// lookup used to run *after* the interner had already exhausted the visit budget: the ledger
  /// was refusing and the expensive path ran anyway, once more for every remaining element.
  /// al8n/smear#196.
  RuntimeTypeBudget {
    abstract_ty: TypeId,
    parent: TypeId,
    field: Sym,
    limit: u32,
  },
  /// Not a specification failure: a name would have taken the interner past
  /// [`Limits::max_interned_bytes`](super::Limits::max_interned_bytes).
  ///
  /// Distinct from [`ResolverUnstorable`](Raw::ResolverUnstorable), which is the same refusal on a
  /// message the *driver* supplied: that one loses text from an error that is still reported, and
  /// this one means a response key or a type name could not be recorded, so the position it
  /// belongs to cannot be built at all.
  NameStorage { limit: u32 },
  /// A field's response key is not a name, so no response can carry it (draft §2.1.9, §7.1.2).
  ///
  /// Carries nothing. Half of what it refuses has no rendering at all — a response key need not be
  /// UTF-8 — and the other half renders perfectly and is still not a name, so a variant that quoted
  /// whenever it could would be two messages for one finding, and would spend the arena and the
  /// visit budget on a diagnostic for a document that is not GraphQL. Naming the *field* instead
  /// is no way out: the key **is** the field's name whenever there is no alias.
  ///
  /// Unreachable from a lexed document — draft §2.1.9 makes a name ASCII and both dialects spell
  /// exactly that — and reachable from an assembled one, which is an input
  /// [`Executor::new`](super::Executor::new) admits by signature. Raised rather than skipped
  /// because the client asked for this position: dropping the key silently is the only outcome
  /// with nothing in `errors` to account for it.
  ResponseKeyUnreadable,
  /// The driver reported a field error whose message could not be recorded.
  ///
  /// The failure is the driver's and is reported; only its text is missing, which is why this is
  /// [`Kind::Resolver`] and not a budget kind **whichever ceiling refused** — the finding is that
  /// the resolver failed, and that is true either way. Losing the text is strictly better than the
  /// alternative the arena case replaces: storing it would have narrowed an arena offset that no
  /// longer fits a `u32`, and every name interned afterwards — every response key, every
  /// `__typename` — would have read back the wrong bytes.
  ///
  /// [`Unstored`] is carried so the message names the ceiling that actually refused. Both are
  /// reachable here: the message is the driver's text, so its length is the arena's problem, and
  /// finding it in the table is the visit budget's.
  ResolverUnstorable { unstored: Unstored },
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
  /// `if` names a variable the request did not supply. The name is held out of line, and is
  /// `None` when it could not be interned — see
  /// [`Raw::ArgumentVariableMissing`](Raw::ArgumentVariableMissing) for why the message shortens
  /// rather than the error changing, and why the ceiling that refused is not reported here.
  VariableMissing { variable: Option<u32> },
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
  /// The response reached one of [`Limits`](super::Limits)'s two response ceilings — positions
  /// ([`max_response_slots`](super::Limits::max_response_slots)) or merged selections
  /// ([`max_response_metadata`](super::Limits::max_response_metadata)). Not a draft §6 failure.
  ///
  /// One kind for both, because a driver's remedy is identical: ask for less, or raise the ceiling
  /// that the message names. Two kinds would ask a caller to distinguish cases it would handle the
  /// same way; one message would name the wrong knob half the time, which is why the *messages*
  /// differ and the classification does not.
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
  /// A field's response key is not a name, so no response can carry it (draft §2.1.9, §7.1.2).
  ///
  /// Not reachable from a document the lexer produced: draft §2.1.9's `Name` is
  /// `[_A-Za-z][_0-9A-Za-z]*` and both dialects spell exactly that, so any key that was *lexed* is
  /// ASCII whatever the source type is. It is reachable from a document that was **assembled** —
  /// [`Executor::new`](super::Executor::new) takes any `&ExecutableDocument<S>` for any
  /// `S: AsRef<[u8]>`, and a rewriter, a persisted-query store or an FFI bridge builds one.
  ///
  /// Its own kind rather than a shared one because a driver's remedy is unlike every other kind's:
  /// nothing about the request's size, the schema or the resolved value is wrong, and the position
  /// cannot be retried. Whatever produced the document produced something that is not GraphQL.
  ResponseKeyUnreadable,
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
      Self::AbstractNotPossible { .. } | Self::AbstractNotPossibleUnnamed { .. } => {
        Kind::AbstractNotPossible
      }
      Self::ArgumentMissing { .. } => Kind::ArgumentMissing,
      Self::ArgumentNull { .. } => Kind::ArgumentNull,
      Self::ArgumentVariableMissing { .. } => Kind::ArgumentVariableMissing,
      Self::DirectiveCondition { .. } => Kind::DirectiveCondition,
      Self::ResponseBudget { .. }
      | Self::SelectionBudget { .. }
      | Self::CollectionBudget { .. }
      | Self::MetadataBudget { .. }
      | Self::NameStorage { .. }
      | Self::RuntimeTypeBudget { .. } => Kind::ResponseBudget,
      Self::ResponseKeyUnreadable => Kind::ResponseKeyUnreadable,
      Self::ResolverUnstorable { .. } => Kind::Resolver,
    }
  }
}

/// One entry of a response's `errors` (draft §7.1.2).
pub struct Error<'r, V> {
  pub(super) schema: &'r Schema,
  pub(super) slots: &'r [Slot<V>],
  pub(super) names: &'r str,
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
    Path::new(self.slots, self.names, self.name_spans, self.row.slot)
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

  /// Returns one entry of the executor's name table.
  ///
  /// No conversion and no fallback. The arena is a `str` because a fallback here is not a degraded
  /// reading — `unwrap_or("")` rendered every spelling it could not read as the *same* one, so
  /// `ArgumentVariableMissing` printed `$` with nothing after it and two distinct names became one
  /// message. Both spellings the arena can hold from a document now refuse at admission instead;
  /// see [`Interner`](super::collect::Interner) and al8n/smear#139.
  fn name(&self, id: u32) -> &'r str {
    let (start, len) = self.name_spans[id as usize];
    &self.names[start as usize..(start + len) as usize]
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
    // How a budget refusal names the position it could not complete.
    //
    // The `Type.field` form is a lie at the root: `start` fills the root's `field_sym` with the
    // root *type's* own name, so `owner` renders `Query.Query` — a field that does not exist. And
    // `slot == 0` **is** the root, by construction, because `start` pushes it before anything else.
    // A field genuinely named `Query` on `Query` still renders `Query.Query`, because its slot is
    // not zero, so there is no sentinel to collide with.
    //
    // "the operation's" rather than "the query's": draft §6.2.2 made the root a mutation's as well,
    // and a message that names the wrong operation kind is the same class of lie as the one above.
    let position = |f: &mut fmt::Formatter<'_>, parent: TypeId, field: Sym| -> fmt::Result {
      if self.row.slot == 0 {
        f.write_str("Cannot complete the operation's root selection set")
      } else {
        f.write_str("Cannot complete field ")?;
        owner(f, parent, field)
      }
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
      Raw::AbstractNotPossibleUnnamed {
        abstract_ty,
        unstored,
      } => {
        let abstract_name = schema.name(schema.type_def(abstract_ty).name());
        // One sentence per ceiling rather than one sentence with the number swapped: the arena had
        // no room to *keep* the name and the budget had none to *look it up*, and a reader deciding
        // which knob to move is being told two different things.
        match unstored {
          Unstored::Arena { limit } => write!(
            f,
            "The runtime Object type is not a possible type for \"{abstract_name}\", and its name \
             could not be stored within the executor's limit of {limit} interned bytes."
          ),
          Unstored::Budget { limit } => write!(
            f,
            "The runtime Object type is not a possible type for \"{abstract_name}\", and its name \
             could not be looked up within the executor's limit of {limit} selection visits."
          ),
        }
      }
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
        match variable {
          Some(variable) => write!(
            f,
            "\" was provided the variable \"${}\" which was not provided a runtime value.",
            self.name(variable)
          ),
          None => f.write_str("\" was provided a variable which was not provided a runtime value."),
        }
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
        ConditionFault::VariableMissing {
          variable: Some(variable),
        } => write!(
          f,
          "Argument \"if\" of required type \"Boolean!\" was provided the variable \"${}\" \
           which was not provided a runtime value.",
          self.name(variable)
        ),
        ConditionFault::VariableMissing { variable: None } => f.write_str(
          "Argument \"if\" of required type \"Boolean!\" was provided a variable which was not \
           provided a runtime value.",
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
        position(f, parent, field)?;
        write!(
          f,
          ": the response would exceed the executor's limit of {limit} positions."
        )
      }
      Raw::SelectionBudget {
        parent,
        field,
        limit,
      } => {
        position(f, parent, field)?;
        write!(
          f,
          ": the response would exceed the executor's limit of {limit} metadata entries."
        )
      }
      Raw::MetadataBudget { limit } => write!(
        f,
        "Collecting this selection set would exceed the executor's limit of {limit} metadata \
         entries."
      ),
      Raw::CollectionBudget { limit } => write!(
        f,
        "Collecting this operation's fields would exceed the executor's limit of {limit} \
         selection visits."
      ),
      Raw::RuntimeTypeBudget {
        abstract_ty,
        parent,
        field,
        limit,
      } => {
        // It names the *lookup*, not the type: the executor never learned what the driver's
        // spelling resolves to, so a message saying anything about that type would be inventing
        // the fact this variant exists to withhold.
        write!(
          f,
          "Resolving the runtime Object type of abstract type \"{}\" for field \"",
          schema.name(schema.type_def(abstract_ty).name())
        )?;
        owner(f, parent, field)?;
        write!(
          f,
          "\" would exceed the executor's limit of {limit} selection visits."
        )
      }
      Raw::NameStorage { limit } => write!(
        f,
        "A name in this response would exceed the executor's limit of {limit} interned bytes."
      ),
      // No spelling, deliberately: see the variant. "not a name" is the whole finding, and the
      // bytes that are not one have no rendering this could quote without inventing it.
      Raw::ResponseKeyUnreadable => f.write_str(
        "A field's response key is not a name and cannot appear in a response (draft \u{a7}2.1.9).",
      ),
      Raw::ResolverUnstorable { unstored } => match unstored {
        Unstored::Arena { limit } => write!(
          f,
          "The driver reported a field error whose message exceeded the executor's storage limit \
           of {limit} interned bytes and was not recorded."
        ),
        Unstored::Budget { limit } => write!(
          f,
          "The driver reported a field error whose message could not be recorded within the \
           executor's limit of {limit} selection visits."
        ),
      },
    }
  }
}
