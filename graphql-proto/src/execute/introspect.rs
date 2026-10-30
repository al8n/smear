//! Draft §4's `__schema` and `__type`, answered from the [`Schema`] with no driver involvement.
//!
//! # Why the executor answers them
//!
//! Every value under the two meta-fields is a fact about the schema the executor already holds,
//! so asking a driver for one would ask it to restate the executor's own input — and leave it free
//! to restate it wrongly, the reason `__typename` was taken out of its hands first. So `expand`
//! never offers them: it commits each one as a [`State::MetaField`] placeholder, and the walk here
//! answers every placeholder, and every meta object beneath it, before the call that collected
//! them returns. That call is [`start`](Executor::start), for the root, or the
//! [`handle_resolved`](Executor::handle_resolved) that completed an object of the query root type.
//! [`poll_resolve`](Executor::poll_resolve) and [`poll_response`](Executor::poll_response) never
//! meet a meta position, and `{ __schema { … } }` is delivered with nothing enqueued, the way a
//! `__typename`-only selection set is.
//!
//! Only the query root type answers `__schema` and `__type`, matched by type and by symbol, which
//! is draft §4.4's own placement of them: an object of that type answers them wherever it stands,
//! and no other type has the fields at all.
//!
//! # A worklist, and no nested `expand`
//!
//! A meta object's selection set is collected by the same `expand` every object uses —
//! fragments, aliases, `@skip` and `@include`, `__typename` and the budgets all come with it — and
//! `expand` must not call back into this walk, or the depth of an introspection query would be the
//! depth of the native stack. So the walk drives `expand` from a loop: `introspect` answers the
//! placeholders one object holds, a meta object among the answers is queued, and the loop takes
//! each queued object, runs draft §6.4.3's `MergeSelectionSets` over it, `expand`s it, and answers
//! the placeholders that produced. One collection scratch serves every object, as it does for the
//! rest of the executor.
//!
//! The queue is threaded through [`Slot::next_ready`](crate::response::Slot::next_ready), first in
//! first out. A meta position is never on the ready chain, so the link is free to borrow, and the
//! walk holds no buffer of its own: there is nothing for [`Limits`](super::Limits)'s census of
//! retained buffers to account for.
//!
//! # What the walk spends
//!
//! Every position it creates is a position like any other, charged against
//! [`max_response_slots`](super::Limits::max_response_slots) by `push_child`, and every meta
//! object's collection is charged against
//! [`max_response_metadata`](super::Limits::max_response_metadata) and
//! [`max_selection_visits`](super::Limits::max_selection_visits) by `expand`. Two things are this
//! walk's own and are charged here against the visit ceiling, both before the work they pay for:
//!
//! - **One unit per schema member a list reads**, whether or not `includeDeprecated` lets it
//!   through, and one per possible-object word an interface's `possibleTypes` scans. A list a
//!   filter empties creates no position, so without the charge `fields(includeDeprecated: false)`
//!   over a type whose fields are all deprecated would cost a schema's width per alias and nothing
//!   any ceiling counts — positions are the client's to multiply and the width is the deployment's,
//!   which is the product [`Limits`](super::Limits) refuses.
//! - **The bytes of `__type`'s name**, one pass per walk over them, for the reason
//!   [`Values::type_name`]'s spelling is charged: a lookup hashes the whole name, and the name is
//!   the request's.
//!
//! # Refusals unwind where they happen
//!
//! A ceiling that refuses a list's element, or the walk's own charge, restores the list to the
//! mark it took before its first element and fails it there, as `complete`'s list arm does, and
//! nothing is created under a failed position afterwards — the invariant `discard`'s amortised
//! bound rests on. A refused meta object's collection is `expand`'s own refusal, restored by
//! `expand`.
//!
//! The consequence worth stating is draft §6.4.4's, not this walk's. `__schema` is `__Schema!`,
//! `types` is `[__Type!]!` and each of its elements is non-null, so a refusal at `types` or at the
//! collection of any type in it nulls `__schema` and the object it was selected on — for a
//! `__schema` at the root, the operation's whole `data`. A refusal deeper stops at the first
//! nullable position on the way up — a type's `fields`, say, which is `[__Field!]`.
//!
//! # Order
//!
//! Every list is in the order the document declared it, which is what the schema's order tables
//! keep:
//! fields, arguments, input fields, enum values, interfaces and directives as written, and
//! `__Schema.types` in the type table's own order — the document's types, then the meta-schema,
//! then the built-in scalars the document did not spell out. Two lists have no declared order and
//! take a stated one instead: a directive's `locations` come in [`DirectiveLocation`] bit order,
//! and an interface's `possibleTypes` in object order, which is the type table's. A union's
//! members are declared, and come as declared.
//!
//! [`DirectiveLocation`]: smear_schema::DirectiveLocation

use std::string::String;

use smear_parser::{
  graphql::ast::InputValue,
  lexer::{decode_string_value, string_value_pieces},
};
use smear_schema::{
  InputValueDef, PackedType, Range32, RootOperation, Schema, Sym, Text, TypeId, TypeKind, builtin,
  is_reserved,
};
use tokora::{SimpleSpan, span::AsSpan};

use crate::{
  ArgumentSource, Values,
  collect::{Visits, byte_units},
  error::Raw,
  response::{Entity, Key, MetaKind, MetaLeaf, NONE, State, TypeRef},
};

use super::{Coercion, Executor, Mark};

/// What one meta field resolves to, short of a list.
#[derive(Clone, Copy)]
enum Answer {
  Null,
  Leaf(MetaLeaf),
  Object(Entity),
}

/// One element of a meta list: an object the walk still has to collect, or a leaf.
#[derive(Clone, Copy)]
enum Element {
  Object(Entity),
  Leaf(MetaLeaf),
}

/// What reading `__type`'s `name` came to.
enum Read {
  /// The type the name names, or `None` for a name no type has — which `__type` answers `null`.
  Named(Option<TypeId>),
  /// The argument holds something that is not a string, which draft §6.4.1 step 5.j refuses.
  Invalid,
  /// [`max_selection_visits`](super::Limits::max_selection_visits) had no room for the lookup.
  Refused,
}

/// The meta objects whose selection sets are still to be collected, oldest first.
///
/// Threaded through [`Slot::next_ready`](crate::response::Slot::next_ready): see the module header.
#[derive(Clone, Copy)]
struct Pending {
  head: u32,
  tail: u32,
}

impl Pending {
  const EMPTY: Self = Self {
    head: NONE,
    tail: NONE,
  };
}

impl<'a, S, V> Executor<'a, S, V>
where
  S: AsRef<[u8]>,
  V: Values,
{
  /// Whether `sym` on `object_type` is one of the two meta-fields only the query root type has.
  ///
  /// The symbols first, because they are what an ordinary field fails on: two integer comparisons,
  /// and the root is read only for a field that is spelled `__schema` or `__type`.
  #[inline]
  pub(super) fn answers_at_root(&self, object_type: TypeId, sym: Sym) -> bool {
    (Some(sym) == self.schema_field || Some(sym) == self.type_field)
      && self.schema.root(RootOperation::Query) == Some(object_type)
  }

  /// Answers every draft §4 placeholder `expand` committed under `object`, and every meta object
  /// beneath them, before returning.
  ///
  /// `object_type` is the object's concrete type. Only the query root type can hold a placeholder
  /// that is not itself under a meta object, so any other type returns at once and an ordinary
  /// object pays one comparison.
  pub(super) fn introspect(&mut self, ctx: &mut V, object: u32, object_type: TypeId) {
    if self.schema.root(RootOperation::Query) != Some(object_type)
      || self.slots[object as usize].discarded
    {
      return;
    }
    let mut pending = Pending::EMPTY;
    self.answer_fields(ctx, object, &mut pending);
    while let Some(meta_object) = self.next_pending(&mut pending) {
      // Discarded after it was queued: a refusal under a sibling reached an ancestor, and nothing
      // may be created under a discarded position.
      if self.slots[meta_object as usize].discarded {
        continue;
      }
      self.merge_selection_sets(meta_object);
      let _ = self.expand(ctx, meta_object);
      // A refused collection has already failed the object, and restored what it committed.
      if self.slots[meta_object as usize].discarded {
        continue;
      }
      self.slots[meta_object as usize].state = State::Expanded;
      self.answer_fields(ctx, meta_object, &mut pending);
    }
  }

  /// Draft §6.4.3's `MergeSelectionSets` over a meta object, into `scratch_sets`, as `complete`
  /// runs it for any other object.
  ///
  /// A list element shares its field's metadata, so an element of `types` collects the
  /// sub-selections of every `types` selection merged into that response key.
  fn merge_selection_sets(&mut self, slot: u32) {
    let (start, len) = self.meta[slot as usize].merged;
    self.scratch_sets.clear();
    for field in &self.merged[start as usize..(start + len) as usize] {
      if let Some(set) = field.selection_set() {
        self.scratch_sets.push(set);
      }
    }
  }

  /// Answers the placeholders among `object`'s children, in collection order.
  ///
  /// Stops when one of them takes `object` with it: a non-null meta field that fails propagates,
  /// and the siblings after it are already discarded by the time this would reach them.
  fn answer_fields(&mut self, ctx: &mut V, object: u32, pending: &mut Pending) {
    let mut child = self.slots[object as usize].first_child;
    while child != NONE {
      // Read first: answering a child may restore positions created under it, but never a sibling
      // `expand` created before any of them.
      let next = self.slots[child as usize].next_sibling;
      if let State::MetaField(of) = self.slots[child as usize].state {
        self.answer(ctx, child, of, pending);
        if self.slots[object as usize].discarded {
          return;
        }
      }
      child = next;
    }
  }

  /// Answers one placeholder: the field of `of` that `slot`'s metadata names.
  ///
  /// Dispatched on the meta-schema's own spelling of the field, which `expand` found in the meta
  /// type's field table — so a field no arm names is one the meta-schema declares and draft §4 does
  /// not, and `null` is the answer that invents nothing.
  fn answer(&mut self, ctx: &mut V, slot: u32, of: Entity, pending: &mut Pending) {
    let schema = self.schema;
    let field = schema.name(self.meta[slot as usize].field_sym);
    let answer = match of {
      // `__schema` and `__type` are fields of the query root type, answered as fields of the
      // schema itself; `__Schema` has neither, so the two arms cannot meet a field of its own.
      Entity::Schema => match field {
        builtin::SCHEMA_FIELD => Answer::Object(Entity::Schema),
        builtin::TYPE_FIELD => match self.named_type(ctx, slot) {
          Some(answer) => answer,
          None => return,
        },
        "description" => match schema.description() {
          Some(_) => Answer::Leaf(MetaLeaf::SchemaDescription),
          None => Answer::Null,
        },
        "types" => {
          return self.list(slot, pending, 0..schema.types().len() as u32, |index| {
            Some(Element::Object(Entity::Type(TypeRef::named(TypeId::new(
              index,
            )))))
          });
        }
        "queryType" => root(schema, RootOperation::Query),
        "mutationType" => root(schema, RootOperation::Mutation),
        "subscriptionType" => root(schema, RootOperation::Subscription),
        "directives" => {
          return self.list(
            slot,
            pending,
            schema.directive_order().iter().copied(),
            |at| Some(Element::Object(Entity::Directive(at))),
          );
        }
        _ => Answer::Null,
      },
      Entity::Type(ty) => return self.answer_type(ctx, slot, ty, field, pending),
      Entity::Field { owner, at } => {
        let (Some(def), Some(meta)) = (
          schema.fields_of(owner).get(at as usize),
          schema.field_meta_of(owner).get(at as usize),
        ) else {
          return self.settle(slot, Answer::Null, pending);
        };
        match field {
          "name" => Answer::Leaf(MetaLeaf::Name(def.name())),
          "description" => text(meta.description()),
          "args" => return self.arguments(ctx, slot, def.args(), pending),
          "type" => Answer::Object(Entity::Type(TypeRef::of(def.ty()))),
          "isDeprecated" => Answer::Leaf(MetaLeaf::Bool(meta.deprecation().is_deprecated())),
          "deprecationReason" => text(meta.deprecation().reason()),
          _ => Answer::Null,
        }
      }
      Entity::InputValue(row) => {
        let rows = Range32::new(row, row.saturating_add(1));
        let (Some(def), Some(meta)) =
          (schema.inputs(rows).first(), schema.input_meta(rows).first())
        else {
          return self.settle(slot, Answer::Null, pending);
        };
        match field {
          "name" => Answer::Leaf(MetaLeaf::Name(def.name())),
          "description" => text(meta.description()),
          "type" => Answer::Object(Entity::Type(TypeRef::of(def.ty()))),
          "defaultValue" => text(meta.default_value()),
          "isDeprecated" => Answer::Leaf(MetaLeaf::Bool(meta.deprecation().is_deprecated())),
          "deprecationReason" => text(meta.deprecation().reason()),
          _ => Answer::Null,
        }
      }
      Entity::EnumValue { owner, at } => {
        let (Some(&value), Some(meta)) = (
          schema.enum_values_of(owner).get(at as usize),
          schema.enum_value_meta_of(owner).get(at as usize),
        ) else {
          return self.settle(slot, Answer::Null, pending);
        };
        match field {
          "name" => Answer::Leaf(MetaLeaf::Name(value)),
          "description" => text(meta.description()),
          "isDeprecated" => Answer::Leaf(MetaLeaf::Bool(meta.deprecation().is_deprecated())),
          "deprecationReason" => text(meta.deprecation().reason()),
          _ => Answer::Null,
        }
      }
      Entity::Directive(at) => {
        let (Some(def), Some(meta)) = (
          schema.directives().get(at as usize),
          schema.directive_meta().get(at as usize),
        ) else {
          return self.settle(slot, Answer::Null, pending);
        };
        match field {
          "name" => Answer::Leaf(MetaLeaf::Name(def.name())),
          "description" => text(meta.description()),
          "locations" => {
            let locations = def.locations();
            return self.list(slot, pending, locations.iter(), |location| {
              Some(Element::Leaf(MetaLeaf::Location(location)))
            });
          }
          "args" => return self.arguments(ctx, slot, def.args(), pending),
          "isRepeatable" => Answer::Leaf(MetaLeaf::Bool(def.is_repeatable())),
          _ => Answer::Null,
        }
      }
    };
    self.settle(slot, answer, pending);
  }

  /// Answers one field of `__Type`, with draft §4's kind-dependent nulls.
  ///
  /// A wrapper — `LIST` or `NON_NULL` — answers `kind` and `ofType` and `null` for everything else.
  /// A named type answers `fields` and `interfaces` as an object or an interface, `possibleTypes`
  /// as an interface or a union, `enumValues` as an enum, `inputFields` and `isOneOf` as an input
  /// object and `specifiedByURL` as a scalar, and `null` for each of them otherwise; `ofType` is
  /// `null` for every named type. An object implementing nothing answers `interfaces` with `[]`.
  fn answer_type(
    &mut self,
    ctx: &mut V,
    slot: u32,
    ty: TypeRef,
    field: &str,
    pending: &mut Pending,
  ) {
    let schema = self.schema;
    let id = ty.id;
    let def = schema.type_def(id);
    let packed = PackedType::from_parts(def.name(), id, ty.wrappers);
    if packed.depth() != 0 {
      let answer = match field {
        "kind" if packed.is_non_null() => Answer::Leaf(MetaLeaf::Kind(MetaKind::NonNull)),
        "kind" => Answer::Leaf(MetaLeaf::Kind(MetaKind::List)),
        "ofType" => match packed.strip_outer() {
          Some(inner) => Answer::Object(Entity::Type(TypeRef::of(inner))),
          None => Answer::Null,
        },
        _ => Answer::Null,
      };
      return self.settle(slot, answer, pending);
    }

    let kind = def.kind();
    let answer = match (field, kind) {
      ("kind", _) => Answer::Leaf(MetaLeaf::Kind(MetaKind::Named(kind))),
      ("name", _) => Answer::Leaf(MetaLeaf::Name(def.name())),
      ("description", _) => text(schema.type_meta(id).description()),
      ("specifiedByURL", TypeKind::Scalar) => text(schema.type_meta(id).specified_by()),
      ("fields", TypeKind::Object | TypeKind::Interface) => {
        let Some(include) = self.include_deprecated(ctx, slot) else {
          return;
        };
        let fields = schema.fields_of(id);
        let meta = schema.field_meta_of(id);
        return self.list(
          slot,
          pending,
          schema.field_order_of(id).iter().copied(),
          |at| {
            let (def, meta) = (fields.get(at as usize)?, meta.get(at as usize)?);
            // The meta-fields are in the field tables draft §4.4 puts them in — `__typename` in
            // every composite type's, `__schema` and `__type` in the query root type's — and in no
            // server's `__Type.fields`: draft §4.4 defines them outside the type system, and the
            // builder admits no other `__`-prefixed field name.
            let shown = !is_reserved(schema.name_bytes(def.name()))
              && (include || !meta.deprecation().is_deprecated());
            shown.then_some(Element::Object(Entity::Field { owner: id, at }))
          },
        );
      }
      ("interfaces", TypeKind::Object | TypeKind::Interface) => {
        return self.list(slot, pending, schema.interfaces_in_order(id), |interface| {
          Some(Element::Object(Entity::Type(TypeRef::named(interface))))
        });
      }
      ("possibleTypes", TypeKind::Union) => {
        return self.list(
          slot,
          pending,
          schema.members_of(id).iter().copied(),
          |member| Some(Element::Object(Entity::Type(TypeRef::named(member)))),
        );
      }
      ("possibleTypes", TypeKind::Interface) => {
        // The bitset is scanned word by word whatever it holds, so its width is paid before the
        // scan; each object it yields is then paid as a member like any other list's.
        if !self.visits.take(schema.possible_words()) {
          return self.refuse_read(slot);
        }
        return self.list(slot, pending, schema.possible_object_ids(id), |object| {
          Some(Element::Object(Entity::Type(TypeRef::named(object))))
        });
      }
      ("enumValues", TypeKind::Enum) => {
        let Some(include) = self.include_deprecated(ctx, slot) else {
          return;
        };
        let meta = schema.enum_value_meta_of(id);
        return self.list(
          slot,
          pending,
          schema.enum_value_order_of(id).iter().copied(),
          |at| {
            let meta = meta.get(at as usize)?;
            (include || !meta.deprecation().is_deprecated())
              .then_some(Element::Object(Entity::EnumValue { owner: id, at }))
          },
        );
      }
      ("inputFields", TypeKind::InputObject) => {
        let Some(include) = self.include_deprecated(ctx, slot) else {
          return;
        };
        // An input object's field group is a range of the input-value table, so a position in the
        // group is a row of the table once the range's start is added back.
        let start = def.fields().start();
        let meta = schema.input_field_meta_of(id);
        return self.list(
          slot,
          pending,
          schema.input_field_order_of(id).iter().copied(),
          |at| {
            let meta = meta.get(at as usize)?;
            (include || !meta.deprecation().is_deprecated())
              .then_some(Element::Object(Entity::InputValue(start + at)))
          },
        );
      }
      ("isOneOf", TypeKind::InputObject) => Answer::Leaf(MetaLeaf::Bool(def.is_one_of())),
      _ => Answer::Null,
    };
    self.settle(slot, answer, pending);
  }

  /// `__Field.args` or `__Directive.args`: an argument group, in declaration order, filtered by
  /// `includeDeprecated`.
  fn arguments(&mut self, ctx: &mut V, slot: u32, args: Range32, pending: &mut Pending) {
    let Some(include) = self.include_deprecated(ctx, slot) else {
      return;
    };
    let schema = self.schema;
    let start = args.start();
    let meta = schema.input_meta(args);
    self.list(
      slot,
      pending,
      schema.input_order(args).iter().copied(),
      |at| {
        let meta = meta.get(at as usize)?;
        (include || !meta.deprecation().is_deprecated())
          .then_some(Element::Object(Entity::InputValue(start + at)))
      },
    );
  }

  /// Writes one answer onto `slot`, queueing a meta object to be collected.
  ///
  /// `null` at a non-null position is draft §6.4.3 step 1's field error, as it is for a resolver's
  /// value. The meta-schema answers none — every non-null meta field has an answer in any schema
  /// that built — so this is the branch a disagreement between the meta-schema and this walk would
  /// take, rather than a `null` the schema forbids.
  fn settle(&mut self, slot: u32, answer: Answer, pending: &mut Pending) {
    match answer {
      Answer::Null if self.slots[slot as usize].ty.is_non_null() => {
        let (parent, field) = self.owner(slot);
        self.fail(slot, Raw::NullInNonNull { parent, field });
      }
      Answer::Null => self.slots[slot as usize].state = State::Null,
      Answer::Leaf(leaf) => self.slots[slot as usize].state = State::MetaLeaf(leaf),
      Answer::Object(entity) => {
        self.slots[slot as usize].state = State::MetaObject(entity);
        self.defer(pending, slot);
      }
    }
  }

  /// Answers a list-typed meta field with one element per member `positions` yields that
  /// `element` admits, each charged one visit before it is read.
  ///
  /// The two halves are separate so that the charge can stand between them: `positions` only steps
  /// through an order table or a range, and `element` is where a member is read — its metadata,
  /// for the `includeDeprecated` filter — and where one the filter drops answers `None`.
  ///
  /// A refusal — the visit ceiling, or [`max_response_slots`](super::Limits::max_response_slots)
  /// at an element — restores everything this list created and fails the list, as `complete`'s
  /// list arm does: the elements were about to be nulled with it, so `data` is unchanged, and their
  /// positions stop being charged against whatever comes next.
  fn list<T>(
    &mut self,
    slot: u32,
    pending: &mut Pending,
    positions: impl Iterator<Item = T>,
    mut element: impl FnMut(T) -> Option<Element>,
  ) {
    // Every list field of the meta-schema is declared one, so the item type is there; a meta-schema
    // that said otherwise would be answered with the `null` that invents nothing.
    let Some(item) = self.slots[slot as usize].ty.nullable().list_item() else {
      return self.settle(slot, Answer::Null, pending);
    };
    self.slots[slot as usize].state = State::List;
    let mark = self.mark(slot);
    let queued = *pending;
    // An element index is below `max_response_slots`, which is a `u32`, because every element was
    // admitted by `push_child` under it.
    let mut index = 0u32;
    for position in positions {
      if !self.visits.take(1) {
        let (parent, field) = self.owner(slot);
        let limit = self.visits.limit();
        return self.unwind(
          slot,
          mark,
          pending,
          queued,
          Raw::IntrospectionBudget {
            parent,
            field,
            limit,
          },
        );
      }
      let Some(member) = element(position) else {
        continue;
      };
      let state = match member {
        Element::Object(entity) => State::MetaObject(entity),
        Element::Leaf(leaf) => State::MetaLeaf(leaf),
      };
      let Some(child) = self.push_child(slot, Key::Index(index), item, state) else {
        let (parent, field) = self.owner(slot);
        let limit = self.limits.max_response_slots.get();
        return self.unwind(
          slot,
          mark,
          pending,
          queued,
          Raw::ResponseBudget {
            parent,
            field,
            limit,
          },
        );
      };
      if let Element::Object(_) = member {
        self.defer(pending, child);
      }
      index += 1;
    }
  }

  /// Takes back everything a refused list created, and fails it.
  ///
  /// The queue goes back first, to what it held before the list's first element: those elements
  /// are the queue's suffix, and the restore is about to truncate them.
  fn unwind(&mut self, slot: u32, mark: Mark, pending: &mut Pending, queued: Pending, raw: Raw) {
    self.restore(slot, mark);
    *pending = queued;
    if queued.tail != NONE {
      self.slots[queued.tail as usize].next_ready = NONE;
    }
    self.fail(slot, raw);
  }

  /// Draft §4's `includeDeprecated`, read through draft §6.4.1: `None` when coercing it raised and
  /// the field is already nulled.
  ///
  /// The meta-schema declares it `Boolean = false`, and an explicit `null` asks for the same thing:
  /// draft §4 gives the argument no third meaning, so both are `false` here rather than a reading
  /// of the default's text. A variable is read through [`Values::as_bool`], as `@skip`'s is.
  fn include_deprecated(&mut self, ctx: &mut V, slot: u32) -> Option<bool> {
    if self.coerce_arguments(ctx, slot) != Coercion::Coerced {
      return None;
    }
    let read = match self.scratch_args.first().map(|argument| &argument.source) {
      None
      | Some(ArgumentSource::SchemaDefault)
      | Some(ArgumentSource::Literal(InputValue::Null(_))) => Some(false),
      Some(ArgumentSource::Literal(InputValue::Boolean(literal))) => Some(literal.value()),
      Some(ArgumentSource::Variable { value, .. }) => {
        if ctx.is_null(value) {
          Some(false)
        } else {
          ctx.as_bool(value)
        }
      }
      Some(ArgumentSource::Literal(_)) => None,
    };
    // The values are this walk's to drop: it is their only reader, and it has read them.
    self.retire_arguments();
    if read.is_none() {
      self.refuse_argument(slot);
    }
    read
  }

  /// Draft §4.4's `__type(name:)`: the type its argument names, read through draft §6.4.1 — or
  /// `None` when the field has been failed instead.
  ///
  /// A literal is decoded with the lexer's own `StringValue` decoder and never by a UTF-8
  /// conversion with a fallback: a spelling that does not decode is an argument that is not a
  /// string, and `__type` raises rather than looking up a name nobody wrote. A variable is read
  /// through [`Values::as_str`]. A name no type has answers `null`, with no error — draft §4.4's
  /// own answer for it.
  fn named_type(&mut self, ctx: &mut V, slot: u32) -> Option<Answer> {
    if self.coerce_arguments(ctx, slot) != Coercion::Coerced {
      return None;
    }
    let schema = self.schema;
    let read = match self.scratch_args.first().map(|argument| &argument.source) {
      Some(ArgumentSource::Literal(InputValue::String(literal))) => {
        literal_type(schema, literal.source().as_ref(), &mut self.visits)
      }
      Some(ArgumentSource::Variable { value, .. }) => match ctx.as_str(value) {
        Some(name) if self.visits.take_bytes(name.len()) => {
          Read::Named(schema.type_by_name(name.as_bytes()).map(|(id, _)| id))
        }
        Some(_) => Read::Refused,
        None => Read::Invalid,
      },
      // `name` is `String!` with no default, so draft §6.4.1 has already raised for an absent or
      // `null` argument; anything left is a literal that is not a string.
      _ => Read::Invalid,
    };
    self.retire_arguments();
    match read {
      Read::Named(Some(id)) => Some(Answer::Object(Entity::Type(TypeRef::named(id)))),
      Read::Named(None) => Some(Answer::Null),
      Read::Invalid => {
        self.refuse_argument(slot);
        None
      }
      Read::Refused => {
        self.refuse_read(slot);
        None
      }
    }
  }

  /// Raises draft §6.4.1 step 5.j's field error for the one argument of the meta field at `slot`,
  /// located at the value the document wrote for it.
  fn refuse_argument(&mut self, slot: u32) {
    let Some(argument) = self.declared_argument(slot) else {
      // Unreachable against a meta-schema that declares the argument, which every schema this
      // crate builds does. Without one there was no value to refuse, so the position is answered
      // as a field with no argument would be — `null`, or step 1's error where null is not allowed
      // — rather than left a placeholder.
      if self.slots[slot as usize].ty.is_non_null() {
        let (parent, field) = self.owner(slot);
        return self.fail(slot, Raw::NullInNonNull { parent, field });
      }
      self.slots[slot as usize].state = State::Null;
      return;
    };
    let raw = Raw::ArgumentCoercion {
      field: self.meta[slot as usize].field_sym,
      argument: argument.name(),
      ty: argument.ty(),
    };
    match self.written_value(slot, argument.name()) {
      Some(location) => self.fail_at(slot, raw, location),
      None => self.fail(slot, raw),
    }
  }

  /// Raises the visit ceiling's refusal at `slot`, for a read this walk was about to make.
  fn refuse_read(&mut self, slot: u32) {
    let (parent, field) = self.owner(slot);
    let limit = self.visits.limit();
    self.fail(
      slot,
      Raw::IntrospectionBudget {
        parent,
        field,
        limit,
      },
    );
  }

  /// The single argument the meta field at `slot` declares: `includeDeprecated`, or `__type`'s
  /// `name`.
  fn declared_argument(&self, slot: u32) -> Option<&'a InputValueDef> {
    let schema: &'a Schema = self.schema;
    let meta = &self.meta[slot as usize];
    let definition = schema.field(meta.parent_type, meta.field_sym)?;
    schema.inputs(definition.args()).first()
  }

  /// Where the document wrote `argument`'s value at `slot`, for an error that points at it.
  ///
  /// The scan stops at the first entry spelled like the argument, which is the entry draft §6.4.1's
  /// scan stopped at — so it reads only entries that scan already paid for, once more, on a path
  /// taken at most once per position.
  fn written_value(&self, slot: u32, argument: Sym) -> Option<SimpleSpan> {
    let name = self.schema.name_bytes(argument);
    self.meta[slot as usize]
      .field?
      .arguments()?
      .arguments()
      .iter()
      .find(|written| written.name().source().as_ref() == name)
      .map(|written| *written.value().as_span())
  }

  /// Puts a meta object at the back of the queue.
  fn defer(&mut self, pending: &mut Pending, slot: u32) {
    self.slots[slot as usize].next_ready = NONE;
    if pending.tail == NONE {
      pending.head = slot;
    } else {
      self.slots[pending.tail as usize].next_ready = slot;
    }
    pending.tail = slot;
  }

  /// Takes the oldest meta object off the queue, unlinking it.
  fn next_pending(&mut self, pending: &mut Pending) -> Option<u32> {
    let head = pending.head;
    if head == NONE {
      return None;
    }
    pending.head = self.slots[head as usize].next_ready;
    if pending.head == NONE {
      pending.tail = NONE;
    }
    self.slots[head as usize].next_ready = NONE;
    Some(head)
  }
}

/// A `String` meta field: the text a handle names, or `null` for none.
#[inline]
fn text(text: Text) -> Answer {
  if text.is_some() {
    Answer::Leaf(MetaLeaf::Text(text))
  } else {
    Answer::Null
  }
}

/// `queryType`, `mutationType` or `subscriptionType`.
#[inline]
fn root(schema: &Schema, operation: RootOperation) -> Answer {
  match schema.root(operation) {
    Some(id) => Answer::Object(Entity::Type(TypeRef::named(id))),
    None => Answer::Null,
  }
}

/// The type a `__type(name:)` literal names, read off its spelling by the lexer's `StringValue`
/// decoder and charged one pass over the spelling per walk.
///
/// The value is looked up in place when it is one piece — any inline string without an escape and
/// any one-line block string — and assembled only when an escape or a line break splits it. That
/// assembly is the one allocation this walk makes, it is freed before this returns, and it happens
/// only where an escape forces it, which is the rule the introspection door keeps for the text it
/// decodes.
fn literal_type(schema: &Schema, spelling: &[u8], visits: &mut Visits) -> Read {
  // The piece walk, and the probe of its first piece, which is no longer than the spelling.
  if !visits.take(byte_units(spelling.len())) {
    return Read::Refused;
  }
  let mut pieces = 0usize;
  let mut found = None;
  let walked = string_value_pieces(spelling, |piece| {
    pieces += 1;
    if pieces == 1 {
      found = schema.type_by_name(piece.as_bytes()).map(|(id, _)| id);
    }
  });
  if walked.is_err() {
    return Read::Invalid;
  }
  // No piece is the empty string, which names no type, and one piece is the whole value.
  if pieces <= 1 {
    return Read::Named(found);
  }
  // The decoder's sizing walk, its copy, and the probe of what it assembled.
  if !visits.take(byte_units(spelling.len()).saturating_mul(3)) {
    return Read::Refused;
  }
  let mut value = String::new();
  match decode_string_value(spelling, &mut value) {
    Ok(()) => Read::Named(schema.type_by_name(value.as_bytes()).map(|(id, _)| id)),
    Err(_) => Read::Invalid,
  }
}
