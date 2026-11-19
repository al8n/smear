//! What only compile-time metadata reaches: the definitions the tier rule does not judge, found
//! once, at the start of the tier phase, over every package together.
//!
//! # The roots
//!
//! Compile-time metadata is every type a type-system directive writes, a type-system directive
//! being one that lists no executable location, such as `FIELD` or `QUERY`, where an operation
//! would apply it: its arguments' types, its type parameters' defaults and its bounds'
//! interfaces, every package's directives, the built-in ones among them. It is also every type
//! argument of every directive application, wherever the application stands and whatever directive
//! it applies: each one written, by its own complete type, even where a phase refused the
//! application, as for a count its directive does not take; and each of its complete type
//! arguments, the slots its directive's defaults fill among them. An application whose directive
//! names nothing keeps no type argument, so it roots nothing.
//!
//! An executable directive, one that lists an executable location, roots nothing here: an
//! operation applies it and gives its arguments at run time, so what its arguments' types name is
//! used outside metadata. The types it writes are exempt all the same, as any directive's are.
//!
//! # The two closures
//!
//! A definition's occurrences are the types its contributions write, every contribution's: each
//! direct member's type, each field's arguments' after the field's; each union member; each
//! `implements` entry; each type parameter's default; and each bound's interface. Its
//! applications' type arguments are roots, and no value is an occurrence. Each is read by its
//! complete type, so an alias is its expansion and the slots it leaves to defaults are filled. An
//! occurrence with no type, or with the unresolved type, names nothing: it lies within a phase's
//! error.
//!
//! What metadata reaches, `reached`, is the least set that holds each scalar, enum and input
//! object a supplied package defines that a root names, and what each such definition's
//! occurrences name in turn. A list names what its element names, and a named type its definition
//! and what each of its arguments names, whatever its definition's kind. An object, an interface,
//! a union or a directive is never held, and its occurrences are not read from here: an object can
//! be a runtime type that no type names, through `implements`, so a closure over names cannot
//! clear one. A built-in is never held, but its arguments are looked through: `HashMap<string, In>`
//! and `Sized<In>`, a bound's interface, both name `In`.
//!
//! What is used outside metadata, `runtime`, starts from every definition but a type-system
//! directive and those `reached` holds, chosen by its kind: a scalar, an enum or an input object
//! `reached` does not hold, every object, interface and union, and every executable directive.
//! It starts from the schema's written roots too. Each such definition's occurrences are read,
//! and a type names every definition in its tree, through every list and every type argument,
//! whatever the definition's kind: a definition of `reached` so named joins `runtime`, and its own
//! occurrences are read in turn, first in, first out.
//!
//! A definition is metadata when `reached` holds it and `runtime` does not, and the tier rule
//! judges nothing within it. A definition both reach is judged. A value names nothing either way:
//! a `TypeRef` that names a definition neither makes it metadata nor makes a metadata definition
//! runtime.
//!
//! # Why it is not the `TypeRef` reach
//!
//! `reach::Reach` follows what a value can hold, and hides a scalar's arguments when it is no
//! collection. A tier counts every argument, so `runtime` follows arguments no value holds: a field
//! of `Ref<Binding>` raises its definition's tier through `Binding`, which a value of `Ref` never
//! holds. `Reach` also reads the finished model, while a backend reads this answer off the model,
//! so a phase finds it on the builder. What the two share is their shape: dense ids, guarded
//! worklists, and each node read once.
//!
//! # What it costs
//!
//! Each closure keeps one bit for each definition and one for each type, and pushes a type only
//! when its bit was clear, the first push of each occurrence's type included: a type is walked at
//! most once by each, however many occurrences write it, so a bare `Big` whose thousand slots
//! defaults fill is walked once for all its uses. Each closure reads each definition's
//! occurrences at most once. The work is linear in the definitions, their occurrences, the types
//! and their arguments; nothing recurses, nothing is interned and nothing is reported. When
//! `reached` holds nothing, `runtime` is not walked. Both are least sets, so the answer depends on
//! no order.

use std::{vec, vec::Vec};

use crate::graphqlx::{
  id::{ApplicationId, DefId, TyId, TypeExprId},
  model::{
    DefKind, DirectiveLocation, ModelBuilder, Occurrence, Origin, Root, Shape, TypeBound,
    WrittenParam,
  },
};

/// The definitions only compile-time metadata reaches, found once, before the tiers.
pub(super) struct Metadata {
  /// For each definition, by id: whether it is metadata.
  pub(super) flags: Vec<bool>,
}

/// Which definitions of `builder` are compile-time metadata: those that what metadata reaches
/// holds and that what is used outside it does not.
pub(super) fn metadata(builder: &ModelBuilder) -> Metadata {
  let reached = reached(builder);
  if !reached.contains(&true) {
    return Metadata { flags: reached };
  }
  let runtime = used(builder, &reached);
  let flags: Vec<bool> = reached
    .iter()
    .zip(&runtime)
    .map(|(&marked, &outside): (&bool, &bool)| marked && !outside)
    .collect();
  Metadata { flags }
}

/// What metadata reaches, by definition id: each scalar, enum and input object a supplied package
/// defines that a type a type-system directive writes, or an application's type argument, names,
/// through lists and every type argument, and what each such definition's occurrences name in
/// turn.
fn reached(builder: &ModelBuilder) -> Vec<bool> {
  let mut reached: Vec<bool> = vec![false; builder.def_count() as usize];
  let mut seen: Vec<bool> = vec![false; builder.ty_count() as usize];
  let mut types: Vec<TyId> = Vec::new();
  let mut defs: Vec<DefId> = Vec::new();
  let mut written: Vec<TypeExprId> = Vec::new();

  // Every type a type-system directive writes.
  for index in 0..builder.def_count() {
    let def = DefId::new(index);
    if builder.def_kind(def) == Some(DefKind::Directive) && !executable_directive(builder, def) {
      occurrences(builder, def, &mut written);
      for &occurrence in &written {
        if let Some(ty) = typed(builder, occurrence) {
          push(&mut seen, &mut types, ty);
        }
      }
    }
  }
  // Every type argument of every application: each written one by its own type, whether or not a
  // phase refused the application, and each of its complete ones. An application has no type until
  // its arguments complete, and the unresolved type has no argument.
  for index in 0..builder.application_count() {
    let application = ApplicationId::new(index);
    for &argument in builder.application_type_args(application) {
      if let Some(ty) = typed(builder, argument) {
        push(&mut seen, &mut types, ty);
      }
    }
    if let Some(ty) = builder.application_ty(application) {
      for &argument in builder.ty_args(ty) {
        push(&mut seen, &mut types, argument);
      }
    }
  }

  loop {
    while let Some(ty) = types.pop() {
      match builder.ty_shape(ty) {
        Some(Shape::List(element)) => push(&mut seen, &mut types, element),
        Some(Shape::Named(def)) => {
          for &argument in builder.ty_args(ty) {
            push(&mut seen, &mut types, argument);
          }
          if builder.def_kind(def).is_some_and(input_kind)
            && builder.def_origin(def) == Some(Origin::User)
            && let Some(slot) = reached.get_mut(def.get() as usize)
            && !*slot
          {
            *slot = true;
            defs.push(def);
          }
        }
        Some(Shape::Param(_) | Shape::Unresolved) | None => {}
      }
    }
    let Some(def) = defs.pop() else {
      break;
    };
    occurrences(builder, def, &mut written);
    for &occurrence in &written {
      if let Some(ty) = typed(builder, occurrence) {
        push(&mut seen, &mut types, ty);
      }
    }
  }
  reached
}

/// What is used outside metadata, by definition id, given what metadata reaches, `reached`: each
/// definition of `reached` that a type outside metadata names, through lists and every type
/// argument. The walk starts from each definition [`runtime_root`] answers for, in id order, and
/// then from the schema's written roots, and reads each definition it makes runtime in turn.
fn used(builder: &ModelBuilder, reached: &[bool]) -> Vec<bool> {
  let mut walk = Walk {
    builder,
    reached,
    runtime: vec![false; builder.def_count() as usize],
    looked: vec![false; builder.ty_count() as usize],
    stack: Vec::new(),
    queue: Vec::new(),
  };
  let mut written: Vec<TypeExprId> = Vec::new();
  for index in 0..builder.def_count() {
    let def = DefId::new(index);
    if runtime_root(builder, def, reached) {
      occurrences(builder, def, &mut written);
      for &occurrence in &written {
        walk.occurrence(occurrence);
      }
    }
  }
  for root in builder.schema_roots().into_iter().flatten() {
    if let Root::Written(occurrence) = root {
      walk.occurrence(occurrence);
    }
  }
  let mut head = 0usize;
  while let Some(&def) = walk.queue.get(head) {
    head = head.saturating_add(1);
    occurrences(builder, def, &mut written);
    for &occurrence in &written {
      walk.occurrence(occurrence);
    }
  }
  walk.runtime
}

/// The walk of what is used outside metadata.
struct Walk<'a> {
  /// The model being written.
  builder: &'a ModelBuilder,
  /// What metadata reaches, by definition id.
  reached: &'a [bool],
  /// Each definition of `reached` the walk made runtime, by id.
  runtime: Vec<bool>,
  /// Each type the walk pushed, by id.
  looked: Vec<bool>,
  /// The types still to look at.
  stack: Vec<TyId>,
  /// Each definition the walk made runtime, in that order, each read in turn, first in, first out.
  queue: Vec<DefId>,
}

impl Walk<'_> {
  /// Walks the type of the occurrence `written`, once a phase gave it one.
  fn occurrence(&mut self, written: TypeExprId) {
    if let Some(ty) = typed(self.builder, written) {
      self.walk(ty);
    }
  }

  /// Walks `ty` and every type under it, each that the walk has not pushed before: a list leads to
  /// its element, and a named type to every argument, whatever its definition's kind. A definition
  /// of `reached` that a named type names is made runtime and queued, the first time.
  fn walk(&mut self, ty: TyId) {
    let builder = self.builder;
    push(&mut self.looked, &mut self.stack, ty);
    while let Some(ty) = self.stack.pop() {
      match builder.ty_shape(ty) {
        Some(Shape::List(element)) => push(&mut self.looked, &mut self.stack, element),
        Some(Shape::Named(def)) => {
          let index = def.get() as usize;
          if self.reached.get(index).copied().unwrap_or(false)
            && let Some(slot) = self.runtime.get_mut(index)
            && !*slot
          {
            *slot = true;
            self.queue.push(def);
          }
          for &argument in builder.ty_args(ty) {
            push(&mut self.looked, &mut self.stack, argument);
          }
        }
        Some(Shape::Param(_) | Shape::Unresolved) | None => {}
      }
    }
  }
}

/// Pushes `ty` onto `stack` unless `pushed` says it was pushed before, and records it pushed.
fn push(pushed: &mut [bool], stack: &mut Vec<TyId>, ty: TyId) {
  if let Some(slot) = pushed.get_mut(ty.get() as usize)
    && !*slot
  {
    *slot = true;
    stack.push(ty);
  }
}

/// The type of the occurrence `written`, once a phase gave it one.
fn typed(builder: &ModelBuilder, written: TypeExprId) -> Option<TyId> {
  builder
    .occurrence(written)
    .and_then(|occurrence: Occurrence<'_>| occurrence.ty)
}

/// Fills `into`, cleared first, with the occurrences of `def`, in this order: each direct member's
/// type in id order, each field's followed by its arguments'; each union member; each `implements`
/// entry; each type parameter's default in index order; and each bound's interface in written
/// order.
fn occurrences(builder: &ModelBuilder, def: DefId, into: &mut Vec<TypeExprId>) {
  into.clear();
  for member in builder.def_members(def) {
    if let Some(ty) = builder.member_ty(member) {
      into.push(ty);
    }
    for argument in builder.member_arguments(member) {
      if let Some(ty) = builder.member_ty(argument) {
        into.push(ty);
      }
    }
  }
  into.extend_from_slice(builder.def_union_members(def));
  into.extend_from_slice(builder.def_implements(def));
  for index in 0..builder.type_param_count(def) {
    if let Some(WrittenParam {
      default: Some(default),
      ..
    }) = builder.type_param(def, index)
    {
      into.push(default);
    }
  }
  into.extend(
    builder
      .def_bounds(def)
      .iter()
      .map(|bound: &TypeBound| bound.interface()),
  );
}

/// Whether a definition of `kind` may be compile-time metadata: a scalar, an enum or an input
/// object. A match with no wildcard arm, so that a kind added later stops the build here until it
/// is said what it is.
fn input_kind(kind: DefKind) -> bool {
  match kind {
    DefKind::Scalar | DefKind::Enum | DefKind::InputObject => true,
    DefKind::Object | DefKind::Interface | DefKind::Union | DefKind::Directive => false,
  }
}

/// Whether `def` is a root of what is used outside metadata, before any reach, given what
/// metadata reaches, `reached`: a scalar, an enum or an input object `reached` does not hold;
/// every object, interface and union; and an executable directive. A match with no wildcard arm,
/// so that a kind added later stops the build here until it is said what it is.
fn runtime_root(builder: &ModelBuilder, def: DefId, reached: &[bool]) -> bool {
  let Some(kind) = builder.def_kind(def) else {
    return false;
  };
  match kind {
    DefKind::Scalar | DefKind::Enum | DefKind::InputObject => {
      !reached.get(def.get() as usize).copied().unwrap_or(false)
    }
    DefKind::Object | DefKind::Interface | DefKind::Union => true,
    DefKind::Directive => executable_directive(builder, def),
  }
}

/// Whether the directive `def` lists an executable location, where an operation applies it and
/// gives its arguments at run time. A directive that lists both kinds of location is executable.
fn executable_directive(builder: &ModelBuilder, def: DefId) -> bool {
  builder.def_locations(def).iter().copied().any(executable)
}

/// Whether `location` is one of the eight executable locations, which an operation's document
/// holds. A match with no wildcard arm, so that a location added later stops the build here until
/// it is said which kind it is.
fn executable(location: DirectiveLocation) -> bool {
  match location {
    DirectiveLocation::Query
    | DirectiveLocation::Mutation
    | DirectiveLocation::Subscription
    | DirectiveLocation::Field
    | DirectiveLocation::FragmentDefinition
    | DirectiveLocation::FragmentSpread
    | DirectiveLocation::InlineFragment
    | DirectiveLocation::VariableDefinition => true,
    DirectiveLocation::Schema
    | DirectiveLocation::Scalar
    | DirectiveLocation::Object
    | DirectiveLocation::FieldDefinition
    | DirectiveLocation::ArgumentDefinition
    | DirectiveLocation::Interface
    | DirectiveLocation::Union
    | DirectiveLocation::Enum
    | DirectiveLocation::EnumValue
    | DirectiveLocation::InputObject
    | DirectiveLocation::InputFieldDefinition
    | DirectiveLocation::DirectiveDefinition => false,
  }
}
