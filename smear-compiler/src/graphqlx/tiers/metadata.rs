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
//! An executable directive's own type parameter that one of its arguments' types holds, directly,
//! in a list or as an argument of a named type, is filled by the operations that apply it, which a
//! schema cannot know, and its bounds do not narrow what they choose: met there, it makes every
//! definition `reached` holds runtime, each entered at that occurrence. A parameter only its
//! defaults or its bounds name receives no value at run time, and a parameter of any other
//! definition is filled by the schema's own types.
//!
//! A definition is metadata when `reached` holds it and `runtime` does not, and the tier rule
//! judges nothing within it. A definition both reach is judged. A value names nothing either way:
//! a `TypeRef` that names a definition neither makes it metadata nor makes a metadata definition
//! runtime.
//!
//! # Where a reach entered
//!
//! For each definition of `reached` that it makes runtime, the walk of `runtime` keeps where that
//! reach entered from outside metadata, and the tier rule labels its reports within it there. For
//! a definition that the type of a schema root, or of an occurrence of a root definition, an
//! executable directive among them, names, it is that root or that occurrence; for one that a
//! definition the walk made runtime names, it is that definition's own entry, carried through the
//! queue. So when a field's argument takes `B` and `B` holds `A`, `A`'s entry is `B` in that
//! argument, where the chain starts, and not `B`'s field. The walk's order is fixed: the root
//! definitions in id order, each definition's occurrences in the order the section above lists
//! them, the schema's roots, then the queue, first in, first out; and each type is walked once,
//! from the first occurrence that reaches it. No entry depends on the order of the files or of the
//! dependencies.
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
//! defaults fill is walked once for all its uses. An application's complete type is pushed whole,
//! and not argument by argument, so the bare applications of one directive, which share the one
//! complete type its defaults fill, read its arguments once between them. What an executable
//! directive's own parameter can receive is entered once, however many such parameters the walk
//! meets: one look at each definition. Each closure reads each definition's occurrences at most
//! once, but that `runtime` reads an executable directive's arguments' types twice: first as the
//! directive's own, then with the rest of its occurrences, when each is a type walked already and
//! costs one look. The work is linear in the definitions, their occurrences, the applications and
//! the type arguments they write, the types and their arguments; nothing recurses, nothing is
//! interned and nothing is reported. When `reached` holds nothing, `runtime` is not walked. Both
//! are least sets, so the answer depends on no order.

use std::{vec, vec::Vec};

use tokora::diagnostic::Location;

use crate::graphqlx::{
  id::{ApplicationId, DefId, TyId, TypeExprId, dense},
  model::{
    DefKind, DirectiveLocation, ModelBuilder, Occurrence, Origin, Root, Shape, TypeBound,
    TypeParamRef, WrittenParam,
  },
};

/// The definitions only compile-time metadata reaches, found once, before the tiers.
pub(super) struct Metadata {
  /// For each definition, by id: whether it is metadata.
  pub(super) flags: Vec<bool>,
  /// Each definition compile-time metadata reaches that a type outside it reaches too, with where
  /// that reach entered from outside metadata, in id order.
  pub(super) entries: Vec<(DefId, Location)>,
}

/// Which definitions of `builder` are compile-time metadata: those that what metadata reaches
/// holds and that what is used outside it does not; and, for each that both reach, where the reach
/// from outside metadata entered.
pub(super) fn metadata(builder: &ModelBuilder) -> Metadata {
  let reached = reached(builder);
  if !reached.contains(&true) {
    return Metadata {
      flags: reached,
      entries: Vec::new(),
    };
  }
  let entered = used(builder, &reached);
  let mut flags: Vec<bool> = Vec::with_capacity(reached.len());
  let mut entries: Vec<(DefId, Location)> = Vec::new();
  for (index, (&marked, entry)) in reached.iter().zip(&entered).enumerate() {
    match *entry {
      Some(at) => {
        flags.push(false);
        entries.push((DefId::new(dense(index)), at));
      }
      None => flags.push(marked),
    }
  }
  Metadata { flags, entries }
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
  // Every type argument of every application.
  application_roots(builder, &mut seen, &mut types);

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

/// Pushes onto `types` the roots every application gives, each unless `seen` says it was pushed
/// before: the type of each type argument it writes, whether or not a phase refused the
/// application, as for a count its directive does not take; and its complete type, its
/// directive's instantiation, whose arguments are its complete type arguments, the slots its
/// directive's defaults fill among them. An application has no complete type until its arguments
/// complete, and the unresolved type has no argument.
///
/// The complete type is pushed whole, and not argument by argument: the applications that share
/// it, as the bare applications of one directive do, push it once between them, and the closure
/// reads its arguments once, when it reaches it. Its directive, which is no input kind, never joins
/// what metadata reaches. So each application costs one push, and one for each type argument it
/// writes, however many slots its directive's defaults fill.
fn application_roots(builder: &ModelBuilder, seen: &mut [bool], types: &mut Vec<TyId>) {
  for index in 0..builder.application_count() {
    let application = ApplicationId::new(index);
    for &argument in builder.application_type_args(application) {
      if let Some(ty) = typed(builder, argument) {
        push(seen, types, ty);
      }
    }
    if let Some(ty) = builder.application_ty(application) {
      push(seen, types, ty);
    }
  }
}

/// What is used outside metadata, by definition id, given what metadata reaches, `reached`: for
/// each definition of `reached` that a type outside metadata names, through lists and every type
/// argument, or that an executable directive's own type parameter can receive, where that reach
/// entered. The walk starts from each definition [`runtime_root`] answers for, in id order, an
/// executable directive's arguments' types first, and then from the schema's written roots, each
/// occurrence the entry of what its type names; and reads each definition it makes runtime in
/// turn, with its entry.
fn used(builder: &ModelBuilder, reached: &[bool]) -> Vec<Option<Location>> {
  let mut walk = Walk {
    builder,
    reached,
    entered: vec![None; builder.def_count() as usize],
    looked: vec![false; builder.ty_count() as usize],
    stack: Vec::new(),
    queue: Vec::new(),
    flooded: false,
  };
  let mut written: Vec<TypeExprId> = Vec::new();
  for index in 0..builder.def_count() {
    let def = DefId::new(index);
    if !runtime_root(builder, def, reached) {
      continue;
    }
    // A directive that is a root is executable: its arguments' types are walked with the
    // directive as the one whose own parameters the operations fill. No other type holds those
    // parameters, so no walk before this one has looked at a type that does.
    if builder.def_kind(def) == Some(DefKind::Directive) {
      for argument in builder.def_members(def) {
        if let Some(occurrence) = builder.member_ty(argument) {
          walk.occurrence(occurrence, None, Some(def));
        }
      }
    }
    occurrences(builder, def, &mut written);
    for &occurrence in &written {
      walk.occurrence(occurrence, None, None);
    }
  }
  for root in builder.schema_roots().into_iter().flatten() {
    if let Root::Written(occurrence) = root {
      walk.occurrence(occurrence, None, None);
    }
  }
  let mut head = 0usize;
  while let Some(&(def, at)) = walk.queue.get(head) {
    head = head.saturating_add(1);
    occurrences(builder, def, &mut written);
    for &occurrence in &written {
      walk.occurrence(occurrence, Some(at), None);
    }
  }
  walk.entered
}

/// The walk of what is used outside metadata.
struct Walk<'a> {
  /// The model being written.
  builder: &'a ModelBuilder,
  /// What metadata reaches, by definition id.
  reached: &'a [bool],
  /// For each definition of `reached` the walk made runtime, by id, where that reach entered.
  entered: Vec<Option<Location>>,
  /// Each type the walk pushed, by id.
  looked: Vec<bool>,
  /// The types still to look at.
  stack: Vec<TyId>,
  /// Each definition the walk made runtime, with where that reach entered, in that order, each
  /// read in turn, first in, first out.
  queue: Vec<(DefId, Location)>,
  /// Whether an executable directive's own type parameter made every definition of `reached`
  /// runtime already.
  flooded: bool,
}

impl Walk<'_> {
  /// Walks the type of the occurrence `written`, once a phase gave it one, with `at` as the entry
  /// of what it names, or, when `at` is `None`, where `written` is written. `own` is the
  /// executable directive whose argument's type it is, whose own type parameters the operations
  /// fill.
  fn occurrence(&mut self, written: TypeExprId, at: Option<Location>, own: Option<DefId>) {
    let builder = self.builder;
    let Some(occurrence) = builder.occurrence(written) else {
      return;
    };
    let Some(ty) = occurrence.ty else {
      return;
    };
    let entry = at.unwrap_or(occurrence.location);
    self.walk(ty, entry, own);
  }

  /// Walks `ty` and every type under it, each that the walk has not pushed before: a list leads to
  /// its element, and a named type to every argument, whatever its definition's kind. A definition
  /// of `reached` that a named type names is made runtime and queued, the first time, with `at` as
  /// its entry. A type parameter of `own`, the executable directive whose argument's type `ty` is,
  /// can receive any of them, so meeting one makes them all runtime, with `at` as their entry.
  fn walk(&mut self, ty: TyId, at: Location, own: Option<DefId>) {
    let builder = self.builder;
    push(&mut self.looked, &mut self.stack, ty);
    while let Some(ty) = self.stack.pop() {
      match builder.ty_shape(ty) {
        Some(Shape::List(element)) => push(&mut self.looked, &mut self.stack, element),
        Some(Shape::Named(def)) => {
          let index = def.get() as usize;
          if self.reached.get(index).copied().unwrap_or(false)
            && let Some(slot) = self.entered.get_mut(index)
            && slot.is_none()
          {
            *slot = Some(at);
            self.queue.push((def, at));
          }
          for &argument in builder.ty_args(ty) {
            push(&mut self.looked, &mut self.stack, argument);
          }
        }
        Some(Shape::Param(TypeParamRef { def, .. })) => {
          if own == Some(def) {
            self.flood(at);
          }
        }
        Some(Shape::Unresolved) | None => {}
      }
    }
  }

  /// Makes runtime every definition of `reached` the walk has not, each with `at` as its entry and
  /// queued: what an executable directive's own type parameter, which one of its arguments' types
  /// holds at `at`, can receive from the operations that apply it. That is any of them, and no
  /// schema bounds it. It runs once, since after it nothing of `reached` is left.
  fn flood(&mut self, at: Location) {
    if self.flooded {
      return;
    }
    self.flooded = true;
    let reached = self.reached;
    for (index, &marked) in reached.iter().enumerate() {
      if marked
        && let Some(slot) = self.entered.get_mut(index)
        && slot.is_none()
      {
        *slot = Some(at);
        self.queue.push((DefId::new(dense(index)), at));
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

#[cfg(test)]
mod tests;
