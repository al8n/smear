//! Where a type parameter stands, and whether a type fits where it stands: what the rules that
//! judge an instantiation's arguments share.
//!
//! # Places
//!
//! A place is a kind of position a type parameter can stand in: a field's type, which takes an
//! output type; a field argument's, an input field's and a directive argument's, which take an
//! input type; what a value an operation's field argument takes can hold, which holds no
//! `TypeRef`; and the whole type of a field of a OneOf input object, written nullable, which takes
//! a nullable type. [`Positions`] finds, once for a walk, the places each type parameter of every
//! package's definitions stands in, and for each, where in its own definition it stands there or
//! is passed to a parameter that does, the witness a diagnostic labels, and the member whose type
//! the place came from. A place that is a member's whole type does not pass from a list to its
//! element, or from a collection to its arguments: the list or the collection stands there.
//!
//! A parameter stands where its definition puts it: in the complete type of each of its members,
//! a field's arguments among them, every contribution's, a slot a default fills included; through
//! a list's element and a collection's arguments, which a value holds; and, given as an argument to
//! another definition, which an object, an interface, a union, an input object and a directive
//! are, wherever that definition's parameter stands. A scalar that is no collection and an enum
//! hide their arguments. A union member and an application's type, the directive's instantiation,
//! pass a parameter on and put it nowhere themselves. An `implements` entry, a bound and a
//! parameter's default put it nowhere: the implementing type declares each field itself, a bound's
//! argument is judged where it is written, and what a default gives is judged where an occurrence
//! leaves its slot out.
//!
//! The table is a graph whose nodes are the slots, one for each type parameter of each definition,
//! and the open types the roots reach, each interned type once: a type entered before is passed
//! over, whatever reached it. A type parameter's type leads to its slot; a list to its element and a
//! collection to each argument; and a slot of another definition to each argument given to it, the
//! edge carrying where that is written. Each root gives its type its place, and the places spread
//! along the edges, each node gaining each place once, a slot keeping the witness it gained it
//! with: across an edge out of a slot the witness becomes the edge's.
//!
//! What an operation's argument reaches is found last: each field argument's type is given it,
//! and it spreads along the same edges, and from a named type of an input object to each argument
//! a value of the input object holds. So with `type Q<A> { f(a: A): Int }` and
//! `input Wrapped<W> { v: W }`, the `T` of `type D<T> { q: Q<Wrapped<T>> }` is reached from an
//! operation's argument: `Q`'s field argument takes a `Wrapped<T>`, whose value holds a `T`.
//!
//! # What a value holds
//!
//! Which arguments of an input object a value of it holds is no place: a value holds what its
//! input fields' complete types hold, under lists, collections' arguments and the arguments of
//! other input objects that their values hold, and nothing a directive applied to it, or that
//! directive's arguments, name, which are metadata. A parameter can stand in an input field's
//! place through a directive's argument, which the kind its arguments take must answer to, while
//! no value holds it: `input Empty<T> @tag<T> { n: Int }` holds no `T`, whatever `@tag`'s
//! arguments are. [`Positions::holds`] answers it, for what an operation's argument reaches and
//! for the `reach` module.
//!
//! # Classes
//!
//! [`Fits`] answers whether a type is of a class, and, when it is not, where under it the type that
//! is not lies:
//!
//! - a list fits when its element does;
//! - a collection, a scalar `core`'s `@set` or `@map` makes one, when each of its arguments does,
//!   in order, since a value of it holds values of them;
//! - a type parameter and the unresolved type fit every class: what a parameter takes is judged
//!   where an instantiation gives it a type, and what did not resolve was refused where it is
//!   written;
//! - any other type by its definition's kind: an input object is no output type, an object, an
//!   interface and a union are no input types, and a scalar that is no collection, an enum and a
//!   directive are of every class, whatever their arguments.
//!
//! # What a fill derives
//!
//! [`Derived`] judges what the slots an occurrence leaves to their defaults derive, by a rule's
//! own verdict: every named type under those slots that no occurrence of the program writes, each
//! once, a type the occurrence or the program writes being judged where it is written.
//!
//! # What it costs
//!
//! [`Positions::of`] looks once at each member and union member, and at each application, whose
//! closed type it passes over at once and whose arguments its defaults fill it enters once for
//! each type and count of written arguments; enters each open type the roots reach once, reading
//! its arguments once; and spreads each place to each node once, reading each edge once for each
//! place its source gains, and each argument of a named type of an input object once more, for
//! what an operation's argument reaches: linear in the slots, the written type arguments, the open
//! types the roots reach and their arguments, with a logarithm for each lookup of an ordered map.
//! A type given to many slots is one node with an edge from each, never a walk of what it holds
//! for each. What a value holds is one more worklist over the input fields, as linear.
//! [`Fits`] judges each pair of a type and a class asked about once, with an explicit stack, a
//! type's children before it; reading where a misfit lies follows one path down the type, beside
//! the written occurrence that holds it. [`Derived`] walks, for each type and count of written
//! arguments, the types its fill typed, each once, which `Limits::max_substitutions` paid for when
//! the instantiation phase filled them. Nothing recurses, and every table is keyed by the model's
//! ids.

use std::{
  collections::{BTreeMap, BTreeSet},
  format,
  string::String,
  vec,
  vec::Vec,
};

use tokora::diagnostic::Location;

use crate::graphqlx::{
  ApplicationId, DefId, DefKind, MemberId, MemberKind, Model, SourceMap, TyId, TyKind, TypeExprId,
  TypeExprKind, diagnostic::Excerpt, id::dense, rules::one_of_in,
};

/// A class of type a position takes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Class {
  /// An output type, which a field's type is.
  Output,
  /// An input type, which an argument's, a directive's argument's and an input field's are.
  Input,
}

/// A kind of position a type parameter can stand in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Place {
  /// A field's type.
  Field,
  /// A field argument's type.
  Argument,
  /// An input field's type.
  InputField,
  /// A directive definition's argument's type.
  DirectiveArgument,
  /// What a value an operation's field argument takes can hold: the argument's type, through
  /// lists, collections' arguments, the definitions it is passed to, and the arguments a value of
  /// an input object holds.
  Operation,
  /// The whole type of a field of a OneOf input object, which takes a nullable type: the field's
  /// type is the parameter itself, written nullable, and no list or collection stands between.
  OneOfField,
}

/// How many places there are: the witnesses a slot keeps.
const PLACES: usize = 6;

impl Place {
  /// Its index among the places.
  const fn index(self) -> usize {
    match self {
      Self::Field => 0,
      Self::Argument => 1,
      Self::InputField => 2,
      Self::DirectiveArgument => 3,
      Self::Operation => 4,
      Self::OneOfField => 5,
    }
  }

  /// Whether it stands only where the parameter is a member's whole type, so that it does not
  /// pass from a list to its element or from a collection to its arguments.
  const fn whole(self) -> bool {
    matches!(self, Self::OneOfField)
  }

  /// Its bit in a set of places.
  const fn bit(self) -> u8 {
    1 << self.index()
  }
}

/// Where each type parameter of a model stands: for each, the places it stands in, and where, in
/// its own definition, it stands there or is passed to a parameter that does; and, for each type
/// parameter of an input object, whether a value of the input object holds a value of it.
#[derive(Default)]
pub(crate) struct Positions {
  /// The first slot of each definition, in id order, and the count of slots last: definition `d`'s
  /// type parameter `i` is slot `base[d] + i`.
  base: Vec<u32>,
  /// For each slot, and each place in turn, where the slot stands there, when it does.
  witnesses: Vec<Option<Location>>,
  /// For each slot, and each place in turn, the member whose type the place came from, when the
  /// root it came from is a member's type.
  origins: Vec<Option<MemberId>>,
  /// For each slot, whether a value of its definition, an input object, holds a value of its
  /// argument.
  held: Vec<bool>,
}

impl Positions {
  /// The positions of `model`'s every type parameter, found once, `sources` saying who wrote each
  /// definition and application, which tells a OneOf input object.
  pub(crate) fn of(model: &Model, sources: &SourceMap) -> Self {
    let base = bases(model);
    let slots = base.last().copied().unwrap_or(0);
    let held = containment(model, &base);
    let mut graph = Graph {
      base: &base,
      slots,
      types: BTreeMap::new(),
      edges: Vec::new(),
      holds: Vec::new(),
      seeds: Vec::new(),
      operations: Vec::new(),
      filled: BTreeSet::new(),
      stack: Vec::new(),
    };
    for def in model.defs() {
      graph.def(model, sources, def);
    }
    let (witnesses, origins) = graph.spread(&held);
    Self {
      base,
      witnesses,
      origins,
      held,
    }
  }

  /// The member whose type the place `place` of the type parameter `index` of `def` came from: the
  /// field of a OneOf whose whole type it is, for `OneOfField`; `None` when it does not stand
  /// there, or came from no member's type.
  pub(crate) fn origin(&self, def: DefId, index: u32, place: Place) -> Option<MemberId> {
    let slot = slot(&self.base, def, index)?;
    let at = (slot as usize)
      .checked_mul(PLACES)?
      .checked_add(place.index())?;
    self.origins.get(at).copied().flatten()
  }

  /// Where the type parameter `index` of `def` stands in `place`, in its own definition, or is
  /// passed to a parameter that does; `None` when it does not, or `def` has no such parameter.
  pub(crate) fn witness(&self, def: DefId, index: u32, place: Place) -> Option<Location> {
    let slot = slot(&self.base, def, index)?;
    let at = (slot as usize)
      .checked_mul(PLACES)?
      .checked_add(place.index())?;
    self.witnesses.get(at).copied().flatten()
  }

  /// Whether a value of `def`, an input object, holds a value of its type parameter `index`'s
  /// argument: what [`containment`] answers, which no place is. False for any other definition.
  pub(crate) fn holds(&self, def: DefId, index: u32) -> bool {
    slot(&self.base, def, index)
      .and_then(|slot: u32| self.held.get(slot as usize).copied())
      .unwrap_or(false)
  }
}

/// A step [`containment`] takes from a type it reached.
#[derive(Clone, Copy)]
enum Step {
  /// To a type a value of it always holds: a list's element, a collection's argument.
  Always(u32),
  /// To an argument of an input object, held when the input object's slot, the first, is.
  If(u32, u32),
  /// To the slot of the type parameter it is.
  Param(u32),
}

/// For each slot under `base`, whether a value of its definition, an input object of `model`,
/// holds a value of its argument: whether the complete type of one of the definition's input
/// fields, every contribution's, holds its type parameter, under lists, collections' arguments and
/// the arguments of input objects at the slots that hold theirs, found the same way.
///
/// A value holds what its input fields hold, and nothing else: a directive applied to the input
/// object, and the arguments of that directive, are metadata, and give no value of the input object
/// a value of anything. So it is no place, which says where a type parameter stands for the kind
/// its arguments take: `input Empty<T> @tag<T> { n: Int }` holds no `T`, whatever `@tag`'s
/// arguments do with its own.
///
/// A worklist: the open types the input fields' complete types reach, each `TyId` once, each with
/// the steps it leads by; a type is reached once, a slot held once, and a step to an argument of
/// an input object is read once when its type is reached and once when its slot is held. Linear in
/// the input fields and the open types they reach, with a logarithm for each lookup.
fn containment(model: &Model, base: &[u32]) -> Vec<bool> {
  let slots = base.last().copied().unwrap_or(0) as usize;
  let mut held = vec![false; slots];
  let mut types: BTreeMap<TyId, u32> = BTreeMap::new();
  let mut steps: Vec<(u32, Step)> = Vec::new();
  let mut stack: Vec<TyId> = Vec::new();
  let mut roots: Vec<u32> = Vec::new();
  let node_of = |types: &mut BTreeMap<TyId, u32>, stack: &mut Vec<TyId>, ty: TyId| -> Option<u32> {
    // A closed type names no parameter, and holds none.
    if model.ty(ty).is_closed() {
      return None;
    }
    if let Some(node) = types.get(&ty) {
      return Some(*node);
    }
    let node = dense(types.len());
    types.insert(ty, node);
    stack.push(ty);
    Some(node)
  };
  for def in model.defs() {
    let definition = model.def(def);
    if definition.kind() != DefKind::InputObject {
      continue;
    }
    for field in definition.input_fields() {
      let Some(written) = model.member(field).ty() else {
        continue;
      };
      if let Some(node) = node_of(&mut types, &mut stack, model.type_expr(written).ty()) {
        roots.push(node);
      }
    }
  }
  // Each open type's steps, once: the stack holds each type when it is first numbered.
  while let Some(ty) = stack.pop() {
    let Some(&from) = types.get(&ty) else {
      continue;
    };
    match model.ty(ty).kind() {
      TyKind::Param { def, index } => {
        if let Some(slot) = slot(base, def, index) {
          steps.push((from, Step::Param(slot)));
        }
      }
      TyKind::List { element } => {
        if let Some(to) = node_of(&mut types, &mut stack, element) {
          steps.push((from, Step::Always(to)));
        }
      }
      TyKind::Named { def, args } => {
        let definition = model.def(def);
        let input = definition.kind() == DefKind::InputObject;
        if definition.collection().is_some() || input {
          for (index, arg) in args.iter().enumerate() {
            let Some(to) = node_of(&mut types, &mut stack, *arg) else {
              continue;
            };
            if !input {
              steps.push((from, Step::Always(to)));
            } else if let Some(slot) = slot(base, def, dense(index)) {
              steps.push((from, Step::If(slot, to)));
            }
          }
        }
      }
      TyKind::Unresolved => {}
    }
  }
  steps.sort_by_key(|step: &(u32, Step)| step.0);
  // The steps to an argument of an input object, by the slot that holds it.
  let mut waits: Vec<(u32, u32, u32)> = steps
    .iter()
    .filter_map(|&(from, step): &(u32, Step)| match step {
      Step::If(slot, to) => Some((slot, from, to)),
      Step::Always(_) | Step::Param(_) => None,
    })
    .collect();
  waits.sort_unstable();
  let mut reached = vec![false; types.len()];
  // What is reached or held and not followed yet.
  let mut queue: Vec<Event> = roots.into_iter().map(Event::Reached).collect();
  while let Some(event) = queue.pop() {
    match event {
      Event::Reached(node) => {
        let Some(seen) = reached.get_mut(node as usize) else {
          continue;
        };
        if *seen {
          continue;
        }
        *seen = true;
        for &(_, step) in run(&steps, node, |step: &(u32, Step)| step.0) {
          match step {
            Step::Always(to) => queue.push(Event::Reached(to)),
            Step::If(slot, to) => {
              if held.get(slot as usize).copied().unwrap_or(false) {
                queue.push(Event::Reached(to));
              }
            }
            Step::Param(slot) => queue.push(Event::Held(slot)),
          }
        }
      }
      Event::Held(slot) => {
        let Some(holds) = held.get_mut(slot as usize) else {
          continue;
        };
        if *holds {
          continue;
        }
        *holds = true;
        for &(_, from, to) in run(&waits, slot, |wait: &(u32, u32, u32)| wait.0) {
          if reached.get(from as usize).copied().unwrap_or(false) {
            queue.push(Event::Reached(to));
          }
        }
      }
    }
  }
  held
}

/// What [`containment`]'s worklist follows next.
#[derive(Clone, Copy)]
enum Event {
  /// A type's node, reached from an input field.
  Reached(u32),
  /// A slot, which a value of its input object holds.
  Held(u32),
}

/// The first slot of each definition of `model`, in id order, and the count of slots last: one
/// slot for each type parameter of each definition, every package's.
pub(crate) fn bases(model: &Model) -> Vec<u32> {
  let mut base: Vec<u32> = Vec::with_capacity((model.defs().len() as usize).saturating_add(1));
  let mut slots: u32 = 0;
  for def in model.defs() {
    base.push(slots);
    slots = slots.saturating_add(dense(model.def(def).type_params().len()));
  }
  base.push(slots);
  base
}

/// The slot of the type parameter `index` of `def`, under `base`, as [`bases`] numbers them,
/// when `def` has it.
pub(crate) fn slot(base: &[u32], def: DefId, index: u32) -> Option<u32> {
  let at = def.get() as usize;
  let first = *base.get(at)?;
  let end = *base.get(at.checked_add(1)?)?;
  let slot = first.checked_add(index)?;
  (slot < end).then_some(slot)
}

/// An edge of the graph: from a node, to a node, where it is written, and whether it leads from a
/// list to its element or from a collection to an argument.
type Edge = (u32, u32, Location, bool);

/// A node given a place, with the witness it gains it with and the member whose type the place
/// came from, when one did.
type Seed = (u32, Place, Location, Option<MemberId>);

/// The graph [`Positions::of`] spreads places over, dropped once they are found.
struct Graph<'b> {
  /// The first slot of each definition.
  base: &'b [u32],
  /// How many slots there are: they are the first nodes.
  slots: u32,
  /// The node of each open type entered, numbered after the slots.
  types: BTreeMap<TyId, u32>,
  /// Each edge, from a node to a node, with where it is written, the witness it gives when it
  /// leaves a slot, and whether it leads from a list to its element or from a collection to an
  /// argument.
  edges: Vec<Edge>,
  /// Each open argument of a named type of an input object: from the type's node to the
  /// argument's, with the slot it is given to, which a value of the type holds a value of when
  /// [`containment`] says so.
  holds: Vec<(u32, u32, u32)>,
  /// The places the roots give, in the order of the roots: a node, its place, the root's witness,
  /// and the member whose type the root is.
  seeds: Vec<Seed>,
  /// The field arguments' roots, in the order of the roots, each a node and the root's witness:
  /// what an operation's argument reaches, given once the other places are found.
  operations: Vec<(u32, Location)>,
  /// Each open type of an application, with its count of written type arguments, whose arguments
  /// its defaults fill were entered: once for each, whatever the applications.
  filled: BTreeSet<(TyId, u32)>,
  /// The types still to enter, each with the witness of the root it is entered for.
  stack: Vec<(TyId, Location)>,
}

impl Graph<'_> {
  /// Enters the roots of `def`, of `model`: its applications, its union members, and each member's
  /// applications and type, a field's arguments' after the field's. `sources` tells whether it is
  /// a OneOf input object.
  fn def(&mut self, model: &Model, sources: &SourceMap, def: DefId) {
    let definition = model.def(def);
    for application in definition.applications() {
      self.application(model, *application);
    }
    for entry in definition.union_members() {
      let occurrence = model.type_expr(*entry);
      self.enter(model, occurrence.ty(), occurrence.location());
    }
    let one_of = one_of_in(model, sources, def);
    let members = definition
      .fields()
      .into_iter()
      .chain(definition.input_fields())
      .chain(definition.arguments())
      .chain(definition.enum_values());
    for member in members {
      self.member(model, member, one_of);
      for argument in model.member(member).arguments() {
        self.member(model, argument, one_of);
      }
    }
  }

  /// Enters `member`'s applications, and its type, which it gives the place its kind is; and, the
  /// field of a OneOf, when `one_of`, typed by its definition's type parameter written nullable,
  /// the place of a OneOf's field too.
  fn member(&mut self, model: &Model, member: MemberId, one_of: bool) {
    let written = model.member(member);
    for application in written.applications() {
      self.application(model, *application);
    }
    let Some(ty) = written.ty() else {
      return;
    };
    let place = match (written.kind(), written.parent()) {
      (MemberKind::Field, _) => Place::Field,
      (MemberKind::Argument, Some(_)) => Place::Argument,
      (MemberKind::Argument, None) => Place::DirectiveArgument,
      (MemberKind::InputField, _) => Place::InputField,
      (MemberKind::EnumValue, _) => return,
    };
    let occurrence = model.type_expr(ty);
    let at = occurrence.location();
    let Some(node) = self.enter(model, occurrence.ty(), at) else {
      return;
    };
    self.seeds.push((node, place, at, Some(member)));
    // A field's argument is what an operation sets.
    if place == Place::Argument {
      self.operations.push((node, at));
    }
    let complete = model.ty(occurrence.ty());
    if let TyKind::Param { def, .. } = complete.kind()
      && def == written.owner()
      && !complete.non_null()
      && one_of
      && place == Place::InputField
    {
      self.seeds.push((node, Place::OneOfField, at, Some(member)));
    }
  }

  /// Enters the arguments of `application`'s type, each from its directive's slot, with where it
  /// is written: each written type argument where it is written, which the source pays for; and
  /// the arguments its defaults fill at its path, once for each type and count of written
  /// arguments, which the fill that typed them paid for. A closed type gives no edge, and is
  /// passed over at once.
  fn application(&mut self, model: &Model, id: ApplicationId) {
    let application = model.application(id);
    let Some(ty) = application.ty() else {
      return;
    };
    let complete = model.ty(ty);
    if complete.is_closed() {
      return;
    }
    let TyKind::Named { def, args } = complete.kind() else {
      return;
    };
    let written = application.type_args();
    for (index, (arg, argument)) in args.iter().zip(written).enumerate() {
      self.argument(
        model,
        def,
        index,
        *arg,
        model.type_expr(*argument).location(),
      );
    }
    if written.len() < args.len() && self.filled.insert((ty, dense(written.len()))) {
      let at = application.path_location();
      for (index, arg) in args.iter().enumerate().skip(written.len()) {
        self.argument(model, def, index, *arg, at);
      }
    }
  }

  /// Enters `arg`, the argument at `index` of a type of `def`, for a root written at `at`, with an
  /// edge to it from `def`'s slot.
  fn argument(&mut self, model: &Model, def: DefId, index: usize, arg: TyId, at: Location) {
    let Some(to) = self.enter(model, arg, at) else {
      return;
    };
    if let Some(from) = slot(self.base, def, dense(index)) {
      self.edges.push((from, to, at, false));
    }
  }

  /// The node of `root`, entered for a root written at `witness`, with every open type under it
  /// that was not entered before; `None` for a closed type.
  fn enter(&mut self, model: &Model, root: TyId, witness: Location) -> Option<u32> {
    let node = self.open(model, root, witness)?;
    while let Some((ty, at)) = self.stack.pop() {
      let Some(&from) = self.types.get(&ty) else {
        continue;
      };
      match model.ty(ty).kind() {
        TyKind::Param { def, index } => {
          if let Some(to) = slot(self.base, def, index) {
            self.edges.push((from, to, at, false));
          }
        }
        TyKind::List { element } => {
          if let Some(to) = self.open(model, element, at) {
            self.edges.push((from, to, at, true));
          }
        }
        TyKind::Named { def, args } => {
          let definition = model.def(def);
          if definition.collection().is_some() {
            for arg in args {
              if let Some(to) = self.open(model, *arg, at) {
                self.edges.push((from, to, at, true));
              }
            }
          } else if !matches!(definition.kind(), DefKind::Scalar | DefKind::Enum) {
            let input = definition.kind() == DefKind::InputObject;
            // What an argument takes is the definition's slot's to say.
            for (index, arg) in args.iter().enumerate() {
              if let Some(to) = self.open(model, *arg, at)
                && let Some(given) = slot(self.base, def, dense(index))
              {
                self.edges.push((given, to, at, false));
                if input {
                  self.holds.push((from, to, given));
                }
              }
            }
          }
        }
        TyKind::Unresolved => {}
      }
    }
    Some(node)
  }

  /// The node of `ty`, made and pushed to be entered when it is new, for a root written at
  /// `witness`; `None` for a closed type, which names no parameter.
  fn open(&mut self, model: &Model, ty: TyId, witness: Location) -> Option<u32> {
    if model.ty(ty).is_closed() {
      return None;
    }
    if let Some(node) = self.types.get(&ty) {
      return Some(*node);
    }
    let node = self.slots.saturating_add(dense(self.types.len()));
    self.types.insert(ty, node);
    self.stack.push((ty, witness));
    Some(node)
  }

  /// Spreads each root's place along the edges, root by root, then what an operation's argument
  /// reaches, which passes from an input object's type to the arguments a value of it holds by
  /// `held`, and answers each slot's witnesses and the members its places came from.
  fn spread(mut self, held: &[bool]) -> (Vec<Option<Location>>, Vec<Option<MemberId>>) {
    let nodes = (self.slots as usize).saturating_add(self.types.len());
    let size = (self.slots as usize).saturating_mul(PLACES);
    // The edges by their source, so that a node's are one run.
    self.edges.sort_by_key(|edge: &Edge| edge.0);
    self.holds.sort_by_key(|hold: &(u32, u32, u32)| hold.0);
    let mut spread = Spread {
      slots: self.slots,
      edges: &self.edges,
      holds: &self.holds,
      held,
      gained: vec![0; nodes],
      witnesses: vec![None; size],
      origins: vec![None; size],
      queue: Vec::new(),
    };
    for &seed in &self.seeds {
      spread.gain(seed);
      spread.drain();
    }
    // Last, and root by root as well, so that a slot's witness is that of the first field
    // argument, in the order of the roots, that reaches it.
    for &(node, at) in &self.operations {
      spread.gain((node, Place::Operation, at, None));
      spread.drain();
    }
    (spread.witnesses, spread.origins)
  }
}

/// The places spreading over a graph: what each node gained, each slot's witnesses and origins,
/// and the nodes still to pass a place on.
struct Spread<'e> {
  /// How many slots there are: they are the first nodes.
  slots: u32,
  /// The edges, by their source.
  edges: &'e [Edge],
  /// The arguments of named types of input objects, by the type's node, each with its slot.
  holds: &'e [(u32, u32, u32)],
  /// For each slot, whether a value of its input object holds a value of its argument.
  held: &'e [bool],
  /// For each node, the places it gained, a bit for each.
  gained: Vec<u8>,
  /// For each slot, and each place in turn, the witness it gained the place with.
  witnesses: Vec<Option<Location>>,
  /// For each slot, and each place in turn, the member the place came from, when one did.
  origins: Vec<Option<MemberId>>,
  /// The nodes that gained a place and have not passed it on, with the witness they gained it
  /// with and the member it came from.
  queue: Vec<Seed>,
}

impl Spread<'_> {
  /// A node gains a place, with a witness and an origin, unless it has it.
  fn gain(&mut self, (node, place, at, origin): Seed) {
    let Some(gained) = self.gained.get_mut(node as usize) else {
      return;
    };
    if *gained & place.bit() != 0 {
      return;
    }
    *gained |= place.bit();
    if node < self.slots
      && let Some(index) = (node as usize)
        .checked_mul(PLACES)
        .and_then(|first: usize| first.checked_add(place.index()))
    {
      if let Some(witness) = self.witnesses.get_mut(index) {
        *witness = Some(at);
      }
      if let Some(from) = self.origins.get_mut(index) {
        *from = origin;
      }
    }
    self.queue.push((node, place, at, origin));
  }

  /// Passes every queued place along each edge out of its node, until none is left: but a place
  /// that stands only as a member's whole type not from a list to its element or from a
  /// collection to an argument; and what an operation's argument reaches, from a named type of an
  /// input object to each of its arguments a value of it holds.
  fn drain(&mut self) {
    let edges = self.edges;
    let holds = self.holds;
    let held = self.held;
    while let Some((node, place, at, origin)) = self.queue.pop() {
      for &(_, next, written, flagged) in run(edges, node, |edge: &Edge| edge.0) {
        if flagged && place.whole() {
          continue;
        }
        // Across an edge out of a slot, the witness is where the argument is given.
        let witness = if node < self.slots { written } else { at };
        self.gain((next, place, witness, origin));
      }
      if place != Place::Operation {
        continue;
      }
      for &(_, next, given) in run(holds, node, |hold: &(u32, u32, u32)| hold.0) {
        if held.get(given as usize).copied().unwrap_or(false) {
          self.gain((next, place, at, origin));
        }
      }
    }
  }
}

/// The run of `sorted`, sorted by `source`, whose source is `node`.
fn run<T>(sorted: &[T], node: u32, source: fn(&T) -> u32) -> &[T] {
  let from = sorted.partition_point(|item: &T| source(item) < node);
  let to = sorted.partition_point(|item: &T| source(item) <= node);
  sorted.get(from..to).unwrap_or(&[])
}

/// Where a type does not fit a class: nowhere, in itself, or under one of its children.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Under {
  /// It fits.
  Nowhere,
  /// It is of a kind the class does not take.
  Itself,
  /// A list's element does not fit.
  Element,
  /// A collection's argument at this index does not fit, the first of them that does not.
  Argument(u32),
}

/// A type that does not fit a class, as [`Fits::misfit`] reads it.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Misfit {
  /// The definition of the type that does not fit, under the lists and the collections' arguments
  /// that hold it: an object, an interface, a union or an input object.
  pub(crate) leaf: DefId,
  /// Whether it lies under an argument of a collection.
  pub(crate) nested: bool,
  /// The deepest of the written occurrences handed in and nested in it that writes the way to it,
  /// when one is handed in.
  pub(crate) at: Option<TypeExprId>,
  /// The collection, and the index of its type parameter, whose slot the way leaves the written
  /// occurrences by: a slot the occurrence does not write, which its default fills.
  pub(crate) filled: Option<(DefId, u32)>,
}

/// Whether types fit classes, each type and class judged once.
///
/// A rule keeps one for its walk and resets it in `begin`: what it holds is keyed by the model's
/// ids.
#[derive(Default)]
pub(crate) struct Fits {
  /// Each type and class judged, with where the type does not fit.
  judged: BTreeMap<(TyId, Class), Under>,
  /// The types still to judge, each with whether its children were pushed above it.
  stack: Vec<(TyId, bool)>,
}

impl Fits {
  /// Whether `ty`, of `model`, fits `class`.
  pub(crate) fn fits(&mut self, model: &Model, ty: TyId, class: Class) -> bool {
    self.under(model, ty, class) == Under::Nowhere
  }

  /// Where `ty`, of `model`, does not fit `class`, `written` being the occurrence that writes it,
  /// if one does: the definition of the type under it that does not fit, and the deepest
  /// occurrence of `written`'s tree that writes the way to it. `None` when it fits.
  pub(crate) fn misfit(
    &mut self,
    model: &Model,
    ty: TyId,
    class: Class,
    written: Option<TypeExprId>,
  ) -> Option<Misfit> {
    let mut under = self.under(model, ty, class);
    let mut ty = ty;
    let mut at = written;
    // Whether `at` still writes the type the way has reached.
    let mut writes = written.is_some();
    let mut nested = false;
    let mut filled: Option<(DefId, u32)> = None;
    // Each step goes one level down the type, so the loop ends.
    loop {
      match (under, model.ty(ty).kind()) {
        (Under::Itself, TyKind::Named { def, .. }) => {
          return Some(Misfit {
            leaf: def,
            nested,
            at,
            filled,
          });
        }
        (Under::Element, TyKind::List { element }) => {
          ty = element;
          if writes && let Some(occurrence) = at {
            match model.type_expr(occurrence).kind() {
              TypeExprKind::List { element } => at = Some(element),
              _ => writes = false,
            }
          }
        }
        (Under::Argument(index), TyKind::Named { def, args }) => {
          ty = *args.get(index as usize)?;
          nested = true;
          if writes && let Some(occurrence) = at {
            let next = match model.type_expr(occurrence).kind() {
              TypeExprKind::Named { args: written, .. } => written.get(index as usize).copied(),
              _ => None,
            };
            match next {
              Some(next) => at = Some(next),
              None => {
                writes = false;
                filled = Some((def, index));
              }
            }
          }
        }
        _ => return None,
      }
      // Every type under a judged one is judged, so this reads a kept answer.
      under = self
        .judged
        .get(&(ty, class))
        .copied()
        .unwrap_or(Under::Nowhere);
    }
  }

  /// Where `ty`, of `model`, does not fit `class`, judging it and every type under it not judged
  /// yet, children before their parent.
  fn under(&mut self, model: &Model, ty: TyId, class: Class) -> Under {
    if let Some(under) = self.judged.get(&(ty, class)) {
      return *under;
    }
    self.stack.clear();
    self.stack.push((ty, false));
    while let Some((next, pushed)) = self.stack.pop() {
      if self.judged.contains_key(&(next, class)) {
        continue;
      }
      let under = match model.ty(next).kind() {
        TyKind::List { element } => {
          if !pushed {
            self.stack.push((next, true));
            self.stack.push((element, false));
            continue;
          }
          if self.kept(element, class) {
            Under::Nowhere
          } else {
            Under::Element
          }
        }
        TyKind::Named { def, args } if model.def(def).collection().is_some() => {
          if !pushed {
            self.stack.push((next, true));
            self
              .stack
              .extend(args.iter().rev().map(|arg: &TyId| (*arg, false)));
            continue;
          }
          match args.iter().position(|arg: &TyId| !self.kept(*arg, class)) {
            Some(index) => Under::Argument(dense(index)),
            None => Under::Nowhere,
          }
        }
        TyKind::Named { def, .. } => leaf(model.def(def).kind(), class),
        TyKind::Param { .. } | TyKind::Unresolved => Under::Nowhere,
      };
      self.judged.insert((next, class), under);
    }
    self
      .judged
      .get(&(ty, class))
      .copied()
      .unwrap_or(Under::Nowhere)
  }

  /// Whether `ty`, judged for `class` already, fits it.
  fn kept(&self, ty: TyId, class: Class) -> bool {
    self
      .judged
      .get(&(ty, class))
      .is_none_or(|under: &Under| *under == Under::Nowhere)
  }
}

/// Whether a type of a definition of `kind` that is no collection fits `class`.
fn leaf(kind: DefKind, class: Class) -> Under {
  let refused = match class {
    Class::Output => kind == DefKind::InputObject,
    Class::Input => matches!(kind, DefKind::Object | DefKind::Interface | DefKind::Union),
  };
  if refused {
    Under::Itself
  } else {
    Under::Nowhere
  }
}

/// A type that a fill derives and a rule refuses: what [`Derived::verdict`] answers.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Fill<V> {
  /// The slot of the occurrence's type whose default derives it.
  pub(crate) slot: u32,
  /// The type it derives.
  pub(crate) ty: TyId,
  /// The rule's verdict on that type.
  pub(crate) verdict: V,
}

/// A rule's verdicts on what the slots an occurrence leaves to their defaults derive, each type and
/// count of written arguments judged once.
///
/// A rule keeps one for its walk and resets it in `begin`: what it holds is keyed by the model's
/// ids, and what it answers for a type and a count is the same whatever the walk met before.
pub(crate) struct Derived<V> {
  /// Each type and count of written arguments judged, with the first failure found.
  judged: BTreeMap<(TyId, u32), Option<Fill<V>>>,
  /// The types under a slot still to look at.
  stack: Vec<TyId>,
  /// The types looked at for the type and count being judged.
  seen: BTreeSet<TyId>,
}

impl<V> Default for Derived<V> {
  fn default() -> Self {
    Self {
      judged: BTreeMap::new(),
      stack: Vec::new(),
      seen: BTreeSet::new(),
    }
  }
}

impl<V: Copy> Derived<V> {
  /// The first failure, by `judge`, the rule's own verdict on one type, among what the slots
  /// `written` and after of `ty`, of `model`, derive: an occurrence of `ty` that writes `written`
  /// arguments leaves those slots to their defaults. For each slot in order, every type under it,
  /// each once; a type the occurrence writes, or that an occurrence of the program writes, is
  /// passed over with what it holds, and each other named type of a definition that has type
  /// parameters is judged, then what it holds. `None` when none fails.
  pub(crate) fn verdict(
    &mut self,
    model: &Model,
    ty: TyId,
    written: u32,
    mut judge: impl FnMut(TyId) -> Option<V>,
  ) -> Option<Fill<V>> {
    if let Some(kept) = self.judged.get(&(ty, written)) {
      return *kept;
    }
    let found = self.walk(model, ty, written, &mut judge);
    self.judged.insert((ty, written), found);
    found
  }

  /// The walk [`verdict`](Self::verdict) keeps the answer of.
  fn walk(
    &mut self,
    model: &Model,
    ty: TyId,
    written: u32,
    judge: &mut impl FnMut(TyId) -> Option<V>,
  ) -> Option<Fill<V>> {
    let TyKind::Named { args, .. } = model.ty(ty).kind() else {
      return None;
    };
    self.seen.clear();
    // What the occurrence writes is judged where it is written.
    self
      .seen
      .extend(args.iter().take(written as usize).copied());
    for (slot, arg) in args.iter().enumerate().skip(written as usize) {
      self.stack.clear();
      self.stack.push(*arg);
      while let Some(next) = self.stack.pop() {
        if !self.seen.insert(next) {
          continue;
        }
        match model.ty(next).kind() {
          TyKind::Named { def, args } => {
            // What the program writes is judged where it is written.
            if model.instantiations(def).binary_search(&next).is_ok() {
              continue;
            }
            if model.def(def).type_params().next().is_some()
              && let Some(verdict) = judge(next)
            {
              return Some(Fill {
                slot: dense(slot),
                ty: next,
                verdict,
              });
            }
            self.stack.extend(args.iter().rev().copied());
          }
          TyKind::List { element } => self.stack.push(element),
          TyKind::Param { .. } | TyKind::Unresolved => {}
        }
      }
    }
    None
  }
}

/// Who takes what an occurrence or an application is refused for, and where to say so: what a rule
/// that judges an instantiation reports, whatever it judges.
pub(crate) struct Taking {
  /// The definition whose type parameter takes it.
  pub(crate) def: DefId,
  /// The parameter's index.
  pub(crate) slot: u32,
  /// The type it takes.
  pub(crate) argument: TyId,
  /// The message's lead, which names who takes it: `` `D` takes ``, `` `D` takes, from the default
  /// of `P`, `` or `` the default of `Q` of `E` makes `D` take ``.
  pub(crate) lead: String,
  /// Where it is reported: the argument as written, or the occurrence, an application at its path,
  /// for an argument a default fills.
  pub(crate) at: Location,
  /// Where the default that fills it is written, when a default fills it.
  pub(crate) filled: Option<Location>,
}

/// Who takes the argument at `slot` of the type `ty`, of `model`, of an occurrence or an
/// application that writes the type arguments `written` and is written at `whole`; or, when
/// `derived` is the slot of `ty` a default fills and the type that fill derives, the argument at
/// `slot` of that type. A directive is named with its `@`.
pub(crate) fn taking(
  model: &Model,
  ty: TyId,
  written: &[TypeExprId],
  whole: Location,
  slot: u32,
  derived: Option<(u32, TyId)>,
) -> Option<Taking> {
  let TyKind::Named { def, args } = model.ty(ty).kind() else {
    return None;
  };
  let definition = Excerpt(model.def(def).name().as_bytes());
  let sign = if model.def(def).kind() == DefKind::Directive {
    "@"
  } else {
    ""
  };
  let Some((filled, derived)) = derived else {
    let argument = *args.get(slot as usize)?;
    return Some(match written.get(slot as usize) {
      Some(given) => Taking {
        def,
        slot,
        argument,
        lead: format!("`{sign}{definition}` takes"),
        at: model.type_expr(*given).location(),
        filled: None,
      },
      None => Taking {
        def,
        slot,
        argument,
        lead: format!(
          "`{sign}{definition}` takes, from the default of `{}`,",
          Excerpt(parameter(model, def, slot).as_bytes())
        ),
        at: whole,
        filled: default_of(model, def, slot),
      },
    });
  };
  let TyKind::Named {
    def: derived_def,
    args: derived_args,
  } = model.ty(derived).kind()
  else {
    return None;
  };
  Some(Taking {
    def: derived_def,
    slot,
    argument: *derived_args.get(slot as usize)?,
    lead: format!(
      "the default of `{}` of `{sign}{definition}` makes `{}` take",
      Excerpt(parameter(model, def, filled).as_bytes()),
      Excerpt(model.def(derived_def).name().as_bytes())
    ),
    at: whole,
    filled: default_of(model, def, filled),
  })
}

/// How a message names a definition of `kind`, before its name: "the object type `Obj`".
pub(crate) fn named(kind: DefKind) -> &'static str {
  match kind {
    DefKind::InputObject => "the input object",
    DefKind::Object => "the object type",
    DefKind::Interface => "the interface",
    DefKind::Union => "the union",
    DefKind::Scalar => "the scalar",
    DefKind::Enum => "the enum",
    DefKind::Directive => "the directive",
  }
}

/// Where the default of the type parameter `index` of `def`, of `model`, is written, when it has
/// one: what fills a slot an occurrence leaves out. A constant step: `TypeParams::nth` reads the
/// row it is asked for.
pub(crate) fn default_of(model: &Model, def: DefId, index: u32) -> Option<Location> {
  let default = model
    .def(def)
    .type_params()
    .nth(index as usize)?
    .default()?;
  Some(model.type_expr(default).location())
}

/// The name of the type parameter `index` of `def`, of `model`, or nothing when it has none. A
/// constant step, as [`default_of`]'s.
pub(crate) fn parameter(model: &Model, def: DefId, index: u32) -> &str {
  match model.def(def).type_params().nth(index as usize) {
    Some(param) => param.name(),
    None => "",
  }
}
