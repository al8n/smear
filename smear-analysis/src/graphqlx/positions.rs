//! Where a type parameter stands, and whether a type fits where it stands: what the rules that
//! judge an instantiation's arguments share.
//!
//! # Places
//!
//! A place is a kind of position a type parameter can stand in, and takes one class of type: a
//! field's type an output type, and a field argument's, an input field's and a directive
//! argument's an input type. [`Positions`] finds, once for a walk, the places each type parameter
//! of every package's definitions stands in, and for each, where in its own definition it stands
//! there or is passed to a parameter that does: the witness a diagnostic labels.
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
//! [`Positions::of`] looks once at each member, union member and application, enters each open
//! type the roots reach once, reading its arguments once, and spreads each place to each node once,
//! reading each edge once for each place its source gains: linear in the slots, the open types the
//! roots reach and their arguments, with a logarithm for each lookup of an ordered map. A type
//! given to many slots is one node with an edge from each, never a walk of what it holds for each.
//! [`Fits`] judges each pair of a type and a class asked about once, with an explicit stack, a
//! type's children before it; reading where a misfit lies follows one path down the type, beside
//! the written occurrence that holds it. [`Derived`] walks, for each type and count of written
//! arguments, the types its fill typed, each once, which `Limits::max_substitutions` paid for when
//! the instantiation phase filled them. Nothing recurses, and every table is keyed by the model's
//! ids.

use std::{
  collections::{BTreeMap, BTreeSet},
  vec,
  vec::Vec,
};

use tokora::diagnostic::Location;

use crate::graphqlx::{
  ApplicationId, DefId, DefKind, MemberId, MemberKind, Model, TyId, TyKind, TypeExprId,
  TypeExprKind, id::dense,
};

/// A class of type a position takes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Class {
  /// An output type, which a field's type is.
  Output,
  /// An input type, which an argument's, a directive's argument's and an input field's are.
  Input,
}

/// A kind of position a type parameter can stand in, each taking one class of type.
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
}

/// How many places there are: the witnesses a slot keeps.
const PLACES: usize = 4;

impl Place {
  /// The class of type it takes.
  pub(crate) const fn class(self) -> Class {
    match self {
      Self::Field => Class::Output,
      Self::Argument | Self::InputField | Self::DirectiveArgument => Class::Input,
    }
  }

  /// Its index among the places.
  const fn index(self) -> usize {
    match self {
      Self::Field => 0,
      Self::Argument => 1,
      Self::InputField => 2,
      Self::DirectiveArgument => 3,
    }
  }

  /// Its bit in a set of places.
  const fn bit(self) -> u8 {
    1 << self.index()
  }
}

/// Where each type parameter of a model stands: for each, the places it stands in, and where, in
/// its own definition, it stands there or is passed to a parameter that does.
#[derive(Default)]
pub(crate) struct Positions {
  /// The first slot of each definition, in id order, and the count of slots last: definition `d`'s
  /// type parameter `i` is slot `base[d] + i`.
  base: Vec<u32>,
  /// For each slot, and each place in turn, where the slot stands there, when it does.
  witnesses: Vec<Option<Location>>,
}

impl Positions {
  /// The positions of `model`'s every type parameter, found once.
  pub(crate) fn of(model: &Model) -> Self {
    let base = bases(model);
    let slots = base.last().copied().unwrap_or(0);
    let mut graph = Graph {
      base: &base,
      slots,
      types: BTreeMap::new(),
      edges: Vec::new(),
      seeds: Vec::new(),
      stack: Vec::new(),
    };
    for def in model.defs() {
      graph.def(model, def);
    }
    let witnesses = graph.spread();
    Self { base, witnesses }
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

/// The graph [`Positions::of`] spreads places over, dropped once they are found.
struct Graph<'b> {
  /// The first slot of each definition.
  base: &'b [u32],
  /// How many slots there are: they are the first nodes.
  slots: u32,
  /// The node of each open type entered, numbered after the slots.
  types: BTreeMap<TyId, u32>,
  /// Each edge, from a node to a node, with where it is written: the witness it gives when it
  /// leaves a slot.
  edges: Vec<(u32, u32, Location)>,
  /// The places the roots give, in the order of the roots: a node, its place, and the root's
  /// witness.
  seeds: Vec<(u32, Place, Location)>,
  /// The types still to enter, each with the witness of the root it is entered for.
  stack: Vec<(TyId, Location)>,
}

impl Graph<'_> {
  /// Enters the roots of `def`, of `model`: its applications, its union members, and each member's
  /// applications and type, a field's arguments' after the field's.
  fn def(&mut self, model: &Model, def: DefId) {
    let definition = model.def(def);
    for application in definition.applications() {
      self.application(model, *application);
    }
    for entry in definition.union_members() {
      let occurrence = model.type_expr(*entry);
      self.enter(model, occurrence.ty(), occurrence.location());
    }
    let members = definition
      .fields()
      .into_iter()
      .chain(definition.input_fields())
      .chain(definition.arguments())
      .chain(definition.enum_values());
    for member in members {
      self.member(model, member);
      for argument in model.member(member).arguments() {
        self.member(model, argument);
      }
    }
  }

  /// Enters `member`'s applications, and its type, which it gives the place its kind is.
  fn member(&mut self, model: &Model, member: MemberId) {
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
    if let Some(node) = self.enter(model, occurrence.ty(), at) {
      self.seeds.push((node, place, at));
    }
  }

  /// Enters the arguments of `application`'s type, each from its directive's slot, with where it
  /// is written: its written type argument, or its path for one a default fills.
  fn application(&mut self, model: &Model, id: ApplicationId) {
    let application = model.application(id);
    let Some(ty) = application.ty() else {
      return;
    };
    let TyKind::Named { def, args } = model.ty(ty).kind() else {
      return;
    };
    let written = application.type_args();
    for (index, arg) in args.iter().enumerate() {
      let at = match written.get(index) {
        Some(argument) => model.type_expr(*argument).location(),
        None => application.path_location(),
      };
      let Some(to) = self.enter(model, *arg, at) else {
        continue;
      };
      if let Some(from) = slot(self.base, def, dense(index)) {
        self.edges.push((from, to, at));
      }
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
            self.edges.push((from, to, at));
          }
        }
        TyKind::List { element } => {
          if let Some(to) = self.open(model, element, at) {
            self.edges.push((from, to, at));
          }
        }
        TyKind::Named { def, args } => {
          let definition = model.def(def);
          if definition.collection().is_some() {
            for arg in args {
              if let Some(to) = self.open(model, *arg, at) {
                self.edges.push((from, to, at));
              }
            }
          } else if !matches!(definition.kind(), DefKind::Scalar | DefKind::Enum) {
            // What an argument takes is the definition's slot's to say.
            for (index, arg) in args.iter().enumerate() {
              if let Some(to) = self.open(model, *arg, at)
                && let Some(from) = slot(self.base, def, dense(index))
              {
                self.edges.push((from, to, at));
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

  /// Spreads each root's place along the edges, root by root, and answers each slot's witnesses.
  fn spread(mut self) -> Vec<Option<Location>> {
    let nodes = (self.slots as usize).saturating_add(self.types.len());
    // The edges by their source, so that a node's are one run.
    self.edges.sort_by_key(|edge: &(u32, u32, Location)| edge.0);
    let mut spread = Spread {
      slots: self.slots,
      edges: &self.edges,
      gained: vec![0; nodes],
      witnesses: vec![None; (self.slots as usize).saturating_mul(PLACES)],
      queue: Vec::new(),
    };
    for &(node, place, at) in &self.seeds {
      spread.gain(node, place, at);
      spread.drain();
    }
    spread.witnesses
  }
}

/// The places spreading over a graph: what each node gained, each slot's witnesses, and the nodes
/// still to pass a place on.
struct Spread<'e> {
  /// How many slots there are: they are the first nodes.
  slots: u32,
  /// The edges, by their source.
  edges: &'e [(u32, u32, Location)],
  /// For each node, the places it gained, a bit for each.
  gained: Vec<u8>,
  /// For each slot, and each place in turn, the witness it gained the place with.
  witnesses: Vec<Option<Location>>,
  /// The nodes that gained a place and have not passed it on, with the witness they gained it
  /// with.
  queue: Vec<(u32, Place, Location)>,
}

impl Spread<'_> {
  /// `node` gains `place`, with the witness `at`, unless it has it.
  fn gain(&mut self, node: u32, place: Place, at: Location) {
    let Some(gained) = self.gained.get_mut(node as usize) else {
      return;
    };
    if *gained & place.bit() != 0 {
      return;
    }
    *gained |= place.bit();
    if node < self.slots
      && let Some(witness) = (node as usize)
        .checked_mul(PLACES)
        .and_then(|first: usize| first.checked_add(place.index()))
        .and_then(|index: usize| self.witnesses.get_mut(index))
    {
      *witness = Some(at);
    }
    self.queue.push((node, place, at));
  }

  /// Passes every queued place along each edge out of its node, until none is left.
  fn drain(&mut self) {
    while let Some((node, place, at)) = self.queue.pop() {
      let from = self
        .edges
        .partition_point(|edge: &(u32, u32, Location)| edge.0 < node);
      let to = self
        .edges
        .partition_point(|edge: &(u32, u32, Location)| edge.0 <= node);
      let out: &[(u32, u32, Location)] = self.edges.get(from..to).unwrap_or(&[]);
      for &(_, next, written) in out {
        // Across an edge out of a slot, the witness is where the argument is given.
        let witness = if node < self.slots { written } else { at };
        self.gain(next, place, witness);
      }
    }
  }
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
/// one: what fills a slot an occurrence leaves out.
pub(crate) fn default_of(model: &Model, def: DefId, index: u32) -> Option<Location> {
  let default = model
    .def(def)
    .type_params()
    .nth(index as usize)?
    .default()?;
  Some(model.type_expr(default).location())
}

/// The name of the type parameter `index` of `def`, of `model`, or nothing when it has none.
pub(crate) fn parameter(model: &Model, def: DefId, index: u32) -> &str {
  match model.def(def).type_params().nth(index as usize) {
    Some(param) => param.name(),
    None => "",
  }
}
