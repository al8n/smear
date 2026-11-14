//! No input object requires a value of itself.
//!
//! GraphQL refuses an input object no finite value satisfies: one that requires a value of itself
//! through fields each of which must be given. An input object is *finite* when some finite value
//! satisfies it, the least fixed point the draft's unbreakable-cycle algorithm answers: an ordinary
//! input object is finite when what each of its links requires is, and a OneOf input object, which
//! is given exactly one field, when one of its fields can be given a finite value. A *link* is a
//! field that is no list, whose base is an input object or a type parameter of its own input
//! object, and that is non-null, or any field of a OneOf. The input objects that are not finite are
//! walked in id order, each link field in member order, and the field that closes a cycle is
//! reported, once for the input object that owns it, under the code the same rule has in GraphQL:
//! `smear-schema`'s `circular-non-null-input-field` when every link on the cycle is non-null, and
//! `circular-one-of-input-field` when one is a nullable field of a OneOf, with its label and its
//! help. On a GraphQL document, which has no type parameter, this is `smear-schema`'s walk, in the
//! same order, a one-file package numbering its definitions as written.
//!
//! Generics add what an instantiation's arguments must hold. A type parameter of an input object
//! is *carried* when a value of the input object must hold a value of the parameter's argument
//! whatever that argument's `!`, and *carried when non-null* when it must where the argument is
//! non-null:
//!
//! - a field typed by the bare parameter carries it when written `T!` or in a OneOf, and carries it
//!   when non-null when written `T` in an ordinary input object;
//! - a link field that passes the parameter, written `T` or `T!`, as an argument of input objects
//!   carries it as the slots it is passed through carry their arguments: each step asks that its
//!   slot be carried, or carried when non-null where the argument written there is non-null, and
//!   the last gives the parameter what its slot has, carried when non-null being carried where the
//!   parameter is written `T!`;
//! - a link field whose complete type holds the parameter only inside a slot a default filled,
//!   under input objects' arguments, carries it: a conservative answer, which can be relaxed.
//!
//! A OneOf with a field that is no link carries none of its parameters: that field is a finite
//! alternative whatever its arguments. A link whose base is an instantiation requires the input
//! object and what each argument at a slot it carries requires, a list breaking it; an
//! instantiation of a OneOf every one of whose fields is a bare type parameter requires one of its
//! arguments, an alternative. A link to a type parameter of its own input object requires nothing
//! of it: what the parameter stands for is required at each instantiation instead. A cycle closes
//! at a requirement on the walk's path, an instantiation's included, and its kind is the OneOf one
//! when it passes a nullable field of a OneOf, directly or through a slot such a field carries.
//!
//! The rule reads every input object of the model, every package's, so a dependency's generic
//! input object closes a cycle the root package writes, and every contribution's fields. A field
//! is reported where the walk reaches it, whichever contribution wrote it, an orphan extension
//! included, beside which the orphan rule speaks; it says nothing where that contribution holds a
//! phase's error, whatever errors another input object of the cycle holds.
//!
//! The rule reads the public model and nothing else.

use std::{
  collections::{BTreeMap, BTreeSet},
  format, vec,
  vec::Vec,
};

use crate::graphqlx::{
  Contribution, Cx, DefId, DefKind, Diagnostic, MemberId, Model, TyId, TyKind, TypeExprId,
  TypeExprKind, Visitor,
  code::schema,
  diagnostic::Excerpt,
  id::dense,
  positions::{bases, slot},
};

use super::one_of;

/// Reports each field of an input object that closes a cycle no finite value breaks, once for its
/// input object.
///
/// It finds the fields in the `begin` hook, over every input object of the model, and reports each
/// in the `enter_member` hook, where the walk reaches it. It says nothing about a field whose
/// contribution has its `has_errors()` set: a phase already refused something in it.
///
/// It costs, once per walk: one look at each input object's applications, to tell a OneOf; one
/// pass over each input object's fields and the written occurrences of its link fields, each
/// once, and of the types its link fields' defaults fill, each once for the input object; one node
/// for each input object and for each instantiation a link requires, each type once; and the
/// fixed points and the coloured walk, each node and edge a bounded number of times, with explicit
/// stacks and a logarithm for each lookup of an ordered map or a sorted list. For each member, one
/// binary search. It keeps the fields to report, for the walk. Its messages quote names as
/// excerpts, so each is bounded whatever the names' lengths.
#[derive(Default)]
pub(super) struct CircularInputField {
  /// The fields that close a cycle, sorted by id, each with whether the cycle passes a nullable
  /// field of a OneOf.
  closing: Vec<(MemberId, bool)>,
}

impl Visitor for CircularInputField {
  fn begin(&mut self, cx: &mut Cx<'_>) {
    self.closing = closing(cx);
  }

  fn enter_member(&mut self, cx: &mut Cx<'_>, member: MemberId) {
    let Ok(index) = self
      .closing
      .binary_search_by_key(&member, |&(field, _): &(MemberId, bool)| field)
    else {
      return;
    };
    let Some(&(_, through_one_of)) = self.closing.get(index) else {
      return;
    };
    if cx
      .contribution()
      .is_some_and(|part: Contribution<'_>| part.has_errors())
    {
      return;
    }
    let model = cx.model();
    let field = model.member(member);
    let owner = Excerpt(model.def(field.owner()).name().as_bytes());
    let name = Excerpt(field.name().as_bytes());
    let diagnostic = if through_one_of {
      Diagnostic::builder(schema::CIRCULAR_ONE_OF_INPUT_FIELD, field.location())
        .message(format!(
          "the input object `{owner}` requires a value of itself through its field `{name}`, by \
           way of a `@oneOf` input object none of whose fields can be given a finite value"
        ))
        .primary_label("input object cycle through a @oneOf input object has no finite value")
        .help(
          "a `@oneOf` input object is given exactly one field, so it breaks a cycle only through \
           a field that can be given a finite value; give it one, such as a scalar or a list.",
        )
        .build()
    } else {
      Diagnostic::builder(schema::CIRCULAR_NON_NULL_INPUT_FIELD, field.location())
        .message(format!(
          "the input object `{owner}` requires a value of itself through its field `{name}`: \
           every field on the way is non-null and no list"
        ))
        .primary_label("input object cycle has no nullable or list link")
        .help(
          "break the cycle with a nullable link or a list; as written, no finite value satisfies \
           it.",
        )
        .build()
    };
    cx.report(diagnostic);
  }
}

/// The fields of the input objects of `cx`'s model that close a cycle, sorted by id, each with
/// whether the cycle passes a nullable field of a OneOf.
fn closing(cx: &Cx<'_>) -> Vec<(MemberId, bool)> {
  let model = cx.model();
  let inputs: Vec<DefId> = model
    .defs()
    .into_iter()
    .filter(|def: &DefId| is_input(model, *def))
    .collect();
  if inputs.is_empty() {
    return Vec::new();
  }
  let one_ofs: BTreeSet<DefId> = inputs
    .iter()
    .copied()
    .filter(|def: &DefId| one_of(cx, *def))
    .collect();
  let carried = Carried::of(model, &inputs, &one_ofs);
  let mut graph = Requirements {
    model,
    inputs: &inputs,
    one_ofs: &one_ofs,
    carried: &carried,
    alternatives: BTreeMap::new(),
    nodes: BTreeMap::new(),
    any: Vec::new(),
    ready: Vec::new(),
    edges: Vec::new(),
    pending: Vec::new(),
  };
  graph.build();
  let finite = graph.finite();
  graph.walk(&finite)
}

/// Whether `def`, of `model`, is an input object.
fn is_input(model: &Model, def: DefId) -> bool {
  model.def(def).kind() == DefKind::InputObject
}

/// Whether the OneOf `input`, of `model`, has a field that is no link: a list, a type of another
/// kind than an input object or a type parameter, or one that did not resolve.
fn has_leaf(model: &Model, input: DefId) -> bool {
  model
    .def(input)
    .input_fields()
    .into_iter()
    .any(|field: MemberId| {
      let Some(ty) = model.member(field).ty() else {
        return false;
      };
      match model.ty(model.type_expr(ty).ty()).kind() {
        TyKind::Param { .. } => false,
        TyKind::Named { def, .. } => !is_input(model, def),
        TyKind::List { .. } | TyKind::Unresolved => true,
      }
    })
}

/// The slots the fields of the OneOf `input`, of `model`, take, when every one of its fields is a
/// bare type parameter of it, in order and each once; `None` otherwise.
fn alternative(model: &Model, input: DefId) -> Option<Vec<u32>> {
  let mut slots: Vec<u32> = Vec::new();
  for field in model.def(input).input_fields() {
    let ty = model.member(field).ty()?;
    match model.ty(model.type_expr(ty).ty()).kind() {
      TyKind::Param { def, index } if def == input => slots.push(index),
      _ => return None,
    }
  }
  if slots.is_empty() {
    return None;
  }
  slots.sort_unstable();
  slots.dedup();
  Some(slots)
}

/// What each type parameter of an input object carries, by slot: whether a value of the input
/// object must hold a value of its argument, and whether a nullable field of a OneOf is on the way.
struct Carried {
  /// The first slot of each definition, in id order, and the count of slots last.
  base: Vec<u32>,
  /// For each slot: 0 when nothing carries it, 1 when it is carried when non-null, 2 when it is
  /// carried.
  levels: Vec<u8>,
  /// For each slot: whether a nullable field of a OneOf carries it, or is on the way.
  nullable: Vec<bool>,
}

impl Carried {
  /// What the type parameters of `inputs`, of `model`, carry, `one_ofs` being the OneOfs among
  /// them.
  fn of(model: &Model, inputs: &[DefId], one_ofs: &BTreeSet<DefId>) -> Self {
    let base = bases(model);
    let count = base.last().copied().unwrap_or(0) as usize;
    let mut work = Work {
      levels: vec![0; count],
      nullable: vec![false; count],
      gates: Vec::new(),
      rules: Vec::new(),
      slots: Vec::new(),
      opened: Vec::new(),
    };
    let mut seeds: Vec<(u32, u8, bool)> = Vec::new();
    let mut written: Vec<(TypeExprId, Parent)> = Vec::new();
    let mut complete: Vec<(TyId, Option<TypeExprId>)> = Vec::new();
    let mut filled: Vec<TyId> = Vec::new();
    // The types a default filled that the input object's link fields hold, each looked at once.
    let mut seen: BTreeSet<TyId> = BTreeSet::new();
    for &input in inputs {
      let is_one_of = one_ofs.contains(&input);
      if is_one_of && has_leaf(model, input) {
        continue;
      }
      seen.clear();
      for field in model.def(input).input_fields() {
        let Some(occurrence) = model.member(field).ty() else {
          continue;
        };
        let ty = model.type_expr(occurrence).ty();
        let non_null = model.ty(ty).non_null();
        // A nullable field of a OneOf.
        let through = is_one_of && !non_null;
        match model.ty(ty).kind() {
          TyKind::Param { def, index } if def == input => {
            if let Some(slot) = slot(&base, input, index) {
              let level = if is_one_of || non_null { 2 } else { 1 };
              seeds.push((slot, level, through));
            }
          }
          TyKind::Named { def, .. } if is_input(model, def) && (is_one_of || non_null) => {
            // The chains its written type passes a parameter through.
            written.push((occurrence, Parent::Root(through)));
            while let Some((next, parent)) = written.pop() {
              let TypeExprKind::Named {
                def: holder,
                args: arguments,
              } = model.type_expr(next).kind()
              else {
                continue;
              };
              if !is_input(model, holder) {
                continue;
              }
              for (position, argument) in arguments.iter().enumerate() {
                let Some(step) = slot(&base, holder, dense(position)) else {
                  continue;
                };
                let given = model.type_expr(*argument);
                match given.kind() {
                  TypeExprKind::Param { def, index } if def == input => {
                    if let Some(target) = slot(&base, input, index) {
                      work.rules.push(Rule {
                        parent,
                        slot: step,
                        target,
                        bang: given.non_null(),
                      });
                    }
                  }
                  TypeExprKind::Named { def, .. } if is_input(model, def) => {
                    let gate = dense(work.gates.len());
                    work.gates.push(Gate {
                      parent,
                      slot: step,
                      threshold: if given.non_null() { 1 } else { 2 },
                      open: false,
                      bit: false,
                    });
                    written.push((*argument, Parent::Gate(gate)));
                  }
                  _ => {}
                }
              }
            }
            // What a default filled, under input objects' arguments: each type parameter it
            // holds is taken as carried.
            complete.push((ty, Some(occurrence)));
            while let Some((next, at)) = complete.pop() {
              let TyKind::Named {
                def: holder,
                args: arguments,
              } = model.ty(next).kind()
              else {
                continue;
              };
              if !is_input(model, holder) {
                continue;
              }
              let writes: &[TypeExprId] = match at.map(|at: TypeExprId| model.type_expr(at).kind())
              {
                Some(TypeExprKind::Named { args, .. }) => args,
                _ => &[],
              };
              for (position, argument) in arguments.iter().enumerate() {
                match writes.get(position) {
                  Some(argument_at) => complete.push((*argument, Some(*argument_at))),
                  None => {
                    if seen.insert(*argument) {
                      filled.push(*argument);
                    }
                  }
                }
              }
            }
            while let Some(next) = filled.pop() {
              match model.ty(next).kind() {
                TyKind::Param { def, index } if def == input => {
                  if let Some(slot) = slot(&base, input, index) {
                    seeds.push((slot, 2, through));
                  }
                }
                TyKind::Named { def, args } if is_input(model, def) => {
                  for argument in args {
                    if seen.insert(*argument) {
                      filled.push(*argument);
                    }
                  }
                }
                _ => {}
              }
            }
          }
          _ => {}
        }
      }
    }
    work.run(&seeds);
    Self {
      base,
      levels: work.levels,
      nullable: work.nullable,
    }
  }

  /// What the type parameter `index` of `def` carries: 0, 1 when non-null, or 2.
  fn level(&self, def: DefId, index: u32) -> u8 {
    slot(&self.base, def, index)
      .and_then(|slot: u32| self.levels.get(slot as usize).copied())
      .unwrap_or(0)
  }

  /// Whether a nullable field of a OneOf carries the type parameter `index` of `def`, or is on
  /// the way.
  fn nullable(&self, def: DefId, index: u32) -> bool {
    slot(&self.base, def, index)
      .and_then(|slot: u32| self.nullable.get(slot as usize).copied())
      .unwrap_or(false)
  }
}

/// What a step of a chain hangs from: the link field itself, with whether it is a nullable field
/// of a OneOf, or the step above it.
#[derive(Clone, Copy)]
enum Parent {
  /// The link field.
  Root(bool),
  /// The gate of the step above.
  Gate(u32),
}

/// A step of a chain that holds more below it: open when what it hangs from is, and its slot
/// carries at least `threshold`.
struct Gate {
  /// What it hangs from.
  parent: Parent,
  /// The slot of the argument it is.
  slot: u32,
  /// What the slot must carry: 1 for an argument written non-null, 2 otherwise.
  threshold: u8,
  /// Whether it is open.
  open: bool,
  /// Whether a nullable field of a OneOf is on the way to it.
  bit: bool,
}

/// The last step of a chain: when what it hangs from is open, what its slot carries the target
/// slot carries, carried when non-null being carried where the parameter is written `T!`.
struct Rule {
  /// What it hangs from.
  parent: Parent,
  /// The slot the parameter is passed to.
  slot: u32,
  /// The parameter's own slot.
  target: u32,
  /// Whether the parameter is written `T!` there.
  bang: bool,
}

/// One gate or rule, waiting on a slot or on the gate above it.
#[derive(Clone, Copy)]
enum Waiter {
  /// A gate.
  Gate(u32),
  /// A rule.
  Rule(u32),
}

/// What the slots carry while it is found: the chains, and what is still to pass on.
struct Work {
  /// For each slot, what it carries.
  levels: Vec<u8>,
  /// For each slot, whether a nullable field of a OneOf is on the way.
  nullable: Vec<bool>,
  /// Every gate.
  gates: Vec<Gate>,
  /// Every rule.
  rules: Vec<Rule>,
  /// The slots that gained a level or a bit and whose waiters have not seen it.
  slots: Vec<u32>,
  /// The gates that opened or gained a bit and whose children have not seen it.
  opened: Vec<u32>,
}

impl Work {
  /// Gives the slots `seeds`' levels, and spreads them along the chains until nothing changes.
  /// Each slot changes at most three times and each gate twice, so each waiter is woken a bounded
  /// number of times.
  fn run(&mut self, seeds: &[(u32, u8, bool)]) {
    // Who waits on each slot, and on each gate, each as one run of a sorted list.
    let mut waiting: Vec<(u32, Waiter)> = Vec::new();
    let mut children: Vec<(u32, Waiter)> = Vec::new();
    for (index, gate) in self.gates.iter().enumerate() {
      waiting.push((gate.slot, Waiter::Gate(dense(index))));
      if let Parent::Gate(above) = gate.parent {
        children.push((above, Waiter::Gate(dense(index))));
      }
    }
    for (index, rule) in self.rules.iter().enumerate() {
      waiting.push((rule.slot, Waiter::Rule(dense(index))));
      if let Parent::Gate(above) = rule.parent {
        children.push((above, Waiter::Rule(dense(index))));
      }
    }
    waiting.sort_by_key(|entry: &(u32, Waiter)| entry.0);
    children.sort_by_key(|entry: &(u32, Waiter)| entry.0);
    for &(slot, level, bit) in seeds {
      self.raise(slot, level, bit);
    }
    // What hangs from a link field is looked at once; the rest when what it waits on changes.
    for index in 0..self.gates.len() {
      if self
        .gates
        .get(index)
        .is_some_and(|gate: &Gate| matches!(gate.parent, Parent::Root(_)))
      {
        self.wake(Waiter::Gate(dense(index)));
      }
    }
    for index in 0..self.rules.len() {
      if self
        .rules
        .get(index)
        .is_some_and(|rule: &Rule| matches!(rule.parent, Parent::Root(_)))
      {
        self.wake(Waiter::Rule(dense(index)));
      }
    }
    loop {
      let (list, key) = if let Some(slot) = self.slots.pop() {
        (&waiting, slot)
      } else if let Some(gate) = self.opened.pop() {
        (&children, gate)
      } else {
        break;
      };
      let from = list.partition_point(|entry: &(u32, Waiter)| entry.0 < key);
      let to = list.partition_point(|entry: &(u32, Waiter)| entry.0 <= key);
      for &(_, waiter) in list.get(from..to).unwrap_or(&[]) {
        self.wake(waiter);
      }
    }
  }

  /// Looks at `waiter` again.
  fn wake(&mut self, waiter: Waiter) {
    match waiter {
      Waiter::Gate(index) => self.gate(index),
      Waiter::Rule(index) => self.rule(index),
    }
  }

  /// Whether what `parent` names is open, and if so whether a nullable field of a OneOf is on the
  /// way to it.
  fn open(&self, parent: Parent) -> Option<bool> {
    match parent {
      Parent::Root(bit) => Some(bit),
      Parent::Gate(index) => self
        .gates
        .get(index as usize)
        .filter(|gate: &&Gate| gate.open)
        .map(|gate: &Gate| gate.bit),
    }
  }

  /// Opens the gate `index`, or gives it a bit, when it can now.
  fn gate(&mut self, index: u32) {
    let Some(&Gate {
      parent,
      slot,
      threshold,
      open,
      bit,
    }) = self.gates.get(index as usize)
    else {
      return;
    };
    let Some(above) = self.open(parent) else {
      return;
    };
    if self.level(slot) < threshold {
      return;
    }
    let gained = above || self.bit(slot);
    if open && (bit || !gained) {
      return;
    }
    if let Some(gate) = self.gates.get_mut(index as usize) {
      gate.open = true;
      gate.bit = bit || gained;
    }
    self.opened.push(index);
  }

  /// Gives the rule `index`'s target what its slot carries, when what it hangs from is open.
  fn rule(&mut self, index: u32) {
    let Some(&Rule {
      parent,
      slot,
      target,
      bang,
    }) = self.rules.get(index as usize)
    else {
      return;
    };
    let Some(above) = self.open(parent) else {
      return;
    };
    let level = match self.level(slot) {
      0 => return,
      1 if !bang => 1,
      _ => 2,
    };
    let bit = above || self.bit(slot);
    self.raise(target, level, bit);
  }

  /// What `slot` carries.
  fn level(&self, slot: u32) -> u8 {
    self.levels.get(slot as usize).copied().unwrap_or(0)
  }

  /// Whether a nullable field of a OneOf is on the way to `slot`.
  fn bit(&self, slot: u32) -> bool {
    self.nullable.get(slot as usize).copied().unwrap_or(false)
  }

  /// Raises `slot` to `level`, and gives it `bit`, queueing it when either changes.
  fn raise(&mut self, slot: u32, level: u8, bit: bool) {
    let mut changed = false;
    if let Some(current) = self.levels.get_mut(slot as usize)
      && level > *current
    {
      *current = level;
      changed = true;
    }
    if bit
      && let Some(current) = self.nullable.get_mut(slot as usize)
      && !*current
    {
      *current = true;
      changed = true;
    }
    if changed {
      self.slots.push(slot);
    }
  }
}

/// An edge of the requirements: from, to, whether it passes a nullable field of a OneOf, and the
/// link field it is when it leaves an input object's node.
type Edge = (u32, u32, bool, Option<MemberId>);

/// A frame of the coloured walk: a node, its next edge, the nullable links on the path down to it,
/// and the link field of the input object it was reached through.
type Frame = (u32, u32, u32, Option<MemberId>);

/// What each link requires, as a graph: a node for each input object, in id order, then one for
/// each instantiation a link requires, each type once.
struct Requirements<'g> {
  model: &'g Model,
  /// The input objects, in id order: the first nodes.
  inputs: &'g [DefId],
  /// The OneOfs among them.
  one_ofs: &'g BTreeSet<DefId>,
  /// What their type parameters carry.
  carried: &'g Carried,
  /// For each OneOf every one of whose fields is a bare type parameter, the slots its fields take.
  alternatives: BTreeMap<DefId, Vec<u32>>,
  /// The node of each instantiation a link requires.
  nodes: BTreeMap<TyId, u32>,
  /// For each node, whether one edge's target being finite makes it finite, rather than every
  /// one.
  any: Vec<bool>,
  /// For each node, whether it is finite whatever its edges' targets are.
  ready: Vec<bool>,
  /// Each edge: from, to, whether it passes a nullable field of a OneOf, and the link field it is
  /// when it leaves an input object's node.
  edges: Vec<Edge>,
  /// The instantiations whose edges are still to make.
  pending: Vec<(u32, TyId)>,
}

impl Requirements<'_> {
  /// Makes every node and edge: each input object's links, in member order, then what each
  /// instantiation they require requires.
  fn build(&mut self) {
    for &input in self.inputs {
      if self.one_ofs.contains(&input)
        && let Some(slots) = alternative(self.model, input)
      {
        self.alternatives.insert(input, slots);
      }
    }
    self.any = self
      .inputs
      .iter()
      .map(|input: &DefId| self.one_ofs.contains(input))
      .collect();
    self.ready = vec![false; self.inputs.len()];
    for (node, &input) in self.inputs.iter().enumerate() {
      let node = dense(node);
      let is_one_of = self.one_ofs.contains(&input);
      for field in self.model.def(input).input_fields() {
        let Some(occurrence) = self.model.member(field).ty() else {
          continue;
        };
        let ty = self.model.type_expr(occurrence).ty();
        let non_null = self.model.ty(ty).non_null();
        if !is_one_of && !non_null {
          continue;
        }
        match self.model.ty(ty).kind() {
          TyKind::Named { def, .. } if is_input(self.model, def) => {
            if let Some(target) = self.requirement(ty) {
              self.edges.push((node, target, !non_null, Some(field)));
            }
          }
          // A link to a type parameter requires nothing of this input object, and a OneOf's field
          // that is no link can be given a finite value.
          _ => {
            if is_one_of {
              self.set_ready(node);
            }
          }
        }
      }
    }
    while let Some((node, ty)) = self.pending.pop() {
      self.instantiation(node, ty);
    }
  }

  /// The node `ty`, an input object's type, requires: its definition's for one with no argument,
  /// and its own for an instantiation, made the first time.
  fn requirement(&mut self, ty: TyId) -> Option<u32> {
    let TyKind::Named { def, args } = self.model.ty(ty).kind() else {
      return None;
    };
    if args.is_empty() {
      return self.node_of(def);
    }
    if let Some(node) = self.nodes.get(&ty) {
      return Some(*node);
    }
    let node = dense(self.any.len());
    self.any.push(false);
    self.ready.push(false);
    self.nodes.insert(ty, node);
    self.pending.push((node, ty));
    Some(node)
  }

  /// The node of the input object `def`.
  fn node_of(&self, def: DefId) -> Option<u32> {
    self.inputs.binary_search(&def).ok().map(dense)
  }

  /// Marks `node` finite whatever its edges' targets are.
  fn set_ready(&mut self, node: u32) {
    if let Some(ready) = self.ready.get_mut(node as usize) {
      *ready = true;
    }
  }

  /// Makes the edges of `node`, the requirement of the instantiation `ty`.
  fn instantiation(&mut self, node: u32, ty: TyId) {
    let TyKind::Named { def, args } = self.model.ty(ty).kind() else {
      return;
    };
    if let Some(slots) = self.alternatives.get(&def).cloned() {
      // An alternative: finite when one of the arguments its fields take is.
      if let Some(any) = self.any.get_mut(node as usize) {
        *any = true;
      }
      for slot in slots {
        let Some(&argument) = args.get(slot as usize) else {
          continue;
        };
        match self.model.ty(argument).kind() {
          TyKind::Named { def: inner, .. } if is_input(self.model, inner) => {
            if let Some(target) = self.requirement(argument) {
              self.edges.push((node, target, true, None));
            }
          }
          _ => self.set_ready(node),
        }
      }
      return;
    }
    if let Some(target) = self.node_of(def) {
      self.edges.push((node, target, false, None));
    }
    for (index, &argument) in args.iter().enumerate() {
      let index = dense(index);
      let level = self.carried.level(def, index);
      let required = level >= 2 || (level == 1 && self.model.ty(argument).non_null());
      if !required {
        continue;
      }
      if let TyKind::Named { def: inner, .. } = self.model.ty(argument).kind()
        && is_input(self.model, inner)
        && let Some(target) = self.requirement(argument)
      {
        self
          .edges
          .push((node, target, self.carried.nullable(def, index), None));
      }
    }
  }

  /// Which nodes are finite: the least fixed point, a node of `any` finite when one target is and
  /// any other when every target is, each found once with a counter of what it still waits on.
  fn finite(&self) -> Vec<bool> {
    let count = self.any.len();
    let mut finite = vec![false; count];
    let mut waiting = vec![0u32; count];
    for &(from, _, _, _) in &self.edges {
      if let Some(waits) = waiting.get_mut(from as usize) {
        *waits = waits.saturating_add(1);
      }
    }
    // The edges by their target, so that the nodes that wait on one are one run.
    let mut incoming: Vec<(u32, u32)> = self
      .edges
      .iter()
      .map(|&(from, to, _, _): &Edge| (to, from))
      .collect();
    incoming.sort_unstable();
    let mut found: Vec<u32> = Vec::new();
    for node in 0..count {
      let any = self.any.get(node).copied().unwrap_or(false);
      let ready = self.ready.get(node).copied().unwrap_or(false);
      let none = waiting.get(node).is_some_and(|waits: &u32| *waits == 0);
      if ready || (!any && none) {
        if let Some(is) = finite.get_mut(node) {
          *is = true;
        }
        found.push(dense(node));
      }
    }
    while let Some(target) = found.pop() {
      let from = incoming.partition_point(|&(to, _): &(u32, u32)| to < target);
      let to = incoming.partition_point(|&(to, _): &(u32, u32)| to <= target);
      for &(_, source) in incoming.get(from..to).unwrap_or(&[]) {
        let source_at = source as usize;
        if finite.get(source_at).copied().unwrap_or(true) {
          continue;
        }
        if !self.any.get(source_at).copied().unwrap_or(false) {
          let Some(waits) = waiting.get_mut(source_at) else {
            continue;
          };
          *waits = waits.saturating_sub(1);
          if *waits != 0 {
            continue;
          }
        }
        if let Some(is) = finite.get_mut(source_at) {
          *is = true;
        }
        found.push(source);
      }
    }
    finite
  }

  /// The coloured walk over the nodes that are not finite, from each input object in id order,
  /// each node's edges in the order they were made: the link field that closes each cycle, once
  /// for the input object that owns it, with whether the cycle passes a nullable field of a OneOf;
  /// sorted by the field's id.
  fn walk(mut self, finite: &[bool]) -> Vec<(MemberId, bool)> {
    // The edges by their source, a stable sort keeping each node's in the order they were made.
    self.edges.sort_by_key(|edge: &Edge| edge.0);
    let count = self.any.len();
    // 0 for a node not reached, 1 for one on the current path, 2 for one settled.
    let mut colour = vec![0u8; count];
    // Where on the stack a node on the current path is.
    let mut depth = vec![0u32; count];
    let mut owners: BTreeSet<DefId> = BTreeSet::new();
    let mut found: Vec<(MemberId, bool)> = Vec::new();
    // Each frame: a node, its next edge, the nullable links on the path down to it, and the link
    // field of the input object it was reached through.
    let mut stack: Vec<Frame> = Vec::new();
    let is_finite = |node: u32| finite.get(node as usize).copied().unwrap_or(true);
    for start in 0..self.inputs.len() {
      let start = dense(start);
      if is_finite(start) || colour.get(start as usize).copied() != Some(0) {
        continue;
      }
      if let Some(state) = colour.get_mut(start as usize) {
        *state = 1;
      }
      if let Some(at) = depth.get_mut(start as usize) {
        *at = 0;
      }
      stack.push((start, 0, 0, None));
      while let Some((node, cursor, through, link)) = stack.pop() {
        let first = self.edges.partition_point(|edge: &Edge| edge.0 < node);
        let end = self.edges.partition_point(|edge: &Edge| edge.0 <= node);
        let at = first.saturating_add(cursor as usize);
        let edge = if at < end { self.edges.get(at) } else { None };
        let Some(&(_, target, nullable, field)) = edge else {
          if let Some(state) = colour.get_mut(node as usize) {
            *state = 2;
          }
          continue;
        };
        stack.push((node, cursor.saturating_add(1), through, link));
        if is_finite(target) {
          continue;
        }
        let here = through.saturating_add(u32::from(nullable));
        let field = field.or(link);
        match colour.get(target as usize).copied() {
          Some(0) => {
            if let Some(state) = colour.get_mut(target as usize) {
              *state = 1;
            }
            if let Some(at) = depth.get_mut(target as usize) {
              *at = dense(stack.len());
            }
            stack.push((target, 0, here, field));
          }
          Some(1) => {
            let Some(field) = field else {
              continue;
            };
            if !owners.insert(self.model.member(field).owner()) {
              continue;
            }
            // The cycle is the path from `target` down to here, then this link: its nullable
            // links are the ones counted since `target`, and this one.
            let entered = depth
              .get(target as usize)
              .and_then(|at: &u32| stack.get(*at as usize))
              .map(|frame: &Frame| frame.2);
            found.push((field, entered != Some(here)));
          }
          _ => {}
        }
      }
    }
    found.sort_unstable_by_key(|&(field, _): &(MemberId, bool)| field);
    found
  }
}
