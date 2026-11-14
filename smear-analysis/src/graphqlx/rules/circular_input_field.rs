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
//! - a field of an ordinary input object typed by the bare parameter carries it when written `T!`,
//!   and carries it when non-null when written `T`;
//! - a link field whose complete type passes the parameter, `T` or `T!`, as an argument of input
//!   objects carries it as the slots it is passed through carry their arguments: each step asks
//!   that its slot be carried, or carried when non-null where the argument there is non-null, and
//!   the last gives the parameter what its slot has, carried when non-null being carried where the
//!   parameter is `T!`. An argument a default fills is read as a written one, with the nullness
//!   the default gives it: the complete type holds both, so a slot whose parameter is carried
//!   nowhere carries nothing, whatever fills it.
//!
//! A OneOf carries none of its type parameters: it is given one field, any one, so no argument is
//! one a value must hold. An instantiation of a OneOf is finite when one of its alternatives is: a
//! field that is no link; a field typed by a type parameter, whose finiteness is its argument's; a
//! field of an input object type. Through an input object that holds it the choice stays a
//! choice: an instantiation of a OneOf some of whose alternatives are the arguments of the
//! holder's own type parameters makes the holder a *group*, the requirement that one of those
//! arguments, or one of the other alternatives, be finite; and an input object that holds an
//! instantiation of a grouped one passes the group on through the arguments it gives. So
//! `input W<T> { c: Choice<T>! }` with `input Choice<T> @oneOf { again: T, stop: Stop }` requires
//! nothing of `W<A>`'s `A` while `Stop` is finite.
//!
//! A link whose base is an instantiation requires the input object, what each argument at a slot
//! it carries requires, a list breaking it, and one alternative of each of its groups. A link to a
//! type parameter of its own input object requires nothing of it: what the parameter stands for is
//! required at each instantiation instead. A cycle closes at a requirement on the walk's path, an
//! instantiation's included, and its kind is the OneOf one when it passes a nullable field of a
//! OneOf or an alternative.
//!
//! The rule reads every input object of the model, every package's, so a dependency's generic
//! input object closes a cycle the root package writes, and every contribution's fields. A field
//! is reported where the walk reaches it, whichever contribution wrote it, an orphan extension
//! included, beside which the orphan rule speaks; it says nothing where that contribution holds a
//! phase's error, whatever errors another input object of the cycle holds.
//!
//! The rule reads the public model and nothing else.
//!
//! # What it never invents, and what it may miss
//!
//! The rule never reports an input object that a finite value satisfies: where it cannot say
//! exactly what an instantiation requires, it requires less. So it can miss a cycle, and only in
//! a program with type parameters, in two places, each a recorded gap:
//!
//! - An alternative that is an open input object type, a field of the OneOf such as `a: Wrap<T>`
//!   or an argument such as the `Wrap<U>` of `Choice<Wrap<U>>`, is finite under a use's arguments
//!   as a substitution of them would say. It is judged with the type parameters it names taken as
//!   finite instead, which requires no more than any substitution of them would. With
//!   `input Hard<T> { h: T! }` and `input Choice<T> @oneOf { a: Hard<T> }`,
//!   `input A { c: Choice<A>! }` is a cycle that is not reported.
//! - An input object keeps at most twice as many groups as it has type parameters, and two more,
//!   naming at most as many slots together, so that an instantiation costs a bounded number of
//!   steps for each argument: the monotone condition its arguments must meet can need
//!   exponentially many groups. Past that cap, the input object is judged as if it had no group,
//!   its instantiations requiring none, so a cycle that only one of its groups would close is not
//!   reported. The groups it passed on before stay with the input objects that hold it: each is
//!   true of them.
//!
//! A GraphQL document, which has no type parameter, meets neither, and is judged exactly.

use std::{
  collections::{BTreeMap, BTreeSet},
  format, vec,
  vec::Vec,
};

use crate::graphqlx::{
  Contribution, Cx, DefId, DefKind, Diagnostic, MemberId, Model, TyId, TyKind, Visitor,
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
/// pass over each input object's fields; each open input object type their complete types hold
/// read once, its arguments once, whichever fields hold it, and a closed one passed over at once;
/// a group made at most once for each open type held, passed on once through each open type held
/// of its input object, an input object keeping groups naming at most twice its type parameters
/// and two more; one node for each input object and for each instantiation a link requires, each
/// type once, with one for each of its groups, their edges a bounded number for each argument, and
/// one for the other alternatives of each group and for the input object fields of each OneOf,
/// once; and the fixed points and the coloured walk, each node and edge a bounded number of times,
/// with explicit stacks and a logarithm for each lookup of an ordered map or a sorted list. For
/// each member, one binary search. It keeps the fields to report, for the walk. Its messages quote
/// names as excerpts, so each is bounded whatever the names' lengths.
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
    nodes: BTreeMap::new(),
    parts: BTreeMap::new(),
    unbuilt: Vec::new(),
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

/// The alternatives of a OneOf input object: its fields, any one of which, given a finite value,
/// gives the OneOf one.
#[derive(Default)]
struct Alternatives {
  /// Whether one of its fields is no link, which a finite value always satisfies: a list, a type
  /// of another kind than an input object, or one that did not resolve.
  leaf: bool,
  /// The indices of its type parameters its fields are typed by, bare, in order and each once:
  /// each an alternative whose finiteness is its argument's.
  params: Vec<u32>,
  /// Whether one of its fields is an input object type: a closed one an alternative as it is, an
  /// open one, whose finiteness under a use's arguments only a substitution would say, with the
  /// OneOf's type parameters taken as finite.
  fields: bool,
}

/// The alternatives of the OneOf `input`, of `model`.
fn alternatives_of(model: &Model, input: DefId) -> Alternatives {
  let mut alternatives = Alternatives::default();
  for field in model.def(input).input_fields() {
    let Some(occurrence) = model.member(field).ty() else {
      continue;
    };
    match model.ty(model.type_expr(occurrence).ty()).kind() {
      TyKind::Param { def, index } if def == input => alternatives.params.push(index),
      TyKind::Named { def, .. } if is_input(model, def) => alternatives.fields = true,
      _ => alternatives.leaf = true,
    }
  }
  alternatives.params.sort_unstable();
  alternatives.params.dedup();
  alternatives
}

/// An alternative of a group other than the arguments at its slots, judged without them.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Other {
  /// An input object type: a closed one as it is, and an open one, which names the type
  /// parameters of the input object that holds it, with them taken as finite, which requires less
  /// than any substitution of them would.
  Ty(TyId),
  /// The fields of a OneOf that are input object types.
  OneOf(DefId),
  /// Another group's other alternatives.
  Group(u32),
}

/// A requirement of an input object's instantiations that a OneOf on the way makes a choice: a
/// value of the input object holds a finite value of one of the arguments at `slots`, or of one of
/// the other alternatives.
struct Group {
  /// The input object.
  owner: DefId,
  /// The slots, of the input object's type parameters, whose arguments are alternatives.
  slots: Vec<u32>,
  /// The other alternatives.
  others: Vec<Other>,
}

/// What each type parameter of an input object carries, by slot: whether a value of the input
/// object must hold a value of its argument; the choices a OneOf on the way leaves its
/// instantiations; and the alternatives of each OneOf.
struct Carried {
  /// The first slot of each definition, in id order, and the count of slots last.
  base: Vec<u32>,
  /// For each slot: 0 when nothing carries it, 1 when it is carried when non-null, 2 when it is
  /// carried.
  levels: Vec<u8>,
  /// Every group.
  groups: Vec<Group>,
  /// Each group by the input object whose instantiations it constrains, sorted: none of an input
  /// object past its cap.
  owners: Vec<(DefId, u32)>,
  /// The alternatives of each OneOf.
  alternatives: BTreeMap<DefId, Alternatives>,
}

impl Carried {
  /// What the type parameters of `inputs`, of `model`, carry, `one_ofs` being the OneOfs among
  /// them.
  fn of(model: &Model, inputs: &[DefId], one_ofs: &BTreeSet<DefId>) -> Self {
    let base = bases(model);
    let count = base.last().copied().unwrap_or(0) as usize;
    let alternatives: BTreeMap<DefId, Alternatives> = one_ofs
      .iter()
      .map(|&input: &DefId| (input, alternatives_of(model, input)))
      .collect();
    let mut work = Work {
      model,
      base,
      alternatives,
      levels: vec![0; count],
      types: Vec::new(),
      held: Vec::new(),
      entries: Vec::new(),
      rules: Vec::new(),
      nodes: BTreeMap::new(),
      by_def: BTreeMap::new(),
      groups: Vec::new(),
      owned: BTreeMap::new(),
      saturated: BTreeSet::new(),
      seen: BTreeSet::new(),
      slots: Vec::new(),
      opened: Vec::new(),
      fresh: Vec::new(),
      added: Vec::new(),
    };
    let mut seeds: Vec<(u32, u8)> = Vec::new();
    // The types whose arguments are still to read.
    let mut stack: Vec<TyId> = Vec::new();
    for &input in inputs {
      // A OneOf carries none of its type parameters: any one of its fields may be given, so no
      // argument is one a value must hold. What its alternatives give is its instantiations'.
      if one_ofs.contains(&input) {
        continue;
      }
      for field in model.def(input).input_fields() {
        let Some(occurrence) = model.member(field).ty() else {
          continue;
        };
        let ty = model.type_expr(occurrence).ty();
        let complete = model.ty(ty);
        let non_null = complete.non_null();
        match complete.kind() {
          TyKind::Param { def, index } if def == input => {
            if let Some(slot) = slot(&work.base, input, index) {
              let level = if non_null { 2 } else { 1 };
              seeds.push((slot, level));
            }
          }
          TyKind::Named { def, .. } if is_input(model, def) && non_null => {
            if let Some(to) = work.node(&mut stack, ty) {
              work.entries.push(Entry {
                from: Origin::Root,
                to,
                slot: None,
              });
            }
          }
          _ => {}
        }
      }
    }
    // Each open type's arguments, once, whichever fields hold it: a type parameter is passed to
    // the slot it is the argument of, and an open input object type is held when that slot
    // carries what the argument's `!` asks. A filled argument is read as a written one: the
    // complete type holds both.
    while let Some(ty) = stack.pop() {
      let Some(&from) = work.nodes.get(&ty) else {
        continue;
      };
      let TyKind::Named { def: holder, args } = model.ty(ty).kind() else {
        continue;
      };
      for (position, &argument) in args.iter().enumerate() {
        let Some(step) = slot(&work.base, holder, dense(position)) else {
          continue;
        };
        let given = model.ty(argument);
        match given.kind() {
          TyKind::Param { def, index } => {
            if let Some(target) = slot(&work.base, def, index) {
              work.rules.push(Rule {
                from,
                slot: step,
                target,
                bang: given.non_null(),
              });
            }
          }
          TyKind::Named { def, .. } if is_input(model, def) => {
            if let Some(to) = work.node(&mut stack, argument) {
              let threshold = if given.non_null() { 1 } else { 2 };
              work.entries.push(Entry {
                from: Origin::Node(from),
                to,
                slot: Some((step, threshold)),
              });
            }
          }
          _ => {}
        }
      }
    }
    work.held = vec![false; work.types.len()];
    work.run(&seeds);
    let Work {
      base,
      alternatives,
      levels,
      groups,
      owned,
      saturated,
      ..
    } = work;
    // In the order of the owners' ids, each owner's groups in the order they were made. An owner
    // past its cap is judged as if it had none: its instantiations require less.
    let mut owners: Vec<(DefId, u32)> = Vec::new();
    for (owner, (list, _)) in owned {
      if saturated.contains(&owner) {
        continue;
      }
      owners.extend(list.into_iter().map(|group: u32| (owner, group)));
    }
    Self {
      base,
      levels,
      groups,
      owners,
      alternatives,
    }
  }

  /// The alternatives of `def`, when it is a OneOf.
  fn alternatives(&self, def: DefId) -> Option<&Alternatives> {
    self.alternatives.get(&def)
  }

  /// The groups that constrain the instantiations of `def`, by id.
  fn groups_of(&self, def: DefId) -> impl Iterator<Item = u32> + '_ {
    let from = self
      .owners
      .partition_point(|&(owner, _): &(DefId, u32)| owner < def);
    let to = self
      .owners
      .partition_point(|&(owner, _): &(DefId, u32)| owner <= def);
    self
      .owners
      .get(from..to)
      .unwrap_or(&[])
      .iter()
      .map(|&(_, group): &(DefId, u32)| group)
  }

  /// The group `id`.
  fn group(&self, id: u32) -> Option<&Group> {
    self.groups.get(id as usize)
  }

  /// What the type parameter `index` of `def` carries: 0, 1 when non-null, or 2.
  fn level(&self, def: DefId, index: u32) -> u8 {
    slot(&self.base, def, index)
      .and_then(|slot: u32| self.levels.get(slot as usize).copied())
      .unwrap_or(0)
  }
}

/// What a way into an open type hangs from: a link field, or the open type that holds it as an
/// argument.
#[derive(Clone, Copy)]
enum Origin {
  /// The link field.
  Root,
  /// The node of the open type that holds it.
  Node(u32),
}

/// A way into an open type: it is held when what it hangs from is, and the slot it is the
/// argument of, when it is one, carries at least the threshold: 1 for an argument that is
/// non-null, 2 otherwise.
#[derive(Clone, Copy)]
struct Entry {
  /// What it hangs from.
  from: Origin,
  /// The open type's node.
  to: u32,
  /// The slot it is the argument of, and what the slot must carry.
  slot: Option<(u32, u8)>,
}

/// A type parameter an open type holds as an argument: when the type is held, the parameter's
/// own slot carries what the slot it is the argument of carries, carried when non-null being
/// carried where the argument is non-null.
#[derive(Clone, Copy)]
struct Rule {
  /// The node of the open type that holds it.
  from: u32,
  /// The slot it is the argument of.
  slot: u32,
  /// The parameter's own slot.
  target: u32,
  /// Whether the argument is non-null.
  bang: bool,
}

/// One entry or rule, waiting on a slot or on the open type it hangs from.
#[derive(Clone, Copy)]
enum Waiter {
  /// An entry.
  Entry(u32),
  /// A rule.
  Rule(u32),
}

/// What the slots carry while it is found: the open types and how they are held, the groups, and
/// what is still to pass on.
struct Work<'m> {
  /// The model.
  model: &'m Model,
  /// The first slot of each definition, in id order, and the count of slots last.
  base: Vec<u32>,
  /// The alternatives of each OneOf.
  alternatives: BTreeMap<DefId, Alternatives>,
  /// For each slot, what it carries.
  levels: Vec<u8>,
  /// The type of each node, an open input object type a link field's complete type holds.
  types: Vec<TyId>,
  /// For each open type, whether a value of the input object that holds it must hold one of it.
  held: Vec<bool>,
  /// Every entry.
  entries: Vec<Entry>,
  /// Every rule.
  rules: Vec<Rule>,
  /// The node of each open type.
  nodes: BTreeMap<TyId, u32>,
  /// The held nodes of each ordinary input object, which its groups pass on through.
  by_def: BTreeMap<DefId, Vec<u32>>,
  /// Every group.
  groups: Vec<Group>,
  /// The groups of each input object, with how many slots they name together.
  owned: BTreeMap<DefId, (Vec<u32>, usize)>,
  /// The input objects past their cap, which make no more groups and are judged as if they had
  /// none.
  saturated: BTreeSet<DefId>,
  /// Each group made, by its input object and alternatives, so that none is made twice.
  seen: BTreeSet<(DefId, Vec<u32>, Vec<Other>)>,
  /// The slots that gained a level and whose waiters have not seen it.
  slots: Vec<u32>,
  /// The open types that were held and whose waiters have not seen it.
  opened: Vec<u32>,
  /// The open types held for the first time, whose groups are still to make.
  fresh: Vec<u32>,
  /// The groups made that the held open types of their input object have not seen.
  added: Vec<u32>,
}

impl Work<'_> {
  /// The node of `ty`, an input object's type, numbered and pushed onto `stack` to have its
  /// arguments read when it is new; `None` for a closed type, which holds no type parameter.
  fn node(&mut self, stack: &mut Vec<TyId>, ty: TyId) -> Option<u32> {
    if self.model.ty(ty).is_closed() {
      return None;
    }
    if let Some(node) = self.nodes.get(&ty) {
      return Some(*node);
    }
    let node = dense(self.types.len());
    self.nodes.insert(ty, node);
    self.types.push(ty);
    stack.push(ty);
    Some(node)
  }

  /// Gives the slots `seeds`' levels, and spreads them along the entries and rules until nothing
  /// changes. Each slot changes at most twice and each open type once, so each waiter is woken a
  /// bounded number of times.
  fn run(&mut self, seeds: &[(u32, u8)]) {
    // Who waits on each slot, and on each open type, each as one run of a sorted list.
    let mut waiting: Vec<(u32, Waiter)> = Vec::new();
    let mut children: Vec<(u32, Waiter)> = Vec::new();
    for (index, entry) in self.entries.iter().enumerate() {
      if let Some((slot, _)) = entry.slot {
        waiting.push((slot, Waiter::Entry(dense(index))));
      }
      if let Origin::Node(above) = entry.from {
        children.push((above, Waiter::Entry(dense(index))));
      }
    }
    for (index, rule) in self.rules.iter().enumerate() {
      waiting.push((rule.slot, Waiter::Rule(dense(index))));
      children.push((rule.from, Waiter::Rule(dense(index))));
    }
    waiting.sort_by_key(|entry: &(u32, Waiter)| entry.0);
    children.sort_by_key(|entry: &(u32, Waiter)| entry.0);
    for &(slot, level) in seeds {
      self.raise(slot, level);
    }
    // What hangs from a link field is looked at once; the rest when what it waits on changes.
    for index in 0..self.entries.len() {
      if self
        .entries
        .get(index)
        .is_some_and(|entry: &Entry| matches!(entry.from, Origin::Root))
      {
        self.wake(Waiter::Entry(dense(index)));
      }
    }
    loop {
      let (list, key) = if let Some(slot) = self.slots.pop() {
        (&waiting, slot)
      } else if let Some(node) = self.opened.pop() {
        (&children, node)
      } else if let Some(node) = self.fresh.pop() {
        self.first_held(node);
        continue;
      } else if let Some(group) = self.added.pop() {
        self.pass_on(group);
        continue;
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
      Waiter::Entry(index) => self.entry(index),
      Waiter::Rule(index) => self.rule(index),
    }
  }

  /// Whether what `from` names is held: a link field always is.
  fn source(&self, from: Origin) -> bool {
    match from {
      Origin::Root => true,
      Origin::Node(node) => self.held.get(node as usize).copied().unwrap_or(false),
    }
  }

  /// Holds the open type the entry `index` leads to, when it can now.
  fn entry(&mut self, index: u32) {
    let Some(&Entry { from, to, slot }) = self.entries.get(index as usize) else {
      return;
    };
    if !self.source(from) {
      return;
    }
    if let Some((slot, threshold)) = slot
      && self.level(slot) < threshold
    {
      return;
    }
    self.hold(to);
  }

  /// Holds the open type `node`, queueing it, and as fresh, the first time it is held.
  fn hold(&mut self, node: u32) {
    let Some(held) = self.held.get_mut(node as usize) else {
      return;
    };
    if *held {
      return;
    }
    *held = true;
    self.fresh.push(node);
    self.opened.push(node);
  }

  /// The open type `node`, held for the first time: an instantiation of a OneOf makes the input
  /// object that holds it a group of its alternatives, and one of an ordinary input object passes
  /// on that input object's groups, now and as it gains them.
  fn first_held(&mut self, node: u32) {
    let Some(&ty) = self.types.get(node as usize) else {
      return;
    };
    let model = self.model;
    let TyKind::Named { def, args } = model.ty(ty).kind() else {
      return;
    };
    if self.alternatives.contains_key(&def) {
      if let Some((owner, slots, others)) = self.choice(def, args) {
        self.add(owner, slots, others);
      }
      return;
    }
    self.by_def.entry(def).or_default().push(node);
    let groups: Vec<u32> = self
      .owned
      .get(&def)
      .map(|(groups, _): &(Vec<u32>, usize)| groups.clone())
      .unwrap_or_default();
    for group in groups {
      self.induce(node, group);
    }
  }

  /// The group `id`, made: each open type of its input object held so far passes it on.
  fn pass_on(&mut self, id: u32) {
    let Some(owner) = self
      .groups
      .get(id as usize)
      .map(|group: &Group| group.owner)
    else {
      return;
    };
    let held: Vec<u32> = self.by_def.get(&owner).cloned().unwrap_or_default();
    for node in held {
      self.induce(node, id);
    }
  }

  /// What the OneOf `def`'s instantiation with `args` gives the input object that holds it: its
  /// group of the alternatives whose argument is a type parameter of that input object, with the
  /// others. Nothing when an alternative is finite whatever the arguments, or when none is a type
  /// parameter of that input object: the requirements graph answers the type at its node, and no
  /// argument is required of an instantiation. An open input object type, an argument or a field
  /// of the OneOf, is an alternative with the type parameters it names taken as finite, which
  /// requires less than a substitution would: what only a substitution would say is never
  /// required.
  fn choice(&self, def: DefId, args: &[TyId]) -> Option<(DefId, Vec<u32>, Vec<Other>)> {
    let alternatives = self.alternatives.get(&def)?;
    if alternatives.leaf {
      return None;
    }
    let mut owner: Option<DefId> = None;
    let mut slots: Vec<u32> = Vec::new();
    let mut others: Vec<Other> = Vec::new();
    for &index in &alternatives.params {
      let Some(&argument) = args.get(index as usize) else {
        continue;
      };
      match self.model.ty(argument).kind() {
        TyKind::Param { def: holder, index } => {
          if let Some(slot) = slot(&self.base, holder, index) {
            owner = Some(holder);
            slots.push(slot);
          }
        }
        TyKind::Named { def: inner, .. } if is_input(self.model, inner) => {
          others.push(Other::Ty(argument));
        }
        // A scalar, an enum, a list: an alternative a finite value always satisfies.
        _ => return None,
      }
    }
    if alternatives.fields {
      others.push(Other::OneOf(def));
    }
    let owner = owner?;
    Some((owner, slots, others))
  }

  /// Passes the group `id` of an ordinary input object on through its held open type `node`, to
  /// the input object that holds that type. An argument at one of its slots that is an open input
  /// object type is an alternative as `choice` takes one.
  fn induce(&mut self, node: u32, id: u32) {
    let Some(&ty) = self.types.get(node as usize) else {
      return;
    };
    let model = self.model;
    let TyKind::Named { def: holder, args } = model.ty(ty).kind() else {
      return;
    };
    let Some(group) = self.groups.get(id as usize) else {
      return;
    };
    let first = self.base.get(holder.get() as usize).copied().unwrap_or(0);
    let mut owner: Option<DefId> = None;
    let mut slots: Vec<u32> = Vec::new();
    let mut others: Vec<Other> = Vec::new();
    if !group.others.is_empty() {
      others.push(Other::Group(id));
    }
    for &at in &group.slots {
      let Some(position) = at.checked_sub(first) else {
        continue;
      };
      let Some(&argument) = args.get(position as usize) else {
        continue;
      };
      match model.ty(argument).kind() {
        TyKind::Param { def, index } => {
          if let Some(slot) = slot(&self.base, def, index) {
            owner = Some(def);
            slots.push(slot);
          }
        }
        TyKind::Named { def, .. } if is_input(model, def) => others.push(Other::Ty(argument)),
        _ => return,
      }
    }
    if let Some(owner) = owner {
      self.add(owner, slots, others);
    }
  }

  /// Makes `owner` the group of `slots` and `others`, once. An input object keeps at most twice as
  /// many groups as it has type parameters, and two more, naming at most as many slots together:
  /// past that, each instantiation would pay more than a bounded number of steps for each of its
  /// arguments. Then the input object is past its cap: it makes no more groups, and is judged as if
  /// it had none, which requires less of its instantiations than any group would, so that a cap
  /// never becomes a cycle.
  fn add(&mut self, owner: DefId, mut slots: Vec<u32>, mut others: Vec<Other>) {
    slots.sort_unstable();
    slots.dedup();
    others.sort_unstable();
    others.dedup();
    if slots.is_empty() || self.saturated.contains(&owner) {
      return;
    }
    if !self.seen.insert((owner, slots.clone(), others.clone())) {
      return;
    }
    let at = owner.get() as usize;
    let arity = match (
      self.base.get(at),
      at.checked_add(1).and_then(|next| self.base.get(next)),
    ) {
      (Some(&start), Some(&end)) => end.saturating_sub(start) as usize,
      _ => 0,
    };
    let (count, named) = self
      .owned
      .get(&owner)
      .map_or((0, 0), |(groups, named): &(Vec<u32>, usize)| {
        (groups.len(), *named)
      });
    let bound = arity.saturating_mul(2).saturating_add(2);
    if count >= bound || named.saturating_add(slots.len()) > bound {
      self.saturated.insert(owner);
      return;
    }
    let id = dense(self.groups.len());
    let width = slots.len();
    self.groups.push(Group {
      owner,
      slots,
      others,
    });
    let entry = self.owned.entry(owner).or_default();
    entry.0.push(id);
    entry.1 = entry.1.saturating_add(width);
    self.added.push(id);
  }

  /// Gives the rule `index`'s target what its slot carries, when the open type it hangs from is
  /// held.
  fn rule(&mut self, index: u32) {
    let Some(&Rule {
      from,
      slot,
      target,
      bang,
    }) = self.rules.get(index as usize)
    else {
      return;
    };
    if !self.source(Origin::Node(from)) {
      return;
    }
    let level = match self.level(slot) {
      0 => return,
      1 if !bang => 1,
      _ => 2,
    };
    self.raise(target, level);
  }

  /// What `slot` carries.
  fn level(&self, slot: u32) -> u8 {
    self.levels.get(slot as usize).copied().unwrap_or(0)
  }

  /// Raises `slot` to `level`, queueing it when it changes.
  fn raise(&mut self, slot: u32, level: u8) {
    if let Some(current) = self.levels.get_mut(slot as usize)
      && level > *current
    {
      *current = level;
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

/// A node of the requirements that is no type's, finite when one of its targets is: what a group's
/// other alternatives give, or what a OneOf's fields of an input object type give.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Part {
  /// The other alternatives of the group.
  Group(u32),
  /// The OneOf's fields of an input object type: a closed one as it is, and an open one, with the
  /// OneOf's type parameters taken as finite, which requires less than any substitution of them
  /// would.
  OneOf(DefId),
}

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
  /// The node of each instantiation a link requires.
  nodes: BTreeMap<TyId, u32>,
  /// The node of each part, made the first time it is asked for.
  parts: BTreeMap<Part, u32>,
  /// The parts whose edges are still to make.
  unbuilt: Vec<(u32, Part)>,
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
    loop {
      if let Some((node, ty)) = self.pending.pop() {
        self.instantiation(node, ty);
      } else if let Some((node, part)) = self.unbuilt.pop() {
        self.part_edges(node, part);
      } else {
        break;
      }
    }
  }

  /// A new node: one of `any`, finite when one target is, or else when every target is.
  fn fresh(&mut self, any: bool) -> u32 {
    let node = dense(self.any.len());
    self.any.push(any);
    self.ready.push(false);
    node
  }

  /// The node of `part`, made the first time.
  fn part(&mut self, part: Part) -> u32 {
    if let Some(node) = self.parts.get(&part) {
      return *node;
    }
    let node = self.fresh(true);
    self.parts.insert(part, node);
    self.unbuilt.push((node, part));
    node
  }

  /// Makes the edges of `node`, the node of `part`: to each other alternative of a group, or to the
  /// requirement of each of a OneOf's fields of an input object type, an open one's with the
  /// OneOf's type parameters taken as finite.
  fn part_edges(&mut self, node: u32, part: Part) {
    let carried = self.carried;
    let model = self.model;
    match part {
      Part::Group(id) => {
        let Some(group) = carried.group(id) else {
          return;
        };
        for &other in &group.others {
          let target = match other {
            Other::Ty(ty) => self.requirement(ty),
            Other::OneOf(def) => Some(self.part(Part::OneOf(def))),
            Other::Group(next) => Some(self.part(Part::Group(next))),
          };
          if let Some(target) = target {
            self.edges.push((node, target, true, None));
          }
        }
      }
      Part::OneOf(def) => {
        for field in model.def(def).input_fields() {
          let Some(occurrence) = model.member(field).ty() else {
            continue;
          };
          let ty = model.type_expr(occurrence).ty();
          if let TyKind::Named { def: inner, .. } = model.ty(ty).kind()
            && is_input(model, inner)
            && let Some(target) = self.requirement(ty)
          {
            self.edges.push((node, target, true, None));
          }
        }
      }
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
    let carried = self.carried;
    let TyKind::Named { def, args } = self.model.ty(ty).kind() else {
      return;
    };
    if let Some(alternatives) = carried.alternatives(def) {
      self.choice(node, def, args, alternatives);
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
      // A slot an ordinary input object carries passes no field of a OneOf: a OneOf carries none.
      if let TyKind::Named { def: inner, .. } = self.model.ty(argument).kind()
        && is_input(self.model, inner)
        && let Some(target) = self.requirement(argument)
      {
        self.edges.push((node, target, false, None));
      }
    }
    let groups: Vec<u32> = carried.groups_of(def).collect();
    for id in groups {
      self.group(node, id, def, args);
    }
  }

  /// Makes `node`, an instantiation with `args` of the OneOf `def` of `alternatives`, finite when
  /// one of its alternatives is: a field that is no link; one typed by a type parameter, through
  /// its argument; one of an input object type. In an open type, an argument that is a type
  /// parameter of the input object that holds it is taken as finite here, its group answering for
  /// it. An open input object type, an argument or a field of the OneOf, is the requirement of
  /// that open type, with the type parameters it names taken as finite, which requires less than a
  /// substitution would: what only a substitution would say is never required.
  fn choice(&mut self, node: u32, def: DefId, args: &[TyId], alternatives: &Alternatives) {
    let model = self.model;
    if let Some(any) = self.any.get_mut(node as usize) {
      *any = true;
    }
    if alternatives.leaf {
      self.set_ready(node);
      return;
    }
    for &index in &alternatives.params {
      let Some(&argument) = args.get(index as usize) else {
        continue;
      };
      match model.ty(argument).kind() {
        TyKind::Named { def: inner, .. } if is_input(model, inner) => {
          if let Some(target) = self.requirement(argument) {
            self.edges.push((node, target, true, None));
          }
        }
        // A type parameter of the input object that holds the type, whose group answers for it;
        // a scalar, an enum, a list, which a finite value always satisfies.
        _ => {
          self.set_ready(node);
          return;
        }
      }
    }
    if alternatives.fields {
      let target = self.part(Part::OneOf(def));
      self.edges.push((node, target, true, None));
    }
  }

  /// Gives `node`, the requirement of an instantiation with `args` of the ordinary input object
  /// `def`, the group `id` of `def`: one node, finite when one of the group's alternatives is,
  /// read as `choice` reads a OneOf's.
  fn group(&mut self, node: u32, id: u32, def: DefId, args: &[TyId]) {
    let carried = self.carried;
    let model = self.model;
    let Some(group) = carried.group(id) else {
      return;
    };
    let first = carried.base.get(def.get() as usize).copied().unwrap_or(0);
    let any = self.fresh(true);
    self.edges.push((node, any, true, None));
    for &at in &group.slots {
      let Some(&argument) = at
        .checked_sub(first)
        .and_then(|position: u32| args.get(position as usize))
      else {
        continue;
      };
      match model.ty(argument).kind() {
        TyKind::Named { def: inner, .. } if is_input(model, inner) => {
          if let Some(target) = self.requirement(argument) {
            self.edges.push((any, target, true, None));
          }
        }
        _ => {
          self.set_ready(any);
          return;
        }
      }
    }
    if !group.others.is_empty() {
      let target = self.part(Part::Group(id));
      self.edges.push((any, target, true, None));
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
